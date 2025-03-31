% :- assertz(file_search_path(library,pce('prolog/lib'))).
:- use_module(library(pce)).

:- dynamic field/1, ball/1, player/5.
:- dynamic ball_holder/1.
:- dynamic score/2.
:- dynamic tackle_cooldown/1.


% Define the soccer field dimensions.
field(size(1000, 500)).

% Define the ball's initial position.
ball(position(500, 250)).

% Tackle Cooldown
tackle_cooldown(0).

% Define player positions and states.
% Team 1 players:
player(p1, team1, forward, position(200, 250), stamina(100)).
player(p2, team1, forward, position(500, 300), stamina(100)).
player(p3, team1, defender, position(700, 120), stamina(100)).
player(p4, team1, goalkeeper, position(50, 250), stamina(100)).
% Team 2 players:
player(p5, team2, forward, position(800, 250), stamina(100)).
player(p6, team2, forward, position(600, 200), stamina(100)).
player(p7, team2, defender, position(900, 200), stamina(100)).
player(p8, team2, goalkeeper, position(950, 250), stamina(100)).

score(team1, 0).
score(team2, 0).

get_other_team(team1, team2).
get_other_team(team2, team1).

% Goal position range (Y-coordinates) for scoring check.
goal_position(team1, 0, Y) :- Y >= 200, Y =< 300. % Left goal
goal_position(team2, 1000, Y) :- Y >= 200, Y =< 300. % Right goal

% Center point of the goal (for aiming passes/shots).
middle_goal_position(team1, 0, 250).    % Left goal center
middle_goal_position(team2, 1000, 250). % Right goal center

euclidean_distance(X1, Y1, X2, Y2, L) :-
    DX is X1 - X2,
    DY is Y1 - Y2,
    L is sqrt((DX**2) + (DY**2)).

taxicab_distance(X1, Y1, X2, Y2, L) :-
    DX is abs(X1 - X2),
    DY is abs(Y1 - Y2),
    L is DX + DY.

normalize(DX1, DY1, DX, DY) :-
    L is sqrt(DX1**2 + DY1**2),
    ( L =:= 0 -> DX = 0, DY = 0
    ; DX is DX1 / L,
      DY is DY1 / L
    ).

clamp(Value, Min, Max, ClampedValue) :-
    ClampedValue is max(Min, min(Max, Value)).

% Check if a position is near any opponent
is_near_opponent(X, Y, MyTeam, Threshold) :-
    get_other_team(MyTeam, OpponentTeam),
    player(_, OpponentTeam, _, position(OppX, OppY), _),
    euclidean_distance(X, Y, OppX, OppY, Dist),
    Dist =< Threshold,
    !.

% Check if a position is near any opponent
is_near_teammate(X, Y, MyTeam, Threshold) :-
    player(_, MyTeam, _, position(OppX, OppY), _),
    euclidean_distance(X, Y, OppX, OppY, Dist),
    Dist =< Threshold,
    !.

% Find an open space away from opponents near a given point (X1, Y1)
find_open_space(X1, Y1, MyTeam, SearchRadius, FoundX, FoundY) :-
    field(size(MaxX, MaxY)),
    % try points around X1, Y1
    between(0, SearchRadius, R), % Iterate radius
    Lower is -R,
    Upper is R,
    ( between(Lower, Upper, DX) ; between(Lower, Upper, DY)), % Check perimeter first
    abs(DX) =:= R ; abs(DY) =:= R, % Ensure we are on the perimeter of the square for this radius R
    TempX is X1 + DX,
    TempY is Y1 + DY,
    % check bounds
    TempX >= 0, TempX =< MaxX,
    TempY >= 0, TempY =< MaxY,
    % check if space is clear of opponents
    \+ is_near_opponent(TempX, TempY, MyTeam, 20), %check against a proximity threshold
    FoundX = TempX,
    FoundY = TempY,
    !.

% Default if no suitable open space found nearby
find_open_space(X, Y, _, _, X, Y).


% Ball Holder Management

update_ball_holder(PlayerID) :-
    retractall(ball_holder(_)),   % Remove any previous ball holder
    assertz(ball_holder(PlayerID)), % Set new ball holder
    player(PlayerID, Team, Role, position(X, Y), _),
    retractall(ball(_)),           % Update ball position to holder's position
    assertz(ball(position(X, Y))),
    format('~w (~w ~w) now has the ball at (~w, ~w)~n', [PlayerID, Team, Role, X, Y]).

clear_ball_holder :-
    retractall(ball_holder(_)).

%  Player State Update 

update_player_position(PlayerID, NewX, NewY) :-
    player(PlayerID, Team, Role, position(_, _), stamina(S)),
    field(size(MaxX, MaxY)),
    clamp(NewX, 0, MaxX, ClampedX), % Ensure within bounds
    clamp(NewY, 0, MaxY, ClampedY),
    retract(player(PlayerID, Team, Role, position(_, _), stamina(S))),
    assertz(player(PlayerID, Team, Role, position(ClampedX, ClampedY), stamina(S))),
    % If this player is the ball holder, move the ball too
    ( ball_holder(PlayerID) ->
        retractall(ball(_)),
        assertz(ball(position(ClampedX, ClampedY)))
    ; true ).

update_player_stamina(PlayerID, Stamina) :-
    player(PlayerID, Team, Role, position(X, Y), _),
    retract(player(PlayerID, _Team, _Role, _Position, _)),
    assertz(player(PlayerID, Team, Role, position(X, Y), stamina(Stamina))),
    !.

regenerate_player_stamina(PlayerID, StaminaPerSec) :-
    player(PlayerID, _Team, _Role, _Position, stamina(S)),
    Sn is S+StaminaPerSec,
    ( Sn > 100 ->
        (
            Stamina is 100
        ); (
            Stamina is Sn
        )
    ),
    update_player_stamina(PlayerID, Stamina),
    !.

move_player(PlayerID,TargetX,TargetY,Speed) :-
    % Calculate stamina usage
    player(PlayerID, Team, Role, position(X, Y), stamina(S)),
    StaminaRampThreshold is 8,
    StaminaPerSpeed is 0.5,
    ((Speed>StaminaRampThreshold)->
        StaminaUsage is StaminaPerSpeed*(Speed-StaminaRampThreshold)
    ; StaminaUsage is 0),
    StaminaLeft is S-StaminaUsage,
    ((StaminaUsage>S)->
        (
        ((S>StaminaRampThreshold)->
            ClampedSpeed is (S/StaminaPerSpeed)+StaminaRampThreshold
        ; ClampedSpeed is StaminaRampThreshold)
        )
    ; ClampedSpeed is Speed),
    ((StaminaUsage>S)->
        Sn is 0
    ; Sn is StaminaLeft),
    Dx is TargetX-X,
    Dy is TargetY-Y,
    normalize(Dx,Dy,NormX,NormY),
    Dxn is NormX*ClampedSpeed,
    Dyn is NormY*ClampedSpeed,
    NewX is X+Dxn,
    NewY is Y+Dyn,
    % Ensure within bounds
    field(size(MaxX, MaxY)),
    clamp(NewX, 0, MaxX, ClampedX), 
    clamp(NewY, 0, MaxY, ClampedY),
    retractall(player(PlayerID, _Team, _Role, _Position, _Stamina)),
    assertz(player(PlayerID, Team, Role, position(ClampedX, ClampedY), stamina(Sn))),
    % If this player is the ball holder, move the ball too
    ( ball_holder(PlayerID) ->
        retractall(ball(_)),
        assertz(ball(position(ClampedX, ClampedY)))
    ; true ),!.

% --- Actions for Player WITH Ball ---

decide_action_with_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X, Y), _),
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    euclidean_distance(X, Y, GoalX, GoalY, DistToGoal),

    % 1. Shoot if close enough?
    ( DistToGoal =< 150, Role \= goalkeeper -> % Increased shooting range, Goalkeepers usually don't shoot
        shoot(PlayerID), ! % Cut: Action decided
    ; % 2. Smart Pass?
      find_best_teammate_to_pass(PlayerID, Team, X, Y, BestTeammateID), % Check if a good pass exists
      BestTeammateID \= none -> % Found a teammate to pass to
        pass_ball_to(PlayerID, BestTeammateID), !
    ; % 3. Else, Move Towards Goal (avoiding opponents)
      move_towards_goal_with_ball(PlayerID), !
    ).
decide_action_with_ball(_). % If none of the above, do nothing this tick

distance_between_point_and_line(AX, AY, BX1, BY1, BX2, BY2, D, IX, IY) :-
    D1 is abs((BY2 - BY1)*AX - (BX2 - BX1)*AY + (BX2*BY1 - BX1*BY2)),
    L is sqrt((BY2 - BY1) ** 2 + (BX2 - BX1) ** 2),
    Lambda is ((AX - BX1) * (BX2 - BX1) + (AY - BY1) * (BY2 - BY1)) / ((BX2 - BX1) ** 2 + (BY2 - BY1) ** 2),
    IX is BX1 + Lambda * (BX2 - BX1),
    IY is BY1 + Lambda * (BY2 - BY1),
    D is D1 / L.

% Shoot the ball towards the opponent's goal center
shoot(PlayerID) :-
    ball_holder(PlayerID), % Ensure player still has ball
    player(PlayerID, Team, Role, position(X, Y), _),
    get_other_team(Team, OpponentTeam),
    player(GKID, OpponentTeam, goalkeeper, position(GKX, GKY), _),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    % Add slight randomness to shot direction
    random_between(-30, 30, RandY), % Random offset for Y
    TargetY is GoalY + RandY,
    field(size(_, MaxY)),
    clamp(TargetY, 0, MaxY, ClampedTargetY), % Clamp target Y within field bounds

    distance_between_point_and_line(GKX, GKY, X, Y, GoalX, ClampedTargetY, D, IX, IY),
    (D < 10 -> 
        (retractall(ball(_)),
        retract(player(GKID, _, _, _, _)),
        assertz(ball(position(IX, IY))), % Ball instantly moves to goal line target
        assertz(player(GKID, OpponentTeam, goalkeeper, position(IX, IY), stamina(100))),
        update_ball_holder(GKID));
        (retractall(ball(_)),
        assertz(ball(position(GoalX, ClampedTargetY))), % Ball instantly moves to goal line target
        clear_ball_holder)% Player no longer has the ball after shooting
    ), % Player no longer has the ball after shooting
    format('~w (~w ~w) SHOOTS towards (~w, ~w)!~n', [PlayerID, Team, Role, GoalX, ClampedTargetY]).

% Helper: Find the best teammate to pass to
find_best_teammate_to_pass(PlayerID, Team, X, Y, BestTeammateID) :-
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    player(PlayerID, _, Role, _, _),
    ( Role = goalkeeper -> (
        PassRange is 300
    );(
        PassRange is 100
    )), % Max pass distance

    % Find all valid teammates within range, closer to goal, and relatively open
    findall(
        Score-TeammateID,
        (   player(TeammateID, Team, _, position(TX, TY), _),
            TeammateID \= PlayerID, % Not the player themselves
            euclidean_distance(X, Y, TX, TY, DistToTeammate),
            DistToTeammate =< PassRange, % Within pass range
            euclidean_distance(X, Y, GoalX, GoalY, MyDistToGoal),
            euclidean_distance(TX, TY, GoalX, GoalY, TeammateDistToGoal),
            TeammateDistToGoal < MyDistToGoal, % Teammate is closer to goal
            \+ is_near_opponent(TX, TY, Team, 30), % Teammate is relatively open (check 30 unit radius)
            % Score: Lower is better (closer to goal is main factor)
            Score = TeammateDistToGoal
        ),
        ScoredTeammates
    ),
    sort(ScoredTeammates, SortedTeammates), % Sort by score (ascending)
    % Pick the best one (first in the sorted list)
    ( SortedTeammates = [BestScore-BestTeammateID | _] ->
        format('~w considering pass to ~w (Score: ~w)~n', [PlayerID, BestTeammateID, BestScore])
    ; BestTeammateID = none % No suitable teammate found
    ).


% Pass ball to a specific teammate
pass_ball_to(PlayerID, TeammateID) :-
    ball_holder(PlayerID),
    player(PlayerID, Team, Role, _, _),
    player(TeammateID, Team, TeammateRole, position(TeammateX, TeammateY), _),
    format('~w (~w ~w) passes to ~w (~w ~w) at (~w, ~w)~n', [PlayerID, Team, Role, TeammateID, Team, TeammateRole, TeammateX, TeammateY]),
    update_ball_holder(TeammateID).


% Move towards opponent goal, trying to avoid opponents
move_towards_goal_with_ball(PlayerID) :-
    ball_holder(PlayerID), 
    player(PlayerID, Team, Role, position(X, Y), _),
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    MoveStep is 15,

    % Calculate direct path vector
    DX_direct is GoalX - X,
    DY_direct is GoalY - Y,
    normalize(DX_direct, DY_direct, NormX, NormY),
    NextX_direct is X + NormX * MoveStep,
    NextY_direct is Y + NormY * MoveStep,

    % Check if the direct path is blocked
    ( is_near_opponent(NextX_direct, NextY_direct, Team, 25) -> 
        % Path blocked so find open space towards goal
        format('~w path blocked, finding open space...~n', [PlayerID]),
        find_open_space_towards_goal(X, Y, Team, GoalX, GoalY, 100, ChosenX, ChosenY), % Search radius 50
        DiffX is ChosenX - X,
        DiffY is ChosenY - Y,
        normalize(DiffX, DiffY, DX, DY),
        NewX is X + DX * MoveStep,
        NewY is Y + DY * MoveStep
    ; % Path clear
        NewX = NextX_direct,
        NewY = NextY_direct
    ),

    % update_player_position(PlayerID, NewX, NewY),
    move_player(PlayerID, NewX, NewY, MoveStep),
    player(PlayerID, _, _, position(CurrentX, CurrentY), _),
    format('~w (~w ~w) moves with ball towards goal to (~1f, ~1f)~n', [PlayerID, Team, Role, CurrentX, CurrentY]).

%  Find open space towards the goal
find_open_space_towards_goal(X1, Y1, MyTeam, GoalX, GoalY, SearchRadius, FoundX, FoundY) :-
    field(size(MaxX, MaxY)),
    findall(
        DistScore-pos(CandX, CandY),
        (   between(1, SearchRadius, R), % get all iterate radius
            Lower is -R, Upper is R, % lower and upper bounds

            between(Lower, Upper, DX),
            between(Lower, Upper, DY),
            %Ensure the point (DX, DY) is on the perimeter of the square with side 2*R
            (abs(DX) =:= R ; abs(DY) =:= R),
            % Now DX and DY are guaranteed to be instantiated
            CandX is X1 + DX,
            CandY is Y1 + DY,
            % Check bounds
            CandX >= 0, CandX =< MaxX,
            CandY >= 0, CandY =< MaxY,
            % Check if space is clear of opponents
            \+ is_near_opponent(CandX, CandY, MyTeam, 100),
            \+ is_near_teammate(CandX, CandY, MyTeam, 50),
            % Score based on distance to goal
            euclidean_distance(CandX, CandY, GoalX, GoalY, DistToGoal),
            DistScore = DistToGoal
        ),
        Candidates
    ),
    sort(Candidates, SortedCandidates),
    % Pick the one closest to goal
    ( SortedCandidates = [_-pos(FoundX, FoundY) | _] ->
        true
    ; FoundX = X1, FoundY = Y1 % Else no open space found
    ).


decide_action_without_ball(PlayerID) :-
    player(PlayerID, MyTeam, Role, position(X, Y), _),
    Role \= goalkeeper,
    format('~w (~w ~w) deciding action without ball...~n', [PlayerID, MyTeam, Role]), % DEBUG
    ( ball_holder(HolderID) ->
        ( player(HolderID, HolderTeam, _, _, _) -> 
            ( HolderTeam == MyTeam ->
                format('~w moving offensively.~n', [PlayerID]), % debugging remove later
                move_to_offensive_position(PlayerID, MyTeam, X, Y)
            ;
                format('~w moving defensively.~n', [PlayerID]),  
                move_to_defensive_position(PlayerID, MyTeam, X, Y, HolderID)
            )
        ;   %
            format('ERROR: Ball holder ~w player data not found! Doing nothing.~n', [HolderID]),
            true
        )
    ; % No ball holder then move towards ball
        format('~w moving towards loose ball.~n', [PlayerID]), % debugging remove later
        move_towards_ball_basic(PlayerID)
    ),
    !.

%
decide_action_without_ball(PlayerID) :-
    player(PlayerID, _, goalkeeper, _, _),
    !, 
    true.

% debuggin remove later
decide_action_without_ball(PlayerID) :-
    format('WARNING: decide_action_without_ball failed unexpectedly for ~w. Doing nothing.~n', [PlayerID]),
    true. % Succeed to prevent forall from failing


% Move to open space towards opponent goal (Offensive positioning)
move_to_offensive_position(PlayerID, Team, X, Y) :-
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    player(PlayerID, _, _, _, stamina(S)),
    ( (S < 30) ->
        (MoveStep is 9);
        (MoveStep is 12)
    ),

    % Find open space towards the opponent goal
    find_open_space_towards_goal(X, Y, Team, GoalX, GoalY, 80, TargetX, TargetY),

    % Move to that open space
    ( (TargetX =:= X, TargetY =:= Y) -> % No good space found nearby, maybe move slightly towards goal?
        DX is GoalX - X, DY is GoalY - Y
    ; DX is TargetX - X, DY is TargetY - Y % Move towards found space
    ),
    normalize(DX, DY, NormX, NormY),
    NewX is X + NormX * MoveStep,
    NewY is Y + NormY * MoveStep,

    % update_player_position(PlayerID, NewX, NewY),
    move_player(PlayerID, NewX, NewY, MoveStep),
    player(PlayerID, _, _, position(CurrentX, CurrentY), _),
    format('~w (~w) moves offensively towards (~1f, ~1f)~n', [PlayerID, Team, CurrentX, CurrentY]), !.


% Move defensively between opponent with the ball and own goal
move_to_defensive_position(PlayerID, Team, X, Y, OpponentHolderID) :-
    player(OpponentHolderID, OpponentTeam, _, position(OppX, OppY), _),
    player(PlayerID,_, Role, _, _),
    middle_goal_position(Team, MyGoalX, MyGoalY), 
    MoveStep is 14, % Defensive moves can be slightly faster/more direct

    % Calculate target position: A point on the line between opponent and goal,
    % but closer to the goal (e.g., 1/3rd of the way from goal to opponent)
    % VectorX = OppX - MyGoalX,
    % VectorY = OppY - MyGoalY,
    % TargetX = MyGoalX + VectorX * 0.3, % Adjust fraction as needed (0.3 means 30% from goal)
    % TargetY = MyGoalY + VectorY * 0.3,

    % Calculate movement vector towards the target defensive spot
    ( Role = defender -> (
        findall(
            DistX-OpponentID,
            (   player(OpponentID, OpponentTeam, _, position(OppX, _), _),
                DistX is abs(MyGoalX - OppX)
            ),
            DistToGoalList
        ),
        sort(DistToGoalList, SortedDistToGoalList), % Sort by score (ascending)
        % Pick the best one (first in the sorted list)
        ( SortedDistToGoalList = [_-OpponentClosestID | _] -> (
            player(OpponentClosestID, _, _, position(OppX, OppY), _),
            HalfX is (OppX + MyGoalX)/2,
            HalfY is (OppY + MyGoalY)/2,
            DX is (HalfX - X),
            DY is (HalfY - Y),
            normalize(DX, DY, NormX, NormY),
            NewX is X + NormX * MoveStep,
            NewY is Y + NormY * MoveStep
        ); true ) 
    );
        TargetX is OppX,
        TargetY is OppY,
        DX is TargetX - X,
        DY is TargetY - Y,
        normalize(DX, DY, NormX, NormY),
        NewX is X + NormX * MoveStep,
        NewY is Y + NormY * MoveStep
    ),
    
    % update_player_position(PlayerID, NewX, NewY),
    move_player(PlayerID, NewX, NewY, MoveStep),
    player(PlayerID, _, _, position(CurrentX, CurrentY), _),
    format('~w (~w) moves defensively towards (~1f, ~1f) covering (~1f, ~1f)~n', [PlayerID, Team, CurrentX, CurrentY, OppX, OppY]), !.

% Basic move towards ball if no one holds it
move_towards_ball_basic(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), _),
    ball(position(X2, Y2)),
    euclidean_distance(X1,Y1, X2,Y2, DistToBall),
    ProximityThreshold is 20,

    ( DistToBall =< ProximityThreshold ->
        update_ball_holder(PlayerID), !
    ; % Move towards ball
        MoveStep is 10,
        XDiff is X2 - X1,
        YDiff is Y2 - Y1,
        normalize(XDiff, YDiff, DX, DY),
        NewX is X1 + DX * MoveStep,
        NewY is Y1 + DY * MoveStep,
        % update_player_position(PlayerID, NewX, NewY),
        move_player(PlayerID, NewX, NewY, MoveStep),
        player(PlayerID, _, _, position(CurrentX, CurrentY), _),
        format('~w (~w ~w) moves towards loose ball to (~w, ~w)~n', [PlayerID, Team, Role, CurrentX, CurrentY])
    ).


% Goalkeeper catches the ball if close enough.
catch_ball(PlayerID) :-
    player(PlayerID, Team, goalkeeper, position(X, Y), _), 
    ball(position(BX, BY)),
    \+ ball_holder(_), % Only catch if ball is loose after a shot
    CatchRange is 30,
    euclidean_distance(X, Y, BX, BY, Dist),
    Dist =< CatchRange,
    middle_goal_position(Team, GoalX, _),
    ( (Team == team1, BX < 100) ; (Team == team2, BX > 900) ),
    format('~w (~w goalkeeper) CATCHES the ball at (~w, ~w)!~n', [PlayerID, Team, X, Y]),
    update_ball_holder(PlayerID),  % Update ball holder to goalkeeper
    !.

% Try to Tackle player with ball and steal ball (has random chance of unsuccess)
tackle :-
    % Find all player around the ball with proximity = 20
    ball_holder(HolderID),
    player(HolderID, HolderTeam, _, _, _),
    findall(player(PlayerID, _, _, _, _),(
        player(PlayerID, PlayerTeam, _, position(PX, PY), _),
        ball(position(X, Y)),
        PlayerID \= HolderID,
        PlayerTeam \= HolderTeam,
        XDiff is PX - X,
        YDiff is PY - Y,
        D is XDiff**2 + YDiff**2,
        D < 900,
        format('Tackle: ~w is in range~n',[PlayerID])
    ), PlayerNearby),

    length(PlayerNearby, L),

    (L > 0 -> (
        ball_holder(HolderID),
        format('Tackle: ~w is being tackled with ~w people attacking~n',[HolderID, L]),
        random(0, L, RandomIndex),
        nth0(RandomIndex, PlayerNearby, player(TackleID, _, _, _, _)),
        random(RandomNumber),
        format('Random ~w~n', [RandomNumber]),
        (RandomNumber > 0.80 -> (
            update_ball_holder(TackleID),
            retract(tackle_cooldown(_)),
            assertz(tackle_cooldown(10)),
            format('~w got the ball~n', [TackleID])
        ); true)
    ); true),
    !.

update_cooldown:-
    tackle_cooldown(C),
    C1 is C - 1,
    retract(tackle_cooldown(_)),
    assertz(tackle_cooldown(C1)).

check_goal(TeamScored) :-
    ball(position(BX, BY)),
    get_other_team(TeamScored, TeamConceded),
    goal_position(TeamConceded, BX, BY), % Check if ball crossed opponent's goal line
    score(TeamScored, S),
    Snew is S + 1,
    retract(score(TeamScored, S)),
    assertz(score(TeamScored, Snew)),
    score(team1, S1), score(team2, S2), % Get updated scores for message
    format('GOAL!!! ~w scores! Score: Team1 ~w - Team2 ~w~n', [TeamScored, S1, S2]),
    sleep(1),
    reset_field, % Reset positions after goal
    !.

% Check if ball went out of bounds (basic check)
check_ball_out :-
    ball(position(BX, BY)),
    field(size(MaxX, MaxY)),
    ( BX =< 0 ; BX >= MaxX ; BY =< 0 ; BY >= MaxY ),
    \+ goal_position(team1, BX, BY), 
    \+ goal_position(team2, BX, BY), 
    format('Ball out of bounds at (~w, ~w). Resetting.~n', [BX, BY]),
    sleep(1),
    reset_field,
    !. 

reset_field :-
    format('Resetting field...~n'),
    clear_ball_holder,
    retractall(ball(_)),
    retractall(player(_,_,_,_,_)),

    assertz(ball(position(500, 250))),
    % Reset players to initial positions
    assertz(player(p1, team1, forward, position(200, 250), stamina(100))),
    assertz(player(p2, team1, forward, position(400, 300), stamina(100))),
    assertz(player(p3, team1, defender, position(100, 120), stamina(100))),
    assertz(player(p4, team1, goalkeeper, position(50, 250), stamina(100))),
    assertz(player(p5, team2, forward, position(800, 250), stamina(100))),
    assertz(player(p6, team2, forward, position(600, 200), stamina(100))),
    assertz(player(p7, team2, defender, position(900, 200), stamina(100))),
    assertz(player(p8, team2, goalkeeper, position(950, 250), stamina(100))).


simulate_round :-
    %Check for goals or out of bounds
    ( check_goal(team1) ; check_goal(team2) ; check_ball_out ->
        true % skip rest of the logic
    ;
        % Decide action for ball holder 
        ( ball_holder(HolderID) ->
            decide_action_with_ball(HolderID),
            regenerate_player_stamina(HolderID, 2)
        ; true % No ball holder, do nothing
        ),


        % Decide actions for players w/o the ball
        findall(PlayerID, player(PlayerID, _, _, _, _), AllPlayers), % Get all players
        forall(
            member(PlayerID, AllPlayers), % Iterate through players
            ( 
                ball_holder(PlayerID) %  check if are there ball holder
                -> true % do nothing
                ; ( % else (they DON'T have the ball)...
                    decide_action_without_ball(PlayerID), % Decide based on game state
                    regenerate_player_stamina(PlayerID, 2)
                  )
            )
        ),

        % Tackle
        ( ball_holder(_) -> (
            ( (tackle_cooldown(C), C>0) -> (update_cooldown, format('Cooldown ~w~n', [C])) ; tackle)
        ); true ),

        % Goalkeepers attempt to catch (if ball is loose near them)
        ( catch_ball(p4) ; true ), 
        ( catch_ball(p8) ; true ), 

        % Print current ball position
        ball(position(BX, BY)),
        format('Ball is at (~1f, ~1f)~n', [BX, BY])
    ),
    !.


run_simulation(0).
run_simulation(N) :-
    N > 0,
    simulate_round,
    N1 is N - 1,
    run_simulation(N1),
    !.


:- pce_global(@soccer_window, new(picture('Prolog Soccer Simulation'))).
:- pce_global(@pause_button, new(button('Pause'))).
:- dynamic sim_paused/1.


create_gui :-
    field(size(FieldW, FieldH)),

    ButtonHeight = 40,
    WindowW is FieldW + 20, WindowH is FieldH + 20 + ButtonHeight,
    send(@soccer_window, size, size(WindowW, WindowH)),
    send(@soccer_window, open),
    draw_field,

    ButtonY is FieldH + 20,
    ButtonX is 10,
    send(@pause_button, label, 'Pause'), % Ensure correct initial label
    send(@soccer_window, display, @pause_button), % Display the global button
    send(@pause_button, move, point(ButtonX, ButtonY)),
    send(@pause_button, message, message(@prolog, toggle_pause, @pause_button)),

    draw_players,
    draw_ball,

    retractall(sim_paused(_)),
    assertz(sim_paused(false)).

toggle_pause(Button) :-
    retract(sim_paused(IsPaused)),
    ( IsPaused == false ->
        send(Button, label, 'Resume'),
        assertz(sim_paused(true)),
        format('Simulation paused.~n')
    ; send(Button, label, 'Pause'),
      assertz(sim_paused(false)),
      format('Simulation resumed.~n')
    ).

wait_if_paused :-
    sim_paused(true),
    !, % Cut
    sleep(0.1),
    wait_if_paused.
wait_if_paused.

draw_field :-
    field(size(FieldW, FieldH)),
    OffsetX = 10, OffsetY = 10,
    send(@soccer_window, display, new(Box, box(FieldW, FieldH))),
    send(Box, pen, 2),
    send(Box, move, point(OffsetX, OffsetY)),
    send(Box, fill_pattern, colour(forestgreen)),
    GoalHeight = 100, GoalWidth = 10,
    GoalCenterY = FieldH / 2,
    GoalTopY is GoalCenterY - GoalHeight / 2,
    % Team 1 Goal (Left)
    Goal1_VisualX = OffsetX, Goal1_VisualY is OffsetY + GoalTopY,
    send(@soccer_window, display, new(Goal1Rect, box(GoalWidth, GoalHeight))),
    send(Goal1Rect, fill_pattern, colour(white)), send(Goal1Rect, pen, 0),
    send(Goal1Rect, move, point(Goal1_VisualX, Goal1_VisualY)),
    % Team 2 Goal (Right)
    Goal2_VisualX is OffsetX + FieldW - GoalWidth, Goal2_VisualY is OffsetY + GoalTopY,
    send(@soccer_window, display, new(Goal2Rect, box(GoalWidth, GoalHeight))),
    send(Goal2Rect, fill_pattern, colour(white)), send(Goal2Rect, pen, 0),
    send(Goal2Rect, move, point(Goal2_VisualX, Goal2_VisualY)).

draw_players :-
    OffsetX = 10, OffsetY = 10,
    forall(player(PlayerID,Team, Role, position(X, Y), _),
        ( CX is OffsetX + X, CY is OffsetY + Y,
          send(@soccer_window, display, new(Circle, circle(15))),
          (Team == team1 -> send(Circle, fill_pattern, colour(red))
          ; send(Circle, fill_pattern, colour(dodgerblue))),
          send(Circle, move, point(CX, CY)),
          format(string(DisplayText), '~w ~w', [PlayerID, Role]),
          send(@soccer_window, display, new(T, text(DisplayText))),
          send(T, alignment, center),
          send(T, center, point(CX, CY - 10))
        )).

draw_ball :-
    ball(position(X, Y)),
    OffsetX = 10, OffsetY = 10,
    CX is OffsetX + X, CY is OffsetY + Y,
    ( catch(send(@soccer_window, display, new(Ball, circle(8))), _, fail) ->
      send(Ball, fill_pattern, colour(black)),
      CXNew is CX + 3.75,
      CYNew is CY + 3.75,
      send(Ball, move, point(CXNew, CYNew))
    ; true
    ).

update_gui :-
    ( catch(send(@soccer_window, clear), _, fail) ->
        draw_field,
        draw_players,
        draw_ball,
        send(@soccer_window, display, @pause_button),
        send(@soccer_window, flush) 
    ; format('GUI Window closed or not available.~n'), fail 
    ).

run_simulation_gui(N) :-
    create_gui,
    run_simulation_gui_loop(N).

run_simulation_gui_loop(0) :-
    score(team1, S1), score(team2, S2),
    format('Final Score: Team1 ~w - Team2 ~w~n', [S1, S2]),
    format('Simulation finished.~n').
run_simulation_gui_loop(N) :-
    N > 0,
    wait_if_paused,
    simulate_round,
    ( update_gui -> 
        sleep(0.05), 
        N1 is N - 1,
        run_simulation_gui_loop(N1)
    ; format('Stopping simulation due to GUI error.~n'), N1 = 0 

    ).
