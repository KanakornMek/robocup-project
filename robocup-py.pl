
:- dynamic field/1, ball/1, player/5.
:- dynamic ball_holder/1.
:- dynamic score/2.

% --- Core Game Data ---

% Define the soccer field dimensions.
field(size(1000, 500)).

% Define the ball's initial position.
ball(position(500, 250)).

% Define player positions and states.
% Team 1 players:
player(p1, team1, forward, position(200, 250), stamina(100)).
player(p2, team1, forward, position(500, 300), stamina(100)). % Adjusted starting pos for testing
player(p3, team1, defender, position(700, 120), stamina(100)).
player(p4, team1, goalkeeper, position(50, 250), stamina(100)).
% Team 2 players:
player(p5, team2, forward, position(800, 250), stamina(100)). % Adjusted starting pos for testing
player(p6, team2, forward, position(600, 200), stamina(100)). % Adjusted starting pos for testing
player(p7, team2, defender, position(900, 200), stamina(100)).
player(p8, team2, goalkeeper, position(950, 250), stamina(100)).

score(team1, 0).
score(team2, 0).

% --- Helper Predicates ---

get_other_team(team1, team2).
get_other_team(team2, team1).

% Goal position range (Y-coordinates) for scoring check.
goal_position(team1, 0, Y) :- Y >= 200, Y =< 300. % Left goal
goal_position(team2, 1000, Y) :- Y >= 200, Y =< 300. % Right goal

% Center point of the goal (for aiming passes/shots).
middle_goal_position(team1, 0, 250).    % Left goal center
middle_goal_position(team2, 1000, 250). % Right goal center

distance(X1, Y1, X2, Y2, L) :-
    DX is X1 - X2,
    DY is Y1 - Y2,
    L is sqrt((DX**2) + (DY**2)).

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
    distance(X, Y, OppX, OppY, Dist),
    Dist =< Threshold,
    !. % Cut: Found one nearby opponent, no need to check others

% Find an open space away from opponents near a given point (X1, Y1)
find_open_space(X1, Y1, MyTeam, SearchRadius, FoundX, FoundY) :-
    field(size(MaxX, MaxY)),
    % Try points in expanding circles/squares around X1, Y1
    between(0, SearchRadius, R), % Iterate radius
    Lower is -R,
    Upper is R,
    ( between(Lower, Upper, DX) ; between(Lower, Upper, DY)), % Check perimeter first (implicitly by search order)
    abs(DX) =:= R ; abs(DY) =:= R, % Ensure we are on the perimeter of the square for this radius R
    TempX is X1 + DX,
    TempY is Y1 + DY,
    % Check bounds
    TempX >= 0, TempX =< MaxX,
    TempY >= 0, TempY =< MaxY,
    % Check if space is clear of opponents
    \+ is_near_opponent(TempX, TempY, MyTeam, 20), % Check against a proximity threshold
    FoundX = TempX,
    FoundY = TempY,
    !. % Cut: Found the first suitable open space

% Default if no suitable open space found nearby
find_open_space(X, Y, _, _, X, Y).


% --- Ball Holder Management ---

update_ball_holder(PlayerID) :-
    retractall(ball_holder(_)),   % Remove any previous ball holder
    assertz(ball_holder(PlayerID)), % Set new ball holder
    player(PlayerID, Team, Role, position(X, Y), _),
    retractall(ball(_)),           % Update ball position to holder's position
    assertz(ball(position(X, Y))),
    format('~w (~w ~w) now has the ball at (~w, ~w)~n', [PlayerID, Team, Role, X, Y]).

clear_ball_holder :-
    retractall(ball_holder(_)).

% --- Player State Update ---

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


% --- Actions for Player WITH Ball ---

decide_action_with_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X, Y), _),
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    distance(X, Y, GoalX, GoalY, DistToGoal),

    % 1. Shoot if close enough?
    ( DistToGoal =< 200, Role \= goalkeeper -> % Increased shooting range, Goalkeepers usually don't shoot
        shoot(PlayerID), ! % Cut: Action decided
    ; % 2. Smart Pass?
      find_best_teammate_to_pass(PlayerID, Team, X, Y, BestTeammateID), % Check if a good pass exists
      BestTeammateID \= none -> % Found a teammate to pass to
        pass_ball_to(PlayerID, BestTeammateID), ! % Cut: Action decided
    ; % 3. Else, Move Towards Goal (avoiding opponents)
      move_towards_goal_with_ball(PlayerID), ! % Cut: Action decided
    ).
decide_action_with_ball(_). % If none of the above, do nothing this tick

% Helper: Shoot the ball towards the opponent's goal center
shoot(PlayerID) :-
    ball_holder(PlayerID), % Ensure player still has ball
    player(PlayerID, Team, Role, position(X, Y), _),
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    % Add slight randomness to shot direction
    random_between(-30, 30, RandY), % Random offset for Y
    TargetY is GoalY + RandY,
    field(size(_, MaxY)),
    clamp(TargetY, 0, MaxY, ClampedTargetY), % Clamp target Y within field bounds

    retractall(ball(_)),
    assertz(ball(position(GoalX, ClampedTargetY))), % Ball instantly moves to goal line target
    clear_ball_holder, % Player no longer has the ball after shooting
    format('~w (~w ~w) SHOOTS towards (~w, ~w)!~n', [PlayerID, Team, Role, GoalX, ClampedTargetY]).

% Helper: Find the best teammate to pass to
find_best_teammate_to_pass(PlayerID, Team, X, Y, BestTeammateID) :-
    get_other_team(Team, OpponentTeam),
    middle_goal_position(OpponentTeam, GoalX, GoalY),
    PassRange is 300, % Max pass distance

    % Find all valid teammates within range, closer to goal, and relatively open
    findall(
        Score-TeammateID,
        (   player(TeammateID, Team, _, position(TX, TY), _),
            TeammateID \= PlayerID, % Not the player themselves
            distance(X, Y, TX, TY, DistToTeammate),
            DistToTeammate =< PassRange, % Within pass range
            distance(X, Y, GoalX, GoalY, MyDistToGoal),
            distance(TX, TY, GoalX, GoalY, TeammateDistToGoal),
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
        find_open_space_towards_goal(X, Y, Team, GoalX, GoalY, 50, NewX, NewY) % Search radius 50
    ; % Path clear
        NewX = NextX_direct,
        NewY = NextY_direct
    ),

    update_player_position(PlayerID, NewX, NewY),
    format('~w (~w ~w) moves with ball towards goal to (~1f, ~1f)~n', [PlayerID, Team, Role, NewX, NewY]).

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
            \+ is_near_opponent(CandX, CandY, MyTeam, 20),
            % Score based on distance to goal
            distance(CandX, CandY, GoalX, GoalY, DistToGoal),
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
    MoveStep is 12,

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

    update_player_position(PlayerID, NewX, NewY),
    format('~w (~w) moves offensively towards (~1f, ~1f)~n', [PlayerID, Team, NewX, NewY]), !.


% Move defensively between opponent with the ball and own goal
move_to_defensive_position(PlayerID, Team, X, Y, OpponentHolderID) :-
    player(OpponentHolderID, _, _, position(OppX, OppY), _),
    middle_goal_position(Team, MyGoalX, MyGoalY), 
    MoveStep is 14, % Defensive moves can be slightly faster/more direct

    % Calculate target position: A point on the line between opponent and goal,
    % but closer to the goal (e.g., 1/3rd of the way from goal to opponent)
    VectorX = OppX - MyGoalX,
    VectorY = OppY - MyGoalY,
    TargetX = MyGoalX + VectorX * 0.3, % Adjust fraction as needed (0.3 means 30% from goal)
    TargetY = MyGoalY + VectorY * 0.3,

    % Calculate movement vector towards the target defensive spot
    DX is TargetX - X,
    DY is TargetY - Y,
    normalize(DX, DY, NormX, NormY),
    NewX is X + NormX * MoveStep,
    NewY is Y + NormY * MoveStep,

    update_player_position(PlayerID, NewX, NewY),
    format('~w (~w) moves defensively towards (~1f, ~1f) covering (~1f, ~1f)~n', [PlayerID, Team, NewX, NewY, OppX, OppY]), !.

% Basic move towards ball if no one holds it
move_towards_ball_basic(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), _),
    ball(position(X2, Y2)),
    distance(X1,Y1, X2,Y2, DistToBall),
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
        update_player_position(PlayerID, NewX, NewY),
        format('~w (~w ~w) moves towards loose ball to (~w, ~w)~n', [PlayerID, Team, Role, NewX, NewY])
    ).


% Goalkeeper catches the ball if close enough.
catch_ball(PlayerID) :-
    player(PlayerID, Team, goalkeeper, position(X, Y), _), 
    ball(position(BX, BY)),
    \+ ball_holder(_), % Only catch if ball is loose after a shot
    CatchRange is 30,
    distance(X, Y, BX, BY, Dist),
    Dist =< CatchRange,
    middle_goal_position(Team, GoalX, _),
    ( (Team == team1, BX < 100) ; (Team == team2, BX > 900) ),
    format('~w (~w goalkeeper) CATCHES the ball at (~w, ~w)!~n', [PlayerID, Team, X, Y]),
    update_ball_holder(PlayerID),  % Update ball holder to goalkeeper
    !.


check_goal(TeamScored) :-
    ball(position(BX, BY)),
    get_other_team(TeamScored, TeamConceded),
    goal_position(TeamConceded, BX, BY), % Check if ball crossed opponent's goal line
    score(TeamScored, S),
    Snew is S + 1,
    retract(score(TeamScored, S)),
    assertz(score(TeamScored, Snew)),
    format('GOAL!!! ~w scores! Score: Team1 ~w - Team2 ~w~n', [TeamScored, S1, S2]),
    score(team1, S1), score(team2, S2), % Get updated scores for message
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
            decide_action_with_ball(HolderID)
        ; true % No ball holder, do nothing
        ),


        % Decide actions for players w/o the ball
        findall(PlayerID, player(PlayerID, _, _, _, _), AllPlayers), % Get all players
        forall(
            member(PlayerID, AllPlayers), % Iterate through players
            ( 
                ball_holder(PlayerID) %  check if are there ball holder?
                -> true % do nothing
                ; ( % else (they DON'T have the ball)...
                    decide_action_without_ball(PlayerID) % Decide based on game state
                  )
            )
        ),

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
    run_simulation(N1).