% :- assertz(file_search_path(library,pce('prolog/lib'))).
:- use_module(library(pce)).

:- dynamic field/1, ball/1, player/5.
:- dynamic ball_holder/1.

% Define the soccer field dimensions.
field(size(1000, 500)).

% Define the ball's initial position.
ball(position(500, 250)).

% Define player positions and states.
% Team 1 players:
player(p1, team1, forward, position(200, 250), stamina(100)).
player(p2, team1, forward, position(400, 300), stamina(100)).
player(p3, team1, defender, position(100, 120), stamina(100)).
player(p4, team1, goalkeeper, position(50, 250), stamina(100)).
% Team 2 players:
player(p5, team2, forward, position(800, 250), stamina(100)).
player(p6, team2, forward, position(600, 200), stamina(100)).
player(p7, team2, defender, position(900, 200), stamina(100)).
player(p8, team2, goalkeeper, position(950, 250), stamina(100)).

% Update the ball holder when a player has the ball
update_ball_holder(PlayerID) :-
    retractall(ball_holder(_)),  % Remove any previous ball holder
    assertz(ball_holder(PlayerID)), % Set new ball holder
    % Ball is now at the position of the ball holder
    player(PlayerID, _, _, position(X, Y), _),
    retract(ball(position(_, _))),
    assertz(ball(position(X, Y))).

% Move towards the ball
move_towards_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    % Check if the player is a forward and whether there is a ball holder
    ((Role == forward ->
        (
            ball_holder(_),  % Check if there is a ball holder
            prioritize_offensive_move(XDiff, YDiff),
            move_to_open_space(Team, X1, Y1, NewX, NewY)
        ) ; true); true),  % For forwards, check if there's a ball holder before moving
    normalize(XDiff, YDiff, DX, DY),
    % Scale movement step for larger field
    MoveStep is 10,
    NewX is X1 + DX * MoveStep,
    NewY is Y1 + DY * MoveStep,
    % Basic boundary check (optional, but good practice)
    field(size(MaxX, MaxY)),
    clamp(NewX, 0, MaxX, ClampedX),
    clamp(NewY, 0, MaxY, ClampedY),
    retract(player(PlayerID, Team, Role, position(X1, Y1), stamina(S))),
    assertz(player(PlayerID, Team, Role, position(ClampedX, ClampedY), stamina(S))),
    format('~w ~w ~w moves to (~w, ~w)~n', [PlayerID, Team, Role, ClampedX, ClampedY]),
    % Update ball holder if near the ball (Scaled distance)
    ProximityThreshold is 20,
    (abs(ClampedX - X2) =< ProximityThreshold, abs(ClampedY - Y2) =< ProximityThreshold ->
        (update_ball_holder(PlayerID),
         format('~w ~w ~w holds the ball~n',[PlayerID,Team, Role]))
    ; true), !.

% Helper to clamp values within a range
clamp(Value, Min, Max, ClampedValue) :-
    ClampedValue is max(Min, min(Max, Value)).

% Prioritize attacking or intercepting for forwards
prioritize_offensive_move(XDiff, YDiff) :-
    (XDiff > 0 -> true ; XDiff < 0 -> true ; YDiff > 0).

% Move to an open space with no enemies nearby
move_to_open_space(Team, X1, Y1, NewX, NewY) :-
    ball_holder(PlayerID),
    player(PlayerID, HoldingTeam, _, _, _),
    (HoldingTeam \= Team),
    findall(position(EX, EY), (player(_,OpponentTeam, _, position(EX, EY), _), OpponentTeam \= Team), EnemyPositions),
    find_open_space(X1, Y1, EnemyPositions, NewX, NewY).

% Find an open space away from enemies
find_open_space(X1, Y1, EnemyPositions, NewX, NewY) :-
    SearchRadius is 50,
    LowerBound is -SearchRadius,
    between(LowerBound, SearchRadius, DX),
    between(LowerBound, SearchRadius, DY),
    NewX is X1 + DX, NewY is Y1 + DY,
    field(size(MaxX, MaxY)),
    NewX >= 0, NewX =< MaxX,
    NewY >= 0, NewY =< MaxY,
    \+ is_near_enemy(NewX, NewY, EnemyPositions), !.
find_open_space(X, Y, _, X, Y). % Default: stay if no open space found nearby or if out of bounds

% Check if a position is near any enemy
is_near_enemy(X, Y, EnemyPositions) :-
    ProximityThreshold is 20,
    member(position(EX, EY), EnemyPositions),
    abs(X - EX) =< ProximityThreshold, abs(Y - EY) =< ProximityThreshold.

sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

normalize(DX1, DY1, DX, DY) :-
    L is sqrt(DX1**2+DY1**2),
    ( L =:= 0 -> DX = 0, DY = 0 ;
    DX is DX1/L,
    DY is DY1/L).

% Kick the ball towards the goal.
kick_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    ProximityThreshold is 20,
    abs(X1 - X2) =< ProximityThreshold, abs(Y1 - Y2) =< ProximityThreshold, % Scaled kicking range
    goal_position(Team, GoalX, GoalY),
    XDiff is GoalX - X2,
    YDiff is GoalY - Y2,
    normalize(XDiff, YDiff, DX, DY), % Normalize direction
    % Scale kick distance
    KickStep is 10,
    NewBallX is X2 + DX * KickStep,
    NewBallY is Y2 + DY * KickStep,
    retract(ball(position(X2, Y2))),
    assertz(ball(position(NewBallX, NewBallY))),
    format('~w ~w ~w kicks the ball to (~w, ~w)~n', [PlayerID, Team, Role, NewBallX, NewBallY]), !.

% Ensure that only the player holding the ball can pass it.
pass_ball(PlayerID) :-
    ball_holder(PlayerID), % Ensure that the player is holding the ball
    player(PlayerID, Team, Role, position(X1, Y1), _),
    find_teammate_in_better_position(PlayerID, Team, X1, Y1, TeammateID, TeammateX, TeammateY),
    % Update ball holder to the new player
    retract(ball_holder(PlayerID)),
    assertz(ball_holder(TeammateID)),
    retract(ball(position(_, _))),
    assertz(ball(position(TeammateX, TeammateY))),
    format('~w ~w ~w passes the ball to teammate(~w ~w ~w) at (~w, ~w)~n', [PlayerID, Team, Role, TeammateID, Team, _, TeammateX, TeammateY]).

% Find a teammate who is in a better position (closer to the goal or free of defenders)
find_teammate_in_better_position(PlayerID, Team, X, Y, TeammateID, TeammateX, TeammateY) :-
    player(TeammateID, Team, _, position(TeammateX, TeammateY), _),
    PlayerID \= TeammateID, % Ensure it's not the same player
    PassRange is 200, % Scaled pass range
    abs(X - TeammateX) =< PassRange,
    abs(Y - TeammateY) =< PassRange,
    teammate_is_better_position(Team, TeammateX, TeammateY).

% Evaluate if a teammate is in a better position (e.g., closer to the goal)
teammate_is_better_position(Team, X, _) :-
    (Team == team1, goal_position(team1, GoalX, _), X < GoalX);  % For Team1, closer to the goal
    (Team == team2, goal_position(team2, GoalX, _), X > GoalX).  % For Team2, closer to the goal

% Goal position depending on the team.
goal_position(team1, 0, 250).
goal_position(team2, 1000, 250).

% Goalkeeper catches the ball if close enough.
catch_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X, Y), stamina(S)),
    Role == goalkeeper, % Ensure only goalkeepers catch
    ball(position(BX, BY)),
    ProximityThreshold is 20, 
    abs(X - BX) =< ProximityThreshold,
    abs(Y - BY) =< ProximityThreshold,
    retract(ball(position(BX, BY))),
    assertz(ball(position(500, 250))), % Reset to new center (Scaled)
    format('~w ~w catches the ball at (~w, ~w)!~n', [Team, Role, X, Y]),
    retractall(ball_holder(_)), % Goalkeeper catch resets ball holder status
    !.

% Simulate one round of the game. (Logic structure unchanged)
simulate_round :-
    % Ball Holder Actions first (Pass)
    ( ball_holder(HolderID) ->
        ( (pass_ball(HolderID); true) )
    ; true ),

    % Move non-holder players
    forall(player(PlayerID, _, Role, _, _),
           ( \+ ball_holder(PlayerID), Role \= goalkeeper -> (move_towards_ball(PlayerID) ; true) ; true )),

    % Goalkeepers attempt to catch.
    (catch_ball(p4) ; true),
    (catch_ball(p8) ; true),

    % Print current ball position.
    ball(position( BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]).

run_simulation(0).
run_simulation(N) :-
    N > 0,
    simulate_round,
    N1 is N - 1,
    run_simulation(N1).

:- pce_global(@soccer_window, new(picture('Prolog Soccer Simulation'))).

% Create the gui window
create_gui :-
    field(size(FieldW, FieldH)),
    WindowW is FieldW + 20, % Add padding
    WindowH is FieldH + 20, % Add padding
    send(@soccer_window, size, size(WindowW, WindowH)),
    send(@soccer_window, open),
    draw_field,
    draw_players,
    draw_ball.

draw_field :-
    field(size(FieldW, FieldH)),
    OffsetX = 10,
    OffsetY = 10,
    send(@soccer_window, display, new(Box, box(FieldW, FieldH))),
    send(Box, pen, 2),
    send(Box, move, point(OffsetX, OffsetY)), % Apply offset
    send(Box, fill_pattern, colour(green)),

    GoalHeight = 100, % Visual height of the goal in the GUI
    GoalWidth = 10,   % Visual depth/width of the goal rectangle in the GUI
    GoalCenterY = FieldH / 2, % Vertical center of the field/goal
    GoalTopY is GoalCenterY - GoalHeight / 2, % Calculate top Y coordinate for the goal rectangle

    %  Team 1 Goal (Left side)
    Goal1_VisualX is OffsetX,
    Goal1_VisualY is OffsetY + GoalTopY, 
    send(@soccer_window, display, new(Goal1Rect, box(GoalWidth, GoalHeight))),
    send(Goal1Rect, fill_pattern, colour(white)),
    send(Goal1Rect, pen, 0), 
    send(Goal1Rect, move, point(Goal1_VisualX, Goal1_VisualY)),

    %  Team 2 Goal (Right side)
    Goal2_VisualX is OffsetX + FieldW - GoalWidth, 
    Goal2_VisualY is OffsetY + GoalTopY, 
    send(@soccer_window, display, new(Goal2Rect, box(GoalWidth, GoalHeight))),
    send(Goal2Rect, fill_pattern, colour(white)),
    send(Goal2Rect, pen, 0),
    send(Goal2Rect, move, point(Goal2_VisualX, Goal2_VisualY)).

draw_players :-
    OffsetX = 10,
    OffsetY = 10,
    forall(player(PlayerID,Team, Role, position(X, Y), _),
           ( CX is OffsetX + X, % Apply offset
             CY is OffsetY + Y, % Apply offset
             send(@soccer_window, display, new(Circle, circle(10))), % Size kept same, might look small
             (Team == team1 -> send(Circle, fill_pattern, colour(red)) ; send(Circle, fill_pattern, colour(blue))),
             send(Circle, move, point(CX, CY)),
             format(string(DisplayText), '~w', [PlayerID]), % Simplified label
             send(@soccer_window, display, new(T, text(DisplayText))),
             send(T, move, point(CX - 5, CY - 15)) % Adjusted label position slightly
           )).

draw_ball :-
    ball(position(X, Y)),
    OffsetX = 10,
    OffsetY = 10,
    CX is OffsetX + X, % Apply offset
    CY is OffsetY + Y, % Apply offset
    send(@soccer_window, display, new(Ball, circle(8))), % Size kept same
    send(Ball, fill_pattern, colour(black)),
    send(Ball, move, point(CX, CY)).

% Update the GUI after a simulation step
update_gui :-
    send(@soccer_window, clear),
    draw_field,
    draw_players,
    draw_ball.

% Run simulation with GUI updates
run_simulation_gui(N) :-
    create_gui,
    run_simulation_gui_loop(N).

run_simulation_gui_loop(0) :-
    format('Simulation finished.~n').
run_simulation_gui_loop(N) :-
    N > 0,
    simulate_round,
    update_gui,
    send(@soccer_window, flush),
    sleep(0.1), 
    N1 is N - 1,
    run_simulation_gui_loop(N1).
