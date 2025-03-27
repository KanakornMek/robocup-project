% :- assertz(file_search_path(library,pce('prolog/lib'))).

% :- use_module(library(pce)).

:- dynamic field/1, ball/1, player/4.

% Dynamically track who holds the ball
:- dynamic ball_holder/2.

% Define the soccer field dimensions.
field(size(100, 50)).

% Define the ball's initial position.
ball(position(50, 25)).

% Define player positions and states.
% Team 1 players:
player(team1, forward, position(20, 25), stamina(100)).
player(team1, forward, position(40, 30), stamina(100)).
player(team1, defender, position(10, 12), stamina(100)).
player(team1, goalkeeper, position(5, 25), stamina(100)).
% Team 2 players:
player(team2, forward, position(80, 25), stamina(100)).
player(team2, forward, position(60, 20), stamina(100)).
player(team2, defender, position(90, 20), stamina(100)).
player(team2, goalkeeper, position(95, 25), stamina(100)).


% Update the ball holder when a player has the ball
update_ball_holder(Team, Role, X, Y) :-
    retractall(ball_holder(_, _, _, _)),  % Remove any previous ball holder
    assertz(ball_holder(Team, Role, X, Y)), % Set new ball holder
    % Ball is now at the position of the ball holder
    retract(ball(position(_, _))),
    assertz(ball(position(X, Y))).



% Move towards the ball or to a strategic position
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    (Role == forward -> 
        (prioritize_offensive_move(XDiff, YDiff), 
         move_to_open_space(Team, X1, Y1, NewX, NewY)) 
        ; 
        (XDiff is X2 - X1, YDiff is Y2 - Y1, normalize(XDiff, YDiff, DX, DY), NewX is X1 + DX, NewY is Y1 + DY)),
    retract(player(Team, Role, position(X1, Y1), stamina(S))),
    assertz(player(Team, Role, position(NewX, NewY), stamina(S))),
    format('~w ~w moves to (~w, ~w)~n', [Team, Role, NewX, NewY]),
    % Update ball holder if near the ball
    (abs(NewX - X2) =< 2, abs(NewY - Y2) =< 2 -> (update_ball_holder(Team, Role, NewX, NewY), format('~w holds the ball')) ; true), !.

% Prioritize attacking or intercepting for forwards
prioritize_offensive_move(XDiff, YDiff) :-
    (XDiff > 0 -> true ; XDiff < 0 -> true ; YDiff > 0).

% Move to an open space with no enemies nearby
move_to_open_space(Team, X1, Y1, NewX, NewY) :-
    findall(position(EX, EY), (player(OpponentTeam, _, position(EX, EY), _), OpponentTeam \= Team), EnemyPositions),
    find_open_space(X1, Y1, EnemyPositions, NewX, NewY).

% Find an open space away from enemies
find_open_space(X1, Y1, EnemyPositions, NewX, NewY) :-
    between(-20, 20, DX), between(-20, 20, DY), % Search within a 20-unit radius
    NewX is X1 + DX, NewY is Y1 + DY,
    \+ is_near_enemy(NewX, NewY, EnemyPositions), !.

% Check if a position is near any enemy
is_near_enemy(X, Y, EnemyPositions) :-
    member(position(EX, EY), EnemyPositions),
    abs(X - EX) =< 2, abs(Y - EY) =< 2.

% Sign function to normalize movement.
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

normalize(DX1, DY1, DX, DY) :-
    L is sqrt(DX1**2+DY1**2),
    ( L =:= 0 -> DX = 0, DY = 0 ; 
    DX is DX1/L,
    DY is DY1/L).



% Kick the ball towards the goal.
kick_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 2, abs(Y1 - Y2) =< 2, % Relaxed kicking range for action
    goal_position(Team, GoalX, GoalY),
    XDiff is GoalX - X2,
    YDiff is GoalY - Y2,
    sign(XDiff, DX),
    sign(YDiff, DY),
    NewBallX is X2 + DX,
    NewBallY is Y2 + DY,
    retract(ball(position(X2, Y2))),
    assertz(ball(position(NewBallX, NewBallY))),
    format('~w ~w kicks the ball to (~w, ~w)~n', [Team, Role, NewBallX, NewBallY]), !.

% Ensure that only the player holding the ball can pass it.
pass_ball(Team, Role) :-
    ball_holder(Team, Role), % Ensure that the player is holding the ball
    player(Team, Role, position(X1, Y1), _),
    find_teammate_in_better_position(Team, X1, Y1, _, X2, Y2), % Find a teammate in a better position
    % Update ball holder to the new player
    retract(ball_holder(Team, Role)),
    assertz(ball_holder(Team, _)),
    retract(ball(position(_, _))),
    assertz(ball(position(X2, Y2))),
    format('~w ~w passes the ball to teammate at (~w, ~w)~n', [Team, Role, X2, Y2]).


% Find a teammate who is in a better position (closer to the goal or free of defenders)
find_teammate_in_better_position(Team, X, Y, Role, TeammateX, TeammateY) :-
    player(Team, Role, position(TeammateX, TeammateY), _),
    abs(X - TeammateX) =< 10,  % Teammate within 10 units
    abs(Y - TeammateY) =< 10,  % Teammate within 10 units
    teammate_is_better_position(TeammateX, TeammateY).

% Evaluate if a teammate is in a better position (e.g., closer to the goal)
teammate_is_better_position(X, Y) :-
    (goal_position(team1, GoalX, _), X < GoalX);  % For Team1, closer to the goal
    (goal_position(team2, GoalX, _), X > GoalX).  % For Team2, closer to the goal



% Goal position depending on the team.
goal_position(team1, 0, 25).
goal_position(team2, 100, 25).


% Goalkeeper catches the ball if close enough.
catch_ball(Team, Role) :-
    player(Team, Role, position(X, Y), stamina(S)),
    ball(position(BX, BY)),
    abs(X - BX) =< 2,
    abs(Y - BY) =< 2,
    retract(ball(position(BX, BY))),
    assertz(ball(position(50, 25))),
    format('~w ~w catches the ball at (~w, ~w)!~n', [Team, Role, X, Y]), !.





% Simulate one round of the game.
simulate_round :-
    % Attempt kicking first, prioritizing Team 1 then Team 2
    % (kick_ball(team1, forward ) ; true),
    % (kick_ball(team2, forward ) ; true),
    % Attempt passing to a teammate
    (pass_ball(team1, forward) ; true),
    (pass_ball(team2, forward) ; true),
    % Move all players
    move_towards_ball(team1, forward),
    move_towards_ball(team1, defender),
    move_towards_ball(team2, forward),
    move_towards_ball(team2, defender),
    % Goalkeepers attempt to catch.
    (catch_ball(team1, goalkeeper) ; true),
    (catch_ball(team2, goalkeeper) ; true),
    % Print current ball position.
    ball(position( BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]).

% Run simulation for N rounds.
run_simulation(0).
run_simulation(N) :-
    N > 0,
    simulate_round,
    N1 is N - 1,
    run_simulation(N1).


% :- pce_global(@soccer_window, new(picture('Prolog Soccer Simulation'))).

% % Create the gui window
% create_gui :-
%     send(@soccer_window, size, size(500, 250)),
%     send(@soccer_window, open),
%     draw_field,
%     draw_players,
%     draw_ball.

% draw_field :-
%     send(@soccer_window, display, new(Box, box(400, 200))),
%     send(Box, pen, 2),
%     send(Box, move, point(50, 25)),
%     send(Box, fill_pattern, colour(green)).

% draw_players :-
%     forall(player(Team, Role, position(X, Y), _),
%            (convert_coords(X, Y, CX, CY),
%             send(@soccer_window, display, new(Circle, circle(10))),
%             (Team == team1 -> send(Circle, fill_pattern, colour(red)) ; send(Circle, fill_pattern, colour(blue))),
%             send(Circle, move, point(CX, CY)),
%             send(@soccer_window, display, new(T, text(Role))),
%             send(T, move, point(CX, CY - 12)))).

% draw_ball :-
%     ball(position(X, Y)),
%     convert_coords(X, Y, CX, CY),
%     send(@soccer_window, display, new(Ball, circle(8))),
%     send(Ball, fill_pattern, colour(black)),
%     send(Ball, move, point(CX, CY)).

% % Convert field coordinates to gui coordinates
% convert_coords(X, Y, CX, CY) :-
%     CX is 50 + X * 4,
%     CY is 25 + Y * 4.

% % Update the GUI after a simulation step
% update_gui :-
%     send(@soccer_window, clear),
%     draw_field,
%     draw_players,
%     draw_ball.

% % Run simulation with GUI updates
% run_simulation_gui(N) :-
%     create_gui,
%     run_simulation_gui_loop(N).

% run_simulation_gui_loop(0).
% run_simulation_gui_loop(N) :-
%     N > 0,
%     simulate_round,
%     update_gui,
%     send(@soccer_window, flush),
%     sleep(0.05),
%     N1 is N - 1,
%     run_simulation_gui_loop(N1).
