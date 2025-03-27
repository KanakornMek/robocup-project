% :- assertz(file_search_path(library,pce('prolog/lib'))).

% :- use_module(library(pce)).

:- dynamic field/1, ball/1, player/5.

% Dynamically track who holds the ball
:- dynamic ball_holder/1.

% Define the soccer field dimensions.
field(size(100, 50)).

% Define the ball's initial position.
ball(position(50, 25)).

% Define player positions and states.
% Team 1 players:
player(p1, team1, forward, position(20, 25), stamina(100)).
player(p2, team1, forward, position(40, 30), stamina(100)).
player(p3, team1, defender, position(10, 12), stamina(100)).
player(p4, team1, goalkeeper, position(5, 25), stamina(100)).
% Team 2 players:
player(p5, team2, forward, position(80, 25), stamina(100)).
player(p6, team2, forward, position(60, 20), stamina(100)).
player(p7, team2, defender, position(90, 20), stamina(100)).
player(p8, team2, goalkeeper, position(95, 25), stamina(100)).

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
    NewX is X1 + DX,
    NewY is Y1 + DY,
    retract(player(PlayerID, Team, Role, position(X1, Y1), stamina(S))),
    assertz(player(PlayerID, Team, Role, position(NewX, NewY), stamina(S))),
    format('~w ~w ~w moves to (~w, ~w)~n', [PlayerID, Team, Role, NewX, NewY]),
    % Update ball holder if near the ball
    (abs(NewX - X2) =< 2, abs(NewY - Y2) =< 2 -> (update_ball_holder(PlayerID), 
    format('~w ~w holds the ball~n',[Team, Role])) ; true), !.

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
    between(-5, 5, DX), between(-5, 5, DY), % Search within a 5-unit radius
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
kick_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), stamina(S)),
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
    abs(X - TeammateX) =< 20,  % Teammate within 20 units
    abs(Y - TeammateY) =< 20,  % Teammate within 20 units
    teammate_is_better_position(Team, TeammateX, TeammateY).

% Evaluate if a teammate is in a better position (e.g., closer to the goal)
teammate_is_better_position(Team, X, _) :-
    (Team == team1, goal_position(team1, GoalX, _), X < GoalX);  % For Team1, closer to the goal
    (Team == team2, goal_position(team2, GoalX, _), X > GoalX).  % For Team2, closer to the goal



% Goal position depending on the team.
goal_position(team1, 0, 25).
goal_position(team2, 100, 25).


% Goalkeeper catches the ball if close enough.
catch_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X, Y), stamina(S)),
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
    (pass_ball(p1) ; true),
    (pass_ball(p2) ; true),
    (pass_ball(p3) ; true),
    (pass_ball(p5) ; true),
    (pass_ball(p6) ; true),
    (pass_ball(p7) ; true),
    % Move all players
    (move_towards_ball(p1) ; true),
    (move_towards_ball(p2) ; true),
    (move_towards_ball(p3) ; true),
    (move_towards_ball(p5) ; true),
    (move_towards_ball(p6) ; true),
    (move_towards_ball(p7) ; true),
    % Goalkeepers attempt to catch.
    (catch_ball(p4) ; true),
    (catch_ball(p8) ; true),
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