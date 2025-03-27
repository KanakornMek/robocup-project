:- dynamic field/1, ball/1, player/5, ball_holder/1.

% Define the soccer field dimensions.
field(size(100, 50)).

% Define the ball's initial position.
ball(position(50, 25)).

% Define player positions and states with unique IDs.
player(p1, team1, forward, position(20, 25), stamina(100)).
player(p2, team1, forward, position(40, 30), stamina(100)).
player(p3, team1, defender, position(10, 12), stamina(100)).
player(p4, team1, goalkeeper, position(5, 25), stamina(100)).
player(p5, team2, forward, position(80, 25), stamina(100)).
player(p6, team2, forward, position(60, 20), stamina(100)).
player(p7, team2, defender, position(90, 20), stamina(100)).
player(p8, team2, goalkeeper, position(95, 25), stamina(100)).

% Update the ball holder when a player has the ball.
update_ball_holder(PlayerID) :-
    retractall(ball_holder(_)),  
    assertz(ball_holder(PlayerID)),  
    player(PlayerID, _, _, position(X, Y), _),
    retract(ball(position(_, _))),
    assertz(ball(position(X, Y))).

% Move towards the ball
move_towards_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    (Role == forward -> (prioritize_offensive_move(XDiff, YDiff), move_to_open_space(Team, X1, Y1, NewX, NewY)) ; true), 
    normalize(XDiff, YDiff, DX, DY),
    NewX is X1 + DX,
    NewY is Y1 + DY,
    retract(player(PlayerID, Team, Role, position(X1, Y1), stamina(S))),
    assertz(player(PlayerID, Team, Role, position(NewX, NewY), stamina(S))),
    format('~w ~w moves to (~w, ~w)~n', [Team, Role, NewX, NewY]),
    (abs(NewX - X2) =< 2, abs(NewY - Y2) =< 2 -> 
        (update_ball_holder(PlayerID), 
        format('~w ~w holds the ball~n',[Team, Role])) ; true), !.

% Kick the ball towards the goal.
kick_ball(PlayerID) :-
    player(PlayerID, Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    abs(X1 - X2) =< 2, abs(Y1 - Y2) =< 2, 
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

% Ensure only the player holding the ball can pass it.
pass_ball(PlayerID) :-
    ball_holder(PlayerID), 
    player(PlayerID, Team, Role, position(X1, Y1), _),
    player(TeammateID, Team, _, position(X2, Y2), _),
    PlayerID \= TeammateID, 
    abs(X1 - X2) < 20, 
    abs(Y1 - Y2) < 20, 
    retract(ball_holder(PlayerID)),
    assertz(ball_holder(TeammateID)),
    format('~w ~w passes the ball to teammate (~w, ~w)~n', [Team, Role, X2, Y2]).

% Goalkeeper catches the ball if close enough.
catch_ball(PlayerID) :-
    player(PlayerID, Team, goalkeeper, position(X, Y), _),
    ball(position(BX, BY)),
    abs(X - BX) =< 2,
    abs(Y - BY) =< 2,
    retract(ball(position(BX, BY))),
    assertz(ball(position(50, 25))),
    format('~w goalkeeper catches the ball at (~w, ~w)!~n', [Team, X, Y]), !.

% Run simulation
simulate_round :-
    (pass_ball(p1) ; true),
    (pass_ball(p5) ; true),
    (move_towards_ball(p1) ; true),
    (move_towards_ball(p3) ; true),
    (move_towards_ball(p5) ; true),
    (move_towards_ball(p7) ; true),
    (catch_ball(p4) ; true),
    (catch_ball(p8) ; true),
    ball(position(BX, BY)),
    format('Ball is now at (~w, ~w)~n', [BX, BY]).

run_simulation(0).
run_simulation(N) :-
    N > 0,
    simulate_round,
    N1 is N - 1,
    run_simulation(N1).
