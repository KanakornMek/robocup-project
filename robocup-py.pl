:- dynamic field/1, ball/1, player/4.

% Define the soccer field dimensions.
field(size(100, 50)).

% Define the ball's initial position.
ball(position(50, 25)).

% Define player positions and states.
% Team 1 players:
player(team1, forward, position(20, 25), stamina(100)).
player(team1, defender, position(10, 12), stamina(100)).
player(team1, goalkeeper, position(5, 25), stamina(100)).
% Team 2 players:
player(team2, forward, position(80, 25), stamina(100)).
player(team2, defender, position(90, 20), stamina(100)).
player(team2, goalkeeper, position(95, 25), stamina(100)).


get_player_positions(PlayerList) :-
    findall(player(Team, Role, position(X, Y), stamina(S)), player(Team, Role, position(X, Y), stamina(S)), PlayerList).


% Move towards the ball.
move_towards_ball(Team, Role) :-
    player(Team, Role, position(X1, Y1), stamina(S)),
    ball(position(X2, Y2)),
    XDiff is X2 - X1,
    YDiff is Y2 - Y1,
    sign(XDiff, DX),
    sign(YDiff, DY),
    NewX is X1 + DX,
    NewY is Y1 + DY,
    retract(player(Team, Role, position(X1, Y1), stamina(S))),
    assertz(player(Team, Role, position(NewX, NewY), stamina(S))),
    format('~w ~w moves to (~w, ~w)~n', [Team, Role, NewX, NewY]) , !.

% Sign function to normalize movement.
sign(X, 1) :- X > 0.
sign(X, -1) :- X < 0.
sign(X, 0) :- X =:= 0.

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
    (kick_ball(team1, forward ) ; true),
    (kick_ball(team2, forward ) ; true),
    % Move all players toward the ball
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

