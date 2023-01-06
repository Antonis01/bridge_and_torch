% bridge is a dynamic fact that contains a tourist,
% the time they need to cross the bridge
% and the side of the bridge that they are on.

:- dynamic bridge/3.
bridge(_,_,_).

% flashlight is a dynamic fact that contains the side of the flashlight
:- dynamic flashlight/1.
flashlight(bot).

% depending on the number of tourists that we want to cross the bridge
% and the side that they are on and if the flashlight is on the same side
% we move the tourists and the flashlight to the other side of the bridge.
moves(X,T,bot):- 	
		write('Move '), write(X), write(' to '),write('bot'), nl,
		retract(bridge(X,T,top)),
		asserta(bridge(X,T,bot)),
		retract(flashlight(top)),
		asserta(flashlight(bot)).
		
moves(X,T,top):- 
		write('Move '), write(X), write(' to '),write('top'), nl,
		retract(bridge(X,T,bot)),
		assertz(bridge(X,T,top)),
		retract(flashlight(bot)),
		asserta(flashlight(top)).

moves(X,T1,Y,T2,bot):- 
		write('Move '), write(X), write(','),write(Y), write(' to '),write('bot'), nl,
		retract(bridge(X,T1,top)),
		retract(bridge(Y,T2,top)),
		asserta(bridge(X,T1,bot)),
		asserta(bridge(Y,T2,bot)),
		retract(flashlight(top)),
		asserta(flashlight(bot)).

moves(X,T1,Y,T2,top):-
       		write('Move '), write(X), write(','),write(Y), write(' to '),write('top'), nl,
		retract(bridge(X,T1,bot)),
		retract(bridge(Y,T2,bot)),
		assertz(bridge(X,T1,top)),
		assertz(bridge(Y,T2,top)),
		retract(flashlight(bot)),
		asserta(flashlight(top)).

moves(X,T1,Y,T2,Z,T3,bot):- 
		write('Move '), write(X), write(','),write(Y), write(','), write(Z), write(' to '),write('bot'), nl,
		retract(bridge(X,T1,top)),
		retract(bridge(Y,T2,top)),
		retract(bridge(Z,T3,top)),
		asserta(bridge(X,T1,bot)),
		asserta(bridge(Y,T2,bot)),
		asserta(bridge(Z,T3,bot)),
		retract(flashlight(top)),
		asserta(flashlight(bot)).

moves(X,T1,Y,T2,Z,T3,top):- 
		write('Move '), write(X), write(','),write(Y), write(','), write(Z), write(' to '),write('top'), nl,
		retract(bridge(X,T1,bot)),
		retract(bridge(Y,T2,bot)),
		retract(bridge(Z,T3,bot)),
		assertz(bridge(X,T1,top)),
		assertz(bridge(Y,T2,top)),
		assertz(bridge(Z,T3,top)),
		retract(flashlight(bot)),
		asserta(flashlight(top)).


% insert the tourists and the time they need to cross the bridge to the top side
insert_top_values(Total,Y) :-
	Y =< Total -> (
	write('Insert '), write(Y), write(' tourist: '),
	read(X), nl, 
	write('Insert '), write(Y), write(' tourist\'s time: '),
	read(T), nl, 
	assertz(bridge(X,T,top)), 
	Y1 is Y+1,
	insert_top_values(Total,Y1)).

% insert the tourists and the time they need to cross the bridge to the bottom side
insert_bot_values(Total,Y) :-
	Y =< Total -> (
        write('Insert '), write(Y), write(' tourist: '),
        read(X), nl, 
        write('Insert '), write(Y), write(' tourist\'s time: '),
        read(T), nl,
        assertz(bridge(X,T,'bot')), 
        Y1 is Y+1,
        insert_bot_values(Total,Y1)).

move(Num) :-
	Num = 2 -> (
		moves(_,_,_,_,top),
		moves(_,_,_,_,_,_,bot),
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot),
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot)
	
	);
	Num = 3 -> (
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot),
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot)
	);
	Num = 4 -> (
		moves(_,_,top),
		moves(_,_,_,_,bot),
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot)
	);
	Num = 5 -> (
		moves(_,_,top),
		moves(_,_,_,_,_,_,bot)
	).


start :-
	TotalTourists is 7,
	write('Insert number of tourists who are on top (2-5): '),
	read(TopNum), nl,
	(TopNum < 6, TopNum > 1),
	
	% We use the \+ (not provable operator) because insert_top_values 
	% is a recursive function that returns false when the number 
	% of tourists is equal to the total number of tourists on top, 
	% so we use it to turn false into true.
	\+ insert_top_values(TopNum,1),

	BotNum is TotalTourists - TopNum, 
	write('Insert '),write(BotNum),write(' tourists who are on bot: '),
	\+ insert_bot_values(BotNum,1),
	flashlight(bot),
	
	write('Start'), nl,
	move(BotNum),
	write('Finish').



