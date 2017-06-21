% Ludwig Tranheden
% z5129849
% Assignment 1
% 12/5-17

%% Task 1 - trigger ———————————

% trigger(+Events, -Goals) takes a list of events, in form truffle(X,Y,S) or restaurant(X,Y,S). 
% Returns Goals in the form goals(Goals_rest,Goals_truff). 
% Goals_rest and Goals_truff are lists of items in the form goal(X,Y,S).

% Base case: Empty Events -> Empty Goal_rest and Goal_truff.
trigger([], goals([],[])).

% If next event is restaurant(X,Y,S), add to Goal_rest as goal(X,Y,S)
trigger([restaurant(X, Y, S)|Tail], goals([goal(X,Y,S)|R],T)):-
    trigger(Tail, goals(R,T)).
% If next event is truffle(X,Y,S), add to Goal_truff as goal(X,Y,S)
trigger([truffle(X, Y, S)|Tail], goals(T,[goal(X,Y,S)|R])):-
    trigger(Tail, goals(T,R)).


%% Task 2 - incorporate_goals ———————————

% incorporate_goals(+Goals, +Beliefs, +Intentions, -Intentions1)
% Goals are in the form goals(Goals_rest,Goals_truff)
% Beliefs in the form beliefs(at(X,Y),stock(T))
% Current Intentions of the agent, in the form intents(Int_sell,Int_pick) where Int_sell, 
% Int_pick are lists of intentions in the form [goal(X,Y,S), Plan]

% Returns the updated Intentions of the agent after inserting the new goals from Goals_rest and 
% Goals_truff into Int_sell and Int_pick, respectively. The new goals are inserted into the 
% existing list in decreasing order of S, the Manhattan distance from the agent's current 
% position break ties.

% No goals, nothing to update.
incorporate_goals(goals([], []), _,intents(Intents_sell,Intents_pick), intents(Intents_sell,Intents_pick)).


% Incorporate the two different kind of goals separately.
incorporate_goals(goals(Goals_rest, Goals_truff),Beliefs,intents(Intents_sell,Intents_pick), intents(Intents_sell_1,Intents_pick_1)):-
	incorporate_goals_part(Goals_rest,Beliefs,Intents_sell,Intents_sell_1),
 	incorporate_goals_part(Goals_truff,Beliefs,Intents_pick,Intents_pick_1).


% incorporate_goals_part(+Goals,+Beliefs,+Intentions,-Intentions1).
% Returns the updated Intentions of the agent after inserting the new goals from Goals

% If Goal G already in Intentions, dont add it.
incorporate_goals_part([G|Rest], Belief, Intentions, Intentions1):-
    member([G,_], Intentions),
    incorporate_goals_part(Rest, Belief, Intentions, Intentions1).
    
% If Goal G not in Intentions, add it.
incorporate_goals_part([G|Rest], Belief, Intentions, Intentions1):-
    not(member(G, Intentions)),
    insert(G, Intentions, Belief, IntentionsU),
    incorporate_goals_part(Rest, Belief, IntentionsU, Intentions1).

% Base case, no goals to add.
incorporate_goals_part([],_,Intentions,Intentions).


% insert(+Goal, +Intentions, +Belief, -IntentionsU).
% The new goal are inserted into the existing list in decreasing order 
% of S, the Manhattan distance from the agent's current position break ties.

% Traverse Intentions and look for correct placement, if found force stop with
% cut.
insert(G, [I|Intentions], Belief, [I|Intentions1]):-
    not(compare_goals(G, I, Belief)), !, 
    insert(G, Intentions, Belief, Intentions1).

% Base case, goal is inserted with empty plan.
insert(G, Intentions, _, [[G, []]|Intentions]).

% compare_goals(+G, +Plan, +Belief).

% If value of goal larger than that in intent, stop.
compare_goals(goal(_, _, Sg), [goal(_, _, Si)|_], _) :-
    Sg > Si.

% If value of goal equal to that in intent, compare using Manhattan distance.
compare_goals(goal(X1, Y1, Sg), [goal(X2, Y2, Si)|_], [at(X, Y)|_]) :-
    Sg =:= Si,
    distance(X, Y, X1, Y1, R1),
    distance(X, Y, X2, Y2, R2),
    R1 < R2.

% distance(+X1, +Y1, +X2, +Y2, -R) calculates the manhattan distance between (X1,Y1) 
% and (X2,Y2) and bounds the result to R.
distance(X1, Y1, X2, Y2, R):- 
	R is abs(X2-X1) + abs(Y2-Y1).


%% Task 3 - get_action ———————————

% get_action(+Beliefs, +Intentions, -Intentions1, -Action)
% takes the agent's Beliefs in the form belief(at(X,Y),stock(T)) 
% and its current Intentions in the form intents(Int_sell,Int_pick), 
% and computes an action to be taken by the agent as well as the updated Intentions. 

% If the list Int_sell of selling intentions is not empty, and its first item 
% [goal(X,Y,S), Plan] satisfies the property that S ≤ T then this intention is selected.

% Otherwise, if the list Int_pick of picking intentions is not empty, then its first item 
% [goal(X,Y,S), Plan] is selected.

% Otherwise, no intention is selected. The agent's Intention remain as they are, and stays in 
% its current location.


% The list Int_sell of selling intentions is not empty, and S ≤ T.
get_action(beliefs(at(X0,Y0),stock(T)), intents([[goal(X,Y,S), P1] | Tail], Int_pick), intents([[goal(X,Y,S), P2] | Tail], Int_pick), Action):-
	T >= S,
	plan(at(X0,Y0), at(X,Y), P1, sell(X,Y), P2, Action).

% Int_pick of picking intentions is not empty.
get_action(beliefs(at(X0,Y0),stock(_)), intents(Int_sell, [[goal(X,Y,S), P1] | Tail]), intents(Int_sell, [[goal(X,Y,S), P2] | Tail]), Action):-
	plan(at(X0,Y0), at(X,Y), P1, pick(X,Y), P2, Action).

% Base case, No intention selected.
get_action(beliefs(at(X,Y),_), Intentions, Intentions, move(X,Y)).


% plan(+Position, +Endposition ,+P1, +Endact, -P2, -Action)
% Creates the plan P2 and first Action based on current Position, Endposition and the last action EndAct (sell/pick).

% The current plan P1 works, use it.
plan(_, _, [H|T], _, T, H):-
    applicable(H).

% Plan not executable, so create new one. 
plan(at(X0,Y0), at(X,Y), [H|_], Endact, Plan, Act):-
    not(applicable(H)),
    createplan(at(X0,Y0), at(X,Y), Endact, Plan, Act).
% Empty plan, create one.
plan(at(X0,Y0), at(X,Y), [], Endact, Plan, Act):-
	createplan(at(X0,Y0), at(X,Y), Endact, Plan, Act).


% createplan(+Position, +Endposition, +EndAction, -Plan, -FirstAction)
% Creates a Plan without the first action which is bound to FirstAction.

% Initialize the partial plan to empty.
createplan(at(X0,Y0), at(X,Y), EndAction, Plan , FirstAction):-
    createplan(at(X0,Y0), at(X,Y), EndAction, Plan, FirstAction, []).


% createplan(+Position, +Endposition, +EndAction, -Plan, -FirstAction, +PartialPlan)
% constructs a plan PartialPlan successively without the first action which is bound to FirstAction.

% Calculate the new move and recursively call with updated position and PartialPlan.
createplan(at(X0,Y0), at(X,Y), EndAction, Plan, FirstAction, PartialPlan):-
    newmove(at(X0,Y0), at(X,Y), Xnew, Ynew),
    createplan(at(Xnew,Ynew), at(X,Y), EndAction, Plan, FirstAction, [move(Xnew, Ynew)|PartialPlan]).

% Base case, we're at the target position.
% Reverse to get right order of actions and remove the first one and Bound to FirstAction.
createplan(at(X,Y), at(X,Y), EndAction, NewPlan, FirstAction, PartialPlan):-
    reverse([EndAction|PartialPlan], Plan),
    getandremovefirst(Plan, NewPlan, FirstAction).

% newmove(+Position, +GoalPosition, -Xnew, -Ynew) 
% Calculates a new move to reach GoalPosition
newmove(at(X0,Y0), at(X,_), Xnew, Y0):-
	X > X0,
    Xnew is X0 + 1.

newmove(at(X0,Y0), at(X,_), Xnew, Y0):-
	X < X0,
    Xnew is X0 - 1.

newmove(at(X0,Y0), at(_,Y), X0, Ynew):-
	Y > Y0,
    Ynew is Y0 + 1.

newmove(at(X0,Y0), at(_,Y), X0, Ynew):-
	Y < Y0,
    Ynew is Y0 - 1.

% getandremovefirst(+Plan, -NewPlan, -FirstAction)
getandremovefirst([H|T], T, H).
    
% reverse(+List, -Reverse).
reverse(L, R) :-
    reverse(L, [], R).

reverse([], R, R).

reverse([H|T], PR, TR) :-
    reverse(T, [H|PR], TR).

%% Task 4 - update_beliefs ———————————
% update_beliefs(+Observation, +Beliefs, -Beliefs1)
% Computes the new beliefs resulting from the agent's observations, as follows:
% at(X,Y) - the agent should believe it is at(X,Y)
% picked(X,Y,S) - stock(T) changes to stock(T1) where T1 is T+S
% sold(X,Y,S) - stock(T) changes to stock(T1) where T1 is T-S

update_beliefs(at(X,Y), beliefs(_,stock(T)), beliefs(at(X,Y),stock(T))).
update_beliefs(picked(X,Y,S), beliefs(_,stock(T)), beliefs(at(X,Y),stock(T1))):-
	T1 is T+S.
update_beliefs(sold(X,Y,S), beliefs(_,stock(T)), beliefs(at(X,Y),stock(T1))):-
	T1 is T-S.


%% Task 5 - update_intentions ———————————
% update_intentions(+Observation, +Intentions, -Intentions1)
% to update the agent's intentions, based on observation. 
% An at(X,Y) observation does not change the agent's intentions. 
% picked() or sold() observation, the agent removes the corresponding intention from its list of intentions

update_intentions(at(_,_), Intentions, Intentions).
update_intentions(picked(_,_,_), intents(Int_sell,[_|Rest]), intents(Int_sell,Rest)).
update_intentions(sold(_,_,_), intents([_|Rest],Int_pick), intents(Rest,Int_pick)).
