% Ludwig Tranheden
% z5129849
% Assignment 1
% 24/3-17

%% Task 1 - Weird sum ———————————

% weird_sum(+List, -Result) takes a List of numbers, and computes the sum
% of the squares of the numbers in the list that are greater than or equal to 5, 
% minus the sum of the absolute values of the numbers that are less than or equal to % 2.

% Base case, If the List is empty Result is 0.
weird_sum([],0).

% If Head of list is greater than 5, keep traversing list recursively and then add
% its square to Result.
weird_sum([Head | Tail], Result):- 
    Head >= 5,						
    weird_sum(Tail, Result2),
    Result is Result2 + Head**2.

% Else if Head of the list is less than 2, keep traversing list recursively and then % subtract its absolute value to Result.
weird_sum([Head | Tail], Result):-
    Head =< 2,
    weird_sum(Tail, Result2),
    Result is Result2 - abs(Head).

% Else head of list is less than 5 but greater than 2, keep traversing recursively.
weird_sum([Head|Tail],Result):-
   	weird_sum(Tail,Result).

% TEST cases.
% weird_sum([3,6,2,-1], Result) -> Result = 6x6-2-1 = 33
% weird_sum([], Result) -> Result = 0
% weird_sum([6,7,8,9], Result) -> Result = 230
% weird_sum([3,4], Result) -> Result = 0
% weird_sum([-1,2,-1], Result) -> Result = -1-2-1 = -4




% Task 2 - Family relationships ———————————

% same_name(+Person1,+Person2) succeeds if it can be deduced 
% from the facts in the database that Person1 and Person2 
% will have the same family name. 
% 
% We assume that each person will have the same family
% name as their father, but that married women retain their original birth name.

% Same person, same name.  
same_name(Person,Person).

% Search for a common ancestor.
same_name(Person1,Person2) :-
    findnext(Person1,Parent),
    same_name(Parent,Person2).
same_name(Person1,Person2) :-
    findnext(Person2,Parent),
    same_name(Parent,Person1).

% Predicate findnext(+Person, -Parent) Finds the next male ancestor or female if and % only if a male does not exist and bounds it to Parent.
% The cut prevents it from finding more unvalid solutions.
findnext(Person,Parent):-
    parent(Parent,Person),
    male(Parent),!.
findnext(Person,Parent):-
    parent(Parent,Person).

% TEST cases
% same_name(pat, brian). -> false.
% same_name(jenny, jim). -> true.
% same_name(sofie,pat). -> false
% same_name(erika,jim). -> True




% Task 3 - Log table ———————————

% log_table(+NumberList, -ResultList) bounds ResultList to the list 
% of pairs consisting of a number and its log, for each number in NumberList. The
% numbers are expected to be defined by under the log-function, i.e be strictly
% positive.

% Base case. Empty table, empty log-table.
log_table([], []).

% Recursively traverses NumberList until its end, then successively adds the number
% and the log of the number to the beginning of ResultList. 
log_table([Head | Tail], ResultList):-
    log_table(Tail,ResultList2),
    LogHead is log(Head),
    ResultList = [[Head,LogHead]|ResultList2].

% TEST cases.
% log_table([1,3.7,5], ResultList) -> ResultList = 
% [[1, 0.0], [3.7, 1.308332819650179], [5, 1.6094379124341003]]
% log_table([], ResultList) -> ResultList = 
% []
% log_table([0.1, 10000], ResultList) -> ResultList = 
% [[0.1, -2.3025850929940455], [10000, 9.210340371976184]]




% Task 4 - Parity runs ———————————

% paruns(+List, -RunList) converts 
% List into the corresponding list of parity runs which is
% bounded to RunList

% Base case. Empty List, Empty list of parity runs.
paruns([],[]).

% Find sequence of odd or even numbers. Then keep traversing recursively.
paruns([Head|Tail], [RH|RT]):-
    findconsecutive(Head,Tail,Next,RH),
    paruns(Next,RT).


% findconsecutive(+Head,+Tail,-Next,-Result) 
% Will find all consecutive even or odd numbers in [Head | Tail].
% The sequence is bounded to Result.
% Next is where the sequence stopped in the original list.

% Base case, if we reach end of list while in middle of sequence.	  	
findconsecutive(X,[],[],[X]).

% If the current number and the next number is not both odd or even
% we succed and return back the recursive calls.
findconsecutive(CurrentHead,[NextHead|Tail],[NextHead|Tail],[CurrentHead]):-
	(CurrentHead mod 2) =\= (NextHead mod 2).

% If the current and the next number is both odd or even we append the current number
% and keep traversing the list recursively.
findconsecutive(CurrentHead,[NextHead|Tail],Next,[CurrentHead|CurrentTail]):-
    findconsecutive(NextHead,Tail,Next,CurrentTail).
    

% TEST cases.
% paruns([8,0,4,3,7,2,-1,9,9], RunList). -> RunList = 
% [[8, 0, 4], [3, 7], [2], [-1, 9, 9]]
% paruns([], RunList). -> RunList = []
% paruns([1], RunList). -> RunList = [[1]]
% paruns([1,3,5,7], RunList). -> RunList = [[1,3,5,7]]
% paruns([1,2,3,4], RunList). -> RunList = [[1], [2], [3], [4]]




% Task 5 - Heap property ———————————

% is_heap(+Tree) which returns true if Tree satisfies 
% the heap property, and false otherwise.

% Base case. Empty tree satisfies heap-property.
is_heap(empty).

% DFS to control heap-property. 
is_heap(tree(L,Value,R)):-
    is_heap(L),
    is_heap(R),
    node_heap(L,Value),
    node_heap(R,Value).

% node_heap(+Tree,+Value)
% True if the node rooted at Tree satisfies the heap property.
% The node satisfies heap property if the node is empty
node_heap(empty,Value).

% The node satisfies heap property if the Value of the parent is less than the 
% node.
node_heap(tree(L,Value,R),ParentValue):-
    ParentValue < Value.

% TEST cases.
% is_heap(tree(tree(tree(empty,4,empty),
%         3,tree(empty,5,empty)),6,tree(tree(empty,9,empty),7,empty))). -> False
% is_heap(tree(empty,3,tree(tree(empty,8,empty),5,tree(empty,7,empty)))). -> True
% is_heap(tree(tree(empty,8,empty),5,tree(empty,6,empty))). -> True
% is_heap(tree(tree(empty,8,empty),10,tree(empty,6,empty))). -> False
% is_heap(tree(empty,0,empty)). -> True
% is_heap(empty). -> True