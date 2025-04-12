%%%======================================================================%%%
% Author: Xin Shen
% StudentID: 1266811
% Subject: COMP90048 Declarative Programming, 2024 Semester 1
% Purpose: Reads a math puzzle stored in a nested-list, solves the puzzle
%          according to required constraints, and return a filled-in puzzle
%%%======================================================================%%%
% Description:
%-----------------------------------------%
% Math Puzzle:
% The puzzle is in the form of a sqare grid of squares with the following property:
% 1. Each row and column contains no repeated integers from 1 to 9
% 2. The top-left to bottom-right diagnoal contains identical integers
% 3. Each row/column has a header at the start of it, where the header is 
% either the sum or product of the row/column
%-----------------------------------------%
% Puzzle Solver:
% This program is a solver of this puzzle. It will read in a nested-list
% representing grids of a puzzle, with completed headings, and partially filled
% or empty body. The program will use constraint logic programming to solve
% the puzzle by finding a combination of integers that when filled in to the
% empty body, our puzzle will satisfiy all the above constraints.
%%%======================================================================%%%

:- use_module(library(clpfd)).
%======================================================%
% predicates used from library clpfd
% ?X #= ?Y: the arithmetic expression of X equals Y
% +Vars ins +Domain: checks if list Vars are elements of Domain
% all_distinct(+Vars): true if and only if Vars are pairwise distinct
% transpose(+Matrix, ?Transpose): transpose Matrix, turn rows to columns,
% returns as Transpose
% label(+Vars): systematically trying out values for the finite domain 
% variables Vars until all of them are ground. In this program we use it
% to find exact integers that solves the puzzle.
%======================================================%
:- use_module(library(apply)).
%======================================================%
% predicate used from library apply
% maplist(:Goal, ?List1)
% maplist(:Goal, ?List1, ?List2)
% Applies predicate Goal on each element of List
%======================================================%

%======================================================%
%% prod_list(+List, -Product)
%% prod_list(+List, +Accumulator, -Prodct)
% 
% Reads a List and calculate the product of all elements
% in the list, giving Product. 
% prod_list/2 calls prod_list/3 by passing List and Product, 
% then adds an extra accumulator,initially set as 1. 
% The program will recursively multiplies each element of the list 
% with the accumulator, results in Product.
%======================================================%
prod_list(List,Product):-
    prod_list(List,1,Product).
prod_list([],Product,Product).
prod_list([H|T],Product0,Product):-
    Product1 #= Product0 * H,
    prod_list(T,Product1,Product).

%======================================================%
%% sum_list(+List, -Sum)
%% sum_list(+List, +Accumulator, -Sum)
%
% Reads a List and calculate the sum of all elements
% in the list, giving Sum. 
% sum_list/2 calls sum_list/3 by passing List and Sum, 
% then adds an extra accumulator,initially set as 0. 
% The program will recursively adds each element of the list 
% to the accumulator, results in Sum.
%======================================================%
sum_list(List,Sum):-
    sum_list(List,0,Sum).
sum_list([],Sum,Sum).
sum_list([H|T],Sum0,Sum):-
    Sum1 #= Sum0 + H,
    sum_list(T,Sum1,Sum).

%======================================================%
%% valid_head(+List)
%
% The constraint for header is that is has to be either the 
% sum or the product of its body. Therefore we checks if a head 
% is valid by calling the two predicates: sum_list/2 and prod_list/2.
% valid_head is true if either of the sum_list or prod_list
% holds true.
%======================================================%
valid_head([H|T]):-
    sum_list(T,H);
    prod_list(T,H).

%======================================================%
%% get_diagonal(+Matrix,+Diagonal,+Layer)
%
% Reads a n x n Matrix, for each i^th row of the matrix, 
% it calls get_nth/3 to extract the i^th element of that row, 
% and the resulted list is Diagonal. This will extract the 
% top-left to bottom-right diagonal of the matrix.
% The index of row is saved as Layer, initially set to 1.
%======================================================%
get_diagonal([],[],_).
get_diagonal([FirstR|Tail],[D1|DTail],I):-
    get_nth(FirstR, D1, I),
    I1 #= I + 1,
    get_diagonal(Tail, DTail, I1).

%======================================================%
%% get_nth(+List,+Diagonal,+N)
%
% Reads in a List, extract the N th element of the row.
% Given we want the i^th element of the list, the predicate
% will recursively removes the beginning of the list,
% one-by-one until we reach the index we want, then return
% as Diagonal.
%======================================================%
get_nth([Diagonal|_],Diagonal,1).
get_nth([_|Tail],Diagonal,N):-
    N > 1,
    N1 #= N - 1,
    get_nth(Tail,Diagonal, N1).
    
%======================================================% 
%% all_same(+Head, +List)
%
% Reads in List and checks if all elements in the list
% are the same. It will recursively checks if the n^th
% and n+1^th element are the same.
%======================================================%
all_same(_,[]).
all_same(Head, [Head|Tail]) :-
	all_same(Head,Tail).

%======================================================%
%% body_row(+Row,-Tail)
%
% Reads Row, removes the first element of the row, 
% gives Tail.
%======================================================%
body_row([_|Tail],Tail).

%======================================================%
%% puzzle_solution(+Puzzle)
%
% This is the main body of the puzzle solver, it will 
% checks all constraints of the puzzle with assistance
% of all predicates above.
% After reading and checking we have a sqaure matrix, we
% decompose the matrix into:
% - Transpose to Column to check for column-wise constraints
% - Remove the first row and column, gives BodyRows and BodyCols
% - Remove the header in each row and column, gives BodyRMatrix and BodyCMatrix
% - Flattern all elements (other than heading) to Elements
% Afterwards, it checks the constraints 1 to 3 listed in the Math Puzzle section.
%======================================================%
puzzle_solution(Puzzle):-
    
    % Decompose Puzzle
    maplist(same_length(Puzzle), Puzzle),
    transpose(Puzzle,Column),
    Puzzle = [_HeadRow|BodyRows],
    Column = [_HeadCol|BodyCols],
    maplist(body_row, BodyRows, BodyRMatrix),
    maplist(body_row, BodyCols, BodyCMatrix),
    flatten(BodyRMatrix, Elements),
    
    % Checks contraint 1: row/column has distinct integers from 1 to 9
    Elements ins 1..9,   
    maplist(all_distinct, BodyRMatrix),
    maplist(all_distinct, BodyCMatrix),
    
    % Checks constraint 2: diagonal has identical values
    get_diagonal(BodyRMatrix, Diagonal, 1),
    all_same(_, Diagonal),
    
    % Checks constraint 3: headers are either the sum or the product of body
    maplist(valid_head, BodyRows),
    maplist(valid_head,BodyCols),

    % Find the exact values satisfying the constrants
    label(Elements).


    
    
    