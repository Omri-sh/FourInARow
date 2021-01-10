%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Four In a Row with SWI-Prolog.                                %
% Player VS AI using Minimax algorithm with Alpha-Beta pruning. %
% --------------------------------------------------------------%
% Programmer: Omri-sh                                           %
% File Name: FourInARow.pl                                      %
% Description: This is a classic game of four in a row.         %
%    Each player should fill 4 of his pawns consecutively in a  %
%    (Horizontal) row, column, left or right diagonal to win.   %
%    Human pawn is o, AI pawn is x.                             %
% Input: 0-N as the numbers of columns                          %
% Output: prints the board after each move                      %
% Synopsys: enter - play. then, enter the depth size (1-6) of   %     
%   the AI algorithm and the size of the board (NxN), N >= 4.   %
% --Make sure to add the dot (.) in the end of each command.--  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   


%%%%%%%%%%%%%%%%%%%%
%  Game Managnment %
%%%%%%%%%%%%%%%%%%%%

% Starts the game. Depth represents the difficulty of the game.
play:-
    tutorial,
    write('How difficult you want it to be, between 1 (easiest) to 6 (hadrest): '),
    readInt(Depth),
    depth_limits(MinDepth, MaxDepth),
    Depth >= MinDepth,
    Depth =< MaxDepth, % makes sure that the depth is among the configured size.
    nl,
    initialize(Board, Size, Player),
    turn(Depth, Board, Size, Player), !
    ;
    nl, write('*** Level is outside limits ***'), nl, fail. % fail in case of wrong depth

% Depth of AI algorithm
depth_limits(1, 8).

% Basic tutorial for the player
tutorial:-
    write('This is a classic game of four in a row.'),nl,
    write('There are 6 levels of difficulties between 1-6.'),nl,
    write('Level 1 is the easiest and level 6 is the hadrest.'),nl,
    write('Make sure to add a dot (.) in the end of each command!'),nl.

% Initialize the board and starts the game
initialize(Board, Size, Player):-
    write('Enter the board size as a single number (at least 4): '),
    readInt(Size),
    Size >= 4, % makes sure that the size is bigger than 4
    createBoard(Size, Board),
    showBoard(Board, Size),
    startingPlayer(Player)
    ;
    nl, write('*** Board size should be at least 4 ***'), nl, % let the user re-enter the board size
    initialize(Board, Size, Player).

% Create a board as a list of lists. Each list represents a row in the board.
% 0 represents empty cell, -1 represents AI pawn, 1 represents human pawn
createBoard(Size, Board) :-
    length(Row, Size),
    maplist(=(0), Row),  
    length(Board, Size),
    maplist(=(Row), Board).  

% Decided which player (human or computer) should start first randomly
startingPlayer(computer):-  
    random_between(1, 2, J), 
    J = 1, !.
startingPlayer(human).

% Handles the user and the computer moves
turn(Depth, Board, Size, human):-
    isFull(Board),write('Draw - The board is full.'),nl,!
    ;   
    ( 
      nl, write('Your Turn! '), nl,
      repeat, % for wrong pick os columns
      MaxCol is Size -1,
      write('Enter a column number (0-'), write(MaxCol), write('): '),
      readInt(ColNum),nl, % reads the column entered by the human
      (   
      	  isColNotFull(Board, ColNum) % checks that the column is not full and exists
     	  ;   
          nl, write('***Invalid move: column is full or not exist***'), nl, 
          fail, ! % In case of full or not exists column, let the user try again. Cut used to stop when the input is ok.
      ),
      putInCol(Board, ColNum, 1, NewBoard),
      showBoard(NewBoard, Size),
      (
     	isItAWin(NewBoard, 1), write('You Won!'), nl, ! % check if human wins and stops there
     	;   
     	turn(Depth, NewBoard, Size, computer), ! % change turns
      )
    ).

turn(Depth, Board, Size, computer):-
    isFull(Board),write('Draw - The board is full.'),nl,!
    ;   
    (   
      nl, write('My Turn! '), nl,nl,
      alphabeta(Depth, Board, -32767, 32767, NewBoard, _), % runs the AI algorithm
      showBoard(NewBoard, Size),
      (
     	isItAWin(NewBoard, -1), write('You lost!'), nl, !  % check if AI wins and stops there
     	;   
     	turn(Depth, NewBoard, Size, human), ! % change turns
      )
    ).

% Checks if the board is full by checking if there is any empty cells in the first row
isFull([FirstRow | _]):-
    not(member(0, FirstRow)).

% Checks if in a given column is full by checking the first row and the relevant cell.
isColNotFull([FirstRow | _], ColNum):-
    nth0(ColNum,FirstRow,X),
    X =:= 0.

% Put the pawn in the column, in the lowest possible place
putInCol(Board, ColNum, Pawn, NewBoard):-
    reverse(Board, ReversedBoard), % reverse the board, so the lowest row will be the hightest and vice verse
    putInFirstEmptyCell(ReversedBoard, Pawn, ColNum, NewReversedBoard),
    reverse(NewReversedBoard, NewBoard). % reverse the board again

% put the pawn in the lowest possible cell
% Takes into account that the board is flipped so, we put the first empty cell in this board is the lowest possible empty cell is the original board
putInFirstEmptyCell([Row | Tail], Pawn, ColNum, [NewRow | Tail]):-
    nth0(ColNum,Row,X), % picks the right column in the row
    X =:= 0,
    replace(Row, ColNum, Pawn, NewRow),!. % if the cell is empty then replace

putInFirstEmptyCell([Row | Tail], Pawn, ColNum, [Row | ReturnedBoard]):-
    nth0(ColNum,Row,X), % picks the right column in the row
    X =\= 0,
    putInFirstEmptyCell(Tail, Pawn, ColNum, ReturnedBoard). %if the cell isn't empty then go to the next down row
    
% Checks if there is a win in the board    
isItAWin(Board, Pawn):-
    checkWinInRows(Board, Pawn)
    ;   
    checkWinInCols(Board, Pawn)
    ;   
    checkLeftDiagonal(Board, Pawn)
    ;   
    checkRightDiagonal(Board, Pawn).
    
% Checks if there is a row with same 4 consecutive pawns
checkWinInRows(Board, Pawn):- 
    append(_, [Row | _], Board),  % find a row in the board that consisnt of same 4 consecutive pawns in it
    append(_, [Pawn, Pawn, Pawn, Pawn | _], Row).

% Checks if there is a column with same 4 consecutive pawns
checkWinInCols(Board,Pawn):- 
    append(_, [Row1, Row2, Row3, Row4 | _], Board), % find 4 rows that each of them have the same pawn in the same position in the list
    append(C1, [Pawn | _], Row1),
    append(C2, [Pawn | _], Row2),
    append(C3, [Pawn | _], Row3),
    append(C4, [Pawn | _], Row4),
    length(C1,M), length(C2,M), length(C3,M), length(C4,M).

% Checks if there is a left diagonal with same 4 consecutive pawns
checkLeftDiagonal(Board,Pawn):- 
    append(_, [Row1, Row2, Row3, Row4 | _], Board),  % find 4 rows that each of them have the same pawn in the position reduced by 1 in each pawn
    append(C1, [Pawn | _], Row1), 
    append(C2, [Pawn | _], Row2),
    append(C3, [Pawn | _], Row3),
    append(C4, [Pawn | _], Row4),
    length(C1,M1), length(C2,M2), length(C3,M3), length(C4,M4),
    M2 is M1-1, M3 is M2-1, M4 is M3-1.

% Checks if there is a right diagonal with same 4 consecutive pawns
checkRightDiagonal(Board,Pawn):- 
    append(_, [Row1, Row2, Row3, Row4 | _], Board), % find 4 rows that each of them have the same pawn in the position increased by 1 in each pawn
    append(C1, [Pawn | _], Row1), 
    append(C2, [Pawn | _], Row2),
    append(C3, [Pawn | _], Row3),
    append(C4, [Pawn | _], Row4),
    length(C1,M1), length(C2,M2), length(C3,M3), length(C4,M4),
    M2 is M1+1, M3 is M2+1, M4 is M3+1.

%%%%%%%%%%%%%%%%%%%%
%        AI        %
%%%%%%%%%%%%%%%%%%%%

% Minimax with alpha-beta algorithm as given to us by Roy Rachmany
alphabeta(Depth, Board, _, _, [], Value):-
    isItAWin(Board, 1), % in case of no more moves and its a lost, return the lost value minus the depth
    Value is -1000 - Depth, !.
alphabeta(Depth, Board, Alpha, Beta, BestMove, Value):-
    Depth > 0,
    recommended_moves(Board, Moves), % get all recommanded moves according to current board
    Moves = [_|_], !,
    NewDepth is Depth - 1,
    Alpha1 is -Beta,
    Beta1 is -Alpha,
    bestmove(Moves, Board, NewDepth, Alpha1, Beta1, [], BestMove, Value).
alphabeta(Depth, Board, _, _, [], Value):-
    value(Board, Depth, Value). % Depth is 0, or no moves left, so valuate the board (heuristic function)

bestmove([Move|Moves], Posn, Depth, Alpha, Beta, Move0, Move1, Value1):-
    move(Posn, Move, _), !,
    swapBoard(Move, SwappedBoard),
    alphabeta(Depth, SwappedBoard, Alpha, Beta, _, MinusValue),
    Value is -MinusValue,
    cutoff(Move, Value, Depth, Alpha, Beta, Moves, Posn, Move0, Move1, Value1).
bestmove([], _, _, Alpha, _, Move, Move, Alpha).

cutoff(_, Value, Depth, Alpha, Beta, Moves, Position, Move0, Move1, Value1):-
    Value =< Alpha, !,
    bestmove(Moves, Position, Depth, Alpha, Beta, Move0, Move1, Value1).
cutoff(Move, Value, Depth, _, Beta, Moves, Position, _, Move1, Value1):-
    Value < Beta, !,
    bestmove(Moves, Position, Depth, Value, Beta, Move, Move1, Value1).
cutoff(Move, Value, _, _, _, _, _, _, Move, Value).

% Calculate all the recommended moves according to current board
recommended_moves(Board, [Move]):-
    % If there is a winning move, recommend it, and ignore all other moves
    move(Board, Move, _),
    isItAWin(Move, -1), !.
recommended_moves(Board, [Move]):-
    % If the other player has a potential winning move, recommend the
    %   move which would prevent it, and ignore all other moves
    swapBoard(Board, SwappedBoard), % swaps the board, so pawns is not need to be change, By that we can check the swapped board which represent the board of the human in the eyes of the AI
    move(SwappedBoard, SwappedMove, SwappedHumanMove), % get the move with opponent pawn and the move with AI pawn
    isItAWin(SwappedMove, -1), !, % if the human wins by this move, then the swappedHumanMove is a blocking move
    swapBoard(SwappedHumanMove, Move).
recommended_moves(Board, Moves):-
    % Otherwise, recommend all valid moves
    findall(Move, move(Board, Move, _), Moves). 

% Swap the board by changing -1 to 1 and vice versa       
swapBoard([], []).
swapBoard([Row | Tail], [RowRes | TailRes]):-
    swapRow(Row, RowRes),
    swapBoard(Tail, TailRes).

% Swap each row of the board by changing -1 to 1 and vice versa
swapRow([] ,[]).
swapRow([X | Xs], [Y | Ys]):-
    Y is -X,
    swapRow(Xs, Ys).

% returns the move.
% Move - move of AI to the column
% HumanMove - move of human to the same column
move(Board, Move, HumanMove):-
    getAllValidCols(Board,ColNum),
    ColNum > -1,
    putInCol(Board, ColNum, -1, Move),
    putInCol(Board, ColNum, 1, HumanMove).

% get all the valid columns in the board which is not full. Returns -1, if all columns has been covered
getAllValidCols([FirstRow | _], ColNum):-
    nth0(ColNum,FirstRow,0)
    ;   
    ColNum is -1.

% The heuristic function. Checks if its a winning board or a losing board.
% Winning Board for AI -The score is 1000 + the depth of the AI algorithm, So early moves will get higher scores
% Losing Board for AI -The score is -1000 - the depth of the AI algorithm, So latter moves will get higher scores
% Not a losing or a winning board - randomly selects a value between 0 and 100.
value(Board, Depth, Value):-
    isItAWin(Board, -1), % winning board
    Value is 1000 + Depth. 
value(Board, Depth, Value):-
    isItAWin(Board, 1), % losing board
    Value is -1000 - Depth.
value(_, _, Value):- % not a losing or a winning board
    random_between(0, 100, Value).

%%%%%%%%%%%%%%%%%%%%
%      Utils       %
%%%%%%%%%%%%%%%%%%%%

% Reads an integer input
readInt(X):-
    read(X),
    integer(X),!. % Cut used to stop when an integer is read
readInt(X):-
    writeln('Error'),
    readInt(X).

% Replace element in list by index
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
    I > -1,
    NI is I-1,
    replace(T, NI, X, R), !.
replace(L, _, _, L).

%%%%%%%%%%%%%%%%%%%%
%        UI        %
%%%%%%%%%%%%%%%%%%%%

% Prints the board
showBoard([], _).
showBoard(Board, Size):-
    write('|'),
    printHeader(0, Size), nl,
    printRow(Board), nl,
    write('-----------------------------'), nl.

% Prints each row
printRow([]).
printRow([Row | Tail]):-
    ints_chars(Row, CharsRow),
    printCell(CharsRow),
    printRow(Tail).

% Prints each cell of the row
printCell([]):-
    write('|'),nl.
printCell([Cell|Tails]):-
    write('|'),
	write(Cell),
    printCell(Tails).

% prints the column numbers
printHeader(I,N):-
    I =:= N, !
    ;   
    write(I), 
    write('|'),
    I1 is I + 1,
    printHeader(I1, N).
    
% Replace numberic representation of the board to a character representation
ints_chars([], []).
ints_chars([0|Xs],  ['-'|Ys]):-
    ints_chars(Xs, Ys).
ints_chars([1|Xs],  ['o'|Ys]):-
    ints_chars(Xs, Ys).
ints_chars([-1|Xs], ['x'|Ys]):-
    ints_chars(Xs, Ys).
