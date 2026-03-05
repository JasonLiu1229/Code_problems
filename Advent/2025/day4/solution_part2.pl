% ------------------------------------------------------------
% read_lines(+File, -Lines)
%
% Reads a file and returns its contents as a list of strings,
% one string per line.
%
% Example file:
%   .@..
%   @@..
%
% Result:
%   Lines = [".@..","@@.."]
% ------------------------------------------------------------
read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r", Lines).


% ------------------------------------------------------------
% convert_to_char_grid(+GridStr, -GridChr)
%
% Converts a grid represented as a list of strings into
% a grid represented as a list of lists of characters.
%
% Example:
%   ["@..","..@"]  ->
%   [['@','.','.'],['.','.','@']]
%
% maplist applies string_chars to every row.
% ------------------------------------------------------------
convert_to_char_grid(GridStr, GridChr) :- 
  maplist(string_chars, GridStr, GridChr).


% ------------------------------------------------------------
% get_cell(+Grid, +R, +C, -Char)
%
% Retrieves the character at position (R,C) in the grid.
%
% Grid is a list of rows, each row is a list of characters.
%
% Example:
%   get_cell(Grid,2,3,X)
%   returns the character at row 2 column 3.
% ------------------------------------------------------------
get_cell(Grid, R, C, Char) :-
  nth1(R, Grid, Row),
  nth1(C, Row, Char).


% ------------------------------------------------------------
% in_bounds(+Grid, +R, +C)
%
% Checks whether coordinates (R,C) are inside the grid.
%
% Conditions:
% - R and C must be positive
% - R must not exceed the number of rows
% - C must not exceed the number of columns
% ------------------------------------------------------------
in_bounds(Grid, R, C) :-     
  R > 0,
  C > 0,
  length(Grid, RowCount),
  R =< RowCount,
  nth1(R, Grid, Row),
  length(Row, ColCount),
  C =< ColCount.


% ------------------------------------------------------------
% count_neighbors(+Grid, +R, +C, -Count)
%
% Counts how many neighboring cells around (R,C) contain '@'.
%
% Neighbors include the 8 surrounding positions:
%   (-1,-1) (-1,0) (-1,1)
%   ( 0,-1)   X    (0,1)
%   ( 1,-1) ( 1,0) (1,1)
%
% For every neighbor that contains '@', a 1 is collected.
% The total number of collected items is the neighbor count.
% ------------------------------------------------------------
count_neighbors(Grid, R, C, Count) :-
  findall(1, % everytime it succeeds we collect a 1
    (
      member(DR, [-1,0,1]),
      member(DC, [-1,0,1]),
      \+ (DR = 0, DC = 0),  
      R2 is R + DR,
      C2 is C + DC,
      in_bounds(Grid, R2, C2),
      get_cell(Grid, R2, C2, '@')
    ),
  L),
  length(L, Count).


% ------------------------------------------------------------
% valid_roll(+Grid, +R, +C)
%
% A cell is considered a valid roll if:
% 1. The position is inside the grid
% 2. The cell contains '@'
% 3. It has fewer than 4 neighboring '@' cells
% ------------------------------------------------------------
valid_roll(Grid, R, C) :- 
  in_bounds(Grid, R, C),
  get_cell(Grid, R, C, '@'),
  count_neighbors(Grid, R, C, N),
  N < 4.


% ------------------------------------------------------------
% find_rolls(+Grid, -Positions, -Amount)
%
% Finds all positions (R,C) in the grid that satisfy valid_roll.
%
% Steps:
% 1. Iterate over all possible grid coordinates.
% 2. Keep only those where valid_roll succeeds.
% 3. Collect them as pairs (R,C).
% 4. Amount is the number of such positions.
% ------------------------------------------------------------
find_rolls(Grid, L, Amt) :- 
 length(Grid, RowCount),
  nth1(1, Grid, FirstRow),
  string_length(FirstRow, ColCount),

  findall((R, C),
    (
      between(1, RowCount, R),
      between(1, ColCount, C),
      valid_roll(Grid, R, C)
    ),
    L
  ),

  length(L, Amt).


% ------------------------------------------------------------
% replace_nth1(+Index, +List, +Element, -NewList)
%
% Replaces the element at position Index in List
% with Element and returns the modified list.
%
% Recursive structure:
% - Base case: replace first element
% - Recursive case: keep head and recurse on tail
% ------------------------------------------------------------
replace_nth1(1, [_|T], X, [X|T]) :- !. % base case
replace_nth1(N, [H|T], X, [H|T2]) :- % Keep moving tail till we have base case
  N > 1,
  N1 is N - 1,
  replace_nth1(N1, T, X, T2).


% ------------------------------------------------------------
% set_cell(+Grid, +R, +C, +NewChar, -NewGrid)
%
% Replaces the cell at position (R,C) in the grid with NewChar.
%
% Steps:
% 1. Extract the row R
% 2. Replace column C in that row
% 3. Replace the modified row back into the grid
% ------------------------------------------------------------
set_cell(Grid, R, C, NewChar, NewGrid) :-
  nth1(R, Grid, Row),
  replace_nth1(C, Row, NewChar, NewRow), % replace in that row
  replace_nth1(R, Grid, NewRow, NewGrid). % replace in the grid itself


% ------------------------------------------------------------
% remove_all(+Grid, +Positions, -NewGrid)
%
% Removes all rolls at the given positions.
% Each roll is replaced by '.'.
%
% The predicate processes the list recursively.
% ------------------------------------------------------------
remove_all(Grid, [], Grid). % base case 
remove_all(Grid, [(R,C)|Ps], Out) :- 
  set_cell(Grid, R, C, '.', Grid1),
  remove_all(Grid1, Ps, Out).


% ------------------------------------------------------------
% remove_loop(+Grid, -TotalRemoved)
%
% Repeatedly removes rolls until no more valid rolls remain.
%
% Steps:
% 1. Find all removable rolls.
% 2. If none exist, stop.
% 3. Otherwise remove them and repeat.
% 4. Keep track of how many rolls were removed in total.
% ------------------------------------------------------------
remove_loop(Grid, 0) :- find_rolls(Grid, _, 0).
remove_loop(Grid, TotalRemoved) :-
  find_rolls(Grid, Positions, K),
  K > 0,
  remove_all(Grid, Positions, Grid2),
  remove_loop(Grid2, Rest),
  TotalRemoved is K + Rest.


% ------------------------------------------------------------
% solve(+File, -Answer)
%
% Main solving predicate.
%
% Steps:
% 1. Read the grid from the file.
% 2. Convert string rows into character lists.
% 3. Repeatedly remove invalid rolls.
% 4. Return the total number removed.
% ------------------------------------------------------------
solve(File, Answer) :- 
  read_lines(File, Grid),
  convert_to_char_grid(Grid, GridChr),
  remove_loop(GridChr, Answer).


% ------------------------------------------------------------
% main
%
% Entry point of the program.
% Reads the input file, solves the problem,
% and prints the result.
% ------------------------------------------------------------
main:-
  solve('input.txt', Answer),
  writeln(Answer).
