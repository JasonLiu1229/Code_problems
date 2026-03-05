% ------------------------------------------------------------
% read_lines(+File, -Lines)
%
% Reads the content of a file and splits it into lines.
%
% Steps:
% 1. read_file_to_string reads the entire file into a single string.
% 2. split_string splits that string at newline characters.
%
% "\n" is used as separator.
% "\r" is removed if present (Windows line endings).
%
% Example file:
%   .@.
%   @@.
%
% Result:
%   Lines = [".@.","@@."]
% ------------------------------------------------------------
read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r", Lines).


% ------------------------------------------------------------
% get_cell(+Grid, +R, +C, -Char)
%
% Retrieves the character at row R and column C of the grid.
%
% Grid is a list of strings (each string is one row).
%
% Steps:
% 1. nth1 gets the R-th row from Grid.
% 2. string_chars converts the row string into a list of characters.
% 3. nth1 gets the C-th character from that list.
%
% Example:
%   Grid = ["..@", "@@."]
%   get_cell(Grid,2,1,X) -> X='@'
% ------------------------------------------------------------
get_cell(Grid, R, C, Char) :-
  nth1(R, Grid, Row),
  string_chars(Row, Chars),
  nth1(C, Chars, Char).


% ------------------------------------------------------------
% in_bounds(+Grid, +R, +C)
%
% Checks whether position (R,C) lies inside the grid.
%
% Conditions:
% 1. R and C must be positive.
% 2. R must not exceed number of rows.
% 3. C must not exceed number of columns in that row.
%
% This prevents accessing cells outside the grid.
% ------------------------------------------------------------
in_bounds(Grid, R, C) :-     
  R > 0,
  C > 0,
  length(Grid, RowCount),
  R =< RowCount,
  nth1(R, Grid, Row),
  string_length(Row, ColCount),
  C =< ColCount.


% ------------------------------------------------------------
% count_neighbors(+Grid, +R, +C, -Count)
%
% Counts how many neighboring cells around position (R,C)
% contain the character '@'.
%
% The neighbors considered are the 8 surrounding cells:
%
%   (-1,-1) (-1,0) (-1,1)
%   ( 0,-1)  cell   (0,1)
%   ( 1,-1) ( 1,0) (1,1)
%
% Steps:
% 1. Try all combinations of row offsets DR and column offsets DC
%    from [-1,0,1].
% 2. Skip the case DR=0 and DC=0 (the cell itself).
% 3. Compute neighbor coordinates R2,C2.
% 4. Check if they are inside the grid.
% 5. Check if the cell contains '@'.
% 6. For every success, collect a 1.
% 7. The length of that list is the number of neighbors.
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
% solve(+File, -Answer)
%
% Solves the main problem:
% Count how many '@' cells have fewer than 4 neighboring '@' cells.
%
% Steps:
% 1. Read the grid from the file.
% 2. Determine the number of rows and columns.
% 3. Iterate over every cell position (R,C).
% 4. If the cell contains '@':
%       - count its neighbors
%       - check if the count is < 4
% 5. For every valid cell, collect a 1.
% 6. The total number of such cells is the length of the list.
% ------------------------------------------------------------
solve(File, Answer) :- 
  read_lines(File, Grid),
  length(Grid, RowCount),
  nth1(1, Grid, FirstRow),
  string_length(FirstRow, ColCount),

  findall(1,
    (
      between(1, RowCount, R),
      between(1, ColCount, C),
      get_cell(Grid, R, C, '@'),
      count_neighbors(Grid, R, C, N),
      N < 4
    ),
    L
  ),

  length(L, Answer).


% ------------------------------------------------------------
% main
%
% Entry point of the program.
% Reads "input.txt", solves the problem,
% and prints the result.
% ------------------------------------------------------------
main:-
  solve('input.txt', Answer),
  writeln(Answer).
