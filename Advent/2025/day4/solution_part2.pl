read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r", Lines).

convert_to_char_grid(GridStr, GridChr) :- 
  maplist(string_chars, GridStr, GridChr).

get_cell(Grid, R, C, Char) :-
  nth1(R, Grid, Row),
  nth1(C, Row, Char).

in_bounds(Grid, R, C) :-     
  R > 0,
  C > 0,
  length(Grid, RowCount),
  R =< RowCount,
  nth1(R, Grid, Row),
  length(Row, ColCount),
  C =< ColCount.

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

valid_roll(Grid, R, C) :- 
  in_bounds(Grid, R, C),
  get_cell(Grid, R, C, '@'),
  count_neighbors(Grid, R, C, N),
  N < 4.

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

replace_nth1(1, [_|T], X, [X|T]) :- !. % base case
replace_nth1(N, [H|T], X, [H|T2]) :- % Keep moving tail till we have base case
  N > 1,
  N1 is N - 1,
  replace_nth1(N1, T, X, T2).

set_cell(Grid, R, C, NewChar, NewGrid) :-
  nth1(R, Grid, Row),
  replace_nth1(C, Row, NewChar, NewRow), % replace in that row
  replace_nth1(R, Grid, NewRow, NewGrid). % replace in the grid itself

remove_all(Grid, [], Grid). % base case 
remove_all(Grid, [(R,C)|Ps], Out) :- 
  set_cell(Grid, R, C, '.', Grid1),
  remove_all(Grid1, Ps, Out).

remove_loop(Grid, 0) :- find_rolls(Grid, _, 0).
remove_loop(Grid, TotalRemoved) :-
  find_rolls(Grid, Positions, K),
  K > 0,
  remove_all(Grid, Positions, Grid2),
  remove_loop(Grid2, Rest),
  TotalRemoved is K + Rest.

solve(File, Answer) :- 
  read_lines(File, Grid),
  convert_to_char_grid(Grid, GridChr),
  remove_loop(GridChr, Answer).

main:-
  solve('input.txt', Answer),
  writeln(Answer).
 
