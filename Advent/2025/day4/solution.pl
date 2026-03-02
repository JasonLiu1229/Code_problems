read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r", Lines).

get_cell(Grid, R, C, Char) :-
  nth1(R, Grid, Row),
  string_chars(Row, Chars),
  nth1(C, Chars, Char).

in_bounds(Grid, R, C) :-     
  R > 0,
  C > 0,
  length(Grid, RowCount),
  R =< RowCount,
  nth1(R, Grid, Row),
  string_length(Row, ColCount),
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

main:-
  solve('input.txt', Answer),
  writeln(Answer).
 
