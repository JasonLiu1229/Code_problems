find_ranges(File, StrRanges) :-
  read_file_to_string(File, Content, []),
  normalize_space(string(Line), Content),
  split_string(Line, ",", "\t\r\n", StrRanges).

parse_range(S, A-B) :-
  split_string(S, "-", " \t\r\n", [AStr, BStr]),
  number_string(A, AStr),
  number_string(B, BStr).

convert_ranges(File, Ranges) :-
  find_ranges(File, StrRanges),
  maplist(parse_range, StrRanges, Ranges).


check_id(N) :-
  number_string(N, S),
  string_length(S, Len),
  Len mod 2 =:= 0,
  HalfLen is Len // 2,
  sub_string(S, 0, HalfLen, _, Left),
  sub_string(S, HalfLen, HalfLen, 0, Right),
  Left = Right.

sum_invalid_in_range(A, B, Sum) :-
    findall(N,
            ( between(A, B, N),
              check_id(N)
            ),
            Invalids),
    sum_list(Invalids, Sum).

sum_invalid_in_one_range(A-B, Acc, Acc2) :-
    sum_invalid_in_range(A, B, RangeSum),
    Acc2 is Acc + RangeSum.

sum_invalid_in_ranges(Ranges, Sum) :-
    foldl(sum_invalid_in_one_range, Ranges, 0, Sum).

main :-
    convert_ranges("input.txt", Ranges),
    sum_invalid_in_ranges(Ranges, Sum),
    writeln(Sum).
