find_ranges(File, StrRanges) :-
  read_file_to_string(File, Content, []),
  normalize_space(string(Line), Content),
  split_string(Line, ",", "\t\r\n", StrRanges).

parse_range(S, A-B) :-
  split_string(S, "-", " \t\r\n", [AStr, BStr]),
  number_string(A, AStr),
  number_string(B, BStr). % convert string to number (also works in the other direction)

convert_ranges(File, Ranges) :-
  find_ranges(File, StrRanges),
  maplist(parse_range, StrRanges, Ranges). % apply predicate to all list items and convert it to the Ranges output

repeat_check(String, Block, Times) :- 
  Times > 1,
  length(Blocks, Times),
  maplist(=(Block), Blocks),
  atomic_list_concat(Blocks, '', String).

check_id(N) :-
  number_string(N, S),
  atom_string(AtomS, S),
  string_length(S, Len),
  between(1, Len, K), % generator
  K < Len,
  0 is Len mod K, % no rest remains
  Times is Len // K, % how many pieces does it devide it into
  Times > 1,
  sub_string(S, 0, K, _, Block),
  atom_string(BlockAtom, Block),
  repeat_check(AtomS, BlockAtom, Times),
  !. % ! makes it stop after finding the first repeated is found

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
