% ------------------------------------------------------------
% find_ranges(+File, -StrRanges)
%
% Reads the file and extracts the ranges as strings.
% Example file content:
%   3-5,10-14,16-20
%
% Steps:
% 1. Read the entire file as one string.
% 2. Normalize spaces and remove extra whitespace/newlines.
% 3. Split the string by commas to obtain individual ranges.
%
% Output example:
%   ["3-5","10-14","16-20"]
% ------------------------------------------------------------
find_ranges(File, StrRanges) :-
  read_file_to_string(File, Content, []),
  normalize_space(string(Line), Content),
  split_string(Line, ",", "\t\r\n", StrRanges).


% ------------------------------------------------------------
% parse_range(+StringRange, -Range)
%
% Converts a string like "3-5" into a Prolog range structure.
%
% Example:
%   "3-5"  ->  3-5
%
% Steps:
% 1. Split the string at '-'
% 2. Convert the two resulting strings into numbers
% 3. Return them as A-B
%
% Output example:
%   parse_range("3-5", R)  ->  R = 3-5
% ------------------------------------------------------------
parse_range(S, A-B) :-
  split_string(S, "-", " \t\r\n", [AStr, BStr]),
  number_string(A, AStr),
  number_string(B, BStr).


% ------------------------------------------------------------
% convert_ranges(+File, -Ranges)
%
% Reads the ranges from the file and converts them into
% numeric Prolog ranges using parse_range.
%
% Example:
%   File content: "3-5,10-14"
%
% Result:
%   Ranges = [3-5, 10-14]
%
% maplist applies parse_range to each element of StrRanges.
% ------------------------------------------------------------
convert_ranges(File, Ranges) :-
  find_ranges(File, StrRanges),
  maplist(parse_range, StrRanges, Ranges).


% ------------------------------------------------------------
% check_id(+N)
%
% Checks whether a number is an "invalid ID" according to
% the rule:
%   - Convert number to string
%   - Length must be even
%   - Left half must equal the right half
%
% Example:
%   1212 -> valid (12 == 12)
%   1234 -> false
%
% Steps:
% 1. Convert number to string
% 2. Get length
% 3. Ensure length is even
% 4. Split string into two equal halves
% 5. Compare the halves
% ------------------------------------------------------------
check_id(N) :-
  number_string(N, S),
  string_length(S, Len),
  Len mod 2 =:= 0,
  HalfLen is Len // 2,
  sub_string(S, 0, HalfLen, _, Left),
  sub_string(S, HalfLen, HalfLen, 0, Right),
  Left = Right.


% ------------------------------------------------------------
% sum_invalid_in_range(+A, +B, -Sum)
%
% Computes the sum of all numbers between A and B
% that satisfy check_id.
%
% Steps:
% 1. Generate all numbers between A and B using between/3
% 2. Keep only numbers that satisfy check_id
% 3. Collect them into a list (Invalids)
% 4. Sum the list
%
% Example:
%   Range: 1100-1122
%   Valid numbers might be: [1111]
%   Sum = 1111
% ------------------------------------------------------------
sum_invalid_in_range(A, B, Sum) :-
    findall(N,
            ( between(A, B, N),
              check_id(N)
            ),
            Invalids),
    sum_list(Invalids, Sum).


% ------------------------------------------------------------
% sum_invalid_in_one_range(+Range, +Acc, -Acc2)
%
% Helper predicate used with foldl.
%
% Range is written as A-B.
%
% Steps:
% 1. Calculate the sum of invalid IDs in this range
% 2. Add it to the current accumulator Acc
% 3. Return the updated accumulator Acc2
% ------------------------------------------------------------
sum_invalid_in_one_range(A-B, Acc, Acc2) :-
    sum_invalid_in_range(A, B, RangeSum),
    Acc2 is Acc + RangeSum.


% ------------------------------------------------------------
% sum_invalid_in_ranges(+Ranges, -Sum)
%
% Computes the total sum of invalid IDs across
% all ranges.
%
% foldl works like a loop:
%
%   Acc0 = 0
%   for each Range in Ranges:
%       Acc = sum_invalid_in_one_range(Range, Acc)
%
% Final result is Sum.
%
% Example:
%   Ranges = [3-5,10-14]
% ------------------------------------------------------------
sum_invalid_in_ranges(Ranges, Sum) :-
    foldl(sum_invalid_in_one_range, Ranges, 0, Sum).


% ------------------------------------------------------------
% main
%
% Entry point of the program.
%
% Steps:
% 1. Read ranges from "input.txt"
% 2. Convert them to numeric ranges
% 3. Calculate the sum of invalid IDs
% 4. Print the result
% ------------------------------------------------------------
main :-
    convert_ranges("input.txt", Ranges),
    sum_invalid_in_ranges(Ranges, Sum),
    writeln(Sum).
