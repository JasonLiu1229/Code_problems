% ------------------------------------------------------------
% find_ranges(+File, -StrRanges)
%
% Reads a file that contains ranges like:
%   3-5,10-14,16-20
%
% Steps:
% 1. Read the entire file into a string (Content)
% 2. Normalize whitespace (remove extra spaces/newlines)
% 3. Split the string by commas into individual range strings
%
% Example output:
%   ["3-5","10-14","16-20"]
% ------------------------------------------------------------
find_ranges(File, StrRanges) :-
  read_file_to_string(File, Content, []),
  normalize_space(string(Line), Content),
  split_string(Line, ",", "\t\r\n", StrRanges).


% ------------------------------------------------------------
% parse_range(+String, -Range)
%
% Converts a range string into a Prolog structure A-B.
%
% Example:
%   "3-5" -> 3-5
%
% Steps:
% 1. Split the string at "-"
% 2. Convert both parts to numbers
% ------------------------------------------------------------
parse_range(S, A-B) :-
  split_string(S, "-", " \t\r\n", [AStr, BStr]),
  number_string(A, AStr),
  number_string(B, BStr). % convert string to number (also works the other direction)


% ------------------------------------------------------------
% convert_ranges(+File, -Ranges)
%
% Reads ranges from the file and converts them into
% numeric Prolog ranges using parse_range.
%
% maplist applies parse_range to each element of StrRanges.
%
% Example:
%   ["3-5","10-14"] -> [3-5,10-14]
% ------------------------------------------------------------
convert_ranges(File, Ranges) :-
  find_ranges(File, StrRanges),
  maplist(parse_range, StrRanges, Ranges). % apply predicate to all list items


% ------------------------------------------------------------
% repeat_check(+String, +Block, +Times)
%
% Checks if String is equal to Block repeated Times times.
%
% Example:
%   Block = "12"
%   Times = 3
%
% Constructed string:
%   "121212"
%
% Steps:
% 1. Create a list of length Times
% 2. Fill the list with Block
% 3. Concatenate all blocks into one string
% 4. Compare it with the original String
% ------------------------------------------------------------
repeat_check(String, Block, Times) :- 
  Times > 1,
  length(Blocks, Times),        % create list of size Times
  maplist(=(Block), Blocks),    % fill list with Block
  atomic_list_concat(Blocks, '', String). % join blocks into one string, also does the check for us


% ------------------------------------------------------------
% check_id(+N)
%
% Checks if number N consists of a repeating pattern.
%
% Examples:
%   1212  -> "12" repeated twice
%   123123 -> "123" repeated twice
%   1111 -> "1" repeated four times
%
% Steps:
% 1. Convert number to string
% 2. Try all possible block sizes K
% 3. Check if the string length is divisible by K
% 4. Extract the first block
% 5. Check if repeating that block reconstructs the string
%
% The cut (!) stops searching after the first valid match.
% ------------------------------------------------------------
check_id(N) :-
  number_string(N, S),          % convert number to string
  atom_string(AtomS, S),        % convert string to atom
  string_length(S, Len),        % get length of string

  between(1, Len, K),           % generator: possible block sizes
  K < Len,                      % block cannot be the full string
  0 is Len mod K,               % block must divide string evenly

  Times is Len // K,            % number of repetitions
  Times > 1,                    % must repeat at least twice

  sub_string(S, 0, K, _, Block),% extract first block
  atom_string(BlockAtom, Block),

  repeat_check(AtomS, BlockAtom, Times), % check repetition
  !.                             % stop after first successful match


% ------------------------------------------------------------
% sum_invalid_in_range(+A, +B, -Sum)
%
% Computes the sum of all numbers between A and B
% that satisfy check_id.
%
% Steps:
% 1. Generate numbers using between/3
% 2. Keep only numbers satisfying check_id
% 3. Collect them into a list
% 4. Sum the list
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
% Helper predicate for foldl.
%
% Range has form A-B.
% Acc is the current accumulated sum.
% Acc2 is the updated sum after processing this range.
% ------------------------------------------------------------
sum_invalid_in_one_range(A-B, Acc, Acc2) :-
    sum_invalid_in_range(A, B, RangeSum),
    Acc2 is Acc + RangeSum.


% ------------------------------------------------------------
% sum_invalid_in_ranges(+Ranges, -Sum)
%
% Computes the total sum of invalid IDs across all ranges.
%
% foldl works like a loop:
%
%   Acc = 0
%   for each Range:
%       Acc += sum_invalid_in_range(Range)
%
% ------------------------------------------------------------
sum_invalid_in_ranges(Ranges, Sum) :-
    foldl(sum_invalid_in_one_range, Ranges, 0, Sum).


% ------------------------------------------------------------
% main
%
% Entry point of the program.
%
% Steps:
% 1. Read and convert ranges from "input.txt"
% 2. Compute the total sum of invalid IDs
% 3. Print the result
% ------------------------------------------------------------
main :-
    convert_ranges("input.txt", Ranges),
    sum_invalid_in_ranges(Ranges, Sum),
    writeln(Sum).
