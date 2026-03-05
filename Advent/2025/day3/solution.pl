% ------------------------------------------------------------
% read_lines(+File, -Lines)
%
% Reads a file and returns its contents as a list of strings,
% one string per line.
%
% Steps:
% 1. Read the entire file into a single string (Content).
% 2. Split the string at newline characters.
%
% "\n"  -> separator
% "\r\n" -> characters to trim
%
% Example file:
%   123
%   456
%
% Result:
%   Lines = ["123","456"]
% ------------------------------------------------------------
read_lines(File, Lines) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r\n", Lines).


% ------------------------------------------------------------
% char_digit(+Char, -Digit)
%
% Converts a character representing a digit into its
% numeric value.
%
% Example:
%   '5' -> 5
%
% How it works:
% 1. char_code/2 gets the ASCII code of the character
% 2. ASCII code of '0' is 48
% 3. Subtract 48 to obtain the numeric digit
%
% Example:
%   '5' -> ASCII 53
%   53 - 48 = 5
% ------------------------------------------------------------
char_digit(Char, Digit) :-
    char_code(Char, Code),
    Digit is Code - 48. % ASCII value of '0' = 48


% ------------------------------------------------------------
% string_digits(+String, -Digits)
%
% Converts a string like "1234" into a list of digits.
%
% Steps:
% 1. Convert the string to a list of characters
% 2. Convert each character to its numeric value
%
% Example:
%   "1234" -> [1,2,3,4]
% ------------------------------------------------------------
string_digits(String, Digits) :-
    string_chars(String, Chars),
    maplist(char_digit, Chars, Digits).


% ------------------------------------------------------------
% pair_value(+Digits, -Value)
%
% Generates a two-digit number using two different digits
% from the list.
%
% Steps:
% 1. Select digit Di at position I
% 2. Select digit Dj at position J
% 3. Ensure J > I so the pair uses digits in order
% 4. Build the two-digit number
%
% Example:
%   Digits = [3,5,7]
%
% Possible pairs:
%   3 and 5 -> 35
%   3 and 7 -> 37
%   5 and 7 -> 57
% ------------------------------------------------------------
pair_value(Digits, Value) :-
    nth0(I, Digits, Di),
    nth0(J, Digits, Dj), % takes J-th element of Digits
    J > I,
    Value is Di*10 + Dj. % Di becomes the tens digit


% ------------------------------------------------------------
% all_pair_values(+Digits, -Values)
%
% Collects all two-digit numbers that can be formed
% from ordered pairs of digits.
%
% findall/3 runs pair_value/2 repeatedly and collects
% all generated values in a list.
%
% Example:
%   Digits = [3,5,7]
%
% Result:
%   Values = [35,37,57]
% ------------------------------------------------------------
all_pair_values(Digits, Values) :-
    findall(Value,
            pair_value(Digits, Value),
            Values). % list all successful pair values


% ------------------------------------------------------------
% max_bank(+Digits, -Max)
%
% Computes the maximum two-digit value that can be formed
% from the digits in one line (one bank).
%
% Steps:
% 1. Generate all pair values
% 2. Take the maximum value
%
% Example:
%   Digits = [3,5,7]
%   Pair values = [35,37,57]
%   Max = 57
% ------------------------------------------------------------
max_bank(Digits, Max) :-
    all_pair_values(Digits, Values),
    max_list(Values, Max).


% ------------------------------------------------------------
% solve(+File, -Total)
%
% Main solving predicate.
%
% Steps:
% 1. Read all lines from the file
% 2. Convert each line to digits
% 3. Compute the maximum value per line (bank)
% 4. Sum all maximum values
%
% Example:
%
% File:
%   123
%   459
%
% Process:
%   "123" -> [1,2,3] -> max pair = 23
%   "459" -> [4,5,9] -> max pair = 59
%
% Total = 23 + 59 = 82
% ------------------------------------------------------------
solve(File, Total) :-
    read_lines(File, Lines),
    maplist(string_digits, Lines, BanksDigits),
    maplist(max_bank, BanksDigits, MaxPerBank),
    sum_list(MaxPerBank, Total).


% ------------------------------------------------------------
% main
%
% Program entry point.
%
% Steps:
% 1. Solve the problem using input.txt
% 2. Print the total result
% ------------------------------------------------------------
main :- 
  solve("input.txt", Total),
  writeln(Total).
