% ------------------------------------------------------------
% read_lines(+File, -Lines)
%
% Reads the whole file into one string, then splits it into lines.
%
% Example file:
%   12345
%   67890
%
% Output:
%   Lines = ["12345","67890"]
% ------------------------------------------------------------
read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r\n", Lines).


% ------------------------------------------------------------
% string_digits(+String, -Digits)
%
% Converts a string of digit characters (e.g. "507") into a list
% of integers (e.g. [5,0,7]).
%
% Steps:
% 1. string_chars turns "507" into ['5','0','7']
% 2. maplist(char_digit, ...) turns each char into its integer value
% ------------------------------------------------------------
string_digits(String, Digits) :-
  string_chars(String, Chars),
  maplist(char_digit, Chars, Digits).


% ------------------------------------------------------------
% char_digit(+Char, -Digit)
%
% Converts a character digit to an integer digit using ASCII codes.
%
% '0' has ASCII code 48, '1' is 49, ..., '9' is 57.
% So Digit = Code - 48.
%
% Example:
%   Char = '7' -> Code = 55 -> Digit = 7
% ------------------------------------------------------------
char_digit(Char, Digit) :-
  char_code(Char, Code),
  Digit is Code - 48.


% ------------------------------------------------------------
% take_prefix(+List, +N, -Prefix)
%
% Takes the first N elements of List and returns them as Prefix.
% (Assumes List has at least N elements.)
%
% How it works:
% - length(Prefix, N) creates a list Prefix of length N (unknown elems)
% - append(Prefix, _Rest, List) forces Prefix to unify with the first
%   N elements of List, and _Rest with the remaining tail.
%
% Example:
%   take_prefix([1,2,3,4], 2, Prefix) -> Prefix = [1,2]
% ------------------------------------------------------------
take_prefix(List, N, Prefix) :-
  length(Prefix, N),
  append(Prefix, _Rest, List).


% ------------------------------------------------------------
% drop_until(+List, +X, -Rest)
%
% Drops elements from the front of List until it finds X,
% then returns the tail immediately AFTER that X.
%
% The cut (!) means: once we found the first X, stop and do not
% search for later occurrences of X.
%
% Example:
%   drop_until([3,1,9,9,2], 9, Rest) -> Rest = [9,2]
%   (because it stops at the first 9, then returns what comes after it)
% ------------------------------------------------------------
drop_until([X|Rest], X, Rest) :- !.
drop_until([_|T], X, Rest) :-
  drop_until(T, X, Rest).


% ------------------------------------------------------------
% best_in_prefix(+Digits, +Window, -BestDigit, -AfterBest)
%
% Chooses the best digit you can take next, using a greedy rule:
% - Look only at the first Window digits
% - Pick the maximum digit within that prefix (BestDigit)
% - Then drop the list up to and including the first occurrence
%   of that chosen digit, returning the remainder (AfterBest)
%
% This is a standard greedy step for building the lexicographically
% largest number while keeping the original order.
%
% Example:
%   Digits = [1,9,2,8,3], Window = 3
%   Prefix = [1,9,2] -> max is 9
%   AfterBest = digits after the first 9 -> [2,8,3]
% ------------------------------------------------------------
best_in_prefix(Digits, Window, BestDigit, AfterBest) :-
  take_prefix(Digits, Window, Prefix),
  max_list(Prefix, BestDigit),
  drop_until(Digits, BestDigit, AfterBest).


% ------------------------------------------------------------
% pick_k(+Digits, +Need, -Picked)
%
% Picks Need digits from Digits (in the same order) to form the
% "best" possible sequence, using the greedy strategy:
%
% At each step:
% - Remaining = length(Digits)
% - Window = Remaining - Need + 1
%   This ensures you don't look too far ahead: you must leave enough
%   digits to still pick Need total.
%
% Then:
% - best_in_prefix chooses the best digit within the allowed window,
%   and returns the list after that digit.
% - Recurse to pick the remaining Need-1 digits.
%
% Base case:
%   pick_k(_Digits, 0, []) means: if you need 0 digits, you're done.
% The cut (!) prevents Prolog from trying alternative ways to satisfy it.
% ------------------------------------------------------------
pick_k(_Digits, 0, []) :- !.
pick_k(Digits, Need, [Best|Rest]) :-
  length(Digits, Remaining),
  Window is Remaining - Need + 1,
  best_in_prefix(Digits, Window, Best, AfterBest),
  Need1 is Need - 1,
  pick_k(AfterBest, Need1, Rest).


% ------------------------------------------------------------
% best_k_digits_simple(+Digits, +K, -BestDigits)
%
% Wrapper predicate: returns the "best" K digits (in order)
% from Digits using pick_k.
% ------------------------------------------------------------
best_k_digits_simple(Digits, K, BestDigits) :-
  pick_k(Digits, K, BestDigits).


% ------------------------------------------------------------
% digits_to_number(+Digits, -Number)
%
% Converts a list of digits into the corresponding integer.
%
% Example:
%   [1,2,3] -> 123
%
% Uses foldl with an accumulator:
%   start Acc = 0
%   for each digit D:
%       Acc := Acc*10 + D
% ------------------------------------------------------------
digits_to_number(Digits, Number) :-
  foldl(acc_digit, Digits, 0, Number).


% ------------------------------------------------------------
% acc_digit(+D, +Acc, -Out)
%
% One step of building a number digit-by-digit.
%
% If Acc is the number built so far, and D is the next digit:
%   Out = Acc*10 + D
%
% Example:
%   Acc=12, D=3 -> Out=123
% ------------------------------------------------------------
acc_digit(D, Acc, Out) :-
  Out is Acc*10 + D.


% ------------------------------------------------------------
% max_bank_k(+Digits, +K, -Number)
%
% For one "bank" (one line of digits):
% 1. Choose the best K digits while keeping order
% 2. Convert those K digits to a number
%
% Example:
%   Digits = [1,9,2,8,3], K=3
%   BestDigits might be [9,8,3]
%   Number = 983
% ------------------------------------------------------------
max_bank_k(Digits, K, Number) :-
  best_k_digits_simple(Digits, K, BestDigits),
  digits_to_number(BestDigits, Number).


% ------------------------------------------------------------
% compute_bank(+K, +Digits, -Number)
%
% Helper predicate used with maplist/3.
%
% For a given list of digits (Digits), it computes the largest
% possible number that can be formed by selecting K digits
% while keeping their original order.
%
% It simply calls max_bank_k/3 with the same arguments.
%
% Example:
%   K = 3
%   Digits = [1,9,2,8,3]
%
%   BestDigits = [9,8,3]
%   Number = 983
%
% Used inside solve/2 like this:
%   maplist(compute_bank(K), BanksDigits, BankNumbers)
%
% Which means:
%   For each digit list Ds in BanksDigits,
%   compute the best K-digit number N and store it in BankNumbers.
% ------------------------------------------------------------
compute_bank(K, Ds, N) :-
  max_bank_k(Ds, K, N).


% ------------------------------------------------------------
% solve(+File, -Total)
%
% Solves the whole problem for the input file.
%
% Steps:
% 1. Fix K = 12 (you always pick 12 digits per line)
% 2. Read file lines
% 3. Convert each line string -> list of digits (BanksDigits)
% 4. For each digit list Ds, compute the best K-digit number N
% 5. Sum all those numbers into Total
%
% ------------------------------------------------------------
solve(File, Total) :-
  K = 12,
  read_lines(File, Lines),
  maplist(string_digits, Lines, BanksDigits),
  maplist(compute_bank(K), BanksDigits, BankNumbers), % reason why we use compute bank over max bank k is because the order of the function is mismatched
  sum_list(BankNumbers, Total).


% ------------------------------------------------------------
% main
%
% Entry point:
% - solve using "input.txt"
% - print Total
% ------------------------------------------------------------
main :-
  solve("input.txt", Total),
  writeln(Total).
