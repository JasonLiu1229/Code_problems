read_lines(File, Lines) :-
  read_file_to_string(File, Content, []),
  split_string(Content, "\n", "\r\n", Lines).

string_digits(String, Digits) :-
  string_chars(String, Chars),
  maplist(char_digit, Chars, Digits).

char_digit(Char, Digit) :-
  char_code(Char, Code),
  Digit is Code - 48.

take_prefix(List, N, Prefix) :-
  length(Prefix, N),
  append(Prefix, _Rest, List).

drop_until([X|Rest], X, Rest) :- !.
drop_until([_|T], X, Rest) :-
  drop_until(T, X, Rest).

best_in_prefix(Digits, Window, BestDigit, AfterBest) :-
  take_prefix(Digits, Window, Prefix),
  max_list(Prefix, BestDigit),
  drop_until(Digits, BestDigit, AfterBest).

pick_k(_Digits, 0, []) :- !.
pick_k(Digits, Need, [Best|Rest]) :-
  length(Digits, Remaining),
  Window is Remaining - Need + 1,
  best_in_prefix(Digits, Window, Best, AfterBest),
  Need1 is Need - 1,
  pick_k(AfterBest, Need1, Rest).

best_k_digits_simple(Digits, K, BestDigits) :-
  pick_k(Digits, K, BestDigits).


digits_to_number(Digits, Number) :-
  foldl(acc_digit, Digits, 0, Number).

acc_digit(D, Acc, Out) :-
  Out is Acc*10 + D.

max_bank_k(Digits, K, Number) :-
  best_k_digits_simple(Digits, K, BestDigits),
  digits_to_number(BestDigits, Number).

solve(File, Total) :-
  K = 12,
  read_lines(File, Lines),
  maplist(string_digits, Lines, BanksDigits),
  maplist({K}/[Ds,N]>>max_bank_k(Ds, K, N), BanksDigits, BankNumbers),
  sum_list(BankNumbers, Total).

main :-
  solve("input.txt", Total),
  writeln(Total).
