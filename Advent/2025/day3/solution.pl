read_lines(File, Lines) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "\r\n", Lines).

char_digit(Char, Digit) :-
    char_code(Char, Code),
    Digit is Code - 48. % ASCII value of 0 = 48

string_digits(String, Digits) :-
    string_chars(String, Chars),
    maplist(char_digit, Chars, Digits).

pair_value(Digits, Value) :-
    nth0(I, Digits, Di),
    nth0(J, Digits, Dj), % takes J-th item of list Digits
    J > I,
    Value is Di*10 + Dj. % Di is the first number so make it decimal

all_pair_values(Digits, Values) :-
    findall(Value,
            pair_value(Digits, Value),
            Values). % list all succesfull items

max_bank(Digits, Max) :-
    all_pair_values(Digits, Values),
    max_list(Values, Max).

solve(File, Total) :-
    read_lines(File, Lines),
    maplist(string_digits, Lines, BanksDigits),
    maplist(max_bank, BanksDigits, MaxPerBank),
    sum_list(MaxPerBank, Total).

main :- 
  solve("input.txt", Total),
  writeln(Total).
