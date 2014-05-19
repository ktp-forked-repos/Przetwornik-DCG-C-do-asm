
 execute :-
        readFromFile("f:\\Studia\\Dropbox\\sem VI\\SI\\input.txt", R),
        program(Z, R, []),
	write(Z), !.

readFromFile(File, Output) :-
                open(File, read, Temp, [encoding(utf8)]),
                read_stream_to_codes(Temp, Output),
                close(Temp).

program(Z) --> code(Za), {concat_atom([Za], Z)}.

code(Z) --> declaration(Z1), whitespace, {concat_atom([Z1], Z)}.
code(Z) --> declaration(Z).
code(Z) --> declaration(Z), code(Z).

declaration(Z) --> "int", whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), whitespace,";",       {concat_atom([A,': dd ',B],Z)}.

declaration(Z) --> "char", whitespace, chars(A), whitespace, "=", whitespace, "'", char(B), "'", whitespace,";",       {concat_atom([A,': db ','"',B,'"'],Z)}.

declaration(Z) --> "char", whitespace, chars(A),"[]", whitespace, "=", whitespace, "'", string(B), "'", whitespace,";",       {concat_atom([A,': db ','"',B,'",0'],Z)}.

% Bia³e znaki.
whitespace --> " ", whitespace.
whitespace --> "\t", whitespace.
whitespace --> "".

%liczby ca³kowite
integer_number(I) --> digit(I1), integer_number(Rest), {concat_atom([I1,Rest], I)}.
integer_number(I) --> digit(I).
digit(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

%ci¹gi znaków
string(C) --> chars(C).

chars(C) --> char(C1), chars(Rest), {concat_atom([C1, Rest], C)}.
chars(C) --> char(C).
char(C) --> [C1], {code_type(C1, alnum), atom_codes(C, [C1])}.
%alpha_char(C) --> [C], {code_type(C, alpha)}.

% c_code --> functions, [int, main, '(' ], args, [')'], ['{'], code,
% [return], [0,';'] ,['}'].
% c_code --> functions, [void, main, '(' ], args, [')'], ['{'], code
% ,['}'].

%args --> [].
%functions --> [].
