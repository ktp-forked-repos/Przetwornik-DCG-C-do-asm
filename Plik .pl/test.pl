
 execute :-
        readFromFile("f:\\Studia\\Dropbox\\sem VI\\SI\\input.txt", R),
        program(Z, R, []),
	write(Z), !.

readFromFile(File, Output) :-
                open(File, read, Temp, [encoding(utf8)]),
                read_stream_to_codes(Temp, Output),
                close(Temp).

%program(Z) --> code(Za), {concat_atom([Za], Z)}.

%tymczasowe wywolanei dla test�w

program(Z) --> add(Za), {concat_atom([Za], Z)}.

program(Z) --> sub(Za), {concat_atom([Za], Z)}.

program(Z) --> div(Za), {concat_atom([Za], Z)}.

program(Z) --> mul(Za), {concat_atom([Za], Z)}.

program(Z) --> inc(Za), {concat_atom([Za], Z)}.

program(Z) --> dec(Za), {concat_atom([Za], Z)}.

program(Z) --> if(Za), {concat_atom([Za], Z)}.


code(Z) --> declaration(Z1), whitespace, {concat_atom([Z1], Z)}.
code(Z) --> declaration(Z).
code(Z) --> declaration(Z), code(Z).

%operacje arytmetyczne
%dodawanie

add(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,add_op,whitespace, chars(B),whitespace,";", {concat_atom(['\nmov eax, [',A,']\nadd eax, [',B,']\nmov [',C,'], eax\n'],Z)}.

sub(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,sub_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nsub eax, [',B,']\nmov [',C,'], eax\n'],Z)}.

div(Z) --> whitespace, chars(C), whitespace, "=",whitespace, chars(A),whitespace,div_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nmov edx,0\nidiv dword [',B,']\nmov [',C,'], eax\n'],Z)}.

mul(Z) --> whitespace, chars(C), whitespace, "=" , whitespace, chars(A),whitespace,mul_op,whitespace, chars(B),whitespace,";", {concat_atom(['mov eax, [',A,']\nimul dword [',B,']\nmov [',C,'], eax\n'],Z)}.

inc(Z) --> whitespace, chars(A), whitespace, add_op, add_op, whitespace, {concat_atom(['mov eax, [',A,']\ninc eax'],Z)}.

dec(Z) --> whitespace, chars(A), whitespace, sub_op, sub_op, whitespace, {concat_atom(['mov eax, [',A,']\ndec eax'],Z)}.

%instrukcje warunkowe
if(Z) --> "if",whitespace,"(",whitespace,if_cond(Za),whitespace,")",whitespace,"{",whitespace,exp_if(Zb),whitespace,"}", {concat_atom([Za,Zb],Z)}.
% wyra�enie kt�re ma si� wykonywa� w if {} p�ki co jest za�o�eniem operacji dodawania
%zak�adam, �e mo�emy por�wnywa� tylko liczby w postaci if(x>5)

%if(x>5)
if_cond(Z) --> chars(A),whitespace,cond_op_greater,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njg klamra'],Z)}.

%if(x<5)
if_cond(Z) --> chars(A),whitespace,cond_op_less,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njl klamra'],Z)}.

%if(x==5)
if_cond(Z) --> chars(A),whitespace,cond_op_equal_to,whitespace,integer_number(B), {concat_atom(['mov eax, [',A,']\ncmp eax, ',B,'\njz klamra\n'],Z)}.


%operatory arytmetyczne
add_op --> "+".
sub_op--> "-".
div_op --> "/".
mul_op --> "*".

%operatory warunkowe
cond_op_greater --> ">".
%cond_op_greater_or_equal --> ">=".
cond_op_less --> "<".
%cond_op_less_or_equal --> "<=".
cond_op_equal_to --> "==".

% exp to ka�de mo�liwe wyra�enie, kt�re si� moze pojawi�, nale�a�oby
% zdefiniowa� kilka(na�cie lub set) mo�liwo�ci
%
exp_if(Z) --> exp(Za), {concat_atom(['\n\nklamra:',Za],Z)}.

exp(Z) --> add(Za), {concat_atom([Za], Z)}.


declaration(Z) --> "int", whitespace, chars(A), whitespace, "=", whitespace, integer_number(B), whitespace,";",       {concat_atom([A,': dd ',B],Z)}.

declaration(Z) --> "char", whitespace, chars(A), whitespace, "=", whitespace, "'", char(B), "'", whitespace,";",       {concat_atom([A,': db ','"',B,'"'],Z)}.

declaration(Z) --> "char", whitespace, chars(A),"[]", whitespace, "=", whitespace, "'", string(B), "'", whitespace,";",       {concat_atom([A,': db ','"',B,'",0'],Z)}.

% Bia�e znaki.
whitespace --> " ", whitespace.
whitespace --> "\t", whitespace.
whitespace --> "\n", whitespace.
whitespace --> "".

%liczby ca�kowite
integer_number(I) --> digit(I1), integer_number(Rest), {concat_atom([I1,Rest], I)}.
integer_number(I) --> digit(I).
digit(I) --> [I1], {code_type(I1, digit), atom_codes(I, [I1])}.

%ci�gi znak�w
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







