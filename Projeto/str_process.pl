%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funções auxiliares para processar strings!
%--------------------------------- - - - - - - - - - -  -  -  -  -   -





%_________________Split de strings ________________________

atomic_list_concat_(L, Sep, Atom) :-
        ( atom(Sep), ground(L), is_list(L) )
    ->  list_atom(L, Sep, Atom)
    ;   ( atom(Sep), atom(Atom) )
    ->  atom_list(Atom, Sep, L)
    ;   instantiation_error(atomic_list_concat_(L, Sep, Atom))
    .

list_atom([Word], _Sep, Word).
list_atom([Word|L], Sep, Atom) :-
    list_atom(L, Sep, Right),
    atom_concat(Sep, Right, Right1),
    atom_concat(Word, Right1, Atom).

atom_list(Atom, Sep, [Word|L]) :-
    sub_atom(Atom, X,N,_, Sep),
    sub_atom(Atom, 0,X,_, Word),
    Z is X+N,
    sub_atom(Atom, Z,_,0, Rest),
    !, atom_list(Rest, Sep, L).
atom_list(Atom, _Sep, [Atom]).



%_________________strings to Integer ________________________

stringToNumber("0",0).
stringToNumber("1",1).
stringToNumber("2",2).
stringToNumber("3",3).
stringToNumber("4",4).
stringToNumber("5",5).
stringToNumber("6",6).
stringToNumber("7",7).
stringToNumber("8",8).
stringToNumber("9",9).

% converte uma string(que é um numero), em um inteiro.
strToNum([X],1,P):-stringToNumber([X],P).
strToNum([H|T],S,R):- S2 is S-1 , 
 					   stringToNumber([H],H1),
  					   S1 is H1*(10^S2),
  					   strToNum(T,S2,AUX), 
  					   R is AUX+S1.



map1([],[]).
map1([L|R],[L1|R1]):- atom_codes(L,Val),length(Val,Tam),strToNum(Val,Tam,L1),map1(R,R1).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% atomic_list_concat_(L, ',', '01,07,10,12,13,15').
% map1(['01','07','10','12','13','15'],R).




