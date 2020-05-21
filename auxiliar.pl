%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funções auxiliares
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Listas

% Verifica se um elemento existe na lista.
containsList(X, [],nao).
containsList(X, [X|T] ,sim). 
containsList(X, [H|T] ,R) :- containsList(X,T,R). 


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Criação da base de conhecimento.
% :- use_module(library(csv)) 
% Exemplo de execucao 
% criarBC("/Users/ril/github/SRCR-Transport-recommendation/Data.csv","/Users/ril/github/SRCR-Transport-recommendation/baseDeConhecimento2.pl",R).

criarBC(Name_Exel,DirDestino,criada_Com_Sucesso) :- csv_read_file(Name_Exel,Data,[functor(paragem),arity(11),separator(0';)]), %'
									open(DirDestino,write,OS),
									mapBC(OS,Data,sucesso), 	
								   	close(OS).

mapBC(OS,[],sucesso).
mapBC(OS,[Y|YS],R):-write(OS,Y),nl(OS), mapBC(OS,YS,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -



