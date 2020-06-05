%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funções auxiliares para a criação da baseDeConhecimento e do grafo.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- include('str_process.pl').

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
% criarBC("/Users/ril/github/SRCR-Transport-recommendation/Projeto/Data.csv","/Users/ril/github/SRCR-Transport-recommendation/Projeto/baseDeConhecimento2.pl",R).

criarBC(Name_Exel,DirDestino,criada_Com_Sucesso) :- csv_read_file(Name_Exel,Data,[functor(paragem),arity(11),separator(0';)]), %'
									open(DirDestino,write,OS),
									mapBC(OS,Data,sucesso), 	
								   	close(OS).

mapBC(OS,[],sucesso).
mapBC(OS,[Y1|YS],R):- processaPar(Y1,Y), writeq(OS,Y),write(OS,"."),nl(OS) ,mapBC(OS,YS,R).

processaPar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,[Carr1],CodRua1,NomeRua1,Freguesia1)):- integer(Carr1).
processaPar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr2,CodRua1,NomeRua1,Freguesia1)):- atomic_list_concat_(L,',',Carr1),alltoInt(L,Carr2) .
processaPar(X,X).


alltoInt([],[]).
alltoInt([X|XS],[Y|YS]):- atom_number(X,Y),alltoInt(XS,YS).

%--------------------------------- 

% criarGrafo("/Users/ril/github/SRCR-Transport-recommendation/Projeto/lista_adjacencias.csv","/Users/ril/github/SRCR-Transport-recommendation/Projeto/grafo.pl",R). 
criarGrafo(Name_Exel,DirDestino,criado_Com_Sucesso) :- csv_read_file(Name_Exel,Data,[functor(paragem),arity(11),separator(0';)]), %'
													 	  open(DirDestino,write,OS),
													  	  write(OS,"grafo("),
													      todasParagens(Data,Res),length(Res,P),
													      write(OS,Res),nl(OS),write(OS,","),
                                                          agrupaParagem(Data,Pares),length(Pares,C),
                                                          writeq(OS,Pares),write(OS,")."),
                                                          nl(OS),close(OS), print("Grafo com "),print(C),print("Arcos(caminhos) e "),print(P), print(" vertices(paragens) !").




%--------------------------------- 


% Retorna todos os Ids das paragens.
todasParagens([],R).
todasParagens([paragem(X1,_,_,_,_,_,_,_,_,_,_)|YS],XS):- memberchk(X1,XS),todasParagens(YS,XS). 
todasParagens([paragem(X1,_,_,_,_,_,_,_,_,_,_)|YS],[X1|XS]):- todasParagens(YS,XS). 




% csv_read_file("/Users/ril/github/SRCR-Transport-recommendation/Projeto/lista_adjacencias.csv",Data,[functor(paragem),arity(11),separator(0';)]) %'

%Função que cria os grupos 
agrupaParagem([],[]).
agrupaParagem([paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)],R).

agrupaParagem([paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
			   paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)|YS],
			   [viajar(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
			   paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1))|R]) :-  agrupaParagem([paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)|YS],R).

agrupaParagem([paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
			   paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)|YS], R) :- agrupaParagem([paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)|YS],R).










%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Lista Ligada.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%construir_ListaLigada(L,DirDestino):- open(DirDestino,write,OS),
									  


%map(OS,A,[viajar(paragem(X,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Y,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1))|R],TodasPar) :-
%	write(OS,"carrAdjacente("),
%	write(OS,Carr),write(OS,","),
%	((A==Carr) -> obterCarr(X,TodasParagens,UmaLista)),
%	write(OS,)


%Criar um dicionario ! ver no
%findall(KKKK,viajar(X,Y),( obterCarr(Id_Y,Todas,KKKK)) L)











%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

aux(X) :-agrupaParagem([paragem(183,-103678.36, -96590.26, 'Bom', 'Fechado dos Lados', 'Yes', 'Vimeca', 1, 286, 'Rua Aquilino Ribeiro', 'Carnaxide e Queijas'), paragem(791, -103705.46, -96673.6, 'Bom', 'Aberto dos Lados', 'Yes', 'Vimeca', 1, 286, 'Rua Aquilino Ribeiro', 'Carnaxide e Queijas'), paragem(595, -103725.69, -95975.2, 'Bom', 'Fechado dos Lados', 'Yes', 'Vimeca', 1, 354, 'Rua Manuel Teixeira Gomes', 'Carnaxide e Queijas'),paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(182, -103746.76, -96396.66, 'Bom', 'Fechado dos Lados', 'Yes', 'Scotturb', 1, 286, 'Rua Aquilino Ribeiro', 'Carnaxide e Queijas')],X).

%csv_read_file("/Users/ril/github/SRCR-Transport-recommendation/Projeto/lista_adjacencias.csv",Data,[functor(paragem),arity(11),separator(0';)]).
	
