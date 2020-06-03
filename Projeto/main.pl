%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Individual 1- Sistema de recomendação de viagens.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Modulo para obter o dia da maquina.
% :- use_module(library(system), 
%         [datime/1,now/1]).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- 
:- op( 900,xfy,'::' ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Load dos ficheiros

:- include('grafo.pl').
:- include('baseDeConhecimento.pl').

% Funcoes auxiliares
:- include('str_process.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funcionalidades Obrigatorias.


% Calcular um trajeto entre dois pontos.




% Selecionar apenas algumas das operadoras de transporte para um determinado percurso;




% Excluir um ou mais operadores de transporte para o percurso;




% Identificar quais as paragens com o maior número de carreiras num determinado percurso.






% Escolher o menor percurso (usando critério menor número de paragens);




% Escolher o percurso mais rápido (usando critério da distância);





% Escolher o percurso que passe apenas por abrigos com publicidade;






% Escolher o percurso que passe apenas por paragens abrigadas;





% Escolher um ou mais pontos intermédios por onde o percurso deverá passar. 




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

existeParagem(Id,grafo(L,_)) :- memberchk(Id,L).


adj(X,Y) :- bagof(_,grafo(L1,L),[L|R]),
(memberchk(viajar(paragem(X,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Y,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L);
memberchk(viajar(paragem(Y,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(X,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L)).



% retorna os 2 adj(2 sentidos)
getAdj(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L),
   	   memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L) .





% Calcula o vertice mais perto(distancia em linha reta) do destino passando como parametro a as paragens adjacentes nos 2 sentidos, Cuidado que isto pode entrar em ciclo, tem de se gardar todos os vertices visitados. Ex:: A->B->C->B->C....
heur1(P1,P2,PD,P1):-calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1<R2. %print('P_antes:'),print(R1),print('\tP_despois:'),print(R2) .
heur1(P1,P2,PD,P2):-calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1>=R2.% print('P_antes:'),print(R1),print('\tP_despois:'),print(R2) .







% calcula a distancia entre uma viagem.
% calDist(_,0).
calDist(viajar(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)),R) :- 
	R is sqrt(((Lat -Lat1) * (Lat -Lat1)) + ((Long-Long1)*(Long-Long1))).




% calcula a distancia entre uma lista de viagens.
calDistViag([],R,R). 
calDistViag([X|XS],Acc,R):-calDist(X,S1) , Acc2 is (Acc + S1),calDistViag(XS,Acc2,R).
						  



% calcula o numero de paragens de um percurso .
calParagens([],R,R). 
calParagens([X|XS],Acc,R):- Acc2 is (Acc + 1), calParagens(XS,Acc2,R).









% findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),L),encontraCaminho(L,)


encontraCaminho(_,_,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),R).


encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),[XS]):-
	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_des,C_ori,NewC), 						     % Existe carreiras em comum.
	NewC \= nao,
	print('Mudei para a CARREIRA->'),print(NewC),print('\n'),
	mudarCarreira(NewC,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Prox),
	print(Prox),print('\n'),
	encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Prox,XS).



encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),[getId(Prox)|XS]):-
	Carr1 \= Carr2,
	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_des,C_ori,nao), 								 % Não existem carreiras em comum
	print('Carr-ORi:'),print(C_ori),print('\t CArr-destino:\t'),print(C_des),print('\t ID_Dest:'),print(Id2),print('----'),print(Carr2),print('\n'),
	getAdj(L_adj,Id2,Carr2,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	heur1(Par_ant,Par_seg,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Prox),
	print('\n\t'),print(Prox),
	encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Prox,XS).



encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr1,CodRua2,NomeRua2,Freguesia2),[XS]) :-
	print('Mesma Carr\n'),
	getAdj(L_adj,Id2,Carr2,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	heur1(Par_ant,Par_seg,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Prox),
	print(Prox),
	encontraCaminho(L_adj,TodasPar,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Prox,XS).



%bagof(_,grafo(L1,L),[L|R]),
%findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
%encontraCaminho(L,Todas,paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
%				paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',11,1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),CAminho). 






obterCarr(Id1,[],[]).
obterCarr(Id1,[paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)|R],Carr2):- atomic_list_concat_(L,',',Carr),map1(L,Carr2).
obterCarr(Id1,[paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)|R],Carr).
obterCarr(Id1,[paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)|R],Carr1):-  Id1 \= Id,obterCarr(Id1,R,Carr1).


pri_Comum(X,Y,nao):- integer(X),integer(Y),X \= Y.
pri_Comum(X,X,X):- integer(X).

pri_Comum(X,Y,X):-integer(X),memberchk(X,Y).
pri_Comum(X,Y,nao):-integer(X),\+ memberchk(X,Y).

pri_Comum(X,Y,Y):-integer(Y),memberchk(Y,X).
pri_Comum(X,Y,nao):-integer(Y),\+ memberchk(Y,X).



pri_Comum(_,[],nao).
pri_Comum([],_,nao).
pri_Comum([X|XS],L,X):- memberchk(X,L).
pri_Comum([X|XS],L,X1):- \+ memberchk(X,L),pri_Comum(XS,L,X1).



mudarCarreira(NovaC,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
	paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,NovaC,CodRua1,NomeRua1,Freguesia1)).


getId(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Id1).

%pri_Comum([1,2,3],[9,4,9,6,7,5,5],P).






%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% calDist(viajar(paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas')),R).
% calDistViag([viajar(paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas')),viajar(paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas')),viajar(paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas'),paragem(182,-103746.76,-96396.66,'Bom','Fechado dos Lados','Yes','Scotturb',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'))],0,R).
% calParagens([viajar(paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas')),viajar(paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas')),viajar(paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas'),paragem(182,-103746.76,-96396.66,'Bom','Fechado dos Lados','Yes','Scotturb',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'))],0,R).


% adj(791,183,[viajar(paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas')),viajar(paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas')),viajar(paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas'),paragem(182,-103746.76,-96396.66,'Bom','Fechado dos Lados','Yes','Scotturb',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'))]).
% adj(183,791,[viajar(paragem(183,-103678.36,-96590.26,'Bom','Fechado dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas')),viajar(paragem(791,-103705.46,-96673.6,'Bom','Aberto dos Lados','Yes','Vimeca',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'),paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas')),viajar(paragem(595,-103725.69,-95975.2,'Bom','Fechado dos Lados','Yes','Vimeca',1,354,'Rua Manuel Teixeira Gomes','Carnaxide e Queijas'),paragem(182,-103746.76,-96396.66,'Bom','Fechado dos Lados','Yes','Scotturb',1,286,'Rua Aquilino Ribeiro','Carnaxide e Queijas'))]).

% bagof(_,grafo(L1,L),[L|R]),getAdj(L,181,1,Par_ant,Par_seg). 
% findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),L),obterCarr(181,L,Carr). 


%heur1(paragem(365,-106016.12,-96673.87,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
%	  paragem(460,-106247.39,-96517.97,'Bom','Fechado dos Lados','Yes','Vimeca',1,1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
%	  paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',11,1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
%	   R).



%  mudarCarreira(40,paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),R).






%Teste 1.
%bagof(_,grafo(L1,L),[L|R]),
%findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
%encontraCaminho(L,Todas,paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
%				paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',11,1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),CAminho).







