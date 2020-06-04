%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pesquisas na  base de conhecimento.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

getParagem(Id,Par):- findall(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
							 paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),[Par]).
existeParagem(Id,grafo(L,_)) :- memberchk(Id,L).


adj(X,Y) :- bagof(_,grafo(L1,L),[L|R]),
(memberchk(viajar(paragem(X,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Y,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L);
memberchk(viajar(paragem(Y,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(X,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L)).



% retorna os 2 adj(2 sentidos)
getAdj(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L),
   	   memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L) .


getAdj(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(-1,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L).
   	  

getAdj(L,O,Carr,paragem(-1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L) .






% Calcula o vertice mais perto(distancia em linha reta) do destino passando como parametro a as paragens adjacentes nos 2 sentidos, Cuidado que isto pode entrar em ciclo, tem de se gardar todos os vertices visitados. Ex:: A->B->C->B->C....




heur1(L,P1,P2,PD,P2):- getId(P1,-1).
heur1(L,P1,P2,PD,P1):- getId(P2,-1).

heur1(L,P1,P2,PD,P1):- getId(P2,Id2),memberchk(Id2,L).
heur1(L,P1,P2,PD,P2):- getId(P1,Id1),memberchk(Id1,L).

heur1(L,P1,P2,PD,P2):- getId(P1,X),getId(P2,X).

heur1(L,P1,P2,PD,P1):- calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1<R2.
heur1(L,P1,P2,PD,P2):- calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1>=R2.

%\+ memberchk(P2,L), \+ memberchk(P1,L),
%\+ memberchk(P2,L), \+ memberchk(P1,L),




% calcula a distancia entre uma viagem.
% calDist(_,0).
calDist(viajar(paragem(-1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)),R):-R is sqrt(100234102341).
calDist(viajar(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(-1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)),R):-R is sqrt(100234102341).
calDist(viajar(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)),R) :- 
	R is sqrt(((Lat -Lat1) * (Lat -Lat1)) + ((Long-Long1)*(Long-Long1))).




% calcula a distancia entre uma lista de viagens.
calDistViag([],R,R). 
calDistViag([X|XS],Acc,R):-calDist(X,S1) , Acc2 is (Acc + S1),calDistViag(XS,Acc2,R).
						  



% calcula o numero de paragens de um percurso .
calParagens([],R,R). 
calParagens([X|XS],Acc,R):- Acc2 is (Acc + 1), calParagens(XS,Acc2,R).





insere(X,[],[X]).
insere(X,L,[X|L]).




% findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),L),encontraCaminho(L,)

encontraCaminho(_,_,paragem(Id1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,R). % cheguei ao destino.



encontraCaminho(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,NewC), 						     % Existe uma carreira em comum.
	NewC \= nao,
	mudarCarreira(NewC,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Prox2),
	getAdj(L_adj,Id1,NewC,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	heur1(P,Par_ant,Par_seg,Prox2,P_Adj_Final),					 % Função heuristica.
	getId(P_Adj_Final,IDf),insere(IDf,P,Pf),					 % Obtem e Insere o ID da paragem ,nas listas
	print(P_Adj_Final),print('\n'),
	encontraCaminho(L_adj,TodasPar,P_Adj_Final,Prox2,Pf,XS).





encontraCaminho(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,nao),									 % Não existem carreiras em comum
	toList(C_ori,C_ori1),										 % garante que seja uma lista, pois pode ser um inteiro.
	map2(P,L_adj,Id1,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),C_ori1,Res),
	best_Choice(P,Res,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P_Adj_Final),
	getId(P_Adj_Final,IDf),insere(IDf,P,Pf),
	%print(P_Adj_Final),print('\n'),
	encontraCaminho(L_adj,TodasPar,P_Adj_Final,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Pf,XS).

%paragem(44,-104458.52,-94926.22,'Bom','Fechado dos Lados','Yes','Vimeca','01,13,15',1134,'Largo Sete de Junho de 1759','Carnaxide e Queijas').
%teste1(X):- pp(44,paragem(162,-102962.16,-98672.14,'Bom','Sem Abrigo','No','Vimeca','02,06,13',830,'Estrada Militar','Barcarena'),[13,15,1],X).

%pp(Par_ori,Par_Dest,Carr,P_Adj_Final):-bagof(_,grafo(L1,L),[L|R]),map2([],L,Par_ori,Par_Dest,Carr,Res),best_Choice([],Res,Par_Dest,FINAL).


map2(Percorr,L_adj,Par_ori,Par_Dest,[],R). 

map2(Percorr,L_adj,Par_ori,Par_Dest,[X|XS],[Par_seg|FS]):- 
	getAdj(L_adj,Par_ori,X,Par_ant,Par_seg), 
	getId(Par_ant,-1),
	%print(Fin),
	map2(Percorr,L_adj,Par_ori,Par_Dest,XS,FS).

map2(Percorr,L_adj,Par_ori,Par_Dest,[X|XS],[Par_ant|FS]):- 
	getAdj(L_adj,Par_ori,X,Par_ant,Par_seg), 
	getId(Par_seg,-1),
	%print(Fin),
	map2(Percorr,L_adj,Par_ori,Par_Dest,XS,FS).

map2(Percorr,L_adj,Par_ori,Par_Dest,[X|XS],[Fin|FS]):- 
	getAdj(L_adj,Par_ori,X,Par_ant,Par_seg), 
	heur1(Percorr,Par_ant,Par_seg,Par_Dest,Fin),
	map2(Percorr,L_adj,Par_ori,Par_Dest,XS,FS).

best_Choice(Percorr,[F1],Par_Dest,F1).
best_Choice(Percorr,[F1,F2|FS],Par_Dest,Fin1) :-
	heur1(Percorr,F1,F2,Par_Dest,Fin),
	best_Choice(Percorr,[Fin|FS],Par_Dest,Fin1).






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
getCarr(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Carr1).


toList([],[]).
toList(X,[X]):- integer(X).
toList(X,X).



printl([X]):- write(X).
printl([Y|Tail]):-write(Y),write('\n'),printl(Tail).


printONE([X]):- write(X),write(']\n').
printONE([Y|Tail]):-write(Y),write(','),printONE(Tail).

printONELine(L):-write('['),printONE(L).








pontos_inter(L,Todas,O,D,[],SOL):- percurso_mais_rapido(O,D).%getParagem(O,Par1), getParagem(D,Par2), encontraCaminho(L,Todas,Par1,Par2,[O],SOL).

pontos_inter(L,Todas,O,D,[X|XS],SOL):-percurso_mais_rapido(O,X),pontos_inter(L,Todas,O,D,XS,SOL).	%getParagem(O,Par1),getParagem(X,Par2),
										%encontraCaminho(L,Todas,Par1,Par2,[O],Caminho),
										%pontos_inter(X,D,XS,CAminho2),
										%append(Caminho,CAminho2,SOL).
							





%au(R):-bagof(_,grafo(L1,L),[L|R]),
%		findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
%	   			paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
%		pontos_inter(L,Todas,251,227,[44,45],R).



%indexOf(Todas,44,123,0,A,B,C).
indexOf([],O,D,Carr,C,A,B).
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,A,Conta):- getId(X1,D), getCarr(X1,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,A,Conta). 
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,Conta,B):- getId(X1,O), getCarr(X1,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,Conta,B).  
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,A,B):- Contador is Conta+1, indexOf(XS,O,D,Carr,Contador,A,B).



lol(A,B):-bagof(_,grafo(L1,L),[L|R]),
		indexOf(L,51,622,1,0,A,B).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%pri_Comum([1,2,3],[9,4,9,6,7,5,5],P).
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
%bagof(_,grafo(L1,L),[L|R]),getAdj(L,859,11,Par_ant,Par_seg). 



teste1(Caminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(859,-105043.39,-96109.56,'Bom','Fechado dos Lados','Yes','Vimeca','02,11,13',1283,'Avenida Vinte e Cinco de Abril de 1974','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
paragem(488,-106492.31,-96447.01,'Bom','Sem Abrigo','No','Vimeca',11,1292,'Rua Manuel Ferreira','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),[859],Caminho),printl(Caminho).


teste2(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(27,-105587.02,-95875.21,'Bom','Fechado dos Lados','Yes','Vimeca',13,430,'Avenida Carolina Michaelis','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
				paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),[27],CAminho),printl(CAminho).


teste3(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(251,-104487.69,-96548.01,'Bom','Fechado dos Lados','Yes','Vimeca',1,1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas'),
				paragem(227,-104412.8,-98632.87,'Bom','Sem Abrigo','No','Vimeca','02,06,13',805,'Rua Ilha de Sao Jorge','Carnaxide e Queijas'),[],CAminho),printl(CAminho).


teste4(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
				paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',11,1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),[366],CAminho),printl(CAminho).



