%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Pesquisas na  base de conhecimento.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


%Get de uma paragem.
getParagem([],Id,paragem(-1,_,_,_,_,_,_,_,_,_,_) ).
getParagem([X|XS],Id,X) :- getId(X,Id),!.
getParagem([X|XS],Id,Res) :- getParagem(XS,Id,Res).
getParagem(Id,Par):- findall(paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
							 paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),[Par]).

%verifica se existe uma paragem no grafo.
existeParagem(Id,grafo(L,_)) :- memberchk(Id,L).

%Verifica se 2 vertices/paragens são adjacentes.
adj(X,Y) :- bagof(_,grafo(L1,L),[L|R]),
(memberchk(viajar(paragem(X,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Y,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L);
memberchk(viajar(paragem(Y,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(X,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1)),L)).



% retorna os 2 adj(2 sentidos)
getAdj(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L),
   	   memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L).
   	    

getAdj(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(-1,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L).
   	  

getAdj(L,O,Carr,paragem(-1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L) .



% retorna os 2 adj e remove essas viagens do grafo.
getAdj_Rem(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Todas)
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L),
   	   memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L),
   	    delete(L,viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L2),
   	     delete(L2,viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),Todas).


getAdj_Rem(L,O,Carr,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(-1,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)),L).
   	  

getAdj_Rem(L,O,Carr,paragem(-1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2))
	:- memberchk(viajar(paragem(O,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)),L) .






% Calcula o vertice mais perto(distancia em linha reta) do destino passando como parametro a as paragens adjacentes nos 2 sentidos, Cuidado que isto pode entrar em ciclo, tem de se gardar todos os vertices visitados. Ex:: A->B->C->B->C....

heur1(L,P1,P2,PD,P2):- getId(P1,-1).
heur1(L,P1,P2,PD,P1):- getId(P2,-1).

heur1(L,P1,P2,PD,P1):- getId(P2,Id2),memberchk(Id2,L).
heur1(L,P1,P2,PD,P2):- getId(P1,Id1),memberchk(Id1,L).

heur1(L,P1,P2,PD,P2):- getId(P1,X),getId(P2,X).

heur1(L,P1,P2,PD,P1):- calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1<R2.
heur1(L,P1,P2,PD,P2):- calDist(viajar(P1,PD),R1) , calDist(viajar(P2,PD),R2) , R1>=R2.



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




%insere à cabeça de uma lista.
insere(X,[],[X]).
insere(X,L,[X|L]).





%--------------------------------- 
%Função que encontra um caminho no grafo.

encontraCaminho(_,_,paragem(Id1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,R). % cheguei ao destino.
encontraCaminho(_,_,paragem(-1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,[]).% qd remove no arestas do grafo, necessito deste caso de paragem.


encontraCaminho(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,NewC), 						    	 % Existe uma carreira em comum.
	NewC \= nao,
	mudarCarreira(NewC,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Prox2),
	getAdj(L_adj,Id1,NewC,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	upOrDown(L_adj,Id1,Id2,NewC,A,B),							 % o A,B são posicoes da lista de adj. 
	integer(A),integer(B),							
	( A<B -> (getId(Par_seg,IDf) ,print(Par_seg),print('\n'), insere(IDf,P,Pf),encontraCaminho(L_adj,TodasPar,Par_seg,Prox2,Pf,XS))
	; (getId(Par_ant,IDf) ,print(Par_ant),print('\n'), insere(IDf,P,Pf),encontraCaminho(L_adj,TodasPar,Par_ant,Prox2,Pf,XS)) ).





encontraCaminho(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,nao),									 % Não existem carreiras em comum
	toList(C_ori,C_ori1),										 % garante que seja uma lista, pois pode ser um inteiro.
	map2(P,L_adj,Id1,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),C_ori1,Res),
	best_Choice(P,Res,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P_Adj_Final),
	getId(P_Adj_Final,IDf),insere(IDf,P,Pf),
	print(P_Adj_Final),print('\n'),
	encontraCaminho(L_adj,TodasPar,P_Adj_Final,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Pf,XS).

%--------------------------------- 
%igual à de cima sem prints.

encontraCaminhoS(_,_,paragem(Id1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,R). % cheguei ao destino.
encontraCaminhoS(_,_,paragem(-1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,[]).% qd remove no arestas do grafo, necessito deste caso de paragem.
encontraCaminhoS(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,NewC), 						    	 % Existe uma carreira em comum.
	NewC \= nao,
	mudarCarreira(NewC,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Prox2),
	getAdj(L_adj,Id1,NewC,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	upOrDown(L_adj,Id1,Id2,NewC,A,B),							 % o A,B são posicoes da lista de adj. 
	integer(A),integer(B),							
	( A<B -> (getId(Par_seg,IDf) , insere(IDf,P,Pf),encontraCaminhoS(L_adj,TodasPar,Par_seg,Prox2,Pf,XS))
	; (getId(Par_ant,IDf) ,insere(IDf,P,Pf),encontraCaminhoS(L_adj,TodasPar,Par_ant,Prox2,Pf,XS)) ).
encontraCaminhoS(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-

	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,nao),									 % Não existem carreiras em comum
	toList(C_ori,C_ori1),										 % garante que seja uma lista, pois pode ser um inteiro.
	map2(P,L_adj,Id1,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),C_ori1,Res),
	best_Choice(P,Res,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P_Adj_Final),
	getId(P_Adj_Final,IDf),insere(IDf,P,Pf),
	encontraCaminhoS(L_adj,TodasPar,P_Adj_Final,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Pf,XS).


%--------------------------------- 
%Função que encontra um caminho no grafo, com um algoritmo diferente à de cima.
%--------------------------------- 
%Faz o Print das paragens!
encontraCaminho2(_,_,paragem(Id1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,R). % cheguei ao destino.
encontraCaminho2(_,_,paragem(-1,_,_,_,_,_,_,_,_,_,_),paragem(Id1,_,_,_,_,_,_,_,_,_,_),_,[]). % Nao existe caminho.
encontraCaminho2(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-
	%print('\n\t1\n'),
	integer(Carr),
	getAdj(L_adj,Id1,Carr,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.

	upOrDown(L_adj,Id1,Id2,Carr,A,B),							 % o A,B são posicoes da lista de adj. 
	integer(A),integer(B),							
	( A<B -> (getId(Par_seg,IDf) ,print(Par_seg),print('\n'), insere(IDf,P,Pf),encontraCaminho2(L_adj,TodasPar,Par_seg,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr,CodRua2,NomeRua2,Freguesia2),Pf,XS))
	;(getId(Par_ant,IDf) , print(Par_ant),print('\n'), insere(IDf,P,Pf), encontraCaminho2(L_adj,TodasPar,Par_ant,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr,CodRua2,NomeRua2,Freguesia2),Pf,XS)) ).

encontraCaminho2(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-
	%print('\n\t2\n'),
	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,NewC), 						     % Existe uma carreira em comum.
	%write('ENCONTRACAMINHO2\tCARR:'),write(NewC),write('\tC_ORI'),write(C_ori),write('\tID1'),write(Id1),write('\t'),
	NewC \= nao,
	mudarCarreira(NewC,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Prox2),
	getAdj(L_adj,Id1,NewC,Par_ant,Par_seg), 					 % Obtem as paragens adjacentes.
	upOrDown(L_adj,Id1,Id2,NewC,A,B),
	integer(A),integer(B),
	( A<B -> (getId(Par_seg,IDf),\+ memberchk(IDf,P) ,print(Par_seg),print('\n'), insere(IDf,P,Pf),encontraCaminho2(L_adj,TodasPar,Par_seg,Prox2,Pf,XS))
	; (getId(Par_ant,IDf),\+ memberchk(IDf,P), print(Par_ant),print('\n'), insere(IDf,P,Pf),encontraCaminho2(L_adj,TodasPar,Par_ant,Prox2,Pf,XS)) ).





encontraCaminho2(L_adj,TodasPar, 
				paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
				paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P,[IDf|XS]):-
	%print('\n\t3\n'),
	obterCarr(Id1,TodasPar,C_ori),obterCarr(Id2,TodasPar,C_des), % Obtem as carreiras.
	pri_Comum(C_ori,C_des,nao),									 % Não existem carreiras em comum
	toList(C_ori,C_ori1),										 % garante que seja uma lista, pois pode ser um inteiro.
	map2(P,L_adj,Id1,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),C_ori1,Res),
	best_Choice(P,Res,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),P_Adj_Final),
	getId(P_Adj_Final,IDf),\+ memberchk(IDf,P),insere(IDf,P,Pf),
	print(P_Adj_Final),print('\n'),
	encontraCaminho2(L_adj,TodasPar,P_Adj_Final,paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2),Pf,XS).



%--------------------------------- 


% todosCaminhos(L_adj,TodasPar,O,D,Res):-obterCarr(O,TodasPar,C_ori),obterCarr(D,TodasPar,C_des),pri_Comum(C_ori,C_des,nao).
% todosCaminhos(L_adj,TodasPar,O,D,R):-getParagem(TodasPar,O,Par1),getParagem(TodasPar,D,Par2),write('AH\n'),encontraCaminho2(L_adj,TodasPar,Par1,Par2,[],Sol),
% 									memberchk(-1,Sol),
% 									obterCarr(O,TodasPar,C_ori),obterCarr(D,TodasPar,C_des),
% 									pri_Comum(C_ori,C_des,NewC),
% 									write(C_ori),print('\t'),write(C_des),print('\t<-1\n'),
% 									%write('Eliminei a Carr_:'),write(NewC),write('\n'),
% 									deleteCarr(NewC,TodasPar,Par1,NewBC),
% 									todosCaminhos(L_adj,NewBC,O,D,R).
% todosCaminhos(L_adj,TodasPar,O,D,[Sol|R]):-getParagem(TodasPar,O,Par1),getParagem(TodasPar,D,Par2),encontraCaminho2(L_adj,TodasPar,Par1,Par2,[],Sol),
% 											todosCaminhos(L_adj,TodasPar,O,D,R).
% pp(X):-bagof(_,grafo(L1,L),[L|R]),
% 		findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
% 		todosCaminhos(L,Todas,183,181,X),write('PP->->'),write(X).
% 

%--------------------------------- 

%obtem todas as paragens adjacentes de todas as carreiras, para escolher a melhor.
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



%funçoes que faz a escolha das viagens dependendo da heuristica.
best_Choice(Percorr,[F1],Par_Dest,F1).
best_Choice(Percorr,[F1,F2|FS],Par_Dest,Fin1) :-
	heur1(Percorr,F1,F2,Par_Dest,Fin),
	best_Choice(Percorr,[Fin|FS],Par_Dest,Fin1).

%obtem as carreiras de uma paragem.
obterCarr(Id1,[],[]).
obterCarr(Id1,[paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)|R],Carr).
obterCarr(Id1,[paragem(Id,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia)|R],Carr1):-  Id1 \= Id,obterCarr(Id1,R,Carr1).

%Devolve as carreiras em comum dado uma origem e um destino.
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


%muda a carreira de uma paragem
mudarCarreira(NovaC,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
	paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,NovaC,CodRua1,NomeRua1,Freguesia1)).

%Funções para fazer o get
getId(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Id1).
getCarr(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Carr1).
getOperadora(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Operadora1).
getAbrPub(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),AbrigoPub1).
getTipoAbr(paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),TipoAbrigo1).


%converte um inteiro numa lista ou devolve o que recebe.
toList([],[]).
toList(X,[X]):- integer(X).
toList(X,X).


%Prints personalizados.
printl([X]):- write(X).
printl([Y|Tail]):-write(Y),write('\n'),printl(Tail).


printONE([X]):- write(X),write(']\n').
printONE([Y|Tail]):-write(Y),write(','),printONE(Tail).

printONELine(L):-write('['),printONE(L).



%função que calcula um caminho com pontos intermedios.
pontos_inter(L,Todas,O,D,[],SOL):- pontos_inter_aux(O,D).
pontos_inter(L,Todas,O,D,[X|XS],SOL):-pontos_inter_aux(O,X),write('->Paragem\n'),pontos_inter(L,Todas,X,D,XS,SOL).	

							
pontos_inter_aux(O,D):-bagof(_,grafo(L1,L),[L|R]),
					   findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
					   	    paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
					   getParagem(O,Par1),getParagem(D,Par2),
					   encontraCaminhoS(L,Todas,Par1,Par2,[O],Caminho),
					   printl(Caminho).






%indexOf(Todas,44,123,0,A,B,C).
indexOf([],O,D,Carr,C,A,B).
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,A,Conta):- getId(X1,D), getCarr(X1,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,A,Conta). 
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,Conta,B):- getId(X1,O), getCarr(X1,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,Conta,B).  
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,A,Conta):- getId(X2,D), getCarr(X2,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,A,Conta). 
indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,Conta,B):- getId(X2,O), getCarr(X2,Carr),Contador is Conta+1,indexOf(XS,O,D,Carr,Contador,Conta,B).  

indexOf([viajar(X1,X2)|XS],O,D,Carr,Conta,A,B):- Contador is Conta+1, indexOf(XS,O,D,Carr,Contador,A,B).


%predicado para  saber o sentido da rota, para cima da paragem atual ou para baixo da paragem atual
upOrDown(L,O,D,Carr,A,B):-indexOf(L,O,D,Carr,0,A,B).
 



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover do grafo.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% Funcao auxliar para selecionar viagens de  operadoras dadas.
seleciona_OP_Aux([],L_OP,R).
seleciona_OP_Aux([viajar(X1,X2)|XS],L_OP,[viajar(X1,X2)|PP]) :- getOperadora(X1,A),getOperadora(X2,B), 
																memberchk(A,L_OP),memberchk(B,L_OP),	% são membros
																seleciona_OP_Aux(XS,L_OP,PP).
seleciona_OP_Aux([viajar(X1,X2)|XS],L_OP,PP) :- seleciona_OP_Aux(XS,L_OP,PP).




% Funcao auxliar para remover viagens de  operadoras dadas.
excluir_OP([],L_OP,R).
excluir_OP([viajar(X1,X2)|XS],L_OP,PP) :- getOperadora(X1,A),getOperadora(X2,B),
									      (memberchk(A,L_OP);memberchk(B,L_OP)),
									      excluir_OP(XS,L_OP,PP).
excluir_OP([viajar(X1,X2)|XS],L_OP,[viajar(X1,X2)|PP]) :- excluir_OP(XS,L_OP,PP).




%exclui todas as viagens sem publicidade.
excluir_ABR_SemPub([],[]).
excluir_ABR_SemPub([viajar(X1,X2)|XS],Res):- (getAbrPub(X1,'No');getAbrPub(X2,'No')),excluir_ABR_SemPub(XS,Res) .
excluir_ABR_SemPub([viajar(X1,X2)|XS],[viajar(X1,X2)|Res]):- excluir_ABR_SemPub(XS,Res) .


%exclui todas as viagens que não são abrigadas.('Sem Abrigo')
seleciona_Abrigadas([],[]).
seleciona_Abrigadas([viajar(X1,X2)|XS],Res):- (getTipoAbr(X1,'Sem Abrigo'); getTipoAbr(X2,'Sem Abrigo')),seleciona_Abrigadas(XS,Res).
seleciona_Abrigadas([viajar(X1,X2)|XS],[viajar(X1,X2)|Res]):- seleciona_Abrigadas(XS,Res).



%função que remove um elemento de uma lista.
delete([], _, []).
delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        delete(Tail, Del, Rest)
    ).

%remove uma viagem do grafo.
removerViagem(L,V,R):-delete(L,V,R).

%remove uma carreira de uma paragem.
deleteCarr(_,[],_,[]).
deleteCarr(C,[paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1)|XS],
	paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
	[paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr2,CodRua1,NomeRua1,Freguesia1)|XS]):- delete(Carr1,C,Carr2).

deleteCarr(C,[paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)|XS],
	paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),
	[paragem(Id2,Lat2,Long2,Estado2,TipoAbrigo2,AbrigoPub2,Operadora2,Carr2,CodRua2,NomeRua2,Freguesia2)|Res]):-deleteCarr(C,XS,paragem(Id1,Lat1,Long1,Estado1,TipoAbrigo1,AbrigoPub1,Operadora1,Carr1,CodRua1,NomeRua1,Freguesia1),Res).


%remove o ultimo elemento de uma lista.
remLast([],[]).
remLast([X],[]).
remLast([X|XS],[X|P]):-remLast(XS,P).

%retorna todas as paragens passando uma lista de GID das paragens.
allPar([],[]).
allPar([X|XS],[P|PS]):- getParagem(X,P),allPar(XS,PS).



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


teste3_1(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(251,-104487.69,-96548.01,'Bom','Fechado dos Lados','Yes','Vimeca',1,1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas'),
				paragem(227,-104412.8,-98632.87,'Bom','Sem Abrigo','No','Vimeca','02,06,13',805,'Rua Ilha de Sao Jorge','Carnaxide e Queijas'),[251],CAminho),printl(CAminho).

teste3_2(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho2(L,Todas,paragem(251,-104487.69,-96548.01,'Bom','Fechado dos Lados','Yes','Vimeca',1,1279,'Avenida Tomas Ribeiro','Carnaxide e Queijas'),
				paragem(227,-104412.8,-98632.87,'Bom','Sem Abrigo','No','Vimeca','02,06,13',805,'Rua Ilha de Sao Jorge','Carnaxide e Queijas'),[251],CAminho),printl(CAminho).


teste4(CAminho):- bagof(_,grafo(L1,L),[L|R]),
findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
encontraCaminho(L,Todas,paragem(366,-106021.37,-96684.5,'Bom','Fechado dos Lados','Yes','Vimeca',1,411,'Avenida Dom Pedro V','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),
				paragem(337,-105713.9,-96309.68,'Bom','Aberto dos Lados','No','Vimeca',11,1251,'Rua Pedro Alvares Cabral','Alges, Linda-a-Velha e Cruz Quebrada-Dafundo'),[366],CAminho),printl(CAminho).




teste_seleciona_OP_Aux(X):-bagof(_,grafo(L1,L),[L|R]),seleciona_OP_Aux(L,['Scotturb'],X).

teste_exclui_OP(X):-bagof(_,grafo(L1,L),[L|R]),excluir_OP(L,['Scotturb'],X).


teste_excluir_ABR_SemPub(X):-bagof(_,grafo(L1,L),[L|R]),excluir_ABR_SemPub(L,X).


teste_seleciona_Abrigadas(X):-bagof(_,grafo(L1,L),[L|R]),seleciona_Abrigadas(L,X).




%%Teste dos pontos intermedios.
au(R):-bagof(_,grafo(L1,L),[L|R]),
		findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
	   			paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
		pontos_inter(L,Todas,251,227,[44,45],R).

