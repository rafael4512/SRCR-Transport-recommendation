%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calculos relativos aos tempos de viagem
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
:- dynamic funcionamento/4.
:- dynamic hora_atual/2.


%Predicado que indicao horario de funcionamento.
funcionamento(6,0,24,0).

%Predicado que indica a hora de ponta.
hora_ponta(13,0,14,0).
hora_ponta(17,30,18,30).

%Predicado que indica a hora atual.
hora_atual(6,39).

%calcula o tempo entre paragens em minutos.
calcTempoViagem([],0).
calcTempoViagem(L,Res):-length(L,TAM),Tp is TAM*2, verificaAbertura(sim),agrupaParagem2(L,Via),coef_hora_ponta(Min),calDistViag(Via,0,R1),Res is round( Tp + (Min + (R1/100))).%,write(R1).
calcTempoViagem(L,fechado).




%Retorna os minutos em espera devido ao transito (Hora de ponta),
coef_hora_ponta(0):- findall(hora_ponta(H,M,H1,M1),hora_ponta(H,M,H1,M1),[S,S1]),
					bagof((H3,M3),hora_atual(H3,M3),[(H_a,M_a)]),
					nao(processaL(H_a,M_a,[S,S1],X)).


coef_hora_ponta(MIN):- findall(hora_ponta(H,M,H1,M1),hora_ponta(H,M,H1,M1),[S,S1]),
					bagof((H3,M3),hora_atual(H3,M3),[(H_a,M_a)]),
					processaL(H_a,M_a,[S,S1],AUX),
					coef(AUX,MIN).


%calcula o valor em minutos entre a hora atual e a hora de ponta.
processaL(H,M,[hora_ponta(H1,M1,H2,M2),hora_ponta(H3,M3,H4,M4)],Aux):-  min1(H,M,X),min1(H1,M1,X1),  Aux is (X1-X) , Aux>= 0,Aux=<60. 
processaL(H,M,[hora_ponta(H1,M1,H2,M2),hora_ponta(H3,M3,H4,M4)],Aux):-  min1(H,M,X),min1(H2,M2,X1),  Aux is (X1-X) , Aux>= 0,Aux=<60. 
processaL(H,M,[hora_ponta(H1,M1,H2,M2),hora_ponta(H3,M3,H4,M4)],Aux):-  min1(H,M,X),min1(H3,M3,X1),  Aux is (X1-X) , Aux>= 0,Aux=<60. 
processaL(H,M,[hora_ponta(H1,M1,H2,M2),hora_ponta(H3,M3,H4,M4)],Aux):- 	min1(H,M,X),min1(H4,M4,X1),  Aux is (X1-X) , Aux>= 0,Aux=<60. 
-processaL(H,M,[hora_ponta(H1,M1,H2,M2),hora_ponta(H3,M3,H4,M4)],-1).
																		

%tranforma as horas em minutos.
min1(H1,M1,X):- X is (H1*60 + M1). 




% coeficiente que determina os mintos em espera, X esta entre 0 e 60 sempre.Tempo max em espera é 15 min por paragem.
coef(X,R):- X =< 30, R is (X*0.5).
coef(X,R):- R is ((60-R) * 0.5).



%verifica se está na hora de abertura.
verificaAbertura(sim):- bagof((H3,M3),hora_atual(H3,M3),[(H_a,M_a)]),
						bagof((A,B,C,D),funcionamento(A,B,C,D),[(H,M,H1,M1)]),
						min1(H_a,M_a,MM),
						min1(H,M,Fi),min1(H1,M1,FF),
						MM>=Fi , MM=<FF.
verificaAbertura(nao).
	






%agrupa paragens em viagen.
agrupaParagem2([],[]).
agrupaParagem2([X],[]).
agrupaParagem2([X,Y|XS],[viajar(X,Y)|VS]):-agrupaParagem2(XS,PS).




%Predicado que muda a hora atual.
mudaHora(H,M):- H < 24,H>=0,  M<60,M>=0,involucao(hora_atual(X,Y)),evolucao(hora_atual(H,M)).
mudaHora(H,M):- write('Insira uma hora possivel!'),!,fail.









