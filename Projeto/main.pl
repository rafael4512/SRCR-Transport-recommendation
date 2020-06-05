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
:- include('evolucao.pl').
:- include('str_process.pl').
:- include('pesquisa.pl').
:- include('tempo.pl').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Funcionalidades Obrigatorias.


% Calcular um trajeto entre dois pontos.

calcula_Percurso(O,D):- bagof(_,grafo(L1,L),[L|R]),
						findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
							    paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
						getParagem(O,Par1),getParagem(D,Par2),
						encontraCaminho(L,Todas,Par1,Par2,[O],Caminho),
						write('\nPercurso :\n'),printl([O|Caminho]),write('\n').



% Selecionar apenas algumas das operadoras de transporte para um determinado percurso;
percurso_seleciona_op(OPs,O,D):-bagof(_,grafo(L1,L_aux),[L_aux|R]),seleciona_OP_Aux(L_aux,OPs,L),
				   			 	findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
				   						paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
				   				 getParagem(O,Par1),getParagem(D,Par2),
				   				write(Par1),write('\n'),
				   			 	encontraCaminho(L,Todas,Par1,Par2,[O],Caminho), length(Caminho,Tam),
				   			 	(memberchk(-1,Caminho)-> (write('\nNao foi possivel obter esse caminho.'),! , fail );
				   			 	allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
				   			 	write([O|Caminho]),write('\n'),
				   			 	(Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
							  	write('\nTempo estimado: '), write(Tempo),write(' minutos!\n'))).



% Excluir um ou mais operadores de transporte para o percurso;
percurso_exclui_op(OPs,O,D):-bagof(_,grafo(L1,L_aux),[L_aux|R]),excluir_OP(L_aux,OPs,L),
				   			 findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
				   					paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
				   			 getParagem(O,Par1),getParagem(D,Par2),
				   			 write(Par1),write('\n'),
				   			 encontraCaminho(L,Todas,Par1,Par2,[O],Caminho), length(Caminho,Tam),
				   			 (memberchk(-1,Caminho)-> (write('\nNao foi possivel obter esse caminho.'),! , fail );
				   			 allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
				   			 write([O|Caminho]),write('\n'),
				   			 (Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
							 write('\nTempo estimado: '), write(Tempo),write(' minutos!\n'))).




% Identificar quais as paragens com o maior número de carreiras num determinado percurso(Lista de GID). 
identificar_carr(L):- write('\nParagem\t->\t Carreiras\n'),identificar_carr_aux(L),write('\n').
identificar_carr_aux([]).
identificar_carr_aux([P|PS]):-findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
									  paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
							  obterCarr(P,Todas,L), write(P),write('\t->\t'),printONELine(L),
							  identificar_carr_aux(PS).






% Escolher o menor percurso (usando critério menor número de paragens);
percurso_menos_paragens(O,D):-bagof(_,grafo(L1,L),[L|R]),
							  findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
							  	    paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
							  getParagem(O,Par1),getParagem(D,Par2),
							  write(Par1),write('\n'),
							  encontraCaminho2(L,Todas,Par1,Par2,[O],Caminho),allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
							  write('\nPercurso com menos trocas de autocarros:\n'),printl([O|Caminho]),
							  (Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
							  write('\nTempo estimado: '), write(Tempo),write(' minutos!\n')).


% Escolher o percurso mais rápido (usando critério da distância);
percurso_mais_rapido(O,D):-bagof(_,grafo(L1,L),[L|R]),
						   findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
						   	       paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
						   getParagem(O,Par1),getParagem(D,Par2),
						   write(Par1),write('\n'),
						   encontraCaminho(L,Todas,Par1,Par2,[O],Caminho),allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
						   write('\nPercurso mais rapido!\n'),printl([O|Caminho]),
						   (Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
						   write('\nTempo estimado: '), write(Tempo),write(' minutos!\n')).



% Escolher o percurso que passe apenas por abrigos com publicidade; 
percurso_pub(O,D):-bagof(_,grafo(L1,L_aux),[L_aux|R]),excluir_ABR_SemPub(L_aux,L),
				   findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
				   		paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
				   getParagem(O,Par1),getParagem(D,Par2),
				   write(Par1),write('\n'),
				   encontraCaminho(L,Todas,Par1,Par2,[O],Caminho), length(Caminho,Tam),
				   (memberchk(-1,Caminho)-> (write('\nNao foi possivel obter esse caminho.'),! , fail );
				   allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
				   write([O|Caminho]),write('\n'),
				   (Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
				   write('\nTempo estimado: '), write(Tempo),write(' minutos!\n'))).



% Escolher o percurso que passe apenas por paragens abrigadas;
percurso_abrigado(O,D):-bagof(_,grafo(L1,L_aux),[L_aux|R]),seleciona_Abrigadas(L_aux,L),
						findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
								paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
						getParagem(O,Par1),getParagem(D,Par2),
						write(Par1),write('\n'),
				   		encontraCaminho(L,Todas,Par1,Par2,[O],Caminho), length(Caminho,Tam),
				   		(memberchk(-1,Caminho)-> (write('\nNao foi possivel obter esse caminho.'),! , fail );
				   		allPar([O|Caminho],PL),calcTempoViagem(PL,Tempo),
				   		write([O|Caminho]),write('\n'),
				   		(Tempo == fechado ->write('\nFora do Horario de funcionamento!\n'); 
				   		write('\nTempo estimado: '), write(Tempo),write(' minutos!\n'))).





% Escolher um ou mais pontos intermédios por onde o percurso deverá passar. 
percurso_inter(O,D,L_inter):-bagof(_,grafo(L1,L),[L|R]),
							findall(paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),
	   						paragem(Id1,Lat,Long,Estado,TipoAbrigo,AbrigoPub,Operadora,Carr,CodRua,NomeRua,Freguesia),Todas),
							write(O),write('\n'),pontos_inter(L,Todas,O,D,L_inter,RR).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% calcula_Percurso(27,366). 
% percurso_seleciona_op(['Vimeca'],242,30).
% percurso_exclui_op(['Vimeca','LT'],375,523).

%percurso_menos_paragens(251,227).
%percurso_mais_rapido(251,227).

%percurso_pub(13,667).
%percurso_pub(13,667).
%percurso_abrigado(795,827).

%percurso_inter(251,227,[44,45]).
%percurso_inter(336,337,[488]). 




