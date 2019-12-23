% FABRICA

% Linhas
linhas([lA]).


% Maquinas
maquinas([ma,mb,mc,md,me]).



% Ferramentas
ferramentas([fa,fb,fc,fd,fe,ff,fg,fh,fi,fj]).


% Maquinas que constituem as Linhas
tipos_maq_linha(lA,[ma,mb,mc,md,me]).


% Operacoes
tipo_operacoes([opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8,opt9,opt10]).

% operacoes/1 vai ser criado dinamicamente
%no exemplo dara' uma lista com 30 operacoes 6 lotes de produtos * 5 operacoes por produto

%operacoes_atrib_maq/2 vai ser criado dinamicamente
%no exemplo cada maquina tera' 6 operacoes atribuidas, uma por cada lote de produtos

% classif_operacoes/2 deve ser criado dinamicamente 
%no exemplo teremos 30 factos deste tipo, um para cada operacao

% Afetacao de tipos de operacoes a tipos de maquinas
% com ferramentas, tempos de setup e tempos de execucao)

operacao_maquina(opt1,ma,fa,1,1).
operacao_maquina(opt2,mb,fb,2.5,2).
operacao_maquina(opt3,mc,fc,1,3).
operacao_maquina(opt4,md,fd,1,1).
operacao_maquina(opt5,me,fe,2,3).
operacao_maquina(opt6,mb,ff,1,4).
operacao_maquina(opt7,md,fg,2,5).
operacao_maquina(opt8,ma,fh,1,6).
operacao_maquina(opt9,me,fi,1,7).
operacao_maquina(opt10,mc,fj,20,2).


% PRODUTOS

produtos([pA,pB,pC,pD,pE,pF]).

operacoes_produto(pA,[opt1,opt2,opt3,opt4,opt5]).
operacoes_produto(pB,[opt1,opt6,opt3,opt4,opt5]).
operacoes_produto(pC,[opt1,opt2,opt3,opt7,opt5]).
operacoes_produto(pD,[opt8,opt2,opt3,opt4,opt5]).
operacoes_produto(pE,[opt1,opt2,opt3,opt4,opt9]).
operacoes_produto(pF,[opt1,opt2,opt10,opt4,opt5]).



% ENCOMENDAS

%Clientes

clientes([clA,clB,clC,clD]).


% prioridades dos clientes

prioridade_cliente(clA,3).
prioridade_cliente(clB,1).
prioridade_cliente(clC,2).
prioridade_cliente(clD,4).



% Encomendas do cliente, 
% termos e(<produto>,<n.unidades>,<tempo_conclusao>)

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,1,198)]).
encomenda(clC,[e(pE,4,60)]).
encomenda(clD,[e(pA,2,180),e(pE,3,500),e(pD,4,700)]).


% cria_op_enc - fizeram-se correcoes face a versao anterior

:- dynamic operacoes_atrib_maq/2.
:- dynamic classif_operacoes/2.
:- dynamic op_prod_client/9.
:- dynamic operacoes/1.
:- dynamic encomendas/1.


cria_op_enc:-
retractall(operacoes(_)),
retractall(operacoes_atrib_maq(_,_)),
retractall(classif_operacoes(_,_)),
retractall(op_prod_client(_,_,_,_,_,_,_,_,_)),
retractall(encomendas(_)),
		findall(t(Cliente,Prod,Qt,TConc),
		(encomenda(Cliente,LE),member(e(Prod,Qt,TConc),LE)),
		LT),cria_ops(LT,0),asserta(encomendas(LT)),
findall(Op,classif_operacoes(Op,_),LOp),asserta(operacoes(LOp)),
maquinas(LM),
 findall(_,
		(member(M,LM),
		 findall(Opx,op_prod_client(Opx,M,_,_,_,_,_,_,_),LOpx),
		 assertz(operacoes_atrib_maq(M,LOpx))),_).

cria_ops([],_).
cria_ops([t(Cliente,Prod,Qt,TConc)|LT],N):-
			operacoes_produto(Prod,LOpt),
	cria_ops_prod_cliente(LOpt,Cliente,Prod,Qt,TConc,N,N1),
			cria_ops(LT,N1).


cria_ops_prod_cliente([],_,_,_,_,Nf,Nf).
cria_ops_prod_cliente([Opt|LOpt],Client,Prod,Qt,TConc,N,Nf):-
		cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni),
	cria_ops_prod_cliente(LOpt,Client,Prod,Qt,TConc,Ni,Nf).


cria_ops_prod_cliente2(Opt,Prod,Client,Qt,TConc,N,Ni):-
			Ni is N+1,
			atomic_concat(op,Ni,Op),
			assertz(classif_operacoes(Op,Opt)),
			operacao_maquina(Opt,M,F,Tsetup,Texec),
	assertz(op_prod_client(Op,M,F,Prod,Client,Qt,TConc,Tsetup,Texec)).

%-------------------------------------Tarefas-------------------------------------%

:-dynamic tarefa/4.
:-dynamic tarefas/1.

cria_tarefas:- 
	retractall(tarefas(_)),
	retractall(tarefa(_,_,_,_)),
	cria_op_enc(),
	encomendas(LE),
	cria_tarefas(LE,0).

cria_tarefas([],NTarefas) :- asserta(tarefas(NTarefas)),!.
cria_tarefas([H|T],NTarefas):- cria_tarefa(H,NTarefas),N1 is NTarefas + 1,cria_tarefas(T,N1).

cria_tarefa(t(Cliente,Prod,Qt,TConc),NTarefa):-
	calcula_makespan(Cliente,Prod,Qt,MakeSpan),
	atomic_concat('t',NTarefa,Tarefa),
	prioridade_cliente(Cliente,Prioridade),calcula_penalizacao(Prioridade,Penalizacao),
	write(Tarefa),write(' --> '),write(Cliente),write(', '),write(Prod),write(', Quantidade  -> '),write(Qt),write(', Penalizacao -> '),write(Penalizacao),write(', Tempo de Conlusao -> '),write(TConc),write(', ['),write(MakeSpan),write(']'),nl,
	assertz(tarefa(Tarefa,MakeSpan,TConc,Penalizacao)).

calcula_makespan(Cliente,Prod,Qt,MakeSpan):-
	findall(s(Texec,Tsetup),op_prod_client(_,_,_,Prod,Cliente,_,_,Tsetup,Texec),LSS),
	soma_valores_setup(LSS,Setup),
	findall(Texec,op_prod_client(_,_,_,Prod,Cliente,_,_,_,Texec),LOPT),
	sort(0, @>, LOPT, LOPTO),
	soma_valores_makespan(LOPTO,Qt,MakeSpan,Setup).


soma_valores_setup([s(Texec,Tsetup)|T],Setup):-
	soma_valores_setup([s(Texec,Tsetup)|T],0,0,NegativeSetup),Setup is -NegativeSetup.

soma_valores_setup([],_,Setup,Setup):-!.
soma_valores_setup([s(Texec,Tsetup)|T],TempoActual,Setup,Resultado):-
	NSetup is TempoActual - Tsetup,
	NTempoActual is TempoActual + Texec,
	(
		(NSetup<Setup,!,soma_valores_setup(T,NTempoActual,NSetup,Resultado))
		;
		soma_valores_setup(T,NTempoActual,Setup,Resultado)
	).

soma_valores_makespan([TSoma|T],Qt,Makespan,Setup):-
	ValorOpCadencia is TSoma * Qt,
	soma_lista_valores(T,Soma),
	Makespan is ValorOpCadencia + Soma + Setup.

soma_lista_valores([],0):-!.

soma_lista_valores([Texec|T],Soma):-soma_lista_valores(T,Valor),Soma is Valor + Texec.

calcula_penalizacao(Prioridade,Penalizacao):-Penalizacao is 0.9 + Prioridade / 10.
%-------------------------------------Heuristicas adaptadas a Tarefas--------------------------%

:-dynamic melhor_solucao/1.
%Calculo da melhor solucao

% permuta/2 gera permutações de listas
permuta([ ],[ ]).
permuta(L,[X|L1]):-apaga1(X,L,Li),permuta(Li,L1).

apaga1(X,[X|L],L).
apaga1(X,[Y|L],[Y|L1]):-apaga1(X,L,L1).

permuta_tempo(LP,Tempo):- findall(T,tarefa(T,_,_,_),LT),
permuta(LT,LP),soma_tempos_atraso(LP,NTempo),
melhor_solucao(Tempo),
(
	(NTempo<Tempo,retract(melhor_solucao(_)),asserta(melhor_solucao(NTempo)))
).


% melhor escalonamento com findall, gera todas as solucoes e escolhe melhor

melhor_escalonamento():-
				asserta(melhor_solucao(10000000)),
				get_time(Ti),
				findall(_,permuta_tempo(_,_),_),
				melhor_solucao(Tempo),
				write('melhor solucao : '),write(Tempo),nl,
				get_time(Tf),Tcomp is Tf-Ti,
				retractall(melhor_solucao(_)),
				write('GERADO EM '),write(Tcomp),
				write(' SEGUNDOS'),nl.


% Earliest Due Date

heuristica_MenorTempoAtraso_EDD(Resultlist,Result):-
	% Encontra todas as tarefas
	findall(t(T,TP,TCP),(tarefa(T,TP,TC,P),TCP is TC * P),ListaTarefas),
	sort(3,@<,ListaTarefas,ListaTarefasOrd),
	to_task_id(ListaTarefasOrd,Resultlist),
	soma_tempos_atraso(Resultlist,Result),!.                                        

to_task_id([],[]):-!.
to_task_id([t(ID,_,_)|Rest],ResultList):- to_task_id(Rest,ResultListTMP),append([ID],ResultListTMP,ResultList).

%Critical Ratio

heuristica_MenorTempoAtraso_CR(ResultList,Result):- 
	findall(t(T,TP,TCP),(tarefa(T,TP,TC,P),TCP is TC * P),ListaTarefas),
	encontra_sequencia_cr(ListaTarefas,0,ResultList),
	soma_tempos_atraso(ResultList,Result),!.

encontra_sequencia_cr([],_,[]):-!.
encontra_sequencia_cr(ListaTarefas,TempoActual,ResultList):-
	%calcula Critical Ratio para uma determinada lista tendo em conta os tempos de atraso pesados
	soma_processamento_tarefas(ListaTarefas,TempoTotal),
	calcula_cr(ListaTarefas,TempoTotal,TempoActual,ListaCr),
	%organiza lista de critical Ratio de maneira a ter o elemento com melhor critical ratio
	sort(2,@<,ListaCr,ListaCrOrd),
	get_head(ListaCrOrd,TarefaEscolhida,NTempoActual),
	%remove tarefa da proxima lista a ser avaliada
	remove_tarefa_escolhida(TarefaEscolhida,ListaTarefas,NListaTarefas),
	encontra_sequencia_cr(NListaTarefas,NTempoActual,ResultListTMP),append([TarefaEscolhida],ResultListTMP,ResultList).

remove_tarefa_escolhida(T,LT,NLT):- 
	tarefa(T,TP,TC,P),
	TCP is TC * P,
	Tarefa = t(T,TP,TCP),
	delete(LT,Tarefa,NLT).

get_head([cr(Tarefa,_,TempoActual)|_],Tarefa,TempoActual).	

calcula_cr([],_,_,[]):-!.
calcula_cr([t(T,TP,TCP)|R],TempoTotal,TempoActual,ListaCr):-
	NTempoActual is TempoActual + TP,
	CR is (TCP - NTempoActual) / TempoTotal,
	calcula_cr(R,TempoTotal,TempoActual,ListaCrTmp),append([cr(T,CR,NTempoActual)],ListaCrTmp,ListaCr).
	
soma_processamento_tarefas([],0):-!.	
soma_processamento_tarefas([t(_,TP,_)|T],TempoTotal):- soma_processamento_tarefas(T,Soma),TempoTotal is Soma + TP.

%Soma dos TemosAtraso (adaptacao do soma_tempos)
soma_tempos_atraso([Id|List],Result):-
	tarefa(Id,TempoProcessamento,TempoConclusao,Peso),
	TempoAtual is TempoProcessamento,
	((TempoAtual-TempoConclusao)>0,!,TempoAtraso is (TempoAtual - TempoConclusao)*Peso;
	TempoAtraso is 0),!, soma_tempos_atraso(List,TempoAtual,TempoAtraso,Result),!.

soma_tempos_atraso([],_,Result,Result).
soma_tempos_atraso([Id|List],TempoAtual,TempoAtraso,Result):-!,
	tarefa(Id,TempoProcessamento,TempoConclusao,Peso),
	NTempoAtual is TempoAtual + TempoProcessamento,
	((NTempoAtual-TempoConclusao)>0,!,NTempoAtraso is TempoAtraso + (NTempoAtual-TempoConclusao)*Peso;NTempoAtraso is TempoAtraso),
	soma_tempos_atraso(List,NTempoAtual,NTempoAtraso,Result).



%-------------------------------------Algoritmos Genericos-------------------------------------%


:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic tempo_geracao/1.
:-dynamic peso_minimo_solucao/1.
:-dynamic n_populacoes_estavel/1.

% parameterizacao
inicializa:-
	write('Numero de novas Geracoes: '),read(NG), 			
	(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

inicializa_tempo:-
	write('Tempo de geracao em segundos: '),read(TS),
	(retract(tempo_geracao(_));true), asserta(tempo_geracao(TS)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

inicializa_solucao_pretendida:-
	write('Peso minimo da Solucao: '),read(PMS),
	(retract(peso_minimo_solucao(_));true), asserta(peso_minimo_solucao(PMS)),
	write('Numero maximo Geracoes: '),read(NG),
	(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).

inicializa_solucao_populacao_estavel:-
	write('Numero populacoes para considerar Estabilidade: '),read(E),
	(retract(n_populacoes_estavel(_));true), asserta(n_populacoes_estavel(E)),
	write('Numero maximo Geracoes: '),read(NG),
	(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).


gera_tempo:- 
	inicializa_tempo,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	get_time(TempoAtual),
	tempo_geracao(TempoEmSegundos),
	TempoAlvo is TempoAtual + TempoEmSegundos,
	gera_geracao_tempo(PopOrd,0,TempoAtual,TempoAlvo).


gera_solucao_pretendida:-
	inicializa_solucao_pretendida,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),peso_minimo_solucao(VMS),
	gera_geracao_peso_minimo_solucao(PopOrd,0,NG,VMS).

gera_solucao_populacao_estavel:-
	inicializa_solucao_populacao_estavel,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NMax),
	n_populacoes_estavel(NPopEstavel),
	gera_geracao_populacao_estavel(PopOrd,0,NMax,1,NPopEstavel).


gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

solucao_heuristicas_repetidas(H1,H2,NH2):-
	(compare_list(H1,H2),!,
	gerar_pontos_cruzamento(P1,P2),
	cruzar(H1,H1,P1,P2,NH2))
	;
	(NH2 = H2).

gera_populacao(Pop):-
	populacao(TamPop),
	tarefas(NumT),
	findall(Tarefa,tarefa(Tarefa,_,_,_),ListaTarefas),
	gera_populacao(TamPop,ListaTarefas,NumT,Pop).

%gera_populacao(0,_,_,[]):-!.


gera_populacao(2,_,_,[H1,NH2]):-
	heuristica_MenorTempoAtraso_EDD(H1,_),
	heuristica_MenorTempoAtraso_CR(H2,_),
	solucao_heuristicas_repetidas(H1,H2,NH2),!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
	gera_individuo(ListaTarefas,NumT,Ind),
	not(member(Ind,Resto)),!.
gera_populacao(TamPop,ListaTarefas,NumT,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	avalia(Seq,0,V).

avalia([],_,0).
avalia([T|Resto],Inst,V):-
	tarefa(T,Dur,Prazo,Pen),
	InstFim is Inst+Dur,
	avalia(Resto,InstFim,VResto),
	(
		(InstFim =< Prazo,!, VT is 0)
  ;
		(VT is (InstFim-Prazo)*Pen)
	),
	V is VT+VResto.

ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).

gera_geracao_base(Pop,PopFinal):-
	%permuta elementos da população
	random_permutation(Pop,PermutedPop),
	cruzamento(PermutedPop,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	nova_geracao(Pop,NPopOrd,PopFinal).

escreve_media_populacao(Pop):-escreve_media_populacao(Pop,0,0).

escreve_media_populacao([],N,Soma):-
	Media is Soma / N,write('Media de valores da populacao final : '),write(Media),nl.
escreve_media_populacao([_*V|T],N,Soma):-
	NN is N + 1,
	NSoma is Soma + V,
	escreve_media_populacao(T,NN,NSoma).

gera_geracao(G,G,[H|T]):-!,
	write('Geracao '), write(G), write(':'), nl, write([H|T]), nl,
	write('Melhor Solucao: '),write(H),nl,
	escreve_media_populacao([H|T]).

gera_geracao(N,G,Pop):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	gera_geracao_base(Pop,PopFinal),
	N1 is N+1,
	gera_geracao(N1,G,PopFinal).

gera_geracao_tempo([H|T],N,TempoAtual,TempoAlvo):- 
	TempoAtual >= TempoAlvo,!,
	write('Geracao '), write(N), write(':'), nl, write([H|T]), nl,
	write('Melhor Solucao: '),write(H),nl.

gera_geracao_tempo(Pop,N,TempoAtual,TempoAlvo):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	write('tempo de geracao restante: '),TempoRestante is TempoAlvo - TempoAtual,write(TempoRestante),nl,
	gera_geracao_base(Pop,PopFinal),
	N1 is N + 1,get_time(NTempoAtual),
	gera_geracao_tempo(PopFinal,N1,NTempoAtual,TempoAlvo).

gera_geracao_populacao_estavel([H|T],N,_,PopEstavel,PopEstavel):- 
	write('Geracao '), write(N), write(':'), nl, write([H|T]), nl,
	write('Estabilidade de solucoes atingida !'),nl,
	write('Melhor Solucao: '),write(H),nl.

gera_geracao_populacao_estavel([H|T],N,N,_,_):- 
	write('Geracao '), write(N), write(':'), nl, write([H|T]), nl,
	write('Numero Maximo de solucoes atingido !'),nl,
	write('Melhor Solucao: '),write(H),nl.

gera_geracao_populacao_estavel(Pop,N,NMax,PopEstavel,NPopEstavel):-
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	gera_geracao_base(Pop,PopFinal),
	(
		(not(compara_populacao(Pop,PopFinal)),!,PopEstavelTmp is 1)
		;
		(PopEstavelTmp is PopEstavel + 1)
	),
	N1 is N+1,
	gera_geracao_populacao_estavel(PopFinal,N1,NMax,PopEstavelTmp,NPopEstavel).


compara_populacao(Pop,Pop).	

gera_geracao_peso_minimo_solucao([S*V|R],N,N,_):-
	write('Geracao '), write(N), write(':'), nl, write([S*V|R]), nl,nl,
	write('Valor Maximo de solucoes atingido !'),nl,
	write('Melhor Solucao: '),write(S*V),nl,!.

gera_geracao_peso_minimo_solucao([S*V|R],N,_,ValorSolucaoAAtingir):-
	V =< ValorSolucaoAAtingir,
	write('Geracao '), write(N), write(':'), nl, write([S*V|R]), nl,nl,
	write('Valor Solucao pretendida atingido !'),nl,
	write('Melhor Solucao: '),write(S*V),nl,!.

gera_geracao_peso_minimo_solucao([S*V|R],N,ValorMaxGeracoes,ValorSolucaoAAtingir):-
	write('Geracao '), write(N), write(':'), nl, write([S*V|R]), nl,
	gera_geracao_base([S*V|R],PopFinal),
	N1 is N + 1,
	gera_geracao_peso_minimo_solucao(PopFinal,N1,ValorMaxGeracoes,ValorSolucaoAAtingir).

 
nova_geracao(Pop1,Pop2,PopFinal):-
	%Tamanho da população
	populacao(PopSize),
	%Tamanho maximo lista com a soma das duas populações
	MaxListSize is PopSize * 2,
	%Junta as duas geracoes sem repeticao
	append_without_repetition(Pop2,Pop1,PopTotal,PopSize,MaxListSize),
	%tamanho da lista com a soma das duas populacoes sem repeticao
	length(PopTotal, ListSize),
	%ordena a populacao por ordem decrescente de peso
	ordena_populacao(PopTotal,PopTotalOrd),
	%remove os dois primeiros elementos (dois melhores elementos porque a lista encontra-se ordenada)
	remove_dois_primeiros(PopTotalOrd,DoisMelhores,PopTotalOrdSemDoisMelhores),
	%Baralha aleatorimente a populacao que se encontra na lista
	random_permutation(PopTotalOrdSemDoisMelhores,PermutedPop),
	%seleciona a populacao que vai e nao vai entrar para o torneio
	Diferenca is ListSize - PopSize,TamanhoAlvo is PopSize - 2,
	seleciona_populacao(PermutedPop,PopTorneio,PopAprovada,TamanhoAlvo,Diferenca),
	%efectua torneio
	torneio(PopTorneio,PopVencedora),
	%Constroi populacao final
	append(PopVencedora,PopAprovada,PopSemDoisMelhores),
	(
		(
			not(compare_list(PopSemDoisMelhores,[])),!,
				ordena_populacao(PopSemDoisMelhores,PopSemDoisMelhoresOrd)
		)
		;
		PopFinal = DoisMelhores
	),
	append(DoisMelhores,PopSemDoisMelhoresOrd,PopFinal).
	

remove_dois_primeiros([H1,H2|R],[H1,H2],R):- !.

seleciona_populacao(Pop,PopTorneio,PopAprovada,_,0):-
	PopAprovada = Pop,PopTorneio = [].

seleciona_populacao([_,_|Pop],PopTorneio,PopAprovada,TamanhoAlvo,Diferenca):-
	Diferenca =:= TamanhoAlvo + 2,PopAprovada = [],PopTorneio = Pop.

seleciona_populacao([_|Pop],PopTorneio,PopAprovada,TamanhoAlvo,Diferenca):-
	Diferenca =:= TamanhoAlvo + 1 ,PopAprovada = [],PopTorneio = Pop.

seleciona_populacao(Pop,PopTorneio,PopAprovada,_,Diferenca):-
	seleciona_populacao(Pop,PopTorneio,PopAprovada,Diferenca).

seleciona_populacao(Pop,PopTorneio,PopAprovada,0):- PopTorneio = [],PopAprovada = Pop,!.
seleciona_populacao([E1,E2|R],PopTorneio,PopAprovada,NDuplas):-
	NNDuplas is NDuplas - 1,
	seleciona_populacao(R,PopTorneioBT,PopAprovadaBT,NNDuplas),append([E1,E2],PopTorneioBT,PopTorneio),PopAprovada = PopAprovadaBT.
	

	


append_without_repetition(Pop1,[],Pop1,_,_):- !.

append_without_repetition(Pop1,[H|T],PopFinal,PopSize,ListSize):-
	(
		(
			not(member(H,Pop1)),
			not(member(H,T)),! 
		)
		; 
		(
			ListSize =:= PopSize
		)
	),
	append_without_repetition(Pop1,T,PopCumulativa,PopSize,ListSize),
	append([H],PopCumulativa,PopFinal).

append_without_repetition(Pop1,[_|T],PopFinal,PopSize,ListSize):-
	NListSize is ListSize - 1,
	append_without_repetition(Pop1,T,PopCumulativa,PopSize,NListSize),PopFinal = PopCumulativa.	



torneio([],[]).

torneio([Seq1*Peso1,Seq2*Peso2|Resto],PopVencedora):-
	PesoTotal is Peso1 + Peso2,
	MaxPeso1 is Peso1 / PesoTotal,
	MaxPeso2 is Peso2 / PesoTotal,
	random(0,MaxPeso1,Valor1),
	random(0,MaxPeso2,Valor2),
	torneio(Resto,PopTmp),
	( (Valor1 =< Valor2,append([Seq1*Peso1],PopTmp,PopVencedora)) ; append([Seq2*Peso2],PopTmp,PopVencedora)).

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


cruzamento([],[]).
cruzamento([Ind*_],[Ind]).
cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tarefas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tarefas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tarefas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).

compare_list([],[]):-!.
compare_list([],_):-!,false.
compare_list([L1Head|L1Tail], [L2Head|L2Tail]):-
    L1Head == L2Head,
    compare_list(L1Tail, L2Tail).











