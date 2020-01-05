%-------------------------------------Prints-------------------------------------%

print_factory():-
	write("Operacao maquina :"),nl,
	findall((opt=Opt,maquina=M,ferramenta=F,setup=S,exec=E),
			operacao_maquina(Opt,M,F,S,E),
			LOM),write(LOM),nl,
	write("linha maquinas :"),nl,
	findall((linha=L,maquinas=LM),
			linha_maquinas(L,LM),
			LLM),write(LLM),nl.

print_products():-
	write("produtos operacoes :"),nl,
	findall((produto=P,operacoes=LOP),
			operacoes_produto(P,LOP),
			LOPP),
	write(LOPP),nl.

print_client_orders:-
	write("client orders :"),nl,
	findall((cliente=Client,orders=Orders),
			encomendas_cliente(Client,Orders),
			LCO),
	write(LCO),nl.

%-------------------------------------HTTP Server-------------------------------------%

:-dynamic id_operacaoTipo/2.
:-dynamic id_produto/2.
:-dynamic id_encomenda/5.


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json_convert)).
:- set_setting(http:cors, [*]).

server:-http_server(http_dispatch, [port(2226)]).
server_stop:-http_stop_server(2226,[]).

:- http_handler(root(api), handle_request, []).
:- http_handler(root(api/productionplanning),createProductionPlanning,[]).
:- http_handler(root(api/products/makespan),productsMakespan,[]).

handle_request(_Request) :-
    format("Content-type: text/plain~n~n"),
    format("Hello!").

productsMakespan(Request):-
	(
		(
			(option(methods(options),Request),!,
			cors_enable(Request, [methods([post,get,delete])]),
			format("~n"))
		)
		;
		(	
			%fetch factory overview
			fetch_factory_overview(),
			%fetch products overview
			fetch_products_overview(),
			findall((productId=PID,makespan=M),(operacoes_produto(P,_),calcula_makespan(P,1,M),id_produto(PID,P)),LPM),
			%order list
			sort(2,@=<,LPM,LPMO),
			%shorten list
			shorten_List(LPMO,3,SHLPMO),
			%list to json
			list_to_json(SHLPMO,JSON_SHLPMO),
			R = json([productsMakespan=JSON_SHLPMO]),
			is_json_term(R),
			format('Access-Control-Allow-Origin: *~n'),
			format('Content-type: application/json~n'),
			reply_json(R)
		)
	).

list_to_json([],[]):-!.
list_to_json([(productId=P,makespan=M)|T],[NH|NT]):-
	NH = json([productId=P,makespan=M]),
	list_to_json(T,NT).

shorten_List(_,0,[]):-!.
shorten_List([],_,[]):-!.
shorten_List([H|T],Size,[H|T2]):-
	NSize is Size - 1,
	shorten_List(T,NSize,T2).


createProductionPlanning(Request):-
	(
		(
			(option(methods(options),Request),!,
			cors_enable(Request, [methods([post,get,delete])]),
			format("~n"))
		)
		;
		(	
			%read response
			http_read_json(Request,PROLOG),
   			%get start time
			get_time(StartTime),
			%fetch everything
			fetch_factory_overview(),
			fetch_products_overview(),
			%process clients and orders
			process_clientsOrders_json(PROLOG,StartTime),
			%start process
			cria_op_enc(),
			gera_production_planning(),
			agenda_maquinas(),
			build_json_response(StartTime,JSON_Response),
			is_json_term(JSON_Response),
			format('Access-Control-Allow-Origin: *~n'),
			format('Content-type: application/json~n'),
			reply_json(JSON_Response)
		)
	).




build_json_response(StartTime,JSON_Response):-
    findall(json([orderId=OrderId,endTime=EndTime]),
			(
				tarefa_fim(TID,RelativeEndTime),
				id_encomenda(OrderId,Client,Prod,Qt,TConc),
				tarefa_encomenda(TID,EID),
				encomenda(EID,Client,Prod,Qt,TConc),
				EndTime is StartTime + RelativeEndTime
			),
            LE),JSON_Response=json([orderList=LE]).


:-dynamic encomendas_cliente/2.
:-dynamic prioridade_cliente/2.

process_clientsOrders_json(json([orders=LE,clients=LC]),StartTime):-
	%remove all old predicates
	retractall(encomendas_cliente(_,_)),
	retractall(prioridade_cliente(_,_)),
	retractall(id_encomenda(_,_,_,_,_)),
	process_orders_json(LE,1,StartTime),!,
	process_clients_json(LC).

process_orders_json([],_,_):-!.
process_orders_json([json([orderId=OrderId,productId=ProductId,clientId=ClientId,quantity=Quantity,conclusionTime=ConclusionTime])|T],N,CurrentTime):-
	%build relative time
	RelativeConclusionTime is ConclusionTime - CurrentTime,
	%build order id
	id_produto(ProductId,P),
	assertz(id_encomenda(OrderId,ClientId,P,Quantity,RelativeConclusionTime)),
	(
		(
			encomendas_cliente(ClientId,LE),!,
			append(LE,[e(P,Quantity,RelativeConclusionTime)],NLE),
			retract(encomendas_cliente(ClientId,_)),
			assertz(encomendas_cliente(ClientId,NLE))
		)
		;
		(	
			assertz(encomendas_cliente(ClientId,[e(P,Quantity,RelativeConclusionTime)]))
		)
	),
	N1 is N + 1,
	process_orders_json(T,N1,CurrentTime).

process_clients_json([]):-!.
process_clients_json([json([clientId=ClientId,priority=Priority])|T]):-
	assertz(prioridade_cliente(ClientId,Priority)),
	process_clients_json(T).


:-dynamic linhas/1.
:-dynamic linha_maquinas/2.
:-dynamic operacao_maquina/5.

%FACTORY OVERVIEW

fetch_factory_overview:-
	%retract all predicates related to old factory overview
	retractall(linhas(_)),
	retractall(linha_maquinas(_,_)),
	retractall(operacao_maquina(_,_,_,_,_)),
	%open connection to mdf overview
	http_open("https://masterdatafactory.azurewebsites.net/api/factoryoverview",In,[cert_verify_hook(cert_accept_any)]),
	json_read(In,FactoryOverviewJson),
	process_factory_json(FactoryOverviewJson),close(In).

process_factory_json(json([productionLines=LL,productionLineMachines=LLMJson,operationMachines=LOPTSJson])):-
	%asserta lista de linhas
	asserta(linhas(LL)),
	process_productionLineMachines_json(LLMJson),
	process_operationTypes_json(LOPTSJson,1).

process_productionLineMachines_json([]):-!.
process_productionLineMachines_json([json([productionLine=L,machines=LM])|T]):-
	asserta(linha_maquinas(L,LM)),
	process_productionLineMachines_json(T).

process_operationTypes_json([],_):-!.
process_operationTypes_json([json([operationType=OPTID,machine=M,tool=F,setupTime=TS,executionTime=TE])|T],N):-
	%relate operation type
	(
		(id_operacaoTipo(OPTID,OPT),!,N1 is N)
		;
		(
		atomic_concat("opt",N,OPT),
		assertz(id_operacaoTipo(OPTID,OPT)),
		N1 is N + 1
		)
	),
	assertz(operacao_maquina(OPT,M,F,TS,TE)),
	process_operationTypes_json(T,N1).

%PRODUCTS OVERVIEW

:- dynamic operacoes_produto/2.


fetch_products_overview:-
	%retract all predicates related to old products overview
	retractall(operacoes_produto(_,_)),
	retractall(id_produto(_,_)),
	%open connection to mdf overview
	http_open("https://masterdataproduct.azurewebsites.net/api/productsoverview",In,[cert_verify_hook(cert_accept_any)]),
	json_read(In,ProductsOverviewJson),
	process_products_json(ProductsOverviewJson),close(In).

process_products_json(json([productOperations=LPOPT])):-
	process_productOperations_json(LPOPT,1).

process_productOperations_json([],_):-!.
process_productOperations_json([json([product=PID,operations=LOPPID])|T],N):-
	atomic_concat("p",N,P),
	assertz(id_produto(PID,P)),
	N1 is N + 1,
	idList_to_operationTypeList(LOPPID,LOPP),
	assertz(operacoes_produto(P,LOPP)),
	process_productOperations_json(T,N1).

idList_to_operationTypeList([],[]):-!.
idList_to_operationTypeList([Id|T],[OPT|T2]):-
	idList_to_operationTypeList(T,T2),
	id_operacaoTipo(Id,OPT).

%-------------------------------------Cria_op_enc-------------------------------------%
:- (dynamic op_prod_client/6).
:- (dynamic encomendas/1).
:- (dynamic linha_makespan/2).
:- (dynamic linha_tarefas/2).


cria_op_enc:-
retractall(op_prod_client(_,_,_,_,_,_)),
retractall(tarefas(_)),
retractall(tarefa(_,_,_,_)),
retractall(encomenda(_,_,_,_,_)),
retractall(tarefa_encomenda(_,_)),
retractall(linha_makespan(_,_)),
retractall(linha_tarefas(_,_)),
retractall(linhas_em_uso(_)),
assertz(linhas_em_uso([])),
		findall(t(Cliente,Prod,Qt,TConc),
		       (encomendas_cliente(Cliente,LE),member(e(Prod,Qt,TConc),LE)),
               LT),
        %cria tarefas e ligaçoes entre as mesmas
        cria_tarefas_encomendas(LT,0),
        %atribui tarefas a linhas
        inicializa_linhas(),
		balanceamento_linhas(),
        %cria operaçoes atribuindo cada uma das tarefas á linha correspondente
		findall(EID,encomenda(EID,_,_,_,_),LE),
		cria_ops(LE,0).


inicializa_linhas():- linhas(LL),inicializa_linhas(LL).

inicializa_linhas([]):-!.
inicializa_linhas([L|T]):- assertz(linha_makespan(L,0)),inicializa_linhas(T).

cria_ops([],_).
cria_ops([EID|LT],N):-
	% Vai buscar encomenda através de id
	encomenda(EID,_,Prod,_,_),
	operacoes_produto(Prod,LOpt),
	cria_ops_prod_cliente(LOpt,EID,N,N1),
	cria_ops(LT,N1).


cria_ops_prod_cliente([],_,Nf,Nf).
cria_ops_prod_cliente([Opt|LOpt],EID,N,Nf):-
	cria_ops_prod_cliente2(Opt,EID,N,Ni),
	cria_ops_prod_cliente(LOpt,EID,Ni,Nf).


cria_ops_prod_cliente2(Opt,EID,N,Ni):-
			Ni is N+1,
			atomic_concat(op,Ni,Op),
            %maquinas linha
            tarefa_encomenda(TID,EID),
            encontra_linha(TID,L),
            linha_maquinas(L,LM),
            %Maquina tem de estar na linha
            operacao_maquina(Opt,M,F,Tsetup,Texec),
            member(M,LM),!,
	assertz(op_prod_client(Op,M,F,EID,Tsetup,Texec)).



%-------------------------------------Balanceamento de linhas-------------------------------------%

balanceamento_linhas():-
    findall(tld(TID,Tconc,Makespan,LLD),(
                                    tarefa(TID,Makespan,Tconc,_),
                                    tarefa_encomenda(TID,EID),
                                    encomenda(EID,_,Prod,_,_),
                                    verifica_linhas_disponiveis(Prod,LLD)
           ),LTLD),    
    sort(2,@=<,LTLD,LTLDO),
    atribui_tarefas_linha(LTLDO).

atribui_tarefas_linha([]):- !.

atribui_tarefas_linha([tld(TID,_,Makespan,LLD)|T]):-
    findall(lms(L,MS),(linha_makespan(L,MS),member(L,LLD)),LLMS),
    sort(2,@=<,LLMS,LLMSO),
    atribui_tarefa_linha(LLMSO,tld(TID,_,Makespan,LLD)),
    atribui_tarefas_linha(T).

atribui_tarefa_linha([lms(L,MakespanActual)|_],tld(TID,_,Makespan,_)):-
    %novo makespan
    MakespanNovo is MakespanActual + Makespan,
    retract(linha_makespan(L,_)),
	assertz(linha_makespan(L,MakespanNovo)),
	
	(
		(linha_tarefas(L,LT),!,
			retract(linha_tarefas(L,_)),
			append([TID],LT,NLT),
			assertz(linha_tarefas(L,NLT))
		)
		;
		(assertz(linha_tarefas(L,[TID])))
	).

encontra_linha(TID,L):- linha_tarefas(L,LT),member(TID,LT),!.

verifica_linhas_disponiveis(Prod,LD):-
    operacoes_produto(Prod,LOpP),
    findall(L,(linha_maquinas(L,LM),valida_maquinas_operacoes(LM,LOpP)),LD).

valida_maquinas_operacoes([],_):- !.

valida_maquinas_operacoes([M|T],[Opt|T2]):-
    %todas as possibilidades para um tipo de operacao
    findall(MOpt,operacao_maquina(Opt,MOpt,_,_,_),LM),
    member(M,LM),
    valida_maquinas_operacoes(T,T2).

%-------------------------------------Agendamento de maquinas-------------------------------------%

:-dynamic agenda_maquina/2.%(maquina,lista de agendamentos)
:-dynamic machine_tool/2. %(maquina, ferramenta)
:-dynamic tarefa_fim/2. %fim da tarefa
%Agendamento de setup a(tempo_inicial,tempo_final,"setup",ferramenta_em_uso)
%Agendamento de execucao  a(tempo_inicial,tempo_final,"exec",op(Operacao, id da encomenda))


agenda_maquinas:- 
	retractall(agenda_maquina(_,_)),
	retractall(machine_tool(_,_)),
	retractall(tarefa_fim(_,_)),
	asserta(deslizamento_total(0)),
	findall(T,(linha_solucao(_,T),agenda_maquinas_tarefas(0,T)),_).


agenda_maquinas_tarefas(_,[]):-!.
agenda_maquinas_tarefas(TempoActual,[TID|T]):-
	%encomenda através de tarefa
	tarefa_encomenda(TID,EID),
	%operacoes associadas a encomenda
	findall(op(Op,M,F,Tsetup,Texec,EID),op_prod_client(Op,M,F,EID,Tsetup,Texec),LOPS),
	%maquinas associadas a encomenda
	findall(M,op_prod_client(_,M,_,EID,_,_),LM),
	%descobre operacao que marca a cadencia
	operacao_cadencia(LOPS,OPCA),
	lista_operacoes_tempo_atribuido(LOPS,LOPSTA,TID,OPCA),!,
	%agenda uma tarefa nova
	agenda_maquinas_operacoes(TempoActual,LOPSTA),!,
	%desliza a tarefa agendada
	calcula_deslizamento_maximo(LM,DM),
	desliza_agendamentos(LM,DM),!,
	%encontra makespan de tarefa
	tarefa(TID,MS,_,_),
	%calcula novo tempo actual
	NTempoActual is TempoActual + MS,
	%calcula valor final da tarefa
	FimTarefa is NTempoActual - DM,
	assertz(tarefa_fim(TID,FimTarefa)),
	agenda_maquinas_tarefas(NTempoActual,T).

junta_listas([],[]):-!.
junta_listas([H1|T1],L):- junta_listas(T1,L1),append(H1,L1,L).

%operacoes com tempo atribuido 
%Setup ---> opta(M,TI,TF,"setup",F)
%Exec ---> opta(M,TI,TF,"exec",op(op,EID))
operacao_cadencia(LOPS,OPCA):-sort(5,@>=,LOPS,[op(OPCA,_,_,_,_,_)|_]).

lista_operacoes_tempo_atribuido(LOPS,LOPSTA,TID,OPCA):-
%começa do fim para o inicio
reverse(LOPS,LOPSR),
%tempo de fim
tarefa(TID,MS,_,_),
lista_operacoes_tempo_atribuido(LOPSR,LOPSTA,_,MS,OPCA).

lista_operacoes_tempo_atribuido([],[],_,_,_):-!.

lista_operacoes_tempo_atribuido([op(OPCA,M,F,Tsetup,Texec,EID)|T],LOPSTA,TempoIActualSuperior,TempoFActual,OPCA):-!,
	%vai buscar quantidade a encomenda
	encomenda(EID,_,_,Qt,_),
	%determina tempo inicial actual
	TempoIActual is TempoFActual - Qt * Texec,
	TFexec is TempoFActual,
	TIexec is TempoIActual,
	TFsetup is TIexec,
	TIsetup is TFsetup-Tsetup,
	%determina tempo final actual
	NTempoFActual is TempoFActual - Texec,
	lista_operacoes_tempo_atribuido(T,LOPSTA1,TempoIActual,NTempoFActual,OPCA),!,
	%backtracking!!! :)
	%determina tempoIActualSuperior
	TempoIActualSuperior is TempoIActual + Texec,
	%cria variaveis para serem introduzidas na lista
	OPTAS = opta(M,TIsetup,TFsetup,"setup",F),
	OPTAE = opta(M,TIexec,TFexec,"exec",op(OPCA,EID)),
	%introduz nas listas
	append(LOPSTA1,[OPTAS,OPTAE],LOPSTA).

lista_operacoes_tempo_atribuido([op(Op,M,F,Tsetup,Texec,EID)|T],LOPSTA,TempoIActual,TempoFActual,OPCA):-
	TFexec is TempoFActual,
	%encontra novo tempo actual
	NTempoFActual is TempoFActual - Texec,
	(
		(nonvar(TempoIActual),!,NTempoIActual is TempoIActual - Texec)
		;
		true
	),
	lista_operacoes_tempo_atribuido(T,LOPSTA1,NTempoIActual,NTempoFActual,OPCA),!,
	%backtracking!!! :)
	(
		(var(TempoIActual),!,TempoIActual is NTempoIActual + Texec)
		;
		true
	),
	TIexec is NTempoIActual,
	TFsetup is TIexec,
	TIsetup is TFsetup - Tsetup,
	%cria variaveis para serem introduzidas na lista
	OPTAS = opta(M,TIsetup,TFsetup,"setup",F),
	OPTAE = opta(M,TIexec,TFexec,"exec",op(Op,EID)),
	%introduz na lista
	append(LOPSTA1,[OPTAS,OPTAE],LOPSTA). 
	

agenda_maquinas_operacoes(_,[]):-!.
agenda_maquinas_operacoes(TempoActual,[opta(M,TI,TF,"setup",F)|T]):-
	(
		(
			%se ja existir uma agenda para M
			agenda_maquina(M,LA),!,
			(
				(
					machine_tool(M,FA),
					FA \= F,
					%calculo de variaveis acumuladas
					TIA is TI + TempoActual,
					TFA is TF + TempoActual,
					append(LA,[a(TIA,TFA,"setup",F)],NLA),
					retract(agenda_maquina(M,_)),
					assertz(agenda_maquina(M,NLA)),
					retract(machine_tool(M,_)),
					assertz(machine_tool(M,F))
				)
				;
				true
			)
		)
		;
		(
			%se ainda nao existir uma agenda para M
			assertz(agenda_maquina(M,[a(TI,TF,"setup",F)])),
			assertz(machine_tool(M,F))
		)
	),agenda_maquinas_operacoes(TempoActual,T).

agenda_maquinas_operacoes(TempoActual,[opta(M,TI,TF,"exec",op(Op,EID))|T]):-
	agenda_maquina(M,LA),
	%calculo de variaveis acumuladas
	TIA is TI + TempoActual,
	TFA is TF + TempoActual,
	append(LA,[a(TIA,TFA,"exec",op(Op,EID))],NLA),
	retract(agenda_maquina(M,_)),
	%asserta nova agenda da maquina
	assertz(agenda_maquina(M,NLA)),
	agenda_maquinas_operacoes(TempoActual,T).


desliza_agendamentos([],_):-!.
desliza_agendamentos([M|T],DM):-
	agenda_maquina(M,LA),
	desliza_maquina(LA,DM,NLA),
	retract(agenda_maquina(M,_)),
	asserta(agenda_maquina(M,NLA)),
	desliza_agendamentos(T,DM).

desliza_maquina([a(TIE,TFE,"exec",Op)|[]],D,[a(NTIE,NTFE,"exec",Op)]):-!,
	NTIE is TIE - D,
	NTFE is TFE - D.
desliza_maquina([a(TIS,TFS,"setup",F),a(TIE,TFE,"exec",Op)|[]],D,[a(NTIS,NTFS,"setup",F),a(NTIE,NTFE,"exec",Op)]):-!,
	NTIE is TIE - D,
	NTFE is TFE - D,
	NTIS is TIS - D,
	NTFS is TFS - D.

desliza_maquina([A|T],D,[A|NLA]):- desliza_maquina(T,D,NLA).


calcula_deslizamento_maximo([M|[]],D):-!,agenda_maquina(M,LA),reverse(LA,LAR),calcula_deslizamento(LAR,D).
calcula_deslizamento_maximo([M|T],DM):-
	calcula_deslizamento_maximo(T,DN),
	agenda_maquina(M,LA),
	reverse(LA,LAR),
	calcula_deslizamento(LAR,D),
	(
		(DN=<D,!,DM is DN)
		;
		DM is D
	).

calcula_deslizamento([a(_,_,"exec",_),a(TI,_,"setup",_)],TI):-!.
calcula_deslizamento([a(_,_,"exec",_),a(TI,_,"setup",_),a(_,TF,"exec",_)|_],TI - TF):-!.
calcula_deslizamento([a(TI,_,"exec",_),a(_,TF,"exec",_)|_],TI - TF):-!.
	

%-------------------------------------Tarefas-------------------------------------%

:-dynamic tarefa/4.
:-dynamic tarefas/1.
:-dynamic tarefa_encomenda/2.
:-dynamic encomenda/5.


cria_tarefas_encomendas([],N) :- asserta(tarefas_encomedas(N)),!.
cria_tarefas_encomendas([H|T],N):- cria_tarefa_encomenda(H,N),N1 is N + 1,cria_tarefas_encomendas(T,N1).

cria_tarefa_encomenda(t(Cliente,Prod,Qt,TConc),N):-
    calcula_makespan(Prod,Qt,MakeSpan),
    atomic_concat('t',N,NTarefa),
    atomic_concat('e',N,NEncomenda),
    prioridade_cliente(Cliente,Prioridade),calcula_penalizacao(Prioridade,Penalizacao),
    assertz(tarefa(NTarefa,MakeSpan,TConc,Penalizacao)),
    assertz(encomenda(NEncomenda,Cliente,Prod,Qt,TConc)),
    assertz(tarefa_encomenda(NTarefa,NEncomenda)).

calcula_makespan(Prod,Qt,MakeSpan):-
	cria_lista_operacoes_produto(Prod,LOpP),
	soma_valores_setup(LOpP,Setup),
	sort(1, @>=, LOpP, LOpPO),
	soma_valores_makespan(LOpPO,Qt,MakeSpan,Setup).

cria_lista_operacoes_produto(Prod,LOpP):-
    operacoes_produto(Prod,LOpPSemDetalhe),
    cria_lista_operacoes_produto2(LOpPSemDetalhe,LOpP).

cria_lista_operacoes_produto2([],[]):-!.
cria_lista_operacoes_produto2([Opt|T],[(Texec,Tsetup)|T2]):-
    operacao_maquina(Opt,_,_,Tsetup,Texec),!,
    cria_lista_operacoes_produto2(T,T2).


soma_valores_setup([(Texec,Tsetup)|T],Setup):-
	soma_valores_setup([(Texec,Tsetup)|T],0,0,NegativeSetup),Setup is -NegativeSetup.

soma_valores_setup([],_,Setup,Setup):-!.
soma_valores_setup([(Texec,Tsetup)|T],TempoActual,Setup,Resultado):-
	NSetup is TempoActual - Tsetup,
	NTempoActual is TempoActual + Texec,
	(
		(NSetup<Setup,!,soma_valores_setup(T,NTempoActual,NSetup,Resultado))
		;
		soma_valores_setup(T,NTempoActual,Setup,Resultado)
	).

soma_valores_makespan([(TSoma,_)|T],Qt,Makespan,Setup):-
	ValorOpCadencia is TSoma * Qt,
	soma_lista_valores(T,Soma),
	Makespan is ValorOpCadencia + Soma + Setup.

soma_lista_valores([],0):-!.

soma_lista_valores([(Texec,_)|T],Soma):-soma_lista_valores(T,Valor),Soma is Valor + Texec.

calcula_penalizacao(Prioridade,Penalizacao):-Penalizacao is 0.9 + Prioridade / 10.
%-------------------------------------Heuristicas adaptadas a Tarefas--------------------------%



% Earliest Due Date

heuristica_MenorTempoAtraso_EDD(ListaTarefas,Resultlist,Result):-
	findall(t(T,TP,TCP),(tarefa(T,TP,TC,P),member(T,ListaTarefas),TCP is TC * P),ListaTarefasDetalhadas),
	sort(3,@=<,ListaTarefasDetalhadas,ListaTarefasOrd),
	to_task_id(ListaTarefasOrd,Resultlist),
	avalia(Resultlist,Result),!.                                        

to_task_id([],[]):-!.
to_task_id([t(ID,_,_)|Rest],ResultList):- to_task_id(Rest,ResultListTMP),append([ID],ResultListTMP,ResultList).

%Critical Ratio
heuristica_MenorTempoAtraso_CR(ListaTarefas,ResultList,Result):- 
	findall(t(T,TP,TCP),(tarefa(T,TP,TC,P),member(T,ListaTarefas),TCP is TC * P),ListaTarefasDetalhadas),
	encontra_sequencia_cr(ListaTarefasDetalhadas,0,ResultList),
	avalia(ResultList,Result),!.

encontra_sequencia_cr([],_,[]):-!.
encontra_sequencia_cr(ListaTarefas,TempoActual,ResultList):-
	%calcula Critical Ratio para uma determinada lista tendo em conta os tempos de atraso pesados
	soma_processamento_tarefas(ListaTarefas,TempoTotal),
	calcula_cr(ListaTarefas,TempoTotal,TempoActual,ListaCr),
	%organiza lista de critical Ratio de maneira a ter o elemento com melhor critical ratio
	sort(2,@=<,ListaCr,ListaCrOrd),
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

%-------------------------------------Gera production planning-------------------------------------%


:-dynamic linha_solucao/2.

gera_production_planning:-
%remove todas as solucoes
retractall(linha_solucao(_,_)),
findall((L,LT),linha_tarefas(L,LT),LLT),!,
setup_gera(),
gera_production_planning(LLT),!.




gera_production_planning([]):- !.

gera_production_planning([(L,LT)|T]):-

	((retract(linha_em_uso(_)),!);true), asserta(linha_em_uso(L)),
	((retract(tarefas_linha_em_uso(_)),!);true),length(LT,NT),asserta(tarefas_linha_em_uso(NT)),
	(
		(NT >= 6,!,gera())
		;
		(melhor_escalonamento())
	),gera_production_planning(T).

%-------------------------------------Melhor Escalonamento-------------------------------------%

:-dynamic melhor_tempo/1.

%Calculo da melhor solucao

% permuta/2 gera permutações de listas
permuta([ ],[ ]).
permuta(L,[X|L1]):-apaga1(X,L,Li),permuta(Li,L1).

apaga1(X,[X|L],L).
apaga1(X,[Y|L],[Y|L1]):-apaga1(X,L,L1).

permuta_tempo(LP,Tempo):- 
linha_em_uso(L),linha_tarefas(L,LT1),findall(T,(tarefa(T,_,_,_),member(T,LT1)),LT),
permuta(LT,LP),avalia(LP,NTempo),
melhor_tempo(Tempo),
NTempo<Tempo,
retract(melhor_tempo(_)),asserta(melhor_tempo(NTempo)),
retractall(linha_solucao(L,_)),asserta(linha_solucao(L,LP)).


% melhor escalonamento com findall, gera todas as solucoes e escolhe melhor

melhor_escalonamento():-
				assertz(melhor_tempo(100000000)),
				findall(_,permuta_tempo(_,_),_),
				retractall(melhor_tempo(_)).
			

%-------------------------------------Algoritmos Geneticos-------------------------------------%
	
:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic linha_em_uso/1.
:-dynamic tarefas_linha_em_uso/1.

setup_gera:-
	((retract(geracoes(_)),!);true), asserta(geracoes(100)),
    ((retract(populacao(_)),!);true), asserta(populacao(10)),
	PC is 80 / 100,PM is 20 / 100,
    ((retract(prob_cruzamento(_)),!);true),asserta(prob_cruzamento(PC)),
    ((retract(prob_mutacao(_)),!);true), asserta(prob_mutacao(PM)).

gera():-
	gera_populacao(Pop),!,
	avalia_populacao(Pop,PopAv),
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

solucao_heuristicas_repetidas(H1,H2,NH2):-
	((compare_list(H1,H2),!,
	mutacao1(H2,NH2))
	;
	(NH2 = H2)).

gera_populacao(Pop):-
	linha_em_uso(L),
	%tamanho da populacao
	populacao(TamPop),
    %tarefas para uma determinada linha
	linha_tarefas(L,ListaTarefas),
	%numero de tarefas
	length(ListaTarefas,NumT),!,
	gera_populacao(TamPop,ListaTarefas,NumT,Pop).

%gera_populacao(0,_,_,[]):-!.


gera_populacao(2,ListaTarefas,_,[H1,NH2]):-
	heuristica_MenorTempoAtraso_EDD(ListaTarefas,H1,_),
	heuristica_MenorTempoAtraso_CR(ListaTarefas,H2,_),
	solucao_heuristicas_repetidas(H1,H2,NH2).

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

gera_geracao(_,_,[S*0|_]):-!,
	linha_em_uso(L),
    assertz(linha_solucao(L,S)).

gera_geracao(G,G,[S*_|_]):-!,
	linha_em_uso(L),
    assertz(linha_solucao(L,S)).

gera_geracao(N,G,Pop):-
	gera_geracao_base(Pop,PopFinal),
	N1 is N+1,
	gera_geracao(N1,G,PopFinal).
 
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
	tarefas_linha_em_uso(N),
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
	tarefas_linha_em_uso(N),
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
	tarefas_linha_em_uso(T),
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
	tarefas_linha_em_uso(NumT),
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



%%%%%%%%%%%%%%%%% SERVER END %%%%%%%%%%%%%%%%%%%%








