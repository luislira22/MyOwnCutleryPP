%6 encomendas
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

%6 encomendas
encomenda(clA,[e(pA,4,50),e(pB,4,70)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120)]).

%7 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120)]).

%8 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120)]).
encomenda(clD,[e(pA,2,180)]).

%9 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pF,5,320)]).
encomenda(clD,[e(pA,2,180)]).

%10 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,1,198)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pF,5,320)]).
encomenda(clD,[e(pA,2,180)]).

%11 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,1,198)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pF,5,320)]).
encomenda(clD,[e(pA,2,180),e(pE,3,500)]).

%12 encomendas

encomenda(clA,[e(pA,4,50),e(pB,4,70),e(pD,6,100)]).
encomenda(clB,[e(pC,3,30),e(pD,5,200),e(pA,1,198)]).
encomenda(clC,[e(pE,4,60),e(pF,6,120),e(pF,5,320)]).
encomenda(clD,[e(pA,2,180),e(pE,3,500),e(pD,4,700)]).
