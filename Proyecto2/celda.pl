%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Solucion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%$$$$$%%%%%%%%% Reglas iniciales para pasillo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solucion(Mapa, Interruptores, Tipo) :- Mapa = pasillo(X, Modo),
                                       Modo = regular,
                                       Tipo = seguro,
                                       Interruptores = [(X, encendido)].

solucion(Mapa, Interruptores, Tipo) :- Mapa = pasillo(X, Modo),
                                       Modo = regular,
                                       Tipo = peligroso,
                                       Interruptores = [(X, apagado)].

solucion(Mapa, Interruptores, Tipo) :- Mapa = pasillo(X, Modo),
                                       Modo = invertido,
                                       Tipo = seguro,
                                       Interruptores = [(X, apagado)].

solucion(Mapa, Interruptores, Tipo) :- Mapa = pasillo(X, Modo),
                                       Modo = invertido,
                                       Tipo = peligroso,
                                       Interruptores = [(X, encendido)].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%$$$$$%%%%%%%%% Reglas iniciales para pasillo $$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solucion(Mapa, Interruptores, Tipo) :- Mapa = secuencia(SubMapa1, SubMapa2),
                                       Tipo = seguro,
                                       solucion(SubMapa1, S1, Tipo),
                                       solucion(SubMapa2, S2, Tipo),
                                       (member(X, S1), member(X, S2), member(X, Interruptores);
                                        member(X,S1), not(member(X, S2)), member(X, Interruptores);
                                        not(member(X, S1)), member(X, S2), member(X, Interruptores)).

%% solucion(Mapa, Interruptores, Tipo) :- Mapa = secuencia(SubMapa1, SubMapa2),
%%                                        Tipo = peligroso,
%%                                        solucion(SubMapa1, S1, Tipo),
%%                                        solucion(SubMapa2, S2, Tipo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Cruzar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%$$$$$%%%%%%%%% Reglas iniciales para pasillo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cruzar(Mapa, Interruptores, Seguro) :- Mapa = pasillo(X, Modo), 
                                       %% write(Mapa), nl,
                                       Modo = regular, 
                                       member((X, encendido), Interruptores), 
                                       Seguro = seguro, !.

cruzar(Mapa, Interruptores, Seguro) :- Mapa = pasillo(X, Modo), 
                                       %% write(Mapa), nl,
                                       Modo = regular, 
                                       member((X, apagado), Interruptores), 
                                       Seguro = peligroso, !.

cruzar(Mapa, Interruptores, Seguro) :- Mapa = pasillo(X, Modo), 
                                       %% write(Mapa), nl,
                                       Modo = invertido, 
                                       member((X, apagado), Interruptores), 
                                       Seguro = seguro, !.

cruzar(Mapa, Interruptores, Seguro) :- Mapa = pasillo(X, Modo), 
                                       %% write(Mapa), nl,
                                       Modo = invertido, 
                                       member((X, encendido), Interruptores), 
                                       Seguro = peligroso, !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%$$$$$%%%%%%%%% Reglas iniciales para secuencia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cruzar(Mapa, Interruptores, Seguro) :- Mapa = secuencia(SubMapa1, SubMapa2), 
                                       %% write(Mapa), nl,
                                       cruzar(SubMapa1, Interruptores, seguro), 
                                       cruzar(SubMapa2, Interruptores, seguro),
                                       Seguro = seguro, !.

cruzar(Mapa, _, Seguro) :- Mapa = secuencia(_, _), 
                                  %% write(Mapa), nl,
                                  Seguro = peligroso, !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%$$$$$%%%%%%%%% Reglas iniciales para secuencia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cruzar(Mapa, Interruptores, Seguro) :- Mapa = division(SubMapa1, SubMapa2),
                                       %% write(Mapa), nl,
                                       cruzar(SubMapa1, Interruptores, peligroso),
                                       cruzar(SubMapa2, Interruptores, peligroso),
                                       Seguro = peligroso, !.

cruzar(Mapa, _, Seguro) :- Mapa = division(_, _),
                                  %% write(Mapa), nl,
                                  Seguro = seguro, !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
