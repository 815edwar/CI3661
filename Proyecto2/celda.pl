%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Verificacion Interruptores y Pasillos %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% Reglas iniciales de mergesort para unir dos listas %%%%%%%%%%%%%%%%%%

mergesort([], L, L).
mergesort(L, [], L).
mergesort([(X, Xm)|XS], [(Y, Ym)|YS], [(X, Xm)|Z]) :- X @=< Y, 
                                                      mergesort(XS, [(Y, Ym)|YS], Z).

mergesort([(X, Xm)|XS], [(Y, Ym)|YS], [(Y, Ym)|Z]) :- X @> Y, 
                                                      mergesort([(X, Xm)|XS], YS, Z).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Reglas iniciales para validar pasillos con los interruptores %%%%%%%%%%%%

validar_pasillos(pasillo(X, _), [(X,_)]).
validar_pasillos(secuencia(SubMapa1, SubMapa2), Interruptores) :- validar_pasillos(SubMapa1, Interruptores1),
                                                                  validar_pasillos(SubMapa2, Interruptores2),
                                                                  mergesort(Interruptores1,Interruptores2,Interruptores).

validar_pasillos(division(SubMapa1, SubMapa2), Interruptores) :-  validar_pasillos(SubMapa1, Interruptores1),
                                                                  validar_pasillos(SubMapa2, Interruptores2),
                                                                  mergesort(Interruptores1,Interruptores2,Interruptores).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Reglas iniciales para validar los interruptores %%%%%%%%%%%%%%%%%

validar_modo([]).
validar_modo([(_,X)|XS]) :- validar_modo(XS), 
                            (X = encendido; X = apagado).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Reglas iniciales para instanciar los interruptores %%%%%%%%%%%%%%%%%

instanciar_interruptores (Mapa, Interruptores) :- validar_modo (Interruptores),
                                                 validar_pasillos (Mapa, Interruptores).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Solucion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%% Reglas iniciales para pasillo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%% Reglas iniciales para pasillo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solucion(Mapa, Interruptores, Tipo) :- Mapa = secuencia(SubMapa1, SubMapa2),
                                       Tipo = seguro,
                                       instanciar_interruptores(Mapa, Interruptores),
                                       solucion(SubMapa1, S1, Tipo),
                                       solucion(SubMapa2, S2, Tipo),
                                       (member(X, S1), member(X, S2), member(X, Interruptores);
                                        member(X,S1), not(member(X, S2)), member(X, Interruptores);
                                        not(member(X, S1)), member(X, S2), member(X, Interruptores)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Cruzar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% Reglas iniciales para pasillo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%% Reglas iniciales para secuencia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cruzar(Mapa, Interruptores, Seguro) :- Mapa = secuencia(SubMapa1, SubMapa2),
                                       instanciar_interruptores(Mapa, Interruptores), 
                                       %% write(Mapa), nl,
                                       cruzar(SubMapa1, Interruptores, seguro), 
                                       cruzar(SubMapa2, Interruptores, seguro),
                                       Seguro = seguro, !.

cruzar(Mapa, _, Seguro) :- Mapa = secuencia(_, _), 
                                  %% write(Mapa), nl,
                                  Seguro = peligroso, !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%% Reglas iniciales para division %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cruzar(Mapa, Interruptores, Seguro) :- Mapa = division(SubMapa1, SubMapa2),
                                       instanciar_interruptores(Mapa, Interruptores),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Leer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leer(Mapa) :-
  write( user_output, "Escriba el nombre del archivo donde se encuentra el mapa: " ), 
  flush_output( user_output ),
  read_string( user_input, "\n", " ", End, NombreArchivo),
  open(NombreArchivo,read,Archivo), 
  read(Archivo,Mapa),
close(Archivo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
