:- dynamic estudiante/3.  % -- dynamic le dice a prolog que el predicado estudiante va a cambiar, asi que este permite usar assertz y retract 


%-- imprimir en formato hh:mm --
imprimir_hhmm(Total) :-
    Horas is Total // 60,
    Mins is Total mod 60,
    write(Horas), write(':'),
    (Mins < 10 -> write('0'), write(Mins) ; write(Mins)).

% -- para calcular la duracion en el campus --
calcular_duracion:-
    write('ID del estudiante: '), read(ID),
    (estudiante(ID, Entrada, Salida) ->
        (Salida =:= 0 ->
            write('El estudiante sigue en el campus.'), nl
        ;
            Duracion is Salida - Entrada,
            write('La duracion en el campus es: '), imprimir_hhmm(Duracion), write(' h'), nl
        )
    ;
        write('Estudiante no encontrado.'), nl
    ).


%-- imprimir estudiante --
imprimir_estudiante(estudiante(ID, Entrada, Salida)) :-
        write('ID: '), write(ID),
        write(' | Entrada: '), imprimir_hhmm(Entrada),
        (Salida =:= 0 ->
            write(' | Se encuentra en el campus'),nl
        ;
        Duracion is Salida - Entrada,
            write(' | Salida: '), imprimir_hhmm(Salida),
            write(' | Duracion: '), imprimir_hhmm(Duracion), write(' h'), nl
        ), nl.


% -- check in -- 
check_in :- 
    write('ID del estudiante: '), read(ID), 
    (estudiante(ID, _, 0) ->
        write('El estudiante sigue en el campus.'), nl
    ;
        write('Hora de entrada - Horas: '), read(Horas),
        write('Hora de entrada - Minutos: '), read(Minutos),
        Entrada is Horas * 60 + Minutos,      % -- para que se guarde en minutos en la bd y se puedan hacer operaciones matematicas
        assertz(estudiante(ID, Entrada, 0)), 
        write('Entrada registrada.'), nl,
        guardar
    ).
    

% -- buscar --
buscar :-
    write('ID del estudiante: '), read(ID),
    (estudiante(ID, Entrada, Salida) ->
        (Salida =:= 0 ->
            write('El estudiante esta en el campus. Entrada: '), imprimir_hhmm(Entrada), nl
        ;
            Duracion is Salida - Entrada,
            write('El estudiante ha salido. Salida: '), imprimir_hhmm(Salida),
            write(' | Duracion: '), imprimir_hhmm(Duracion), write(' h'), nl
        )
    ;
        write('Estudiante no encontrado.'), nl
    ).

% -- check out --
check_out :-
    write('ID del estudiante: '), read(ID),
    write('Hora de Salida - Horas: '), read(Horas),
    write('Hora de Salida - Minutos: '), read(Minutos),
    Salida is Horas * 60 + Minutos,
    (estudiante(ID, Entrada, 0) ->
        retract(estudiante(ID, Entrada, 0)),
        assertz(estudiante(ID, Entrada, Salida)),
        write('Salida registrada.'), nl,
        guardar
    ;
        write('No hay salida registrada.'), nl
    ).


% -- to list --
listar :-
    findall(estudiante(ID, Entrada, Salida), estudiante(ID, Entrada, Salida), Lista),
    write('Estudiantes registrados:'), nl,
    maplist(imprimir_estudiante, Lista).  % recorre la Lista y por cada estudiante llama a la funcion impr
   

%-- to save--
guardar:-
    open('University.txt', write, File),  % File es como el lapicero para escribir en el archivo
    forall(
        estudiante(ID, Entrada, Salida),
        (write( File, estudiante(ID, Entrada, Salida) ), write(File, '.'), nl(File)) % con el File escribes DENTRO del archivo, sin el escribiria en pantalla 
        ),
        close(File).


%--to load--
cargar :-
    (exists_file('University.txt') -> consult('University.txt') ; true). % - consult carga el archivo para que pueda trabajar con su contenido 


%--menu--
menu :-
    write('--- Menu ---'), nl,
    write('1. Registrar entrada'), nl,
    write('2. Buscar estudiante'), nl,
    write('3. Calcular duracion'), nl,
    write('4. Registrar salida'), nl,
    write('5. Listar estudiantes'), nl,
    write('0. Salir'), nl,
    write('Seleccione una opcion: '), read(Opcion), ejecutar(Opcion).

    ejecutar(1) :- check_in, menu.  % para que se vuelva a ejecutar menu 
    ejecutar(2) :- buscar, menu.
    ejecutar(3) :- calcular_duracion, menu.
    ejecutar(4) :- check_out, menu.
    ejecutar(5) :- listar, menu.
    ejecutar(0) :- write('Saliendo...'), nl.
    ejecutar(_) :- write('Opcion no valida.'), nl, menu.

inicio :-
    cargar,
    menu.

:- inicio.

