% Unidades
% Este predicado indica cuáles son las unidades militares disponibles.
unidad(lancero).
unidad(arquero).
unidad(jinete).
unidad(guerrillero).

% Edificios
% Este predicado indica cuáles son los edificios que se pueden construir.
edificio(cuartel).
edificio(arqueria).
edificio(establo).

% Entrenamiento
% Este predicado indica en qué edificio se entrena cada unidad.
entrena(lancero,      cuartel).
entrena(arquero,      arqueria).
entrena(guerrillero,  arqueria).
entrena(jinete,       establo).

% Costos
% Este predicado indica el costo de cada unidad y de cada edificio.
costo(lancero,      80).
costo(arquero,      90).
costo(guerrillero,  70).
costo(jinete,       120).
costo(cuartel,      300).
costo(arqueria,     330).
costo(establo,      400).


desde(X,X).
desde(X,Y) :- nonvar(Y), Y > X.
desde(X,Y) :- var(Y), N is X+1, desde(N,Y).

% Ej 1 : costo para listas (tanto de batallones como de edificios)
% costo ( +L , -C )
% Enunciado: Extender el predicado "costo" para que funcione con listas, tanto de batallones como de edificios.

costo((U,C),Costo) :- costo(U,Costo2), Costo is Costo2*C.

costo([],0).
costo([L|LS],C) :- costo(LS,C2),costo(L,CL), C is (CL + C2).

% Ej 2 : instanciar un ejército arbitrario
% ejercito ( -E )

ejercito(E) :- desde(1,X), ejercitosDeNSoldados(X,E).

ejercitosDeNSoldados(0,[]).
ejercitosDeNSoldados(N,[(S,A)|ES]) :- between(1,N,A), unidad(S), M is N - A, ejercitosDeNSoldados(M,ES). 


% Reversibilidad: En el caso de E, si no esta instanciado se empezaran a generar todos los ejercitos posibles, de a un batallon a la vez. Generando 
% el primer elemento del ejercito y luego recursivamente el resto, avanzando sobre el numero de soldados que tiene el ejercito.
% En el caso de que E si este instanciado se hara algo parecido, esta vez se intentara encontrar algun ejercito que unifique con E, para 
% eso se intentara de generar todos los ejercitos posibles y ver cuales unifican con E, y aunque encuentre el que si unifica luego seguira buscando 
% si algun otro lo hace.
%Por lo explicado anteriormente entonces E no es reversible, ya que el comportamiento esperado seria que si E esta instanciado devuelva true si es un ejercito correcto nomas, pero no termina nunca.


% Ej 3 : instancia una lista de edificios necesarios para el ejército
% edificiosNecesarios ( +Ej , -Ed )
edificiosNecesarios([],[]).

edificiosNecesarios([(U,C)|Ls],[Ed|Eds]) :- edificiosNecesarios(Ls,Eds),entrena(U,Ed),not(member(Ed,Eds)). %%falta eliminar repetidos
edificiosNecesarios([(U,C)|Ls],Eds) :- edificiosNecesarios(Ls,Eds),entrena(U,Ed),member(Ed,Eds).

% Reversibilidad: Si Ej esta instanciado, voy a armar la lista de edificios necesarios para mi ejercito de "atras para adelante", 
% voy a llamar recursivamente hasta encontrar el ultimo elemento y empezar a armar la lista desde ahí. En cada elemento necesito verificar si el edificio
% que voy a agregar a la lista ya fue agregado anteriormente, si ya existe entonces no necesito agregarlo nuevamente, si no esta entonces se vuelve la cabeza de mi
% lista de edificios.
% Si Ej no esta instanciado se empezaran a generar ejercitos (estos ejercitos tienen variables libres en la parte de cantidad), y se los unificara con los 
% edificios necesarios para entrenar a dichos ejercitos. Solo se unificaran aquellos ejercitos cuyos batallones requieren edificios distintos, ya que si
% requieren el mismo edificio no seran agregados. Una vez que se hayan generado todos los ejercitos distintos que tengan edificios distintos se generaran
% infinitamente el resto de los ejercitos que repiten edificios pero no se instanciaran.

% Si Ed no esta instanciado, se repetira exactamente el mismo caso que se explico en +Ej.
% Si Ed esta instanciado se intetara de ver si Ej y Ed unifican correctamente, utilizando el mismo metodo explicado anteriormente, viendo el ultimo elemento primero
% y luego recursivamente "para atras". En caso de que unifiquen correctamente luego intentara repetir el proceso utilizando el predicado que no se utilizo antes
% lo que produce un false ya que son complementarios.

%En Ed si es reversible, si Ed esta instanciado el comportamiento esperado seria que al instanciar tanto un edificio como un ejercito me diga si la lista de edificiosNecesarios2
% son los necesarios para entrenar a dicho ejercito.

%En el caso de Ej no es reversible, se esperaria que se instancien todos los ejercitos posibles y los edificios necesarios para dichos ejercitos pero Solo
% se instanciaran ejercitos (y sin su cantidad de unidades ) hasta cierto punto y luego el programa se traba, generando infinitos ejercitos pero sin poder unificarlos a nuevas listas de edificios distintas.





edificiosNecesarios2([L|LS],Ed) :- ejercito([L|LS]), edificiosNecesarios([L|LS],Ed).
% En el caso que E no este instanciada, edificiosNecesarios2 va a generar todos los posibles ejercitos en E con Ejercito(-E), para luego instanciar en 
% Ed la lista de edificios necesarios para cada uno de ellos.
% En el caso que E sí este instanciada, va a pasar algo similiar a lo que pasa en Ejercito(+E), donde ésta devuelve true con el mismo E, pero tambien
% prueba con todos los otros posibles ejercitos si unifican, devolviendo false.Por lo que la funcion va a devolver los edificios necesarios para E en Ed, pero luego se va a colgar.

%Como se menciono, en el caso de que Ej si esta instanciado pasa algo similar como en Ejercito, que el programa busca eternamente unificar Ej con algun otro ejercito y por lo tanto 
% trabandose, eso lo hace no reversible.

% Ej 4 : índice de superioridad para unidades
% ids ( +A , +B , -I )
% Enunciado:
  % Se cuenta con el predicado "ids" que calcula el IdS de una unidad sobre otra instanciando dicho valor en el tercer parámetro.
  % Sin embargo, este sólo funciona en algunos casos particulares.
  % Completar y/o modificar la implementación de este predicado para que:
  % a) funcione cuando los primeros dos argumentos corresponden a la misma unidad.
  % En este caso se debe instanciar el tercer parámetro en 1.
  % b) funcione cuando el par de los primeros dos argumentos se corresponde a uno de los ya contemplados pero en el orden inverso.
  % En este caso se debe instanciar el tercer parámetro con el inverso multiplicativo del caso contemplado.
  % c) no se cuelgue ni genere soluciones repetidas.
ids(jinete,       arquero,      1.5):- !.
ids(jinete,       guerrillero,  0.5):- !.
ids(lancero,      jinete,       2):- !.
ids(lancero,      arquero,      0.6):- !.
ids(guerrillero,  lancero,      1.1):- !.
ids(guerrillero,  arquero,      2):- !.



ids(X,X,1):- !.
ids(X,Y,I) :- unidad(X),unidad(Y),ids(Y,X,I2), I is 1/I2.


%% Reversibilidad: Si I no esta instanciado, se intentara unificar con alguno de los predicados ya dados por "default" instanciando I con el valor adecuado, en caso de que ninguno de ellos
% unifique se vera si son la misma unidad, de ser asi unificara el I con un 1, si este nuevamente no es el caso, se vera de unificar cambiando de lugar X e Y .
% En caso de que I esta instanciado, va a ver si unifica con alguno de los predicados "default" o si son iguales, o si unifica al cambiar de lugar las variables. Al hacer esto 
% ahora se deja I2 como variable no instanciada, y se instanciara con el primer predicado cuyas unidades  concuerden y se hara un cut cortando otras posibles soluciones,
% luego se vera si el I2 instanciado es igual al valor dado originalmente. 
% Por lo explicado anteriormente es reversible en I, se esperaria que si esta instanciado I devuelva true si se puede unificar con alguno de las relaciones de unidades dadas (tanto las defult como
% las que definimos nosotros) y sucede.


% Ej 5
% ids ( +A , +B , -I )
ids((UA,CA),(UB,CB),Ib) :- ids(UA,UB,Iu), Ib is Iu * (CA / CB).
% gana ( +A , +B )
gana(A,B) :- ids(A,B,I), I >= 1.
gana(_,[]) :- !.
gana([A|AS],[B|BS]) :- gana(A,B), gana([A|AS],BS), !.
gana([A|AS],[B|BS]) :- gana(B,A), gana(AS,[B|BS]), !.

% ganaA ( ?A , +B , ?N )
ganaA(A,B,N) :- nonvar(A), gana(A,B).

ganaA((S,T),(UB,CB),N) :-  var(N),between(1,CB,T),  unidad(S), gana((S,T),(UB,CB)).
ganaA((S,N),(UB,CB),N) :-  nonvar(N), unidad(S), gana((S,N),(UB,CB)).

ganaA(A,[B|BS],N) :-  var(N), cantUnisEnEjercito([B|BS],Cb),between(1,Cb,T), ejercitosDeNSoldados(T,A), gana(A,[B|BS]).
ganaA(A,[B|BS],N) :-  nonvar(N),ejercitosDeNSoldados(N,A), gana(A,[B|BS]).


cantUnisEnEjercito([],0).
cantUnisEnEjercito((UA,CA),CA).
cantUnisEnEjercito([(UA,CA)|LS],C) :- cantUnisEnEjercito(LS,C2), C is CA+C2.

%% No utilizamos el predicado Ejercito, ya que ganaA en ciertos casos tiene que generar ejercitos con la misma cantidad de unidades que el ya instanciado
% aprovechamos una funcion auxiliar(ejercitosdeNSoldados) que ya nos provee un ejercito con la cantidad de unidades que necesitamos. Si utilizaramos Ejercito
% tendriamos que generar tambien todos los ejercitos posibles con menos unidades de las necesarias.


% Ej 6 : instancia un pueblo para derrotar a un ejército enemigo
% puebloPara ( +En , ?A , -Ed , -Ej )
puebloPara([],[],[],[]).
puebloPara(En,A,Ed,Ej) :- var(A), generarEjYEd(En,Ed,Ej), aldeanosNecesarios(Ed,Ej,A).
puebloPara(En,A,Ed,Ej) :- nonvar(A), generarEjYEd(En,Ed,Ej), ejercitosConNaldeanos(A,Ej,Ed).

generarEjYEd(En,Ed,Ej):-ganaA(Ej,En,N), edificiosNecesarios(Ej,Ed).

ejercitosConNaldeanos(A,Ej,Ed) :- costo(Ed,C1), costo(Ej,C2), Ct is C1 + C2 ,R is A * 50, R >= Ct.
aldeanosNecesarios(Ed,Ej,A) :- costo(Ed,C1), costo(Ej,C2), Ct is C1+C2, A is ceiling(Ct/50) .

% Ej 7 : pueblo óptimo (en cantidad de aldenos necesarios)
% puebloOptimoPara( +En , ?A , -Ed , -Ej )
puebloOptimoPara([],[],[],[]).
puebloOptimoPara(En, A, Ed, Ej) :- puebloPara(En,A,Ed,Ej), not(hayMejor(En,A,Ed,Ej)).
hayMejor(En,A,Ed,Ej) :- puebloPara(En,A2,_,_), A > A2.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsCosto(10).
testCosto(1) :- costo([(arquero, 2)], 180).
testCosto(2) :- costo([cuartel], 300).
testCosto(3) :- costo([establo, cuartel], 700).
testCosto(4) :- costo([establo, cuartel, arqueria], 1030).
testCosto(5) :- costo([(lancero, 5), (arquero, 2)], 580).
testCosto(6) :- costo([(guerrillero, 7), (jinete, 1)], 610).
testCosto(7) :- costo([], 0).
testCosto(8) :- costo([cuartel, arqueria], 630).
testCosto(9) :- costo([(lancero, 1), (arquero, 77), (jinete, 2), (arquero, 8)], 7970).
testCosto(10) :- costo([(guerrillero, 2),(lancero, 3), (guerrillero, 4), (jinete, 5)], 1260).

cantidadTestsEjercito(5).
testEjercito(1) :- ejercito([(lancero, 1), (jinete, 3)]), !.
testEjercito(2) :- ejercito([(jinete, 5)]), !.
testEjercito(3) :- ejercito([(guerrillero, 4), (guerrillero, 2)]), !.
testEjercito(4) :- ejercito([(arquero, 1)]), !.
testEjercito(5) :- ejercito([(arquero, 2), (guerrillero, 2), (jinete, 2), (lancero, 1)]), !.

cantidadTestsEdificios(5).
testEdificios(1) :- edificiosNecesarios([(arquero, 2), (guerrillero, 2)], [arqueria]).
testEdificios(2) :- edificiosNecesarios([(arquero, 11)], [arqueria]).
testEdificios(3) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed,[arqueria, cuartel]).
testEdificios(4) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed, [cuartel, arqueria]).
testEdificios(5) :- edificiosNecesarios([(lancero, 1), (jinete, 10)], Ed), mismos(Ed, [establo, cuartel]).

% Auxiliar para chequear si tienen los mismos elementos
mismos(A,B) :- inc(A,B), inc(B,A).
inc([],_).
inc([A|As],Bs) :- member(A,Bs), inc(As,Bs).

cantidadTestsIdS(8).
testIdS(1) :- ids(jinete, jinete, X), X=:=1.
testIdS(2) :- ids(jinete, lancero, X), X=:=0.5.
testIdS(3) :- ids(lancero, jinete, X), X=:=2.
testIdS(4) :- ids(guerrillero, guerrillero, X), X=:=1.
testIdS(5) :- ids(lancero, guerrillero, X), X=:=0.9090909090909091.
testIdS(6) :- ids(arquero, lancero, X), X=:=1.6666666666666667.
testIdS(7) :- ids(arquero, guerrillero, X), X=:=0.5.
testIdS(8) :- ids(lancero, lancero, X), X=:=1.

cantidadTestsGanaA(5).
testGanaA(1) :- ganaA(E, (jinete, 3), 3), gana(E, (jinete, 3)), !.
testGanaA(2) :- not(ganaA(_, (guerrillero, 7), 6)).
testGanaA(3) :- ganaA(E, [(arquero, 1), (jinete, 1), (lancero, 1)], 2), gana(E, [(arquero, 1), (jinete, 1), (lancero, 1)]), !.
testGanaA(4) :- not(ganaA((guerrillero, 2),[(arquero, 2), (lancero, 4), (jinete, 6)], 2)).
testGanaA(5) :- not(ganaA([(arquero, 2), (jinete, 2), (guerrillero, 2)], [(lancero, 6)], 6)).

cantidadTestsPueblo(4).
testPueblo(1) :- En=[(jinete, 3)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej, En), !.
testPueblo(2) :- En=[(arquero, 1), (lancero, 4)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(3) :- En=[(guerrillero, 5)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(4) :- En=[(jinete, 1), (lancero, 1), (guerrillero, 2), (arquero, 2)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.

cantidadTestsPuebloOptimo(5).
testPuebloOptimo(1) :- En=[(jinete,2)], puebloOptimoPara(En,8,[cuartel],[(lancero,1)]), !.
testPuebloOptimo(2) :- En=[(jinete,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.%Si no optimizan recursos.
testPuebloOptimo(3) :- En=[(arquero,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.
testPuebloOptimo(4) :- En=[(guerrillero, 2), (arquero, 3)], puebloOptimoPara(En, 10, [arqueria], [(guerrillero, 2)]), !.
testPuebloOptimo(5) :- En=[(arquero,4)], not(puebloOptimoPara(En,5,_,_)).

tests(costo) :- cantidadTestsCosto(M), forall(between(1,M,N), testCosto(N)).
tests(ejercito) :- cantidadTestsEjercito(M), forall(between(1,M,N), testEjercito(N)).
tests(edificios) :- cantidadTestsEdificios(M), forall(between(1,M,N), testEdificios(N)).
tests(ids) :- cantidadTestsIdS(M), forall(between(1,M,N), testIdS(N)).
tests(ganaA) :- cantidadTestsGanaA(M), forall(between(1,M,N), testGanaA(N)).
tests(pueblo) :- cantidadTestsPueblo(M), forall(between(1,M,N), testPueblo(N)).
tests(puebloOptimo) :- cantidadTestsPuebloOptimo(M), forall(between(1,M,N), testPuebloOptimo(N)).

tests(todos) :-
  tests(costo),
  tests(ejercito),
  tests(edificios),
  tests(ids),
  tests(ganaA),
  tests(pueblo),
  tests(puebloOptimo).

tests :- tests(todos).
