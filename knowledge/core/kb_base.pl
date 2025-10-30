:- module(kb_base, [
    tiene/2,
    sintoma_registrado/1,
    enfermedades_posibles/2,
    tipo_probable/2
]).

:- dynamic tiene/2.

/* ===== Detectar síntomas globalmente ===== */
sintoma_registrado(S) :-
    current_predicate(M:sintoma_de/2),
    call(M:sintoma_de, _, S).

/* ===== Calcular enfermedades posibles ===== */
enfermedades_posibles(P, EnfermsUnicas) :-
    findall(E,
        ( current_predicate(M:sintoma_de/2),
          call(M:sintoma_de, E, S),
          tiene(P, S)
        ),
        Enferms),
    sort(Enferms, EnfermsUnicas).

/* ===== Determinar tipo o categoría ===== */
tipo_probable(P, Tipo) :-
    current_predicate(M:categoria/1),
    call(M:categoria, Tipo),
    findall(S, (tiene(P,S), call(M:sintoma_de, _, S)), Ss),
    Ss \= [],
    !.
tipo_probable(_, desconocido).
