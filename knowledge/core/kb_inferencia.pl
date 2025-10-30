:- module(kb_inferencia, [
    score_enfermedad/3,
    categoria_score/3,
    mejor_categoria_por_score/2
]).

:- use_module(knowledge/core/kb_base).
:- use_module(library(lists)).

score_enfermedad(P, Enf, Score) :-
    findall(S, (kb_base:tiene(P,S), sintoma_de(Enf,S)), L),
    length(L, Score).

categoria_score(P, Categoria, Score) :-
    findall(S, (
        current_predicate(M:categoria/1),
        call(M:categoria, Categoria),
        call(M:sintoma_de, _, S),
        kb_base:tiene(P,S)
    ), L),
    length(L, Score).

mejor_categoria_por_score(P, Mejor) :-
    findall(Score-Cat, categoria_score(P, Cat, Score), Pares),
    sort(Pares, Ordenados),
    last(Ordenados, _-Mejor).
