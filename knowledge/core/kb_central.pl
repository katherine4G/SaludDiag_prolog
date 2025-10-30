:- module(kb_central, [
    es_dolencia_infecciosa/1,
    es_dolencia_traumatica/1,
    es_dolencia_cardiovascular/1,
    categoria_de_enfermedad/2,
    sintoma_de/2,
    atajo_categoria/2,
    score_enfermedad/3,
    enfermedad_posible/2,
    enfermedades_posibles/2,
    categoria_score/3,
    mejor_categoria_por_score/2,
    tipo_probable/2,
    explicacion_categoria/3,
    tiene/2,
    reload_kb/0
]).

:- set_prolog_flag(encoding, utf8).
:- dynamic tiene/2.

:- use_module(library(lists)).
:- use_module(library(filesex)).


init_paths :-
    prolog_load_context(directory, CoreDir),
    directory_file_path(CoreDir, '..', KnowledgeDir),
    (   clause(user:file_search_path(knowledge, KnowledgeDir), _)
    ->  true
    ;   asserta(user:file_search_path(knowledge, KnowledgeDir))
    ).


kb_subdir_list([
    respiratorias, cardiovasculares, traumatologicas,
    digestivas, neurologicas, infecciosas
]).


reload_kb :-
    init_paths,
    source_file(reload_kb, ThisFile),
    file_directory_name(ThisFile, CoreDir),
    directory_file_path(CoreDir, '..', KnowledgeDir),
    expand_file_name(KnowledgeDir, [KnowledgeAbs]),
    format('Ruta base de conocimiento: ~w~n', [KnowledgeAbs]),
    kb_subdir_list(Subdirs),
    forall(
        member(Name, Subdirs),
        (
            directory_file_path(KnowledgeAbs, Name, DirAbs),
            format('Cargando categoría: ~w (~w)~n', [Name, DirAbs]),
            (   exists_directory(DirAbs)
            ->  load_dir_modules(DirAbs)
            ;   format('⚠️  No existe el directorio ~w~n', [DirAbs])
            )
        )
    ).


load_dir_modules(Dir) :-
    directory_files(Dir, Entries),
    include(valid_pl_file, Entries, PLs),
    forall(
        member(F, PLs),
        (
            directory_file_path(Dir, F, Path),
            format(' - Cargando archivo: ~w~n', [Path]),
            catch(load_files(Path, [if(changed), silent(true), imports([])]),
                  E,
                  print_message(error, E))
        )
    ).


valid_pl_file(File) :-
    file_name_extension(_, 'pl', File).


categoria_de_enfermedad(E, C) :-
    current_predicate(M:categoria/1),
    call(M:categoria, C),
    call(M:sintoma_de, E, _).

es_dolencia_infecciosa(E)     :- categoria_de_enfermedad(E, infecciosa).
es_dolencia_traumatica(E)     :- categoria_de_enfermedad(E, traumatica).
es_dolencia_cardiovascular(E) :- categoria_de_enfermedad(E, cardiovascular).

sintoma_de(E, S) :-
    findall(M,
        ( module_property(M, exports(Exports)),
          member(sintoma_de/2, Exports),
          atom_concat('kb_', _, M)
        ),
        Mods),
    member(Mod, Mods),
    call(Mod:sintoma_de, E, S).


atajo_categoria(P, traumatica) :-
    tiene(P, antecedente_golpe),
    tiene(P, dolor_localizado).

atajo_categoria(P, cardiovascular) :-
    tiene(P, dolor_toracico_opresivo),
    tiene(P, disnea).

atajo_categoria(P, infecciosa) :-
    tiene(P, fiebre),
    (   tiene(P, tos)
    ;   tiene(P, mialgia)
    ;   tiene(P, rinorrea)
    ;   tiene(P, diarrea)
    ).


score_enfermedad(P, Enf, Score) :-
    findall(S, (tiene(P, S), sintoma_de(Enf, S)), Ss),
    length(Ss, Score).

enfermedad_posible(P, Enf) :-
    score_enfermedad(P, Enf, N),
    N >= 2.

enfermedades_posibles(P, R) :-
    findall(E,
        (sintoma_de(E, _), score_enfermedad(P, E, S), S >= 2),
        L),
    sort(L, R).

categoria_score(P, C, Score) :-
    findall(S,
        (categoria_de_enfermedad(E, C), score_enfermedad(P, E, S)),
        L),
    sum_list(L, Score).

mejor_categoria_por_score(P, Mejor) :-
    findall(Score-Cat, categoria_score(P, Cat, Score), Pares),
    sort(Pares, Orden),
    last(Orden, _-Mejor).


tipo_probable(P, T) :-
    atajo_categoria(P, T), !.
tipo_probable(P, T) :-
    mejor_categoria_por_score(P, T), !.
tipo_probable(_, desconocido).

explicacion_categoria(P, T, detalle(atajo(T, Hechos))) :-
    atajo_categoria(P, T),
    findall(S, tiene(P, S), Hechos), !.

explicacion_categoria(P, T, detalle(score(T, ScoreCat, Top))) :-
    categoria_score(P, T, ScoreCat),
    findall(Score-Enf,
        (sintoma_de(Enf, _),
         score_enfermedad(P, Enf, Score),
         Score > 0),
        Pares),
    sort(Pares, Orden),
    reverse(Orden, Desc),
    first_n(3, Desc, Top).

first_n(N, L, R) :- length(R, N), append(R, _, L), !.
first_n(_, L, L).
