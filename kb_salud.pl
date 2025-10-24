:- module(kb_salud, [
    % Tipos / taxonomía
    es_dolencia_infecciosa/1,
    es_dolencia_traumatica/1,
    es_dolencia_cardiovascular/1,
    categoria_de_enfermedad/2,

    % Conocimiento clínico
    sintoma_de/2,

    % Reglas de ayuda y scoring
    atajo_categoria/2,
    score_enfermedad/3,
    enfermedad_posible/2,
    enfermedades_posibles/2,
    categoria_score/3,
    mejor_categoria_por_score/2,
    tipo_probable/2,
    explicacion_categoria/3
]).
:- set_prolog_flag(encoding, utf8).

/* Utilidades de listas (miembros y sumatoria) */
:- use_module(library(lists), [member/2, sum_list/2]).

% =========================================
% kb_salud.pl — Conocimiento de salud
% =========================================

% ----------------- Tipos de dolencia -----------------
es_dolencia_infecciosa(gripe).
es_dolencia_infecciosa(malaria).
es_dolencia_infecciosa(covid19).
es_dolencia_infecciosa(dengue).

es_dolencia_traumatica(fractura).
es_dolencia_traumatica(esguince).
es_dolencia_traumatica(contusion).
es_dolencia_traumatica(hematoma).
es_dolencia_traumatica(cortadura).

es_dolencia_cardiovascular(infarto).
es_dolencia_cardiovascular(hipertension).
es_dolencia_cardiovascular(ictus).
es_dolencia_cardiovascular(angina_de_pecho).
es_dolencia_cardiovascular(arritmia).

% ----------------- Mapeo: enfermedad → categoria -----
categoria_de_enfermedad(E, infecciosa)     :- es_dolencia_infecciosa(E).
categoria_de_enfermedad(E, traumatica)     :- es_dolencia_traumatica(E).
categoria_de_enfermedad(E, cardiovascular) :- es_dolencia_cardiovascular(E).

% ----------------- Síntomas por enfermedad ------------
% Infecciosas
sintoma_de(gripe, fiebre).
sintoma_de(gripe, tos).
sintoma_de(gripe, dolor_garganta).
sintoma_de(gripe, mialgia).
sintoma_de(gripe, cefalea).
sintoma_de(gripe, rinorrea).

sintoma_de(covid19, fiebre).
sintoma_de(covid19, tos).
sintoma_de(covid19, disnea).
sintoma_de(covid19, anosmia).
sintoma_de(covid19, fatiga).

sintoma_de(dengue, fiebre_alta).
sintoma_de(dengue, mialgia).
sintoma_de(dengue, artralgia).
sintoma_de(dengue, dolor_retroocular).
sintoma_de(dengue, exantema).
sintoma_de(dengue, sangrado_nasal).

sintoma_de(malaria, fiebre_periodica).
sintoma_de(malaria, escalofrios).
sintoma_de(malaria, sudoracion).
sintoma_de(malaria, cefalea).
sintoma_de(malaria, anemia).

% Traumatológicas
sintoma_de(fractura, dolor_localizado).
sintoma_de(fractura, deformidad).
sintoma_de(fractura, crepitacion).
sintoma_de(fractura, imposibilidad_apoyo).
sintoma_de(fractura, antecedente_golpe).

sintoma_de(esguince, dolor_localizado).
sintoma_de(esguince, inflamacion).
sintoma_de(esguince, movilidad_reducida).
sintoma_de(esguince, antecedente_torsion).

sintoma_de(contusion, hematoma_visible).
sintoma_de(contusion, dolor_localizado).
sintoma_de(contusion, antecedente_golpe).

sintoma_de(hematoma, hematoma_visible).
sintoma_de(hematoma, dolor_localizado).

sintoma_de(cortadura, sangrado).
sintoma_de(cortadura, herida_abierta).

% Cardiovasculares
sintoma_de(infarto, dolor_toracico_opresivo).
sintoma_de(infarto, irradiacion_brazo_izq).
sintoma_de(infarto, disnea).
sintoma_de(infarto, diaforesis).
sintoma_de(infarto, nauseas).

sintoma_de(angina_de_pecho, dolor_toracico_opresivo).
sintoma_de(angina_de_pecho, disnea).
sintoma_de(angina_de_pecho, esfuerzo_desencadenante).

sintoma_de(hipertension, cefalea).
sintoma_de(hipertension, zumbido_oidos).
sintoma_de(hipertension, vision_borrosa).

sintoma_de(ictus, debilidad_hemicuerpo).
sintoma_de(ictus, dificultad_hablar).
sintoma_de(ictus, desviacion_boca).
sintoma_de(ictus, confusion_aguda).

sintoma_de(arritmia, palpitaciones).
sintoma_de(arritmia, mareo).
sintoma_de(arritmia, sincope).

% ----------------- Pacientes de ejemplo ----------------
tiene(ana, fiebre).
tiene(ana, tos).
tiene(ana, dolor_garganta).
tiene(ana, fatiga).

tiene(luis, dolor_toracico_opresivo).
tiene(luis, disnea).
tiene(luis, diaforesis).

tiene(carla, antecedente_golpe).
tiene(carla, dolor_localizado).
tiene(carla, hematoma_visible).

% ----------------- Atajos clínicos ----------------------
atajo_categoria(P, traumatica) :-
    tiene(P, antecedente_golpe),
    tiene(P, dolor_localizado).

atajo_categoria(P, cardiovascular) :-
    tiene(P, dolor_toracico_opresivo),
    tiene(P, disnea).

% Ictus: síntoma neurológico + inicio_brusco
atajo_categoria(P, cardiovascular) :-
    (   tiene(P, debilidad_hemicuerpo)
    ;   tiene(P, dificultad_hablar)
    ;   tiene(P, desviacion_boca)
    ),
    tiene(P, inicio_brusco).

% Infecciosa: fiebre + (tos | mialgia | rinorrea | diarrea)
atajo_categoria(P, infecciosa) :-
    tiene(P, fiebre),
    (   tiene(P, tos)
    ;   tiene(P, mialgia)
    ;   tiene(P, rinorrea)
    ;   tiene(P, diarrea)
    ).

% ----------------- Scoring por síntomas -----------------
score_enfermedad(P, Enf, Score) :-
    findall(S, (tiene(P, S), sintoma_de(Enf, S)), Ss),
    length(Ss, Score).

enfermedad_posible(P, Enf) :-
    score_enfermedad(P, Enf, Score),
    Score >= 2.

% Robusto (findall + sort)
enfermedades_posibles(P, Unicas) :-
    findall(Enf, enfermedad_posible(P, Enf), L),
    sort(L, Unicas).

categoria_score(P, infecciosa, Score) :-
    findall(S, (es_dolencia_infecciosa(E), score_enfermedad(P, E, S)), L),
    sum_list(L, Score).
categoria_score(P, traumatica, Score) :-
    findall(S, (es_dolencia_traumatica(E), score_enfermedad(P, E, S)), L),
    sum_list(L, Score).
categoria_score(P, cardiovascular, Score) :-
    findall(S, (es_dolencia_cardiovascular(E), score_enfermedad(P, E, S)), L),
    sum_list(L, Score).

mejor_categoria_por_score(P, CatMax) :-
    findall(Score-Cat,
            ( member(Cat, [infecciosa, traumatica, cardiovascular]),
              categoria_score(P, Cat, Score)
            ),
            Pares),
    sort(Pares, Ordenados),
    last(Ordenados, _-CatMax).

tipo_probable(P, Tipo) :-
    atajo_categoria(P, Tipo), !.
tipo_probable(P, Tipo) :-
    mejor_categoria_por_score(P, Tipo).

explicacion_categoria(P, Tipo, detalle(atajo(Tipo, Hecho))) :-
    atajo_categoria(P, Tipo),
    findall(S, tiene(P,S), Hecho).
explicacion_categoria(P, Tipo, detalle(score(Tipo, ScoreCat, TopEnfs))) :-
    \+ atajo_categoria(P, _),
    categoria_score(P, Tipo, ScoreCat),
    findall(Score-Enf,
            ( member(Enf, [gripe, covid19, dengue, malaria,
                           fractura, esguince, contusion, hematoma, cortadura,
                           infarto, hipertension, ictus, angina_de_pecho, arritmia]),
              score_enfermedad(P, Enf, Score),
              Score > 0),
            Pares),
    sort(Pares, Orden), reverse(Orden, Desc),
    first_n(3, Desc, TopEnfs).

% ----------- Auxiliares internas (no exportar) ----------
first_n(N, L, R) :- length(R, N), append(R, _, L), !.
first_n(_, L, L).
