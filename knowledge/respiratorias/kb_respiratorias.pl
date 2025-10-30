% knowledge/respiratorias/kb_respiratorias.pl

:- module(kb_respiratorias, [
    sintoma_de/2,
    categoria/1,
    es_dolencia_respiratoria/1
]).

:- dynamic sintoma_de/2.

categoria(respiratoria).

% ------------------------------------
% Enfermedades respiratorias
% ------------------------------------
es_dolencia_respiratoria(asma).
es_dolencia_respiratoria(bronquitis).
es_dolencia_respiratoria(neumonia).

% ------------------------------------
% Síntomas asociados
% ------------------------------------
sintoma_de(asma, disnea).
sintoma_de(asma, tos).
sintoma_de(asma, opresion_toracica).

sintoma_de(bronquitis, tos).
sintoma_de(bronquitis, flema).
sintoma_de(bronquitis, fiebre).

sintoma_de(neumonia, fiebre_alta).
sintoma_de(neumonia, dolor_toracico_opresivo).
sintoma_de(neumonia, dificultad_respirar).
