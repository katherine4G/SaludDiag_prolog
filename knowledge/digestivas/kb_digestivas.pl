% knowledge/digestivas/kb_digestivas.pl
:- module(kb_digestivas, [es_dolencia_digestiva/1, sintoma_de/2]).

% ------------------------------------
% Dolencias digestivas frecuentes
% ------------------------------------
es_dolencia_digestiva(gastritis).
es_dolencia_digestiva(ulcera_gastrica).
es_dolencia_digestiva(hepatitis).
es_dolencia_digestiva(colitis).

% ------------------------------------
% Síntomas asociados
% ------------------------------------
sintoma_de(gastritis, dolor_abdominal).
sintoma_de(gastritis, nausea).
sintoma_de(gastritis, ardor_estomago).

sintoma_de(ulcera_gastrica, dolor_abdominal_intenso).
sintoma_de(ulcera_gastrica, vomito_con_sangre).
sintoma_de(ulcera_gastrica, heces_negras).

sintoma_de(hepatitis, ictericia).
sintoma_de(hepatitis, cansancio).
sintoma_de(hepatitis, fiebre).

sintoma_de(colitis, diarrea).
sintoma_de(colitis, dolor_abdominal).
sintoma_de(colitis, gases).
