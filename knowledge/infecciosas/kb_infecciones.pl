% knowledge/infecciosas/kb_infecciones.pl
:- module(kb_infecciones, [es_dolencia_infecciosa/1, sintoma_de/2]).
es_dolencia_infecciosa(gripe).
es_dolencia_infecciosa(covid19).
es_dolencia_infecciosa(dengue).

sintoma_de(gripe, fiebre).
sintoma_de(gripe, tos).
sintoma_de(gripe, dolor_garganta).
sintoma_de(covid19, disnea).
sintoma_de(dengue, fiebre_alta).
