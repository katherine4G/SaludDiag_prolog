:- module(kb_respiratorias, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(respiratoria).

sintoma_de(asma, disnea).
sintoma_de(asma, tos).
sintoma_de(asma, opresion_toracica).
