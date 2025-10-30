:- module(kb_cardiovasculares, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(cardiovascular).

sintoma_de(angina, dolor_toracico_opresivo).
sintoma_de(angina, disnea).
