:- module(kb_infecciones, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(infecciosa).

sintoma_de(gripe, fiebre_alta).
sintoma_de(gripe, tos).
sintoma_de(gripe, rinorrea).
sintoma_de(covid19, fiebre_alta).
sintoma_de(covid19, disnea).
sintoma_de(covid19, mialgia).
