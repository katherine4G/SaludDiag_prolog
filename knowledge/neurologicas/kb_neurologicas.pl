:- module(kb_neurologicas, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(neurologica).

sintoma_de(migraña, cefalea).
sintoma_de(migraña, fotofobia).
sintoma_de(migraña, nausea).
