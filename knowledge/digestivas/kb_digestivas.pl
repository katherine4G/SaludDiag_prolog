:- module(kb_digestivas, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(digestiva).

sintoma_de(gastritis, dolor_abdominal).
sintoma_de(gastritis, nausea).
