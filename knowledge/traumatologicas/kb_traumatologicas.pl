:- module(kb_traumatologicas, [sintoma_de/2, categoria/1]).
:- dynamic sintoma_de/2.
categoria(traumatologica).

sintoma_de(fractura, dolor_localizado).
sintoma_de(fractura, deformidad).
sintoma_de(esguince, inflamacion).
