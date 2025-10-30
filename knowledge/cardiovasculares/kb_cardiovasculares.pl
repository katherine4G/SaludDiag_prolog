% kmowledge/cardiovasculares/kb_cardiovasculares.pl
:- module(kb_cardiovasculares, [es_dolencia_cardiovascular/1, sintoma_de/2]).

es_dolencia_cardiovascular(infarto).
es_dolencia_cardiovascular(hipertension).

sintoma_de(infarto, dolor_toracico_opresivo).
sintoma_de(infarto, disnea).
sintoma_de(hipertension, cefalea).
sintoma_de(hipertension, zumbido_oidos).
