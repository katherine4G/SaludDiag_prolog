%  knowledge/traumatologicas/kb_traumatologicas.pl
:- module(kb_traumatologicas, [es_dolencia_traumatica/1, sintoma_de/2]).

es_dolencia_traumatica(fractura).
es_dolencia_traumatica(contusion).
es_dolencia_traumatica(cortadura).

sintoma_de(fractura, dolor_localizado).
sintoma_de(fractura, deformidad).
sintoma_de(contusion, hematoma_visible).
sintoma_de(cortadura, sangrado).
