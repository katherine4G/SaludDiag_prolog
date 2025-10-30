% knowledge/neurologicas/kb_neurologicas.pl
:- module(kb_neurologicas, [es_dolencia_neurologica/1, sintoma_de/2]).

% ------------------------------------
% Dolencias neurológicas comunes
% ------------------------------------
es_dolencia_neurologica(migraña).
es_dolencia_neurologica(epilepsia).
es_dolencia_neurologica(parkinson).
es_dolencia_neurologica(esclerosis_multiple).

% ------------------------------------
% Síntomas asociados
% ------------------------------------
sintoma_de(migraña, cefalea_intensa).
sintoma_de(migraña, fotofobia).
sintoma_de(migraña, nauseas).

sintoma_de(epilepsia, convulsiones).
sintoma_de(epilepsia, perdida_conciencia).
sintoma_de(epilepsia, rigidez_muscular).

sintoma_de(parkinson, temblor_manos).
sintoma_de(parkinson, rigidez_muscular).
sintoma_de(parkinson, lentitud_movimientos).

sintoma_de(esclerosis_multiple, vision_borrosa).
sintoma_de(esclerosis_multiple, debilidad_muscular).
sintoma_de(esclerosis_multiple, perdida_equilibrio).
