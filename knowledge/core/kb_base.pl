% knowledge/core/kb_base.pl

:- module(kb_base, [
    tiene/2,
    sintoma_de/2,
    normalizar_sintoma/2
]).

:- dynamic tiene/2.

% ------------------------------------------------
% Ejemplos de síntomas globales comunes
% ------------------------------------------------
sintoma_de(desconocido, ninguno).

tiene(ana, fiebre).
tiene(ana, tos).

% ------------------------------------------------
% Normalización de síntomas equivalentes
% ------------------------------------------------
% Permite que fiebre y fiebre_alta se traten igual,
% nausea y nauseas también, etc.

% Casos equivalentes de fiebre
normalizar_sintoma(fiebre_alta, fiebre).
normalizar_sintoma(fiebre, fiebre).

% Casos respiratorios
normalizar_sintoma(dificultad_respirar, disnea).
normalizar_sintoma(disnea, disnea).
normalizar_sintoma(opresion_toracica, dolor_toracico_opresivo).
normalizar_sintoma(dolor_toracico_opresivo, dolor_toracico_opresivo).
normalizar_sintoma(tos, tos).

% Casos digestivos
normalizar_sintoma(nauseas, nausea).
normalizar_sintoma(nausea, nausea).
normalizar_sintoma(vomito_con_sangre, vomito_con_sangre).
normalizar_sintoma(diarrea, diarrea).
normalizar_sintoma(gases, gases).
normalizar_sintoma(ardor_estomago, ardor_estomago).

% Casos neurológicos
normalizar_sintoma(cefalea, cefalea).
normalizar_sintoma(cefalea_intensa, cefalea).
normalizar_sintoma(perdida_conciencia, perdida_conciencia).
normalizar_sintoma(convulsiones, convulsiones).
normalizar_sintoma(perdida_equilibrio, perdida_equilibrio).
normalizar_sintoma(vision_borrosa, vision_borrosa).
normalizar_sintoma(zumbido_oidos, zumbido_oidos).

% Casos traumatológicos
normalizar_sintoma(dolor_localizado, dolor_localizado).
normalizar_sintoma(deformidad, deformidad).
normalizar_sintoma(hematoma_visible, hematoma_visible).

% Casos cardiovasculares
normalizar_sintoma(cansancio, cansancio).
normalizar_sintoma(debilidad_muscular, debilidad_muscular).
normalizar_sintoma(temblor_manos, temblor_manos).
normalizar_sintoma(lentitud_movimientos, lentitud_movimientos).

% Casos hepáticos / infecciosos
normalizar_sintoma(ictericia, ictericia).
normalizar_sintoma(sangrado, sangrado).
normalizar_sintoma(heces_negras, heces_negras).

% Dolor abdominal
normalizar_sintoma(dolor_abdominal_intenso, dolor_abdominal).
normalizar_sintoma(dolor_abdominal, dolor_abdominal).

% Otros equivalentes directos
normalizar_sintoma(dolor_garganta, dolor_garganta).
normalizar_sintoma(fotofobia, fotofobia).

% Por defecto, deja el mismo síntoma
normalizar_sintoma(S, S).
