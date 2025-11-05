% knowledge/respiratorias/kb_respiratorias.pl

:- module(kb_respiratorias, [
    sintoma_de/2,
    categoria/1,
    es_dolencia_respiratoria/1
]).

:- dynamic sintoma_de/2.

categoria(respiratoria).

% ------------------------------------
% Enfermedades respiratorias
% ------------------------------------
es_dolencia_respiratoria(asma).
es_dolencia_respiratoria(bronquitis).
es_dolencia_respiratoria(neumonia).
es_dolencia_respiratoria(faringitis).
es_dolencia_respiratoria(laringitis).
es_dolencia_respiratoria(sinusitis).
es_dolencia_respiratoria(rinitis_alergica).
es_dolencia_respiratoria(epoc).
es_dolencia_respiratoria(tuberculosis).
es_dolencia_respiratoria(covid19).
es_dolencia_respiratoria(resfriado_comun).
es_dolencia_respiratoria(pleuresia).
es_dolencia_respiratoria(laringotraqueitis).
es_dolencia_respiratoria(edema_pulmonar).
es_dolencia_respiratoria(enfisema).
es_dolencia_respiratoria(neumotorax).
es_dolencia_respiratoria(bronquiolitis).
es_dolencia_respiratoria(traqueitis).
es_dolencia_respiratoria(pulmonia_bacteriana).
es_dolencia_respiratoria(pulmonia_viral).
es_dolencia_respiratoria(aspergilosis).
es_dolencia_respiratoria(alergia_respiratoria).
es_dolencia_respiratoria(carcinoma_pulmonar).
es_dolencia_respiratoria(fibrosis_pulmonar).
es_dolencia_respiratoria(bronquiectasia).
es_dolencia_respiratoria(sarcoidosis_pulmonar).
es_dolencia_respiratoria(sindrome_respiratorio_agudo).
es_dolencia_respiratoria(alveolitis).
es_dolencia_respiratoria(hiperventilacion_cronica).
es_dolencia_respiratoria(asma_ocupacional).
es_dolencia_respiratoria(bronquitis_alergica).
es_dolencia_respiratoria(otitis_media_respiratoria).
es_dolencia_respiratoria(sinusitis_maxilar).
es_dolencia_respiratoria(bronconeumonia).
es_dolencia_respiratoria(neumonitis_intersticial).
es_dolencia_respiratoria(tracheobronquitis).
es_dolencia_respiratoria(candidiasis_pulmonar).
es_dolencia_respiratoria(virus_sincitial_respiratorio).
es_dolencia_respiratoria(influenza_b).
es_dolencia_respiratoria(influenza_a).
es_dolencia_respiratoria(laringotraqueobronquitis).
es_dolencia_respiratoria(hemoptisis_infecciosa).
es_dolencia_respiratoria(edema_agudo_pulmonar).
es_dolencia_respiratoria(pulmonia_atipica).
es_dolencia_respiratoria(pulmonia_cronica).
es_dolencia_respiratoria(aspergilosis_broncopulmonar).
es_dolencia_respiratoria(rinitis_vasomotora).
es_dolencia_respiratoria(silicosis).

% ------------------------------------
% Síntomas asociados
% ------------------------------------
sintoma_de(asma, disnea).
sintoma_de(asma, tos).
sintoma_de(asma, opresion_toracica).
sintoma_de(bronquitis, tos).
sintoma_de(bronquitis, flema).
sintoma_de(bronquitis, fiebre).
sintoma_de(neumonia, fiebre_alta).
sintoma_de(neumonia, dolor_toracico_opresivo).
sintoma_de(neumonia, dificultad_respirar).
sintoma_de(faringitis, dolor_garganta).
sintoma_de(faringitis, fiebre_leve).
sintoma_de(laringitis, voz_ronca).
sintoma_de(laringitis, tos_seca).
sintoma_de(sinusitis, dolor_frontal).
sintoma_de(sinusitis, congestion_nasal).
sintoma_de(rinitis_alergica, estornudos).
sintoma_de(rinitis_alergica, picazon_nasal).
sintoma_de(epoc, tos_cronica).
sintoma_de(epoc, dificultad_respirar).
sintoma_de(tuberculosis, tos_cronica).
sintoma_de(tuberculosis, perdida_peso).
sintoma_de(covid19, fiebre).
sintoma_de(covid19, disnea).
sintoma_de(resfriado_comun, congestion_nasal).
sintoma_de(resfriado_comun, dolor_garganta).
sintoma_de(pleuresia, dolor_toracico_punzante).
sintoma_de(edema_pulmonar, disnea_nocturna).
sintoma_de(enfisema, respiracion_silbante).
sintoma_de(neumotorax, dolor_toracico_subito).
sintoma_de(bronquiolitis, sibilancias).
sintoma_de(traqueitis, tos_metalica).
sintoma_de(pulmonia_bacteriana, escalofrios).
sintoma_de(pulmonia_viral, fiebre_moderada).
sintoma_de(aspergilosis, tos_con_sangre).
sintoma_de(alergia_respiratoria, ojos_llorosos).
sintoma_de(carcinoma_pulmonar, tos_persistente).
sintoma_de(fibrosis_pulmonar, tos_seca_cronica).
sintoma_de(fibrosis_pulmonar, fatiga).
sintoma_de(bronquiectasia, tos_productiva).
sintoma_de(bronquiectasia, flema_cronica).
sintoma_de(sarcoidosis_pulmonar, disnea_progresiva).
sintoma_de(sarcoidosis_pulmonar, tos_seca).
sintoma_de(sindrome_respiratorio_agudo, fiebre_alta).
sintoma_de(sindrome_respiratorio_agudo, disnea_aguda).
sintoma_de(alveolitis, tos_seca_persistente).
sintoma_de(alveolitis, disnea_cronica).
sintoma_de(hiperventilacion_cronica, mareo).
sintoma_de(hiperventilacion_cronica, hormigueo_manos).
sintoma_de(asma_ocupacional, disnea_nocturna).
sintoma_de(asma_ocupacional, tos_recurrente).
sintoma_de(bronquitis_alergica, sibilancias).
sintoma_de(bronquitis_alergica, tos_nocturna).
sintoma_de(bronconeumonia, fiebre_moderada).
sintoma_de(bronconeumonia, tos_con_flema).
sintoma_de(neumonitis_intersticial, fatiga).
sintoma_de(neumonitis_intersticial, disnea_leve).
sintoma_de(candidiasis_pulmonar, tos_con_sangre).
sintoma_de(influenza_b, fiebre_alta).
sintoma_de(influenza_b, dolor_muscular).
sintoma_de(rinitis_vasomotora, congestion_nasal).
sintoma_de(rinitis_vasomotora, estornudos_frecuentes).
sintoma_de(silicosis, tos_cronica).
sintoma_de(silicosis, dificultad_respirar).
