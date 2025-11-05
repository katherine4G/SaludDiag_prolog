% knowledge/digestivas/kb_digestivas.pl
:- module(kb_digestivas, [es_dolencia_digestiva/1, sintoma_de/2]).

% ------------------------------------
% Dolencias digestivas 
% ------------------------------------
es_dolencia_digestiva(gastritis).
es_dolencia_digestiva(ulcera_gastrica).
es_dolencia_digestiva(hepatitis).
es_dolencia_digestiva(colitis).
es_dolencia_digestiva(gastroenteritis).
es_dolencia_digestiva(reflujo_gastroesofagico).
es_dolencia_digestiva(intolerancia_lactosa).
es_dolencia_digestiva(pancreatitis).
es_dolencia_digestiva(apendicitis).
es_dolencia_digestiva(sindrome_intestino_irritable).
es_dolencia_digestiva(esteatosis_hepatica).
es_dolencia_digestiva(hernia_hiatal).
es_dolencia_digestiva(enfermedad_celiaca).
es_dolencia_digestiva(estreñimiento_cronico).
es_dolencia_digestiva(gastroesofagitis).
es_dolencia_digestiva(cancer_gastrico).
es_dolencia_digestiva(hepatomegalia).
es_dolencia_digestiva(cirrosis).
es_dolencia_digestiva(polidipsia_digestiva).
es_dolencia_digestiva(ulcera_duodenal).
es_dolencia_digestiva(diverticulitis).
es_dolencia_digestiva(hemorroides).
es_dolencia_digestiva(proctitis).
es_dolencia_digestiva(gastrocolitis).
es_dolencia_digestiva(gastroesclerosis).
es_dolencia_digestiva(hepatitis_alcoholica).
es_dolencia_digestiva(colecistitis).
es_dolencia_digestiva(colangitis).
es_dolencia_digestiva(ulcera_peptica).
es_dolencia_digestiva(enteritis_cronica).
es_dolencia_digestiva(duodenitis).
es_dolencia_digestiva(peritonitis).
es_dolencia_digestiva(gastroparesia).
es_dolencia_digestiva(litiasis_biliar).
es_dolencia_digestiva(polidipsia_digestiva_aguda).
es_dolencia_digestiva(hipersecrecion_acida).
es_dolencia_digestiva(amebiasis_intestinal).
es_dolencia_digestiva(giardiasis_digestiva).
es_dolencia_digestiva(colangitis_esclerosante).
es_dolencia_digestiva(ascitis_digestiva).
es_dolencia_digestiva(ulcera_esofagica).
es_dolencia_digestiva(hernia_umibilical).
es_dolencia_digestiva(absceso_hepatico).
es_dolencia_digestiva(colelitiasis).
es_dolencia_digestiva(pancreatopatia_cronica).
es_dolencia_digestiva(hepatopatia_grasa).
es_dolencia_digestiva(estomatitis).
es_dolencia_digestiva(esofagitis).
es_dolencia_digestiva(fisura_anal).

% ------------------------------------
% Síntomas asociados
% ------------------------------------
sintoma_de(gastritis, dolor_abdominal).
sintoma_de(gastritis, nausea).
sintoma_de(gastritis, ardor_estomago).
sintoma_de(ulcera_gastrica, dolor_abdominal_intenso).
sintoma_de(ulcera_gastrica, vomito_con_sangre).
sintoma_de(ulcera_gastrica, heces_negras).
sintoma_de(hepatitis, ictericia).
sintoma_de(hepatitis, cansancio).
sintoma_de(hepatitis, fiebre).
sintoma_de(colitis, diarrea).
sintoma_de(colitis, dolor_abdominal).
sintoma_de(colitis, gases).
sintoma_de(gastroenteritis, vomitos).
sintoma_de(gastroenteritis, diarrea_liquida).
sintoma_de(gastroenteritis, fiebre_leve).
sintoma_de(reflujo_gastroesofagico, acidez_estomacal).
sintoma_de(reflujo_gastroesofagico, regurgitacion).
sintoma_de(intolerancia_lactosa, distension_abdominal).
sintoma_de(intolerancia_lactosa, gases).
sintoma_de(pancreatitis, dolor_abdominal_severo).
sintoma_de(pancreatitis, vomitos_frecuentes).
sintoma_de(apendicitis, dolor_fosa_derecha).
sintoma_de(apendicitis, nauseas).
sintoma_de(sindrome_intestino_irritable, cambios_habito_intestinal).
sintoma_de(sindrome_intestino_irritable, flatulencia).
sintoma_de(esteatosis_hepatica, pesadez_postprandial).
sintoma_de(hernia_hiatal, dificultad_tragar).
sintoma_de(enfermedad_celiaca, perdida_peso).
sintoma_de(estreñimiento_cronico, esfuerzo_defecar).
sintoma_de(gastroesofagitis, dolor_toracico_postcomida).
sintoma_de(cancer_gastrico, perdida_apetito).
sintoma_de(cancer_gastrico, vomitos_con_sangre).
sintoma_de(cirrosis, ictericia).
sintoma_de(cirrosis, abdomen_distendido).
sintoma_de(diverticulitis, fiebre_moderada).
sintoma_de(diverticulitis, dolor_flanco_izquierdo).
sintoma_de(hemorroides, sangrado_rectal).
sintoma_de(proctitis, dolor_rectal).
sintoma_de(gastrocolitis, colicos_intensos).
sintoma_de(gastroesclerosis, rigidez_gastrica).
sintoma_de(gastroesclerosis, nausea).
sintoma_de(hepatitis_alcoholica, ictericia).
sintoma_de(hepatitis_alcoholica, vomito).
sintoma_de(colecistitis, dolor_flanco_derecho).
sintoma_de(colecistitis, fiebre_moderada).
sintoma_de(colangitis, fiebre_alta).
sintoma_de(colangitis, ictericia).
sintoma_de(ulcera_peptica, acidez_estomacal).
sintoma_de(ulcera_peptica, dolor_nocturno).
sintoma_de(enteritis_cronica, diarrea_cronica).
sintoma_de(duodenitis, ardor_estomago).
sintoma_de(peritonitis, dolor_abdominal_generalizado).
sintoma_de(peritonitis, fiebre_alta).
sintoma_de(gastroparesia, plenitud_postprandial).
sintoma_de(litiasis_biliar, nauseas).
sintoma_de(litiasis_biliar, ictericia_leve).
sintoma_de(amebiasis_intestinal, diarrea_con_sangre).
sintoma_de(amebiasis_intestinal, colicos).
sintoma_de(giardiasis_digestiva, diarrea_espumosa).
sintoma_de(colangitis_esclerosante, ictericia_progresiva).
sintoma_de(ulcera_esofagica, dolor_pecho_quemante).
sintoma_de(absceso_hepatico, fiebre_nocturna).
sintoma_de(esofagitis, dolor_al_tragar).
sintoma_de(fisura_anal, dolor_defecar).
sintoma_de(fisura_anal, sangrado_anal_leve).
