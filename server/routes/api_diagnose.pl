:- module(api_diagnose, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(knowledge(core/kb_central)).
:- use_module(server(utils/logger)).

:- http_handler(root(api/diagnose), api_diagnose, []).

api_diagnose(Request) :-
	catch(http_read_json_dict(Request, DictIn),
		_,
		throw(http_reply(bad_request('JSON inválido')))),
	Syms = DictIn.get(symptoms),
	maplist(to_atom, Syms, Symptoms),
	retractall(kb_central : tiene(temp, _)),
	forall(member(S, Symptoms),
		assertz(kb_central : tiene(temp, S))),
	kb_central : tipo_probable(temp, Tipo),
	kb_central : enfermedades_posibles(temp, Enferms),
	kb_central : explicacion_categoria(temp, Tipo, Det),
	log(green, 'Diagnóstico: ~w -> ~w~n', [Tipo, Enferms]),
	reply_json_dict(_{tipo : Tipo, enfermedades : Enferms, detalle : Det}).

to_atom(X, A) :-
	(atom(X) ->
	A = X;
	atom_string(A, X)).
