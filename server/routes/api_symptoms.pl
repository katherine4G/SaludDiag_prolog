:- module(api_symptoms, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module(knowledge(core/kb_central)).

:- http_handler(root(api/symptoms), api_symptoms, []).

api_symptoms(_Req) :-
	catch(collect_symptoms(List),
		E,
		(message_to_string(E, Msg),
			reply_json_dict(_{
					error : "internal_error",
					details : Msg
					},
				[status(500)]))).

collect_symptoms(List) :-
	% si kb_central no tiene predicados válidos, evita colgarse
(current_predicate(kb_central : sintoma_de/2) ->
	safe_findall(List);
	List = []).

safe_findall(List) :-
	catch(findall(S,
			(once(kb_central : sintoma_de(_, S))),
			Raw),
		_,
		Raw = []),
	sort(Raw, List),
	reply_json_dict(_{symptoms : List}).
