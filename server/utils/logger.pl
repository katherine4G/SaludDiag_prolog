:- module(logger, [log/3]).
:- use_module(library(ansi_term)).

log(Color, Fmt, Args) :-
	with_output_to(user_output,
		ansi_format([fg(Color)],
			Fmt,
			Args)).
