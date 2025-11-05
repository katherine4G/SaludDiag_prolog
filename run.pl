% run.pl
:- initialization(main, main).
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded('server/paths.pl').
:- use_module('server/server.pl', [server/1, stop_all/0]).

main :-
    setup_call_cleanup(
        (
            catch(stop_all, _, true),              % apaga cualquier servidor previo
            server(8080),                          % inicia servidor
            format('Servidor iniciado en http://localhost:8080/~n', []),
            at_halt(on_exit)                       % registra cierre limpio
        ),
        wait_forever,
        (
            stop_all,
            format('~nServidor detenido correctamente.~n', [])
        )
    ).

wait_forever :-
    thread_get_message(_).

on_exit :-
    stop_all,
    format('~n Servidor detenido correctamente (Ctrl + C).~n', []).
