% run.pl
:- initialization(main, main).
:- set_prolog_flag(encoding, utf8).

:- ensure_loaded('server/paths.pl').
:- use_module('server/server.pl', [server/1, stop_all/0]).

main :-
    catch(stop_all, _, true),
    server(8080),
    format('Servidor iniciado en http://localhost:8080/~n', []),
    wait_loop.

wait_loop :-
    format('Presiona CTRL + C para detener .~n', []),
    read_line_to_string(user_input, _),
    catch(stop_all, _, true),
    format('Servidor detenido correctamente.~n'),
    halt(0).
