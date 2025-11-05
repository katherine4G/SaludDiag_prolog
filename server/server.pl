% server/server.pl
:- module(server, [server/1, stop/0, stop_all/0]).
:- set_prolog_flag(encoding, utf8).

% --- Librerías HTTP ---
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

% --- Rutas internas ---
:- use_module(server(routes/api_diagnose)).
:- use_module(server(routes/api_symptoms)).
:- use_module(server(routes/ui_page)).
:- use_module(server(utils/logger)).

% --- Handlers HTTP ---
:- http_handler(root(.), ui_page, []).
:- http_handler(root(api/symptoms), api_symptoms, []).
:- http_handler(root(api/diagnose), api_diagnose, []).
:- http_handler(root('app.js'), serve_app_js, []).

server(Port) :-
    ignore(stop_all),
    catch(
        http_server(http_dispatch, [port(Port), encoding(utf8)]),
        E,
        ( log(red, 'Error iniciando servidor: ~w~n', [E]), fail )
    ),
    log(green, 'Servidor iniciado en puerto ~w~n', [Port]).

stop :- stop_all.

% =====================================
% Detener todos los servidores HTTP y limpiar hilos residuales
% =====================================
stop_all :-
    % --- Detener servidores activos ---
    findall(Port,
        ( current_prolog_flag(http_server, Servers),
          member(_{port:Port}, Servers)
        ),
        Ports),
    ( Ports == [] ->
        log(yellow, 'No hay servidores activos.~n', [])
    ; forall(member(P, Ports),
          ( log(red, 'Cerrando servidor en puerto ~w...~n', [P]),
            catch(http_stop_server(P, [force(true)]), E,
                  log(red, 'Error al cerrar servidor (~w): ~w~n', [P, E]))
          )
      ),
      log(green, 'Servidores HTTP detenidos correctamente.~n', [])
    ),

    % --- Espera breve para evitar race condition ---
    sleep(0.2),

    % --- Forzar terminación de hilos residuales http@ / httpd@ ---
    repeat,
        findall(Id-Name, (
            thread_property(Id, alias(Name)),
            ( sub_atom(Name, 0, _, _, 'http@')
            ; sub_atom(Name, 0, _, _, 'httpd@')
            )
        ), List),
        ( List == [] ->
            !
        ; forall(member(Id-Name, List),
              (
                ( catch(thread_signal(Id, abort), _, true),
                  catch(thread_join(Id, _), _, true)
                ->  log(yellow, 'Hilo residual terminado: ~w~n', [Name])
                ;   log(gray, ' Esperando cierre natural: ~w~n', [Name])
                )
              )
          ),
          sleep(0.1),
          fail
        ),
    log(green, 'todos los hilos HTTP terminados correctamente.~n', []).

% Servir frontend
serve_app_js(_Req) :-
    http_reply_file('./static/app.js',
        [unsafe(true), mime_type('application/javascript')], []).