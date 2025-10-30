:- module(server, [server/1, stop/0]).
:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

/* ============================================
   🔧 Rutas de búsqueda de módulos
   ============================================ */
:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(server, Dir)), asserta(user:file_search_path(knowledge, Dir+'/../knowledge')), asserta(user:file_search_path(static, Dir+'/../static')).

/* ============================================
   📦 Módulos del proyecto
   ============================================ */
:- use_module(server(routes/dev_debug)).
:- use_module(server(routes/api_symptoms)).
:- use_module(server(routes/api_diagnose)).
:- use_module(server(routes/ui_page)).
:- use_module(server(utils/logger)).
:- use_module(knowledge(core/kb_central)).

/* ============================================
   🧠 Inicialización del conocimiento
   ============================================ */
:- kb_central:reload_kb.

/* ============================================
   🚀 Servidor HTTP
   ============================================ */
server(Port) :-
	ignore(stop),
	catch(http_server(http_dispatch,
			[port(Port), encoding(utf8)]),
		E,
		(log(red, 'Error iniciando servidor: ~w~n', [E]),
			fail)),
	log(green, 'Servidor iniciado en http://localhost:~w/~n', [Port]),
	thread_create(loop,
		_,
		[detached(true)]).

loop :-
	repeat,
	sleep(0.5),
	fail.

stop :-
	catch(http_stop_server(8080,
			[force(true)]),
		_,
		true),
	log(yellow, 'Servidor detenido.~n', []).

/* ============================================
   🧩 Depuración: verificar módulos KB cargados
   ============================================ */
debug_kb :-
	kb_central : reload_kb,
	findall(M,
		( module_property(M,
				exports(Exports)),
			member(sintoma_de/2, Exports),
			atom_concat('kb_', _, M)),
		Mods),
	(Mods = [] ->
	format('⚠️  Ningún módulo KB detectado.~n', []);
	format('~n~`=t~72|~nMÓDULOS KB CARGADOS:~n', []),
		forall(member(X, Mods),
			format(' - ~w~n', [X])),
		format('~`=t~72|~n', [])).

:- initialization(debug_kb, now).
