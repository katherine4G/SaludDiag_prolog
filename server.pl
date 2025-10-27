:- module(server, [server/1, stop/0]).
:- set_prolog_flag(encoding, utf8).
:- set_prolog_flag(verbose, silent).
:- style_check(-singleton).
:- style_check(-discontiguous).

/* --- Librerías HTTP --- */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), [member/2]).
:- use_module(library(dicts)).
:- use_module(library(apply)).

/* --- Base de conocimiento --- */
:- use_module('./kb_salud.pl', [
       sintoma_de/2,
       es_dolencia_infecciosa/1,
       es_dolencia_traumatica/1,
       es_dolencia_cardiovascular/1,
       tipo_probable/2,
       enfermedades_posibles/2,
       explicacion_categoria/3,
       tiene/2
   ]).

:- dynamic compare_scores/3.

/* ======================== LOGS COLOR SEGUROS ======================== */

log(Color, Fmt, Args) :-
    % Evita escribir en flujo HTTP, solo en salida estándar
    with_output_to(user_output, ansi_format([fg(Color)], Fmt, Args)).

/* ======================== RUTAS ======================== */

:- http_handler(root(.),               ui_page,         []).
:- http_handler(root(api/symptoms),    api_symptoms,    []).
:- http_handler(root(api/diagnose),    api_diagnose,    []).
:- http_handler(root('app.js'),        serve_app_js,    []).

/* ======================== ARRANQUE / PARADA ======================== */

server(Port) :-
    ignore(stop),
    catch(
        http_server(http_dispatch, [port(Port), encoding(utf8)]),
        E,
        ( log(red, 'Error iniciando servidor: ~w~n', [E]), fail )
    ),
    log(green, 'Servidor iniciado en ', []),
    log(cyan, 'http://localhost:~w/~n', [Port]),
    !.

stop :-
    findall(Port,
        (   current_prolog_flag(http_server, Servers),
            member(_{port:Port}, Servers)
        ),
        Ports),
    ( Ports == [] ->
        log(yellow, 'No hay servidores activos.~n', [])
    ; forall(member(Port, Ports),
          ( log(red, 'Deteniendo servidor en puerto ~w...~n', [Port]),
            catch(http_stop_server(Port, [force(true)]), _, true)
          )
      ),
      log(green, 'Todos los servidores cerrados correctamente.~n', [])
    ),
    forall(
        ( thread_property(Id, alias(Name)),
          ( sub_atom(Name, 0, _, _, 'httpd@')
          ; sub_atom(Name, 0, _, _, 'http@')
          )
        ),
        (   log(yellow, 'Cerrando hilo ~w...~n', [Name]),
            catch(thread_signal(Id, abort), _, true),
            catch(thread_join(Id, _), _, true)
        )
    ),
    !.

/* ======================== INTERFAZ HTML ======================== */

ui_page(_Request) :-
    reply_html_page(
      [ title('Diagnostico basico (Prolog)')
      , meta([charset='UTF-8'])
      , \styles
      ],
      \page_body
    ).

styles -->
    html(style([
      'body{font-family:system-ui,Segoe UI,Roboto,Arial,sans-serif;margin:0;background:#0f172a;color:#e5e7eb}',
      'header{padding:20px 24px;background:#111827;position:sticky;top:0}',
      'h1{margin:0;font-size:20px}',
      '.wrap{max-width:980px;margin:0 auto;padding:24px}',
      '.card{background:#1f2937;border-radius:16px;box-shadow:0 8px 24px rgba(0,0,0,.25);padding:20px;margin-bottom:16px}',
      '.row{display:flex;gap:16px;align-items:center;flex-wrap:wrap}',
      'button{border:0;border-radius:12px;padding:10px 16px;cursor:pointer;background:#10b981;color:#0b1320;font-weight:600}',
      'button:disabled{opacity:.6;cursor:not-allowed}',
      'input[type="text"]{padding:10px 12px;border-radius:10px;border:1px solid #334155;background:#0b1320;color:#e5e7eb}',
      '.symgrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:10px;max-height:360px;overflow:auto;padding:6px;border:1px solid #334155;border-radius:12px;background:#0b1320}',
      '.pill{display:flex;gap:8px;align-items:center;background:#111827;border:1px solid #334155;border-radius:999px;padding:6px 10px}',
      'label{cursor:pointer;font-size:14px}',
      'code{background:#0b1320;border:1px solid #334155;border-radius:8px;padding:2px 6px}',
      '.muted{color:#94a3b8;font-size:13px}',
      '.tag{display:inline-block;background:#0b1320;border:1px solid #334155;padding:4px 8px;border-radius:999px;margin-right:6px}',
      '.res h3{margin:.2rem 0 .6rem}',
      'footer{color:#94a3b8;padding:12px 24px;border-top:1px solid #334155}'
    ])).

page_body -->
  html([
    header(\header_bar),
    div(class(wrap), [
      div(class(card), [
        h2('Selecciona sintomas'),
        div(class(row), [
          input([type(text), id(q), placeholder('Filtrar sintomas'), oninput('filterSyms()')]),
          button([ id(btnLoad), onclick('loadSyms()') ], 'Cargar sintomas'),
          span(class(muted), 'Sugerencia: escribe "fiebre", "tos", "dolor", etc.')
        ]),
        div([id(symgrid), class(symgrid)], []),
        div(class(row), [
          button([ id(btnDiag), onclick('diagnose()'), disabled(true) ], 'Diagnosticar'),
          span(class(muted), 'El diagnostico es educativo, no medico.')
        ])
      ]),
      div([id(out), class('card res')], [
        h2('Resultado'),
        p(class(muted), 'Aun no hay resultado.')
      ])
    ]),
    footer(small('Hecho con SWI-Prolog + library(http).')),
    script([type('module'), src('/app.js')], [])
  ]).

header_bar -->
  html(div([class(row)], [
    h1('Demo: Clasificador de dolencia (Prolog)')
  ])).

/* ======================== API ======================== */

api_symptoms(_Req) :-
    setof(S, E^(sintoma_de(E,S)), All),
    reply_json_dict(_{symptoms: All}).

api_diagnose(Request) :-
    catch(
        http_read_json_dict(Request, DictIn),
        E,
        ( format(user_error, 'Error leyendo JSON: ~q~n', [E]),
          throw(http_reply(bad_request('Cuerpo JSON inválido')))
        )
    ),
    (   _{symptoms: Syms0} :< DictIn
    ->  true
    ;   throw(http_reply(bad_request('Falta el campo "symptoms"')))
    ),

    maplist(to_atom, Syms0, Syms),
    format(user_output, 'Sintomas recibidos: ~w~n', [Syms]),

    retractall(kb_salud:tiene(temp, _)),
    forall(member(S, Syms),
        ( assertz(kb_salud:tiene(temp, S)),
          format(user_output, 'Hecho agregado: tiene(temp, ~w)~n', [S])
        )
    ),

    findall(X, kb_salud:tiene(temp, X), HechosActuales),
    format(user_output, 'Hechos actuales en kb_salud: ~w~n', [HechosActuales]),

    (   catch((
            (   kb_salud:tipo_probable(temp, Tipo)
            ->  true
            ;   Tipo = desconocido
            ),
            (   kb_salud:enfermedades_posibles(temp, Enferms)
            ->  true
            ;   Enferms = []
            ),
            (   kb_salud:explicacion_categoria(temp, Tipo, Detalle)
            ->  true
            ;   Detalle = _{modo: none, tipo: Tipo, sintomas: Syms}
            )
        ), E, (
            format(user_error, 'Error interno al diagnosticar: ~q~n', [E]),
            throw(http_reply(server_error('Error interno en diagnóstico')))
        ))
    ),

    (   is_dict(Detalle) -> DetOut = Detalle
    ;   Detalle = detalle(score(Tipo, Score, Top)) ->
        (   is_list(Top)
        ->  findall([S,E], (member(S-E, Top)), TopJSON)
        ;   TopJSON = []
        ),
        DetOut = _{
            modo: score,
            tipo: Tipo,
            score_categoria: Score,
            top_enfermedades: TopJSON
        }
    ;   Detalle = detalle(atajo(Tipo, Hechos)) ->
        DetOut = _{
            modo: atajo,
            tipo: Tipo,
            sintomas: Hechos
        }
    ;   DetOut = _{
            modo: desconocido,
            tipo: Tipo,
            sintomas: Syms
        }
    ),

    ( Enferms == [] ->
        format(atom(M), 'No se detectan enfermedades probables, aunque parece una dolencia de tipo ~w.', [Tipo])
    ; include(nonvar, Enferms, EnfermsOK),
      ( EnfermsOK == [] ->
          format(atom(M), 'Parece una dolencia de tipo ~w, pero no se lograron identificar enfermedades especificas.', [Tipo])
      ; atomic_list_concat(EnfermsOK, ', ', EnfermsTxt),
        format(atom(M), 'Probablemente se trate de una dolencia ~w como: ~w.', [Tipo, EnfermsTxt])
      )
    ),

    log(green, 'Diagnostico completado: ~w (~w)~n', [Tipo, Enferms]),

    reply_json_dict(_{
        tipo: Tipo,
        enfermedades: Enferms,
        detalle: DetOut,
        mensaje: M
    }).

/* ==================== Utilidades ==================== */
to_atom(X, A) :-
    ( atom(X) -> A = X
    ; atom_string(A, X)
    ).

/* ==================== Estático: app.js ==================== */
serve_app_js(_Request) :-
    working_directory(D, D),
    directory_file_path(D, 'app.js', RawPath),
    prolog_to_os_filename(RawPath, Path),
    (   exists_file(Path)
    ->  print_message(informational, serving(Path)),
        http_reply_file(Path,
            [ unsafe(true),
              mime_type('application/javascript')
            ],
            [])
    ;   print_message(error, file_not_found(Path)),
        throw(http_reply(not_found('app.js no encontrado o sin permisos')))
    ).