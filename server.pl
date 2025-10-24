
:- module(server, [server/1, stop/0]).
:- set_prolog_flag(encoding, utf8).

/* --- Librerías HTTP --- */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).
:- use_module(library(lists), [member/2]).  % evita choque con sum_list/2

/* --- Base de conocimiento en módulo --- */
:- use_module(kb_salud, [
       sintoma_de/2,
       es_dolencia_infecciosa/1,
       es_dolencia_traumatica/1,
       es_dolencia_cardiovascular/1
   ]).

/* --- Rutas --- */
:- http_handler(root(.),               ui_page,         []).
:- http_handler(root(api/symptoms),    api_symptoms,    []).
:- http_handler(root(api/diagnose),    api_diagnose,    [method(post)]).
:- http_handler(root('app.js'),        serve_app_js,    []).

/* --- Arranque / Parada --- */
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop :-
    http_stop_server(_, []).

/* ======================== UI ======================== */

ui_page(_Request) :-
    reply_html_page(
      [ title('Diagnóstico básico (Prolog)')
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
        h2('Selecciona síntomas'),
        div(class(row), [
          input([type(text), id(q), placeholder('Filtrar síntomas…'), oninput('filterSyms()')]),
          button([ id(btnLoad), onclick('loadSyms()') ], 'Cargar síntomas'),
          span(class(muted), 'Sugerencia: escribe "fiebre", "tos", "dolor", etc.')
        ]),
        div([id(symgrid), class(symgrid)], []),
        div(class(row), [
          button([ id(btnDiag), onclick('diagnose()'), disabled(true) ], 'Diagnosticar'),
          span(class(muted), 'El diagnóstico es educativo, no médico.')
        ])
      ]),
      div([id(out), class('card res')], [
        h2('Resultado'),
        p(class(muted), 'Aún no hay resultado.')
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
    http_read_json_dict(Request, DictIn),
    (   _{symptoms: Syms0} :< DictIn
    ->  true
    ;   throw(http_reply(bad_request('Missing "symptoms"')))
    ),
    maplist(to_atom, Syms0, Syms),
    tipo_probable_list(Syms, Tipo),
    enfermedades_posibles_list(Syms, Enferms),
    explicacion_categoria_list(Syms, Tipo, Detalle),
    reply_json_dict(_{ tipo:Tipo, enfermedades:Enferms, detalle:Detalle }).

to_atom(X, A) :- (atom(X) -> A=X ; atom_string(A, X)).

/* --------- Lógica basada en LISTA de síntomas --------- */

atajo_categoria_list(Syms, traumatica) :-
    member(antecedente_golpe, Syms),
    member(dolor_localizado, Syms).

atajo_categoria_list(Syms, cardiovascular) :-
    member(dolor_toracico_opresivo, Syms),
    member(disnea, Syms).

atajo_categoria_list(Syms, cardiovascular) :-
    (   member(debilidad_hemicuerpo, Syms)
    ;   member(dificultad_hablar, Syms)
    ;   member(desviacion_boca, Syms)
    ),
    member(inicio_brusco, Syms).

atajo_categoria_list(Syms, infecciosa) :-
    member(fiebre, Syms),
    (   member(tos, Syms)
    ;   member(mialgia, Syms)
    ;   member(rinorrea, Syms)
    ;   member(diarrea, Syms)
    ).

score_enfermedad_list(Syms, Enf, Score) :-
    findall(S, (member(S, Syms), sintoma_de(Enf, S)), Ss),
    length(Ss, Score).

enfermedad_posible_list(Syms, Enf) :-
    score_enfermedad_list(Syms, Enf, Score),
    Score >= 2.

enfermedades_posibles_list(Syms, Unicas) :-
    findall(Enf, enfermedad_posible_list(Syms, Enf), L),
    sort(L, Unicas).

categoria_score_list(Syms, infecciosa, Score) :-
    findall(S, (es_dolencia_infecciosa(E), score_enfermedad_list(Syms, E, S)), L),
    lists:sum_list(L, Score).
categoria_score_list(Syms, traumatica, Score) :-
    findall(S, (es_dolencia_traumatica(E), score_enfermedad_list(Syms, E, S)), L),
    lists:sum_list(L, Score).
categoria_score_list(Syms, cardiovascular, Score) :-
    findall(S, (es_dolencia_cardiovascular(E), score_enfermedad_list(Syms, E, S)), L),
    lists:sum_list(L, Score).

mejor_categoria_por_score_list(Syms, CatMax) :-
    findall(Score-Cat,
            ( member(Cat, [infecciosa, traumatica, cardiovascular]),
              categoria_score_list(Syms, Cat, Score)
            ), Pares),
    ( Pares == [] -> CatMax = infecciosa
    ; sort(Pares, Ordenados),
      last(Ordenados, _-CatMax)
    ).

tipo_probable_list(Syms, Tipo) :-
    (   atajo_categoria_list(Syms, Tipo) -> true
    ;   mejor_categoria_por_score_list(Syms, Tipo)
    ).

explicacion_categoria_list(Syms, Tipo, Detalle) :-
    (   atajo_categoria_list(Syms, Tipo)
    ->  Detalle = _{modo:atajo, tipo:Tipo, sintomas:Syms}
    ;   categoria_score_list(Syms, Tipo, ScoreCat),
        findall(Score-Enf,
                ( member(Enf, [gripe, covid19, dengue, malaria,
                               fractura, esguince, contusion, hematoma, cortadura,
                               infarto, hipertension, ictus, angina_de_pecho, arritmia]),
                  score_enfermedad_list(Syms, Enf, Score),
                  Score > 0),
                Pares),
        sort(Pares, Orden), reverse(Orden, Desc),
        take(3, Desc, Top),
        Detalle = _{modo:score, tipo:Tipo, score_categoria:ScoreCat, top_enfermedades:Top}
    ).

take(N, L, R) :- length(R, N), append(R, _, L), !.
take(_, L, L).

/* ==================== Estático: app.js ==================== */
serve_app_js(_Request) :-
    http_reply_file('app.js', [mime_type(text/javascript)], _).
