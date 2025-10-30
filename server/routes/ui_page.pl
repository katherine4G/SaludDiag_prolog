:- module(ui_page, []).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(.), ui_page, []).
:- http_handler(root('app.js'), serve_app_js, []).

ui_page(_Request) :-
    reply_html_page(
      [title('Diagnóstico básico (Prolog)')],
      [\page_body]
    ).

page_body -->
    html([
        h1('Demo: Clasificador de dolencia (Prolog)'),
        script([type('module'), src('/app.js')], [])
    ]).

serve_app_js(_Request) :-
    http_reply_file('static/app.js',
        [unsafe(true), mime_type('application/javascript')],
        []).
