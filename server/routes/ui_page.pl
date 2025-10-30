% server/routes/ui_page.pl
:- module(ui_page, [ui_page/1]).
:- use_module(library(http/html_write)).

ui_page(_Request) :-
    reply_html_page(
        [title('Diagnóstico básico (Prolog)'), meta([charset='UTF-8'])],
        [\styles, \page_body]
    ).

styles -->
    html(style([
        'body{font-family:system-ui;background:#0f172a;color:#e5e7eb;margin:0}',
        'header{padding:20px;background:#111827;position:sticky;top:0}',
        '.wrap{max-width:960px;margin:auto;padding:24px}',
        '.card{background:#1f2937;border-radius:16px;padding:20px;margin-bottom:16px}',
        '.row{display:flex;gap:10px;flex-wrap:wrap;align-items:center}',
        'button{border:0;border-radius:10px;padding:10px 16px;cursor:pointer;background:#10b981;color:#0b1320}',
        'button:disabled{opacity:.6;cursor:not-allowed}',
        'input[type=text]{padding:10px;border-radius:8px;background:#0b1320;color:#e5e7eb;border:1px solid #334155}',
        '.symgrid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:10px;max-height:360px;overflow:auto}',
        '.pill{display:flex;align-items:center;gap:8px;background:#111827;border:1px solid #334155;border-radius:999px;padding:6px 10px}',
        '.tag{background:#0b1320;border:1px solid #334155;border-radius:999px;padding:4px 8px}',

        '.tag.bg-blue{background:#2563eb;color:#fff}',
        '.tag.bg-red{background:#dc2626;color:#fff}',
        '.tag.bg-yellow{background:#facc15;color:#111}',
        '.tag.bg-green{background:#22c55e;color:#111}',
        '.tag.bg-orange{background:#fb923c;color:#111}',
        '.tag.bg-purple{background:#a855f7;color:#fff}',
        '.tag.bg-gray{background:#6b7280;color:#fff}',

        'footer{color:#94a3b8;padding:10px;border-top:1px solid #334155}'
    ])).

page_body -->
    html([
        header(h1('Clasificador de Dolencia (Prolog)')),
        div(class(wrap), [
            div(class(card), [
                h2('Selecciona síntomas'),
                div(class(row), [
                    input([type(text), id(q), placeholder('Filtrar síntomas'), oninput('filterSyms()')]),
                    button([id(btnLoad), onclick('loadSyms()')], 'Cargar síntomas'),
                    span(class(muted), 'Ejemplo: fiebre, tos, dolor...')
                ]),
                div([id(symgrid), class(symgrid)], []),
                div(class(row), [
                    button([id(btnDiag), onclick('diagnose()'), disabled(true)], 'Diagnosticar'),
                    span(class(muted), 'Diagnóstico educativo.')
                ])
            ]),
            div([id(out), class('card res')], [
                h2('Resultado'),
                p(class(muted), 'Aún no hay resultado.')
            ])
        ]),
        footer('Hecho con SWI-Prolog + library(http).'),
        script([type('module'), src('/app.js')], [])
    ]).
