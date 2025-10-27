let ALL_SYMPTOMS = [];
const SELECTED = new Set(); // guarda casilla yaseleccionada
const $ = (id) => document.getElementById(id);

async function loadSyms() {
  const btn = $('btnLoad');
  if (btn) btn.disabled = true;
  try {
    const res = await fetch('/api/symptoms');
    if (!res.ok) throw new Error('HTTP ' + res.status);
    const data = await res.json();
    ALL_SYMPTOMS = data.symptoms || [];
    renderSyms(ALL_SYMPTOMS);
    const btnDiag = $('btnDiag');
    if (btnDiag) btnDiag.disabled = false;
  } catch (e) {
    out('<p class="muted">Error cargando sintomas.</p>');
    console.error(e);
  } finally {
    if (btn) btn.disabled = false;
  }
}

/**
 * renderSyms: pinta los checkboxes y restaura selecciones previas
 */
function renderSyms(list) {
  const q = ($('q').value || '').trim().toLowerCase();
  const tgt = $('symgrid');
  tgt.innerHTML = '';
  const filtered = list.filter(s => s.replaceAll('_', ' ').toLowerCase().includes(q));

  if (filtered.length === 0) {
    tgt.innerHTML = '<p class="muted" style="grid-column:1/-1;margin:6px 0;">Sin resultados</p>';
    return;
  }

  for (const s of filtered) {
    const id = 'sym-' + s;
    const wrap = document.createElement('div');
    wrap.className = 'pill';

    //  crear checkbox con restauración del estado anterior
    const input = document.createElement('input');
    input.type = 'checkbox';
    input.id = id;
    input.value = s;
    if (SELECTED.has(s)) input.checked = true; 

    input.onchange = () => {
      if (input.checked) SELECTED.add(s);
      else SELECTED.delete(s);
      toggleDiagnoseButton();
    };

    const label = document.createElement('label');
    label.htmlFor = id;
    label.textContent = s.replaceAll('_', ' ');

    wrap.append(input, label);
    tgt.appendChild(wrap);
  }
}

/**
 * Filtra sin perder las selecciones previas
 */
function filterSyms() {
  renderSyms(ALL_SYMPTOMS);
}

/**
 * Activa/desactiva el botón Diagnosticar
 */
function toggleDiagnoseButton() {
  const btn = $('btnDiag');
  if (btn) btn.disabled = SELECTED.size === 0;
}

/**
 * Diagnóstico basado en selección actual
 */
async function diagnose() {
  const checked = Array.from(SELECTED);
  if (checked.length === 0) {
    out('<p class="muted">Selecciona al menos un sintoma.</p>');
    return;
  }
  try {
    const res = await fetch('/api/diagnose', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ symptoms: checked })
    });
    if (!res.ok) throw new Error('HTTP ' + res.status);
    const data = await res.json();

    const tipo = data.tipo || '(desconocido)';
    const enferms = (data.enfermedades || [])
      .map(e => `<span class="tag">${e}</span>`).join(' ');

    const det = data.detalle || {};
    let detailHtml = '';
    if (det.modo === 'atajo') {
      const lista = Array.isArray(det.sintomas) ? det.sintomas.join(', ') : '';
      detailHtml = `<p><b>Regla de atajo activada</b>. Sintomas recibidos: <code>${lista}</code></p>`;
    } else {
      const top = (det.top_enfermedades || []);
      const items = top.map(parsePair)
                       .map(({ score, enf }) => `<li><code>${enf}</code> → ${score} coincidencias</li>`)
                       .join('');
      detailHtml = `
        <p><b>Por puntaje</b>. Score de categoría: <code>${det.score_categoria ?? 0}</code></p>
        <p>Top enfermedades:</p>
        <ul>${items || '<li>(sin coincidencias)</li>'}</ul>`;
    }

    out(`
      <h3>Tipo probable: <span class="tag">${tipo}</span></h3>
      <p>Enfermedades candidatas:</p>
      <p>${enferms || '<span class="muted">(ninguna con ≥2 sintomas)</span>'}</p>
      ${detailHtml}
    `);
  } catch (e) {
    out('<p class="muted">Error al diagnosticar.</p>');
    console.error(e);
  }
}

/**
 * Mostrar resultados
 */
function out(html) {
  $('out').innerHTML = '<h2>Resultado</h2>' + html;
}

/**
 * parsePair: robusto para top_enfermedades
 */
function parsePair(p) {
  if (Array.isArray(p) && p.length >= 2) {
    return { score: Number(p[0]) || 0, enf: String(p[1]) };
  }
  if (p && typeof p === 'object' && '-' in p && Array.isArray(p['-'])) {
    const a = p['-'];
    return { score: Number(a[0]) || 0, enf: String(a[1]) };
  }
  if (typeof p === 'string') {
    const m = p.match(/^(\d+)\s*-\s*(.+)$/);
    if (m) return { score: Number(m[1]) || 0, enf: m[2] };
    return { score: 0, enf: p };
  }
  return { score: 0, enf: String(p) };
}

window.addEventListener('DOMContentLoaded', loadSyms);
window.filterSyms = filterSyms;
window.loadSyms = loadSyms;
window.diagnose = diagnose;
