let ALL_SYMPTOMS = [];
const SELECTED = new Set();
const $ = (id) => document.getElementById(id);

// Cargar síntomas desde /api/symptoms
async function loadSyms() {
  const btn = $("btnLoad");
  if (btn) btn.disabled = true;
  try {
    const res = await fetch("/api/symptoms");
    if (!res.ok) throw new Error("HTTP " + res.status);
    const data = await res.json();
    ALL_SYMPTOMS = data.symptoms || [];
    renderSyms(ALL_SYMPTOMS);
    const btnDiag = $("btnDiag");
    if (btnDiag) btnDiag.disabled = false;
  } catch (e) {
    out('<p class="muted">Error cargando síntomas.</p>');
    console.error(e);
  } finally {
    if (btn) btn.disabled = false;
  }
}

// Renderizar los checkboxes de síntomas
function renderSyms(list) {
  const q = ($("q").value || "").trim().toLowerCase();
  const tgt = $("symgrid");
  tgt.innerHTML = "";

  const filtered = list.filter((s) =>
    s.replaceAll("_", " ").toLowerCase().includes(q)
  );

  if (filtered.length === 0) {
    tgt.innerHTML =
      '<p class="muted" style="grid-column:1/-1;margin:6px 0;">Sin resultados</p>';
    return;
  }

  for (const s of filtered) {
    const id = "sym-" + s;
    const wrap = document.createElement("div");
    wrap.className = "pill";

    const input = document.createElement("input");
    input.type = "checkbox";
    input.id = id;
    input.value = s;
    if (SELECTED.has(s)) input.checked = true;

    input.onchange = () => {
      if (input.checked) SELECTED.add(s);
      else SELECTED.delete(s);
      toggleDiagnoseButton();
    };

    const label = document.createElement("label");
    label.htmlFor = id;
    label.textContent = s.replaceAll("_", " ");

    wrap.append(input, label);
    tgt.appendChild(wrap);
  }
}

// Mantener las selecciones al filtrar
function filterSyms() {
  renderSyms(ALL_SYMPTOMS);
}

// Activar o desactivar el botón de diagnóstico
function toggleDiagnoseButton() {
  const btn = $("btnDiag");
  if (btn) btn.disabled = SELECTED.size === 0;
}

// Diagnóstico: enviar síntomas seleccionados al backend
async function diagnose() {
  const checked = Array.from(SELECTED);
  if (checked.length === 0) {
    out('<p class="muted">Selecciona al menos un síntoma.</p>');
    return;
  }

  try {
    const res = await fetch("/api/diagnose", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ symptoms: checked }),
    });
    if (!res.ok) throw new Error("HTTP " + res.status);
    const data = await res.json();

    const tipo = data.tipo || "desconocido";
    const enferms = (data.enfermedades || [])
      .map((e) => `<span class="tag">${e}</span>`)
      .join(" ");

    const det = data.detalle || {};
    let detailHtml = "";

    if (det.modo === "atajo") {
      const lista = Array.isArray(det.sintomas) ? det.sintomas.join(", ") : "";
      detailHtml = `
        <p><b>Regla de atajo activada</b> — síntomas: <code>${lista}</code></p>`;
    } else {
      const top = det.top_enfermedades || [];
      const items = top
        .map(parsePair)
        .map(
          ({ enf, score }) =>
            `<li><b>${enf}</b>: <code>${score}</code> coincidencia${
              score !== 1 ? "s" : ""
            }</li>`
        )
        .join("");

      detailHtml = `
        <p><b>Por puntaje</b>. Score de categoría: <code>${
          det.score_categoria ?? 0
        }</code></p>
        <p>Enfermedades relacionadas:</p>
        <ul>${items || "<li>(sin coincidencias)</li>"}</ul>`;
    }
    out(`
      <h3>Tipo probable: <span class="tag ${colorPorCategoria(tipo)}">${tipo}</span></h3>
      <p class="muted">${descripcionPorCategoria(tipo)}</p>
      <p><b>Enfermedades candidatas:</b></p>
      <p>${
        enferms || '<span class="muted">(ninguna con ≥2 síntomas)</span>'
      }</p>
      ${detailHtml}
    `);
  } catch (e) {
    out('<p class="muted">Error al diagnosticar.</p>');
    console.error(e);
  }
}

// Mostrar resultados en el panel de salida
function out(html) {
  $("out").innerHTML = "<h2>Resultado</h2>" + html;
}

// parsePair: compatible con JSON moderno {enfermedad, score}
function parsePair(p) {
  if (p && typeof p === "object" && "enfermedad" in p && "score" in p) {
    return { enf: p.enfermedad, score: p.score };
  }
  if (Array.isArray(p) && p.length >= 2) {
    return { enf: String(p[1]), score: Number(p[0]) || 0 };
  }
  if (typeof p === "string") {
    const m = p.match(/^(\d+)\s*-\s*(.+)$/);
    if (m) return { enf: m[2], score: Number(m[1]) || 0 };
    return { enf: p, score: 0 };
  }
  return { enf: "(?)", score: 0 };
}

// Colores por tipo de categoría
function colorPorCategoria(tipo) {
  switch (tipo.toLowerCase()) {
    case "respiratoria":
      return "bg-blue";
    case "cardiovascular":
      return "bg-red";
    case "digestiva":
      return "bg-yellow";
    case "neurologica":
      return "bg-purple";
    case "infecciosa":
      return "bg-green";
    case "traumatica":
      return "bg-orange";
    default:
      return "bg-gray";
  }
}

function descripcionPorCategoria(tipo) {
  switch (tipo.toLowerCase()) {
    case "respiratoria":
      return "Las enfermedades respiratorias afectan los pulmones y las vías respiratorias, causando tos, disnea o dolor torácico.";
    case "cardiovascular":
      return "Las enfermedades cardiovasculares involucran el corazón y los vasos sanguíneos, y pueden manifestarse con dolor torácico o fatiga.";
    case "digestiva":
      return "Las enfermedades digestivas afectan el sistema gastrointestinal, con síntomas como dolor abdominal o náuseas.";
    case "neurologica":
      return "Las enfermedades neurológicas involucran el cerebro o los nervios, produciendo síntomas como convulsiones, mareo o temblor.";
    case "traumatica":
      return "Las enfermedades o lesiones traumáticas derivan de golpes o esfuerzos físicos, generando dolor localizado o hematomas.";
    case "infecciosa":
      return "Las enfermedades infecciosas son causadas por agentes patógenos como virus o bacterias y suelen acompañarse de fiebre o malestar general.";
    default:
      return "Categoría no determinada. Los síntomas no son suficientes para una clasificación clara.";
  }
}

// Inicializar
window.addEventListener("DOMContentLoaded", loadSyms);
window.filterSyms = filterSyms;
window.loadSyms = loadSyms;
window.diagnose = diagnose;
