/* Académia : petits outils communs (DOM, hasard, sons, écrans, messages). */
const TEST = /(^#|&)test\b/.test(location.hash);   // automated recette: no waiting, no animation, right answers marked
if (TEST) document.documentElement.classList.add("test");   // CSS animations off: the robot taps steady targets
const $ = id => document.getElementById(id);
const rnd = n => Math.floor(Math.random() * n);
const shuffle = a => { a = a.slice(); for (let i = a.length - 1; i > 0; i--) { const j = rnd(i + 1); [a[i], a[j]] = [a[j], a[i]]; } return a; };
const pick = (a, n) => shuffle(a).slice(0, n);
const uid = () => Date.now().toString(36) + Math.random().toString(36).slice(2, 7);
const wait = ms => new Promise(r => setTimeout(r, TEST ? 0 : ms));
function el(tag, cls, html){ const n = document.createElement(tag); if (cls) n.className = cls; if (html != null) n.innerHTML = html; return n; }
function markOk(n){ if (TEST && n) n.dataset.ok = "1"; return n; }
const calme = () => TEST || matchMedia("(prefers-reduced-motion: reduce)").matches;

function toast(msg, ms = 2600){
  const t = $("toast"); t.textContent = msg; t.hidden = false;
  clearTimeout(toast.h); toast.h = setTimeout(() => { t.hidden = true; }, ms);
}

/* screens: one visible at a time; the top bar shows once a Gardien is chosen */
const ECRANS = ["accueil", "creation", "choix", "monde", "combat", "fin", "equipe"];
let ecranCourant = "accueil", combatEnCours = 0;   // leaving the fight for another screen stops it (no ghost fight behind)
function montrer(id){
  if (id !== "combat" && id !== "fin") combatEnCours++;
  ECRANS.forEach(e => { if ($(e)) $(e).hidden = e !== id; });
  $("barre").hidden = id !== "monde";
  ecranCourant = id; window.scrollTo(0, 0);
  document.dispatchEvent(new CustomEvent("ecran", {detail: id}));
}

/* sounds: Web Audio, no file; Android keeps a context born outside a tap silent until resumed */
let audioCtx = null;
function ton(freqs, dur = .12, type = "triangle", vol = .2){
  if (TEST) return;
  try {
    audioCtx = audioCtx || new (window.AudioContext || window.webkitAudioContext)();
    if (audioCtx.state === "suspended") audioCtx.resume();
    const t = audioCtx.currentTime;
    freqs.forEach((f, i) => {
      const o = audioCtx.createOscillator(), g = audioCtx.createGain();
      o.type = type; o.frequency.value = f;
      g.gain.setValueAtTime(.0001, t + i * dur); g.gain.exponentialRampToValueAtTime(vol, t + i * dur + .02);
      g.gain.exponentialRampToValueAtTime(.0001, t + (i + 1) * dur);
      o.connect(g); g.connect(audioCtx.destination); o.start(t + i * dur); o.stop(t + (i + 1) * dur + .05);
    });
  } catch (e) {}
}
const sfx = {
  tap: () => ton([520], .05), juste: () => ton([660, 880, 1320], .1), faux: () => ton([300, 220], .16, "sine"),
  coup: () => ton([180, 120], .08, "square", .15), critique: () => ton([400, 800, 1600], .07, "sawtooth", .12),
  victoire: () => ton([523, 659, 784, 1047, 1319], .12), niveau: () => ton([659, 784, 988, 1319], .14),
  evolution: () => ton([392, 523, 659, 784, 1047, 1319, 1568], .16), bouclier: () => ton([880, 1175], .08, "sine")
};
function centre(n){ const r = n.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height / 2]; }
