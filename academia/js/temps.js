/* Académia : le temps de jeu réel, jour par jour, pour le coin Parents (js/parents.js, vueTemps). Le temps compte
   quand la page est visible, qu'un Gardien joue (ni l'accueil, ni la création, ni le coin Parents) et que l'enfant a
   touché l'écran, cliqué ou appuyé sur une touche dans les deux dernières minutes : une tablette oubliée allumée ne
   compte pas. Il s'écrit dans P().temps (« 2026-10-12 » -> millisecondes) et part avec la partie à chacun de ses
   enregistrements ; entre deux, lui seul est écrit dans la sauvegarde, après une minute de jeu comptée et quand la page
   se cache (un onglet resté ouvert ne remet jamais une partie plus ancienne par-dessus) ; on garde quatre-vingt-dix
   jours. Les autres fichiers peuvent lire tempsDuJour() et tempsDeSeance() (une séance : depuis l'ouverture). */
const TEMPS_INACTIF_MS = 2 * 60000, TEMPS_PAS_MS = 5000, TEMPS_SAUVER_MS = 60000, TEMPS_JOURS = 90;
const ECRANS_SANS_TEMPS = ["accueil", "creation", "parents"];
const TEMPS = {geste: -Infinity, reprise: 0, avant: Date.now(), nonSauve: 0, seance: 0};

// the local day of a moment, as the engine writes it (js/adaptatif.js, jour)
const jourTemps = (t = Date.now()) => {
  if (typeof jour === "function") return jour(t);
  const d = new Date(t); return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
};

// a touch, a click or a key: the child is there (a touch after a long pause starts the count again from now)
function gesteTemps(){
  const t = Date.now();
  if (t - TEMPS.geste > TEMPS_INACTIF_MS) TEMPS.reprise = t;
  TEMPS.geste = t;
}
["pointerdown", "touchstart", "mousedown", "keydown"].forEach(k => addEventListener(k, gesteTemps, {capture: true, passive: true}));

// every few seconds: the time since the last count, the part of it the child was there, added to the day (`visible`:
// false when the page is hidden, except for the last count made as it hides)
function compterTemps(visible = !document.hidden){
  const t = Date.now(), debut = Math.max(TEMPS.avant, t - 2 * TEMPS_PAS_MS, TEMPS.reprise);   // a device asleep counts nothing
  TEMPS.avant = t;
  const p = P(), fin = Math.min(t, TEMPS.geste + TEMPS_INACTIF_MS);
  if (!p || !visible || ECRANS_SANS_TEMPS.includes(ecranCourant) || fin <= debut) return;
  if (!p.temps || typeof p.temps !== "object") p.temps = {};
  const j = jourTemps(t), ms = Math.round(fin - debut);
  p.temps[j] = (p.temps[j] || 0) + ms;
  TEMPS.seance += ms; TEMPS.nonSauve += ms;
  if (TEMPS.nonSauve >= TEMPS_SAUVER_MS) sauverTemps();
}
// the oldest days go; only the time is written, into the saved game as it is (js/etat.js): a tab left open on the same
// child never puts back an older game over the progress made in another one. The game's own saves carry it too
function sauverTemps(){
  const p = P(); if (!p || !TEMPS.nonSauve) return;
  TEMPS.nonSauve = 0;
  const limite = jourTemps(Date.now() - TEMPS_JOURS * 864e5);
  Object.keys(p.temps || {}).forEach(j => { if (j < limite) delete p.temps[j]; });
  const d = lireSauvegarde(CLE), q = d && d.profils[p.id];
  if (!q) return sauver();   // not saved yet, or the save unreadable: the whole game, as the game would
  const t = q.temps && typeof q.temps === "object" && !Array.isArray(q.temps) ? q.temps : {};
  Object.entries(t).forEach(([j, ms]) => { if (j >= limite && ms > (p.temps[j] || 0)) p.temps[j] = ms; });   // two tabs: the longer count
  q.temps = {...p.temps};
  try { const s = JSON.stringify(d); localStorage.setItem(CLE, s); localStorage.setItem(CLE_COPIE, s); } catch (e) {}
}
const tempsDuJour = (p = P()) => (p && p.temps && p.temps[jourTemps()]) || 0;
const tempsDeSeance = () => TEMPS.seance;

setInterval(() => compterTemps(), TEMPS_PAS_MS);
// hidden (another app, the screen off): what was played until now is counted and kept; back, nothing hidden counts
document.addEventListener("visibilitychange", () => {
  if (document.hidden) { compterTemps(true); sauverTemps(); }
  else TEMPS.avant = Date.now();
});
addEventListener("pagehide", () => { compterTemps(); sauverTemps(); });
