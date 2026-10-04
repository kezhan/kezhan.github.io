/* Académia : le moteur adaptatif (GDD §5 et §8.2).
   1. Par matière, la notion courante est la première non maîtrisée dont les prérequis le sont.
   2. Attaque rapide : une révision (notion déjà maîtrisée) ; frappe massive : la notion courante.
   3. Pas deux fois la même question de suite ; chaque erreur est gardée, et le prochain boss la repose (revanche). */
function suiviNotion(id){ const p = P(); return p.maitrise[id] = p.maitrise[id] || {ok: 0, ko: 0, hist: ""}; }
function maitrisee(n){
  const e = P().maitrise[n.id]; if (!e) return false;
  if (e.acquis) return true;
  const h = e.hist.slice(-8), justes = [...h].filter(c => c === "1").length;
  return e.ok >= 4 && h.length >= 4 && justes / h.length >= .75;
}
// a new child: what is clearly below their age counts as known (they can still fail it in revision)
function placer(p){
  Q.notions.forEach(n => { if (ageMin(n) <= p.age - 2 && !p.maitrise[n.id]) p.maitrise[n.id] = {ok: 0, ko: 0, hist: "", acquis: true}; });
}
function notionCourante(matiere){
  const ns = notionsDe(matiere), age = P().age;
  const sues = new Set(ns.filter(maitrisee).map(n => n.id));
  const pour = ns.filter(n => ageMin(n) <= age + 1);
  const pret = n => (n.prerequis || []).every(x => sues.has(x) || !notionDe(x));
  return pour.find(n => !sues.has(n.id) && pret(n)) || pour[pour.length - 1] || ns[0] || null;
}
const recents = {};
const cleQ = q => q.id || `${q.question}|${q.reponse}`;
function dejaVue(matiere, q){ return (recents[matiere] || []).includes(cleQ(q)); }
function retenir(matiere, q){ const r = recents[matiere] = recents[matiere] || []; r.push(cleQ(q)); if (r.length > 8) r.shift(); }

function tirer(matiere, mecanique, boss){
  if (!P().erreurs) P().erreurs = [];
  // the boss asks again what was missed (revanche, GDD §5)
  if (boss && mecanique !== "bouclier") {
    const i = P().erreurs.findIndex(e => e.matiere === matiere);
    if (i >= 0) { const q = P().erreurs.splice(i, 1)[0]; sauver(); return {...q, revanche: true}; }
  }
  if (mecanique === "bouclier") {
    const age = P().age, l = (Q.vraifaux[matiere] || []).filter(q => ageMin(notionDe(q.notion) || q) <= age + 1);
    for (let k = 0; k < 6 && l.length; k++) { const q = pick(l, 1)[0]; if (!dejaVue(matiere, q)) { retenir(matiere, q); return q; } }
    const base = tirer(matiere, "rapide"); const vf = vraiFauxDepuis(base);
    if (vf) return vf;
  }
  const revision = notionsDe(matiere).filter(n => maitrisee(n) && ageMin(n) <= P().age + 1);
  const notion = mecanique === "rapide" && revision.length ? pick(revision.slice(-4), 1)[0] : notionCourante(matiere);
  let q = null;
  for (let k = 0; k < 10; k++) { q = questionPour(notion, mecanique); if (q && !dejaVue(matiere, q)) break; }
  if (q) retenir(matiere, q);
  return q;
}
function noter(q, juste){
  if (!q || !q.notion) return;
  const e = suiviNotion(q.notion);
  if (juste) e.ok++; else e.ko++;
  e.hist = (e.hist + (juste ? "1" : "0")).slice(-12);
  // a notion taken for known but missed twice lately goes back to practice
  if (!juste && e.acquis && [...e.hist.slice(-4)].filter(c => c === "0").length >= 2) e.acquis = false;
  if (!juste && !q.revanche && !q.genere) {
    const p = P(); p.erreurs.push({...q, revanche: undefined});
    if (p.erreurs.length > 40) p.erreurs.shift();
  }
  sauver();
}
// share of the notions of a subject the child masters (the hidden school report of the notebook, GDD §9)
function progresMatiere(matiere){
  const ns = notionsDe(matiere).filter(n => ageMin(n) <= P().age + 2);
  return ns.length ? ns.filter(maitrisee).length / ns.length : 0;
}
