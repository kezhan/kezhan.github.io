/* Académia : le moteur adaptatif (GDD §5 et §8.2).
   1. Par matière, la notion du moment est la première pas encore sue dont les prérequis le sont : elle s'ouvre dès
      qu'ils sont sus. L'âge du jour (l'âge donné à la création, plus les années passées) ne sert qu'au placement de
      départ et au confort (voix lente, pas de minuteur, ailleurs dans le jeu).
   2. Placement : sont supposées sues les notions d'âge ≤ âge − 1 en maths, logique et sciences, aucune en langues.
   3. Une notion se valide en jouant : deux bonnes réponses sans aucune erreur (saut), sinon quatre bonnes et les trois
      quarts des huit dernières ; une grande notion (tables, opérations jusqu'à 100), 18 bonnes sur les 20 dernières, sur
      deux jours (8 sur 10 laissait passer un enfant qui ne sait que la moitié des tables, 9 fois sur 10 en 60 réponses,
      tests/moteur.js). Une notion sue retombe après trois erreurs sur ses cinq dernières réponses, et se revalide en
      deux bonnes. Une notion validée revient en révision espacée : 1, 3, 7 jours, puis 14 et 30.
   4. Tirage. Attaque rapide : la notion du moment une fois sur deux, sinon une révision (celle dont le jour est venu,
      sinon une des plus hautes, jamais sous âge − 2). Frappe massive : la notion du moment, en plus dur. Jusqu'à 5 ans,
      deux questions sur trois dans la notion du moment, quel que soit le bouton. Matière toute sue : la massive tire
      parmi les plus hautes, en forme plus dure. Bouclier : un Vrai/Faux de la notion du moment une fois sur deux, sinon
      d'une révision. Boss : d'abord les revanches (les erreurs gardées), puis les défis du chef (q.defi).
   5. Pas deux fois la même question de suite ; un Vrai/Faux fabriqué par le moteur (q.genere) ne compte pas. */
const LANGUES_ETRANGERES = ["anglais", "allemand", "luxembourgeois"];
const PLACEMENT = 2;                          // version of the placement rules, kept in each game
const ECARTS_REVISION = [1, 3, 7, 14, 30];    // days between two revisions of a notion learnt in play

const jour = (t = Date.now()) => { const d = new Date(t); return `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`; };
const plusJours = (j, n) => { const [a, m, d] = j.split("-").map(Number); return jour(new Date(a, m - 1, d + n, 12).getTime()); };
// the age of the day: the age given at creation, plus the years gone by since
function ageDuJour(p){
  if (!p) return 4;
  if (p.ageCree == null) p.ageCree = p.age;
  const ans = Math.floor((Date.now() - Date.parse(p.cree || 0)) / (365.25 * 864e5));
  return p.ageCree + (ans > 0 ? ans : 0);
}

/* ---- what the child knows */
function suiviNotion(id){ const p = P(); return p.maitrise[id] = p.maitrise[id] || {ok: 0, ko: 0, hist: ""}; }
const estSue = e => !!e && (!!e.acquis || !!e.valide);
function maitrisee(n){ const p = P(); return !!p && !!n && estSue(p.maitrise[n.id]); }
// learnt in play, not only supposed known by the placement (the stars of the bag)
function gagneeEnJeu(n){ const p = P(), e = p && n && p.maitrise[n.id]; return !!e && !!e.valide; }
const justes = s => [...s].filter(c => c === "1").length, fautes = s => [...s].filter(c => c === "0").length;
function validee(e, n){
  const h = e.hist;
  if (e.chute) return h.endsWith("11");                               // fell: back in two right answers
  if (n && n.grande) { const d = h.slice(-20); return d.length === 20 && justes(d) >= 18 && (e.jours || []).length >= 2; }
  if (!e.ko) return e.ok >= 2;                                        // a jump: two right answers, never a mistake
  const d = h.slice(-8); return e.ok >= 4 && d.length >= 4 && justes(d) / d.length >= .75;
}
function valider(e, auj){ e.valide = auj; delete e.chute; e.rev = {pas: 0, le: plusJours(auj, ECARTS_REVISION[0])}; }
function reviser(e, juste, auj){   // spaced revision: a right answer on its day pushes the next one further
  const r = e.rev = e.rev || {pas: 0, le: auj};
  if (!juste) { r.pas = 0; r.le = plusJours(auj, ECARTS_REVISION[0]); }
  else if (r.le <= auj) { r.pas = Math.min(r.pas + 1, ECARTS_REVISION.length - 1); r.le = plusJours(auj, ECARTS_REVISION[r.pas]); }
}
function noter(q, juste){
  const p = P();
  if (!p || !q || !q.notion || q.genere) return;   // a makeshift True/False built by the engine does not count
  const e = suiviNotion(q.notion), n = notionDe(q.notion), auj = jour();
  if (juste) { e.ok++; e.jours = (e.jours || []).filter(j => j !== auj).concat(auj).slice(-4); } else e.ko++;
  e.hist = (e.hist + (juste ? "1" : "0")).slice(-24);
  if (estSue(e) && !juste && fautes(e.hist.slice(-5)) >= 3) {
    delete e.acquis; delete e.valide; delete e.rev; e.chute = true;   // three mistakes among the last five: it falls
  } else if (e.valide) reviser(e, juste, auj);
  else if (juste && validee(e, n)) valider(e, auj);   // learnt, or a placed notion confirmed in play
  if (!juste && !q.revanche) {
    p.erreurs = p.erreurs || [];
    p.erreurs.push({...q, revanche: undefined, defi: undefined});
    if (p.erreurs.length > 40) p.erreurs.shift();
  }
  sauver();
}

/* ---- placement */
function placer(p){
  if (!p || !Q.notions.length) return false;
  const age = p.age = ageDuJour(p);
  p.maitrise = p.maitrise || {};
  Q.notions.forEach(n => {   // arriving in Luxembourg at 7 does not make one speak Luxembourgish: no language is given
    if (LANGUES_ETRANGERES.includes(n.matiere) || ageMin(n) > age - 1 || p.maitrise[n.id]) return;
    p.maitrise[n.id] = {ok: 0, ko: 0, hist: "", acquis: true};
  });
  p.place = PLACEMENT;
  return true;
}
// once the map of notions is there: the age of the day for every child; the games not placed yet (a network too slow
// at creation) or placed by older rules (languages taken for known without a question) are placed now
function placerLesParties(){
  let change = false;
  Object.values(E.profils || {}).forEach(p => {
    const age = ageDuJour(p);
    if (p.age !== age) { p.age = age; change = true; }
    if (p.place === PLACEMENT) return;
    Object.keys(p.maitrise = p.maitrise || {}).forEach(id => {
      const n = notionDe(id), e = p.maitrise[id];
      if (n && LANGUES_ETRANGERES.includes(n.matiere) && e.acquis && !e.ok && !e.ko) delete p.maitrise[id];
    });
    placer(p); change = true;
  });
  if (change) sauver();
}

/* ---- the notion of the moment and the revisions */
function notionCourante(matiere){
  const ns = notionsDe(matiere), sues = new Set(ns.filter(maitrisee).map(n => n.id));
  const ouverte = n => (n.prerequis || []).every(x => sues.has(x) || !notionDe(x));
  return ns.find(n => !sues.has(n.id) && ouverte(n)) || ns[ns.length - 1] || null;   // all known: the highest
}
const toutSu = matiere => { const ns = notionsDe(matiere); return ns.length > 0 && ns.every(maitrisee); };
// the highest notions of a subject: the last two in order, and every notion of the highest age
function plusHautes(matiere){
  const ns = notionsDe(matiere), haut = Math.max(...ns.map(ageMin));
  return ns.filter((n, i) => i >= ns.length - 2 || ageMin(n) === haut);
}
// a notion to revise: the one whose day has come first, else one of the highest known, never below age − 2
function notionARevoir(matiere, sauf){
  const p = P(), age = ageDuJour(p), auj = jour(), le = n => p.maitrise[n.id].rev ? p.maitrise[n.id].rev.le : "";
  const sues = notionsDe(matiere).filter(n => maitrisee(n) && n !== sauf);
  if (!sues.length) return null;
  const dues = sues.filter(n => le(n) && le(n) <= auj).sort((a, b) => le(a).localeCompare(le(b)));
  if (dues.length) return dues[0];
  const hautes = sues.filter(n => ageMin(n) >= age - 2);
  return pick((hautes.length ? hautes : sues).slice(-4), 1)[0];
}
const rythme = {};   // per subject: questions drawn, and whether the last quick one was a revision
function notionPour(matiere, mecanique, courante){
  const r = rythme[matiere] = rythme[matiere] || {n: 0, revision: true, bouclier: 0};
  let revoir;
  if (ageDuJour(P()) <= 5) revoir = r.n++ % 3 === 2;   // the game chooses: two out of three on the notion of the moment
  else if (mecanique === "massive") revoir = false;
  else revoir = !r.revision;                              // the quick attack: every other time
  const n = revoir ? notionARevoir(matiere, courante) : null;
  if (mecanique !== "massive") r.revision = !!n;
  return n || courante;
}

/* ---- drawing a question */
const recents = {};
const cleQ = q => q.id || `${q.question}|${q.reponse}`;
function dejaVue(matiere, q){ return (recents[matiere] || []).includes(cleQ(q)); }
function retenir(matiere, q){ const r = recents[matiere] = recents[matiere] || []; r.push(cleQ(q)); if (r.length > 8) r.shift(); }
function unique(matiere, fabriquer, essais = 10){   // not the same question twice in a row
  let q = null;
  for (let k = 0; k < essais; k++) { q = fabriquer(); if (q && !dejaVue(matiere, q)) break; }
  if (q) retenir(matiere, q);
  return q;
}

function tirer(matiere, mecanique, boss){
  const p = P(); if (!p) return null;
  if (!p.erreurs) p.erreurs = [];
  if (boss && mecanique !== "bouclier") {   // the boss asks again what was missed (revanche, GDD §5)
    const i = p.erreurs.findIndex(e => e.matiere === matiere);
    if (i >= 0) { const q = p.erreurs.splice(i, 1)[0]; sauver(); return {...q, revanche: true}; }
  }
  if (!notionsDe(matiere).length) return null;
  const tout = toutSu(matiere), courante = tout ? null : notionCourante(matiere);
  if (mecanique === "bouclier") return bouclier(matiere, courante);
  if (boss && mecanique === "massive") {   // then the chief's challenges, at the top of the child's level
    const q = unique(matiere, () => questionPour(courante || pick(plusHautes(matiere), 1)[0], "boss"));
    if (q) return {...q, defi: true};
  }
  if (tout && mecanique === "massive") {   // the whole subject is known: the highest notions, harder
    const notion = pick(plusHautes(matiere), 1)[0];
    return unique(matiere, () => formeDure(questionPour(notion, "boss"), ageDuJour(p)));
  }
  const notion = tout ? notionARevoir(matiere) : notionPour(matiere, mecanique, courante);
  return unique(matiere, () => questionPour(notion, mecanique));
}

// the shield: a True/False of the notion of the moment every other time, else of a revision
function bouclier(matiere, courante){
  const r = rythme[matiere] = rythme[matiere] || {n: 0, revision: true, bouclier: 0};
  const notion = (r.bouclier++ % 2 === 0 && courante) || notionARevoir(matiere) || courante || notionCourante(matiere);
  return unique(matiere, () => vraiFauxPour(notion), 6)
    || unique(matiere, () => pick(vraiFauxOuverts(matiere), 1)[0], 6)
    || vraiFauxDepuis(questionPour(notion, "rapide"))
    || unique(matiere, () => questionPour(notion, "rapide"));   // nothing to judge: an ordinary question
}
function vraiFauxPour(notion){
  if (!notion) return null;
  if (notion.generateur) { const q = questionPour(notion, "bouclier"); return estVraiFaux(q) ? q : null; }
  return pick(Q.vfParNotion[notion.id] || [], 1)[0] || null;
}
function vraiFauxOuverts(matiere){   // the written True/False of notions known or of the moment
  const c = notionCourante(matiere), ok = new Set(notionsDe(matiere).filter(n => n === c || maitrisee(n)).map(n => n.id));
  return (Q.vraifaux[matiere] || []).filter(q => ok.has(q.notion));
}

// share of the notions of a subject the child masters (the hidden school report of the notebook, GDD §9)
function progresMatiere(matiere){
  const ns = notionsDe(matiere).filter(n => ageMin(n) <= ageDuJour(P()) + 2);
  return ns.length ? ns.filter(maitrisee).length / ns.length : 0;
}
