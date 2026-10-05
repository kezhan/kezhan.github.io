/* Académia : charge la carte des notions et les questions écrites (donnees/), et fabrique une question
   pour une notion, écrite ou calculée par un générateur (js/questions/<matiere>.js, GENERATEURS).
   Un réseau qui ne répond pas : on le dit, on réessaie tout seul, et les parties qui attendaient leur placement
   sont placées dès que la carte des notions arrive (js/adaptatif.js, placerLesParties). */
const Q = {notions: [], parNotion: {}, vraifaux: {}, vfParNotion: {}, mots: {}, pret: false, charges: new Set()};
const FICHIERS = ["anglais", "allemand", "luxembourgeois", "logique", "sciences", "vraifaux"];

let chargementEnCours = null, reessaiReseau = null, attenteReseau = 2000;
function chargerQuestions(){
  if (!chargementEnCours) chargementEnCours = lireDonnees().finally(() => { chargementEnCours = null; });
  return chargementEnCours;
}
async function lireDonnees(){
  const lire = async f => {
    for (let essai = 0; essai < 4; essai++) {
      try { const r = await fetch(f, {cache: "no-cache"}); if (r.ok) return await r.json(); } catch (e) {}
      await new Promise(ok => setTimeout(ok, 400 * (essai + 1)));
    }
    return null;
  };
  if (!Q.notions.length) { const n = await lire("donnees/notions.json"); if (Array.isArray(n)) Q.notions = n; }
  await Promise.all(FICHIERS.filter(m => !Q.charges.has(m)).map(async m => {
    const l = await lire(`donnees/questions/${m}.json`);
    if (Array.isArray(l) && !Q.charges.has(m)) { l.forEach(ajouterQuestion); Q.charges.add(m); }
  }));
  Q.pret = Q.notions.length > 0 && Q.charges.size === FICHIERS.length;
  if (Q.notions.length && typeof placerLesParties === "function") placerLesParties();
  if (!Q.pret) reessayerPlusTard();
}
// the network did not answer: say so, and try again on its own, more and more slowly
function reessayerPlusTard(){
  if (location.protocol === "file:") return toast("Les questions n'ont pas pu être chargées : ouvrez le jeu par son adresse internet", 6000);
  if (reessaiReseau) return;
  toast("Le réseau ne répond pas, on réessaie…", 4000);
  reessaiReseau = setTimeout(() => { reessaiReseau = null; chargerQuestions(); }, attenteReseau);
  attenteReseau = Math.min(2 * attenteReseau, 20000);
}

const estVraiFaux = q => !!q && (q.reponse === "vrai" || q.reponse === "faux");
function ajouterQuestion(q){
  if (!q || !q.question || q.reponse == null) return;
  const dans = (t, k) => (t[k] = t[k] || []);
  if (estVraiFaux(q)) { dans(Q.vraifaux, q.matiere).push(q); dans(Q.vfParNotion, q.notion).push(q); }
  else dans(Q.parNotion, q.notion).push(q);
  // the foreign words of a subject, so that an explanation says them with the right voice
  const m = q.langue && q.langue !== "fr" && /«\s*(.+?)\s*»/.exec(q.question);
  if (m) motConnu(q.langue, m[1]);
  if (q.oreille) [q.reponse, ...(q.distracteurs || [])].forEach(w => motConnu(q.oreille, w));
}
function motConnu(lang, mot){ (Q.mots[lang] = Q.mots[lang] || new Set()).add(String(mot).toLowerCase()); }
const notionDe = id => Q.notions.find(n => n.id === id) || null;
const notionsDe = matiere => Q.notions.filter(n => n.matiere === matiere).sort((a, b) => (a.ordre || 0) - (b.ordre || 0));
const ageMin = x => x && x.age_min != null ? x.age_min : 4;

// one question for a notion and an attack: the generator when there is one, else a written question
function questionPour(notion, mecanique){
  if (!notion) return null;
  const g = notion.generateur && typeof GENERATEURS !== "undefined" && GENERATEURS[notion.generateur];
  if (g) {
    try {
      const q = g(notion.params || {}, mecanique);
      if (q) return {matiere: notion.matiere, notion: notion.id, langue: "fr", ...q};
    } catch (e) { console.warn("générateur", notion.generateur, e); }
  }
  const l = Q.parNotion[notion.id] || [];
  const adaptees = l.filter(q => !q.mecaniques || q.mecaniques.includes(mecanique));
  return pick(adaptees.length ? adaptees : l, 1)[0] || null;
}

// the pictures of a text, one by one ("☀️" and "👨‍👩‍👧" are one picture each)
function graphemes(t){
  t = String(t || "");
  if (typeof Intl !== "undefined" && typeof Intl.Segmenter === "function") return [...new Intl.Segmenter("fr", {granularity: "grapheme"}).segment(t)].map(x => x.segment);
  const out = [];
  for (const c of t) {
    const lie = /[\u{FE0F}\u{200D}\u{20E3}\u{1F3FB}-\u{1F3FF}]/u.test(c) || (out.length && out[out.length - 1].endsWith("‍"));
    if (lie && out.length) out[out.length - 1] += c; else out.push(c);
  }
  return out;
}
const estImage = g => /\p{Extended_Pictographic}/u.test(g);
// an answer made of several pictures (a group to count or compare): its content is the choice itself
const estGroupe = v => { const g = graphemes(v); return g.length > 1 && g.every(estImage); };

/* a True/False shield built from a written question, when its notion has none written: the question, a proposed
   answer shown big (q.propose), and the child says whether it is the right one. Never when the choices are the
   content (groups of pictures, "le plus", "Montre"), nor when the question already has a picture to look at. */
function vraiFauxDepuis(q){
  if (!q || !q.distracteurs || !q.distracteurs.length || estVraiFaux(q) || q.oreille || q.visuel || q.dessin) return null;
  if ([q.reponse, ...q.distracteurs].some(estGroupe) || /^(Montre|Où y a-t-il)/.test(q.question)) return null;
  const vrai = Math.random() < .5, propose = vrai ? q.reponse : pick(q.distracteurs, 1)[0];
  return {id: `${q.id || ""}_vf_${propose}`, matiere: q.matiere, notion: q.notion, langue: q.langue, niveau: q.niveau || 1,
    mecaniques: ["bouclier"], question: q.question, propose, visuel: "", dire: q.dire || q.question, legendes: q.legendes,
    reponse: vrai ? "vrai" : "faux", distracteurs: [vrai ? "faux" : "vrai"], explication: q.explication || "", indice: "", genere: true};
}

/* a harder form of a written language question, for the heavy strike once the whole subject is known (GDD §4):
   the word only heard (q.mot, not written), or, for a reader, the picture shown and the foreign word to find
   among others of the same notion (animals, colours, numbers: their pictures never mean two things) */
function formeDure(q, age){
  const m = q && q.langue && q.langue !== "fr" && !q.oreille && !estVraiFaux(q) && /«\s*(.+?)\s*»/.exec(q.question);
  if (!m) return q;
  const mot = m[1], n = notionDe(q.notion), sens = n && /animaux|couleurs|nombres/.test(n.id);
  if (age >= 6 && sens && Math.random() < .5) {
    const vus = new Set([mot.toLowerCase()]), autres = [];
    shuffle(Q.parNotion[q.notion] || []).forEach(x => {
      const w = /«\s*(.+?)\s*»/.exec(x.question);
      if (w && !vus.has(w[1].toLowerCase()) && x.reponse !== q.reponse && autres.length < 3) { vus.add(w[1].toLowerCase()); autres.push(w[1]); }
    });
    if (autres.length >= 2) return {...q, id: `${q.id}_image`, question: "Quel mot va avec cette image ?", dire: "Quel mot va avec cette image ?",
      langue: "fr", oreille: q.langue, visuel: q.reponse, legendes: undefined, reponse: mot, distracteurs: autres, indice: "", forme: "image"};
  }
  return {...q, id: `${q.id}_ecoute`, question: "Qu'est-ce qui va avec ce mot ?", dire: mot, mot, forme: "ecoute"};
}

// the answers shown on the buttons, the right one among them, shuffled
function boutonsDe(q){
  if (estVraiFaux(q)) return ["vrai", "faux"];
  const d = [...new Set((q.distracteurs || []).filter(x => x !== q.reponse))].slice(0, 3);
  return shuffle([q.reponse, ...d]);
}
