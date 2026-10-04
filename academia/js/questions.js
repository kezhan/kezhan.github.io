/* Académia : charge la carte des notions et les questions écrites (donnees/), et fabrique une question
   pour une notion, écrite ou calculée par un générateur (js/questions/<matiere>.js, GENERATEURS). */
const Q = {notions: [], parNotion: {}, vraifaux: {}, pret: false};
const FICHIERS = ["anglais", "allemand", "luxembourgeois", "logique", "sciences", "vraifaux"];

async function chargerQuestions(){
  const lire = async f => { try { const r = await fetch(f, {cache: "no-cache"}); return r.ok ? await r.json() : null; } catch (e) { return null; } };
  Q.notions = (await lire("donnees/notions.json")) || [];
  const listes = await Promise.all(FICHIERS.map(m => lire(`donnees/questions/${m}.json`)));
  listes.forEach(l => (Array.isArray(l) ? l : []).forEach(ajouterQuestion));
  Q.pret = true;
}
const estVraiFaux = q => q.reponse === "vrai" || q.reponse === "faux";
function ajouterQuestion(q){
  if (!q || !q.question || q.reponse == null) return;
  if (estVraiFaux(q)) (Q.vraifaux[q.matiere] = Q.vraifaux[q.matiere] || []).push(q);
  else (Q.parNotion[q.notion] = Q.parNotion[q.notion] || []).push(q);
}
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
// a True/False shield question built from an ordinary one, when a subject has no written ones
function vraiFauxDepuis(q){
  if (!q || !q.distracteurs || !q.distracteurs.length) return null;
  const vrai = Math.random() < .5, propose = vrai ? q.reponse : pick(q.distracteurs, 1)[0];
  return {id: (q.id || "") + "_vf_" + propose, matiere: q.matiere, notion: q.notion, langue: q.langue,
    question: `${q.question} ➜ ${propose}`, visuel: q.visuel || "", dire: `${q.dire || q.question}. ${sansEmoji(propose)} ?`,
    reponse: vrai ? "vrai" : "faux", explication: q.explication || "", genere: true};
}
// the answers shown on the buttons, the right one among them, shuffled
function boutonsDe(q){
  if (estVraiFaux(q)) return ["vrai", "faux"];
  const d = [...new Set((q.distracteurs || []).filter(x => x !== q.reponse))].slice(0, 3);
  return shuffle([q.reponse, ...d]);
}
