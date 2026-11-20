/* Académia : les exercices sans boutons (donnees/formes.json, écrits par outils/contenu/scripts/formes.py) : remettre
   dans l'ordre, relier, trier. js/questions.js les charge avec les questions et en tire un de temps en temps à la place
   d'une question à boutons, pour une attaque rapide ou massive ; js/question_formes.js les joue.
   Un exercice à lire (« lecture ») n'est jamais donné jusqu'à 5 ans. Le fichier manque : le jeu joue sans eux. */
const FORMES = {parNotion: {}, charge: false, part: 0.3};   // part: the share of the questions of a notion that become an exercise, when it has some

async function chargerFormes(){
  if (FORMES.charge) return;
  try {
    const r = await fetch("donnees/formes.json", {cache: "no-cache"});
    const l = r.ok ? await r.json() : null;
    if (!Array.isArray(l)) return;
    const par = {};
    l.forEach(f => { if (f && f.notion && f.forme) (par[f.notion] = par[f.notion] || []).push(f); });
    FORMES.parNotion = par; FORMES.charge = true;
  } catch (e) {}
}

// the exercises a child can do for a notion
function formesDe(notion){
  const p = P(), age = p ? (typeof ageDuJour === "function" ? ageDuJour(p) : p.age) : 7;
  return (FORMES.parNotion[notion.id] || []).filter(f => estForme(f) && !(age <= 5 && f.lecture));
}
function formePour(notion, mecanique){
  if (!notion || (mecanique !== "rapide" && mecanique !== "massive") || typeof estForme !== "function") return null;
  const l = formesDe(notion);
  return l.length && Math.random() < FORMES.part ? pick(l, 1)[0] : null;
}
