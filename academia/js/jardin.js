/* Académia : le jardin de l'enfant, ses règles et son état (P().jardin). Des parcelles où il plante une graine payée
   en étincelles ; arrosée avec une bonne réponse, une fois par jour, la plante grandit d'un stade (1 semée, 2 pousse,
   3 grande, 4 mûre) ; mûre, il la récolte et compte ce qu'il cueille, et gagne une étincelle par récolte. Une récolte
   donnée au compagnon lui donne de l'XP. Rien ne meurt ni ne fane si l'enfant ne vient pas.
   L'écran est dans js/jardin_vue.js, le potager du village qui l'ouvre dans js/jeu/jardin_village.js. */
const PARCELLES = 6, STADE_MUR = 4, XP_RECOLTE = 12;
// each crop: its French names, its price, how many it gives, and its name in the three languages (the Luxembourgish
// one is said by its recording of lod.lu, never by the browser: js/voix.js)
const CULTURES = {
  carotte: {nom: "carotte", pluriel: "carottes", une: "une carotte", prix: 5, recolte: [3, 5], en: "carrot", de: "die Karotte", lb: "d'Muert"},
  fraise: {nom: "fraise", pluriel: "fraises", une: "une fraise", prix: 6, recolte: [4, 6], en: "strawberry", de: "die Erdbeere", lb: "d'Äerdbier"},
  pomme_de_terre: {nom: "pomme de terre", pluriel: "pommes de terre", une: "une pomme de terre", prix: 5, recolte: [3, 5], en: "potato", de: "die Kartoffel", lb: "d'Gromper"},
  tomate: {nom: "tomate", pluriel: "tomates", une: "une tomate", prix: 6, recolte: [3, 5], en: "tomato", de: "die Tomate", lb: ""},
  salade: {nom: "salade", pluriel: "salades", une: "une salade", prix: 4, recolte: [1, 2], en: "lettuce", de: "der Salat", lb: ""},
  citrouille: {nom: "citrouille", pluriel: "citrouilles", une: "une citrouille", prix: 8, recolte: [1, 2], en: "pumpkin", de: "der Kürbis", lb: ""}
};
const MATIERES_JARDIN = ["maths", "anglais", "allemand", "luxembourgeois", "logique", "sciences"];

const parcelleVide = () => ({culture: null, stade: 0, semee: null, arrosee: null});
// a garden as it should be, whatever the save holds (an old game has none; js/etat.js, reparer)
function reparerJardin(j){
  const ok = j && typeof j === "object" && !Array.isArray(j);
  const parcelles = (ok && Array.isArray(j.parcelles) ? j.parcelles : []).slice(0, PARCELLES).map(pa =>
    pa && CULTURES[pa.culture] ? {culture: pa.culture, stade: Math.max(1, Math.min(STADE_MUR, Number(pa.stade) || 1)),
      semee: pa.semee || null, arrosee: pa.arrosee || null} : parcelleVide());
  while (parcelles.length < PARCELLES) parcelles.push(parcelleVide());
  const recoltes = {};
  Object.entries(ok && j.recoltes && typeof j.recoltes === "object" ? j.recoltes : {})
    .forEach(([k, n]) => { if (CULTURES[k] && n > 0) recoltes[k] = Math.floor(n); });
  return {parcelles, recoltes, tour: ok && Number(j.tour) || 0, vu: !!(ok && j.vu)};
}
function jardinDe(p = P()){
  if (!p) return null;
  if (!p.jardin || !Array.isArray(p.jardin.parcelles) || p.jardin.parcelles.length !== PARCELLES) p.jardin = reparerJardin(p.jardin);
  return p.jardin;
}

// what a plot waits for: vide (a seed), soif (its water of the day), arrosee (tomorrow), mure (to pick)
function etatParcelle(pa){
  if (!pa.culture) return "vide";
  if (pa.stade >= STADE_MUR) return "mure";
  return pa.arrosee === jour() ? "arrosee" : "soif";
}
// what the village's sign says: plants waiting for water, ripe ones
function aFaireAuJardin(p = P()){
  const j = p && p.jardin;
  if (!j || !Array.isArray(j.parcelles)) return {soif: 0, mures: 0, plantes: 0};
  const e = j.parcelles.map(etatParcelle);
  return {soif: e.filter(x => x === "soif").length, mures: e.filter(x => x === "mure").length, plantes: e.filter(x => x !== "vide").length};
}

function planter(i, culture){
  const p = P(), j = jardinDe(p), c = CULTURES[culture], pa = j.parcelles[i];
  if (!c || !pa || pa.culture || p.etincelles < c.prix) return false;
  p.etincelles -= c.prix;
  j.parcelles[i] = {culture, stade: 1, semee: jour(), arrosee: null};
  sauver();
  return true;
}
// watered after a right answer: one stage more, once a day
function arroser(i){
  const j = jardinDe(), pa = j.parcelles[i];
  if (!pa || etatParcelle(pa) !== "soif") return null;
  pa.arrosee = jour(); pa.stade = Math.min(STADE_MUR, pa.stade + 1);
  sauver();
  return pa.stade;
}
function recolter(i){
  const p = P(), j = jardinDe(p), pa = j.parcelles[i];
  if (!pa || etatParcelle(pa) !== "mure") return null;
  const c = CULTURES[pa.culture], [a, b] = c.recolte, n = a + Math.floor(Math.random() * (b - a + 1));
  j.recoltes[pa.culture] = (j.recoltes[pa.culture] || 0) + n;
  p.etincelles += n;
  j.parcelles[i] = parcelleVide();
  sauver();
  return {culture: pa.culture, n};
}
// one crop given to a companion: XP (twice as much for one far behind the team, as in the fights)
function donnerRecolte(culture, c){
  const j = jardinDe();
  if (!c || !(j.recoltes[culture] > 0)) return null;
  j.recoltes[culture]--;
  if (!j.recoltes[culture]) delete j.recoltes[culture];
  const xp = XP_RECOLTE * rattrapage(c), gain = gagnerXP(c, xp);
  sauver();
  return {...gain, xp};
}
// the subject of the next watering: each in turn, among those that have questions for this child
function matiereArrosage(){
  const j = jardinDe(), l = MATIERES_JARDIN.filter(m => notionsDe(m).length);
  if (!l.length) return null;
  const m = l[j.tour % l.length];
  j.tour++;
  return m;
}
