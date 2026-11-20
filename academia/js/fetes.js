/* Académia : les fêtes du calendrier, leurs dates et ce que la partie en garde (P().fetes). La première : le Kleeschen
   (saint Nicolas), du 1er au 6 décembre au village avec le Houseker (js/jeu/fetes_village.js, js/kleeschen.js).
   1. jourFete() : le jour qui compte pour les fêtes, « 2026-12-02 ». En jeu, c'est le jour de l'appareil (jour(),
      js/adaptatif.js). Dans un navigateur piloté par un robot (les recettes, navigator.webdriver)
      ou en mode TEST, c'est le jour écrit dans localStorage « academia.jourTest », et sans lui aucune fête : une recette
      lancée en décembre ne trouve pas de visiteurs sur la place.
   2. Le Kleeschen : du 1er au 5 décembre, une bonne réponse par jour remplit le chausson du jour
      (P().fetes.kleeschen.chaussons, la liste des jours remplis) ; à partir du 6 et jusqu'au 12, le cadeau, une seule
      fois par an (P().fetes.kleeschen.cadeau, le jour où il a été donné) : des étincelles selon les chaussons remplis
      et l'autocollant du Kleeschen. Le Kleeschen reste sur la place du 7 au 12 tant que le cadeau n'est pas pris ; le
      Houseker repart le 6 au soir. */
const KLEESCHEN = {debut: "12-01", veille: "12-05", cadeau: "12-06", fin: "12-12", base: 10, parChausson: 10, autocollant: "kleeschen"};
const JOURS_CHAUSSONS = ["12-01", "12-02", "12-03", "12-04", "12-05"];

function jourFete(){
  if (!TEST && !navigator.webdriver) return jour();
  try { return localStorage.getItem("academia.jourTest") || ""; } catch (e) { return ""; }
}
const moisJourFete = () => jourFete().slice(5);
// the clock of what takes time to be ready (the harvests of the island's buildings, js/chantiers.js): the device's. In a
// piloted browser or in TEST mode, « academia.avanceTest » (milliseconds) is added: a recette makes the hours pass
function maintenantJeu(){
  let plus = 0;
  if (TEST || navigator.webdriver) { try { plus = Number(localStorage.getItem("academia.avanceTest")) || 0; } catch (e) {} }
  return Date.now() + plus;
}

// this year's Kleeschen in the child's game (a new year starts again, empty)
function etatKleeschen(p = P()){
  const an = jourFete().slice(0, 4);
  if (!p.fetes || typeof p.fetes !== "object" || Array.isArray(p.fetes)) p.fetes = {};
  const k = p.fetes.kleeschen;
  if (!k || typeof k !== "object" || k.an !== an || !Array.isArray(k.chaussons)) p.fetes.kleeschen = {an, chaussons: [], cadeau: null};
  return p.fetes.kleeschen;
}
// where the feast is today: "chaussons" (1st to 5th), "cadeau" (from the 6th to the 12th, the gift not taken yet),
// "adieu" (the 6th, the gift taken), or null. Without a child (the home screen): the dates alone
function momentKleeschen(p = P()){
  const j = moisJourFete();
  if (!j || j < KLEESCHEN.debut || j > KLEESCHEN.fin) return null;
  if (j <= KLEESCHEN.veille) return "chaussons";
  if (!p) return j === KLEESCHEN.cadeau ? "cadeau" : null;
  if (!etatKleeschen(p).cadeau) return "cadeau";
  return j === KLEESCHEN.cadeau ? "adieu" : null;
}
const kleeschenIci = (p = P()) => !!momentKleeschen(p);
const housekerIci = () => { const j = moisJourFete(); return !!j && j >= KLEESCHEN.debut && j <= KLEESCHEN.cadeau; };

// the five slippers: {jour: 1..5, plein, aujourdhui, passe (a day gone by, left empty)}
function chaussonsKleeschen(p = P()){
  const k = etatKleeschen(p), j = moisJourFete();
  return JOURS_CHAUSSONS.map((d, i) => ({jour: i + 1, plein: k.chaussons.includes(d), aujourdhui: d === j, passe: d < j}));
}
const chaussonDuJourPlein = (p = P()) => etatKleeschen(p).chaussons.includes(moisJourFete());
// a right answer: today's slipper is filled (once a day, from the 1st to the 5th); true when it was just filled
function remplirChausson(p = P()){
  const j = moisJourFete(), k = etatKleeschen(p);
  if (momentKleeschen(p) !== "chaussons" || k.chaussons.includes(j)) return false;
  k.chaussons.push(j); sauver();
  return true;
}
const etincellesKleeschen = (p = P()) => KLEESCHEN.base + KLEESCHEN.parChausson * etatKleeschen(p).chaussons.length;
// the gift of the 6th, once a year: the étincelles and the sticker in the album; null when it cannot be given
function donnerCadeauKleeschen(p = P()){
  if (momentKleeschen(p) !== "cadeau") return null;
  const k = etatKleeschen(p), n = etincellesKleeschen(p);
  k.cadeau = jourFete();
  p.etincelles += n;
  if (!Array.isArray(p.album.autocollants)) p.album.autocollants = [];
  p.album.autocollants.push({id: KLEESCHEN.autocollant, jour: jourFete(), texte: ""});
  sauver();
  return {etincelles: n, chaussons: k.chaussons.length};
}
