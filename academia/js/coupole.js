/* Académia : la coupole de l'Observatoire, une salle en 3D derrière la porte ouverte (js/jeu/lanceur.js, entrerMaison).
   Le Sage y montre le ciel : les planètes autour du Soleil (on en touche une, il dit son nom et ce qu'elle a de
   spécial), puis la Terre qui tourne (le jour et la nuit au Luxembourg). Le bouton « Exercice » pose une question de
   sciences sur ce qu'on vient de voir ; une bonne réponse donne une étincelle. La scène est dans js/coupole_3d.js ;
   si l'appareil ne sait pas la dessiner, le Sage le dit et les exercices restent. */
if (!ECRANS.includes("coupole")) ECRANS.push("coupole");
const FAITS_PLANETES = {
  mercure: "Mercure ! La plus petite planète, et la plus proche du Soleil.",
  venus: "Vénus ! La planète la plus chaude. On la voit briller le soir, comme une étoile.",
  terre: "La Terre ! Notre planète : la seule où l'on connaît de la vie. Le bleu, c'est l'eau des océans.",
  mars: "Mars ! La planète rouge. Des robots y roulent pour l'explorer.",
  jupiter: "Jupiter ! La plus grosse planète : une boule de gaz géante.",
  saturne: "Saturne ! La planète aux anneaux, faits de glace et de cailloux.",
  uranus: "Uranus ! Une planète glacée, qui roule couchée sur le côté.",
  neptune: "Neptune ! La planète la plus lointaine : il y souffle des vents très forts.",
};
const COUPOLE = {occupe: false, vue: "planetes", choisie: null, ok: true};

function annoncerCoupole(texte){ ecrireIcones($("bulleCoupole"), affiche(texte)); return dire(texte); }
function vuesCoupole(p = P()){ if (!p.coupole) p.coupole = {vues: []}; return p.coupole.vues; }

async function ouvrirCoupole(){
  const p = P(); if (!p) return;
  montrer("coupole");
  imageReessayee($("coupoleSage"), `${DOSSIER_HD}/portraits/sage.png`);
  $("coupoleEtincelles").textContent = p.etincelles;
  $("panneauCoupole").classList.add("cache");
  $("coupoleNom").hidden = true; COUPOLE.choisie = null;
  try {
    await chargerThree();
    C3.surPlanete = planeteTouchee; C3.surJourNuit = jourOuNuit; C3.apresImage = suivreCoupole;
    C3.ouvrir($("coupoleToile"));
    COUPOLE.ok = true;
  } catch (e) { COUPOLE.ok = false; }
  $("coupole").dataset.pret = COUPOLE.ok ? "1" : "0";
  vueCoupole("planetes", true);
}
function fermerCoupole(){
  if (COUPOLE.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  if (COUPOLE.ok) C3.fermer();
  montrer("monde");
  if (typeof majBarre === "function") majBarre();
}
function vueCoupole(nom, accueil){
  COUPOLE.vue = nom; COUPOLE.choisie = null;
  $("coupole").dataset.vue = nom;
  $("coupoleNom").hidden = true; $("coupoleJour").hidden = true;
  ["planetes", "terre"].forEach(v => $("btnVue_" + v).classList.toggle("choisi", v === nom));
  if (!COUPOLE.ok) return annoncerCoupole("La coupole ne s'allume pas sur cet appareil. Mais je peux te poser une question sur le ciel !");
  C3.vue(nom);
  if (nom === "planetes") annoncerCoupole((accueil ? "Bienvenue sous la coupole ! " : "") + "Voici le Soleil et ses planètes. Touche une planète, et fais glisser ton doigt pour tourner autour !");
  else annoncerCoupole("Voici la Terre. Le point rouge, c'est le Luxembourg. Fais glisser ton doigt pour la faire tourner !");
}
function planeteTouchee(pl){
  sfx.tap(); if (typeof jouerSon === "function") jouerSon("scintille");
  COUPOLE.choisie = pl.id;
  const n = $("coupoleNom"); n.textContent = majuscule(pl.nom); n.hidden = false; n.dataset.planete = pl.id;
  const v = vuesCoupole(); if (!v.includes(pl.id)) { v.push(pl.id); sauver(); }
  annoncerCoupole(FAITS_PLANETES[pl.id]);
}
// the Earth has turned: the pin of Luxembourg went into the day, or into the night
function jourOuNuit(jour, premier){
  const j = $("coupoleJour"); j.hidden = false; j.dataset.jour = jour ? "1" : "0";
  j.textContent = jour ? "Jour au Luxembourg" : "Nuit au Luxembourg";
  if (premier) return;
  annoncerCoupole(jour ? "Le Luxembourg passe du côté du Soleil : c'est le jour, on se lève !"
    : "Le Luxembourg tourne le dos au Soleil : c'est la nuit, on dort. La Terre tourne sur elle-même en un jour.");
}
// after each picture: the name stays over its planet
function suivreCoupole(){
  if (!COUPOLE.choisie) return;
  const e = C3.ecranDe(COUPOLE.choisie), n = $("coupoleNom");
  if (e) { n.style.left = e[0] + "px"; n.style.top = e[1] + "px"; }
}

// a question of sciences about the sky (the child's own level for the little ones); right, an étincelle
async function exerciceCoupole(){
  if (COUPOLE.occupe) return;
  sfx.tap();
  const p = P(), ciel = COUPOLE.vue === "terre" ? ["sc_jour_nuit", "sc_espace"] : ["sc_espace", "sc_jour_nuit"];
  let q = null;
  if (p.age >= 7) for (const n of ciel) { q = questionPour(n, "rapide"); if (q) break; }
  if (!q) q = tirer("sciences", "rapide");
  if (!q) return annoncerCoupole("Pas de question pour l'instant : reviens dans un moment !");
  COUPOLE.occupe = true;
  const panneau = $("panneauCoupole"), ecran = $("coupole");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: "Sous la coupole", jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerCoupole("Presque ! Regarde encore le ciel, puis essaie une autre question.");
    p.etincelles += 1; sauver(); $("coupoleEtincelles").textContent = p.etincelles;
    sfx.niveau();
    apresRedit(() => $("coupole").hidden ? null : annoncerCoupole("Bravo ! Une étincelle pour toi. Encore une question, ou on regarde le ciel ?"));
  } finally {
    COUPOLE.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
