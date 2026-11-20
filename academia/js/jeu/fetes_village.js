/* Académia : les visiteurs des fêtes sur la place du village (js/fetes.js dit les dates). Du 1er au 6 décembre, le
   Kleeschen (saint Nicolas) et le Houseker se tiennent au bas de la place, hors des chemins et des portes ;
   js/jeu/carte.js les ajoute aux habitants (habitantsDeFete), js/jeu/monde.js leur donne la parole (lignesFete).
   1. Le Kleeschen dit son nom en luxembourgeois (les « » : l'enregistrement de lod.lu, js/mots_du_jeu.js), raconte la
      nuit du 5 au 6 décembre, puis propose la question du jour ; son écran est js/kleeschen.js. À partir du 6 : le
      cadeau.
   2. Le Houseker, qui porte le sac, dit une chose gentille et drôle, et le nom du Boxemännchen.
   3. Une fois par jour, à l'arrivée au village, le Kleeschen appelle l'enfant dans la bulle du village. */
const PLACES_FETE = {kleeschen: {x: 18, y: 17}, houseker: {x: 20, y: 17}};

function habitantsDeFete(){
  const l = [];
  if (typeof kleeschenIci !== "function") return l;
  if (kleeschenIci()) l.push({...PLACES_FETE.kleeschen, cle: "kleeschen", nom: "Kleeschen", dir: 0, fete: "kleeschen", dit: []});
  if (housekerIci()) l.push({...PLACES_FETE.houseker, cle: "houseker", nom: "Houseker", dir: 0, fete: "houseker", dit: []});
  return l;
}

const DITS_HOUSEKER = [
  "Chut… j'ai goûté un « Boxemännchen » du sac. C'est un bonhomme en brioche, avec des yeux en raisins secs. Un seul, promis !",
  "Mon fagot ? Ce sont des brindilles pour allumer le feu : il fait froid, la nuit, sur les toits !",
  "Le sac du « Kleeschen » est plein de mandarines, de noix et de chocolat. Devine qui le porte ? C'est moi !",
];
// what a visitor says when the hero comes to talk: {lignes, choix, langue, fin} for dialogue() (js/jeu/monde.js)
function lignesFete(scene, n){
  const p = P(), petit = p.age <= 5;
  if (n.fete === "houseker") {
    const k = etatKleeschen(p); k.houseker = (Number(k.houseker) || 0) + 1; sauver();
    return {lignes: [k.houseker === 1 ? "« Houseker » ! C'est mon nom. Je suis l'ami du « Kleeschen » et je porte son gros sac." : "« Houseker » ! C'est encore moi.",
      DITS_HOUSEKER[(k.houseker - 1) % DITS_HOUSEKER.length]], langue: "lb"};
  }
  const m = momentKleeschen(p), k = etatKleeschen(p), ouvrir = () => ouvrirKleeschen();
  const choix = (dit, aide) => petit ? [{texte: "🎁", classe: "epees", aide, dit: "Touche le cadeau !", action: ouvrir}]
    : [{texte: "🎁 " + aide, dit, action: ouvrir}, {texte: "↩️ Plus tard", dit: "ou Plus tard.", action: () => {}}];
  if (m === "cadeau") return {lignes: ["« Kleeschen » ! Cette nuit, je suis passé dans toutes les maisons. Viens voir ce que j'ai laissé dans tes chaussons !"],
    choix: choix("Touche Voir mes chaussons,", "Voir mes chaussons"), langue: "lb"};
  if (m === "adieu") return {lignes: ["« Kleeschen » ! Je repars ce soir. Goûte un « Boxemännchen », le bonhomme en brioche de ma fête, et à l'année prochaine !"], langue: "lb"};
  const vu = k.vu === jourFete();
  k.vu = jourFete(); sauver();
  const lignes = vu ? ["« Kleeschen » ! Te revoilà."] : petit
    ? ["« Kleeschen » ! C'est mon nom : je suis saint Nicolas.", "Dans la nuit du 5 au 6 décembre, je remplis les chaussons des enfants !"]
    : ["« Kleeschen » ! C'est mon nom en luxembourgeois : je suis saint Nicolas.",
      "Dans la nuit du 5 au 6 décembre, je passe dans les maisons du Luxembourg. Les enfants posent leurs chaussons, et je les remplis !"];
  lignes.push(chaussonDuJourPlein(p) ? "Ton chausson d'aujourd'hui est déjà plein. Tu veux les regarder ?" : "Chaque jour, une bonne réponse remplit un de tes chaussons. On essaie ?");
  return {lignes, choix: choix("Touche Mes chaussons,", "Mes chaussons"), langue: "lb"};
}

// once a day, when the village opens: the Kleeschen calls the child (never over another bubble, a dialogue, a moment)
function annoncerFete(scene){
  const p = P();
  if (!p || scene.ile || typeof momentKleeschen !== "function") return;
  const m = momentKleeschen(p);
  if (!m || m === "adieu") return;
  const k = etatKleeschen(p);
  if (k.annonce === jourFete()) return;
  scene.time.delayedCall(TEST ? 400 : 2600, () => {
    if (!scene.sys.isActive() || scene.occupe() || bulleVillageOuverte() || k.annonce === jourFete()) return;
    k.annonce = jourFete(); sauver();
    bulleVillage("kleeschen", "Kleeschen", m === "cadeau" ? "« Kleeschen » ! Je suis passé cette nuit. Viens me voir sur la place !"
      : "« Kleeschen » ! Je suis sur la place du village. Viens me voir !", "lb");
  });
}
