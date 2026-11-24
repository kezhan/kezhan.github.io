/* Académia : un quartier du village dans la scène du village (js/jeu/monde.js, quand la partie est dans un quartier ;
   carte : js/jeu/carte_quartier.js, ce qu'il contient : js/quartiers.js).
   1. Le dessin : le quartier entier est une image (son paysage, son pré, son chemin), plus grande que la zone tenue à
      l'écran (BORD_QUARTIER cases de paysage de chaque côté : un écran plus large ou plus haut ne montre jamais de
      vide). Par-dessus, les hautes herbes du village (les mêmes pièces, le même vent) quand le quartier en a, et les
      quatre choses à toucher, posées et ouvertes par InterieurMaison (js/jeu/interieur_maison.js) : l'écran et la
      règle sont ceux des maisons (js/maison_chose.js). Dessins : outils/village/scripts/quartiers.py et
      quartiers_choses.py (à nous), dans assets/<qualité>/quartiers.
   2. HistoireQuartier : ce que le quartier dit de lui-même. À l'entrée, son habitant salue dans sa langue (un
      enregistrement, js/jeu/langues_habitants.js) ; à la première visite, la voix nomme le quartier et dit quoi
      faire. Les missions du jour tenues ici y sont annoncées. La main du guide montre une chose qui a encore son
      étincelle, puis les Ombres des hautes herbes.
   Le chemin du bas ramène au village (sortirQuartier, js/quartiers.js). Ce que le village a et que le quartier n'a
   pas (la place, le jardin, le marchand, la barque) est absent, comme dans une salle ; deux papillons y volent, et le
   trésor du jour et le visiteur du jour y passent aussi (js/jeu/surprises_village.js). */
const BORD_QUARTIER = {x: 6, y: 3};   // tiles of landscape drawn around the zone
const PAPILLONS_QUARTIER = [[4.5, 9.5], [10, 8.2]];   // tiles from the zone's corner, over the meadow
const dossierQuartiers = () => `${DOSSIER_HD}/quartiers/`;

function chargerQuartierHD(load, id){
  const charger = (cle, fichier) => { if (!load.textureManager.exists(cle)) load.image(cle, dossierQuartiers() + fichier + ".png"); };
  charger("qv_" + id, "quartier_" + id);
  QUARTIERS[id].choses.forEach(c => charger(`mi_chose_${id}_${c.id}`, `chose_${id}_${c.id}`));
  if (!load.textureManager.exists("ico_etincelle")) load.image("ico_etincelle", iconeSrc("etincelle"));
}
// the quarter in world units, and what the village's own code expects of a drawn place (scene.villageHD)
function dessinerQuartierHD(scene, carte){
  const Z = carte.salle, B = BORD_QUARTIER, rendu = {etiquettes: {}, maisons: [], ecrans: [], touffes: {}, lumieres: [], enseignes: {}, carte};
  scene.add.rectangle(0, 0, LARG * CASE, HAUT * CASE, 0x2F7A45).setOrigin(0).setDepth(0);
  const cle = "qv_" + carte.quartier;
  if (scene.textures.exists(cle)) {
    scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
    scene.add.image((Z.x - B.x) * CASE, (Z.y - B.y) * CASE, cle).setOrigin(0).setDisplaySize((Z.w + 2 * B.x) * CASE, (Z.h + 2 * B.y) * CASE).setDepth(1);
  }
  scene.villageHD = rendu;
  if (typeof VILLAGE_HD.herbes === "function" && IMAGES_VILLAGE.every(i => scene.textures.exists(i.cle))) {
    VILLAGE_HD.herbes(scene, carte, rendu);   // the tall grass, as in the village
    VIVANT_HD.animer(scene, carte, rendu);    // its wind
  }
  // two butterflies over the meadow, named in the quarter's language when touched (js/jeu/betes_village.js)
  const maison = Object.keys(LANGUE_MAISON).find(r => LANGUE_MAISON[r] === QUARTIERS[carte.quartier].langue);
  PAPILLONS_QUARTIER.forEach(([x, y], k) => VIVANT_HD.papillon(scene, rendu, (Z.x + x) * CASE, (Z.y + y) * CASE, k % 2, maison));
  return rendu;
}

class HistoireQuartier extends HistoireMaison {
  parole(cle, texte, langue, note){ const h = QUARTIERS[this.s.quartier].habitant; return bulleVillage(cle, h.cle === cle ? h.nom : "", texte, langue, note); }
  // on the way in: the first time, the quarter is named and the voice says what to do; then its resident's word, in
  // his language
  accueillir(){
    const s = this.s, Q = QUARTIERS[s.quartier], n = s.pnj[0], cle = "quartier_" + s.quartier, petit = P().age <= 5;
    if (n) s.vie.saluer(n.s);
    if (!histoireDe()[cle]) {
      noterHistoire(cle);
      return this.parole(Q.habitant.cle, petit ? `Te voici ${Q.a} ! Touche ce que tu vois !` : `Te voici ${Q.a} ! Touche ce que tu vois : chaque chose a un secret, et une étincelle pour toi.`);
    }
    const salut = n && typeof salutHabitant === "function" ? salutHabitant(n) : null;
    return salut ? this.parole(n.cle, salut.ligne, salut.langue) : null;
  }
  // what the residents say when touched: a word of their language, then their lines
  lignes(n){
    const salut = typeof salutHabitant === "function" ? salutHabitant(n) : null;
    return salut ? {lignes: [salut.ligne, ...n.dit], langue: salut.langue} : {lignes: n.dit};
  }
}
