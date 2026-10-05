/* Académia : le moteur Phaser et les passages entre l'accueil, le village, les maisons et les combats.
   La boîte de dialogue est dans js/dialogue.js. */
// the canvas has RATIO pixels per CSS pixel (js/jeu/qualite.js) and is shown at the window's size: sharp drawings on a tablet
const tailleToile = () => [Math.round(innerWidth * RATIO), Math.round(innerHeight * RATIO)];
// a picture that never came is drawn as nothing, never as the engine's black square with a green cross
const TRANSPARENT = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAC0lEQVR4nGNgQAcAABIAAXfx+gAAAAAASUVORK5CYII=";
const JEU = new Phaser.Game({
  type: Phaser.AUTO, parent: "jeu", backgroundColor: "#2B1E5C", images: {missing: TRANSPARENT},
  audio: {noAudio: true},   // the game's voice and sounds do not go through Phaser (no idle audio context)
  scale: {mode: Phaser.Scale.NONE, width: tailleToile()[0], height: tailleToile()[1], zoom: 1 / RATIO},
  scene: [Chargement, Monde, SceneCombat, Vitrine]
});
// the canvas follows the window and a turned tablet (each scene redraws itself on the resize event)
let minuteurToile = null;
const recaler = () => {
  if (!JEU.isBooted) return;
  const [w, h] = tailleToile();
  if (w !== JEU.scale.width || h !== JEU.scale.height) { JEU.scale.resize(w, h); JEU.scale.setZoom(1 / RATIO); }
};
addEventListener("resize", () => { clearTimeout(minuteurToile); minuteurToile = setTimeout(recaler, 80); });
if (screen.orientation) screen.orientation.addEventListener("change", () => setTimeout(recaler, 150));

// the village, whatever the way in (new Guardian, a profile touched, a known first name): it always answers the child
function entrerMonde(apres){
  if (!window.__jeuPret) {   // slow network: the images are not loaded yet, the village waits for them
    if (!entrerMonde.attente) {
      entrerMonde.attente = true; toast("⏳ Le village se prépare…", 60000);
      document.addEventListener("jeu-pret", () => { entrerMonde.attente = false; $("toast").hidden = true; entrerMonde(apres); }, {once: true});
    }
    return;
  }
  fermerDialogue();
  montrer("monde");
  if (JEU.scene.isActive("vitrine")) JEU.scene.stop("vitrine");
  if (JEU.scene.isActive("monde") || JEU.scene.isSleeping("monde")) JEU.scene.stop("monde");
  if (apres) JEU.scene.getScene("monde").events.once("create", apres);
  JEU.scene.start("monde");
  majBarre();
}
function commencerCombat(regionId, boss){
  JEU.scene.sleep("monde");
  lancerCombat(regionId, boss);
}
// through the open door to the boss: the village holds still and the screen closes in a star on the door
// (js/transition.js, when the fights have it)
function entrerChezLeChef(regionId){
  const m = JEU.scene.getScene("monde"), p = m && m.portes && m.portes.portes[regionId];
  if (typeof fermerEtoile !== "function" || !p || !m.sys.isActive()) return commencerCombat(regionId, true);
  m.verrou = true; m.chemin = [];
  const [x, y] = m.ecranDe(p.x, p.y);
  fermerEtoile(x, y).then(() => commencerCombat(regionId, true));
}
// back from a fight; `resultat` ({gagne}) tells the village whether the Ombre that was touched is beaten
function retourMonde(resultat){
  if (JEU.scene.isActive("combat")) JEU.scene.stop("combat");
  montrer("monde"); majBarre();
  const m = JEU.scene.getScene("monde");
  if (JEU.scene.isSleeping("monde")) JEU.scene.wake("monde");
  if (m && m.apresCombat) m.apresCombat(resultat || {gagne: false});
}
// a face in the interface: asked again twice (slow network) under a new address
function imageReessayee(img, url){
  let n = 0;
  img.onerror = () => { if (++n < ESSAIS_MAX) img.src = `${url}?essai=${n}`; else { img.onerror = null; ECHECS.add(url.split("/").pop()); } };
  img.src = url;
}
function majBarre(){
  const p = P(); if (!p) return;
  $("gNom").textContent = p.nom; $("gEtincelles").textContent = p.etincelles;
  imageReessayee($("gPortrait"), `${DOSSIER_HD}/portraits/${herosHD(p)}.png`);
}

// a house: its boss waits once four Ombres of the tall grass nearby are beaten. The buttons are said aloud; up to
// 5 years old, one big button with the swords, named by the voice; walking away (a touch on the village) is « not now »
function entrerMaison(regionId){
  const r = regionDe(regionId), st = regionEtat(regionId), reste = ETAPES - st.etape;
  if (reste > 0) return dialogue(r.nom, CLES_OMBRES["Néantik"], [`${r.icone} ${r.nom}. Une Ombre garde la porte fermée.`,
    `Touche encore ${reste} Ombre${reste > 1 ? "s" : ""} violette${reste > 1 ? "s" : ""} dans les hautes herbes ${r.herbes}, et la porte s'ouvrira !`]);
  if (typeof jouerSon === "function") jouerSon("porte");
  const entrer = () => entrerChezLeChef(regionId), boss = CLES_BOSS[regionId];
  if (P().age <= 5) return dialogue(r.nom, boss, [`La porte est ouverte : le ${r.boss} t'attend !`],
    [{texte: "⚔️", classe: "epees", aide: "Entrer", dit: "Touche les épées pour entrer !", action: entrer}], {quitter: true});
  dialogue(r.nom, boss, [`${r.icone} La porte s'ouvre… Le ${r.boss} t'attend à l'intérieur !`], [
    {texte: "⚔️ Entrer", dit: "Touche Entrer pour le combattre,", action: entrer},
    {texte: "↩️ Pas maintenant", dit: "ou Pas maintenant pour rester dehors.", action: () => {}}], {quitter: true});
}
