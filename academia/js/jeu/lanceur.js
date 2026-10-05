/* Académia : le moteur Phaser et les passages entre le village, les maisons et les combats ;
   la boîte de dialogue des habitants (portrait, texte lu à voix haute, toucher pour continuer). */
// the canvas has RATIO pixels per CSS pixel (js/jeu/qualite.js) and is shown at the window's size: sharp drawings on a tablet
const tailleToile = () => [Math.round(innerWidth * RATIO), Math.round(innerHeight * RATIO)];
const JEU = new Phaser.Game({
  type: Phaser.AUTO, parent: "jeu", backgroundColor: "#2B1E5C",
  scale: {mode: Phaser.Scale.NONE, width: tailleToile()[0], height: tailleToile()[1], zoom: 1 / RATIO},
  scene: [Chargement, Monde, SceneCombat]
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
  $("dialogue").hidden = true;
  montrer("monde");
  if (JEU.scene.isActive("monde") || JEU.scene.isSleeping("monde")) JEU.scene.stop("monde");
  if (apres) JEU.scene.getScene("monde").events.once("create", apres);
  JEU.scene.start("monde");
  majBarre();
}
function commencerCombat(regionId, boss){
  JEU.scene.sleep("monde");
  lancerCombat(regionId, boss);
}
// back from a fight; `resultat` ({gagne}) tells the village whether the Ombre that was touched is beaten
function retourMonde(resultat){
  if (JEU.scene.isActive("combat")) JEU.scene.stop("combat");
  montrer("monde"); majBarre();
  const m = JEU.scene.getScene("monde");
  if (JEU.scene.isSleeping("monde")) JEU.scene.wake("monde");
  if (m && m.apresCombat) m.apresCombat(resultat || {gagne: false});
}
function majBarre(){
  const p = P(); if (!p) return;
  $("gNom").textContent = p.nom; $("gEtincelles").textContent = p.etincelles;
  $("gPortrait").src = `${DOSSIER_HD}/portraits/${herosHD(p)}.png`;
}

// a house: its boss waits once four Ombres of the tall grass nearby are beaten
function entrerMaison(regionId){
  const r = regionDe(regionId), st = regionEtat(regionId);
  const reste = ETAPES - st.etape;
  if (reste > 0) return dialogue(r.nom, null, [`${r.icone} ${r.nom}. La porte est fermée par une Ombre.`,
    `Touche encore ${reste} Ombre${reste > 1 ? "s" : ""} violette${reste > 1 ? "s" : ""} dans les hautes herbes ${r.herbes}, et la porte s'ouvrira !`]);
  dialogue(r.nom, null, [`${r.icone} La porte s'ouvre… ${r.boss} t'attend à l'intérieur !`], [
    {texte: "⚔️ Entrer", action: () => commencerCombat(regionId, true)}, {texte: "↩️ Pas maintenant", action: () => {}}]);
}

// dialogue: lines shown one after the other, read aloud; optional choice buttons at the end.
// While it is open, the village does not move (js/jeu/monde.js, occupe).
function dialogue(nom, portrait, lignes, choix){
  const d = $("dialogue"), t = $("dlgTexte"), b = $("dlgChoix");
  d.hidden = false; $("dlgNom").textContent = nom;
  const img = $("dlgPortrait"); img.hidden = !portrait;
  if (portrait) img.src = `${DOSSIER_HD}/portraits/${portrait}.png`;   // a villager's key
  let i = 0;
  const fermer = apres => { d.hidden = true; d.onclick = null; taire(); window.__calme = performance.now() + 250; if (apres) apres(); };
  const afficher = () => {
    t.textContent = lignes[i]; dire(lignes[i]); b.innerHTML = "";
    const derniere = i === lignes.length - 1;
    if (derniere && choix) choix.forEach((c, k) => {
      const x = el("button", k ? "moyen gris" : "moyen", c.texte);
      if (!k) markOk(x);
      x.onclick = e => { e.stopPropagation(); sfx.tap(); fermer(c.action); };
      b.append(x);
    });
    else { const suite = markOk(el("button", "suite", derniere ? "✔" : "▼")); b.append(suite); }
  };
  d.onclick = () => { if (choix && i === lignes.length - 1) return; sfx.tap(); i++; if (i < lignes.length) afficher(); else fermer(); };
  afficher();
}
