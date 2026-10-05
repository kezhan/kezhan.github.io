/* Académia : le moteur Phaser et les passages entre le village, les maisons et les combats ;
   la boîte de dialogue des habitants (portrait, texte lu à voix haute, toucher pour continuer). */
const JEU = new Phaser.Game({
  type: Phaser.AUTO, parent: "jeu", backgroundColor: "#2B1E5C", pixelArt: true,
  scale: {mode: Phaser.Scale.RESIZE, width: innerWidth, height: innerHeight},
  scene: [Chargement, Monde, SceneCombat]
});
// Phaser 3.85 to 3.90 keeps the previous size after a rotation (phaserjs/phaser#7213): apply the parent's size ourselves
const recaler = () => { if (JEU.isBooted) { JEU.scale.getParentBounds(); JEU.scale.refresh(); } };
addEventListener("resize", () => setTimeout(recaler, 50));
if (screen.orientation) screen.orientation.addEventListener("change", () => setTimeout(recaler, 50));

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
  $("gPortrait").src = `assets/portraits/${herosDe(p)}.png`;
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
  $("dlgPortrait").hidden = !portrait; if (portrait) $("dlgPortrait").src = `assets/portraits/${portrait.replace("portrait", "")}.png`;
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
