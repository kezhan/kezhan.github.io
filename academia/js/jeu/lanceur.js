/* Académia : le moteur Phaser et les passages entre le village, les maisons et les combats ;
   la boîte de dialogue des habitants (portrait, texte qui s'écrit, toucher pour continuer). */
const JEU = new Phaser.Game({
  type: Phaser.AUTO, parent: "jeu", backgroundColor: "#2B1E5C", pixelArt: true,
  scale: {mode: Phaser.Scale.RESIZE, width: innerWidth, height: innerHeight},
  scene: [Chargement, Monde, SceneCombat]
});

function entrerMonde(){
  montrer("monde");
  const m = JEU.scene.getScene("monde");
  if (JEU.scene.isActive("monde") || JEU.scene.isSleeping("monde")) JEU.scene.stop("monde");
  JEU.scene.start("monde");
  majBarre();
}
function commencerCombat(regionId, boss){
  window.__ui = true;
  JEU.scene.sleep("monde");
  lancerCombat(regionId, boss);
}
function retourMonde(){
  window.__ui = false;
  if (JEU.scene.isActive("combat")) JEU.scene.stop("combat");
  montrer("monde"); majBarre();
  if (JEU.scene.isSleeping("monde")) JEU.scene.wake("monde");
  const m = JEU.scene.getScene("monde"); if (m && m.majCompagnon) m.majCompagnon();
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
    `Bats encore ${reste} Ombre${reste > 1 ? "s" : ""} dans les hautes herbes d'à côté, et la porte s'ouvrira !`]);
  dialogue(r.nom, null, [`${r.icone} La porte s'ouvre… ${r.boss} t'attend à l'intérieur !`], [
    {texte: "⚔️ Entrer", action: () => commencerCombat(regionId, true)}, {texte: "Pas maintenant", action: () => {}}]);
}

// dialogue: lines shown one after the other, read aloud; optional choice buttons at the end
function dialogue(nom, portrait, lignes, choix){
  const d = $("dialogue"), t = $("dlgTexte"), b = $("dlgChoix");
  window.__ui = true; d.hidden = false; $("dlgNom").textContent = nom;
  $("dlgPortrait").hidden = !portrait; if (portrait) $("dlgPortrait").src = `assets/portraits/${portrait.replace("portrait", "")}.png`;
  let i = 0;
  const fermer = apres => { d.hidden = true; d.onclick = null; taire(); setTimeout(() => { window.__ui = false; if (apres) apres(); }, 50); };
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
