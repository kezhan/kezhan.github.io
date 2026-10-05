/* Académia : l'équipe. Tous les compagnons, leur niveau, leur XP, leur stade ; ceux qu'il reste à libérer
   en silhouette. Le compagnon choisi suit le héros et combat. Les barres « Savoir » et « Précision » sont
   en vérité la réussite de l'enfant dans la matière (bulletin masqué, GDD §9). */
function ouvrirEquipe(){
  const p = P(); if (!p) return;
  const m = JEU.scene.getScene("monde");
  if (ecranCourant === "monde" && (!$("dialogue").hidden || (m && m.verrou))) return;
  if (m && m.chemin) { m.chemin = []; m.pnjVise = null; }
  montrer("equipe");
  const l = $("listeEquipe"); l.innerHTML = "";
  const rang = f => { const c = p.compagnons.find(x => x.famille === f); return c ? (c.id === p.actif ? 0 : 1) : 2; };
  Object.entries(FAMILLES).sort(([a], [b]) => rang(a) - rang(b)).forEach(([f, F]) => {
    const c = p.compagnons.find(x => x.famille === f), r = REGIONS.find(x => x.famille === f);
    const fiche = el("div", "fiche" + (c ? "" : " inconnue") + (c && c.id === p.actif ? " active" : ""));
    fiche.append(c ? spriteCompagnon(c, 92) : imageHD(f + "1", "face", 84));
    const corps = el("div", "fiche-corps", `<b></b><small></small>`);
    corps.querySelector("b").textContent = c ? nomCompagnon(c) : "???";
    corps.querySelector("small").textContent = c ? `Niveau ${c.niveau} · ${r.icone} ${F.matiere}` : `À libérer : ${r.icone} ${r.nom}`;
    if (c) {
      const xp = el("div", "barrexp petite", `<i style="width:${Math.min(100, 100 * c.xp / xpPour(c.niveau))}%"></i>`);
      corps.append(xp);
      const pct = Math.round(100 * progresMatiere(F.matiere));
      const h = p.hist.filter(x => regionDe(x.region).matiere === F.matiere);
      const bonnes = h.reduce((n, x) => n + x.bonnes, 0), total = h.reduce((n, x) => n + x.total, 0);
      [["Savoir", pct], ["Précision", total ? Math.round(100 * bonnes / total) : 0]].forEach(([k, v]) =>
        corps.append(el("div", "stat", `<span>${k}</span><i style="--v:${v}%"></i><span>${v}%</span>`)));
      const st = stade(c);
      corps.append(el("small", "", st < 3 ? `Évolue au niveau ${PALIERS[st]}` : "Forme ultime ✨"));
      if (c.id !== p.actif) {
        const b = el("button", "moyen", "⚔️ Avec moi !");
        b.onclick = () => { p.actif = c.id; sauver(); sfx.juste(); dire(`${nomCompagnon(c)}, à toi !`); ouvrirEquipe(); const m = JEU.scene.getScene("monde"); if (m && m.majCompagnon) m.majCompagnon(); };
        corps.append(b);
      } else corps.append(el("small", "", "🐾 Te suit partout"));
    }
    fiche.append(corps); l.append(fiche);
  });
  majDiagnostic();
}
// for the grown-ups: how the pictures are drawn, and any that could not be loaded
function majDiagnostic(){
  const r = JEU.renderer, moteur = r && r.type === Phaser.WEBGL ? `WebGL ${TEXTURE_MAX}` : "sans carte graphique";
  const manque = [...ECHECS];
  $("diagImages").textContent = `Images ${QUALITE === "hd" ? "haute définition" : "légères"} · ${moteur} · `
    + (manque.length ? `dessins non chargés : ${manque.join(", ")} (réseau ?)` : "tous les dessins sont chargés");
}
function fermerEquipe(){ window.__calme = performance.now() + 250; montrer("monde"); }
