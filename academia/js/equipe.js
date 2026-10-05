/* Académia : le sac de l'équipe, pour l'enfant. Tous les compagnons en fiches basses (le dessin avec son niveau en
   pastille, le nom, la barre d'XP, une étoile par notion réussie en jeu, la charge du combo, un bouton) ; ceux qu'il
   reste à libérer en silhouette. Le compagnon choisi suit le héros et combat. Au téléphone (ou sur un écran bas), des
   vignettes qui ouvrent la fiche. Toucher un dessin fait dire son nom. Les pourcentages et les réglages sont dans le
   coin Parents (js/parents.js), ouvert par un appui long sur « Parents ». */
const ETOILE_SVG = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 2.4l2.9 5.9 6.5.9-4.7 4.6 1.1 6.5L12 17.2l-5.8 3.1 1.1-6.5-4.7-4.6 6.5-.9z" '
  + 'fill="#FFCF3D" stroke="#1D1640" stroke-width="1.7" stroke-linejoin="round"/><path d="M9.2 8.6l1.5-.2" stroke="#FFF6C9" stroke-width="1.6" stroke-linecap="round"/></svg>';
const sacCompact = () => matchMedia("(max-width: 560px), (max-height: 500px)").matches;

// a notion the child succeeded in play (never one only taken for known when the game was created: js/adaptatif.js,
// placer): gagneeEnJeu() of the engine, or, with an engine that does not have it, mastered with two right answers
const reussieEnJeu = n => {
  if (typeof gagneeEnJeu === "function") return gagneeEnJeu(n);
  const e = P().maitrise[n.id]; return !!e && e.ok >= 2 && maitrisee(n);
};
const notionsReussies = matiere => notionsDe(matiere).filter(reussieEnJeu);

function ouvrirEquipe(){
  const p = P(); if (!p) return;
  const m = JEU.scene.getScene("monde");
  if (ecranCourant === "monde" && (dialogueOuvert() || (m && (m.verrou || m.moment)))) return;
  if (m && m.chemin) { m.chemin = []; m.pnjVise = null; }
  montrer("equipe");
  dessinerEquipe();
}
function dessinerEquipe(){
  const p = P(), l = $("listeEquipe"), compact = sacCompact();
  l.innerHTML = ""; l.hidden = false; $("ficheEquipe").hidden = true;
  l.classList.toggle("vignettes", compact);
  const rang = f => { const c = p.compagnons.find(x => x.famille === f); return c ? (c.id === p.actif ? 0 : 1) : 2; };
  Object.keys(FAMILLES).sort((a, b) => rang(a) - rang(b)).forEach(f => l.append(compact ? vignetteCompagnon(f) : ficheCompagnon(f)));
}
const compagnonDe = f => P().compagnons.find(x => x.famille === f) || null;
const A_REGION = {dojo: "au ", albion: "au ", germania: "à la ", duche: "à la ", jardin: "au ", observatoire: "à l'"};
// what the voice says about a companion whose drawing is touched
function direCompagnon(f){
  const c = compagnonDe(f), r = REGIONS.find(x => x.famille === f);
  if (!c) return dire(`Un compagnon à libérer, ${A_REGION[r.id] || "à "}${r.nom} !`);
  const st = stade(c);
  dire(`${nomCompagnon(c)}, niveau ${c.niveau} !` + (st < 3 ? ` Il évolue au niveau ${PALIERS[st]}.` : " C'est sa forme ultime !"));
}
// a companion's card: drawing and level, name, XP, stars, combo charge, one button
function ficheCompagnon(f){
  const p = P(), c = compagnonDe(f), r = REGIONS.find(x => x.famille === f);
  const d = el("div", "fiche" + (c ? "" : " inconnue") + (c && c.id === p.actif ? " active" : ""));
  const dessin = el("div", "fiche-dessin"), fig = c ? spriteCompagnon(c, 92) : imageHD(f + "1", "face", 84);
  dessin.append(fig);
  if (c) dessin.append(el("span", "niv", `Niv. ${c.niveau}`));
  dessin.onclick = () => { sauterDessin(fig); direCompagnon(f); };
  const corps = el("div", "fiche-corps", "<b></b>");
  corps.querySelector("b").textContent = c ? affiche(nomCompagnon(c)) : "???";
  if (!c) corps.append(ecrireIcones(el("small"), `À libérer : ${r.icone} ${r.nom}`));   // the house's drawn symbol (js/icones.js)
  else {
    corps.append(el("div", "barrexp petite", `<i style="width:${Math.min(100, 100 * c.xp / xpPour(c.niveau))}%"></i>`), etoilesFiche(c));
    if (c.id !== p.actif) { const b = ecrireIcones(el("button", "moyen"), "⚔️ Avec moi !"); b.onclick = () => choisirCompagnon(c); corps.append(b); }
    else corps.append(ecrireIcones(el("span", "suit"), "🐾 Te suit partout"));
  }
  d.append(dessin, corps);
  return d;
}
// one star per notion succeeded in play (five drawn, then « +n »), and the combo's charge when there is one (c.charge,
// kept on the companion from one fight to the next by js/combat.js)
function etoilesFiche(c){
  const n = notionsReussies(FAMILLES[c.famille].matiere).length, l = el("div", "etoiles");
  l.title = `${n} notion${n > 1 ? "s" : ""} réussie${n > 1 ? "s" : ""} en jeu`;
  if (!n) l.innerHTML = `<span class="vide">${ETOILE_SVG}</span>`;
  else { l.innerHTML = ETOILE_SVG.repeat(Math.min(5, n)); if (n > 5) l.append(`+${n - 5}`); }
  const plein = typeof COMBO_PLEIN === "number" ? COMBO_PLEIN : 3, charge = Math.max(0, Math.min(plein, c.charge || 0));
  if (charge) {
    const k = el("span", "charge" + (charge >= plein ? " complete" : ""), "<i></i>".repeat(plein));
    [...k.children].forEach((x, i) => x.classList.toggle("remplie", i < charge));
    k.title = charge >= plein ? `${FAMILLES[c.famille].attaques[2]} est prêt !` : "Combo en charge";
    l.append(k);
  }
  return l;
}
// a phone or a low screen: a thumbnail, which opens the companion's card
function vignetteCompagnon(f){
  const p = P(), c = compagnonDe(f);
  const b = el("button", "vignette" + (c ? "" : " inconnue") + (c && c.id === p.actif ? " active" : ""), "<b></b>");
  const fig = c ? spriteCompagnon(c, 76) : imageHD(f + "1", "face", 70);
  b.prepend(fig); b.querySelector("b").textContent = c ? nomCompagnon(c) : "???";
  if (c) b.append(el("span", "niv", String(c.niveau)));
  b.onclick = () => { sfx.tap(); direCompagnon(f); if (c) ouvrirFiche(f); else sauterDessin(fig); };
  return b;
}
function ouvrirFiche(f){
  const l = $("listeEquipe"), v = $("ficheEquipe");
  l.hidden = true; v.hidden = false; v.innerHTML = "";
  const retour = ecrireIcones(el("button", "moyen gris retour"), "⬅ Équipe");
  retour.onclick = () => { sfx.tap(); dessinerEquipe(); };
  v.append(ficheCompagnon(f), retour);
}
function choisirCompagnon(c){
  const p = P(); p.actif = c.id; sauver(); sfx.juste(); dire(`${nomCompagnon(c)}, à toi !`);
  dessinerEquipe();
  const m = JEU.scene.getScene("monde"); if (m && m.majCompagnon) m.majCompagnon();
}
function fermerEquipe(){ window.__calme = performance.now() + 250; montrer("monde"); }
