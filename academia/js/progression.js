/* Académia : ce que rapporte un combat (XP, étincelles, jokers), les niveaux, les évolutions aux paliers,
   et le compagnon libéré quand le boss d'une région tombe. Écran de fin, qui fête chaque progrès. */
function gagnerXP(c, xp){
  const avant = {niveau: c.niveau, stade: stade(c)};
  c.xp += xp;
  while (c.xp >= xpPour(c.niveau)) { c.xp -= xpPour(c.niveau); c.niveau++; }
  return {niveaux: c.niveau - avant.niveau, evolue: stade(c) > avant.stade, stadeAvant: avant.stade};
}

async function terminerCombat(b){
  const p = P(), c = actif(), region = regionDe(b.region), st = regionEtat(b.region);
  let libere = null, badge = null, joker = null;
  if (b.gagne) {
    b.xp += 10 + 4 * st.niveau + (b.boss ? 20 : 0);
    p.etincelles += 2 * b.bonnes + (b.boss ? 20 : 5);
    if (b.boss) {
      st.badges++; st.niveau++; st.etape = 0; badge = region.badge;
      libere = ajouterCompagnon(region.famille, Math.max(1, c.niveau - 2));   // the region's companion joins the team
      if (!libere) { p.jokers.potion++; p.jokers.indice++; joker = "cadeau"; }   // already in the team: presents instead
    } else st.etape++;
    if (Math.random() < .45) { joker = pick(["potion", "indice"], 1)[0]; p.jokers[joker]++; }
  } else b.xp = Math.round(b.xp * .6);
  const xpAvant = {niveau: c.niveau, xp: c.xp};
  const gain = gagnerXP(c, b.xp);
  noterCombat({region: b.region, boss: b.boss, gagne: b.gagne, xp: b.xp, bonnes: b.bonnes, total: b.total, revanches: b.revanches,
    compagnon: c.famille, niveau: c.niveau});
  sauver();
  await ecranFin(b, c, region, gain, xpAvant, {libere, badge, joker});
}

async function ecranFin(b, c, region, gain, xpAvant, {libere, badge, joker}){
  montrer("fin");
  const carte = $("finCarte"); carte.innerHTML = "";
  const titre = b.gagne ? (b.boss ? "Victoire contre le boss !" : "Victoire !") : "Ton compagnon est fatigué";
  const fig = el("div", "scene-fin");
  const dessin = () => { fig.innerHTML = ""; fig.append(spriteCompagnon(c, 120)); };
  carte.append(el("div", "grand", b.gagne ? (b.boss ? "🏆" : "🎉") : "💤"), el("h2"), fig);
  carte.querySelector("h2").textContent = titre;
  if (gain.evolue) { fig.append(spriteDOM("monstre", spriteDe(c), 120, {echelle: echelleStade(gain.stadeAvant) / 1.3})); } else dessin();
  const figure = {anim: nom => { if (calme()) return Promise.resolve();
    const k = {joie: [[{transform: "translateY(0)"}, {transform: "translateY(-30px)"}, {transform: "translateY(0)"}], 600],
               evolution: [[{transform: "scale(1)", filter: "brightness(1)"}, {transform: "scale(1.4) rotate(720deg)", filter: "brightness(4)"}, {transform: "scale(1)", filter: "brightness(1)"}], 2200]}[nom];
    return fig.animate(k[0], {duration: k[1], easing: "ease-in-out"}).finished.catch(() => {}); }, changer: dessin};
  const lignes = el("div");
  const ligne = t => { const x = el("p"); x.textContent = t; lignes.append(x); return x; };
  ligne(`${b.bonnes} bonne${b.bonnes > 1 ? "s" : ""} réponse${b.bonnes > 1 ? "s" : ""} sur ${b.total} · +${b.xp} XP`);
  if (b.revanches) ligne(`🔁 ${b.revanches} revanche${b.revanches > 1 ? "s" : ""} gagnée${b.revanches > 1 ? "s" : ""} : XP triplée !`);
  if (!b.gagne) ligne("Ce n'est pas grave : il se repose et revient plus fort. Tu as gagné de l'expérience !");
  if (joker) ligne({potion: "🧪 Tu trouves une Potion de clarté !", indice: "🦉 Tu trouves un Indice du sage !", cadeau: "🎁 Le village te remercie : une Potion de clarté et un Indice du sage !"}[joker]);
  const barre = el("div", "barrexp", "<i></i>");
  carte.append(lignes, barre);
  dire(titre + ". " + lignes.textContent);
  if (b.gagne) { etincelles(innerWidth / 2, 160, 24); figure.anim("joie"); }
  await wait(250);
  barre.firstChild.style.width = Math.min(100, 100 * c.xp / xpPour(c.niveau)) + "%";

  if (gain.niveaux) {
    sfx.niveau(); await wait(900);
    const t = `⬆️ ${nomCompagnon(c)} passe au niveau ${c.niveau} !`; ligne(t); dire(t); await wait(1600);
  }
  if (gain.evolue) {
    const avant = FAMILLES[c.famille].noms[gain.stadeAvant - 1];
    const t = `Oh ? ${avant} évolue…`; ligne(t); await dire(t);
    sfx.evolution();
    const anim = figure.anim("evolution");
    await wait(1300); figure.changer(); await anim;
    etincelles(...centre(fig), 30);
    const t2 = `${avant} est devenu ${nomCompagnon(c)} !`; ligne(t2); await dire(t2);
  }
  if (badge) { const t = `🏅 Tu gagnes le ${badge} !`; ligne(t); await dire(t); }
  if (libere) {
    const box = el("div", "carte"); box.style.marginTop = "12px";
    const f2 = el("div", "scene-fin");
    const nom = FAMILLES[libere.famille].noms[0];
    box.append(el("p", "", `<b>✨ Tu as libéré ${nom} !</b><br>${FAMILLES[libere.famille].desc} Il rejoint ton équipe.`), f2);
    carte.append(box);
    f2.append(spriteCompagnon(libere, 110)); etincelles(...centre(f2), 24);
    await dire(`Tu as libéré ${nom} ! Il rejoint ton équipe.`);
  }
  const ligneBtn = el("div", "ligne");
  const suite = markOk(el("button", "gros", "Continuer ➡"));
  suite.onclick = () => { sfx.tap(); taire(); retourMonde(); };
  ligneBtn.append(suite); carte.append(ligneBtn);
}
