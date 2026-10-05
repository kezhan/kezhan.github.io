/* Académia : le choix de l'attaque, au tour du compagnon (GDD §4). Jusqu'à 5 ans, un seul gros bouton avec le
   visage du compagnon, et l'étoile du combo qu'on touche quand elle brille ; la voix dit quoi toucher. À partir de
   6 ans, les trois attaques, lues à voix haute quand le panneau monte, le Sablier qui donne dix secondes de plus au
   combo minuté, et la fuite (touchée deux fois). Rend {action: rapide | massive | combo | fuite, sablier}. */
function choisirAttaque(panneau, c, {boss = false, premier = false} = {}){
  return new Promise(fin => {
    const p = P(), A = attaquesDe(c), nom = nomCompagnon(c), pret = (c.charge || 0) >= COMBO_PLEIN;
    panneau.innerHTML = ""; panneau.classList.remove("cache"); panneau.classList.add("choix");
    let choisi = false, sablier = false;
    const choisir = (action, parole) => {   // the voice reading the buttons stops at once
      if (choisi) return; choisi = true;
      sfx.tap(); if (typeof jouerSon === "function") jouerSon("bouton");
      if (parole) dire(parole); else taire();
      fin({action, sablier: action === "combo" && sablier});
    };
    // the combo's three charges, as little lights
    const pips = () => el("span", "pips", [0, 1, 2].map(i => `<i class="${i < (c.charge || 0) ? "plein" : ""}"></i>`).join(""));
    if (p.age <= 5) return petitPanneau();

    const g = el("div", "attaques");
    const bouton = (cls, ico, titre, aide, action) => {
      const b = el("button", "attaque " + cls, `<span class="ico">${ico}</span><span><b></b><small></small></span>`);
      b.querySelector("b").textContent = affiche(titre); b.querySelector("small").textContent = affiche(aide);
      b.onclick = () => { if (!b.disabled) choisir(action, titre); };
      g.append(b); return b;
    };
    markOk(bouton("rapide", "⚡", A[0], "Une question facile", "rapide"));
    bouton("massive", "💥", A[1], boss ? "Le défi du chef !" : "Plus dur, plus d'XP !", "massive");
    const combo = bouton("combo" + (pret ? " prete" : ""), "🌟", A[2], pret ? "Prêt : trois bonnes réponses !" : "Trois bonnes réponses de suite", "combo");
    combo.querySelector("span:last-child").append(el("div", "jauge-combo", `<i style="width:${100 * (c.charge || 0) / COMBO_PLEIN}%"></i>`));
    combo.disabled = !pret;
    if (pret) scintiller(combo);
    panneau.append(g);
    const l = el("div", "jokers");
    if (pret && (p.jokers.sablier || 0) > 0) {   // the Hourglass: ten more seconds for each question of the timed combo
      const s = el("button", "joker sablier"); s.setAttribute("aria-pressed", "false");
      const ecrire = () => { s.textContent = affiche(`⏳ +10 s pour le combo · ${p.jokers.sablier}`); };
      ecrire();
      s.onclick = () => {
        sfx.tap(); sablier = !sablier; s.setAttribute("aria-pressed", String(sablier));
        dire(sablier ? "Sablier : dix secondes de plus pour chaque question du combo ! Touche le combo." : "Pas de sablier.");
      };
      l.append(s);
    }
    const fuite = el("button", "joker", "🏃 S'enfuir");
    fuite.onclick = () => {
      sfx.tap();
      if (fuite.dataset.sur) { choisir("fuite"); return; }
      fuite.dataset.sur = "1"; fuite.textContent = affiche("🏃 Partir ? Touche encore"); dire("Tu veux partir ? Touche encore.");
      setTimeout(() => { delete fuite.dataset.sur; fuite.textContent = "🏃 S'enfuir"; }, 3000);
    };
    l.append(fuite); panneau.append(l);
    // the voice reads the choices as the panel rises (all of it on the first turn, the names after)
    dire(premier ? `${A[0]} : une question facile. ${A[1]} : plus dur.` + (pret ? ` Ou ${A[2]} : le combo est prêt !` : "")
      : `${A[0]}, ou ${A[1]} ?` + (pret ? ` Ou ${A[2]} !` : ""));

    // up to 5 years: the companion's face to touch, and the star of the combo once it shines
    function petitPanneau(){
      const g = el("div", "attaques unique");
      const b = markOk(el("button", "attaque unique"));
      b.setAttribute("aria-label", `${nom} attaque`);
      const nomB = el("b"); nomB.textContent = affiche(nom);
      b.append(spriteCompagnon(c, 96), nomB);
      b.onclick = () => choisir(boss ? "massive" : "rapide");
      const e = el("button", "attaque etoile" + (pret ? " prete" : ""));
      e.setAttribute("aria-label", A[2]);
      e.append(el("img", "", null), pips());
      e.querySelector("img").src = `${DOSSIER_HD}/effets/etoile.png`; e.querySelector("img").alt = "";
      e.disabled = !pret;
      if (pret) scintiller(e);
      e.onclick = () => { if (pret) choisir("combo"); };
      g.append(b, e); panneau.append(g);
      dire(pret ? `L'étoile brille ! Touche l'étoile : ${nom} va lancer ${A[2]} !`
        : premier ? `À toi, ${p.nom} ! Touche ${nom} pour attaquer !` : `Touche ${nom} !`);
    }
  });
}
// three drawn twinkles around a ready combo button
function scintiller(b){
  ["etincelle", "etoile_p", "etincelle"].forEach((k, i) => {
    const im = el("img", "scintille s" + (i + 1)); im.alt = ""; im.src = pieceDom(k); b.append(im);
  });
}
