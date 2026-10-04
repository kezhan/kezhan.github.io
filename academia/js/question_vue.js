/* Académia : pose une question dans le panneau et attend la réponse de l'enfant.
   Gros boutons, voix (bouton 🔊 pour réécouter), jokers Potion de clarté et Indice du sage, minuteur du combo.
   Une erreur montre la bonne réponse et l'explication, lue à voix haute, avant de continuer. */
function poserQuestion(panneau, q, {titre = "", limite = 0, jokers = true} = {}){
  return new Promise(fin => {
    const vf = estVraiFaux(q), lang = q.langue || "fr", t0 = Date.now();
    window.__q = q;   // the question on screen, read by the recette
    let fini = false, chrono = null;
    panneau.innerHTML = ""; panneau.classList.remove("cache");
    const boite = el("div", "question" + (vf ? " vraifaux" : ""));
    const tete = el("div", "q-tete"), titreP = el("p", "titre-bouclier"); titreP.textContent = titre;
    tete.append(titreP); boite.append(tete);
    if (limite) {
      const m = el("div", "minuteur", "<i></i>"); boite.append(m);
      if (!TEST) m.firstChild.animate([{transform: "scaleX(1)"}, {transform: "scaleX(0)"}], {duration: limite, fill: "forwards"});
      chrono = setTimeout(() => repondre(null), TEST ? 600000 : limite);
    }
    const enonce = el("p", "enonce"); enonce.textContent = q.question; boite.append(enonce);
    if (q.visuel) {   // many pictures get smaller, so that the answers stay in sight
      const v = el("div", "visuel"); v.textContent = q.visuel;
      const n = typeof Intl.Segmenter === "function" ? [...new Intl.Segmenter("fr", {granularity: "grapheme"}).segment(q.visuel)].length : q.visuel.length;
      if (n > 24) v.classList.add("tres-long"); else if (n > 12) v.classList.add("long");
      boite.append(v);
    }
    const ecoute = () => dire(q.dire || q.question, lang);
    const outils = el("div", "outils");
    const outil = (ico, nom) => { const b = el("button", "outil"); b.textContent = ico; b.title = nom; b.setAttribute("aria-label", nom); return b; };
    const b1 = outil("🔊", "Écouter"); b1.onclick = () => { sfx.tap(); ecoute(); }; outils.append(b1);
    const p = P();
    const boutons = boutonsDe(q);
    if (jokers && !vf && boutons.length >= 3) {
      const potion = outil(`🧪 ${p.jokers.potion}`, "Potion de clarté");
      potion.disabled = !p.jokers.potion;
      potion.onclick = () => {   // removes two wrong answers (GDD §4)
        p.jokers.potion--; sauver(); potion.disabled = true; sfx.juste(); dire("Potion de clarté !");
        [...grille.children].filter(b => b.dataset.v !== q.reponse).slice(0, 2).forEach(b => b.classList.add("retiree"));
      };
      const indice = outil(`🦉 ${p.jokers.indice}`, "Indice du sage");
      indice.disabled = !p.jokers.indice || !(q.indice || q.explication);
      indice.onclick = () => {
        p.jokers.indice--; sauver(); indice.disabled = true;
        const t = el("div", "explication"); t.textContent = "🦉 " + (q.indice || q.explication); boite.append(t); dire(t.textContent);
      };
      outils.append(potion, indice);
    }
    tete.append(outils);
    const grille = el("div", "reponses");
    boutons.forEach(v => {
      const b = el("button", "reponse");
      b.dataset.v = v;
      b.textContent = vf ? (v === "vrai" ? "✅ Vrai" : "❌ Faux") : v;
      if (v === q.reponse) markOk(b);
      b.onclick = () => repondre(b);
      grille.append(b);
    });
    boite.append(grille);
    panneau.append(boite);
    ecoute();

    function repondre(b){
      if (fini) return; fini = true; clearTimeout(chrono);
      const juste = !!b && b.dataset.v === q.reponse, ms = Date.now() - t0;
      [...grille.children].forEach(x => { x.disabled = true; if (x.dataset.v === q.reponse) x.classList.add("juste"); });
      if (juste) { sfx.juste(); etincelles(...centre(b), 10); return setTimeout(() => fin({juste, ms}), TEST ? 0 : 650); }
      sfx.faux(); if (b) b.classList.add("faux");
      // the companion explains (GDD §8.1), then the child taps on to continue
      const t = el("div", "explication");
      const bonne = vf ? (q.reponse === "vrai" ? "C'est vrai !" : "C'est faux !") : `La bonne réponse : ${q.reponse}`;
      t.textContent = `${b ? "" : "⏳ Trop tard ! "}${bonne}${q.explication ? " " + q.explication : ""}`;
      const suite = markOk(el("button", "moyen vert", "J'ai compris 👍")); suite.style.marginTop = "10px";
      suite.onclick = () => { sfx.tap(); taire(); fin({juste: false, ms}); };
      boite.append(t, suite);
      dire(t.textContent);
    }
  });
}
