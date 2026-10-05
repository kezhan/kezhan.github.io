/* Académia : les grands moments en plein écran (évolution, badge, compagnon libéré), par-dessus tout le jeu.
   Une scène est une suite d'étapes minutées ; un toucher l'accélère (trois fois plus vite) sans jamais en sauter
   une, et ses boutons n'arrivent qu'à la fin : aucun toucher ne fait rater le moment. Fond nuit doux, rayons qui
   tournent lentement ; la voix dit ce qui se passe. En recette et pour qui préfère moins de mouvement (calme()),
   les étapes passent tout de suite et seuls restent les boutons. */
function ouvrirMoment(theme){
  let m = $("moment");
  if (!m) { m = el("section", "moment"); m.id = "moment"; document.body.append(m); }
  m.className = "moment " + theme; m.innerHTML = ""; m.hidden = false;
  window.__moment = theme;   // read by the recette
  const etat = {vitesse: 1, fini: false};
  const accelerer = e => {
    if (e.target.closest("button") || etat.vitesse > 1) return;
    etat.vitesse = 3;
    try {   // an older browser without getAnimations: the timed steps still go faster
      m.getAnimations({subtree: true}).forEach(a => {
        const t = a.effect && a.effect.getComputedTiming ? a.effect.getComputedTiming() : null;
        if (t && t.iterations !== Infinity) a.playbackRate = 3;
      });
    } catch (e) {}
  };
  m.onpointerdown = accelerer;
  const M = {
    m, etat,
    q: s => m.querySelector(s),
    // a pause that a touch shortens, never skips
    attendre: async ms => {
      if (calme()) return;
      let reste = ms, t = performance.now();
      while (reste > 0 && !etat.fini) {
        await new Promise(r => setTimeout(r, Math.max(10, Math.min(40, reste / etat.vitesse))));
        const now = performance.now(); reste -= (now - t) * etat.vitesse; t = now;
      }
    },
    // a Web Animation that keeps its end state, faster once the child has touched
    animer: (n, images, o = {}) => {
      const a = n.animate(images, {fill: "both", ...o, duration: calme() ? 0 : o.duration});
      if (etat.vitesse > 1 && o.iterations !== Infinity) a.playbackRate = etat.vitesse;
      // its end state is written on the element, so that a CSS animation can take over (the bubble that trembles)
      return a.finished.then(() => { try { a.commitStyles(); a.cancel(); } catch (e) {} }).catch(() => {});
    },
    // the big buttons at the end; the promise gives the value of the one touched
    boutons: liste => new Promise(fin => {
      const l = M.q(".boutons-moment") || m.appendChild(el("div", "boutons-moment"));
      l.innerHTML = ""; delete l.dataset.choisi;
      liste.forEach((b, i) => {
        const x = el("button", "gros " + (b.cls || ""), "");
        x.textContent = affiche(b.texte);
        if (!i) markOk(x);
        x.onclick = () => {   // one choice: the buttons then rest while the scene ends
          if (etat.fini || l.dataset.choisi) return; l.dataset.choisi = "1";
          [...l.children].forEach(y => { y.disabled = y !== x; });
          sfx.tap(); if (typeof jouerSon === "function") jouerSon("bouton"); fin(b.valeur);
        };
        l.append(x);
      });
      l.classList.add("vu");
    }),
    fermer: () => {
      etat.fini = true; m.onpointerdown = null;
      try { m.getAnimations({subtree: true}).forEach(a => a.cancel()); } catch (e) {}
      m.hidden = true; m.innerHTML = ""; m.className = "moment"; window.__moment = null;
    }
  };
  return M;
}
// a moment's sky: soft night and slowly turning rays of its colour
const fondMoment = () => `<div class="rayons-moment"></div><div class="halo-moment"></div>`;
// the drawings of the coming moments are asked for at once: on a slow network they are there when their scene comes
function prechargerMoments(cles, pieces = []){
  cles.forEach(k => { const t = typeAtlas(k); atlasJSON(k, t); const i = new Image(); i.src = `${DOSSIER_HD}/${t}/${k}.png`; });
  pieces.forEach(n => { const i = new Image(); i.src = pieceDom(n); });
}
