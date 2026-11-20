/* Académia : les formes d'exercice qui ne sont pas des boutons à choisir. js/question_vue.js les appelle quand la
   question porte une forme connue (q.forme) :
   1. « ordre » : q.suite, des tuiles mélangées à toucher dans l'ordre ; chacune va se ranger dans sa case numérotée.
   2. « paires » : q.paires, deux colonnes ; on touche une tuile de gauche, puis celle de droite qui va avec.
   3. « tri » : q.paniers, les objets arrivent un par un ; on touche le panier où chacun va.
   Une erreur arrête l'exercice : la solution est montrée, expliquée et dite, puis « J'ai compris ». Tout réussi : les
   étincelles, comme pour une bonne réponse. Une tuile touchée dit son nom (la petite ne lit pas).
   L'élément à toucher ensuite porte data-suite, les mauvais data-piege : la recette joue par de vrais gestes. */
const FORMES_Q = {};
const estForme = q => !!q && !!q.forme && !!FORMES_Q[q.forme];

function poserForme(panneau, q, {titre = ""} = {}){
  return new Promise(fin => {
    const t0 = Date.now(), p = P(), petit = !!p && p.age <= 5;
    window.__q = q; window.__qn = (window.__qn || 0) + 1;   // read by the recette, as for a question with buttons
    let fini = false;
    panneau.innerHTML = ""; panneau.classList.remove("cache"); panneau.scrollTop = 0;
    const boite = el("div", "question exercice exercice-" + q.forme);
    const haut = el("div", "q-haut"), tete = el("div", "q-tete"), titreP = el("p", "titre-bouclier");
    ecrireIcones(titreP, titre); tete.append(titreP);
    const ligne = el("div", "q-enonce"), ecoute = el("button", "ecouter"), enonce = el("p", "enonce");
    ecoute.append(iconeDom("haut_parleur")); ecoute.setAttribute("aria-label", "Écouter"); ecoute.title = "Écouter";
    enonce.textContent = typo(q.question);
    ligne.append(ecoute, enonce); haut.append(tete, ligne);
    const corps = el("div", "f-corps");
    boite.append(haut, corps); panneau.append(boite);
    const parler = () => direMixte(q.dire || q.question, q);
    ecoute.onclick = () => { son("bouton"); sfx.tap(); parler(); };
    parler();

    const nom = v => (q.legendes && q.legendes[v]) || sansEmoji(v);
    const o = {
      petit, corps,
      // a tile: a picture with its name under it, or a word
      tuile(v, classe){
        const b = el("button", "reponse f-tuile" + (classe ? " " + classe : "")), c = contenuReponse(v, q, false);
        b.append(c.contenu); b.classList.add(c.genre); b.dataset.v = v;
        return b;
      },
      // the same, small and not to touch (inside a basket, in the solution)
      vignette(v){ const s = el("span", "f-vignette"); s.append(contenuReponse(v, q, false).contenu); return s; },
      // its name aloud: a written foreign word by its own voice, a picture by its French name
      nommer(v){
        const leg = q.legendes && q.legendes[v], t = nom(v);
        if (t) dire(t, leg ? "fr" : (q.langueTuiles || "fr"));
      },
      viser(cible, pieges){
        corps.querySelectorAll("[data-suite],[data-piege]").forEach(x => { delete x.dataset.suite; delete x.dataset.piege; });
        if (cible) cible.dataset.suite = "1";
        (pieges || []).forEach(x => { x.dataset.piege = "1"; });
      },
      bien(){ son("bouton"); sfx.tap(); },
      finir(juste, solution, dite, ou){
        if (fini) return; fini = true;
        const ms = Date.now() - t0;
        if (typeof noterJournal === "function") noterJournal("question", {id: q.id || null, n: q.notion || null, m: q.matiere || null, niv: q.niveau || null,
          j: juste, r: q.forme, ms, ec: 0});
        o.viser(null);
        corps.querySelectorAll("button").forEach(b => { b.disabled = true; });
        erreurAvant = !juste;
        if (juste) {
          sfx.juste();
          const [x, y] = centre(ou || corps);
          jaillir(x, y, {pieces: ["etincelle", "etoile_p", "etincelle_p"], n: 14, angle: [0, 360], gravite: 240, vitesse: [130, 260], taille: [14, 26], duree: [600, 900]});
          return setTimeout(() => fin({juste, ms}), TEST ? 0 : 900);
        }
        sfx.faux(); boite.classList.add("corrige");
        const t = el("div", "explication"), l1 = el("b"); l1.textContent = "Regarde la bonne réponse :"; t.append(l1, solution);
        if (q.explication) { const l2 = el("p"); l2.textContent = typo(q.explication); t.append(l2); }
        const suite = markOk(el("button", "moyen vert", "J'ai compris")); suite.append(iconeDom("pouce"));
        suite.onclick = () => { son("bouton"); sfx.tap(); taire(); fin({juste: false, ms}); };
        const correction = el("div", "q-correction"); correction.append(t, suite); boite.append(correction);
        suite.scrollIntoView({block: "nearest"});
        direMixte((dite ? dite + " " : "Regarde la bonne réponse. ") + (q.explication || ""), q);
      },
    };
    FORMES_Q[q.forme](q, o);
  });
}

// a shuffle that never leaves the tiles already in order
function melangerAutrement(l){
  if (l.length < 2) return [...l];
  for (let k = 0; k < 8; k++) { const m = shuffle([...l]); if (m.some((x, i) => x !== l[i])) return m; }
  return [...l].reverse();
}

FORMES_Q.ordre = (q, o) => {
  const cases = el("div", "f-cases"), tas = el("div", "f-tas");
  const places = q.suite.map((_, i) => { const c = el("div", "f-case"); c.dataset.n = i + 1; cases.append(c); return c; });
  const tuiles = melangerAutrement(q.suite).map(v => o.tuile(v));
  let k = 0;
  const libres = () => tuiles.filter(b => !b.classList.contains("placee"));
  const viser = () => o.viser(libres().find(b => b.dataset.v === q.suite[k]), libres().filter(b => b.dataset.v !== q.suite[k]));
  const solution = () => { const s = el("div", "f-solution"); q.suite.forEach((v, i) => { if (i) s.append(el("i", "f-fleche", "➜")); s.append(o.vignette(v)); }); return s; };
  tuiles.forEach(b => {
    tas.append(b);
    b.onclick = () => {
      if (b.dataset.v !== q.suite[k]) { b.classList.add("faux"); return o.finir(false, solution(), "", b); }
      o.bien(); o.nommer(b.dataset.v);
      b.classList.add("placee", "juste"); b.disabled = true; places[k].append(b); k++;
      if (k === q.suite.length) o.finir(true, null, "", cases); else viser();
    };
  });
  o.corps.append(cases, tas); viser();
};

FORMES_Q.paires = (q, o) => {
  const zone = el("div", "f-paires"), gauche = el("div", "f-col"), droite = el("div", "f-col");
  const cote = j => shuffle(q.paires.map((p, i) => { const b = o.tuile(p[j]); b.dataset.i = i; return b; }));
  const G = cote(0), D = cote(1);
  let choisie = null, faites = 0;
  const viser = () => {
    if (!choisie) return o.viser(G.find(b => !b.disabled), []);
    o.viser(D.find(b => b.dataset.i === choisie.dataset.i), D.filter(b => !b.disabled && b.dataset.i !== choisie.dataset.i));
  };
  const solution = () => {
    const s = el("div", "f-solution colonne");
    q.paires.forEach(([a, b]) => { const l = el("div", "f-ligne"); l.append(o.vignette(a), el("i", "f-fleche", "➜"), o.vignette(b)); s.append(l); });
    return s;
  };
  G.forEach(b => { gauche.append(b); b.onclick = () => {
    if (choisie) choisie.classList.remove("choisie");
    choisie = b; b.classList.add("choisie"); o.bien(); o.nommer(b.dataset.v); viser();
  }; });
  D.forEach(b => { droite.append(b); b.onclick = () => {
    if (!choisie) { gauche.classList.remove("secoue"); void gauche.offsetWidth; gauche.classList.add("secoue"); return; }   // the left one first
    if (b.dataset.i !== choisie.dataset.i) { b.classList.add("faux"); return o.finir(false, solution(), "", b); }
    o.bien(); o.nommer(b.dataset.v);
    [b, choisie].forEach(x => { x.disabled = true; x.classList.remove("choisie"); x.classList.add("juste", "paire" + faites % 4); });
    choisie = null; faites++;
    if (faites === q.paires.length) o.finir(true, null, "", zone); else viser();
  }; });
  zone.append(gauche, droite); o.corps.append(zone); viser();
};

FORMES_Q.tri = (q, o) => {
  const objets = shuffle(q.paniers.flatMap((p, k) => p.items.map(v => ({v, k}))));
  const scene = el("div", "f-objet"), bas = el("div", "f-paniers");
  const paniers = q.paniers.map((p, k) => {
    const b = el("button", "reponse f-panier"), titre = el("b"), dedans = el("span", "f-dedans");
    ecrireIcones(titre, p.nom); b.append(titre, dedans); b.dataset.k = k; bas.append(b);
    return b;
  });
  let i = 0;
  const montrer = () => {
    scene.innerHTML = "";
    const t = el("div", "f-courant"); t.append(contenuReponse(objets[i].v, q, false).contenu); scene.append(t);
    if (o.petit || i) o.nommer(objets[i].v);
    o.viser(paniers[objets[i].k], paniers.filter((_, k) => k !== objets[i].k));
  };
  const solution = () => {
    const s = el("div", "f-solution colonne");
    q.paniers.forEach(p => { const l = el("div", "f-ligne"), n = el("b"); ecrireIcones(n, p.nom + " :"); l.append(n); p.items.forEach(v => l.append(o.vignette(v))); s.append(l); });
    return s;
  };
  paniers.forEach((b, k) => { b.onclick = () => {
    if (k !== objets[i].k) { b.classList.add("faux"); return o.finir(false, solution(), "", b); }
    o.bien(); b.querySelector(".f-dedans").append(o.vignette(objets[i].v)); i++;
    if (i === objets.length) { scene.innerHTML = ""; o.finir(true, null, "", bas); } else montrer();
  }; });
  o.corps.append(scene, bas); montrer();
};
