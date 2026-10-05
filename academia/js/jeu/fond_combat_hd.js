/* Académia : le décor des combats en haute définition. Des pièces vectorielles rendues en PNG (outils/fonds/ :
   pack Background Elements Remastered de Kenney, CC0, et formes dessinées dans le même style) sont posées à la
   taille de la toile : ciel et sol en dégradé, lointains teintés par la lumière, monument de la région, arbres,
   reflets du sol et ses fleurs ; la mer, l'écume et les deux estrades sont peintes à la taille exacte (toile 2D,
   bords lisses). Chez le boss, le crépuscule (la nuit violette à l'Observatoire).
   Contrat : IMAGES_FONDS, chargerFondsHD(load, version), dessinerFondHD(S, L, regionId, boss) ; profondeurs 0 à 4. */
const IMAGES_FONDS = ["nuage1", "nuage2", "nuage3", "nuage4", "soleil", "lune", "croissant", "etoile", "etoile_filante", "eclat", "luciole", "petale",
  "loin_sapin", "loin_arbre", "loin_arbre_long", "loin_montagne1", "loin_montagne2", "loin_collines",
  "sapin", "sapin_neige", "arbre", "arbre_long", "cerisier", "cerisier_long", "arbre_blanc",
  "buisson", "buisson_bleu", "buisson_fleuri", "herbe1", "herbe2", "herbe3", "herbe4",
  "torii", "lanterne", "ile", "voilier", "phare", "mouette", "remous", "montagne1", "montagne2", "chateau", "maison",
  "fontaine", "butte", "observatoire",
  "fleurs_rouge", "fleurs_jaune", "fleurs_blanc", "fleurs_rose", "fleurs_violet", "fleurs_bleu", "cailloux", "coquillage", "etoile_mer",
  "tache1", "tache2"]
  .map(n => ({cle: "fd_" + n, fichier: n + ".png"}));

// version 'hd' (assets/hd/fonds) or 'leger' (assets/leger/fonds, half the resolution): same drawing, same code
function chargerFondsHD(load, version){
  const dossier = `assets/${version === "leger" ? "leger" : "hd"}/fonds/`;
  IMAGES_FONDS.forEach(i => { if (!load.textureManager.exists(i.cle)) load.image(i.cle, dossier + i.fichier); });
}

// L = {w, h, hs, yh, r, lui, moi} in canvas pixels; the backdrop covers the whole canvas, depths 0 to 4
function dessinerFondHD(S, L, regionId, boss){ FONDS_HD.dessiner(S, L, regionId, boss); }

const FONDS_HD = (() => {
  // the light: sky from top to horizon, haze of the far rows, tint multiplied on everything else
  const LUMIERES = {
    jour: {ciel: ["#5FB6F2", "#9BD5FA", "#D9F2FD"], loin: 0xC4E4F7, collines: 0xB4DEC2},
    montagne: {ciel: ["#5E9FEA", "#A3CBF6", "#E2EEFF"], loin: 0xCBDDF6},
    jardin: {ciel: ["#74C2FA", "#B8E1FD", "#FFE1EF"], loin: 0xF6DCEC},
    crepuscule: {ciel: ["#2E1B5E", "#6A3592", "#CF628E", "#FFAD72"], loin: 0xB98AC0, teinte: 0xE2B2D2, vert: 0x9CC8E4, nuages: 0xFFB2A6,
      soleil: true, etoiles: .3},
    nuit: {ciel: ["#100C38", "#221C62", "#463A9A"], loin: 0x343E86, teinte: 0x7682C0, sol: 0xA6AEE0, lune: "lune", etoiles: 1, nuit: true},
    nuitBoss: {ciel: ["#140A34", "#3A1A6C", "#8A3486"], loin: 0x40307E, teinte: 0x8C78C2, sol: 0xB6A6E2, lune: "croissant", etoiles: 1, nuit: true}
  };
  // the ground: band under the horizon (its depth, in horizon heights), main colour, near colour, its streaks of light; what grows
  const SOLS = {
    sable: {haut: "#DB9550", bande: .07, base: "#F2B36E", bas: "#F5BC7D", reflet: "#F6BF81",
      deco: ["herbe1", "herbe3", "herbe4", "cailloux"], densite: .12},
    plage: {haut: "#E2B274", bande: .16, base: "#F7D59C", bas: "#F9DCA8", reflet: "#FADDAD",
      deco: ["coquillage", "etoile_mer", "cailloux", "herbe2"], densite: .1},
    herbe: {vert: true, haut: "#4FA055", bande: .06, base: "#79C766", bas: "#82CE6C", reflet: "#86CF71",
      deco: ["herbe1", "herbe2", "fleurs_jaune", "fleurs_blanc", "fleurs_rouge", "cailloux"], densite: .16},
    jardin: {vert: true, haut: "#58AE5A", bande: .06, base: "#83CF6C", bas: "#8BD472", reflet: "#90D779",
      deco: ["fleurs_rose", "fleurs_blanc", "fleurs_jaune", "fleurs_violet", "herbe1", "petale"], densite: .3}
  };
  const ESTRADES = {   // top light, side darker, a thin darker line around; planks for the wooden one
    sable: {trait: "#7E4E2A", cote: "#C4844A", cote2: "#DFA05E", dessus: "#FAD7A2", clair: "#FFE7BE"},
    bois: {trait: "#6A3E22", cote: "#A0612F", cote2: "#BD7A40", dessus: "#E2A867", clair: "#EDBB7E", planche: "#C88A4C"},
    herbe: {vert: true, trait: "#2E6A36", cote: "#4D9842", cote2: "#69B553", dessus: "#A6DF80", clair: "#C4EFA0"},
    pierre: {trait: "#3E3A66", cote: "#8A84B6", cote2: "#A6A0CC", dessus: "#DEDAF2", clair: "#F0EEFB"}
  };
  const ECLATS = [0xFFC93C, 0xFF6FB5, 0x5CBDFF, 0x4FD99A, 0xA77BFF];   // the garden's sparkles: gold, pink, sky, mint, lilac
  const TAILLES = {herbe1: .14, herbe2: .14, herbe3: .14, herbe4: .14, cailloux: .08, coquillage: .075, etoile_mer: .08, petale: .045,
    fleurs_rouge: .13, fleurs_jaune: .13, fleurs_blanc: .13, fleurs_rose: .13, fleurs_violet: .13, fleurs_bleu: .13};
  const DECORS = {
    dojo: {lumiere: "jour", sol: "sable", estrade: "sable", dessin: dojo},
    albion: {lumiere: "jour", sol: "plage", estrade: "bois", dessin: port},
    germania: {lumiere: "montagne", sol: "herbe", estrade: "herbe", dessin: montagnes,
      deco: ["herbe1", "herbe2", "fleurs_blanc", "fleurs_violet", "fleurs_blanc", "cailloux"]},
    duche: {lumiere: "jour", sol: "herbe", estrade: "herbe", dessin: duche},
    jardin: {lumiere: "jardin", sol: "jardin", estrade: "herbe", dessin: jardin},
    observatoire: {lumiere: "nuit", sol: "herbe", estrade: "pierre", dessin: observatoire, nuit: true,
      deco: ["herbe1", "herbe2", "fleurs_bleu", "fleurs_violet", "fleurs_blanc", "cailloux"]}
  };
  const TOILES = ["fd_ciel", "fd_sol", "fd_mer", "fd_ecume", "fd_estrade0", "fd_estrade1"];   // painted here, freed with the scene

  // colours: 0xRRGGBB or '#RRGGBB', multiplied like a Phaser tint
  const hexa = c => typeof c === "number" ? c : parseInt(c.slice(1), 16);
  const fois = (a, b) => [16, 8, 0].reduce((s, d) => s + (Math.round(((a >> d) & 255) * ((b >> d) & 255) / 255) << d), 0);
  const teinter = (c, t) => t == null ? c : "#" + fois(hexa(c), t).toString(16).padStart(6, "0");

  function dessiner(S, L, regionId, boss){
    const d = DECORS[regionId] || DECORS.dojo;
    const lum = LUMIERES[boss ? (d.nuit ? "nuitBoss" : "crepuscule") : d.lumiere];
    IMAGES_FONDS.forEach(i => { if (S.textures.exists(i.cle)) S.textures.get(i.cle).setFilter(Phaser.Textures.FilterMode.LINEAR); });
    const D = outils(S, L, lum, [...regionId + (boss ? "!" : "")].reduce((h, c) => Math.imul(h ^ c.charCodeAt(0), 16777619), 2166136261));
    ciel(D);
    d.dessin(D);
    sol(D, SOLS[d.sol], d.deco);
    [L.lui, L.moi].forEach((p, i) => estrade(D, "fd_estrade" + i, p, ESTRADES[d.estrade]));
    S.events.once("shutdown", () => TOILES.forEach(k => { if (S.textures.exists(k)) S.textures.remove(k); }));
  }

  function outils(S, L, lum, graine){
    let a = graine | 0;
    const rnd = () => {   // mulberry32: the same backdrop each time for a region
      a = (a + 0x6D2B79F5) | 0; let t = Math.imul(a ^ (a >>> 15), 1 | a);
      t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
    const u = L.yh, r = L.r || 1, D = {S, L, lum, u, rnd, portrait: L.h > L.w * 1.1};
    D.entre = (m, n) => m + (n - m) * rnd();
    D.choix = t => t[Math.floor(rnd() * t.length)];
    // the region's landmark, on the left: under the Ombre's life box on a tall screen, just right of it otherwise
    // (the box: 12 CSS pixels from the left, min(300, 50vw - 20) wide, style_combat.css)
    D.x0 = D.portrait ? Math.max(.14 * L.w, .56 * u) : Math.max(.14 * L.w, r * (12 + Math.min(300, L.w / r / 2 - 20)) + .48 * u);
    // a piece standing on (x, y) (or centred with oy .5), `haut` canvas pixels tall, tinted by the light unless told
    D.img = (nom, x, y, haut, prof, o = {}) => {
      const cle = "fd_" + nom;
      if (!S.textures.exists(cle)) return null;
      const i = S.add.image(Math.round(x), Math.round(y), cle).setOrigin(o.ox ?? .5, o.oy ?? 1).setDepth(prof);
      i.setScale(haut / i.height).setFlipX(!!o.miroir);
      const t = o.teinte === undefined ? lum.teinte : o.teinte;
      if (t != null) i.setTint(t);
      if (o.alpha != null) i.setAlpha(o.alpha);
      return i;
    };
    // a row of pieces across the width, feet on `bas`; `libre(x)` false leaves a gap (a landmark stands there)
    D.rangee = (noms, bas, h0, h1, p0, p1, prof, o = {}) => {
      for (let x = -D.entre(0, p1); x < L.w + p1; x += D.entre(p0, p1)) {
        const nom = D.choix(noms), h = D.entre(h0, h1), m = rnd() < .5, dy = D.entre(-.01, .02) * u;
        if (!o.libre || o.libre(x)) D.img(nom, x, bas + dy, h, prof, {...o, miroir: m});
      }
    };
    // a canvas painted at the size asked (smooth edges at any size), shown as one image
    D.toile = (cle, x, y, w, h, prof, peindre) => {
      if (S.textures.exists(cle)) S.textures.remove(cle);
      const W = Math.max(2, Math.ceil(w)), H = Math.max(2, Math.ceil(h)), t = S.textures.createCanvas(cle, W, H), g = t.getContext();
      peindre(g, W, H); t.refresh(); t.setFilter(Phaser.Textures.FilterMode.LINEAR);
      return S.add.image(Math.round(x), Math.round(y), cle).setOrigin(0).setDepth(prof);
    };
    D.degrade = (g, H, arrets) => {   // a vertical gradient: [[position 0..1, colour], ...]
      const d = g.createLinearGradient(0, 0, 0, H);
      arrets.forEach(([p, c]) => d.addColorStop(Math.min(1, Math.max(0, p)), c));
      return d;
    };
    return D;
  }

  // ---------- sky ----------
  function ciel(D){
    const {L, u, lum} = D, n = lum.ciel.length;
    D.toile("fd_ciel", 0, 0, 4, 256, 0, (g, W, H) => { g.fillStyle = D.degrade(g, H, lum.ciel.map((c, i) => [i / (n - 1), c])); g.fillRect(0, 0, W, H); })
      .setDisplaySize(L.w, L.yh + 4);
    if (lum.etoiles) for (let i = 0, k = Math.round(lum.etoiles * 9 * L.w / u); i < k; i++)
      D.img("etoile", D.rnd() * L.w, D.entre(.03, .75 * lum.etoiles) * L.yh, D.entre(.035, .08) * u, .05, {oy: .5, teinte: null, alpha: D.entre(.45, 1)});
    if (lum.lune) D.img(lum.lune, L.w - Math.max(.1 * L.w, .42 * u), .3 * u, .3 * u, .1, {oy: .5, teinte: null});
    if (lum.soleil)   // between the two life boxes; on a tall screen, low behind the trees, under the Ombre's box
      D.img("soleil", .5 * L.w, L.yh - (D.portrait ? .3 : .62) * u, (D.portrait ? .38 : .46) * u, .1, {oy: .5, teinte: 0xFFE2A8});
    if (lum.nuit) return;
    const k = Math.max(2, Math.round(.6 * L.w / u)), nuages = ["nuage1", "nuage2", "nuage3", "nuage4"];
    for (let i = 0; i < k; i++)
      D.img(nuages[i % 4], (i + D.entre(.2, .8)) * L.w / k, D.entre(.1, .36) * u, D.entre(.2, .28) * u, .2,
        {oy: .5, teinte: lum.nuages ?? null, miroir: D.rnd() < .5});
  }

  // far away: a pale row of trees, tinted by the haze of the light
  const lointains = (D, noms, h = [.5, .72]) =>
    D.rangee(noms, D.L.yh - .02 * D.u, h[0] * D.u, h[1] * D.u, .1 * D.u, .2 * D.u, .5, {teinte: D.lum.loin});
  // the bushes that hide the foot of the trees, all along the horizon (not in front of a landmark)
  const haie = (D, noms, libre) => D.rangee(noms, D.L.yh + .05 * D.u, .1 * D.u, .15 * D.u, .26 * D.u, .46 * D.u, 1.8, {libre});
  const autour = (D, demi) => x => Math.abs(x - D.x0) > demi * D.u;
  // gentle far hills, a strip laid end to end
  function collines(D, bas, haut, prof){
    const t = D.S.textures.exists("fd_loin_collines") && D.S.textures.get("fd_loin_collines").getSourceImage();
    if (!t) return;
    const l = Math.round(t.width * haut / t.height);
    for (let x = -Math.round(D.rnd() * l); x < D.L.w; x += l - 1) D.img("loin_collines", x, bas, haut, prof, {ox: 0, teinte: D.lum.collines ?? D.lum.loin});
  }

  // ---------- the regions ----------
  function dojo(D){
    const {L, u} = D, yh = L.yh;
    lointains(D, ["loin_sapin", "loin_arbre", "loin_arbre_long"]);
    D.img("torii", D.x0, yh + .035 * u, .52 * u, 1);
    D.img("lanterne", D.x0 + .5 * u, yh + .16 * u, .34 * u, 2.45);
    D.rangee(["sapin", "arbre", "arbre_long", "sapin"], yh + .02 * u, .46 * u, .68 * u, .2 * u, .32 * u, 1.5, {libre: autour(D, .52)});
    haie(D, ["buisson", "buisson_bleu"], autour(D, .3));
  }

  function port(D){
    const {L, u, lum} = D, yh = L.yh, w = L.w, ym = Math.round(yh - .26 * u);
    D.toile("fd_mer", 0, ym, w, yh - ym + 2, .6, (g, W, H) => {   // deeper far away, lighter near the shore, small crests
      g.fillStyle = D.degrade(g, H, [[0, "#2C8BDD"], [.5, "#49A7EB"], [1, "#84D2F6"]].map(([p, c]) => [p, teinter(c, lum.teinte)]));
      g.fillRect(0, 0, W, H);
      g.fillStyle = "rgba(255,255,255,.7)"; g.fillRect(0, 0, W, Math.max(1, .012 * u));
      for (let i = 0, k = Math.round(W / u * 9); i < k; i++) {
        const y = (.15 + .8 * D.rnd()) * H, l = (.025 + .045 * y / H) * u;
        g.beginPath(); g.ellipse(D.rnd() * W, y, l, Math.max(1, l * .14), 0, 0, Math.PI * 2); g.fill();
      }
    });
    D.img("ile", D.x0, ym + .1 * u, .42 * u, .7);
    const xb = D.portrait ? w - .3 * u : Math.max(.47 * w, D.x0 + .9 * u);
    D.img("voilier", xb, ym + .17 * u, .28 * u, .8);
    D.img("remous", xb, ym + .19 * u, .07 * u, .85);
    if (w > 3 * u) { const xp = w - Math.max(.07 * w, .32 * u); D.img("phare", xp, ym + .11 * u, .5 * u, .7); D.img("remous", xp, ym + .13 * u, .06 * u, .85); }
    for (let i = 0; i < 3; i++) D.img("mouette", (.3 + .12 * i + D.entre(0, .06)) * w, D.entre(.18, .4) * u, D.entre(.04, .06) * u, .3, {teinte: null, miroir: i % 2});
    D.toile("fd_ecume", 0, yh - .03 * u, w, .09 * u, 1, (g, W, H) => {   // the foam on the sand, scalloped
      const p = .22 * u, n = Math.ceil(W / p) + 1;
      g.fillStyle = teinter("#FFFFFF", lum.teinte); g.beginPath(); g.moveTo(0, 0); g.lineTo(W, 0);
      for (let i = n; i >= 0; i--) g.quadraticCurveTo(i * p + p / 2, H * (i % 2 ? .95 : .75), i * p, H * .55);
      g.closePath(); g.fill();
    });
  }

  function montagnes(D){
    const {L, u} = D, yh = L.yh;
    D.rangee(["loin_montagne1", "loin_montagne2"], yh - .08 * u, .5 * u, .7 * u, .9 * u, 1.3 * u, .4, {teinte: D.lum.loin});
    D.rangee(["montagne1", "montagne2"], yh - .03 * u, .62 * u, .88 * u, 1.45 * u, 2 * u, .6);
    D.rangee(["sapin_neige", "sapin", "sapin_neige"], yh + .02 * u, .36 * u, .56 * u, .15 * u, .28 * u, 1.5);
    haie(D, ["buisson_bleu", "buisson"]);
  }

  function duche(D){
    const {L, u} = D, yh = L.yh, xm = D.x0 + .76 * u;
    collines(D, yh + .01 * u, .3 * u, .4);
    lointains(D, ["loin_arbre", "loin_arbre_long"], [.4, .56]);
    D.img("chateau", D.x0, yh + .03 * u, .62 * u, 1);
    const maison = xm < (D.portrait ? .6 : .62) * L.w;   // the red house of the valley (the village has one), when there is room
    if (maison) D.img("maison", xm, yh + .08 * u, .34 * u, 1.85);
    const libre = x => x < D.x0 - .62 * u || x > (maison ? xm + .4 * u : D.x0 + .62 * u);
    D.rangee(["arbre", "arbre_long", "arbre"], yh + .02 * u, .44 * u, .66 * u, .2 * u, .32 * u, 1.5, {libre});
    haie(D, ["buisson", "buisson"], x => Math.abs(x - D.x0) > .3 * u && (!maison || Math.abs(x - xm) > .3 * u));
  }

  function jardin(D){
    const {L, u} = D, yh = L.yh;
    lointains(D, ["loin_arbre", "loin_arbre_long"]);
    D.rangee(["cerisier", "arbre_blanc", "cerisier_long", "cerisier"], yh + .02 * u, .46 * u, .68 * u, .2 * u, .3 * u, 1.5);
    haie(D, ["buisson_fleuri", "buisson", "buisson_fleuri"], autour(D, .32));
    D.img("fontaine", D.x0, yh + .1 * u, .42 * u, 1.9);
    for (let i = 0, k = Math.round(1.3 * L.w / u); i < k; i++) {   // the sparkles of the garden, floating: a white star in a coloured glow
      const x = D.rnd() * L.w, y = D.entre(.12, 1.1) * yh, s = D.entre(.13, .2) * u;
      D.img("eclat", x, y, s, 1.95, {oy: .5, teinte: ECLATS[i % ECLATS.length]});
      D.img("etoile", x, y, .55 * s, 1.96, {oy: .5, teinte: null});
    }
  }

  function observatoire(D){
    const {L, u} = D, yh = L.yh, hb = .16 * u;
    collines(D, yh + .01 * u, .26 * u, .4);
    lointains(D, ["loin_sapin", "loin_arbre"], [.42, .62]);
    D.img("etoile_filante", D.portrait ? .62 * L.w : .4 * L.w, .3 * yh, .15 * u, .15, {oy: .5, teinte: null});
    D.img("butte", D.x0, yh + .05 * u, hb, .95);
    D.img("observatoire", D.x0, yh + .05 * u - hb * .8, .5 * u, 1, {teinte: null});
    D.rangee(["sapin", "arbre", "arbre_long"], yh + .02 * u, .42 * u, .62 * u, .22 * u, .34 * u, 1.5, {libre: autour(D, .62)});
    haie(D, ["buisson", "buisson_bleu"], autour(D, .4));
    for (let i = 0, k = Math.round(2.2 * L.w / u); i < k; i++)   // fireflies over the grass
      D.img("luciole", D.rnd() * L.w, yh + D.entre(-.25, 1.6) * u, D.entre(.07, .12) * u, 2.7, {oy: .5, teinte: null, alpha: D.entre(.7, 1)});
  }

  // ---------- ground ----------
  function sol(D, s, deco){
    const {L, u, lum} = D, h = L.h - L.yh, t = (s.vert && lum.vert) || lum.teinte;   // grass at dusk: a cool tint
    // a vertical gradient stretched to the ground: a darker band under the horizon, then the ground, lighter near us
    D.toile("fd_sol", 0, 0, 4, 512, .9, (g, W, H) => {
      g.fillStyle = D.degrade(g, H, [[0, s.haut], [s.bande * u / h, s.base], [.5, s.base], [1, s.bas]].map(([p, c]) => [p, teinter(c, t)]));
      g.fillRect(0, 0, W, H);
    }).setPosition(0, L.yh).setDisplaySize(L.w, h);
    // long flat streaks of light, wider near us
    for (let i = 0, k = Math.round(L.w * h / (u * u) * .4), y0 = L.yh + (s.bande + .05) * u; i < k; i++) {
      const y = y0 + D.rnd() * (L.h - y0), p = Math.min(1, (y - L.yh) / (1.8 * u)), lw = D.entre(.9, 1.6) * u * (.4 + .8 * p);
      const im = D.img(D.choix(["tache1", "tache2"]), D.rnd() * L.w, y, 1, .92, {oy: .5, miroir: D.rnd() < .5, teinte: hexa(teinter(s.reflet, t))});
      if (im) im.setDisplaySize(lw, lw * (.08 + .05 * p));
    }
    // what grows on it, smaller far away, never on a stand nor right in front of one
    const noms = deco || s.deco, teinte = lum.sol ?? t;
    for (let y = L.yh + .1 * u; y < L.h + .1 * u; ) {
      const f = .55 + .55 * Math.min(1, (y - L.yh) / (1.6 * u));
      for (let x = D.entre(0, .3) * u * f; x < L.w; x += D.entre(.2, .38) * u * f) {
        const nom = D.choix(noms), hd = TAILLES[nom] * u * f;
        if (D.rnd() > s.densite) continue;
        const libre = [L.lui, L.moi].every(p => ((x - p.x) / (p.rx + .14 * u)) ** 2 + ((y - p.y - .28 * p.ry) / (1.3 * p.ry + hd + .04 * u)) ** 2 > 1);
        if (libre) D.img(nom, x, y, hd, 2.5 + .1 * y / L.h, {miroir: D.rnd() < .5, teinte});
      }
      y += D.entre(.15, .22) * u * f;
    }
  }

  // ---------- the stands: a round stage seen from the side, painted at its exact size ----------
  function estrade(D, cle, p, c0){
    const t = (c0.vert && D.lum.vert) || D.lum.teinte, c = Object.fromEntries(Object.entries(c0).filter(([k]) => k !== "vert").map(([k, v]) => [k, teinter(v, t)]));
    const r = D.L.r || 1, ep = Math.round(p.ry * .55), tr = Math.max(1.5, 2.2 * r), m = Math.ceil(tr) + 2;
    D.toile(cle, p.x - p.rx - m, p.y - p.ry - m, 2 * (p.rx + m), 2 * (p.ry + m) + ep, 3, (g, W, H) => {
      const cx = W / 2, cy = m + p.ry;
      const ell = (y, rx, ry, col) => { g.fillStyle = col; g.beginPath(); g.ellipse(cx, y, Math.max(1, rx), Math.max(1, ry), 0, 0, Math.PI * 2); g.fill(); };
      const bande = (y0, y1, rx, col) => { g.fillStyle = col; g.fillRect(cx - rx, y0, 2 * rx, y1 - y0); };
      ell(cy + ep, p.rx + tr, p.ry + tr, c.trait); bande(cy, cy + ep, p.rx + tr, c.trait); ell(cy, p.rx + tr, p.ry + tr, c.trait);
      ell(cy + ep, p.rx, p.ry, c.cote); bande(cy, cy + ep, p.rx, c.cote);
      ell(cy + ep * .42, p.rx, p.ry, c.cote2);
      ell(cy, p.rx, p.ry, c.dessus);
      ell(cy - p.ry * .1, p.rx * .86, p.ry * .72, c.clair);
      if (!c.planche) return;
      // wooden deck: planks across the top, staves down the side
      g.save(); g.beginPath(); g.ellipse(cx, cy, p.rx, p.ry, 0, 0, Math.PI * 2); g.clip();
      g.strokeStyle = c.planche; g.lineWidth = Math.max(1, 1.4 * r);
      for (let k = -2; k <= 2; k++) { const y = cy + k * p.ry * .38; g.beginPath(); g.moveTo(cx - p.rx, y); g.lineTo(cx + p.rx, y); g.stroke(); }
      g.restore();
      g.save(); g.beginPath(); g.rect(cx - p.rx, cy, 2 * p.rx, ep + p.ry); g.ellipse(cx, cy, p.rx, p.ry, 0, 0, Math.PI * 2, true); g.clip("evenodd");
      g.strokeStyle = c.trait; g.globalAlpha = .35;
      for (let k = -6; k <= 6; k++) {
        const x = cx + p.rx * Math.sin(k * Math.PI / 14), yb = cy + ep + p.ry * Math.cos(k * Math.PI / 14);
        g.beginPath(); g.moveTo(x, cy); g.lineTo(x, yb); g.stroke();
      }
      g.restore();
    });
  }

  return {dessiner};
})();
