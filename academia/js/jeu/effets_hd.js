/* Académia : les effets en haute définition, dans le style plat des personnages (pièces dessinées en SVG,
   outils/effets/) : étincelle au toucher, herbe qui bruisse, fumée des Ombres, coups, bouclier, cœurs de l'Ombre
   consolée, pluie d'étoiles. Deux versions comme les personnages : assets/hd/effets et assets/leger/effets.
   Contrat : IMAGES_EFFETS, chargerEffetsHD(load, version), effetHD(scene, nom, x, y, taille, profondeur).
   (x, y) : le centre de la cible dans les coordonnées du monde de la scène (toucher : le point touché ; herbe : les
   pieds) ; taille : la hauteur de la cible dans ces unités (une case du village = 16, au combat des pixels de toile).
   Chaque effet se détruit seul (tweens d'images et particules de Phaser) et rend sa durée en ms. Rien d'effrayant. */
const IMAGES_EFFETS = ["etoile", "etoile_p", "coeur", "coeur_p", "feuille", "brin", "etincelle", "etincelle_p", "halo", "aura", "point",
  "anneau", "anneau_p", "choc", "choc_p", "bouclier", "bouclier_p", "nuage", "nuage_p",
  "cube", "chiffre1", "chiffre2", "chiffre3", "plume", "caillou", "pion"].map(n => ({cle: "fx_" + n, fichier: n + ".png"}));   // the last ones: the families' attacks

// version 'hd' (assets/hd/effets) or 'leger' (assets/leger/effets, half the resolution): same drawing, same code
function chargerEffetsHD(load, version){
  const dossier = `assets/${version === "leger" ? "leger" : "hd"}/effets/`;
  IMAGES_EFFETS.forEach(i => { if (!load.textureManager.exists(i.cle)) load.image(i.cle, dossier + i.fichier); });
}

// noms : toucher, herbe, fumee, apparition, coup, coup_fort, critique, esquive, recu, consolee, evolution
function effetHD(scene, nom, x, y, taille, profondeur = 0){ return EFFETS_HD.jouer(scene, nom, x, y, taille, profondeur); }

const EFFETS_HD = (() => {
  const sortie = t => 1 - (1 - t) ** 3, entree = t => t * t, cloche = t => Math.sin(Math.PI * Math.min(1, Math.max(0, t)));
  const rebond = t => { const c = 2.2, u = t - 1; return 1 + (c + 1) * u ** 3 + c * u * u; };   // overshoots, then settles
  const entre = (a, b) => a + Math.random() * (b - a);
  // a piece's size over its life (t from 0 to 1): pops in then shrinks, swells (smoke), steady, twinkles
  const FORMES = {
    pop: t => t < .2 ? rebond(t / .2) : 1 - .85 * entree((t - .2) / .8),
    gonfle: t => .4 + .6 * sortie(t),
    fixe: t => Math.min(1, t / .1),
    scintille: t => (t < .2 ? rebond(t / .2) : 1) * (.8 + .2 * Math.cos(t * 22))
  };
  // its opacity: holds then fades, fades in and out, fades all along
  const FONDUS = {fin: t => t < .55 ? 1 : 1 - entree((t - .55) / .45), doux: t => t < .12 ? t / .12 : t < .6 ? 1 : 1 - (t - .6) / .4, vite: t => 1 - t};

  // the drawings are smoothed when scaled, like the characters
  const lisses = new Set();
  function lisser(S){
    IMAGES_EFFETS.forEach(i => {
      if (!lisses.has(i.cle) && S.textures.exists(i.cle)) { S.textures.get(i.cle).setFilter(Phaser.Textures.FilterMode.LINEAR); lisses.add(i.cle); }
    });
  }

  function outils(S, x, y, T, prof){
    const z = S.cameras && S.cameras.main ? S.cameras.main.zoom : 1;   // canvas pixels per world unit
    let fin = 0;
    const haut = k => S.textures.getFrame(k).height;
    // the small drawing (village) unless it would be enlarged by more than a third on screen, else the big one
    const cle = (base, px) => {
      const p = `fx_${base}_p`, g = `fx_${base}`;
      if (S.textures.exists(p) && (!S.textures.exists(g) || haut(p) * 1.33 >= px)) return p;
      return S.textures.exists(g) ? g : null;
    };
    const poser = (k, o) => {
      const im = S.add.image(o.cx != null ? o.cx : x, o.cy != null ? o.cy : y, k).setDepth(prof + (o.dz || 0));
      if (o.add) im.setBlendMode(Phaser.BlendModes.ADD);
      if (o.tint != null) im.setTint(o.tint);
      if (o.miroir) im.setFlipX(true);
      return im;
    };
    // the image follows its progress t (0 to 1) through etat(t), then goes
    const lancer = (im, ms, delai, etat) => {
      etat(0);
      if (delai) im.setVisible(false);
      S.tweens.addCounter({from: 0, to: 1, duration: ms, delay: delai, onStart: () => im.setVisible(true),
        onUpdate: tw => etat(tw.getValue()), onComplete: () => im.destroy()});
      fin = Math.max(fin, delai + ms);
    };
    // one image: f(im, t, h) sets its state at t, h(v) gives it the height v (world units)
    const anime = (base, hMax, ms, f, o = {}) => {
      const k = cle(base, hMax * z);
      if (!k) return;
      const im = poser(k, o), u = 1 / haut(k), h = v => im.setScale(Math.max(0, v) * u);
      lancer(im, ms, o.delai || 0, t => f(im, t, h));
    };
    // n pieces thrown from the centre, slowing down (evenly spread with pas), falling a little with gravite
    const eclats = (base, o) => {
      const k = cle(base, Math.sqrt(o.taille[0] * o.taille[1]) * z * 1.1);
      if (!k) return;
      const u = 1 / haut(k), forme = FORMES[o.forme || "pop"], fondu = FONDUS[o.fondu || "fin"];
      const a0 = o.depart != null ? o.depart : Math.random() * 360, r0 = o.depuis || 0, cx = o.cx != null ? o.cx : x, cy = o.cy != null ? o.cy : y;
      for (let i = 0; i < o.n; i++) {
        const a = (o.pas ? a0 + i * 360 / o.n + entre(-12, 12) : entre(...(o.angle || [0, 360]))) * Math.PI / 180;
        const d = entre(...o.loin), h = entre(...o.taille), ms = entre(...o.ms);
        const rot0 = o.droit ? 0 : entre(-180, 180), rot = (o.tourne || 0) * entre(.5, 1) * (Math.random() < .5 ? -1 : 1);
        const im = poser(k, o);
        lancer(im, ms, (o.delai || 0) + (o.etale ? entre(0, o.etale) : 0), t => {
          const r = r0 + (d - r0) * sortie(t);
          im.setPosition(cx + Math.cos(a) * r, cy + Math.sin(a) * r + (o.gravite || 0) * t * t);
          im.setScale(h * u * forme(t)).setAlpha(fondu(t) * (o.alpha || 1)).setAngle(rot0 + rot * t);
        });
      }
    };
    // Phaser particles: a spray under gravity, all at once or one every `cadence` ms
    const gerbe = (base, o) => {
      const [t0, t1] = o.taille, k = cle(base, Math.sqrt(t0 * t1) * z * 1.1);
      if (!k) return;
      const u = 1 / haut(k), forme = FORMES[o.forme || "fixe"], fondu = FONDUS[o.fondu || "fin"], a = o.alpha || 1, tourne = o.tourne || 0;
      const cfg = {
        emitting: !!o.cadence, frequency: o.cadence || 0, quantity: 1, stopAfter: o.cadence ? o.n : 0,
        lifespan: {min: o.ms[0], max: o.ms[1]}, speed: {min: o.vitesse[0], max: o.vitesse[1]},
        angle: {min: o.angle[0], max: o.angle[1]}, gravityY: o.gravite || 0,
        scale: {onEmit: p => (p.fxH = entre(t0, t1) * u) * forme(0), onUpdate: (p, c, t) => p.fxH * forme(t)},
        alpha: {onEmit: () => fondu(0) * a, onUpdate: (p, c, t) => fondu(t) * a},
        rotate: {onEmit: p => { p.fxV = tourne * entre(.5, 1) * (Math.random() < .5 ? -1 : 1); return (p.fxA = o.droit ? 0 : entre(-180, 180)); },
          onUpdate: (p, c, t) => p.fxA + p.fxV * t}
      };
      if (o.add) cfg.blendMode = Phaser.BlendModes.ADD;
      if (o.tint != null) cfg.tint = o.tint;
      if (o.zone) cfg.emitZone = {type: "random", source: o.zone};   // a Phaser.Geom shape around the centre
      const duree = (o.cadence || 0) * o.n + o.ms[1];
      const creer = () => {
        const em = S.add.particles(x + (o.dx || 0), y + (o.dy || 0), k, cfg).setDepth(prof + (o.dz || 0));
        if (!o.cadence) em.explode(o.n);
        const finir = () => { if (em.scene) em.destroy(); };
        em.once("complete", () => S.time.delayedCall(0, finir));
        S.time.delayedCall(duree + 120, finir);
      };
      if (o.delai) S.time.delayedCall(o.delai, creer); else creer();
      fin = Math.max(fin, (o.delai || 0) + duree);
    };
    return {x, y, T, anime, eclats, gerbe, duree: () => fin};
  }

  // a ring of puffs that swell, drift out and up, then shrink away (cartoon smoke, no see-through stack); one rises in the middle
  function bouffee(F, n, r0, r1, hMax, ms){
    const {T, x, y} = F, a0 = Math.random() * 360;
    const vie = t => t < .4 ? .5 + .5 * sortie(t / .4) : 1 - .92 * entree((t - .4) / .6);
    for (let i = 0; i < n; i++) {
      const a = (a0 + i * 360 / n + entre(-15, 15)) * Math.PI / 180, h = T * hMax * entre(.75, 1), rot = entre(-25, 25);
      F.anime("nuage", h, ms * entre(.85, 1.1), (im, t, hh) => {
        const e = sortie(t), r = T * (r0 + (r1 - r0) * e);
        im.setPosition(x + Math.cos(a) * r, y + Math.sin(a) * r * .8 - T * .1 * e).setAlpha(t < .88 ? 1 : 1 - (t - .88) / .12).setAngle(rot * e);
        hh(h * vie(t));
      }, {delai: entre(0, 60), miroir: Math.random() < .5, dz: .1});
    }
    F.anime("nuage", T * hMax * 1.25, ms * 1.1, (im, t, hh) => {
      im.setY(y - T * .2 * sortie(t)).setAlpha(t < .85 ? 1 : 1 - (t - .85) / .15);
      hh(T * hMax * 1.25 * vie(t));
    }, {dz: .2});
  }

  // a hit: a comic star pops a little off centre and is gone fast (the face of the Ombre shows at once), a warm flash,
  // rings, sparks; small stars for the strong one, big golden ones for the critical one
  function choc(F, o){
    const T = F.T, ang = entre(-25, 25), x = F.x + entre(-.08, .08) * T, y = F.y + entre(-.1, .05) * T, h0 = T * o.h;
    F.anime("halo", h0 * 1.3, o.flash, (im, t, h) => { h(h0 * (.8 + .5 * sortie(t))); im.setAlpha(o.lueur * (1 - t)); },
      {cx: x, cy: y, add: true, tint: o.halo, dz: -.1});
    F.anime("choc", h0 * 1.1, o.ms, (im, t, h) => {
      h(h0 * (t < .25 ? rebond(t / .25) : t < .45 ? 1 : 1 - entree((t - .45) / .55)));
      im.setAngle(ang + 12 * t).setAlpha(t < .85 ? 1 : 1 - (t - .85) / .15);
    }, {cx: x, cy: y, tint: o.tint, dz: .1});
    for (let i = 0; i < o.anneaux; i++)
      F.anime("anneau", h0 * 1.4, o.ms * .9, (im, t, h) => { h(h0 * (.3 + 1.1 * sortie(t))); im.setAlpha(.9 * (1 - entree(t))); },
        {cx: x, cy: y, tint: o.anneau, delai: i * 70, dz: .2});
    F.eclats("etincelle", {cx: x, cy: y, n: o.eclats, pas: true, loin: [h0 * .5, h0 * .75], depuis: T * .1, taille: [T * .11, T * .16],
      ms: [o.ms, o.ms + 70], forme: "pop", droit: true, dz: .4});
    if (o.etoiles) F.eclats("etoile", {cx: x, cy: y, n: o.etoiles, pas: true, loin: o.loin.map(v => v * T), depuis: T * .15,
      taille: o.etoile.map(v => v * T), ms: o.duree, gravite: T * o.gravite, tourne: 360, forme: "pop", dz: .5});
  }

  const RECETTES = {
    // every touch gets an answer: a ring opens, a twinkle turns, four tiny sparks
    toucher(F){
      const T = F.T;
      F.anime("anneau", T * .9, 340, (im, t, h) => { h(T * (.2 + .7 * sortie(t))); im.setAlpha(.95 * (1 - entree(t))); }, {tint: 0xFFF6D0});
      F.anime("etincelle", T * .75, 320, (im, t, h) => {
        h(T * .75 * (t < .35 ? rebond(t / .35) : 1 - sortie((t - .35) / .65))); im.setAngle(60 * t);
      }, {dz: .2});
      F.eclats("etincelle", {n: 4, pas: true, depart: 45, loin: [T * .5, T * .62], depuis: T * .12, taille: [T * .2, T * .26],
        ms: [300, 360], forme: "pop", droit: true, dz: .3});
    },
    // tall grass rustles under the feet: blades and a leaf jump up and fall back
    herbe(F){
      const T = F.T;
      F.gerbe("brin", {n: 3, taille: [T * .24, T * .3], vitesse: [T * 1.8, T * 2.6], angle: [-128, -52], gravite: T * 14,
        ms: [360, 440], tourne: 160, dz: .1, dy: -T * .05});
      F.gerbe("feuille", {n: 1, taille: [T * .15, T * .18], vitesse: [T * 1.8, T * 2.4], angle: [-120, -60], gravite: T * 9,
        ms: [420, 500], tourne: 380, dz: .2});
    },
    // an Ombre comes or goes: a soft puff that swells and fades
    fumee(F){ bouffee(F, 7, .2, .48, .5, 650); },
    // an Ombre rises before a fight: a bigger puff, stars and twinkles
    apparition(F){
      const T = F.T;
      F.anime("halo", T * 1.5, 520, (im, t, h) => { h(T * (.9 + .6 * sortie(t))); im.setAlpha(.55 * (1 - t)); }, {tint: 0xE9E0FF, dz: -.1});
      bouffee(F, 9, .26, .6, .62, 820);
      F.eclats("etoile", {n: 6, pas: true, loin: [T * .55, T * .75], depuis: T * .15, taille: [T * .12, T * .17], ms: [650, 800],
        gravite: T * .15, tourne: 300, forme: "pop", dz: .5, delai: 80});
      F.gerbe("etincelle", {n: 5, zone: new Phaser.Geom.Circle(0, 0, T * .45), vitesse: [0, T * .1], angle: [0, 360],
        taille: [T * .1, T * .16], ms: [450, 650], forme: "scintille", droit: true, dz: .6, delai: 150});
    },
    coup(F){ choc(F, {h: .5, ms: 260, flash: 160, lueur: .45, halo: 0xFFF3C0, anneaux: 1, eclats: 5}); },
    coup_fort(F){
      choc(F, {h: .62, ms: 300, flash: 200, lueur: .55, halo: 0xFFE2A0, tint: 0xFFE6C8, anneaux: 2, anneau: 0xFFF0C0, eclats: 7,
        etoiles: 3, etoile: [.09, .12], loin: [.55, .7], duree: [420, 520], gravite: .1});
    },
    critique(F){
      choc(F, {h: .74, ms: 340, flash: 260, lueur: .6, halo: 0xFFD45C, tint: 0xFFE27A, anneaux: 2, anneau: 0xFFD45C, eclats: 5,
        etoiles: 8, etoile: [.15, .22], loin: [.7, .95], duree: [700, 850], gravite: .3});
    },
    // a dodge: a bubble of light pops up, shines, and fades
    esquive(F){
      const T = F.T;
      F.anime("bouclier", T * 1.08, 620, (im, t, h) => {
        h(T * .98 * (t < .22 ? .6 + .4 * rebond(t / .22) : t < .62 ? 1 + .025 * Math.sin((t - .22) * 30) : 1 + .1 * sortie((t - .62) / .38)));
        im.setAlpha(t < .62 ? .95 : .95 * (1 - entree((t - .62) / .38)));
      }, {dz: .1});
      F.anime("anneau", T * 1.35, 420, (im, t, h) => { h(T * (.9 + .45 * sortie(t))); im.setAlpha(.8 * (1 - entree(t))); },
        {tint: 0xA8E6FF, delai: 90, dz: .2});
      F.eclats("etincelle", {n: 6, pas: true, loin: [T * .5, T * .62], depuis: T * .44, taille: [T * .1, T * .15], ms: [380, 460],
        forme: "pop", droit: true, dz: .3, delai: 70});
    },
    // the companion is hit, gently: a soft pink glow, three little puffs, twinkles
    recu(F){
      const T = F.T;
      F.anime("halo", T, 320, (im, t, h) => { h(T * (.55 + .4 * sortie(t))); im.setAlpha(.55 * (1 - t)); }, {tint: 0xFFE6F1, dz: -.1});
      F.anime("anneau", T * .9, 340, (im, t, h) => { h(T * (.3 + .55 * sortie(t))); im.setAlpha(.7 * (1 - entree(t))); }, {tint: 0xFFB8D8, dz: .1});
      F.eclats("nuage", {n: 3, pas: true, loin: [T * .32, T * .4], depuis: T * .18, taille: [T * .18, T * .24], ms: [420, 500],
        forme: "gonfle", tourne: 30, dz: .2});
      F.eclats("etincelle", {n: 4, pas: true, depart: -45, loin: [T * .38, T * .5], depuis: T * .2, taille: [T * .1, T * .14],
        ms: [380, 460], forme: "scintille", droit: true, dz: .3, delai: 60});
    },
    // the beaten Ombre leaves consoled: hearts and twinkles rise
    consolee(F){
      const {T, x, y} = F;
      F.anime("aura", T * 1.5, 800, (im, t, h) => { h(T * (.95 + .5 * sortie(t))); im.setAlpha(.7 * cloche(t)); }, {tint: 0xFFC2DF, dz: -.1});
      for (let i = 0; i < 6; i++) {
        const cote = i % 2 ? 1 : -1, dx = cote * T * entre(.3, .45), dy = -T * entre(-.05, .3), h = T * entre(.13, .18);
        const monte = T * entre(.75, 1), ph = entre(0, 6.3);
        F.anime("coeur", h * 1.15, entre(1050, 1300), (im, t, hh) => {
          const s = Math.sin(ph + t * 6.5), e = sortie(t);
          im.setPosition(x + dx * (1 - .35 * e) + s * T * .05, y + dy - monte * e).setAngle(s * 12).setAlpha(t < .7 ? 1 : 1 - (t - .7) / .3);
          hh(h * (t < .18 ? rebond(t / .18) : 1));
        }, {delai: i * 120 + entre(0, 40), dz: .3});
      }
      F.gerbe("etincelle", {n: 7, cadence: 100, zone: new Phaser.Geom.Rectangle(-T * .45, -T * .45, T * .9, T * .75), vitesse: [T * .25, T * .45],
        angle: [-100, -80], taille: [T * .08, T * .13], ms: [700, 900], forme: "scintille", fondu: "doux", droit: true, dz: .4});
    },
    // an evolution: a golden glow twice, a ring, and a rain of stars and twinkles
    evolution(F){
      const T = F.T;
      for (let i = 0; i < 2; i++)
        F.anime("aura", T * 2, 900, (im, t, h) => { h(T * (1.05 + .9 * sortie(t))); im.setAlpha(.9 * cloche(t)); },
          {add: true, tint: 0xFFE38A, delai: i * 650, dz: -.1});
      F.anime("anneau", T * 1.4, 650, (im, t, h) => { h(T * (.4 + sortie(t))); im.setAlpha(.9 * (1 - entree(t))); }, {tint: 0xFFE38A, delai: 200, dz: .2});
      const pluie = new Phaser.Geom.Rectangle(-T * .85, -T * 1.1, T * 1.7, T * .3);
      F.gerbe("etoile", {n: 20, cadence: 55, zone: pluie, vitesse: [T * .45, T * .8], angle: [80, 100], gravite: T * .9,
        taille: [T * .09, T * .15], ms: [1100, 1450], tourne: 300, fondu: "doux", dz: .4});
      F.gerbe("etincelle", {n: 14, cadence: 80, zone: pluie, vitesse: [T * .35, T * .6], angle: [85, 95], gravite: T * .6,
        taille: [T * .1, T * .17], ms: [1000, 1300], forme: "scintille", fondu: "doux", droit: true, dz: .5});
    }
  };

  function jouer(S, nom, x, y, taille, prof){
    const r = RECETTES[nom];
    if (!r || !S || !S.add || !S.textures || !(taille > 0)) return 0;
    lisser(S);
    const F = outils(S, x, y, taille, prof || 0);
    r(F);
    return Math.round(F.duree());
  }
  // the same tools for effects written elsewhere (js/jeu/attaques_hd.js: the families' attacks)
  const preparer = (S, x, y, taille, prof) => { lisser(S); return outils(S, x, y, taille, prof || 0); };
  return {jouer, preparer, noms: Object.keys(RECETTES)};
})();
