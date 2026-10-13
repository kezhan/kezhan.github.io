/* Académia : la place de l'Arbre-Monde (GDD §2), au milieu du village (js/jeu/monde.js).
   1. L'Arbre-Monde, sur la rosace : gris et endormi au départ, il reverdit à chaque badge (sept dessins, du gris endormi
      à l'arbre en fête : outils/village/scripts/place_arbre.py) ; ses lumières battent ; touché, il frémit et la voix
      dit où il en est.
   2. Ce que chaque chef consolé rend à la place, dessiné avant et après (scripts/place_objets.py) : le Dojo rallume les
      lanternes, Albion fait couler la fontaine, Germania remet l'horloge à l'heure vraie, le Grand-Duché fait revenir
      les oiseaux, le Jardin fait fleurir les bacs, l'Observatoire pose des étoiles autour de l'arbre.
   3. Le panneau des missions du Sage (js/missions.js) : ses trois ronds montrent les missions du jour, cochées une à
      une, et un point d'exclamation tant que l'enfant ne les a pas regardées.
   Toucher l'arbre ou le panneau : le héros y va, puis l'arbre parle ou les missions s'ouvrent. La place montre ce que
   l'enfant a déjà vu (P().histoire) : js/jeu/histoire_village.js met en scène le reste (grandir, rendre).
   Images : assets/hd/place (8 pixels par unité du monde) et assets/leger/place (4), posées par leur ancre. */
const PLACE_IMAGES = ["lanterne_eteinte", "fontaine", "fontaine_seche", "goutte_eau", "horloge", "horloge_arretee", "aiguille_heures",
  "aiguille_minutes", "oiseau", "oiseau_bleu", "bac_fleurs", "bac_fleurs_vide", "panneau_missions"];
// each drawing's anchor (the middle of the bottom of its tiles), as outils/village/scripts/place.py draws it
const PLACE_ANCRES = {arbre_monde: [.5, .9747], lanterne_eteinte: [.5, 1], fontaine: [.5, .9524], fontaine_seche: [.5, .9524],
  goutte_eau: [.5, .5], horloge: [.5, .9643], horloge_arretee: [.5, .9643], aiguille_heures: [.5, .6923], aiguille_minutes: [.5, .7576],
  oiseau: [.5, .9286], oiseau_bleu: [.5, .9286], bac_fleurs: [.5, .925], bac_fleurs_vide: [.5, .925], panneau_missions: [.5, .9571]};
const ARBRE_ECHELLE = .66, ARBRE_ETATS = 7;
// the tree's lights in HD pixels of its drawing (place_arbre.py, LUMIERES), its foot at (368, 772)
const ARBRE_LUMIERES = [[150, 448], [262, 494], [368, 516], [474, 494], [586, 448], [218, 300], [518, 300], [368, 214]];
const ARBRE_COULEURS = [0xFFD84D, 0xFF9CC8, 0x7FD3FF, 0xFFD84D, 0xFF9CC8, 0xFFE680, 0xB9F27A, 0xFFD84D];
const HORLOGE_CADRAN = -29, PANNEAU_RONDS = [[-5.5, -15.75], [0, -15.75], [5.5, -15.75]];   // world units from the foot
const OISEAUX = [[19.4, 16.55, "oiseau"], [24.3, 14.6, "oiseau_bleu"], [23.6, 17.5, "oiseau"]];   // where the birds peck, in tiles
// what each beaten chief gives back to the square, and what the voice says when it happens
const CADEAUX = {
  dojo: "Le Grand Vizir des Nombres est consolé : les lanternes de la place se rallument !",
  albion: "Le Roi du Quotidien est consolé : la fontaine coule de nouveau !",
  germania: "Le Dragon du Temps est consolé : l'horloge de la place repart !",
  duche: "L'Esprit des Traditions est consolé : les oiseaux reviennent sur la place !",
  jardin: "Le Gardien des Énigmes est consolé : les bacs de la place se couvrent de fleurs !",
  observatoire: "Le Maître des Astres est consolé : des étoiles brillent autour de l'Arbre-Monde !"
};

// the regions whose chief is beaten (a badge) and the tree's state, one step per badge
const chefsConsoles = (p = P()) => REGIONS.filter(r => ((p && p.regions) || {})[r.id] && p.regions[r.id].badges > 0).map(r => r.id);
const etatArbre = (p = P()) => Math.min(ARBRE_ETATS - 1, chefsConsoles(p).length);
// what the child has already seen on the square (P().histoire: id -> ISO date)
const vuHistoire = id => { const p = P(); return !!(p && p.histoire && p.histoire[id]); };
const arbreVu = () => { let k = 0; for (let e = 1; e < ARBRE_ETATS; e++) if (vuHistoire("arbre_" + e)) k = e; return k; };
const unitePlace = () => (QUALITE === "leger" ? 2 : 1) / 8;   // world units per pixel of the square's drawings
const dossierPlace = () => `assets/${QUALITE === "leger" ? "leger" : "hd"}/place/`;

// the square's drawings, the icons of the board's missions (js/icones.js), and the tree in the states asked (the next
// one comes in the background)
function chargerPlaceHD(load, etats = [arbreVu()]){
  PLACE_IMAGES.forEach(n => { if (!load.textureManager.exists("pl_" + n)) load.image("pl_" + n, dossierPlace() + n + ".png"); });
  ["coche", "etoile", "dodo", ...ICONES_MISSIONS].forEach(n => { if (!load.textureManager.exists("ico_" + n)) load.image("ico_" + n, iconeSrc(n)); });
  etats.forEach(e => { if (e >= 0 && e < ARBRE_ETATS && !load.textureManager.exists("pl_arbre_monde_" + e)) load.image("pl_arbre_monde_" + e, `${dossierPlace()}arbre_monde_${e}.png`); });
}
function poserPlace(scene, nom, x, y, prof, echelle = 1){
  const cle = "pl_" + nom; if (!scene.textures.exists(cle)) return null;
  scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
  const a = PLACE_ANCRES[nom.replace(/_\d+$/, "")] || [.5, 1];
  return scene.add.image(x, y, cle).setOrigin(a[0], a[1]).setDepth(prof).setScale(echelle * unitePlace());
}
// a drawing is under the finger (its own pixels, or within 3 units: a finger is wide)
function sousLeDoigt(scene, im, wx, wy){
  if (!im || !im.active || !im.visible || !im.getBounds().contains(wx, wy)) return false;
  return [[0, 0], [3, 0], [-3, 0], [0, 3], [0, -3]].some(([a, b]) => {
    const px = (wx + a - im.x) / im.scaleX + im.displayOriginX, py = (wy + b - im.y) / im.scaleY + im.displayOriginY;
    return px >= 0 && py >= 0 && px < im.frame.realWidth && py < im.frame.realHeight
      && (scene.textures.getPixelAlpha(Math.floor(px), Math.floor(py), im.texture.key, im.frame.name) || 0) > 60;
  });
}

class ArbreMonde {
  constructor(scene){
    this.s = scene; this.etat = -1; this.rendus = new Set(); this.vise = null; this.anim = !calme();
    const el = t => scene.carte.elements.filter(e => e.type === t), pied = e => ({x: (e.x + e.w / 2) * CASE, y: (e.y + e.h) * CASE - 1, e});
    const a = el("arbre_monde")[0], b = el("panneau_missions")[0];
    this.arbrePied = a ? {x: (a.x + a.w / 2) * CASE, y: (a.y + a.h) * CASE - 8, ligne: a.y + a.h - 1} : null;   // the rosette's centre
    this.devantArbre = a ? [{x: a.x, y: a.y + a.h}, {x: a.x + 1, y: a.y + a.h}] : [];
    this.lieux = {fontaine: el("fontaine").map(pied), horloge: el("horloge").map(pied), bacs: el("bac").map(pied)};
    this.panneauPied = b ? pied(b) : null; this.devantPanneau = b ? {x: b.x, y: b.y + b.h} : null;
    this.objets = {fontaine: [], horloge: [], bacs: [], oiseaux: [], etoiles: [], lumieres: [], ronds: []};
    this.arbre(arbreVu());
    chefsConsoles().filter(r => vuHistoire("cadeau_" + r)).forEach(r => this.rendus.add(r));
    ["dojo", "albion", "germania", "duche", "jardin", "observatoire"].forEach(r => this.cadeau(r, false));
    this.panneau();
    // the missions' little drawings of the garden come with js/jeu/jardin_village.js: the board is drawn again then
    const plus = () => this.majPanneau();
    scene.load.on("complete", plus); scene.events.once("shutdown", () => scene.load.off("complete", plus));
    if (this.anim) scene.time.addEvent({delay: 20000, loop: true, callback: () => this.aiguilles()});
  }
  // ---------- the tree ----------
  arbre(k){
    const s = this.s, p = this.arbrePied; if (!p) return;
    if (!s.textures.exists("pl_arbre_monde_" + k)) return this.charger([k], () => this.arbre(k));
    if (!this.im) {
      this.im = poserPlace(s, "arbre_monde_" + k, p.x, p.y, profondeur(p.ligne, 0), ARBRE_ECHELLE);
      const r = s.villageHD;
      if (this.im && r) { this.im.bornes = this.im.getBounds(); r.ecrans.push(this.im); }   // the hero behind it shows through
    } else this.im.setTexture("pl_arbre_monde_" + k);
    if (!this.im) return;
    this.im.caches = {}; if (s.villageHD) s.villageHD.devoile = null;
    this.etat = k;
    this.lumieres(k >= 4);
    // ready for the next badge, loaded once the village is settled (never in the way of its first moments)
    if (k + 1 < ARBRE_ETATS && !this.suivant) this.suivant = s.time.delayedCall(6000, () => { this.suivant = null; if (this.etat + 1 < ARBRE_ETATS) this.charger([this.etat + 1], () => {}); });
  }
  // a state of the tree that is not there yet: loaded, then fn
  charger(etats, fn){
    const l = this.s.load, avant = l.list.size;
    chargerPlaceHD(l, etats);
    if (l.list.size === avant && !l.isLoading()) return fn();
    l.once("complete", () => { if (this.s.sys.isActive() || this.s.sys.isSleeping()) fn(); });
    if (!l.isLoading()) l.start();
  }
  // the tree a little more awake: a burst of light and leaves, the new drawing appears over the old one
  grandir(k){
    const s = this.s, p = this.arbrePied; if (!p || !this.im) return this.arbre(k);
    const avant = this.im, k8 = ARBRE_ECHELLE / 8, haut = p.y - 792 * .6 * k8;
    if (!s.textures.exists("pl_arbre_monde_" + k)) return this.charger([k], () => this.grandir(k));
    if (this.anim) {
      const vieux = s.add.image(avant.x, avant.y, avant.texture.key).setOrigin(avant.originX, avant.originY).setScale(avant.scale).setDepth(avant.depth + .01);
      s.tweens.add({targets: vieux, alpha: 0, duration: 900, ease: "Sine.easeIn", onComplete: () => vieux.destroy()});
      s.tweens.add({targets: avant, scaleY: avant.scale * 1.04, duration: 260, yoyo: true, ease: "Sine.easeOut"});
      effetHD(s, "evolution", p.x, haut, 40, avant.depth + .3);
      for (let i = 0; i < 6; i++) effetHD(s, "toucher", p.x + (Math.random() - .5) * 46, haut + (Math.random() - .5) * 30, 10, avant.depth + .3);
    }
    if (typeof jouerSon === "function") jouerSon("magie");
    this.arbre(k);
  }
  // the lights hanging in the crown (from the fourth badge) beat softly, each in its colour
  lumieres(on){
    this.objets.lumieres.forEach(o => o.destroy()); this.objets.lumieres = [];
    const s = this.s, p = this.arbrePied, k8 = ARBRE_ECHELLE / 8;
    if (!on || !p || !this.im) return;
    ARBRE_LUMIERES.forEach(([hx, hy], i) => {
      const g = VILLAGE_HD.poser(s, "lueur", p.x + (hx - 368) * k8, p.y + (hy - 772) * k8, this.im.depth + .05, {echelle: .5});
      if (!g) return;
      g.setBlendMode(Phaser.BlendModes.ADD).setTint(ARBRE_COULEURS[i]).setAlpha(.7);
      if (this.anim) s.tweens.add({targets: g, alpha: .25, duration: 900 + i * 130, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      this.objets.lumieres.push(g);
    });
  }
  // the tree touched: the hero goes in front of it (true: the touch was for it)
  touche(wx, wy){
    const s = this.s, x = Math.floor(wx / CASE), y = Math.floor(wy / CASE);
    if (s.pnj.some(q => q.x === x && (q.y === y || q.y === y + 1))) return false;   // a villager in front: him first
    const vers = (lieu, devant) => {
      s.marquer(devant.x, devant.y); this.vise = lieu;
      if (s.case.x === devant.x && s.case.y === devant.y) return this.arrive();
      s.allerVers(devant.x, devant.y);
      return true;
    };
    if (this.panneauIm && sousLeDoigt(s, this.panneauIm, wx, wy)) return vers("panneau", this.devantPanneau);
    if (this.im && sousLeDoigt(s, this.im, wx, wy)) {
      const d = this.devantArbre.reduce((a, b) => s.dist(b) < s.dist(a) ? b : a, this.devantArbre[0]);
      return vers("arbre", d);
    }
    return false;
  }
  // the hero has walked there (Monde.arrive): the tree speaks, or the missions open
  arrive(){
    const lieu = this.vise; this.vise = null;
    if (!lieu) return false;
    const s = this.s, d = lieu === "panneau" ? [this.devantPanneau] : this.devantArbre;
    if (!d.some(q => q && q.x === s.case.x && q.y === s.case.y)) return true;
    s.chemin = []; s.pnjVise = null; s.arreter();
    if (lieu === "panneau") ouvrirMissions();
    else this.frissonner();
    return true;
  }
  frissonner(){
    const s = this.s, im = this.im, p = this.arbrePied; if (!im) return;
    if (this.anim && !this.tremble) {
      this.tremble = s.tweens.add({targets: im, angle: {from: -1.6, to: 1.6}, duration: 120, yoyo: true, repeat: 2, ease: "Sine.easeInOut",
        onComplete: () => { im.setAngle(0); this.tremble = null; }});
      for (let i = 0; i < 5; i++) effetHD(s, "toucher", p.x + (Math.random() - .5) * 44, p.y - 38 + (Math.random() - .5) * 24, 9, im.depth + .3);
    }
    if (typeof jouerSon === "function") jouerSon("scintille");
    if (s.histoire) s.histoire.parlerArbre();
  }
  // ---------- what the chiefs give back ----------
  // drawn as given back (`rendu`) or not yet; `region` given back now: js/jeu/histoire_village.js plays it
  rendre(region){ this.rendus.add(region); this.cadeau(region, true); }
  // where the camera looks when a gift is given back
  lieuCadeau(region){
    const l = this.lieux, p = this.arbrePied || {x: 22 * CASE, y: 15 * CASE};
    const lumieres = (this.s.villageHD ? this.s.villageHD.lumieres : []).filter(q => q.im);
    const centre = a => a.length ? {x: a.reduce((t, q) => t + q.x, 0) / a.length, y: a.reduce((t, q) => t + q.y, 0) / a.length} : null;
    const lieu = {dojo: centre(lumieres.map(q => ({x: q.im.x, y: q.im.y - 10}))), albion: l.fontaine[0] && {x: l.fontaine[0].x, y: l.fontaine[0].y - 18},
      germania: l.horloge[0] && {x: l.horloge[0].x, y: l.horloge[0].y - 22}, duche: {x: p.x, y: p.y + 6}, jardin: {x: p.x, y: p.y},
      observatoire: {x: p.x, y: p.y - 34}}[region];
    return lieu || {x: p.x, y: p.y - 20};
  }
  cadeau(region, maintenant){
    const s = this.s, on = this.rendus.has(region), eclat = (x, y, t) => { if (maintenant && this.anim) effetHD(s, "evolution", x, y, t, 9500); };
    const vider = l => { l.forEach(o => { s.tweens.killTweensOf(o); o.destroy(); }); l.length = 0; };
    if (region === "dojo") (s.villageHD ? s.villageHD.lumieres : []).forEach(l => {   // the square's lanterns
      if (!l.im) return;
      if (!l.piece) l.piece = {cle: l.im.texture.key, cadre: l.im.frame.name, echelle: l.im.scale};
      if (on) l.im.setTexture(l.piece.cle, l.piece.cadre).setScale(l.piece.echelle);
      else if (s.textures.exists("pl_lanterne_eteinte")) l.im.setTexture("pl_lanterne_eteinte").setScale(unitePlace());
      if (l.lueur) l.lueur.setVisible(on);
      l.im.caches = {};
      if (on) eclat(l.im.x, l.im.y - 14, 14);
    });
    if (region === "albion") { vider(this.objets.fontaine); this.lieux.fontaine.forEach(f => this.fontaine(f, on)); if (on) this.lieux.fontaine.forEach(f => eclat(f.x, f.y - 18, 30)); }
    if (region === "germania") { vider(this.objets.horloge); this.lieux.horloge.forEach(h => this.horloge(h, on)); if (on) this.lieux.horloge.forEach(h => eclat(h.x, h.y + HORLOGE_CADRAN, 20)); }
    if (region === "jardin") {
      vider(this.objets.bacs);
      this.lieux.bacs.forEach(b => { const im = poserPlace(s, on ? "bac_fleurs" : "bac_fleurs_vide", b.x, b.y, profondeur(b.e.y, 0)); if (im) this.objets.bacs.push(im); if (on) eclat(b.x, b.y - 10, 14); });
    }
    if (region === "duche") { vider(this.objets.oiseaux); if (on) this.oiseaux(maintenant); }
    if (region === "observatoire") { vider(this.objets.etoiles); if (on) this.etoiles(maintenant); }
    if (s.villageHD) s.villageHD.devoile = null;
  }
  // the fountain, dry or flowing; flowing, drops jump from its bowl
  fontaine(f, coule){
    const s = this.s, im = poserPlace(s, coule ? "fontaine" : "fontaine_seche", f.x, f.y, profondeur(f.e.y + f.e.h - 1, 0));
    if (!im) return;
    im.bornes = im.getBounds(); if (s.villageHD) s.villageHD.ecrans.push(im);
    this.objets.fontaine.push(im);
    if (!coule || !this.anim || !s.textures.exists("pl_goutte_eau")) return;
    const ev = s.time.addEvent({delay: 240, loop: true, callback: () => {
      const sens = Math.random() < .5 ? -1 : 1, d = poserPlace(s, "goutte_eau", f.x + sens * 2, f.y - 32, im.depth + .1, .8 + Math.random() * .4);
      if (!d) return;
      const dx = sens * (5 + Math.random() * 7);
      s.tweens.add({targets: d, x: f.x + dx, duration: 620, ease: "Linear"});
      s.tweens.add({targets: d, y: f.y - 38, duration: 200, ease: "Quad.easeOut", onComplete: () =>
        s.tweens.add({targets: d, y: f.y - 15, alpha: .4, duration: 420, ease: "Quad.easeIn", onComplete: () => d.destroy()})});
    }});
    this.objets.fontaine.push({destroy: () => ev.remove()});
  }
  // the clock, stopped and grey, or alive with its hands at the real time
  horloge(h, vivante){
    const s = this.s, im = poserPlace(s, vivante ? "horloge" : "horloge_arretee", h.x, h.y, profondeur(h.e.y, 0));
    if (!im) return;
    im.bornes = im.getBounds(); if (s.villageHD) s.villageHD.ecrans.push(im);
    this.objets.horloge.push(im);
    if (!vivante) return;
    const a = poserPlace(s, "aiguille_heures", h.x, h.y + HORLOGE_CADRAN, im.depth + .1), m = poserPlace(s, "aiguille_minutes", h.x, h.y + HORLOGE_CADRAN, im.depth + .2);
    this.objets.horloge.push(...[a, m].filter(Boolean));
    this.mains = {heures: a, minutes: m};
    this.aiguilles();
  }
  aiguilles(){
    const m = this.mains; if (!m) return;
    const t = new Date(), mn = t.getMinutes() + t.getSeconds() / 60;
    if (m.heures && m.heures.active) m.heures.setAngle((t.getHours() % 12) * 30 + mn / 2);
    if (m.minutes && m.minutes.active) m.minutes.setAngle(mn * 6);
  }
  // the birds back on the square: they hop, peck, and turn round now and then
  oiseaux(maintenant){
    const s = this.s;
    OISEAUX.forEach(([tx, ty, nom], i) => {
      const x = tx * CASE, y = ty * CASE, o = poserPlace(s, nom, maintenant && this.anim ? x + (i - 1) * 60 : x, maintenant && this.anim ? y - 70 : y, profondeur(Math.floor(ty), 3), .95);
      if (!o) return;
      o.setFlipX(i % 2 === 1); this.objets.oiseaux.push(o);
      if (!this.anim) return;
      const vivre = () => s.time.delayedCall(1200 + Math.random() * 2600, () => {
        if (!o.active) return;
        if (Math.random() < .35) { o.setFlipX(!o.flipX); return vivre(); }
        if (Math.random() < .5) return s.tweens.add({targets: o, angle: o.flipX ? -28 : 28, duration: 110, yoyo: true, repeat: 1, onComplete: () => { o.setAngle(0); vivre(); }});
        const nx = x + (Math.random() - .5) * 14, ny = y + (Math.random() - .5) * 5;
        o.setFlipX(nx < o.x);
        s.tweens.add({targets: o, x: nx, duration: 320, ease: "Linear"});
        s.tweens.add({targets: o, y: ny - 3, duration: 160, yoyo: true, ease: "Quad.easeOut", onComplete: () => { o.y = ny; vivre(); }});
      });
      if (maintenant) s.tweens.add({targets: o, x, y, duration: 1100 + i * 200, ease: "Sine.easeOut", onComplete: vivre});   // they fly in
      else vivre();
    });
  }
  // the Observatoire's stars floating around the crown, twinkling
  etoiles(maintenant){
    const s = this.s, p = this.arbrePied; if (!p) return;
    // around the top of the crown (about 60 units wide, 63 high), each star in its soft halo; none low on the left,
    // where the Sage's head is (40 units left of the foot, 31 up)
    [[-28, -62], [28, -60], [-12, -76], [14, -80], [36, -40], [-2, -70]].forEach(([dx, dy], i) => {
      const c = [0xFFF3B0, 0xBFE6FF, 0xFFD45C][i % 3], x = p.x + dx, y = p.y + dy, d = profondeur(15, 9);
      const h = VILLAGE_HD.poser(s, "lueur", x, y, d - .01, {echelle: .55});
      if (h) { h.setBlendMode(Phaser.BlendModes.ADD).setTint(c).setAlpha(.6); this.objets.etoiles.push(h); }
      const e = VILLAGE_HD.poser(s, "eclat", x, y, d, {echelle: 1.9 + (i % 3) * .35});
      if (!e) return;
      e.setTint(c); this.objets.etoiles.push(e);
      if (!this.anim) return;
      if (maintenant) [e, h].filter(Boolean).forEach(o => { const k = o.scale; o.setScale(0); s.tweens.add({targets: o, scale: k, duration: 500, delay: i * 150, ease: "Back.easeOut"}); });
      s.tweens.add({targets: e, y: y - 3, angle: 45, duration: 1300 + i * 170, yoyo: true, repeat: -1, ease: "Sine.easeInOut", delay: i * 120});
      if (h) s.tweens.add({targets: h, alpha: .25, duration: 1100 + i * 150, yoyo: true, repeat: -1, ease: "Sine.easeInOut", delay: i * 90});
    });
  }
  // ---------- the board of the day's missions ----------
  panneau(){
    const s = this.s, b = this.panneauPied; if (!b) return;
    this.panneauIm = poserPlace(s, "panneau_missions", b.x, b.y, profondeur(b.e.y, 0));
    if (this.panneauIm) { this.panneauIm.bornes = this.panneauIm.getBounds(); if (s.villageHD) s.villageHD.ecrans.push(this.panneauIm); }
    this.majPanneau();
  }
  // each round shows a mission's drawing, a gold tick once it is done; a bouncing « ! » while new ones wait to be seen
  majPanneau(){
    const s = this.s, b = this.panneauPied, im = this.panneauIm;
    this.objets.ronds.forEach(o => { s.tweens.killTweensOf(o); o.destroy(); }); this.objets.ronds = [];
    if (!b || !im || !s.sys.isActive() && !s.sys.isSleeping()) return;
    const liste = typeof missionsDuJour === "function" ? missionsDuJour() : [], d = im.depth;
    liste.slice(0, PANNEAU_RONDS.length).forEach((m, i) => {
      const [dx, dy] = PANNEAU_RONDS[i], cle = cleDessinMission(m, s);
      if (cle) { const x = s.add.image(b.x + dx, b.y + dy, cle).setDepth(d + .01); x.setScale(3.6 / Math.max(x.width, x.height)); this.objets.ronds.push(x); }
      if (m.fait && s.textures.exists("ico_coche")) { const c = s.add.image(b.x + dx + 1.6, b.y + dy + 1.4, "ico_coche").setDepth(d + .02); c.setScale(2.6 / c.height); this.objets.ronds.push(c); }
    });
    if (liste.length && typeof missionsVues === "function" && !missionsVues() && s.textures.exists("ico_etoile")) {
      const x = b.x + 9, y = b.y - 33, e = s.add.image(x, y, "ico_etoile").setDepth(9400);
      e.setScale(6 / e.height); this.objets.ronds.push(e);
      if (this.anim) s.tweens.add({targets: e, y: y - 2.5, scale: e.scale * 1.15, duration: 420, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
  }
}
