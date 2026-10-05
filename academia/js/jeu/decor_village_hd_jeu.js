/* Académia : ce que le jeu appelle pendant qu'on marche dans le village en haute définition (js/jeu/decor_village_hd.js).
   1. maisonToucheeHD : un toucher sur le dessin d'une maison (son toit compris), sur le panneau du Dojo ou sur le
      panneau d'une maison désigne cette maison : on va à sa porte.
   2. devoilerHD : un dessin placé devant le héros ou son compagnon, qui recouvre son corps, garde ses couleurs : leur
      silhouette se dessine par-dessus (js/jeu/silhouettes_hd.js) ; sans carte graphique, il passe à 75 % seulement si
      le héros se tient vraiment derrière lui. Un panneau qu'ils touchent devient presque transparent. On l'appelle à
      chaque image : il ne refait rien tant que personne ne bouge.
   3. panneauVillageHD et ecrirePanneauHD : le nom d'une maison, texte sur une pastille arrondie, au-dessus du panneau
      planté devant elle (js/jeu/decor_village_hd.js), sinon au-dessus de la maison ; il n'apparaît que quand le héros
      s'approche (à trois cases du panneau ou de la porte). cadrerPanneauxHD le montre ou le cache, et le garde à
      l'écran à chaque image : jamais sous la barre du haut (il passe sous le panneau), jamais coupé par un bord (il
      glisse vers l'intérieur, ou se cache quand plus de la moitié sortirait).
   4. etoilesPanneauHD : les étoiles d'or dans les creux du panneau planté, une par Ombre battue près de la maison ;
      celle qui vient de s'allumer saute.
   5. ombreAuSolHD : l'ombre douce au sol sous un personnage, qui le suit. */

function maisonToucheeHD(scene, wx, wy){ return VILLAGE_HD_JEU.touchee(scene, wx, wy); }
function devoilerHD(scene, persos, panneaux){ VILLAGE_HD_JEU.devoiler(scene, persos, panneaux); }
function cadrerPanneauxHD(scene, panneaux, frais){ VILLAGE_HD_JEU.cadrer(scene, panneaux, frais); }
// a house's sign at (x, y) in world units (where it goes when the village is not drawn in high definition)
function panneauVillageHD(scene, region, x, y, texte = ""){ return VILLAGE_HD_JEU.panneau(scene, region, x, y, texte); }
function ecrirePanneauHD(panneau, texte){ VILLAGE_HD_JEU.ecrire(panneau, texte); }
function etoilesPanneauHD(scene, region, n){ VILLAGE_HD_JEU.etoiles(scene, region, n); }
function ombreAuSolHD(scene, sprite){ return VILLAGE_HD_JEU.ombre(scene, sprite); }

const VILLAGE_HD_JEU = (() => {
  const duree = () => (typeof TEST !== "undefined" && TEST ? 1 : 150);

  // the alpha (0 to 255) of a drawing at a world point, read in its texture (mirrored drawings too)
  function alpha(scene, im, wx, wy){
    const f = im.frame;
    let px = (wx - im.x) / im.scaleX + im.displayOriginX;
    const py = (wy - im.y) / im.scaleY + im.displayOriginY;
    if (im.flipX) px = f.realWidth - px;
    if (px < 0 || py < 0 || px >= f.realWidth || py >= f.realHeight) return 0;
    return scene.textures.getPixelAlpha(Math.floor(px), Math.floor(py), im.texture.key, f.name) || 0;
  }
  // a finger is wide: the drawing counts if it is there or within 3 units around
  const sous = (scene, im, wx, wy) => [[0, 0], [3, 0], [-3, 0], [0, 3], [0, -3]].some(([a, b]) => alpha(scene, im, wx + a, wy + b) > 60);

  function touchee(scene, wx, wy){
    const r = scene.villageHD; if (!r) return null;
    const p = (r.panneaux || []).find(q => q.active && q.visible && q.alpha > .6 && q.getBounds().contains(wx, wy));
    if (p) return p.region;
    let m = null;
    r.maisons.forEach(q => { if (q.bornes.contains(wx, wy) && sous(scene, q.im, wx, wy) && (!m || q.im.depth > m.im.depth)) m = q; });
    return m ? m.region : null;
  }

  // the body of a character standing in its tile (feet at its origin, drawn upward): a box, and twelve points over it
  const corps = p => {
    const w = p.displayWidth, h = p.displayHeight, x = Math.floor(p.x / CASE) * CASE + 8, y = Math.floor(p.y / CASE) * CASE + 15;
    const points = [];
    [-.25, 0, .25].forEach(a => [.2, .4, .6, .8].forEach(b => points.push([x + a * w, y - b * h])));
    return {x, y, boite: new Phaser.Geom.Rectangle(x - w * .3, y - h * .95, w * .6, h * .85), points};
  };
  const voiler = (scene, o, cible) => {
    if (o.voile === cible || (o.voile === undefined && cible === 1)) return;
    o.voile = cible;
    if (o.voileTw) o.voileTw.stop();
    o.voileTw = scene.tweens.add({targets: o, alpha: cible, duration: duree()});
  };
  // a drawing hides a character when it stands in front of it and covers a sixth of its body (worked out once per tile)
  const cache = (scene, im, c, p) => {
    const k = `${c.x},${c.y},${Math.round(c.boite.height)}`, m = im.caches || (im.caches = {});
    if (m[k] === undefined) m[k] = Phaser.Geom.Rectangle.Overlaps(im.bornes, c.boite) && c.points.filter(([x, y]) => alpha(scene, im, x, y) > 100).length >= 2;
    return im.depth > p.depth && m[k];
  };
  // really behind it: the character's tile is under the drawing, not just beside it
  const derriere = (scene, im, c) => cache(scene, im, c, c.p) && c.x > im.bornes.left + 2 && c.x < im.bornes.right - 2;
  function devoiler(scene, persos, panneaux = []){
    const r = scene.villageHD; if (!r) return;
    const ps = persos.filter(p => p && p.active), cs = ps.map(p => ({p, ...corps(p)}));
    const sil = !!(scene.silhouettes && scene.silhouettes.ok);
    const cle = cs.map(c => `${c.x},${c.y},${c.p.depth},${c.p.texture.key}`).join(";") + "|" + panneaux.length;
    if (cle === r.devoile) return;
    r.devoile = cle;
    if (sil) scene.silhouettes.devant(cs.map(c => ({p: c.p, ecrans: r.ecrans.filter(im => cache(scene, im, c, c.p))})));
    else r.ecrans.forEach(im => voiler(scene, im, cs.some(c => derriere(scene, im, c)) ? .75 : 1));
    panneaux.forEach(pa => {
      if (!pa || !pa.active) return;
      const b = pa.getBounds();
      voiler(scene, pa, cs.some(c => Phaser.Geom.Rectangle.Overlaps(b, c.boite)) ? .25 : 1);
    });
  }

  // the top bar's pill and button, in CSS pixels, read once per frame (the pill grows with the sparkles won in a fight);
  // while a menu hides the bar, the village keeps the places it had with it (it comes back with the village)
  let barre = {image: -1, rects: []};
  const rectsBarre = (image, frais) => {
    if ((frais || image !== barre.image) && !$("barre").hidden) barre = {image, rects: [".gardien", "#btnEquipe"].map(s => document.querySelector(s))
      .filter(Boolean).map(e => e.getBoundingClientRect()).filter(b => b.width)};
    return barre.rects;
  };
  // the camera's view as it will be drawn at this frame: worldView is only worked out when the frame is drawn, the
  // scroll is already there (a pan moves it before the scene's update, a follow just before the frame is drawn:
  // Monde calls cadrer again then, on the camera's « followupdate »)
  const vue = cam => ({x: cam.scrollX + cam.width * cam.originX * (1 - 1 / cam.zoom), y: cam.scrollY + cam.height * cam.originY * (1 - 1 / cam.zoom)});
  // every frame: each sign where it was placed (p.ideal) unless the top bar's pill or button, or the top edge, would
  // cover it: then it goes under its house (p.dessous), and hides if that is covered too. Cut by a side or the bottom:
  // it slides in, or hides when more than half of it would be out
  // the hero is within three tiles of the house's sign or of one of its doors (always, without the drawn village)
  const PRES = 3;
  function proche(scene, region){
    const h = scene.case, r = scene.villageHD;
    if (!h || !r || !r.enseignes) return true;
    const pres = (x, y) => Math.max(Math.abs(x - h.x), Math.abs(y - h.y)) <= PRES, e = r.enseignes[region];
    return !!(e && pres(e.tx, e.ty)) || scene.carte.portes.some(q => q.region === region && pres(q.x, q.y));
  }
  // the name fades in as the hero comes near, out as he goes (its pill, its text and its icons: the container's alpha is
  // devoiler's)
  function montrer(scene, p, pres){
    p.pres = pres;
    const l = [p.texte, p.pastille, ...(p.icones || [])];
    scene.tweens.killTweensOf(l);
    if (pres) p.setVisible(true);
    if (calme()) { l.forEach(o => o.setAlpha(pres ? 1 : 0)); if (!pres) p.setVisible(false); return; }
    scene.tweens.add({targets: l, alpha: pres ? 1 : 0, duration: 220, onComplete: () => { if (!p.pres && p.active) p.setVisible(false); }});
  }
  function cadrer(scene, panneaux, frais){
    const cam = scene.cameras.main, v = vue(cam), k = cam.zoom / RATIO;
    const W = scene.scale.width / RATIO, H = scene.scale.height / RATIO, m = 6, rects = rectsBarre(scene.game.loop.frame, frais);
    panneaux.forEach(p => {
      if (!p || !p.active || !p.ideal) return;
      const pres = proche(scene, p.region);
      if (pres !== p.pres) montrer(scene, p, pres);
      if (!pres && !p.visible) return;   // gone; one that is fading still keeps clear of the bar and the edges
      const w = p.largeur * k, h = p.hauteur * k;
      const couvert = (sx, sy) => sy - h / 2 < m || rects.some(b => sx + w / 2 > b.left - 4 && sx - w / 2 < b.right + 4 && sy - h / 2 < b.bottom + 4);
      const ici = q => {
        let sx = (q.x - v.x) * k, sy = (q.y - v.y) * k;
        const dx = Math.max(0, m - (sx - w / 2)) - Math.max(0, sx + w / 2 - (W - m)), dyBas = Math.max(0, sy + h / 2 - (H - m));
        if (Math.abs(dx) > w / 2 || dyBas > h / 2) return null;
        sx += dx; sy -= dyBas;
        return couvert(sx, sy) ? null : {x: v.x + sx / k, y: v.y + sy / k};
      };
      const q = ici(p.ideal) || (p.dessous && ici(p.dessous));
      if (!q) return p.setVisible(false);
      p.setVisible(true).setPosition(q.x, q.y);
    });
  }

  // n gold stars in the hollows of the house's sign; the ones just lit pop
  function etoiles(scene, region, n){
    const r = scene.villageHD, e = r && r.enseignes && r.enseignes[region];
    if (!e) return;
    if (!e.etoiles) e.etoiles = [1, 2, 3, 4].map(i => {
      const q = VILLAGE_HD.point(e.piece, "etoile" + i, e.x, e.y), s = q && VILLAGE_HD.poser(scene, "etoile_panneau", q.x, q.y, e.im.depth + .02);
      if (s) { s.base = s.scale; s.setVisible(false); }
      return s;
    }).filter(Boolean);
    const avant = e.allumees === undefined ? -1 : e.allumees;
    e.allumees = n;
    e.etoiles.forEach((s, i) => {
      const on = i < n;
      s.setVisible(on);
      if (!on || avant < 0 || i < avant || calme()) return;
      s.setScale(0);
      scene.tweens.add({targets: s, scale: s.base, duration: 420, delay: (i - avant) * 150, ease: "Back.easeOut"});
      effetHD(scene, "toucher", s.x, s.y, 6, s.depth + .1);
    });
  }

  // where a sign of this width goes: above its house (depart), slid sideways off the vertical paths it would cover,
  // then out of the tall grass (on the nearer side: an Ombre's eyes stay in sight), slid again if a villager is under it
  function placer(rendu, region, depart, largeur = 116, hauteur = 12){
    const carte = rendu.carte, demi = largeur / 2, dh = hauteur / 2;
    let {x, y} = depart;
    carte.elements.filter(c => c.type === "chemin" && c.h > c.w).forEach(c => {
      const x0 = c.x * CASE, x1 = (c.x + c.w) * CASE;
      if (y + dh > c.y * CASE && y - dh < (c.y + c.h) * CASE && x + demi > x0 && x - demi < x1) x = x < (x0 + x1) / 2 ? x0 - demi - 2 : x1 + demi + 2;
    });
    x = Math.max(demi + 2, Math.min(LARG * CASE - demi - 2, x));
    carte.elements.filter(t => t.type === "touffe").forEach(t => {   // the blades rise 4 above the first row
      const haut = t.y * CASE - 4, bas = (t.y + t.h) * CASE;
      if (x + demi > t.x * CASE && x - demi < (t.x + t.w) * CASE && y + dh > haut && y - dh < bas) y = y < (haut + bas) / 2 ? haut - dh - 1 : bas + dh + 1;
    });
    (carte.pnj || []).forEach(n => {
      const px = n.x * CASE + 8, pied = n.y * CASE + 15;
      if (Math.abs(px - x) < demi + 10 && y + dh > pied - 27 && y - dh < pied) x = px < x ? px + demi + 11 : px - demi - 11;
    });
    return {x, y};
  }

  // a house's name: its text (the game's style) on a rounded pill like the interface's (.gardien: #1D1640 at 0.6),
  // hidden until the hero comes near (cadrer)
  function panneau(scene, region, x, y, texte){
    const t = scene.add.text(0, 0, "", {fontFamily: "Fredoka, sans-serif", fontSize: "7px", color: "#FFFFFF", fontStyle: "bold",
      stroke: "#1D1640", strokeThickness: 2, padding: {x: 2, y: 1}}).setOrigin(.5).setResolution(12);
    const g = scene.add.graphics();
    const p = scene.add.container(x, y, [g, t]).setDepth(10000);
    Object.assign(p, {region, texte: t, pastille: g, depart: {x, y}});
    const r = scene.villageHD;
    if (r) (r.panneaux = r.panneaux || []).push(p);
    if (r && r.enseignes) { p.pres = false; p.setVisible(false); [t, g].forEach(o => o.setAlpha(0)); }
    ecrire(p, texte);
    // the game's font may come after the village (a slow network): the name is written again with it
    if (document.fonts && document.fonts.load) document.fonts.load('bold 7px "Fredoka"').then(() => { if (p.active) ecrire(p, p.brut); }).catch(() => {});
    return p;
  }
  // the medals won and the open padlock follow the name as drawn icons (js/icones.js, loaded with the village); the
  // text keeps its emojis for whoever reads it (texteEtiquette), and shows them while an icon is missing
  function ecrire(p, texte){
    const sc = p.scene, T = 8;
    const noms = [...String(texte).matchAll(/🏅|🔓/gu)].map(m => m[0] === "🔓" ? "cadenas_ouvert" : "medaille_" + p.region);
    const dessines = noms.length > 0 && noms.every(n => sc && sc.textures.exists("ico_" + n));
    p.brut = texte;
    p.texte.setText(dessines ? String(texte).replace(/\s*(🏅|🔓)/gu, "") : texte);
    (p.icones || []).forEach(i => i.destroy());
    const wt = p.texte.width, wi = dessines ? noms.length * (T + 1) : 0;
    const w = wt + wi + 4, h = Math.max(p.texte.height, dessines ? T + 2 : 0);
    p.texte.setX(-wi / 2);
    p.icones = dessines ? noms.map((n, i) => sc.add.image(-w / 2 + 3 + wt + i * (T + 1) + T / 2, 0, "ico_" + n).setDisplaySize(T, T).setAlpha(p.texte.alpha)) : [];
    if (p.icones.length) p.add(p.icones);
    p.pastille.clear().fillStyle(0x1D1640, .6).fillRoundedRect(-w / 2, -h / 2, w, h, h / 2);
    const r = p.scene && p.scene.villageHD;
    const s = r && r.enseignes && r.enseignes[p.region];
    const e = r && r.etiquettesDepart && r.etiquettesDepart[p.region];
    const m = r && r.maisons.find(q => q.region === p.region && q.nom.startsWith("maison_"));
    // over the house's sign, stretching away from the house (its door, its stars and its arrow stay in sight)
    const dx = s && m ? (s.x < m.x ? -1 : 1) * Math.max(0, w / 2 - 14) : 0;
    if (s) p.setPosition(s.x + dx, s.haut - h / 2 - 1);
    else if (e) { const q = placer(r, p.region, e, w, h); p.setPosition(q.x, q.y); }
    // where it stands when nothing is in the way, its size, and where it goes when the top of the screen hides it:
    // under the sign, or under the house, in front of the door
    const dessous = s ? {x: s.x + dx, y: s.y + h / 2 + 2} : m ? {x: m.x, y: m.y + h / 2 + 3} : null;
    Object.assign(p, {ideal: {x: p.x, y: p.y}, largeur: w, hauteur: h, dessous});
    if (r) r.devoile = null;   // the next devoilerHD looks again
  }

  // the soft shadow under a character: an ellipse three quarters of its width, 4 high, the violet of the trees'
  // shadows at 0.2, just under the character's depth; it follows it (position, depth, visibility, fading) and goes with it
  function ombre(scene, s){
    if (!s) return null;
    if (!scene.textures.exists("vh_ombre_perso")) {   // two nested ellipses, as under the trees: lighter rim, denser middle
      const c = document.createElement("canvas"); c.width = 128; c.height = 32;
      const g = c.getContext("2d");
      g.fillStyle = "rgba(58,42,106,.55)"; g.beginPath(); g.ellipse(64, 16, 63, 15, 0, 0, Math.PI * 2); g.fill();
      g.fillStyle = "rgba(58,42,106,1)"; g.beginPath(); g.ellipse(66, 16, 44, 10.5, 0, 0, Math.PI * 2); g.fill();
      scene.textures.addCanvas("vh_ombre_perso", c).setFilter(Phaser.Textures.FilterMode.LINEAR);
    }
    const o = scene.add.image(s.x, s.y, "vh_ombre_perso").setDisplaySize(Math.max(8, Math.min(17, s.width * Math.abs(s.scaleX) * .75)), 4);
    let liste = scene.ombresHD;
    if (!liste) {
      liste = scene.ombresHD = [];
      const suivre = () => {
        for (let i = liste.length - 1; i >= 0; i--) {
          const {s: p, o: q} = liste[i];
          if (!p.active || !p.scene) { q.destroy(); liste.splice(i, 1); continue; }
          const dessin = p.texture.key !== "__DEFAULT";   // a character still waiting for its drawing casts no shadow
          if (dessin && q.largeur !== p.displayWidth) { q.largeur = p.displayWidth; q.setDisplaySize(Math.max(8, Math.min(17, Math.abs(p.displayWidth) * .75)), 4); }
          q.setPosition(p.x, p.y + 1.2).setDepth(p.depth - .5).setVisible(p.visible && dessin).setAlpha(.22 * p.alpha);   // half of it under the feet
        }
      };
      scene.events.on("update", suivre);
      scene.events.once("shutdown", () => { scene.events.off("update", suivre); scene.ombresHD = null; });
    }
    liste.push({s, o});
    o.setPosition(s.x, s.y + 1.2).setDepth(s.depth - .5).setAlpha(.22 * s.alpha);
    return o;
  }
  return {touchee, devoiler, cadrer, placer, panneau, ecrire, etoiles, ombre};
})();
