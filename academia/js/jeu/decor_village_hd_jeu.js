/* Académia : ce que le jeu appelle pendant qu'on marche dans le village en haute définition (js/jeu/decor_village_hd.js).
   1. maisonToucheeHD : un toucher sur le dessin d'une maison (son toit compris), sur le panneau du Dojo ou sur le
      panneau d'une maison désigne cette maison : on va à sa porte.
   2. devoilerHD : un dessin placé devant le héros ou son compagnon, qui recouvre son corps, devient à moitié
      transparent ; un panneau qu'ils touchent devient presque transparent. On l'appelle à chaque image : il ne refait
      rien tant que personne ne bouge.
   3. panneauVillageHD et ecrirePanneauHD : le panneau d'une maison, texte sur une pastille arrondie, posé au-dessus de
      la maison, hors des chemins et des hautes herbes, décalé si un habitant est dessous.
   4. ombreAuSolHD : l'ombre douce au sol sous un personnage, qui le suit. */

function maisonToucheeHD(scene, wx, wy){ return VILLAGE_HD_JEU.touchee(scene, wx, wy); }
function devoilerHD(scene, persos, panneaux){ VILLAGE_HD_JEU.devoiler(scene, persos, panneaux); }
// a house's sign at (x, y) in world units (where it goes when the village is not drawn in high definition)
function panneauVillageHD(scene, region, x, y, texte = ""){ return VILLAGE_HD_JEU.panneau(scene, region, x, y, texte); }
function ecrirePanneauHD(panneau, texte){ VILLAGE_HD_JEU.ecrire(panneau, texte); }
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
  function devoiler(scene, persos, panneaux = []){
    const r = scene.villageHD; if (!r) return;
    const ps = persos.filter(p => p && p.active), cs = ps.map(p => ({p, ...corps(p)}));
    const cle = cs.map(c => `${c.x},${c.y},${c.p.depth}`).join(";") + "|" + panneaux.length;
    if (cle === r.devoile) return;
    r.devoile = cle;
    r.ecrans.forEach(im => voiler(scene, im, cs.some(c => cache(scene, im, c, c.p)) ? .5 : 1));
    panneaux.forEach(pa => {
      if (!pa || !pa.active) return;
      const b = pa.getBounds();
      voiler(scene, pa, cs.some(c => Phaser.Geom.Rectangle.Overlaps(b, c.boite)) ? .25 : 1);
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

  // a sign: its text (the game's style) on a rounded pill like the interface's (.gardien: #1D1640 at 0.6)
  function panneau(scene, region, x, y, texte){
    const t = scene.add.text(0, 0, "", {fontFamily: "Fredoka, sans-serif", fontSize: "7px", color: "#FFFFFF", fontStyle: "bold",
      stroke: "#1D1640", strokeThickness: 2, padding: {x: 2, y: 1}}).setOrigin(.5).setResolution(12);
    const g = scene.add.graphics();
    const p = scene.add.container(x, y, [g, t]).setDepth(10000);
    Object.assign(p, {region, texte: t, pastille: g, depart: {x, y}});
    const r = scene.villageHD;
    if (r) (r.panneaux = r.panneaux || []).push(p);
    ecrire(p, texte);
    return p;
  }
  function ecrire(p, texte){
    p.texte.setText(texte);
    const w = p.texte.width + 4, h = p.texte.height;
    p.pastille.clear().fillStyle(0x1D1640, .6).fillRoundedRect(-w / 2, -h / 2, w, h, h / 2);
    const r = p.scene && p.scene.villageHD;
    const e = r && r.etiquettesDepart && r.etiquettesDepart[p.region];
    if (e) { const q = placer(r, p.region, e, w, h); p.setPosition(q.x, q.y); }
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
  return {touchee, devoiler, placer, panneau, ecrire, ombre};
})();
