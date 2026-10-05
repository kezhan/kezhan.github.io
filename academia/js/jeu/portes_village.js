/* Académia : les portes des maisons, dans le village (js/jeu/monde.js).
   Fermée : une brume violette sur la porte, deux yeux d'Ombre qui clignent et regardent le héros, et au-dessus quatre
   petites étoiles qui s'allument une à une, une par Ombre battue dans les hautes herbes de la maison.
   Ouverte : la porte s'emplit d'une lumière dorée qui pulse, une flèche rebondit au-dessus, des étincelles montent.
   Quand la quatrième Ombre tombe, la caméra glisse jusqu'à la maison, la quatrième étoile s'allume, la brume s'envole,
   la porte s'ouvre devant l'enfant et la voix le dit (elle le redit 10 s plus tard sans toucher, js/jeu/guide_village.js),
   puis la caméra revient au héros. Rien que des pièces d'effets (assets/<qualité>/effets) et des formes simples. */
const PORTES_DESSIN = {   // the door in each house's drawing: its centre's height, its size, and how high above it the stars
  // stand (over the Duché's coat of arms), in world units from the house's foot
  dojo: {y: -15, w: 16, h: 18, e: 3.2}, albion: {y: -13, w: 13, h: 21, e: 3.2}, germania: {y: -13, w: 14, h: 21, e: 3.2},
  duche: {y: -12.5, w: 12.5, h: 20, e: 8.5}, jardin: {y: -15, w: 18, h: 22, e: 3.2}, observatoire: {y: -13, w: 12, h: 21, e: 3.2}};
const PORTE_PAN_MS = 700, PORTE_REGARD_MS = 2000;

class PortesVillage {
  constructor(scene){
    this.s = scene; this.portes = {};
    const r = scene.villageHD; if (!r) return;
    r.maisons.filter(m => m.nom.startsWith("maison_")).forEach(m => {
      const d = PORTES_DESSIN[m.region] || {y: -13, w: 13, h: 20}, t = scene.carte.portes.find(q => q.region === m.region);
      this.portes[m.region] = {region: m.region, m, x: m.x, y: m.y + d.y, w: d.w, h: d.h, e: d.e || 3.2, prof: m.im.depth,
        tx: t ? t.x : Math.floor(m.x / CASE), ty: t ? t.y : Math.floor(m.y / CASE), objets: [], etoiles: [], ouverte: null, allumees: -1};
    });
    this.maj();
  }
  // the open doors, for the guide: centre of the door and the tile in front of it
  ouvertes(){ return Object.values(this.portes).filter(p => p.ouverte); }
  // every door as the game stands; `sauf` keeps its mist (its door is about to open before the child's eyes)
  maj(sauf){
    Object.values(this.portes).forEach(p => {
      const n = Math.min(regionEtat(p.region).etape, ETAPES), ouverte = n >= ETAPES && p.region !== sauf;
      if (ouverte !== p.ouverte) this.dessiner(p, ouverte);
      this.allumer(p, ouverte ? ETAPES : Math.min(n, ETAPES - (p.region === sauf ? 1 : 0)));
    });
  }
  vider(p){
    p.objets.forEach(o => { this.s.tweens.killTweensOf(o); o.destroy(); });
    p.objets = []; p.etoiles = []; p.yeux = null;
    if (p.minuteur) p.minuteur.remove();
    p.minuteur = null;
  }
  dessiner(p, ouverte){
    this.vider(p); p.ouverte = ouverte; p.allumees = -1;
    const s = this.s, anim = !calme(), garder = o => { p.objets.push(o); return o; };
    const haut = p.y - p.h / 2, bas = p.y + p.h / 2;
    // four little stars over the door, one per Ombre beaten near the house
    for (let i = 0; i < ETAPES; i++) {
      if (!s.textures.exists("fx_etoile_p")) break;
      const e = garder(s.add.image(p.x + (i - 1.5) * 4.6, haut - p.e, "fx_etoile_p").setDepth(p.prof + .3));
      e.base = 3.8 / e.height; e.setScale(e.base);
      p.etoiles.push(e);
    }
    if (!ouverte) {   // violet mist and an Ombre's eyes in the doorway
      const brume = garder(s.add.graphics().setDepth(p.prof + .1));
      brume.fillStyle(0x4B2F96, .5).fillRoundedRect(p.x - p.w / 2, haut, p.w, p.h, Math.min(5, p.w / 2));
      for (let i = 0; i < 3 && s.textures.exists("fx_nuage_p"); i++) {
        const n = garder(s.add.image(p.x + (i - 1) * p.w * .34, bas - 2.5, "fx_nuage_p").setTint(0x9C7BF2).setAlpha(.75).setDepth(p.prof + .15));
        const k = p.w * .5 / n.width; n.setScale(k).setFlipX(i === 2);
        if (anim) s.tweens.add({targets: n, scale: k * 1.12, alpha: .55, x: n.x + (i - 1) * .8, duration: 1300 + i * 260, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      }
      const yeux = p.yeux = garder(s.add.graphics().setDepth(p.prof + .2));
      yeux.setPosition(p.x, p.y - p.h * .12);
      const oeil = dx => { yeux.fillStyle(0x1D1640, 1).fillEllipse(dx, 0, 2.6, 3.4); yeux.fillStyle(0xFFFFFF, 1).fillCircle(dx - .45, -.7, .55); };
      oeil(-2.3); oeil(2.3);
      if (anim) p.minuteur = s.time.addEvent({delay: 2600, loop: true, callback: () => {   // a blink now and then
        if (Math.random() < .7) s.tweens.add({targets: yeux, scaleY: .12, duration: 90, yoyo: true, ease: "Quad.easeIn"});
      }});
      return;
    }
    // open: golden light in the doorway, a glow around it, an arrow that bounces, sparkles that rise
    const lumiere = garder(s.add.graphics().setDepth(p.prof + .1));
    lumiere.fillStyle(0xFFE58A, .85).fillRoundedRect(p.x - p.w / 2, haut, p.w, p.h, Math.min(5, p.w / 2));
    if (anim) s.tweens.add({targets: lumiere, alpha: .45, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    if (s.textures.exists("fx_halo")) {
      const g = garder(s.add.image(p.x, p.y, "fx_halo").setTint(0xFFD45C).setBlendMode(Phaser.BlendModes.ADD).setAlpha(.6).setDepth(p.prof + .25));
      const k = p.h * 2.1 / g.height; g.setScale(k);
      if (anim) s.tweens.add({targets: g, scale: k * 1.15, alpha: .3, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
    if (s.textures.exists("fx_guide_fleche")) {
      const f = garder(s.add.image(p.x, haut - 7, "fx_guide_fleche").setOrigin(.5, 1).setDepth(9700));
      f.setScale(10 / f.height);
      if (anim) s.tweens.add({targets: f, y: f.y - 4, duration: 420, yoyo: true, repeat: -1, ease: "Quad.easeOut"});
    }
    if (anim && s.textures.exists("fx_etincelle_p")) p.minuteur = s.time.addEvent({delay: 520, loop: true, callback: () => {
      const e = s.add.image(p.x + (Math.random() - .5) * p.w, bas - 2, "fx_etincelle_p").setDepth(p.prof + .3).setTint(0xFFF3B0);
      const k = (2 + Math.random() * 1.5) / e.height; e.setScale(0);
      s.tweens.add({targets: e, scale: k, y: e.y - 10 - Math.random() * 8, alpha: {from: 1, to: 0}, duration: 1100, ease: "Sine.easeOut", onComplete: () => e.destroy()});
    }});
  }
  // n stars lit; the one that just lit pops
  allumer(p, n){
    if (n === p.allumees) return;
    const avant = p.allumees; p.allumees = n;
    p.etoiles.forEach((e, i) => {
      const allumee = i < n;
      e.setTint(allumee ? 0xFFFFFF : 0xA79FD0).setAlpha(allumee ? 1 : .8);   // not yet: a pale lilac star
      if (allumee && avant >= 0 && i >= avant && !calme()) {
        e.setScale(0); this.s.tweens.add({targets: e, scale: e.base, duration: 420, delay: (i - avant) * 150, ease: "Back.easeOut"});
        effetHD(this.s, "toucher", e.x, e.y, 6, e.depth + .1);
      }
    });
  }
  // the fourth Ombre just fell: the camera goes to the house, the door opens, the voice says it, the camera comes back
  ouvrir(region){
    const p = this.portes[region], s = this.s, r = regionDe(region);
    if (!p) return;
    const ouvrirLa = () => {
      this.allumer(p, ETAPES);
      s.time.delayedCall(calme() ? 0 : 450, () => {
        effetHD(s, "fumee", p.x, p.y, p.h, p.prof + .4);
        this.dessiner(p, true); this.allumer(p, ETAPES);
        if (!calme()) effetHD(s, "evolution", p.x, p.y, p.h, p.prof + .5);
        if (typeof jouerSon === "function") jouerSon("porte");
        dire(phrasePorte(r));
      });
    };
    if (calme()) { ouvrirLa(); s.guide.rappelPorte(region); return; }
    const cam = s.cameras.main;
    s.moment = true; s.chemin = [];
    cam.stopFollow();
    cam.pan(p.x, p.y, PORTE_PAN_MS, "Sine.easeInOut", true, (c, k) => {
      if (k < 1) return;
      ouvrirLa();
      s.time.delayedCall(PORTE_REGARD_MS, () => cam.pan(s.heros.x, s.heros.y - 8, PORTE_PAN_MS, "Sine.easeInOut", true, (c2, k2) => {
        if (k2 < 1) return;
        s.moment = false; s.suivreHeros(); s.guide.rappelPorte(region);
      }));
    });
  }
  // the eyes in a closed door follow the hero
  update(){
    const h = this.s.heros; if (!h) return;
    Object.values(this.portes).forEach(p => { if (!p.ouverte && p.yeux && p.yeux.active) p.yeux.x = p.x + Math.max(-.7, Math.min(.7, (h.x - p.x) / 40)); });
  }
}
