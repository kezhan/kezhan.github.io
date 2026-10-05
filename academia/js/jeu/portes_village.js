/* Académia : les portes des maisons, dans le village (js/jeu/monde.js), dessinées dans la planche des entrées
   (outils/village/scripts/portes.py et fenetres.py, posées au pied de la maison).
   Fermée : une brume violette remplit l'entrée et déborde sur la marche, deux yeux d'Ombre y clignent et regardent le
   héros, et au-dessus quatre petites étoiles s'allument une à une, une par Ombre battue dans les hautes herbes de la
   maison (le panneau planté devant la maison les compte aussi, js/jeu/decor_village_hd_jeu.js).
   Ouverte : la porte s'est ouverte, sa lumière dorée bat doucement, une flèche rebondit au-dessus, des étincelles
   montent ; la fenêtre de la maison s'allume et, de temps en temps, l'ombre du chef y passe (au Jardin et à
   l'Observatoire, dans l'entrée même).
   Quand la quatrième Ombre tombe, la caméra glisse jusqu'à la maison, la quatrième étoile s'allume, la brume s'envole,
   la porte s'ouvre devant l'enfant et la voix le dit (elle le redit 10 s plus tard sans toucher, js/jeu/guide_village.js),
   puis la caméra revient au héros. Sans ces dessins (une table plus ancienne), la brume et la lumière sont des formes
   simples. Quand l'enfant demande le calme, rien ne bouge ni ne passe. */
const PORTES_DESSIN = {   // the door in each house's drawing (without the drawn pieces): its centre's height, its size, and how
  // high above it the stars stand (over the Duché's coat of arms), in world units from the house's foot
  dojo: {y: -15, w: 16, h: 18, e: 3.2}, albion: {y: -13, w: 13, h: 21, e: 3.2}, germania: {y: -13, w: 14, h: 21, e: 3.2},
  duche: {y: -12.5, w: 12.5, h: 20, e: 8.5}, jardin: {y: -15, w: 18, h: 22, e: 3.2}, observatoire: {y: -13, w: 12, h: 21, e: 3.2}};
const PORTE_PAN_MS = 700, PORTE_REGARD_MS = 2000;
const OMBRE_CHEF_PAS_MS = 320, OMBRE_CHEF_ATTENTE = [4500, 9000];   // the chef's shadow: a step every 320 ms, then a rest

class PortesVillage {
  constructor(scene){
    this.s = scene; this.portes = {};
    const r = scene.villageHD; if (!r) return;
    r.maisons.filter(m => m.nom.startsWith("maison_")).forEach(m => {
      const d = PORTES_DESSIN[m.region] || {y: -13, w: 13, h: 20}, t = scene.carte.portes.find(q => q.region === m.region);
      const pt = q => VILLAGE_HD.point(`porte_${m.region}_brume`, q, m.x, m.y);
      const c = pt("centre"), haut = pt("haut");
      this.portes[m.region] = {region: m.region, m, x: c ? c.x : m.x, y: c ? c.y : m.y + d.y, w: d.w, h: d.h, e: d.e || 3.2,
        haut: haut ? haut.y : m.y + d.y - d.h / 2, oeil: pt("yeux"), prof: m.im.depth,
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
    p.objets = []; p.etoiles = []; p.yeux = null; p.brume = null; p.fenetres = null;
    [p.minuteur, p.passage].forEach(m => { if (m) m.remove(); });
    p.minuteur = p.passage = null;
  }
  dessiner(p, ouverte){
    this.vider(p); p.ouverte = ouverte; p.allumees = -1;
    const s = this.s, anim = !calme(), garder = o => { if (o) p.objets.push(o); return o; };
    const piece = (nom, x, y, prof) => garder(VILLAGE_HD.poser(s, nom, x, y, prof));
    const haut = p.haut, bas = p.y + p.h / 2;
    // four little stars over the door, one per Ombre beaten near the house
    for (let i = 0; i < ETAPES; i++) {
      if (!s.textures.exists("fx_etoile_p")) break;
      const e = garder(s.add.image(p.x + (i - 1.5) * 4.6, haut - p.e, "fx_etoile_p").setDepth(p.prof + .3));
      e.base = 3.8 / e.height; e.setScale(e.base);
      p.etoiles.push(e);
    }
    if (!ouverte) {   // violet mist and an Ombre's eyes in the doorway
      p.brume = piece(`porte_${p.region}_brume`, p.m.x, p.m.y, p.prof + .1);
      if (p.brume && anim) s.tweens.add({targets: p.brume, alpha: .88, duration: 1500, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      if (!p.brume) this.brumeSimple(p, garder, anim);
      const yeux = p.yeux = (p.oeil && piece("yeux_ombre", p.oeil.x, p.oeil.y, p.prof + .2)) || this.yeuxSimples(p, garder);
      yeux.base = yeux.scaleY; yeux.x0 = yeux.x;
      if (anim) p.minuteur = s.time.addEvent({delay: 2600, loop: true, callback: () => {   // a blink now and then
        if (Math.random() < .7 && yeux.active) s.tweens.add({targets: yeux, scaleY: yeux.base * .12, duration: 90, yoyo: true, ease: "Quad.easeIn"});
      }});
      return;
    }
    // open: the door open on its golden light, a glow around it that beats, an arrow that bounces, sparkles that rise
    if (!piece(`porte_${p.region}_ouverte`, p.m.x, p.m.y, p.prof + .1)) {
      const lumiere = garder(s.add.graphics().setDepth(p.prof + .1));
      lumiere.fillStyle(0xFFE58A, .85).fillRoundedRect(p.x - p.w / 2, haut, p.w, p.h, Math.min(5, p.w / 2));
      if (anim) s.tweens.add({targets: lumiere, alpha: .45, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
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
    this.fenetre(p, garder, anim);
  }
  // the house's window lit (fenetre_<region>_0), and the chef's shadow passing in it (_1 to _4, one way or the other)
  fenetre(p, garder, anim){
    const s = this.s, nom = k => `fenetre_${p.region}_${k}`;
    if (!PIECES_VILLAGE[nom(1)]) return;
    p.fenetres = [0, 1, 2, 3, 4].map(k => PIECES_VILLAGE[nom(k)] ? garder(VILLAGE_HD.poser(s, nom(k), p.m.x, p.m.y, p.prof + .12)) : null);
    const voir = k => p.fenetres.forEach((f, i) => { if (f) f.setVisible(i === k); });
    voir(0);
    if (!anim) return;
    let sens = 1;
    const attendre = () => {
      const [a, b] = OMBRE_CHEF_ATTENTE;
      p.passage = s.time.delayedCall(a + Math.random() * (b - a), () => {
        const pas = sens > 0 ? [1, 2, 3, 4, 0] : [4, 3, 2, 1, 0];
        sens = -sens;
        pas.forEach((k, i) => s.time.delayedCall(i * OMBRE_CHEF_PAS_MS, () => { if (p.fenetres) voir(k); }));
        attendre();
      });
    };
    attendre();
  }
  // without the drawn pieces: a violet veil, three clouds, two eyes drawn in shapes
  brumeSimple(p, garder, anim){
    const s = this.s, haut = p.haut, bas = p.y + p.h / 2;
    const brume = garder(s.add.graphics().setDepth(p.prof + .1));
    brume.fillStyle(0x4B2F96, .5).fillRoundedRect(p.x - p.w / 2, haut, p.w, p.h, Math.min(5, p.w / 2));
    for (let i = 0; i < 3 && s.textures.exists("fx_nuage_p"); i++) {
      const n = garder(s.add.image(p.x + (i - 1) * p.w * .34, bas - 2.5, "fx_nuage_p").setTint(0x9C7BF2).setAlpha(.75).setDepth(p.prof + .15));
      const k = p.w * .5 / n.width; n.setScale(k).setFlipX(i === 2);
      if (anim) s.tweens.add({targets: n, scale: k * 1.12, alpha: .55, x: n.x + (i - 1) * .8, duration: 1300 + i * 260, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
  }
  yeuxSimples(p, garder){
    const yeux = garder(this.s.add.graphics().setDepth(p.prof + .2));
    yeux.setPosition(p.x, p.y - p.h * .12);
    const oeil = dx => { yeux.fillStyle(0x1D1640, 1).fillEllipse(dx, 0, 2.6, 3.4); yeux.fillStyle(0xFFFFFF, 1).fillCircle(dx - .45, -.7, .55); };
    oeil(-2.3); oeil(2.3);
    return yeux;
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
  // the fourth Ombre just fell: the camera goes to the house, the mist flies away, the door opens, the voice says it,
  // the camera comes back
  ouvrir(region){
    const p = this.portes[region], s = this.s, r = regionDe(region);
    if (!p) return;
    const ouvrirLa = () => {
      this.allumer(p, ETAPES);
      s.time.delayedCall(calme() ? 0 : 450, () => {
        const partent = [p.brume, p.yeux].filter(o => o && o.active);   // the mist and the eyes rise and fade
        p.objets = p.objets.filter(o => !partent.includes(o));
        partent.forEach(o => {
          s.tweens.killTweensOf(o);
          if (calme()) return o.destroy();
          s.tweens.add({targets: o, alpha: 0, y: o.y - 10, duration: 650, ease: "Sine.easeIn", onComplete: () => o.destroy()});
        });
        effetHD(s, "fumee", p.x, p.y, p.h, p.prof + .4);
        this.dessiner(p, true); this.allumer(p, ETAPES);
        if (!calme()) {
          p.objets.filter(o => o.frame && o.frame.name === `porte_${region}_ouverte`).forEach(o => {
            o.setAlpha(0); s.tweens.add({targets: o, alpha: 1, duration: 500, ease: "Sine.easeOut"});
          });
          effetHD(s, "evolution", p.x, p.y, p.h, p.prof + .5);
        }
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
    Object.values(this.portes).forEach(p => {
      if (!p.ouverte && p.yeux && p.yeux.active) p.yeux.x = p.yeux.x0 + Math.max(-.7, Math.min(.7, (h.x - p.x) / 40));
    });
  }
}
