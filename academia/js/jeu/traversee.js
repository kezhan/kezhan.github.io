/* Académia : la traversée en barque, entre le premier village et l'Île des Bâtisseurs (js/jeu/voyage.js la lance).
   Quelques secondes, pour le plaisir : le ciel et la mer jusqu'à l'horizon, la côte du village d'un côté, l'île de
   l'autre ; la barque glisse de l'un à l'autre en se berçant, le passeur à l'arrière agite sa godille, le héros et son
   compagnon (avec ce qu'il porte, js/jeu/accessoires.js) à bord, vus jusqu'à la taille ; l'écume du sillage, des
   vagues qui naissent et s'effacent, deux mouettes. La voix dit où l'on va. Puis l'écran se ferme en étoile et le
   village d'arrivée s'ouvre (entrerMonde, js/jeu/lanceur.js). Vers l'île, la barque va de gauche à droite ; au retour,
   de droite à gauche ; la barque entière reste à l'écran, du départ à l'arrivée, sur un écran debout aussi. Le soir venu
   (P().histoire.soir du jour), la mer est au crépuscule, comme les villages. Quand l'enfant demande le calme, seule la
   barque bouge. Ce qu'elle montre est déjà chargé par le village (js/jeu/voyage.js, prechargerVoyage) ; son décor et
   sa barque, qui ne servent qu'ici, sont libérés de la mémoire à la fin. Dessins : outils/village/scripts/
   ile_traversee.py (assets/<qualité>/ile/). */
const TRAVERSEE_MS = 4600, DEMI_BARQUE = 52;   // half the boat with its oar swinging, in world units
const TEXTURES_TRAVERSEE = ["vh_traversee_fond", "vh_ile_barque"];   // only the crossing shows them
// where each one stands in the boat, in world units from the middle of its waterline (the boat faces the way it goes)
const A_BORD = {passeur: [-27, -10], godille: [-39, -13], heros: [3, -11], compagnon: [25, -17]};

class Traversee extends Phaser.Scene {
  constructor(){ super("traversee"); }
  init(d){
    this.vers = d && d.vers === "village" ? "village" : "ile"; this.sens = this.vers === "ile" ? 1 : -1;
    this.fini = false; this.t0 = null; this.membres = null; this.ciel = null; this.mer = null; this.soir = null;   // nothing kept from the last trip
  }
  preload(){
    reessayerImages(this);
    const a = actif();
    chargerAtlas(this, [herosHD(P()), "passeur", a && cleDe(a)]);
    chargerTraverseeHD(this.load, QUALITE);
    chargerVillageHD(this.load, QUALITE);   // the glints on the water are the village's (already there)
  }
  create(){
    const sens = this.sens, anim = !calme();
    this.fond = this.textures.exists("vh_traversee_fond") ? this.add.image(0, 0, "vh_traversee_fond").setDepth(0) : null;
    if (this.fond) this.fond.texture.setFilter(Phaser.Textures.FilterMode.LINEAR);
    ["vh_ile_barque", "vh_ile"].forEach(k => { if (this.textures.exists(k)) this.textures.get(k).setFilter(Phaser.Textures.FilterMode.LINEAR); });
    const poser = (nom, d) => { const im = VILLAGE_HD.poser(this, nom, 0, 0, d); if (im && sens < 0) im.setFlipX(true); return im; };
    // the boat, its ferryman, the hero and the companion; each one placed at every frame (update)
    const p = P(), c = actif();
    const passeur = spriteHumain(this, "passeur", 0, 0).setDepth(11);
    const heros = spriteHumain(this, herosHD(p), 0, 0).setDepth(12);
    [passeur, heros].forEach(s => { s.dir = sens > 0 ? "droite" : "gauche"; reposHumain(s, s.dir); });
    const k = c ? spriteCreature(this, cleDe(c), 0, 0).setDepth(13) : null;
    if (k) { k.setFlipX(sens < 0); porterAccessoire(this, k, c); }
    const godille = poser("godille", 11.5);
    this.membres = [[poser("barque_mer_dos", 10), 0, 0], [passeur, ...A_BORD.passeur], [godille, ...A_BORD.godille], [heros, ...A_BORD.heros],
      [k, ...A_BORD.compagnon], [poser("barque_mer_devant", 20), 0, 0]].filter(m => m[0]);
    this.membresVus = {passeur, heros, compagnon: k};
    if (godille && anim) this.tweens.add({targets: godille, angle: {from: -16, to: 16}, duration: 560, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    this.cadrer();
    const recadrer = () => this.cadrer();
    this.scale.on("resize", recadrer); this.events.once("shutdown", () => this.scale.off("resize", recadrer));
    this.duree = TEST ? 60 : TRAVERSEE_MS;   // counted from the first frame (the clock still holds the last trip's time here)
    if (anim) {
      this.time.addEvent({delay: 140, loop: true, callback: () => this.sillage()});
      this.time.addEvent({delay: 230, loop: true, callback: () => this.vague()});
      [.34, .24].forEach((h, i) => this.time.delayedCall(300 + i * 900, () => this.mouette(h)));
    }
    // the evening seen today: the same warm dusk as over the villages (js/jeu/histoire_village.js, crepuscule)
    if (typeof vuAujourdhui === "function" && vuAujourdhui("soir")) {
      this.soir = this.add.rectangle(0, 0, 100, 100, 0xE89E8E).setScrollFactor(0).setDepth(9990).setBlendMode(Phaser.BlendModes.MULTIPLY);
      this.cadrer();
    }
    // its backdrop and its boat freed once the crossing is over (they come back from the cache for the next one)
    this.events.once("shutdown", () => setTimeout(() => TEXTURES_TRAVERSEE.forEach(k => { if (this.textures.exists(k)) this.textures.remove(k); }), 0));
    if (typeof jouerAmbiance === "function") jouerAmbiance("village");
    dire(this.vers === "ile" ? "En route pour l'Île des Bâtisseurs !" : "En route pour le village !");
  }
  // the camera on the boat, the backdrop covering the screen, its horizon a little above the boat; on a tall screen
  // (a phone held upright) the boat is bigger and the backdrop shows both coasts, the sky's and the sea's own colours
  // continuing it above and below
  cadrer(){
    const cam = this.cameras.main, W = this.scale.width / RATIO, H = this.scale.height / RATIO, debout = H > W * 1.15;
    const z = Math.max(1.2, Math.min(W / (debout ? 150 : 260), H / 140));
    cam.setZoom(z * RATIO).centerOn(0, -18);
    if (this.soir) this.soir.setPosition(cam.width / 2, cam.height / 2).setScale((cam.width / cam.zoom + 8) / 100, (cam.height / cam.zoom + 8) / 100);
    const v = this.vue = {w: W / z, h: H / z, x: -W / z / 2, y: -18 - H / z / 2};
    const course = Math.max(0, Math.min(v.w * .3, v.w / 2 - DEMI_BARQUE));   // from one side to the other, the whole boat in sight
    this.x0 = -this.sens * course; this.x1 = this.sens * course;
    if (!this.fond) return;
    const f = this.fond, fw = f.width, fh = f.height, hf = 820 / 1350, hy = -18 - v.h * (debout ? .12 : .1);
    const k = debout ? v.w / (fw * .6) : Math.max(v.w / fw, (hy - v.y) / (fh * hf), (v.y + v.h - hy) / (fh * (1 - hf))) * 1.02;
    f.setOrigin(.5, hf).setPosition(0, hy).setScale(k);
    const haut = hy - fh * hf * k + .5, bas = hy + fh * (1 - hf) * k - .5;
    if (!this.ciel) { this.ciel = this.add.rectangle(0, 0, 1, 1, 0x86CCF3).setOrigin(.5, 1).setDepth(-1); this.mer = this.add.rectangle(0, 0, 1, 1, 0x4AAEE0).setOrigin(.5, 0).setDepth(-1); }
    this.ciel.setPosition(0, haut).setSize(v.w + 20, Math.max(1, haut - v.y + 10)).setVisible(haut > v.y);
    this.mer.setPosition(0, bas).setSize(v.w + 20, Math.max(1, v.y + v.h - bas + 10)).setVisible(bas < v.y + v.h);
  }
  update(time){
    if (!this.membres) return;
    if (this.t0 === null) this.t0 = time;
    const t = Math.min(1, (time - this.t0) / this.duree), e = .5 - Math.cos(Math.PI * t) / 2;
    const x = this.x0 + (this.x1 - this.x0) * e, y = calme() ? 0 : Math.sin(time / 380) * 1.1;
    this.barqueX = x;
    this.membres.forEach(([o, dx, dy]) => { if (o.active) o.setPosition(x + this.sens * dx, y + dy); });
    if (t >= 1 && !this.fini) this.arriver();
  }
  // the boat has crossed: the screen closes on it, and the village reached opens
  arriver(){
    this.fini = true;
    const cam = this.cameras.main, k = cam.zoom / RATIO;
    const sx = (this.barqueX - cam.worldView.x) * k, sy = (-10 - cam.worldView.y) * k;
    this.time.delayedCall(TEST ? 0 : 250, () => Promise.resolve(typeof fermerEtoile === "function" ? fermerEtoile(sx, sy) : null).then(() => {
      this.scene.stop();
      VOYAGE.enCours = false;
      entrerMonde();
      if (typeof ouvrirEtoile === "function") ouvrirEtoile();
    }));
  }
  // foam left behind the stern
  sillage(){
    const im = VILLAGE_HD.poser(this, "ecume", this.barqueX - this.sens * (44 + Math.random() * 6), 1 + Math.random() * 3, 9, {echelle: .5 + Math.random() * .3});
    if (!im) return;
    this.tweens.add({targets: im, alpha: 0, scale: im.scale * 1.7, x: im.x - this.sens * 14, duration: 1100, ease: "Sine.easeOut", onComplete: () => im.destroy()});
  }
  // little waves rising and fading on the sea, drifting the other way
  vague(){
    const v = this.vue;
    const im = VILLAGE_HD.poser(this, "vague", v.x + Math.random() * v.w, 4 + Math.random() * Math.max(10, v.y + v.h - 8), 5, {echelle: .5 + Math.random() * .5});
    if (!im) return;
    im.setAlpha(0);
    this.tweens.add({targets: im, alpha: .85, x: im.x - this.sens * 10, duration: 700, yoyo: true, hold: 200, onComplete: () => im.destroy()});
  }
  // a gull flying the boat's way, a little faster, its wings beating
  mouette(h){
    const v = this.vue, y = v.y + v.h * h, x0 = this.sens > 0 ? v.x - 12 : v.x + v.w + 12;
    const m = VILLAGE_HD.poser(this, "mouette_0", x0, y, 30, {echelle: .8});
    if (!m) return;
    VILLAGE_HD.cadre(this, "mouette_1");
    m.setFlipX(this.sens < 0);
    let n = 0;
    const ailes = this.time.addEvent({delay: 170, loop: true, callback: () => { if (m.active) m.setFrame(n++ % 2 ? "mouette_0" : "mouette_1"); }});
    this.tweens.add({targets: m, x: x0 + this.sens * (v.w + 24), duration: 5200, ease: "Linear", onComplete: () => { ailes.remove(); m.destroy(); }});
    this.tweens.add({targets: m, y: y - 5, duration: 1100, yoyo: true, repeat: 2, ease: "Sine.easeInOut"});
  }
}
