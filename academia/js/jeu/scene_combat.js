/* Académia : la scène de combat, à la manière des jeux de créatures. L'Ombre en haut à droite, face à nous ;
   le compagnon de dos en bas à gauche, chacun sur son estrade, devant le décor de la région. La scène se calcule sur
   la place que laisse le panneau des questions : au-dessus de lui (paysage, portrait), ou à sa gauche sur un écran
   bas (téléphone en paysage) ; une créature n'est jamais plus large que 45 % de l'écran, son estrade fait 1,3 fois
   sa largeur. Coordonnées en pixels de la toile (RATIO par pixel CSS). Les cases de vie sont en HTML ; la mise en
   scène des coups, de l'entrée du boss et de l'attaque ultime est dans js/jeu/combat_spectacle.js. */
class SceneCombat extends Phaser.Scene {
  constructor(){ super("combat"); }
  init(d){ this.d = d; }
  preload(){
    reessayerImages(this);
    chargerAtlas(this, [cleDe(this.d.c), this.d.o.cle]); chargerFondsHD(this.load, QUALITE); chargerEffetsHD(this.load, QUALITE);
  }
  create(){
    const {c, o, region} = this.d, w = this.scale.width, h = this.scale.height;
    const L = this.L = dispositionCombat(this, w, h, o, c);
    document.documentElement.style.setProperty("--scene", Math.round(L.hs / L.r) + "px");
    document.documentElement.style.setProperty("--zone", Math.round(L.zone.w / L.r) + "px");
    $("combat").dataset.mode = L.mode;   // style_scenes.css: the panel beside the scene on a low screen, compact life boxes
    dessinerFondHD(this, L, region, o.boss);   // js/jeu/fond_combat_hd.js

    // the creatures, feet on their stands
    this.lui = this.creature(o.cle, "face", L.lui, L.hLui, L.lMax);
    this.moi = this.creature(cleDe(c), "dos", L.moi, L.hMoi, L.lMax);
    halo(this, this.lui, o.boss ? 0xFF5EC9 : 0x7B4DFF, 1.06, .55);   // js/jeu/sprites_hd.js
    if (stade(c) === 3) halo(this, this.moi, 0xFFD45C, 1.06, .5);
    this.base = {lui: {x: this.lui.x, y: this.lui.y}, moi: {x: this.moi.x, y: this.moi.y}};
    this.occupe = {lui: false, moi: false};
    this.colere = null; this.phase2Faite = !!this.d.phase2;   // the same scene serves every fight
    this.pret = true;
    this.preparerEntree();   // js/jeu/combat_spectacle.js: who comes in, and how
    // a touch on a creature: the Ombre sulks, the companion jumps
    this.input.on("pointerdown", ptr => {
      if (this.d.fige) return;
      for (const k of ["lui", "moi"]) {
        const s = this[k];
        if (s && s.visible && s.alpha > .5 && s.getBounds().contains(ptr.worldX, ptr.worldY)) return this.touche(k);
      }
    });
    // a turned tablet: the scene is drawn again for the new size, once the size has really changed
    let minuteur = null;
    const taille = () => {
      if (minuteur) minuteur.remove();
      minuteur = this.time.delayedCall(150, () => {
        if (this.scale.width !== w || this.scale.height !== h) { this.d.vu = true; this.d.phase2 = this.phase2Faite; this.scene.restart(this.d); }
      });
    };
    this.scale.on("resize", taille);
    this.events.once("shutdown", () => { this.pret = false; this.scale.off("resize", taille); });
    if (this.d.phase2) this.phase2(true);   // redrawn after a turn: the boss stays cross
  }
  // a creature: its drawing in pose `vue` (face or back), feet on its stand, a soft shadow under it; a drawing not
  // there yet (slow network) is waited for, the creature stays invisible meanwhile (js/jeu/sprites_hd.js, quandAtlas)
  creature(cle, vue, p, hauteur, largeur){
    const s = this.add.sprite(p.x, p.y + p.ry * .35, "__DEFAULT").setOrigin(.5, 1).setDepth(5);
    s.base = 1; s.hd = cle; s.vue = vue;
    const ombre = this.add.graphics().setDepth(4);
    quandAtlas(this, cle, () => {
      if (!s.active) return;
      const t = this.textures.get("hd_" + cle);
      s.setTexture("hd_" + cle, t.has(vue) ? vue : "face").setScale(1);
      s.base = Math.min(hauteur / s.height, largeur / s.width); s.setScale(s.base); s.vue = s.frame.name;
      ombre.clear().fillStyle(0x000000, .18).fillEllipse(p.x, p.y + p.ry * .3, s.displayWidth * .75, p.ry * .7);
      if (!TEST) s.souffle = this.tweens.add({targets: s, scaleY: s.base * 1.03, scaleX: s.base * .985, duration: 1000, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    });
    return s;
  }
  pose(cible, nom){   // a high-definition expression, when that drawing has it
    const s = this[cible];
    if (s && s.hd && this.textures.exists("hd_" + s.hd) && this.textures.get("hd_" + s.hd).has(nom)) s.setFrame(nom);
  }
  effet(cible, nom, echelle = 1){   // js/jeu/effets_hd.js, centred on the creature and as big as it
    const s = this[cible];
    if (s) return effetHD(this, nom, s.x, s.y - s.displayHeight / 2, s.displayHeight * echelle, 6);
    return 0;
  }
  // a number or a word born on the creature's belly: it pops, rises a little (24 CSS pixels) and fades, never under
  // the top edge of the screen
  texte(cible, t, couleur = "#FFFFFF", taille = 1){
    const s = this[cible], r = RATIO;
    if (!s) return;
    const x = this.add.text(s.x, s.y - s.displayHeight * .5, t, {fontFamily: "Fredoka, sans-serif", fontSize: Math.round(44 * taille * r) + "px",
      color: couleur, fontStyle: "bold", stroke: "#1D1640", strokeThickness: 8 * r}).setOrigin(.5).setDepth(8);
    x.y = Math.max(x.y, 40 * r + x.height / 2 + 24 * r);
    if (TEST) { x.destroy(); return; }
    x.setScale(.3);
    this.tweens.add({targets: x, scale: 1, duration: 260, ease: "Back.easeOut"});
    this.tweens.add({targets: x, y: x.y - 24 * r, duration: 900, ease: "Sine.easeOut"});
    this.tweens.add({targets: x, alpha: 0, delay: 700, duration: 350, onComplete: () => x.destroy()});
  }
  // the creature's own little dance; a promise resolved at its end (at once in the recette)
  anim(cible, nom){
    const s = this[cible], b = this.base[cible], autre = this[cible === "moi" ? "lui" : "moi"], r = RATIO;
    if (TEST || !s) return Promise.resolve();
    return new Promise(fin => {
      let fait = false; const ok = () => { if (!fait) { fait = true; this.occupe[cible] = false; fin(); } };
      this.occupe[cible] = true;
      this.events.once("shutdown", ok); setTimeout(ok, 1600);
      const T = cfg => this.tweens.add({targets: s, ...cfg, onComplete: ok});
      if (nom === "attaque") T({x: b.x + (autre.x - b.x) * .3, y: b.y + (autre.y - b.y) * .3, duration: 170, yoyo: true, ease: "Quad.easeOut"});
      else if (nom === "touche") {
        s.setTintFill(0xFFFFFF); this.time.delayedCall(90, () => s.clearTint());
        this.pose(cible, "touche"); this.time.delayedCall(650, () => { if (s.active && s.frame.name === "touche") s.setFrame(s.vue); });
        this.secouer(cible);
        T({x: b.x + 6 * r, duration: 50, yoyo: true, repeat: 3});
      }
      else if (nom === "esquive") T({x: b.x + 70 * r * (cible === "lui" ? 1 : -1), duration: 150, yoyo: true, hold: 120, ease: "Quad.easeOut"});
      else if (nom === "joie") T({y: b.y - 28 * r, duration: 200, yoyo: true, repeat: 1, ease: "Quad.easeOut"});
      else if (nom === "ko" && cible === "lui") {   // the Ombre is consoled: it smiles, sparkles and floats away
        if (s.souffle) s.souffle.stop();
        this.pose("lui", "vaincu"); this.effet("lui", "consolee");
        T({y: b.y - 60 * r, alpha: 0, duration: 900, delay: 350, ease: "Sine.easeIn"});
      }
      else if (nom === "ko") { if (s.souffle) s.souffle.stop(); T({alpha: .45, scaleY: s.base * .8, duration: 550, ease: "Quad.easeIn"}); }
      else ok();
    });
  }
  // a blow shakes the arena (its backdrop is painted beyond the edges, js/jeu/fond_combat_hd.js) and tilts the creature
  secouer(cible, force = 1){
    if (calme()) return;
    const s = this[cible];
    this.cameras.main.shake(200, .007 * force);
    if (s) this.tweens.add({targets: s, angle: {from: -4 * force, to: 0}, duration: 260, ease: "Back.easeOut"});
  }
  // touched by the child: the Ombre sulks a moment, the companion jumps for joy
  touche(cible){
    if (this.occupe[cible] || TEST) return;
    const s = this[cible], b = this.base[cible];
    if (!s || !s.active) return;
    this.occupe[cible] = true;
    const fin = () => { this.occupe[cible] = false; };
    effetHD(this, "toucher", s.x, s.y - s.displayHeight * .6, s.displayHeight * .35, 9);
    if (cible === "lui") {
      this.pose("lui", "touche");
      this.tweens.add({targets: s, angle: {from: -7, to: 7}, duration: 110, yoyo: true, repeat: 2, ease: "Sine.easeInOut",
        onComplete: () => { s.setAngle(0); if (s.active && s.frame.name === "touche") s.setFrame(s.vue); fin(); }});
    } else {
      this.tweens.add({targets: s, y: b.y - 34 * RATIO, duration: 190, yoyo: true, ease: "Quad.easeOut", onComplete: fin});
      if (typeof jouerSon === "function") jouerSon("joie");
    }
  }
}

// where everything goes, in canvas pixels: the free room (zone), the stands, the creatures' heights and widest size
function dispositionCombat(S, w, h, o, c){
  const r = RATIO, W = w / r, H = h / r;
  const mode = H <= 500 && W > H * 1.25 ? "cote" : H > W * 1.1 ? "portrait" : "paysage";
  const zone = mode === "cote" ? {x: 0, w: Math.round(w * .44), h} : {x: 0, w, h: Math.round(h * (mode === "portrait" ? .5 : .58))};
  const P = {paysage: {lui: [.72, .58], moi: [.27, .9], hLui: .4, hBoss: .5, hMoi: .44},
             portrait: {lui: [.68, .46], moi: [.3, .9], hLui: .36, hBoss: .44, hMoi: .38},
             cote: {lui: [.68, .5], moi: [.3, .9], hLui: .34, hBoss: .42, hMoi: .36}}[mode];
  const lMax = Math.min(.45 * w, mode === "cote" ? .62 * zone.w : w);
  const hLui = zone.h * (o.boss ? P.hBoss : P.hLui), hMoi = zone.h * P.hMoi * [1, 1.08, 1.16][stade(c) - 1];
  // a creature's width on screen, read from its drawing when it is loaded (else square)
  const largeur = (cle, vue, haut) => {
    const k = "hd_" + cle, t = S.textures.exists(k) && S.textures.get(k);
    const f = t && (t.has(vue) ? t.get(vue) : t.has("face") ? t.get("face") : null);
    const a = f ? f.width / f.height : 1;
    return Math.min(haut * a, lMax);
  };
  const estrade = (pos, lc) => {
    const rx = Math.round(Math.max(48 * r, .65 * lc));   // 1.3 times the creature's width
    return {x: Math.round(zone.x + pos[0] * zone.w), y: Math.round(pos[1] * zone.h), rx, ry: Math.round(rx * .28)};
  };
  const lui = estrade(P.lui, largeur(o.cle, "face", hLui)), moi = estrade(P.moi, largeur(cleDe(c), "dos", hMoi));
  // the horizon: a little over a third of the free room (the whole height beside a panel)
  return {w, h, r, mode, zone, hs: zone.h, yh: Math.round(zone.h * (mode === "cote" ? .36 : .4)), lui, moi, hLui, hMoi, lMax};
}

// the arena seen by js/combat.js: life boxes in HTML, the rest drawn by the Phaser scene
function monterArene(c, o, region){
  JEU.scene.start("combat", {c, o, region: region.id});
  const S = () => { const s = JEU.scene.getScene("combat"); return s && s.pret ? s : null; };
  const boite = (id, nom, niv, pv, max, segments) => {
    const b = $(id), r = Math.max(0, pv) / max, i = b.querySelector(".pv i"), t = b.querySelector(".pvtxt");
    b.querySelector("b").textContent = affiche(nom); b.querySelector(".niv").textContent = affiche("Niv. " + niv);
    i.style.width = 100 * r + "%"; i.classList.toggle("bas", r < .3);
    if (segments) b.querySelector(".pv").style.setProperty("--n", segments);
    if (t) t.textContent = `${Math.max(0, pv)} / ${max}`;
  };
  $("etatLui").classList.toggle("chef", !!o.boss);
  $("etatLui").classList.remove("fache");
  return spectacle(S, {   // js/jeu/combat_spectacle.js: entrance, blows, ultimate attack, boss getting cross
    maj(){ boite("etatLui", o.nom, o.niveau, o.pv, o.pvMax, o.coups); boite("etatMoi", nomCompagnon(c), c.niveau, c.pv, pvMax(c)); },
    // a message in the text box at the bottom; the question panel steps aside meanwhile
    bulle(texte, ms = 1400){
      $("panneau").classList.add("cache");
      const b = $("bulleCombat"); b.textContent = affiche(texte); b.hidden = false;
      return wait(ms).then(() => { b.hidden = true; });
    }
  }, c, o, region);
}
