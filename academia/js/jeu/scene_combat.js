/* Académia : la scène de combat, à la manière des jeux de créatures. L'Ombre en haut à droite, face à nous ;
   le compagnon de dos en bas à gauche, chacun sur son estrade, devant le décor de la région. Dessins en haute
   définition (js/jeu/qualite.js) : l'Ombre grogne, fait la grimace quand elle est touchée et sourit, consolée, quand
   elle est vaincue. Coordonnées en pixels de la toile (RATIO par pixel CSS). Les cases de vie sont en HTML ; les
   attaques restent des questions posées dans le panneau du bas, qui s'efface pendant les coups. */
class SceneCombat extends Phaser.Scene {
  constructor(){ super("combat"); }
  init(d){ this.d = d; }
  preload(){ chargerAtlas(this, [cleDe(this.d.c), this.d.o.cle]); chargerFondsHD(this.load, QUALITE); chargerEffetsHD(this.load, QUALITE); }
  create(){
    const {c, o, region} = this.d, w = this.scale.width, h = this.scale.height, r = RATIO;
    const portrait = h > w * 1.1, part = portrait ? .5 : .58, hs = Math.round(h * part);
    const estrade = (x, y, rx) => ({x: Math.round(x), y: Math.round(y), rx: Math.round(rx), ry: Math.round(rx * .28)});
    const L = this.L = {w, h, hs, yh: Math.round(hs * .4), r,
      lui: estrade(w * (portrait ? .68 : .72), hs * .58, Math.min(Math.max(w * .15, 60 * r), 180 * r)),
      moi: estrade(w * (portrait ? .3 : .27), hs * .9, Math.min(Math.max(w * .19, 72 * r), 220 * r))};
    document.documentElement.style.setProperty("--scene", Math.round(hs / r) + "px");
    dessinerFondHD(this, L, region, o.boss);   // js/jeu/fond_combat_hd.js

    // the creatures, feet on their stands
    this.lui = this.creature(o.cle, "face", L.lui, hs * (o.boss ? .5 : .4));
    this.moi = this.creature(cleDe(c), "dos", L.moi, hs * .44 * [1, 1.08, 1.16][stade(c) - 1]);
    if (this.lui.preFX) this.lui.preFX.addGlow(o.boss ? 0xFF5EC9 : 0x7B4DFF, 3, 0, false, .1, 10);
    if (stade(c) === 3 && this.moi.preFX) this.moi.preFX.addGlow(0xFFD45C, 3, 0);
    this.base = {lui: {x: this.lui.x, y: this.lui.y}, moi: {x: this.moi.x, y: this.moi.y}};
    this.pret = true;
    if (!TEST && !this.d.vu) {   // entrance: the companion runs in, the Ombre rises from a puff of smoke
      this.d.vu = true;
      this.moi.x = -this.moi.displayWidth; this.tweens.add({targets: this.moi, x: this.base.moi.x, duration: 650, ease: "Back.easeOut"});
      this.lui.setAlpha(0).x += 30 * r; this.tweens.add({targets: this.lui, alpha: 1, x: this.base.lui.x, duration: 600, delay: 250, ease: "Quad.easeOut"});
      this.time.delayedCall(250, () => this.effet("lui", "apparition"));
    }
    // a turned tablet: the scene is drawn again for the new size, once the size has really changed
    let minuteur = null;
    const taille = () => {
      if (minuteur) minuteur.remove();
      minuteur = this.time.delayedCall(150, () => { if (this.scale.width !== w || this.scale.height !== h) this.scene.restart(this.d); });
    };
    this.scale.on("resize", taille);
    this.events.once("shutdown", () => { this.pret = false; this.scale.off("resize", taille); });
  }
  // a creature: its drawing in pose `vue` (face or back), feet on its stand, a soft shadow under it
  creature(cle, vue, p, hauteur){
    const t = this.textures.get("hd_" + cle);
    const s = this.add.sprite(p.x, p.y + p.ry * .35, "hd_" + cle, t.has(vue) ? vue : "face").setOrigin(.5, 1);
    s.base = hauteur / s.height; s.setScale(s.base); s.hd = cle; s.vue = s.frame.name;
    s.setDepth(5);
    this.add.graphics().setDepth(4).fillStyle(0x000000, .18).fillEllipse(p.x, p.y + p.ry * .3, s.displayWidth * .75, p.ry * .7);
    if (!TEST) s.souffle = this.tweens.add({targets: s, scaleY: s.base * 1.03, scaleX: s.base * .985, duration: 1000, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    return s;
  }
  pose(cible, nom){   // a high-definition expression, when that drawing has it
    const s = this[cible];
    if (s && s.hd && this.textures.get("hd_" + s.hd).has(nom)) s.setFrame(nom);
  }
  effet(cible, nom){   // js/jeu/effets_hd.js, centred on the creature and as big as it
    const s = this[cible];
    if (s) effetHD(this, nom, s.x, s.y - s.displayHeight / 2, s.displayHeight, 6);
  }
  texte(cible, t, couleur = "#FFFFFF", taille = 1){
    const s = this[cible], r = RATIO;
    const x = this.add.text(s.x, s.y - s.displayHeight - 4 * r, t, {fontFamily: "Fredoka, sans-serif", fontSize: Math.round(44 * taille * r) + "px",
      color: couleur, fontStyle: "bold", stroke: "#1D1640", strokeThickness: 8 * r}).setOrigin(.5, 1).setDepth(7);
    this.tweens.add({targets: x, y: x.y - 56 * r, alpha: 0, duration: TEST ? 1 : 1100, ease: "Cubic.easeOut", onComplete: () => x.destroy()});
  }
  anim(cible, nom){
    const s = this[cible], b = this.base[cible], autre = this[cible === "moi" ? "lui" : "moi"], r = RATIO;
    if (TEST || !s) return Promise.resolve();
    return new Promise(fin => {
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      this.events.once("shutdown", ok); setTimeout(ok, 1500);
      const T = cfg => this.tweens.add({targets: s, ...cfg, onComplete: ok});
      if (nom === "attaque") T({x: b.x + (autre.x - b.x) * .3, y: b.y + (autre.y - b.y) * .3, duration: 170, yoyo: true, ease: "Quad.easeOut"});
      else if (nom === "touche") {
        s.setTintFill(0xFFFFFF); this.time.delayedCall(90, () => s.clearTint());
        this.pose(cible, "touche"); this.time.delayedCall(650, () => { if (s.active && s.frame.name === "touche") s.setFrame(s.vue); });
        this.cameras.main.shake(200, .008);
        T({x: b.x + 6 * r, duration: 50, yoyo: true, repeat: 3});
      }
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
}

// the arena seen by js/combat.js: life boxes in HTML, the rest drawn by the Phaser scene
function monterArene(c, o, region){
  JEU.scene.start("combat", {c, o, region: region.id});
  const S = () => { const s = JEU.scene.getScene("combat"); return s && s.pret ? s : null; };
  const boite = (id, nom, niv, pv, max) => {
    const b = $(id), r = Math.max(0, pv) / max, i = b.querySelector(".pv i"), t = b.querySelector(".pvtxt");
    b.querySelector("b").textContent = nom; b.querySelector(".niv").textContent = "Niv. " + niv;
    i.style.width = 100 * r + "%"; i.classList.toggle("bas", r < .3);
    if (t) t.textContent = `${Math.max(0, pv)} / ${max}`;
  };
  return {
    maj(){ boite("etatLui", o.nom, o.niveau, o.pv, o.pvMax); boite("etatMoi", nomCompagnon(c), c.niveau, c.pv, pvMax(c)); },
    anim(k, nom){ const s = S(); return s ? s.anim(k, nom) : Promise.resolve(); },
    degats(k, n, type = ""){
      const s = S(); if (!s) return;
      if (type === "rate") return s.texte(k, "Raté !", "#E7E1FF", .8);
      if (type === "esquive") { s.effet(k, "esquive"); return s.texte(k, "🛡️ Esquivé !", "#BDF2CF", .8); }
      s.effet(k, k === "lui" ? (type === "critique" ? "critique" : n > 12 ? "coup_fort" : "coup") : "recu");
      s.texte(k, "−" + n, type === "critique" ? "#FFD45C" : "#FFFFFF", type === "critique" ? 1.3 : 1);
    },
    // a message in the text box at the bottom; the question panel steps aside meanwhile
    bulle(texte, ms = 1400){
      $("panneau").classList.add("cache");
      const b = $("bulleCombat"); b.textContent = texte; b.hidden = false;
      return wait(ms).then(() => { b.hidden = true; });
    }
  };
}
