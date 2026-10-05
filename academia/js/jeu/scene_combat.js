/* Académia : la scène de combat, à la manière des jeux de créatures. L'Ombre en haut à droite, face à nous ;
   le compagnon de dos en bas à gauche, chacun sur son estrade, devant le décor de la région (js/jeu/fond_combat.js).
   Les cases de vie sont en HTML, nettes à toute taille ; les attaques restent des questions posées dans le panneau
   du bas (js/combat.js, js/question_vue.js), qui s'efface pendant les coups pour laisser voir la scène. */
const TEINTE_OMBRE = 0xB9A8FF;   // the Ombres are violet: an enemy is recognised at once
const VIOLET = [.3 * .8, .59 * .8, .11 * .8, 0, 0,  .3 * .52, .59 * .52, .11 * .52, 0, 0,  .3 * 1.2, .59 * 1.2, .11 * 1.2, 0, 0,  0, 0, 0, 1, 0];

class SceneCombat extends Phaser.Scene {
  constructor(){ super("combat"); }
  init(d){ this.d = d; }
  create(){
    const {c, o, region} = this.d, w = this.scale.width, h = this.scale.height, portrait = h > w * 1.1;
    const part = portrait ? .5 : .58;   // share of the screen above the question panel
    const k = this.k = Math.max(2, Math.min(6, Math.round(w / (portrait ? 140 : 300)), Math.floor(h * part / 110)));
    const W = Math.ceil(w / k), H = Math.ceil(h / k), Hs = Math.floor(h * part / k);
    const estrade = (x, y, r) => ({x: Math.round(x), y: Math.round(y), rx: Math.round(r), ry: Math.round(r * .3)});
    const L = this.L = {W, H, Hs, yh: Math.round(Hs * .4),
      lui: estrade(W * (portrait ? .68 : .72), Hs * .58, Math.min(Math.max(W * .15, 20), 46)),
      moi: estrade(W * (portrait ? .3 : .27), Hs * .9, Math.min(Math.max(W * .19, 24), 56))};
    this.cameras.main.setZoom(k).centerOn(w / k / 2, h / k / 2).setRoundPixels(true);
    document.documentElement.style.setProperty("--scene", Hs * k + "px");
    dessinerFond(this, L, region, o.boss);

    // the creatures on their stands, in whole screen pixels at any zoom
    const net = m => Math.max(1, Math.round(m * k)) / k;
    this.teinte = {lui: o.boss ? null : TEINTE_OMBRE, moi: null};
    this.lui = this.add.sprite(L.lui.x, L.lui.y + 1, "monstre" + o.sprite, 0).setOrigin(.5, 1).setScale(net(o.boss ? 3 : 2.2)).setDepth(5);
    this.moi = this.add.sprite(L.moi.x, L.moi.y + 1, "monstre" + spriteDe(c), 1).setOrigin(.5, 1).setScale(net(2.4 * echelleStade(stade(c)))).setDepth(5);
    if (this.lui.preFX) {   // brightness kept, colours turned violet, a soft glow around
      if (!o.boss) { this.lui.preFX.addColorMatrix().set(VIOLET); this.teinte.lui = null; }
      this.lui.preFX.addGlow(o.boss ? 0xFF5EC9 : 0x7B4DFF, 2, 0, false, .1, 8);
    }
    this.teinter("lui");
    if (stade(c) === 3 && this.moi.preFX) this.moi.preFX.addGlow(0xFFD45C, 2, 0);
    const ombres = this.add.graphics().setDepth(4);
    [[this.lui, L.lui], [this.moi, L.moi]].forEach(([s, p]) => disque(ombres, p.x, p.y, Math.round(s.displayWidth * .36), 2, 0x000000, .22));
    this.lui.play(`monstre${o.sprite}-bas`); this.moi.play(`monstre${spriteDe(c)}-haut`);
    this.lui.anims.timeScale = .6; this.moi.anims.timeScale = .6;   // a calm idle
    this.base = {lui: {x: this.lui.x, y: this.lui.y}, moi: {x: this.moi.x, y: this.moi.y}};
    this.pret = true;
    if (!TEST && !this.d.vu) {   // entrance: the companion runs in, the Ombre rises from a puff of smoke
      this.d.vu = true;
      this.moi.x = -this.moi.displayWidth; this.tweens.add({targets: this.moi, x: this.base.moi.x, duration: 650, ease: "Back.easeOut"});
      this.lui.setAlpha(0).x += 24; this.tweens.add({targets: this.lui, alpha: 1, x: this.base.lui.x, duration: 600, delay: 250, ease: "Quad.easeOut"});
      this.time.delayedCall(250, () => this.effet("lui", 18, 1.3));
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
  teinter(cible){ const s = this[cible], t = this.teinte[cible]; s.clearTint(); if (t) s.setTint(t); }
  effet(cible, n, echelle = 1){
    const s = this[cible], e = this.add.sprite(s.x, s.y - s.displayHeight / 2, "effet" + n).setScale(echelle * s.scaleX / 2.2).setDepth(6);
    e.play("effet" + n); e.once("animationcomplete", () => e.destroy());
  }
  texte(cible, t, couleur = "#FFFFFF", taille = 1){
    const s = this[cible];
    const x = this.add.text(s.x, s.y - s.displayHeight - 2, t, {fontFamily: "Fredoka, sans-serif", fontSize: Math.round(12 * taille) + "px",
      color: couleur, fontStyle: "bold", stroke: "#1D1640", strokeThickness: 3}).setOrigin(.5, 1).setResolution(this.k * 2).setDepth(7);
    this.tweens.add({targets: x, y: x.y - 14, alpha: 0, duration: TEST ? 1 : 1100, ease: "Cubic.easeOut", onComplete: () => x.destroy()});
  }
  anim(cible, nom){
    const s = this[cible], b = this.base[cible], autre = this[cible === "moi" ? "lui" : "moi"];
    if (TEST || !s) return Promise.resolve();
    return new Promise(fin => {
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      this.events.once("shutdown", ok); setTimeout(ok, 1500);
      const T = cfg => this.tweens.add({targets: s, ...cfg, onComplete: ok});
      if (nom === "attaque") T({x: b.x + (autre.x - b.x) * .3, y: b.y + (autre.y - b.y) * .3, duration: 170, yoyo: true, ease: "Quad.easeOut"});
      else if (nom === "touche") {
        s.setTintFill(0xFFFFFF); this.time.delayedCall(90, () => this.teinter(cible));
        this.cameras.main.shake(200, .008);
        T({x: b.x + 3, duration: 50, yoyo: true, repeat: 3});
      }
      else if (nom === "joie") T({y: b.y - 10, duration: 200, yoyo: true, repeat: 1, ease: "Quad.easeOut"});
      else if (nom === "ko") { this.effet(cible, 18, 1.4); T({alpha: 0, scaleY: s.scaleY * .3, duration: 550, ease: "Quad.easeIn"}); }
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
      if (type === "esquive") { s.effet(k, 2, 1.2); return s.texte(k, "🛡️ Esquivé !", "#BDF2CF", .8); }
      s.effet(k, k === "lui" ? (type === "critique" ? 12 : n > 12 ? 11 : 7) : 20, type === "critique" ? 1.4 : 1.1);
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
