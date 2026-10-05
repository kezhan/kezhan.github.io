/* Académia : la mise en scène du combat, vue par js/combat.js. L'entrée (l'arène s'ouvre en étoile, le compagnon
   accourt, l'Ombre surgit d'une bouffée ; le boss : l'écran s'assombrit, son bandeau passe avec son portrait, il tombe
   sur son estrade dans la poussière et fait la grimace). Les coups : la gerbe de la famille du compagnon vole jusqu'à
   l'Ombre (js/jeu/attaques_hd.js), le chiffre naît sur son ventre. Le boss qui se fâche à mi-vie, l'attaque ultime
   (arène assombrie, compagnon en grand au premier plan, nom qui traverse l'écran), l'Ombre qui s'enfuit.
   En recette (TEST) et pour qui préfère moins de mouvement (calme()), tout se résout tout de suite. */
function spectacle(S, vue, c, o, region){
  const scene = async () => {   // the scene once it is drawn (a slow network: its drawings may still be coming)
    for (let i = 0; i < 100 && !S(); i++) await wait(50);
    return S();
  };
  const anim = (k, nom) => { const s = S(); return s ? s.anim(k, nom) : Promise.resolve(); };
  return Object.assign(vue, {
    anim,
    async entree(){
      const s = await scene();
      const fini = s ? s.entrer() : Promise.resolve();
      ouvrirEtoile();   // js/transition.js: the star opens on the arena (nothing if it was not closed)
      if (o.boss) bandeauBoss(o, region);
      await fini;
    },
    async frappe({degats, type = "", ultime = false}){
      const s = S(); if (!s) return;
      const fort = type === "fort" || type === "critique" || ultime;
      const vol = anim("moi", "attaque");
      const duree = s.attaqueFamille(c.famille, fort, ultime);   // the spray flies to the Ombre
      await (calme() ? vol : wait(Math.max(170, duree - 60)));
      if (!S()) return;
      s.effet("lui", type === "critique" ? "critique" : fort ? "coup_fort" : "coup", ultime ? 1.3 : 1);
      s.texte("lui", "−" + degats, type === "critique" ? "#FFD45C" : "#FFFFFF", type === "critique" ? 1.3 : fort ? 1.15 : 1);
      if (type === "critique" && !calme()) etincellesCanvas(s, "lui");
      await Promise.all([vol, anim("lui", "touche")]);
      await wait(200);
    },
    async rate(){   // the attack misses: the Ombre steps aside
      const s = S(); if (!s) return;
      const vol = anim("moi", "attaque");
      await wait(120);
      s.texte("lui", "Raté !", "#E7E1FF", .8);
      await Promise.all([vol, anim("lui", "esquive")]);
      await wait(200);
    },
    async coupOmbre(n){
      const s = S(); if (!s) return;
      await anim("lui", "attaque");
      if (!S()) return;
      s.effet("moi", "recu"); s.texte("moi", "−" + n);
      await anim("moi", "touche");
    },
    async esquive(){
      const s = S(); if (!s) return;
      s.effet("moi", "esquive"); s.texte("moi", "Esquivé !", "#BDF2CF", .8);
      await wait(700);
    },
    async phase2(){ $("etatLui").classList.add("fache"); const s = S(); if (s) await s.phase2(); },
    async ultime(nom){ const s = S(); if (s) await s.ultime(nom); },
    async fuite(){ const s = S(); if (s) await s.fuite(); },
    ko: k => anim(k, "ko"),
    joie: () => anim("moi", "joie")
  });
}

// the boss's banner: its portrait and its name slide across the darkened arena
function bandeauBoss(o, region){
  let b = $("bandeauBoss");
  if (!b) { b = el("div", "bandeau-boss"); b.id = "bandeauBoss"; document.body.append(b); }
  b.innerHTML = `<div class="portrait"></div><div><small></small><b></b></div>`;
  b.querySelector(".portrait").append(imageHD(o.cle, "face", 84));
  b.querySelector("small").textContent = affiche(`Chef de la maison · ${region.nom}`);
  b.querySelector("b").textContent = affiche(o.nom);
  b.hidden = false; b.classList.remove("part"); void b.offsetWidth; b.classList.add("passe");
  setTimeout(() => { b.classList.add("part"); setTimeout(() => { b.hidden = true; b.classList.remove("passe", "part"); }, 500); }, calme() ? 0 : 2200);
}

// golden sparkles bursting from a creature (a critical hit)
function etincellesCanvas(s, cible){
  const t = s[cible]; if (!t) return;
  effetHD(s, "toucher", t.x - t.displayWidth * .3, t.y - t.displayHeight * .8, t.displayHeight * .4, 9);
  effetHD(s, "toucher", t.x + t.displayWidth * .3, t.y - t.displayHeight * .4, t.displayHeight * .35, 9);
}

// ---------- the scene's side ----------
// a promise for a piece of staging: resolved by it, or when the scene is redrawn (a turned tablet) or stopped, or after
// `ms` at the latest: the fight never waits for a show that will not end
SceneCombat.prototype.promesse = function(ms, f){
  return new Promise(fin => {
    let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
    this.events.once("shutdown", ok); setTimeout(ok, ms);
    f(ok);
  });
};
// before the entrance: the companion waits off screen, the Ombre is not there yet (a boss waits above the screen)
SceneCombat.prototype.preparerEntree = function(){
  if (TEST || this.d.vu) return;
  this.moi.x = -this.moi.displayWidth - 200 * RATIO;
  if (this.d.o.boss) this.lui.y = -40 * RATIO; else this.lui.setAlpha(0);
};
SceneCombat.prototype.entrer = function(){
  if (TEST || this.d.vu || calme()) {
    this.d.vu = true; this.moi.x = this.base.moi.x; this.lui.y = this.base.lui.y; this.lui.setAlpha(1);
    return Promise.resolve();
  }
  this.d.vu = true;
  const r = RATIO, accourir = (delai = 0) => this.tweens.add({targets: this.moi, x: this.base.moi.x, duration: 650, delay: delai, ease: "Back.easeOut"});
  if (!this.d.o.boss) {   // the companion runs in, the Ombre rises from a puff of smoke
    accourir();
    this.lui.x += 30 * r; this.tweens.add({targets: this.lui, alpha: 1, x: this.base.lui.x, duration: 600, delay: 250, ease: "Quad.easeOut"});
    this.time.delayedCall(250, () => this.effet("lui", "apparition"));
    return wait(850);
  }
  // the boss: a darker arena, its banner (HTML), it falls onto its stand in the dust, makes a face, then the fight
  this.d.fige = true;
  const nuit = this.add.rectangle(0, 0, this.scale.width, this.scale.height, 0x140C30, .62).setOrigin(0).setDepth(4.6);
  this.lui.setDepth(9);
  return this.promesse(4000, fin => {
    this.tweens.add({targets: this.lui, y: this.base.lui.y, duration: 520, delay: 700, ease: "Quad.easeIn", onComplete: () => {
      if (!this.lui.active) return fin();
      this.cameras.main.shake(260, .01);
      effetHD(this, "fumee", this.lui.x - this.lui.displayWidth * .35, this.lui.y - 6 * r, this.lui.displayHeight * .45, 8);
      effetHD(this, "fumee", this.lui.x + this.lui.displayWidth * .35, this.lui.y - 6 * r, this.lui.displayHeight * .45, 8);
      this.tweens.add({targets: this.lui, scaleY: {from: this.lui.base * .78, to: this.lui.base}, scaleX: {from: this.lui.base * 1.15, to: this.lui.base},
        duration: 380, ease: "Back.easeOut"});
      this.time.delayedCall(260, () => this.pose("lui", "touche"));   // the grimace
      this.time.delayedCall(1000, () => { if (this.lui.active && this.lui.frame.name === "touche") this.lui.setFrame(this.lui.vue); });
      this.time.delayedCall(700, () => {
        this.tweens.add({targets: nuit, alpha: 0, duration: 400, onComplete: () => nuit.destroy()});
        this.lui.setDepth(5); accourir();
      });
      this.time.delayedCall(1450, () => { this.d.fige = false; fin(); });
    }});
  });
};
// half its life gone: the boss gets cross (a hop, a face, puffs of steam, an orange glow that pulses)
SceneCombat.prototype.phase2 = function(deja = false){
  this.phase2Faite = true;
  const s = this.lui; if (!s || !s.active) return Promise.resolve();
  if (this.textures.exists("fx_aura") && !this.colere) {
    this.colere = this.add.image(s.x, s.y - s.displayHeight / 2, "fx_aura").setTint(0xFF8A3D).setDepth(4.8).setBlendMode(Phaser.BlendModes.ADD);
    const t = Math.max(s.displayWidth, s.displayHeight) * 1.5;
    this.colere.setDisplaySize(t, t).setAlpha(.7);
    if (!TEST) this.tweens.add({targets: this.colere, alpha: .35, duration: 520, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    const suivre = () => { if (this.colere && this.colere.active && s.active) this.colere.setPosition(s.x, s.y - s.displayHeight / 2).setVisible(s.alpha > .5); };
    this.events.on("update", suivre); this.events.once("shutdown", () => this.events.off("update", suivre));
  }
  if (deja || TEST || calme()) return Promise.resolve();
  const b = this.base.lui;
  this.pose("lui", "touche");
  effetHD(this, "fumee", s.x - s.displayWidth * .28, s.y - s.displayHeight * .95, s.displayHeight * .3, 8);
  this.time.delayedCall(180, () => effetHD(this, "fumee", s.x + s.displayWidth * .28, s.y - s.displayHeight * .95, s.displayHeight * .3, 8));
  return this.promesse(1500, fin => this.tweens.add({targets: s, y: b.y - 40 * RATIO, duration: 180, yoyo: true, repeat: 1, ease: "Quad.easeOut",
    onComplete: () => { if (s.active && s.frame.name === "touche") s.setFrame(s.vue); fin(); }}));
};
// the ultimate attack: the arena goes dark, the companion stands huge in front, the attack's name crosses the screen
SceneCombat.prototype.ultime = function(nom){
  if (TEST || calme()) return Promise.resolve();
  const r = RATIO, W = this.scale.width, H = this.scale.height, c = this.d.c;
  this.d.fige = true;
  const nuit = this.add.rectangle(0, 0, W, H, 0x140C30, 0).setOrigin(0).setDepth(20);
  this.tweens.add({targets: nuit, fillAlpha: .7, duration: 250});
  const cle = cleDe(c), k = "hd_" + cle;
  const grand = this.textures.exists(k) ? this.add.image(W * .28, H + 10 * r, k, "face").setOrigin(.5, 1).setDepth(21) : null;
  if (grand) {
    const hMax = Math.min(H * .72, W * .5 / (grand.width / grand.height));
    grand.setScale(hMax / grand.height * .6).setAlpha(0);
    this.tweens.add({targets: grand, alpha: 1, scale: hMax / grand.height, y: H - 4 * r, duration: 420, ease: "Back.easeOut"});
    effetHD(this, "evolution", grand.x, H - hMax * .55, hMax * .8, 20.5);
  }
  const t = this.add.text(W + 20 * r, H * .3, nom, {fontFamily: "Fredoka, sans-serif", fontSize: Math.round(Math.min(64, W / r / 10) * r) + "px",
    color: "#FFD45C", fontStyle: "bold", stroke: "#1D1640", strokeThickness: 10 * r}).setOrigin(0, .5).setDepth(22);
  return this.promesse(2500, fin => {
    this.tweens.add({targets: t, x: (W - t.width) / 2, duration: 380, ease: "Cubic.easeOut", onComplete: () => {
      this.tweens.add({targets: t, x: -t.width - 20 * r, duration: 380, delay: 650, ease: "Cubic.easeIn", onComplete: () => t.destroy()});
    }});
    this.time.delayedCall(1250, () => {
      this.tweens.add({targets: nuit, fillAlpha: 0, duration: 300, onComplete: () => nuit.destroy()});
      if (grand) this.tweens.add({targets: grand, alpha: 0, y: H + grand.displayHeight * .3, duration: 300, onComplete: () => grand.destroy()});
      this.d.fige = false; fin();
    });
  });
};
// not one right answer: the Ombre hops and runs away into the grass, in a puff
SceneCombat.prototype.fuite = function(){
  const s = this.lui; if (!s || !s.active || TEST || calme()) { if (s) s.setAlpha(0); return Promise.resolve(); }
  if (s.souffle) s.souffle.stop();
  return this.promesse(2000, fin => {
    this.tweens.add({targets: s, y: s.y - 40 * RATIO, duration: 180, yoyo: true, ease: "Quad.easeOut", onComplete: () => {
      s.setFlipX(true);
      this.tweens.add({targets: s, x: this.scale.width + s.displayWidth, duration: 650, ease: "Quad.easeIn", onComplete: fin});
      this.time.delayedCall(420, () => effetHD(this, "fumee", this.scale.width - 30 * RATIO, s.y - s.displayHeight * .4, s.displayHeight * .6, 8));
    }});
  });
};
