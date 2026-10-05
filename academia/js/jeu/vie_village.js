/* Académia : ce qui vit au village (js/jeu/monde.js). Les habitants respirent, font un petit geste de temps en temps,
   lèvent les bras quand on les touche, puis parlent avec les mains pendant que la voix lit leur phrase. Toucher le
   compagnon le fait sauter de joie, des cœurs s'envolent et la voix dit son nom. Rien ne bouge quand l'enfant demande
   le calme (ou pendant les tests automatiques). */
class VieVillage {
  constructor(scene){
    this.s = scene; this.habitants = []; this.parleur = null; this.saut = null; this.gesteMains = null;
    if (!calme()) scene.time.addEvent({delay: 3500, loop: true, callback: () => this.geste()});
  }
  // a villager standing on the square, turned the way the map says, breathing
  habitant(n){
    const s = spriteHumain(this.s, n.cle, n.x, n.y, true).setDepth(profondeur(n.y));
    s.dirRepos = DIRS[n.dir]; reposHumain(s, s.dirRepos); ombreAuSolHD(this.s, s);
    this.habitants.push(s);
    return s;
  }
  // now and then, a villager who is not talking waves for a moment
  geste(){
    if (this.s.occupe() || Math.random() < .45) return;
    const l = this.habitants.filter(s => s !== this.parleur && !s.salue);
    if (l.length) this.saluer(l[rnd(l.length)], 800);
  }
  saluer(s, duree = 700){
    if (!s || !s.active || s.salue) return;
    s.salue = true; poseHumain(s, "joie");
    this.s.time.delayedCall(TEST ? 0 : duree, () => { s.salue = false; if (s !== this.parleur) reposHumain(s, s.dirRepos); });
  }
  // talking: the villager faces the child, and its hands move while the voice reads
  parler(s){ this.parleur = s; s.salue = false; reposHumain(s, "bas"); }
  mains(s, voix){
    if (this.gesteMains) this.gesteMains.remove();
    this.gesteMains = null;
    if (calme() || !s.active) return;
    let k = 0;
    const g = this.gesteMains = this.s.time.addEvent({delay: 380, loop: true, callback: () => poseHumain(s, k++ % 2 ? "face" : "parle")});
    Promise.resolve(voix).then(() => { g.remove(); if (this.parleur === s) poseHumain(s, "face"); });
  }
  repos(s){
    if (this.gesteMains) { this.gesteMains.remove(); this.gesteMains = null; }
    this.parleur = null;
    if (s.active) reposHumain(s, s.dirRepos);
  }
  // the companion touched: it jumps twice, little hearts fly up, the voice says its name
  toucherCompagnon(wx, wy){
    const k = this.s.compagnon, c = actif();
    if (!k || !k.active || !dessine(k) || !c) return false;
    const b = k.getBounds();
    if (!Phaser.Geom.Rectangle.Contains(Phaser.Geom.Rectangle.Inflate(b, 3, 3), wx, wy)) return false;
    dire(`${nomCompagnon(c)} !`);
    if (typeof jouerSon === "function") jouerSon("rire");
    if (calme() || this.saut) return true;
    const y0 = k.y;
    this.saut = this.s.tweens.add({targets: k, y: y0 - 7, duration: 170, yoyo: true, repeat: 1, ease: "Quad.easeOut",
      onComplete: () => { this.saut = null; k.y = y0; }});
    for (let i = 0; i < 3; i++) {
      if (!this.s.textures.exists("fx_coeur_p")) break;
      const h = this.s.add.image(k.x + (i - 1) * 5, y0 - k.displayHeight * .7, "fx_coeur_p").setDepth(k.depth + 1);
      const t = 5 / h.height; h.setScale(0);
      this.s.tweens.add({targets: h, scale: t, y: h.y - 12 - i * 2, x: h.x + (i - 1) * 3, alpha: {from: 1, to: 0}, delay: i * 90,
        duration: 750, ease: "Sine.easeOut", onComplete: () => h.destroy()});
    }
    return true;
  }
  // the hero walks off: the companion's jump stops where it stands
  arreterSaut(){ if (this.saut) { this.saut.stop(); this.saut = null; } }
}
