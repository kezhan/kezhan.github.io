/* Académia : ce qui vit au village (js/jeu/monde.js). Les habitants respirent, font un petit geste de temps en temps,
   lèvent les bras quand on les touche, puis parlent avec les mains pendant que la voix lit leur phrase. Toucher le
   compagnon le fait sauter de joie, des cœurs s'envolent et la voix dit son nom ; Plumix, Bärli et Léiwchen ajoutent un
   mot de leur langue (« I love you! », « Ich habe Hunger! », « Moien »), écrit dans la bulle avec son sens. Une fois
   par séance au plus, le compagnon éternue (« Atchoum ! ») et l'habitant le plus proche lui souhaite « Bless you! »,
   « Gesundheit! » ou « Gesondheet! » ; le toucher ensuite le lui fait redire, avec son sens. Rien ne bouge quand
   l'enfant demande le calme (ou pendant les tests automatiques), et rien ne coupe un habitant qui parle. */
// once a sitting, never in its first 45 s; window.__eternuement (set before the page, as the save, by the recette:
// tests/recette_apprendre.py) makes it come at the first quiet moment
const ETERNUEMENT = {fait: false, depuis: performance.now(), vite: !!window.__eternuement};
class VieVillage {
  constructor(scene){
    this.s = scene; this.habitants = []; this.parleur = null; this.saut = null; this.gesteMains = null;
    if (!calme()) scene.time.addEvent({delay: 3500, loop: true, callback: () => this.geste()});
    const vite = ETERNUEMENT.vite;
    if (!TEST) scene.time.addEvent({delay: vite ? 1500 : 6000, loop: true, callback: () => {
      if (!ETERNUEMENT.fait && performance.now() - ETERNUEMENT.depuis > (vite ? 2000 : 45000) && (vite || Math.random() < .12)) this.eternuer();
    }});
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
  // the companion touched: it jumps twice, little hearts fly up, the voice says its name, then a word of its language
  // for a language companion (in the bubble, with its meaning); a villager speaking: it only jumps
  toucherCompagnon(wx, wy){
    const k = this.s.compagnon, c = actif();
    if (!k || !k.active || !dessine(k) || !c) return false;
    const b = k.getBounds();
    if (!Phaser.Geom.Rectangle.Contains(Phaser.Geom.Rectangle.Inflate(b, 3, 3), wx, wy)) return false;
    const nom = nomCompagnon(c), m = typeof habitantParle === "function" && habitantParle() ? null : motCompagnon(c.famille);
    if (m) bulleMotVillage(c, nom, m, suiteDeVoix([() => dire(`${nom} !`), () => direMot(m)]));
    else if (!(typeof habitantParle === "function" && habitantParle())) dire(`${nom} !`);
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
  // the companion sneezes (« Atchoum ! » by the French voice, a little jolt), and the villager nearest to it, ten tiles
  // at most, wishes it well in their language; that villager says it again, with its meaning, when touched next
  // (js/jeu/langues_habitants.js). Never before a first fight, nor while something else happens or a voice speaks.
  // True when it sneezed
  eternuer(){
    const s = this.s, k = s.compagnon, c = actif(), p = P();
    if (ETERNUEMENT.fait || !k || !k.active || !dessine(k) || !c || !p || !(p.hist || []).length) return false;
    if (s.occupe() || s.enMarche || s.moment || bulleVillageOuverte() || (typeof voixEnCours === "function" && voixEnCours())) return false;
    const d = n => Phaser.Math.Distance.Between(n.s.x, n.s.y, k.x, k.y);
    const n = (s.pnj || []).filter(q => q.s && q.s.active && LANGUES_HABITANTS[q.cle]).sort((a, b) => d(a) - d(b))[0];
    const m = n && d(n) <= 10 * CASE ? motDuJeu("a_tes_souhaits", LANGUES_HABITANTS[n.cle].langue) : null;
    if (!m) return false;
    ETERNUEMENT.fait = true;
    if (!calme()) {   // it breathes in, then the sneeze jolts it forward
      arreterSouffle(k);
      s.tweens.add({targets: k, scaleY: k.base * 1.1, duration: 380, ease: "Sine.easeIn", onComplete: () => {
        if (k.active) s.tweens.add({targets: k, scaleY: k.base * .88, y: k.y + 1.5, duration: 90, yoyo: true, ease: "Quad.easeOut", onComplete: () => { if (k.active) reposCreature(k); }});
      }});
      const t = s.add.text(k.x, k.y - k.displayHeight - 2, "Atchoum !", {fontFamily: "Fredoka, sans-serif", fontSize: "6px", color: "#FFFFFF",
        fontStyle: "bold", stroke: "#1D1640", strokeThickness: 2, padding: {x: 1, y: 1}}).setOrigin(.5, 1).setResolution(12).setDepth(9996).setAlpha(0);
      s.tweens.add({targets: t, alpha: 1, y: t.y - 4, delay: 380, duration: 250, hold: 900, yoyo: true, onComplete: () => t.destroy()});
    }
    MOT_EN_ATTENTE[n.cle] = {mot: m.mot, sens: m.sens};
    // something else said meanwhile (the child touched): the villager keeps the word for the next visit
    const atchoum = dire("Atchoum !"), g = typeof generation !== "undefined" ? generation : 0;
    atchoum.then(() => {
      if (!n.s.active || ecranCourant !== "monde" || dialogueOuvert() || (typeof generation !== "undefined" && generation !== g)) return;
      this.saluer(n.s, 1200);
      bulleMotVillage(n.cle, n.nom, m);
    });
    return true;
  }
}
