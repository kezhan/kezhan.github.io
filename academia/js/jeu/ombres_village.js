/* Académia : les Ombres qu'on voit dans les hautes herbes. Une par touffe, violette comme au combat ; elle se promène
   dans son herbe sans jamais venir coller le héros. La toucher, ou passer juste à côté, lance le combat ; battue,
   elle s'efface dans un nuage et une autre revient un peu plus tard. L'enfant voit ce qu'il chasse. */
const OMBRE_REVIENT_MS = 5000, OMBRE_PAS_MS = 1500;

class OmbresVillage {
  constructor(scene){
    this.s = scene; this.liste = [];
    this.cases = {};   // region -> the tiles of its tall grass
    const h = scene.carte.herbe;
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) if (h[y][x]) (this.cases[h[y][x]] = this.cases[h[y][x]] || []).push({x, y});
    Object.keys(this.cases).forEach(r => this.poser(r, false));
    scene.time.addEvent({delay: OMBRE_PAS_MS, loop: true, callback: () => this.promener()});
  }
  a(x, y){ return this.liste.find(o => o.x === x && o.y === y) || null; }
  // an Ombre on the hero's tile or right next to it
  pres(x, y){ return this.liste.find(o => !o.combat && Math.abs(o.x - x) + Math.abs(o.y - y) <= 1) || null; }
  occupee(x, y){
    const s = this.s, t = s.traces[0];
    return !!this.a(x, y) || (s.case.x === x && s.case.y === y) || (!!t && t.x === x && t.y === y);
  }
  loinDuHeros(c){ return Math.abs(c.x - this.s.case.x) + Math.abs(c.y - this.s.case.y) > 1; }
  // an Ombre in this grass; one coming back (`bouffee`) appears in a puff of smoke
  poser(region, bouffee = true){
    const libres = this.cases[region].filter(c => !this.occupee(c.x, c.y) && this.loinDuHeros(c));
    if (!libres.length) { this.s.time.delayedCall(2000, () => this.poser(region, bouffee)); return null; }   // the hero stands in the grass: later
    const c = libres[rnd(libres.length)], qui = ombre(regionDe(region), false);
    const o = {region, x: c.x, y: c.y};
    o.s = spriteCreature(this.s, qui.cle, c.x, c.y).setDepth(profondeur(c.y));
    ombreAuSolHD(this.s, o.s);
    halo(this.s, o.s, 0x7B4DFF, 1.14, .6);   // its violet glow (js/jeu/sprites_hd.js)
    o.s.setAlpha(0); this.s.tweens.add({targets: o.s, alpha: 1, duration: TEST ? 1 : 400});
    if (bouffee) this.fumee(o);
    this.liste.push(o);
    return o;
  }
  promener(){
    if (this.s.occupe()) return;
    this.liste.forEach(o => {
      if (o.fige || o.combat || o.bouge || Math.random() < .4) return;
      const herbe = this.s.carte.herbe;
      const voisins = [[0, 1], [1, 0], [0, -1], [-1, 0]].map(([a, b]) => ({x: o.x + a, y: o.y + b}))
        .filter(c => herbe[c.y] && herbe[c.y][c.x] === o.region && !this.occupee(c.x, c.y) && this.loinDuHeros(c));
      if (!voisins.length) return;
      const c = voisins[rnd(voisins.length)], d = c.x > o.x ? "droite" : c.x < o.x ? "gauche" : c.y > o.y ? "bas" : "haut";
      o.x = c.x; o.y = c.y; o.bouge = true;
      secouerHerbeHD(this.s, c.x, c.y);
      marcherCreature(o.s, d, 420);
      this.s.tweens.add({targets: o.s, x: c.x * CASE + 8, y: c.y * CASE + o.s.dy, duration: TEST ? 1 : 420,
        onComplete: () => { o.bouge = false; o.s.setDepth(profondeur(c.y)); reposCreature(o.s); }});
    });
  }
  // the child chose this one: it stops and hops, so that the hero can reach it
  viser(o){ o.fige = true; if (!TEST && !o.bouge) this.s.tweens.add({targets: o.s, y: o.y * CASE + o.s.dy - 4, duration: 120, yoyo: true}); }
  // after its fight: beaten, it vanishes in a puff and another one comes back later; otherwise it wanders again
  apres(o, gagne){
    if (!o) return;
    o.combat = false; o.fige = false;
    if (!gagne) return;
    this.liste = this.liste.filter(x => x !== o);
    this.fumee(o);
    if (o.s.souffle) { o.s.souffle.stop(); o.s.souffle = null; }   // it shrinks from its own size (a drawing is scaled down)
    this.s.tweens.add({targets: o.s, alpha: 0, scale: o.s.base * .4, duration: TEST ? 1 : 350, onComplete: () => o.s.destroy()});
    this.s.time.delayedCall(TEST ? 10 : OMBRE_REVIENT_MS, () => this.poser(o.region));
  }
  fumee(o){ effetHD(this.s, "fumee", o.s.x, o.s.y - o.s.displayHeight / 2, o.s.displayHeight, o.s.depth + 4); }
}
