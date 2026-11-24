/* Académia : les trouvailles qui brillent et le visiteur du jour dans la scène du village (js/jeu/monde.js), au village
   et dans ses cinq quartiers (règles : js/surprises.js ; ce qu'on trouve : js/surprises_donnees.js).
   1. Le trésor du jour du lieu : une étincelle qui bat sur son halo doré, posée dans l'herbe à un endroit qui change
      chaque jour (une case d'herbe dégagée, que le héros peut atteindre, loin des chemins, des portes et des
      habitants ; au village, à un écran au plus du plan planté sous la place). Touchée, le héros y va ; arrivé, l'écran d'une chose montre ce qu'il a trouvé (js/maison_chose.js)
      et l'étincelle s'éteint jusqu'au lendemain.
   2. Le visiteur du jour, quand c'est son jour et son lieu : l'animal est debout dans l'herbe, il sautille, une
      petite étincelle flotte sur lui tant que la sienne est à gagner. Touché, le héros va devant lui, et le même
      écran s'ouvre. Le héros passe à travers lui : il ne ferme aucun chemin.
   3. Au village, une fois par jour, quand tout est calme, le Sage dit où est l'animal et qu'un trésor brille
      (HistoireVillage.update, js/jeu/histoire_village.js).
   Dessins : outils/village/scripts/surprises.py, dans assets/<qualité>/surprises. */
const RAYON_TROUVAILLE = 10;   // world units around the sparkle that take the touch (a finger is wider than it)
const PRES_DE_LA_PLACE = {x: 10, y: 7};   // tiles around the map of the village where the day's treasure and visitor stand

function chargerSurprisesHD(load, lieu){
  if (!load.textureManager.exists("ico_etincelle")) load.image("ico_etincelle", iconeSrc("etincelle"));
  const j = P() ? visiteurDuJour() : null;
  if (j && j.lieu === lieu && !load.textureManager.exists("sv_" + j.v.id)) load.image("sv_" + j.v.id, srcSurprise("visiteur", j.v.id));
}

class SurprisesVillage {
  constructor(scene){
    this.s = scene; this.lieu = scene.quartier || "village"; this.vise = null; this.tresor = null; this.visiteur = null;
    if (!surprisesActives()) return;   // a recette that did not ask for them (js/surprises.js)
    const cases = this.casesLibres(), r = hasardDe(`${jourMaison()}|${this.lieu}`), anim = !calme();
    if (!cases.length || !scene.textures.exists("ico_etincelle")) return;
    // the day's treasure: where it shines today
    const c = cases[Math.floor(r() * cases.length)], x = (c.x + .5) * CASE, y = (c.y + .5) * CASE;
    const halo = scene.add.circle(x, y + 2, 6, 0xFFE27A, .4).setDepth(profondeur(c.y, 1));
    const im = scene.add.image(x, y, "ico_etincelle").setDisplaySize(10, 10).setDepth(profondeur(c.y, 2));
    if (anim) {
      scene.tweens.add({targets: im, scale: im.scale * 1.3, angle: 12, duration: 620, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      scene.tweens.add({targets: halo, alpha: .12, scale: 1.5, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
    this.tresor = {c, im, halo};
    // the day's visitor, when it is here: a few steps from the treasure
    const j = visiteurDuJour(), loin = cases.filter(q => Math.abs(q.x - c.x) + Math.abs(q.y - c.y) >= 4);
    if (j && j.lieu === this.lieu && loin.length && scene.textures.exists("sv_" + j.v.id)) {
      const q = loin[Math.floor(r() * loin.length)], vx = (q.x + .5) * CASE, vy = (q.y + 1) * CASE;
      scene.textures.get("sv_" + j.v.id).setFilter(Phaser.Textures.FilterMode.LINEAR);
      const v = scene.add.image(vx, vy, "sv_" + j.v.id).setOrigin(.5, 1).setDisplaySize(1.5 * CASE, 1.5 * CASE).setDepth(profondeur(q.y, 0));
      const bulle = scene.add.image(vx, vy - 1.5 * CASE - 2, "ico_etincelle").setDisplaySize(8, 8).setDepth(9000);
      if (anim) {
        scene.tweens.add({targets: v, y: vy - 2.5, duration: 240, yoyo: true, repeat: -1, repeatDelay: 1300, ease: "Quad.easeOut"});
        scene.tweens.add({targets: bulle, y: bulle.y - 2.5, duration: 720, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      }
      this.visiteur = {c: q, devant: {x: q.x, y: q.y + 1}, im: v, bulle, v: j.v};
    }
    this.maj();
  }
  // the tiles of open grass the hero can walk to, the same list whatever his tile: each one and its eight neighbours
  // are free grass (no tall grass, no path, no door), nothing tall stands just below, no villager is near
  casesLibres(){
    const s = this.s, c = s.carte, l = [], vu = new Set([s.case.x + "," + s.case.y]), file = [[s.case.x, s.case.y]];
    const pre = (x, y) => s.libre(x, y) && (c.sol[y][x] === "herbe" || c.sol[y][x] === "pre") && !c.herbe[y][x] && !c.maison[y][x] && !c.portes.some(p => p.x === x && p.y === y);
    const degagee = (x, y) => { for (let b = -1; b <= 1; b++) for (let a = -1; a <= 1; a++) if (!pre(x + a, y + b)) return false; return s.libre(x, y + 2); };
    const seule = (x, y) => !(s.pnj || []).some(n => Math.abs(n.x - x) <= 2 && Math.abs(n.y - y) <= 2) && !(c.sorties || []).some(q => q.cases.some(([a, b]) => Math.abs(a - x) <= 2 && Math.abs(b - y) <= 2));
    while (file.length) {
      const [x, y] = file.shift();
      if (degagee(x, y) && seule(x, y)) l.push({x, y});
      [[1, 0], [-1, 0], [0, 1], [0, -1]].forEach(([a, b]) => { const k = (x + a) + "," + (y + b); if (!vu.has(k) && s.libre(x + a, y + b)) { vu.add(k); file.push([x + a, y + b]); } });
    }
    // at the village, a wide map: what shines stays about one screen around the map of the village, under the square
    const o = !s.quartier && s.sorties && s.sorties.devant, pres = o ? l.filter(q => Math.abs(q.x - o.x) <= PRES_DE_LA_PLACE.x && Math.abs(q.y - o.y) <= PRES_DE_LA_PLACE.y) : [];
    return (pres.length >= 6 ? pres : l).sort((a, b) => a.y - b.y || a.x - b.x);
  }
  // the scene as the game stands: the sparkle until today's treasure is picked up, the visitor's étincelle until won
  maj(){
    const t = this.tresor, v = this.visiteur, la = !!trouvailleDuJour(this.lieu);
    if (t) { t.im.setVisible(la); t.halo.setVisible(la); }
    if (v) v.bulle.setVisible(etincelleChose({region: this.lieu, id: v.v.id}));
  }
  // what the finger is on: "tresor", "visiteur" or null
  sous(wx, wy){
    const t = this.tresor, v = this.visiteur;
    if (v && (v.im.getBounds().contains(wx, wy) || (v.bulle.visible && Phaser.Math.Distance.Between(wx, wy, v.bulle.x, v.bulle.y) < 7))) return "visiteur";
    return t && t.im.visible && Phaser.Math.Distance.Between(wx, wy, t.im.x, t.im.y) <= RAYON_TROUVAILLE ? "tresor" : null;
  }
  but(q){ return q === "tresor" ? this.tresor.c : this.visiteur.devant; }
  // the sparkle or the visitor is touched: the hero walks there (already there: it opens at once)
  touche(wx, wy){
    const q = this.sous(wx, wy), s = this.s;
    this.vise = null;
    if (!q) return false;
    const d = this.but(q);
    s.marquer(d.x, d.y);
    if (q === "visiteur" && !calme()) s.tweens.add({targets: this.visiteur.im, angle: {from: -5, to: 5}, duration: 90, yoyo: true, repeat: 1, onComplete: () => this.visiteur.im.setAngle(0)});
    if (s.case.x === d.x && s.case.y === d.y) { this.ouvrir(q); return true; }
    s.allerVers(d.x, d.y);
    this.vise = q;
    return true;
  }
  // the hero has stopped (Monde.arrive): where the walk it started was headed, it opens
  arrive(){
    const q = this.vise, s = this.s;
    this.vise = null;
    if (!q) return false;
    const d = this.but(q);
    if (s.case.x !== d.x || s.case.y !== d.y) return false;
    this.ouvrir(q);
    return true;
  }
  ouvrir(q){
    const s = this.s, r = q === "tresor" ? ramasserTrouvaille(this.lieu) : rencontrerVisiteur();
    if (!r) return;
    if (q === "visiteur") { s.dir = "haut"; reposHumain(s.heros, "haut"); }
    else if (!calme()) effetHD(s, "evolution", this.tresor.im.x, this.tresor.im.y, 22, 9600);
    this.maj();
    ouvrirChose(r.chose);
  }
  // the Sage's word of the day, at the village only (js/jeu/histoire_village.js)
  aDire(){ return this.lieu === "village" && !!P() && !!annonceSurprises(); }
  dire(histoire){
    const a = annonceSurprises(); if (!a) return null;
    a.dit();
    return histoire.parole("sage", a.texte);
  }
}
