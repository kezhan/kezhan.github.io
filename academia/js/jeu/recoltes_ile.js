/* Académia : ce que les bâtiments finis de l'Île des Bâtisseurs donnent à récupérer, montré sans rien à lire (règles :
   js/chantiers.js, RECOLTES_CHANTIER ; dessins : outils/village/scripts/chantier_recoltes.py).
   1. Sur l'île (RecoltesIle, tenu par js/jeu/chantiers_ile.js) : la chose prête flotte dans une bulle au-dessus de son
      bâtiment ; toucher la bulle, c'est toucher le bâtiment. Récupérée, elle s'envole et la bulle s'en va ; elle revient
      toute seule quand le temps est passé (regardé toutes les quelques secondes).
   2. Au village (annoncerRecoltes, appelé par js/jeu/monde.js) : quand des choses attendent sur l'île et que la barque
      est ouverte, la même bulle flotte au-dessus de la barque, et le passeur le dit une fois par séance, jamais
      par-dessus une autre bulle, un dialogue ou un moment du village. */
const RECOLTES_SEANCE = {annonce: false};

// the bubble with the thing in it, in world units, bobbing (still when the child asks for calm); null without its drawing
function bulleRecolte(scene, chose, x, y, prof){
  const cle = "ch_" + chose;
  if (!scene.textures.exists(cle)) return null;
  scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
  const fond = scene.add.graphics();
  fond.fillStyle(0x1D1640, 1).fillCircle(0, 0, 9.4).fillTriangle(-3.4, 7.6, 3.4, 7.6, 0, 13.4);
  fond.fillStyle(0xFFF8EC, 1).fillCircle(0, 0, 8.2).fillTriangle(-2.4, 7.2, 2.4, 7.2, 0, 11.4);
  const im = scene.add.image(0, 0, cle);
  im.setScale(11.5 / Math.max(im.width, im.height));
  const c = scene.add.container(x, y, [fond, im]).setDepth(prof);
  c.chose = chose;
  if (!calme()) c.flotte = scene.tweens.add({targets: c, y: y - 3, duration: 760, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
  return c;
}

class RecoltesIle {
  constructor(chantiers){
    this.ch = chantiers; this.s = chantiers.s; this.bulles = {};
    this.s.time.addEvent({delay: TEST ? 500 : 4000, loop: true, callback: () => this.maj()});
    this.s.time.delayedCall(0, () => this.maj());
  }
  // each finished building: its bubble when its thing is ready, none otherwise
  maj(){
    if (!this.s.sys.isActive()) return;
    Object.keys(RECOLTES_CHANTIER).forEach(id => {
      const b = this.ch.batiments[id], pret = recoltePrete(id), la = this.bulles[id];
      if (!pret || !b || !b.im || b.etape < CHANTIER_FINI) { if (la) { la.destroy(); delete this.bulles[id]; } return; }
      if (la) return;
      const c = bulleRecolte(this.s, RECOLTES_CHANTIER[id].chose, b.im.x, b.im.y - b.im.displayHeight * (POINTS_BULLE[id] || .72), 9000);
      if (c) { this.bulles[id] = c; c.setSize(22, 26); }
    });
  }
  // the building whose bubble is under the finger
  sous(wx, wy){ return Object.keys(this.bulles).find(id => Math.hypot(this.bulles[id].x - wx, this.bulles[id].y - wy) <= 13) || null; }
  // taken: the thing flies up out of its bubble, sparkles
  prendre(id){
    const c = this.bulles[id], s = this.s;
    delete this.bulles[id];
    if (!c) return;
    if (typeof jouerSon === "function") jouerSon("scintille");
    if (calme()) return c.destroy();
    if (c.flotte) c.flotte.remove();
    effetHD(s, "toucher", c.x, c.y, 14, c.depth + 1);
    s.tweens.add({targets: c, y: c.y - 22, scale: 1.5, alpha: 0, duration: 620, ease: "Cubic.easeOut", onComplete: () => c.destroy()});
  }
}
// how high over its foot each building's bubble floats, as a part of its drawing's height (the tall ones lower)
const POINTS_BULLE = {phare: .5, moulin: .62};

// in the village: the bubble over the boat, and the ferryman's word once in the sitting
function annoncerRecoltes(scene){
  const p = P();
  if (!p || scene.ile || typeof recoltesPretes !== "function" || !barqueOuverte(p)) return;
  // what waits on the island: the buildings' things, then the companions' places' (js/lieux.js)
  const attendent = () => [...recoltesPretes(p).map(id => RECOLTES_CHANTIER[id]), ...(typeof recoltesLieux === "function" ? recoltesLieux(p) : [])];
  const pretes = attendent();
  if (!pretes.length) return;
  const r = pretes[0], pa = scene.passage, centre = pa && pa.centre && pa.centre();
  if (centre) {
    (r.lieu ? chargerLieuxHD : chargerChantiersHD)(scene.load, [r.chose]);
    const poser = () => { if (scene.sys.isActive() && !scene.signeRecolte) scene.signeRecolte = bulleRecolte(scene, r.chose, centre.x, centre.y - 22, 9000); };
    if (scene.textures.exists("ch_" + r.chose)) poser();
    else { scene.load.once("complete", poser); if (!scene.load.isLoading()) scene.load.start(); }
  }
  if (RECOLTES_SEANCE.annonce) return;
  // the village's own first moments come first (the tree, the Sage's missions): he waits for a quiet moment, a minute at most
  const dire = essais => scene.time.delayedCall(TEST ? 900 : 5000, () => {
    if (!scene.sys.isActive() || RECOLTES_SEANCE.annonce) return;
    if (scene.occupe() || scene.enMarche || bulleVillageOuverte()) { if (essais) dire(essais - 1); return; }
    if (!attendent().length) return;
    RECOLTES_SEANCE.annonce = true;
    bulleVillage("passeur", "Le passeur", pretes.length > 1 ? "Des choses t'attendent sur l'Île des Bâtisseurs ! Monte dans ma barque pour aller les chercher."
      : `${r.dit} ! Monte dans ma barque pour aller chercher ${r.nom}.`);
  });
  dire(12);
}
