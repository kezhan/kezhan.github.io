/* Académia : les sorties du village vers ses quartiers (js/quartiers.js) et le plan du village.
   1. Cinq chemins vont jusqu'au bord de la carte (js/jeu/carte.js, SORTIES_VILLAGE), où la forêt s'ouvre. À côté de
      chacun, un panneau planté montre en dessin où il mène (la vache de la ferme, le parasol du marché, la cloche de
      l'école, la chouette de la forêt, le coquillage de la plage). Le héros qui s'arrête au bout d'un chemin passe dans
      le quartier (allerQuartier) ; il en reviendra sur ce chemin, à quelques pas du bord. Un panneau touché : le héros
      va de lui-même au bout de son chemin.
   2. Près de la place, le plan du village, un grand panneau de bois : touché, le héros va devant lui et le plan
      s'ouvre (js/plan_village.js). Tant que l'enfant ne l'a jamais ouvert, une petite bulle flotte au-dessus.
   3. Le plan ou un panneau a montré un quartier : le héros y marche tout seul, et le guide (js/jeu/guide_village.js)
      montre le chemin, la flèche au bord de l'écran puis la main sur le panneau.
   Dessins : outils/village/scripts/quartiers_plan.py, dans assets/<qualité>/quartiers. */
function chargerSortiesHD(load){
  const charger = n => { if (!load.textureManager.exists("qv_" + n)) load.image("qv_" + n, dossierQuartiers() + n + ".png"); };
  SORTIES_VILLAGE.forEach(s => charger("panneau_" + s.id));
  ["plan_village", "plan_bulle"].forEach(charger);
}

class SortiesVillage {
  constructor(scene){
    this.s = scene; this.vise = false;
    const image = (cle, x, y, w, h, prof) => {
      if (!scene.textures.exists(cle)) return null;
      scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
      const im = scene.add.image(x, y, cle).setOrigin(.5, 1).setDisplaySize(w, h).setDepth(prof);
      im.bornes = im.getBounds();
      if (scene.villageHD && scene.villageHD.ecrans) scene.villageHD.ecrans.push(im);
      return im;
    };
    // a sign by each path, in front of the forest's trees
    this.sorties = (scene.carte.sorties || []).map(q => ({...q, im: image("qv_panneau_" + q.id, (q.panneau.x + .5) * CASE, (q.panneau.y + 1) * CASE, 1.75 * CASE, 2.5 * CASE, profondeur(q.panneau.y, 9))}));
    // the map of the village, by the square
    const z = this.zone = scene.carte.elements.find(e => e.type === "plan_village") || null;
    this.devant = z ? {x: z.x + 1, y: z.y + 1} : null;
    this.plan = z ? image("qv_plan_village", (z.x + z.w / 2) * CASE, (z.y + 1) * CASE, 2.5 * CASE, 2.5 * CASE, profondeur(z.y, 0)) : null;
    this.bulle = this.plan ? image("qv_plan_bulle", this.plan.x, this.plan.y - 2.5 * CASE + 2, 11, 11, 9000) : null;
    if (this.bulle && !calme()) scene.tweens.add({targets: this.bulle, y: this.bulle.y - 3, duration: 760, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    this.maj();
  }
  maj(){ if (this.bulle) this.bulle.setVisible(!histoireDe().plan); }
  sortie(id){ return this.sorties.find(q => q.id === id) || null; }
  // the tile at the end of a path the nearest to the hero
  bout(q){ const s = this.s; return q.cases.map(([x, y]) => ({x, y})).reduce((a, b) => s.dist(b) < s.dist(a) ? b : a); }
  // a sign or the map is touched: the hero walks to the end of the path, or in front of the map
  touche(wx, wy){
    const s = this.s, sur = im => !!im && im.getBounds().contains(wx, wy);
    this.vise = false;
    const q = this.sorties.find(x => sur(x.im));
    if (q) { this.aller(q.id); return true; }
    if (!sur(this.plan) && !(this.bulle && this.bulle.visible && sur(this.bulle))) return false;
    const d = this.devant;
    s.marquer(d.x, d.y);
    if (!calme()) s.tweens.add({targets: this.plan, angle: {from: -2, to: 2}, duration: 90, yoyo: true, repeat: 1, onComplete: () => this.plan.setAngle(0)});
    if (s.case.x === d.x && s.case.y === d.y) { this.ouvrir(); return true; }
    s.allerVers(d.x, d.y);
    this.vise = true;
    return true;
  }
  // the hero walks by himself to a quarter, and the guide shows the way (the plan's choice, a touched sign)
  aller(id){
    const s = this.s, q = this.sortie(id); if (!q) return false;
    const b = this.bout(q);
    s.marquer(b.x, b.y);
    if (q.im && !calme()) s.tweens.add({targets: q.im, angle: {from: -3, to: 3}, duration: 90, yoyo: true, repeat: 1, onComplete: () => q.im.setAngle(0)});
    if (q.cases.some(([x, y]) => x === s.case.x && y === s.case.y)) { allerQuartier(q.id, q.retour); return true; }
    if (q.im && s.guide && s.guide.montrerChemin) s.guide.montrerChemin({x: q.im.x, y: q.im.y - 1.6 * CASE, haut: q.im.y - 2.5 * CASE});
    s.allerVers(b.x, b.y);
    return true;
  }
  // the hero has stopped (Monde.arrive): at the end of a path, he goes on to its quarter; in front of the map, it opens
  arrive(){
    const s = this.s, c = s.case, q = this.sorties.find(x => x.cases.some(([a, b]) => a === c.x && b === c.y));
    if (q) { this.vise = false; allerQuartier(q.id, q.retour); return true; }
    if (!this.vise) return false;
    this.vise = false;
    if (this.devant && c.x === this.devant.x && c.y === this.devant.y) this.ouvrir();
    return true;
  }
  ouvrir(){
    const s = this.s;
    s.dir = "haut"; reposHumain(s.heros, "haut");
    ouvrirPlan();
  }
}
