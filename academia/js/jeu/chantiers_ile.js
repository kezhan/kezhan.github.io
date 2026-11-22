/* Académia : les chantiers sur l'Île des Bâtisseurs (js/jeu/monde.js, quand la partie est sur l'île ; règles et état :
   js/chantiers.js ; écran : js/chantier_vue.js).
   1. Chaque terrain montre où en est son bâtiment : le terrain vide (ses piquets), puis l'image de son étape
      (assets/<qualité>/chantiers, une image par étape : seules celles que l'île montre sont chargées, la suivante
      arrive quand l'enfant bâtit).
   2. Toucher un terrain, son bâtiment ou son panneau : le héros va devant le panneau, le panneau frémit. Un bâtiment
      à construire : l'écran du chantier s'ouvre (la cheffe y dit l'étape suivante et son prix). Un bâtiment fini : son
      petit plaisir sur l'île, et la cheffe dit une chose vraie à apprendre (js/chantiers.js, SAVOIRS_CHANTIER).
   3. Ce qu'un bâtiment fini a fabriqué flotte dans une bulle au-dessus de lui (js/jeu/recoltes_ile.js) : le toucher
      le récupère (js/chantiers.js, recolterChantier : la brioche nourrit le compagnon, les autres donnent des étincelles), et
      la cheffe dit de revenir en chercher. Les plaisirs, à chaque visite : la lampe du phare
      s'allume quand on le touche, et toute seule quand le soir tombe sur l'île ; les ailes du moulin tournent, et plus
      vite quand on le touche ; le kiosque joue son air, des notes s'envolent ; la serre et la bibliothèque brillent.
   4. Au retour de l'écran du chantier, l'étape bâtie apparaît sur l'île, et la cheffe lève les bras.
   Rien ne bouge quand l'enfant demande le calme. Dessins : outils/village/scripts/ile_chantiers.py (table :
   js/jeu/chantiers_ile_pieces.js). Chaque terrain vu est noté dans P().histoire (terrain_<id>). */
const uniteChantier = () => (QUALITE === "leger" ? 2 : 1) / 8;   // world units per pixel of the buildings' drawings
// what a finished building needs besides its own drawing
const PLUS_CHANTIER = {moulin: ["moulin_ailes", "farine"], phare: ["phare_lumiere", "coquillage"], kiosque: ["note"], boulangerie: ["brioche"],
  serre: ["tomate"], bibliotheque: ["livre"]};
const AIR_KIOSQUE = [523, 659, 784, 659, 880, 784, 1047, 784, 659, 784, 523];   // the bandstand's own little tune
// the drawings the island shows for this child: each building at its step, and what the finished ones need
function imagesChantiers(p = P()){
  if (!p || typeof chantiersDe !== "function") return [];
  return Object.keys(SUJETS_CHANTIER).flatMap(id => {
    const k = etapeChantier(id, p);
    return k ? [`${id}_${k}`, ...(k >= CHANTIER_FINI ? PLUS_CHANTIER[id] || [] : [])] : [];
  });
}
function chargerChantiersHD(load, noms = imagesChantiers()){
  noms.forEach(n => { if (!load.textureManager.exists("ch_" + n)) load.image("ch_" + n, `${DOSSIER_HD}/chantiers/${n}.png`); });
}

class ChantiersIle {
  constructor(scene){
    this.s = scene; this.vise = null; this.terrains = scene.carte.terrains || []; this.batiments = {};
    this.terrains.forEach(t => this.maj(t.id));
    this.recoltes = new RecoltesIle(this);   // what the finished buildings have made, in a bubble over them
    this.reserves = new ReservesIle(this);   // the places kept for the companions: touched and reached like a lot (js/jeu/lieux_ile.js)
    // the lighthouse lights up by itself when the evening falls on the island (js/jeu/histoire_village.js)
    scene.time.addEvent({delay: 1500, loop: true, callback: () => { if (scene.histoire && scene.histoire.voile && !this.allume) this.allumer(0); }});
  }
  terrain(id){ return this.terrains.find(t => t.id === id) || null; }
  pied(t){ return {x: (t.x + t.w / 2) * CASE, y: (t.y + t.h) * CASE}; }
  // drawings that are not there yet: loaded, then fn
  charger(noms, fn){
    const l = this.s.load, avant = l.list.size;
    chargerChantiersHD(l, noms);
    if (l.list.size === avant && !l.isLoading()) return fn();
    l.once("complete", () => { if (this.s.sys.isActive() || this.s.sys.isSleeping()) fn(); });
    if (!l.isLoading()) l.start();
  }
  poser(nom, x, y, prof){
    const s = this.s, cle = "ch_" + nom;
    if (!s.textures.exists(cle)) return null;
    s.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
    return s.add.image(x, y, cle).setDepth(prof).setScale(uniteChantier());
  }
  // the building of a lot as the child's game has it; `fete`: back from the building site, a new step pops in
  maj(id, fete){
    const s = this.s, t = this.terrain(id), k = t ? etapeChantier(id) : 0;
    if (!k) return;
    const b = this.batiments[id] = this.batiments[id] || {etape: 0}, nom = `${id}_${k}`;
    const noms = [nom, ...(k >= CHANTIER_FINI ? PLUS_CHANTIER[id] || [] : [])];
    if (noms.some(n => !s.textures.exists("ch_" + n))) return this.charger(noms, () => this.maj(id, fete));
    if (b.etape === k) return;
    const r = s.villageHD && s.villageHD.ileHD && s.villageHD.ileHD.terrains[id], p = this.pied(t), neuf = fete && b.etape < k;
    if (r && r.terrain) r.terrain.setVisible(false);   // the empty lot is under the building's own ground
    if (!b.im) {
      b.im = this.poser(nom, p.x, p.y, profondeur(t.y + t.h - 1, 0)).setOrigin(.5, 1);
      b.im.bornes = b.im.getBounds();
      if (s.villageHD) s.villageHD.ecrans.push(b.im);   // the hero behind it shows through
    } else { b.im.setTexture("ch_" + nom); b.im.caches = {}; if (s.villageHD) s.villageHD.devoile = null; }
    b.etape = k;
    if (k >= CHANTIER_FINI) this.vivre(id);
    if (neuf) this.feter(id);
  }
  // a new step on the island: sparkles, the building pops, the cheffe raises her arms
  feter(id){
    const s = this.s, b = this.batiments[id], t = this.terrain(id), n = s.pnj.find(q => q.cle === "cheffe");
    if (n && n.s) s.vie.saluer(n.s, 1600);
    if (typeof jouerSon === "function") jouerSon("scintille");
    if (calme() || !b || !b.im) return;
    effetHD(s, "apparition", b.im.x, b.im.y - 20, 40, b.im.depth + 1);
    s.tweens.add({targets: b.im, scaleY: {from: b.im.scaleY * .9, to: b.im.scaleY}, duration: 420, ease: "Back.easeOut"});
    for (let i = 0; i < 6; i++) effetHD(s, "toucher", (t.x + .5 + Math.random() * (t.w - 1)) * CASE, (t.y - 1 + Math.random() * t.h) * CASE, 10, profondeur(t.y + t.h, 9));
  }
  // what lives on a finished building: the mill's sails turn, the lighthouse has its light (off until asked)
  vivre(id){
    const b = this.batiments[id], t = this.terrain(id), p = this.pied(t), s = this.s, pt = POINTS_CHANTIERS[id] || {};
    if (id === "moulin" && !b.ailes) {
      b.ailes = this.poser("moulin_ailes", p.x + pt.moyeu[0], p.y + pt.moyeu[1], b.im.depth + .2);
      if (b.ailes && !calme()) b.tourne = s.tweens.add({targets: b.ailes, angle: 360, duration: 9000, repeat: -1});
    }
    if (id === "phare" && !b.lumiere) {
      b.lumiere = this.poser("phare_lumiere", p.x + pt.lampe[0], p.y + pt.lampe[1], 9000);
      if (b.lumiere) b.lumiere.setVisible(false).setBlendMode(Phaser.BlendModes.SCREEN);
    }
  }
  // the lighthouse's lamp: its beams sweep the sea; `ms`: how long (0: until the scene ends, the evening)
  allumer(ms){
    const s = this.s, b = this.batiments.phare, l = b && b.lumiere;
    if (!l) return;
    if (!ms) this.allume = true;
    l.setVisible(true).setAlpha(1);
    if (!b.balaie && !calme()) b.balaie = s.tweens.add({targets: l, scaleX: {from: l.scaleX, to: -l.scaleX}, duration: 2400, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    if (b.eteint) { b.eteint.remove(); b.eteint = null; }
    if (ms) b.eteint = s.time.delayedCall(TEST ? 400 : ms, () => { b.eteint = null; if (!this.allume) l.setVisible(false); });
  }

  // the lot under a touch: its tiles (and the stakes standing over its first row), its sign, or its building's drawing
  terrainSous(wx, wy){
    const x = wx / CASE, y = wy / CASE;
    return this.terrains.find(t => (x >= t.x && x < t.x + t.w && y >= t.y - .4 && y < t.y + t.h)
      || (Math.floor(x) === t.panneau.x && y >= t.panneau.y - .9 && y < t.panneau.y + 1)
      || (this.batiments[t.id] && typeof sousLeDoigt === "function" && sousLeDoigt(this.s, this.batiments[t.id].im, wx, wy))) || null;
  }
  touche(wx, wy){
    const bulle = this.recoltes.sous(wx, wy), t = bulle ? this.terrain(bulle) : this.terrainSous(wx, wy) || this.reserves.sous(wx, wy), s = this.s;
    this.vise = null;
    if (!t) return false;
    s.marquer(t.devant.x, t.devant.y);
    (t.reserve ? this.reserves : this).fremir(t);
    if (s.case.x === t.devant.x && s.case.y === t.devant.y) { this.ouvrir(t); return true; }
    s.allerVers(t.devant.x, t.devant.y);
    this.vise = t;
    return true;
  }
  // the hero has walked there (Monde.arrive)
  arrive(){
    const t = this.vise, s = this.s;
    this.vise = null;
    if (!t) return false;
    if (s.case.x === t.devant.x && s.case.y === t.devant.y) this.ouvrir(t);
    return true;
  }
  // the sign shivers, a few sparkles over the lot
  fremir(t){
    const s = this.s, r = s.villageHD && s.villageHD.ileHD && s.villageHD.ileHD.terrains[t.id], p = r && r.panneau;
    if (!p || calme() || p.tremble) return;
    p.tremble = s.tweens.add({targets: p, angle: {from: -4, to: 4}, duration: 90, yoyo: true, repeat: 2, ease: "Sine.easeInOut",
      onComplete: () => { p.setAngle(0); p.tremble = null; }});
    for (let i = 0; i < 4; i++) effetHD(s, "toucher", (t.x + .5 + Math.random() * (t.w - 1)) * CASE, (t.y + .5 + Math.random() * (t.h - 1)) * CASE, 10, profondeur(t.y + t.h, 9));
  }
  // in front of the sign: the building site opens, or the finished building gives its little pleasure
  ouvrir(t){
    const s = this.s;
    if (t.reserve) return this.reserves.ouvrir(t);
    if (!CHANTIERS[t.id]) return;
    s.chemin = []; s.pnjVise = null; s.arreter();
    reposHumain(s.heros, t.panneau.x > s.case.x ? "droite" : t.panneau.x < s.case.x ? "gauche" : t.panneau.y < s.case.y ? "haut" : "bas");
    if (typeof noterHistoire === "function") noterHistoire("terrain_" + t.id);
    if (chantierFini(t.id)) return this.plaisir(t);
    if (typeof jouerSon === "function") jouerSon("scintille");
    ouvrirChantier(t.id);
  }
  dire(texte){ return bulleVillage("cheffe", "La cheffe de chantier", texte); }
  // a finished building touched: what it does, and one true thing to learn
  plaisir(t){
    const s = this.s, id = t.id, b = this.batiments[id], p = this.pied(t), n = s.pnj.find(q => q.cle === "cheffe");
    if (n && n.s) s.vie.saluer(n.s, 1200);
    if (b && b.im && !calme()) for (let i = 0; i < 5; i++) effetHD(s, "toucher", p.x + (Math.random() - .5) * 50, p.y - 8 - Math.random() * 36, 10, b.im.depth + 1);
    const fait = recolterChantier(id), r = RECOLTES_CHANTIER[id];   // what it has made is taken, when it is ready
    if (fait) { this.recoltes.prendre(id); if (typeof majBarre === "function") majBarre(); }
    if (fait && fait.xp) return this.brioche(fait);
    if (id === "phare") this.allumer(8000);
    if (id === "moulin" && b && b.tourne) { b.tourne.timeScale = 4; s.time.delayedCall(4000, () => { if (b.tourne) b.tourne.timeScale = 1; }); }
    if (id === "kiosque") this.jouer(p);
    else if (typeof jouerSon === "function") jouerSon("scintille");
    const debut = {phare: "Clic ! La lampe du phare s'allume. ", kiosque: "Écoute la musique du kiosque ! ", moulin: "Regarde les ailes tourner ! "}[id] || "";
    if (fait) return this.dire(`${debut}${fait.dit} : ${etincellesDites(fait.etincelles)} pour toi ! ${savoirChantier(id)} Reviens plus tard : il y en aura encore !`);
    return this.dire(debut + savoirChantier(id) + (r ? ` Reviens plus tard chercher ${r.nom} !` : ""));
  }
  // the bandstand plays its tune, notes fly up
  jouer(p){
    const s = this.s;
    ton(AIR_KIOSQUE, .17, "triangle", .16);
    if (calme()) return;
    for (let i = 0; i < 6; i++) s.time.delayedCall(i * 300, () => {
      const im = this.poser("note", p.x + (Math.random() - .5) * 40, p.y - 26, 9000);
      if (!im) return;
      im.setScale(im.scaleX * .5).setAlpha(0);
      s.tweens.add({targets: im, y: im.y - 30 - Math.random() * 14, x: im.x + (Math.random() - .5) * 20, alpha: {from: 1, to: 0}, angle: (Math.random() - .5) * 40,
        duration: 1500, ease: "Sine.easeOut", onComplete: () => im.destroy()});
    });
  }
  // the bakery's brioche: it pops over the companion, who eats it (XP, its evolution when it wins it)
  async brioche(g){
    const s = this.s, c = s.compagnon, nom = nomCompagnon(g.compagnon);
    sfx.niveau();
    if (c && !calme()) {
      const im = this.poser("brioche", c.x, c.y - 22, 9000);
      if (im) { im.setScale(0); s.tweens.add({targets: im, scale: uniteChantier() * .6, y: c.y - 16, duration: 500, ease: "Back.easeOut", hold: 900, yoyo: true, onComplete: () => im.destroy()}); }
    }
    await this.dire(`La boulangerie a cuit une brioche pour ${nom} ! Miam : ${g.xp} points d'expérience ! Reviens plus tard : elle en cuira une autre !`);
    if (g.evolue && s.sys.isActive() && ecranCourant === "monde") { await sceneEvolution(g.compagnon, g.stadeAvant); montrer("monde"); }
    if (s.sys.isActive()) s.majCompagnon();
  }
}
