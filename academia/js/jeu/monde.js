/* Académia : le village. Le héros marche case par case jusqu'à l'endroit touché (chemin trouvé en contournant
   les obstacles) ou avec les flèches ; son compagnon le suit ; les habitants parlent (js/jeu/vie_village.js) ; toucher
   une maison mène à sa porte (js/jeu/portes_village.js) ; dans les hautes herbes, des Ombres violettes
   (js/jeu/ombres_village.js) : en toucher une, ou juste à côté, lance le combat. Une main et une flèche guident l'enfant
   qui ne sait plus quoi faire (js/jeu/guide_village.js). */
const PAS_MS = 170;

class Monde extends Phaser.Scene {
  constructor(){ super("monde"); }
  // the high-definition drawings this village needs: hero, villagers, companion, every Ombre, the effects
  preload(){
    reessayerImages(this);
    const a = actif();
    chargerAtlas(this, [herosHD(P()), "sage", "hugo", "paco", a && cleDe(a), ...Object.values(CLES_OMBRES)]);
    chargerEffetsHD(this.load, QUALITE);
    chargerGuideHD(this.load, QUALITE);
    chargerVillageHD(this.load, QUALITE);
  }
  create(){
    this.carte = construireCarte();
    dessinerVillageHD(this, this.carte);   // js/jeu/decor_village_hd.js
    this.cameras.main.setBounds(0, 0, LARG * CASE, HAUT * CASE).setRoundPixels(true);
    // signs above the houses (icon, name, badges, Ombres beaten before the door opens), crisp at any zoom, on a rounded pill
    this.panneaux = this.carte.etiquettes.map(e => panneauVillageHD(this, e.region, e.x * CASE, e.y * CASE));
    this.majPanneaux();
    this.vie = new VieVillage(this);   // villagers who breathe, wave and talk
    this.pnj = this.carte.pnj.map(n => ({...n, s: this.vie.habitant(n)}));
    const p = P(), pos = p.position || this.carte.depart;
    this.case = {x: pos.x, y: pos.y};
    this.heros = spriteHumain(this, herosHD(p), pos.x, pos.y).setDepth(profondeur(pos.y, 6));
    ombreAuSolHD(this, this.heros);
    this.dir = "bas"; this.chemin = []; this.enMarche = false; this.verrou = false; this.moment = false;
    const voisin = [[0, 1], [-1, 0], [1, 0], [0, -1]].map(([a, b]) => ({x: pos.x + a, y: pos.y + b})).find(v => this.libre(v.x, v.y));
    this.traces = [voisin || {...this.case}];
    this.compagnon = null; this.cleCompagnon = null; this.majCompagnon();
    this.suivreHeros();
    this.cameras.main.on("followupdate", () => cadrerPanneauxHD(this, this.panneaux));   // the signs once the camera has moved
    const zoom = () => this.ajusterZoom();
    zoom(); this.scale.on("resize", zoom); this.events.once("shutdown", () => this.scale.off("resize", zoom));
    this.ombres = new OmbresVillage(this);
    this.portes = new PortesVillage(this);   // closed doors in violet mist, open doors in golden light
    this.guide = new GuideVillage(this);     // the arrow before the first fight, the hand when nothing happens
    this.silhouettes = new SilhouettesHD(this);   // the hero and the companion seen through what hides them
    window.__calme = performance.now() + 300;
    // touch: walk where the child taps (a villager: go and talk; a house, its roof or its sign: go to its door; an Ombre
    // or right next to it: go and fight; the companion: it jumps for joy)
    this.input.on("pointerdown", () => this.guide.toucher());
    this.input.on("pointerup", ptr => this.toucher(ptr));
    // arrows only, without capturing them for the whole page (the first-name field keeps its space and arrows)
    this.touches = this.input.keyboard.addKeys({up: "UP", down: "DOWN", left: "LEFT", right: "RIGHT"}, false);
    if (typeof jouerAmbiance === "function") jouerAmbiance("village");
  }
  // the village answers unless another screen, a dialogue, a fight starting or a door opening is in front of it
  occupe(){ return ecranCourant !== "monde" || dialogueOuvert() || this.verrou || this.moment || performance.now() < (window.__calme || 0); }
  suivreHeros(){ this.cameras.main.startFollow(this.heros, true, .2, .2, 0, this.heros.dy - 8); }   // centred on the hero's tile
  ajusterZoom(){
    const w = this.scale.width / RATIO, h = this.scale.height / RATIO;   // in CSS pixels
    const z = Math.min(6, Math.max(2, Math.round(Math.min(w / (20 * CASE), h / (14 * CASE)))));   // whole pixels only
    this.cameras.main.setZoom(z * RATIO);
  }
  toucher(ptr){
    if (dialogueFermable()) fermerDialogue(true);   // the last line of a dialogue: the touch closes it and goes on
    if (this.occupe()) return;
    const wx = ptr.worldX, wy = ptr.worldY;
    if (this.vie.toucherCompagnon(wx, wy)) return;
    const o = this.ombres.touchee(wx, wy);
    if (o) return this.allerVers(o.x, o.y);
    let x = Math.floor(wx / CASE), y = Math.floor(wy / CASE);
    const r = maisonToucheeHD(this, wx, wy), m = this.carte.maison;
    if (r && !(m[y] && m[y][x] === r)) for (let b = 0; b < HAUT; b++) { const a = m[b].indexOf(r); if (a >= 0) { x = a; y = b; break; } }
    const n = this.pnj.find(q => q.x === x && (q.y === y || q.y === y + 1));
    if (n) this.vie.saluer(n.s);   // the villager waves at once, the hero walks to talk
    this.allerVers(x, y, n);
  }
  texteEtiquette(regionId){
    const r = regionDe(regionId), st = regionEtat(regionId), n = Math.min(st.etape, ETAPES);
    return `${r.icone} ${r.nom.replace(/^(Le |La |Les |L')/, "")}` + (st.badges ? " " + "🏅".repeat(Math.min(3, st.badges)) : "")
      + "  " + (n >= ETAPES ? "🔓" : "●".repeat(n) + "○".repeat(ETAPES - n));
  }
  majPanneaux(){ this.carte.etiquettes.forEach((e, i) => ecrirePanneauHD(this.panneaux[i], this.texteEtiquette(e.region))); }
  // the companion following the hero, drawn again only when it changed (chosen in the bag, evolved): its drawing is
  // loaded first if it is new, and the old one stops breathing before it goes
  majCompagnon(){
    const c = actif(), cle = c ? cleDe(c) : null;
    if (cle === this.cleCompagnon && (this.compagnon ? this.compagnon.active : !cle)) return;
    this.cleCompagnon = cle;
    chargerPuis(this, [cle], () => {
      if (cle !== this.cleCompagnon) return;   // changed again meanwhile
      if (this.compagnon) { arreterSouffle(this.compagnon); this.compagnon.destroy(); this.compagnon = null; }
      if (!cle) return;
      const t = this.traces[0] || this.case;
      this.compagnon = spriteCreature(this, cle, t.x, t.y).setDepth(profondeur(t.y, 4));
      ombreAuSolHD(this, this.compagnon);
    });
  }
  libre(x, y){ return x >= 0 && y >= 0 && x < LARG && y < HAUT && !this.carte.bloque[y][x]; }
  dist(c){ return Math.abs(c.x - this.case.x) + Math.abs(c.y - this.case.y); }
  // shortest path on the grid (breadth first), to the tile or, if it is blocked, to the nearest free tiles around it
  chercher(x, y){
    const cibles = this.libre(x, y) ? [[x, y]] : [];
    for (let r = 1; !cibles.length && r <= 3; r++)
      for (let b = y - r; b <= y + r; b++) for (let a = x - r; a <= x + r; a++)
        if (Math.max(Math.abs(a - x), Math.abs(b - y)) === r && this.libre(a, b)) cibles.push([a, b]);
    if (!cibles.length) return null;
    const cle = (a, b) => a + "," + b, vu = new Map([[cle(this.case.x, this.case.y), null]]), file = [[this.case.x, this.case.y]];
    while (file.length) {
      const [a, b] = file.shift();
      if (cibles.some(([u, v]) => u === a && v === b)) {
        const ch = []; let k = cle(a, b);
        while (vu.get(k)) { ch.unshift(k.split(",").map(Number)); k = vu.get(k); }
        return ch;
      }
      for (const [da, db] of [[0, 1], [1, 0], [0, -1], [-1, 0]]) {
        const na = a + da, nb = b + db, k2 = cle(na, nb);
        if (!vu.has(k2) && this.libre(na, nb)) { vu.set(k2, cle(a, b)); file.push([na, nb]); }
      }
    }
    return null;
  }
  allerVers(x, y, pnj){
    const maison = this.carte.maison[y] && this.carte.maison[y][x];
    if (maison) {   // a house: to its door; already in front of it, the door answers at once
      const ps = this.carte.portes.filter(q => q.region === maison);
      if (ps.some(q => q.x === this.case.x && q.y === this.case.y)) { this.marquer(x, y); return this.devantPorte(maison); }
      ({x, y} = ps.reduce((a, b) => this.dist(b) < this.dist(a) ? b : a));
    }
    const o = this.ombres.a(x, y);
    if (this.ombreVisee && this.ombreVisee !== o) this.ombreVisee.fige = false;
    this.marquer(x, y);
    if (o && aUnPas(o, this.case)) return this.rencontre(o);
    const ch = this.chercher(x, y);
    if (!ch) return;
    if (o) this.ombres.viser(o);
    this.chemin = ch; this.pnjVise = pnj || null; this.ombreVisee = o || null;
    if (!ch.length) {
      if (pnj) return this.parler(pnj);
      const porte = this.carte.portes.find(q => q.x === this.case.x && q.y === this.case.y);
      return porte ? this.devantPorte(porte.region) : undefined;
    }
    if (!this.enMarche) this.pas();
  }
  marquer(x, y){ effetHD(this, "toucher", x * CASE + 8, y * CASE + 8, CASE, profondeur(y, 9)); }   // every touch gets an answer
  pas(){
    const suiv = this.chemin.shift();
    if (!suiv) { this.enMarche = false; this.arreter(); this.arrive(); return; }
    const [x, y] = suiv, dx = x - this.case.x, dy = y - this.case.y;
    this.dir = dx > 0 ? "droite" : dx < 0 ? "gauche" : dy > 0 ? "bas" : "haut";
    this.enMarche = true;
    marcherHumain(this.heros, this.dir);
    if (typeof jouerSon === "function") jouerSon("pas");
    this.traces.unshift({...this.case}); this.traces.length = 3;
    this.case = {x, y};
    this.tweens.add({targets: this.heros, x: x * CASE + 8, y: y * CASE + this.heros.dy, duration: TEST ? 1 : PAS_MS, onComplete: () => this.apresPas()});
    this.heros.setDepth(profondeur(y, 6));
    if (this.compagnon) {   // the companion walks into the hero's previous tile
      this.vie.arreterSaut();
      const k = this.compagnon, t = this.traces[0], cdx = t.x * CASE + 8 - k.x, cdy = t.y * CASE + k.dy - k.y;
      const d = Math.abs(cdx) > Math.abs(cdy) ? (cdx > 0 ? "droite" : "gauche") : (cdy > 0 ? "bas" : "haut");
      if (cdx || cdy) marcherCreature(k, d, PAS_MS);
      this.tweens.add({targets: k, x: t.x * CASE + 8, y: t.y * CASE + k.dy, duration: TEST ? 1 : PAS_MS});
      this.compagnon.setDepth(profondeur(t.y, 4));
      secouerHerbeHD(this, t.x, t.y);
    }
  }
  apresPas(){
    const {x, y} = this.case;
    if (ecranCourant !== "monde" || this.verrou) { this.chemin = []; this.pnjVise = null; return this.pas(); }   // a menu opened while walking: stop here
    if (this.carte.herbe[y][x]) { effetHD(this, "herbe", this.heros.x, this.heros.y, CASE, this.heros.depth + 3); secouerHerbeHD(this, x, y); }   // tall grass rustles
    const o = this.ombres.pres(x, y);
    if (o) { this.enMarche = false; this.arreter(); return this.rencontre(o); }
    const porte = this.carte.portes.find(p => p.x === x && p.y === y);
    if (porte && !this.chemin.length) { this.enMarche = false; this.arreter(); return this.devantPorte(porte.region); }
    this.pas();
  }
  arreter(){ reposHumain(this.heros, this.dir); if (this.compagnon) reposCreature(this.compagnon); }
  arrive(){
    const p = P(); p.position = {...this.case}; sauver();
    if (this.pnjVise) { const n = this.pnjVise; this.pnjVise = null; this.parler(n); }
  }
  update(time){
    devoilerHD(this, [this.heros, this.compagnon], this.panneaux);   // a sign in front of the hero turns see-through
    this.silhouettes.suivre([this.heros, this.compagnon]);           // what hides them: their silhouette shows through
    cadrerPanneauxHD(this, this.panneaux);   // signs never under the top bar nor cut by an edge
    this.portes.update(time); this.guide.update(time);
    if (this.enMarche || this.occupe() || !this.touches) return;
    const t = this.touches, d = t.left.isDown ? [-1, 0] : t.right.isDown ? [1, 0] : t.up.isDown ? [0, -1] : t.down.isDown ? [0, 1] : null;
    if (d && this.libre(this.case.x + d[0], this.case.y + d[1])) { this.guide.toucher(); this.chemin = [[this.case.x + d[0], this.case.y + d[1]]]; this.pas(); }
  }
  parler(n){
    this.vie.parler(n.s);
    dialogue(n.nom, n.cle, n.dit, null, {ligne: (i, voix) => this.vie.mains(n.s, voix), fin: () => this.vie.repos(n.s)});
  }
  devantPorte(regionId){ this.chemin = []; this.pnjVise = null; entrerMaison(regionId); }
  // a point of the world on the screen, in CSS pixels (the camera's view as it is drawn at this frame)
  ecranDe(wx, wy){
    const c = this.cameras.main, k = c.zoom / RATIO;
    return [(wx - c.scrollX - c.width * c.originX * (1 - 1 / c.zoom)) * k, (wy - c.scrollY - c.height * c.originY * (1 - 1 / c.zoom)) * k];
  }
  // an Ombre is touched: the village holds still, the Ombre jumps, and the screen closes in a star on it
  // (js/transition.js, when the fights have it) before the fight
  rencontre(o){
    this.verrou = true; this.chemin = []; this.pnjVise = null; this.ombreVisee = null;
    o.combat = true; this.ombreEnCombat = o; this.etapeAvant = regionEtat(o.region).etape;
    sfx.critique();
    if (!calme()) this.tweens.add({targets: o.s, y: o.s.y - 6, duration: 140, yoyo: true, ease: "Quad.easeOut"});
    const lancer = () => commencerCombat(o.region, false);
    if (typeof fermerEtoile === "function") {
      const [x, y] = this.ecranDe(o.s.x, o.s.y - o.s.displayHeight / 2);
      this.time.delayedCall(TEST ? 0 : 200, () => fermerEtoile(x, y).then(lancer));
    } else {
      this.cameras.main.flash(250, 255, 255, 255);
      this.time.delayedCall(TEST ? 0 : 350, lancer);
    }
  }
  // back from a fight (js/jeu/lanceur.js, retourMonde): the beaten Ombre vanishes, signs, doors and companion are
  // redrawn; the fourth Ombre near a house opens its door before the child's eyes
  apresCombat({gagne}){
    this.verrou = false; this.chemin = []; this.pnjVise = null;
    window.__calme = performance.now() + 400;   // the second tap of a double tap on « Continuer » does not walk
    const o = this.ombreEnCombat, region = o && o.region;
    const ouvre = !!region && this.etapeAvant < ETAPES && regionEtat(region).etape >= ETAPES;
    this.ombres.apres(o, gagne); this.ombreEnCombat = null;
    this.majCompagnon(); this.majPanneaux(); this.portes.maj(ouvre ? region : null);
    cadrerPanneauxHD(this, this.panneaux, true);   // the bar is back, its pill a little wider: the signs at once
    if (ouvre) this.portes.ouvrir(region);
    this.guide.retour();
    if (typeof jouerAmbiance === "function") jouerAmbiance("village");
  }
}
