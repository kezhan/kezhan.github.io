/* Académia : le village. Le héros marche case par case jusqu'à l'endroit touché (chemin trouvé en contournant
   les obstacles) ou avec les flèches ; son compagnon le suit ; les habitants parlent ; toucher une maison mène à sa
   porte ; dans les hautes herbes, des Ombres violettes (js/jeu/ombres_village.js) : en toucher une lance le combat. */
const PAS_MS = 170;

class Monde extends Phaser.Scene {
  constructor(){ super("monde"); }
  // the high-definition drawings this village needs: hero, villagers, companion, every Ombre
  preload(){
    const a = actif();
    chargerAtlas(this, [herosHD(P()), "sage", "hugo", "paco", a && cleDe(a), ...Object.values(CLES_OMBRES)]);
  }
  create(){
    this.carte = construireCarte();
    const {sol, objets} = this.carte;
    const couche = (data, profondeur) => {
      const m = this.make.tilemap({data, tileWidth: CASE, tileHeight: CASE});
      const l = m.createLayer(0, m.addTilesetImage("decor"), 0, 0); l.setDepth(profondeur); return l;
    };
    couche(sol, 0); couche(objets, 1);
    this.cameras.main.setBounds(0, 0, LARG * CASE, HAUT * CASE).setRoundPixels(true);
    // signs above the houses (icon, name, badges, Ombres beaten before the door opens), crisp at any zoom
    this.panneaux = this.carte.etiquettes.map(e => this.add.text(e.x * CASE, e.y * CASE, "", {fontFamily: "Fredoka, sans-serif", fontSize: "7px",
      color: "#FFFFFF", fontStyle: "bold", stroke: "#1D1640", strokeThickness: 2, backgroundColor: "rgba(29,22,64,.45)", padding: {x: 2, y: 1}})
      .setOrigin(.5).setResolution(12).setDepth(60));
    this.majPanneaux();
    // villagers
    this.pnj = this.carte.pnj.map(n => {
      const s = spriteHumain(this, n.cle, n.sprite, n.x, n.y).setDepth(10 + n.y);
      reposHumain(s, DIRS[n.dir]);
      return {...n, s};
    });
    const p = P(), pos = p.position || this.carte.depart;
    this.case = {x: pos.x, y: pos.y};
    this.heros = spriteHumain(this, herosHD(p), herosDe(p), pos.x, pos.y).setDepth(20 + pos.y);
    this.dir = "bas"; this.chemin = []; this.enMarche = false; this.verrou = false;
    const voisin = [[0, 1], [-1, 0], [1, 0], [0, -1]].map(([a, b]) => ({x: pos.x + a, y: pos.y + b})).find(v => this.libre(v.x, v.y));
    this.traces = [voisin || {...this.case}];
    this.compagnon = null; this.majCompagnon();
    this.cameras.main.startFollow(this.heros, true, .2, .2, 0, this.heros.dy - 8);   // centred on the hero's tile, as for the pixel hero
    const zoom = () => this.ajusterZoom();
    zoom(); this.scale.on("resize", zoom); this.events.once("shutdown", () => this.scale.off("resize", zoom));
    this.ombres = new OmbresVillage(this);
    window.__calme = performance.now() + 300;
    // touch: walk where the child taps (a villager: go and talk; a house: go to its door; an Ombre: go and fight)
    this.input.on("pointerup", ptr => {
      if (this.occupe()) return;
      const x = Math.floor(ptr.worldX / CASE), y = Math.floor(ptr.worldY / CASE);
      this.allerVers(x, y, this.pnj.find(q => q.x === x && (q.y === y || (q.s.hd && q.y === y + 1))));
    });
    // arrows only, without capturing them for the whole page (the first-name field keeps its space and arrows)
    this.touches = this.input.keyboard.addKeys({up: "UP", down: "DOWN", left: "LEFT", right: "RIGHT"}, false);
  }
  // the village answers unless another screen, a dialogue or a fight starting is in front of it
  occupe(){ return ecranCourant !== "monde" || !$("dialogue").hidden || this.verrou || performance.now() < (window.__calme || 0); }
  ajusterZoom(){
    const w = this.scale.width / RATIO, h = this.scale.height / RATIO;   // in CSS pixels
    const z = Math.min(6, Math.max(2, Math.round(Math.min(w / (20 * CASE), h / (14 * CASE)))));   // whole pixels only
    this.cameras.main.setZoom(z * RATIO);
  }
  texteEtiquette(regionId){
    const r = regionDe(regionId), st = regionEtat(regionId), n = Math.min(st.etape, ETAPES);
    return `${r.icone} ${r.nom.replace(/^(Le |La |Les |L')/, "")}` + (st.badges ? " " + "🏅".repeat(Math.min(3, st.badges)) : "")
      + "  " + (n >= ETAPES ? "🔓" : "●".repeat(n) + "○".repeat(ETAPES - n));
  }
  majPanneaux(){ this.carte.etiquettes.forEach((e, i) => this.panneaux[i].setText(this.texteEtiquette(e.region))); }
  majCompagnon(){
    const c = actif(); if (this.compagnon) this.compagnon.destroy();
    if (!c) { this.compagnon = null; return; }
    const t = this.traces[0] || this.case;
    this.compagnon = spriteCreature(this, cleDe(c), spriteDe(c), t.x, t.y).setDepth(19 + t.y);
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
    if (o && this.dist(o) <= 1) return this.rencontre(o);
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
  marquer(x, y){   // a little sparkle where the child tapped: every touch gets an answer
    const e = this.add.sprite(x * CASE + 8, y * CASE + 8, "effet3").setDepth(5).setScale(.5).setAlpha(.8);
    e.play("effet3"); e.once("animationcomplete", () => e.destroy());
  }
  pas(){
    const suiv = this.chemin.shift();
    if (!suiv) { this.enMarche = false; this.arreter(); this.arrive(); return; }
    const [x, y] = suiv, dx = x - this.case.x, dy = y - this.case.y;
    this.dir = dx > 0 ? "droite" : dx < 0 ? "gauche" : dy > 0 ? "bas" : "haut";
    this.enMarche = true;
    marcherHumain(this.heros, this.dir);
    this.traces.unshift({...this.case}); this.traces.length = 3;
    this.case = {x, y};
    this.tweens.add({targets: this.heros, x: x * CASE + 8, y: y * CASE + this.heros.dy, duration: TEST ? 1 : PAS_MS, onComplete: () => this.apresPas()});
    this.heros.setDepth(20 + y);
    if (this.compagnon) {   // the companion walks into the hero's previous tile
      const k = this.compagnon, t = this.traces[0], cdx = t.x * CASE + 8 - k.x, cdy = t.y * CASE + k.dy - k.y;
      const d = Math.abs(cdx) > Math.abs(cdy) ? (cdx > 0 ? "droite" : "gauche") : (cdy > 0 ? "bas" : "haut");
      if (cdx || cdy) marcherCreature(k, d, PAS_MS);
      this.tweens.add({targets: k, x: t.x * CASE + 8, y: t.y * CASE + k.dy, duration: TEST ? 1 : PAS_MS});
      this.compagnon.setDepth(19 + t.y);
    }
  }
  apresPas(){
    const {x, y} = this.case;
    if (ecranCourant !== "monde" || this.verrou) { this.chemin = []; this.pnjVise = null; return this.pas(); }   // a menu opened while walking: stop here
    if (this.carte.herbe[y][x]) {   // tall grass rustles
      const f = this.add.sprite(x * CASE + 8, y * CASE + 10, "effet18").setDepth(30).setScale(.35); f.play("effet18"); f.once("animationcomplete", () => f.destroy());
    }
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
  update(){
    if (this.enMarche || this.occupe() || !this.touches) return;
    const t = this.touches, d = t.left.isDown ? [-1, 0] : t.right.isDown ? [1, 0] : t.up.isDown ? [0, -1] : t.down.isDown ? [0, 1] : null;
    if (d && this.libre(this.case.x + d[0], this.case.y + d[1])) { this.chemin = [[this.case.x + d[0], this.case.y + d[1]]]; this.pas(); }
  }
  parler(n){
    const vers = {x: this.case.x - n.x, y: this.case.y - n.y};
    reposHumain(n.s, vers.y > 0 ? "bas" : vers.y < 0 ? "haut" : vers.x < 0 ? "gauche" : "droite");
    dialogue(n.nom, n.cle, n.dit);
  }
  devantPorte(regionId){ this.chemin = []; this.pnjVise = null; entrerMaison(regionId); }
  // an Ombre is touched: the village holds still, a flash, and the fight
  rencontre(o){
    this.verrou = true; this.chemin = []; this.pnjVise = null; this.ombreVisee = null;
    o.combat = true; this.ombreEnCombat = o;
    sfx.critique();
    this.cameras.main.flash(250, 255, 255, 255);
    this.time.delayedCall(TEST ? 0 : 350, () => commencerCombat(o.region, false));
  }
  // back from a fight (js/jeu/lanceur.js, retourMonde): the beaten Ombre vanishes, signs and companion are redrawn
  apresCombat({gagne}){
    this.verrou = false; this.chemin = []; this.pnjVise = null;
    window.__calme = performance.now() + 400;   // the second tap of a double tap on « Continuer » does not walk
    this.ombres.apres(this.ombreEnCombat, gagne); this.ombreEnCombat = null;
    this.majCompagnon(); this.majPanneaux();
  }
}
