/* Académia : le village. Le héros marche case par case jusqu'à l'endroit touché (chemin trouvé en contournant
   les obstacles) ou avec les flèches ; son compagnon le suit ; les habitants parlent ; dans les hautes herbes,
   une Ombre peut surgir ; devant une porte, l'entrée de la maison. */
const PAS_MS = 170;

class Monde extends Phaser.Scene {
  constructor(){ super("monde"); }
  create(){
    this.carte = construireCarte();
    const {sol, objets} = this.carte;
    const couche = (data, profondeur) => {
      const m = this.make.tilemap({data, tileWidth: CASE, tileHeight: CASE});
      const l = m.createLayer(0, m.addTilesetImage("decor"), 0, 0); l.setDepth(profondeur); return l;
    };
    couche(sol, 0); couche(objets, 1);
    this.cameras.main.setBounds(0, 0, LARG * CASE, HAUT * CASE).setRoundPixels(true);
    // signs above the houses, crisp at any zoom (text drawn at a high resolution)
    this.carte.etiquettes.forEach(e => {
      const r = regionDe(e.region), st = regionEtat(e.region);
      const txt = `${r.icone} ${r.nom.replace(/^(Le |La |Les |L')/, "")}` + (st.badges ? " " + "🏅".repeat(Math.min(3, st.badges)) : "");
      this.add.text(e.x * CASE, e.y * CASE, txt, {fontFamily: "Fredoka, sans-serif", fontSize: "7px", color: "#FFFFFF", fontStyle: "bold",
        stroke: "#1D1640", strokeThickness: 2, backgroundColor: "rgba(29,22,64,.45)", padding: {x: 2, y: 1}}).setOrigin(.5).setResolution(8).setDepth(60);
    });
    // villagers
    this.pnj = this.carte.pnj.map(n => {
      const s = this.add.sprite(n.x * CASE + 8, n.y * CASE + 8, "perso" + n.sprite, n.dir).setDepth(10 + n.y);
      return {...n, s};
    });
    const p = P(), pos = p.position || this.carte.depart;
    this.case = {x: pos.x, y: pos.y};
    this.heros = this.add.sprite(pos.x * CASE + 8, pos.y * CASE + 8, "perso" + herosDe(p), 0).setDepth(20 + pos.y);
    this.dir = "bas"; this.chemin = []; this.enMarche = false; this.pasSansOmbre = 0;
    const voisin = [[0, 1], [-1, 0], [1, 0], [0, -1]].map(([a, b]) => ({x: pos.x + a, y: pos.y + b})).find(v => this.libre(v.x, v.y));
    this.traces = [voisin || {...this.case}];
    this.compagnon = null; this.majCompagnon();
    this.cameras.main.startFollow(this.heros, true, .2, .2);
    const zoom = () => this.ajusterZoom();
    zoom(); this.scale.on("resize", zoom); this.events.once("shutdown", () => this.scale.off("resize", zoom));
    // touch: walk where the child taps (a villager: go next to them and talk)
    this.input.on("pointerup", ptr => {
      if (window.__ui) return;                  // a dialogue or a menu is open
      const x = Math.floor(ptr.worldX / CASE), y = Math.floor(ptr.worldY / CASE);
      const n = this.pnj.find(q => q.x === x && q.y === y);
      this.allerVers(x, y, n);
    });
    this.touches = this.input.keyboard.createCursorKeys();
  }
  ajusterZoom(){
    const w = this.scale.width, h = this.scale.height;
    const z = Math.min(6, Math.max(2, Math.round(Math.min(w / (20 * CASE), h / (14 * CASE)))));   // whole pixels only
    this.cameras.main.setZoom(z);
  }
  majCompagnon(){
    const c = actif(); if (this.compagnon) this.compagnon.destroy();
    if (!c) { this.compagnon = null; return; }
    const t = this.traces[0] || this.case;
    this.compagnon = this.add.sprite(t.x * CASE + 8, t.y * CASE + 8, "monstre" + spriteDe(c), 0).setDepth(19 + t.y);
  }
  libre(x, y){ return x >= 0 && y >= 0 && x < LARG && y < HAUT && !this.carte.bloque[y][x]; }
  // shortest path on the grid (breadth first), to the tile or, if it is blocked, to its nearest free neighbour
  chercher(x, y){
    const cibles = this.libre(x, y) ? [[x, y]] : [[x, y + 1], [x - 1, y], [x + 1, y], [x, y - 1]].filter(([a, b]) => this.libre(a, b));
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
    const ch = this.chercher(x, y);
    if (!ch) return;
    this.chemin = ch; this.pnjVise = pnj || null;
    if (!ch.length && pnj) return this.parler(pnj);
    this.marquer(x, y);
    if (!this.enMarche) this.pas();
  }
  marquer(x, y){   // a little sparkle where the child tapped
    const e = this.add.sprite(x * CASE + 8, y * CASE + 8, "effet3").setDepth(5).setScale(.5).setAlpha(.8);
    e.play("effet3"); e.once("animationcomplete", () => e.destroy());
  }
  pas(){
    const suiv = this.chemin.shift();
    if (!suiv) { this.enMarche = false; this.heros.stop(); this.heros.setFrame(DIRS.indexOf(this.dir)); if (this.compagnon) this.compagnon.stop();
      this.arrive(); return; }
    const [x, y] = suiv, dx = x - this.case.x, dy = y - this.case.y;
    this.dir = dx > 0 ? "droite" : dx < 0 ? "gauche" : dy > 0 ? "bas" : "haut";
    this.enMarche = true;
    this.heros.play(`perso${herosDe(P())}-${this.dir}`, true);
    this.traces.unshift({...this.case}); this.traces.length = 3;
    this.case = {x, y};
    this.tweens.add({targets: this.heros, x: x * CASE + 8, y: y * CASE + 8, duration: TEST ? 1 : PAS_MS, onComplete: () => this.apresPas()});
    this.heros.setDepth(20 + y);
    if (this.compagnon) {   // the companion walks into the hero's previous tile
      const t = this.traces[0], cdx = t.x * CASE + 8 - this.compagnon.x, cdy = t.y * CASE + 8 - this.compagnon.y;
      const d = Math.abs(cdx) > Math.abs(cdy) ? (cdx > 0 ? "droite" : "gauche") : (cdy > 0 ? "bas" : "haut");
      if (cdx || cdy) this.compagnon.play(`monstre${spriteDe(actif())}-${d}`, true);
      this.tweens.add({targets: this.compagnon, x: t.x * CASE + 8, y: t.y * CASE + 8, duration: TEST ? 1 : PAS_MS});
      this.compagnon.setDepth(19 + t.y);
    }
  }
  apresPas(){
    const {x, y} = this.case, region = this.carte.herbe[y][x];
    if (region) {   // tall grass: rustles, and sometimes an Ombre jumps out
      const f = this.add.sprite(x * CASE + 8, y * CASE + 10, "effet18").setDepth(30).setScale(.35); f.play("effet18"); f.once("animationcomplete", () => f.destroy());
      this.pasSansOmbre++;
      const chance = TEST ? .5 : .16;
      if (this.pasSansOmbre >= 3 && Math.random() < chance) { this.pasSansOmbre = 0; this.chemin = []; this.enMarche = false; this.heros.stop(); return this.rencontre(region); }
    }
    const porte = this.carte.portes.find(p => p.x === x && p.y === y);
    if (porte && !this.chemin.length) { this.enMarche = false; this.heros.stop(); return this.devantPorte(porte.region); }
    this.pas();
  }
  arrive(){
    const p = P(); p.position = {...this.case}; sauver();
    if (this.pnjVise) { const n = this.pnjVise; this.pnjVise = null; this.parler(n); }
  }
  update(){
    if (this.enMarche || window.__ui || !this.touches) return;
    const t = this.touches, d = t.left.isDown ? [-1, 0] : t.right.isDown ? [1, 0] : t.up.isDown ? [0, -1] : t.down.isDown ? [0, 1] : null;
    if (d && this.libre(this.case.x + d[0], this.case.y + d[1])) { this.chemin = [[this.case.x + d[0], this.case.y + d[1]]]; this.pas(); }
  }
  parler(n){
    const vers = {x: this.case.x - n.x, y: this.case.y - n.y};
    n.s.setFrame(vers.y > 0 ? 0 : vers.y < 0 ? 1 : vers.x < 0 ? 2 : 3);
    dialogue(n.nom, "portrait" + n.sprite, n.dit);
  }
  devantPorte(regionId){ entrerMaison(regionId); }
  rencontre(regionId){
    sfx.critique();
    this.cameras.main.flash(250, 255, 255, 255);
    this.time.delayedCall(TEST ? 0 : 350, () => commencerCombat(regionId, false));
  }
}
