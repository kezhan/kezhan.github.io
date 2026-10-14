/* Académia : les Ombres qu'on voit dans les hautes herbes. Une par touffe, aux couleurs de sa maison, un peu plus grande
   que les compagnons pour qu'un doigt la trouve ; elle se promène dans son herbe sans jamais venir coller le héros.
   La toucher (ou toucher juste à côté, diagonales comprises) lance le combat ; passer à côté sans l'avoir touchée la
   fait seulement lever les yeux (jamais de combat par hasard, Kezhan, 13/10/2026) ; battue, elle
   s'efface dans un nuage et une autre revient un peu plus tard. L'enfant voit ce qu'il chasse. */
const OMBRE_REVIENT_MS = 5000, OMBRE_PAS_MS = 1500, HAUTEUR_OMBRE = 19;
const aUnPas = (a, b) => Math.max(Math.abs(a.x - b.x), Math.abs(a.y - b.y)) <= 1;   // the same tile or one of the eight around

/* Les Ombres des maisons (js/jeu/ombres_regions.js) : chacune dans la teinte et la parure de sa maison, sa lueur
   violette autour, comme au combat. Une sur vingt environ est brillante : dorée, plus grande, une lueur d'or et des
   étincelles ; la première fois qu'elle passe à l'écran, sa petite musique et la voix l'annoncent. Le combat a lieu
   contre l'Ombre touchée, la même (js/jeu/lanceur.js). */
const HAUTEUR_BRILLANTE = 23, OMBRES_TEINTES_APRES = 450, OMBRE_VUE_MAX = 1500;
const voixOmbres = {parle: false};   // the voice is speaking (the « voix » event of js/voix.js)
document.addEventListener("voix", e => { voixOmbres.parle = e.detail === "debut"; });

class OmbresVillage {
  constructor(scene){
    this.s = scene; this.liste = [];
    this.cases = {};   // region -> the tiles of its tall grass
    const h = scene.carte.herbe;
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) if (h[y][x]) (this.cases[h[y][x]] = this.cases[h[y][x]] || []).push({x, y});
    // the first ones are dressed just after the Sage's word (0.4 s after the village opens, js/accueil.js), which
    // never waits for them; the guide's arrow has one to show 0.7 s after
    this.debut = TEST ? 0 : OMBRES_TEINTES_APRES;
    Object.keys(this.cases).forEach(r => this.poser(r, false));
    this.debut = 0;
    scene.time.addEvent({delay: OMBRE_PAS_MS, loop: true, callback: () => this.promener()});
  }
  a(x, y){ return this.liste.find(o => o.x === x && o.y === y) || null; }
  // an Ombre on the hero's tile or right next to it, diagonals too
  pres(x, y){ return this.liste.find(o => !o.combat && aUnPas(o, {x, y})) || null; }
  // the Ombre a finger meant: on it or within a tile around it, the nearest to the finger (a world point)
  touchee(wx, wy){
    const t = {x: Math.floor(wx / CASE), y: Math.floor(wy / CASE)};
    let vise = null, dmin = Infinity;
    this.liste.forEach(o => {
      if (o.combat || !aUnPas(o, t)) return;
      const d = Math.hypot(o.s.x - wx, o.s.y - o.s.displayHeight / 2 - wy);
      if (d < dmin) { dmin = d; vise = o; }
    });
    return vise;
  }
  // the nearest Ombre to a tile (walking distance on the grid), among those `filtre` keeps, for the guide's arrow and hand
  proche(x, y, filtre = () => true){
    let vise = null, dmin = Infinity;
    this.liste.forEach(o => {
      const d = Math.abs(o.x - x) + Math.abs(o.y - y);
      if (!o.combat && o.s.active && o.s.alpha > .5 && d < dmin && filtre(o)) { dmin = d; vise = o; }
    });
    return vise;
  }
  occupee(x, y){
    const s = this.s, t = s.traces[0];
    return !!this.a(x, y) || (s.case.x === x && s.case.y === y) || (!!t && t.x === x && t.y === y);
  }
  loinDuHeros(c){ return !aUnPas(c, this.s.case); }
  // an Ombre in this grass; one coming back (`bouffee`) appears in a puff of smoke
  poser(region, bouffee = true){
    const libres = this.cases[region].filter(c => !this.occupee(c.x, c.y) && this.loinDuHeros(c));
    if (!libres.length) { this.s.time.delayedCall(2000, () => this.poser(region, bouffee)); return null; }   // the hero stands in the grass: later
    const c = libres[rnd(libres.length)], qui = ombre(regionDe(region), false), brillante = tirerBrillante();
    const o = {region, x: c.x, y: c.y, cle: qui.cle, brillante};   // what the fight will meet (js/jeu/lanceur.js)
    o.s = spriteCreature(this.s, qui.cle, c.x, c.y, brillante ? HAUTEUR_BRILLANTE : HAUTEUR_OMBRE).setDepth(profondeur(c.y));
    ombreAuSolHD(this.s, o.s);
    if (brillante) { halo(this.s, o.s, OR_BRILLANT, 1.2, .75); scintillerOmbre(this.s, o.s, {cadence: 420, taille: .3}); }
    else halo(this.s, o.s, 0x7B4DFF, 1.14, .6);   // its violet glow (js/jeu/sprites_hd.js)
    o.s.setAlpha(0);
    this.teindre(o);
    if (bouffee) this.fumee(o);
    this.liste.push(o);
    return o;
  }
  // its house's colours, gold when shining (js/jeu/ombres_regions.js), worked out a moment later, one Ombre every 60 ms
  // and its small face only: the village shows at once; the Ombre fades in once dressed (its drawing late on a slow
  // network: on arrival). Never hidden long: on a slow device (the game's clock crawls when the frames are slow), it
  // shows in its own colours 1.5 s after its drawing has come, and is dressed when its colours are ready
  teindre(o){
    this.attente = (this.attente || 0) + 1;
    const montrer = () => {
      if (o.montree || !o.s.active) return;
      o.montree = true; this.s.tweens.add({targets: o.s, alpha: 1, duration: TEST ? 1 : 400});
    };
    if (!TEST) quandAtlas(this.s, o.cle, () => setTimeout(montrer, OMBRE_VUE_MAX));
    this.s.time.delayedCall(TEST ? 0 : (this.debut || 0) + 60 * this.attente, () => {
      this.attente--;
      quandAtlas(this.s, o.cle, () => {
        if (!o.s.active) return;
        const d = ombreTeinte(o.cle, o.region, {brillante: o.brillante, petit: true});
        if (d !== o.s.hd) { o.s.hd = d; o.s.setTexture("hd_" + d, "face_petit"); }
        montrer();
      });
    });
  }
  // a shining Ombre seen for the first time on the screen, golden already: its little tune, a burst of stars, the
  // voice says it (once the voice is quiet: the guide's sentence is never cut)
  annoncer(){
    if (voixOmbres.parle) return;
    const v = this.s.cameras.main.worldView;
    const o = this.liste.find(x => x.brillante && !x.annoncee && !x.combat && x.s.active && x.s.alpha > .5 && x.s.hd !== x.cle
      && v.contains(x.s.x, x.s.y - 8));
    if (!o) return;
    o.annoncee = true;
    if (typeof jouerSon === "function") jouerSon("brillante");
    effetHD(this.s, "apparition", o.s.x, o.s.y - o.s.displayHeight / 2, o.s.displayHeight, o.s.depth + 4);
    dire("Oh ! Une Ombre brillante scintille dans les hautes herbes !");
  }
  promener(){
    if (this.s.occupe()) return;
    this.annoncer();
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
  // the hero walks past without having touched it: it looks up with a little hop, once, and no fight starts
  coucou(o){
    if (TEST || o.bouge || o.combat || o.coucou) return;
    o.coucou = true;
    this.s.tweens.add({targets: o.s, y: o.y * CASE + o.s.dy - 3, duration: 110, yoyo: true, onComplete: () => { o.coucou = false; }});
  }
  // after its fight: beaten, it vanishes in a puff and another one comes back later; otherwise it wanders again
  apres(o, gagne){
    if (!o) return;
    o.combat = false; o.fige = false;
    if (!gagne) return;
    this.liste = this.liste.filter(x => x !== o);
    this.fumee(o);
    arreterSouffle(o.s);   // it shrinks from its own size (a drawing is scaled down)
    this.s.tweens.add({targets: o.s, alpha: 0, scale: o.s.base * .4, duration: TEST ? 1 : 350, onComplete: () => o.s.destroy()});
    this.s.time.delayedCall(TEST ? 10 : OMBRE_REVIENT_MS, () => this.poser(o.region));
  }
  fumee(o){ effetHD(this.s, "fumee", o.s.x, o.s.y - o.s.displayHeight / 2, o.s.displayHeight, o.s.depth + 4); }
}
