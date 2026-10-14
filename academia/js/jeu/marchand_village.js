/* Académia : le marchand de la place et son étal (js/jeu/carte.js le met parmi les habitants, l'étal parmi les petits
   lieux). L'étal a deux dessins (outils/personnages/scripts/etal_marchand.py) : le fond (poteaux et auvent), derrière
   le marchand, et le devant (comptoir et marchandise), devant lui. Un toucher sur le marchand ou son étal : il salue,
   le héros va devant le comptoir, puis la boutique s'ouvre (js/marchand.js) ; le marchand parle avec les mains
   pendant que la voix l'accueille. */
const srcEtal = nom => `${DOSSIER_HD}/marchand/${nom}.png`;
const HAUT_AUVENT = 1.6;   // the awning rises 1.6 tiles above the stall's first row (outils/personnages/scripts/etal_marchand.py)
// the merchant's sheet and his stall, loaded with the village (js/jeu/chargement.js); at once if already there
function chargerMarchandHD(scene){
  chargerAtlas(scene, ["marchand"]);
  ["etal_fond", "etal_devant"].forEach(n => { if (!scene.textures.exists("marchand_" + n)) scene.load.image("marchand_" + n, srcEtal(n)); });
}
// the stall drawn on the map (village and the home screens' backdrop): behind and in front of the merchant's row;
// a drawing that can hide the hero goes with the others the village makes see-through
function dessinerEtal(scene, carte){
  const e = carte.elements.find(x => x.type === "etal");
  if (!e) return [];
  const x = (e.x + e.w / 2) * CASE, y = (e.y + e.h) * CASE, poses = [];
  const poser = () => {
    [["etal_fond", profondeur(e.y, 2)], ["etal_devant", profondeur(e.y + e.h - 1, 0)]].forEach(([n, d]) => {
      if (!scene.textures.exists("marchand_" + n)) return;
      const im = scene.add.image(x, y, "marchand_" + n).setOrigin(.5, 1).setDepth(d);
      im.setScale(e.w * CASE / (im.width * (384 / 432)));   // its three tiles are 384 of its 432 HD pixels
      poses.push(im);
      if (scene.villageHD && scene.villageHD.ecrans) { im.bornes = im.getBounds(); scene.villageHD.ecrans.push(im); }
    });
  };
  if (scene.textures.exists("marchand_etal_fond") && scene.textures.exists("marchand_etal_devant")) poser();
  else {   // not there yet (a slow network): asked now, drawn when it comes
    chargerMarchandHD(scene);
    scene.load.once("complete", () => { if (scene.sys.isActive() || scene.sys.isSleeping()) poser(); });
    if (!scene.load.isLoading()) scene.load.start();
  }
  return poses;
}

class MarchandVillage {
  constructor(scene){
    this.s = scene; this.vise = false;
    this.zone = scene.carte.elements.find(e => e.type === "etal") || null;
    this.pnj = scene.pnj.find(n => n.boutique) || null;
    this.devant = this.zone && this.pnj && {x: this.pnj.x, y: this.zone.y + this.zone.h};   // in front of the counter
    if (this.zone) dessinerEtal(scene, scene.carte);
  }
  // the hero at the counter (its front tile, or one beside it)
  pres(){
    const d = this.devant, c = this.s.case;
    return !!d && c.y === d.y && Math.abs(c.x - d.x) <= 1;
  }
  // a touch on the merchant or anywhere on his stall, its awning and golden sign too (what a little finger aims at);
  // an Ombre in the Dojo's grass just behind it, or a touch right next to one, is taken first (Monde.toucher asks the
  // Ombres before the stall); any other touch forgets an earlier one
  touche(wx, wy){
    const z = this.zone, n = this.pnj, s = this.s;
    const surEtal = !!z && wx >= z.x * CASE && wx <= (z.x + z.w) * CASE && wy >= (z.y - HAUT_AUVENT) * CASE && wy <= (z.y + z.h) * CASE;
    const surLui = !!n && n.s && n.s.active && dessine(n.s) && n.s.getBounds().contains(wx, wy);
    this.vise = false;
    if (!surEtal && !surLui) return false;
    // the guide's arrow drawn over the stall (before the first fight, its Ombre out of sight above): a touch on it
    // follows the arrow to the Ombre, never into the shop
    const f = s.guide && s.guide.fleche;
    if (f && f.visible && Phaser.Geom.Rectangle.Contains(Phaser.Geom.Rectangle.Inflate(f.getBounds(), 4, 4), wx, wy)) {
      const c = s.guide.cible();
      if (c && c.ombre) { s.allerVers(c.ombre.x, c.ombre.y); return true; }
      return false;
    }
    s.marquer(this.devant.x, this.devant.y);
    if (n && n.s) s.vie.saluer(n.s);
    if (this.pres()) { this.ouvrir(); return true; }
    s.allerVers(this.devant.x, this.devant.y);
    this.vise = true;
    return true;
  }
  // the hero has walked to it (Monde.arrive)
  arrive(){
    if (!this.vise) return false;
    this.vise = false;
    if (this.pres()) this.ouvrir();
    return true;
  }
  ouvrir(){
    const s = this.s, n = this.pnj;
    s.chemin = []; s.pnjVise = null; s.arreter();
    reposHumain(s.heros, "haut");   // the hero faces the counter
    if (n && n.s) s.vie.parler(n.s);
    const voix = ouvrirBoutique();
    if (n && n.s) s.vie.mains(n.s, voix);
  }
  // the shop closed (js/marchand.js): the merchant rests, the companion shows what it wears
  fini(){ if (this.pnj && this.pnj.s) this.s.vie.repos(this.pnj.s); }
}
