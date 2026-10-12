/* Académia : le potager du village ouvre le jardin de l'enfant (js/jardin_vue.js). Un toucher sur le potager (ses
   cases et la palissade dessinée au-dessus) : le héros va devant lui, puis l'écran du jardin s'ouvre. Au-dessus du
   potager, un signe qui se balance : la récolte quand une plante est mûre, une goutte quand une plante a soif, un
   sachet de graines tant que rien n'est planté. */
function chargerJardinVillage(load){
  load.image("jardin_goutte", srcJardin("goutte"));
  load.image("jardin_graines", srcJardin("graines_carotte"));
  Object.keys(CULTURES).forEach(id => load.image("jardin_recolte_" + id, srcJardin("recolte_" + id)));
}

class JardinVillage {
  constructor(scene){
    this.s = scene; this.vise = false; this.signe = null;
    this.zone = scene.carte.elements.find(e => e.type === "potager") || null;
    this.devant = this.zone && {x: this.zone.x + Math.floor(this.zone.w / 2), y: this.zone.y + this.zone.h};
    this.maj();
  }
  // the hero next to the vegetable garden (on a tile around it)
  pres(){
    const z = this.zone, c = this.s.case;
    return !!z && c.x >= z.x - 1 && c.x <= z.x + z.w && c.y >= z.y - 1 && c.y <= z.y + z.h;
  }
  touche(wx, wy){
    const z = this.zone;
    if (!z || wx < z.x * CASE || wx > (z.x + z.w) * CASE || wy < (z.y - 1) * CASE || wy > (z.y + z.h) * CASE) return false;
    const s = this.s;
    s.marquer(this.devant.x, this.devant.y);
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
    const s = this.s;
    s.chemin = []; s.pnjVise = null; s.arreter();
    ouvrirJardin();
  }
  maj(){
    const z = this.zone, p = P();
    if (this.signe) { this.s.tweens.killTweensOf(this.signe); this.signe.destroy(); this.signe = null; }
    if (!z || !p) return;
    const j = jardinDe(p), a = aFaireAuJardin(p), mure = j.parcelles.find(pa => etatParcelle(pa) === "mure");
    const cle = mure ? "jardin_recolte_" + mure.culture : a.soif ? "jardin_goutte" : !a.plantes ? "jardin_graines" : null;
    if (!cle || !this.s.textures.exists(cle)) return;
    // over the middle of its vegetable rows (above it, the top bar would hide it at the top of the map)
    const x = (z.x + z.w / 2) * CASE, y = (z.y + .2) * CASE;
    const fond = this.s.add.circle(0, 0, 6, 0xFFFFFF, .96).setStrokeStyle(1, 0x1D1640);
    const im = this.s.add.image(0, 0, cle);
    im.setScale(9 / Math.max(im.width, im.height));
    this.signe = this.s.add.container(x, y, [fond, im]).setDepth(9000);
    if (!calme()) this.s.tweens.add({targets: this.signe, y: y - 2, duration: 700, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
  }
}
