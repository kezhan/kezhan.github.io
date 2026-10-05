/* Académia : le héros et son compagnon vus à travers ce qui les cache. Quand une maison, un arbre ou un petit lieu
   dessiné devant eux recouvre une partie de leur corps, le décor reste entier et leur silhouette se dessine par-dessus,
   à moitié transparente, seulement là où ils sont cachés : un masque fait des dessins qui sont devant eux. Leurs
   propres couleurs, et pas une teinte : un violet uni ferait penser à une Ombre, et se perd sur le vert des arbres.
   Il faut la carte graphique (WebGL) ; sans elle, js/jeu/decor_village_hd_jeu.js voile à 75 % un dessin derrière
   lequel le héros se tient vraiment. */
const SILHOUETTE = {teinte: null, alpha: .5};   // the tint (null: the character's own colours) and the opacity

class SilhouettesHD {
  constructor(scene){
    this.s = scene; this.persos = new Map();   // character -> {sil, devant, masque, cle}
    this.ok = scene.sys.renderer.type === Phaser.WEBGL && !!(Phaser.Display.Masks && Phaser.Display.Masks.BitmapMask);
    scene.silhouettes = this;
    scene.events.once("shutdown", () => this.persos.forEach(e => { e.masque.destroy(); e.devant.destroy(true); }));
  }
  entree(p){
    let e = this.persos.get(p);
    if (!e) {
      const s = this.s, devant = s.make.container({x: 0, y: 0, add: false});
      const sil = s.add.sprite(0, 0, "__DEFAULT").setOrigin(.5, 1).setDepth(9500).setVisible(false).setAlpha(SILHOUETTE.alpha);
      const masque = new Phaser.Display.Masks.BitmapMask(s, devant);
      sil.setMask(masque);
      e = {sil, devant, masque, cle: ""};
      this.persos.set(p, e);
    }
    return e;
  }
  // the drawings in front of each character that hide part of it (js/jeu/decor_village_hd_jeu.js, devoiler): the
  // shape of its mask is rebuilt only when that list changes
  devant(liste){
    if (!this.ok) return;
    liste.forEach(({p, ecrans}) => {
      const e = this.entree(p), cle = ecrans.map(im => im.texture.key + im.frame.name + im.x + "," + im.y).join(";");
      if (cle === e.cle) return;
      e.cle = cle;
      e.devant.removeAll(true);
      ecrans.forEach(im => e.devant.add(this.s.make.image({x: im.x, y: im.y, key: im.texture.key, frame: im.frame.name, add: false})
        .setOrigin(im.originX, im.originY).setScale(im.scaleX, im.scaleY).setFlipX(im.flipX)));
      e.vide = !ecrans.length;
    });
  }
  // every frame: each silhouette takes its character's place, pose and size; hidden while nothing hides it
  suivre(persos){
    if (!this.ok) return;
    persos.forEach(p => {
      if (!p) return;
      const e = this.persos.get(p); if (!e) return;
      const vu = p.active && p.visible && p.texture.key !== "__DEFAULT" && !e.vide && ecranCourant === "monde";
      e.sil.setVisible(vu);
      if (!vu) return;
      if (e.sil.texture.key !== p.texture.key || e.sil.frame.name !== p.frame.name) {
        e.sil.setTexture(p.texture.key, p.frame.name);
        if (SILHOUETTE.teinte === null) e.sil.clearTint(); else e.sil.setTintFill(SILHOUETTE.teinte);
      }
      e.sil.setPosition(p.x, p.y).setScale(p.scaleX, p.scaleY).setFlipX(p.flipX).setAngle(p.angle).setOrigin(p.originX, p.originY);
    });
    this.persos.forEach((e, p) => { if (!p.active) { e.sil.destroy(); e.masque.destroy(); e.devant.destroy(true); this.persos.delete(p); } });
  }
}
