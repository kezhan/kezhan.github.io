/* Académia : le village dessiné derrière l'accueil, la création du Gardien et le choix du premier compagnon. Dès que
   les images sont là (js/jeu/chargement.js), la place du village se dessine une fois, avec ses habitants et des
   Ombres dans les hautes herbes ; sa photo, réduite (donc douce, légèrement floue), devient le fond de ces écrans
   (variable CSS --fond-accueil), puis la scène s'arrête : rien ne tourne pendant que l'enfant tape son prénom. */
class Vitrine extends Phaser.Scene {
  constructor(){ super("vitrine"); }
  create(){
    const carte = construireCarte();
    dessinerVillageHD(this, carte);
    carte.pnj.forEach(n => { const s = spriteHumain(this, n.cle, n.x, n.y).setDepth(profondeur(n.y)); reposHumain(s, DIRS[n.dir]); ombreAuSolHD(this, s); });
    [["neantik", 26, 8], ["grisouille", 30, 21], ["brumichon", 9, 21]].forEach(([cle, x, y]) => {   // a few Ombres in the grass
      const o = spriteCreature(this, cle, x, y, HAUTEUR_OMBRE).setDepth(profondeur(y)); ombreAuSolHD(this, o);
    });
    const w = this.scale.width / RATIO, h = this.scale.height / RATIO, portrait = h > w;
    const z = Math.min(5, Math.max(2, Math.round(Math.min(w / ((portrait ? 14 : 24) * CASE), h / (16 * CASE)))));
    this.cameras.main.setBounds(0, 0, LARG * CASE, HAUT * CASE).setZoom(z * RATIO).centerOn(22 * CASE, (portrait ? 11 : 12) * CASE);
    this.time.delayedCall(250, () => photographierVitrine(this));
  }
}
// the photo, made small (the background is then soft without a filter that would cost at every frame), as the
// background of the home screens; then the scene stops
function photographierVitrine(scene){
  if (!scene.sys.isActive()) return;
  JEU.renderer.snapshot(img => {
    if (!scene.sys.isActive()) return;   // the child went to the village meanwhile: that picture is not the square
    try {
      const k = Math.min(1, 520 / img.width), c = document.createElement("canvas");
      c.width = Math.round(img.width * k); c.height = Math.round(img.height * k);
      const g = c.getContext("2d"); g.imageSmoothingQuality = "high"; g.drawImage(img, 0, 0, c.width, c.height);
      document.documentElement.style.setProperty("--fond-accueil", `url(${c.toDataURL("image/jpeg", .82)})`);
      document.body.classList.add("monde-dessine");
    } catch (e) {}
    scene.scene.stop();
  }, "image/jpeg", .9);
}
// the home screens are shown: the village is photographed once, as soon as its images are there
function preparerVitrine(){
  if (document.body.classList.contains("monde-dessine") || preparerVitrine.fait) return;
  const lancer = () => {
    if (preparerVitrine.fait || !["accueil", "creation", "choix"].includes(ecranCourant)) return;
    preparerVitrine.fait = true;
    JEU.scene.start("vitrine");
  };
  if (window.__jeuPret) lancer(); else document.addEventListener("jeu-pret", lancer, {once: true});
}
