/* Académia : chargement des images et création des animations (pack Ninja Adventure, CC0).
   Personnages et monstres : planches de 4 colonnes (bas, haut, gauche, droite) et une ligne par image de marche. */
const HEROS = [6, 17, 16, 25, 12, 19, 2];          // the child chooses one
const herosDe = p => p && HEROS.includes(p.heros) ? p.heros : HEROS[0];
// the same heroes drawn in high definition, in the same order (a saved choice keeps its hero)
const HEROS_HD = ["heros_garcon", "heros_fille", "heros_3", "heros_4", "heros_5", "heros_6", "heros_7"];
const herosHD = p => HEROS_HD[HEROS.indexOf(herosDe(p))] || HEROS_HD[0];
const PNJ = [9, 10, 14, 13, 22, 24];
const MONSTRES = [1, 2, 3, 5, 7, 8, 11, 12, 13, 14, 15, 16, 17, 18, 21, 22];
const DIRS = ["bas", "haut", "gauche", "droite"];

class Chargement extends Phaser.Scene {
  constructor(){ super("chargement"); }
  preload(){
    this.load.image("decor", "assets/decor.png");
    this.load.spritesheet("tuiles", "assets/decor.png", {frameWidth: 16, frameHeight: 16});   // single tiles, for the battle backdrops
    [...new Set([...HEROS, ...PNJ])].forEach(i => {
      this.load.spritesheet("perso" + i, `assets/personnages/${i}.png`, {frameWidth: 16, frameHeight: 16});
      this.load.image("portrait" + i, `assets/portraits/${i}.png`);
    });
    MONSTRES.forEach(i => this.load.spritesheet("monstre" + i, `assets/monstres/${i}.png`, {frameWidth: 16, frameHeight: 16}));
    [2, 3, 7, 11, 12, 18, 20].forEach(i => this.load.spritesheet("effet" + i, `assets/effets/${i}.png`, {frameWidth: 32, frameHeight: 32}));
  }
  create(){
    const marche = (cle, lignes) => DIRS.forEach((d, k) => this.anims.create({
      key: `${cle}-${d}`, frames: lignes.map(r => ({key: cle, frame: r * 4 + k})), frameRate: 8, repeat: -1}));
    [...new Set([...HEROS, ...PNJ])].forEach(i => marche("perso" + i, [0, 1, 2, 3]));
    MONSTRES.forEach(i => marche("monstre" + i, [0, 1, 2, 3]));
    [2, 3, 7, 11, 12, 18, 20].forEach(i => this.anims.create({key: "effet" + i, frames: this.anims.generateFrameNumbers("effet" + i), frameRate: 16}));
    window.__jeuPret = true;
    document.dispatchEvent(new Event("jeu-pret"));
  }
}
