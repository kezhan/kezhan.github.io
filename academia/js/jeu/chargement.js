/* Académia : chargement des images, pendant que l'enfant choisit son prénom : ce que tout village et tout combat
   montrent (habitants, Ombres, effets, décors de combat, village), en haute définition ou en version légère
   (js/jeu/qualite.js). Le héros et le compagnon se chargent à l'entrée du village (js/jeu/monde.js). */
const HEROS = [6, 17, 16, 25, 12, 19, 2];          // the hero chosen, as saved (numbers kept from the first versions)
const herosDe = p => p && HEROS.includes(p.heros) ? p.heros : HEROS[0];
// the heroes' drawings, in the same order (a saved choice keeps its hero)
const HEROS_HD = ["heros_garcon", "heros_fille", "heros_3", "heros_4", "heros_5", "heros_6", "heros_7"];
const herosHD = p => HEROS_HD[HEROS.indexOf(herosDe(p))] || HEROS_HD[0];
const DIRS = ["bas", "haut", "gauche", "droite"];

class Chargement extends Phaser.Scene {
  constructor(){ super("chargement"); }
  preload(){
    reessayerImages(this);
    chargerAtlas(this, ["sage", "hugo", "paco", ...Object.values(CLES_OMBRES)]);
    chargerEffetsHD(this.load, QUALITE);
    chargerFondsHD(this.load, QUALITE);
    chargerVillageHD(this.load, QUALITE);
  }
  create(){
    window.__jeuPret = true;
    document.dispatchEvent(new Event("jeu-pret"));
  }
}
