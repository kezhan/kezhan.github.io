/* Académia : les lieux réservés aux compagnons, sur l'Île des Bâtisseurs (règles et état : js/lieux.js ; écran :
   js/lieu_vue.js ; leurs cases : js/jeu/carte_ile.js, reserves). Tenus par js/jeu/chantiers_ile.js, qui leur passe le
   toucher et l'arrivée du héros comme pour un terrain.
   1. Chaque lieu est dessiné sur ses deux cases, sa sentinelle de pierre à côté. Tant que son prochain palier est
      fermé à l'enfant (js/lieux.js, etatLieu), un cadenas flotte au-dessus et le dessin est un peu pâle.
   2. Ce que le lieu a fabriqué depuis la dernière visite flotte dans la même bulle que les récoltes des bâtiments
      (js/jeu/recoltes_ile.js, bulleRecolte), à la place du cadenas.
   3. Toucher le lieu, sa sentinelle ou sa bulle : le héros va devant, et l'écran du lieu s'ouvre.
   Rien ne bouge quand l'enfant demande le calme. Dessins : outils/village/scripts/ile_lieux.py. */
const uniteLieu = () => (QUALITE === "leger" ? 2 : 1) / 8;   // world units per pixel of the places' drawings
// the drawings the island shows for this child: the places, their sentinel, the padlock, and what each place makes
function imagesLieux(p = P()){
  if (!p || typeof reservesDe !== "function") return [];
  return [...Object.keys(LIEUX_RESERVES).flatMap(id => ["lieu_" + id, ...(reservesDe(p)[id].palier ? [choseRecolteLieu(id, p)] : [])]), "sentinelle", "cadenas"];
}
// under the buildings' keys (ch_<name>): the harvest bubble is the same for both
function chargerLieuxHD(load, noms = imagesLieux()){
  noms.forEach(n => { if (!load.textureManager.exists("ch_" + n)) load.image("ch_" + n, `${DOSSIER_HD}/lieux/${n}.png`); });
}

class ReservesIle {
  constructor(chantiers){
    this.ch = chantiers; this.s = chantiers.s; this.vues = {};
    this.lieux = (this.s.carte.reserves || []).filter(l => LIEUX_RESERVES[l.id]).map(l => ({...l, reserve: true}));
    this.lieux.forEach(l => this.poser(l));
    this.s.time.addEvent({delay: TEST ? 500 : 4000, loop: true, callback: () => this.maj()});
    this.s.time.delayedCall(0, () => this.maj());
  }
  pied(l){ return {x: (l.x + l.w / 2) * CASE, y: (l.y + l.h) * CASE}; }
  image(nom, x, y, prof, echelle = 1){
    const s = this.s, cle = "ch_" + nom;
    if (!s.textures.exists(cle)) return null;
    s.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
    return s.add.image(x, y, cle).setDepth(prof).setScale(uniteLieu() * echelle);
  }
  // the place on its tiles, its sentinel at its right foot, its padlock over it (hidden until maj() says it is closed)
  poser(l){
    const s = this.s, p = this.pied(l), prof = profondeur(l.y + l.h - 1, 0), v = this.vues[l.id] = {};
    v.im = this.image("lieu_" + l.id, p.x, p.y + 2, prof);
    if (!v.im) return;
    v.im.setOrigin(.5, 1); v.im.bornes = v.im.getBounds();
    v.sentinelle = this.image("sentinelle", p.x + 15, p.y + 3, prof + .3, .52);
    if (v.sentinelle) v.sentinelle.setOrigin(.5, 1);
    v.haut = p.y - v.im.displayHeight - 3;
    v.cadenas = this.image("cadenas", p.x, v.haut, 9000, .9);
    if (v.cadenas) {
      v.cadenas.setVisible(false);
      if (!calme()) s.tweens.add({targets: v.cadenas, y: v.haut - 2.5, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    }
    if (s.villageHD) s.villageHD.ecrans.push(v.im);   // the hero behind it shows through
  }
  // each place as the child's game has it: padlock or not, and its bubble when its thing is ready
  maj(){
    const s = this.s;
    if (!s.sys.isActive()) return;
    this.lieux.forEach(l => {
      const v = this.vues[l.id], e = etatLieu(l.id), pret = recolteLieuPrete(l.id);
      if (!v || !v.im) return;
      v.im.setTint(e.ouvert ? 0xFFFFFF : 0xC4C4DC);
      if (v.cadenas) v.cadenas.setVisible(!e.ouvert && !pret);
      if (v.bulle && (!pret || v.bulle.chose !== choseRecolteLieu(l.id))) { v.bulle.destroy(); v.bulle = null; }
      if (!pret || v.bulle) return;
      const chose = choseRecolteLieu(l.id);
      if (!s.textures.exists("ch_" + chose)) return this.charger(chose);
      v.bulle = bulleRecolte(s, chose, v.im.x, v.haut - 4, 9000);
    });
  }
  // a find's drawing that is not there yet (the tier was just won): loaded, then shown at the next look
  charger(chose){
    const l = this.s.load;
    chargerLieuxHD(l, [chose]);
    l.once("complete", () => { if (this.s.sys.isActive()) this.maj(); });
    if (!l.isLoading()) l.start();
  }
  // the place under a touch: its tiles and the row above (its drawing is taller), its drawing, its bubble or padlock
  sous(wx, wy){
    const x = wx / CASE, y = wy / CASE;
    return this.lieux.find(l => { const v = this.vues[l.id] || {};
      return (x >= l.x && x < l.x + l.w && y >= l.y - .6 && y < l.y + l.h)
        || (v.im && typeof sousLeDoigt === "function" && sousLeDoigt(this.s, v.im, wx, wy))
        || (v.im && Math.hypot(v.im.x - wx, v.haut - 4 - wy) <= 13 && (v.bulle || (v.cadenas && v.cadenas.visible))); }) || null;
  }
  // touched: the sentinel hops, a few sparkles
  fremir(l){
    const s = this.s, v = this.vues[l.id], g = v && v.sentinelle;
    if (!g || calme() || g.saute) return;
    g.saute = s.tweens.add({targets: g, y: g.y - 4, duration: 130, yoyo: true, repeat: 1, ease: "Sine.easeOut", onComplete: () => { g.saute = null; }});
    for (let i = 0; i < 4; i++) effetHD(s, "toucher", (l.x + .3 + Math.random() * 1.4) * CASE, (l.y + Math.random() * 1.6) * CASE, 10, profondeur(l.y + l.h, 9));
  }
  // the hero stands in front of it: the place's screen opens
  ouvrir(l){
    const s = this.s;
    s.chemin = []; s.pnjVise = null; s.arreter();
    const cx = l.x + l.w / 2 - .5, cy = l.y + l.h / 2 - .5;
    reposHumain(s.heros, Math.abs(cx - s.case.x) > Math.abs(cy - s.case.y) ? (cx > s.case.x ? "droite" : "gauche") : (cy < s.case.y ? "haut" : "bas"));
    if (typeof noterHistoire === "function") noterHistoire("lieu_" + l.id);
    if (typeof jouerSon === "function") jouerSon("scintille");
    ouvrirLieu(l.id);
  }
}
