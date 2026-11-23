/* Académia : l'intérieur d'une maison dans la scène du village (js/jeu/monde.js, quand la partie est dans une maison ;
   carte : js/jeu/carte_maison.js).
   1. Le dessin : la salle entière est une image (son mur du fond, son plancher, son paillasson), posée au milieu de la
      grille ; par-dessus, les quatre choses à toucher (triées par ligne comme les personnages) et la porte de la salle
      du chef, fermée dans sa brume violette ou ouverte sur sa lumière dorée. Dessins : outils/village/scripts/
      interieurs.py et interieurs_choses.py (à nous), dans assets/<qualité>/interieurs.
   2. InterieurMaison (aussi pour les choses d'un quartier du village, js/jeu/quartier_village.js) : une chose touchée, le héros va devant elle, et son écran s'ouvre (js/maison_chose.js) ; la
      lunette de l'Observatoire ouvre la coupole. Une petite étincelle flotte sur chaque chose qui en a une à donner
      aujourd'hui. Devant la porte du fond : la salle du chef (js/jeu/lanceur.js, porteDuChef) ; sur le paillasson :
      la sortie, devant la maison (sortirMaison).
   3. HistoireMaison : ce que la salle dit d'elle-même. À l'entrée, l'habitant d'une maison de langue salue dans sa
      langue (js/jeu/langues_habitants.js) ; sans habitant, la voix nomme la maison à la première visite. Les missions
      du jour tenues ici y sont annoncées. La main du guide montre une chose qui a encore son étincelle.
   Ce que le village a et que la salle n'a pas (la place, le jardin, le marchand, la barque, les petites bêtes) est
   absent : lieuAbsent() répond à leur place (js/jeu/ile_village.js). */
const dossierInterieurs = () => `${DOSSIER_HD}/interieurs/`;
const IMAGES_INTERIEUR = ["porte_chef_fermee", "porte_chef_ouverte"];

function chargerMaisonHD(load, region){
  const charger = (cle, fichier) => { if (!load.textureManager.exists(cle)) load.image(cle, dossierInterieurs() + fichier + ".png"); };
  charger("mi_salle_" + region, "salle_" + region);
  IMAGES_INTERIEUR.forEach(n => charger("mi_" + n, n));
  MAISONS[region].choses.forEach(c => charger(`mi_chose_${region}_${c.id}`, `chose_${region}_${c.id}`));
  if (!load.textureManager.exists("ico_etincelle")) load.image("ico_etincelle", iconeSrc("etincelle"));
}
// the room in world units, and what the village's own code expects of a drawn place (scene.villageHD)
function dessinerMaisonHD(scene, carte){
  const S = carte.salle, rendu = {etiquettes: {}, maisons: [], ecrans: [], touffes: {}, lumieres: [], enseignes: {}, carte};
  scene.add.rectangle(0, 0, LARG * CASE, HAUT * CASE, 0x1D1640).setOrigin(0).setDepth(0);
  const cle = "mi_salle_" + carte.interieur;
  if (scene.textures.exists(cle)) {
    scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
    scene.add.image(S.x * CASE, S.y * CASE, cle).setOrigin(0).setDisplaySize(S.w * CASE, S.h * CASE).setDepth(1);
  }
  scene.villageHD = rendu;
  return rendu;
}

class InterieurMaison {
  constructor(scene){
    this.s = scene; this.region = scene.carte.interieur; this.vise = null;
    const image = (cle, x, y, w, h, prof) => {
      if (!scene.textures.exists(cle)) return null;
      scene.textures.get(cle).setFilter(Phaser.Textures.FilterMode.LINEAR);
      return scene.add.image(x, y, cle).setOrigin(.5, 1).setDisplaySize(w, h).setDepth(prof);
    };
    // the four things, each 2 x 2 tiles over its two tiles of floor; a little étincelle floats over it
    this.choses = scene.carte.choses.map(c => {
      const x = (c.x + 1) * CASE, y = (c.y + 1) * CASE;
      const im = image(`mi_chose_${c.region}_${c.id}`, x, y, 2 * CASE, 2 * CASE, profondeur(c.y, 0));
      if (im) { im.bornes = im.getBounds(); scene.villageHD.ecrans.push(im); }
      const bulle = image("ico_etincelle", x, y - 2 * CASE + 5, 9, 9, 9000);
      if (bulle && !calme()) scene.tweens.add({targets: bulle, y: bulle.y - 2.5, duration: 700 + c.x * 20, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
      return {c, im, bulle};
    });
    // the door of the chief's room, in the back wall
    // (a quarter of the village has none: js/jeu/carte_quartier.js)
    const p = scene.carte.porteChef, px = p ? (p.x + p.w / 2) * CASE : 0, py = p ? (p.y + p.h) * CASE : 0;
    this.centre = {x: px, y: py - 16};
    this.porteFermee = p ? image("mi_porte_chef_fermee", px, py, 40, 40, 2) : null;
    this.porteOuverte = p ? image("mi_porte_chef_ouverte", px, py, 40, 40, 2) : null;
    if (this.porteOuverte && !calme()) scene.tweens.add({targets: this.porteOuverte, alpha: {from: 1, to: .82}, duration: 1300, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    this.maj();
  }
  ouverte(){ return !!this.region && regionEtat(this.region).etape >= ETAPES; }
  // the room as the game stands: the étincelles still to win today, the chief's door
  maj(){
    this.choses.forEach(q => { if (q.bulle) q.bulle.setVisible(etincelleChose(q.c)); });
    const o = this.ouverte();
    if (this.porteFermee) this.porteFermee.setVisible(!o);
    if (this.porteOuverte) this.porteOuverte.setVisible(o);
  }
  sous(wx, wy){
    return this.choses.find(q => (q.im && q.im.getBounds().contains(wx, wy)) || (q.bulle && q.bulle.visible && Phaser.Math.Distance.Between(wx, wy, q.bulle.x, q.bulle.y - 4) < 8)) || null;
  }
  // a thing is touched: it shivers, the hero walks in front of it (already there: it opens at once)
  touche(wx, wy){
    const q = this.sous(wx, wy), s = this.s;
    this.vise = null;
    if (!q) return false;
    const d = q.c.devant;
    s.marquer(d.x, d.y);
    if (q.im && !calme()) s.tweens.add({targets: q.im, angle: {from: -3, to: 3}, duration: 90, yoyo: true, repeat: 1, onComplete: () => q.im.setAngle(0)});
    if (s.case.x === d.x && s.case.y === d.y) { this.ouvrir(q); return true; }
    s.allerVers(d.x, d.y);
    this.vise = q;
    return true;
  }
  // the hero has walked there (Monde.arrive)
  arrive(){
    const q = this.vise, s = this.s;
    this.vise = null;
    if (!q) return false;
    if (s.case.x === q.c.devant.x && s.case.y === q.c.devant.y) this.ouvrir(q);
    return true;
  }
  ouvrir(q){
    const s = this.s;
    s.dir = "haut"; reposHumain(s.heros, "haut");
    ouvrirChose(q.c);
  }
  // in front of a door of the room (Monde.devantPorte): the chief's room, or the way out
  // (a quarter's way out is its path back to the village, js/quartiers.js)
  porte(quoi){ return quoi !== "sortie" ? porteDuChef(this.region) : this.s.quartier ? sortirQuartier() : sortirMaison(); }
}

class HistoireMaison extends HistoireVillage {
  crepuscule(){}   // the lamps are lit inside: no dusk over the room
  parole(cle, texte, langue, note){ const h = MAISONS[this.s.interieur].habitant; return bulleVillage(cle, h && h.cle === cle ? h.nom : "", texte, langue, note); }
  update(){
    const s = this.s, now = performance.now();
    if (dialogueOuvert() && bulleVillageOuverte()) $("bulleVillage").hidden = true;
    if (now < this.prochain) return;
    this.prochain = now + 400;
    if (ecranCourant !== "monde" || this.occupee || s.moment || s.verrou || dialogueOuvert() || !P()) return;
    if (typeof voixEnCours === "function" && voixEnCours()) return;
    if (!this.accueilli) { this.accueilli = true; return this.raconter(() => this.accueillir()); }
    verifierMissions();   // a mission done in a house counts here, the same day
    const m = missionADire();
    if (m) return this.raconter(() => annoncerMission(m));
  }
  // on the way in: the resident's word in the house's language; without a resident, the house named at the first visit
  accueillir(){
    const s = this.s, r = regionDe(s.interieur), n = s.pnj[0], cle = "maison_" + r.id, petit = P().age <= 5;
    if (n) {
      const salut = typeof salutHabitant === "function" ? salutHabitant(n) : null;
      s.vie.saluer(n.s);
      return salut ? this.parole(n.cle, salut.ligne, salut.langue) : null;
    }
    if (histoireDe()[cle]) return null;
    noterHistoire(cle);
    return dire(petit ? "Touche les choses de la salle : elles ont des secrets pour toi !"
      : `Te voici dans ${{"du ": "le ", "de la ": "la ", "de l'": "l'"}[DE_REGION[r.id]] || ""}${r.nom} ! Touche les choses de la salle : chacune a un secret, et une étincelle pour toi.`);
  }
  // what the guide's hand shows here: a thing that still has its étincelle today
  cibleGuide(){
    const q = this.s.maison.choses.filter(x => x.im && etincelleChose(x.c)).sort((a, b) => b.c.y - a.c.y)[0];   // a low one first: clear of the top bar
    return q ? {x: q.im.x, y: q.im.y - CASE, haut: q.im.y - 2 * CASE - 6, dit: `Touche ${q.c.nom} !`} : null;
  }
}
