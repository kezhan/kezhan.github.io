/* Académia : ce qui se passe sur l'Île des Bâtisseurs (js/jeu/monde.js, quand la partie est sur l'île).
   1. Les terrains : toucher un terrain ou son panneau de chantier, le héros va devant le panneau, le panneau frémit, la
      cheffe de chantier lève les bras et dit, dans la bulle qui ne bloque rien, ce qui s'y construira bientôt
      (CHANTIERS, js/jeu/carte_ile.js) : « Ici, nous construirons bientôt la boulangerie ! On y comptera les pains… ».
      Chaque terrain vu est noté dans P().histoire (terrain_<id>), pour les chantiers à venir (ticket
      construire-et-collectionner).
   2. Ce que disent les deux habitants (Monde.parler) : un mot de leur langue d'abord (la cheffe l'anglais, le passeur
      le luxembourgeois, js/jeu/langues_habitants.js), puis leurs phrases ; le passeur propose le retour (js/jeu/voyage.js).
      Pour la petite, le sens du mot suit sensADire (js/mots_du_jeu.js) : les premières fois, jamais deux fois dans la
      minute ; ensuite le mot seul.
   3. Ce que l'île raconte d'elle-même (HistoireIle, comme HistoireVillage au village, une chose à la fois, quand rien ne
      se passe) : les missions du jour tenues sur l'île y sont comptées et annoncées, l'autocollant gagné le jour même ;
      le soir vient sur l'île comme au village (le crépuscule, le compagnon qui bâille), et c'est le passeur qui dit au
      revoir (« Gutt Nuecht ») et propose de rentrer. À la première arrivée, la main du guide montre la cheffe jusqu'à
      ce que l'enfant lui ait parlé (js/jeu/guide_village.js), puis les Ombres comme ailleurs.
   4. Ce que suppose le premier village (la place de l'Arbre-Monde et son panneau, le jardin, le marchand) est absent :
      lieuAbsent() répond à leur place, sans rien faire. Une porte qui s'ouvre au village pendant que l'enfant est sur
      l'île (la quatrième Ombre battue ici) : le passeur le dit. */
// a place of the first village that the island does not have: it answers, and does nothing
const lieuAbsent = () => ({zone: null, vise: false, im: null, rendus: new Set(), touche: () => false, arrive: () => false, maj(){}, majPanneau(){}, fini(){}});

// an islander's first line: a word of their language (a new one at each visit; the one said after a sneeze first),
// then its meaning, said for the little one only when sensADire allows it (the big one reads it, as with the
// villagers); null when they have no recorded word
function salutIle(n){
  const L = LANGUES_HABITANTS[n.cle]; if (!L || typeof motsDuJeu !== "function") return null;
  const attente = MOT_EN_ATTENTE[n.cle]; delete MOT_EN_ATTENTE[n.cle];
  const mots = motsDuJeu(L.mots, L.langue);
  let m = attente ? (mots.find(x => x.mot === attente.mot) || {cle: "a_tes_souhaits", langue: L.langue, mot: attente.mot, sens: attente.sens, nom: L.nom}) : null;
  if (!m) {
    if (!mots.length) return null;
    const i = VISITES_HABITANTS[n.cle] = ((VISITES_HABITANTS[n.cle] ?? -1) + 1) % mots.length;
    m = mots[i];
  }
  if (typeof noterHistoire === "function") noterHistoire("visite_" + n.cle);   // the day's visit (the « Dis bonjour » mission)
  const petit = P() && P().age <= 5, sens = !petit || sensADire(m);
  return {langue: L.langue, ligne: sens ? `« ${m.mot} » : c'est « ${m.sens} », en ${L.nom} !` : `« ${m.mot} » !`};
}

class ChantiersIle {
  constructor(scene){ this.s = scene; this.vise = null; this.terrains = scene.carte.terrains || []; }
  // the lot under a touch: its tiles (and the stakes standing over its first row) or its sign
  terrainSous(wx, wy){
    const x = wx / CASE, y = wy / CASE;
    return this.terrains.find(t => (x >= t.x && x < t.x + t.w && y >= t.y - .4 && y < t.y + t.h)
      || (Math.floor(x) === t.panneau.x && y >= t.panneau.y - .9 && y < t.panneau.y + 1)) || null;
  }
  touche(wx, wy){
    const t = this.terrainSous(wx, wy), s = this.s;
    this.vise = null;
    if (!t) return false;
    s.marquer(t.devant.x, t.devant.y);
    this.fremir(t);
    if (s.case.x === t.devant.x && s.case.y === t.devant.y) { this.expliquer(t); return true; }
    s.allerVers(t.devant.x, t.devant.y);
    this.vise = t;
    return true;
  }
  // the hero has walked there (Monde.arrive)
  arrive(){
    const t = this.vise, s = this.s;
    this.vise = null;
    if (!t) return false;
    if (s.case.x === t.devant.x && s.case.y === t.devant.y) this.expliquer(t);
    return true;
  }
  // the sign shivers, a few sparkles over the lot
  fremir(t){
    const s = this.s, r = s.villageHD && s.villageHD.ileHD && s.villageHD.ileHD.terrains[t.id], p = r && r.panneau;
    if (!p || calme() || p.tremble) return;
    p.tremble = s.tweens.add({targets: p, angle: {from: -4, to: 4}, duration: 90, yoyo: true, repeat: 2, ease: "Sine.easeInOut",
      onComplete: () => { p.setAngle(0); p.tremble = null; }});
    for (let i = 0; i < 4; i++) effetHD(s, "toucher", (t.x + .5 + Math.random() * (t.w - 1)) * CASE, (t.y + .5 + Math.random() * (t.h - 1)) * CASE, 10, profondeur(t.y + t.h, 9));
  }
  // the cheffe tells what will be built there
  expliquer(t){
    const s = this.s, c = CHANTIERS[t.id];
    if (!c) return;
    s.chemin = []; s.pnjVise = null; s.arreter();
    reposHumain(s.heros, t.panneau.x > s.case.x ? "droite" : t.panneau.x < s.case.x ? "gauche" : t.panneau.y < s.case.y ? "haut" : "bas");
    const n = s.pnj.find(q => q.cle === "cheffe");
    if (n && n.s) { s.vie.saluer(n.s, 1600); }
    if (typeof jouerSon === "function") jouerSon("scintille");
    if (typeof noterHistoire === "function") noterHistoire("terrain_" + t.id);
    bulleVillage("cheffe", "La cheffe de chantier", `Ici, nous construirons bientôt ${c.nom} ! ${c.dit}`);
  }
}

// what the island tells by itself: the day's missions, the evening; what its two islanders say. The first village's
// story (the Arbre-Monde, the Sage's board, Hugo) stays in the village: its crepuscule, yawn and words of tiredness
// are shared (js/jeu/histoire_village.js)
class HistoireIle extends HistoireVillage {
  parole(cle, texte, langue, note){ return bulleVillage(cle, {cheffe: "La cheffe de chantier", passeur: "Le passeur"}[cle] || "", texte, langue, note); }
  // every frame (Monde.update), one thing at a time, when nothing else happens: a mission done here, the evening
  update(){
    const s = this.s, now = performance.now();
    if (dialogueOuvert() && bulleVillageOuverte()) $("bulleVillage").hidden = true;
    if (now < this.prochain) return;
    this.prochain = now + 400;
    if (ecranCourant !== "monde" || this.occupee || s.moment || s.verrou || dialogueOuvert() || !P()) return;
    if (typeof voixEnCours === "function" && voixEnCours()) return;
    verifierMissions();   // a mission done on the island counts here, the same day (its sticker in the album)
    const m = missionADire();
    if (m) return this.raconter(() => annoncerMission(m));
    if (s.enMarche || now - s.guide.dernier < 1500) return;
    if (!vuAujourdhui("soir") && tempsJoue() >= SOIR_MS) return this.raconter(() => this.coucher());
    if (this.voile && now > this.baille) { this.baille = now + BAILLE_MS; this.bailler(); }
  }
  // the evening on the island: the dusk falls, the companion yawns, then the ferryman says good night and offers to take
  // the child home (the game stays playable: « Pas maintenant », or a touch on the island, and the child stays)
  coucher(){
    noterHistoire("soir");
    const s = this.s, n = s.pnj.find(q => q.passeur);
    this.crepuscule(true);
    return wait(2400).then(() => { this.baille = performance.now() + BAILLE_MS; return Promise.all([this.bailler(true), wait(1200)]); })
      .then(() => {
        if (!s.sys.isActive() || ecranCourant !== "monde" || dialogueOuvert() || !n) return;
        s.parler({...n, soir: true});
      });
  }
  lignes(n){
    const salut = salutIle(n), h = this.lignesHistoire(n);
    return salut ? {...h, lignes: [salut.ligne, ...h.lignes], langue: salut.langue} : h;
  }
  lignesHistoire(n){
    if (n.cle !== "cheffe") return {lignes: n.dit};
    if (!histoireDe().cheffe) noterHistoire("cheffe");   // the guide's hand stops showing her
    return {lignes: P().age <= 5
      ? ["Je suis la cheffe de chantier !", "Bientôt, nous construirons des maisons sur les terrains. Touche un panneau pour voir !"]
      : ["Je suis la cheffe de chantier de l'Île des Bâtisseurs !",
         "Sur chaque terrain, nous construirons bientôt un bâtiment : une boulangerie, un phare, un moulin, une bibliothèque, une serre et un kiosque à musique.",
         "Touche un panneau de chantier pour savoir ce qui s'y construira !"]};
  }
  // what the guide's hand shows here before the Ombres: the cheffe, until the child has talked to her
  cibleGuide(){
    if (histoireDe().cheffe) return null;
    const n = this.s.pnj.find(q => q.cle === "cheffe"), sp = n && n.s;
    if (!sp || !sp.active || !dessine(sp)) return null;
    return {x: sp.x, y: sp.y - sp.displayHeight / 2, haut: sp.y - sp.displayHeight, dit: "Touche la cheffe de chantier !"};
  }
  // the fourth Ombre beaten here opened a door of the first village: the ferryman says it, and that he can take the
  // child back there
  porteOuverte(region){
    const r = regionDe(region);
    if (r) this.parole("passeur", `${majuscule(porteDe(r))} s'est ouverte, au village ! Ma barque t'y ramène quand tu veux.`);
  }
}
