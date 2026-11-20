/* Académia : ce qui se passe sur l'Île des Bâtisseurs (js/jeu/monde.js, quand la partie est sur l'île).
   1. Les terrains et leurs bâtiments à construire sont dans js/jeu/chantiers_ile.js (ChantiersIle).
   2. Ce que disent les deux habitants (Monde.parler) : un mot de leur langue d'abord (la cheffe l'anglais, le passeur
      le luxembourgeois, js/jeu/langues_habitants.js), puis leurs phrases ; la cheffe dit où en sont les chantiers, le
      passeur propose le retour (js/jeu/voyage.js). Pour la petite, le sens du mot suit sensADire (js/mots_du_jeu.js) :
      les premières fois, jamais deux fois dans la minute ; ensuite le mot seul.
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
    const finis = batimentsFinis().length, tout = finis >= Object.keys(SUJETS_CHANTIER).length;
    const ou = tout ? "Tu as tout construit : bravo ! Touche tes bâtiments, ils ont des choses à t'apprendre."
      : finis ? `Tu as déjà fini ${finis > 1 ? finis + " bâtiments" : "un bâtiment"} : bravo ! Touche un panneau pour construire encore.` : "";
    return {lignes: P().age <= 5
      ? ["Je suis la cheffe de chantier !", ou || "Ici, on construit des maisons ! Touche un panneau : on construit ensemble."]
      : ["Je suis la cheffe de chantier de l'Île des Bâtisseurs !",
         ou || "Sur chaque terrain, tu peux construire un bâtiment : une boulangerie, un phare, un moulin, une bibliothèque, une serre et un kiosque à musique.",
         "Chaque étape demande des étincelles et une bonne réponse. Touche un panneau de chantier pour construire !"]};
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
