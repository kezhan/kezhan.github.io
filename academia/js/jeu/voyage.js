/* Académia : le voyage en barque entre le premier village et l'Île des Bâtisseurs (ticket plusieurs-villages).
   1. Au village, la barque tirée sur la plage de la mare : la toucher, le héros va devant elle, et le passeur, qui se
      reposait dans sa barque, se lève et parle. Sur l'île, le passeur se tient sur le ponton, près de sa barque à quai :
      le toucher, ou toucher la barque, le fait parler (js/jeu/monde.js, Monde.parler).
   2. Il commence par un mot de sa langue, le luxembourgeois, dit par son enregistrement (salutIle, js/jeu/ile_village.js).
      Depuis le village, la barque ne part qu'après le premier badge ou dix Ombres consolées (P().regions,
      P().album.ombres et P().hist) ; avant, il dit avec gentillesse ce qui manque (« Encore 3 Ombres à consoler, et je
      t'emmène sur l'île ! »). Rentrer au village est toujours permis. Il propose de partir : jusqu'à 5 ans, un seul gros
      bouton, la barque dessinée, nommé par la voix (s'en aller, un toucher sur le village, veut dire « pas
      maintenant ») ; après, « Partir » et « Pas maintenant ». Le soir sur l'île, il dit « Gutt Nuecht » et propose de
      rentrer (js/jeu/ile_village.js).
   3. Ce que montre le voyage (la traversée, l'île) se charge à l'avance : au village, en tâche de fond dès que la barque
      est ouverte et que le village est au calme (avant le premier voyage de la séance), et chaque fois que le passeur
      propose de partir. Si l'enfant part avant la fin, l'écran fermé en étoile montre la barque qui se berce et « La
      barque se prépare… » (dit pour la petite), jamais un écran vide.
   4. Partir : la partie passe dans l'autre village, devant la barque (js/etat.js, lieu et positions), le journal note
      le voyage (js/journal.js) ; l'écran se ferme en étoile sur la barque, la traversée se joue (js/jeu/traversee.js),
      puis l'autre village s'ouvre. À l'arrivée sur l'île, le passeur dit « Wëllkomm », au village « Äddi », et
      retourne se reposer dans sa barque.
   Une page rechargée pendant la traversée s'ouvre dans le village d'arrivée. */
const OMBRES_POUR_PARTIR = 10, VOYAGE_ATTENTE_MAX = 20000;
// a crossing playing; the village just reached (its greeting is due); a trip already made in this sitting
const VOYAGE = {enCours: false, arrivee: null, fait: false};
if (typeof ECRANS === "object" && !ECRANS.includes("voyage")) ECRANS.push("voyage");   // the crossing's screen (js/outils.js)

// ---- the rules
// the Ombres consoled: the album's count, or the fights won when the game is older than the album
function ombresConsolees(p = P()){
  if (!p) return 0;
  const album = Object.values((p.album && p.album.ombres) || {}).reduce((t, n) => t + (Number(n) || 0), 0);
  const combats = (p.hist || []).filter(h => h && h.gagne && !h.boss).length;
  return Math.max(album, combats);
}
const badgesDesChefs = (p = P()) => Object.values((p && p.regions) || {}).reduce((t, r) => t + ((r && Number(r.badges)) || 0), 0);
const barqueOuverte = (p = P()) => !!p && (badgesDesChefs(p) > 0 || ombresConsolees(p) >= OMBRES_POUR_PARTIR);
const ombresAConsoler = (p = P()) => Math.max(1, OMBRES_POUR_PARTIR - ombresConsolees(p));

// the tiles where the hero stands to board: around the boat, the map's arrival first (on the island's pier)
function devantsBarque(carte){
  const z = carte.ile ? carte.barque : carte.elements.find(e => e.type === "barque");
  if (!z) return [];
  const l = [];
  for (let x = z.x; x < z.x + z.w; x++) l.push({x, y: z.y + z.h});
  for (let y = z.y; y < z.y + z.h; y++) l.push({x: z.x - 1, y}, {x: z.x + z.w, y});
  for (let x = z.x; x < z.x + z.w; x++) l.push({x, y: z.y - 1});
  if (carte.arrivee) l.unshift(carte.arrivee);
  return l.filter((c, i) => c.x >= 0 && c.y >= 0 && c.x < LARG && c.y < HAUT && !carte.bloque[c.y][c.x]
    && l.findIndex(d => d.x === c.x && d.y === c.y) === i);
}
const arriveeDe = carte => carte.arrivee || devantsBarque(carte)[0] || carte.depart;

// ---- what a trip shows, loaded ahead by the village's own loader (never two loaders on the same drawing)
// the textures the crossing needs, and the island's when going there
const texturesVoyage = vers => ["vh_traversee_fond", "vh_ile_barque", "vh_ile", "hd_passeur",
  ...(vers === "ile" ? ["vh_sol_plage", "vh_sol_ecume", "vh_sol_herbe_ile", "vh_mer", "hd_cheffe"] : [])];
const voyagePret = (scene, vers) => texturesVoyage(vers).every(k => scene.textures.exists(k));
// asked to the scene's loader (already there: skipped); the promise ends when the loader is idle again
function prechargerVoyage(scene, vers){
  return new Promise(ok => {
    if (!scene.sys.isActive()) return ok(false);
    const a = actif(), l = scene.load;
    chargerAtlas(scene, ["passeur", herosHD(P()), a && cleDe(a), ...(vers === "ile" ? ["cheffe"] : [])]);
    chargerTraverseeHD(l, QUALITE);
    if (vers === "ile") chargerIleHD(l, QUALITE);
    if (!l.isLoading() && !l.list.size) return ok(voyagePret(scene, vers));
    l.once("complete", () => ok(voyagePret(scene, vers)));
    if (!l.isLoading()) l.start();
  });
}

// ---- what the ferryman says (Monde.parler): a word of his language, then the boat closed or the trip proposed; in
// the island's evening, good night and the way home
function lignesPasseur(scene, n){
  const p = P(), petit = p.age <= 5, ile = !!(scene && scene.ile), h = histoireDe(p);
  const fin = () => { if (scene && scene.passage) scene.passage.apresDialogue(); };
  let l = [], langue = null;
  if (n.soir) {
    const c = actif(), m = typeof motDuJeu === "function" ? motDuJeu("bonne_nuit", "lb") : null;
    l.push(`Le soleil se couche sur l'île. Bravo pour aujourd'hui, ${p.nom} !${c ? ` ${nomCompagnon(c)} bâille : il va bientôt dormir.` : ""}`);
    if (m) { langue = "lb"; l.push(petit && !sensADire(m) ? `« ${m.mot} » !` : `« ${m.mot} » : c'est « ${m.sens} », en luxembourgeois !`); }
  } else {
    const salut = typeof salutIle === "function" ? salutIle(n) : null;
    if (salut) { l.push(salut.ligne); langue = salut.langue; }
    if (!ile && !h.passeur) { h.passeur = new Date().toISOString(); sauver(); l.push("Je suis le passeur. Ma barque traverse l'eau jusqu'à l'Île des Bâtisseurs !"); }
  }
  if (!ile && !barqueOuverte(p)) {   // going home is always allowed: only the trip to the island waits for the rule
    const k = ombresAConsoler(p), ombres = `${k === 1 ? "une" : k} Ombre${k > 1 ? "s" : ""}`;
    l.push(petit ? `Encore ${ombres} à consoler, et je t'emmène sur l'île !`
      : `Encore ${ombres} à consoler, ou un badge à gagner chez un chef des Ombres, et je t'emmène sur l'île !`);
    return {lignes: l, langue, fin};
  }
  const vers = ile ? "village" : "ile";
  if (scene && scene.sys.isActive()) prechargerVoyage(scene, vers);   // ready by the time the child chooses
  l.push(ile ? "On rentre au village ?" : "On part pour l'Île des Bâtisseurs ?");
  const partir = () => voyager(vers);
  const choix = petit
    ? [{texte: "🛶", classe: "barque", aide: ile ? "Rentrer au village" : "Partir sur l'île", dit: "Touche la barque pour partir !", action: partir}]
    : [{texte: ile ? "🛶 Rentrer" : "🛶 Partir", dit: ile ? "Touche Rentrer pour monter dans la barque," : "Touche Partir pour monter dans la barque,", action: partir},
       {texte: "↩️ Pas maintenant", dit: "ou Pas maintenant pour rester ici.", action: () => {}}];
  return {lignes: l, choix, langue, fin};
}

// the screen closed in a star while the boat is made ready: the boat rocking, its words (said for the little one)
function attenteVoyage(){
  const d = el("div", "attente-voyage"), t = el("b");
  t.textContent = "La barque se prépare…";
  d.append(iconeDom("barque"), t);
  document.body.append(d);
  if (P() && P().age <= 5) dire("Attends un peu, la barque se prépare !");
  return () => d.remove();
}

// ---- the trip: the game goes to the other village, the crossing plays once all it shows is there
function voyager(vers){
  const m = JEU.scene.getScene("monde"), p = P();
  if (!m || !p || !m.sys.isActive() || VOYAGE.enCours || !LIEUX.includes(vers)) return false;
  if (vers !== "village" && !barqueOuverte(p)) return false;   // going home is always allowed
  const de = lieuDe(p);
  if (de === vers) return false;
  VOYAGE.enCours = true; VOYAGE.arrivee = vers; VOYAGE.fait = true;
  garderPosition(p, de, m.case);
  p.lieu = vers;
  garderPosition(p, vers, arriveeDe(vers === "ile" ? construireCarteIle() : construireCarte()));   // the hero lands by the boat
  if (typeof noterJournal === "function") noterJournal("voyage", {de, vers, ombres: ombresConsolees(p), badges: badgesDesChefs(p)});
  sauver();
  m.verrou = true; m.chemin = []; m.pnjVise = null;
  if (typeof jouerSon === "function") jouerSon("magie");
  const b = m.passage && m.passage.centre(), [x, y] = b ? m.ecranDe(b.x, b.y) : [innerWidth / 2, innerHeight / 2];
  Promise.resolve(typeof fermerEtoile === "function" ? fermerEtoile(x, y) : null).then(() => {
    fermerDialogue(); taire();
    // what the trip shows, by the village's loader (the crossing then finds it all there): a sign while it comes
    const finAttente = voyagePret(m, vers) && !m.load.isLoading() ? null : attenteVoyage();
    return Promise.race([prechargerVoyage(m, vers), new Promise(ok => setTimeout(ok, VOYAGE_ATTENTE_MAX))]).then(() => { if (finAttente) finAttente(); });
  }).then(() => {
    montrer("voyage");
    if (JEU.scene.isActive("monde") || JEU.scene.isSleeping("monde")) JEU.scene.stop("monde");
    JEU.scene.start("traversee", {de, vers});
    if (typeof ouvrirEtoile === "function") ouvrirEtoile();
  });
  return true;
}

// ---- the boat and its ferryman, in either village
class Passage {
  constructor(scene){
    this.s = scene; this.vise = false; this.sprite = null; this.pnjPasseur = null; this.depart = null;
    const c = scene.carte;
    this.ile = !!c.ile;
    this.zone = this.ile ? c.barque : c.elements.find(e => e.type === "barque") || null;
    this.pnj = this.ile ? scene.pnj.find(n => n.passeur) || null : null;
    this.devants = devantsBarque(c);
    // in the village, he rises from his boat beside it, on the side away from the hero
    this.places = this.zone && !this.ile ? [[this.zone.w, 0], [-1, 0], [this.zone.w, 1], [-1, 1]]
      .map(([a, b]) => ({x: this.zone.x + a, y: this.zone.y + b})).filter(q => !c.bloque[q.y][q.x]) : [];
    // once the village is settled and calm (never in the way of its first moments): the boat open, the first trip of
    // the sitting is loaded ahead (the crossing, the island); closed, only the ferryman's drawing
    if (!this.ile && this.zone) scene.time.delayedCall(TEST ? 0 : 4000, () => this.prevoir());
  }
  prevoir(){
    const s = this.s;
    if (!s.sys.isActive()) return;
    if (s.occupe() || s.enMarche || s.moment) { s.time.delayedCall(2000, () => this.prevoir()); return; }
    if (barqueOuverte(P()) && !VOYAGE.fait) prechargerVoyage(s, "ile");
    else chargerPuis(s, ["passeur"], () => {});
  }
  // the middle of the boat, in world units (where the star closes)
  centre(){ const z = this.zone; return z && {x: (z.x + z.w / 2) * CASE, y: (z.y + z.h / 2) * CASE}; }
  image(){ const r = this.s.villageHD; return r && (this.ile ? r.ileHD && r.ileHD.barque : r.barque); }
  pres(){ const c = this.s.case; return this.devants.some(d => d.x === c.x && d.y === c.y); }
  // the nearest free boarding tile (the ferryman may stand on one)
  devant(){
    const s = this.s, l = this.devants.filter(d => s.libre(d.x, d.y) || (d.x === s.case.x && d.y === s.case.y));
    return l.length ? l.reduce((a, b) => s.dist(b) < s.dist(a) ? b : a) : null;
  }
  // a touch on the boat (its drawing, a finger's width around it) or on the ferryman standing by it: the hero goes in
  // front of it, then the ferryman speaks
  touche(wx, wy){
    const s = this.s, z = this.zone, im = this.image();
    this.vise = false;
    if (!z) return false;
    const surBarque = im && im.active ? Phaser.Geom.Rectangle.Contains(Phaser.Geom.Rectangle.Inflate(im.getBounds(), 2, 2), wx, wy)
      : wx >= z.x * CASE && wx < (z.x + z.w) * CASE && wy >= z.y * CASE - 8 && wy < (z.y + z.h) * CASE;
    const surLui = !!this.sprite && this.sprite.active && dessine(this.sprite) && this.sprite.getBounds().contains(wx, wy);
    if (!surBarque && !surLui) return false;
    const d = this.devant();
    if (!d) return false;
    s.marquer(d.x, d.y);
    if (this.pres()) { this.parler(); return true; }
    s.allerVers(d.x, d.y);
    this.vise = true;
    return true;
  }
  // the hero has walked there (Monde.arrive)
  arrive(){
    if (!this.vise) return false;
    this.vise = false;
    if (this.pres()) this.parler();
    return true;
  }
  parler(){
    const s = this.s;
    s.chemin = []; s.pnjVise = null; s.arreter();
    if (this.depart) { this.depart.remove(); this.depart = null; }
    const n = this.ile ? this.pnj : this.appeler();
    if (!n) return;
    const c = s.case;   // the hero turns toward him
    reposHumain(s.heros, n.x > c.x ? "droite" : n.x < c.x ? "gauche" : n.y < c.y ? "haut" : "bas");
    if (n.s) s.vie.saluer(n.s);
    s.parler(n);
  }
  // the village's ferryman rises from his boat (already there: the same one)
  appeler(){
    const s = this.s;
    if (this.sprite && this.sprite.active) return this.pnjPasseur;
    const c = s.case, q = this.places.find(p => !(p.x === c.x && p.y === c.y) && s.libre(p.x, p.y)) || this.places[0];
    if (!q) return null;
    const sp = this.sprite = spriteHumain(s, "passeur", q.x, q.y, true).setDepth(profondeur(q.y));
    ombreAuSolHD(s, sp);
    sp.dirRepos = q.x > c.x ? "gauche" : q.x < c.x ? "droite" : "bas";
    sp.dir = sp.dirRepos; reposHumain(sp, sp.dirRepos);
    s.carte.bloque[q.y][q.x] = true;   // nobody walks through him while he stands there
    this.pnjPasseur = {x: q.x, y: q.y, cle: "passeur", nom: "Le passeur", passeur: true, s: sp, dit: []};
    if (!calme()) {
      const y = sp.y; sp.y += 7; sp.setAlpha(0);
      s.tweens.add({targets: sp, y, alpha: 1, duration: 420, ease: "Back.easeOut"});
      effetHD(s, "toucher", sp.x, sp.y - 10, CASE, sp.depth + 1);
    }
    return this.pnjPasseur;
  }
  // he goes back to rest in his boat (village)
  partir(){
    const sp = this.sprite, n = this.pnjPasseur, s = this.s;
    this.sprite = null; this.pnjPasseur = null;
    if (!sp) return;
    if (n) s.carte.bloque[n.y][n.x] = false;
    arreterSouffle(sp);
    if (calme() || !sp.active) return sp.destroy();
    s.tweens.add({targets: sp, y: sp.y + 7, alpha: 0, duration: 380, ease: "Sine.easeIn", onComplete: () => sp.destroy()});
  }
  // after his dialogue: in the village he lies down again a moment later, unless the trip began
  apresDialogue(){
    if (this.ile || !this.sprite) return;
    if (this.depart) this.depart.remove();
    this.depart = this.s.time.delayedCall(TEST ? 0 : 1800, () => {
      this.depart = null;
      if (!VOYAGE.enCours && !dialogueOuvert()) this.partir();
    });
  }
  // the village just reached: on the island he welcomes the child (« Wëllkomm »), in the village he says goodbye
  // (« Äddi ») and lies down in his boat again
  accueillir(){
    const s = this.s, ici = () => s.sys.isActive() && ecranCourant === "monde";
    reposHumain(s.heros, this.ile ? "droite" : "haut");
    if (this.ile) {
      const n = this.pnj, h = histoireDe(), premiere = !h.ile_arrivee;
      if (premiere) noterHistoire("ile_arrivee");
      if (n && n.s) s.vie.saluer(n.s, 1600);
      const m = typeof motDuJeu === "function" ? motDuJeu("bienvenue", "lb") : null;
      const texte = premiere ? "Voici l'Île des Bâtisseurs ! La cheffe de chantier t'attend au milieu de l'île." : "Te revoilà sur l'Île des Bâtisseurs !";
      // then, until the child has talked to her, the guide's hand shows the cheffe (js/jeu/ile_village.js, cibleGuide)
      const montrer = () => { const c = ici() && s.histoire.cibleGuide && s.histoire.cibleGuide(); if (c && s.guide && !s.guide.mainLa) s.guide.montrerMain(c); };
      return (m ? bulleMotVillage("passeur", "Le passeur", m) : Promise.resolve())
        .then(() => { if (ici()) return bulleVillage("passeur", "Le passeur", texte); }).then(montrer);
    }
    const n = this.appeler();
    if (n && n.s) s.vie.saluer(n.s, 1600);
    const m = typeof motDuJeu === "function" ? motDuJeu("au_revoir", "lb") : null;
    return (m ? bulleMotVillage("passeur", "Le passeur", m) : wait(1500)).then(() => { if (ici() && !dialogueOuvert()) this.partir(); });
  }
}
