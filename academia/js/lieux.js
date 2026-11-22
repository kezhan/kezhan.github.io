/* Académia : les lieux réservés aux compagnons, sur l'Île des Bâtisseurs (Kezhan, 21/11/2026 : « créer aussi des
   endroits uniquement à certains pokemon avec certains niveaux, pour obliger les joueurs à développer certains
   pokemons »). Ici les règles et l'état ; l'écran : js/lieu_vue.js ; l'île : js/jeu/lieux_ile.js (leurs cases :
   js/jeu/carte_ile.js) ; les dessins : outils/village/scripts/ile_lieux.py.
   1. Un lieu par famille de compagnons, et le belvédère pour toute l'équipe. Chaque lieu a trois paliers (niveaux 8,
      15 et 25 ; 6, 10 et 15 jusqu'à 5 ans). Un palier s'ouvre quand le compagnon de la bonne famille, au bon niveau,
      marche avec le héros (le belvédère : les six compagnons libérés, tous au niveau du palier).
   2. Lieu fermé : etatLieu() dit pourquoi (compagnon à libérer, trop petit, ou resté dans le sac) et la sentinelle
      dit quoi faire : le compagnon grandit deux fois plus vite dans sa maison (js/progression.js, bonusMaison).
   3. Palier ouvert : une chose vraie à apprendre, puis des exercices de la matière du compagnon, plus durs que
      d'habitude (les revanches puis les défis du chef : tirer(matière, "massive", true)). Deux bonnes réponses : la
      trouvaille du palier est gagnée pour toujours (l'album du sac la montre), avec des étincelles.
   4. Dès la première trouvaille, le lieu fabrique quelque chose à revenir chercher : la règle des récoltes des
      chantiers (js/chantiers.js : attenteDepuis, momentRecolte, l'horloge maintenantJeu de js/fetes.js). La récolte se
      ramasse quel que soit le compagnon qui suit : la trouvaille est à l'enfant.
   L'état, dans P().reserves : lieu -> {palier (trouvailles gagnées, 0 à 3), bonnes (réponses justes vers la prochaine),
   tour (questions posées), vus (visites), recolte (le moment de la dernière : gagnée ou ramassée)}. */
const PALIERS_LIEU = {grand: [8, 15, 25], petit: [6, 10, 15]};
const BONNES_LIEU = 2;                   // right answers to win a tier's find
const ETINCELLES_LIEU = [5, 8, 12];      // what each tier's find brings
const HEURES_LIEU = 4, ETINCELLES_RECOLTE_LIEU = 2;   // every four hours: two étincelles per find already won
const LIEUX_RESERVES = {
  carriere: {nom: "la Carrière des Cristaux", famille: "matty", choses: ["le cristal bleu", "le cristal violet", "le cristal doré"], pousse: "Un cristal a poussé dans la carrière",
    dits: ["Un grain de sel est un tout petit cube : regarde-le à la loupe !", "Un flocon de neige a toujours six branches.",
      "Le cristal de quartz pousse en colonnes à six côtés : c'est un hexagone, comme les alvéoles des abeilles."]},
  falaise: {nom: "la Falaise aux Plumes", famille: "plumix", choses: ["la plume blanche", "la plume bleue", "la plume dorée"], pousse: "Le vent a posé une plume sur la falaise",
    mots: ["vent", "soleil", "arc_en_ciel"],
    dits: ["Les oiseaux ont des os creux, très légers : c'est ce qui les aide à voler.", "L'eau glisse sur les plumes : un canard sort de la mare tout sec.",
      "On parle anglais dans beaucoup de pays : en Angleterre, en Irlande, aux États-Unis, en Australie."]},
  grotte: {nom: "la Grotte aux Échos", famille: "barli", choses: ["le fossile gris", "le fossile ambré", "le fossile doré"], pousse: "La grotte a rendu un fossile",
    mots: ["ours", "bonne_nuit", "soleil"],
    dits: ["Dans une grotte, ta voix rebondit sur les murs et revient : c'est l'écho.", "Les chauves-souris dorment la tête en bas, et se dirigent avec leurs oreilles.",
      "En allemand, tous les noms s'écrivent avec une majuscule."]},
  source: {nom: "la Source Dorée", famille: "leiwchen", choses: ["la perle blanche", "la perle rose", "la perle dorée"], pousse: "La source a roulé une perle",
    mots: ["bonjour", "lion", "soleil"],
    dits: ["L'eau d'une source vient de la pluie, qui a voyagé sous la terre.", "Au Luxembourg, on parle trois langues : le luxembourgeois, le français et l'allemand.",
      "La Moselle, la Sûre et l'Alzette sont des rivières du Luxembourg."]},
  damier: {nom: "la Clairière du Damier", famille: "cavalin", choses: ["le pion blanc", "le pion noir", "le pion doré"], pousse: "Un pion est apparu sur le damier",
    dits: ["Un échiquier a 64 cases : 8 rangées de 8.", "Aux échecs, le cavalier est la seule pièce qui saute par-dessus les autres.",
      "Aux échecs, un pion qui arrive tout au bout du plateau peut devenir une dame."]},
  bosquet: {nom: "le Bosquet Sauvage", famille: "poussik", choses: ["la feuille verte", "la feuille rouge", "la feuille dorée"], pousse: "Une feuille est tombée dans le bosquet",
    dits: ["Les plantes boivent par leurs racines et fabriquent leur nourriture avec la lumière du soleil.", "Les champignons ne sont pas des plantes : ils n'ont ni feuilles ni graines.",
      "On peut lire l'âge d'un arbre coupé : un cercle dans le tronc par année."]},
  belvedere: {nom: "le Belvédère des Six", equipe: true, choses: ["l'étoile de bronze", "l'étoile d'argent", "l'étoile d'or"], pousse: "Une étoile est tombée sur le belvédère",
    dits: ["Du haut d'un belvédère on voit plus loin : plus on monte, plus l'horizon recule.", "La Terre est ronde : un bateau qui s'éloigne disparaît par le bas en premier.",
      "Les oies sauvages volent en V : chacune aide celle qui la suit, et elles se relaient en tête."]}
};
const lieuNeuf = () => ({palier: 0, bonnes: 0, tour: 0, vus: 0, recolte: null});
// the places as they should be, whatever the save holds (an old game has none; js/etat.js, reparer)
function reparerReserves(lu){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v), l = {}, entier = (v, max) => Math.max(0, Math.min(max, Math.floor(Number(v)) || 0));
  Object.keys(LIEUX_RESERVES).forEach(id => {
    const e = objet(lu) && objet(lu[id]) ? lu[id] : {}, n = lieuNeuf();
    n.palier = entier(e.palier, 3); n.bonnes = n.palier >= 3 ? 0 : entier(e.bonnes, BONNES_LIEU - 1); n.tour = entier(e.tour, 1e6); n.vus = entier(e.vus, 1e6);
    if (typeof e.recolte === "string" && !isNaN(new Date(e.recolte).getTime()) && n.palier) n.recolte = e.recolte;
    l[id] = n;
  });
  return l;
}
function reservesDe(p = P()){
  if (!p.reserves || typeof p.reserves !== "object" || Object.keys(LIEUX_RESERVES).some(id => !p.reserves[id])) p.reserves = reparerReserves(p.reserves);
  return p.reserves;
}
const paliersLieu = (p = P()) => PALIERS_LIEU[p.age <= 5 ? "petit" : "grand"];
const famillesLieu = id => LIEUX_RESERVES[id].equipe ? Object.keys(FAMILLES) : [LIEUX_RESERVES[id].famille];
const trouvailleLieu = (id, k) => `tr_${id}_${k}`;   // the drawing of the tier's find (assets/<quality>/lieux)

// where the place stands for this child: {palier (finds won), fini, niveau (asked by the next tier), ouvert}, and when
// it is closed: raison ("absent": to free ; "bas": too low ; "sac": the right one, but not walking with the hero),
// famille, compagnon
function etatLieu(id, p = P()){
  const e = reservesDe(p)[id], k = e.palier, fini = k >= 3, niveau = paliersLieu(p)[Math.min(k, 2)], base = {id, palier: k, fini, niveau, bonnes: e.bonnes};
  if (fini) return {...base, ouvert: true};
  const equipe = famillesLieu(id).map(f => ({f, c: p.compagnons.find(x => x.famille === f) || null}));
  const absent = equipe.find(x => !x.c);
  if (absent) return {...base, ouvert: false, raison: "absent", famille: absent.f, compagnon: null};
  const bas = equipe.filter(x => x.c.niveau < niveau).sort((a, b) => a.c.niveau - b.c.niveau)[0];
  if (bas) return {...base, ouvert: false, raison: "bas", famille: bas.f, compagnon: bas.c};
  if (!LIEUX_RESERVES[id].equipe && equipe[0].c.id !== p.actif) return {...base, ouvert: false, raison: "sac", famille: equipe[0].f, compagnon: equipe[0].c};
  return {...base, ouvert: true};
}
// one more visit; the place says its sentences in turn
function voirLieu(id){
  const e = reservesDe()[id];
  e.vus++; sauver();
  if (typeof noterJournal === "function") noterJournal("lieu", {l: id, p: e.palier, n: e.vus});
  return e.vus;
}
// the word of a language place for its tier, when it has its recording (js/mots_du_jeu.js): {mot, sens, langue, nom}
function motLieu(id, k){
  const L = LIEUX_RESERVES[id], langue = L.famille && {anglais: "en", allemand: "de", luxembourgeois: "lb"}[FAMILLES[L.famille].matiere];
  return langue && L.mots && typeof motDuJeu === "function" ? motDuJeu(L.mots[Math.min(k, 2)], langue) : null;
}
// a question of the place: its companion's subject (each subject in turn at the belvedere), harder than usual: first
// what the child missed, then the chief's challenges, at the top of the child's level (js/adaptatif.js)
function questionLieu(id){
  const e = reservesDe()[id], l = famillesLieu(id).map(f => FAMILLES[f].matiere).filter(m => notionsDe(m).length);
  if (!l.length) return null;
  for (let k = 0; k < l.length; k++) { const q = tirer(l[(e.tour + k) % l.length], "massive", true); if (q) { e.tour += k + 1; return q; } }
  return null;
}
// a right answer in an open place: counted; the second one wins the tier's find, its étincelles, and starts the
// place's clock. {bonnes, reste} or {trouvaille: k, chose, etincelles, fini}; null when the place is closed or finished
function reussirLieu(id){
  const p = P(), e = reservesDe(p)[id], s = etatLieu(id, p);
  if (!s.ouvert || s.fini) return null;
  e.bonnes++;
  if (e.bonnes < BONNES_LIEU) { sauver(); return {bonnes: e.bonnes, reste: BONNES_LIEU - e.bonnes}; }
  e.bonnes = 0; e.palier++;
  const etincelles = ETINCELLES_LIEU[e.palier - 1];
  p.etincelles += etincelles;
  e.recolte = momentRecolte();
  if (typeof noterJournal === "function") noterJournal("trouvaille", {l: id, p: e.palier});
  sauver();
  return {trouvaille: e.palier, chose: LIEUX_RESERVES[id].choses[e.palier - 1], etincelles, fini: e.palier >= 3};
}
// ---- what a place makes, to come back for: the buildings' own rule (js/chantiers.js)
// how long before it is ready, in milliseconds (0: ready ; null: no find won there yet)
function attenteLieu(id, p = P()){
  const e = reservesDe(p)[id];
  return e.palier ? attenteDepuis(e.recolte, HEURES_LIEU) : null;
}
const recolteLieuPrete = (id, p = P()) => attenteLieu(id, p) === 0;
const choseRecolteLieu = (id, p = P()) => trouvailleLieu(id, reservesDe(p)[id].palier);
// the places whose thing is ready, as the ferryman announces them (js/jeu/recoltes_ile.js): {id, chose, nom, dit}
const recoltesLieux = (p = P()) => p && Array.isArray(p.compagnons) ? Object.keys(LIEUX_RESERVES).filter(id => recolteLieuPrete(id, p))
  .map(id => ({id, chose: choseRecolteLieu(id, p), nom: LIEUX_RESERVES[id].choses[reservesDe(p)[id].palier - 1], dit: LIEUX_RESERVES[id].pousse, lieu: true})) : [];
// taken: the étincelles, and the clock starts again. null when nothing is ready
function recolterLieu(id){
  const p = P(), e = reservesDe(p)[id];
  if (!recolteLieuPrete(id, p)) return null;
  const etincelles = ETINCELLES_RECOLTE_LIEU * e.palier, chose = choseRecolteLieu(id, p);
  e.recolte = momentRecolte();
  p.etincelles += etincelles;
  if (typeof noterJournal === "function") noterJournal("recolte", {l: id});
  sauver();
  return {id, chose, etincelles, dit: LIEUX_RESERVES[id].pousse};
}
// the finds for the bag's album: every place's three, won or still to find
const trouvaillesAlbum = (p = P()) => Object.keys(LIEUX_RESERVES).flatMap(id => [1, 2, 3].map(k => ({id, k, gagnee: reservesDe(p)[id].palier >= k})));
