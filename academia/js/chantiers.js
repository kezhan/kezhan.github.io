/* Académia : les chantiers de l'Île des Bâtisseurs, leurs règles et leur état (P().chantiers ; ticket
   construire-et-collectionner). Six bâtiments, un par terrain de l'île (CHANTIERS, js/jeu/carte_ile.js), chacun bâti en
   quatre étapes : fondations, murs, toit, décoration. Une étape coûte des étincelles (de plus en plus ; moins jusqu'à
   5 ans) et une bonne réponse à une question de la matière du bâtiment, à la hauteur de l'enfant (js/adaptatif.js).
   Les étincelles ne partent qu'après la bonne réponse : une erreur ne coûte rien, et une étape bâtie le reste.
   Un bâtiment fini a toujours quelque chose à apprendre à l'enfant (SAVOIRS_CHANTIER, une phrase vraie à chaque
   visite), son petit plaisir (js/jeu/chantiers_ile.js), et fabrique sa chose à lui, à venir récupérer : prête au bout
   de quelques heures de l'horloge (RECOLTES_CHANTIER ; la brioche de la boulangerie nourrit le compagnon, les autres
   donnent quelques étincelles), puis le temps repart. P().chantiers.cadeaux garde le moment de la dernière récolte
   de chaque bâtiment (« 2026-11-21T14:05:00 », heure de l'appareil ; une ancienne partie y a le jour de sa brioche).
   L'écran est dans js/chantier_vue.js, les bâtiments dessinés sur l'île dans js/jeu/chantiers_ile.js. */
const ETAPES_CHANTIER = [
  {id: "fondations", nom: "les fondations", verbe: "posé"},
  {id: "murs", nom: "les murs", verbe: "monté"},
  {id: "toit", nom: "le toit", verbe: "posé"},
  {id: "decoration", nom: "la décoration", verbe: "fini"}
];
const CHANTIER_FINI = ETAPES_CHANTIER.length;
const PRIX_CHANTIER = {petit: [3, 6, 9, 12], grand: [10, 20, 30, 40]};   // per step, in étincelles (petit: up to 5 years)
const XP_BRIOCHE = 12;
// what each finished building makes, to come back for: its drawing (assets/<qualité>/chantiers/<chose>.png), how long
// it takes (hours of the clock), what it gives (étincelles, or XP for the companion), how the cheffe says it
const RECOLTES_CHANTIER = {
  moulin: {chose: "farine", heures: 2, etincelles: 2, nom: "le sac de farine", dit: "Le moulin a moulu un sac de farine"},
  boulangerie: {chose: "brioche", heures: 3, xp: XP_BRIOCHE, nom: "la brioche", dit: "La boulangerie a cuit une brioche"},
  kiosque: {chose: "note", heures: 3, etincelles: 2, nom: "la note de musique", dit: "Le kiosque t'offre une note de musique"},
  serre: {chose: "tomate", heures: 4, etincelles: 3, nom: "la tomate", dit: "Une tomate a mûri dans la serre"},
  bibliotheque: {chose: "livre", heures: 5, etincelles: 4, nom: "le livre", dit: "Un nouveau livre est arrivé à la bibliothèque"},
  phare: {chose: "coquillage", heures: 6, etincelles: 5, nom: "le coquillage", dit: "La mer a posé un coquillage au pied du phare"}
};
const HEURE_MS = 3600000;
// each building: the subjects of its questions (each in turn), the notions of its own theme (asked two times out of
// three when the child has them: known, of the moment, or a discovery of their age), and the names of its own steps
const SUJETS_CHANTIER = {
  boulangerie: {matieres: ["maths"]},
  phare: {matieres: ["sciences"], notions: ["sc_jour_nuit", "sc_ocean", "sc_meteo", "sc_espace"], etapes: {toit: "la lanterne"}},
  moulin: {matieres: ["sciences"], notions: ["sc_meteo", "sc_eau", "sc_matiere", "sc_alimentation"]},
  bibliotheque: {matieres: ["anglais", "allemand", "luxembourgeois"]},
  serre: {matieres: ["sciences"], notions: ["sc_plantes", "sc_vivant", "sc_saisons", "sc_insectes"], etapes: {murs: "la charpente", toit: "le toit de verre"}},
  kiosque: {matieres: ["logique"], notions: ["lo_suites", "lo_suites_nombres", "lo_suites_longues"], etapes: {murs: "les colonnes"}}
};
// what a finished building teaches, one true sentence at each visit, each in turn
const SAVOIRS_CHANTIER = {
  boulangerie: ["Le pain est fait de farine, d'eau, de sel et de levure : c'est la levure qui le fait gonfler.",
    "Le boulanger se lève très tôt : le pain cuit pendant que tu dors encore.",
    "Les trous de la mie du pain sont des bulles d'air, faites par la levure."],
  phare: ["La lumière d'un phare se voit de très loin : la nuit, elle montre aux bateaux où est la côte.",
    "Chaque phare a son rythme d'éclats : les marins le reconnaissent de loin.",
    "Rien ne va plus vite que la lumière."],
  moulin: ["Le vent pousse les ailes du moulin. En tournant, elles font tourner une grosse pierre, la meule, qui écrase le blé en farine.",
    "Le vent, c'est de l'air qui bouge.",
    "Avec la farine du moulin, on fait le pain de la boulangerie."],
  bibliotheque: ["À la bibliothèque, on emprunte un livre, on le lit, puis on le rapporte pour que d'autres le lisent.",
    "Les livres sont rangés dans un ordre précis, pour retrouver vite celui qu'on cherche.",
    "Au Luxembourg, on parle luxembourgeois, français et allemand : trois langues dans un seul pays."],
  serre: ["Le verre de la serre laisse entrer le soleil et garde sa chaleur : les plantes y poussent même en hiver.",
    "Une plante a besoin d'eau, de lumière et d'air pour grandir.",
    "Les abeilles portent le pollen d'une fleur à l'autre : c'est grâce à elles que viennent les fruits."],
  kiosque: ["Dans un kiosque à musique, les musiciens jouent dehors, pour tout le monde.",
    "Le rythme, c'est ce qui se répète dans la musique : fort, doux, doux, fort, doux, doux.",
    "Un son aigu vibre vite, un son grave vibre lentement."]
};
// what the cheffe promises when a building is finished
const PROMESSES_CHANTIER = {
  boulangerie: "Elle cuira des brioches pour ton compagnon. Reviens souvent les chercher sur l'île !",
  phare: "Sa lampe s'allumera chaque soir, et la mer y posera des coquillages. Reviens souvent les chercher !",
  moulin: "Ses ailes tournent dans le vent, et il moudra de la farine. Reviens souvent la chercher !",
  bibliotheque: "Elle a toujours quelque chose à t'apprendre, et de nouveaux livres y arriveront. Reviens souvent les chercher !",
  serre: "Les plantes y poussent au chaud : des tomates y mûriront. Reviens souvent les chercher !",
  kiosque: "Touche-le sur l'île pour entendre sa musique : il t'offrira des notes. Reviens souvent les chercher !"
};

// the chantiers as they should be, whatever the save holds (an old game has none; js/etat.js, reparer)
function reparerChantiers(c){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v), ok = objet(c);
  const date = v => typeof v === "string" && /^\d{4}-\d\d-\d\d$/.test(v) ? v : null;
  const moment = v => typeof v === "string" && /^\d{4}-\d\d-\d\d(T\d\d:\d\d:\d\d)?$/.test(v) ? v : null;   // a harvest: its day (old games), or day and hour
  const lus = ok && objet(c.batiments) ? c.batiments : {}, batiments = {}, cadeaux = {}, savoirs = {};
  Object.keys(SUJETS_CHANTIER).forEach(id => {
    const b = objet(lus[id]) ? lus[id] : {}, etape = Math.max(0, Math.min(CHANTIER_FINI, Math.floor(Number(b.etape) || 0)));
    batiments[id] = {etape, fini: etape >= CHANTIER_FINI ? date(b.fini) || jour() : null};
    if (ok && objet(c.cadeaux) && moment(c.cadeaux[id])) cadeaux[id] = c.cadeaux[id];
    if (ok && objet(c.savoirs) && c.savoirs[id] > 0) savoirs[id] = Math.floor(c.savoirs[id]);
  });
  return {batiments, cadeaux, savoirs, tour: Math.max(0, Math.floor(ok && Number(c.tour) || 0))};
}
function chantiersDe(p = P()){
  if (!p) return null;
  const c = p.chantiers;
  if (!c || typeof c.batiments !== "object" || !c.batiments || Object.keys(SUJETS_CHANTIER).some(id => !c.batiments[id])) p.chantiers = reparerChantiers(c);
  return p.chantiers;
}
const etapeChantier = (id, p = P()) => { const c = chantiersDe(p), b = c && c.batiments[id]; return b ? b.etape : 0; };
const chantierFini = (id, p = P()) => etapeChantier(id, p) >= CHANTIER_FINI;
const batimentsFinis = (p = P()) => Object.keys(SUJETS_CHANTIER).filter(id => chantierFini(id, p));
// the next step of a building (null when it is finished): its name (the building's own when it has one), its price
function etapeSuivante(id, p = P()){
  const k = etapeChantier(id, p), e = ETAPES_CHANTIER[k], s = SUJETS_CHANTIER[id];
  if (!e || !s || !p) return null;
  return {...e, k: k + 1, nom: (s.etapes && s.etapes[e.id]) || e.nom, prix: PRIX_CHANTIER[p.age <= 5 ? "petit" : "grand"][k]};
}
// « de la boulangerie », « du phare »
const duChantier = id => CHANTIERS[id].nom.replace(/^le /, "du ").replace(/^la /, "de la ");

// the question of a step: the building's subject (each in turn when it has several), its own notions two times out of
// three when the child has them, else what the engine chooses for this child
function questionChantier(id){
  const p = P(), c = chantiersDe(p), s = SUJETS_CHANTIER[id];
  if (!s) return null;
  const l = s.matieres.filter(m => notionsDe(m).length);
  if (!l.length) return null;
  const tour = c.tour++, m = l[tour % l.length], age = ageDuJour(p), courante = toutSu(m) ? null : notionCourante(m);
  const siennes = (s.notions || []).map(notionDe).filter(n => n && n.matiere === m && (maitrisee(n) || n === courante || (n.decouverte && ageMin(n) <= age)));
  if (siennes.length && tour % 3 !== 2) {
    const n = pick(siennes, 1)[0], q = unique(m, () => questionPour(n, "rapide"));
    if (q) return q;
  }
  return tirer(m, "rapide");
}

// a step built after a right answer: paid now, kept for good. null when the child cannot pay or all is built
function construire(id){
  const p = P(), c = chantiersDe(p), e = etapeSuivante(id, p);
  if (!e || p.etincelles < e.prix) return null;
  const b = c.batiments[id];
  p.etincelles -= e.prix;
  b.etape = e.k;
  if (b.etape >= CHANTIER_FINI) b.fini = jour();
  if (typeof noterJournal === "function") noterJournal("chantier", {b: id, e: b.etape, prix: e.prix});
  sauver();
  return {...e, fini: b.etape >= CHANTIER_FINI};
}
// what a finished building teaches at this visit (the next sentence each time)
function savoirChantier(id){
  const c = chantiersDe(), l = SAVOIRS_CHANTIER[id] || [];
  if (!l.length) return "";
  const k = c.savoirs[id] || 0;
  c.savoirs[id] = k + 1; sauver();
  return l[k % l.length];
}
// ---- the harvests: what a finished building has made since the last visit
const maintenantRecolte = () => typeof maintenantJeu === "function" ? maintenantJeu() : Date.now();
const momentRecolte = (t = maintenantRecolte()) => { const d = new Date(t), z = n => String(n).padStart(2, "0"); return `${jour(t)}T${z(d.getHours())}:${z(d.getMinutes())}:${z(d.getSeconds())}`; };
// the one rule of what takes time to be ready: `heures` after `moment` (a momentRecolte(), or nothing: ready at once),
// how long is left in milliseconds. The buildings below and the companions' places (js/lieux.js) both ask it
const attenteDepuis = (moment, heures) => Math.max(0, ((moment && new Date(moment).getTime()) || 0) + heures * HEURE_MS - maintenantRecolte());
// how long before the building's thing is ready, in milliseconds (0: ready ; null: not finished, or nothing to give)
function attenteRecolte(id, p = P()){
  const r = RECOLTES_CHANTIER[id], c = chantiersDe(p);
  if (!r || !c || !chantierFini(id, p)) return null;
  return attenteDepuis(c.cadeaux[id], r.heures);
}
const recoltePrete = (id, p = P()) => attenteRecolte(id, p) === 0;
const recoltesPretes = (p = P()) => p && typeof p.chantiers === "object" ? Object.keys(RECOLTES_CHANTIER).filter(id => recoltePrete(id, p)) : [];
// the thing is taken, the time starts again: {chose, nom, etincelles} or, for the brioche, {xp, compagnon, ...gain}.
// null when nothing is ready (the brioche needs a companion to eat it)
function recolterChantier(id){
  const p = P(), c = chantiersDe(p), r = RECOLTES_CHANTIER[id], a = actif();
  if (!recoltePrete(id, p) || (r.xp && !a)) return null;
  c.cadeaux[id] = momentRecolte();
  let fait = {id, chose: r.chose, nom: r.nom, dit: r.dit};
  if (r.xp) { const xp = r.xp * rattrapage(a); fait = {...fait, ...gagnerXP(a, xp), xp, compagnon: a}; }
  else { p.etincelles += r.etincelles; fait.etincelles = r.etincelles; }
  if (typeof noterJournal === "function") noterJournal("recolte", {b: id});
  sauver();
  return fait;
}
