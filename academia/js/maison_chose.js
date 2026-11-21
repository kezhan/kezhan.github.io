/* Académia : une chose touchée dans une maison (js/jeu/carte_maison.js, MAISONS ; js/jeu/interieur_maison.js). L'écran
   la montre de près ; elle se nomme et dit une chose vraie à voix haute (dans une maison de langue, d'abord son nom
   dans la langue de la maison, par son enregistrement, puis ce qu'il veut dire), et le gros bouton propose un exercice
   de la matière de la maison, au niveau de l'enfant (les notions de la chose deux fois sur trois quand l'enfant les a,
   comme sur les chantiers de l'île, sinon ce que le moteur choisit pour lui).
   Les règles, gardées dans P().maisons :
   1. vues : "région:chose" -> le nombre de fois où l'enfant l'a touchée (la chose dit ses phrases tour à tour) ;
   2. recompenses : "région:chose" -> le jour de sa dernière étincelle. Une bonne réponse donne une étincelle, une
      seule par chose et par jour ; ensuite la chose dit de revenir demain, et l'exercice reste ouvert pour apprendre.
   Pour la petite (5 ans et moins) : tout est dit, le bouton est une image (l'étincelle), rien n'est à lire.
   La lunette de l'Observatoire n'a pas cet écran : elle ouvre la coupole (js/coupole.js). */
if (!ECRANS.includes("chose")) ECRANS.push("chose");   // one more screen for montrer() (js/outils.js)
const CHOSE = {c: null, occupe: false, tour: 0};
const srcChose = c => `${DOSSIER_HD}/interieurs/chose_${c.region}_${c.id}.png`;
const jourMaison = () => jour(typeof maintenantJeu === "function" ? maintenantJeu() : Date.now());

function maisonsDe(p = P()){
  if (!p.maisons || typeof p.maisons !== "object" || Array.isArray(p.maisons)) p.maisons = {};
  const m = p.maisons, objet = v => !!v && typeof v === "object" && !Array.isArray(v);
  if (!objet(m.vues)) m.vues = {};
  if (!objet(m.recompenses)) m.recompenses = {};
  return m;
}
// a damaged game: only what is well formed is kept (js/etat.js, reparer)
function reparerMaisons(lu){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v), m = {vues: {}, recompenses: {}};
  if (!objet(lu)) return m;
  Object.entries(objet(lu.vues) ? lu.vues : {}).forEach(([k, v]) => { if (Number(v) > 0) m.vues[k] = Math.floor(Number(v)); });
  Object.entries(objet(lu.recompenses) ? lu.recompenses : {}).forEach(([k, v]) => { if (typeof v === "string" && /^\d{4}-\d\d-\d\d$/.test(v)) m.recompenses[k] = v; });
  return m;
}
const cleChose = c => `${c.region}:${c.id}`;
// its étincelle is still to win today
const etincelleChose = (c, p = P()) => !c.lunette && maisonsDe(p).recompenses[cleChose(c)] !== jourMaison();
// its name in the house's language, when it has its recording: {mot, sens, langue, nom}
function motChose(c){
  const langue = LANGUE_MAISON[c.region];
  return langue && c.mot && typeof motEnregistre === "function" && motEnregistre(c.mot, langue) ? {mot: c.mot, sens: c.sens, langue, nom: NOMS_LANGUES[langue]} : null;
}
// a line of the thing, written in its bubble and said by the voice (the « word » of the house by its recording)
function annoncerChose(texte, langue){
  ecrireIcones($("bulleChose"), affiche(texte));
  return langue ? direMixte(texte, {langue}) : dire(texte);
}

function ouvrirChose(c){
  const p = P(); if (!p || !c) return;
  if (c.lunette && typeof ouvrirCoupole === "function") { voirChose(c); return ouvrirCoupole(); }
  CHOSE.c = c;
  montrer("chose");
  $("chose").dataset.region = c.region;
  $("choseTitre").textContent = majuscule(c.nom);
  $("choseEtincelles").textContent = p.etincelles;
  const h = MAISONS[c.region].habitant;
  imageReessayee($("chosePortrait"), `${DOSSIER_HD}/portraits/${h ? h.cle : "sage"}.png`);
  const i = document.createElement("img");
  i.alt = ""; i.draggable = false; imageReessayee(i, srcChose(c));
  $("choseDessin").replaceChildren(i);
  $("panneauChose").classList.add("cache");
  const n = voirChose(c);
  dessinerBoutonChose();
  if (!calme()) i.animate([{transform: "scale(.8)", opacity: .3}, {transform: "scale(1.05)", opacity: 1, offset: .7}, {transform: "scale(1)"}], {duration: 420, easing: "ease-out"});
  if (typeof jouerSon === "function") jouerSon("scintille");
  // its name (in the house's language first), a true thing, then what to do
  const petit = p.age <= 5, dits = (petit && c.petit) || c.dits, dit = dits[(n - 1) % dits.length], m = motChose(c);
  const nom = m ? `« ${m.mot} » : c'est « ${m.sens} », en ${m.nom} ! ` : "";
  const suite = !etincelleChose(c) ? " Reviens demain : j'aurai une nouvelle étincelle pour toi !"
    : petit ? " Touche le gros bouton pour jouer !" : " Un exercice, pour gagner une étincelle ?";
  return annoncerChose(nom + dit + suite, m && m.langue);
}
// one more look at it: how many times it has been touched
function voirChose(c){
  const v = maisonsDe().vues, k = cleChose(c);
  v[k] = (v[k] || 0) + 1; sauver();
  if (typeof noterJournal === "function") noterJournal("chose", {m: c.region, c: c.id, n: v[k]});
  return v[k];
}
function fermerChose(){
  if (CHOSE.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  if (typeof majBarre === "function") majBarre();
  const m = JEU.scene.getScene("monde");
  if (m && m.maison && m.maison.maj) m.maison.maj();   // its étincelle won: the room no longer shows it
}
// the big button: the étincelle to win (or already won today, pale); the little one has the picture only
function dessinerBoutonChose(){
  const b = $("btnExerciceChose"), c = CHOSE.c, reste = etincelleChose(c), petit = P().age <= 5;
  b.className = "gros vert" + (reste ? "" : " attente") + (petit ? " image" : "");
  b.dataset.etat = reste ? "pret" : "gagne";
  b.replaceChildren(iconeDom("etincelle"), ...(petit ? [] : [" Exercice"]));
  b.setAttribute("aria-label", "Exercice");
}

// the question of a thing: the house's subject, the thing's own notions two times out of three when the child has
// them (mastered, being learnt, or a discovery of its age), else what the engine chooses for this child
function questionChose(c){
  const p = P(), m = regionDe(c.region).matiere, age = ageDuJour(p), courante = toutSu(m) ? null : notionCourante(m);
  if (!notionsDe(m).length) return null;
  const siennes = (c.notions || []).map(notionDe).filter(n => n && n.matiere === m && (maitrisee(n) || n === courante || (n.decouverte && ageMin(n) <= age)));
  if (siennes.length && CHOSE.tour++ % 3 !== 2) {
    const n = pick(siennes, 1)[0], q = unique(m, () => questionPour(n, "rapide"));
    if (q) return q;
  }
  return tirer(m, "rapide");
}
// a right answer: one étincelle, once a day for this thing; null when today's is already won
function recompenserChose(c){
  const p = P(), m = maisonsDe(p);
  if (!etincelleChose(c, p)) return null;
  m.recompenses[cleChose(c)] = jourMaison();
  p.etincelles += 1;
  if (typeof noterJournal === "function") noterJournal("chose_etincelle", {m: c.region, c: c.id});
  sauver();
  return {etincelles: 1};
}
async function exerciceChose(){
  if (CHOSE.occupe || !CHOSE.c) return;
  sfx.tap();
  const c = CHOSE.c, q = questionChose(c);
  if (!q) return annoncerChose("Pas de question pour l'instant : reviens dans un moment !");
  CHOSE.occupe = true;
  const panneau = $("panneauChose"), ecran = $("chose");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: majuscule(c.nom), jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerChose("Presque ! Rien n'est perdu : touche le bouton pour une autre question.");
    const gain = recompenserChose(c);
    $("choseEtincelles").textContent = P().etincelles;
    dessinerBoutonChose();
    if (gain) {
      sfx.niveau();
      const d = $("choseDessin").getBoundingClientRect();
      if (!calme()) jaillir(d.left + d.width / 2, d.top + d.height / 2, {pieces: ["etincelle_p", "etoile_p"], n: 12});
    }
    // after the foreign word found, said again (js/question_vue.js), never over it
    apresRedit(() => $("chose").hidden || CHOSE.c !== c ? null
      : annoncerChose(gain ? "Bravo ! Une étincelle pour toi. Reviens demain : j'en aurai une autre !"
        : "Bravo ! Tu as déjà gagné mon étincelle aujourd'hui : reviens demain pour la prochaine !"));
  } finally {
    CHOSE.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
