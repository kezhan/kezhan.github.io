/* Académia : de petites conversations avec Hugo et Paco (Kezhan, 13/10/2026 : « revoir tout et donner des
   opportunités pour apprendre » ; lot C du ticket apprendre-partout, conception/audit_apprendre.md, proposition 19).
   Une visite sur deux de chaque habitant, à partir de sa deuxième de la séance, il pose une question dans sa langue au
   lieu de son mot de bienvenue, et l'enfant répond par un bouton qui parle. Chacun a ses tours : sa première
   conversation demande comment ça va, la suivante l'âge (quand il est enregistré), et ainsi de suite.
   1. « How are you? » (Hugo), « Wie geht's? » (Paco) : trois réponses, un visage qui sourit (« I'm fine! »), qui
      bâille (« I'm tired… »), qui a faim (« I'm hungry! »). L'habitant répond à son tour : « Have a nice day! »,
      « Good night! », « Enjoy your meal! » (en allemand : « Einen schönen Tag! », « Gute Nacht! », « Guten Appetit! »).
   2. « How old are you? », « Wie alt bist du? » : trois gâteaux, l'âge de l'enfant et ses voisins ; le bon dit « I am
      seven! », et l'habitant le félicite (« Well done! ») ; un autre, et il redit doucement la bonne phrase. Posée
      seulement à un enfant dont l'âge est enregistré (de 5 à 10 ans) : la petite de 4 ans n'a que la première.
   Les boutons viennent quand la question a été dite (4 s au plus). Le grand lit la phrase sur le bouton ; la petite
   touche l'image, la voix dit la phrase pour elle. Le sens suit la règle du jeu (sensADire : la petite, les premières
   fois, jamais deux fois dans la minute). La réponse de l'enfant passe dans la bulle avec le visage du héros, celle de
   l'habitant ensuite, chacune par son enregistrement (js/mots_du_jeu.js), puis l'habitant dit la plus utile de ses
   lignes habituelles (le niveau du compagnon pour Hugo, qui reste à libérer pour Paco). Une phrase sans
   enregistrement, et la visite reste comme avant. Un toucher sur le village ferme la question sans répondre. La
   visite compte pour la mission « Dis bonjour » ; la réponse choisie va dans le journal de jeu. */
const CONVERSATIONS = {
  comment: {question: "comment_vas_tu", reponses: [
    {cle: "je_vais_bien", image: "😊", reponse: "bonne_journee"},
    {cle: "fatigue", image: "😴", reponse: "bonne_nuit"},
    {cle: "faim", image: "😋", reponse: "bon_appetit"}]},
  age: {question: "quel_age"}   // the answers: age_5 … age_10, three cakes around the child's age
};
const AGES_DITS = [5, 6, 7, 8, 9, 10];   // MOTS_DU_JEU age_<n>
const TOURS_CONVERSATION = {visites: {}, sujets: {}};   // in this sitting, for each villager: their visits, their next subject

// the conversation of a visit (HistoireVillage.lignes): {lignes, choix, langue, choixApresVoix} for the dialogue, or
// null (a plain visit)
function conversation(n, scene){
  const L = LANGUES_HABITANTS[n.cle], p = P();
  if (!L || !p || !["hugo", "paco"].includes(n.cle) || typeof motDuJeu !== "function") return null;
  if (MOT_EN_ATTENTE[n.cle]) return null;   // a word kept for this visit (« Gesundheit! ») comes first
  const v = TOURS_CONVERSATION.visites[n.cle] = (TOURS_CONVERSATION.visites[n.cle] || 0) + 1;
  if (v % 2) return null;
  const sujets = ["comment", ...(AGES_DITS.includes(p.age) ? ["age"] : [])], tour = TOURS_CONVERSATION.sujets[n.cle] || 0;
  for (let k = 0; k < sujets.length; k++) {
    const sujet = sujets[(tour + k) % sujets.length], c = sujet === "age" ? questionAge(n, L, scene) : questionComment(n, L, scene);
    if (!c) continue;
    TOURS_CONVERSATION.sujets[n.cle] = (tour + k + 1) % sujets.length;
    if (typeof noterHistoire === "function") noterHistoire("visite_" + n.cle);   // the day's visit (the « Dis bonjour » mission)
    return {...c, choixApresVoix: true};
  }
  return null;
}
const petiteOreille = () => !!P() && P().age <= 5;
// a button that speaks: its picture (alone for the little one), the sentence written for the big one
function boutonParle(image, m, plus){
  const f = document.createDocumentFragment(), i = el("span", "image-conv");
  i.textContent = image; if (plus != null) { const n = el("i"); n.textContent = plus; i.append(n); }
  f.append(i);
  if (!petiteOreille()) { const b = el("b", "mot-etranger"); b.lang = m.langue; b.textContent = m.mot; f.append(b); }
  return f;
}
// the question's line: the question by its recording, its meaning by the game's rule, what to touch
const ligneQuestion = (q, L, consigne) => `« ${q.mot} »${sensADire(q) ? ` : c'est « ${q.sens} », en ${L.nom}.` : ""} ${consigne}`;
function questionComment(n, L, scene){
  const q = motDuJeu(CONVERSATIONS.comment.question, L.langue);
  const reps = CONVERSATIONS.comment.reponses.map(r => ({...r, m: motDuJeu(r.cle, L.langue), apres: motDuJeu(r.reponse, L.langue)})).filter(r => r.m);
  if (!q || reps.length < 2) return null;
  return {langue: L.langue, lignes: [ligneQuestion(q, L, petiteOreille() ? "Touche un visage pour me répondre !" : "Touche ta réponse !")],
    choix: reps.map(r => ({classe: "reponse-conv", aide: r.m.mot, contenu: () => boutonParle(r.image, r.m),
      action: () => repondreConversation(n, scene, {sujet: "comment", reponse: r.cle}, r.m, r.apres)}))};
}
function questionAge(n, L, scene){
  const age = P().age, q = motDuJeu(CONVERSATIONS.age.question, L.langue);
  const d = Math.max(0, Math.min(AGES_DITS.indexOf(age) - 1, AGES_DITS.length - 3)), autour = AGES_DITS.slice(d, d + 3);   // its age and the next ones
  const reps = autour.map(a => ({age: a, m: motDuJeu("age_" + a, L.langue)}));
  const bravo = motDuJeu("bravo", L.langue), bon = reps.find(r => r.age === age);
  if (!q || !bon || !bon.m || reps.some(r => !r.m)) return null;
  return {langue: L.langue, lignes: [ligneQuestion(q, L, "Touche le gâteau de ton âge !")],
    choix: reps.map(r => ({classe: "reponse-conv", aide: r.m.mot, contenu: () => boutonParle("🎂", r.m, r.age),
      action: () => repondreConversation(n, scene, {sujet: "age", reponse: "age_" + r.age, juste: r.age === age}, r.m,
        r.age === age ? bravo : null, r.age === age ? null : `Tu as ${age} ans, je crois : « ${bon.m.mot} »`)}))};
}
// the child's answer by its recording, in the bubble with the hero's face (its meaning by the game's rule); then the
// villager answers in turn: a word of their language, or (`ligne`) a French line quoting the right sentence; then the
// most useful of their usual lines (HistoireVillage.ligneUtile). A voice cut meanwhile (the child touched something
// else): the villager stays quiet
function repondreConversation(n, scene, journal, m, apres, ligne){
  if (typeof noterJournal === "function") noterJournal("conversation", {h: n.cle, ...journal});
  const p = P(), voix = direMot(m), libre = () => ecranCourant === "monde" && !dialogueOuvert() && !!n.s && n.s.active;
  const utile = scene.histoire && typeof scene.histoire.ligneUtile === "function" ? scene.histoire.ligneUtile(n) : null;
  bulleMotVillage(herosHD(p), p.nom, m, voix);
  voix.then(fini => {
    if (fini === false || !libre()) return;
    if (scene.vie) scene.vie.saluer(n.s, 1200);
    if (!apres) return bulleVillage(n.cle, n.nom, ligne + (utile ? " " + utile : ""), LANGUES_HABITANTS[n.cle].langue);
    const v2 = direMot(apres);
    bulleMotVillage(n.cle, n.nom, apres, v2);
    v2.then(f2 => { if (f2 !== false && utile && libre()) bulleVillage(n.cle, n.nom, utile); });
  });
}
