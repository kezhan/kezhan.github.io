/* Académia : les mots et petites phrases étrangers que le jeu dit hors des questions (Kezhan, 13/10/2026 : « n'oublie
   pas, dès que possible faire apprendre »). Une seule liste, lue par le jeu et par les outils qui font les voix :
   outils/voix/decoupage.py (mots_du_jeu, relevé par tout_ce_qui_se_dit : l'anglais et l'allemand, enregistrés par
   Azure) et outils/contenu/scripts/fabrique.py (ses textes : le luxembourgeois, enregistré par lod.lu, que
   lexique.nettoyer() garde donc). tests/test_questions.py vérifie que chaque mot de la liste a son enregistrement.
   1. MOTS_DU_JEU, entre les repères <mots> : une entrée par ligne, son sens en français (fr), puis le mot de chaque
      langue où le jeu le dit (en, de, lb), écrit comme il s'affiche, entre guillemets droits (les outils lisent ce
      fichier tel quel). Un mot ajouté ici n'est dit qu'une fois enregistré : le jeu le filtre (aSaVoix pour l'anglais
      et l'allemand, couvertLb pour le luxembourgeois, js/voix.js), sans enregistrement il se tait.
   2. motDuJeu(cle, langue) : {cle, langue, mot, sens, nom} quand le mot a son enregistrement, sinon null.
   3. direMot(m) : le mot par sa voix enregistrée ; pour la petite, les premières fois, son sens ensuite (« C'est
      « merci », en allemand ! »), compté dans P().histoire.mots (langue:clé -> fois).
   4. langueDuJour() : une langue qui change d'un jour à l'autre (le luxembourgeois du Sage d'abord) ; LANGUE_MAISON :
      celle d'une maison de langue ; bulleMot(ancre, m) : le mot écrit dans une petite bulle, son sens en petit. */
// <mots>
const MOTS_DU_JEU = {
  // greetings and good manners (the villagers, the companions, the home screen, the doors, the Ombres)
  bonjour: {fr: "bonjour", en: "Hello!", de: "Hallo!", lb: "Moien"},
  bonjour_matin: {fr: "bonjour, le matin", en: "Good morning!", de: "Guten Morgen!", lb: "Gudde Moien"},
  bonsoir: {fr: "bonsoir", en: "Good evening!", de: "Guten Abend!", lb: "Gudden Owend"},
  comment_vas_tu: {fr: "comment vas-tu ?", en: "How are you?", de: "Wie geht's?"},
  bonne_journee: {fr: "bonne journée", en: "Have a nice day!", de: "Einen schönen Tag!"},
  bravo: {fr: "bravo", en: "Well done!", de: "Gut gemacht!"},
  merci: {fr: "merci", en: "Thank you!", de: "Danke!", lb: "Merci"},
  au_revoir: {fr: "au revoir", en: "Goodbye!", de: "Tschüss!", lb: "Äddi"},
  bienvenue: {fr: "bienvenue", en: "Welcome!", de: "Willkommen!", lb: "Wëllkomm"},
  bonne_nuit: {fr: "bonne nuit", lb: "Gutt Nuecht"},
  a_tes_souhaits: {fr: "à tes souhaits", en: "Bless you!", de: "Gesundheit!", lb: "Gesondheet!"},
  bon_appetit: {fr: "bon appétit", de: "Guten Appetit!"},
  // what a companion says
  je_t_aime: {fr: "je t'aime", en: "I love you!", de: "Ich hab dich lieb!", lb: "Ech hunn dech gär!"},
  faim: {fr: "j'ai faim", en: "I'm hungry!", de: "Ich habe Hunger!", lb: "Ech sinn hongereg!"},
  fatigue: {fr: "je suis fatigué", en: "I'm tired…", de: "Ich bin müde…"},
  // counting: the badge's stars, the harvest of the garden (German)
  un: {fr: "un", en: "one", de: "eins", lb: "eent"},
  deux: {fr: "deux", en: "two", de: "zwei", lb: "zwee"},
  trois: {fr: "trois", en: "three", de: "drei", lb: "dräi"},
  quatre: {fr: "quatre", de: "vier"},
  cinq: {fr: "cinq", de: "fünf"},
  six: {fr: "six", de: "sechs"},
  // the days, Monday first (the Sage's missions)
  lundi: {fr: "lundi", en: "Monday", de: "Montag", lb: "Méindeg"},
  mardi: {fr: "mardi", en: "Tuesday", de: "Dienstag", lb: "Dënschdeg"},
  mercredi: {fr: "mercredi", en: "Wednesday", de: "Mittwoch", lb: "Mëttwoch"},
  jeudi: {fr: "jeudi", en: "Thursday", de: "Donnerstag", lb: "Donneschdeg"},
  vendredi: {fr: "vendredi", en: "Friday", de: "Freitag", lb: "Freideg"},
  samedi: {fr: "samedi", en: "Saturday", de: "Samstag", lb: "Samschdeg"},
  dimanche: {fr: "dimanche", en: "Sunday", de: "Sonntag", lb: "Sonndeg"},
  // the stickers of the missions (js/missions.js, AUTOCOLLANTS: the same keys)
  soleil: {fr: "le soleil", en: "sun", de: "die Sonne", lb: "d'Sonn"},
  arc_en_ciel: {fr: "l'arc-en-ciel", en: "rainbow", de: "der Regenbogen", lb: "de Reebou"},
  pomme: {fr: "la pomme", en: "apple", de: "der Apfel", lb: "den Apel"},
  papillon: {fr: "le papillon", en: "butterfly", de: "der Schmetterling", lb: "de Päiperlek"},
  glace: {fr: "la glace", en: "ice cream", de: "das Eis", lb: "d'Glace"},
  chat: {fr: "le chat", en: "cat", de: "die Katze", lb: "d'Kaz"},
  hibou: {fr: "le hibou", en: "owl", de: "die Eule", lb: "d'Eil"},
  // where the companions' names and elements come from (js/equipe.js)
  ours: {fr: "l'ours", de: "der Bär"},
  lion: {fr: "le lion", lb: "de Léiw"},
  vent: {fr: "le vent", en: "wind", de: "der Wind", lb: "de Wand"}
};
// </mots>
const NOMS_LANGUES = {en: "anglais", de: "allemand", lb: "luxembourgeois"};
const LANGUES_JOUR = ["lb", "en", "de"];   // the Sage's language first
const LANGUE_MAISON = {albion: "en", germania: "de", duche: "lb"};   // the language houses (js/compagnons.js, REGIONS)
const FOIS_SENS = 3;   // the little one hears a word's meaning the first three times

// a foreign text said only by its recording (js/voix.js): an English or German one in every piece, a Luxembourgish
// one entirely
const motEnregistre = (texte, langue) => langue === "lb" ? typeof couvertLb === "function" && couvertLb(texte)
  : typeof aSaVoix === "function" && aSaVoix(texte, langue);
function motDuJeu(cle, langue){
  const e = MOTS_DU_JEU[cle], mot = e && e[langue];
  return mot && motEnregistre(mot, langue) ? {cle, langue, mot, sens: e.fr, nom: NOMS_LANGUES[langue]} : null;
}
// the words of a list that have their recording, in its order
const motsDuJeu = (cles, langue) => cles.map(k => motDuJeu(k, langue)).filter(Boolean);
// the recordings' indexes, loaded (the home screen can be touched before they are)
const motsPrets = () => Promise.all([typeof chargerVoixEtrangeres === "function" ? chargerVoixEtrangeres() : null,
  typeof chargerLexiqueLb === "function" ? chargerLexiqueLb() : null]).catch(() => {});

// a language that changes from one day to the next: today's, or the one of `decalage` days later
function langueDuJour(decalage = 0){
  const d = new Date(), n = Math.floor(Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) / 864e5) + decalage;
  return LANGUES_JOUR[((n % 3) + 3) % 3];
}
const langueDeMaison = id => LANGUE_MAISON[id] || langueDuJour();

// the meaning of a word, for the little one, the first times (P().histoire.mots: « de:merci » -> times said)
function histoireMots(p = P()){
  if (!p) return {};
  if (!p.histoire || typeof p.histoire !== "object") p.histoire = {};
  if (!p.histoire.mots || typeof p.histoire.mots !== "object" || Array.isArray(p.histoire.mots)) p.histoire.mots = {};
  return p.histoire.mots;
}
const SENS_RECENT = {};   // in this sitting: when each word's meaning was last said
function sensADire(m){
  const p = P(); if (!p || p.age > 5 || !m) return false;
  const h = histoireMots(p), k = `${m.langue}:${m.cle}`;
  if ((Number(h[k]) || 0) >= FOIS_SENS) return false;
  if (performance.now() - (SENS_RECENT[k] ?? -Infinity) < 60000) return false;   // just heard (the door, then its chief): once is enough
  SENS_RECENT[k] = performance.now();
  h[k] = (Number(h[k]) || 0) + 1; sauver();
  return true;
}
const phraseSens = m => `C'est « ${m.sens} », en ${m.nom} !`;
// the word by its own voice, then its meaning in French when the little one should hear it (sens: true or false
// forces it); the promise ends with the voice
function direMot(m, {sens} = {}){
  if (!m) return Promise.resolve(false);
  const s = sens === undefined ? sensADire(m) : sens;
  return suiteDeVoix([() => dire(m.mot, m.langue), ...(s ? [() => dire(phraseSens(m))] : [])]);
}

// the companions of the language houses speak their language (Plumix English, Bärli German, Léiwchen Luxembourgish):
// a word after the other when touched; null for the other companions
const MOTS_COMPAGNONS = ["bonjour", "je_t_aime", "faim", "merci"], TOURS_COMPAGNONS = {};
const langueCompagnon = f => { const r = typeof REGIONS === "object" ? REGIONS.find(x => x.famille === f) : null; return (r && LANGUE_MAISON[r.id]) || null; };
function motCompagnon(f){
  const l = langueCompagnon(f), mots = l ? motsDuJeu(MOTS_COMPAGNONS, l) : [];
  if (!mots.length) return null;
  const i = TOURS_COMPAGNONS[f] = ((TOURS_COMPAGNONS[f] ?? -1) + 1) % mots.length;
  return mots[i];
}

// a voice is speaking (js/voix.js tells when it starts and ends): what the village tells by itself waits for it
let voixParle = false;
document.addEventListener("voix", e => { voixParle = e.detail === "debut"; });
const voixEnCours = () => voixParle;

// a little speech bubble by the head of a drawing (its top right corner, its top left near the right edge of the
// screen: never over the card or the button above it), or at the top of the screen: the word in bold, its meaning in
// small; it goes by itself
function bulleMot(ancre, m, {ms = 3000} = {}){
  if (!m) return null;
  const b = el("div", "bulle-mot"), mot = el("b"), s = el("small");
  mot.textContent = m.mot; s.textContent = m.sens; mot.lang = m.langue; s.lang = "fr";
  b.append(mot, s); document.body.append(b);
  const r = ancre && ancre.isConnected ? ancre.getBoundingClientRect() : null;
  if (r && r.width) {
    const w = b.offsetWidth, h = b.offsetHeight;
    let x = r.right - w * .35;
    if (x + w > innerWidth - 8) x = r.left - w * .65;
    b.style.left = `${Math.max(8, Math.min(innerWidth - w - 8, x))}px`;
    b.style.top = `${Math.max(8, Math.min(innerHeight - h - 8, r.top))}px`;
  } else b.classList.add("haut");
  setTimeout(() => b.remove(), ms);
  return b;
}
