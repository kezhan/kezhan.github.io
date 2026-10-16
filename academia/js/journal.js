/* Académia : le journal de jeu (ticket journal-de-jeu ; Kezhan, 16/10/2026 : « faut faire des logs pour analyser ce
   qui est bien ou pas »). Ce que l'enfant fait, pour savoir ce qui plaît et ce qui bloque :
   1. les séances : la page ouverte, prête, cachée, revenue, quittée ;
   2. chaque écran ouvert (sa durée se lit d'un écran au suivant) ;
   3. chaque toucher au village et ce qu'il a fait (marcher, une Ombre, le marchand, le jardin, un habitant, une
      maison…, ou rien : un dessin mal compris), chaque toucher sur les menus, et ceux qui tombent à côté de tout ;
   4. chaque question : sa notion, son niveau, juste ou faux, la réponse donnée, le temps de réponse, la voix réécoutée ;
   5. chaque combat fini (ce que garde la partie : maison, gagné, bonnes réponses) ; un combat quitté sans fin se lit à
      l'écran qui suit ;
   6. chaque dialogue : lignes vues, lignes passées avant la fin de leur écriture (l'enfant s'impatiente) ;
   7. les erreurs JavaScript, les dessins non chargés, le nombre d'images par seconde.
   Un journal par enfant, dans le stockage du navigateur, à côté de sa partie (clé academia.journal.<id>), écrit par
   paquets (toutes les 5 s, et quand la page se cache) ; 5 000 événements et 30 jours au plus, le plus ancien s'efface,
   et il rétrécit plutôt que de prendre la place de la partie. Il ne part jamais sur internet : il se lit par câble
   (outils/tablette/lire_parties.py, puis analyser_journal.py) ou s'enregistre avec la partie depuis le coin Parents ;
   il s'efface avec la partie. Avant qu'un Gardien soit choisi, les événements attendent et vont au premier choisi. */
const JOURNAL_MAX = 5000, JOURNAL_JOURS = 30, JOURNAL_PAQUET_MS = 5000, JOURNAL_TAILLE_MAX = 600000;
const cleJournal = id => "academia.journal." + id;
const JOURNAL = {attente: [], sans: [], minuteur: null};

// one event: {t: ms since 1970, e: its kind, ...what it says}
function noterJournal(e, d = {}){
  try {
    const x = {t: Date.now(), e, ...d}, p = typeof P === "function" ? P() : null;
    if (!p) { JOURNAL.sans.push(x); if (JOURNAL.sans.length > 200) JOURNAL.sans.shift(); return; }
    if (JOURNAL.sans.length) { JOURNAL.attente.push(...JOURNAL.sans.map(y => ({...y, _p: p.id}))); JOURNAL.sans = []; }
    JOURNAL.attente.push({...x, _p: p.id});   // _p: whose journal (an event has its own id: a screen, a question)
    if (!JOURNAL.minuteur) JOURNAL.minuteur = setTimeout(viderJournal, JOURNAL_PAQUET_MS);
  } catch (err) {}   // the journal never stops the game
}
function lireJournal(id){
  try { const l = JSON.parse(localStorage.getItem(cleJournal(id)) || "[]"); return Array.isArray(l) ? l : []; } catch (e) { return []; }
}
// the waiting events written, each child's into its own journal; kept to its bounds
function viderJournal(){
  clearTimeout(JOURNAL.minuteur); JOURNAL.minuteur = null;
  if (!JOURNAL.attente.length) return;
  const parId = {};
  JOURNAL.attente.forEach(({_p, ...y}) => { (parId[_p] = parId[_p] || []).push(y); });
  JOURNAL.attente = [];
  const depuis = Date.now() - JOURNAL_JOURS * 864e5;
  Object.entries(parId).forEach(([id, l]) => {
    if (!E.profils[id]) return;   // a game removed meanwhile: its events go with it
    let tout = lireJournal(id).concat(l).filter(y => y && y.t >= depuis).slice(-JOURNAL_MAX), t = JSON.stringify(tout);
    while (t.length > JOURNAL_TAILLE_MAX && tout.length > 100) { tout = tout.slice(Math.floor(tout.length / 4)); t = JSON.stringify(tout); }
    try { localStorage.setItem(cleJournal(id), t); }
    catch (e) {   // the browser's memory is full: the journal shrinks, the game's save keeps its room
      try { localStorage.setItem(cleJournal(id), JSON.stringify(tout.slice(-Math.floor(JOURNAL_MAX / 10)))); } catch (e2) {}
    }
  });
}
function effacerJournal(id){ try { localStorage.removeItem(cleJournal(id)); } catch (e) {} }

// what a touched element of a menu is, in a word: its id, its tab, its page, its object, or its first class
function quoiJournal(c){
  if (!c) return null;
  const d = c.dataset || {};
  return c.id || (d.onglet && "onglet:" + d.onglet) || (d.page && "page:" + d.page) || (d.article && "article:" + d.article)
    || (d.famille && "famille:" + d.famille) || (d.v != null && "reponse") || (typeof c.className === "string" && c.className.split(" ")[0]) || c.tagName.toLowerCase();
}

if (typeof document !== "undefined" && typeof addEventListener === "function") {
  noterJournal("ouvre", {v: typeof VERSION !== "undefined" ? VERSION : null, w: innerWidth, h: innerHeight, dpr: devicePixelRatio});
  const pret = setInterval(() => {
    if (window.__pret) { clearInterval(pret); noterJournal("pret", {ms: Math.round(performance.now())}); }
  }, 500);
  setTimeout(() => clearInterval(pret), 120000);
  document.addEventListener("ecran", ev => noterJournal("ecran", {id: ev.detail}));
  // a touch on a menu (the village and the fights are noted by their own scenes): what it hit, or nothing
  document.addEventListener("click", ev => {
    const t = ev.target;
    if (!t || t.tagName === "CANVAS" || (t.closest && t.closest("#question, .question"))) return;   // the answers are noted with their question
    const c = t.closest ? t.closest("button, [role=tab], a, input, label, .profil, .article, .autocollant, .starter, .parcelle, .vignette, .fiche") : null;
    noterJournal("clic", {ec: typeof ecranCourant !== "undefined" ? ecranCourant : null, q: quoiJournal(c)});
  }, {capture: true});
  document.addEventListener("visibilitychange", () => {
    if (document.hidden) {
      noterJournal("cache", {ec: typeof ecranCourant !== "undefined" ? ecranCourant : null, img: typeof ECHECS !== "undefined" ? ECHECS.size : null});
      viderJournal();
    } else noterJournal("revient");
  });
  addEventListener("pagehide", () => { noterJournal("quitte"); viderJournal(); });
  addEventListener("error", e => noterJournal("erreur", {m: String(e.message || "").slice(0, 200), s: `${String(e.filename || "").split("/").pop()}:${e.lineno || 0}`}));
  addEventListener("unhandledrejection", e => noterJournal("erreur", {m: String((e.reason && e.reason.message) || e.reason || "").slice(0, 200), s: "promesse"}));
  // the frames per second, once a minute in the village or a fight: a slow tablet shows here
  setInterval(() => {
    if (document.hidden || typeof JEU === "undefined" || !JEU.loop || !["monde", "combat"].includes(ecranCourant)) return;
    noterJournal("fps", {ec: ecranCourant, f: Math.round(JEU.loop.actualFps)});
  }, 60000);
}
