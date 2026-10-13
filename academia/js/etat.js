/* Académia : l'état du jeu, une partie par enfant retrouvée par son prénom, sauvegardée à chaque étape
   dans le navigateur (localStorage, l'équivalent moderne des cookies). Kezhan : « demander au début le nom
   de l'enfant, et save l'historique en fonction du nom ». Une copie de secours est gardée ; une partie abîmée ou
   d'une version plus ancienne est réparée ; deux onglets ouverts ne s'effacent pas la partie l'un de l'autre. */
const VERSION = "0.12";
const CLE = "academia.v1", CLE_COPIE = "academia.v1.copie";
const E = {profils: {}, courant: null};
const P = () => E.profils[E.courant] || null;

function nouveauProfil(nom, age){
  return {id: uid(), nom, age, cree: new Date().toISOString(), vu: new Date().toISOString(),
    etincelles: 0, jokers: {potion: 2, indice: 2, sablier: 1},
    compagnons: [], actif: null,          // {id, famille, niveau, xp, pv}
    regions: {},                          // id -> {niveau, etape, badges}
    maitrise: {},                         // notion -> {ok, ko, hist}  (hist: "1101…", the latest answers)
    erreurs: [],                          // missed questions, replayed by the next boss (revanche)
    hist: [],                             // fights: {t, region, gagne, xp, bonnes, total}
    // the album in the bag: Ombres consoled by kind {cle: count}, shiny ones met {cle: true}, stickers [{id, jour}],
    // mistakes repaired [{question, reponse, matiere, t}] (the latest 50)
    album: {ombres: {}, brillantes: {}, autocollants: [], reparees: []},
    missions: {jour: "", liste: []},      // the day's missions from the Sage: {id, texte, but, fait, autocollant}
    histoire: {},                         // the village's story: moments already seen (id -> ISO date)
    accessoires: {possedes: [], porte: {}},   // bought from the merchant; porte: famille -> accessory worn
    temps: {}};                           // time really played, per day ("2026-10-12" -> ms)
}
const normNom = s => s.trim().toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "");
function profilParNom(nom){ return Object.values(E.profils).find(p => normNom(p.nom) === normNom(nom)) || null; }

function lireSauvegarde(cle){
  try { const d = JSON.parse(localStorage.getItem(cle) || "null"); return d && d.profils && typeof d.profils === "object" ? d : null; }
  catch (e) { return null; }
}
// a game saved by an older version, or damaged: what is missing comes back with its starting value; unusable: left out
function reparer(p){
  if (!p || typeof p !== "object" || !p.id || !p.nom) return null;
  const base = nouveauProfil(String(p.nom), Number(p.age) || 6), q = {...base, ...p};
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v);
  ["regions", "maitrise", "histoire", "temps"].forEach(k => { if (!objet(q[k])) q[k] = base[k]; });
  ["compagnons", "erreurs", "hist"].forEach(k => { if (!Array.isArray(q[k])) q[k] = []; });
  // the newer parts (0.11): each field back to its starting shape when it is missing or damaged
  const forme = (k, objets, listes) => {
    q[k] = objet(q[k]) ? {...base[k], ...q[k]} : base[k];
    objets.forEach(c => { if (!objet(q[k][c])) q[k][c] = {}; });
    listes.forEach(c => { if (!Array.isArray(q[k][c])) q[k][c] = []; });
  };
  forme("album", ["ombres", "brillantes"], ["autocollants", "reparees"]);
  forme("missions", [], ["liste"]); if (typeof q.missions.jour !== "string") q.missions.jour = "";
  forme("accessoires", ["porte"], ["possedes"]);
  q.jokers = {...base.jokers, ...(q.jokers && typeof q.jokers === "object" ? q.jokers : {})};
  q.compagnons = q.compagnons.filter(c => c && c.id && FAMILLES[c.famille]);
  if (!q.compagnons.some(c => c.id === q.actif)) q.actif = q.compagnons[0] ? q.compagnons[0].id : null;
  if (typeof reparerJardin === "function") q.jardin = reparerJardin(q.jardin);   // the child's garden (js/jardin.js)
  return q;
}
function prendre(d){
  E.profils = {};
  Object.entries(d.profils).forEach(([id, p]) => { const q = reparer(p && {...p, id: p.id || id}); if (q) E.profils[q.id] = q; });
  E.courant = E.profils[d.courant] ? d.courant : null;
}
function charger(){
  const d = lireSauvegarde(CLE) || lireSauvegarde(CLE_COPIE);   // the main save unreadable: its copy
  if (d) prendre(d);
  // ask the browser not to evict a child's progress when space runs low
  try { if (navigator.storage && navigator.storage.persist) navigator.storage.persist(); } catch (e) {}
}
let pleinDit = false;
const supprimes = new Set();   // games removed in this tab: never brought back from another tab's save
function sauver(){
  const p = P(); if (p) p.vu = new Date().toISOString();
  // another tab may have saved another child meanwhile: the latest of each other game is kept, this one is written
  const ailleurs = lireSauvegarde(CLE);
  if (ailleurs) Object.entries(ailleurs.profils).forEach(([id, q]) => {
    if (id === E.courant || !q || supprimes.has(id)) return;
    const moi = E.profils[id];
    if (!moi || String(q.vu || "") > String(moi.vu || "")) { const r = reparer({...q, id}); if (r) E.profils[id] = r; }
  });
  try { const t = JSON.stringify(E); localStorage.setItem(CLE, t); localStorage.setItem(CLE_COPIE, t); }
  catch (e) { if (!pleinDit) { pleinDit = true; toast("La mémoire du navigateur est pleine : prévenez un parent"); } }
}
// another tab saved: the other children's games are refreshed here (never the one played in this tab)
if (typeof addEventListener === "function") addEventListener("storage", ev => {   // (not in the engine's simulation, tests/moteur.js)
  if (ev.key !== CLE || !ev.newValue) return;
  try {
    const d = JSON.parse(ev.newValue);
    Object.entries(d.profils || {}).forEach(([id, q]) => { if (id !== E.courant && !supprimes.has(id)) { const r = reparer(q && {...q, id}); if (r) E.profils[id] = r; } });
  } catch (x) {}
});
function choisirProfil(id){ E.courant = id; sauver(); }
function creerProfil(nom, age){
  const deja = profilParNom(nom);
  if (deja) { deja.age = age; choisirProfil(deja.id); return deja; }   // same name: the same child, same game
  const p = nouveauProfil(nom.trim(), age);
  E.profils[p.id] = p; choisirProfil(p.id);
  return p;
}
function supprimerProfil(id){ supprimes.add(id); delete E.profils[id]; if (E.courant === id) E.courant = null; sauver(); }

/* companions of the current child */
const actif = () => { const p = P(); return p && p.compagnons.find(c => c.id === p.actif) || null; };
function ajouterCompagnon(famille, niveau = 1){
  const p = P(); if (p.compagnons.some(c => c.famille === famille)) return null;
  const c = {id: uid(), famille, niveau, xp: 0, pv: null};
  p.compagnons.push(c); if (!p.actif) p.actif = c.id;
  sauver(); return c;
}
function regionEtat(id){
  const p = P(); p.regions[id] = p.regions[id] || {niveau: 1, etape: 0, badges: 0};
  return p.regions[id];
}
function noterCombat(entree){
  const p = P(); p.hist.push({t: new Date().toISOString(), ...entree});
  if (p.hist.length > 300) p.hist.splice(0, p.hist.length - 300);
  sauver();
}
