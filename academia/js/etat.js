/* Académia : l'état du jeu, une partie par enfant retrouvée par son prénom, sauvegardée à chaque étape
   dans le navigateur (localStorage, l'équivalent moderne des cookies). Kezhan : « demander au début le nom
   de l'enfant, et save l'historique en fonction du nom ». */
const VERSION = "0.6";
const CLE = "academia.v1";
const E = {profils: {}, courant: null};
const P = () => E.profils[E.courant] || null;

function nouveauProfil(nom, age){
  return {id: uid(), nom, age, cree: new Date().toISOString(), vu: new Date().toISOString(),
    etincelles: 0, jokers: {potion: 2, indice: 2, sablier: 1},
    compagnons: [], actif: null,          // {id, famille, niveau, xp, pv}
    regions: {},                          // id -> {niveau, etape, badges}
    maitrise: {},                         // notion -> {ok, ko, hist}  (hist: "1101…", the latest answers)
    erreurs: [],                          // missed questions, replayed by the next boss (revanche)
    hist: []};                            // fights: {t, region, gagne, xp, bonnes, total}
}
const normNom = s => s.trim().toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "");
function profilParNom(nom){ return Object.values(E.profils).find(p => normNom(p.nom) === normNom(nom)) || null; }

function charger(){
  try {
    const d = JSON.parse(localStorage.getItem(CLE) || "null");
    if (d && d.profils) { E.profils = d.profils; E.courant = d.courant || null; }
  } catch (e) {}
  // ask the browser not to evict a child's progress when space runs low
  try { if (navigator.storage && navigator.storage.persist) navigator.storage.persist(); } catch (e) {}
}
function sauver(){
  const p = P(); if (p) p.vu = new Date().toISOString();
  try { localStorage.setItem(CLE, JSON.stringify(E)); }
  catch (e) { toast("La mémoire du navigateur est pleine : prévenez un parent"); }
}
function choisirProfil(id){ E.courant = id; sauver(); }
function creerProfil(nom, age){
  const deja = profilParNom(nom);
  if (deja) { deja.age = age; choisirProfil(deja.id); return deja; }   // same name: the same child, same game
  const p = nouveauProfil(nom.trim(), age);
  E.profils[p.id] = p; choisirProfil(p.id);
  return p;
}
function supprimerProfil(id){ delete E.profils[id]; if (E.courant === id) E.courant = null; sauver(); }

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
