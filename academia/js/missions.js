/* Académia : les missions du jour, données par le Sage (js/jeu/histoire_village.js). Chaque jour, trois missions tirées
   parmi celles que la partie permet de tenir ce jour-là : une maison où consoler des Ombres, un exploit (sans faute,
   un chef, une erreur réparée, la révision du jour…), le jardin, un compagnon à faire grandir, évoluer ou libérer ; un
   seul chef par jour. Chacune se vérifie d'après l'état de la partie seulement : les combats du jour (P().hist), les
   erreurs réparées du jour (P().album.reparees, ou les revanches notées avec les combats), les révisions
   (P().maitrise), le jardin (P().jardin), les compagnons. Écrites dans P().missions {jour, liste: [{id, texte, but, fait, autocollant, ...}]} ;
   une mission tenue est annoncée (son autocollant saute au milieu de l'écran, puis file vers le sac) et va dans
   l'album du sac (P().album.autocollants : {id, jour, texte}).
   L'écran des missions (#missions) s'ouvre en touchant le panneau du Sage sur la place (js/jeu/arbre_monde.js) ou par
   le Sage : trois cartes dessinées, dites par la voix au toucher, leur avancement en points, l'autocollant à gagner.
   Autocollants : outils/village/scripts/autocollants.py (assets/hd/autocollants, assets/leger/autocollants). */
if (!ECRANS.includes("missions")) ECRANS.push("missions");   // one more screen for montrer() (js/outils.js)
const MISSIONS_PAR_JOUR = 3;
const srcAutocollant = id => `${DOSSIER_HD}/autocollants/${id}.png`;
// the stickers, the catalogue the album of the bag reads (js/album.js says « L'autocollant ${nom} ») :
// AUTOCOLLANTS[id] = {nom (« du soleil », after « l'autocollant »), titre (« Le soleil », to show), image (its drawing,
// in the quality of the game)}
const AUTOCOLLANTS = {};
[["soleil", "Le soleil", "du soleil"], ["arc_en_ciel", "L'arc-en-ciel", "de l'arc-en-ciel"], ["fusee", "La fusée", "de la fusée"],
 ["champignon", "Le champignon", "du champignon"], ["coeur", "Le cœur", "du cœur"], ["etoile_filante", "L'étoile filante", "de l'étoile filante"],
 ["pomme", "La pomme", "de la pomme"], ["papillon", "Le papillon", "du papillon"], ["lune", "La lune", "de la lune"],
 ["fleur", "La fleur", "de la fleur"], ["glace", "La glace", "de la glace"], ["baleine", "La baleine", "de la baleine"],
 ["chat", "Le chat", "du chat"], ["trefle", "Le trèfle", "du trèfle"], ["cadeau", "Le cadeau", "du cadeau"],
 ["dino", "Le dinosaure", "du dinosaure"], ["hibou", "Le hibou", "du hibou"]
].forEach(([id, titre, nom]) => { AUTOCOLLANTS[id] = {nom, titre}; Object.defineProperty(AUTOCOLLANTS[id], "image", {enumerable: true, get: () => srcAutocollant(id)}); });
// the drawn icons the missions show (on the board of the square too)
const ICONES_MISSIONS = ["pattes", "epees", "etoile", "etincelle", "couronne", "revanche", "niveau", "fusee", "cadenas_ouvert", "chouette", ...REGIONS.map(r => "maison_" + r.id)];

const duJour = t => !!t && jour(Date.parse(t)) === jour();
const victoiresDuJour = p => (p.hist || []).filter(h => h.gagne && duJour(h.t));
const compagnonMission = (p, id) => (p.compagnons || []).find(c => c.id === id) || null;
const etapeDe = (p, r) => ((p.regions || {})[r.id] || {}).etape || 0;
const presDe = id => { const r = regionDe(id); return "près " + ((typeof DE_REGION === "object" && DE_REGION[id]) || "de ") + r.nom; };
const nomPour = id => { const c = compagnonMission(P(), id); return c ? nomCompagnon(c) : "ton compagnon"; };
const parcelles = p => (typeof jardinDe === "function" && jardinDe(p) ? jardinDe(p).parcelles : []);
// the notions whose spaced revision is due today (js/adaptatif.js): {id, pas}
const revisionsDues = p => Object.entries(p.maitrise || {}).filter(([, e]) => e && e.valide && e.rev && e.rev.le <= jour()).map(([id, e]) => ({id, pas: e.rev.pas || 0}));
// the next house to free: the one whose door is nearest to opening, among those whose chief is not consoled yet
function prochaineMaison(p = P()){
  const l = REGIONS.filter(r => !(((p.regions || {})[r.id] || {}).badges > 0));
  return l.length ? l.reduce((a, b) => etapeDe(p, b) > etapeDe(p, a) ? b : a) : null;
}

// each kind of mission: its group (one mission per group, then the others), when it can be given, how it is made,
// said and drawn, and how far the child is (read in the game's state only)
const TYPES_MISSIONS = {
  ombres: {groupe: "combat", creer: (p, petit) => ({region: (prochaineMaison(p) || REGIONS[0]).id, but: petit ? 2 : 3}),
    texte: m => `Console ${m.but} Ombres ${presDe(m.region)}.`, dessin: m => ({icone: "maison_" + m.region}),
    compte: (p, m) => victoiresDuJour(p).filter(h => h.region === m.region && !h.boss).length},
  maisons: {groupe: "combat", creer: (p, petit) => ({but: petit ? 2 : 3}), texte: m => `Console des Ombres près de ${m.but} maisons différentes.`,
    dessin: () => ({icone: "pattes"}), compte: p => new Set(victoiresDuJour(p).map(h => h.region)).size},
  victoires: {groupe: "combat", creer: (p, petit) => ({but: petit ? 3 : 5}), texte: m => `Gagne ${m.but} combats.`,
    dessin: () => ({icone: "epees"}), compte: p => victoiresDuJour(p).length},
  sansFaute: {groupe: "exploit", creer: () => ({but: 1}), texte: () => "Gagne un combat sans te tromper.", dessin: () => ({icone: "etoile"}),
    compte: p => victoiresDuJour(p).filter(h => h.total > 0 && h.bonnes >= h.total).length},
  bonnes: {groupe: "exploit", creer: (p, petit) => ({but: petit ? 8 : 15}), texte: m => `Trouve ${m.but} bonnes réponses.`,
    dessin: () => ({icone: "etincelle"}), compte: p => (p.hist || []).filter(h => duJour(h.t)).reduce((n, h) => n + (h.bonnes || 0), 0)},
  chef: {groupe: "exploit", pareil: "chef", possible: p => REGIONS.some(r => etapeDe(p, r) >= 2), creer: () => ({but: 1}),
    texte: () => "Console un chef des Ombres.", dessin: () => ({icone: "couronne"}), compte: p => victoiresDuJour(p).filter(h => h.boss).length},
  // a companion still held by a chief whose door is near to opening: free it (the team grows by one)
  liberer: {groupe: "compagnon", pareil: "chef", possible: p => REGIONS.some(r => etapeDe(p, r) >= 2 && !(p.compagnons || []).some(c => c.famille === r.famille)),
    creer: p => ({but: 1, depart: (p.compagnons || []).length}), texte: () => "Libère un nouveau compagnon chez un chef des Ombres.",
    dessin: () => ({icone: "cadenas_ouvert"}), compte: (p, m) => (p.compagnons || []).length > m.depart ? 1 : 0},
  reparer: {groupe: "exploit", grand: true, creer: () => ({but: 1}), texte: () => "Répare une erreur chez un chef des Ombres.",
    possible: p => REGIONS.some(r => etapeDe(p, r) >= 2 && (p.erreurs || []).some(e => e.matiere === r.matiere)), dessin: () => ({icone: "revanche"}),
    compte: p => Math.max(((p.album || {}).reparees || []).filter(e => duJour(e.t)).length,
      (p.hist || []).filter(h => duJour(h.t)).reduce((n, h) => n + (h.revanches || 0), 0))},
  // the day's revision (js/adaptatif.js): a notion learnt days ago whose day has come, answered right again
  revision: {groupe: "exploit", possible: p => revisionsDues(p).length > 0, creer: p => ({but: 1, dues: revisionsDues(p)}),
    texte: () => "Réussis ta révision du jour.", dessin: () => ({icone: "chouette"}),
    compte: (p, m) => (m.dues || []).some(d => { const e = (p.maitrise || {})[d.id], r = e && e.rev;
      return !!r && r.le > jour() && r.pas >= Math.min(d.pas + 1, ECARTS_REVISION.length - 1); }) ? 1 : 0},
  niveau: {groupe: "compagnon", possible: () => !!actif(), creer: () => ({compagnon: actif().id, depart: actif().niveau, but: 1}),
    texte: m => `Fais grandir ${nomPour(m.compagnon)} d'un niveau.`, dessin: m => ({compagnon: m.compagnon, icone: "niveau"}),
    compte: (p, m) => { const c = compagnonMission(p, m.compagnon); return c && c.niveau > m.depart ? 1 : 0; }},
  evoluer: {groupe: "compagnon", possible: () => { const c = actif(); return !!c && PALIERS.some(n => n > c.niveau && n - c.niveau <= 2); },
    creer: () => ({compagnon: actif().id, depart: stade(actif()), but: 1}), texte: m => `Aide ${nomPour(m.compagnon)} à évoluer.`,
    dessin: m => ({compagnon: m.compagnon, icone: "fusee"}), compte: (p, m) => { const c = compagnonMission(p, m.compagnon); return c && stade(c) > m.depart ? 1 : 0; }},
  arroser: {groupe: "jardin", possible: p => parcelles(p).some(pa => etatParcelle(pa) === "soif"), creer: () => ({but: 1}),
    texte: () => "Arrose une plante de ton jardin.", dessin: () => ({jardin: "goutte"}),
    compte: p => parcelles(p).some(pa => pa.culture && pa.arrosee === jour()) ? 1 : 0},
  recolter: {groupe: "jardin", possible: p => parcelles(p).some(pa => etatParcelle(pa) === "mure"),
    creer: p => ({but: 1, depart: parcelles(p).map((pa, i) => etatParcelle(pa) === "mure" ? i : -1).filter(i => i >= 0)}),
    texte: () => "Récolte une plante mûre de ton jardin.", dessin: () => ({jardin: "recolte_fraise"}),
    compte: (p, m) => (m.depart || []).some(i => parcelles(p)[i] && etatParcelle(parcelles(p)[i]) !== "mure") ? 1 : 0},
  planter: {groupe: "jardin", possible: p => parcelles(p).some(pa => !pa.culture) && p.etincelles >= 4, creer: () => ({but: 1}),
    texte: () => "Plante une graine dans ton jardin.", dessin: () => ({jardin: "graines_carotte"}),
    compte: p => parcelles(p).some(pa => pa.semee === jour()) ? 1 : 0}
};

// a little random that gives the same draw for the same child on the same day
function hasardDe(graine){
  let h = 2166136261;
  for (const c of String(graine)) h = Math.imul(h ^ c.charCodeAt(0), 16777619);
  return () => { h = Math.imul(h ^ (h >>> 15), 2246822507); h = Math.imul(h ^ (h >>> 13), 3266489909); h ^= h >>> 16; return (h >>> 0) / 4294967296; };
}
const melanger = (l, r) => { l = l.slice(); for (let i = l.length - 1; i > 0; i--) { const j = Math.floor(r() * (i + 1)); [l[i], l[j]] = [l[j], l[i]]; } return l; };

// three missions for today: one where Ombres wait, one in the garden when it has something to do, then exploits and
// the companion; each with the next sticker of the child's collection
function tirerMissions(p){
  const petit = p.age <= 5, r = hasardDe(p.id + jour());
  const ok = Object.keys(TYPES_MISSIONS).filter(id => { const t = TYPES_MISSIONS[id]; try { return !(t.grand && petit) && (!t.possible || t.possible(p)); } catch (e) { return false; } });
  const de = (...g) => melanger(ok.filter(id => g.includes(TYPES_MISSIONS[id].groupe)), r);
  const combat = de("combat"), choisis = [combat.shift(), ...de("jardin").slice(0, 1)];
  const pareil = id => { const q = TYPES_MISSIONS[id].pareil; return !!q && choisis.some(c => c && TYPES_MISSIONS[c].pareil === q); };   // one chief a day
  [...de("exploit", "compagnon"), ...combat].forEach(id => { if (choisis.length < MISSIONS_PAR_JOUR && !pareil(id)) choisis.push(id); });
  const a = new Set((p.album.autocollants || []).map(x => x.id)), libres = melanger(Object.keys(AUTOCOLLANTS), hasardDe(p.id)).filter(id => !a.has(id));
  const ordre = [...libres, ...melanger(Object.keys(AUTOCOLLANTS), r).filter(id => !libres.includes(id))];   // the album full: again, never twice a day
  return choisis.filter(Boolean).map((id, i) => {
    const m = {id, ...TYPES_MISSIONS[id].creer(p, petit), fait: false, autocollant: ordre[i]};
    m.texte = TYPES_MISSIONS[id].texte(m);
    return m;
  });
}
// today's missions are there (drawn on the first look of the day); false without a child
function preparerMissions(p = P()){
  if (!p) return false;
  if (!p.missions || typeof p.missions !== "object") p.missions = {jour: "", liste: []};
  if (p.missions.jour === jour() && Array.isArray(p.missions.liste) && p.missions.liste.length) return true;
  p.missions = {jour: jour(), liste: tirerMissions(p)};
  sauver();
  return true;
}
const missionsDuJour = () => (preparerMissions() ? P().missions.liste.filter(m => TYPES_MISSIONS[m.id]) : []);
const missionsVues = () => { const p = P(); return !!(p && p.histoire && duJour(p.histoire.missions_vues)); };
function progresMission(m, p = P()){   // a mission done stays full (a harvested plant, a companion put back in the bag…)
  if (m.fait) return m.but;
  try { return Math.min(m.but, TYPES_MISSIONS[m.id].compte(p, m)); } catch (e) { return 0; }
}

// the missions just done: marked, their sticker in the album, waiting to be announced in the village (each with its
// child: another child chosen meanwhile never hears it)
const MISSIONS_A_DIRE = [];
function verifierMissions(){
  const p = P(); if (!p || !preparerMissions(p)) return [];
  const faites = missionsDuJour().filter(m => !m.fait && progresMission(m, p) >= m.but);
  faites.forEach(m => {
    m.fait = new Date().toISOString();
    p.album.autocollants.push({id: m.autocollant, jour: jour(), texte: m.texte});
    MISSIONS_A_DIRE.push({m, qui: p.id});
  });
  if (faites.length) sauver();
  return faites;
}
// the next mission to announce for the child playing, or null
function missionADire(){
  const p = P();
  while (MISSIONS_A_DIRE.length) { const a = MISSIONS_A_DIRE.shift(); if (p && a.qui === p.id) return a.m; }
  return null;
}
// a mission done, in the village: its sticker pops in the middle of the screen with sparkles, the voice says it, then
// it flies into the bag (the album is there); nothing to touch, the game goes on under it
function annoncerMission(m){
  const a = AUTOCOLLANTS[m.autocollant] || {nom: ""}, toutes = missionsDuJour().every(x => x.fait) && !MISSIONS_A_DIRE.length;   // said once, with the last one
  const voix = dire(`Bravo, mission réussie ! Tu gagnes l'autocollant ${a.nom} pour ton album !` + (toutes ? " Toutes les missions du jour sont réussies !" : ""));
  if (typeof jouerSon === "function") jouerSon("etoile");
  const d = el("div", "annonce-mission"), im = el("img"), t = el("b");
  im.alt = ""; im.src = srcAutocollant(m.autocollant); t.textContent = "Mission réussie !";
  d.append(im, t); document.body.append(d);
  const [x, y] = centre(im);
  if (typeof jaillir === "function") jaillir(x, y, {pieces: ["etoile", "etincelle", "confetti_jaune", "confetti_rose"], n: 18, angle: [0, 360], gravite: 300, vitesse: [160, 360], taille: [14, 26], duree: [700, 1100], z: 31});
  const fin = () => { if (d.isConnected) d.remove(); };
  const sac = $("btnEquipe"), b = sac && !$("barre").hidden ? sac.getBoundingClientRect() : null;
  return Promise.race([Promise.resolve(voix), wait(4500)]).then(() => wait(calme() ? 0 : 500)).then(() => {
    if (calme() || !b || !b.width || !d.animate) return fin();
    const r = im.getBoundingClientRect();
    const an = d.animate([{transform: "translate(-50%, -50%)", opacity: 1},
      {transform: `translate(calc(-50% + ${b.left + b.width / 2 - r.left - r.width / 2}px), calc(-50% + ${b.top + b.height / 2 - r.top - r.height / 2}px)) scale(.15)`, opacity: .6}],
      {duration: 700, easing: "ease-in", fill: "forwards"});
    return new Promise(ok => { an.onfinish = an.oncancel = () => { fin(); ok(); }; });
  });
}

// ---------- the screen of the day's missions ----------
// what the voice says for a mission: the mission, then how far the child is
function phraseMission(m){
  const n = progresMission(m);
  if (m.fait) return `${m.texte} C'est réussi ! Tu as gagné l'autocollant ${(AUTOCOLLANTS[m.autocollant] || {nom: ""}).nom}.`;
  if (m.but > 1 && n > 0) return `${m.texte} Déjà ${n} sur ${m.but} !`;
  return `${m.texte} Tu gagneras un autocollant !`;
}
// a mission's drawing: its icon, a companion's face with a small icon, or a drawing of the garden
function dessinMissionDom(m){
  const d = (TYPES_MISSIONS[m.id] || {dessin: () => ({icone: "etoile"})}).dessin(m), f = document.createDocumentFragment();
  const c = d.compagnon && compagnonMission(P(), d.compagnon);
  if (c) { f.append(imageHD(cleDe(c), "face", 64)); if (d.icone) f.append(iconeDom(d.icone, "mission-badge")); }
  else if (d.jardin) { const i = el("img", "mission-jardin"); i.alt = ""; i.src = srcJardin(d.jardin); f.append(i); }
  else f.append(iconeDom(d.icone || "etoile"));
  return f;
}
// the same drawing on the board of the square (a texture of the village scene), or null
function cleDessinMission(m, scene){
  const d = (TYPES_MISSIONS[m.id] || {dessin: () => ({})}).dessin(m);
  const cle = d.jardin ? "jardin_" + (d.jardin.startsWith("graines") ? "graines" : d.jardin) : d.icone ? "ico_" + d.icone : null;
  return cle && scene.textures.exists(cle) ? cle : null;
}
function carteMission(m){
  const n = progresMission(m), b = el("button", "mission" + (m.fait ? " faite" : ""));
  const dessin = el("span", "mission-dessin"), texte = el("span", "mission-texte"), avance = el("span", "mission-avance"), prix = el("span", "mission-prix");
  dessin.append(dessinMissionDom(m));
  if (m.fait) dessin.append(iconeDom("coche", "mission-coche"));
  texte.textContent = m.texte;
  if (m.but <= 6) for (let k = 0; k < m.but; k++) avance.append(el("i", k < n ? "plein" : ""));
  else { const barre = el("span", "mission-barre"), i = el("i"), t = el("b"); i.style.width = `${100 * n / m.but}%`; t.textContent = `${n} / ${m.but}`; barre.append(i); avance.append(barre, t); }
  const im = el("img"); im.alt = ""; im.src = srcAutocollant(m.autocollant); prix.append(im);
  b.append(dessin, texte, avance, prix);
  b.setAttribute("aria-label", phraseMission(m));
  b.onclick = () => { sfx.tap(); sauterDessin(dessin); dire(phraseMission(m)); };
  return b;
}
function dessinerMissions(){
  const l = missionsDuJour(), faites = l.filter(m => m.fait).length;
  $("missionsListe").replaceChildren(...l.map(carteMission));
  if (l[0]) markOk($("missionsListe").firstChild);
  $("missionsPied").textContent = faites === l.length ? "Toutes les missions sont réussies. Bravo !" : "Chaque mission réussie donne un autocollant pour ton album.";
}
function ouvrirMissions(){
  const p = P(); if (!p || !preparerMissions(p)) return;
  verifierMissions();
  montrer("missions");
  p.histoire = p.histoire && typeof p.histoire === "object" ? p.histoire : {};
  const premiere = !missionsVues();
  p.histoire.missions_vues = new Date().toISOString(); sauver();
  imageReessayee($("missionsSage"), `${DOSSIER_HD}/portraits/sage.png`);
  dessinerMissions();
  const l = missionsDuJour(), restent = l.filter(m => !m.fait).length;
  // the little one hears them all the first time; then a touch on a mission says it again
  if (!restent) dire("Tes missions du jour sont toutes réussies. Bravo ! Reviens demain pour de nouvelles missions.");
  else if (p.age <= 5 && premiere) dire("Voici tes missions du jour. " + l.map(m => m.texte).join(" ") + " Touche une mission pour l'écouter encore.");
  else dire(premiere ? "Voici tes missions du jour. Touche une mission pour l'écouter." : `Encore ${restent} mission${restent > 1 ? "s" : ""} à réussir. Touche une mission pour l'écouter.`);
  majPanneauMissions();
}
function fermerMissions(){
  if ($("missions").hidden) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  majPanneauMissions();
}
function majPanneauMissions(){
  const m = typeof JEU === "object" && JEU.scene.getScene("monde");
  if (m && m.place && (m.sys.isActive() || m.sys.isSleeping())) m.place.majPanneau();
}
document.addEventListener("DOMContentLoaded", () => {
  $("btnFermerMissions").onclick = () => { sfx.tap(); fermerMissions(); };
  $("missions").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerMissions(); } };
});
