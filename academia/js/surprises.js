/* Académia : les trouvailles qui brillent et le visiteur du jour (ticket surprises, lot 1 ; Kezhan, 19/11/2026 : « plein
   de surprises, plein de petites choses, pour accrocher l'attention et faire apprendre »). Au village et dans ses cinq
   quartiers (LIEUX_SURPRISES) ; ce qu'on trouve : js/surprises_donnees.js ; dans la scène : js/jeu/surprises_village.js.
   1. Une trouvaille par jour et par lieu : une étincelle brille dans l'herbe, à un endroit qui change chaque jour.
      Touchée, le héros y va, et l'écran d'une chose (js/maison_chose.js) montre ce qu'il a trouvé : son nom, un mot
      dans une langue (par son enregistrement), une chose vraie, puis un exercice pour une étincelle. D'abord les
      trésors pas encore trouvés du lieu ; ensuite ils reviennent tour à tour. Les trois trésors d'un lieu trouvés :
      l'autocollant du trésor.
   2. Le visiteur du jour : un jour sur deux, un animal passe dans l'un des six lieux (la chouette, le soir
      seulement). Touché, le héros va près de lui : même écran, son nom dans une langue, une chose vraie, un exercice.
      Les cinq animaux vus : l'autocollant des jumelles.
   3. Tout se garde : la page « Trouvailles » de l'album (js/album.js), le journal de jeu, et le jeu dit qu'il faut
      revenir (le Sage, une fois par jour ; le plan du village montre où cela brille et où est l'animal).
   L'état, dans P().surprises : trouvees (« lieu:id » -> fois), pris (lieu -> le jour de sa dernière trouvaille),
   visiteurs (id -> fois), vu (le jour du dernier visiteur vu), annonce (le jour de la dernière annonce du Sage),
   series (lieu -> vrai : son autocollant est donné), tous (vrai : celui des visiteurs aussi).
   Le jour est celui du jeu (maintenantJeu, js/fetes.js : les tests le forcent). Dans un navigateur piloté ou en mode
   TEST, rien ne brille et aucun animal ne passe sans « academia.surprisesTest » = 1 dans localStorage, comme les fêtes
   sans leur jour (js/fetes.js) : les autres recettes gardent le village qu'elles connaissent. */
const LIEUX_SURPRISES = ["village", "ferme", "marche", "ecole", "foret", "plage"];
const HEURE_DU_SOIR = 17;   // the owl comes from then on
const srcSurprise = (sorte, id) => `${DOSSIER_HD}/surprises/${sorte}_${id}.png`;
const cleTrouvaille = (lieu, id) => `${lieu}:${id}`;
const numeroJourSurprise = () => { const [a, m, d] = jourMaison().split("-").map(Number); return Math.floor(Date.UTC(a, m - 1, d) / 864e5); };
function surprisesActives(){
  if (!TEST && !navigator.webdriver) return true;
  try { return localStorage.getItem("academia.surprisesTest") === "1"; } catch (e) { return false; }
}
const heureSurprise = () => new Date(typeof maintenantJeu === "function" ? maintenantJeu() : Date.now()).getHours();
// « au village », « à la ferme » ; « du village », « de la ferme »
const aLieuSurprise = lieu => QUARTIERS[lieu] ? QUARTIERS[lieu].a : "au village";
const duLieuSurprise = lieu => (n => n.startsWith("le ") ? "du " + n.slice(3) : "de " + n)(QUARTIERS[lieu] ? QUARTIERS[lieu].nom : "le village");

function surprisesDe(p = P()){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v);
  if (!objet(p.surprises)) p.surprises = {};
  const s = p.surprises;
  ["trouvees", "pris", "visiteurs", "series"].forEach(k => { if (!objet(s[k])) s[k] = {}; });
  return s;
}
// a damaged game: only what is well formed is kept (js/etat.js, reparer)
function reparerSurprises(lu){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v), date = v => typeof v === "string" && /^\d{4}-\d\d-\d\d$/.test(v);
  const s = {trouvees: {}, pris: {}, visiteurs: {}, series: {}};
  if (!objet(lu)) return s;
  const nombres = (de, vers, ok) => Object.entries(objet(de) ? de : {}).forEach(([k, v]) => { if (ok(k) && Number(v) > 0) vers[k] = Math.floor(Number(v)); });
  nombres(lu.trouvees, s.trouvees, k => { const [l, id] = k.split(":"); return (TROUVAILLES[l] || []).some(t => t.id === id); });
  nombres(lu.visiteurs, s.visiteurs, k => VISITEURS.some(v => v.id === k));
  LIEUX_SURPRISES.forEach(l => { if (objet(lu.pris) && date(lu.pris[l])) s.pris[l] = lu.pris[l]; if (objet(lu.series) && lu.series[l]) s.series[l] = true; });
  ["vu", "annonce"].forEach(k => { if (date(lu[k])) s[k] = lu[k]; });
  if (lu.tous) s.tous = true;
  return s;
}

// ---- what waits today
// the find of a place today: null once it is picked up; a treasure not found yet first, then each one in turn
function trouvailleDuJour(lieu, p = P()){
  const l = TROUVAILLES[lieu], s = surprisesDe(p);
  if (!l || !surprisesActives() || s.pris[lieu] === jourMaison()) return null;
  return l.find(t => !s.trouvees[cleTrouvaille(lieu, t.id)]) || l[numeroJourSurprise() % l.length];
}
// today's visitor: {v, lieu, vu} one day out of two (the owl from the evening on), else null
function visiteurDuJour(p = P()){
  const n = numeroJourSurprise();
  if (n % 2 || !surprisesActives()) return null;
  const k = n / 2, v = VISITEURS[k % VISITEURS.length];
  if (v.soir && heureSurprise() < HEURE_DU_SOIR) return null;
  return {v, lieu: LIEUX_SURPRISES[(k * 5 + 1) % LIEUX_SURPRISES.length], vu: surprisesDe(p).vu === jourMaison()};
}
// the quarter where a mission of the day sends the child to look for what shines (js/missions.js), null once picked up
function lieuTresorDuJour(p = P()){
  const lieu = LIEUX_SURPRISES[1 + numeroJourSurprise() % (LIEUX_SURPRISES.length - 1)];
  return trouvailleDuJour(lieu, p) ? lieu : null;
}
// its word in a language, by its recording: the day's language when it has it, else the next one
function motSurprise(cle){
  if (!cle || typeof motDuJeu !== "function") return null;
  const d = LANGUES_JOUR.indexOf(langueDuJour());
  for (let k = 0; k < LANGUES_JOUR.length; k++) { const m = motDuJeu(cle, LANGUES_JOUR[(d + k) % LANGUES_JOUR.length]); if (m) return m; }
  return null;
}
// a find or a visitor as the thing's screen wants it (js/maison_chose.js): region is the place (its colours, the key
// of its étincelle of the day), habitant the one who tells (the quarter's resident, the Sage at the village)
function choseSurprise(sorte, lieu, x, intro){
  const m = motSurprise(x.mot), Q = QUARTIERS[lieu];
  return {...x, region: lieu, surprise: sorte, image: srcSurprise(sorte, x.id), habitant: Q ? Q.habitant : null, intro,
    mot: m ? m.mot : null, sens: m ? x.sens || m.sens : null, langue: m ? m.langue : null};
}
// a sticker given outside the missions, announced like theirs when the village is quiet (js/missions.js)
function donnerAutocollantSurprise(id, texte, p = P()){
  p.album = p.album && typeof p.album === "object" ? p.album : {};
  if (!Array.isArray(p.album.autocollants)) p.album.autocollants = [];
  p.album.autocollants.push({id, jour: jourMaison(), texte});
  MISSIONS_A_DIRE.push({m: {autocollant: id, texte}, qui: p.id});
}

// ---- what the child does
// the find of the place is picked up: the thing to show, and whether it is new
function ramasserTrouvaille(lieu, p = P()){
  const t = trouvailleDuJour(lieu, p); if (!t) return null;
  const s = surprisesDe(p), k = cleTrouvaille(lieu, t.id), nouvelle = !s.trouvees[k];
  s.trouvees[k] = (s.trouvees[k] || 0) + 1; s.pris[lieu] = jourMaison();
  const serie = !s.series[lieu] && TROUVAILLES[lieu].every(x => s.trouvees[cleTrouvaille(lieu, x.id)]);
  if (serie) { s.series[lieu] = true; donnerAutocollantSurprise("tresor", `Trouve les trois trésors ${duLieuSurprise(lieu)}.`, p); }
  if (typeof noterJournal === "function") noterJournal("trouvaille_village", {l: lieu, c: t.id, n: s.trouvees[k]});
  sauver();
  const petit = p.age <= 5, reste = TROUVAILLES[lieu].filter(x => !s.trouvees[cleTrouvaille(lieu, x.id)]).length;
  const suite = serie ? ` Tu as les trois trésors ${duLieuSurprise(lieu)} !` : nouvelle && reste && !petit ? ` Encore ${reste === 1 ? "un trésor" : reste + " trésors"} à trouver ${aLieuSurprise(lieu)} : reviens demain !` : "";
  return {t, nouvelle, serie, chose: choseSurprise("trouvaille", lieu, t, `${nouvelle ? "Tu as trouvé" : "Tu as retrouvé"} ${t.nom} !${suite} `)};
}
// the visitor is met: the thing to show
function rencontrerVisiteur(p = P()){
  const j = visiteurDuJour(p); if (!j) return null;
  const s = surprisesDe(p), v = j.v, premiere = !s.visiteurs[v.id];
  if (!j.vu) s.visiteurs[v.id] = (s.visiteurs[v.id] || 0) + 1;   // one visit a day, however often it is touched
  s.vu = jourMaison();
  const tous = !s.tous && VISITEURS.every(x => s.visiteurs[x.id]);
  if (tous) { s.tous = true; donnerAutocollantSurprise("jumelles", "Vois les cinq animaux de passage.", p); }
  if (typeof noterJournal === "function") noterJournal("visiteur", {l: j.lieu, c: v.id, n: s.visiteurs[v.id]});
  sauver();
  return {v, premiere, tous, chose: choseSurprise("visiteur", j.lieu, v, `Regarde, ${v.un} est de passage ${aLieuSurprise(j.lieu)} ! `)};
}
// what the Sage says once a day at the village: where the visitor is, and that something shines (null: already said)
function annonceSurprises(p = P()){
  const s = surprisesDe(p), j = visiteurDuJour(p), petit = p.age <= 5;
  if (s.annonce === jourMaison()) return null;
  const brille = LIEUX_SURPRISES.some(l => trouvailleDuJour(l, p));
  if (!brille && !(j && !j.vu)) return null;
  const animal = j && !j.vu ? `Aujourd'hui, ${j.v.un} est de passage ${aLieuSurprise(j.lieu)} : va vite ${j.v.id === "grenouille" || j.v.id === "souris" || j.v.id === "chouette" ? "la" : "le"} voir ! ` : "";
  const tresor = !brille ? "" : petit ? "Cherche ce qui brille dans l'herbe, près de la place : c'est un trésor !"
    : "Cherche ce qui brille dans l'herbe, près de la place, et par terre dans chaque quartier : il y a un trésor par jour !";
  return {texte: (animal + tresor).trim(), dit: () => { s.annonce = jourMaison(); sauver(); }};
}

// ---- the bag's album, page « Trouvailles » (js/album.js): the treasures of the village and its quarters, the
// visitors, then the finds of the island's reserved places (js/lieux.js)
function surprisesAlbum(p = P()){
  const s = surprisesDe(p);
  return [...LIEUX_SURPRISES.flatMap(lieu => TROUVAILLES[lieu].map(t => ({surprise: "trouvaille", lieu, x: t, gagnee: !!s.trouvees[cleTrouvaille(lieu, t.id)]}))),
    ...VISITEURS.map(v => ({surprise: "visiteur", x: v, gagnee: !!s.visiteurs[v.id], fois: s.visiteurs[v.id] || 0}))];
}
const toutesTrouvaillesAlbum = (p = P()) => [...surprisesAlbum(p), ...trouvaillesAlbum(p)];
function caseTrouvailleAlbum(t){
  if (!t.surprise) return caseTrouvaille(t);   // js/lieu_vue.js
  const b = el("button", "case-trouvaille" + (t.gagnee ? " gagnee" : "")), cadre = el("span", "cadre"), nom = el("b"), fig = document.createElement("img");
  fig.alt = ""; fig.draggable = false; imageReessayee(fig, srcSurprise(t.surprise, t.x.id));
  b.dataset.trouvaille = `${t.surprise}_${t.x.id}`;
  cadre.append(fig);
  nom.textContent = t.gagnee ? affiche(majuscule(t.x.nom.replace(/^(le |la |les |l')/, ""))) : "???";
  b.append(cadre, nom);
  b.onclick = () => {
    sfx.tap(); sauterDessin(fig);
    const m = t.gagnee ? motSurprise(t.x.mot) : null, mot = m ? ` « ${m.mot} », en ${m.nom} !` : "";
    const texte = t.surprise === "visiteur"
      ? (t.gagnee ? `${majuscule(t.x.nom)} : tu l'as ${t.x.nom.startsWith("la ") ? "vue" : "vu"} ${t.fois > 1 ? t.fois + " fois" : "une fois"} !${mot}`
        : "Un animal de passage : il en vient un, un jour sur deux. Regarde le plan du village !")
      : (t.gagnee ? `${majuscule(t.x.nom)} : ta trouvaille ${duLieuSurprise(t.lieu)} !${mot}` : `Un trésor qui brille par terre, ${aLieuSurprise(t.lieu)} : cherche bien !`);
    return m ? direMixte(texte, {langue: m.langue}) : dire(texte);
  };
  return b;
}
