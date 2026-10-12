/* Académia : le coin Parents. Il s'ouvre depuis le sac en gardant le doigt appuyé trois secondes sur « Parents » (une
   barre se remplit pendant l'appui ; un enfant qui touche ne l'ouvre pas). « Changer de Gardien » est dans l'en-tête.
   Onglets, chacun un tableau :
   1. Matières : par matière, la notion du moment, les notions réussies en jeu, la précision (à découvrir avant dix
      réponses), les erreurs gardées ; une ligne touchée montre les notions de la matière et leur état.
   2. Erreurs : le classement des erreurs fréquentes, d'abord les notions qui résistent (le plus d'erreurs parmi leurs
      huit dernières réponses), puis les questions gardées que le prochain chef d'Ombres reposera (revanche).
   3. Temps : par jour, le temps de jeu réel (js/temps.js), estimé d'après les combats pour les jours d'avant ce
      compteur ; les combats, les bonnes réponses.
   4. Réglages : images (le jeu redémarre après accord), musique de ce Gardien (js/sons.js), sauvegarde dans un
      fichier et reprise, état des images.
   Tout vient de la partie enregistrée (js/etat.js) : ce coin ne note rien de nouveau. */
const APPUI_PARENTS_MS = 3000, SEANCE_PAUSE_MS = 15 * 60000, SEANCE_COMBAT_MS = 3 * 60000;
if (!ECRANS.includes("parents")) ECRANS.push("parents");   // one more screen for montrer() (js/outils.js)
const NOMS_MATIERES = {maths: "Maths", anglais: "Anglais", allemand: "Allemand", luxembourgeois: "Luxembourgeois", logique: "Logique", sciences: "Sciences"};

// a long press: a bar fills while the finger stays; full, the button is ready and acts when the finger lifts (a moment
// later: the click that ends the press lands on this button, not on what opens); released before, a hint for the
// grown-ups
function appuiLong(b, ms, action){
  let t0 = 0, raf = 0, pret = false;
  const remettre = () => { cancelAnimationFrame(raf); raf = 0; pret = false; b.style.setProperty("--appui", 0); b.classList.remove("pret"); };
  const avance = () => {
    const k = Math.min(1, (performance.now() - t0) / ms); b.style.setProperty("--appui", k);
    if (k < 1) { raf = requestAnimationFrame(avance); return; }
    raf = 0; pret = true; b.classList.add("pret");
  };
  // pointer events where there are, mouse and touch events where a browser sends only those (one start per press)
  const debut = e => { if (e.cancelable) e.preventDefault(); if (raf || pret) return; t0 = performance.now(); raf = requestAnimationFrame(avance); };
  const lache = () => {
    if (pret) { remettre(); setTimeout(action, 80); }
    else if (raf) { remettre(); toast("Coin des parents : garder le doigt appuyé 3 secondes", 2400); }
  };
  ["pointerdown", "mousedown"].forEach(t => b.addEventListener(t, debut));
  b.addEventListener("touchstart", debut, {passive: false});
  ["pointerup", "pointercancel", "pointerleave", "mouseup", "mouseleave", "touchend", "touchcancel"].forEach(t => b.addEventListener(t, lache));
  b.addEventListener("contextmenu", e => e.preventDefault());
}

function ouvrirParents(){
  const p = P(); if (!p) return;
  taire(); montrer("parents");
  ecrireIcones($("parentsTitre"), `👪 Parents · ${p.nom}`);   // drawn icons (js/icones.js)
  ongletParents("matieres");
}
function fermerParents(){ window.__calme = performance.now() + 250; montrer("monde"); }
function ongletParents(nom){
  document.querySelectorAll("#ongletsParents button").forEach(b => b.setAttribute("aria-selected", String(b.dataset.onglet === nom)));
  const v = $("vueParents"); v.innerHTML = ""; v.scrollTop = 0;
  ({matieres: vueMatieres, erreurs: vueErreurs, temps: vueTemps, reglages: vueReglages})[nom](v);
}

// a table: headers, then rows of cells (text, or an element); `nombres`: the columns aligned right; `secondaires`: the
// columns a phone leaves out (on a phone, each row is stacked: its first cell, then the others after their name)
function tableauParents(entetes, nombres = [], secondaires = []){
  const t = el("table", "tableau"), tete = el("tr");
  const cls = i => [nombres.includes(i) ? "nombre" : "", secondaires.includes(i) ? "secondaire" : ""].join(" ").trim();
  entetes.forEach((h, i) => { const th = el("th", cls(i)); th.textContent = h; tete.append(th); });
  const thead = el("thead"); thead.append(tete); t.append(thead, el("tbody"));
  t.ligne = (cellules, classe = "") => {
    const tr = el("tr", classe);
    cellules.forEach((c, i) => {
      const td = el("td", cls(i)); td.dataset.label = entetes[i];   // a phone shows each value under its column's name
      if (c instanceof Node) td.append(c); else td.textContent = c;
      tr.append(td);
    });
    t.tBodies[0].append(tr); return tr;
  };
  return t;
}
const videParents = (v, texte) => { const p = el("p", "vide-parents"); p.textContent = texte; v.append(p); };
const combatsDe = matiere => (P().hist || []).filter(x => { const r = regionDe(x.region); return r && r.matiere === matiere; });

function vueMatieres(v){
  const p = P(), t = tableauParents(["Matière", "Notion du moment", "Réussies en jeu", "Précision", "Erreurs gardées"], [2, 3, 4], [4]);
  REGIONS.forEach(r => {
    const m = r.matiere, ns = notionsDe(m), h = combatsDe(m);
    const bonnes = h.reduce((n, x) => n + (x.bonnes || 0), 0), total = h.reduce((n, x) => n + (x.total || 0), 0);
    const courante = ns.length ? notionCourante(m) : null;
    t.ligne([ecrireIcones(el("span"), `${r.icone} ${NOMS_MATIERES[m] || m}`), courante ? courante.titre : "", `${notionsReussies(m).length} sur ${ns.length}`,
      total >= 10 ? `${Math.round(100 * bonnes / total)} %` : "à découvrir", String((p.erreurs || []).filter(e => e.matiere === m).length)], "lien")
      .onclick = () => { sfx.tap(); vueNotions(v, r); };
  });
  v.append(t);
}
// the notions of one subject, in the order of the curriculum, and where the child stands on each
function vueNotions(v, r){
  const p = P(), m = r.matiere, courante = notionCourante(m);
  v.innerHTML = "";
  const retour = ecrireIcones(el("button", "joker retour-vue"), `⬅ ${r.icone} ${NOMS_MATIERES[m] || m}`);
  retour.onclick = () => { sfx.tap(); ongletParents("matieres"); };
  const t = tableauParents(["Notion", "État", "Bonnes", "Erreurs", "Âge"], [2, 3, 4], [3, 4]);
  notionsDe(m).forEach(n => {
    const e = p.maitrise[n.id], reussie = reussieEnJeu(n);
    const [etat, cls] = reussie ? ["réussie en jeu", "en-jeu"] : courante && n.id === courante.id ? ["notion du moment", "du-moment"]
      : e && e.acquis ? ["sue au départ", "au-depart"] : e && e.ok + e.ko ? ["en travail", "en-travail"] : ["pas encore", ""];
    const badge = el("span", "etat-notion " + cls); badge.textContent = etat;
    t.ligne([n.titre, badge, e ? String(e.ok) : "", e ? String(e.ko) : "", `${ageMin(n)} ans`]);
  });
  v.append(retour, t);
}
// the mistakes, ranked: first the notions that resist (the most mistakes among their last eight answers, then in all),
// then the questions kept for the bosses' revenge, the same question counted once with how often it was missed
const titreParents = (v, texte) => { const h = el("h3", "titre-parents"); h.textContent = texte; v.append(h); };
const noteParents = (v, texte) => { const n = el("p", "note-parents"); n.textContent = texte; v.append(n); };
function vueErreurs(v){
  const p = P(), maitrise = p.maitrise || {}, compteFautes = s => [...String(s || "")].filter(c => c === "0").length;
  const resistent = Object.entries(maitrise).map(([id, e]) => ({n: notionDe(id), e})).filter(x => x.n && x.e && x.e.ko > 0)
    .map(x => ({...x, recentes: compteFautes(String(x.e.hist || "").slice(-8)), vues: String(x.e.hist || "").slice(-8).length}))
    .filter(x => maitrisee(x.n) ? x.recentes >= 2 : true)   // a notion known that slipped once does not resist
    .sort((a, b) => b.recentes - a.recentes || b.e.ko - a.e.ko || a.e.ok - b.e.ok).slice(0, 12);
  const gardees = new Map();
  (p.erreurs || []).forEach((q, i) => {
    const cle = [q.matiere, q.question, q.visuel, q.reponse].join("|"), g = gardees.get(cle) || {q, fois: 0, dernier: 0};
    g.fois++; g.dernier = i; gardees.set(cle, g);
  });
  const reparees = ((p.album || {}).reparees || []).length;
  if (!resistent.length && !gardees.size) return videParents(v, "Aucune erreur pour l'instant : rien ne résiste, et le prochain chef d'Ombres n'aura pas de revanche à proposer."
    + (reparees ? ` Erreurs réparées en revanche : ${reparees}.` : ""));
  if (resistent.length) {
    titreParents(v, "Les notions qui résistent");
    const t = tableauParents(["Notion", "Matière", "Dernières réponses", "Erreurs", "État"], [3], [1, 4]);
    resistent.forEach(({n, e, recentes, vues}) => {
      const dernieres = el("span", "dernieres");   // the last eight answers, oldest first: green right, red missed
      [...String(e.hist || "").slice(-8)].forEach(c => dernieres.append(el("i", c === "1" ? "juste" : "rate")));
      dernieres.append(` ${recentes} ratée${recentes > 1 ? "s" : ""} sur ${vues}`);
      const etat = el("span", "etat-notion " + (maitrisee(n) ? "au-depart" : e.chute ? "en-travail" : "du-moment"));
      etat.textContent = maitrisee(n) ? "sue, à revoir" : e.chute ? "retombée" : "en travail";
      t.ligne([n.titre, NOMS_MATIERES[n.matiere] || n.matiere, dernieres, String(e.ko), etat]);
    });
    v.append(t);
  }
  if (gardees.size) {
    titreParents(v, "Les questions gardées pour la revanche");
    const t = tableauParents(["Question", "Bonne réponse", "Matière", "Fois"], [3], [2]);
    [...gardees.values()].sort((a, b) => b.fois - a.fois || b.dernier - a.dernier).forEach(({q, fois}) =>
      t.ligne([[q.question, q.visuel].filter(Boolean).join(" "), String(q.reponse), NOMS_MATIERES[q.matiere] || q.matiere || "", String(fois)]));
    v.append(t);
  }
  if (reparees) noteParents(v, `Erreurs réparées en revanche : ${reparees} (un tampon chacune dans le carnet de l'album, au sac).`);
}
// time played per day: really counted (js/temps.js) from the day the counter came; before, estimated from the fights
// (each one is dated when it ends): fights less than a quarter of an hour apart belong to the same sitting, which lasts
// from its first fight to its last, plus one fight
function vueTemps(v){
  const p = P(), jours = new Map(), cle = ms => typeof jourTemps === "function" ? jourTemps(ms) : new Date(ms).toISOString().slice(0, 10);
  const prendre = d => jours.get(d) || jours.set(d, {combats: 0, bonnes: 0, total: 0, ms: 0, debut: null, dernier: null, reel: 0}).get(d);
  [...(p.hist || [])].map(x => ({...x, ms: Date.parse(x.t)})).filter(x => x.ms).sort((a, b) => a.ms - b.ms).forEach(x => {
    const j = prendre(cle(x.ms));
    if (j.dernier === null || x.ms - j.dernier > SEANCE_PAUSE_MS) { if (j.debut !== null) j.ms += j.dernier - j.debut + SEANCE_COMBAT_MS; j.debut = x.ms; }
    j.dernier = x.ms; j.combats++; j.bonnes += x.bonnes || 0; j.total += x.total || 0;
  });
  Object.entries(p.temps || {}).forEach(([d, ms]) => { if (/^\d{4}-\d\d-\d\d$/.test(d) && ms > 0) prendre(d).reel = ms; });
  if (!jours.size) return videParents(v, "Pas encore de temps de jeu : il se compte dès que l'enfant joue.");
  const duree = ms => { const m = Math.round(ms / 60000); return m < 1 ? "< 1 min" : m < 60 ? `${m} min` : `${Math.floor(m / 60)} h ${String(m % 60).padStart(2, "0")}`; };
  const lignes = [...jours.entries()].sort((a, b) => b[0].localeCompare(a[0])).slice(0, 30).map(([d, j]) => {
    const estime = j.debut === null ? 0 : j.ms + j.dernier - j.debut + SEANCE_COMBAT_MS;
    return {d, j, ms: j.reel || estime, estime: !j.reel};
  });
  // the last seven days at a glance
  const semaine = lignes.filter(l => l.d >= cle(Date.now() - 6 * 864e5)), somme = semaine.reduce((n, l) => n + l.ms, 0);
  if (semaine.length) noteParents(v, `Ces sept derniers jours : ${semaine.some(l => l.estime) ? "≈ " : ""}${duree(somme)} de jeu, en ${semaine.length} jour${semaine.length > 1 ? "s" : ""}.`);
  const t = tableauParents(["Jour", "Temps de jeu", "Combats", "Bonnes réponses"], [1, 2, 3], [2]), total = {ms: 0, combats: 0, bonnes: 0, total: 0, estime: false};
  const nomJour = d => { const [a, m, j] = d.split("-").map(Number); return new Date(a, m - 1, j).toLocaleDateString("fr-FR", {weekday: "short", day: "numeric", month: "short"}); };
  lignes.forEach(({d, j, ms, estime}) => {
    t.ligne([d === cle(Date.now()) ? `Aujourd'hui (${nomJour(d)})` : nomJour(d), (estime ? "≈ " : "") + duree(ms), String(j.combats), j.total ? `${j.bonnes} sur ${j.total}` : ""]);
    total.ms += ms; total.combats += j.combats; total.bonnes += j.bonnes; total.total += j.total; total.estime = total.estime || estime;
  });
  t.ligne(["Total", (total.estime ? "≈ " : "") + duree(total.ms), String(total.combats), total.total ? `${total.bonnes} sur ${total.total}` : ""], "total");
  v.append(t);
  noteParents(v, "Le temps compte quand la page est ouverte et que l'enfant a touché l'écran dans les deux dernières minutes. "
    + "« ≈ » : estimé d'après les combats, pour les jours d'avant ce compteur.");
}

function vueReglages(v){
  const g = el("div", "reglages"), ligne = (titre, ...contenu) => { const b = el("b"); b.textContent = titre; const c = el("div", "boutons"); c.append(...contenu); g.append(b, c); };
  const bouton = (texte, action, id) => { const b = ecrireIcones(el("button", "joker"), texte); if (id) b.id = id; b.onclick = () => { sfx.tap(); action(); }; return b; };
  const autre = QUALITE === "hd" ? "leger" : "hd", nomQ = q => q === "hd" ? "haute définition" : "légères";
  ligne("Images", bouton(`🖼️ Passer aux images ${nomQ(autre)}`, () => dialogue("Parents", null,
    [`Le jeu va redémarrer avec les images ${nomQ(autre)}, d'accord ?`],
    [{texte: "✔ D'accord", action: () => { choisirQualite(autre); location.reload(); }}, {texte: "✖ Non", action: () => {}}], {instantane: true}), "btnImages"));
  if (typeof boutonMusique === "function") ligne("Musique", boutonMusique());
  const fichier = el("input"); fichier.type = "file"; fichier.accept = ".json,application/json"; fichier.hidden = true;
  fichier.onchange = () => { if (fichier.files[0]) reprendrePartie(fichier.files[0]); fichier.value = ""; };
  ligne("Sauvegarde", bouton("💾 Enregistrer dans un fichier", enregistrerPartie, "btnEnregistrer"), bouton("📂 Reprendre depuis un fichier", () => fichier.click()), fichier);
  const diag = el("small", "diag"); diag.textContent = diagnosticImages();
  ligne("État", diag);
  // whether the browser promised never to erase the games (js/etat.js asks it when the game loads)
  if (navigator.storage && navigator.storage.persisted) navigator.storage.persisted()
    .then(ok => { diag.textContent += ok ? " · partie protégée de l'effacement" : " · partie non protégée (installer le jeu depuis Chrome)"; }).catch(() => {});
  v.append(g);
}
// for the grown-ups: how the pictures are drawn, and any that could not be loaded
function diagnosticImages(){
  const r = JEU.renderer, moteur = r && r.type === Phaser.WEBGL ? `WebGL ${TEXTURE_MAX}` : "sans carte graphique", manque = [...ECHECS];
  const app = !window.AcademiaAndroid ? "" : ` · application Android ${AcademiaAndroid.version()}`
    + (AcademiaAndroid.pret() ? "" : " · aucune voix sur cet appareil (activer « Services vocaux Google » dans les réglages)");
  return `Version ${VERSION}${app} · images ${QUALITE === "hd" ? "haute définition" : "légères"} · ${moteur} · `
    + (manque.length ? `dessins non chargés : ${manque.join(", ")} (réseau ?)` : "tous les dessins sont chargés");
}

// the child's game in a file, to keep it or to carry it to another tablet; and back from such a file
function enregistrerPartie(){
  const p = P(), jour = new Date().toISOString().slice(0, 10);
  const contenu = JSON.stringify({jeu: "academia", version: VERSION, enregistre: new Date().toISOString(), profil: p}, null, 1);
  const nom = `academia-${normNom(p.nom).replace(/[^a-z0-9]+/g, "-")}-${jour}.json`;
  if (window.AcademiaAndroid) {   // the Android app writes it in Downloads (android/, Pont.java)
    const ok = AcademiaAndroid.enregistrer(nom, contenu);
    return toast(ok ? `Partie de ${p.nom} enregistrée dans Téléchargements` : "La partie n'a pas pu être enregistrée");
  }
  const blob = new Blob([contenu], {type: "application/json"});
  const a = el("a"); a.href = URL.createObjectURL(blob); a.download = nom;
  document.body.append(a); a.click(); a.remove(); setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  toast(`Partie de ${p.nom} enregistrée dans un fichier`);
}
async function reprendrePartie(f){
  let d = null;
  try { d = JSON.parse(await f.text()); } catch (e) {}
  const p = d && d.jeu === "academia" && d.profil;
  if (!p || !p.id || !p.nom || !Array.isArray(p.compagnons) || typeof p.maitrise !== "object") return toast("Ce fichier n'est pas une partie d'Académia");
  const poser = () => {   // what an older file lacks is completed, as for a new game
    E.profils[p.id] = Object.assign(nouveauProfil(p.nom, p.age), p);
    choisirProfil(p.id); taire(); JEU.scene.stop("monde"); ouvrirAccueil(); toast(`Partie de ${p.nom} reprise`);
  };
  if (!E.profils[p.id]) return poser();
  dialogue("Parents", null, [`La partie de ${p.nom} est déjà sur cet appareil. La remplacer par celle du fichier ?`],
    [{texte: "✔ Remplacer", action: poser}, {texte: "✖ Non", action: () => {}}], {instantane: true});
}
