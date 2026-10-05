/* Académia : le coin Parents. Il s'ouvre depuis le sac en gardant le doigt appuyé trois secondes sur « Parents » (une
   barre se remplit pendant l'appui ; un enfant qui touche ne l'ouvre pas). « Changer de Gardien » est dans l'en-tête.
   Onglets, chacun un tableau :
   1. Matières : par matière, la notion du moment, les notions réussies en jeu, la précision (à découvrir avant dix
      réponses), les erreurs gardées ; une ligne touchée montre les notions de la matière et leur état.
   2. Erreurs : les questions ratées, que le prochain chef d'Ombres reposera (revanche).
   3. Temps : par jour, le temps de jeu estimé d'après les combats, les combats, les bonnes réponses.
   4. Réglages : images (le jeu redémarre après accord), sauvegarde dans un fichier et reprise, état des images.
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
  $("parentsTitre").textContent = `👪 Parents · ${p.nom}`;
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
    t.ligne([`${r.icone} ${NOMS_MATIERES[m] || m}`, courante ? courante.titre : "", `${notionsReussies(m).length} sur ${ns.length}`,
      total >= 10 ? `${Math.round(100 * bonnes / total)} %` : "à découvrir", String((p.erreurs || []).filter(e => e.matiere === m).length)], "lien")
      .onclick = () => { sfx.tap(); vueNotions(v, r); };
  });
  v.append(t);
}
// the notions of one subject, in the order of the curriculum, and where the child stands on each
function vueNotions(v, r){
  const p = P(), m = r.matiere, courante = notionCourante(m);
  v.innerHTML = "";
  const retour = el("button", "joker retour-vue", `⬅ ${r.icone} ${NOMS_MATIERES[m] || m}`);
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
function vueErreurs(v){
  const l = [...(P().erreurs || [])].reverse();
  if (!l.length) return videParents(v, "Aucune erreur gardée : le prochain chef d'Ombres n'aura pas de revanche à proposer.");
  const t = tableauParents(["Matière", "Question", "Bonne réponse"]);
  l.forEach(q => t.ligne([NOMS_MATIERES[q.matiere] || q.matiere || "", [q.question, q.visuel].filter(Boolean).join(" "), String(q.reponse)]));
  v.append(t);
}
// time played per day, estimated from the fights (each one is dated when it ends): fights less than a quarter of an
// hour apart belong to the same sitting, which lasts from its first fight to its last, plus one fight
function vueTemps(v){
  const jours = new Map();
  [...(P().hist || [])].map(x => ({...x, ms: Date.parse(x.t)})).filter(x => x.ms).sort((a, b) => a.ms - b.ms).forEach(x => {
    const cle = new Date(x.ms).toLocaleDateString("fr-FR", {weekday: "short", day: "numeric", month: "short"});
    const j = jours.get(cle) || {combats: 0, bonnes: 0, total: 0, ms: 0, debut: null, dernier: null};
    if (j.dernier === null || x.ms - j.dernier > SEANCE_PAUSE_MS) { if (j.debut !== null) j.ms += j.dernier - j.debut + SEANCE_COMBAT_MS; j.debut = x.ms; }
    j.dernier = x.ms; j.combats++; j.bonnes += x.bonnes || 0; j.total += x.total || 0;
    jours.set(cle, j);
  });
  if (!jours.size) return videParents(v, "Pas encore de combat : le temps de jeu se compte à partir du premier.");
  const t = tableauParents(["Jour", "Temps de jeu", "Combats", "Bonnes réponses"], [1, 2, 3], [2]), somme = {ms: 0, combats: 0, bonnes: 0, total: 0};
  const minutes = ms => `≈ ${Math.max(1, Math.round(ms / 60000))} min`;
  [...jours.entries()].reverse().slice(0, 30).forEach(([jour, j]) => {
    const ms = j.ms + j.dernier - j.debut + SEANCE_COMBAT_MS;
    t.ligne([jour, minutes(ms), String(j.combats), `${j.bonnes} sur ${j.total}`]);
    somme.ms += ms; somme.combats += j.combats; somme.bonnes += j.bonnes; somme.total += j.total;
  });
  t.ligne(["Total", minutes(somme.ms), String(somme.combats), `${somme.bonnes} sur ${somme.total}`], "total");
  v.append(t);
}

function vueReglages(v){
  const g = el("div", "reglages"), ligne = (titre, ...contenu) => { const b = el("b"); b.textContent = titre; const c = el("div", "boutons"); c.append(...contenu); g.append(b, c); };
  const bouton = (texte, action, id) => { const b = el("button", "joker", texte); if (id) b.id = id; b.onclick = () => { sfx.tap(); action(); }; return b; };
  const autre = QUALITE === "hd" ? "leger" : "hd", nomQ = q => q === "hd" ? "haute définition" : "légères";
  ligne("Images", bouton(`🖼️ Passer aux images ${nomQ(autre)}`, () => dialogue("Parents", null,
    [`Le jeu va redémarrer avec les images ${nomQ(autre)}, d'accord ?`],
    [{texte: "✔ D'accord", action: () => { choisirQualite(autre); location.reload(); }}, {texte: "✖ Non", action: () => {}}], {instantane: true}), "btnImages"));
  const fichier = el("input"); fichier.type = "file"; fichier.accept = ".json,application/json"; fichier.hidden = true;
  fichier.onchange = () => { if (fichier.files[0]) reprendrePartie(fichier.files[0]); fichier.value = ""; };
  ligne("Sauvegarde", bouton("💾 Enregistrer dans un fichier", enregistrerPartie), bouton("📂 Reprendre depuis un fichier", () => fichier.click()), fichier);
  const diag = el("small", "diag"); diag.textContent = diagnosticImages();
  ligne("État", diag);
  v.append(g);
}
// for the grown-ups: how the pictures are drawn, and any that could not be loaded
function diagnosticImages(){
  const r = JEU.renderer, moteur = r && r.type === Phaser.WEBGL ? `WebGL ${TEXTURE_MAX}` : "sans carte graphique", manque = [...ECHECS];
  return `Version ${VERSION} · images ${QUALITE === "hd" ? "haute définition" : "légères"} · ${moteur} · `
    + (manque.length ? `dessins non chargés : ${manque.join(", ")} (réseau ?)` : "tous les dessins sont chargés");
}

// the child's game in a file, to keep it or to carry it to another tablet; and back from such a file
function enregistrerPartie(){
  const p = P(), jour = new Date().toISOString().slice(0, 10);
  const blob = new Blob([JSON.stringify({jeu: "academia", version: VERSION, enregistre: new Date().toISOString(), profil: p}, null, 1)], {type: "application/json"});
  const a = el("a"); a.href = URL.createObjectURL(blob); a.download = `academia-${normNom(p.nom).replace(/[^a-z0-9]+/g, "-")}-${jour}.json`;
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
