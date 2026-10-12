/* Académia : les onglets Badges et Album du sac (js/equipe.js ouvre le sac et dessine l'onglet Équipe).
   1. Badges : la médaille de chaque maison (assets/hd/icones/medaille_*.png), grisée tant que son chef n'est pas battu,
      avec ses étoiles (une à trois, js/progression.js) ; toucher une médaille dit son nom et sa maison.
   2. Album, en quatre pages : les Ombres consolées (P().album.ombres : silhouette avant, couleur et compteur après),
      les Ombres brillantes (P().album.brillantes), les autocollants des missions du Sage (P().album.autocollants), le
      carnet des erreurs réparées (P().album.reparees : un tampon par erreur, la question et sa bonne réponse). Le
      combat et les missions remplissent ces champs ; un album vide montre les places qui attendent.
   Rien ne défile : une page trop pleine pour l'écran se tourne avec deux flèches. Toucher un dessin le fait dire. */
const regionLue = id => (P().regions || {})[id] || {niveau: 1, etape: 0, badges: 0};   // read only: nothing written
// a badge's stars: the best kept by the region (js/progression.js), or the best of its boss fights (older games)
function etoilesBadge(id){
  const st = regionLue(id);
  if (!(st.badges > 0)) return 0;
  const vues = (P().hist || []).filter(x => x.region === id && x.boss && x.gagne).map(x => x.etoiles || 0);
  return Math.max(1, Math.min(3, Math.max(st.etoiles || 0, ...vues)));
}
const etoilesSac = n => { const l = el("span", "etoiles-sac"); for (let i = 0; i < 3; i++) l.append(el("span", i < n ? "pleine" : "vide", ETOILE_SVG)); return l; };
const unDeux = n => n === 1 ? "une" : n === 2 ? "deux" : String(n);
const badgesGagnes = () => REGIONS.filter(r => regionLue(r.id).badges > 0).length;
// what the voice says when the Badges tab is touched: how many are won (the count is also written, for the big one)
function texteOngletBadges(){
  const n = badgesGagnes();
  return !n ? "Tes badges ! Chaque chef des Ombres en garde un : touche une médaille pour savoir où."
    : n === REGIONS.length ? "Tes badges ! Tu les as tous gagnés, bravo !" : `Tes badges ! Tu en as ${n} sur ${REGIONS.length}.`;
}

function dessinerBadges(v){
  v.replaceChildren();
  const gagnes = badgesGagnes(), compte = el("p", "compte-sac");
  compte.append(iconeDom("trophee"), ` ${gagnes} badge${gagnes > 1 ? "s" : ""} sur ${REGIONS.length}`);
  const g = el("div", "grille-badges");
  REGIONS.forEach(r => g.append(tuileBadge(r)));
  v.append(compte, g);
}
function tuileBadge(r){
  const st = regionLue(r.id), gagne = st.badges > 0, n = etoilesBadge(r.id);
  const b = el("button", "badge-sac" + (gagne ? " gagne" : "")), med = el("span", "medaille-sac"), nom = el("b");
  b.dataset.region = r.id;
  med.append(iconeDom(`medaille_${r.id}`));
  if (st.badges > 1) med.append(el("span", "fois", `×${st.badges}`));   // its chief beaten again, at a higher level
  nom.append(iconeDom(`maison_${r.id}`), " " + affiche(r.badge));
  b.append(med, etoilesSac(n), nom);
  b.setAttribute("aria-label", `${r.badge}, ${r.nom}`);
  b.onclick = () => { sfx.tap(); sauterDessin(med); dire(texteBadge(r, gagne, n, st.badges)); };
  return b;
}
function texteBadge(r, gagne, n, fois = 1){
  const a = A_REGION[r.id] || "à ";
  if (!gagne) return `Le ${r.badge} t'attend ${a}${r.nom} : ${r.chef} le garde !`;
  return `Le ${r.badge} ! Tu l'as gagné ${a}${r.nom}${fois > 1 ? `, ${unDeux(fois)} fois` : ""}. ` + (n === 3 ? "Trois étoiles : bravo !"
    : `${n === 2 ? "Deux étoiles" : "Une étoile"} ! Reviens battre ${r.chef} : ${n === 2 ? "la troisième étoile t'attend" : "deux étoiles t'attendent"} !`);
}

/* ---- the album */
const ESPECES = OMBRES.map(nom => ({nom, cle: CLES_OMBRES[nom]}));
// the kind of an Ombre noted by the fight: its drawing's key or its name, perhaps with its region (« grisouille_albion »)
function especeDe(k){
  const s = normNom(String(k)), meme = x => ESPECES.find(e => x === e.cle || x === normNom(e.nom));
  return meme(s) || meme(s.split(/[_.:@/ -]/)[0]) || null;
}
function parEspece(table, compter){
  const n = {};
  Object.entries(table || {}).forEach(([k, v]) => {
    const e = especeDe(k); if (!e || !v) return;
    n[e.cle] = (n[e.cle] || 0) + (compter ? (typeof v === "number" ? Math.max(0, Math.floor(v)) : 1) : 1);
  });
  return n;
}
const PAGES_ALBUM = [
  {id: "ombres", titre: "Ombres", dit: "Les Ombres que tu as consolées !"},
  {id: "brillantes", titre: "Brillantes", dit: "Les Ombres brillantes : elles sont très rares !", icone: "etincelle"},
  {id: "autocollants", titre: "Autocollants", dit: "Tes autocollants des missions !", icone: "autocollant"},
  {id: "carnet", titre: "Carnet", dit: "Ton carnet des erreurs réparées !", icone: "tampon"}];
let pageAlbum = "ombres";
const feuillets = {};   // album page -> the leaf open (a page too full for the screen has several)

function dessinerAlbum(v){
  const a = P().album || {}, consolees = parEspece(a.ombres, true), brillantes = parEspece(a.brillantes, false);
  const liste = l => (Array.isArray(l) ? l : []).filter(Boolean).reverse();   // the latest first; a damaged entry left out
  const autocollants = liste(a.autocollants), reparees = liste(a.reparees).filter(r => typeof r === "object");
  const comptes = {ombres: `${Object.keys(consolees).length}/${ESPECES.length}`, brillantes: `${Object.keys(brillantes).length}/${ESPECES.length}`,
    autocollants: String(autocollants.length), carnet: String(reparees.length)};
  v.replaceChildren();
  const tete = el("div", "tete-album"), nav = el("div", "pages-album");
  nav.setAttribute("role", "tablist");
  PAGES_ALBUM.forEach(pg => {
    const b = el("button", "page-album"), ico = el("span", "ico-page"), t = el("span", "titre-page");
    b.dataset.page = pg.id; b.setAttribute("role", "tab"); b.setAttribute("aria-selected", String(pg.id === pageAlbum));
    ico.append(pg.icone ? iconeDom(pg.icone) : imageHD("grisouille", "face", 30));
    t.textContent = pg.titre;
    b.append(ico, t, el("span", "compte-page", comptes[pg.id]));
    b.onclick = () => { sfx.tap(); pageAlbum = pg.id; dire(pg.dit); dessinerAlbum(v); };
    nav.append(b);
  });
  const fleches = el("div", "fleches-album");
  tete.append(nav, fleches);
  const feuille = el("div", "feuille-album " + pageAlbum);
  v.append(tete, feuille);
  if (pageAlbum === "ombres") paginer(feuille, fleches, v, ESPECES, e => caseOmbre(e, consolees[e.cle] || 0, false));
  else if (pageAlbum === "brillantes") paginer(feuille, fleches, v, ESPECES, e => caseOmbre(e, brillantes[e.cle] || 0, true));
  else if (pageAlbum === "autocollants") paginer(feuille, fleches, v, autocollants, caseAutocollant, videAutocollants);
  else paginer(feuille, fleches, v, reparees, caseReparee, videCarnet);
}
// the items of the open leaf that fit whole in the page (measured on the screen), and two arrows when there are more
function paginer(feuille, fleches, v, items, dessiner, vide){
  const avant = el("button", "rond fleche-album avant"), apres = el("button", "rond fleche-album apres"), num = el("span", "num-feuillet");
  avant.append(iconeDom("retour")); apres.append(iconeDom("suivant"));
  avant.setAttribute("aria-label", "Page d'avant"); apres.setAttribute("aria-label", "Page d'après");
  fleches.append(avant, num, apres);
  if (!items.length) { fleches.hidden = true; feuille.append(vide()); return; }
  const grille = el("div", "grille-album");
  feuille.append(grille);
  grille.append(...items.map(dessiner));
  const bas = feuille.getBoundingClientRect().bottom + 1, droite = feuille.getBoundingClientRect().right + 1;
  const par = Math.max(1, [...grille.children].filter(c => { const r = c.getBoundingClientRect(); return r.bottom <= bas && r.right <= droite; }).length);
  const nb = Math.ceil(items.length / par), k = Math.max(0, Math.min(feuillets[pageAlbum] || 0, nb - 1));
  feuillets[pageAlbum] = k;
  if (nb <= 1) { fleches.hidden = true; return; }   // measured with the arrows in place: without them, more room still
  grille.replaceChildren(...items.slice(k * par, k * par + par).map(dessiner));
  num.textContent = `${k + 1}/${nb}`;
  avant.disabled = k === 0; apres.disabled = k === nb - 1;
  const tourner = d => { sfx.tap(); if (typeof jouerSon === "function") jouerSon("bouton"); feuillets[pageAlbum] = k + d; dessinerAlbum(v); };
  avant.onclick = () => tourner(-1); apres.onclick = () => tourner(1);
}

// an Ombre's place: its silhouette, then its drawing in colour and how many times it was consoled; a shiny one glows
function caseOmbre(e, n, brillante){
  const b = el("button", "case-ombre" + (n ? " vue" : "") + (brillante ? " brillante" : "")), cadre = el("span", "cadre");
  const fig = imageHD(e.cle, "face", 120), nom = el("b");
  b.dataset.espece = e.cle;
  cadre.append(fig);
  if (brillante && n) ["etincelle", "etoile_p", "etincelle_p"].forEach((k, i) => { const s = el("img", "eclat e" + (i + 1)); s.alt = ""; s.src = pieceDom(k); cadre.append(s); });
  nom.textContent = n ? affiche(e.nom) : "???";
  b.append(cadre, nom);
  if (n && !brillante) b.append(el("span", "compteur", `×${n}`));
  b.onclick = () => {
    sfx.tap(); sauterDessin(fig);
    if (brillante) dire(n ? `Une Ombre brillante : ${e.nom} ! C'est très rare, bravo !`
      : "Une Ombre brillante se cache encore. Elles sont très rares : ouvre l'œil dans les hautes herbes !");
    else dire(n ? `${e.nom} ! Tu as consolé ${e.nom} ${unDeux(n)} fois.` : "Une Ombre que tu n'as pas encore consolée. Elle se cache dans les hautes herbes !");
  };
  return b;
}

// a sticker: its drawing from the missions' catalogue when there is one (AUTOCOLLANTS), else the game's icon of that
// name, else one of the game's drawings chosen by its name (always the same for the same sticker)
const DESSINS_AUTOCOLLANTS = ["etoile", "couronne", "trophee", "cadeau", "fusee", "confettis", "chouette", "etincelle", "pattes", "potion", "bouclier", "pouce"];
const jourLisible = (t, mois = "short") => { const d = t ? new Date(String(t).length === 10 ? t + "T12:00:00" : t) : null;
  return d && !isNaN(d) ? d.toLocaleDateString("fr-FR", {day: "numeric", month: mois}) : ""; };
function infoAutocollant(a){
  const id = String((a && (a.id || a.autocollant)) || a || ""), cat = typeof AUTOCOLLANTS === "object" && AUTOCOLLANTS ? AUTOCOLLANTS[id] : null;
  const m = ((P().missions || {}).liste || []).find(x => x && (x.autocollant === id || x.id === id));
  let h = 0; for (const c of id) h = (h * 31 + c.charCodeAt(0)) >>> 0;
  return {id, jour: a && (a.jour || a.t), image: cat && cat.image, nom: (cat && cat.nom) || (a && a.nom) || "",
    icone: (cat && cat.icone) || (ICONES.includes(id) ? id : DESSINS_AUTOCOLLANTS[h % DESSINS_AUTOCOLLANTS.length]),
    texte: (m && m.texte) || (a && a.texte) || ""};
}
function caseAutocollant(a, i){
  const x = infoAutocollant(a), b = el("button", "autocollant"), s = el("span", "collant"), d = el("small");
  if (x.image) { const im = el("img"); im.alt = ""; im.src = x.image; s.append(im); } else s.append(iconeDom(x.icone));
  b.style.setProperty("--penche", `${((x.id.length * 7 + (i || 0) * 5) % 13) - 6}deg`);
  d.textContent = jourLisible(x.jour);
  b.append(s, d);
  b.onclick = () => {
    sfx.tap(); sauterDessin(s);
    const quand = jourLisible(x.jour, "long"), mission = sansEmoji(x.texte).replace(/[\s.!]+$/, "");   // « Console trois Ombres. »
    dire(`${x.nom ? `L'autocollant ${x.nom}` : "Un autocollant"}${quand ? `, gagné le ${quand}` : ""} !${mission ? ` Ta mission : ${mission}.` : ""}`);
  };
  return b;
}
function videAutocollants(){
  const d = el("div", "album-vide"), places = el("div", "places");
  for (let i = 0; i < 5; i++) {
    const b = el("button", "autocollant attend"); b.append(el("span", "collant"));
    b.firstChild.append(iconeDom("autocollant"));
    b.onclick = () => { sfx.tap(); sauterDessin(b.firstChild); dire("Ici, tu colleras les autocollants des missions du Sage. Va le voir sur la place !"); };
    places.append(b);
  }
  const t = el("p"); t.textContent = "Les missions du Sage donnent des autocollants.";
  d.append(places, t);
  return d;
}

// the notebook: one stamp per mistake repaired, the question and its right answer drawn as in the fight. The fight may
// note only {question, reponse, matiere, t, legende}: the written question it came from gives back its language, its
// pictures' names and its explanation (a calculated one has none, and needs none)
const banqueCarnet = {n: -1, m: new Map()};
function completerReparee(r){
  if (banqueCarnet.n !== Q.charges.size) {   // the bank indexed again once more of it has arrived
    banqueCarnet.n = Q.charges.size; banqueCarnet.m.clear();
    [...Object.values(Q.parNotion), ...Object.values(Q.vraifaux)].forEach(l => l.forEach(q => banqueCarnet.m.set(`${q.question}|${q.reponse}`, q)));
  }
  const q = banqueCarnet.m.get(`${r.question}|${r.reponse}`);
  return q ? {...q, ...r} : r;
}
function reponseDessinee(r){
  const v = String(r.reponse == null ? "" : r.reponse), s = el("span", "reponse-carnet");
  if (v === "vrai" || v === "faux") { s.append(iconeDom(v), v === "vrai" ? " Vrai" : " Faux"); return s; }
  try { s.append(contenuReponse(v, r, false).contenu); } catch (e) { s.textContent = v; }
  return s;
}
function direReparee(r){
  const lang = r.oreille || r.langue || "fr", v = String(r.reponse == null ? "" : r.reponse), leg = r.legende || (r.legendes && r.legendes[v]);
  const rep = leg || sansEmoji(v), dite = lang === "fr" || leg ? rep : `« ${rep} »`;
  const suite = r.explication || [r.question, v === "vrai" || v === "faux" ? `C'est ${v} !` : rep ? `La bonne réponse : ${dite} !` : ""].filter(Boolean).join(" ");
  const t = "Erreur réparée, bravo ! " + suite;
  if (typeof direMixte === "function") direMixte(t, {...r, langue: r.langue || "fr"}); else dire(t);
}
function caseReparee(r0){
  const r = completerReparee(r0);
  const b = el("button", "tampon-carte"), q = el("span", "q-carnet"), marque = el("span", "tampon-marque"), d = el("small", "date-carnet");
  const reg = REGIONS.find(x => x.matiere === r.matiere);
  q.textContent = typo([r.question, r.visuel].filter(Boolean).join(" ")) || "?";
  if (reg) q.prepend(iconeDom(`maison_${reg.id}`), " ");
  marque.append(iconeDom("coche"), "Réparée !");
  d.textContent = jourLisible(r.t);
  b.append(q, reponseDessinee(r), marque, d);
  b.onclick = () => { sfx.tap(); sauterDessin(marque); direReparee(r); };
  return b;
}
function videCarnet(){
  const b = el("button", "tampon-carte vide-carnet"), q = el("span", "q-carnet");
  q.textContent = "Ton carnet attend son premier tampon.";
  b.append(iconeDom("tampon"), q);
  b.onclick = () => { sfx.tap(); sauterDessin(b.firstChild); dire("Quand un chef d'Ombres te repose une erreur et que tu la répares, un tampon arrive ici !"); };
  return b;
}
