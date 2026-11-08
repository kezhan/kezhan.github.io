/* Académia : les mots appris dans les questions de langue (Kezhan, 13/10/2026 : « revoir tout et donner des
   opportunités pour apprendre » ; lot C du ticket apprendre-partout, conception/audit_apprendre.md, propositions 18
   et 22).
   1. motDeQuestion(q) : le mot étranger d'une question, celui que la bonne réponse redit (js/question_vue.js,
      motReussi), seulement quand il a son enregistrement : {cle, mot, langue, nom, sens, image}. Le sens vient de la
      légende de l'image ou de l'explication (« « pig », c'est le cochon », « Sur l'image, c'est la maman : en
      anglais, on dit « mum » », « « Lundi », en anglais, c'est « Monday » »…) ; l'image, de la réponse (👋, 🐱, ⭐⭐⭐,
      5) ou de ce que montre la question (👩), une couleur par sa pastille. Un mot sans enregistrement n'est jamais
      retenu : il ne serait pas dit.
   2. noterMot(q, juste) : chaque question de langue posée (js/adaptatif.js, noter) compte son mot dans P().mots
      (js/etat.js : « en:cat » -> {mot, sens, image, vu, ok, jour, t}).
   3. La carte de fin d'un combat (js/bilan.js) : une rangée de petites cartes, les mots trouvés pendant ce combat
      (les trois derniers) ; rien n'est dit tout seul, une carte ne se touche qu'une fois apparue, et dit son mot.
   4. L'imagier « Mots » de l'album du sac (js/album.js) : une case par mot rencontré, une langue à la fois, les mots
      trouvés d'abord (le plus récent en tête), puis les silhouettes ; une étoile dès trois fois ; un toucher dit le
      mot. Le petit dessin de la maison de sa langue sert de drapeau (Albion l'anglais, Germania l'allemand, le
      Grand-Duché le luxembourgeois). La page s'ouvre sur sa première feuille, où sont les mots nouveaux.
   Un mot gardé dans la partie est revérifié au moment d'être dit : sans enregistrement (un index réécrit par les
   outils des voix peut l'avoir perdu), sa case reste montrée et le mot écrit, mais aucune voix ne le dit. Le sens
   suit la règle du reste du jeu (sensADire, js/mots_du_jeu.js : la petite, les premières fois, jamais deux fois dans
   la minute). Un mot long passe sur deux lignes, en plus petit, jamais coupé. */
const MAISON_LANGUE = {en: "albion", de: "germania", lb: "duche"};
const ORDRE_LANGUES = ["en", "de", "lb"];
const ETOILE_MOT = 3;   // found three times: its star
// a colour word is drawn as its colour (never the strawberry of « red »), by its meaning
const COULEURS_MOTS = {rouge: "#E53935", orange: "#FB8C00", jaune: "#FDD835", vert: "#43A047", bleu: "#1E88E5", violet: "#8E24AA",
  marron: "#8D5524", noir: "#212121", blanc: "#FFFFFF", gris: "#9E9E9E", rose: "#F06292"};

// ---------- the word of a question ----------
// its meaning in French, when the question tells it (its picture's caption first, js/question_vue.js sensReussi)
function sensDuMot(q, w){
  const s0 = typeof sensReussi === "function" ? sensReussi(q, w) : null;
  if (s0) return s0;
  const e = sansEmoji(q.explication || "").replace(/^(?:Oui|Non)\s*:\s*/, ""), meme = x => !!x && x.trim().toLowerCase() === w.toLowerCase();
  const net = s => { s = String(s || "").trim().replace(/^«\s*(.+?)\s*»$/, "$1").trim(); return !s || /[«»]/.test(s) ? null : s; };
  let m;
  const re = /«\s*([^«»]+?)\s*»\s*,\s*(?:c'est|ce sont|ça veut dire)\s+([^:.]+)/g;   // « bear », c'est l'ours
  while ((m = re.exec(e))) if (meme(m[1])) return net(m[2]);
  if ((m = /(?:c'est|ce sont) ([^:.]+?) : en \p{L}+, on dit «\s*(.+?)\s*»/u.exec(e)) && meme(m[2])) return net(m[1]);   // Sur l'image, c'est la maman
  if ((m = /[Ee]n \p{L}+, (.+?) se di(?:t|sent) «\s*(.+?)\s*»/u.exec(e)) && meme(m[2])) return net(m[1]);   // la mamie se dit « grandma »
  if ((m = /^«\s*(.+?)\s*», en \p{L}+, c'est «\s*(.+?)\s*»/u.exec(e)) && meme(m[2])) return net(m[1].toLowerCase());   // « Lundi », en anglais
  if ((m = /il y a «\s*(.+?)\s*» : .*c'est (\p{L}+)\s*\.?$/u.exec(e)) && meme(m[1])) return net(m[2]);   // après lundi, c'est mardi
  return null;
}
// its picture: the answer when it is one (a drawing, a group, a number), else what the question shows (a True/False
// shows the word's picture only when it is true)
function imageDuMot(q){
  const vf = estVraiFaux(q), oreille = q.oreille && q.oreille !== "fr";
  const image = v => {
    const g = graphemes(String(v || "").replace(/\s+/g, ""));
    return g.length && g.every(estImage) && (g.length <= 3 || (g.length <= 6 && g.every(x => x === g[0]))) ? g.join("") : null;
  };
  if (!vf && !oreille) { const r = image(q.reponse); if (r) return r; if (/^\d{1,2}$/.test(String(q.reponse))) return String(q.reponse); }
  return !vf || q.reponse === "vrai" ? image(q.visuel) : null;
}
function motDeQuestion(q){
  if (!q || typeof motReussi !== "function" || typeof motEtranger !== "function") return null;
  const r = motReussi(q, motEtranger(q));
  if (!r || !ORDRE_LANGUES.includes(r.lang)) return null;
  const sens = r.sens || sensDuMot(q, r.mot), couleur = /_couleurs$/.test(q.notion || "") ? COULEURS_MOTS[String(sens).toLowerCase()] || null : undefined;
  return {cle: `${r.lang}:${cleVoix(r.mot)}`, mot: r.mot, langue: r.lang, nom: NOMS_LANGUES[r.lang], sens, image: couleur === undefined ? imageDuMot(q) : couleur};
}

// ---------- the words met, kept in the game (js/etat.js: P().mots) ----------
const motsDe = p => { if (!p.mots || typeof p.mots !== "object" || Array.isArray(p.mots)) p.mots = {}; return p.mots; };
// a language question asked (js/adaptatif.js, noter, which saves): its word counted, found or not; the first spelling,
// meaning and picture are kept (a later question may bring the missing ones)
function noterMot(q, juste){
  const p = P(), w = p && !q.genere && motDeQuestion(q);
  if (!w) return;
  const l = motsDe(p), e = l[w.cle] = l[w.cle] || {mot: w.mot, vu: 0, ok: 0};
  e.vu = (Number(e.vu) || 0) + 1;
  if (juste) e.ok = (Number(e.ok) || 0) + 1;
  if (!e.sens && w.sens) e.sens = w.sens;
  if (!e.image && w.image) e.image = w.image;
  e.jour = jour(); e.t = Date.now();   // last met: the picture book shows the latest first
}
// the words found in this fight, for its end card (the latest three, each once); a question without the word's
// picture (a True/False that was false) takes the one the game already keeps for it
function retenirMot(bilan, q){
  if (!q || q.genere) return;   // a True/False made up by the engine is not counted (js/adaptatif.js): not shown either
  const w = motDeQuestion(q), p = P();
  if (!w) return;
  if (!w.image && p) w.image = (motsDe(p)[w.cle] || {}).image || null;
  bilan.mots = (bilan.mots || []).filter(x => x.cle !== w.cle).concat(w).slice(-3);
}

// ---------- drawn ----------
// the picture of a word: its drawings (a colour as a disc, js/question_dessin.js), a number, else the speaker (its
// meaning is written under the word for the big one, and said)
function imageMot(w){
  const s = el("span", "image-mot");
  if (w.image && /^#[0-9A-F]{6}$/i.test(w.image)) { const d = el("span", "q-pastille"); d.style.setProperty("--c", w.image); s.append(d); }
  else if (w.image && /^\d+$/.test(w.image)) { s.classList.add("nombre"); s.textContent = w.image; }
  else if (w.image) {   // two drawings side by side a little smaller, more of them smaller still
    const g = graphemes(w.image);
    s.classList.add(g.length > 2 ? "groupe" : g.length > 1 ? "deux" : "seule");
    g.forEach(x => s.append(imageQ(x)));
  }
  else { s.classList.add("sans-image"); s.append(iconeDom("haut_parleur")); }
  return s;
}
// the word written whole: a long one on two lines, smaller (never cut)
const motEcrit = w => {
  const b = el("b", "mot-etranger"), n = [...String(w.mot)].length;
  b.lang = w.langue; b.textContent = w.mot;
  if (n > 11) b.classList.add(n > 20 ? "tres-long" : "long");
  return b;
};
// a word kept in the game, said again: by its recording only if it still has one (checked now, never trusted from the
// day it was met), then its meaning in French by the game's rule (« C'est « le chat », en anglais ! », sensADire);
// the promise ends with the voice (true when nothing had to be said)
function direMotAppris(w){
  return motsPrets().then(() => {
    const m = {cle: cleVoix(w.mot), langue: w.langue, mot: w.mot, sens: w.sens, nom: NOMS_LANGUES[w.langue]};
    const voix = motEnregistre(w.mot, w.langue), sens = !!w.sens && sensADire(m);
    const etapes = [...(voix ? [() => dire(w.mot, w.langue)] : []), ...(sens ? [() => dire(phraseSens(m))] : [])];
    return etapes.length ? suiteDeVoix(etapes) : true;
  });
}

// the end card's row: the words found in the fight, each a little card (its picture, the word); nothing is said by
// itself, and a card answers only once it has come in (a child still tapping after the last answer touches nothing).
// A touch says the word; `touche(voix)` is told, with the promise of that voice (js/bilan.js: the card's own voice
// goes on after it)
function rangeeMots(mots, touche){
  const r = el("div", "mots-bilan");
  const prete = () => r.classList.add("prete");
  if (calme()) prete(); else { r.addEventListener("animationend", prete, {once: true}); setTimeout(prete, 1600); }
  mots.forEach(w => {
    const b = el("button", "carte-mot");
    b.dataset.mot = w.mot; b.dataset.langue = w.langue;
    b.setAttribute("aria-label", w.mot);
    b.append(imageMot(w), motEcrit(w));
    b.onclick = () => {
      if (!r.classList.contains("prete")) return;
      sfx.tap(); sauterDessin(b.firstChild);
      const voix = direMotAppris(w);
      if (touche) touche(voix);
    };
    r.append(b);
  });
  return r;
}

// the album's picture book: the words met, one language at a time (the language of the latest word first shown);
// the words found first, the latest met at the top, then the silhouettes, the latest first too
let langueImagier = null;
const recent = w => Number(w.t) || 0;
function motsRencontres(p = P()){
  return Object.entries(motsDe(p)).map(([cle, e], rang) => ({...e, cle, rang, langue: cle.split(":")[0]}))
    .filter(w => ORDRE_LANGUES.includes(w.langue))
    .sort((a, b) => (b.ok > 0) - (a.ok > 0) || recent(b) - recent(a) || String(b.jour || "").localeCompare(String(a.jour || "")) || b.rang - a.rang);
}
// the bag opened: the picture book opens on its first leaf, where the new words are
document.addEventListener("ecran", e => { if (e.detail === "equipe" && typeof feuillets === "object") feuillets.mots = 0; });
const comptesMots = (p = P()) => { const l = motsRencontres(p); return {trouves: l.filter(w => w.ok > 0).length, total: l.length}; };
function pageMots(feuille, fleches, v){
  const tous = motsRencontres(), langues = ORDRE_LANGUES.filter(l => tous.some(w => w.langue === l));
  const derniere = () => tous.reduce((a, w) => recent(w) > recent(a) || (recent(w) === recent(a) && w.rang > a.rang) ? w : a).langue;
  if (!langues.includes(langueImagier)) langueImagier = tous.length ? derniere() : null;
  if (langues.length > 1) {   // one tab per language that has words
    const choix = el("div", "langues-mots");
    langues.forEach(l => {
      const b = el("button", "langue-mot"), n = tous.filter(w => w.langue === l);
      b.dataset.langue = l; b.setAttribute("aria-pressed", String(l === langueImagier));
      b.append(iconeDom(`maison_${MAISON_LANGUE[l]}`), el("span", "nom-langue", majuscule(NOMS_LANGUES[l])), el("span", "compte-page", `${n.filter(w => w.ok > 0).length}/${n.length}`));
      b.onclick = () => { sfx.tap(); langueImagier = l; feuillets.mots = 0; dire(`Tes mots en ${NOMS_LANGUES[l]} !`); dessinerAlbum(v); };
      choix.append(b);
    });
    feuille.append(choix);
  }
  paginer(feuille, fleches, v, tous.filter(w => w.langue === langueImagier), caseMot, videMots);
}
// a word's place: its picture (a silhouette until it is found), the word, its little flag; a star from three times
function caseMot(w){
  const trouve = w.ok > 0, b = el("button", "case-mot" + (trouve ? " trouve" : "") + (w.ok >= ETOILE_MOT ? " etoile" : ""));
  const cadre = el("span", "cadre"), petit = !!P() && P().age <= 5;
  b.dataset.mot = w.cle; b.dataset.langue = w.langue;
  cadre.append(imageMot(w));
  b.append(cadre, motEcrit(w));
  if (!petit && w.sens) { const s = el("small", "sens-mot"); s.textContent = w.sens; b.append(s); }   // the big one reads it too
  b.append(iconeDom(`maison_${MAISON_LANGUE[w.langue]}`, "langue-case"));
  if (w.ok >= ETOILE_MOT) b.append(iconeDom("etoile", "etoile-case"));
  b.setAttribute("aria-label", w.mot);
  b.onclick = () => { sfx.tap(); sauterDessin(cadre); direMotAppris(w); };
  return b;
}
function videMots(){
  const d = el("div", "album-vide"), places = el("div", "places");
  for (let i = 0; i < 4; i++) {
    const b = el("button", "case-mot attend"), c = el("span", "cadre");
    c.append(iconeDom(["maison_albion", "maison_germania", "maison_duche", "haut_parleur"][i]));
    b.append(c);
    b.onclick = () => { sfx.tap(); sauterDessin(c); dire("Ici arrivent les mots que tu rencontres dans les combats d'Albion, de Germania et du Grand-Duché !"); };
    places.append(b);
  }
  const t = el("p"); t.textContent = "Les mots des questions de langue arrivent ici.";
  d.append(places, t);
  return d;
}
