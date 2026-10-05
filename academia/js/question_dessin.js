/* Académia : ce que dessine le panneau de question (js/question_vue.js).
   1. Les objets à compter rangés comme les points d'un dé, une case par objet, jusqu'à six ; au-delà, par paquets de
      cinq (cinq en dé, puis le reste) ; une soustraction en un seul tas dont les objets enlevés sont barrés et pâlis ;
      un complément à dix dans un cadre de dix cases.
   2. Les couleurs en pastilles plates à contour sombre (le blanc se voit sur le papier crème).
   3. Sous un nombre de 1 à 10 d'une réponse, ses points de dé (au-delà de six : cinq, puis le reste).
   4. La roue des saisons, l'objet à compter montré à part quand d'autres l'entourent (« on compte : 🎈 »), la réponse
      proposée d'un Vrai/Faux fabriqué (« C'est ça ? »), le nom sous une image (q.legendes). */
const POINTS_DE = {1: [5], 2: [1, 9], 3: [1, 5, 9], 4: [1, 3, 7, 9], 5: [1, 3, 5, 7, 9], 6: [1, 3, 4, 6, 7, 9]};
const PASTILLES = {"🔴": "#E53935", "🟠": "#FB8C00", "🟡": "#FDD835", "🟢": "#43A047", "🔵": "#1E88E5", "🟣": "#8E24AA",
  "🟤": "#8D5524", "⚫": "#212121", "⚪": "#FFFFFF"};
const sansVS = g => String(g).replace(/️/g, "");
const estNombreDe = v => /^\d+$/.test(v) && +v >= 1 && +v <= 10;

// one picture: a flat colour disc, or the emoji itself
function imageQ(g){
  const c = PASTILLES[sansVS(g)];
  if (c) { const s = el("span", "q-pastille"); s.style.setProperty("--c", c); s.setAttribute("role", "img"); s.setAttribute("aria-label", g); return s; }
  const s = el("span", "q-img"); s.textContent = g; return s;
}
// n pictures of one object on dice faces; the last `barres` ones crossed out and faded (what is taken away)
function tasQ(g, n, barres = 0){
  const w = el("span", "q-tas"), faces = n <= 6 ? [n] : [...Array(Math.floor(n / 5)).fill(5), ...(n % 5 ? [n % 5] : [])];
  let k = 0;
  faces.forEach(f => {
    const face = el("span", "q-face");
    for (let i = 1; i <= 9; i++) {
      const c = el("span", "q-case");
      if (POINTS_DE[f].includes(i)) { const im = imageQ(g); if (k++ >= n - barres) { c.classList.add("barre"); } c.append(im); }
      face.append(c);
    }
    w.append(face);
  });
  return w;
}
// the dots of a die under a number from 1 to 10
function pointsDe(n){
  const w = el("span", "q-points");
  (n <= 6 ? [n] : [5, n - 5]).forEach(k => {
    const d = el("span", "q-de");
    for (let i = 1; i <= 9; i++) d.append(el("i", POINTS_DE[k].includes(i) ? "on" : ""));
    w.append(d);
  });
  return w;
}
function dizaineQ(n){   // a ten frame: n blue discs, the rest white
  const w = el("span", "q-dizaine");
  for (let i = 0; i < 10; i++) w.append(imageQ(i < n ? "🔵" : "⚪"));
  return w;
}
// the wheel of the seasons, clockwise from the top; the season asked about lit, the one to find hidden
function roueQ(d, legendes){
  const w = el("span", "q-roue");
  d.cases.forEach((g, i) => {
    const c = el("span", `q-saison q-s${i}` + (g === "❓" ? " cherchee" : i === d.depart ? " depart" : ""));
    if (g === "❓") c.append(el("span", "q-trou", "?"));
    else { c.append(imageQ(g)); const t = el("small"); t.textContent = legendes[g] || ""; c.append(t); }
    w.append(c);
  });
  w.append(el("span", "q-fleche", "↻"));
  return w;
}
// a token of a picture text: a group of the same object, a row of pictures, a number, a sign
function morceauQ(t){
  if (t === "❓") return el("span", "q-trou", "?");
  const g = graphemes(t);
  if (g.every(estImage)) {
    if (g.length > 1 && new Set(g.map(sansVS)).size === 1) return tasQ(g[0], g.length);
    const r = el("span", "q-rang");
    g.forEach(x => r.append(x === "❓" ? el("span", "q-trou", "?") : imageQ(x)));
    return r;
  }
  const s = el("span", /^\d+$/.test(t) ? "q-tuile" : "q-signe"); s.textContent = t; return s;
}
// what the question shows under its sentence (null: nothing)
function dessinVisuel(q){
  const d = q.dessin || {}, v = el("div", "visuel");
  if (q.cible) { const c = el("span", "q-cible", "<small>on compte :</small>"); c.append(imageQ(q.cible)); v.append(c); }
  if (d.type === "barre") v.append(tasQ(d.e, d.n, d.barres));
  else if (d.type === "dizaine") v.append(dizaineQ(d.n));
  else if (d.type === "roue") v.append(roueQ(d, q.legendes || {}));
  else String(q.visuel || "").trim().split(/\s+/).filter(Boolean).forEach(t => v.append(morceauQ(t)));
  // a single picture to look at (a True/False « C'est cat ? », a word to find): as big as an answer
  if (!d.type && !q.cible && v.childNodes.length === 1 && v.firstChild.classList.contains("q-rang") && v.firstChild.childNodes.length === 1) v.classList.add("seule");
  if (q.oreille && !v.classList.contains("seule")) v.classList.add("scene");   // the situation of a question to the ear
  if (q.propose != null) {   // a True/False built from a question: the answer proposed, to judge
    const c = el("span", "q-propose", "<small>C'est ça ?</small>");
    c.append(contenuReponse(q.propose, q, false).contenu);
    v.append(c);
  }
  return v.childNodes.length ? v : null;
}
/* what an answer button shows: {contenu, genre} where genre is "image" (one picture filling the button), "groupe"
   (pictures on dice faces) or "texte" (a word, a number and its dots) */
function contenuReponse(v, q, enGroupes){
  const f = document.createDocumentFragment(), g = graphemes(v), leg = q.legendes && q.legendes[v];
  let genre = "texte";
  if (g.length && g.every(estImage)) {
    if (enGroupes || (g.length > 1 && new Set(g.map(sansVS)).size === 1)) { f.append(tasQ(g[0], g.length)); genre = "groupe"; }
    else { const r = el("span", "q-rang"); g.forEach(x => r.append(imageQ(x))); f.append(r); genre = "image"; }
  } else {
    const m = el("span", "q-mot"); m.textContent = v; f.append(m);
    const lettres = Math.max(...String(v).split(/\s+/).map(w => w.length));   // a long word (« Donneschdeg ») shrinks
    if (lettres > 7) m.style.setProperty("--lettres", lettres);                 // to fit its button, never cut in two
    if (estNombreDe(v)) f.append(pointsDe(+v));
  }
  if (leg) { const s = el("small", "q-legende"); s.textContent = leg; f.append(s); }
  return {contenu: f, genre};
}
