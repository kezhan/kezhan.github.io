/* Académia : les figures que dessinent les questions du grand, en SVG plat au contour d'encre (#1D1640), nettes à
   toutes les tailles et sans fichier à charger (js/question_dessin.js les pose sous l'énoncé ou dans les boutons).
   1. Fractions : la pizza coupée en parts égales (ou inégales, le piège), dont les parts mangées sont en pointillés ;
      la tablette de chocolat, une rangée par part ; deux pizzas côte à côte pour comparer ; une fraction écrite
      l'une sur l'autre (numérateur, trait, dénominateur), dans l'énoncé comme dans les boutons.
   2. Calcul : une multiplication posée en colonnes, le résultat à trouver en cases vides.
   3. Échecs : un petit échiquier (5 ou 6 cases de côté), ses pièces blanches et noires, les ronds de couleur des
      cases à choisir et l'étoile visée ; chaque pièce est aussi dessinée dans les boutons.
   4. Jour et nuit : le Soleil et la Terre qui tourne, sa moitié dans la nuit, des petits bonshommes posés dessus ;
      l'arbre, le Soleil haut ou bas, et son ombre. */
(() => {
  const ENCRE = "#1D1640";
  const COULEURS_RONDS = {"🔴": "#E53935", "🔵": "#1E88E5", "🟡": "#FDD835", "🟣": "#8E24AA", "🟢": "#43A047"};
  const sansSel = g => String(g).replace(/️/g, "");

  // an inline SVG element from its markup (the numbers come from the generators, never from a typed text)
  function svgQ(cls, vb, corps, titre){
    const s = el("span", "q-figure " + cls);
    s.innerHTML = `<svg viewBox="${vb}" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">${corps}</svg>`;
    s.setAttribute("role", "img"); if (titre) s.setAttribute("aria-label", titre);
    return s;
  }
  const pt = (r, a) => `${(r * Math.cos(a)).toFixed(2)},${(r * Math.sin(a)).toFixed(2)}`;
  function part(r, a0, a1){   // a slice of a disc from angle a0 to a1 (radians, clockwise from the top)
    return `M0,0 L${pt(r, a0)} A${r},${r} 0 ${a1 - a0 > Math.PI ? 1 : 0} 1 ${pt(r, a1)} Z`;
  }

  /* ---- fractions */
  // the cuts of a pizza: d equal parts, or the unequal ones of the trap (angles in turns, summing to 1)
  const COUPES_INEGALES = {2: [.32, .68], 3: [.18, .34, .48], 4: [.12, .2, .3, .38], 6: [.08, .12, .15, .18, .22, .25],
    8: [.06, .08, .1, .12, .13, .15, .17, .19]};
  function pizzaQ(k, d, inegale = false){
    const parts = inegale ? COUPES_INEGALES[d] : Array(d).fill(1 / d);
    let a = -Math.PI / 2, corps = `<circle r="57" fill="#E7E1FF" stroke="${ENCRE}" stroke-width="3"/>`;
    parts.forEach((t, i) => {
      const a0 = a, a1 = a + t * 2 * Math.PI; a = a1;
      if (i >= k) { corps += `<path d="${part(47, a0, a1)}" fill="none" stroke="${ENCRE}" stroke-width="2" stroke-dasharray="5 5" stroke-opacity=".45"/>`; return; }
      corps += `<path d="${part(47, a0, a1)}" fill="#E9A54B" stroke="${ENCRE}" stroke-width="3" stroke-linejoin="round"/>`
        + `<path d="${part(38, a0 + .05, a1 - .05)}" fill="#FFD45C"/>`;
      const n = t > .3 ? 3 : t > .15 ? 2 : 1;   // pepperoni, more on a bigger part
      for (let j = 0; j < n; j++) {
        const ang = a0 + (a1 - a0) * (j + 1) / (n + 1), r = n === 1 ? 25 : j % 2 ? 30 : 19;
        corps += `<circle cx="${(r * Math.cos(ang)).toFixed(1)}" cy="${(r * Math.sin(ang)).toFixed(1)}" r="5.5" fill="#E5533D" stroke="${ENCRE}" stroke-width="1.6"/>`;
      }
      corps += `<path d="M${pt(41, a0 + .12)} A41,41 0 0 1 ${pt(41, Math.min(a1 - .12, a0 + .7))}" fill="none" stroke="#fff" stroke-width="2.4" stroke-linecap="round" opacity=".6"/>`;
    });
    return svgQ("q-pizza", "-60 -60 120 120", corps);
  }
  // a chocolate bar: `lignes` rows of `colonnes` squares, the first `gardees` rows kept, the others eaten (dotted)
  function tabletteQ(lignes, colonnes, gardees = lignes){
    const c = 20, w = colonnes * c + 8, h = lignes * c + 8;
    let corps = `<rect x="1.5" y="1.5" width="${w - 3}" height="${h - 3}" rx="5" fill="#6B3E26" stroke="${ENCRE}" stroke-width="3"/>`;
    for (let y = 0; y < lignes; y++) for (let x = 0; x < colonnes; x++) {
      const px = 4 + x * c, py = 4 + y * c;
      corps += y < gardees
        ? `<rect x="${px + 1.5}" y="${py + 1.5}" width="${c - 3}" height="${c - 3}" rx="2.5" fill="#9A5B37" stroke="${ENCRE}" stroke-width="1.4"/><path d="M${px + 4},${py + c - 5} L${px + 4},${py + 4} L${px + c - 5},${py + 4}" fill="none" stroke="#C98A5E" stroke-width="2"/>`
        : `<rect x="${px + 1.5}" y="${py + 1.5}" width="${c - 3}" height="${c - 3}" rx="2.5" fill="#FFF8EC" stroke="${ENCRE}" stroke-width="1.2" stroke-dasharray="3 3"/>`;
    }
    return svgQ("q-tablette", `0 0 ${w} ${h}`, corps);
  }
  // "3/4" written one above the other; a fraction inside a text keeps its line
  function fractionQ(t){
    const m = /^(\d+|\?)\/(\d+)$/.exec(t), f = el("span", "q-frac");
    if (!m) { f.textContent = t; return f; }
    f.append(el("b"), el("i"), el("b"));
    f.firstChild.textContent = m[1]; f.lastChild.textContent = m[2];
    if (m[1] === "?") f.firstChild.className = "trou";
    f.setAttribute("aria-label", `${m[1]} sur ${m[2]}`);
    return f;
  }
  // a text of the game where "1/4" is written as a school fraction (the sentence of a question, its correction)
  function ecrireTexteQ(n, t){
    n.textContent = "";
    String(t).split(/((?:\d+|\?)\/\d+)/).forEach((x, i) => { if (i % 2) n.append(fractionQ(x)); else if (x) n.append(x); });
    return n;
  }

  /* ---- calculation: a multiplication set out in columns, the answer in empty boxes */
  function poseeQ(a, b){
    const lignes = [String(a), String(b)], larg = Math.max(...lignes.map(x => x.length), String(a * b).length) + 1, c = 22;
    const w = larg * c + 10, h = 3 * c + 22;
    let corps = "";
    const chiffres = (txt, y) => [...txt].reverse().forEach((ch, i) => {
      corps += `<text x="${w - 8 - i * c - c / 2}" y="${y}" text-anchor="middle">${ch}</text>`;
    });
    chiffres(lignes[0], c); chiffres(lignes[1], 2 * c + 4);
    corps += `<text x="${w - 8 - (larg - .5) * c}" y="${2 * c + 4}" text-anchor="middle" fill="#433088">×</text>`;
    corps += `<line x1="6" x2="${w - 4}" y1="${2 * c + 13}" y2="${2 * c + 13}" stroke="${ENCRE}" stroke-width="3" stroke-linecap="round"/>`;
    for (let i = 0; i < String(a * b).length; i++)
      corps += `<rect x="${w - 8 - (i + 1) * c + 2}" y="${2 * c + 19}" width="${c - 4}" height="${c - 2}" rx="4" fill="#fff" stroke="#433088" stroke-width="2" stroke-dasharray="4 3"/>`;
    return svgQ("q-posee", `0 0 ${w} ${h}`, `<g font-family="Fredoka, system-ui, sans-serif" font-weight="700" font-size="22" fill="${ENCRE}">${corps}</g>`,
      `${a} fois ${b}, posée`);
  }

  /* ---- chess: the pieces in a 100 x 100 box, white (ours) or black */
  const PIECES = {
    "♔": "roi", "♕": "dame", "♖": "tour", "♗": "fou", "♘": "cavalier", "♙": "pion",
    "♚": "roi", "♛": "dame", "♜": "tour", "♝": "fou", "♞": "cavalier", "♟": "pion"};
  const estPiece = g => !!PIECES[sansSel(g)];
  const FORMES_PIECES = {
    pion: `<path d="M33,79 C36,64 42,58 42,52 L58,52 C58,58 64,64 67,79 Z"/><rect x="35" y="46" width="30" height="9" rx="4.5"/><circle cx="50" cy="32" r="15"/>`,
    tour: `<path d="M31,79 L35,42 L65,42 L69,79 Z"/><path d="M27,42 L27,17 L37,17 L37,25 L45,25 L45,17 L55,17 L55,25 L63,25 L63,17 L73,17 L73,42 Z"/>`,
    fou: `<path d="M34,79 C38,68 42,64 42,60 L58,60 C58,64 62,68 66,79 Z"/><rect x="36" y="53" width="28" height="8" rx="4"/>`
      + `<path d="M50,16 C67,28 69,42 61,54 L39,54 C31,42 33,28 50,16 Z"/><circle cx="50" cy="12" r="5.5"/><path class="trait" d="M56,28 L46,41"/>`,
    cavalier: `<path d="M29,79 C29,66 35,58 44,51 C36,54 27,57 21,52 C16,47 22,39 32,32 C36,22 44,15 49,9 L53,18 C67,20 77,34 77,52 C77,64 73,73 71,79 Z"/>`
      + `<circle class="oeil" cx="43" cy="31" r="3.6"/><path class="trait" d="M58,22 C66,30 70,40 70,52"/>`,
    dame: `<path d="M32,79 L38,50 L62,50 L68,79 Z"/><path d="M30,50 L23,24 L38,37 L50,17 L62,37 L77,24 L70,50 Z"/>`
      + `<circle cx="23" cy="21" r="5"/><circle cx="50" cy="13" r="5.5"/><circle cx="77" cy="21" r="5"/>`,
    roi: `<path d="M32,79 L38,50 L62,50 L68,79 Z"/><path d="M31,50 C24,38 28,27 39,29 C43,22 57,22 61,29 C72,27 76,38 69,50 Z"/>`
      + `<rect x="46" y="4" width="8" height="22" rx="2"/><rect x="39" y="10" width="22" height="8" rx="2"/>`};
  function corpsPiece(g){
    const s = sansSel(g), noire = "♚♛♜♝♞♟".includes(s), fond = noire ? "#3B2E6E" : "#FFFFFF", trait = noire ? "#B8A8F0" : ENCRE;
    return `<g fill="${fond}" stroke="${ENCRE}" stroke-width="5" stroke-linejoin="round">`
      + `<rect x="21" y="78" width="58" height="13" rx="6"/>${FORMES_PIECES[PIECES[s]]}</g>`
      .replace(/class="trait"/g, `fill="none" stroke="${trait}" stroke-width="4" stroke-linecap="round"`)
      .replace(/class="oeil"/g, `fill="${noire ? "#FFF8EC" : ENCRE}" stroke="none"`)
      + (noire ? "" : `<path d="M28,84 L44,84" stroke="#E7E1FF" stroke-width="3" stroke-linecap="round"/>`);
  }
  function pieceQ(g){ return svgQ("q-piece", "8 0 84 96", corpsPiece(g), PIECES[sansSel(g)]); }
  const etoileD = (cx, cy, r) => Array.from({length: 10}, (_, i) => {
    const a = -Math.PI / 2 + i * Math.PI / 5, rr = i % 2 ? r * .45 : r;
    return `${(cx + rr * Math.cos(a)).toFixed(1)},${(cy + rr * Math.sin(a)).toFixed(1)}`;
  }).join(" ");
  /* d: {n: side, pieces: [[glyph, col, row]], marques: [[disc, col, row]], etoile: [col, row]}; row 0 at the bottom (the
     white side), as the child sits in front of the board */
  function echiquierQ(d){
    const n = d.n, c = 20, w = n * c + 8;
    const X = x => 4 + x * c, Y = y => 4 + (n - 1 - y) * c;
    let corps = `<rect x="1.5" y="1.5" width="${w - 3}" height="${w - 3}" rx="4" fill="${ENCRE}"/>`;
    for (let y = 0; y < n; y++) for (let x = 0; x < n; x++)
      corps += `<rect x="${X(x)}" y="${Y(y)}" width="${c}" height="${c}" fill="${(x + y) % 2 ? "#FFF3D6" : "#A996E3"}"/>`;
    (d.marques || []).forEach(([m, x, y]) => {
      corps += `<circle cx="${X(x) + c / 2}" cy="${Y(y) + c / 2}" r="6.4" fill="${COULEURS_RONDS[sansSel(m)] || "#fff"}" stroke="${ENCRE}" stroke-width="2"/>`;
    });
    if (d.etoile) corps += `<polygon points="${etoileD(X(d.etoile[0]) + c / 2, Y(d.etoile[1]) + c / 2 + .6, 8.4)}" fill="#FFD45C" stroke="${ENCRE}" stroke-width="1.8" stroke-linejoin="round"/>`;
    (d.pieces || []).forEach(([g, x, y]) => {
      corps += `<g transform="translate(${X(x) - .25},${Y(y) + .1}) scale(.205)">${corpsPiece(g)}</g>`;
    });
    return svgQ("q-echiquier", `0 0 ${w} ${w}`, corps, "un échiquier");
  }

  /* ---- day and night: the Sun on the left, the Earth turning on itself; the bonshommes stand at an angle (degrees,
     0 = the side away from the Sun, where it is midnight; 180 = facing the Sun, where it is noon) */
  // a little person standing on the Earth at `ang` degrees, head away from its centre, arms up, its body of the colour
  // of its disc; a pale outline under it, so that it shows on the night side too
  function bonhommeD(x, y, ang, couleur){
    const formes = (c, trait, e) => `<g stroke="${trait}" stroke-linejoin="round" stroke-linecap="round">`
      + `<path d="M-3,-8 L-3.8,0 M3,-8 L3.8,0 M-4,-17 L-9.5,-22 M4,-17 L9.5,-22" fill="none" stroke-width="${2.8 + e}"/>`
      + `<path d="M-6,-6.5 L-4.2,-19 Q0,-21.5 4.2,-19 L6,-6.5 Z" fill="${c}" stroke-width="${1.8 + e}"/>`
      + `<circle cx="0" cy="-26" r="6.2" fill="${c === "#FFF8EC" ? c : "#FFE0BD"}" stroke-width="${1.8 + e}"/></g>`;
    return `<g transform="translate(${x},${y}) rotate(${(90 - ang).toFixed(1)}) scale(1.5)">${formes("#FFF8EC", "#FFF8EC", 3)}${formes(couleur, ENCRE, 0)}</g>`;
  }
  function terreQ(d){
    const sx = 34, sy = 70, tx = 168, ty = 70, R = 44;
    let corps = `<rect x="0" y="0" width="266" height="140" rx="14" fill="#2B1E5C"/>`;
    [[205, 18], [236, 44], [226, 118], [120, 14], [128, 126], [256, 88]].forEach(([x, y]) => { corps += `<circle cx="${x}" cy="${y}" r="1.6" fill="#FFF6C9"/>`; });
    corps += `<path d="M${sx + 30},${sy - 26} L${tx - 30},${ty - 40} M${sx + 34},${sy} L${tx - R - 6},${ty} M${sx + 30},${sy + 26} L${tx - 30},${ty + 40}" stroke="#FFD45C" stroke-width="2" stroke-dasharray="4 6" opacity=".6"/>`;
    for (let i = 0; i < 12; i++) { const a = i * Math.PI / 6; corps += `<path d="M${sx + 31 * Math.cos(a)},${sy + 31 * Math.sin(a)} L${sx + 39 * Math.cos(a)},${sy + 39 * Math.sin(a)}" stroke="#FFD45C" stroke-width="4" stroke-linecap="round"/>`; }
    corps += `<circle cx="${sx}" cy="${sy}" r="26" fill="#FFD45C" stroke="${ENCRE}" stroke-width="3"/><circle cx="${sx - 8}" cy="${sy - 8}" r="7" fill="#FFF6C9"/>`;
    corps += `<circle cx="${tx}" cy="${ty}" r="${R}" fill="#36B5F0" stroke="${ENCRE}" stroke-width="3"/>`
      + `<path d="M${tx - 30},${ty - 22} C${tx - 14},${ty - 34} ${tx + 2},${ty - 20} ${tx - 6},${ty - 8} C${tx - 14},${ty + 2} ${tx - 30},${ty - 4} ${tx - 30},${ty - 22} Z" fill="#3DBE7A" stroke="${ENCRE}" stroke-width="2"/>`
      + `<path d="M${tx + 4},${ty + 10} C${tx + 20},${ty + 4} ${tx + 32},${ty + 18} ${tx + 22},${ty + 32} C${tx + 12},${ty + 40} ${tx - 2},${ty + 26} ${tx + 4},${ty + 10} Z" fill="#3DBE7A" stroke="${ENCRE}" stroke-width="2"/>`
      + `<path d="M${tx},${ty - R} A${R},${R} 0 0 1 ${tx},${ty + R} Z" fill="#14102E" opacity=".62"/>`
      + `<circle cx="${tx}" cy="${ty}" r="${R}" fill="none" stroke="${ENCRE}" stroke-width="3"/>`;
    // the Earth turns on itself (anticlockwise seen from above the North Pole): from the night, a place comes to the morning
    // (an arc over the top, from 40° to 115°, clear of the little people; its head points the way the Earth turns)
    const P = a => [tx + 58 * Math.cos(a * Math.PI / 180), ty - 58 * Math.sin(a * Math.PI / 180)], [x0, y0] = P(40), [x1, y1] = P(115);
    const dx = -Math.sin(115 * Math.PI / 180), dy = -Math.cos(115 * Math.PI / 180), f = n => n.toFixed(1);
    corps += `<path d="M${f(x0)},${f(y0)} A58,58 0 0 0 ${f(x1)},${f(y1)}" fill="none" stroke="#FFF8EC" stroke-width="3.4" stroke-linecap="round"/>`
      + `<path d="M${f(x1 + 9 * dx)},${f(y1 + 9 * dy)} L${f(x1 - 6 * dy)},${f(y1 + 6 * dx)} L${f(x1 + 6 * dy)},${f(y1 - 6 * dx)} Z" fill="#FFF8EC"/>`;
    (d.bonshommes || []).forEach(([m, ang]) => {
      const a = ang * Math.PI / 180, x = tx + (R + 1) * Math.cos(a), y = ty - (R + 1) * Math.sin(a);
      corps += bonhommeD(x.toFixed(1), y.toFixed(1), ang, COULEURS_RONDS[sansSel(m)] || "#F0566B");
    });
    return svgQ("q-terre", "0 0 266 140", corps, "le Soleil et la Terre");
  }
  // a tree on the ground, the Sun high (midday) or low on one side, and the shadow it casts (or none drawn)
  function ombreQ(d){
    const sol = 112, ax = 125;
    let corps = `<rect x="0" y="0" width="250" height="140" rx="14" fill="#BFE7FF"/><path d="M0,${sol} L250,${sol} L250,126 Q250,140 236,140 L14,140 Q0,140 0,126 Z" fill="#8FD18B"/>`
      + `<path d="M0,${sol} L250,${sol}" stroke="${ENCRE}" stroke-width="3"/>`;
    const soleil = {haut: [ax, 24], gauche: [30, 74], droite: [220, 74]}[d.soleil];
    const ombre = {courte: `M${ax - 16},${sol + 3} L${ax + 16},${sol + 3} L${ax + 13},${sol + 11} L${ax - 13},${sol + 11} Z`,
      droite: `M${ax + 6},${sol + 2} L${ax + 104},${sol + 8} L${ax + 104},${sol + 22} L${ax + 6},${sol + 12} Z`,
      gauche: `M${ax - 6},${sol + 2} L${ax - 104},${sol + 8} L${ax - 104},${sol + 22} L${ax - 6},${sol + 12} Z`}[d.ombre];
    if (ombre) corps += `<path d="${ombre}" fill="#2B1E5C" opacity=".55"/>`;
    corps += `<rect x="${ax - 6}" y="${sol - 30}" width="12" height="33" rx="3" fill="#9A5B37" stroke="${ENCRE}" stroke-width="3"/>`
      + `<circle cx="${ax}" cy="${sol - 44}" r="21" fill="#3DBE7A" stroke="${ENCRE}" stroke-width="3"/><circle cx="${ax - 8}" cy="${sol - 52}" r="6.5" fill="#7FDCA4"/>`;
    if (soleil) {
      const [x, y] = soleil;
      for (let i = 0; i < 10; i++) { const a = i * Math.PI / 5; corps += `<path d="M${x + 19 * Math.cos(a)},${y + 19 * Math.sin(a)} L${x + 25 * Math.cos(a)},${y + 25 * Math.sin(a)}" stroke="#F7A531" stroke-width="3.4" stroke-linecap="round"/>`; }
      corps += `<circle cx="${x}" cy="${y}" r="15" fill="#FFD45C" stroke="${ENCRE}" stroke-width="3"/>`;
    }
    return svgQ("q-ombre", "0 0 250 140", corps, "un arbre et son ombre");
  }

  /* ---- what js/question_dessin.js asks for */
  // the drawing under the sentence for d.type, or null when the type is not one of these
  function figureQ(d){
    switch (d.type) {
      case "pizza": return pizzaQ(d.k, d.d, !!d.inegale);
      case "pizzas": {   // fractions side by side, each written under its pizza; "?/4": cut in four, the parts to find
        const w = el("span", "q-pizzas");
        d.fractions.forEach(f => {
          const [k, n] = f.split("/"), c = el("span", "q-pizza-legende");
          c.append(pizzaQ(k === "?" ? 0 : +k, +n), fractionQ(f));
          w.append(c);
        });
        return w;
      }
      case "tablette": return tabletteQ(d.lignes, d.colonnes, d.gardees);
      case "posee": return poseeQ(d.a, d.b);
      case "echiquier": return echiquierQ(d);
      case "terre": return terreQ(d);
      case "ombre": return ombreQ(d);
      default: return null;
    }
  }
  // a button drawn instead of written: a pizza for "k/d" (an asterisk: cut in unequal parts)
  function boutonFigureQ(v, d){
    const m = /^(\d+)\/(\d+)(\*?)$/.exec(v);
    if (d.boutons === "pizza" && m) return pizzaQ(+m[1], +m[2], !!m[3]);
    return null;
  }

  /* ---- the drawing fits: on a low screen (a phone, the father's PC) or after a mistake, the figures under the sentence
     shrink until the panel needs no scrolling (the top of a chessboard never hides under the stuck sentence) */
  // still too big at its smallest: the panel may rise higher over the arena (style_combat.css, .panneau.haute)
  const FIGURE_MIN = 120;   // px: a chessboard of six squares still shows its pieces
  function ajusterFiguresQ(panneau){
    const figs = panneau ? [...panneau.querySelectorAll(".visuel .q-figure")] : [];
    if (!figs.length) return;
    const deborde = () => panneau.scrollHeight > panneau.clientHeight + 1;
    const reduire = () => {
      figs.forEach(f => { f.style.width = f.style.height = ""; });
      for (let k = 0; k < 14 && deborde(); k++) {
        let change = false;
        figs.forEach(f => {
          const w = f.getBoundingClientRect().width * .9;
          if (w < FIGURE_MIN) return;
          f.style.width = w + "px";
          if (f.classList.contains("q-pizza")) f.style.height = w + "px";
          change = true;
        });
        if (!change) break;
      }
    };
    panneau.classList.remove("haute");
    reduire();
    if (deborde()) { panneau.classList.add("haute"); reduire(); }
  }
  // a turned tablet or phone: every open panel fits its drawing again
  const toutAjuster = () => document.querySelectorAll(".panneau").forEach(ajusterFiguresQ);
  addEventListener("resize", () => {   // twice: the arena draws itself again a moment later (js/jeu/scene_combat.js)
    (ajusterFiguresQ.t || []).forEach(clearTimeout);
    ajusterFiguresQ.t = [setTimeout(toutAjuster, 250), setTimeout(toutAjuster, 900)];
  });

  // what the panel uses (js/question_dessin.js, js/question_vue.js); the rest stays inside
  Object.assign(self, {figureQ, boutonFigureQ, fractionQ, ecrireTexteQ, estPieceQ: estPiece, pieceQ, ajusterFiguresQ});
})();
