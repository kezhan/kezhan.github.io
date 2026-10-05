/* Académia : la carte du village, construite en code. Une maison par matière, sa porte, une touffe d'herbes hautes
   où rôdent les Ombres. Deux lectures : des grilles de cases pour marcher (bloque, herbe, maison, portes) et la
   liste des éléments (chemins, place, maisons, arbres, mare, fleurs, touffes, petits lieux) que js/jeu/decor_village_hd.js
   dessine. Une case fait 16 unités du monde. */
const CASE = 16;
const LARG = 44, HAUT = 32;
// drawing order by row: whoever stands lower on the map is in front (objects: their last row; tall grass hides feet: +8)
const profondeur = (ligne, decalage = 5) => 100 + 10 * ligne + decalage;

// the pieces standing on the map, in tiles (width, height); the drawing is chosen by the piece's name
const PIECES = {
  maisonA: [4, 3], maisonB: [4, 3], maisonC: [4, 3], maisonRouge: [4, 3], hutte: [3, 2], statue: [2, 2], panneauDojo: [2, 1],
  buisson1: [2, 2], buisson2: [2, 2], buisson3: [2, 2], sapin1: [2, 2], sapin2: [2, 2], grandArbre: [2, 2],
  rochers: [4, 3], mare: [5, 5], barque: [2, 1]
};
const SORTES_FLEURS = 3;   // three kinds of flowers on the grass

function construireCarte(){
  const grille = (v) => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  // the ground of each tile (herbe, sable, place) and whether something already stands on it
  const sol = grille("herbe"), pris = grille(false), bloque = grille(false), herbe = grille(null), maison = grille(null);
  // the same map as elements, in tiles: {type, x, y, w, h, ...} (type: chemin, place, maison, arbre, bordure, rochers, mare, barque, panneau,
  // puits, lanterne, poteau, banc, potager, tonneaux, fleur, touffe)
  const elements = [];
  const hasard = (x, y) => Math.abs(Math.sin(x * 12.9898 + y * 78.233) * 43758.5453) % 1;   // same map every time
  const sable = (x0, y0, w, h) => {
    elements.push({type: "chemin", x: x0, y: y0, w, h});
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) sol[y][x] = "sable";
  };
  const SORTES = {maisonA: "maison", maisonB: "maison", maisonC: "maison", maisonRouge: "maison", hutte: "maison", statue: "maison",
    panneauDojo: "panneau", rochers: "rochers", mare: "mare", barque: "barque"};
  const poser = (nom, x0, y0, opts = {}) => {
    const [w, h] = PIECES[nom];
    elements.push({type: opts.type || SORTES[nom] || "arbre", nom, x: x0, y: y0, w, h, ...(opts.region ? {region: opts.region} : {})});
    for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) {
      pris[y0 + j][x0 + i] = true;
      bloque[y0 + j][x0 + i] = opts.passe ? opts.passe(i, j) === false : true;
      if (opts.region) maison[y0 + j][x0 + i] = opts.region;
    }
  };
  // paths and the square
  sable(17, 12, 10, 7);                      // square, paved
  elements.push({type: "place", x: 18, y: 13, w: 8, h: 5});
  for (let y = 13; y < 18; y++) for (let x = 18; x < 26; x++) sol[y][x] = "place";
  sable(20, 6, 2, 6); sable(20, 19, 2, 6); sable(20, 23, 8, 2);   // north (Dojo), south then east to the Grand-Duché
  sable(6, 16, 11, 2); sable(27, 16, 11, 2); // west (Germania), east (Albion)
  sable(9, 7, 2, 9); sable(34, 7, 2, 9);     // Jardin, Observatoire
  // buildings: one per subject, the door's front tile opens the region (portes)
  poser("maisonC", 19, 3, {region: "dojo"}); poser("panneauDojo", 23, 5, {region: "dojo"});
  poser("maisonRouge", 23, 20, {region: "duche"});
  poser("maisonA", 5, 13, {region: "germania"});
  poser("maisonB", 35, 13, {region: "albion"});
  poser("hutte", 8, 5, {region: "jardin"});
  poser("statue", 34, 5, {region: "observatoire"});
  const portes = [
    {x: 20, y: 6, region: "dojo"}, {x: 21, y: 6, region: "dojo"},
    {x: 24, y: 23, region: "duche"}, {x: 25, y: 23, region: "duche"},
    {x: 6, y: 16, region: "germania"}, {x: 7, y: 16, region: "germania"},
    {x: 36, y: 16, region: "albion"}, {x: 37, y: 16, region: "albion"},
    {x: 9, y: 7, region: "jardin"}, {x: 34, y: 7, region: "observatoire"}, {x: 35, y: 7, region: "observatoire"}
  ];
  // tall grass where the Ombres hide, one patch per region
  const touffe = (x0, y0, w, h, region) => {
    elements.push({type: "touffe", x: x0, y: y0, w, h, region});
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) {
      if (bloque[y][x] || sol[y][x] === "sable") continue;
      pris[y][x] = true; herbe[y][x] = region;
    }
  };
  touffe(24, 7, 5, 4, "dojo"); touffe(28, 20, 6, 3, "duche"); touffe(7, 20, 6, 4, "germania");
  touffe(37, 18, 5, 3, "albion"); touffe(3, 3, 5, 4, "jardin"); touffe(37, 7, 4, 4, "observatoire");
  // nature: the pond near the port, rocks near the mountain, trees all around
  poser("mare", 36, 22, {passe: (i, j) => i === 0 || j === 0 || i === 4 || j === 4}); poser("barque", 37, 27);
  poser("rochers", 2, 20); poser("grandArbre", 27, 2); poser("grandArbre", 14, 2);   // against the forest: nobody hides behind them
  const arbres = ["buisson1", "buisson2", "buisson3", "sapin1", "sapin2"];
  for (let x = 0; x < LARG; x += 2) { poser(arbres[(x / 2) % 5], x, 0, {type: "bordure"}); poser(arbres[(x / 2 + 2) % 5], x, HAUT - 2, {type: "bordure"}); }
  for (let y = 2; y < HAUT - 2; y += 2) { poser(arbres[(y / 2 + 1) % 5], 0, y, {type: "bordure"}); poser(arbres[(y / 2 + 3) % 5], LARG - 2, y, {type: "bordure"}); }
  [[3, 4], [15, 8], [30, 9], [3, 26], [12, 27], [30, 27], [33, 24], [15, 22], [16, 3], [16, 25]].forEach(([x, y], i) => {
    if (!bloque[y][x] && !bloque[y + 1][x + 1] && !pris[y][x] && sol[y][x] === "herbe") poser(arbres[i % 5], x, y);
  });
  // little places of the village, until the places of the design without a house yet take them
  const lieu = (type, x0, y0, w = 1, h = 1) => {
    elements.push({type, x: x0, y: y0, w, h});
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) bloque[y][x] = true;
  };
  lieu("puits", 13, 11, 2, 2); lieu("lanterne", 19, 11); lieu("lanterne", 22, 11); lieu("poteau", 16, 15);
  lieu("banc", 29, 13, 2, 1); lieu("potager", 2, 13, 3, 2); lieu("tonneaux", 39, 14);
  // a few flowers on the grass (not blocking)
  for (let y = 2; y < HAUT - 2; y++) for (let x = 2; x < LARG - 2; x++)
    if (!pris[y][x] && !bloque[y][x] && sol[y][x] === "herbe" && hasard(x * 3, y * 7) < .05) {
      pris[y][x] = true; elements.push({type: "fleur", x, y, w: 1, h: 1, sorte: (x * y) % SORTES_FLEURS});
    }
  // villagers on the square
  const pnj = [
    {x: 19, y: 14, cle: "sage", nom: "Le Sage", dir: 0, dit: ["Bienvenue à Académia, Gardien !", "Des Ombres violettes se cachent dans les hautes herbes. Touche-en une : ton compagnon la combat avec ce que tu sais !", "Quand tu as vaincu quatre Ombres près d'une maison, sa porte s'ouvre : le chef des Ombres t'y attend."]},
    {x: 25, y: 15, cle: "hugo", nom: "Hugo", dir: 2, dit: ["Chaque bonne réponse rend ton compagnon plus fort.", "Au niveau 15, il évolue !"]},
    {x: 22, y: 18, cle: "paco", nom: "Paco", dir: 1, dit: ["Touche l'endroit où tu veux aller, ton héros y marche tout seul. Touche une maison pour aller à sa porte.", "Ton équipe est dans le sac 🎒, en haut de l'écran."]}
  ];
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  portes.forEach(p => { bloque[p.y][p.x] = false; });
  // a sign above each house: the subject's icon and name
  const etiquettes = [["dojo", 21, 2.4], ["duche", 25, 19.4], ["germania", 7, 12.4], ["albion", 37, 12.4], ["jardin", 9.5, 4.4], ["observatoire", 35, 4.4]]
    .map(([region, x, y]) => ({region, x, y}));
  return {sol, bloque, herbe, maison, portes, pnj, etiquettes, elements, depart: {x: 21, y: 16}};
}
