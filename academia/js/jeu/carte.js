/* Académia : la carte du village, construite en code. Une maison par matière, sa porte, une touffe d'herbes hautes
   où rôdent les Ombres. Deux lectures : des grilles de cases pour marcher (bloque, herbe, maison, portes) et la
   liste des éléments (chemins, place, maisons, arbres, mare, fleurs, touffes, petits lieux) que js/jeu/decor_village_hd.js
   dessine. Une case fait 16 unités du monde. Cinq chemins sortent de la carte vers les quartiers du village
   (SORTIES_VILLAGE ; js/quartiers.js). */
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
// the paths out of the village, to its quarters (js/quartiers.js, js/jeu/sorties_village.js): chemin, the path to the
// edge of the map (x, y, w, h), where the forest opens; cases, its last tiles: the hero who stops there goes on to
// the quarter; retour, the tile where he comes back; panneau, the tile of the sign that shows where the path leads
const bandeSortie = (x, y, w, h) => { const l = []; for (let b = y; b < y + h; b++) for (let a = x; a < x + w; a++) l.push([a, b]); return l; };
const SORTIES_VILLAGE = [
  {id: "ecole", chemin: [16, 0, 2, 12], cases: bandeSortie(16, 0, 2, 3), retour: {x: 16, y: 4}, panneau: {x: 15, y: 4}},
  {id: "foret", chemin: [0, 17, 6, 2], cases: bandeSortie(0, 17, 3, 2), retour: {x: 4, y: 17}, panneau: {x: 1, y: 16}},
  {id: "plage", chemin: [38, 16, 6, 2], cases: bandeSortie(41, 16, 3, 2), retour: {x: 39, y: 16}, panneau: {x: 42, y: 15}},
  {id: "marche", chemin: [14, 18, 2, 14], cases: bandeSortie(14, 29, 2, 3), retour: {x: 14, y: 27}, panneau: {x: 16, y: 29}},
  {id: "ferme", chemin: [20, 25, 2, 7], cases: bandeSortie(20, 29, 2, 3), retour: {x: 20, y: 27}, panneau: {x: 22, y: 29}}
];

function construireCarte(){
  const grille = (v) => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  // the ground of each tile (herbe, sable, place) and whether something already stands on it
  const sol = grille("herbe"), pris = grille(false), bloque = grille(false), herbe = grille(null), maison = grille(null);
  // the same map as elements, in tiles: {type, x, y, w, h, ...} (type: chemin, place, maison, arbre, bordure, rochers, mare, barque, panneau,
  // puits, lanterne, poteau, banc, potager, tonneaux, fleur, touffe; on the square: arbre_monde, fontaine, horloge, bac, panneau_missions)
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
  SORTIES_VILLAGE.forEach(s => sable(...s.chemin));   // out of the village, to its quarters
  const surSortie = (x, y, w, h) => SORTIES_VILLAGE.some(({chemin: [a, b, c, d]}) => x < a + c && x + w > a && y < b + d && y + h > b);
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
  // (the forest opens where a path leaves the village, and no tree stands on such a path)
  const bord = (nom, x, y) => { if (!surSortie(x, y, 2, 2)) poser(nom, x, y, {type: "bordure"}); };
  for (let x = 0; x < LARG; x += 2) { bord(arbres[(x / 2) % 5], x, 0); bord(arbres[(x / 2 + 2) % 5], x, HAUT - 2); }
  for (let y = 2; y < HAUT - 2; y += 2) { bord(arbres[(y / 2 + 1) % 5], 0, y); bord(arbres[(y / 2 + 3) % 5], LARG - 2, y); }
  [[3, 4], [15, 8], [30, 9], [3, 26], [12, 27], [30, 27], [33, 24], [15, 22], [16, 3], [16, 25]].forEach(([x, y], i) => {
    if (!bloque[y][x] && !bloque[y + 1][x + 1] && !pris[y][x] && sol[y][x] === "herbe" && !surSortie(x, y, 2, 2)) poser(arbres[i % 5], x, y);
  });
  // little places of the village, until the places of the design without a house yet take them
  const lieu = (type, x0, y0, w = 1, h = 1) => {
    elements.push({type, x: x0, y: y0, w, h});
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) bloque[y][x] = true;
  };
  lieu("puits", 13, 11, 2, 2); lieu("lanterne", 19, 10); lieu("lanterne", 22, 10); lieu("poteau", 16, 15);
  lieu("banc", 29, 13, 2, 1); lieu("potager", 2, 13, 3, 2); lieu("tonneaux", 39, 14);
  // the square of the Arbre-Monde (js/jeu/arbre_monde.js): the tree on the rosette, what the beaten chiefs give back
  // (the fountain, the clock, the flower boxes; the lanterns above), the Sage's board of the day's missions
  lieu("arbre_monde", 21, 14, 2, 2); lieu("fontaine", 24, 12, 2, 2); lieu("horloge", 26, 15); lieu("panneau_missions", 18, 13);
  [[17, 12], [17, 18], [25, 18], [26, 18]].forEach(([x, y]) => lieu("bac", x, y));
  // the merchant's stall east of the square, on the main street (him behind it, js/jeu/marchand_village.js);
  // its awning rises over the free grass above, the bench and the house beside it untouched
  lieu("etal", 31, 14, 3, 2);
  // the map of the village, south of the square, and the signs of the paths to the quarters (js/jeu/sorties_village.js)
  lieu("plan_village", 18, 20, 2, 1);
  SORTIES_VILLAGE.forEach(s => { bloque[s.panneau.y][s.panneau.x] = true; pris[s.panneau.y][s.panneau.x] = true; });
  // a few flowers on the grass (not blocking)
  for (let y = 2; y < HAUT - 2; y++) for (let x = 2; x < LARG - 2; x++)
    if (!pris[y][x] && !bloque[y][x] && sol[y][x] === "herbe" && hasard(x * 3, y * 7) < .05) {
      pris[y][x] = true; elements.push({type: "fleur", x, y, w: 1, h: 1, sorte: (x * y) % SORTES_FLEURS});
    }
  // villagers on the square
  const pnj = [
    {x: 19, y: 14, cle: "sage", nom: "Le Sage", dir: 0, dit: ["Bienvenue à Académia, Gardien !", "Des Ombres se cachent dans les hautes herbes, chacune aux couleurs de sa maison. Touche-en une : ton compagnon la combat avec ce que tu sais !", "Entre dans les maisons : tout s'y touche ! Quand tu as vaincu quatre Ombres près d'une maison, la porte du fond s'ouvre : le chef des Ombres t'y attend."]},
    {x: 25, y: 15, cle: "hugo", nom: "Hugo", dir: 2, dit: ["Chaque bonne réponse rend ton compagnon plus fort.", "Au niveau 15, il évolue !"]},
    {x: 22, y: 18, cle: "paco", nom: "Paco", dir: 1, dit: ["Touche l'endroit où tu veux aller, ton héros y marche tout seul. Touche une maison pour y entrer.", "Ton équipe est dans le sac 🎒, en haut de l'écran.", "Le potager, à côté de la Montagne de Germania, est ton jardin : touche-le pour planter, arroser et récolter !",
      "Le village a grandi ! Des chemins mènent à la ferme, au marché, à l'école, à la forêt et à la plage : regarde le plan, sous la place."]},
    {x: 32, y: 14, cle: "marchand", nom: "Le marchand", dir: 0, boutique: true, dit: ["Bonjour ! Touche mon étal : mes jokers et mes accessoires t'attendent."]}   // his shop: js/marchand.js
  ];
  if (typeof habitantsDeFete === "function") pnj.push(...habitantsDeFete());   // the Kleeschen and the Houseker, in December (js/jeu/fetes_village.js)
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  portes.forEach(p => { bloque[p.y][p.x] = false; });
  // a sign above each house: the subject's icon and name
  const etiquettes = [["dojo", 21, 2.4], ["duche", 25, 19.4], ["germania", 7, 12.4], ["albion", 37, 12.4], ["jardin", 9.5, 4.4], ["observatoire", 35, 4.4]]
    .map(([region, x, y]) => ({region, x, y}));
  return {sol, bloque, herbe, maison, portes, pnj, etiquettes, elements, depart: {x: 21, y: 16}, sorties: SORTIES_VILLAGE};
}
