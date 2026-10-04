/* Académia : la carte du village, construite en code à partir de la planche de décor du pack (cases de 16 px,
   28 colonnes). Une maison par matière, sa porte, une touffe d'herbes hautes où rôdent les Ombres. */
const CASE = 16, COLS_DECOR = 28;
const T = (c, r) => r * COLS_DECOR + c;          // tile index in assets/decor.png
const LARG = 44, HAUT = 32;

// ground and multi-tile pieces of the sheet (column, row, width, height)
const SOL = {herbe: [T(22, 11), T(22, 11), T(22, 11), T(22, 11)], sable: [T(22, 6), T(22, 6), T(22, 6)], place: T(19, 12)};
const PIECES = {
  maisonA: [0, 0, 4, 3], maisonB: [4, 0, 4, 3], maisonC: [8, 0, 4, 3], maisonRouge: [12, 0, 4, 3], hutte: [16, 0, 3, 2],
  statue: [26, 2, 2, 2], panneauDojo: [14, 4, 2, 1], buisson1: [0, 10, 2, 2], buisson2: [2, 10, 2, 2], buisson3: [4, 10, 2, 2],
  sapin1: [6, 10, 2, 2], sapin2: [8, 10, 2, 2], grandArbre: [11, 8, 2, 2], rochers: [14, 9, 4, 3], mare: [18, 6, 5, 5], barque: [26, 10, 2, 1],
  torii: [0, 3, 3, 2], rose1: [4, 16, 2, 2], rose2: [6, 16, 2, 2], rose3: [8, 16, 2, 2]
};
const DECOS = [T(3, 15), T(4, 15), T(1, 8)];   // sunflower, clover, little flowers
const HERBES = [T(10, 16), T(9, 15)];

function construireCarte(){
  const grille = (v) => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  const sol = grille(0), objets = grille(-1), bloque = grille(false), herbe = grille(null);
  const hasard = (x, y) => Math.abs(Math.sin(x * 12.9898 + y * 78.233) * 43758.5453) % 1;   // same map every time
  for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) sol[y][x] = SOL.herbe[Math.floor(hasard(x, y) * 4)];
  const sable = (x0, y0, w, h) => { for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) sol[y][x] = SOL.sable[Math.floor(hasard(x, y) * 3)]; };
  const poser = (nom, x0, y0, opts = {}) => {
    const [c, r, w, h] = PIECES[nom];
    for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) {
      objets[y0 + j][x0 + i] = T(c + i, r + j);
      bloque[y0 + j][x0 + i] = opts.passe ? opts.passe(i, j) === false : true;
    }
  };
  // paths and the square
  sable(17, 12, 10, 7);                      // square, paved
  for (let y = 13; y < 18; y++) for (let x = 18; x < 26; x++) sol[y][x] = SOL.place;
  sable(20, 6, 2, 6); sable(20, 19, 2, 6); sable(20, 23, 8, 2);   // north (Dojo), south then east to the Grand-Duché
  sable(6, 16, 11, 2); sable(27, 16, 11, 2); // west (Germania), east (Albion)
  sable(9, 7, 2, 9); sable(34, 7, 2, 9);     // Jardin, Observatoire
  // buildings: one per subject, the door's front tile opens the region (portes)
  poser("maisonC", 19, 3); poser("panneauDojo", 23, 5);
  poser("maisonRouge", 23, 20);
  poser("maisonA", 5, 13);
  poser("maisonB", 35, 13);
  poser("hutte", 8, 5);
  poser("statue", 34, 5);
  const portes = [
    {x: 20, y: 6, region: "dojo"}, {x: 21, y: 6, region: "dojo"},
    {x: 24, y: 23, region: "duche"}, {x: 25, y: 23, region: "duche"},
    {x: 6, y: 16, region: "germania"}, {x: 7, y: 16, region: "germania"},
    {x: 36, y: 16, region: "albion"}, {x: 37, y: 16, region: "albion"},
    {x: 9, y: 7, region: "jardin"}, {x: 34, y: 7, region: "observatoire"}, {x: 35, y: 7, region: "observatoire"}
  ];
  // tall grass where the Ombres hide, one patch per region
  const touffe = (x0, y0, w, h, region) => {
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) {
      if (bloque[y][x] || SOL.sable.includes(sol[y][x])) continue;
      objets[y][x] = HERBES[(x + y) % 2]; herbe[y][x] = region;
    }
  };
  touffe(24, 7, 5, 4, "dojo"); touffe(28, 20, 6, 3, "duche"); touffe(7, 20, 6, 4, "germania");
  touffe(37, 18, 5, 3, "albion"); touffe(12, 6, 5, 4, "jardin"); touffe(37, 7, 4, 4, "observatoire");
  // nature: the pond near the port, rocks near the mountain, trees all around
  poser("mare", 36, 22, {passe: (i, j) => i === 0 || j === 0 || i === 4 || j === 4}); poser("barque", 37, 27);
  poser("rochers", 2, 20); poser("grandArbre", 27, 3); poser("grandArbre", 14, 2);
  const arbres = ["buisson1", "buisson2", "buisson3", "sapin1", "sapin2"];
  for (let x = 0; x < LARG; x += 2) { poser(arbres[(x / 2) % 5], x, 0); poser(arbres[(x / 2 + 2) % 5], x, HAUT - 2); }
  for (let y = 2; y < HAUT - 2; y += 2) { poser(arbres[(y / 2 + 1) % 5], 0, y); poser(arbres[(y / 2 + 3) % 5], LARG - 2, y); }
  [[3, 4], [15, 8], [29, 9], [3, 26], [12, 27], [30, 27], [33, 24], [15, 22], [16, 3], [16, 25]].forEach(([x, y], i) => {
    if (!bloque[y][x] && !bloque[y + 1][x + 1] && objets[y][x] < 0 && SOL.herbe.includes(sol[y][x])) poser(arbres[i % 5], x, y);
  });
  // a few flowers on the grass (not blocking)
  for (let y = 2; y < HAUT - 2; y++) for (let x = 2; x < LARG - 2; x++)
    if (objets[y][x] < 0 && !bloque[y][x] && SOL.herbe.includes(sol[y][x]) && hasard(x * 3, y * 7) < .05) objets[y][x] = DECOS[(x * y) % DECOS.length];
  // villagers on the square
  const pnj = [
    {x: 19, y: 14, sprite: 9, nom: "Le Sage", dir: 0, dit: ["Bienvenue à Académia, Gardien !", "Des Ombres se cachent dans les hautes herbes. Ton compagnon les combat avec ce que tu sais !", "Quand tu as vaincu quatre Ombres près d'une maison, sa porte s'ouvre : le chef des Ombres t'y attend."]},
    {x: 25, y: 15, sprite: 10, nom: "Hugo", dir: 2, dit: ["Chaque bonne réponse rend ton compagnon plus fort.", "Au niveau 15, il évolue !"]},
    {x: 22, y: 18, sprite: 14, nom: "Paco", dir: 1, dit: ["Touche l'endroit où tu veux aller, ton héros y marche tout seul.", "Ton équipe est dans le sac 🎒, en haut de l'écran."]}
  ];
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  portes.forEach(p => { bloque[p.y][p.x] = false; });
  // a sign above each house: the subject's icon and name
  const etiquettes = [["dojo", 21, 2.4], ["duche", 25, 19.4], ["germania", 7, 12.4], ["albion", 37, 12.4], ["jardin", 9.5, 4.4], ["observatoire", 35, 4.4]]
    .map(([region, x, y]) => ({region, x, y}));
  return {sol, objets, bloque, herbe, portes, pnj, etiquettes, depart: {x: 21, y: 16}};
}
