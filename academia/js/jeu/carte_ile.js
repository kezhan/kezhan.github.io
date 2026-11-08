/* Académia : l'Île des Bâtisseurs, le deuxième village (ticket plusieurs-villages), construite en code comme le premier
   (js/jeu/carte.js) et sur la même grille de cases : la mer tout autour, une plage de sable, un ponton à l'ouest où la
   barque accoste (le passeur s'y tient), des sentiers de sable qui partent d'une clairière (la cheffe de chantier et ses
   matériaux), quatre touffes de hautes herbes rattachées à des maisons du premier village, le Port d'Albion, la
   Montagne de Germania, le Dojo des Nombres et la Vallée du Grand-Duché (leurs Ombres, leurs combats et leurs questions
   marchent tels quels ; le Jardin et l'Observatoire n'y ont pas la place sans serrer les terrains) et six terrains vides, chacun avec son panneau de chantier, où se feront les
   bâtiments du ticket construire-et-collectionner.
   Même contrat que construireCarte() (sol, bloque, herbe, maison, portes, pnj, etiquettes, elements, depart), avec en
   plus : ile (vrai), terrains [{id, x, y, w, h, panneau: {x, y}, devant: {x, y}}], ponton {x, y, w, h}, barque {x, y, w,
   h} (la barque à quai), arrivee (la case du ponton où l'on débarque). Le sol d'une case : eau, plage, herbe, sable
   (sentiers, clairière) ou ponton. Dessin : js/jeu/decor_ile_hd.js. */
// the six buildings to come (ticket construire-et-collectionner), each on its lot: what the cheffe says of it
const CHANTIERS = {
  boulangerie: {nom: "la boulangerie", matiere: "maths", dit: "On y comptera les pains et on y partagera les gâteaux."},
  phare: {nom: "le phare", matiere: "sciences", dit: "Sa lumière guidera les bateaux, la nuit."},
  moulin: {nom: "le moulin", matiere: "sciences", dit: "Le vent fera tourner ses ailes, pour moudre la farine."},
  bibliotheque: {nom: "la bibliothèque", matiere: "langues", dit: "Elle sera pleine de livres et de mots de toutes les langues."},
  serre: {nom: "la serre", matiere: "sciences", dit: "Les plantes y pousseront au chaud, même en hiver."},
  kiosque: {nom: "le kiosque à musique", matiere: "logique", dit: "On y jouera de la musique, en rythme !"}
};

function construireCarteIle(){
  const grille = v => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  const sol = grille("eau"), pris = grille(false), bloque = grille(true), herbe = grille(null), maison = grille(null);
  const elements = [];
  const hasard = (x, y) => Math.abs(Math.sin(x * 12.9898 + y * 78.233) * 43758.5453) % 1;   // same island every time
  const dans = (x, y) => x >= 0 && y >= 0 && x < LARG && y < HAUT;
  const remplir = (x0, y0, w, h, v) => { for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) if (dans(x, y)) sol[y][x] = v; };
  // the land: a body with a bump to the north and the south, the west shore where the pier stands, a point to the east
  // (the lighthouse's), a cove cut in the north-east and the corners rounded off
  [[9, 6, 26, 19], [11, 5, 20, 1], [12, 25, 19, 1], [8, 10, 1, 12], [35, 11, 3, 7], [38, 13, 1, 3]].forEach(([x, y, w, h]) => remplir(x, y, w, h, "terre"));
  [[31, 6, 4, 2], [33, 8, 2, 2], [9, 6, 1, 2], [9, 22, 2, 3], [33, 22, 2, 3], [11, 25, 2, 1]].forEach(([x, y, w, h]) => remplir(x, y, w, h, "eau"));
  // the beach: the land one tile from the water (two on the west shore, where the boat comes in)
  const pres = (x, y, r) => { for (let b = y - r; b <= y + r; b++) for (let a = x - r; a <= x + r; a++) if (!dans(a, b) || sol[b][a] === "eau") return true; return false; };
  for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++)
    if (sol[y][x] === "terre") sol[y][x] = pres(x, y, 1) || (x <= 10 && y >= 12 && y <= 19) ? "plage" : "herbe";
  for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) if (sol[y][x] !== "eau") bloque[y][x] = false;
  // the pier, west of the beach, two tiles wide; the boat moored along it, on the sea side
  const ponton = {x: 4, y: 15, w: 4, h: 2}, barque = {x: 4, y: 17, w: 2, h: 1};
  remplir(ponton.x, ponton.y, ponton.w, ponton.h, "ponton");
  for (let y = ponton.y; y < ponton.y + ponton.h; y++) for (let x = ponton.x; x < ponton.x + ponton.w; x++) bloque[y][x] = false;
  elements.push({type: "ponton", ...ponton}, {type: "barque_quai", ...barque});
  // sandy paths: from the pier east across the island to the lighthouse's point, through the clearing in the middle;
  // one north and one south, to the lots
  const sentier = (x0, y0, w, h) => {
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) if (dans(x, y) && sol[y][x] === "herbe") sol[y][x] = "sable";
  };
  sentier(11, 15, 23, 2);   // the main path, from the beach by the pier
  sentier(18, 12, 7, 6);    // the clearing, where the cheffe keeps her materials
  sentier(15, 11, 12, 1);   // north, to the bakery and the mill
  sentier(21, 18, 2, 1); sentier(14, 19, 14, 1);   // south, to the library and the greenhouse
  elements.push({type: "clairiere", x: 18, y: 12, w: 7, h: 6});
  // the six lots (4 x 3, blocked like a house: nobody walks on a building to come), their sign beside them, on a tile
  // of its own, and the path tile next to the sign where the hero stands to hear the cheffe
  const terrains = [];
  const terrain = (id, x, y, px, py) => {
    for (let b = y; b < y + 3; b++) for (let a = x; a < x + 4; a++) { bloque[b][a] = true; pris[b][a] = true; }   // its ground stays below
    bloque[py][px] = true; pris[py][px] = true;
    const devant = [[0, 1], [0, -1], [-1, 0], [1, 0]].map(([a, b]) => ({x: px + a, y: py + b}))
      .find(c => dans(c.x, c.y) && !bloque[c.y][c.x] && sol[c.y][c.x] === "sable") || {x: px, y: py + 1};
    const t = {id, x, y, w: 4, h: 3, panneau: {x: px, y: py}, devant};
    terrains.push(t);
    elements.push({type: "terrain", chantier: id, x, y, w: 4, h: 3}, {type: "panneau_chantier", chantier: id, x: px, y: py, w: 1, h: 1});
  };
  terrain("boulangerie", 12, 7, 15, 10);
  terrain("moulin", 25, 7, 25, 10);
  terrain("kiosque", 11, 12, 15, 14);
  terrain("phare", 34, 12, 33, 14);
  terrain("bibliotheque", 13, 21, 16, 20);
  terrain("serre", 24, 21, 24, 20);
  // tall grass where the Ombres hide, each patch tied to a house of the first village (its Ombres, its questions)
  const touffe = (x0, y0, w, h, region) => {
    elements.push({type: "touffe", x: x0, y: y0, w, h, region});
    for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) {
      if (!dans(x, y) || bloque[y][x] || sol[y][x] !== "herbe") continue;
      pris[y][x] = true; herbe[y][x] = region;
    }
  };
  touffe(17, 7, 6, 3, "germania");   // the north meadow, between the bakery and the mill: the Montagne de Germania's Ombres
  touffe(28, 17, 5, 3, "albion");    // by the east shore: the Port d'Albion's (English)
  touffe(18, 21, 5, 3, "dojo");      // the south meadow, between the library and the greenhouse: the Dojo des Nombres'
  touffe(27, 11, 5, 3, "duche");     // the east meadow, by the mill: the Vallée du Grand-Duché's (Luxembourgish, as the ferryman)
  const poser = (type, nom, x, y, w, h) => {
    elements.push({type, nom, x, y, w, h});
    for (let b = y; b < y + h; b++) for (let a = x; a < x + w; a++) if (dans(a, b)) { pris[b][a] = true; bloque[b][a] = true; }
  };
  const libre = (x, y, w = 1, h = 1) => { for (let b = y; b < y + h; b++) for (let a = x; a < x + w; a++) if (!dans(a, b) || bloque[b][a] || pris[b][a] || sol[b][a] === "eau") return false; return true; };
  // the cheffe's materials on the clearing, palm trees on the beach, trees in the meadow, rocks on the sand
  poser("lieu_ile", "tas_planches", 18, 12, 2, 1); poser("lieu_ile", "brouette", 24, 12, 1, 1);
  [[10, 7, "palmier"], [9, 19, "palmier_penche"], [11, 23, "palmier"], [30, 24, "palmier_penche"], [30, 5, "palmier_penche"], [37, 11, "palmier_penche"]]
    .forEach(([x, y, nom]) => { if (libre(x, y)) poser("arbre_ile", nom, x, y, 1, 1); });
  [[29, 8, "buisson1"], [13, 17, "sapin1"], [29, 21, "buisson2"], [10, 22, "buisson3"]]
    .forEach(([x, y, nom]) => { if (libre(x, y, 2, 2)) poser("arbre", nom, x, y, 2, 2); });
  [[35, 19, "rochers_plage"], [21, 25, "rochers_plage_petits"], [12, 5, "rochers_plage_petits"], [9, 13, "rochers_plage_petits"]]
    .forEach(([x, y, nom]) => { if (libre(x, y)) poser("lieu_ile", nom, x, y, 1, 1); });
  // flowers on the meadow (not blocking), like the first village's
  for (let y = 1; y < HAUT - 1; y++) for (let x = 1; x < LARG - 1; x++)
    if (!pris[y][x] && !bloque[y][x] && sol[y][x] === "herbe" && hasard(x * 3, y * 7) < .07) {
      pris[y][x] = true; elements.push({type: "fleur", x, y, w: 1, h: 1, sorte: (x * y) % SORTES_FLEURS});
    }
  // the two islanders: the ferryman on his pier, the cheffe on her clearing (js/jeu/ile_village.js, js/jeu/voyage.js)
  const pnj = [
    {x: 4, y: 15, cle: "passeur", nom: "Le passeur", dir: 3, passeur: true, dit: ["Ma barque te ramène au village quand tu veux."]},
    {x: 21, y: 13, cle: "cheffe", nom: "La cheffe de chantier", dir: 0, dit: ["Je suis la cheffe de chantier de l'Île des Bâtisseurs !"]}
  ];
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  const arrivee = {x: 5, y: 16};   // on the pier, beside the moored boat
  return {sol, bloque, herbe, maison, portes: [], pnj, etiquettes: [], elements, depart: arrivee, arrivee, ile: true, terrains, ponton, barque};
}
