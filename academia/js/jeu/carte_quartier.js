/* Académia : la carte d'un quartier du village (js/quartiers.js, QUARTIERS), sur la même grille de cases que le village,
   l'île et les salles des maisons. Les cinq quartiers ont la même forme, posée au milieu de la grille (QUARTIER_ZONE) et
   tenue entière à l'écran comme une salle : en haut, trois rangées de paysage où l'on ne marche pas (la grange, les
   étals, l'école, les grands arbres, la mer) ; dessous, le pré où le héros marche avec son compagnon ; en bas, la
   clôture, et au milieu le chemin qui ramène au village. Quatre choses à toucher (PLACES_QUARTIER), l'habitant au
   milieu, et dans les quartiers qui en ont (QUARTIERS[id].herbes) une touffe de hautes herbes où rôdent les Ombres
   d'une maison du village : leurs combats et leurs questions marchent tels quels.
   Même contrat que construireCarteMaison() (js/jeu/carte_maison.js) : sol, bloque, herbe, maison, portes (région
   "sortie" : le chemin du village), pnj, etiquettes, elements, depart, salle (la zone tenue à l'écran), choses
   [{...chose, x, y, w, h, devant}], avec quartier (son identifiant) à la place d'interieur.
   Dessin : js/jeu/quartier_village.js. */
const QUARTIER_ZONE = {x: 11, y: 9, w: 22, h: 13, mur: 3};   // in tiles of the grid; mur: the rows of landscape
// where the four things stand (tiles from the zone's corner), each 2 x 1 on the ground, drawn 2 x 2
const PLACES_QUARTIER = [{x: 2, y: 5}, {x: 6, y: 4}, {x: 14, y: 4}, {x: 18, y: 5}];
const HABITANT_QUARTIER = {x: 11, y: 5}, HERBES_QUARTIER = {x: 13, y: 8, w: 7, h: 3};

function construireCarteQuartier(id){
  const Q = QUARTIERS[id], Z = QUARTIER_ZONE;
  const grille = v => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  const sol = grille("dehors"), bloque = grille(true), herbe = grille(null), maison = grille(null);
  for (let y = Z.y + Z.mur; y < Z.y + Z.h - 1; y++) for (let x = Z.x + 1; x < Z.x + Z.w - 1; x++) { sol[y][x] = "pre"; bloque[y][x] = false; }
  // the way back: the two tiles of the path, in the bottom fence
  const mx = Z.x + Z.w / 2 - 1, bas = Z.y + Z.h - 1, portes = [];
  [mx, mx + 1].forEach(x => { sol[bas][x] = "chemin"; bloque[bas][x] = false; portes.push({x, y: bas, region: "sortie"}); });
  const choses = chosesQuartier(id).map((c, i) => {
    const x = Z.x + PLACES_QUARTIER[i].x, y = Z.y + PLACES_QUARTIER[i].y;
    bloque[y][x] = bloque[y][x + 1] = true;
    return {...c, x, y, w: 2, h: 1, devant: {x, y: y + 1}};
  });
  const elements = [];
  if (Q.herbes) {
    const H = HERBES_QUARTIER;
    elements.push({type: "touffe", x: Z.x + H.x, y: Z.y + H.y, w: H.w, h: H.h, region: Q.herbes});
    for (let y = Z.y + H.y; y < Z.y + H.y + H.h; y++) for (let x = Z.x + H.x; x < Z.x + H.x + H.w; x++) herbe[y][x] = Q.herbes;
  }
  const h = Q.habitant, pnj = [{x: Z.x + HABITANT_QUARTIER.x, y: Z.y + HABITANT_QUARTIER.y, cle: h.cle, nom: h.nom, dir: 0, dit: h.dit}];
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  return {sol, bloque, herbe, maison, portes, pnj, etiquettes: [], elements, depart: {x: mx, y: bas - 1}, quartier: id, salle: Z, choses};
}
