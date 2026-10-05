/* Académia : la table des pièces du village en haute définition (planche, place, taille, point d'ancrage, points
   nommés), écrite par outils/village/scripts/fabriquer.py : ne pas la modifier à la main. */
const PLANCHES_VILLAGE = {sol_chemin: 816, sol_place: 816, sol_touffe: 816, sol_details: 816, nature: 2032, maisons: 2048};   // image: width in HD pixels (the light one is half)
const PIECES_VILLAGE = {   // piece: [image, x, y, width, height in HD pixels (8 per world unit), anchor x, anchor y]
  mosaique: ["nature", 710, 1826, 384, 384, 0.5, 0.5], arbre_rond: ["nature", 1632, 1812, 336, 386, 0.5, 1], arbre_pommier: ["nature", 1288, 1812, 336, 386, 0.5, 1],
  arbre_fleuri: ["nature", 944, 1432, 336, 386, 0.5, 1], arbre_oranger: ["nature", 366, 1448, 336, 386, 0.5, 1], grand_arbre: ["nature", 4, 538, 462, 462, 0.5, 1],
  grand_cerisier: ["nature", 474, 978, 462, 462, 0.5, 1], sapin: ["nature", 944, 978, 282, 440, 0.5, 1], sapin_haut: ["nature", 1316, 4, 282, 484, 0.5, 1],
  sapin_sombre: ["nature", 1234, 984, 282, 440, 0.5, 1], foret_sapin: ["nature", 4, 4, 326, 526, 0.5, 1], foret_sapin_sombre: ["nature", 994, 4, 314, 494, 0.5, 1],
  foret_rond: ["nature", 1524, 1398, 354, 406, 0.5, 1], foret_rond_sombre: ["nature", 4, 1422, 354, 406, 0.5, 1], foret_sapin_neige: ["nature", 338, 4, 326, 526, 0.5, 1],
  foret_sapin_neige_sombre: ["nature", 672, 4, 314, 494, 0.5, 1], foret_cerisier: ["nature", 1524, 984, 354, 406, 0.5, 1], foret_cerisier_vif: ["nature", 4, 1008, 354, 406, 0.5, 1],
  foret_bas: ["nature", 364, 1842, 330, 272, 0.5, 1], foret_bas_sombre: ["nature", 364, 2122, 330, 272, 0.5, 1], foret_buisson: ["nature", 1102, 2206, 336, 168, 0.5, 1],
  foret_buisson_sombre: ["nature", 1446, 2206, 336, 168, 0.5, 1], herbe_dojo_0: ["nature", 868, 2638, 176, 80, 0.5, 0.9], herbe_dojo_fond_0: ["nature", 886, 2346, 176, 120, 0.5, 0.9333],
  herbe_dojo_1: ["nature", 1052, 2638, 176, 80, 0.5, 0.9], herbe_dojo_fond_1: ["nature", 1790, 2350, 176, 120, 0.5, 0.9333], herbe_dojo_2: ["nature", 1852, 2702, 176, 80, 0.5, 0.9],
  herbe_dojo_fond_2: ["nature", 4, 2372, 176, 120, 0.5, 0.9333], herbe_albion_0: ["nature", 4, 2628, 176, 80, 0.5, 0.9], herbe_albion_fond_0: ["nature", 702, 2218, 176, 120, 0.5, 0.9333],
  herbe_albion_1: ["nature", 1292, 2630, 176, 80, 0.5, 0.9], herbe_albion_fond_1: ["nature", 886, 2218, 176, 120, 0.5, 0.9333], herbe_albion_2: ["nature", 324, 2634, 176, 80, 0.5, 0.9],
  herbe_albion_fond_2: ["nature", 702, 2346, 176, 120, 0.5, 0.9333], herbe_germania_0: ["nature", 4, 2716, 176, 80, 0.5, 0.9], herbe_germania_fond_0: ["nature", 188, 2402, 176, 120, 0.5, 0.9333],
  herbe_germania_1: ["nature", 1236, 2718, 176, 80, 0.5, 0.9], herbe_germania_fond_1: ["nature", 372, 2402, 176, 120, 0.5, 0.9333], herbe_germania_2: ["nature", 188, 2722, 176, 80, 0.5, 0.9],
  herbe_germania_fond_2: ["nature", 556, 2474, 176, 120, 0.5, 0.9333], herbe_duche_0: ["nature", 508, 2706, 176, 80, 0.5, 0.9], herbe_duche_fond_0: ["nature", 1070, 2382, 176, 120, 0.5, 0.9333],
  herbe_duche_1: ["nature", 1476, 2710, 176, 80, 0.5, 0.9], herbe_duche_fond_1: ["nature", 1254, 2382, 176, 120, 0.5, 0.9333], herbe_duche_2: ["nature", 1660, 2710, 176, 80, 0.5, 0.9],
  herbe_duche_fond_2: ["nature", 1438, 2382, 176, 120, 0.5, 0.9333], herbe_jardin_0: ["nature", 692, 2726, 176, 80, 0.5, 0.9], herbe_jardin_fond_0: ["nature", 740, 2474, 176, 120, 0.5, 0.9333],
  herbe_jardin_1: ["nature", 876, 2726, 176, 80, 0.5, 0.9], herbe_jardin_fond_1: ["nature", 1622, 2478, 176, 120, 0.5, 0.9333], herbe_jardin_2: ["nature", 1844, 2790, 176, 80, 0.5, 0.9],
  herbe_jardin_fond_2: ["nature", 1806, 2478, 176, 120, 0.5, 0.9333], herbe_observatoire_0: ["nature", 372, 2794, 176, 80, 0.5, 0.9], herbe_observatoire_fond_0: ["nature", 4, 2500, 176, 120, 0.5, 0.9333],
  herbe_observatoire_1: ["nature", 1420, 2798, 176, 80, 0.5, 0.9], herbe_observatoire_fond_1: ["nature", 924, 2510, 176, 120, 0.5, 0.9333], herbe_observatoire_2: ["nature", 1604, 2798, 176, 80, 0.5, 0.9],
  herbe_observatoire_fond_2: ["nature", 1108, 2510, 176, 120, 0.5, 0.9333], fleurs_rouge: ["nature", 1612, 2606, 112, 96, 0.5, 0.9375], fleurs_jaune: ["nature", 628, 2602, 112, 96, 0.5, 0.9375],
  fleurs_rose: ["nature", 748, 2602, 112, 96, 0.5, 0.9375], fleurs_violet: ["nature", 1732, 2606, 112, 96, 0.5, 0.9375], fleurs_bleu: ["nature", 508, 2602, 112, 96, 0.5, 0.9375],
  fleurs_blanc: ["nature", 1492, 2510, 112, 96, 0.5, 0.9375], trefles: ["nature", 1196, 2806, 96, 72, 0.5, 0.8889], paquerettes: ["nature", 1300, 2806, 96, 64, 0.5, 0.875],
  tapis_trefles: ["nature", 1292, 2510, 192, 112, 0.5, 1], champignons: ["nature", 1852, 2606, 104, 88, 0.5, 1], souche: ["nature", 1060, 2726, 128, 80, 0.5, 1],
  pierres_plates: ["nature", 324, 2530, 176, 96, 0.5, 0.75], rochers: ["nature", 1316, 496, 544, 480, 0.5, 1], mare: ["nature", 672, 506, 464, 464, 0.5, 0.5],
  plage: ["nature", 4, 1836, 352, 336, 0.5, 0], roseaux: ["nature", 1790, 2206, 128, 136, 0.5, 0.9412], roseaux_petits: ["nature", 188, 2530, 128, 104, 0.5, 0.9231],
  barque: ["nature", 4, 2180, 256, 184, 0.5, 1], canard: ["nature", 556, 2794, 96, 80, 0.5, 0.825], fumee: ["nature", 92, 2804, 52, 44, 0.5, 0.5],
  lueur: ["nature", 4, 2804, 80, 80, 0.5, 0.5], eclat: ["nature", 1788, 2798, 48, 48, 0.5, 0.5], reflet: ["nature", 248, 2810, 40, 28, 0.5, 0.5],
  papillon: ["nature", 152, 2810, 40, 36, 0.5, 0.6], papillon_bleu: ["nature", 200, 2810, 40, 36, 0.5, 0.6], maison_dojo: ["maisons", 604, 572, 576, 536, 0.5, 1],
  panneau_dojo: ["maisons", 1188, 1072, 288, 200, 0.5, 1], maison_albion: ["maisons", 1204, 4, 584, 560, 0.5, 1], maison_germania: ["maisons", 604, 4, 592, 560, 0.5, 1],
  maison_duche: ["maisons", 4, 4, 592, 616, 0.5, 1], drapeau: ["maisons", 1484, 1072, 88, 58, 0, 0.04], maison_jardin: ["maisons", 1188, 572, 440, 492, 0.5, 1],
  maison_observatoire: ["maisons", 1636, 572, 336, 456, 0.5, 1], puits: ["maisons", 4, 628, 288, 360, 0.5, 1], lanterne: ["maisons", 1868, 1036, 176, 176, 0.5, 1],
  poteau: ["maisons", 1636, 1036, 224, 288, 0.5, 1], banc: ["maisons", 428, 1116, 280, 200, 0.5, 1], potager: ["maisons", 4, 996, 416, 304, 0.5, 1],
  tonneaux: ["maisons", 716, 1116, 168, 168, 0.5, 1]
};
const POINTS_VILLAGE = {   // piece: {point: [x, y] in world units from where the piece is posed}
  maison_dojo: {lanterne_g: [-26.5, -10.4], lanterne_d: [26.5, -10.4]},
  maison_albion: {phare: [24.5, -57.8]},
  maison_germania: {fumee: [17.0, -63.8]},
  maison_duche: {fumee: [19.6, -60.5], drapeau: [-24.1, -73.2]},
  maison_jardin: {eclat: [0.0, -56.8], fontaine: [18.0, -11.8]},
  maison_observatoire: {etoile: [0.0, -51.2]},
  lanterne: {lumiere: [0.0, -15.9]}
};
