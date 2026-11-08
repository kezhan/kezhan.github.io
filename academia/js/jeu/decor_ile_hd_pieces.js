/* Académia : la table des pièces de l'Île des Bâtisseurs et de la traversée en haute définition (planche, place,
   taille, point d'ancrage), écrite par outils/village/scripts/ile.py : ne pas la modifier à la main. */
const PLANCHES_ILE = {ile: 1992, ile_barque: 1512};   // sheet: width in HD pixels (the light one is half)
const IMAGES_ILE = {sol_plage: [816, 544], sol_ecume: [816, 544], sol_herbe_ile: [816, 544], mer: [512, 512], traversee_fond: [2400, 1350]};   // images of their own (tiles of the ground, the sea, the crossing's backdrop): HD size
const PIECES_ILE = {   // piece: [sheet, x, y, width, height in HD pixels (8 per world unit), anchor x, anchor y]
  ponton: ["ile", 1372, 4, 560, 384, 0.0429, 0.0625], barque_quai: ["ile", 204, 460, 256, 192, 0.5, 1], terrain: ["ile", 820, 4, 544, 432, 0.5, 1],
  palmier: ["ile", 4, 4, 400, 448, 0.5, 0.9688], palmier_penche: ["ile", 412, 4, 400, 448, 0.39, 0.9688], oyats: ["ile", 636, 580, 128, 112, 0.5, 0.9286],
  oyats_2: ["ile", 1740, 628, 128, 112, 0.5, 0.9286], rochers_plage: ["ile", 1508, 628, 224, 160, 0.5, 0.925], rochers_plage_petits: ["ile", 636, 460, 176, 112, 0.5, 0.8929],
  tas_planches: ["ile", 1220, 628, 280, 168, 0.5, 1], brouette: ["ile", 468, 460, 160, 172, 0.5, 1], mouette_0: ["ile", 1876, 628, 112, 80, 0.5, 0.5],
  mouette_1: ["ile", 468, 640, 112, 80, 0.5, 0.5], vague: ["ile", 276, 660, 112, 32, 0.5, 0.5], ecume: ["ile", 204, 660, 64, 36, 0.5, 0.5],
  panneau_chantier_boulangerie: ["ile", 1572, 396, 192, 224, 0.5, 0.9732], panneau_chantier_phare: ["ile", 1020, 444, 192, 224, 0.5, 0.9732], panneau_chantier_moulin: ["ile", 820, 444, 192, 224, 0.5, 0.9732],
  panneau_chantier_bibliotheque: ["ile", 1372, 396, 192, 224, 0.5, 0.9732], panneau_chantier_serre: ["ile", 4, 460, 192, 224, 0.5, 0.9732], panneau_chantier_kiosque: ["ile", 1772, 396, 192, 224, 0.5, 0.9732],
  barque_mer_dos: ["ile_barque", 804, 4, 704, 304, 0.5, 0.7632], barque_mer_devant: ["ile_barque", 92, 4, 704, 304, 0.5, 0.7632], godille: ["ile_barque", 4, 4, 80, 360, 0.5, 0.0278]
};
const POINTS_ILE = {};   // piece: {point: [x, y] in world units from where the piece is posed}
