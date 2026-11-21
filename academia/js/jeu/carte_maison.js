/* Académia : l'intérieur des six maisons (ticket village-plus-grand-maisons-a-explorer, lot 1). Chaque porte du village
   ouvre dès la première visite sur une salle où le héros marche avec son compagnon ; c'est un lieu comme le village et
   l'île (P().lieu = "maison_<région>", js/etat.js), montré par la même scène (js/jeu/monde.js) sur la même grille de
   cases, la salle posée au milieu (SALLE) et tout le reste bloqué.
   La salle, la même forme pour les six : le mur du fond (trois rangées), la porte de la salle du chef au milieu du mur,
   le plancher, le paillasson de la sortie en bas. Quatre choses à toucher sur le sujet de la maison (deux contre le
   mur, deux sur le plancher), et dans une maison de langue un habitant qui salue dans sa langue.
   MAISONS[région] : {habitant: {cle, nom, dit}, choses: [{id, nom, dits: [deux choses vraies, dites tour à tour],
   petit: [les mêmes pour la petite, plus courtes], notions: [celles de son exercice], mot et sens: le mot de la langue de la
   maison qui la nomme et ce qu'il veut dire (seulement s'il a son enregistrement : donnees/voix_etrangeres.json, lod.lu), lunette: vrai pour
   la lunette de l'Observatoire, qui ouvre la coupole]}.
   Même contrat que construireCarte() (sol, bloque, herbe, maison, portes, pnj, etiquettes, elements, depart), avec en
   plus : interieur (la région), salle (SALLE), choses [{...chose, x, y, w, h, devant: {x, y}}] en cases de la grille,
   porteChef {x, y, w, h}. Les portes : région "chef" (devant la salle du chef) et "sortie" (le paillasson).
   Dessin : js/jeu/interieur_maison.js ; ce que la chose touchée dit et demande : js/maison_chose.js. */
const SALLE = {x: 14, y: 11, w: 16, h: 9, mur: 3};   // in tiles of the grid; mur: the rows of the back wall
// where the four things stand in the room (tiles from its corner), each 2 x 1 on the floor, drawn 2 x 2
const PLACES_CHOSES = [{x: 2, y: 3}, {x: 12, y: 3}, {x: 3, y: 6}, {x: 11, y: 6}];
const lieuMaison = region => "maison_" + region;
const maisonDuLieu = lieu => typeof lieu === "string" && lieu.startsWith("maison_") && MAISONS[lieu.slice(7)] ? lieu.slice(7) : null;

const MAISONS = {
  dojo: {choses: [
    {id: "boulier", nom: "le boulier", notions: ["denombrer_10", "add_10", "add_20", "add_100"],
     dits: ["Sur un boulier, on compte en faisant glisser des boules. On s'en sert depuis des milliers d'années !",
            "Sur un boulier d'école, chaque tige porte dix boules. Avec dix tiges, cela fait cent boules."],
     petit: ["C'est un boulier ! On fait glisser les boules pour compter.", "Sur le boulier, on compte les boules : une, deux, trois…"]},
    {id: "balance", nom: "la balance", notions: ["comparer_quantites", "comparer_20", "comparer_100", "ranger_10000"],
     dits: ["Une balance compare deux poids : le plateau le plus lourd descend.",
            "Quand les deux plateaux sont à la même hauteur, les deux côtés pèsent autant : c'est l'égalité."],
     petit: ["C'est une balance ! Le côté le plus lourd descend.", "Quand la balance est toute droite, les deux côtés pèsent pareil."]},
    {id: "gateau", nom: "le gâteau", notions: ["doubles_moities", "partage", "fractions_parts", "fraction_quantite"],
     dits: ["Un gâteau coupé en deux parts égales : chaque part est une moitié.",
            "Un gâteau coupé en quatre parts égales : chaque part est un quart. Deux quarts font une moitié !"],
     petit: ["Un gâteau coupé en deux : cela fait deux moitiés.", "Pour partager, on fait des parts pareilles pour chacun."]},
    {id: "des", nom: "les dés", notions: ["denombrer_5", "complements_10", "tables_2_5_10", "tables_toutes"],
     dits: ["Sur un dé, les deux faces opposées font toujours sept à elles deux : un et six, deux et cinq, trois et quatre.",
            "Un dé a six faces. Avec deux dés, le plus grand total possible est douze : six plus six."],
     petit: ["Un dé a six faces. Sur chaque face, on compte les points !", "Sur un dé, le plus grand nombre est six."]}
  ]},
  albion: {habitant: {cle: "marin", nom: "Le marin", dit: ["Ici, tout se dit en anglais ! Touche les choses de la salle : elles te disent leur nom."]}, choses: [
    {id: "chat", nom: "le chat du bord", mot: "cat", sens: "le chat", notions: ["en_animaux"],
     dits: ["Autrefois, les marins emmenaient un chat sur leur bateau : il chassait les souris.",
            "Un chat voit très bien dans la pénombre, et ses moustaches l'aident à sentir ce qui l'entoure."],
     petit: ["Sur les bateaux, le chat chasse les souris.", "Le chat voit très bien quand il fait sombre."]},
    {id: "livre", nom: "le livre de bord", mot: "book", sens: "le livre", notions: ["en_ecole", "en_jours", "en_nombres"],
     dits: ["Chaque jour, le capitaine écrit dans son livre de bord la route du bateau et le temps qu'il fait.",
            "L'anglais est une des langues les plus parlées du monde : on l'apprend dans presque tous les pays."],
     petit: ["Le capitaine écrit dans son livre, chaque jour.", "On parle anglais dans beaucoup de pays."]},
    {id: "poisson", nom: "le poisson", mot: "fish", sens: "le poisson", notions: ["en_animaux", "en_couleurs"],
     dits: ["Un poisson respire dans l'eau grâce à ses branchies : il n'a pas besoin de remonter à la surface.",
            "Le poisson avec des frites est un plat très aimé en Angleterre."],
     petit: ["Le poisson respire dans l'eau.", "En Angleterre, on mange le poisson avec des frites."]},
    {id: "lait", nom: "le thé au lait", mot: "milk", sens: "le lait", notions: ["en_nourriture", "en_salutations"],
     dits: ["En Angleterre, on boit souvent le thé avec un nuage de lait.",
            "Le lait vient de la vache : avec lui, on fait le beurre, le fromage et les yaourts."],
     petit: ["En Angleterre, on met du lait dans le thé.", "Le lait vient de la vache."]}
  ]},
  germania: {habitant: {cle: "horlogere", nom: "L'horlogère", dit: ["Ici, tout se dit en allemand ! Touche les choses de la salle : elles te disent leur nom."]}, choses: [
    {id: "pain", nom: "le pain", mot: "das Brot", sens: "le pain", notions: ["de_nourriture", "de_nombres"],
     dits: ["En Allemagne, on fait plus de trois mille sortes de pain : c'est le pays du pain !",
            "Le bretzel, ce pain en forme de nœud, est une spécialité du sud de l'Allemagne."],
     petit: ["En Allemagne, il y a beaucoup, beaucoup de sortes de pain.", "Le bretzel est un pain en forme de nœud."]},
    {id: "ours", nom: "l'ours", mot: "der Bär", sens: "l'ours", notions: ["de_animaux", "de_couleurs"],
     dits: ["L'ours est l'animal de Berlin, la capitale de l'Allemagne : il est dessiné sur son drapeau.",
            "En hiver, l'ours dort de longs mois dans sa tanière : il vit sur ses réserves de graisse."],
     petit: ["L'ours est l'animal de Berlin, une grande ville d'Allemagne.", "En hiver, l'ours dort très longtemps."]},
    {id: "contes", nom: "le livre de contes", mot: "das Buch", sens: "le livre", notions: ["de_ecole", "de_famille", "de_jours"],
     dits: ["Les frères Grimm, deux Allemands, ont rassemblé des contes célèbres : Hansel et Gretel, Blanche-Neige, Raiponce.",
            "Le premier livre imprimé avec des lettres mobiles l'a été en Allemagne, par Gutenberg, il y a plus de cinq cents ans."],
     petit: ["Blanche-Neige est un conte qui vient d'Allemagne.", "Dans un livre de contes, il y a plein d'histoires."]},
    {id: "neige", nom: "la neige sur la montagne", mot: "der Schnee", sens: "la neige", notions: ["de_meteo", "de_salutations"],
     dits: ["La plus haute montagne d'Allemagne s'appelle la Zugspitze : elle monte à presque trois mille mètres.",
            "Chaque flocon de neige est une petite étoile de glace à six branches."],
     petit: ["En haut des montagnes, il y a de la neige.", "Un flocon de neige a six branches, comme une étoile."]}
  ]},
  duche: {habitant: {cle: "boulangere", nom: "La boulangère", dit: ["Ici, tout se dit en luxembourgeois ! Touche les choses de la salle : elles te disent leur nom."]}, choses: [
    {id: "lion", nom: "le lion rouge", mot: "de Léiw", sens: "le lion", notions: ["lb_animaux", "lb_couleurs"],
     dits: ["Le lion rouge est l'emblème du Luxembourg : on le voit sur ses armoiries.",
            "Le lion du Luxembourg porte une couronne d'or, sur des bandes blanches et bleues."],
     petit: ["Le lion rouge, c'est l'animal du Luxembourg.", "Le lion du Luxembourg porte une couronne."]},
    {id: "pommes_de_terre", nom: "les pommes de terre", mot: "d'Gromper", sens: "la pomme de terre", notions: ["lb_nourriture", "lb_nombres"],
     dits: ["Au Luxembourg, on fait des galettes de pommes de terre râpées : on les mange aux fêtes et sur les marchés.",
            "La pomme de terre pousse sous la terre : c'est la plante qui la nourrit avec ses feuilles."],
     petit: ["Au Luxembourg, on mange des galettes de pommes de terre.", "La pomme de terre pousse sous la terre."]},
    {id: "ecole", nom: "le livre d'école", mot: "d'Buch", sens: "le livre", notions: ["lb_ecole", "lb_jours", "lb_famille"],
     dits: ["À l'école, au Luxembourg, les enfants apprennent trois langues : le luxembourgeois, l'allemand et le français.",
            "Le Luxembourg est un grand-duché : le seul du monde. Son chef est le Grand-Duc."],
     petit: ["Au Luxembourg, on parle trois langues à l'école.", "Au Luxembourg, il y a un Grand-Duc."]},
    {id: "renard", nom: "le renard malin", mot: "de Fuuss", sens: "le renard", notions: ["lb_animaux", "lb_salutations"],
     dits: ["Un renard malin est le héros d'un livre très connu au Luxembourg, écrit il y a plus de cent cinquante ans.",
            "Le renard vit aussi près des villes : il sort surtout le soir et la nuit."],
     petit: ["Au Luxembourg, un renard malin est le héros d'une histoire.", "Le renard sort le soir et la nuit."]}
  ]},
  jardin: {choses: [
    {id: "echiquier", nom: "l'échiquier", notions: ["lo_echecs", "lo_echecs_regles", "lo_rang", "lo_couleurs"],
     dits: ["Un échiquier a soixante-quatre cases : huit rangées de huit, une claire, une foncée, et ainsi de suite.",
            "Aux échecs, chaque pièce a sa façon d'avancer : la tour va tout droit, le fou en diagonale."],
     petit: ["Sur l'échiquier, les cases sont claires et foncées, chacune à son tour.", "Sur l'échiquier, chaque pièce a sa case."]},
    {id: "solides", nom: "les formes", notions: ["lo_formes", "lo_formes_cotes", "lo_solides"],
     dits: ["Un cube a six faces, toutes carrées. Une boule n'en a aucune : elle roule dans tous les sens.",
            "Un triangle a trois côtés, un carré en a quatre, tous de la même longueur."],
     petit: ["Le cube a des faces carrées. La boule roule !", "Le triangle a trois côtés. Le carré en a quatre."]},
    {id: "perles", nom: "le collier de perles", notions: ["lo_suites", "lo_suites_nombres", "lo_suites_longues"],
     dits: ["Sur ce collier, les couleurs reviennent toujours dans le même ordre : c'est une suite.",
            "Quand on a trouvé la règle d'une suite, on sait ce qui vient après, même très loin."],
     petit: ["Sur le collier, les couleurs reviennent toujours pareil.", "Rouge, jaune, rouge, jaune : après, c'est rouge !"]},
    {id: "poupees", nom: "les poupées gigognes", notions: ["lo_ordre", "lo_intrus", "lo_deduction"],
     dits: ["Les poupées gigognes se rangent l'une dans l'autre, de la plus petite à la plus grande.",
            "Pour ranger, on compare deux par deux : plus petit, plus grand. C'est mettre en ordre."],
     petit: ["Les poupées se rangent de la plus petite à la plus grande.", "La petite poupée se cache dans la grande."]}
  ]},
  observatoire: {choses: [
    {id: "lunette", nom: "la lunette", lunette: true, notions: ["sc_espace", "sc_jour_nuit"],
     dits: ["Une lunette fait voir de près ce qui est très loin : la Lune, les planètes, les étoiles."],
     petit: ["Avec la lunette, on voit les planètes de tout près !"]},
    {id: "globe", nom: "le globe", notions: ["sc_monde", "sc_ocean", "sc_luxembourg", "sc_milieux", "sc_meteo"],
     dits: ["La Terre est ronde comme une boule. Le bleu, c'est l'eau des océans : il y en a bien plus que de terres.",
            "La Terre tourne sur elle-même en un jour : c'est pour cela qu'il fait jour, puis nuit."],
     petit: ["La Terre est ronde comme un ballon. Le bleu, c'est la mer.", "La Terre tourne : il fait jour, puis il fait nuit."]},
    {id: "aimant", nom: "l'aimant", notions: ["sc_matiere", "sc_eau"],
     dits: ["Un aimant attire le fer, mais pas le bois, ni le verre, ni le plastique.",
            "Un aimant a deux pôles. Deux pôles pareils se repoussent, deux pôles différents s'attirent."],
     petit: ["L'aimant attire le fer : clac !", "L'aimant n'attire pas le bois."]},
    {id: "fossile", nom: "le fossile", notions: ["sc_dinosaures", "sc_animaux_etonnants", "sc_vivant", "sc_animaux_petits"],
     dits: ["Un fossile est la trace d'un animal ou d'une plante d'il y a très longtemps, gardée dans la pierre.",
            "Grâce aux fossiles, on sait que les dinosaures ont existé : on a retrouvé leurs os dans la roche."],
     petit: ["Un fossile, c'est un très vieil animal gardé dans la pierre.", "On a trouvé des os de dinosaures dans la pierre."]}
  ]}
};

function construireCarteMaison(region){
  const M = MAISONS[region], S = SALLE;
  const grille = v => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  const sol = grille("dehors"), bloque = grille(true), herbe = grille(null), maison = grille(null);
  // the floor: everything under the back wall but the side walls; the bottom row is a wall with the doormat in it
  for (let y = S.y + S.mur; y < S.y + S.h - 1; y++) for (let x = S.x + 1; x < S.x + S.w - 1; x++) { sol[y][x] = "plancher"; bloque[y][x] = false; }
  const mx = S.x + S.w / 2 - 1, portes = [];
  // the way out: the two tiles of the doormat, in the bottom wall
  [mx, mx + 1].forEach(x => { sol[S.y + S.h - 1][x] = "plancher"; bloque[S.y + S.h - 1][x] = false; portes.push({x, y: S.y + S.h - 1, region: "sortie"}); });
  // the chief's room: its door in the back wall (a touch on it walks to the tiles in front of it)
  const porteChef = {x: mx, y: S.y + 1, w: 2, h: 2};
  for (let y = S.y; y < S.y + S.mur; y++) for (let x = mx; x < mx + 2; x++) maison[y][x] = "chef";
  [mx, mx + 1].forEach(x => portes.push({x, y: S.y + S.mur, region: "chef"}));
  const choses = M.choses.map((c, i) => {
    const x = S.x + PLACES_CHOSES[i].x, y = S.y + PLACES_CHOSES[i].y;
    bloque[y][x] = bloque[y][x + 1] = true;
    return {...c, region, x, y, w: 2, h: 1, devant: {x, y: y + 1}};
  });
  const h = M.habitant, pnj = h ? [{x: S.x + 10, y: S.y + 4, cle: h.cle, nom: h.nom, dir: 0, dit: h.dit}] : [];
  pnj.forEach(n => { bloque[n.y][n.x] = true; });
  return {sol, bloque, herbe, maison, portes, pnj, etiquettes: [], elements: [], depart: {x: mx, y: S.y + S.h - 2},
    interieur: region, salle: S, choses, porteChef};
}
