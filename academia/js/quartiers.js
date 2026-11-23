/* Académia : les quartiers du village (ticket quartiers-du-village). Autour du premier village, cinq lieux où l'on va
   par un chemin qui sort de la carte : la ferme (sud), le marché (sud-ouest), l'école (nord), la forêt et sa clairière
   (ouest), la plage (est). Chacun est un lieu comme l'île ou une salle de maison (P().lieu = "quartier_<id>",
   js/etat.js), montré par la même scène (js/jeu/monde.js) : sa carte dans js/jeu/carte_quartier.js, son dessin et ce
   qui s'y passe dans js/jeu/quartier_village.js, les sorties du village dans js/jeu/sorties_village.js, le plan du
   village dans js/plan_village.js.
   QUARTIERS[id] : {nom (« la ferme »), a (« à la ferme »), langue : celle de son habitant et du nom de ses choses,
   habitant: {cle, nom, dit}, herbes : la maison dont les Ombres rôdent dans ses hautes herbes (aucune au marché et à
   l'école), choses : quatre choses à toucher, comme dans une maison (js/jeu/carte_maison.js, MAISONS), avec en plus
   leur matière (matiere) : l'écran et la règle sont ceux de js/maison_chose.js (une étincelle par chose et par jour).
   Un mot étranger (mot, sens, langue) n'est dit que s'il a son enregistrement (donnees/voix_etrangeres.json, lod.lu).
   Les quartiers portent les notions de découverte qui n'avaient pas de lieu : la ferme, bien manger (le marché), les
   insectes (la forêt), l'océan et le temps qu'il fait (la plage) ; l'école, les nombres et les mots de l'école dans
   les trois langues. */
const QUARTIERS = {
  ecole: {nom: "l'école", a: "à l'école", langue: "lb",
    habitant: {cle: "maitresse", nom: "La maîtresse", dit: ["Bienvenue à l'école ! Ici, on apprend à compter, et les mots de l'école dans trois langues.", "Touche les choses de la cour : chacune a un secret, et une étincelle pour toi."]},
    choses: [
      {id: "tableau", nom: "le tableau", matiere: "maths", notions: ["denombrer_10", "add_10", "sous_10", "add_20", "tables_2_5_10", "add_100"],
       dits: ["Sur le tableau, la maîtresse écrit les calculs à la craie. Un coup d'éponge, et tout s'efface !",
              "Nos dix chiffres, de zéro à neuf, suffisent pour écrire tous les nombres, même les plus grands."],
       petit: ["C'est le tableau ! On y écrit les nombres avec une craie.", "Avec les chiffres, on écrit tous les nombres."]},
      {id: "crayons", nom: "le pot à crayons", matiere: "anglais", langue: "en", mot: "pencil", sens: "le crayon", notions: ["en_ecole", "en_couleurs", "en_nombres"],
       dits: ["La mine d'un crayon à papier est faite de graphite et d'argile. Plus il y a d'argile, plus le trait est clair.",
              "Avec trois couleurs, le jaune, le rouge et le bleu, on peut fabriquer presque toutes les autres en les mélangeant."],
       petit: ["Avec un crayon, on écrit et on dessine.", "Du jaune et du bleu mélangés, cela fait du vert !"]},
      {id: "cartable", nom: "le cartable", matiere: "allemand", langue: "de", mot: "der Schulranzen", sens: "le cartable", notions: ["de_ecole", "de_jours", "de_nombres"],
       dits: ["En Allemagne, le jour de la rentrée, les enfants reçoivent un grand cornet rempli de surprises et de bonbons.",
              "Dans un cartable, on range les cahiers, la trousse et le goûter. Il se porte sur les deux épaules, pour le dos."],
       petit: ["Dans le cartable, on range les cahiers et le goûter.", "Le cartable se porte sur le dos."]},
      {id: "cloche", nom: "la cloche de l'école", matiere: "luxembourgeois", langue: "lb", mot: "d'Schoul", sens: "l'école", notions: ["lb_ecole", "lb_jours", "lb_nombres"],
       dits: ["Quand la cloche sonne, la classe commence. Elle sonne encore pour la récréation !",
              "Une cloche sonne parce que son métal vibre quand on la frappe : le son voyage dans l'air jusqu'à nos oreilles."],
       petit: ["Ding, dong ! La cloche sonne : la classe commence.", "Quand la cloche sonne encore, c'est la récréation !"]}
    ]},
  marche: {nom: "le marché", a: "au marché", langue: "de",
    habitant: {cle: "maraicher", nom: "Le maraîcher", dit: ["Bienvenue au marché ! Ici, on apprend à bien manger.", "Touche mes étals : chacun a un secret, et une étincelle pour toi."]},
    choses: [
      {id: "fruits", nom: "les fruits", matiere: "sciences", mot: "der Apfel", sens: "la pomme", notions: ["sc_alimentation", "sc_plantes", "sc_saisons"],
       dits: ["Un fruit vient d'une fleur, et il garde les graines de la plante : les pépins de la pomme, le noyau de la cerise.",
              "Les fruits donnent des vitamines : on conseille d'en manger chaque jour, avec des légumes."],
       petit: ["Dans la pomme, il y a des pépins : ce sont ses graines.", "Les fruits sont bons pour toi : manges-en chaque jour !"]},
      {id: "legumes", nom: "les légumes", matiere: "sciences", mot: "die Karotte", sens: "la carotte", notions: ["sc_alimentation", "sc_plantes", "sc_vivant"],
       dits: ["On ne mange pas la même partie de chaque légume : la racine de la carotte, les feuilles de la salade, le fruit de la tomate.",
              "La carotte est une racine : elle pousse sous la terre, et seules ses feuilles dépassent."],
       petit: ["La carotte pousse sous la terre.", "De la salade, on mange les feuilles."]},
      {id: "fromage", nom: "le fromage", matiere: "sciences", mot: "der Käse", sens: "le fromage", notions: ["sc_alimentation", "sc_ferme", "sc_matiere"],
       dits: ["Le fromage est fait avec du lait : celui de la vache, de la chèvre ou de la brebis.",
              "Le lait, les yaourts et le fromage apportent du calcium, qui aide les os et les dents à être solides."],
       petit: ["Le fromage est fait avec du lait.", "Le lait et le fromage rendent tes os solides."]},
      {id: "miel", nom: "le miel", matiere: "sciences", mot: "die Biene", sens: "l'abeille", notions: ["sc_insectes", "sc_alimentation", "sc_animaux_petits"],
       dits: ["Les abeilles fabriquent le miel avec le nectar qu'elles récoltent dans les fleurs.",
              "Dans la ruche, les abeilles rangent le miel dans des alvéoles de cire, à six côtés."],
       petit: ["Ce sont les abeilles qui fabriquent le miel.", "Les abeilles vont de fleur en fleur."]}
    ]},
  ferme: {nom: "la ferme", a: "à la ferme", langue: "en", herbes: "albion",
    habitant: {cle: "fermiere", nom: "La fermière", dit: ["Bienvenue à la ferme ! Mes animaux adorent les visites.", "Touche-les : chacun a un secret, et une étincelle pour toi."]},
    choses: [
      {id: "vache", nom: "la vache", matiere: "sciences", mot: "cow", sens: "la vache", notions: ["sc_ferme", "sc_alimentation", "sc_vivant"],
       dits: ["La vache mange de l'herbe et donne du lait. Son petit s'appelle le veau.",
              "La vache rumine : elle fait remonter l'herbe avalée pour la mâcher une deuxième fois."],
       petit: ["La vache fait meuh ! Elle donne du lait.", "Le petit de la vache, c'est le veau."]},
      {id: "poule", nom: "la poule", matiere: "sciences", mot: "hen", sens: "la poule", notions: ["sc_ferme", "sc_animaux_petits", "sc_vivant"],
       dits: ["La poule pond des œufs. Quand elle les couve, il en sort des poussins au bout de trois semaines.",
              "La poule est un oiseau : elle a des plumes, un bec et deux pattes, mais elle vole très mal."],
       petit: ["La poule pond des œufs. Cot cot !", "Le petit de la poule, c'est le poussin."]},
      {id: "mouton", nom: "le mouton", matiere: "sciences", mot: "sheep", sens: "le mouton", notions: ["sc_ferme", "sc_saisons", "sc_matiere"],
       dits: ["Au printemps, on tond le mouton : avec sa laine, on fait du fil, puis des pulls bien chauds.",
              "La maman du mouton s'appelle la brebis, et son petit l'agneau."],
       petit: ["Le mouton fait bêê ! Sa laine tient chaud.", "Le petit du mouton, c'est l'agneau."]},
      {id: "cochon", nom: "le cochon", matiere: "sciences", mot: "pig", sens: "le cochon", notions: ["sc_ferme", "sc_sens", "sc_animaux_petits"],
       dits: ["Le cochon se roule dans la boue pour se rafraîchir : sa peau ne transpire presque pas.",
              "Le cochon a un très bon odorat : avec son groin, il trouve sa nourriture jusque sous la terre."],
       petit: ["Le cochon fait groin groin ! Il aime la boue.", "Avec son nez, le cochon sent très bien."]}
    ]},
  foret: {nom: "la forêt", a: "dans la forêt", langue: "lb", herbes: "observatoire",
    habitant: {cle: "forestier", nom: "Le garde forestier", dit: ["Bienvenue dans la clairière ! Marche doucement : la forêt est pleine de petites bêtes.", "Touche ce que tu vois : chaque chose a un secret, et une étincelle pour toi."]},
    choses: [
      {id: "fourmiliere", nom: "la fourmilière", matiere: "sciences", notions: ["sc_insectes", "sc_animaux_petits", "sc_vivant"],
       dits: ["Une fourmi a six pattes, comme tous les insectes, et deux antennes pour sentir et toucher.",
              "Les fourmis vivent ensemble par milliers. Une fourmi peut porter bien plus lourd qu'elle !"],
       petit: ["La fourmi a six pattes.", "Les fourmis vivent toutes ensemble dans leur maison."]},
      {id: "papillon", nom: "le papillon", matiere: "sciences", mot: "de Päiperlek", sens: "le papillon", notions: ["sc_insectes", "sc_vivant", "sc_saisons"],
       dits: ["Avant d'être un papillon, c'était une chenille. Elle s'est enfermée dans une chrysalide, puis elle en est sortie avec des ailes.",
              "Le papillon boit le nectar des fleurs avec sa trompe, qu'il déroule comme une paille."],
       petit: ["Avant, le papillon était une chenille !", "Le papillon boit dans les fleurs."]},
      {id: "chouette", nom: "la chouette", matiere: "sciences", mot: "d'Eil", sens: "la chouette", notions: ["sc_animaux_etonnants", "sc_jour_nuit", "sc_milieux"],
       dits: ["La chouette chasse la nuit : elle voit très bien dans le noir et vole sans faire de bruit.",
              "La chouette peut tourner la tête très loin sur le côté, presque jusque dans son dos."],
       petit: ["La chouette dort le jour et sort la nuit.", "La chouette voit très bien dans le noir."]},
      {id: "souche", nom: "la souche", matiere: "sciences", notions: ["sc_plantes", "sc_saisons", "sc_milieux"],
       dits: ["Sur une souche, on voit des cercles : l'arbre en fait un chaque année. En les comptant, on connaît son âge.",
              "Les champignons ne sont pas des plantes : ils n'ont ni feuilles ni fleurs, et ils aiment l'ombre humide du sous-bois."],
       petit: ["Sur la souche, il y a des ronds : un pour chaque année de l'arbre.", "Les champignons aiment l'ombre de la forêt."]}
    ]},
  plage: {nom: "la plage", a: "à la plage", langue: "en", herbes: "jardin",
    habitant: {cle: "pecheur", nom: "Le pêcheur", dit: ["Bienvenue à la plage ! Écoute les vagues, et regarde d'où vient le vent.", "Touche ce que la mer a laissé : chaque chose a un secret, et une étincelle pour toi."]},
    choses: [
      {id: "coquillage", nom: "le coquillage", matiere: "sciences", notions: ["sc_ocean", "sc_animaux_etonnants", "sc_milieux"],
       dits: ["Un coquillage est la maison d'un animal au corps mou. Quand la coquille est vide sur le sable, l'animal n'y vit plus.",
              "La mer monte et descend deux fois par jour : ce sont les marées. À marée basse, elle laisse des coquillages sur la plage."],
       petit: ["Le coquillage, c'est la maison d'un petit animal de la mer.", "La mer monte, puis elle redescend."]},
      {id: "tortue", nom: "la tortue de mer", matiere: "sciences", mot: "turtle", sens: "la tortue", notions: ["sc_ocean", "sc_animaux_etonnants", "sc_vivant"],
       dits: ["La tortue de mer nage dans l'océan, mais elle respire de l'air : elle remonte à la surface.",
              "La tortue de mer pond ses œufs dans le sable de la plage. À la naissance, les petits courent vers la mer."],
       petit: ["La tortue de mer nage très loin dans l'océan.", "La tortue pond ses œufs dans le sable."]},
      {id: "manche", nom: "la manche à air", matiere: "sciences", mot: "wind", sens: "le vent", notions: ["sc_meteo", "sc_saisons", "sc_matiere"],
       dits: ["La manche à air montre d'où vient le vent. Plus elle est gonflée et droite, plus le vent souffle fort.",
              "Le vent, c'est de l'air qui se déplace. On ne le voit pas, mais on voit ce qu'il fait bouger."],
       petit: ["La manche à air montre le vent.", "Le vent, c'est de l'air qui bouge."]},
      {id: "seau", nom: "le seau d'eau de mer", matiere: "sciences", mot: "water", sens: "l'eau", notions: ["sc_eau", "sc_ocean", "sc_matiere"],
       dits: ["L'eau de la mer est salée. Si on la laisse sécher au soleil, l'eau s'en va et le sel reste.",
              "L'eau des océans s'évapore au soleil, forme les nuages, puis retombe en pluie : c'est le cycle de l'eau."],
       petit: ["L'eau de la mer est salée !", "Au soleil, l'eau s'en va dans l'air : elle fera les nuages."]}
    ]}
};
const lieuQuartier = id => "quartier_" + id;
const quartierDuLieu = lieu => typeof lieu === "string" && lieu.startsWith("quartier_") && QUARTIERS[lieu.slice(9)] ? lieu.slice(9) : null;
// a quarter's things as the map and the thing's screen want them (js/jeu/carte_quartier.js, js/maison_chose.js):
// region is the quarter (the key of what the game keeps, « ferme:vache »), the word's language the quarter's
const chosesQuartier = id => QUARTIERS[id].choses.map(c => ({langue: QUARTIERS[id].langue, ...c, region: id, quartier: id, dossier: "quartiers", habitant: QUARTIERS[id].habitant}));

// from the village to a quarter and back (js/jeu/sorties_village.js, js/jeu/quartier_village.js): the place changes
// and the scene starts again there, as through a house's door (js/jeu/lanceur.js, franchirPorte). The hero comes back
// in the village on the path, a few steps from the edge he left by
function allerQuartier(id, retour){
  const p = P(); if (!p || !QUARTIERS[id]) return;
  if (retour) garderPosition(p, "village", retour);
  if (p.positions) delete p.positions[lieuQuartier(id)];   // in by its path, every time
  const q = quartiersDe(p);
  q.visites[id] = (q.visites[id] || 0) + 1;
  if (typeof noterJournal === "function") noterJournal("quartier", {q: id, n: q.visites[id]});
  franchirPorte(lieuQuartier(id));
}
function sortirQuartier(){ franchirPorte("village"); }
// what the game keeps of the quarters: visites, id -> the times the child went there
function quartiersDe(p = P()){
  const objet = v => !!v && typeof v === "object" && !Array.isArray(v);
  if (!objet(p.quartiers)) p.quartiers = {};
  if (!objet(p.quartiers.visites)) p.quartiers.visites = {};
  return p.quartiers;
}
// a damaged game: only what is well formed is kept (js/etat.js, reparer)
function reparerQuartiers(lu){
  const q = {visites: {}}, objet = v => !!v && typeof v === "object" && !Array.isArray(v);
  if (objet(lu) && objet(lu.visites)) Object.keys(QUARTIERS).forEach(id => { if (Number(lu.visites[id]) > 0) q.visites[id] = Math.floor(Number(lu.visites[id])); });
  return q;
}
