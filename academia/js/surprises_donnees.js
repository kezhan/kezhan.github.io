/* Académia : ce que l'enfant trouve au village et dans ses quartiers (ticket surprises, lot 1 ; règles : js/surprises.js).
   TROUVAILLES[lieu] : les trois trésors qui brillent dans l'herbe d'un lieu (le village et ses cinq quartiers,
   js/quartiers.js), un par jour. Chacun : id (son dessin : assets/<qualité>/surprises/trouvaille_<id>.png), nom (« la
   plume »), mot (une clé de MOTS_DU_JEU, js/mots_du_jeu.js : le mot étranger dit par son enregistrement, dans la langue
   du jour quand elle l'a) et sens quand ce mot n'est pas le nom de la chose (l'épi de blé dit « le pain »), matiere et
   notions (l'exercice proposé, comme pour une chose d'une maison : js/maison_chose.js), dits et petit (des choses
   vraies, dites tour à tour ; petit : pour cinq ans et moins).
   VISITEURS : les animaux de passage, un jour sur deux, chacun son tour ; un (« un renard »), soir : il ne vient qu'à
   partir de cinq heures du soir (la chouette dort le jour).
   Dessins : outils/village/scripts/surprises.py (à nous). */
const TROUVAILLES = {
  village: [
    {id: "plume", nom: "la plume", mot: "oiseau", matiere: "sciences", notions: ["sc_animaux_petits", "sc_vivant", "sc_animaux_etonnants"],
     dits: ["Les plumes tiennent l'oiseau au chaud et l'aident à voler. Elles sont très légères.",
            "Un oiseau perd ses vieilles plumes, et de nouvelles repoussent : c'est la mue."],
     petit: ["C'est une plume d'oiseau ! Elle est toute légère.", "Les plumes tiennent l'oiseau au chaud."]},
    {id: "trefle", nom: "le trèfle à quatre feuilles", matiere: "maths", notions: ["denombrer_5", "denombrer_10", "doubles_moities", "add_10"],
     dits: ["Un trèfle a presque toujours trois feuilles. En trouver un à quatre feuilles est très rare : on dit qu'il porte bonheur.",
            "Deux feuilles et encore deux feuilles : quatre, c'est le double de deux."],
     petit: ["Compte ses feuilles : une, deux, trois, quatre !", "D'habitude, le trèfle a trois feuilles."]},
    {id: "piece", nom: "la pièce ancienne", matiere: "maths", notions: ["add_10", "comparer_20", "add_20", "problemes_add_sous"],
     dits: ["Les premières pièces de monnaie ont été fabriquées il y a plus de deux mille cinq cents ans.",
            "Avant la monnaie, les gens échangeaient une chose contre une autre : c'est le troc."],
     petit: ["C'est une très vieille pièce ! Avec les pièces, on achète des choses.", "Cette pièce est très, très vieille."]}
  ],
  ferme: [
    {id: "oeuf", nom: "l'œuf", mot: "oeuf", matiere: "sciences", notions: ["sc_ferme", "sc_animaux_petits", "sc_vivant"],
     dits: ["La coquille de l'œuf est dure, mais elle laisse passer l'air : le poussin respire à travers.",
            "Une poule peut pondre presque un œuf par jour."],
     petit: ["C'est la poule qui a pondu cet œuf.", "Dans l'œuf que la poule couve grandit un poussin."]},
    {id: "fer", nom: "le fer à cheval", mot: "cheval", sens: "le cheval", matiere: "sciences", notions: ["sc_ferme", "sc_matiere", "sc_corps"],
     dits: ["Le fer protège le sabot du cheval, comme une chaussure. Le sabot est fait de corne, comme nos ongles.",
            "C'est le maréchal-ferrant qui pose les fers. Cela ne fait pas mal au cheval."],
     petit: ["Le fer à cheval, c'est la chaussure du cheval.", "Le cheval a des sabots au bout des pattes."]},
    {id: "ble", nom: "l'épi de blé", mot: "pain", sens: "le pain", matiere: "sciences", notions: ["sc_alimentation", "sc_plantes", "sc_ferme"],
     dits: ["Les grains de blé sont écrasés pour faire la farine. Avec la farine, de l'eau et de la levure, on fait le pain.",
            "Le blé est semé, il pousse, puis on le moissonne en été, quand il est doré."],
     petit: ["Avec le blé, on fait la farine, puis le pain.", "Le blé devient tout doré en été."]}
  ],
  marche: [
    {id: "fraise", nom: "la fraise", mot: "fraise", matiere: "sciences", notions: ["sc_alimentation", "sc_plantes", "sc_saisons"],
     dits: ["Les petits grains sur la fraise sont ses vrais fruits : chacun contient une graine.",
            "Les fraises mûrissent au printemps et au début de l'été."],
     petit: ["La fraise est rouge et sucrée.", "Les petits points de la fraise sont ses graines."]},
    {id: "citrouille", nom: "la citrouille", mot: "citrouille", matiere: "sciences", notions: ["sc_plantes", "sc_saisons", "sc_alimentation"],
     dits: ["La citrouille pousse au ras du sol, sur une longue tige qui rampe. On la récolte en automne.",
            "La citrouille est un fruit : à l'intérieur, elle est pleine de graines."],
     petit: ["La citrouille est grosse et orange.", "On récolte les citrouilles en automne."]},
    {id: "patate", nom: "la pomme de terre", mot: "pomme_de_terre", matiere: "sciences", notions: ["sc_plantes", "sc_alimentation", "sc_vivant"],
     dits: ["La pomme de terre pousse sous la terre. Ce n'est pas une racine, mais une tige renflée : un tubercule.",
            "Une pomme de terre oubliée fait des germes : plantée, elle donne une nouvelle plante."],
     petit: ["La pomme de terre pousse sous la terre.", "Avec les pommes de terre, on fait la purée et les frites."]}
  ],
  ecole: [
    {id: "regle", nom: "la règle", mot: "regle", matiere: "maths", notions: ["denombrer_10", "comparer_20", "add_20", "comparer_100"],
     dits: ["Avec une règle, on trace des traits droits et on mesure. Ses petits traits marquent les centimètres.",
            "Dix millimètres font un centimètre, et cent centimètres font un mètre."],
     petit: ["La règle sert à tracer des traits bien droits.", "Sur la règle, il y a des nombres."]},
    {id: "ciseaux", nom: "les ciseaux", mot: "ciseaux", matiere: "logique", notions: ["lo_formes", "lo_formes_cotes", "lo_couleurs"],
     dits: ["En découpant un carré d'un coin à l'autre, on obtient deux triangles.",
            "Les ciseaux ont deux lames qui se croisent : quand on serre les anneaux, elles se ferment et coupent."],
     petit: ["Les ciseaux servent à découper le papier.", "On tient les ciseaux par les deux anneaux."]},
    {id: "livre", nom: "le livre", mot: "livre", matiere: "logique", notions: ["lo_ordre", "lo_suites", "lo_rang", "lo_suites_nombres"],
     dits: ["Les pages d'un livre sont numérotées dans l'ordre : après la page neuf vient la page dix.",
            "Avant l'imprimerie, les livres étaient copiés à la main, page après page."],
     petit: ["Dans un livre, il y a des histoires et des images.", "On tourne les pages une par une."]}
  ],
  foret: [
    {id: "gland", nom: "le gland", matiere: "sciences", notions: ["sc_plantes", "sc_saisons", "sc_milieux"],
     dits: ["Le gland est le fruit du chêne. Planté dans la terre, il peut devenir un grand arbre.",
            "Les écureuils et les geais cachent des glands pour l'hiver. Ceux qu'ils oublient peuvent germer."],
     petit: ["Le gland tombe du chêne.", "D'un gland peut pousser un grand arbre."]},
    {id: "pomme_de_pin", nom: "la pomme de pin", matiere: "sciences", notions: ["sc_plantes", "sc_meteo", "sc_saisons"],
     dits: ["La pomme de pin protège les graines du pin sous ses écailles.",
            "Par temps sec, les écailles de la pomme de pin s'ouvrent. Quand il fait humide, elles se referment."],
     petit: ["La pomme de pin cache les graines du pin.", "La pomme de pin s'ouvre quand il fait sec."]},
    {id: "coquille", nom: "la coquille d'escargot", mot: "escargot", sens: "l'escargot", matiere: "sciences", notions: ["sc_animaux_petits", "sc_vivant", "sc_animaux_etonnants"],
     dits: ["La coquille de l'escargot grandit avec lui, en spirale. Il s'y cache quand il fait trop sec.",
            "L'escargot avance sur un seul pied, en glissant sur sa bave."],
     petit: ["La coquille de l'escargot tourne en rond.", "L'escargot porte sa maison sur son dos."]}
  ],
  plage: [
    {id: "etoile_de_mer", nom: "l'étoile de mer", matiere: "sciences", notions: ["sc_ocean", "sc_animaux_etonnants", "sc_vivant"],
     dits: ["L'étoile de mer a le plus souvent cinq bras. Si elle en perd un, il peut repousser.",
            "L'étoile de mer n'a pas de cerveau, et sa bouche est sous son corps."],
     petit: ["Compte ses bras : il y en a cinq !", "L'étoile de mer vit au fond de l'eau."]},
    {id: "galet", nom: "le galet", matiere: "sciences", notions: ["sc_matiere", "sc_ocean", "sc_eau"],
     dits: ["Un galet est une pierre que la mer a roulée très longtemps : les vagues ont usé ses angles.",
            "Le sable est fait de tout petits morceaux de roches et de coquillages."],
     petit: ["Le galet est tout lisse et tout rond.", "C'est la mer qui a poli le galet."]},
    {id: "bois_flotte", nom: "le bois flotté", mot: "eau", sens: "l'eau", matiere: "sciences", notions: ["sc_matiere", "sc_ocean", "sc_eau"],
     dits: ["Le bois flotte sur l'eau. Une pierre de la même taille est plus lourde : elle coule.",
            "Ce bois a voyagé sur la mer. Le sel et le soleil l'ont blanchi."],
     petit: ["La mer a apporté ce bout de bois.", "Le bois flotte sur l'eau."]}
  ]
};
const VISITEURS = [
  {id: "renard", nom: "le renard", un: "un renard", mot: "renard", matiere: "sciences", notions: ["sc_animaux_petits", "sc_milieux", "sc_sens"],
   dits: ["Le renard entend très bien : il repère une souris qui bouge sous la neige.",
          "Le renard vit dans un terrier. Ses petits s'appellent les renardeaux.",
          "Le renard mange de tout : des souris, des insectes, des fruits."],
   petit: ["Le renard a une belle queue touffue.", "Les petits du renard s'appellent les renardeaux."]},
  {id: "grenouille", nom: "la grenouille", un: "une grenouille", mot: "grenouille", matiere: "sciences", notions: ["sc_animaux_petits", "sc_vivant", "sc_eau"],
   dits: ["La grenouille naît dans l'eau : c'est d'abord un têtard, avec une queue et sans pattes.",
          "La grenouille attrape les insectes avec sa langue collante.",
          "La grenouille respire aussi par sa peau, qui doit rester humide."],
   petit: ["Avant, la grenouille était un têtard !", "La grenouille saute et fait coâ, coâ."]},
  {id: "escargot", nom: "l'escargot", un: "un escargot", mot: "escargot", matiere: "sciences", notions: ["sc_animaux_petits", "sc_vivant", "sc_meteo"],
   dits: ["Les yeux de l'escargot sont au bout de ses deux grandes cornes.",
          "L'escargot sort quand il pleut : il a besoin d'humidité pour avancer.",
          "L'escargot avance sur un seul pied, en glissant sur sa bave."],
   petit: ["L'escargot porte sa maison sur son dos.", "L'escargot sort quand il pleut."]},
  {id: "souris", nom: "la souris", un: "une souris", mot: "souris", matiere: "sciences", notions: ["sc_animaux_petits", "sc_sens", "sc_vivant"],
   dits: ["Les dents de devant de la souris poussent toute sa vie : elle les use en rongeant.",
          "Avec ses moustaches, la souris sent ce qui l'entoure, même dans le noir.",
          "La souris est un mammifère : la maman allaite ses petits."],
   petit: ["La souris est toute petite, avec une longue queue.", "La souris grignote des graines."]},
  {id: "chouette", nom: "la chouette", un: "une chouette", mot: "hibou", sens: "la chouette", soir: true, matiere: "sciences", notions: ["sc_jour_nuit", "sc_animaux_etonnants", "sc_sens"],
   dits: ["La chouette chasse la nuit : elle voit très bien dans le noir et vole sans faire de bruit.",
          "La chouette peut tourner la tête très loin sur le côté, presque jusque dans son dos.",
          "La chouette dort le jour, cachée dans un arbre creux ou un vieux mur."],
   petit: ["La chouette dort le jour et sort le soir.", "La chouette voit très bien dans le noir."]}
];
