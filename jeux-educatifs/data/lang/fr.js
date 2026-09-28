/*
 * L'Île aux mots — pack de langue : FRANÇAIS (modèle de référence pour les autres langues).
 *
 * Un pack contient TOUT ce qui dépend de la langue : textes de l'interface, noms des jeux,
 * mots illustrés et contenus pédagogiques. Pour ajouter une langue : copier ce fichier,
 * garder exactement les mêmes clés, traduire/adapter, puis l'ajouter dans les pages HTML.
 *
 * MOTS : { mot, emoji, genre ('m' | 'f' | null), syl (syllabes, syl.join('') === mot),
 *          niveau (1 = 5–6 ans, 2 = 6–7 ans, 3 = 8–10 ans), theme,
 *          art (article indéfini), def (article défini, élidé si besoin : « l’ ») }
 * Pour écrire « article + mot », utiliser Ile.groupe(article, mot) (pas d'espace après « l’ »).
 *
 * PHRASES : { texte, niveau, mots?, variantes? } : la ponctuation finale reste collée au dernier mot ;
 *   variantes (facultatif) = autres ordres justes écrits avec exactement les mêmes mots, en texte
 *   ou en liste de mots ('After colouring, please put back the pencils in the box.'), acceptés
 *   aussi par le jeu « phrase ». Mieux vaut choisir des phrases qui n'ont qu'un seul ordre juste.
 *
 * Champs propres à certaines langues :
 *  - ecriture 'hanzi' (chinois) : chaque mot a aussi pinyin ('píng guǒ', une syllabe par caractère),
 *    epeler (pinyin sans tons, 'pingguo' : utilisé par les jeux de lettres), syl = un caractère par
 *    élément, et un champ de classificateur (ex. cl: '个') ; sepMots = '' et chaque phrase de
 *    PHRASES donne ses mots : { texte, niveau, mots: ['我', '喜欢', '猫', '。'] }.
 *  - articles[n] = { champ, choix, affiche?, filtrer? } : champ = propriété du mot qui donne la
 *    bonne réponse du jeu « genre » ; affiche = propriété à écrire devant le mot après la réponse
 *    (par défaut champ) ; filtrer = n'utiliser que les mots dont champ ∈ choix.
 *  - jeux[id] = null : ce jeu n'existe pas dans la langue (ex. pluriel en chinois).
 *  - htmlLang (ex. 'zh-Hans'), ttsRate (vitesse de lecture) : facultatifs.
 *  - Allemand et luxembourgeois : les noms s'écrivent avec une majuscule (mot: 'Katze').
 * Un mot de niveau N est aussi disponible aux niveaux supérieurs.
 */
(function () {
  const M = (mot, emoji, genre, syl, niveau, theme, extra) =>
    Object.assign({ mot, emoji, genre, syl, niveau, theme }, extra || {});

  const MOTS = [
    // --- Animaux ---
    M('chat', '🐱', 'm', ['chat'], 1, 'animaux'),
    M('chien', '🐶', 'm', ['chien'], 1, 'animaux'),
    M('lapin', '🐰', 'm', ['la', 'pin'], 1, 'animaux'),
    M('poule', '🐔', 'f', ['pou', 'le'], 1, 'animaux'),
    M('vache', '🐮', 'f', ['va', 'che'], 1, 'animaux'),
    M('loup', '🐺', 'm', ['loup'], 1, 'animaux'),
    M('ours', '🐻', 'm', ['ours'], 1, 'animaux'),
    M('lion', '🦁', 'm', ['li', 'on'], 1, 'animaux'),
    M('crabe', '🦀', 'm', ['cra', 'be'], 1, 'animaux'),
    M('hibou', '🦉', 'm', ['hi', 'bou'], 1, 'animaux', { pluriel: 'hiboux' }),
    M('cochon', '🐷', 'm', ['co', 'chon'], 2, 'animaux'),
    M('mouton', '🐑', 'm', ['mou', 'ton'], 2, 'animaux'),
    M('cheval', '🐴', 'm', ['che', 'val'], 2, 'animaux', { pluriel: 'chevaux' }),
    M('souris', '🐭', 'f', ['sou', 'ris'], 2, 'animaux', { pluriel: 'souris' }),
    M('poisson', '🐟', 'm', ['pois', 'son'], 2, 'animaux'),
    M('tortue', '🐢', 'f', ['tor', 'tue'], 2, 'animaux'),
    M('canard', '🦆', 'm', ['ca', 'nard'], 2, 'animaux'),
    M('singe', '🐒', 'm', ['sin', 'ge'], 2, 'animaux'),
    M('girafe', '🦒', 'f', ['gi', 'ra', 'fe'], 2, 'animaux'),
    M('dauphin', '🐬', 'm', ['dau', 'phin'], 2, 'animaux'),
    M('requin', '🦈', 'm', ['re', 'quin'], 2, 'animaux'),
    M('serpent', '🐍', 'm', ['ser', 'pent'], 2, 'animaux'),
    M('baleine', '🐳', 'f', ['ba', 'lei', 'ne'], 2, 'animaux'),
    M('abeille', '🐝', 'f', ['a', 'beil', 'le'], 3, 'animaux'),
    M('papillon', '🦋', 'm', ['pa', 'pil', 'lon'], 3, 'animaux'),
    M('escargot', '🐌', 'm', ['es', 'car', 'got'], 3, 'animaux'),
    M('éléphant', '🐘', 'm', ['é', 'lé', 'phant'], 3, 'animaux'),
    M('crocodile', '🐊', 'm', ['cro', 'co', 'di', 'le'], 3, 'animaux'),
    M('pieuvre', '🐙', 'f', ['pieu', 'vre'], 3, 'animaux'),
    M('grenouille', '🐸', 'f', ['gre', 'nouil', 'le'], 3, 'animaux'),
    M('perroquet', '🦜', 'm', ['per', 'ro', 'quet'], 3, 'animaux'),
    M('kangourou', '🦘', 'm', ['kan', 'gou', 'rou'], 3, 'animaux'),

    // --- Nourriture ---
    M('pomme', '🍎', 'f', ['pom', 'me'], 1, 'nourriture'),
    M('poire', '🍐', 'f', ['poi', 're'], 1, 'nourriture'),
    M('pain', '🍞', 'm', ['pain'], 1, 'nourriture'),
    M('lait', '🥛', 'm', ['lait'], 1, 'nourriture'),
    M('glace', '🍦', 'f', ['gla', 'ce'], 1, 'nourriture'),
    M('pizza', '🍕', 'f', ['piz', 'za'], 1, 'nourriture'),
    M('banane', '🍌', 'f', ['ba', 'na', 'ne'], 1, 'nourriture'),
    M('tomate', '🍅', 'f', ['to', 'ma', 'te'], 1, 'nourriture'),
    M('fraise', '🍓', 'f', ['frai', 'se'], 2, 'nourriture'),
    M('cerise', '🍒', 'f', ['ce', 'ri', 'se'], 2, 'nourriture'),
    M('citron', '🍋', 'm', ['ci', 'tron'], 2, 'nourriture'),
    M('orange', '🍊', 'f', ['o', 'ran', 'ge'], 2, 'nourriture'),
    M('raisin', '🍇', 'm', ['rai', 'sin'], 2, 'nourriture'),
    M('carotte', '🥕', 'f', ['ca', 'rot', 'te'], 2, 'nourriture'),
    M('gâteau', '🎂', 'm', ['gâ', 'teau'], 2, 'nourriture', { pluriel: 'gâteaux' }),
    M('bonbon', '🍬', 'm', ['bon', 'bon'], 2, 'nourriture'),
    M('fromage', '🧀', 'm', ['fro', 'ma', 'ge'], 2, 'nourriture'),
    M('ananas', '🍍', 'm', ['a', 'na', 'nas'], 3, 'nourriture', { pluriel: 'ananas' }),
    M('pastèque', '🍉', 'f', ['pas', 'tè', 'que'], 3, 'nourriture'),
    M('champignon', '🍄', 'm', ['cham', 'pi', 'gnon'], 3, 'nourriture'),
    M('croissant', '🥐', 'm', ['crois', 'sant'], 3, 'nourriture'),

    // --- Nature ---
    M('lune', '🌙', 'f', ['lu', 'ne'], 1, 'nature'),
    M('mer', '🌊', 'f', ['mer'], 1, 'nature'),
    M('feu', '🔥', 'm', ['feu'], 1, 'nature', { pluriel: 'feux' }),
    M('fleur', '🌸', 'f', ['fleur'], 1, 'nature'),
    M('île', '🏝️', 'f', ['î', 'le'], 1, 'nature'),
    M('soleil', '☀️', 'm', ['so', 'leil'], 2, 'nature'),
    M('nuage', '☁️', 'm', ['nua', 'ge'], 2, 'nature'),
    M('pluie', '🌧️', 'f', ['pluie'], 2, 'nature'),
    M('arbre', '🌳', 'm', ['ar', 'bre'], 2, 'nature'),
    M('neige', '❄️', 'f', ['nei', 'ge'], 2, 'nature'),
    M('étoile', '⭐', 'f', ['é', 'toi', 'le'], 2, 'nature'),
    M('feuille', '🍃', 'f', ['feuil', 'le'], 3, 'nature'),
    M('volcan', '🌋', 'm', ['vol', 'can'], 3, 'nature'),
    M('montagne', '⛰️', 'f', ['mon', 'ta', 'gne'], 3, 'nature'),
    M('coquillage', '🐚', 'm', ['co', 'quil', 'la', 'ge'], 3, 'nature'),
    M('palmier', '🌴', 'm', ['pal', 'mier'], 3, 'nature'),

    // --- Objets ---
    M('vélo', '🚲', 'm', ['vé', 'lo'], 1, 'objets'),
    M('robot', '🤖', 'm', ['ro', 'bot'], 1, 'objets'),
    M('livre', '📖', 'm', ['li', 'vre'], 1, 'objets'),
    M('clé', '🔑', 'f', ['clé'], 1, 'objets'),
    M('lampe', '💡', 'f', ['lam', 'pe'], 1, 'objets'),
    M('bateau', '⛵', 'm', ['ba', 'teau'], 1, 'objets', { pluriel: 'bateaux' }),
    M('maison', '🏠', 'f', ['mai', 'son'], 2, 'objets'),
    M('voiture', '🚗', 'f', ['voi', 'tu', 're'], 2, 'objets'),
    M('avion', '✈️', 'm', ['a', 'vion'], 2, 'objets'),
    M('train', '🚂', 'm', ['train'], 2, 'objets'),
    M('fusée', '🚀', 'f', ['fu', 'sée'], 2, 'objets'),
    M('crayon', '✏️', 'm', ['cra', 'yon'], 2, 'objets'),
    M('ballon', '⚽', 'm', ['bal', 'lon'], 2, 'objets'),
    M('chapeau', '🎩', 'm', ['cha', 'peau'], 2, 'objets', { pluriel: 'chapeaux' }),
    M('cadeau', '🎁', 'm', ['ca', 'deau'], 2, 'objets', { pluriel: 'cadeaux' }),
    M('guitare', '🎸', 'f', ['gui', 'ta', 're'], 2, 'objets'),
    M('ancre', '⚓', 'f', ['an', 'cre'], 2, 'objets'),
    M('tambour', '🥁', 'm', ['tam', 'bour'], 3, 'objets'),
    M('parapluie', '☂️', 'm', ['pa', 'ra', 'pluie'], 3, 'objets'),
    M('horloge', '🕰️', 'f', ['hor', 'lo', 'ge'], 3, 'objets'),
    M('téléphone', '📱', 'm', ['té', 'lé', 'pho', 'ne'], 3, 'objets'),
    M('couronne', '👑', 'f', ['cou', 'ron', 'ne'], 3, 'objets'),
    M('trésor', '💰', 'm', ['tré', 'sor'], 3, 'objets'),
    M('chaussure', '👟', 'f', ['chaus', 'su', 're'], 3, 'objets'),
    M('ordinateur', '💻', 'm', ['or', 'di', 'na', 'teur'], 3, 'objets'),

    // --- Corps ---
    M('main', '✋', 'f', ['main'], 1, 'corps'),
    M('pied', '🦶', 'm', ['pied'], 1, 'corps'),
    M('nez', '👃', 'm', ['nez'], 1, 'corps', { pluriel: 'nez' }),
    M('dent', '🦷', 'f', ['dent'], 1, 'corps'),
    M('bouche', '👄', 'f', ['bou', 'che'], 2, 'corps'),
    M('oreille', '👂', 'f', ['o', 'reil', 'le'], 3, 'corps'),
  ];

  // Familles de rimes (son final commun). Les mots sans emoji s'affichent en texte.
  const RIMES = [
    { son: 'on', mots: ['bonbon', 'cochon', 'mouton', 'ballon', 'citron', 'papillon', 'crayon', 'champignon', 'maison', 'poisson'] },
    { son: 'eau', mots: ['bateau', 'gâteau', 'chapeau', 'cadeau', 'château', 'oiseau', 'râteau'] },
    { son: 'in', mots: ['lapin', 'requin', 'dauphin', 'raisin', 'sapin', 'jardin', 'pain', 'main', 'train'] },
    { son: 'ou', mots: ['loup', 'hibou', 'kangourou', 'genou', 'chou', 'bisou', 'caillou'] },
    { son: 'eille', mots: ['abeille', 'oreille', 'bouteille', 'corbeille', 'groseille'] },
    { son: 'age', mots: ['fromage', 'nuage', 'coquillage', 'plage', 'cage', 'page'] },
    { son: 'ise', mots: ['cerise', 'valise', 'chemise', 'église', 'bise'] },
    { son: 'ane', mots: ['banane', 'cabane', 'cane', 'panne', 'canne'] },
    { son: 'otte', mots: ['carotte', 'botte', 'marmotte', 'culotte', 'hotte'] },
    { son: 'eur', mots: ['fleur', 'ordinateur', 'cœur', 'sœur', 'beurre', 'peur'] },
    { son: 'ire', mots: ['navire', 'sourire', 'tirelire', 'vampire', 'lire'] },
    { son: 'é', mots: ['clé', 'fusée', 'bébé', 'café', 'été', 'épée', 'poupée'] },
  ];

  // Paires de contraires : [a, b, niveau, nature ('adj' | 'verbe' | 'nom' | 'adv')].
  const CONTRAIRES = [
    ['grand', 'petit', 1, 'adj'], ['chaud', 'froid', 1, 'adj'], ['jour', 'nuit', 1, 'nom'], ['haut', 'bas', 1, 'adj'],
    ['plein', 'vide', 1, 'adj'], ['content', 'triste', 1, 'adj'], ['propre', 'sale', 1, 'adj'], ['long', 'court', 1, 'adj'],
    ['ouvert', 'fermé', 1, 'adj'], ['monter', 'descendre', 1, 'verbe'], ['rire', 'pleurer', 1, 'verbe'], ['entrer', 'sortir', 1, 'verbe'],
    ['rapide', 'lent', 2, 'adj'], ['lourd', 'léger', 2, 'adj'], ['dur', 'mou', 2, 'adj'], ['jeune', 'vieux', 2, 'adj'],
    ['dedans', 'dehors', 2, 'adv'], ['avant', 'après', 2, 'adv'], ['gagner', 'perdre', 2, 'verbe'], ['fort', 'faible', 2, 'adj'],
    ['mouillé', 'sec', 2, 'adj'], ['allumer', 'éteindre', 2, 'verbe'], ['facile', 'difficile', 2, 'adj'], ['toujours', 'jamais', 2, 'adv'],
    ['clair', 'sombre', 3, 'adj'], ['courageux', 'peureux', 3, 'adj'], ['généreux', 'avare', 3, 'adj'], ['silencieux', 'bruyant', 3, 'adj'],
    ['ancien', 'moderne', 3, 'adj'], ['rare', 'fréquent', 3, 'adj'], ['accepter', 'refuser', 3, 'verbe'], ['construire', 'détruire', 3, 'verbe'],
    ['calme', 'agité', 3, 'adj'], ['poli', 'impoli', 3, 'adj'], ['possible', 'impossible', 3, 'adj'], ['honnête', 'malhonnête', 3, 'adj'],
  ].map(([a, b, niveau, nature]) => ({ a, b, niveau, nature }));

  // Phrases à remettre dans l'ordre (la ponctuation reste collée au dernier mot).
  const PHRASES = [
    ['Le chat dort.', 1], ['Papa lit un livre.', 1], ['La poule mange du pain.', 1],
    ['Le bateau est sur la mer.', 1], ['Je mange une pomme.', 1], ['Le soleil brille.', 1],
    ['Le lapin saute dans le pré.', 1], ['Tom joue au ballon.', 1], ['La lune est ronde.', 1],
    ['Le petit chien court vite.', 2], ['Les enfants nagent dans la mer.', 2],
    ['Ma sœur dessine une maison.', 2], ['Le pirate cache son trésor.', 2],
    ['Il pleut sur la montagne.', 2], ['Le singe grimpe au palmier.', 2],
    ['Nous allons à la plage demain.', 2], ['La tortue avance très lentement.', 2],
    ['Le dauphin saute au-dessus des vagues.', 3],
    ['Hier, nous avons visité une île mystérieuse.', 3],
    ['Le perroquet répète tous les mots du capitaine.', 3],
    ['Les marins hissent la grande voile du navire.', 3],
    ['Pendant la tempête, le phare guide les bateaux.', 3],
    ['Le volcan endormi domine toute la vallée.', 3],
    ['Ma grand-mère prépare un délicieux gâteau.', 3],
  ].map(([texte, niveau]) => ({ texte, niveau }));

  // Mots supplémentaires sans image (pour les dictées, mots cachés, alphabet…).
  const MOTS_SIMPLES = {
    1: ['ami', 'papa', 'maman', 'moto', 'lit', 'sac', 'bol', 'joli', 'rue', 'nid', 'midi', 'tasse', 'domino', 'jupe', 'mardi'],
    2: ['jardin', 'forêt', 'plage', 'ville', 'école', 'cahier', 'table', 'chaise', 'porte', 'fenêtre', 'musique', 'copain', 'dimanche', 'bonjour', 'merci'],
    3: ['capitaine', 'aventure', 'boussole', 'tempête', 'pirate', 'navire', 'marin', 'bibliothèque', 'anniversaire', 'mystérieux', 'explorateur', 'lointain', 'équipage', 'longue-vue', 'horizon'],
  };

  // Pluriels : { s (singulier), p (pluriel), g (genre), regle (clé de REGLES_PLURIEL), niveau, emoji? }
  const P = (s, p, g, regle, niveau, emoji) => ({ s, p, g, regle, niveau, emoji: emoji || null });
  const PLURIELS = [
    P('chat', 'chats', 'm', 's', 1, '🐱'), P('chien', 'chiens', 'm', 's', 1, '🐶'), P('lapin', 'lapins', 'm', 's', 1, '🐰'),
    P('pomme', 'pommes', 'f', 's', 1, '🍎'), P('fleur', 'fleurs', 'f', 's', 1, '🌸'), P('livre', 'livres', 'm', 's', 1, '📖'),
    P('vélo', 'vélos', 'm', 's', 1, '🚲'), P('robot', 'robots', 'm', 's', 1, '🤖'), P('poule', 'poules', 'f', 's', 1, '🐔'),
    P('vache', 'vaches', 'f', 's', 1, '🐮'), P('lampe', 'lampes', 'f', 's', 1, '💡'), P('clé', 'clés', 'f', 's', 1, '🔑'),
    P('dent', 'dents', 'f', 's', 1, '🦷'), P('tomate', 'tomates', 'f', 's', 1, '🍅'), P('banane', 'bananes', 'f', 's', 1, '🍌'),
    P('crabe', 'crabes', 'm', 's', 1, '🦀'), P('loup', 'loups', 'm', 's', 1, '🐺'), P('poire', 'poires', 'f', 's', 1, '🍐'),
    P('bateau', 'bateaux', 'm', 'x', 2, '⛵'), P('gâteau', 'gâteaux', 'm', 'x', 2, '🎂'), P('chapeau', 'chapeaux', 'm', 'x', 2, '🎩'),
    P('cadeau', 'cadeaux', 'm', 'x', 2, '🎁'), P('feu', 'feux', 'm', 'x', 2, '🔥'), P('jeu', 'jeux', 'm', 'x', 2, '🎲'),
    P('cheval', 'chevaux', 'm', 'al', 2, '🐴'), P('journal', 'journaux', 'm', 'al', 2, '📰'), P('animal', 'animaux', 'm', 'al', 2, '🐾'),
    P('hibou', 'hiboux', 'm', 'ou_x', 2, '🦉'), P('bijou', 'bijoux', 'm', 'ou_x', 2, '💍'), P('genou', 'genoux', 'm', 'ou_x', 2, '🦵'),
    P('caillou', 'cailloux', 'm', 'ou_x', 2, '🪨'), P('trou', 'trous', 'm', 'ou_s', 2, '🕳️'), P('kangourou', 'kangourous', 'm', 'ou_s', 2, '🦘'),
    P('souris', 'souris', 'f', 'inv', 2, '🐭'), P('nez', 'nez', 'm', 'inv', 2, '👃'), P('ananas', 'ananas', 'm', 'inv', 2, '🍍'),
    P('noix', 'noix', 'f', 'inv', 2, '🌰'), P('bras', 'bras', 'm', 'inv', 2, '💪'), P('prix', 'prix', 'm', 'inv', 2, '🏷️'),
    P('travail', 'travaux', 'm', 'ail_aux', 3, '🛠️'), P('corail', 'coraux', 'm', 'ail_aux', 3, '🪸'), P('vitrail', 'vitraux', 'm', 'ail_aux', 3),
    P('éventail', 'éventails', 'm', 'ail_s', 3, '🪭'), P('rail', 'rails', 'm', 'ail_s', 3, '🛤️'), P('détail', 'détails', 'm', 'ail_s', 3),
    P('pneu', 'pneus', 'm', 'x_exception', 3, '🛞'), P('landau', 'landaus', 'm', 'x_exception', 3), P('bleu', 'bleus', 'm', 'x_exception', 3),
    P('bal', 'bals', 'm', 'al_s', 3, '💃'), P('festival', 'festivals', 'm', 'al_s', 3, '🎪'), P('carnaval', 'carnavals', 'm', 'al_s', 3, '🎭'),
    P('chacal', 'chacals', 'm', 'al_s', 3), P('chou', 'choux', 'm', 'ou_x', 3, '🥬'), P('joujou', 'joujoux', 'm', 'ou_x', 3, '🧸'),
    P('pou', 'poux', 'm', 'ou_x', 3), P('œil', 'yeux', 'm', 'irr', 3, '👁️'), P('clou', 'clous', 'm', 'ou_s', 3),
  ];
  const REGLES_PLURIEL = {
    s: 'En général, on ajoute un -s : un chat, des chats.',
    x: 'Les mots en -eau, -au et -eu prennent un -x : un bateau, des bateaux.',
    x_exception: 'Exception : pneu, bleu et landau prennent un -s : un pneu, des pneus.',
    al: 'Les mots en -al deviennent -aux : un cheval, des chevaux.',
    al_s: 'Exception : bal, carnaval, festival et chacal prennent un -s : un bal, des bals.',
    ou_x: 'Sept mots en -ou prennent un -x : bijou, caillou, chou, genou, hibou, joujou, pou.',
    ou_s: 'Les autres mots en -ou prennent un -s : un trou, des trous.',
    inv: 'Les mots qui finissent par -s, -x ou -z ne changent pas : une souris, des souris.',
    ail_aux: 'Quelques mots en -ail deviennent -aux : un travail, des travaux.',
    ail_s: 'Les autres mots en -ail prennent un -s : un rail, des rails.',
    irr: 'Pluriel irrégulier à retenir : un œil, des yeux.',
  };

  // Articles (calculés une fois pour toutes, avec élision et h aspiré).
  const H_ASPIRE = ['hibou', 'haut', 'hotte', 'hérisson', 'homard', 'hamac'];
  function defini(mot, genre) {
    const aspire = H_ASPIRE.indexOf(mot) !== -1;
    if (!aspire && /^[aeiouyéèêëàâîïôöûüœh]/i.test(mot)) return 'l’';
    return genre === 'f' ? 'la' : 'le';
  }
  MOTS.forEach((m) => {
    m.art = m.genre === 'f' ? 'une' : 'un';
    m.def = defini(m.mot, m.genre);
  });
  PLURIELS.forEach((x) => {
    x.detS = x.g === 'f' ? 'une' : 'un';
    x.detP = 'des';
  });

  window.ILE_LANGS = window.ILE_LANGS || {};
  window.ILE_LANGS.fr = {
    code: 'fr',
    nom: 'Français',
    drapeau: '🇫🇷',
    tts: 'fr-FR',
    dir: 'ltr',
    // Séparateur entre deux mots (le chinois n'en a pas) et système d'écriture.
    sepMots: ' ',
    ecriture: 'alphabet',

    niveaux: [
      { n: 1, nom: 'Moussaillon', classe: 'Débutant', emoji: '🐣' },
      { n: 2, nom: 'Matelot', classe: 'Intermédiaire', emoji: '⚓' },
      { n: 3, nom: 'Capitaine', classe: 'Avancé', emoji: '🏴‍☠️' },
    ],

    // Textes de l'interface commune (accueil, en-tête, fenêtre de résultat).
    ui: {
      titreSite: 'L’Île aux mots',
      accroche: 'Explore l’île, joue avec les mots et remplis ton coffre au trésor !',
      descriptionSite: 'Jeux éducatifs gratuits pour apprendre à lire et à écrire, de 5 à 10 ans.',
      choisisLangue: 'Langue',
      choisisGrade: 'Choisis ton grade de pirate',
      lesJeux: 'Les jeux',
      etoilesTotal: (n, t) => n + ' / ' + t + ' étoiles',
      etoilesNiveau: (n) => n + ' étoile' + (n > 1 ? 's' : '') + ' sur 3 à ce niveau',
      surprise: '🎲 Jeu surprise',
      footer: 'Jeux éducatifs libres pour les enfants de 5 à 10 ans · progression enregistrée sur cet appareil.',
      effacer: '🧹 Effacer ma progression',
      confirmerEffacer: 'Effacer toutes les étoiles gagnées dans cette langue sur cet appareil ?',
      retourIle: 'Retour à l’île',
      niveau: 'Niveau',
      etoilesJeu: 'Étoiles gagnées dans ce jeu',
      sonActiver: 'Activer le son',
      sonCouper: 'Couper le son',
      question: (i, n) => 'Question ' + i + ' / ' + n,
      ecouter: 'Écouter',
      bravo: ['Bravo !', 'Super !', 'Génial !', 'Excellent !', 'Bien joué !', 'Magnifique !', 'Parfait !'],
      encore: ['Presque !', 'Essaie encore !', 'Pas tout à fait…', 'Courage !'],
      resultTitres: ['Continue de t’entraîner !', 'C’est un bon début !', 'Très bien !', 'Extraordinaire !'],
      score: (s, t) => s + ' / ' + t + ' bonne' + (s > 1 ? 's' : '') + ' réponse' + (s > 1 ? 's' : ''),
      etoilesSur3: (n) => n + ' étoile' + (n > 1 ? 's' : '') + ' sur 3',
      record: '🏆 Nouveau record !',
      rejouer: '🔁 Rejouer',
      jeuSuivant: 'Jeu suivant',
      ile: 'L’île',
      parametres: 'Paramètres',
      languesVisibles: 'Langues proposées',
      languesVisiblesAide: 'Décoche une langue pour la masquer aux enfants (par exemple le français, pour ne jouer que dans les langues étrangères).',
      fermer: 'Fermer',
      jeuIndisponible: 'Ce jeu n\u2019existe pas dans cette langue. Choisis un autre jeu :',
    },

    // Nom, lieu de l'île, compétence et description de chaque jeu.
    jeux: {
      images: { titre: 'Le mot et l’image', lieu: 'La plage', competence: 'Lecture', desc: 'Retrouve le mot qui correspond à l’image.' },
      genre: { titre: 'Un ou une ?', lieu: 'Le ponton', competence: 'Grammaire', desc: 'Choisis le bon article devant chaque mot.' },
      lettre: { titre: 'La lettre perdue', lieu: 'La grotte', competence: 'Orthographe', desc: 'Retrouve la lettre qui manque dans le mot.' },
      melange: { titre: 'Lettres en vrac', lieu: 'Le coffre', competence: 'Orthographe', desc: 'Remets les lettres dans le bon ordre.' },
      syllabes: { titre: 'Le pont des syllabes', lieu: 'Le pont de lianes', competence: 'Lecture', desc: 'Assemble les syllabes pour former le mot.' },
      memory: { titre: 'Mémo des mots', lieu: 'Le village', competence: 'Mémoire', desc: 'Retourne les cartes et associe images et mots.' },
      pendu: { titre: 'Les noix de coco', lieu: 'Le palmier', competence: 'Orthographe', desc: 'Devine le mot lettre par lettre avant que les noix ne tombent.' },
      dictee: { titre: 'La dictée du perroquet', lieu: 'La jungle', competence: 'Orthographe', desc: 'Écoute le perroquet et écris le mot.' },
      'mots-caches': { titre: 'Mots cachés', lieu: 'Les dunes', competence: 'Lecture', desc: 'Trouve les mots cachés dans la grille.' },
      rimes: { titre: 'La chasse aux rimes', lieu: 'La cascade', competence: 'Phonologie', desc: 'Trouve le mot qui rime.' },
      contraires: { titre: 'Les contraires', lieu: 'Le phare', competence: 'Vocabulaire', desc: 'Associe chaque mot à son contraire.' },
      pluriel: { titre: 'Un ou plusieurs', lieu: 'Le marché', competence: 'Grammaire', desc: 'Écris les mots au pluriel.' },
      phrase: { titre: 'La phrase en désordre', lieu: 'Le message en bouteille', competence: 'Grammaire', desc: 'Remets les mots de la phrase dans l’ordre.' },
      alphabet: { titre: 'L’ordre alphabétique', lieu: 'La bibliothèque du capitaine', competence: 'Lecture', desc: 'Range les mots dans l’ordre alphabétique.' },
    },

    // Écriture de la langue.
    alphabet: 'abcdefghijklmnopqrstuvwxyz'.split(''),
    voyelles: ['a', 'e', 'i', 'o', 'u', 'y'],
    // Lettres accentuées regroupées par lettre de base : sert au pendu (E révèle é è ê ë),
    // aux mots cachés (grille sans accents), aux comparaisons tolérantes.
    familles: { a: 'àâä', e: 'éèêë', i: 'îï', o: 'ôö', u: 'ùûü', y: 'ÿ', c: 'ç' },
    // Boutons de lettres spéciales proposés sous les champs de saisie.
    touchesSpeciales: ['é', 'è', 'ê', 'à', 'â', 'ç', 'ù', 'î', 'ô', 'ë', 'ï', 'œ'],
    // Lettres propres à la langue ajoutées au clavier du pendu (en plus de a–z).
    lettresClavier: [],
    // Ligatures à décomposer pour les grilles et le pendu.
    ligatures: { œ: 'oe', æ: 'ae' },

    // Jeu « Un ou une ? » : choix proposés par niveau et champ du mot qui donne la réponse.
    articles: {
      1: { champ: 'art', choix: ['un', 'une'] },
      2: { champ: 'art', choix: ['un', 'une'] },
      3: { champ: 'def', choix: ['le', 'la', 'l\u2019'] },
    },

    MOTS, MOTS_SIMPLES, RIMES, CONTRAIRES, PHRASES, PLURIELS, REGLES_PLURIEL,
  };
})();
