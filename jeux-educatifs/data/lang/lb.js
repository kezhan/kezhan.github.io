/*
 * L'Île aux mots — pack de langue : LUXEMBOURGEOIS (D’Wierderinsel).
 *
 * Même structure et mêmes clés que data/lang/fr.js (pack de référence).
 * Orthographe officielle de 2019 (référence : Lëtzebuerger Online Dictionnaire, LOD).
 * Public : enfants qui APPRENNENT le luxembourgeois ; on tutoie l'enfant (du).
 * La voix luxembourgeoise (lb-LU) manque souvent : tous les jeux restent jouables sans elle.
 *
 * MOTS : { mot (nom commun, donc avec MAJUSCULE : 'Kaz'), emoji, genre ('m' | 'f' | 'n'),
 *          syl (syllabes orales, comme on les frappe dans les mains : A·pel, Ba·nan, Fei·er,
 *               Scho·cke·la ; ch, sch, ck et les voyelles longues ou doubles (aa, ee, ii, ie, ue,
 *               ou, éi, äi, ei, au, äe) ne se coupent pas ; syl.join('') === mot),
 *          niveau (1 = mots très courants, 2 = courants, 3 = plus longs ou composés), theme,
 *          art (article indéfini réel : en / e / eng), def (article défini réel : den / de / d’),
 *          artG (réponse du jeu « genre » : den / déi / dat), defLong (article affiché dans ce jeu),
 *          pluriel (ajouté automatiquement depuis PLURIELS) }
 *
 * Règle de l'n (Eifeler Regel) : le -n final de den / en tombe devant une consonne,
 * SAUF devant n, d, t, z, h et devant une voyelle : den Hond, den Apel, den Zuch, de Bam, e Buch.
 * Articles définis : den (masculin), d’ (féminin ET neutre ET pluriel) : d’Kaz, d’Haus, d’Kazen.
 * Devant un adjectif ou quand on insiste, on emploie la forme longue : den / déi / dat
 * (déi kleng Kaz, dat grousst Haus). Seule la forme longue distingue les trois genres :
 * le jeu « genre » demande donc den / déi / dat (et non den / d’ / dat, car d’ est aussi neutre :
 * d’Haus est juste). Après la réponse, il écrit defLong devant le mot, règle de l'n appliquée
 * (de Bam, den Hond, déi Kaz, dat Haus).
 * Articles indéfinis : en (masculin et neutre, e devant consonne), eng (féminin).
 */
(function () {
  const M = (mot, emoji, genre, syl, niveau, theme) => ({ mot, emoji, genre, syl, niveau, theme });

  const MOTS = [
    // --- Niveau 1 : mots très courants ---
    M('Hond', '🐶', 'm', ['Hond'], 1, 'animaux'),
    M('Fësch', '🐟', 'm', ['Fësch'], 1, 'animaux'),
    M('Vull', '🐦', 'm', ['Vull'], 1, 'animaux'),
    M('Bier', '🐻', 'm', ['Bier'], 1, 'animaux'),
    M('Apel', '🍎', 'm', ['A', 'pel'], 1, 'nourriture'),
    M('Bam', '🌳', 'm', ['Bam'], 1, 'nature'),
    M('Mound', '🌙', 'm', ['Mound'], 1, 'nature'),
    M('Stär', '⭐', 'm', ['Stär'], 1, 'nature'),
    M('Ball', '⚽', 'm', ['Ball'], 1, 'objets'),
    M('Auto', '🚗', 'm', ['Au', 'to'], 1, 'objets'),
    M('Vëlo', '🚲', 'm', ['Vë', 'lo'], 1, 'objets'),
    M('Fouss', '🦶', 'm', ['Fouss'], 1, 'corps'),
    M('Mond', '👄', 'm', ['Mond'], 1, 'corps'),

    M('Kaz', '🐱', 'f', ['Kaz'], 1, 'animaux'),
    M('Mous', '🐭', 'f', ['Mous'], 1, 'animaux'),
    M('Kou', '🐮', 'f', ['Kou'], 1, 'animaux'),
    M('Int', '🦆', 'f', ['Int'], 1, 'animaux'),
    M('Banan', '🍌', 'f', ['Ba', 'nan'], 1, 'nourriture'),
    M('Tomat', '🍅', 'f', ['To', 'mat'], 1, 'nourriture'),
    M('Blumm', '🌸', 'f', ['Blumm'], 1, 'nature'),
    M('Sonn', '☀️', 'f', ['Sonn'], 1, 'nature'),
    M('Wollek', '☁️', 'f', ['Wol', 'lek'], 1, 'nature'),
    M('Dier', '🚪', 'f', ['Dier'], 1, 'objets'),
    M('Hand', '✋', 'f', ['Hand'], 1, 'corps'),
    M('Nues', '👃', 'f', ['Nues'], 1, 'corps'),

    M('Päerd', '🐴', 'n', ['Päerd'], 1, 'animaux'),
    M('Schwäin', '🐷', 'n', ['Schwäin'], 1, 'animaux'),
    M('Schof', '🐑', 'n', ['Schof'], 1, 'animaux'),
    M('Ee', '🥚', 'n', ['Ee'], 1, 'nourriture'),
    M('Brout', '🍞', 'n', ['Brout'], 1, 'nourriture'),
    M('Feier', '🔥', 'n', ['Fei', 'er'], 1, 'nature'),
    M('Haus', '🏠', 'n', ['Haus'], 1, 'objets'),
    M('Buch', '📖', 'n', ['Buch'], 1, 'objets'),
    M('Bett', '🛏️', 'n', ['Bett'], 1, 'objets'),
    M('Ouer', '👂', 'n', ['Ou', 'er'], 1, 'corps'),
    M('Kand', '🧒', 'n', ['Kand'], 1, 'personnes'),

    // --- Niveau 2 : mots courants ---
    M('Hues', '🐰', 'm', ['Hues'], 2, 'animaux'),
    M('Af', '🐒', 'm', ['Af'], 2, 'animaux'),
    M('Léiw', '🦁', 'm', ['Léiw'], 2, 'animaux'),
    M('Fuuss', '🦊', 'm', ['Fuuss'], 2, 'animaux'),
    M('Tiger', '🐯', 'm', ['Ti', 'ger'], 2, 'animaux'),
    M('Elefant', '🐘', 'm', ['E', 'le', 'fant'], 2, 'animaux'),
    M('Papagei', '🦜', 'm', ['Pa', 'pa', 'gei'], 2, 'animaux'),
    M('Kuch', '🎂', 'm', ['Kuch'], 2, 'nourriture'),
    M('Bierg', '⛰️', 'm', ['Bierg'], 2, 'nature'),
    M('Reebou', '🌈', 'm', ['Ree', 'bou'], 2, 'nature'),
    M('Zuch', '🚂', 'm', ['Zuch'], 2, 'objets'),
    M('Fliger', '✈️', 'm', ['Fli', 'ger'], 2, 'objets'),
    M('Kaddo', '🎁', 'm', ['Kad', 'do'], 2, 'objets'),
    M('Schlëssel', '🔑', 'm', ['Schlës', 'sel'], 2, 'objets'),
    M('Schong', '👟', 'm', ['Schong'], 2, 'objets'),
    M('Zant', '🦷', 'm', ['Zant'], 2, 'corps'),

    M('Schlaang', '🐍', 'f', ['Schlaang'], 2, 'animaux'),
    M('Giraff', '🦒', 'f', ['Gi', 'raff'], 2, 'animaux'),
    M('Bei', '🐝', 'f', ['Bei'], 2, 'animaux'),
    M('Kiischt', '🍒', 'f', ['Kiischt'], 2, 'nourriture'),
    M('Zitroun', '🍋', 'f', ['Zi', 'troun'], 2, 'nourriture'),
    M('Muert', '🥕', 'f', ['Muert'], 2, 'nourriture'),
    M('Gromper', '🥔', 'f', ['Grom', 'per'], 2, 'nourriture'),
    M('Schockela', '🍫', 'f', ['Scho', 'cke', 'la'], 2, 'nourriture'),
    M('Insel', '🏝️', 'f', ['In', 'sel'], 2, 'nature'),
    M('Kroun', '👑', 'f', ['Kroun'], 2, 'objets'),
    M('Auer', '🕰️', 'f', ['Au', 'er'], 2, 'objets'),

    M('Blat', '🍃', 'n', ['Blat'], 2, 'nature'),
    M('Mier', '🌊', 'n', ['Mier'], 2, 'nature'),
    M('Schëff', '🚢', 'n', ['Schëff'], 2, 'objets'),
    M('Kleed', '👗', 'n', ['Kleed'], 2, 'objets'),
    M('Hiem', '👕', 'n', ['Hiem'], 2, 'objets'),
    M('Zelt', '⛺', 'n', ['Zelt'], 2, 'objets'),
    M('Bild', '🖼️', 'n', ['Bild'], 2, 'objets'),
    M('Been', '🦵', 'n', ['Been'], 2, 'corps'),
    M('Häerz', '❤️', 'n', ['Häerz'], 2, 'corps'),

    // --- Niveau 3 : mots plus longs ou composés ---
    M('Päiperlek', '🦋', 'm', ['Päi', 'per', 'lek'], 3, 'animaux'),
    M('Pinguin', '🐧', 'm', ['Pin', 'gu', 'in'], 3, 'animaux'),
    M('Delfin', '🐬', 'm', ['Del', 'fin'], 3, 'animaux'),
    M('Walfësch', '🐳', 'm', ['Wal', 'fësch'], 3, 'animaux'),
    M('Kaktus', '🌵', 'm', ['Kak', 'tus'], 3, 'nature'),
    M('Vulkan', '🌋', 'm', ['Vul', 'kan'], 3, 'nature'),
    M('Palmebam', '🌴', 'm', ['Pal', 'me', 'bam'], 3, 'nature'),
    M('Schnéimann', '⛄', 'm', ['Schnéi', 'mann'], 3, 'nature'),
    M('Helikopter', '🚁', 'm', ['He', 'li', 'kop', 'ter'], 3, 'objets'),
    M('Parapli', '☂️', 'm', ['Pa', 'ra', 'pli'], 3, 'objets'),
    M('Bläistëft', '✏️', 'm', ['Bläi', 'stëft'], 3, 'objets'),
    M('Schoulsak', '🎒', 'm', ['Schoul', 'sak'], 3, 'objets'),
    M('Ballon', '🎈', 'm', ['Bal', 'lon'], 3, 'objets'),
    M('Computer', '💻', 'm', ['Com', 'pu', 'ter'], 3, 'objets'),
    M('Anker', '⚓', 'm', ['An', 'ker'], 3, 'objets'),
    M('Traktor', '🚜', 'm', ['Trak', 'tor'], 3, 'objets'),

    M('Waassermeloun', '🍉', 'f', ['Waas', 'ser', 'me', 'loun'], 3, 'nourriture'),
    M('Kokosnoss', '🥥', 'f', ['Ko', 'kos', 'noss'], 3, 'nourriture'),
    M('Mouschel', '🐚', 'f', ['Mou', 'schel'], 3, 'nature'),
    M('Sonneblumm', '🌻', 'f', ['Son', 'ne', 'blumm'], 3, 'nature'),
    M('Bréck', '🌉', 'f', ['Bréck'], 3, 'objets'),
    M('Schéier', '✂️', 'f', ['Schéi', 'er'], 3, 'objets'),
    M('Televisioun', '📺', 'f', ['Te', 'le', 'vi', 'sioun'], 3, 'objets'),
    M('Prinzessin', '👸', 'f', ['Prin', 'zes', 'sin'], 3, 'personnes'),

    M('Kaweechelchen', '🐿️', 'n', ['Ka', 'wee', 'chel', 'chen'], 3, 'animaux'),
    M('Schlass', '🏰', 'n', ['Schlass'], 3, 'objets'),
    M('Spidol', '🏥', 'n', ['Spi', 'dol'], 3, 'objets'),
    M('Meedchen', '👧', 'n', ['Meed', 'chen'], 3, 'personnes'),
  ];

  // Familles de rimes : même voyelle accentuée et même fin, en luxembourgeois parlé.
  // Chaque mot est accentué sur la syllabe qui rime (Metall, Ballett, Kamell, gesond, zréck…).
  // « son » est une graphie repère du son. Les noms gardent leur majuscule.
  // Pièges évités : Mond (o bref) ne rime pas avec Mound (diphtongue ou) ; Bier (ie) pas avec Bir (i long).
  // Hotel rime avec -ell (accent sur -tel) malgré sa graphie : la rime est un son, pas une orthographe.
  const RIMES = [
    { son: 'and', mots: ['Hand', 'Kand', 'Sand', 'Land', 'Strand'] },
    { son: 'all', mots: ['Ball', 'Stall', 'Fall', 'Metall', 'Kristall', 'iwwerall'] },
    { son: 'ond', mots: ['Hond', 'Mond', 'Grond', 'gesond', 'blond'] },
    { son: 'ett', mots: ['Bett', 'nett', 'fett', 'Ballett', 'Skelett'] },
    { son: 'az', mots: ['Kaz', 'Plaz', 'Saz', 'Schatz', 'Spatz', 'Matratz'] },
    { son: 'uch', mots: ['Buch', 'Kuch', 'Zuch', 'Duch', 'Besuch'] },
    { son: 'ell', mots: ['Kamell', 'Karussell', 'hell', 'Modell', 'Hotel'] },
    { son: 'aang', mots: ['Schlaang', 'laang', 'Gaang', 'Staang', 'Zaang'] },
    { son: 'éier', mots: ['Déier', 'Schéier', 'véier', 'Kéier', 'Stéier'] },
    { son: 'éck', mots: ['Bréck', 'Stéck', 'Réck', 'Gléck', 'zréck'] },
    { son: 'aach', mots: ['Daach', 'Saach', 'Baach', 'flaach', 'schwaach'] },
    { son: 'ou', mots: ['Kou', 'Frou', 'zou', 'sou', 'wou'] },
  ];

  // Paires de contraires : [a, b, niveau, nature ('adj' | 'verbe' | 'nom' | 'adv')].
  const CONTRAIRES = [
    ['grouss', 'kleng', 1, 'adj'], ['waarm', 'kal', 1, 'adj'], ['Dag', 'Nuecht', 1, 'nom'], ['uewen', 'ënnen', 1, 'adv'],
    ['voll', 'eidel', 1, 'adj'], ['laang', 'kuerz', 1, 'adj'], ['naass', 'dréchen', 1, 'adj'], ['al', 'jonk', 1, 'adj'],
    ['frou', 'traureg', 1, 'adj'], ['op', 'zou', 1, 'adj'], ['déck', 'dënn', 1, 'adj'], ['hell', 'däischter', 1, 'adj'],
    ['laachen', 'kräischen', 1, 'verbe'], ['kommen', 'goen', 1, 'verbe'],
    ['séier', 'lues', 2, 'adj'], ['haart', 'mëll', 2, 'adj'], ['schwéier', 'liicht', 2, 'adj'], ['staark', 'schwaach', 2, 'adj'],
    ['propper', 'dreckeg', 2, 'adj'], ['gesond', 'krank', 2, 'adj'], ['richteg', 'falsch', 2, 'adj'], ['fréi', 'spéit', 2, 'adv'],
    ['ëmmer', 'ni', 2, 'adv'], ['dobannen', 'dobaussen', 2, 'adv'], ['lénks', 'riets', 2, 'adv'], ['gewannen', 'verléieren', 2, 'verbe'],
    ['ginn', 'huelen', 2, 'verbe'], ['opmaachen', 'zoumaachen', 2, 'verbe'], ['Summer', 'Wanter', 2, 'nom'], ['Fro', 'Äntwert', 2, 'nom'],
    ['couragéiert', 'ängschtlech', 3, 'adj'], ['héiflech', 'onhéiflech', 3, 'adj'], ['éierlech', 'onéierlech', 3, 'adj'], ['méiglech', 'onméiglech', 3, 'adj'],
    ['breet', 'schmuel', 3, 'adj'], ['deier', 'bëlleg', 3, 'adj'], ['déif', 'flaach', 3, 'adj'], ['räich', 'aarm', 3, 'adj'],
    ['bauen', 'zerstéieren', 3, 'verbe'], ['erlaben', 'verbidden', 3, 'verbe'], ['kafen', 'verkafen', 3, 'verbe'], ['aschlofen', 'erwächen', 3, 'verbe'],
    ['Frënd', 'Feind', 3, 'nom'], ['Ufank', 'Enn', 3, 'nom'], ['Agang', 'Ausgang', 3, 'nom'], ['dacks', 'seelen', 3, 'adv'],
  ].map(([a, b, niveau, nature]) => ({ a, b, niveau, nature }));

  // Phrases à remettre dans l'ordre (la ponctuation reste collée au dernier mot).
  // Choisies pour n'avoir qu'un seul ordre correct avec ces mots (majuscule du premier mot, point final).
  // La règle de l'n y est appliquée : e Buch, en Apel, fuere mir, hu mir, säi Schatz.
  const PHRASES = [
    ['Déi kleng Kaz schléift.', 1], ['Mäi Papp liest e Buch.', 1], ['Ech iessen en Apel.', 1],
    ['Ech hunn en Hond.', 1], ['De Mound ass hell.', 1], ['Haut schéngt d’Sonn.', 1],
    ['De Vull séngt am Bam.', 1], ['D’Blumm ass rout.', 1], ['Den Hond spillt mam Ball.', 1],
    ['D’Kou gëtt Mëllech.', 1], ['D’Schëff ass um Mier.', 1],
    ['De klengen Hond leeft séier.', 2], ['D’Kanner schwammen am Mier.', 2],
    ['Meng Schwëster molt en Haus.', 2], ['De Pirat verstoppt säi Schatz.', 2],
    ['Et reent um Bierg.', 2], ['Den Af sëtzt um Bam.', 2],
    ['Muer fuere mir un d’Mier.', 2], ['Déi al Kou geet ganz lues.', 2],
    ['Eis Noperen hunn eng gro Kaz.', 2], ['Am Wanter baue mir e Schnéimann.', 2],
    ['Am Hierscht falen d’Blieder.', 2],
    ['Gëschter hu mir eng nei Insel entdeckt.', 3],
    ['De Papagei widderhëlt all Wuert vum Kapitän.', 3],
    ['Den ale Vulkan schléift zënter dausend Joer.', 3],
    ['D’Kanner bauen eng grouss Sandbuerg um Strand.', 3],
    ['De Kapitän kuckt op seng Schatzkaart.', 3],
    ['No der Schoul spille mir am Gaart.', 3],
    ['Meng Bomi erzielt eis eng spannend Geschicht.', 3],
    ['Déi fläisseg Bei sammelt séissen Hunneg.', 3],
    ['Um Weekend besiche mir eis Bomi.', 3],
  ].map(([texte, niveau]) => ({ texte, niveau }));

  // Mots supplémentaires sans image (pour les dictées, mots cachés, alphabet…).
  const MOTS_SIMPLES = {
    1: ['Mamm', 'Papp', 'Bomi', 'Bopa', 'jo', 'nee', 'rout', 'blo', 'gutt', 'Dësch', 'Stull', 'Numm', 'moien', 'eent', 'zwee'],
    2: ['Gaart', 'Schoul', 'Strooss', 'Waasser', 'Mëllech', 'Kéis', 'Musek', 'Brudder', 'Schwëster', 'Zëmmer', 'Kichen', 'Fënster', 'Famill', 'Sonndeg', 'merci'],
    3: ['Kapitän', 'Matrous', 'Pirat', 'Schatzkaart', 'Stuerm', 'Kompass', 'Rees', 'Bibliothéik', 'Gebuertsdag', 'Geheimnis', 'Entdecker', 'Equipe', 'Luuchttuerm', 'Horizont', 'Abenteuer'],
  };

  // Pluriels : { s (singulier), p (pluriel), g (genre 'm' | 'f' | 'n'), regle (clé de REGLES_PLURIEL), niveau, emoji? }
  // detS = article défini réel du singulier (den / de / d’), detP = 'd’' (toujours au pluriel).
  // Niveau 1 : -en et pluriels identiques. Niveau 2 : changement de voyelle, -er. Niveau 3 : tout.
  const P = (s, p, g, regle, niveau, emoji) => ({ s, p, g, regle, niveau, emoji: emoji || null });
  const PLURIELS = [
    P('Kaz', 'Kazen', 'f', 'en', 1, '🐱'), P('Blumm', 'Blummen', 'f', 'en', 1, '🌸'), P('Banan', 'Bananen', 'f', 'en', 1, '🍌'),
    P('Tomat', 'Tomaten', 'f', 'en', 1, '🍅'), P('Int', 'Inten', 'f', 'en', 1, '🦆'), P('Dier', 'Dieren', 'f', 'en', 1, '🚪'),
    P('Nues', 'Nuesen', 'f', 'en', 1, '👃'), P('Vull', 'Vullen', 'm', 'en', 1, '🐦'), P('Bier', 'Bieren', 'm', 'en', 1, '🐻'),
    P('Stär', 'Stären', 'm', 'en', 1, '⭐'),
    P('Auto', 'Autoen', 'm', 'o_en', 1, '🚗'), P('Vëlo', 'Vëloen', 'm', 'o_en', 1, '🚲'),
    P('Fësch', 'Fësch', 'm', 'gleich', 1, '🐟'), P('Päerd', 'Päerd', 'n', 'gleich', 1, '🐴'), P('Schof', 'Schof', 'n', 'gleich', 1, '🐑'),
    P('Schwäin', 'Schwäin', 'n', 'gleich', 1, '🐷'), P('Schong', 'Schong', 'm', 'gleich', 1, '👟'),

    P('Apel', 'Äppel', 'm', 'umlaut', 2, '🍎'), P('Hond', 'Hënn', 'm', 'umlaut', 2, '🐶'), P('Bam', 'Beem', 'm', 'umlaut', 2, '🌳'),
    P('Fouss', 'Féiss', 'm', 'umlaut', 2, '🦶'), P('Mous', 'Mais', 'f', 'umlaut', 2, '🐭'), P('Kou', 'Kéi', 'f', 'umlaut', 2, '🐮'),
    P('Hand', 'Hänn', 'f', 'umlaut', 2, '✋'), P('Ball', 'Bäll', 'm', 'umlaut', 2, '⚽'), P('Zant', 'Zänn', 'm', 'umlaut', 2, '🦷'),
    P('Kleed', 'Kleeder', 'n', 'er', 2, '👗'), P('Schëff', 'Schëffer', 'n', 'er', 2, '🚢'),
    P('Ee', 'Eeër', 'n', 'er', 2, '🥚'), P('Kand', 'Kanner', 'n', 'er', 2, '🧒'),
    P('Buch', 'Bicher', 'n', 'umlaut_er', 2, '📖'), P('Haus', 'Haiser', 'n', 'umlaut_er', 2, '🏠'), P('Mann', 'Männer', 'm', 'umlaut_er', 2, '👨'),
    P('Blat', 'Blieder', 'n', 'umlaut_er', 2, '🍃'),
    P('Kiischt', 'Kiischten', 'f', 'en', 2, '🍒'), P('Muert', 'Muerten', 'f', 'en', 2, '🥕'), P('Zitroun', 'Zitrounen', 'f', 'en', 2, '🍋'),
    P('Kaddo', 'Kaddoen', 'm', 'o_en', 2, '🎁'), P('Fliger', 'Fliger', 'm', 'gleich', 2, '✈️'), P('Been', 'Been', 'n', 'gleich', 2, '🦵'),

    P('Zuch', 'Zich', 'm', 'umlaut', 3, '🚂'), P('Stull', 'Still', 'm', 'umlaut', 3, '🪑'),
    P('Kapp', 'Käpp', 'm', 'umlaut', 3), P('Dag', 'Deeg', 'm', 'umlaut', 3), P('Brudder', 'Bridder', 'm', 'umlaut', 3),
    P('Schlass', 'Schlässer', 'n', 'umlaut_er', 3, '🏰'), P('Duerf', 'Dierfer', 'n', 'umlaut_er', 3, '🏘️'), P('Wuert', 'Wierder', 'n', 'umlaut_er', 3),
    P('Glas', 'Glieser', 'n', 'umlaut_er', 3), P('Land', 'Länner', 'n', 'umlaut_er', 3),
    P('Bild', 'Biller', 'n', 'er', 3, '🖼️'), P('Bierg', 'Bierger', 'm', 'er', 3, '⛰️'), P('Dësch', 'Dëscher', 'm', 'er', 3),
    P('Kierch', 'Kierchen', 'f', 'en', 3, '⛪'), P('Bréck', 'Brécken', 'f', 'en', 3, '🌉'), P('Schlaang', 'Schlaangen', 'f', 'en', 3, '🐍'),
    P('Giraff', 'Giraffen', 'f', 'en', 3, '🦒'), P('Wollek', 'Wolleken', 'f', 'en', 3, '☁️'),
    P('Kroun', 'Krounen', 'f', 'en', 3, '👑'), P('Schoul', 'Schoulen', 'f', 'en', 3, '🏫'),
    P('Elefant', 'Elefanten', 'm', 'en', 3, '🐘'), P('Léiw', 'Léiwen', 'm', 'en', 3, '🦁'), P('Af', 'Affen', 'm', 'en', 3, '🐒'),
  ];
  const REGLES_PLURIEL = {
    en: 'Vill Wierder kréien -en um Enn: d’Kaz, d’Kazen.',
    o_en: 'Wierder mat -o um Enn kréien och -en: den Auto, d’Autoen.',
    gleich: 'Verschidde Wierder bleiwen am Plural gläich: de Fësch, d’Fësch.',
    umlaut: 'Bei verschiddene Wierder ännert sech de Vokal: den Hond, d’Hënn.',
    er: 'Verschidde Wierder kréien -er um Enn: d’Kleed, d’Kleeder; d’Kand, d’Kanner.',
    umlaut_er: 'Verschidde Wierder kréien -er, an de Vokal ännert sech: d’Buch, d’Bicher.',
  };

  // Articles, règle de l'n appliquée : le -n de den / en reste devant une voyelle et devant n, d, t, z, h.
  const GARDE_N = /^[aeiouäéëndtzh]/i;
  const garde = (mot) => GARDE_N.test(mot);
  const den = (mot) => (garde(mot) ? 'den' : 'de');
  const en = (mot) => (garde(mot) ? 'en' : 'e');
  const LONG = { m: 'den', f: 'déi', n: 'dat' };
  MOTS.forEach((m) => {
    m.art = m.genre === 'f' ? 'eng' : en(m.mot);
    m.def = m.genre === 'm' ? den(m.mot) : 'd’';
    m.artG = LONG[m.genre];
    m.defLong = m.genre === 'm' ? den(m.mot) : LONG[m.genre];
  });
  PLURIELS.forEach((x) => {
    x.detS = x.g === 'm' ? den(x.s) : 'd’';
    x.detP = 'd’';
    const m = MOTS.find((w) => w.mot === x.s);
    if (m) m.pluriel = x.p;
  });

  window.ILE_LANGS = window.ILE_LANGS || {};
  window.ILE_LANGS.lb = {
    code: 'lb',
    nom: 'Lëtzebuergesch',
    drapeau: '🇱🇺',
    tts: 'lb-LU',
    dir: 'ltr',
    htmlLang: 'lb',
    sepMots: ' ',
    ecriture: 'alphabet',

    niveaux: [
      { n: 1, nom: 'Schëffsjong', classe: 'Ufänger', emoji: '🐣' },
      { n: 2, nom: 'Matrous', classe: 'Mëttelstuf', emoji: '⚓' },
      { n: 3, nom: 'Kapitän', classe: 'Fortgeschratt', emoji: '🏴‍☠️' },
    ],

    // Textes de l'interface commune (accueil, en-tête, fenêtre de résultat). On tutoie l'enfant (du).
    // « vu 5 » : règle de l'n devant « fënnef » ; « vun 3 » : le n reste devant « dräi ».
    ui: {
      titreSite: 'D’Wierderinsel',
      accroche: 'Entdeck d’Insel, spill mat Wierder a fëll deng Schatzkëscht!',
      descriptionSite: 'Gratis Léierspiller fir Kanner vu 5 bis 10 Joer, fir Lëtzebuergesch ze léieren.',
      choisisLangue: 'Sprooch',
      choisisGrade: 'Wiel däi Rang als Pirat',
      lesJeux: 'D’Spiller',
      etoilesTotal: (n, t) => n + ' / ' + t + ' Stären',
      etoilesNiveau: (n) => n + ' vun 3 Stären op dësem Niveau',
      surprise: '🎲 Iwwerraschungsspill',
      footer: 'Gratis Léierspiller fir Kanner vu 5 bis 10 Joer · Däi Fortschrëtt gëtt op dësem Apparat gespäichert.',
      effacer: '🧹 Mäi Fortschrëtt läschen',
      confirmerEffacer: 'All d’Stären an dëser Sprooch op dësem Apparat läschen?',
      retourIle: 'Zréck op d’Insel',
      niveau: 'Niveau',
      etoilesJeu: 'Deng Stären an dësem Spill',
      sonActiver: 'Toun uschalten',
      sonCouper: 'Toun ausschalten',
      question: (i, n) => 'Fro ' + i + ' / ' + n,
      ecouter: 'Lauschteren',
      bravo: ['Super!', 'Bravo!', 'Flott!', 'Genial!', 'Gutt gemaach!', 'Perfekt!', 'Prima!'],
      encore: ['Bal!', 'Probéier nach eng Kéier!', 'Net ganz …', 'Courage!'],
      resultTitres: ['Trainéier weider!', 'E gudde Start!', 'Ganz gutt!', 'Fantastesch!'],
      score: (s, t) => s + ' / ' + t + (s === 1 ? ' richteg Äntwert' : ' richteg Äntwerten'),
      etoilesSur3: (n) => n + ' vun 3 Stären',
      record: '🏆 Neie Rekord!',
      rejouer: '🔁 Nach eng Kéier',
      jeuSuivant: 'Nächst Spill',
      ile: 'D’Insel',
      parametres: 'Astellungen',
      languesVisibles: 'Ugewise Sproochen',
      languesVisiblesAide: 'Huel den Haken ewech, fir eng Sprooch virun de Kanner ze verstoppen (zum Beispill Franséisch, fir nëmmen a Friemsproochen ze spillen).',
      fermer: 'Zoumaachen',
      jeuIndisponible: 'Dëst Spill gëtt et an dëser Sprooch net. Wiel en anert Spill:',
    },

    // Nom, lieu de l'île, compétence et description de chaque jeu.
    jeux: {
      images: { titre: 'Wuert a Bild', lieu: 'D’Plage', competence: 'Liesen', desc: 'Fann d’Wuert, dat bei d’Bild passt.' },
      genre: { titre: 'Den, déi oder dat?', lieu: 'Den Hafen', competence: 'Grammatik', desc: 'Wiel de richtegen Artikel fir all Wuert.' },
      lettre: { titre: 'Wou ass de Buschtaf?', lieu: 'D’Hiel', competence: 'Orthographie', desc: 'Fann de Buschtaf, deen am Wuert feelt.' },
      melange: { titre: 'Buschtawesalat', lieu: 'D’Schatzkëscht', competence: 'Orthographie', desc: 'Setz d’Buschtawen an déi richteg Reiefolleg.' },
      syllabes: { titre: 'D’Silbebréck', lieu: 'D’Lianebréck', competence: 'Liesen', desc: 'Setz d’Silben zesummen, fir d’Wuert ze bauen.' },
      memory: { titre: 'Wierder-Memo', lieu: 'D’Duerf', competence: 'Gedächtnes', desc: 'Dréi d’Kaarten ëm a fann d’Bild an d’Wuert, déi zesummepassen.' },
      pendu: { titre: 'D’Kokosnëss', lieu: 'De Palmebam', competence: 'Orthographie', desc: 'Fann d’Wuert Buschtaf fir Buschtaf, ier d’Kokosnëss falen.' },
      dictee: { titre: 'D’Dictée vum Papagei', lieu: 'Den Dschungel', competence: 'Orthographie', desc: 'Lauschter dem Papagei no a schreif d’Wuert.' },
      'mots-caches': { titre: 'Verstoppt Wierder', lieu: 'D’Dünen', competence: 'Liesen', desc: 'Fann d’Wierder, déi am Gitter verstoppt sinn.' },
      rimes: { titre: 'D’Reimjuegd', lieu: 'De Waasserfall', competence: 'Lauschteren', desc: 'Fann d’Wuert, dat sech reimt.' },
      contraires: { titre: 'Géigendeeler', lieu: 'De Luuchttuerm', competence: 'Wuertschatz', desc: 'Fann fir all Wuert säi Géigendeel.' },
      pluriel: { titre: 'Eent oder vill', lieu: 'De Maart', competence: 'Grammatik', desc: 'Schreif d’Wierder am Plural.' },
      phrase: { titre: 'De Saz-Salat', lieu: 'D’Fläschepost', competence: 'Grammatik', desc: 'Setz d’Wierder vum Saz an déi richteg Reiefolleg.' },
      alphabet: { titre: 'Alphabetesch Reiefolleg', lieu: 'D’Bibliothéik vum Kapitän', competence: 'Liesen', desc: 'Setz d’Wierder an déi alphabetesch Reiefolleg.' },
    },

    // Écriture de la langue : 26 lettres + ä, é, ë, lettres à part entière
    // (Kaz ≠ Käz, Fësch ≠ Fesch), donc aucune famille d'accents.
    alphabet: 'abcdefghijklmnopqrstuvwxyz'.split(''),
    voyelles: ['a', 'e', 'i', 'o', 'u', 'ä', 'é', 'ë'],
    familles: {},
    touchesSpeciales: ['ä', 'é', 'ë', 'Ä', 'É'],
    lettresClavier: ['ä', 'é', 'ë'],
    ligatures: {},

    // Jeu « Den, déi oder dat? » : la forme longue de l'article défini, seule à distinguer
    // les trois genres ; affiche = article écrit devant le mot après la réponse.
    articles: {
      1: { champ: 'artG', choix: ['den', 'déi', 'dat'], affiche: 'defLong' },
      2: { champ: 'artG', choix: ['den', 'déi', 'dat'], affiche: 'defLong' },
      3: { champ: 'artG', choix: ['den', 'déi', 'dat'], affiche: 'defLong' },
    },

    MOTS, MOTS_SIMPLES, RIMES, CONTRAIRES, PHRASES, PLURIELS, REGLES_PLURIEL,
  };
})();
