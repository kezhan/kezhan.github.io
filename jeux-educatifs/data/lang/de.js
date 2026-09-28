/*
 * L'Île aux mots — pack de langue : ALLEMAND (Die Wörterinsel).
 *
 * Même structure et mêmes clés que data/lang/fr.js (pack de référence).
 * Allemand standard, orthographe réformée (ß après voyelle longue ou diphtongue : Fuß, Straße ;
 * ss après voyelle brève : Schloss, nass). Public : enfants qui APPRENNENT l'allemand.
 *
 * MOTS : { mot (nom commun, donc avec MAJUSCULE : 'Katze'), emoji, genre ('m' | 'f' | 'n'),
 *          syl (coupure des syllabes selon le Duden : Kat·ze, Ap·fel, Schne·cke, Ele·fant ;
 *               ck, ch, sch ne se coupent pas ; une voyelle seule en début de mot ne se détache pas :
 *               Amei·se ; syl.join('') === mot),
 *          niveau (1 = mots très courants, 2 = courants, 3 = plus longs ou composés), theme,
 *          art (article indéfini : ein / eine), def (article défini : der / die / das),
 *          pluriel (forme exacte du pluriel quand il n'y en a qu'une) }
 * Le jeu « genre » devient « Der, die oder das? » à tous les niveaux : c'est LE point clé
 * pour un apprenant. Chaque niveau équilibre les trois genres.
 */
(function () {
  const M = (mot, emoji, genre, syl, niveau, theme, pluriel) =>
    Object.assign({ mot, emoji, genre, syl, niveau, theme }, pluriel ? { pluriel } : {});

  const MOTS = [
    // --- Niveau 1 : mots très courants (13 der, 13 die, 13 das) ---
    M('Hund', '🐶', 'm', ['Hund'], 1, 'animaux', 'Hunde'),
    M('Fisch', '🐟', 'm', ['Fisch'], 1, 'animaux', 'Fische'),
    M('Bär', '🐻', 'm', ['Bär'], 1, 'animaux', 'Bären'),
    M('Apfel', '🍎', 'm', ['Ap', 'fel'], 1, 'nourriture', 'Äpfel'),
    M('Mond', '🌙', 'm', ['Mond'], 1, 'nature', 'Monde'),
    M('Baum', '🌳', 'm', ['Baum'], 1, 'nature', 'Bäume'),
    M('Stern', '⭐', 'm', ['Stern'], 1, 'nature', 'Sterne'),
    M('Ball', '⚽', 'm', ['Ball'], 1, 'objets', 'Bälle'),
    M('Bus', '🚌', 'm', ['Bus'], 1, 'objets', 'Busse'),
    M('Hut', '🎩', 'm', ['Hut'], 1, 'objets', 'Hüte'),
    M('Fuß', '🦶', 'm', ['Fuß'], 1, 'corps', 'Füße'),
    M('Mund', '👄', 'm', ['Mund'], 1, 'corps', 'Münder'),
    M('Zahn', '🦷', 'm', ['Zahn'], 1, 'corps', 'Zähne'),

    M('Katze', '🐱', 'f', ['Kat', 'ze'], 1, 'animaux', 'Katzen'),
    M('Maus', '🐭', 'f', ['Maus'], 1, 'animaux', 'Mäuse'),
    M('Kuh', '🐮', 'f', ['Kuh'], 1, 'animaux', 'Kühe'),
    M('Ente', '🦆', 'f', ['En', 'te'], 1, 'animaux', 'Enten'),
    M('Banane', '🍌', 'f', ['Ba', 'na', 'ne'], 1, 'nourriture', 'Bananen'),
    M('Tomate', '🍅', 'f', ['To', 'ma', 'te'], 1, 'nourriture', 'Tomaten'),
    M('Birne', '🍐', 'f', ['Bir', 'ne'], 1, 'nourriture', 'Birnen'),
    M('Pizza', '🍕', 'f', ['Piz', 'za'], 1, 'nourriture'),
    M('Blume', '🌸', 'f', ['Blu', 'me'], 1, 'nature', 'Blumen'),
    M('Sonne', '☀️', 'f', ['Son', 'ne'], 1, 'nature', 'Sonnen'),
    M('Tür', '🚪', 'f', ['Tür'], 1, 'objets', 'Türen'),
    M('Hand', '✋', 'f', ['Hand'], 1, 'corps', 'Hände'),
    M('Nase', '👃', 'f', ['Na', 'se'], 1, 'corps', 'Nasen'),

    M('Schaf', '🐑', 'n', ['Schaf'], 1, 'animaux', 'Schafe'),
    M('Schwein', '🐷', 'n', ['Schwein'], 1, 'animaux', 'Schweine'),
    M('Pferd', '🐴', 'n', ['Pferd'], 1, 'animaux', 'Pferde'),
    M('Ei', '🥚', 'n', ['Ei'], 1, 'nourriture', 'Eier'),
    M('Brot', '🍞', 'n', ['Brot'], 1, 'nourriture', 'Brote'),
    M('Eis', '🍦', 'n', ['Eis'], 1, 'nourriture'),
    M('Haus', '🏠', 'n', ['Haus'], 1, 'objets', 'Häuser'),
    M('Auto', '🚗', 'n', ['Au', 'to'], 1, 'objets', 'Autos'),
    M('Buch', '📖', 'n', ['Buch'], 1, 'objets', 'Bücher'),
    M('Boot', '⛵', 'n', ['Boot'], 1, 'objets', 'Boote'),
    M('Bett', '🛏️', 'n', ['Bett'], 1, 'objets', 'Betten'),
    M('Ohr', '👂', 'n', ['Ohr'], 1, 'corps', 'Ohren'),
    M('Auge', '👁️', 'n', ['Au', 'ge'], 1, 'corps', 'Augen'),

    // --- Niveau 2 : mots courants (12 der, 12 die, 12 das) ---
    M('Hase', '🐰', 'm', ['Ha', 'se'], 2, 'animaux', 'Hasen'),
    M('Vogel', '🐦', 'm', ['Vo', 'gel'], 2, 'animaux', 'Vögel'),
    M('Affe', '🐒', 'm', ['Af', 'fe'], 2, 'animaux', 'Affen'),
    M('Löwe', '🦁', 'm', ['Lö', 'we'], 2, 'animaux', 'Löwen'),
    M('Tiger', '🐯', 'm', ['Ti', 'ger'], 2, 'animaux', 'Tiger'),
    M('Frosch', '🐸', 'm', ['Frosch'], 2, 'animaux', 'Frösche'),
    M('Fuchs', '🦊', 'm', ['Fuchs'], 2, 'animaux', 'Füchse'),
    M('Elefant', '🐘', 'm', ['Ele', 'fant'], 2, 'animaux', 'Elefanten'),
    M('Kuchen', '🎂', 'm', ['Ku', 'chen'], 2, 'nourriture', 'Kuchen'),
    M('Schneemann', '⛄', 'm', ['Schnee', 'mann'], 2, 'nature', 'Schneemänner'),
    M('Zug', '🚂', 'm', ['Zug'], 2, 'objets', 'Züge'),
    M('Schlüssel', '🔑', 'm', ['Schlüs', 'sel'], 2, 'objets', 'Schlüssel'),

    M('Biene', '🐝', 'f', ['Bie', 'ne'], 2, 'animaux', 'Bienen'),
    M('Schlange', '🐍', 'f', ['Schlan', 'ge'], 2, 'animaux', 'Schlangen'),
    M('Giraffe', '🦒', 'f', ['Gi', 'raf', 'fe'], 2, 'animaux', 'Giraffen'),
    M('Eule', '🦉', 'f', ['Eu', 'le'], 2, 'animaux', 'Eulen'),
    M('Schnecke', '🐌', 'f', ['Schne', 'cke'], 2, 'animaux', 'Schnecken'),
    M('Kirsche', '🍒', 'f', ['Kir', 'sche'], 2, 'nourriture', 'Kirschen'),
    M('Zitrone', '🍋', 'f', ['Zi', 'tro', 'ne'], 2, 'nourriture', 'Zitronen'),
    M('Karotte', '🥕', 'f', ['Ka', 'rot', 'te'], 2, 'nourriture', 'Karotten'),
    M('Wolke', '☁️', 'f', ['Wol', 'ke'], 2, 'nature', 'Wolken'),
    M('Insel', '🏝️', 'f', ['In', 'sel'], 2, 'nature', 'Inseln'),
    M('Rakete', '🚀', 'f', ['Ra', 'ke', 'te'], 2, 'objets', 'Raketen'),
    M('Brille', '👓', 'f', ['Bril', 'le'], 2, 'objets', 'Brillen'),

    M('Huhn', '🐔', 'n', ['Huhn'], 2, 'animaux', 'Hühner'),
    M('Zebra', '🦓', 'n', ['Ze', 'bra'], 2, 'animaux', 'Zebras'),
    M('Feuer', '🔥', 'n', ['Feu', 'er'], 2, 'nature', 'Feuer'),
    M('Blatt', '🍃', 'n', ['Blatt'], 2, 'nature', 'Blätter'),
    M('Flugzeug', '✈️', 'n', ['Flug', 'zeug'], 2, 'objets', 'Flugzeuge'),
    M('Fahrrad', '🚲', 'n', ['Fahr', 'rad'], 2, 'objets', 'Fahrräder'),
    M('Schiff', '🚢', 'n', ['Schiff'], 2, 'objets', 'Schiffe'),
    M('Geschenk', '🎁', 'n', ['Ge', 'schenk'], 2, 'objets', 'Geschenke'),
    M('Kleid', '👗', 'n', ['Kleid'], 2, 'objets', 'Kleider'),
    M('Zelt', '⛺', 'n', ['Zelt'], 2, 'objets', 'Zelte'),
    M('Handy', '📱', 'n', ['Han', 'dy'], 2, 'objets', 'Handys'),
    M('Bein', '🦵', 'n', ['Bein'], 2, 'corps', 'Beine'),

    // --- Niveau 3 : mots plus longs ou composés (11 der, 11 die, 11 das) ---
    M('Schmetterling', '🦋', 'm', ['Schmet', 'ter', 'ling'], 3, 'animaux', 'Schmetterlinge'),
    M('Marienkäfer', '🐞', 'm', ['Ma', 'ri', 'en', 'kä', 'fer'], 3, 'animaux', 'Marienkäfer'),
    M('Oktopus', '🐙', 'm', ['Ok', 'to', 'pus'], 3, 'animaux'),
    M('Pinguin', '🐧', 'm', ['Pin', 'gu', 'in'], 3, 'animaux', 'Pinguine'),
    M('Papagei', '🦜', 'm', ['Pa', 'pa', 'gei'], 3, 'animaux', 'Papageien'),
    M('Kürbis', '🎃', 'm', ['Kür', 'bis'], 3, 'nourriture', 'Kürbisse'),
    M('Regenbogen', '🌈', 'm', ['Re', 'gen', 'bo', 'gen'], 3, 'nature'),
    M('Vulkan', '🌋', 'm', ['Vul', 'kan'], 3, 'nature', 'Vulkane'),
    M('Hubschrauber', '🚁', 'm', ['Hub', 'schrau', 'ber'], 3, 'objets', 'Hubschrauber'),
    M('Rucksack', '🎒', 'm', ['Ruck', 'sack'], 3, 'objets', 'Rucksäcke'),
    M('Luftballon', '🎈', 'm', ['Luft', 'bal', 'lon'], 3, 'objets'),

    M('Schildkröte', '🐢', 'f', ['Schild', 'krö', 'te'], 3, 'animaux', 'Schildkröten'),
    M('Ameise', '🐜', 'f', ['Amei', 'se'], 3, 'animaux', 'Ameisen'),
    M('Fledermaus', '🦇', 'f', ['Fle', 'der', 'maus'], 3, 'animaux', 'Fledermäuse'),
    M('Erdbeere', '🍓', 'f', ['Erd', 'bee', 're'], 3, 'nourriture', 'Erdbeeren'),
    M('Wassermelone', '🍉', 'f', ['Was', 'ser', 'me', 'lo', 'ne'], 3, 'nourriture', 'Wassermelonen'),
    M('Ananas', '🍍', 'f', ['Ana', 'nas'], 3, 'nourriture'),
    M('Kartoffel', '🥔', 'f', ['Kar', 'tof', 'fel'], 3, 'nourriture', 'Kartoffeln'),
    M('Muschel', '🐚', 'f', ['Mu', 'schel'], 3, 'nature', 'Muscheln'),
    M('Brücke', '🌉', 'f', ['Brü', 'cke'], 3, 'objets', 'Brücken'),
    M('Gitarre', '🎸', 'f', ['Gi', 'tar', 're'], 3, 'objets', 'Gitarren'),
    M('Krone', '👑', 'f', ['Kro', 'ne'], 3, 'objets', 'Kronen'),

    M('Krokodil', '🐊', 'n', ['Kro', 'ko', 'dil'], 3, 'animaux', 'Krokodile'),
    M('Känguru', '🦘', 'n', ['Kän', 'gu', 'ru'], 3, 'animaux', 'Kängurus'),
    M('Eichhörnchen', '🐿️', 'n', ['Eich', 'hörn', 'chen'], 3, 'animaux', 'Eichhörnchen'),
    M('Nashorn', '🦏', 'n', ['Nas', 'horn'], 3, 'animaux', 'Nashörner'),
    M('Nilpferd', '🦛', 'n', ['Nil', 'pferd'], 3, 'animaux', 'Nilpferde'),
    M('Kamel', '🐫', 'n', ['Ka', 'mel'], 3, 'animaux', 'Kamele'),
    M('Einhorn', '🦄', 'n', ['Ein', 'horn'], 3, 'animaux', 'Einhörner'),
    M('Pflaster', '🩹', 'n', ['Pflas', 'ter'], 3, 'objets', 'Pflaster'),
    M('Schloss', '🏰', 'n', ['Schloss'], 3, 'objets', 'Schlösser'),
    M('Klavier', '🎹', 'n', ['Kla', 'vier'], 3, 'objets', 'Klaviere'),
    M('Motorrad', '🏍️', 'n', ['Mo', 'tor', 'rad'], 3, 'objets', 'Motorräder'),
  ];

  // Familles de rimes : même voyelle accentuée (longue ou brève) et même fin, en allemand standard.
  // « son » est une graphie repère du son. Les noms gardent leur majuscule.
  // Pièges évités : Mond (o long) ne rime pas avec blond (o bref), Bär (ä long) pas avec Meer (e long),
  // Hose (s sonore) pas avec Soße (s sourd).
  const RIMES = [
    { son: 'aus', mots: ['Haus', 'Maus', 'Laus', 'aus', 'Strauß', 'Applaus'] },
    { son: 'all', mots: ['Ball', 'Stall', 'Knall', 'Fall', 'Kristall', 'Metall'] },
    { son: 'and', mots: ['Hand', 'Sand', 'Wand', 'Land', 'Strand'] },
    { son: 'aum', mots: ['Baum', 'Traum', 'Schaum', 'Raum', 'kaum'] },
    { son: 'ase', mots: ['Nase', 'Hase', 'Vase', 'Blase', 'Oase'] },
    { son: 'ein', mots: ['Bein', 'Schwein', 'Stein', 'klein', 'nein', 'fein'] },
    { son: 'atze', mots: ['Katze', 'Tatze', 'Glatze', 'Matratze', 'Fratze'] },
    { son: 'ose', mots: ['Hose', 'Rose', 'Dose', 'Matrose', 'Aprikose'] },
    { son: 'ier', mots: ['Tier', 'vier', 'hier', 'Papier', 'Klavier'] },
    { son: 'ange', mots: ['Schlange', 'Stange', 'Zange', 'Wange', 'lange'] },
    { son: 'ücke', mots: ['Brücke', 'Mücke', 'Lücke', 'Krücke', 'Perücke'] },
    { son: 'ut', mots: ['Hut', 'gut', 'Mut', 'Wut', 'Flut'] },
    { son: 'und', mots: ['Hund', 'Mund', 'rund', 'bunt', 'gesund'] },
    { son: 'ee', mots: ['See', 'Tee', 'Schnee', 'Klee', 'Fee', 'Idee'] },
    { son: 'ett', mots: ['Bett', 'nett', 'Brett', 'Ballett', 'Skelett'] },
    { son: 'ecke', mots: ['Schnecke', 'Decke', 'Ecke', 'Hecke', 'Zecke'] },
    { son: 'anne', mots: ['Kanne', 'Tanne', 'Pfanne', 'Wanne', 'Panne'] },
    { son: 'icht', mots: ['Licht', 'nicht', 'Gesicht', 'Gedicht', 'Gewicht'] },
  ];

  // Paires de contraires : [a, b, niveau, nature ('adj' | 'verbe' | 'nom' | 'adv')].
  const CONTRAIRES = [
    ['groß', 'klein', 1, 'adj'], ['heiß', 'kalt', 1, 'adj'], ['Tag', 'Nacht', 1, 'nom'], ['oben', 'unten', 1, 'adv'],
    ['voll', 'leer', 1, 'adj'], ['lang', 'kurz', 1, 'adj'], ['nass', 'trocken', 1, 'adj'], ['jung', 'alt', 1, 'adj'],
    ['schnell', 'langsam', 1, 'adj'], ['hell', 'dunkel', 1, 'adj'], ['dick', 'dünn', 1, 'adj'], ['fröhlich', 'traurig', 1, 'adj'],
    ['lachen', 'weinen', 1, 'verbe'], ['kommen', 'gehen', 1, 'verbe'],
    ['laut', 'leise', 2, 'adj'], ['sauber', 'schmutzig', 2, 'adj'], ['schwer', 'leicht', 2, 'adj'], ['hart', 'weich', 2, 'adj'],
    ['stark', 'schwach', 2, 'adj'], ['gesund', 'krank', 2, 'adj'], ['richtig', 'falsch', 2, 'adj'], ['früh', 'spät', 2, 'adv'],
    ['immer', 'nie', 2, 'adv'], ['drinnen', 'draußen', 2, 'adv'], ['links', 'rechts', 2, 'adv'], ['gewinnen', 'verlieren', 2, 'verbe'],
    ['geben', 'nehmen', 2, 'verbe'], ['öffnen', 'schließen', 2, 'verbe'], ['Frage', 'Antwort', 2, 'nom'], ['Sommer', 'Winter', 2, 'nom'],
    ['mutig', 'ängstlich', 3, 'adj'], ['großzügig', 'geizig', 3, 'adj'], ['höflich', 'unhöflich', 3, 'adj'], ['ehrlich', 'unehrlich', 3, 'adj'],
    ['möglich', 'unmöglich', 3, 'adj'], ['glatt', 'rau', 3, 'adj'], ['breit', 'schmal', 3, 'adj'], ['teuer', 'billig', 3, 'adj'],
    ['flach', 'tief', 3, 'adj'], ['bauen', 'zerstören', 3, 'verbe'], ['erlauben', 'verbieten', 3, 'verbe'], ['kaufen', 'verkaufen', 3, 'verbe'],
    ['einschlafen', 'aufwachen', 3, 'verbe'], ['Freund', 'Feind', 3, 'nom'], ['Anfang', 'Ende', 3, 'nom'], ['Eingang', 'Ausgang', 3, 'nom'],
    ['vorwärts', 'rückwärts', 3, 'adv'], ['oft', 'selten', 3, 'adv'],
  ].map(([a, b, niveau, nature]) => ({ a, b, niveau, nature }));

  // Phrases à remettre dans l'ordre (la ponctuation reste collée au dernier mot).
  // Choisies pour n'avoir qu'un seul ordre correct avec ces mots (majuscule du premier mot, point final).
  const PHRASES = [
    ['Die Katze schläft.', 1], ['Papa liest ein Buch.', 1], ['Der Hund spielt mit dem Ball.', 1],
    ['Das Boot ist auf dem Meer.', 1], ['Ich esse einen Apfel.', 1], ['Die Sonne scheint.', 1],
    ['Der Hase hüpft über die Wiese.', 1], ['Oma backt einen Kuchen.', 1], ['Der Mond ist rund.', 1],
    ['Das Pferd frisst Gras.', 1],
    ['Der kleine Hund bellt laut.', 2], ['Die Kinder schwimmen im Meer.', 2],
    ['Meine Schwester malt ein großes Haus.', 2], ['Der Pirat versteckt seinen Schatz.', 2],
    ['Es regnet auf dem Berg.', 2], ['Der Affe klettert auf die Palme.', 2],
    ['Morgen fahren wir an den Strand.', 2], ['Die alte Schildkröte geht langsam.', 2],
    ['Unser Nachbar hat eine graue Katze.', 2], ['Im Winter bauen wir einen Schneemann.', 2],
    ['Im Herbst werden die Blätter bunt.', 2],
    ['Der fröhliche Delfin springt hoch über die Wellen.', 3],
    ['Gestern haben wir eine geheimnisvolle Insel entdeckt.', 3],
    ['Der Papagei wiederholt jedes Wort des Kapitäns.', 3],
    ['Die Matrosen ziehen das große Segel hoch.', 3],
    ['Bei Sturm hilft der Leuchtturm den Schiffen.', 3],
    ['Der alte Vulkan schläft seit tausend Jahren.', 3],
    ['Meine Großmutter backt einen leckeren Schokoladenkuchen.', 3],
    ['Nach dem Malen räumen wir die Stifte auf.', 3],
    ['Die Kinder bauen eine riesige Sandburg am Strand.', 3],
    ['Der Kapitän schaut durch sein Fernrohr.', 3],
  ].map(([texte, niveau]) => ({ texte, niveau }));

  // Mots supplémentaires sans image (pour les dictées, mots cachés, alphabet…).
  const MOTS_SIMPLES = {
    1: ['Mama', 'Papa', 'Oma', 'Opa', 'ja', 'nein', 'rot', 'blau', 'gut', 'Tag', 'Tisch', 'Name', 'hallo', 'eins', 'zwei'],
    2: ['Garten', 'Schule', 'Strand', 'Wasser', 'Freund', 'Musik', 'Bruder', 'Schwester', 'Zimmer', 'Straße', 'Familie', 'Sonntag', 'Morgen', 'danke', 'bitte'],
    3: ['Kapitän', 'Abenteuer', 'Schatzkarte', 'Sturm', 'Pirat', 'Kompass', 'Reise', 'Bibliothek', 'Geburtstag', 'geheimnisvoll', 'Entdecker', 'Mannschaft', 'Fernrohr', 'Horizont', 'Leuchtturm'],
  };

  // Pluriels : { s (singulier), p (pluriel), g (genre 'm' | 'f' | 'n'), regle (clé de REGLES_PLURIEL), niveau, emoji? }
  // detS = article défini du singulier (der / die / das), detP = 'die' (toujours au pluriel).
  // Niveau 1 : pluriels réguliers les plus fréquents (-e, -n, -s, -en).
  // Niveau 2 : Umlaut + -e, -er, Umlaut + -er, pluriel identique, Apfel / Vogel. Niveau 3 : tout.
  const P = (s, p, g, regle, niveau, emoji) => ({ s, p, g, regle, niveau, emoji: emoji || null });
  const PLURIELS = [
    P('Hund', 'Hunde', 'm', 'e', 1, '🐶'), P('Fisch', 'Fische', 'm', 'e', 1, '🐟'), P('Stern', 'Sterne', 'm', 'e', 1, '⭐'),
    P('Schaf', 'Schafe', 'n', 'e', 1, '🐑'), P('Pferd', 'Pferde', 'n', 'e', 1, '🐴'), P('Schwein', 'Schweine', 'n', 'e', 1, '🐷'),
    P('Brot', 'Brote', 'n', 'e', 1, '🍞'),
    P('Katze', 'Katzen', 'f', 'n', 1, '🐱'), P('Blume', 'Blumen', 'f', 'n', 1, '🌸'), P('Ente', 'Enten', 'f', 'n', 1, '🦆'),
    P('Nase', 'Nasen', 'f', 'n', 1, '👃'), P('Banane', 'Bananen', 'f', 'n', 1, '🍌'), P('Tomate', 'Tomaten', 'f', 'n', 1, '🍅'),
    P('Auto', 'Autos', 'n', 's', 1, '🚗'), P('Oma', 'Omas', 'f', 's', 1, '👵'), P('Opa', 'Opas', 'm', 's', 1, '👴'),
    P('Baby', 'Babys', 'n', 's', 1, '👶'),
    P('Frau', 'Frauen', 'f', 'en', 1, '👩'), P('Tür', 'Türen', 'f', 'en', 1, '🚪'), P('Bett', 'Betten', 'n', 'en', 1, '🛏️'),

    P('Ball', 'Bälle', 'm', 'umlaut_e', 2, '⚽'), P('Baum', 'Bäume', 'm', 'umlaut_e', 2, '🌳'), P('Maus', 'Mäuse', 'f', 'umlaut_e', 2, '🐭'),
    P('Kuh', 'Kühe', 'f', 'umlaut_e', 2, '🐮'), P('Hand', 'Hände', 'f', 'umlaut_e', 2, '✋'), P('Zahn', 'Zähne', 'm', 'umlaut_e', 2, '🦷'),
    P('Hut', 'Hüte', 'm', 'umlaut_e', 2, '🎩'),
    P('Kind', 'Kinder', 'n', 'er', 2, '🧒'), P('Ei', 'Eier', 'n', 'er', 2, '🥚'), P('Kleid', 'Kleider', 'n', 'er', 2, '👗'),
    P('Bild', 'Bilder', 'n', 'er', 2, '🖼️'),
    P('Buch', 'Bücher', 'n', 'umlaut_er', 2, '📖'), P('Haus', 'Häuser', 'n', 'umlaut_er', 2, '🏠'), P('Blatt', 'Blätter', 'n', 'umlaut_er', 2, '🍃'),
    P('Huhn', 'Hühner', 'n', 'umlaut_er', 2, '🐔'), P('Mann', 'Männer', 'm', 'umlaut_er', 2, '👨'),
    P('Apfel', 'Äpfel', 'm', 'umlaut', 2, '🍎'), P('Vogel', 'Vögel', 'm', 'umlaut', 2, '🐦'),
    P('Lehrer', 'Lehrer', 'm', 'gleich', 2), P('Tiger', 'Tiger', 'm', 'gleich', 2, '🐯'), P('Kuchen', 'Kuchen', 'm', 'gleich', 2, '🎂'),
    P('Schlüssel', 'Schlüssel', 'm', 'gleich', 2, '🔑'),
    P('Bär', 'Bären', 'm', 'en', 2, '🐻'), P('Zebra', 'Zebras', 'n', 's', 2, '🦓'),

    P('Mutter', 'Mütter', 'f', 'umlaut', 3), P('Vater', 'Väter', 'm', 'umlaut', 3), P('Bruder', 'Brüder', 'm', 'umlaut', 3),
    P('Garten', 'Gärten', 'm', 'umlaut', 3), P('Mantel', 'Mäntel', 'm', 'umlaut', 3, '🧥'), P('Hammer', 'Hämmer', 'm', 'umlaut', 3, '🔨'),
    P('Fuß', 'Füße', 'm', 'umlaut_e', 3, '🦶'), P('Frosch', 'Frösche', 'm', 'umlaut_e', 3, '🐸'), P('Fuchs', 'Füchse', 'm', 'umlaut_e', 3, '🦊'),
    P('Wolf', 'Wölfe', 'm', 'umlaut_e', 3, '🐺'), P('Stuhl', 'Stühle', 'm', 'umlaut_e', 3, '🪑'),
    P('Schloss', 'Schlösser', 'n', 'umlaut_er', 3, '🏰'), P('Fahrrad', 'Fahrräder', 'n', 'umlaut_er', 3, '🚲'),
    P('Einhorn', 'Einhörner', 'n', 'umlaut_er', 3, '🦄'), P('Wurm', 'Würmer', 'm', 'umlaut_er', 3, '🪱'),
    P('Gespenst', 'Gespenster', 'n', 'er', 3, '👻'), P('Lied', 'Lieder', 'n', 'er', 3, '🎵'),
    P('Mädchen', 'Mädchen', 'n', 'gleich', 3, '👧'), P('Eichhörnchen', 'Eichhörnchen', 'n', 'gleich', 3, '🐿️'),
    P('Fenster', 'Fenster', 'n', 'gleich', 3, '🪟'), P('Messer', 'Messer', 'n', 'gleich', 3, '🔪'),
    P('Elefant', 'Elefanten', 'm', 'en', 3, '🐘'), P('Papagei', 'Papageien', 'm', 'en', 3, '🦜'),
    P('Pinguin', 'Pinguine', 'm', 'e', 3, '🐧'), P('Krokodil', 'Krokodile', 'n', 'e', 3, '🐊'),
    P('Schmetterling', 'Schmetterlinge', 'm', 'e', 3, '🦋'), P('Geschenk', 'Geschenke', 'n', 'e', 3, '🎁'),
    P('Schildkröte', 'Schildkröten', 'f', 'n', 3, '🐢'), P('Ameise', 'Ameisen', 'f', 'n', 3, '🐜'), P('Giraffe', 'Giraffen', 'f', 'n', 3, '🦒'),
    P('Känguru', 'Kängurus', 'n', 's', 3, '🦘'), P('Kamera', 'Kameras', 'f', 's', 3, '📷'), P('Sofa', 'Sofas', 'n', 's', 3, '🛋️'),
  ];
  const REGLES_PLURIEL = {
    e: 'Viele Wörter bekommen am Ende ein -e: der Hund, die Hunde.',
    umlaut_e: 'Manche Wörter bekommen ein -e und einen Umlaut (a → ä, o → ö, u → ü, au → äu): der Ball, die Bälle.',
    en: 'Viele Wörter bekommen am Ende -en: die Frau, die Frauen.',
    n: 'Wörter mit -e am Ende bekommen meistens nur ein -n: die Blume, die Blumen.',
    er: 'Manche Wörter bekommen am Ende -er: das Kind, die Kinder.',
    umlaut_er: 'Manche Wörter bekommen -er und einen Umlaut: das Buch, die Bücher.',
    umlaut: 'Manche Wörter bekommen nur einen Umlaut: der Apfel, die Äpfel.',
    s: 'Wörter mit -a, -i, -o, -u oder -y am Ende bekommen oft ein -s: das Auto, die Autos.',
    gleich: 'Viele Wörter mit -er, -el, -en oder -chen am Ende bleiben gleich: der Lehrer, die Lehrer.',
  };

  // Articles : défini selon le genre (der / die / das), indéfini ein / eine (ein au neutre).
  const DEFINI = { m: 'der', f: 'die', n: 'das' };
  MOTS.forEach((m) => {
    m.art = m.genre === 'f' ? 'eine' : 'ein';
    m.def = DEFINI[m.genre];
  });
  PLURIELS.forEach((x) => {
    x.detS = DEFINI[x.g];
    x.detP = 'die';
  });

  window.ILE_LANGS = window.ILE_LANGS || {};
  window.ILE_LANGS.de = {
    code: 'de',
    nom: 'Deutsch',
    drapeau: '🇩🇪',
    tts: 'de-DE',
    dir: 'ltr',
    htmlLang: 'de',
    sepMots: ' ',
    ecriture: 'alphabet',

    niveaux: [
      { n: 1, nom: 'Schiffsjunge', classe: 'Anfänger', emoji: '🐣' },
      { n: 2, nom: 'Matrose', classe: 'Mittelstufe', emoji: '⚓' },
      { n: 3, nom: 'Kapitän', classe: 'Fortgeschritten', emoji: '🏴‍☠️' },
    ],

    // Textes de l'interface commune (accueil, en-tête, fenêtre de résultat). On tutoie l'enfant (du).
    ui: {
      titreSite: 'Die Wörterinsel',
      accroche: 'Entdecke die Insel, spiel mit Wörtern und füll deine Schatzkiste!',
      descriptionSite: 'Kostenlose Lernspiele für Kinder von 5 bis 10 Jahren: spielerisch Deutsch lernen.',
      choisisLangue: 'Sprache',
      choisisGrade: 'Wähle deinen Piratenrang',
      lesJeux: 'Die Spiele',
      etoilesTotal: (n, t) => n + ' / ' + t + ' Sterne',
      etoilesNiveau: (n) => n + ' von 3 Sternen auf dieser Stufe',
      surprise: '🎲 Überraschungsspiel',
      footer: 'Kostenlose Lernspiele für Kinder von 5 bis 10 Jahren · Dein Fortschritt wird auf diesem Gerät gespeichert.',
      effacer: '🧹 Meinen Fortschritt löschen',
      confirmerEffacer: 'Alle Sterne löschen, die du in dieser Sprache auf diesem Gerät gesammelt hast?',
      retourIle: 'Zurück zur Insel',
      niveau: 'Stufe',
      etoilesJeu: 'In diesem Spiel gesammelte Sterne',
      sonActiver: 'Ton einschalten',
      sonCouper: 'Ton ausschalten',
      question: (i, n) => 'Frage ' + i + ' von ' + n,
      ecouter: 'Anhören',
      bravo: ['Super!', 'Toll!', 'Klasse!', 'Prima!', 'Sehr gut!', 'Spitze!', 'Perfekt!'],
      encore: ['Fast!', 'Versuch es noch mal!', 'Nicht ganz …', 'Nur Mut!'],
      resultTitres: ['Übe weiter!', 'Ein guter Anfang!', 'Sehr gut!', 'Großartig!'],
      score: (s, t) => s + ' / ' + t + (s === 1 ? ' richtige Antwort' : ' richtige Antworten'),
      etoilesSur3: (n) => n + ' von 3 Sternen',
      record: '🏆 Neuer Rekord!',
      rejouer: '🔁 Noch mal spielen',
      jeuSuivant: 'Nächstes Spiel',
      ile: 'Zur Insel',
      parametres: 'Einstellungen',
      languesVisibles: 'Angezeigte Sprachen',
      languesVisiblesAide: 'Entferne das Häkchen bei einer Sprache, um sie für die Kinder auszublenden (zum Beispiel Französisch, damit nur in Fremdsprachen gespielt wird).',
      fermer: 'Schließen',
      jeuIndisponible: 'Dieses Spiel gibt es in dieser Sprache nicht. Wähle ein anderes Spiel:',
      jeuIndisponibleTitre: 'Nicht auf Deutsch verfügbar', // titre d'un jeu qui n'existe pas dans cette langue
    },

    // Nom, lieu de l'île, compétence et description de chaque jeu.
    jeux: {
      images: { titre: 'Wort und Bild', lieu: 'Der Strand', competence: 'Lesen', desc: 'Finde das Wort, das zum Bild passt.' },
      genre: { titre: 'Der, die oder das?', lieu: 'Der Steg', competence: 'Grammatik', desc: 'Wähle den richtigen Artikel für jedes Wort.' },
      lettre: { titre: 'Der verlorene Buchstabe', lieu: 'Die Höhle', competence: 'Rechtschreibung', desc: 'Finde den Buchstaben, der im Wort fehlt.' },
      melange: { titre: 'Buchstabensalat', lieu: 'Die Schatzkiste', competence: 'Rechtschreibung', desc: 'Bring die Buchstaben in die richtige Reihenfolge.' },
      syllabes: { titre: 'Die Silbenbrücke', lieu: 'Die Hängebrücke', competence: 'Lesen', desc: 'Setz die Silben zu einem Wort zusammen.' },
      memory: { titre: 'Wörter-Memo', lieu: 'Das Dorf', competence: 'Gedächtnis', desc: 'Dreh die Karten um und finde Bild und Wort, die zusammengehören.' },
      pendu: { titre: 'Die Kokosnüsse', lieu: 'Die Palme', competence: 'Rechtschreibung', desc: 'Errate das Wort Buchstabe für Buchstabe, bevor die Kokosnüsse herunterfallen.' },
      dictee: { titre: 'Das Papageien-Diktat', lieu: 'Der Dschungel', competence: 'Rechtschreibung', desc: 'Hör dem Papagei zu und schreib das Wort.' },
      'mots-caches': { titre: 'Versteckte Wörter', lieu: 'Die Dünen', competence: 'Lesen', desc: 'Finde die Wörter, die im Buchstabengitter versteckt sind.' },
      rimes: { titre: 'Die Reimjagd', lieu: 'Der Wasserfall', competence: 'Hören', desc: 'Finde das Wort, das sich reimt.' },
      contraires: { titre: 'Gegenteile', lieu: 'Der Leuchtturm', competence: 'Wortschatz', desc: 'Finde zu jedem Wort das Gegenteil.' },
      pluriel: { titre: 'Eins oder viele', lieu: 'Der Markt', competence: 'Grammatik', desc: 'Schreib die Wörter in der Mehrzahl.' },
      phrase: { titre: 'Der Satzsalat', lieu: 'Die Flaschenpost', competence: 'Grammatik', desc: 'Bring die Wörter des Satzes in die richtige Reihenfolge.' },
      alphabet: { titre: 'Das ABC', lieu: 'Die Bibliothek des Kapitäns', competence: 'Lesen', desc: 'Ordne die Wörter nach dem ABC.' },
    },

    // Écriture de la langue : 26 lettres + ä, ö, ü (Umlaute) et ß, lettres à part entière
    // (ä ne se confond pas avec a : Apfel / Äpfel), donc aucune famille d'accents.
    alphabet: 'abcdefghijklmnopqrstuvwxyz'.split(''),
    voyelles: ['a', 'e', 'i', 'o', 'u', 'ä', 'ö', 'ü'],
    familles: {},
    touchesSpeciales: ['ä', 'ö', 'ü', 'ß', 'Ä', 'Ö', 'Ü'],
    lettresClavier: ['ä', 'ö', 'ü', 'ß'],
    ligatures: {},

    // Jeu « Der, die oder das? » : l'article défini à tous les niveaux.
    articles: {
      1: { champ: 'def', choix: ['der', 'die', 'das'] },
      2: { champ: 'def', choix: ['der', 'die', 'das'] },
      3: { champ: 'def', choix: ['der', 'die', 'das'] },
    },

    MOTS, MOTS_SIMPLES, RIMES, CONTRAIRES, PHRASES, PLURIELS, REGLES_PLURIEL,
  };
})();
