/*
 * L'Île aux mots — pack de langue : ANGLAIS BRITANNIQUE (Word Island).
 *
 * Même structure et mêmes clés que data/lang/fr.js (pack de référence).
 * Orthographe et vocabulaire britanniques : colour, neighbour, grey, torch, lorry, biscuit…
 *
 * MOTS : { mot, emoji, genre (null : pas de genre grammatical), syl (syllabes écrites,
 *          découpage des dictionnaires : rab·bit, el·e·phant ; syl.join('') === mot),
 *          niveau (1 = 5–6 ans, 2 = 6–7 ans, 3 = 8–10 ans, selon la difficulté de lecture EN ANGLAIS),
 *          theme (identifiant commun à toutes les langues : animaux, nourriture, nature, objets, corps),
 *          art ('a' | 'an' selon le SON initial), def ('the'), pluriel (seulement s'il n'est pas « + s ») }
 * Seulement des noms comptables (on peut dire « a … ») : pas de milk, bread, rain…
 */
(function () {
  const M = (mot, emoji, syl, niveau, theme, extra) =>
    Object.assign({ mot, emoji, genre: null, syl, niveau, theme }, extra || {});

  const MOTS = [
    // --- Animals ---
    M('cat', '🐱', ['cat'], 1, 'animaux'),
    M('dog', '🐶', ['dog'], 1, 'animaux'),
    M('pig', '🐷', ['pig'], 1, 'animaux'),
    M('hen', '🐔', ['hen'], 1, 'animaux'),
    M('fox', '🦊', ['fox'], 1, 'animaux', { pluriel: 'foxes' }),
    M('bee', '🐝', ['bee'], 1, 'animaux'),
    M('cow', '🐮', ['cow'], 1, 'animaux'),
    M('duck', '🦆', ['duck'], 1, 'animaux'),
    M('fish', '🐟', ['fish'], 1, 'animaux', { pluriel: 'fish' }),
    M('frog', '🐸', ['frog'], 1, 'animaux'),
    M('ant', '🐜', ['ant'], 1, 'animaux'),
    M('owl', '🦉', ['owl'], 1, 'animaux'),
    M('panda', '🐼', ['pan', 'da'], 1, 'animaux'),
    M('rabbit', '🐰', ['rab', 'bit'], 2, 'animaux'),
    M('monkey', '🐒', ['mon', 'key'], 2, 'animaux'),
    M('tiger', '🐯', ['ti', 'ger'], 2, 'animaux'),
    M('horse', '🐴', ['horse'], 2, 'animaux'),
    M('sheep', '🐑', ['sheep'], 2, 'animaux', { pluriel: 'sheep' }),
    M('mouse', '🐭', ['mouse'], 2, 'animaux', { pluriel: 'mice' }),
    M('snake', '🐍', ['snake'], 2, 'animaux'),
    M('whale', '🐳', ['whale'], 2, 'animaux'),
    M('eagle', '🦅', ['ea', 'gle'], 2, 'animaux'),
    M('otter', '🦦', ['ot', 'ter'], 2, 'animaux'),
    M('bear', '🐻', ['bear'], 2, 'animaux'),
    M('kangaroo', '🦘', ['kan', 'ga', 'roo'], 2, 'animaux'),
    M('elephant', '🐘', ['el', 'e', 'phant'], 3, 'animaux'),
    M('octopus', '🐙', ['oc', 'to', 'pus'], 3, 'animaux', { pluriel: 'octopuses' }),
    M('butterfly', '🦋', ['but', 'ter', 'fly'], 3, 'animaux', { pluriel: 'butterflies' }),
    M('crocodile', '🐊', ['croc', 'o', 'dile'], 3, 'animaux'),
    M('dolphin', '🐬', ['dol', 'phin'], 3, 'animaux'),
    M('giraffe', '🦒', ['gi', 'raffe'], 3, 'animaux'),
    M('penguin', '🐧', ['pen', 'guin'], 3, 'animaux'),
    M('hedgehog', '🦔', ['hedge', 'hog'], 3, 'animaux'),
    M('caterpillar', '🐛', ['cat', 'er', 'pil', 'lar'], 3, 'animaux'),
    M('squirrel', '🐿️', ['squir', 'rel'], 3, 'animaux'),
    M('dinosaur', '🦕', ['di', 'no', 'saur'], 3, 'animaux'),
    M('unicorn', '🦄', ['u', 'ni', 'corn'], 3, 'animaux'),
    M('ladybird', '🐞', ['la', 'dy', 'bird'], 3, 'animaux'),
    M('oyster', '🦪', ['oys', 'ter'], 3, 'animaux'),

    // --- Food ---
    M('egg', '🥚', ['egg'], 1, 'nourriture'),
    M('pumpkin', '🎃', ['pump', 'kin'], 1, 'nourriture'),
    M('apple', '🍎', ['ap', 'ple'], 2, 'nourriture'),
    M('orange', '🍊', ['or', 'ange'], 2, 'nourriture'),
    M('banana', '🍌', ['ba', 'na', 'na'], 2, 'nourriture'),
    M('cherry', '🍒', ['cher', 'ry'], 2, 'nourriture', { pluriel: 'cherries' }),
    M('pear', '🍐', ['pear'], 2, 'nourriture'),
    M('cake', '🎂', ['cake'], 2, 'nourriture'),
    M('pizza', '🍕', ['piz', 'za'], 2, 'nourriture'),
    M('tomato', '🍅', ['to', 'ma', 'to'], 2, 'nourriture', { pluriel: 'tomatoes' }),
    M('potato', '🥔', ['po', 'ta', 'to'], 2, 'nourriture', { pluriel: 'potatoes' }),
    M('strawberry', '🍓', ['straw', 'ber', 'ry'], 3, 'nourriture', { pluriel: 'strawberries' }),
    M('pineapple', '🍍', ['pine', 'ap', 'ple'], 3, 'nourriture'),
    M('watermelon', '🍉', ['wa', 'ter', 'mel', 'on'], 3, 'nourriture'),
    M('avocado', '🥑', ['av', 'o', 'ca', 'do'], 3, 'nourriture'),
    M('sandwich', '🥪', ['sand', 'wich'], 3, 'nourriture', { pluriel: 'sandwiches' }),
    M('biscuit', '🍪', ['bis', 'cuit'], 3, 'nourriture'),
    M('onion', '🧅', ['on', 'ion'], 3, 'nourriture'),
    M('doughnut', '🍩', ['dough', 'nut'], 3, 'nourriture'),

    // --- Nature ---
    M('sun', '☀️', ['sun'], 1, 'nature'),
    M('moon', '🌙', ['moon'], 1, 'nature'),
    M('star', '⭐', ['star'], 1, 'nature'),
    M('tree', '🌳', ['tree'], 1, 'nature'),
    M('shell', '🐚', ['shell'], 1, 'nature'),
    M('rainbow', '🌈', ['rain', 'bow'], 1, 'nature'),
    M('cloud', '☁️', ['cloud'], 2, 'nature'),
    M('fire', '🔥', ['fire'], 2, 'nature'),
    M('wave', '🌊', ['wave'], 2, 'nature'),
    M('leaf', '🍃', ['leaf'], 2, 'nature', { pluriel: 'leaves' }),
    M('flower', '🌸', ['flow', 'er'], 2, 'nature'),
    M('volcano', '🌋', ['vol', 'ca', 'no'], 3, 'nature', { pluriel: 'volcanoes' }),
    M('island', '🏝️', ['is', 'land'], 3, 'nature'),
    M('mountain', '⛰️', ['moun', 'tain'], 3, 'nature'),

    // --- Things ---
    M('bus', '🚌', ['bus'], 1, 'objets', { pluriel: 'buses' }),
    M('car', '🚗', ['car'], 1, 'objets'),
    M('box', '📦', ['box'], 1, 'objets', { pluriel: 'boxes' }),
    M('hat', '🎩', ['hat'], 1, 'objets'),
    M('key', '🔑', ['key'], 1, 'objets'),
    M('robot', '🤖', ['ro', 'bot'], 1, 'objets'),
    M('teddy', '🧸', ['ted', 'dy'], 1, 'objets', { pluriel: 'teddies' }),
    M('bucket', '🪣', ['buck', 'et'], 1, 'objets'),
    M('rocket', '🚀', ['rock', 'et'], 1, 'objets'),
    M('laptop', '💻', ['lap', 'top'], 1, 'objets'),
    M('football', '⚽', ['foot', 'ball'], 1, 'objets'),
    M('kite', '🪁', ['kite'], 2, 'objets'),
    M('boat', '⛵', ['boat'], 2, 'objets'),
    M('train', '🚂', ['train'], 2, 'objets'),
    M('plane', '✈️', ['plane'], 2, 'objets'),
    M('house', '🏠', ['house'], 2, 'objets'),
    M('axe', '🪓', ['axe'], 2, 'objets'),
    M('umbrella', '☂️', ['um', 'brel', 'la'], 3, 'objets'),
    M('envelope', '✉️', ['en', 've', 'lope'], 3, 'objets'),
    M('bicycle', '🚲', ['bi', 'cy', 'cle'], 3, 'objets'),
    M('ambulance', '🚑', ['am', 'bu', 'lance'], 3, 'objets'),
    M('helicopter', '🚁', ['hel', 'i', 'cop', 'ter'], 3, 'objets'),
    M('guitar', '🎸', ['gui', 'tar'], 3, 'objets'),
    M('anchor', '⚓', ['an', 'chor'], 3, 'objets'),
    M('castle', '🏰', ['cas', 'tle'], 3, 'objets'),
    M('hourglass', '⌛', ['hour', 'glass'], 3, 'objets', { pluriel: 'hourglasses' }),
    M('uniform', '🥋', ['u', 'ni', 'form'], 3, 'objets'),

    // --- Body ---
    M('arm', '💪', ['arm'], 1, 'corps'),
    M('hand', '✋', ['hand'], 1, 'corps'),
    M('ear', '👂', ['ear'], 1, 'corps'),
    M('foot', '🦶', ['foot'], 1, 'corps', { pluriel: 'feet' }),
    M('nose', '👃', ['nose'], 2, 'corps'),
    M('mouth', '👄', ['mouth'], 2, 'corps'),
    M('tooth', '🦷', ['tooth'], 2, 'corps', { pluriel: 'teeth' }),
    M('eye', '👁️', ['eye'], 2, 'corps'),
  ];

  // Familles de rimes : même SON final en anglais britannique (pas seulement même orthographe).
  // « son » est une graphie repère du son. Les mots sans emoji s'affichent en texte.
  const RIMES = [
    { son: 'at', mots: ['cat', 'hat', 'bat', 'rat', 'mat', 'flat'] },
    { son: 'og', mots: ['dog', 'frog', 'log', 'fog', 'jog'] },
    { son: 'ee', mots: ['bee', 'tree', 'key', 'sea', 'three', 'knee'] },
    { son: 'ight', mots: ['kite', 'night', 'white', 'light', 'bite', 'bright'] },
    { son: 'ake', mots: ['cake', 'snake', 'lake', 'rake', 'shake'] },
    { son: 'un', mots: ['sun', 'bun', 'run', 'fun', 'done'] },
    { son: 'ing', mots: ['ring', 'king', 'wing', 'swing', 'sing', 'spring'] },
    { son: 'ail', mots: ['whale', 'snail', 'tail', 'nail', 'sail'] },
    { son: 'oat', mots: ['boat', 'goat', 'coat', 'float', 'note'] },
    { son: 'ar', mots: ['star', 'car', 'jar', 'far', 'guitar'] },
    { son: 'ed', mots: ['bed', 'red', 'bread', 'head', 'shed'] },
    { son: 'ell', mots: ['bell', 'shell', 'well', 'smell', 'spell'] },
    { son: 'ain', mots: ['train', 'plane', 'rain', 'brain', 'chain', 'crane'] },
    { son: 'oon', mots: ['moon', 'spoon', 'balloon', 'noon', 'cartoon'] },
    { son: 'ock', mots: ['clock', 'sock', 'rock', 'lock', 'block'] },
    { son: 'air', mots: ['bear', 'pear', 'chair', 'hair', 'fair', 'square'] },
    { son: 'ear', mots: ['ear', 'deer', 'year', 'near', 'cheer'] },
    { son: 'ig', mots: ['pig', 'wig', 'dig', 'big', 'fig', 'twig'] },
    { son: 'en', mots: ['hen', 'pen', 'ten', 'men', 'den'] },
    { son: 'all', mots: ['ball', 'wall', 'tall', 'small', 'fall'] },
    { son: 'ose', mots: ['nose', 'rose', 'hose', 'toes', 'froze'] },
    { son: 'ile', mots: ['crocodile', 'smile', 'tile', 'mile', 'pile'] },
    { son: 'ay', mots: ['day', 'play', 'hay', 'tray', 'grey'] },
  ];

  // Paires de contraires : [a, b, niveau, nature ('adj' | 'verbe' | 'nom' | 'adv')].
  const CONTRAIRES = [
    ['big', 'small', 1, 'adj'], ['hot', 'cold', 1, 'adj'], ['day', 'night', 1, 'nom'], ['up', 'down', 1, 'adv'],
    ['full', 'empty', 1, 'adj'], ['happy', 'sad', 1, 'adj'], ['clean', 'dirty', 1, 'adj'], ['long', 'short', 1, 'adj'],
    ['open', 'shut', 1, 'adj'], ['wet', 'dry', 1, 'adj'], ['in', 'out', 1, 'adv'], ['laugh', 'cry', 1, 'verbe'],
    ['push', 'pull', 1, 'verbe'],
    ['fast', 'slow', 2, 'adj'], ['heavy', 'light', 2, 'adj'], ['hard', 'soft', 2, 'adj'], ['young', 'old', 2, 'adj'],
    ['inside', 'outside', 2, 'adv'], ['before', 'after', 2, 'adv'], ['win', 'lose', 2, 'verbe'], ['strong', 'weak', 2, 'adj'],
    ['loud', 'quiet', 2, 'adj'], ['easy', 'difficult', 2, 'adj'], ['always', 'never', 2, 'adv'], ['give', 'take', 2, 'verbe'],
    ['sink', 'float', 2, 'verbe'], ['question', 'answer', 2, 'nom'], ['early', 'late', 2, 'adv'],
    ['brave', 'cowardly', 3, 'adj'], ['generous', 'mean', 3, 'adj'], ['noisy', 'silent', 3, 'adj'], ['ancient', 'modern', 3, 'adj'],
    ['rare', 'common', 3, 'adj'], ['accept', 'refuse', 3, 'verbe'], ['build', 'destroy', 3, 'verbe'], ['polite', 'rude', 3, 'adj'],
    ['possible', 'impossible', 3, 'adj'], ['honest', 'dishonest', 3, 'adj'], ['appear', 'disappear', 3, 'verbe'], ['remember', 'forget', 3, 'verbe'],
    ['shallow', 'deep', 3, 'adj'], ['smooth', 'rough', 3, 'adj'], ['friend', 'enemy', 3, 'nom'], ['entrance', 'exit', 3, 'nom'],
    ['forwards', 'backwards', 3, 'adv'], ['above', 'below', 3, 'adv'],
  ].map(([a, b, niveau, nature]) => ({ a, b, niveau, nature }));

  // Phrases à remettre dans l'ordre (la ponctuation reste collée au dernier mot).
  const PHRASES = [
    ['The cat is asleep.', 1], ['Dad reads a book.', 1], ['The hen eats some seeds.', 1],
    ['The boat is on the sea.', 1], ['I can see an owl.', 1], ['The sun is hot.', 1],
    ['The dog digs in the mud.', 1], ['Sam kicks the red ball.', 1], ['The moon is round.', 1],
    ['The little dog runs very fast.', 2], ['The children swim in the sea.', 2],
    ['My sister is drawing a house.', 2], ['The pirate hides his treasure.', 2],
    ['It is raining on the mountain.', 2], ['The monkey climbs up the palm tree.', 2],
    ['We are going to the beach tomorrow.', 2], ['The old tortoise walks very slowly.', 2],
    ['Our neighbour has a grey kitten.', 2],
    ['The dolphin leaps over the waves.', 3],
    ['Yesterday, we explored a mysterious island.', 3],
    ['The parrot on the ship repeats every word it hears.', 3],
    ['The sailors are raising the huge sails.', 3],
    ['During the storm, the lighthouse guides the boats.', 3],
    ['The sleeping volcano towers over the whole valley.', 3],
    ['My grandmother is baking a delicious chocolate cake.', 3],
    ['Our class visited the museum by the harbour.', 3],
    ['After colouring, please put the pencils back in the box.', 3],
  ].map(([texte, niveau]) => ({ texte, niveau }));

  // Mots supplémentaires sans image (pour les dictées, mots cachés, alphabet…).
  const MOTS_SIMPLES = {
    1: ['mum', 'dad', 'red', 'sit', 'hop', 'jam', 'mud', 'top', 'yes', 'zip', 'lid', 'net', 'tap', 'bag', 'pot'],
    2: ['garden', 'school', 'beach', 'table', 'chair', 'door', 'window', 'friend', 'water', 'music', 'sister', 'brother', 'hello', 'please', 'morning'],
    3: ['captain', 'adventure', 'compass', 'storm', 'pirate', 'sailor', 'voyage', 'library', 'birthday', 'mysterious', 'explorer', 'treasure', 'crew', 'telescope', 'horizon'],
  };

  // Pluriels : { s (singulier), p (pluriel), g (null : pas de genre), regle (clé de REGLES_PLURIEL), niveau, emoji? }
  // Niveau 1 : + s. Niveau 2 : -es, -ies, -ys et irréguliers très courants. Niveau 3 : tout.
  const P = (s, p, regle, niveau, emoji) => ({ s, p, g: null, regle, niveau, emoji: emoji || null });
  const PLURIELS = [
    P('cat', 'cats', 's', 1, '🐱'), P('dog', 'dogs', 's', 1, '🐶'), P('pig', 'pigs', 's', 1, '🐷'),
    P('hen', 'hens', 's', 1, '🐔'), P('frog', 'frogs', 's', 1, '🐸'), P('duck', 'ducks', 's', 1, '🦆'),
    P('bee', 'bees', 's', 1, '🐝'), P('cow', 'cows', 's', 1, '🐮'), P('ant', 'ants', 's', 1, '🐜'),
    P('egg', 'eggs', 's', 1, '🥚'), P('owl', 'owls', 's', 1, '🦉'), P('hat', 'hats', 's', 1, '🎩'),
    P('bed', 'beds', 's', 1, '🛏️'), P('book', 'books', 's', 1, '📖'), P('ring', 'rings', 's', 1, '💍'),
    P('sock', 'socks', 's', 1, '🧦'), P('drum', 'drums', 's', 1, '🥁'), P('bell', 'bells', 's', 1, '🔔'),
    P('star', 'stars', 's', 1, '⭐'), P('tree', 'trees', 's', 1, '🌳'), P('car', 'cars', 's', 1, '🚗'),

    P('box', 'boxes', 'es', 2, '📦'), P('fox', 'foxes', 'es', 2, '🦊'), P('bus', 'buses', 'es', 2, '🚌'),
    P('watch', 'watches', 'es', 2, '⌚'), P('dress', 'dresses', 'es', 2, '👗'), P('brush', 'brushes', 'es', 2, '🖌️'),
    P('torch', 'torches', 'es', 2, '🔦'), P('dish', 'dishes', 'es', 2, '🍽️'),
    P('baby', 'babies', 'ies', 2, '👶'), P('fly', 'flies', 'ies', 2, '🪰'), P('cherry', 'cherries', 'ies', 2, '🍒'),
    P('lolly', 'lollies', 'ies', 2, '🍭'), P('party', 'parties', 'ies', 2, '🎉'), P('teddy', 'teddies', 'ies', 2, '🧸'),
    P('boy', 'boys', 'ys', 2, '👦'), P('key', 'keys', 'ys', 2, '🔑'), P('toy', 'toys', 'ys', 2, '🪀'),
    P('monkey', 'monkeys', 'ys', 2, '🐒'), P('turkey', 'turkeys', 'ys', 2, '🦃'),
    P('child', 'children', 'irr', 2, '🧒'), P('man', 'men', 'irr', 2, '👨'), P('woman', 'women', 'irr', 2, '👩'),
    P('mouse', 'mice', 'irr', 2, '🐭'), P('foot', 'feet', 'irr', 2, '🦶'), P('tooth', 'teeth', 'irr', 2, '🦷'),

    P('leaf', 'leaves', 'ves', 3, '🍃'), P('wolf', 'wolves', 'ves', 3, '🐺'), P('knife', 'knives', 'ves', 3),
    P('loaf', 'loaves', 'ves', 3, '🍞'), P('shelf', 'shelves', 'ves', 3), P('half', 'halves', 'ves', 3),
    P('elf', 'elves', 'ves', 3, '🧝'), P('scarf', 'scarves', 'ves', 3, '🧣'),
    P('tomato', 'tomatoes', 'oes', 3, '🍅'), P('potato', 'potatoes', 'oes', 3, '🥔'), P('hero', 'heroes', 'oes', 3, '🦸'),
    P('echo', 'echoes', 'oes', 3),
    P('piano', 'pianos', 'os', 3, '🎹'), P('photo', 'photos', 'os', 3, '🖼️'), P('kangaroo', 'kangaroos', 'os', 3, '🦘'),
    P('radio', 'radios', 'os', 3, '📻'),
    P('sheep', 'sheep', 'same', 3, '🐑'), P('fish', 'fish', 'same', 3, '🐟'), P('deer', 'deer', 'same', 3, '🦌'),
    P('goose', 'geese', 'irr', 3), P('person', 'people', 'irr', 3, '🧑'), P('ox', 'oxen', 'irr', 3, '🐂'),
    P('sandwich', 'sandwiches', 'es', 3, '🥪'), P('church', 'churches', 'es', 3, '⛪'), P('princess', 'princesses', 'es', 3, '👸'),
    P('compass', 'compasses', 'es', 3, '🧭'),
    P('strawberry', 'strawberries', 'ies', 3, '🍓'), P('butterfly', 'butterflies', 'ies', 3, '🦋'), P('lorry', 'lorries', 'ies', 3, '🚚'),
    P('city', 'cities', 'ies', 3, '🏙️'),
    P('holiday', 'holidays', 'ys', 3, '🏖️'), P('trolley', 'trolleys', 'ys', 3, '🛒'), P('chimney', 'chimneys', 'ys', 3),
    P('donkey', 'donkeys', 'ys', 3),
    P('giraffe', 'giraffes', 's', 3, '🦒'), P('umbrella', 'umbrellas', 's', 3, '☂️'), P('elephant', 'elephants', 's', 3, '🐘'),
  ];
  const REGLES_PLURIEL = {
    s: 'Most words just add -s: a cat, some cats.',
    es: 'Words ending in -s, -x, -ch, -sh or -z add -es: a box, some boxes.',
    ies: 'If a consonant comes before the -y, change the -y to -ies: a baby, some babies.',
    ys: 'If a vowel comes before the -y, just add -s: a boy, some boys.',
    ves: 'Many words ending in -f or -fe change to -ves: a leaf, some leaves.',
    oes: 'Some words ending in -o add -es: a tomato, some tomatoes.',
    os: 'Other words ending in -o just add -s: a piano, some pianos.',
    irr: 'Some words change in a special way: a child, some children.',
    same: 'Some words stay the same: a sheep, some sheep.',
  };

  // Articles : « an » devant un SON de voyelle, « a » devant un son de consonne.
  // On regarde la première lettre, sauf pour les mots dont le son ne suit pas l'écriture.
  const SON_CONSONNE = ['unicorn', 'uniform', 'unicycle', 'ukulele', 'european', 'one']; // u- dit « you », o- dit « w »
  const SON_VOYELLE = ['hour', 'hourglass', 'honest', 'honour', 'heir']; // h muet
  function indefini(mot) {
    if (SON_CONSONNE.indexOf(mot) !== -1) return 'a';
    if (SON_VOYELLE.indexOf(mot) !== -1) return 'an';
    return /^[aeiou]/i.test(mot) ? 'an' : 'a';
  }
  MOTS.forEach((m) => {
    m.art = indefini(m.mot);
    m.def = 'the';
  });
  PLURIELS.forEach((x) => {
    x.detS = indefini(x.s);
    x.detP = 'some';
  });

  window.ILE_LANGS = window.ILE_LANGS || {};
  window.ILE_LANGS.en = {
    code: 'en',
    nom: 'English',
    drapeau: '🇬🇧',
    tts: 'en-GB',
    dir: 'ltr',
    sepMots: ' ',
    ecriture: 'alphabet',

    niveaux: [
      { n: 1, nom: 'Deckhand', classe: 'Beginner', emoji: '🐣' },
      { n: 2, nom: 'Sailor', classe: 'Intermediate', emoji: '⚓' },
      { n: 3, nom: 'Captain', classe: 'Advanced', emoji: '🏴‍☠️' },
    ],

    // Textes de l'interface commune (accueil, en-tête, fenêtre de résultat).
    ui: {
      titreSite: 'Word Island',
      accroche: 'Explore the island, play with words and fill your treasure chest!',
      descriptionSite: 'Free educational games to help children aged 5 to 10 learn to read and write.',
      choisisLangue: 'Language',
      choisisGrade: 'Choose your pirate rank',
      lesJeux: 'Games',
      etoilesTotal: (n, t) => n + ' / ' + t + ' stars',
      etoilesNiveau: (n) => n + ' star' + (n === 1 ? '' : 's') + ' out of 3 at this level',
      surprise: '🎲 Surprise game',
      footer: 'Free educational games for children aged 5 to 10 · your progress is saved on this device.',
      effacer: '🧹 Clear my progress',
      confirmerEffacer: 'Clear all the stars you have earned in this language on this device?',
      retourIle: 'Back to the island',
      niveau: 'Level',
      etoilesJeu: 'Stars earned in this game',
      sonActiver: 'Turn the sound on',
      sonCouper: 'Turn the sound off',
      question: (i, n) => 'Question ' + i + ' of ' + n,
      ecouter: 'Listen',
      bravo: ['Well done!', 'Brilliant!', 'Great job!', 'Excellent!', 'Fantastic!', 'Super!', 'Perfect!'],
      encore: ['Nearly!', 'Try again!', 'Not quite…', 'Keep going!'],
      resultTitres: ['Keep practising!', 'That’s a good start!', 'Very good!', 'Amazing!'],
      score: (s, t) => s + ' / ' + t + ' correct answer' + (s === 1 ? '' : 's'),
      etoilesSur3: (n) => n + ' star' + (n === 1 ? '' : 's') + ' out of 3',
      record: '🏆 New record!',
      rejouer: '🔁 Play again',
      jeuSuivant: 'Next game',
      ile: 'The island',
      parametres: 'Settings',
      languesVisibles: 'Languages offered',
      languesVisiblesAide: 'Untick a language to hide it from children (for example French, to play only in foreign languages).',
      fermer: 'Close',
      jeuIndisponible: 'This game isn\u2019t available in this language. Choose another game:',
    },

    // Nom, lieu de l'île, compétence et description de chaque jeu.
    jeux: {
      images: { titre: 'Word and picture', lieu: 'The beach', competence: 'Reading', desc: 'Find the word that matches the picture.' },
      genre: { titre: 'A or an?', lieu: 'The jetty', competence: 'Grammar', desc: 'Choose “a” or “an” to go in front of each word.' },
      lettre: { titre: 'The lost letter', lieu: 'The cave', competence: 'Spelling', desc: 'Find the letter that is missing from the word.' },
      melange: { titre: 'Letter jumble', lieu: 'The treasure chest', competence: 'Spelling', desc: 'Put the letters back in the right order.' },
      syllabes: { titre: 'Syllable bridge', lieu: 'The rope bridge', competence: 'Reading', desc: 'Join the syllables together to build the word.' },
      memory: { titre: 'Word memory', lieu: 'The village', competence: 'Memory', desc: 'Turn over the cards and match each picture with its word.' },
      pendu: { titre: 'Coconut drop', lieu: 'The palm tree', competence: 'Spelling', desc: 'Guess the word letter by letter before the coconuts fall.' },
      dictee: { titre: 'Parrot dictation', lieu: 'The jungle', competence: 'Spelling', desc: 'Listen to the parrot and write the word.' },
      'mots-caches': { titre: 'Word search', lieu: 'The dunes', competence: 'Reading', desc: 'Find the words hidden in the grid.' },
      rimes: { titre: 'Rhyme hunt', lieu: 'The waterfall', competence: 'Phonics', desc: 'Find the word that rhymes.' },
      contraires: { titre: 'Opposites', lieu: 'The lighthouse', competence: 'Vocabulary', desc: 'Match each word with its opposite.' },
      pluriel: { titre: 'One or many', lieu: 'The market', competence: 'Grammar', desc: 'Write the plural of each word.' },
      phrase: { titre: 'Scrambled sentence', lieu: 'The message in a bottle', competence: 'Grammar', desc: 'Put the words of the sentence back in order.' },
      alphabet: { titre: 'Alphabetical order', lieu: 'The captain’s library', competence: 'Reading', desc: 'Put the words in alphabetical order.' },
    },

    // Écriture de la langue : 26 lettres, aucun accent ni ligature.
    alphabet: 'abcdefghijklmnopqrstuvwxyz'.split(''),
    voyelles: ['a', 'e', 'i', 'o', 'u'],
    familles: {},
    touchesSpeciales: [],
    lettresClavier: [],
    ligatures: {},

    // Jeu « A or an? » : le même choix à tous les niveaux (les pièges de son arrivent au niveau 3).
    articles: {
      1: { champ: 'art', choix: ['a', 'an'] },
      2: { champ: 'art', choix: ['a', 'an'] },
      3: { champ: 'art', choix: ['a', 'an'] },
    },

    MOTS, MOTS_SIMPLES, RIMES, CONTRAIRES, PHRASES, PLURIELS, REGLES_PLURIEL,
  };
})();
