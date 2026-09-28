/* Les noix de coco : un pendu bienveillant. Chaque lettre absente du mot fait tomber
 * une noix de coco du palmier ; quand il n'y en a plus, on découvre le mot et on continue.
 * Niveau 1 : 8 noix, l'image est montrée (et le mot peut être écouté).
 * Niveau 2 : 7 noix, l'image est cachée derrière un bouton « Indice ».
 * Niveau 3 : 6 noix, mots longs sans image ; l'indice révèle la première lettre.
 * Clavier : l'alphabet de la langue + ses lettres propres (ä ö ü ß en allemand, ä é ë en
 * luxembourgeois, ü pour le pinyin), et le clavier physique. Une touche révèle toutes les lettres
 * dont Ile.plier(lettre) est cette touche (E → é è ê ë en français ; ä reste une lettre à part en allemand).
 * La casse est ignorée pour deviner, mais le mot s'affiche avec sa vraie casse (Katze, Kaz).
 * Chinois : on devine le PINYIN sans tons (Ile.epeler). Chaque caractère est écrit au-dessus de sa
 * syllabe (熊 / xiong, 猫 / mao) ; à la fin, la syllabe avec son ton apparaît dessous (xióng māo).
 * Le pinyin n'utilise jamais v : la touche V disparaît et v tape ü, comme sur un clavier chinois.
 * Après j, q, x, y, le son ü s'écrit u (yú, jú zi) : la touche ü y rappelle la règle sans faire tomber de noix.
 * Les mots écrits à l'écran (titre, grades, consignes) ne sont jamais à deviner.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'pendu';
  const TOTAL = 5;
  const NOIX = { 1: 8, 2: 7, 3: 6 };
  const LONGUEUR_MIN = 3; // un mot de 2 lettres (Ei, yu) n'est pas un vrai pendu
  const LONGUEUR_MIN_3 = 6; // niveau 3 : mots d'au moins 6 lettres
  const DELAI_GAGNE = 2000;
  const DELAI_PERDU = 3400;
  const TOUCHE_MIN = 44; // largeur minimale d'une touche (px)
  const SVG = 'http://www.w3.org/2000/svg';

  const listeEt = (et) => (items) => (items.length < 2 ? items.join('')
    : items.slice(0, -1).join(', ') + et + items[items.length - 1]);

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      q: (s) => '“' + s + '”',
      liste: listeEt(' and '),
      consigne1: 'Guess the word for the picture!',
      consigne: 'Guess the word, letter by letter!',
      progres: (i, n) => 'Word ' + i + ' of ' + n,
      progresAria: 'Words found',
      reste: (n) => (n > 1 ? n + ' coconuts left' : n === 1 ? 'One coconut left' : 'No coconuts left'),
      lettres: (n) => (n === 1 ? 'letter' : 'letters'),
      indice: '💡 Hint',
      indiceImage: 'Hint: show the picture',
      indiceLettre: 'Hint: show a letter',
      image: 'Picture of the word to guess',
      imageCachee: 'Hidden picture',
      ecouterMot: 'Listen to the word',
      clavier: 'Keyboard',
      touche: (l) => 'Letter ' + l,
      toucheBonne: (l) => l + ', in the word',
      toucheMauvaise: (l) => l + ', not in the word',
      lecteur: (n, lettres) => 'A ' + n + '-letter word: ' + lettres + '.',
      blanc: 'blank',
      oui: (q, n) => 'Yes! ' + q + ' is in the word' + (n === 1 ? '' : n === 2 ? ' twice' : ' ' + n + ' times') + '.',
      ouiPlusieurs: (liste) => 'Yes! ' + liste + ' are in the word.',
      non: (q, reste) => 'There’s no ' + q + ' in this word. '
        + (reste === 1 ? 'Careful, only one coconut left!' : reste + ' coconuts left.'),
      regardeImage: 'Look carefully at the picture!',
      uJqxy: 'After j, q, x and y, ü loses its dots: we write u!',
      commence: (q) => 'The word begins with ' + q + '.',
      uneDePlus: (q) => 'Here’s one more letter: ' + q + '.',
      gagne: (q) => 'Well done! You found ' + q + '!',
      gagneGroupe: (g) => 'Well done! It’s ' + g + '.',
      perdu: (q) => 'All the coconuts have fallen. The word was ' + q + '.',
      etait: (mot) => 'The word was ' + mot + '.',
      finTout: (t) => 'You found all ' + t + ' words!',
      finPartiel: (s, t, revoir) => (s === 0 ? 'No words found this time.' : 'You found ' + s + ' of the ' + t + ' words.')
        + ' Words to practise: ' + revoir + '.',
    },
    zh: {
      q: (s) => '“' + s + '”',
      liste: (items) => (items.length < 2 ? items.join('')
        : items.slice(0, -1).join('、') + '和' + items[items.length - 1]),
      consigne1: '看图和汉字，猜出拼音字母！',
      consigne: '看汉字，一个一个地猜出拼音字母！',
      progres: (i, n) => '第 ' + i + ' 个词，共 ' + n + ' 个',
      progresAria: '找到的词',
      reste: (n) => (n > 0 ? '还剩 ' + n + ' 个椰子' : '椰子都掉下来了'),
      lettres: () => '个字母',
      indice: '💡 提示',
      indiceImage: '提示：看图片',
      indiceLettre: '提示：显示一个字母',
      image: '要猜的词的图片',
      imageCachee: '藏起来的图片',
      ecouterMot: '听一听这个词',
      clavier: '键盘',
      touche: (l) => '字母 ' + l,
      toucheBonne: (l) => l + '，在拼音里',
      toucheMauvaise: (l) => l + '，不在拼音里',
      lecteur: (n, lettres, mot) => mot + '：拼音有 ' + n + ' 个字母：' + lettres + '。',
      blanc: '空',
      oui: (q, n) => '对了！拼音里有' + (n === 1 ? '' : (['', '一', '两', '三', '四', '五'][n] || n) + '个') + q + '！',
      ouiPlusieurs: (liste) => '对了！拼音里有' + liste + '！',
      non: (q, reste) => '拼音里没有' + q + '。' + (reste === 1 ? '小心，只剩一个椰子了！' : '还剩 ' + reste + ' 个椰子。'),
      regardeImage: '仔细看看图片！',
      uJqxy: 'j、q、x、y 后面的 ü 要去掉两点，写成 u！',
      commence: (q) => '拼音的第一个字母是' + q + '。',
      uneDePlus: (q) => '再给你一个字母：' + q + '。',
      gagne: (q) => '太棒了！你拼出了' + q + '的拼音！',
      gagneGroupe: (g) => '太棒了！这是' + g + '。',
      perdu: (q, py) => '椰子都掉下来了。' + q + '的拼音是 ' + py + '。',
      etait: (mot) => '这个词是' + mot + '。',
      finTout: (t) => '你找到了全部 ' + t + ' 个词！',
      finPartiel: (s, t, revoir) => (s === 0 ? '这次一个词也没有找到。' : t + ' 个词里，你找到了 ' + s + ' 个。')
        + '再练一练：' + revoir + '。',
    },
    de: {
      q: (s) => '„' + s + '“',
      liste: listeEt(' und '),
      consigne1: 'Errate das Wort zum Bild!',
      consigne: 'Errate das Wort, Buchstabe für Buchstabe!',
      progres: (i, n) => 'Wort ' + i + ' von ' + n,
      progresAria: 'Gefundene Wörter',
      reste: (n) => (n > 1 ? 'Noch ' + n + ' Kokosnüsse' : n === 1 ? 'Noch eine Kokosnuss' : 'Keine Kokosnuss mehr'),
      lettres: (n) => (n === 1 ? 'Buchstabe' : 'Buchstaben'),
      indice: '💡 Tipp',
      indiceImage: 'Tipp: Bild zeigen',
      indiceLettre: 'Tipp: einen Buchstaben zeigen',
      image: 'Bild zum gesuchten Wort',
      imageCachee: 'Verstecktes Bild',
      ecouterMot: 'Wort anhören',
      clavier: 'Tastatur',
      touche: (l) => 'Buchstabe ' + l,
      toucheBonne: (l) => l + ', im Wort',
      toucheMauvaise: (l) => l + ', nicht im Wort',
      lecteur: (n, lettres) => 'Ein Wort mit ' + n + ' Buchstaben: ' + lettres + '.',
      blanc: 'leer',
      oui: (q, n) => 'Ja! ' + q + ' ist im Wort' + (n === 2 ? ', sogar zweimal' : n > 2 ? ', sogar ' + n + '-mal' : '') + '.',
      ouiPlusieurs: (liste) => 'Ja! ' + liste + ' sind im Wort.',
      non: (q, reste) => 'Kein ' + q + ' in diesem Wort. '
        + (reste === 1 ? 'Achtung, nur noch eine Kokosnuss!' : 'Noch ' + reste + ' Kokosnüsse.'),
      regardeImage: 'Schau dir das Bild genau an!',
      uJqxy: 'Nach j, q, x und y verliert das ü seine Punkte: Man schreibt u!',
      commence: (q) => 'Das Wort fängt mit ' + q + ' an.',
      uneDePlus: (q) => 'Hier ist noch ein Buchstabe: ' + q + '.',
      gagne: (q) => 'Super! Du hast ' + q + ' gefunden!',
      gagneGroupe: (g) => 'Super! Das ist ' + g + '.',
      perdu: (q) => 'Alle Kokosnüsse sind heruntergefallen. Das Wort war ' + q + '.',
      etait: (mot) => 'Das Wort war ' + mot + '.',
      finTout: (t) => 'Du hast alle ' + t + ' Wörter gefunden!',
      finPartiel: (s, t, revoir) => (s === 0 ? 'Diesmal hast du kein Wort gefunden.' : 'Du hast ' + s + ' von ' + t + ' Wörtern gefunden.')
        + ' Zum Üben: ' + revoir + '.',
    },
    lb: {
      q: (s) => '„' + s + '“',
      liste: listeEt(' an '),
      consigne1: 'Fann d’Wuert vum Bild!',
      consigne: 'Fann d’Wuert, Buschtaf fir Buschtaf!',
      progres: (i, n) => 'Wuert ' + i + ' / ' + n,
      progresAria: 'Fonnt Wierder',
      reste: (n) => (n > 1 ? 'Nach ' + n + ' Kokosnëss' : n === 1 ? 'Nach eng Kokosnoss' : 'Keng Kokosnoss méi'),
      lettres: (n) => (n === 1 ? 'Buschtaf' : 'Buschtawen'),
      indice: '💡 Tipp',
      indiceImage: 'Tipp: d’Bild weisen',
      indiceLettre: 'Tipp: e Buschtaf weisen',
      image: 'Bild vum Wuert',
      imageCachee: 'Verstoppt Bild',
      ecouterMot: 'D’Wuert lauschteren',
      clavier: 'Tastatur',
      touche: (l) => 'Buschtaf ' + l,
      toucheBonne: (l) => l + ', am Wuert',
      toucheMauvaise: (l) => l + ', net am Wuert',
      lecteur: (n, lettres) => 'E Wuert mat ' + n + ' Buschtawen: ' + lettres + '.',
      blanc: 'eidel',
      oui: (q) => 'Jo! ' + q + ' ass am Wuert.',
      ouiPlusieurs: (liste) => 'Jo! ' + liste + ' sinn am Wuert.',
      non: (q, reste) => q + ' ass net an dësem Wuert. '
        + (reste === 1 ? 'Opgepasst, nëmmen nach eng Kokosnoss!' : 'Nach ' + reste + ' Kokosnëss.'),
      regardeImage: 'Kuck d’Bild gutt un!',
      uJqxy: 'No j, q, x an y schreift een ü ouni Punkten, also u!',
      commence: (q) => 'D’Wuert fänkt mat ' + q + ' un.',
      uneDePlus: (q) => 'Hei ass nach e Buschtaf: ' + q + '.',
      gagne: (q) => 'Super! Du hues ' + q + ' fonnt!',
      gagneGroupe: (g) => 'Super! Dat ass ' + g + '.',
      perdu: (q) => 'All d’Kokosnëss sinn erofgefall. D’Wuert war ' + q + '.',
      etait: (mot) => 'D’Wuert war ' + mot + '.',
      finTout: (t) => 'Du hues all ' + t + ' Wierder fonnt!',
      // Règle de l'n : « vu 5 » (fënnef), « vun 3 » (dräi).
      finPartiel: (s, t, revoir) => (s === 0 ? 'Dës Kéier hues du kee Wuert fonnt.'
        : 'Du hues ' + s + ' ' + ([4, 5, 6, 7].indexOf(t) !== -1 ? 'vu' : 'vun') + ' ' + t + ' Wierder fonnt.')
        + ' Fir ze iwwen: ' + revoir + '.',
    },
    fr: {
      q: (s) => '« ' + s + ' »',
      liste: listeEt(' et '),
      consigne1: 'Trouve les lettres du mot de l’image !',
      consigne: 'Trouve le mot, lettre par lettre !',
      progres: (i, n) => 'Mot ' + i + ' / ' + n,
      progresAria: 'Mots trouvés',
      reste: (n) => (n > 1 ? 'Il reste ' + n + ' noix de coco' : n === 1 ? 'Il reste une noix de coco' : 'Plus de noix de coco'),
      lettres: (n) => (n > 1 ? 'lettres' : 'lettre'),
      indice: '💡 Indice',
      indiceImage: 'Indice : montrer l’image',
      indiceLettre: 'Indice : montrer une lettre',
      image: 'Image du mot à trouver',
      imageCachee: 'Image cachée',
      ecouterMot: 'Écouter le mot',
      clavier: 'Clavier',
      touche: (l) => 'Lettre ' + l,
      toucheBonne: (l) => l + ', dans le mot',
      toucheMauvaise: (l) => l + ', pas dans le mot',
      lecteur: (n, lettres) => 'Mot de ' + n + ' lettres : ' + lettres + '.',
      blanc: 'blanc',
      oui: (q, n) => 'Oui ! Il y a ' + (n === 1 ? 'un ' : n + ' ') + q + '.',
      ouiPlusieurs: (liste) => 'Oui ! Il y a ' + liste + '.',
      non: (q, reste) => 'Pas de ' + q + ' dans ce mot. '
        + (reste === 1 ? 'Attention, plus qu’une noix !' : 'Il reste ' + reste + ' noix.'),
      regardeImage: 'Regarde bien l’image !',
      uJqxy: 'Après j, q, x et y, le ü perd ses deux points : on écrit u !',
      commence: (q) => 'Le mot commence par ' + q + '.',
      uneDePlus: (q) => 'Voici une lettre de plus : ' + q + '.',
      gagne: (q) => 'Bravo ! Tu as trouvé ' + q + ' !',
      gagneGroupe: (g) => 'Bravo ! C’est ' + g + '.',
      perdu: (q) => 'Toutes les noix sont tombées. Le mot était ' + q + '.',
      etait: (mot) => 'Le mot était ' + mot + '.',
      finTout: (t) => 'Tu as trouvé les ' + t + ' mots !',
      finPartiel: (s, t, revoir) => (s === 0 ? 'Aucun mot trouvé cette fois-ci.' : 'Tu as trouvé ' + s + (s > 1 ? ' mots' : ' mot') + ' sur ' + t + '.')
        + ' À revoir : ' + revoir + '.',
    },
  };

  // Typographie française : espace fine insécable avant ! ? : ; et à l'intérieur des guillemets.
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, ' $1').replace(/« /g, '« ');
  }

  const hanzi = () => Ile.L().ecriture === 'hanzi';
  // Pinyin sans tons, collé, en minuscules (ü conservé) : même règle que le pack chinois.
  const sansTon = (p) => String(p || '').normalize('NFD').replace(/[̀-̇̉-ͯ]/g, '')
    .normalize('NFC').replace(/[\s'’-]+/g, '').toLowerCase();
  // Touche(s) du clavier qui révèlent un caractère du mot : é → e, œ → o + e, K → k.
  const touchesDe = (c) => Array.from(Ile.plier(c));
  const estLettre = (c) => /\p{L}/u.test(c);

  // Un mot de la partie : ce qu'on affiche, ce qu'on devine, l'image, le pinyin et le groupe « article + mot ».
  function entree(m) {
    const zh = hanzi();
    const pinyin = zh ? (m.pinyin || Ile.pinyin(m.mot)) : '';
    const epel = String(zh ? (m.epeler || sansTon(pinyin)) : m.mot).normalize('NFC');
    // Syllabes (chinois) : un caractère au-dessus de chaque syllabe, si le pinyin s'y prête.
    let syllabes = null;
    if (zh && pinyin) {
      const py = pinyin.split(/\s+/);
      const car = Array.from(m.mot);
      if (py.length === car.length && py.map(sansTon).join('') === epel) {
        syllabes = car.map((c, k) => ({ han: c, py: py[k], n: Array.from(sansTon(py[k])).length }));
      }
    }
    return {
      mot: m.mot, epel, emoji: m.emoji || null, pinyin, syllabes,
      groupe: m.art && !zh ? Ile.groupe(Ile.un(m), m.mot) : null,
    };
  }

  function jouable(e, min) {
    const l = Array.from(e.epel);
    return /^\p{L}+$/u.test(e.epel) && l.length >= min && new Set(l.map((c) => c.toLowerCase())).size > 1;
  }

  // Mots écrits à l'écran pendant la partie (titre, grades du sélecteur de niveau, consignes,
  // messages) : on ne les fait pas deviner, la réponse serait sous les yeux de l'enfant
  // (« Kapitän » dans le sélecteur de niveau, « Kokosnëss » dans le titre pour « Kokosnoss »).
  // Chinois : les caractères sont montrés exprès et on devine le pinyin, qui n'est jamais écrit.
  const normal = (s) => String(s).replace(/ß/g, 'ss').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
  function motsEcran(tx) {
    if (hanzi()) return [];
    const g = Ile.game(ID);
    const textes = [g ? g.titre : '', tx.consigne1, tx.consigne, tx.indice, tx.lettres(1), tx.lettres(2),
      tx.reste(1), tx.reste(2), tx.non('', 1), tx.non('', 2), tx.regardeImage]
      .concat(Ile.niveaux().map((n) => n.nom + ' ' + n.classe));
    const mots = new Set();
    textes.forEach((t) => String(t).split(/[^\p{L}]+/u).forEach((w) => { if (w) mots.add(normal(w)); }));
    return Array.from(mots);
  }
  // Au plus une lettre de différence (remplacée, ajoutée ou enlevée).
  function uneLettre(a, b) {
    if (Math.abs(a.length - b.length) > 1) return false;
    let i = 0;
    while (i < a.length && a[i] === b[i]) i++;
    return a.slice(i + (a.length >= b.length ? 1 : 0)) === b.slice(i + (b.length >= a.length ? 1 : 0));
  }
  // Même mot ou même famille : Kokosnoss / Kokosnëss, Schiff / Schiffsjunge, Buch / Buchstabe.
  function proche(a, b) {
    const n = Math.min(a.length, b.length);
    return a === b || (n >= 4 && (a.startsWith(b) || b.startsWith(a))) || (n >= 6 && uneLettre(a, b));
  }
  const afficheAEcran = (ecran) => (e) => {
    const k = normal(e.epel);
    return ecran.some((w) => proche(k, w));
  };

  function motsDuNiveau(level, ecran) {
    const cache = afficheAEcran(ecran);
    if (level !== 3) {
      return Ile.motsPourPartie(level, TOTAL, (m) => { const e = entree(m); return jouable(e, LONGUEUR_MIN) && !cache(e); }).map(entree);
    }
    // Niveau 3 : mots longs, illustrés ou non (l'image n'est jamais montrée).
    const P = Ile.L();
    const vus = new Set();
    const tous = P.MOTS.filter((m) => m.niveau === 3)
      .concat(((P.MOTS_SIMPLES && P.MOTS_SIMPLES[3]) || []).map((mot) => ({ mot })))
      .map(entree)
      .filter((e) => {
        const k = e.epel.toLowerCase();
        if (!jouable(e, LONGUEUR_MIN_3) || vus.has(k) || cache(e)) return false;
        vus.add(k);
        return true;
      });
    return Ile.shuffle(tous).slice(0, TOTAL);
  }

  // Touches du clavier à l'écran : alphabet + lettres propres à la langue (sans v en pinyin).
  function lettresDuClavier() {
    const P = Ile.L();
    let t = P.alphabet.concat(P.lettresClavier || []);
    if (P.ecriture === 'hanzi') t = t.filter((l) => l !== 'v');
    return Array.from(new Set(t));
  }

  // Clavier en lignes équilibrées : le plus de touches par ligne possible (au moins 44 px chacune),
  // au moins deux lignes, et des lignes de longueurs voisines (7 + 7 + 7 + 5, jamais 9 + 9 + 9 + 3).
  function disposerClavier(clavier) {
    if (!clavier || !clavier.isConnected) return;
    const n = clavier.children.length;
    const w = clavier.clientWidth;
    if (!n || !w) return;
    const gap = parseFloat(window.getComputedStyle(clavier).columnGap) || 6;
    const maxCols = Math.max(1, Math.floor((w + gap) / (TOUCHE_MIN + gap)));
    const lignes = Math.max(2, Math.ceil(n / maxCols));
    clavier.style.setProperty('--cols', String(Math.ceil(n / lignes)));
  }

  // ---------------------------------------------------------------------------
  // Le palmier (SVG, repère 220 × 190). Les formes sont dessinées autour de la couronne (122, 72)
  // puis agrandies (K) et recentrées (CX, CY) pour bien remplir la vignette.
  // ---------------------------------------------------------------------------
  const K = 1.15;
  const CX = 110;
  const CY = 62;
  const R = 8; // rayon d'une noix
  const pt = (x, y) => [CX + (x - 122) * K, CY + (y - 72) * K];
  const trace = (d) => d.replace(/(-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?)/g, (_, x, y) => pt(+x, +y).map((v) => Math.round(v * 10) / 10).join(' '));
  const FEUILLES = [
    'M122 72 C104 56 80 56 56 76 C80 66 102 68 122 76 Z',
    'M122 72 C108 46 88 36 68 38 C90 46 106 58 120 74 Z',
    'M122 72 C142 54 168 56 192 78 C166 66 144 68 122 76 Z',
    'M122 72 C136 44 158 34 180 38 C156 46 140 58 124 74 Z',
    'M122 72 C118 50 124 32 138 22 C130 40 128 56 124 72 Z',
    'M122 74 C104 74 90 86 84 104 C96 90 110 82 122 78 Z',
    'M122 74 C140 74 154 86 160 104 C148 90 136 82 122 78 Z',
  ].map(trace);
  const TRONC = trace('M96 186 C98 146 104 108 122 76');
  // La grappe (les dernières noix de la liste tombent en premier).
  const GRAPPE = [[109, 81], [122, 84], [135, 81], [103, 94], [116, 97], [129, 97], [142, 93], [122, 110]].map(([x, y]) => pt(x, y));
  // Hauteur du sable en x (même courbe que le chemin « pendu-sable »).
  function sable(x) {
    const q = (a, b, c, t) => (1 - t) * (1 - t) * a + 2 * t * (1 - t) * b + t * t * c;
    return x <= 110 ? q(160, 142, 156, x / 110) : q(156, 170, 152, (x - 110) / 110);
  }
  const SOL = [150, 176, 54, 30, 201, 124, 10, 104].map((x) => [x, sable(x) - R / 2]);

  function svg(tag, attrs, parent) {
    const n = document.createElementNS(SVG, tag);
    Object.keys(attrs || {}).forEach((k) => n.setAttribute(k, attrs[k]));
    if (parent) parent.appendChild(n);
    return n;
  }

  function dessinerPalmier(nb) {
    const s = svg('svg', { viewBox: '0 0 220 190', 'aria-hidden': 'true', focusable: 'false' });
    svg('circle', { cx: 28, cy: 28, r: 13, class: 'pendu-soleil' }, s);
    const nuage = svg('g', { class: 'pendu-nuage' }, s);
    [[181, 22, 7], [192, 17, 10], [204, 22, 7]].forEach(([cx, cy, r]) => svg('circle', { cx, cy, r }, nuage));
    svg('rect', { x: 175, y: 20, width: 36, height: 9, rx: 4.5 }, nuage);
    svg('path', { class: 'pendu-mer', d: 'M0 130 Q13.75 124 27.5 130 T55 130 T82.5 130 T110 130 T137.5 130 T165 130 T192.5 130 T220 130 V190 H0 Z' }, s);
    svg('path', { class: 'pendu-tronc', d: TRONC }, s);
    svg('path', { class: 'pendu-tronc-anneaux', d: TRONC }, s);
    svg('path', { class: 'pendu-sable', d: 'M0 190 V160 Q55 142 110 156 Q165 170 220 152 V190 Z' }, s);
    const couronne = svg('g', { class: 'pendu-couronne' }, s);
    FEUILLES.forEach((d, k) => svg('path', { d, class: 'pendu-feuille' + (k % 2 ? '' : ' pendu-feuille--fonce') }, couronne));
    const noix = GRAPPE.slice(0, nb).map(([cx, cy]) => {
      const n = svg('g', { class: 'pendu-noix' }, s);
      svg('circle', { cx, cy, r: R, class: 'pendu-noix__coque' }, n);
      svg('circle', { cx: cx - 2.8, cy: cy - 2.8, r: 2.5, class: 'pendu-noix__reflet' }, n);
      svg('circle', { cx: cx + 1.7, cy: cy + 2.8, r: 1.2, class: 'pendu-noix__oeil' }, n);
      svg('circle', { cx: cx + 4, cy: cy + 0.4, r: 1.2, class: 'pendu-noix__oeil' }, n);
      return { n, cx, cy };
    });
    return { svg: s, noix };
  }

  // Écouteurs globaux de la partie en cours (retirés à chaque nouvelle partie).
  let ecouteurs = null;

  // ---------------------------------------------------------------------------
  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      if (ecouteurs) ecouteurs.retirer();
      const zh = hanzi();
      const alphabet = lettresDuClavier();
      const surClavier = new Set(alphabet);
      const nbNoix = NOIX[level] || 8;
      const mots = motsDuNiveau(level, motsEcran(tx));
      let i = 0;
      let score = 0;
      const aRevoir = [];
      let manche = null; // le mot en cours
      let auClavier = false; // l'enfant joue avec Tab + Entrée : on garde le focus dans le clavier

      const panel = el('section', { class: 'panel pendu' + (zh ? ' pendu--zh' : ''), 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      const actif = () => panel.isConnected;

      // fait = mots terminés (remplissage de la barre) ; courant = mot affiché.
      function majProgress(fait, courant) {
        const bar = Ile.progress(panel, fait, mots.length, score);
        bar.setAttribute('aria-label', tx.progresAria);
        bar.querySelector('.progress__label').textContent =
          tx.progres(Math.min(courant + 1, mots.length), mots.length) + '  ·  ✅ ' + score;
      }

      // Clavier physique (casse ignorée ; É → e en français ; v → ü en pinyin).
      function onKey(e) {
        if (!actif()) { retirer(); return; }
        if (!manche || e.ctrlKey || e.metaKey || e.altKey || e.isComposing || !e.key || Array.from(e.key).length !== 1) return;
        const t = e.target;
        if (t && (/^(SELECT|INPUT|TEXTAREA)$/.test(t.tagName) || t.isContentEditable)) return;
        if (document.querySelector('.result')) return;
        let l = Ile.plier(e.key);
        if (zh && l === 'v') l = 'ü';
        if (!surClavier.has(l)) return;
        e.preventDefault();
        if (e.repeat) return;
        manche.essayer(l);
      }
      function onResize() {
        if (!actif()) { retirer(); return; }
        disposerClavier(panel.querySelector('.pendu-clavier'));
      }
      function retirer() {
        document.removeEventListener('keydown', onKey);
        window.removeEventListener('resize', onResize);
      }
      document.addEventListener('keydown', onKey);
      window.addEventListener('resize', onResize);
      ecouteurs = { retirer };

      function suivant() {
        if (!actif()) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= mots.length) { fin(); return; }
        manche = nouvelleManche(mots[i]);
      }

      function fin() {
        manche = null;
        majProgress(mots.length, mots.length - 1);
        const total = mots.length;
        const message = score === total ? tx.finTout(total) : tx.finPartiel(score, total, tx.liste(aRevoir));
        Ile.showResult({ id: ID, score, total, message: typo(message) });
      }

      function nouvelleManche(m) {
        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        majProgress(i, i);

        const lettres = Array.from(m.epel);
        // Une lettre se joue si toutes ses touches sont sur le clavier ; le reste est montré d'emblée.
        const jouable = lettres.map((c) => estLettre(c) && touchesDe(c).every((b) => surClavier.has(b)));
        const nbLettres = lettres.filter(estLettre).length;
        const essais = new Set(); // touches déjà proposées
        const indices = new Set(); // positions montrées par l'indice (niveau 3)
        let erreurs = 0;
        let fini = false;
        const nomAffiche = zh ? tx.q(m.mot) : tx.q(m.epel);

        // --- Palmier, image ou longueur, indice ---
        const palmier = dessinerPalmier(nbNoix);
        const scene = el('div', { class: 'pendu-scene' }, [palmier.svg]);
        const compteurVu = el('span', { 'aria-hidden': 'true' });
        const compteurLu = el('span', { class: 'sr-only' });
        const compteur = el('span', { class: 'pendu-compteur' }, [compteurVu, compteurLu]);
        function majCompteur() {
          const reste = nbNoix - erreurs;
          compteurVu.textContent = '🥥 × ' + reste;
          compteurLu.textContent = tx.reste(reste);
          compteur.classList.toggle('is-alerte', reste <= 2);
        }
        majCompteur();

        const info = el('div', { class: 'pendu-info' });
        let image = null;
        let boutonIndice = null;
        if (level === 1 && m.emoji) {
          image = el('div', { class: 'pendu-image', role: 'img', 'aria-label': tx.image, text: m.emoji });
          info.append(image, el('div', { class: 'pendu-ligne' }, [compteur, Ile.speakButton(() => m.mot, tx.ecouterMot)]));
        } else {
          if (level === 2 && m.emoji) {
            image = el('div', { class: 'pendu-image is-mystere', role: 'img', 'aria-label': tx.imageCachee, text: '?' });
            info.appendChild(image);
          } else {
            info.appendChild(el('p', { class: 'pendu-longueur' }, [el('strong', { text: String(nbLettres) }), ' ' + tx.lettres(nbLettres)]));
          }
          boutonIndice = el('button', {
            type: 'button',
            class: 'btn btn--sun pendu-indice',
            text: tx.indice,
            'aria-label': image ? tx.indiceImage : tx.indiceLettre,
          });
          boutonIndice.addEventListener('click', indice);
          info.append(boutonIndice, compteur);
        }

        // --- Le mot : une case par lettre, affichée d'emblée ---
        // Chinois : un bloc par syllabe, le caractère au-dessus, la syllabe avec son ton dessous (à la fin).
        const motEl = el('div', { class: 'pendu-mot' + (m.syllabes ? ' pendu-mot--zh' : ''), 'aria-hidden': 'true' });
        motEl.style.setProperty('--n', String(lettres.length));
        const cases = [];
        const tons = [];
        const nouvelleCase = (k) => {
          const c = lettres[k];
          const kase = el('span', { class: 'pendu-case' + (jouable[k] ? '' : ' pendu-case--signe'), text: jouable[k] ? '' : c });
          cases.push(kase);
          return kase;
        };
        if (m.syllabes) {
          motEl.style.setProperty('--s', String(m.syllabes.length));
          motEl.lang = 'zh-Latn-pinyin';
          let k = 0;
          m.syllabes.forEach((s) => {
            const casesSyl = el('span', { class: 'pendu-syl__cases' });
            for (let j = 0; j < s.n; j++) casesSyl.appendChild(nouvelleCase(k++));
            const ton = el('span', { class: 'pendu-syl__py pinyin is-cache', text: s.py });
            tons.push(ton);
            motEl.appendChild(el('span', { class: 'pendu-syl' }, [
              el('span', { class: 'pendu-syl__han', lang: 'zh-Hans', text: s.han }),
              casesSyl,
              ton,
            ]));
          });
        } else {
          // Chinois au pinyin irrégulier (ne devrait pas arriver) : les caractères au-dessus du mot,
          // le pinyin avec ses tons dessous à la fin.
          if (zh) tons.push(el('span', { class: 'pendu-hanzi__py pinyin is-cache', lang: 'zh-Latn-pinyin', text: m.pinyin }));
          lettres.forEach((c, k) => motEl.appendChild(nouvelleCase(k)));
        }
        const lecteur = el('p', { class: 'sr-only' });
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
        const dire = (ok, texte) => Ile.feedback(fb, ok, typo(texte));

        // --- Le clavier de la langue ---
        const clavier = el('div', { class: 'pendu-clavier', role: 'group', 'aria-label': tx.clavier });
        if (zh) clavier.lang = 'zh-Latn-pinyin';
        const touches = {};
        alphabet.forEach((l) => {
          const b = el('button', { type: 'button', class: 'pendu-touche', 'data-lettre': l, 'aria-label': tx.touche(l), text: l });
          b.addEventListener('click', (e) => { auClavier = e.detail === 0; essayer(l); });
          touches[l] = b;
          clavier.appendChild(b);
        });

        const visible = (k) => !jouable[k] || indices.has(k) || touchesDe(lettres[k]).every((b) => essais.has(b));
        const complet = () => lettres.every((c, k) => visible(k));

        function afficher() {
          lettres.forEach((c, k) => {
            if (!jouable[k] || !visible(k) || cases[k].classList.contains('is-trouvee')) return;
            cases[k].textContent = c;
            cases[k].classList.add('is-trouvee');
          });
          lecteur.textContent = tx.lecteur(nbLettres, lettres.map((c, k) => (visible(k) ? c : tx.blanc)).join(', '), m.mot);
        }

        function essayer(l) {
          if (!actif() || fini || essais.has(l) || !touches[l]) return;
          essais.add(l);
          const t = touches[l];
          const avaitFocus = document.activeElement === t;
          t.disabled = true;
          const pos = [];
          lettres.forEach((c, k) => { if (jouable[k] && touchesDe(c).indexOf(l) !== -1) pos.push(k); });

          if (pos.length) {
            t.classList.add('is-bonne');
            t.setAttribute('aria-label', tx.toucheBonne(l));
            afficher();
            if (complet()) { gagne(); return; }
            Ile.sfx('good');
            // Lettres révélées, avec leur vraie casse (K dans Katze) et leurs accents (é, è).
            const vues = Array.from(new Set(pos.map((k) => lettres[k])));
            dire(true, vues.length === 1 ? tx.oui(tx.q(vues[0]), pos.length) : tx.ouiPlusieurs(tx.liste(vues.map(tx.q))));
          } else if (zh && l === 'ü' && /[jqxy]u/.test(m.epel)) {
            // Pinyin : on entend bien ü dans yú, jú zi, xuě huā, mais après j, q, x, y il s'écrit u.
            // L'enfant a bien entendu : aucune noix ne tombe, on lui apprend la règle.
            t.classList.add('is-mauvaise');
            t.setAttribute('aria-label', tx.toucheMauvaise(l));
            Ile.flash(t, 'bad');
            Ile.sfx('click');
            dire(false, tx.uJqxy);
          } else {
            t.classList.add('is-mauvaise');
            t.setAttribute('aria-label', tx.toucheMauvaise(l));
            erreurs++;
            const noix = palmier.noix[nbNoix - erreurs];
            if (noix) {
              const [x, y] = SOL[erreurs - 1];
              noix.n.style.setProperty('--dx', (x - noix.cx) + 'px');
              noix.n.style.setProperty('--dy', (y - noix.cy) + 'px');
              noix.n.classList.add('is-tombee');
            }
            majCompteur();
            Ile.flash(t, 'bad');
            Ile.sfx('bad');
            const reste = nbNoix - erreurs;
            if (reste <= 0) { perdu(); return; }
            dire(false, tx.non(tx.q(l), reste));
          }
          // Au clavier, la touche désactivée perd le focus : on le donne à la touche libre suivante.
          if (avaitFocus) {
            const libres = alphabet.filter((x) => !touches[x].disabled);
            const apres = libres.find((x) => alphabet.indexOf(x) > alphabet.indexOf(l)) || libres[0];
            if (apres) touches[apres].focus();
          }
        }

        function indice() {
          if (!actif() || fini || !boutonIndice || boutonIndice.disabled) return;
          boutonIndice.disabled = true;
          Ile.sfx('click');
          if (image) {
            montrerImage();
            dire(true, tx.regardeImage);
            return;
          }
          // Niveau 3 : la première lettre (ou, si elle est déjà trouvée, la première lettre encore cachée).
          const k = lettres.findIndex((c, j) => !visible(j));
          if (k === -1) return;
          indices.add(k);
          afficher();
          Ile.flash(cases[k], 'good');
          if (complet()) { gagne(); return; }
          dire(true, k === 0 ? tx.commence(tx.q(lettres[0])) : tx.uneDePlus(tx.q(lettres[k])));
        }

        function montrerImage() {
          if (!image || !m.emoji) return;
          image.classList.remove('is-mystere');
          image.textContent = m.emoji;
          image.setAttribute('aria-label', tx.image);
          Ile.flash(image, 'good');
        }

        function terminer(etat) {
          fini = true;
          motEl.dataset.etat = etat;
          clavier.classList.add('is-fini');
          if (boutonIndice) boutonIndice.disabled = true;
          tons.forEach((t) => t.classList.remove('is-cache')); // chinois : le pinyin avec ses tons
          majProgress(i + 1, i);
          i++;
        }

        function gagne() {
          score++;
          terminer('gagne');
          scene.classList.add('is-gagne');
          montrerImage();
          Ile.flash(motEl, 'good');
          Ile.sfx('good');
          dire(true, m.groupe ? tx.gagneGroupe(m.groupe) : tx.gagne(nomAffiche));
          Ile.say(m.groupe || m.mot);
          setTimeout(suivant, DELAI_GAGNE);
        }

        function perdu() {
          terminer('perdu');
          aRevoir.push(zh && m.pinyin ? m.mot + '（' + m.pinyin + '）' : m.mot);
          lettres.forEach((c, k) => {
            if (jouable[k] && !visible(k)) { cases[k].textContent = c; cases[k].classList.add('is-manquee'); }
          });
          lecteur.textContent = tx.etait(zh ? m.mot + ' ' + m.pinyin : m.mot);
          montrerImage();
          dire(false, tx.perdu(nomAffiche, m.pinyin));
          Ile.say(m.groupe || m.mot);
          setTimeout(suivant, DELAI_PERDU);
        }

        const hanziSeul = zh && !m.syllabes ? el('p', { class: 'pendu-hanzi', lang: 'zh-Hans' }, [m.mot, tons[0] || '']) : null;
        panel.append(...[
          el('p', { class: 'consigne', text: typo(level === 1 ? tx.consigne1 : tx.consigne) }),
          el('div', { class: 'pendu-haut' }, [scene, info]),
          hanziSeul,
          motEl,
          lecteur,
          fb,
          clavier,
        ].filter(Boolean));
        motEl.dataset.manche = String(i);
        afficher();
        disposerClavier(clavier);
        if (auClavier) touches[alphabet[0]].focus();
        return { essayer };
      }

      suivant();
    },
  });
})();
