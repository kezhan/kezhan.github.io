/*
 * Mots cachés / Word search / 藏起来的词 / Versteckte Wörter / Verstoppt Wierder :
 * retrouve dans la grille les mots de la liste (touche la première puis la dernière lettre, ou glisse).
 *
 * Langues à alphabet : grille de lettres majuscules (Ile.lettresGrille : sans accents en français,
 * ä ö ü en allemand, ä é ë en luxembourgeois, ß → SS sur deux cases), remplie avec l'alphabet de la
 * langue et ses lettres propres (lettresClavier). Niveau 1 : 6×6, 4 mots, ➡️ ⬇️ ; niveau 2 : 8×8,
 * 5 mots, + diagonale ↘️ ; niveau 3 : 10×10, 6 mots, 8 directions. Allemand et luxembourgeois :
 * la liste garde la majuscule des noms (Katze, Kaz).
 * Chinois : grille de CARACTÈRES, mots de 2 caractères ou plus, remplie avec des caractères d'autres
 * mots du pack. Niveau 1 : 5×5, 3 mots ➡️ ⬇️ ; niveau 2 : 6×6, 4 mots + ↘️ ; niveau 3 : 7×7, 5 mots,
 * 8 directions. La liste montre l'emoji et le pinyin (niveaux 1–2) ou l'emoji seul (niveau 3) :
 * l'enfant apprend à reconnaître les caractères ; un mot trouvé s'affiche en caractères + pinyin.
 * Un point par mot trouvé sans indice ; mot trouvé surligné, barré et prononcé (si une voix existe).
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'mots-caches';
  const NB = ' '; // espace insécable

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      consigne: 'Tap the first letter of a word, then its last letter (or slide your finger).',
      sens: {
        1: 'Words go ➡️ or ⬇️.',
        2: 'Words go ➡️, ⬇️ or diagonally ↘️.',
        3: 'Words hide in every direction, even backwards!',
      },
      horsLigne: {
        1: 'Oops! Pick two letters in the same row or the same column.',
        2: 'Oops! Pick two letters in the same row, column or diagonal ↘️.',
        3: 'Oops! Pick two letters in a straight line.',
      },
      note: (signe, a, b) => 'In the grid, letters have no ' +
        ({ accent: 'accents', trema: 'dots', cedille: 'cedillas' }[signe] || 'accents') +
        ': ' + a + ' is written ' + b + '.',
      noteLigature: (a, b) => 'In the grid, ' + a + ' is written as two letters: ' + b + '.',
      titreListe: '🧭 Words to find:',
      liste: 'Words to find',
      ecouterMot: (m) => 'Listen to the word “' + m + '”',
      ecouterMotTrouve: (m) => 'Listen to the word “' + m + '” (found)',
      indice: '💡 Hint',
      indiceLabel: 'Hint: make the first letter of a word flash',
      grille: (n) => 'Letter grid: ' + n + ' rows and ' + n + ' columns. Use the arrow keys to move around.',
      caseLabel: (l, r, c) => l + ', row ' + r + ', column ' + c,
      progression: 'Words found',
      compteurTitre: 'Words found:',
      premiere: (l) => 'First letter: ' + l + '. Now tap the last letter of the word.',
      dejaTrouve: 'You’ve already found that word. Look for the others!',
      presque: 'Nearly! Go all the way from the first letter of the word to the last.',
      pasDansListe: 'That isn’t a word on the list. Keep looking!',
      oui: 'Yes!',
      trouve: (m) => 'You found “' + m + '”.',
      tousTrouves: 'You’ve found all the words!',
      indice1: (m) => 'The word “' + m + '” starts with the flashing letter.',
      indice2: (m) => 'The word “' + m + '” starts and ends on the flashing letters.',
      indice3: (m) => 'Here’s the whole word “' + m + '”: tap its first letter, then its last.',
      finParfaite: 'Every word found without a hint: you’ve got eyes like a lookout!',
      finNormale: 'You get a point for each word found without a hint. Play again and find them all on your own!',
    },
    zh: {
      consigne: '先点一个词的第一个字，再点最后一个字，也可以用手指划过去。',
      sens: {
        1: '词语从左到右 ➡️ 或从上到下 ⬇️。',
        2: '词语可以 ➡️、⬇️，也可以斜着 ↘️。',
        3: '词语藏在各个方向，还可能倒着写！',
      },
      horsLigne: {
        1: '哎呀！请选同一行或同一列的两个字。',
        2: '哎呀！请选同一行、同一列或同一条斜线 ↘️ 上的两个字。',
        3: '哎呀！请选在一条直线上的两个字。',
      },
      note: (signe, a, b) => '方格里的' + a + '写成' + b + '。',
      noteLigature: (a, b) => '方格里的' + a + '写成' + b + '。',
      titreListe: '🧭 要找的词：',
      liste: '要找的词',
      ecouterMot: (m) => '听一听：' + m,
      ecouterMotTrouve: (m) => '听一听：' + m + '（已经找到）',
      indice: '💡 提示',
      indiceLabel: '提示：让一个词的第一个字闪一闪',
      grille: (n) => '汉字方格：' + n + ' 行 ' + n + ' 列。用方向键移动。',
      caseLabel: (l, r, c) => l + '，第 ' + r + ' 行，第 ' + c + ' 列',
      progression: '找到的词',
      compteurTitre: '找到了：',
      premiere: (l) => '第一个字：' + l + '。现在点这个词的最后一个字。',
      dejaTrouve: '这个词已经找到了，找找别的吧！',
      presque: '差一点！要从第一个字一直选到最后一个字。',
      pasDansListe: '这不是要找的词，再找找！',
      oui: '对了！',
      trouve: (m) => '你找到了“' + m + '”！',
      tousTrouves: '所有的词都找到了！',
      indice1: (m) => m + ' 的第一个字在闪。',
      indice2: (m) => m + ' 的第一个字和最后一个字在闪。',
      indice3: (m) => m + ' 的每个字都在闪：先点第一个字，再点最后一个字。',
      finParfaite: '没用提示就找到了所有的词，你的眼睛真厉害！',
      finNormale: '不用提示找到一个词，就得一分。再玩一次，自己把它们都找出来吧！',
    },
    de: {
      consigne: 'Tippe auf den ersten Buchstaben eines Wortes und dann auf den letzten (oder zieh mit dem Finger darüber).',
      sens: {
        1: 'Die Wörter stehen ➡️ oder ⬇️.',
        2: 'Die Wörter stehen ➡️, ⬇️ oder schräg ↘️.',
        3: 'Die Wörter verstecken sich in allen Richtungen, sogar rückwärts!',
      },
      horsLigne: {
        1: 'Hoppla! Wähle zwei Buchstaben in derselben Zeile oder Spalte.',
        2: 'Hoppla! Wähle zwei Buchstaben in derselben Zeile, Spalte oder Schräge ↘️.',
        3: 'Hoppla! Wähle zwei Buchstaben auf einer geraden Linie.',
      },
      note: (signe, a, b) => 'Im Gitter wird ' + a + ' als ' + b + ' geschrieben.',
      noteLigature: (a, b) => 'Im Gitter wird ' + a + ' als ' + b + ' geschrieben.',
      titreListe: '🧭 Diese Wörter sind versteckt:',
      liste: 'Gesuchte Wörter',
      ecouterMot: (m) => 'Das Wort „' + m + '“ anhören',
      ecouterMotTrouve: (m) => 'Das Wort „' + m + '“ anhören (gefunden)',
      indice: '💡 Tipp',
      indiceLabel: 'Tipp: Der erste Buchstabe eines Wortes blinkt',
      grille: (n) => 'Buchstabengitter mit ' + n + ' Zeilen und ' + n + ' Spalten. Mit den Pfeiltasten bewegst du dich.',
      caseLabel: (l, r, c) => l + ', Zeile ' + r + ', Spalte ' + c,
      progression: 'Gefundene Wörter',
      compteurTitre: 'Gefunden:',
      premiere: (l) => 'Erster Buchstabe: ' + l + '. Tippe jetzt auf den letzten Buchstaben des Wortes.',
      dejaTrouve: 'Dieses Wort hast du schon gefunden. Such die anderen!',
      presque: 'Fast! Geh vom ersten bis zum letzten Buchstaben des Wortes.',
      pasDansListe: 'Dieses Wort steht nicht auf der Liste. Such weiter!',
      oui: 'Ja!',
      trouve: (m) => 'Du hast „' + m + '“ gefunden.',
      tousTrouves: 'Du hast alle Wörter gefunden!',
      indice1: (m) => 'Das Wort „' + m + '“ beginnt beim blinkenden Buchstaben.',
      indice2: (m) => 'Das Wort „' + m + '“ beginnt und endet bei den blinkenden Buchstaben.',
      indice3: (m) => 'Hier ist das ganze Wort „' + m + '“: Tippe auf den ersten und dann auf den letzten Buchstaben.',
      finParfaite: 'Alle Wörter ohne Tipp gefunden: Du hast Adleraugen!',
      finNormale: 'Für jedes Wort ohne Tipp gibt es einen Punkt. Spiel noch mal und finde alle allein!',
    },
    lb: {
      consigne: 'Dréck op den éischte Buschtaf vun engem Wuert an dann op de leschten. Du kanns och mam Fanger driwwer fueren.',
      sens: {
        1: 'D’Wierder stinn ➡️ oder ⬇️.',
        2: 'D’Wierder stinn ➡️, ⬇️ oder diagonal ↘️.',
        3: 'D’Wierder si verstoppt: an all Richtungen, och hannerzeg!',
      },
      horsLigne: {
        1: 'Wiel zwee Buschtawen an der selwechter Zeil oder an der selwechter Kolonn.',
        2: 'Wiel zwee Buschtawen an der selwechter Zeil, Kolonn oder Diagonal ↘️.',
        3: 'Wiel zwee Buschtawen op enger riichter Linn.',
      },
      note: (signe, a, b) => 'Am Gitter gëtt ' + a + ' als ' + b + ' geschriwwen.',
      noteLigature: (a, b) => 'Am Gitter gëtt ' + a + ' als ' + b + ' geschriwwen.',
      titreListe: '🧭 Dës Wierder si verstoppt:',
      liste: 'Verstoppt Wierder',
      ecouterMot: (m) => 'D’Wuert „' + m + '“ lauschteren',
      ecouterMotTrouve: (m) => 'D’Wuert „' + m + '“ lauschteren (fonnt)',
      indice: '💡 Tipp',
      indiceLabel: 'Tipp: Den éischte Buschtaf vun engem Wuert blénkt',
      grille: (n) => 'Gitter mat ' + n + ' Zeilen an ' + n + ' Kolonnen. Benotz d’Feiltasten.',
      caseLabel: (l, r, c) => l + ', Zeil ' + r + ', Kolonn ' + c,
      progression: 'Fonnt Wierder',
      compteurTitre: 'Fonnt:',
      premiere: (l) => 'Éischte Buschtaf: ' + l + '. Dréck elo op de leschte Buschtaf vum Wuert.',
      dejaTrouve: 'Dëst Wuert hues du scho fonnt. Sich déi aner!',
      presque: 'Bal! Wiel vum éischte bis bei de leschte Buschtaf vum Wuert.',
      pasDansListe: 'Dat Wuert ass net op der Lëscht. Sich weider!',
      oui: 'Jo!',
      trouve: (m) => 'Du hues „' + m + '“ fonnt.',
      tousTrouves: 'Du hues all d’Wierder fonnt!',
      indice1: (m) => 'D’Wuert „' + m + '“ fänkt beim Buschtaf un, dee blénkt.',
      indice2: (m) => 'D’Wuert „' + m + '“ fänkt un an hält op bei de Buschtawen, déi blénken.',
      indice3: (m) => 'Hei ass dat ganzt Wuert „' + m + '“: Dréck op den éischten an dann op de leschte Buschtaf.',
      finParfaite: 'All d’Wierder ouni Tipp fonnt. Bravo!',
      finNormale: 'Fir all Wuert ouni Tipp gëtt et ee Punkt. Spill nach eng Kéier a fann se all eleng!',
    },
    fr: {
      consigne: 'Touche la première lettre d’un mot, puis sa dernière lettre (ou fais glisser ton doigt).',
      sens: {
        1: 'Les mots se lisent ➡️ ou ⬇️.',
        2: 'Les mots se lisent ➡️, ⬇️ ou en diagonale ↘️.',
        3: 'Les mots se cachent dans tous les sens, même à l’envers !',
      },
      horsLigne: {
        1: 'Oups ! Choisis deux lettres sur la même ligne ou la même colonne.',
        2: 'Oups ! Choisis deux lettres sur la même ligne, la même colonne ou la même diagonale ↘️.',
        3: 'Oups ! Choisis deux lettres sur une même ligne droite.',
      },
      // Lettre du mot écrite autrement dans la grille (sorte de signe, lettre du mot, lettres de la grille).
      note: (signe, a, b) => 'Dans la grille, les lettres n’ont pas ' +
        ({ accent: 'd’accent', trema: 'de tréma', cedille: 'de cédille' }[signe] || 'd’accent') +
        ' : ' + a + ' s’écrit ' + b + '.',
      noteLigature: (a, b) => 'Dans la grille, ' + a + ' s’écrit en deux lettres : ' + b + '.',
      titreListe: '🧭 Mots à trouver :',
      liste: 'Mots à trouver',
      ecouterMot: (m) => 'Écouter le mot « ' + m + ' »',
      ecouterMotTrouve: (m) => 'Écouter le mot « ' + m + ' » (trouvé)',
      indice: '💡 Indice',
      indiceLabel: 'Indice : faire clignoter la première lettre d’un mot',
      grille: (n) => 'Grille de lettres : ' + n + ' lignes et ' + n + ' colonnes. Utilise les flèches pour te déplacer.',
      caseLabel: (l, r, c) => l + ', ligne ' + r + ', colonne ' + c,
      progression: 'Mots trouvés',
      compteurTitre: 'Mots trouvés :',
      premiere: (l) => 'Première lettre : ' + l + '. Touche maintenant la dernière lettre du mot.',
      dejaTrouve: 'Tu as déjà trouvé ce mot. Cherche les autres !',
      presque: 'Presque ! Va bien de la première à la dernière lettre du mot.',
      pasDansListe: 'Ce n’est pas un mot de la liste. Cherche encore !',
      oui: 'Oui !',
      trouve: (m) => 'Tu as trouvé « ' + m + ' ».',
      tousTrouves: 'Tu as trouvé tous les mots !',
      indice1: (m) => 'Le mot « ' + m + ' » commence par la lettre qui clignote.',
      indice2: (m) => 'Le mot « ' + m + ' » commence et finit sur les lettres qui clignotent.',
      indice3: (m) => 'Voici tout le mot « ' + m + ' » : touche sa première lettre, puis sa dernière.',
      finParfaite: 'Tous les mots trouvés sans indice : tu as des yeux de vigie !',
      finNormale: 'Un point par mot trouvé sans indice. Rejoue pour tous les trouver sans aide !',
    },
  };

  // Typographie française : espace insécable avant ! ? : ; » et après «.
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, NB + '$1').replace(/« /g, '«' + NB);
  }

  // Directions [ligne, colonne].
  const DROITE = [0, 1];
  const BAS = [1, 0];
  const DIAG = [1, 1];
  const TOUTES = [[0, 1], [1, 0], [1, 1], [-1, 1], [0, -1], [-1, 0], [-1, -1], [1, -1]];

  // Taille de la grille, nombre de mots, longueur des mots (en cases) et sens permis.
  const NIVEAUX = {
    1: { taille: 6, nb: 4, min: 3, max: 5, sens: [DROITE, BAS] },
    2: { taille: 8, nb: 5, min: 4, max: 7, sens: [DROITE, BAS, DIAG] },
    3: { taille: 10, nb: 6, min: 5, max: 10, sens: TOUTES },
  };
  // Chinois : une case = un caractère ; les mots ont 2 caractères ou plus.
  const NIVEAUX_HANZI = {
    1: { taille: 5, nb: 3, min: 2, max: 3, sens: [DROITE, BAS] },
    2: { taille: 6, nb: 4, min: 2, max: 3, sens: [DROITE, BAS, DIAG] },
    3: { taille: 7, nb: 5, min: 2, max: 4, sens: TOUTES },
  };

  // Mots qu'on ne veut pas voir apparaître par hasard dans les cases de remplissage
  // (données, jamais affichées). Une langue absente de la liste n'en filtre aucun.
  const INTERDITS = {
    en: ['POO', 'WEE', 'BUM', 'SEX', 'FART', 'PISS', 'SHIT', 'FUCK', 'CRAP', 'TIT', 'TITS', 'ARSE', 'COCK', 'DICK', 'CUNT', 'TWAT', 'WANK', 'NAZI', 'PORN', 'SLUT', 'FAG', 'DAMN', 'KILL', 'NOB'],
    zh: ['笨蛋', '坏蛋', '王八', '猪头', '吃屎', '去死'],
    de: ['ARSCH', 'KACKE', 'KACK', 'SCHEISS', 'PISS', 'PISSE', 'FICK', 'NAZI', 'HURE', 'TITTE', 'SEX', 'NUTTE', 'FOTZE', 'PIMMEL', 'KOT', 'BLÖD', 'HITLER'],
    lb: ['KAKA', 'KACK', 'SCHÉISS', 'SCHEISS', 'PISS', 'FICK', 'NAZI', 'HUER', 'ARSCH', 'FOTZ', 'SEX'],
    fr: ['CUL', 'CON', 'CONNE', 'PUTE', 'MERDE', 'BITE', 'ZOB', 'SEXE', 'NAZI', 'CACA', 'PIPI', 'ZIZI', 'PISSE', 'CHIER', 'SALOPE', 'NIQUE', 'TEUB', 'FION'],
  };

  const hanzi = () => Ile.L().ecriture === 'hanzi';
  const envers = (s) => Array.from(s).reverse().join('');
  let ecouteurVoix = null; // écouteur « voiceschanged » de la partie en cours (un seul à la fois)

  // Contenu des cases d'un mot (chaîne : une lettre majuscule ou un caractère par case), ou null.
  function lettresDe(mot) {
    if (hanzi()) return /^\p{Script=Han}{2,}$/u.test(mot) ? mot : null;
    const L = Ile.lettresGrille(mot);
    return L ? L.join('') : null;
  }
  // Lettres permises dans la grille : alphabet + lettres propres à la langue (Ä, É…), en majuscules.
  function alphabetGrille() {
    const P = Ile.L();
    const set = new Set();
    P.alphabet.concat(P.lettresClavier || []).forEach((l) => {
      const u = l.toLocaleUpperCase(P.tts);
      if (Array.from(u).length === 1) set.add(u); // ß → SS : deux cases, déjà des lettres de l'alphabet
    });
    return set;
  }

  // Sac de remplissage.
  //  - alphabet : les lettres de la langue, pondérées par leur fréquence dans les mots du pack
  //    (chaque lettre au moins une fois) ;
  //  - chinois : les caractères des autres mots du pack (au niveau 1, jamais ceux des mots cherchés,
  //    pour ne pas tendre de piège aux débutants).
  const sacs = {};
  function sac(exclus) {
    const P = Ile.L();
    if (hanzi()) {
      const chars = new Set();
      (P.MOTS || []).forEach((m) => Array.from(m.mot).forEach((c) => chars.add(c)));
      Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach((w) => Array.from(w).forEach((c) => chars.add(c))));
      return Array.from(chars).filter((c) => /\p{Script=Han}/u.test(c) && !(exclus && exclus.has(c)));
    }
    const code = Ile.getLang();
    if (sacs[code]) return sacs[code];
    const freq = {};
    alphabetGrille().forEach((l) => { freq[l] = 1; });
    const mots = (P.MOTS || []).map((m) => m.mot);
    Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach((w) => mots.push(w)));
    mots.forEach((w) => {
      const L = Ile.lettresGrille(w);
      if (L) L.forEach((l) => { if (freq[l] !== undefined) freq[l]++; });
    });
    const s = [];
    Object.keys(freq).forEach((l) => { for (let k = 0; k < freq[l]; k++) s.push(l); });
    sacs[code] = s;
    return s;
  }

  // ---------------------------------------------------------------------------
  // Fabrication de la grille
  // ---------------------------------------------------------------------------

  // Mots de la partie : du niveau exact d'abord, écrits seulement avec des lettres de la grille,
  // et aucun mot ne doit se retrouver à l'intérieur d'un autre (dans un sens ou dans l'autre).
  function choisirMots(level, cfg, alpha) {
    const valide = (m) => {
      const L = lettresDe(m.mot);
      if (!L) return false;
      const n = Array.from(L).length;
      return (!alpha || Array.from(L).every((c) => alpha.has(c))) && n >= cfg.min && n <= Math.min(cfg.max, cfg.taille);
    };
    const conflit = (a, b) => [b, envers(b)].some((x) => a.indexOf(x) !== -1 || x.indexOf(a) !== -1);
    const choisis = [];
    Ile.motsPourPartie(level, Infinity, valide).forEach((m) => {
      if (choisis.length >= cfg.nb) return;
      const L = lettresDe(m.mot);
      if (choisis.some((c) => conflit(c.lettres, L))) return;
      choisis.push({ m, lettres: L });
    });
    return choisis;
  }

  // Toutes les apparitions d'une suite de cases dans la grille (8 directions ;
  // un palindrome lu dans les deux sens ne compte qu'une fois).
  function chercher(g, n, mot) {
    const L = Array.from(mot);
    const trouves = [];
    const vus = new Set();
    for (let r = 0; r < n; r++) {
      for (let c = 0; c < n; c++) {
        if (g[r][c] !== L[0]) continue;
        TOUTES.forEach(([dr, dc]) => {
          const r2 = r + dr * (L.length - 1);
          const c2 = c + dc * (L.length - 1);
          if (r2 < 0 || r2 >= n || c2 < 0 || c2 >= n) return;
          for (let k = 1; k < L.length; k++) if (g[r + dr * k][c + dc * k] !== L[k]) return;
          const cases = [];
          for (let k = 0; k < L.length; k++) cases.push([r + dr * k, c + dc * k]);
          const cle = cases.map(([a, b]) => a * n + b).sort((a, b) => a - b).join(',');
          if (vus.has(cle)) return;
          vus.add(cle);
          trouves.push(cases);
        });
      }
    }
    return trouves;
  }

  // Place les mots (du plus long au plus court) avec retour arrière. Les croisements
  // sur une case identique sont permis. Renvoie la grille (cases vides = '') ou null.
  function placer(mots, cfg) {
    const n = cfg.taille;
    const g = Array.from({ length: n }, () => new Array(n).fill(''));
    // Chaque mot reçoit un sens préféré : les directions du niveau sont bien réparties
    // (au moins une diagonale au niveau 2, plusieurs mots à l'envers au niveau 3).
    let prefs = [];
    while (prefs.length < mots.length) prefs = prefs.concat(Ile.shuffle(cfg.sens));
    const ordre = mots.map((w, k) => ({ w, pref: prefs[k] }))
      .sort((a, b) => Array.from(b.w.lettres).length - Array.from(a.w.lettres).length);
    let budget = 4000;

    function emplacements(L, dr, dc) {
      const res = [];
      for (let r = 0; r < n; r++) {
        for (let c = 0; c < n; c++) {
          const r2 = r + dr * (L.length - 1);
          const c2 = c + dc * (L.length - 1);
          if (r2 < 0 || r2 >= n || c2 < 0 || c2 >= n) continue;
          let ok = true;
          let neuves = 0;
          for (let k = 0; k < L.length && ok; k++) {
            const x = g[r + dr * k][c + dc * k];
            if (x === '') neuves++;
            else if (x !== L[k]) ok = false;
          }
          if (ok && neuves > 0) res.push({ r, c });
        }
      }
      return res;
    }

    function poser(i) {
      if (i === ordre.length) return true;
      const { w, pref } = ordre[i];
      const L = Array.from(w.lettres);
      const dirs = [pref].concat(Ile.shuffle(cfg.sens.filter((d) => d !== pref)));
      for (const [dr, dc] of dirs) {
        for (const p of Ile.shuffle(emplacements(L, dr, dc)).slice(0, 8)) {
          if (--budget < 0) return false;
          const ecrites = [];
          const cases = [];
          for (let k = 0; k < L.length; k++) {
            const r = p.r + dr * k;
            const c = p.c + dc * k;
            cases.push([r, c]);
            if (g[r][c] === '') { g[r][c] = L[k]; ecrites.push([r, c]); }
          }
          w.cases = cases;
          if (poser(i + 1)) return true;
          ecrites.forEach(([r, c]) => { g[r][c] = ''; });
        }
      }
      return false;
    }
    return poser(0) ? g : null;
  }

  // Remplit les cases vides au hasard, sans former une deuxième fois un mot de la liste
  // (ni un gros mot). Renvoie true si la grille est bonne.
  function remplir(g, n, mots, level) {
    const exclus = hanzi() && level === 1 ? new Set(mots.map((w) => Array.from(w.lettres)).flat()) : null;
    const lettres = sac(exclus);
    const interdits = INTERDITS[Ile.getLang()] || [];
    const vides = [];
    for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) if (g[r][c] === '') vides.push([r, c]);
    const libres = new Set(vides.map(([r, c]) => r * n + c));
    const grossier = () => interdits.some((w) =>
      chercher(g, n, w).some((cases) => cases.some(([r, c]) => libres.has(r * n + c))));
    for (let essai = 0; essai < 400; essai++) {
      vides.forEach(([r, c]) => { g[r][c] = lettres[Math.floor(Math.random() * lettres.length)]; });
      if (mots.every((w) => chercher(g, n, w.lettres).length === 1) && !grossier()) return true;
    }
    return false;
  }

  // Grille de secours (jamais utilisée en pratique) : un mot par ligne, de gauche à droite.
  function secours(level, cfg, alpha) {
    const n = cfg.taille;
    const mots = choisirMots(level, cfg, alpha);
    const g = Array.from({ length: n }, () => new Array(n).fill(''));
    mots.forEach((w, k) => {
      const r = Math.min(n - 1, k * 2);
      w.cases = [];
      Array.from(w.lettres).forEach((l, i) => { g[r][i] = l; w.cases.push([r, i]); });
    });
    if (!remplir(g, n, mots, level)) {
      const lettres = sac(null);
      for (let r = 0; r < n; r++) for (let c = 0; c < n; c++) if (g[r][c] === '') g[r][c] = lettres[0];
    }
    return { g, mots };
  }

  // La grille montre bien ce qui est nouveau au niveau : une diagonale au niveau 2,
  // une diagonale et des mots à l'envers au niveau 3.
  function variee(level, mots) {
    const sens = mots.map((w) => [w.cases[1][0] - w.cases[0][0], w.cases[1][1] - w.cases[0][1]]);
    const diag = sens.some(([dr, dc]) => dr !== 0 && dc !== 0);
    const aLEnvers = sens.filter(([dr, dc]) => dc < 0 || (dc === 0 && dr < 0)).length;
    if (level === 2) return diag;
    if (level === 3) return diag && aLEnvers >= 2;
    return true;
  }

  function genererPartie(level, cfg) {
    const alpha = hanzi() ? null : alphabetGrille();
    let premiere = null;
    for (let essai = 0; essai < 60; essai++) {
      const mots = choisirMots(level, cfg, alpha);
      if (mots.length < cfg.nb) continue;
      const g = placer(mots, cfg);
      if (!g || !remplir(g, cfg.taille, mots, level)) continue;
      if (variee(level, mots)) return { g, mots };
      premiere = premiere || { g, mots };
    }
    return premiere || secours(level, cfg, alpha);
  }

  // Première lettre d'un mot écrite autrement dans la grille que dans la liste : accent, tréma,
  // cédille ou ligature (français : Î → I, Œ → OE), ß → SS (allemand).
  function difference(mot, affiche, locale) {
    if (hanzi()) return null;
    if (mot.indexOf('ß') !== -1) return { lettre: 'ß', grille: 'SS', signe: 'eszett', ligature: true };
    if (affiche !== mot.toLocaleUpperCase(locale)) return null; // liste en casse normale : Ä reste Ä
    for (const ch of Array.from(mot)) {
      const L = Ile.lettresGrille(ch);
      if (!L) continue;
      const haut = ch.toLocaleUpperCase(locale);
      const grille = L.join('');
      if (grille === haut) continue;
      const marques = haut.normalize('NFD');
      let signe = 'accent';
      if (/̈/.test(marques)) signe = 'trema';
      else if (/̧/.test(marques)) signe = 'cedille';
      return { lettre: haut, grille, signe, ligature: grille.length > 1 };
    }
    return null;
  }

  // ---------------------------------------------------------------------------
  // Le jeu
  // ---------------------------------------------------------------------------
  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const P = Ile.L();
      const locale = P.tts;
      const zh = hanzi();
      // Allemand, luxembourgeois : la liste garde la casse des noms (Katze) ; ailleurs, majuscules comme la grille.
      const casseNormale = P.MOTS.some((m) => m.mot[0] !== m.mot[0].toLocaleLowerCase(locale));
      const cfg = (zh ? NIVEAUX_HANZI : NIVEAUX)[level] || (zh ? NIVEAUX_HANZI : NIVEAUX)[1];
      const n = cfg.taille;
      const partie = genererPartie(level, cfg);
      const g = partie.g;
      const mots = partie.mots.map((w, k) => ({
        m: w.m,
        lettres: w.lettres,
        cases: w.cases,
        affiche: zh || casseNormale ? w.m.mot : w.m.mot.toLocaleUpperCase(locale),
        pinyin: Ile.aide(w.m),
        couleur: k % 6,
        trouve: false,
        aide: false,
        indices: 0,
      }));
      // Nom d'un mot dans les messages avant qu'il soit trouvé : le mot, ou (chinois) ce que montre la liste.
      const nomListe = (w) => (zh ? (level <= 2 ? w.m.emoji + ' ' + w.pinyin : w.m.emoji) : w.m.mot);
      const total = mots.length;
      let score = 0;
      let trouves = 0;
      let fini = false;
      let premier = null; // première case touchée (mode « touche, touche »)
      let glisse = null; // glissé du doigt ou de la souris en cours

      // État exposé pour les tests automatiques.
      window.__motsCaches = {
        niveau: level,
        langue: Ile.getLang(),
        ecriture: P.ecriture,
        taille: n,
        mots: mots.map((w) => ({ mot: w.m.mot, lettres: w.lettres, pinyin: w.pinyin, cases: w.cases.map((x) => x.slice()) })),
      };

      const panel = el('section', { class: 'panel mc mc--n' + n + (zh ? ' mc--hanzi' : ''), 'data-langue': Ile.getLang() });
      root.appendChild(panel);
      const actif = () => panel.isConnected;

      // --- En-tête : progression + bouton d'indice ---
      const btnIndice = el('button', { type: 'button', class: 'btn btn--sun mc-indice', text: tx.indice, 'aria-label': typo(tx.indiceLabel), title: typo(tx.indiceLabel) });
      btnIndice.addEventListener('click', indice);
      const haut = el('div', { class: 'mc-haut' }, [btnIndice]);
      function majProgress() {
        const bar = Ile.progress(haut, trouves, total, score);
        bar.setAttribute('aria-label', tx.progression);
        const label = bar.querySelector('.progress__label');
        label.textContent = '';
        label.append(el('span', { class: 'mc-compteur__titre', text: typo(tx.compteurTitre) + ' ' }), trouves + ' / ' + total);
      }

      // --- Consigne ---
      const sep = P.sepMots === undefined ? ' ' : P.sepMots; // pas d'espace entre deux phrases chinoises
      const consigne = el('p', { class: 'consigne', id: 'mc-consigne' }, [
        typo(tx.consigne) + sep,
        el('span', { class: 'mc-sens', text: typo(tx.sens[level] || tx.sens[1]) }),
      ]);
      // Petite note si un mot de la liste s'écrit autrement dans la grille : « Î s'écrit I », « ß → SS ».
      let diff = null;
      mots.forEach((w) => { if (!diff) diff = difference(w.m.mot, w.affiche, locale); });
      const note = diff
        ? el('p', { class: 'mc-note', text: typo(diff.ligature ? tx.noteLigature(diff.lettre, diff.grille) : tx.note(diff.signe, diff.lettre, diff.grille)) })
        : null;

      // --- Grille ---
      const traits = el('div', { class: 'mc-traits', 'aria-hidden': 'true' });
      const grilleEl = el('div', {
        class: 'mc-grille', role: 'group',
        'aria-label': typo(tx.grille(n)),
        'aria-describedby': 'mc-consigne',
      });
      const cases = [];
      for (let r = 0; r < n; r++) {
        cases.push([]);
        for (let c = 0; c < n; c++) {
          const b = el('button', {
            type: 'button', class: 'mc-case', tabindex: r === 0 && c === 0 ? '0' : '-1',
            'data-r': String(r), 'data-c': String(c),
            'aria-label': tx.caseLabel(g[r][c], r + 1, c + 1),
            text: g[r][c],
          });
          b._pos = { r, c };
          cases[r].push(b);
          grilleEl.appendChild(b);
        }
      }
      const plateau = el('div', { class: 'mc-plateau', style: '--n:' + n, 'data-taille': String(n) }, [traits, grilleEl]);

      // --- Liste des mots ---
      // Étiquette d'un mot : bouton « écouter » si une voix existe pour la langue, sinon simple étiquette.
      const pyEl = (p, cls) => el('span', { class: 'pinyin' + (cls ? ' ' + cls : ''), lang: 'zh-Latn-pinyin', text: p });
      function contenuChip(w) {
        let texte;
        if (!zh) {
          texte = el('span', { class: 'mc-mot__texte', text: w.affiche });
        } else if (w.trouve) {
          // Trouvé : les caractères (barrés) et leur pinyin dessous.
          texte = el('span', { class: 'mc-mot__texte mc-mot__texte--hanzi' }, [
            el('span', { class: 'mc-mot__hanzi', text: w.m.mot }), pyEl(w.pinyin),
          ]);
        } else if (level <= 2) {
          texte = el('span', { class: 'mc-mot__texte mc-mot__texte--py' }, [pyEl(w.pinyin, 'mc-mot__py')]);
        } else {
          // Niveau 3 : l'emoji seul, et une case vide par caractère à trouver.
          texte = el('span', { class: 'mc-mot__texte mc-mot__cases' }, Array.from(w.m.mot).map(() =>
            el('span', { class: 'mc-mot__case', 'aria-hidden': 'true' })).concat([el('span', { class: 'sr-only', lang: 'zh-Latn-pinyin', text: w.pinyin })]));
        }
        return [
          el('span', { class: 'mc-mot__emoji', 'aria-hidden': 'true', text: w.m.emoji }),
          texte,
          el('span', { class: 'mc-mot__etat', 'aria-hidden': 'true', text: w.trouve ? (w.aide ? '💡' : '✅') : (w.aide ? '💡' : '') }),
        ];
      }
      let voix = Ile.canSpeak;
      function rendreChip(w) {
        const nomVoix = zh ? (w.trouve ? w.m.mot + ' ' + w.pinyin : w.pinyin) : w.m.mot;
        const chip = voix
          ? el('button', { type: 'button', class: 'mc-mot', title: Ile.t('ecouter'),
            'aria-label': typo(w.trouve ? tx.ecouterMotTrouve(nomVoix) : tx.ecouterMot(nomVoix)) }, contenuChip(w))
          : el('span', { class: 'mc-mot' }, contenuChip(w));
        if (voix) chip.addEventListener('click', () => { if (!Ile.say(w.m.mot)) Ile.flash(chip, 'bad'); });
        chip.style.setProperty('--c', 'var(--mc-c' + w.couleur + ')');
        chip.setAttribute('data-mot', w.m.mot);
        if (w.trouve) chip.classList.add('is-found');
        if (w.aide) chip.classList.add('is-aide');
        if (w.chip) w.chip.replaceWith(chip);
        w.chip = chip;
        return chip;
      }
      const liste = el('ul', { class: 'mc-liste', 'aria-label': tx.liste });
      mots.forEach((w) => liste.appendChild(el('li', null, [rendreChip(w)])));
      // La liste des voix arrive parfois après le chargement (et il n'y a souvent aucune voix
      // luxembourgeoise) : les étiquettes deviennent ou cessent d'être des boutons « écouter ».
      const surVoix = () => {
        if (!actif()) { try { window.speechSynthesis.removeEventListener('voiceschanged', surVoix); } catch (e) { /* rien */ } return; }
        if (Ile.canSpeak === voix) return;
        voix = Ile.canSpeak;
        mots.forEach(rendreChip);
      };
      try {
        // L'écouteur de la partie précédente (niveau, langue, rejouer) est retiré tout de suite.
        if (ecouteurVoix) window.speechSynthesis.removeEventListener('voiceschanged', ecouteurVoix);
        ecouteurVoix = surVoix;
        window.speechSynthesis.addEventListener('voiceschanged', surVoix);
      } catch (e) { /* pas de synthèse vocale */ }

      const cote = el('div', { class: 'mc-cote' }, [
        el('h2', { class: 'mc-titre-liste', text: typo(tx.titreListe) }),
        liste,
      ]);

      const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
      const neutre = (text) => { fb.textContent = typo(text); fb.className = 'feedback'; };

      panel.append(haut, consigne);
      if (note) panel.appendChild(note);
      panel.append(el('div', { class: 'mc-jeu' }, [plateau, cote]), fb);
      majProgress();

      // Téléphone : la liste, la grille et le message doivent se voir ensemble pendant la partie
      // (on ne peut pas faire défiler la page en glissant sur la grille). Si le message est sous
      // le bord de l'écran, la page défile une fois, sans cacher la liste (l'en-tête, non collé
      // sur téléphone dans ce jeu, et la consigne peuvent sortir de l'écran).
      function cadrer() {
        if (!actif()) return;
        const bas = fb.getBoundingClientRect().bottom;
        if (bas <= window.innerHeight) return;
        const barre = document.getElementById('topbar');
        const collee = barre && window.getComputedStyle(barre).position === 'sticky' ? barre.getBoundingClientRect().bottom : 0;
        const dy = Math.min(bas - window.innerHeight + 8, cote.getBoundingClientRect().top - collee - 8);
        if (dy > 0) window.scrollBy({ top: dy, left: 0, behavior: 'auto' });
      }
      if (window.requestAnimationFrame) window.requestAnimationFrame(cadrer);

      // ---------------------------------------------------------------------
      // Traits de surlignage (en % de la grille, qui est carrée)
      // ---------------------------------------------------------------------
      function dessiner(t, a, b) {
        const cell = 100 / n;
        const x0 = (a.c + 0.5) * cell;
        const y0 = (a.r + 0.5) * cell;
        const dx = (b.c - a.c) * cell;
        const dy = (b.r - a.r) * cell;
        const L = Math.hypot(dx, dy);
        const h = cell * 0.8;
        t.style.left = (x0 - h / 2) + '%';
        t.style.top = (y0 - h / 2) + '%';
        t.style.width = (L + h) + '%';
        t.style.height = h + '%';
        t.style.transformOrigin = ((h / 2) / (L + h)) * 100 + '% 50%';
        t.style.transform = 'rotate(' + (Math.atan2(dy, dx) * 180) / Math.PI + 'deg)';
        return t;
      }
      const selection = el('span', { class: 'mc-trait mc-trait--selection' });
      selection.hidden = true;
      traits.appendChild(selection);
      function montrerSelection(a, b, apercu) {
        dessiner(selection, a, b);
        selection.classList.toggle('mc-trait--apercu', !!apercu);
        selection.hidden = false;
      }
      function cacherSelection() {
        selection.hidden = true;
        if (premier) montrerSelection(premier, premier);
      }

      // ---------------------------------------------------------------------
      // Géométrie de la sélection
      // ---------------------------------------------------------------------
      // Sens autorisés pour tracer : ceux du niveau, et leurs contraires (on accepte
      // qu'un enfant sélectionne un mot de la dernière case vers la première).
      const tracables = [];
      cfg.sens.concat(cfg.sens.map(([dr, dc]) => [-dr, -dc])).forEach((d) => {
        if (!tracables.some((x) => x[0] === d[0] && x[1] === d[1])) tracables.push(d);
      });
      const dansGrille = (r, c) => r >= 0 && r < n && c >= 0 && c < n;

      function chemin(a, b) {
        const dr = b.r - a.r;
        const dc = b.c - a.c;
        const sr = Math.sign(dr);
        const sc = Math.sign(dc);
        if (!tracables.some(([x, y]) => x === sr && y === sc)) return null;
        if (dr !== 0 && dc !== 0 && Math.abs(dr) !== Math.abs(dc)) return null;
        const len = Math.max(Math.abs(dr), Math.abs(dc)) + 1;
        const res = [];
        for (let k = 0; k < len; k++) res.push({ r: a.r + sr * k, c: a.c + sc * k });
        return res;
      }

      // Case visée par le doigt, « aimantée » sur la direction permise la plus proche.
      function caseVisee(depart, x, y) {
        const rect = grilleEl.getBoundingClientRect();
        const t = rect.width / n;
        const dx = (x - rect.left) / t - 0.5 - depart.c;
        const dy = (y - rect.top) / t - 0.5 - depart.r;
        if (Math.hypot(dx, dy) < 0.5) return depart;
        let meilleur = null;
        let proj = -Infinity;
        tracables.forEach(([dr, dc]) => {
          const p = (dx * dc + dy * dr) / Math.hypot(dr, dc);
          if (p > proj) { proj = p; meilleur = [dr, dc]; }
        });
        let k = Math.max(0, Math.round(proj / Math.hypot(meilleur[0], meilleur[1])));
        while (k > 0 && !dansGrille(depart.r + meilleur[0] * k, depart.c + meilleur[1] * k)) k--;
        return { r: depart.r + meilleur[0] * k, c: depart.c + meilleur[1] * k };
      }

      function caseSous(x, y) {
        const rect = grilleEl.getBoundingClientRect();
        const c = Math.floor(((x - rect.left) / rect.width) * n);
        const r = Math.floor(((y - rect.top) / rect.height) * n);
        return dansGrille(r, c) ? { r, c } : null;
      }
      const memeCase = (a, b) => a && b && a.r === b.r && a.c === b.c;

      // ---------------------------------------------------------------------
      // Actions de l'enfant
      // ---------------------------------------------------------------------
      function marquerPremier(p) {
        cases.forEach((ligne) => ligne.forEach((b) => b.removeAttribute('aria-pressed')));
        premier = p;
        if (p) {
          cases[p.r][p.c].setAttribute('aria-pressed', 'true');
          montrerSelection(p, p);
        } else {
          selection.hidden = true;
        }
      }

      // Une case touchée (tap, clic, Entrée) : 1re case, puis dernière case.
      function toucher(p) {
        if (fini || !actif()) return;
        if (!premier) {
          marquerPremier(p);
          Ile.sfx('click');
          neutre(tx.premiere(g[p.r][p.c]));
          return;
        }
        if (memeCase(premier, p)) { // même case : on annule
          marquerPremier(null);
          neutre('');
          return;
        }
        const a = premier;
        marquerPremier(null);
        valider(a, p);
      }

      function valider(a, b) {
        if (fini || !actif()) return;
        const path = chemin(a, b);
        if (!path) {
          Ile.sfx('bad');
          Ile.flash(cases[b.r][b.c], 'bad');
          Ile.feedback(fb, false, typo(tx.horsLigne[level] || tx.horsLigne[1]));
          return;
        }
        const s = path.map((p) => g[p.r][p.c]).join('');
        const rs = envers(s);
        const w = mots.find((x) => !x.trouve && (x.lettres === s || x.lettres === rs));
        if (w) { trouver(w, path); return; }
        if (mots.some((x) => x.trouve && (x.lettres === s || x.lettres === rs))) {
          Ile.sfx('click');
          neutre(tx.dejaTrouve);
          return;
        }
        // Mauvaise sélection : pas de pénalité, juste un petit trait rouge qui s'efface.
        const faux = dessiner(el('span', { class: 'mc-trait mc-trait--faux' }), path[0], path[path.length - 1]);
        traits.appendChild(faux);
        setTimeout(() => faux.remove(), 800);
        Ile.sfx('bad');
        const cles = new Set(path.map((p) => p.r * n + p.c));
        const partiel = path.length > 1 && mots.some((x) => !x.trouve && x.cases.filter(([r, c]) => cles.has(r * n + c)).length === path.length);
        Ile.feedback(fb, false, typo(partiel ? tx.presque : tx.pasDansListe));
      }

      function trouver(w, path) {
        w.trouve = true;
        trouves++;
        if (!w.aide) score++;
        w.cases = path.map((p) => [p.r, p.c]);
        const t = dessiner(el('span', { class: 'mc-trait is-nouveau' }), path[0], path[path.length - 1]);
        t.style.setProperty('--c', 'var(--mc-c' + w.couleur + ')');
        traits.insertBefore(t, selection);
        path.forEach((p) => {
          const b = cases[p.r][p.c];
          b.classList.add('is-trouvee');
          b.classList.remove('is-indice');
        });
        rendreChip(w);
        Ile.flash(w.chip, 'good');
        Ile.sfx('good');
        Ile.say(Ile.groupe(Ile.un(w.m), w.m.mot));
        const debut = w.aide ? tx.oui : Ile.pick(Ile.t('bravo'), 1)[0];
        majProgress();
        if (trouves === total) {
          fini = true;
          btnIndice.disabled = true;
          Ile.feedback(fb, true, typo(debut + sep + tx.tousTrouves));
          setTimeout(() => {
            if (!actif()) return;
            Ile.showResult({
              id: ID,
              // Score exact : un point par mot trouvé sans indice (le message de fin le dit).
              score,
              total,
              message: typo(score === total ? tx.finParfaite : tx.finNormale),
            });
          }, 1500);
          return;
        }
        // En chinois, le pinyin suit les caractères trouvés.
        Ile.feedback(fb, true, typo(debut + sep + tx.trouve(w.m.mot)));
        if (zh && w.pinyin) fb.append(' ', pyEl(w.pinyin, 'mc-fb-py'));
      }

      // Indice : la première case d'un mot clignote (puis aussi la dernière, puis tout le mot).
      // Le mot ne rapportera pas de point.
      function indice() {
        if (fini || !actif()) return;
        const w = mots.find((x) => !x.trouve && x.aide) || mots.find((x) => !x.trouve);
        if (!w) return;
        w.aide = true;
        w.indices++;
        w.chip.classList.add('is-aide');
        w.chip.querySelector('.mc-mot__etat').textContent = '💡';
        const debut = w.cases[0];
        const fin = w.cases[w.cases.length - 1];
        let aMontrer = [debut];
        if (w.indices === 2) aMontrer = [debut, fin];
        if (w.indices >= 3) aMontrer = w.cases;
        aMontrer.forEach(([r, c]) => {
          const b = cases[r][c];
          b.classList.remove('is-indice');
          void b.offsetWidth; // relance le clignotement
          b.classList.add('is-indice');
        });
        Ile.sfx('flip');
        Ile.say(Ile.groupe(Ile.un(w.m), w.m.mot));
        const texte = w.indices >= 3 ? tx.indice3 : w.indices === 2 ? tx.indice2 : tx.indice1;
        neutre(texte(nomListe(w)));
      }

      // ---------------------------------------------------------------------
      // Souris et doigt : tap-tap ou glisser
      // ---------------------------------------------------------------------
      const pointeurs = !!window.PointerEvent;
      if (pointeurs) {
        grilleEl.addEventListener('pointerdown', (e) => {
          if (fini || glisse || (e.pointerType === 'mouse' && e.button !== 0)) return;
          const p = caseSous(e.clientX, e.clientY);
          if (!p) return;
          e.preventDefault(); // pas de sélection de texte ni de loupe
          glisse = { id: e.pointerId, depart: p, fin: p };
          try { grilleEl.setPointerCapture(e.pointerId); } catch (err) { /* rien */ }
          focusCase(p, false);
          if (!premier) montrerSelection(p, p);
        });
        grilleEl.addEventListener('pointermove', (e) => {
          if (fini) return;
          if (glisse && e.pointerId === glisse.id) {
            const fin = caseVisee(glisse.depart, e.clientX, e.clientY);
            if (!memeCase(fin, glisse.fin)) {
              glisse.fin = fin;
              if (!memeCase(fin, glisse.depart)) montrerSelection(glisse.depart, fin);
              else if (premier) montrerSelection(premier, premier);
            }
            return;
          }
          // Souris : après la 1re case, on montre la ligne en aperçu.
          if (!glisse && premier && e.pointerType === 'mouse') {
            montrerSelection(premier, caseVisee(premier, e.clientX, e.clientY), true);
          }
        });
        const finGlisse = (e, annule) => {
          if (!glisse || e.pointerId !== glisse.id) return;
          const { depart, fin } = glisse;
          glisse = null;
          try { grilleEl.releasePointerCapture(e.pointerId); } catch (err) { /* rien */ }
          if (annule) { cacherSelection(); return; }
          if (memeCase(depart, fin)) { toucher(depart); return; }
          marquerPremier(null);
          valider(depart, fin);
        };
        grilleEl.addEventListener('pointerup', (e) => finGlisse(e, false));
        grilleEl.addEventListener('pointercancel', (e) => finGlisse(e, true));
        // Capture perdue sans pointerup (geste interrompu par le système) : on annule le glissé,
        // sinon la grille resterait bloquée (glisse jamais remis à null).
        grilleEl.addEventListener('lostpointercapture', (e) => finGlisse(e, true));
        grilleEl.addEventListener('pointerleave', (e) => {
          if (!glisse && premier && e.pointerType === 'mouse') montrerSelection(premier, premier);
        });
      }
      // Clic « clavier » (Entrée, Espace) ou lecteur d'écran : detail vaut 0.
      grilleEl.addEventListener('click', (e) => {
        const b = e.target.closest('.mc-case');
        if (!b) return;
        if (e.detail === 0 || !pointeurs) toucher(b._pos);
      });

      // ---------------------------------------------------------------------
      // Clavier : flèches pour se déplacer dans la grille
      // ---------------------------------------------------------------------
      function focusCase(p, focus) {
        cases.forEach((ligne) => ligne.forEach((b) => b.setAttribute('tabindex', '-1')));
        const b = cases[p.r][p.c];
        b.setAttribute('tabindex', '0');
        if (focus) b.focus();
      }
      grilleEl.addEventListener('keydown', (e) => {
        const b = e.target.closest && e.target.closest('.mc-case');
        if (!b) return;
        let { r, c } = b._pos;
        switch (e.key) {
          case 'ArrowRight': c++; break;
          case 'ArrowLeft': c--; break;
          case 'ArrowDown': r++; break;
          case 'ArrowUp': r--; break;
          case 'Home': c = 0; break;
          case 'End': c = n - 1; break;
          case 'Escape':
            if (premier) { marquerPremier(null); neutre(''); e.preventDefault(); }
            return;
          default: return;
        }
        e.preventDefault();
        r = Math.max(0, Math.min(n - 1, r));
        c = Math.max(0, Math.min(n - 1, c));
        focusCase({ r, c }, true);
        if (premier) {
          const path = chemin(premier, { r, c });
          if (path) montrerSelection(premier, { r, c }, true);
          else montrerSelection(premier, premier);
        }
      });
    },
  });
})();
