/* L'ordre alphabétique : la lettre d'avant ou d'après (niveau 1), puis ranger des mots (niveaux 2 et 3).
 * Tri : Ile.compare (l'allemand range ä avec a, le luxembourgeois é et ë avec e) ; les noms allemands et
 * luxembourgeois gardent leur majuscule.
 * Chinois : ordre alphabétique du pinyin (音序). Les tuiles montrent les caractères et leur pinyin ; on range
 * syllabe par syllabe (lettres, puis ton : ā á ǎ à), comme dans un dictionnaire. On ne propose que des séries
 * où cet ordre est aussi celui des lettres du pinyin sans tons (Ile.epeler) : aucune ambiguïté possible.
 * Au niveau 3, deux manches opposent des mots de même pinyin sans tons (书 shū / 树 shù) : le ton décide.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'alphabet';
  const TOTAL = 8;
  const PAUSE = 1500;

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      questionApres: (x) => 'Which letter comes just after ' + x + '?',
      questionAvant: (x) => 'Which letter comes just before ' + x + '?',
      phraseApres: (x, y) => 'After ' + x + ' comes ' + y + '.',
      phraseAvant: (x, y) => 'Before ' + x + ' comes ' + y + '.',
      oui: 'Yes!',
      nonApres: (l, x) => 'No, ' + l + ' doesn’t come just after ' + x + '. Look at the alphabet!',
      nonAvant: (l, x) => 'No, ' + l + ' doesn’t come just before ' + x + '. Look at the alphabet!',
      ecouterQuestion: 'Listen to the question',
      frise: 'The alphabet',
      aTrouver: 'letter to find',
      lettre: (l) => 'Letter ' + l,
      choix: 'Possible answers',
      consigneMots: 'Put the words in alphabetical order: tap the one that comes first, then the next.',
      astuce: 'If two words start the same way, look at the next letter.',
      astuceTon: 'Same letters? Then look at the tone marks.',
      rangement: 'Words in order',
      aRanger: 'Words to sort',
      placeVide: (k) => 'Position ' + k + ': empty',
      placePleine: (k, m) => 'Position ' + k + ': ' + m,
      premierOk: (m) => 'Yes! “' + m + '” comes first.',
      suivantOk: (m) => 'Yes! Then “' + m + '”.',
      rangee: 'The words are in the right order.',
      nonLettre: (l) => 'Not yet! Another word starts with a letter that comes before ' + l + '.',
      nonMeme: (p, k) => 'Not yet! Another word also starts with “' + p + '”: look at letter number ' + k + '.',
      nonTon: 'Not yet! These words have the same letters: look at the tone marks.',
      finParfaite: 'Not a single mistake: you know your alphabet inside out!',
      finNormale: 'You get a point for each round with no mistakes. Keep practising!',
      // Séparateur de la liste rangée lue à voix haute.
      sepListe: ', ',
    },
    zh: {
      questionApres: (x) => '字母 ' + x + ' 后面是哪个字母？',
      questionAvant: (x) => '字母 ' + x + ' 前面是哪个字母？',
      phraseApres: (x, y) => x + ' 后面是 ' + y + '。',
      phraseAvant: (x, y) => x + ' 前面是 ' + y + '。',
      oui: '对了！',
      nonApres: (l, x) => '不对，' + x + ' 后面不是 ' + l + '。看看字母表吧！',
      nonAvant: (l, x) => '不对，' + x + ' 前面不是 ' + l + '。看看字母表吧！',
      ecouterQuestion: '听一听问题',
      frise: '拼音字母表',
      aTrouver: '要找的字母',
      lettre: (l) => '字母 ' + l,
      choix: '选项',
      consigneMots: '按拼音字母的顺序给词语排队：先点排在最前面的词。',
      astuce: '开头的字母一样，就看下一个字母。',
      astuceTon: '拼音字母都一样，就看声调：一声 ā、二声 á、三声 ǎ、四声 à。',
      rangement: '排好的词语',
      aRanger: '要排队的词语',
      placeVide: (k) => '第 ' + k + ' 个位置：空的',
      placePleine: (k, m) => '第 ' + k + ' 个位置：' + m,
      premierOk: (m) => '对了！“' + m + '”排第一。',
      suivantOk: (m) => '对了！下一个是“' + m + '”。',
      rangee: '词语排好队了。',
      nonLettre: (l) => '还不对！有一个词的第一个字母在 ' + l + ' 前面。',
      nonMeme: (p, k) => '还不对！还有一个词也是“' + p + '”开头的：看第 ' + k + ' 个字母。',
      nonTon: '还不对！这两个词的拼音字母一样，要看声调：一声、二声、三声、四声。',
      finParfaite: '一个错误也没有：拼音字母表你记得清清楚楚！',
      finNormale: '每轮没有出错就得一分。继续练习吧！',
      sepListe: '、',
    },
    de: {
      questionApres: (x) => 'Welcher Buchstabe kommt direkt nach ' + x + '?',
      questionAvant: (x) => 'Welcher Buchstabe kommt direkt vor ' + x + '?',
      phraseApres: (x, y) => 'Nach ' + x + ' kommt ' + y + '.',
      phraseAvant: (x, y) => 'Vor ' + x + ' kommt ' + y + '.',
      oui: 'Ja!',
      nonApres: (l, x) => 'Nein, ' + l + ' kommt nicht direkt nach ' + x + '. Schau dir das Alphabet an!',
      nonAvant: (l, x) => 'Nein, ' + l + ' kommt nicht direkt vor ' + x + '. Schau dir das Alphabet an!',
      ecouterQuestion: 'Frage anhören',
      frise: 'Das Alphabet',
      aTrouver: 'gesuchter Buchstabe',
      lettre: (l) => 'Buchstabe ' + l,
      choix: 'Antworten',
      consigneMots: 'Ordne die Wörter nach dem ABC: Tippe zuerst das Wort an, das vorne steht.',
      astuce: 'Fangen zwei Wörter gleich an, schau auf den nächsten Buchstaben.',
      astuceTon: 'Gleiche Buchstaben? Dann schau auf die Tonzeichen.',
      rangement: 'Deine Reihenfolge',
      aRanger: 'Wörter zum Ordnen',
      placeVide: (k) => 'Platz ' + k + ': leer',
      placePleine: (k, m) => 'Platz ' + k + ': ' + m,
      premierOk: (m) => 'Ja! „' + m + '“ kommt zuerst.',
      suivantOk: (m) => 'Ja! Dann kommt „' + m + '“.',
      rangee: 'Die Wörter sind richtig geordnet.',
      nonLettre: (l) => 'Noch nicht! Ein anderes Wort beginnt mit einem Buchstaben, der vor ' + l + ' kommt.',
      nonMeme: (p, k) => 'Noch nicht! Ein anderes Wort beginnt auch mit „' + p + '“: Schau auf den ' + k + '. Buchstaben.',
      nonTon: 'Noch nicht! Diese Wörter haben die gleichen Buchstaben: Schau auf die Tonzeichen.',
      finParfaite: 'Kein einziger Fehler: Du kennst das ABC in- und auswendig!',
      finNormale: 'Für jede Runde ohne Fehler gibt es einen Punkt. Übe weiter!',
      sepListe: ', ',
    },
    lb: {
      questionApres: (x) => 'Wéi ee Buschtaf kënnt direkt no ' + x + '?',
      questionAvant: (x) => 'Wéi ee Buschtaf kënnt direkt virun ' + x + '?',
      phraseApres: (x, y) => 'No ' + x + ' kënnt ' + y + '.',
      phraseAvant: (x, y) => 'Virun ' + x + ' kënnt ' + y + '.',
      oui: 'Jo!',
      nonApres: (l, x) => 'Nee, ' + l + ' kënnt net direkt no ' + x + '. Kuck d’Alphabet un!',
      nonAvant: (l, x) => 'Nee, ' + l + ' kënnt net direkt virun ' + x + '. Kuck d’Alphabet un!',
      ecouterQuestion: 'D’Fro lauschteren',
      frise: 'D’Alphabet',
      aTrouver: 'onbekannte Buschtaf',
      lettre: (l) => 'Buschtaf ' + l,
      choix: 'D’Äntwerten',
      consigneMots: 'Setz d’Wierder an déi alphabetesch Reiefolleg. Fänk mam éischte Wuert un!',
      astuce: 'Wann zwee Wierder gläich ufänken, kuck den nächste Buschtaf.',
      astuceTon: 'Déi selwecht Buschtawen? Da kuck d’Tounzeechen.',
      rangement: 'Deng Reiefolleg',
      aRanger: 'Wierder fir ze sortéieren',
      placeVide: (k) => 'Plaz ' + k + ': eidel',
      placePleine: (k, m) => 'Plaz ' + k + ': ' + m,
      premierOk: (m) => 'Jo! „' + m + '“ kënnt als éischt.',
      suivantOk: (m) => 'Jo! Duerno kënnt „' + m + '“.',
      rangee: 'D’Wierder sinn an der richteger Reiefolleg.',
      nonLettre: (l) => 'Nach net! En anert Wuert fänkt mat engem Buschtaf un, deen virun ' + l + ' kënnt.',
      nonMeme: (p, k) => 'Nach net! En anert Wuert fänkt och mat „' + p + '“ un: kuck de Buschtaf Nummer ' + k + '.',
      nonTon: 'Nach net! Dës Wierder hunn déi selwecht Buschtawen: kuck d’Tounzeechen.',
      finParfaite: 'Keen eenzege Feeler: Du kenns d’Alphabet ganz gutt!',
      finNormale: 'Fir all Ronn ouni Feeler gëtt et e Punkt. Trainéier weider!',
      sepListe: ', ',
    },
    fr: {
      questionApres: (x) => 'Quelle lettre vient juste après ' + x + ' ?',
      questionAvant: (x) => 'Quelle lettre vient juste avant ' + x + ' ?',
      phraseApres: (x, y) => 'Après ' + x + ', il y a ' + y + '.',
      phraseAvant: (x, y) => 'Avant ' + x + ', il y a ' + y + '.',
      oui: 'Oui !',
      nonApres: (l, x) => 'Non, ' + l + ' ne vient pas juste après ' + x + '. Regarde la frise !',
      nonAvant: (l, x) => 'Non, ' + l + ' ne vient pas juste avant ' + x + '. Regarde la frise !',
      ecouterQuestion: 'Écouter la question',
      frise: 'Frise de l’alphabet',
      aTrouver: 'lettre à trouver',
      lettre: (l) => 'Lettre ' + l,
      choix: 'Réponses possibles',
      consigneMots: 'Range les mots dans l’ordre alphabétique : touche d’abord celui qui vient en premier.',
      astuce: 'Si deux mots commencent pareil, regarde la lettre suivante.',
      astuceTon: 'Mêmes lettres ? Regarde alors les tons.',
      rangement: 'Mots rangés',
      aRanger: 'Mots à ranger',
      placeVide: (k) => 'Place ' + k + ' : vide',
      placePleine: (k, m) => 'Place ' + k + ' : ' + m,
      premierOk: (m) => 'Oui ! « ' + m + ' » vient en premier.',
      suivantOk: (m) => 'Oui ! Ensuite, « ' + m + ' ».',
      rangee: 'Les mots sont bien rangés.',
      nonLettre: (l) => 'Pas encore ! Un autre mot commence par une lettre qui vient avant ' + l + '.',
      nonMeme: (p, k) => 'Pas encore ! Un autre mot commence aussi par « ' + p + ' » : regarde la lettre n° ' + k + '.',
      nonTon: 'Pas encore ! Ces mots ont les mêmes lettres : regarde les tons.',
      finParfaite: 'Aucune erreur : tu connais l’alphabet sur le bout des doigts !',
      finNormale: 'Un point par manche réussie sans erreur. Continue de t’entraîner !',
      sepListe: ', ',
    },
  };

  // Typographie française : espace insécable avant ! ? : ; » et après «.
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, ' $1').replace(/« /g, '« ').replace(/n° /g, 'n° ');
  }

  const hanzi = () => Ile.L().ecriture === 'hanzi';

  // ---------------------------------------------------------------------------
  // Pinyin : lettres sans tons (ü conservé) et ton de chaque syllabe (1 à 4, 5 = neutre)
  // ---------------------------------------------------------------------------
  const TONS = { '̄': 1, '́': 2, '̌': 3, '̀': 4 };
  const sansTon = (s) => s.normalize('NFD').replace(/[̀-̇̉-ͯ]/g, '').normalize('NFC').toLowerCase();
  function ton(s) {
    for (const c of s.normalize('NFD')) if (TONS[c]) return TONS[c];
    return 5;
  }
  const RANG = 'abcdefghijklmnopqrstuüvwxyz';
  function cmpLettres(a, b) {
    const A = Array.from(a);
    const B = Array.from(b);
    for (let k = 0; k < Math.min(A.length, B.length); k++) {
      const d = RANG.indexOf(A[k]) - RANG.indexOf(B[k]);
      if (d) return d;
    }
    return A.length - B.length;
  }
  function cmpTons(a, b) {
    for (let k = 0; k < Math.min(a.length, b.length); k++) if (a[k] !== b[k]) return a[k] - b[k];
    return a.length - b.length;
  }

  // Un mot à ranger : { mot (affiché), cle (lettres qui comptent pour l'ordre), py (pinyin), syl, tons }.
  function item(mot) {
    if (!hanzi()) {
      return { mot, py: '', cle: Ile.sansAccents(mot).replace(/ß/g, 'ss') };
    }
    const m = (Ile.L().MOTS || []).find((x) => x.mot === mot);
    const py = m ? Ile.aide(m) : Ile.pinyin(mot);
    if (!py) return null;
    const syl = py.trim().split(/\s+/).map(sansTon);
    const tons = py.trim().split(/\s+/).map(ton);
    return { mot, py, syl, tons, cle: syl.join('') };
  }

  // Ordre du jeu : Ile.compare ; en chinois, syllabe par syllabe (lettres puis ton), comme un dictionnaire.
  function compare(a, b) {
    if (!hanzi()) return Ile.compare(a.mot, b.mot);
    for (let k = 0; k < Math.min(a.syl.length, b.syl.length); k++) {
      const d = cmpLettres(a.syl[k], b.syl[k]) || a.tons[k] - b.tons[k];
      if (d) return d;
    }
    return a.syl.length - b.syl.length;
  }
  // Ordre « lettre par lettre » des clés (celui qu'expliquent les messages d'aide).
  function compareCles(a, b) {
    if (!hanzi()) return a.cle < b.cle ? -1 : a.cle > b.cle ? 1 : 0;
    return cmpLettres(a.cle, b.cle) || cmpTons(a.tons, b.tons);
  }
  const trie = (mots) => mots.slice().sort(compare);
  // Série sans ambiguïté : les deux façons de ranger donnent le même ordre, sans ex æquo.
  function coherent(mots) {
    const a = trie(mots);
    const b = mots.slice().sort(compareCles);
    if (a.some((x, k) => x !== b[k])) return false;
    return a.every((x, k) => k === 0 || compare(a[k - 1], x) < 0);
  }

  // ---------------------------------------------------------------------------
  // Niveau 1 : les manches « juste après / juste avant »
  // ---------------------------------------------------------------------------
  function manchesLettres(alpha) {
    const n = alpha.length;
    // 5 « après » et 3 « avant » ; la première manche est toujours « après ».
    const sens = ['apres'].concat(Ile.shuffle(['apres', 'apres', 'apres', 'apres', 'avant', 'avant', 'avant']));
    // Lettre propre à la langue (hors a–z) : une manche la fait trouver.
    const speciales = alpha.map((l, i) => i).filter((i) => !/^[a-z]$/.test(alpha[i]));
    const utilisees = new Set();
    return sens.map((s, k) => {
      const possibles = [];
      for (let i = 0; i < n; i++) {
        const j = s === 'apres' ? i + 1 : i - 1;
        if (j >= 0 && j < n && !utilisees.has(i)) possibles.push(i);
      }
      let i = Ile.pick(possibles, 1)[0];
      if (k === 3 && speciales.length) {
        const sp = Ile.pick(speciales, 1)[0];
        const cand = s === 'apres' ? sp - 1 : sp + 1;
        if (cand >= 0 && cand < n && !utilisees.has(cand)) i = cand;
      }
      utilisees.add(i);
      return { sens: s, i, j: s === 'apres' ? i + 1 : i - 1 };
    });
  }

  // 4 lettres : la bonne, deux voisines de la réponse (les confusions naturelles), une au hasard.
  function choixLettres(alpha, m) {
    const n = alpha.length;
    const proches = [];
    for (let k = m.j - 3; k <= m.j + 3; k++) {
      if (k >= 0 && k < n && k !== m.i && k !== m.j) proches.push(k);
    }
    const choisis = Ile.pick(proches, 2);
    const reste = [];
    for (let k = 0; k < n; k++) if (k !== m.i && k !== m.j && choisis.indexOf(k) === -1) reste.push(k);
    return Ile.shuffle([m.j].concat(choisis, Ile.pick(reste, 3 - choisis.length)));
  }

  // ---------------------------------------------------------------------------
  // Niveaux 2 et 3 : les mots à ranger
  // ---------------------------------------------------------------------------
  function reserveMots(level) {
    const P = Ile.L();
    const vus = new Map();
    const ajoute = (w) => {
      if (typeof w !== 'string' || /[^\p{L}]/u.test(w)) return; // ni tiret, ni espace, ni apostrophe
      const it = item(w);
      if (!it || !it.cle) return;
      const cle = hanzi() ? w : it.cle;
      if (!vus.has(cle)) vus.set(cle, it);
    };
    (P.MOTS || []).forEach((m) => { if (m.niveau <= level) ajoute(m.mot); });
    for (let k = 1; k <= level; k++) ((P.MOTS_SIMPLES || {})[k] || []).forEach(ajoute);
    return Array.from(vus.values());
  }

  // Deux mots qu'on ne met jamais ensemble : même clé (sauf, si permis, deux tons différents en
  // chinois), ou l'une au début de l'autre.
  function incompatibles(a, b, tonPermis) {
    if (a.mot === b.mot) return true;
    if (a.cle === b.cle) return !(tonPermis && hanzi() && cmpTons(a.tons, b.tons) !== 0);
    return a.cle.startsWith(b.cle) || b.cle.startsWith(a.cle);
  }
  const premiere = (it) => Array.from(it.cle)[0];

  // Niveau 2 : premières lettres toutes différentes.
  function motsDifferents(pool, n) {
    for (let essai = 0; essai < 60; essai++) {
      const choisis = [];
      for (const w of Ile.shuffle(pool)) {
        if (choisis.length >= n) break;
        if (choisis.some((c) => premiere(c) === premiere(w) || incompatibles(c, w))) continue;
        choisis.push(w);
      }
      if (choisis.length === n && coherent(choisis)) return choisis;
    }
    return null;
  }

  // Niveau 3 : au moins deux mots partagent leur(s) première(s) lettre(s).
  function motsVoisins(pool, n, profondeur) {
    for (let d = profondeur; d >= 1; d--) {
      const groupes = {};
      pool.forEach((w) => {
        if (w.cle.length <= d) return;
        const k = w.cle.slice(0, d);
        (groupes[k] = groupes[k] || []).push(w);
      });
      const cles = Ile.shuffle(Object.keys(groupes).filter((k) => groupes[k].length >= 2));
      for (const k of cles) {
        for (let essai = 0; essai < 4; essai++) {
          const combien = n >= 5 && groupes[k].length >= 3 && Math.random() < 0.5 ? 3 : 2;
          const choisis = [];
          for (const w of Ile.shuffle(groupes[k])) {
            if (choisis.length >= combien) break;
            if (!choisis.some((c) => incompatibles(c, w))) choisis.push(w);
          }
          if (choisis.length < 2) break;
          for (const w of Ile.shuffle(pool)) {
            if (choisis.length >= n) break;
            if (!choisis.some((c) => incompatibles(c, w))) choisis.push(w);
          }
          if (choisis.length === n && coherent(choisis)) return choisis;
        }
      }
    }
    return null;
  }

  // Chinois, niveau 3 : deux mots de même pinyin sans tons (书 shū / 树 shù), complétés par d'autres.
  function motsTons(pool, n, dejaPris) {
    const paires = [];
    pool.forEach((a, x) => pool.forEach((b, y) => {
      if (y > x && a.cle === b.cle && !incompatibles(a, b, true) && !dejaPris.has(a.cle)) paires.push([a, b]);
    }));
    for (const paire of Ile.shuffle(paires)) {
      for (let essai = 0; essai < 20; essai++) {
        const choisis = paire.slice();
        for (const w of Ile.shuffle(pool)) {
          if (choisis.length >= n) break;
          if (!choisis.some((c) => incompatibles(c, w))) choisis.push(w);
        }
        if (choisis.length === n && coherent(choisis)) { dejaPris.add(paire[0].cle); return choisis; }
      }
    }
    return null;
  }

  // Ordre d'affichage : mélangé, jamais déjà rangé.
  function melange(mots) {
    const range = trie(mots);
    for (let k = 0; k < 20; k++) {
      const m = Ile.shuffle(mots);
      if (m.some((x, i) => x !== range[i])) return m;
    }
    return range.reverse();
  }

  function manchesMots(level) {
    const pool = reserveMots(level);
    const utilises = new Set();
    const tonsPris = new Set();
    const manches = [];
    for (let r = 0; r < TOTAL; r++) {
      const n = (level === 2 ? 3 : 4) + (r >= TOTAL / 2 ? 1 : 0);
      const libres = pool.filter((w) => !utilises.has(w));
      const source = libres.length >= 3 * n ? libres : pool;
      let mots = null;
      if (level === 3 && hanzi() && (r === 5 || r === 7)) mots = motsTons(source, n, tonsPris) || motsTons(pool, n, tonsPris);
      if (!mots) mots = level === 2 ? motsDifferents(source, n) : motsVoisins(source, n, r < 3 ? 1 : 2);
      if (!mots) mots = motsDifferents(pool, n) || motsDifferents(pool, 3);
      mots.forEach((w) => utilises.add(w));
      manches.push(melange(mots));
    }
    return manches;
  }

  // ---------------------------------------------------------------------------
  // Le jeu
  // ---------------------------------------------------------------------------
  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const P = Ile.L();
      const zh = hanzi();
      const locale = P.htmlLang || P.tts;
      const alpha = P.alphabet.slice();
      const HAUT = alpha.map((l) => l.toLocaleUpperCase(locale));
      const manches = level === 1 ? manchesLettres(alpha) : manchesMots(level);
      let q = 0;
      let score = 0;

      const panel = el('section', { class: 'panel alp alp--n' + level, 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      const actif = () => panel.isConnected;
      // Le focus était dans le jeu (un bouton désactivé le rend au document) : on le garde dans le jeu.
      const focusDansLeJeu = () => !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);
      const pinyinEl = (p, cls) => el('span', { class: 'pinyin' + (cls ? ' ' + cls : ''), lang: 'zh-Latn-pinyin', text: p });

      // Frise de l'alphabet (aide).
      function frise(repere, cachee) {
        const ol = el('ol', { class: 'alp-frise', 'aria-label': tx.frise, style: '--nl:' + alpha.length });
        HAUT.forEach((L, k) => {
          const li = el('li', { class: 'alp-frise__l', 'data-i': String(k) });
          if (k === cachee) {
            li.classList.add('is-cachee');
            li.append(el('span', { 'aria-hidden': 'true', text: '?' }), el('span', { class: 'sr-only', text: tx.aTrouver }));
          } else {
            li.textContent = L;
          }
          if (k === repere) li.classList.add('is-repere');
          ol.appendChild(li);
        });
        return ol;
      }

      function fin() {
        if (!actif()) return;
        Ile.progress(panel, TOTAL, TOTAL, score);
        Ile.showResult({ id: ID, score, total: TOTAL, message: typo(score === TOTAL ? tx.finParfaite : tx.finNormale) });
      }

      function suivante() {
        if (!actif()) return;
        if (q >= manches.length) { fin(); return; }
        const garderFocus = focusDansLeJeu();
        panel.querySelectorAll(':scope > :not(.progress)').forEach((x) => x.remove());
        Ile.progress(panel, q, TOTAL, score);
        if (level === 1) mancheLettre(manches[q]); else mancheMots(manches[q]);
        if (garderFocus && q > 0) {
          const b = panel.querySelector('.alp-lettre, .alp-tuile');
          if (b) b.focus({ preventScroll: true });
        }
      }

      // Attend la fin de la lecture en cours (au plus quelques secondes de plus) avant d'enchaîner :
      // la manche suivante du niveau 1 lit sa question et la fin de partie son titre, ce qui couperait
      // « Après W, il y a X. » ou la liste rangée avant qu'on entende la réponse.
      function apresLecture(fn) {
        const limite = Date.now() + PAUSE + 4000;
        const tic = () => {
          if (!actif()) return;
          let parle = false;
          try { parle = !!window.speechSynthesis.speaking; } catch (e) { parle = false; }
          if (parle && Date.now() < limite) { setTimeout(tic, 150); return; }
          fn();
        };
        setTimeout(tic, PAUSE);
      }

      function terminer(sansErreur) {
        if (sansErreur) score++;
        Ile.progress(panel, q + 1, TOTAL, score);
        q++;
        // Niveaux 2 et 3 : la manche suivante ne lit rien, on n'attend la lecture qu'avant la fin.
        if (level === 1 || q >= manches.length) apresLecture(suivante);
        else setTimeout(suivante, PAUSE);
      }

      // --- Niveau 1 : quelle lettre vient juste après / avant ? ---
      function mancheLettre(m) {
        const X = HAUT[m.i];
        const Y = HAUT[m.j];
        const apres = m.sens === 'apres';
        // La lettre ne se sépare jamais du mot qui la précède (« direkt no W? » reste sur une ligne).
        const q0 = typo(apres ? tx.questionApres(X) : tx.questionAvant(X));
        const cut = q0.lastIndexOf(' ' + X);
        const question = cut < 0 ? q0 : q0.slice(0, cut) + '\u00A0' + q0.slice(cut + 1);
        let erreurs = 0;
        let fini = false;

        const bulle = el('div', { class: 'alp-manche', 'data-manche': String(q) });
        const idQuestion = 'alp-question-' + q;
        const qEl = el('p', { class: 'consigne alp-question', 'data-lettre': X, 'data-sens': m.sens }, [
          el('span', { id: idQuestion, text: question }),
          Ile.speakButton(question, tx.ecouterQuestion),
        ]);
        const inconnue = el('span', { class: 'alp-carte alp-carte--inconnue', text: '?' });
        const duo = el('div', { class: 'alp-duo', 'aria-hidden': 'true' }, apres
          ? [el('span', { class: 'alp-carte', text: X }), el('span', { class: 'alp-fleche', text: '→' }), inconnue]
          : [inconnue, el('span', { class: 'alp-fleche', text: '←' }), el('span', { class: 'alp-carte', text: X })]);
        const f = frise(m.i, m.j);
        // La question est lue par les lecteurs d'écran quand le focus arrive sur les réponses.
        const grille = el('div', { class: 'alp-choix', role: 'group', 'aria-label': tx.choix, 'aria-describedby': idQuestion });
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });

        choixLettres(alpha, m).forEach((k) => {
          const L = HAUT[k];
          const b = el('button', { type: 'button', class: 'btn choice alp-lettre', 'aria-label': tx.lettre(L), 'data-lettre': L, text: L });
          b.addEventListener('click', () => {
            if (fini || b.disabled || !actif()) return;
            if (k === m.j) {
              fini = true;
              b.classList.add('is-correct');
              grille.querySelectorAll('button').forEach((x) => { if (x !== b) x.disabled = true; });
              Ile.flash(b, 'good');
              Ile.sfx('good');
              inconnue.textContent = Y;
              inconnue.classList.add('is-trouvee');
              const cell = f.querySelector('[data-i="' + m.j + '"]');
              cell.textContent = Y;
              cell.classList.remove('is-cachee');
              cell.classList.add('is-trouvee');
              const phrase = typo(apres ? tx.phraseApres(X, Y) : tx.phraseAvant(X, Y));
              Ile.feedback(fb, true, typo(tx.oui) + P.sepMots + phrase);
              Ile.say(phrase);
              terminer(erreurs === 0);
            } else {
              erreurs++;
              b.classList.add('is-wrong');
              b.disabled = true;
              Ile.flash(b, 'bad');
              Ile.sfx('bad');
              Ile.feedback(fb, false, typo(apres ? tx.nonApres(L, X) : tx.nonAvant(L, X)));
              const reste = grille.querySelector('button:not(:disabled)');
              if (reste && focusDansLeJeu()) reste.focus({ preventScroll: true });
            }
          });
          grille.appendChild(b);
        });

        bulle.append(qEl, duo, f, grille, fb);
        panel.appendChild(bulle);
        Ile.say(question);
      }

      // Contenu d'un mot : caractères + pinyin en chinois ; première lettre en couleur au niveau 2.
      function contenuMot(it, marquer) {
        if (zh) {
          const py = marquer
            ? el('span', { class: 'pinyin alp-py', lang: 'zh-Latn-pinyin' }, [el('span', { class: 'alp-init', text: Array.from(it.py)[0] }), it.py.slice(Array.from(it.py)[0].length)])
            : pinyinEl(it.py, 'alp-py');
          return [el('span', { class: 'alp-hanzi', text: it.mot }), py];
        }
        if (!marquer) return [el('span', { class: 'alp-mot', text: it.mot })];
        const p = Array.from(it.mot)[0];
        return [el('span', { class: 'alp-mot' }, [el('span', { class: 'alp-init', text: p }), it.mot.slice(p.length)])];
      }

      // --- Niveaux 2 et 3 : touche les mots dans l'ordre alphabétique ---
      function mancheMots(mots) {
        const ordre = trie(mots);
        const tonsEgaux = zh && mots.some((a) => mots.some((b) => a !== b && a.cle === b.cle));
        let rang = 0;
        let erreurs = 0;
        let fini = false;

        const bulle = el('div', { class: 'alp-manche', 'data-manche': String(q) });
        const consigne = el('p', { class: 'consigne', text: typo(tx.consigneMots) });
        const astuce = tonsEgaux ? tx.astuceTon : level >= 3 ? tx.astuce : '';
        const places = el('ol', { class: 'alp-places', 'aria-label': tx.rangement });
        const cases = ordre.map((it, k) => {
          const li = el('li', { class: 'alp-place', 'aria-label': typo(tx.placeVide(k + 1)) }, [
            el('span', { class: 'alp-place__n', 'aria-hidden': 'true', text: String(k + 1) }),
            el('span', { class: 'alp-place__mot' }),
          ]);
          places.appendChild(li);
          return li;
        });
        const tuiles = el('div', { class: 'alp-tuiles', role: 'group', 'aria-label': tx.aRanger });
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
        const f = frise(-1, -1);

        mots.forEach((it) => {
          const b = el('button', { type: 'button', class: 'tile alp-tuile', 'data-mot': it.mot, 'aria-label': it.mot }, contenuMot(it, level === 2));
          b.addEventListener('click', (ev) => {
            if (ev.detail > 1 || fini || b.disabled || !actif()) return; // double clic : un seul coup
            const attendu = ordre[rang];
            if (it === attendu) {
              b.disabled = true;
              b.classList.add('is-placee');
              const li = cases[rang];
              li.querySelector('.alp-place__mot').replaceChildren(...contenuMot(it, false));
              li.classList.add('is-pleine');
              li.setAttribute('aria-label', typo(tx.placePleine(rang + 1, it.mot)));
              Ile.flash(li, 'good');
              const cell = f.querySelector('[data-i="' + alpha.indexOf(premiere(it)) + '"]');
              if (cell) cell.classList.add('is-utilisee');
              rang++;
              if (rang === ordre.length) {
                fini = true;
                Ile.sfx('good');
                Ile.feedback(fb, true, typo(Ile.pick(Ile.t('bravo'), 1)[0] + P.sepMots + tx.rangee));
                Ile.say(ordre.map((x) => x.mot).join(tx.sepListe));
                terminer(erreurs === 0);
              } else {
                Ile.sfx('click');
                Ile.feedback(fb, true, typo(rang === 1 ? tx.premierOk(it.mot) : tx.suivantOk(it.mot)));
                Ile.say(it.mot);
                const reste = tuiles.querySelector('.alp-tuile:not(:disabled)');
                if (reste && focusDansLeJeu()) reste.focus({ preventScroll: true });
              }
            } else {
              erreurs++;
              Ile.flash(b, 'bad');
              Ile.sfx('bad');
              // Explication : première lettre différente, début commun, ou mêmes lettres (tons).
              const A = Array.from(it.cle);
              const B = Array.from(attendu.cle);
              let k = 0;
              while (k < A.length && k < B.length && A[k] === B[k]) k++;
              let msg;
              if (k === 0) msg = tx.nonLettre(A[0].toLocaleUpperCase(locale));
              else if (k >= A.length && k >= B.length) msg = tx.nonTon;
              else {
                // Début commun écrit comme sur la tuile (majuscule de Katze), sauf si le mot a une lettre
                // qui compte double pour l'ordre (ß = ss) : alors les lettres de la clé.
                const lettres = Array.from(it.mot);
                const debut = !zh && lettres.length === A.length ? lettres.slice(0, k) : A.slice(0, k);
                msg = tx.nonMeme(debut.join(''), k + 1);
              }
              Ile.feedback(fb, false, typo(msg));
            }
          });
          tuiles.appendChild(b);
        });

        bulle.append(consigne);
        if (astuce) bulle.appendChild(el('p', { class: 'alp-astuce', text: '💡 ' + typo(astuce) }));
        bulle.append(places, tuiles, fb, f);
        panel.appendChild(bulle);
      }

      suivante();
    },
  });
})();
