/* La lettre perdue : retrouve la ou les lettres qui manquent dans le mot (le caractère, en chinois).
 * Niveau 1 : une voyelle bien audible manque (voyelle simple de Ile.L().voyelles), 3 choix.
 * Niveau 2 : une lettre qui s'entend manque, 4 choix.
 * Niveau 3 : on alterne une lettre spéciale de la langue à choisir parmi ses « cousines » (é è ê… en
 *            français, ä ö ü ß en allemand, ä é ë en luxembourgeois) et deux lettres voisines à compléter
 *            dans l'ordre (ch, ou, sh, ei, ck, ll, tr…) ; sans lettre spéciale (anglais), toujours deux lettres.
 * Allemand et luxembourgeois : le nom garde sa majuscule (Katze, Kaz), qui n'est jamais cachée.
 * Chinois : un CARACTÈRE manque dans un mot de deux caractères ou plus (trois ou quatre au niveau 3).
 *           Le pinyin complet s'affiche sous le mot aux niveaux 1 et 2 (et sous chaque choix au niveau 1),
 *           il est caché au niveau 3 jusqu'à la réponse. Les choix sont des caractères d'autres mots du pack.
 * Aucun distracteur ne fabrique un autre vrai mot (mots du pack + AUTRES_MOTS) ni ne se prononce pareil.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'lettre';
  const TOTAL = 10;
  const PAUSE = 1700;
  const NB = ' ';

  // ---------------------------------------------------------------------------
  // Textes propres au jeu, par langue (mêmes clés partout).
  // ---------------------------------------------------------------------------
  const T = {
    en: {
      cite: (t) => '“' + t + '”',
      consigne: {
        voyelle: 'Which vowel is missing?',
        lettre: 'Which letter is missing?',
        accent: () => 'Which letter is missing?',
        deux: 'Two letters are missing. Fill in the boxes in order.',
        caractere: 'Which character is missing?',
      },
      image: 'Picture of the word',
      imageDe: (mot) => 'Picture: ' + mot,
      caseVide: 'missing letter',
      tuiles: 'Letters to choose from',
      ecouterMot: 'Listen to the word',
      oui: 'Yes!',
      onEcrit: (w) => 'It’s spelt ' + T.en.cite(w) + '.',
      suivante: 'Yes! Now the next letter.',
      plusTard: (l) => 'The ' + T.en.cite(l) + ' comes later. Which letter comes first?',
      pasCa: (l) => 'It isn’t ' + T.en.cite(l) + '.',
      conseilAccent: () => 'Look carefully and try again!',
      conseilEcoute: 'Listen to the word carefully and try again!',
      conseilLis: 'Read the word again carefully and try again!',
      conseilImage: 'Look at the picture and try again!',
      parfait: 'No letter gets past you!',
      astuceEcoute: 'Tip: tap 🔈 to hear the word before you choose.',
      astuceLis: 'Tip: sound out the word quietly before you choose.',
      nomLettre: {},
    },
    zh: {
      cite: (t) => '“' + t + '”',
      consigne: {
        voyelle: '缺了哪个字？',
        lettre: '缺了哪个字？',
        accent: () => '缺了哪个字？',
        deux: '缺了哪个字？',
        caractere: '缺了哪个字？',
      },
      image: '这个词的图片',
      imageDe: (mot) => '图片：' + mot,
      caseVide: '缺少的字',
      tuiles: '可以选的字',
      ecouterMot: '听一听这个词',
      oui: '对了！',
      onEcrit: (w) => '这个词是' + T.zh.cite(w) + '。',
      suivante: '对了！下一个字呢？',
      plusTard: (l) => T.zh.cite(l) + '在后面。先选前面的字！',
      pasCa: (l) => '不是' + T.zh.cite(l) + '。',
      conseilAccent: () => '仔细看一看，再试一次！',
      conseilEcoute: '听一听这个词，再试一次！',
      conseilLis: '看看拼音，再试一次！',
      conseilImage: '看看图片，再想一想！',
      parfait: '一个字也没有难倒你！',
      astuceEcoute: '小提示：选字以前，先点 🔈 听一听这个词。',
      astuceLis: '小提示：先看看图片，想一想这个词怎么说，再选字。',
      nomLettre: {},
    },
    de: {
      cite: (t) => '„' + t + '“',
      consigne: {
        voyelle: 'Welcher Vokal fehlt?',
        lettre: 'Welcher Buchstabe fehlt?',
        accent: () => 'Welcher Buchstabe fehlt? Schau genau hin!',
        deux: 'Zwei Buchstaben fehlen. Füll die Kästchen der Reihe nach aus.',
        caractere: 'Welcher Buchstabe fehlt?',
      },
      image: 'Bild zum Wort',
      imageDe: (mot) => 'Bild: ' + mot,
      caseVide: 'fehlender Buchstabe',
      tuiles: 'Buchstaben zur Auswahl',
      ecouterMot: 'Wort anhören',
      oui: 'Ja!',
      onEcrit: (w) => 'Das Wort heißt ' + T.de.cite(w) + '.',
      suivante: 'Ja! Und jetzt der nächste Buchstabe.',
      plusTard: (l) => 'Das ' + T.de.cite(l) + ' kommt später. Welcher Buchstabe kommt zuerst?',
      pasCa: (l) => 'Nicht ' + T.de.cite(l) + '.',
      conseilAccent: () => 'Schau genau hin und versuch es noch einmal!',
      conseilEcoute: 'Hör dir das Wort gut an und versuch es noch einmal!',
      conseilLis: 'Lies das Wort langsam und versuch es noch einmal!',
      conseilImage: 'Schau dir das Bild an und versuch es noch einmal!',
      parfait: 'Dir entgeht kein einziger Buchstabe!',
      astuceEcoute: 'Tipp: Tippe auf 🔈 und hör dir das Wort an, bevor du wählst.',
      astuceLis: 'Tipp: Lies das Wort leise, Laut für Laut, bevor du wählst.',
      nomLettre: { ä: 'A-Umlaut', ö: 'O-Umlaut', ü: 'U-Umlaut', ß: 'Eszett' },
    },
    lb: {
      cite: (t) => '„' + t + '“',
      consigne: {
        voyelle: 'Wat fir e Vokal feelt?',
        lettre: 'Wat fir e Buschtaf feelt?',
        accent: () => 'Wat fir e Buschtaf feelt? Kuck gutt!',
        deux: 'Zwee Buschtawe feelen. Setz se an déi richteg Reiefolleg.',
        caractere: 'Wat fir e Buschtaf feelt?',
      },
      image: 'Bild vum Wuert',
      imageDe: (mot) => 'Bild: ' + mot,
      caseVide: 'Hei feelt e Buschtaf',
      tuiles: 'Buschtawen',
      ecouterMot: 'D’Wuert lauschteren',
      oui: 'Jo!',
      onEcrit: (w) => 'D’Wuert ass ' + T.lb.cite(w) + '.',
      suivante: 'Jo! Elo den nächste Buschtaf.',
      plusTard: (l) => T.lb.cite(l) + ' kënnt méi spéit. Wat fir e Buschtaf kënnt als éischt?',
      pasCa: (l) => 'Net ' + T.lb.cite(l) + '.',
      conseilAccent: () => 'Kuck gutt a probéier nach eng Kéier!',
      conseilEcoute: 'Lauschter gutt op d’Wuert a probéier nach eng Kéier!',
      conseilLis: 'Lies d’Wuert lues a probéier nach eng Kéier!',
      conseilImage: 'Kuck d’Bild un a probéier nach eng Kéier!',
      parfait: 'Du hues all d’Buschtawe fonnt!',
      astuceEcoute: 'Tipp: Dréck op 🔈 a lauschter d’Wuert.',
      astuceLis: 'Tipp: Lies d’Wuert lues, Laut fir Laut.',
      nomLettre: {},
    },
    fr: {
      cite: (t) => '«' + NB + t + NB + '»',
      consigne: {
        voyelle: 'Quelle voyelle manque' + NB + '?',
        lettre: 'Quelle lettre manque' + NB + '?',
        accent: () => 'Quelle lettre manque' + NB + '? Attention aux accents' + NB + '!',
        deux: 'Deux lettres manquent. Complète les cases dans l’ordre.',
        caractere: 'Quel caractère manque' + NB + '?',
      },
      image: 'Image du mot',
      imageDe: (mot) => 'Image' + NB + ': ' + mot,
      caseVide: 'lettre manquante',
      tuiles: 'Lettres proposées',
      ecouterMot: 'Écouter le mot',
      oui: 'Oui' + NB + '!',
      onEcrit: (w) => 'On écrit ' + T.fr.cite(w) + '.',
      suivante: 'Oui' + NB + '! Et la lettre suivante' + NB + '?',
      plusTard: (l) => 'Le ' + T.fr.cite(l) + ' vient plus tard. Quelle lettre vient d’abord' + NB + '?',
      pasCa: (l) => 'Ce n’est pas ' + T.fr.cite(l) + '.',
      conseilAccent: () => 'Regarde bien l’accent et écoute le son' + NB + '!',
      conseilEcoute: 'Écoute bien le mot et essaie encore' + NB + '!',
      conseilLis: 'Relis bien le mot et essaie encore' + NB + '!',
      conseilImage: 'Regarde bien l’image et essaie encore' + NB + '!',
      parfait: 'Aucune lettre ne t’échappe' + NB + '!',
      astuceEcoute: 'Astuce' + NB + ': appuie sur 🔈 pour écouter le mot avant de choisir.',
      astuceLis: 'Astuce' + NB + ': lis le mot tout bas, son par son, avant de choisir.',
      nomLettre: {
        é: 'e accent aigu', è: 'e accent grave', ê: 'e accent circonflexe', ë: 'e tréma',
        à: 'a accent grave', â: 'a accent circonflexe', ä: 'a tréma', î: 'i accent circonflexe', ï: 'i tréma',
        ô: 'o accent circonflexe', ö: 'o tréma', û: 'u accent circonflexe', ù: 'u accent grave', ü: 'u tréma',
        ç: 'c cédille', ÿ: 'y tréma',
      },
    },
  };

  // ---------------------------------------------------------------------------
  // Règles de lecture propres à chaque langue à alphabet (aucun texte affiché ici).
  // Elles reçoivent le mot en minuscules :
  //   voyelle(w, i, niveau) : voyelle simple qui s'entend clairement (on peut la retirer)
  //   consonne(w, i)        : consonne qui s'entend (ni muette, ni prise dans un digramme ou une lettre double)
  //   exclusions(w, i)      : distracteurs interdits parce qu'ils se prononceraient pareil (c / k, d / t…)
  //   paire(w, i)           : chaîne des distracteurs interdits si w[i] w[i+1] forme un groupe à compléter
  //                           au niveau 3 (ch, ou, sh, ee, ei, ck, ll, tr…), sinon null
  //   consonnes             : consonnes proposées comme distracteurs
  //   rares                 : lettres proposées comme distracteurs seulement en dernier recours
  // ---------------------------------------------------------------------------

  // --- Français ---
  const FR = (function () {
    const VOY = 'aeiouyàâäéèêëîïôöùûüœæ';
    const estV = (c) => !!c && VOY.indexOf(c) !== -1;
    const CONSONNES_OK = 'bcdfgjklmnprstvz';
    // Graphèmes de deux lettres et lettres interdites parce qu'elles donneraient une écriture qui se prononce pareil.
    const PAIRES = {
      ph: 'f', gn: 'iy', qu: 'ck', gu: 'j', ch: 'sk',
      ou: 'w', oi: 'aw', au: 'oe', eu: 'o', ai: 'e', ei: 'a',
      an: 'em', am: 'en', en: 'am', em: 'an', on: 'm', om: 'n', in: 'aemy', im: 'aeny',
      ss: 'cz', ll: '', rr: '', tt: '', mm: 'n', nn: 'm', pp: '', ff: '', zz: 's',
    };
    // La voyelle w[i] suivie de n / m forme-t-elle un son nasal (an, on, in, am…) ?
    function nasale(w, i) {
      const n = w[i + 1];
      if (n !== 'n' && n !== 'm') return false;
      const apres = w[i + 2];
      return !apres || (!estV(apres) && apres !== 'n' && apres !== 'm' && apres !== 'h');
    }
    // Lettre muette en fin de mot (chat, loup, canard, souris, nez, palmier…).
    function finaleMuette(w, i) {
      if (i !== w.length - 1) return false;
      return 'tdspxzg'.indexOf(w[i]) !== -1 || (w[i] === 'r' && w.length > 3 && w[i - 1] === 'e');
    }
    // l « mouillé » : ill (papillon, feuille), eil / ail en fin de mot (soleil).
    function lMouille(w, i) {
      if (w[i] !== 'l') return false;
      if (w[i - 1] === 'i' && w[i + 1] === 'l') return true;
      if (w[i - 1] === 'l' && w[i - 2] === 'i') return true;
      return w[i - 1] === 'i' && estV(w[i - 2]) && i === w.length - 1;
    }
    // Lettre prise dans un graphème complexe (ch, ph, gn, qu, gu, an, on, ill…) ou muette.
    function dansGrapheme(w, i) {
      const c = w[i];
      const p = w[i - 1] || '';
      const n = w[i + 1] || '';
      if ('hqyxw'.indexOf(c) !== -1) return true;
      if ('cpst'.indexOf(c) !== -1 && n === 'h') return true;
      if ((c === 'g' && n === 'n') || (c === 'n' && p === 'g')) return true;
      if (c === 'u' && (p === 'q' || (p === 'g' && /[eiyéèê]/.test(n)))) return true;
      if (c === 'g' && n === 'u' && /[eiyéèê]/.test(w[i + 2] || '')) return true;
      if (lMouille(w, i)) return true;
      if ((c === 'n' || c === 'm') && estV(p) && nasale(w, i - 1)) return true;
      return false;
    }
    // Voyelle simple qui s'entend clairement : l_pin, t_mate, ch_val (le e seulement à partir du niveau 2).
    function voyelle(w, i, niveau) {
      const c = w[i];
      if ('aiou'.indexOf(c) === -1 && !(niveau >= 2 && c === 'e')) return false;
      if (estV(w[i - 1]) || estV(w[i + 1])) return false; // ou, ai, oi, eau, ie, ui…
      if (nasale(w, i) || dansGrapheme(w, i)) return false;
      if (c === 'i' && w[i + 1] === 'l' && w[i + 2] === 'l') return false; // papillon
      if (c === 'e' && (i === 0 || !w[i + 1] || estV(w[i + 1]) || !estV(w[i + 2]))) return false; // e de « cheval » seulement
      return true;
    }
    function consonne(w, i) {
      return CONSONNES_OK.indexOf(w[i]) !== -1 && !dansGrapheme(w, i) && !finaleMuette(w, i);
    }
    function exclusions(w, i) {
      const c = w[i];
      const n = w[i + 1] || '';
      const doux = /[eiyéèêëîï]/.test(n);
      switch (c) {
        case 'c': return doux ? 'sçz' : 'kq';
        case 'k': case 'q': return 'ckq';
        case 's': return estV(w[i - 1]) && estV(n) ? 'z' : (doux ? 'cç' : 'ç');
        case 'z': return 'sd'; // pizza se prononce [pidza]
        case 'g': return doux ? 'j' : '';
        case 'j': return doux ? 'g' : '';
        case 'i': return 'y';
        case 'y': return 'i';
        default: return '';
      }
    }
    function paire(w, i) {
      const p = w.substr(i, 2);
      if (p.length !== 2 || !Object.prototype.hasOwnProperty.call(PAIRES, p)) return null;
      const avant = w[i - 1];
      const apres = w[i + 2];
      const a = p[0];
      const b = p[1];
      let ok;
      if (estV(a) && (b === 'n' || b === 'm')) ok = !estV(avant) && nasale(w, i); // an, on, in…
      else if (estV(a) && estV(b)) { // ou, oi, au, eu, ai, ei
        ok = !estV(avant) && // eau, ieu…
          !((apres === 'n' || apres === 'm') && nasale(w, i + 1)) && // ain, oin, ein
          !(estV(apres) && !(apres === 'i' && w[i + 3] === 'l' && w[i + 4] === 'l')); // sauf « ouille », « euille »
      } else if (p === 'gu') ok = /[eiyéèê]/.test(apres || '');
      else if (a === b) ok = !finaleMuette(w, i + 1);
      else ok = true;
      return ok ? PAIRES[p] : null;
    }
    // y : voyelle du pack, mais distracteur seulement en dernier recours (« banyne » n'aide personne).
    return { voyelle, consonne, exclusions, paire, consonnes: 'bcdfglmnprstv', rares: 'y' };
  })();

  // --- Anglais (britannique) ---
  const EN = (function () {
    const estV = (c) => !!c && 'aeiou'.indexOf(c) !== -1;
    // Mots dont la première voyelle ne se prononce pas comme elle s'écrit (voyelle réduite, o dit « u »).
    const FLOUES = ['banana', 'potato', 'tomato', 'monkey', 'onion', 'giraffe', 'mother', 'brother', 'money'];
    // Graphèmes et groupes de deux lettres, avec les distracteurs qui donneraient le même son.
    const PAIRES = {
      sh: '', ch: 'tk', th: '', ph: 'f', wh: '', ck: 'kq', ng: '', qu: 'kcw',
      ee: 'aiy', ea: 'i', oo: 'uew', ai: 'ye', ay: 'ie', oa: 'we', ow: 'ua', ou: 'w', oy: 'i', oi: 'y', aw: 'uo', ew: 'uo',
      ar: '', or: 'aw', er: 'iuo', ir: 'euo', ur: 'eio',
      ll: '', ss: 'cz', tt: '', pp: '', bb: '', dd: '', gg: '', mm: 'n', nn: 'm', rr: '', zz: 's', ff: '',
      bl: '', br: '', cl: 'kq', cr: 'kq', dr: '', fl: '', fr: '', gl: '', gr: '', pl: '', pr: '', tr: '', tw: '',
      sc: 'k', sk: 'c', sl: '', sm: '', sn: '', sp: '', st: '', sw: '', nd: '', nt: '', mp: 'n', lt: '', ld: '', ft: '', ct: 'k', pt: '',
    };
    // Voyelle seule (pas ee, oa, ow, ay…), première du mot : c'est la plus nette (cat, rocket, panda).
    function voyelle(w, i) {
      const c = w[i];
      if (!estV(c) || i !== w.search(/[aeiou]/)) return false;
      if (estV(w[i - 1]) || estV(w[i + 1]) || /[wy]/.test(w[i + 1] || '')) return false;
      if (w[i - 1] === 'w' || (w[i - 1] === 'u' && w[i - 2] === 'q')) return false; // water, squash
      if (c === 'e' && i === w.length - 1) return false; // e muet
      return FLOUES.indexOf(w) === -1;
    }
    function consonne(w, i) {
      const c = w[i];
      const p = w[i - 1] || '';
      const n = w[i + 1] || '';
      if (!c || estV(c) || 'bcdfghjklmnpqrstvwxz'.indexOf(c) === -1) return false;
      if (c === n || c === p) return false; // lettres doubles (rabbit, shell, pizza)
      if (c === 'h') return i === 0 ? !/^(hour|hon|heir)/.test(w) : !/[cstpwgr]/.test(p) && estV(n);
      if (n === 'h' && /[cstpwg]/.test(c)) return false; // ch, sh, th, ph, wh, gh
      if (c === 'c' && n === 'k') return false; // ck
      if (c === 'k' && (p === 'c' || (i === 0 && n === 'n'))) return false; // ck, kn
      if (c === 'w' && (n === 'r' || estV(p))) return false; // wr, ow, aw, ew
      if (c === 'q') return false; // qu
      if (c === 'g' && ((i === 0 && n === 'n') || (p === 'n' && !estV(n)))) return false; // gn, -ng
      if (c === 'n' && (n === 'g' || n === 'k')) return false; // ng, nk
      if (c === 'd' && n === 'g' && /[eiy]/.test(w[i + 2] || '')) return false; // dge (hedgehog)
      if (c === 'r' && estV(p) && !estV(n)) return false; // r muet après une voyelle (star, horse, tiger)
      if (c === 't' && p === 's' && n === 'l') return false; // castle
      if (c === 's' && p === 'i' && n === 'l') return false; // island
      if (c === 'd' && p === 'n' && n === 'w') return false; // sandwich
      if (c === 'b' && p === 'm' && !n) return false; // lamb
      return true;
    }
    function exclusions(w, i) {
      const c = w[i];
      const n = w[i + 1] || '';
      const doux = /[eiy]/.test(n);
      switch (c) {
        case 'c': return doux ? 'sz' : 'kq';
        case 'k': return 'cq';
        case 's': return doux ? 'cz' : 'z';
        case 'z': return 's';
        case 'g': return doux ? 'j' : '';
        case 'j': return 'g';
        case 'e': case 'i': case 'u': return n === 'r' && !estV(w[i + 2]) ? 'eiu' : ''; // er, ir, ur : même son
        default: return '';
      }
    }
    function paire(w, i) {
      const p = w.substr(i, 2);
      if (p.length !== 2 || !Object.prototype.hasOwnProperty.call(PAIRES, p)) return null;
      const a = p[0];
      const b = p[1];
      const avant = w[i - 1] || '';
      const apres = w[i + 2] || '';
      let ok;
      if (estV(a) && (estV(b) || b === 'w' || b === 'y')) ok = !estV(avant) && !estV(apres); // ee, oa, ow, ay…
      else if (estV(a) && b === 'r') {
        // ar, or, er… (star, horse, butterfly) ; en syllabe faible (anchor, oyster), ar / or / er / ur
        // se prononcent pareil : aucune autre voyelle comme distracteur.
        return !estV(avant) && !estV(apres) ? PAIRES[p] + 'aeiou' : null;
      } else if (a === b || b === 'h' || p === 'ck' || p === 'ng' || p === 'qu') ok = true; // doubles et digrammes
      else ok = consonne(w, i) && consonne(w, i + 1); // groupes de consonnes (tr, st, nd…)
      return ok ? PAIRES[p] + exclusions(w, i) + exclusions(w, i + 1) : null;
    }
    return { voyelle, consonne, exclusions, paire, consonnes: 'bcdfgklmnprstvw', rares: '' };
  })();

  // --- Allemand ---
  const DE = (function () {
    const estV = (c) => !!c && 'aeiouäöüy'.indexOf(c) !== -1;
    const premiere = (w) => w.search(/[aeiouäöü]/);
    // Groupes de deux lettres à compléter au niveau 3, avec les lettres interdites parce qu'elles
    // donneraient une écriture qui se prononce pareil (ei / ai, eu / äu, ie / ih, ck / kk, tz / zz…).
    const PAIRES = {
      ei: 'a', ie: 'h', au: '', eu: 'ä', ee: 'h', aa: 'h', oo: 'h',
      ch: 'g', ck: 'k', ng: '', pf: '', tz: 'z', st: '', sp: '',
      ll: '', mm: '', nn: '', pp: '', ss: 'ß', tt: '', ff: '', rr: '',
      bl: '', br: '', dr: '', fl: '', fr: '', gl: '', gr: '', kl: '', kr: '', pl: '', pr: '', tr: '',
      nd: '', nt: '', ld: '', lt: '', ft: '', mp: '',
    };
    // Voyelle simple bien audible : ni diphtongue (ei, ie, au, eu), ni voyelle double (ee, aa, oo),
    // ni e réduit en fin de syllabe (Katze, Vogel, Tiger) : le e seulement comme première voyelle (Bett, Zelt).
    function voyelle(w, i) {
      const c = w[i];
      if ('aeiou'.indexOf(c) === -1) return false;
      if (estV(w[i - 1]) || estV(w[i + 1])) return false;
      return c !== 'e' || i === premiere(w);
    }
    function consonne(w, i) {
      const c = w[i];
      const p = w[i - 1] || '';
      const n = w[i + 1] || '';
      if ('bdfgklmnprstwz'.indexOf(c) === -1) return false; // c, h, j, q, v, x, y, ß : graphèmes ou sons ambigus
      if (c === p || c === n) return false; // lettres doubles (Ball, Bett, Affe)
      if (c === 's' && /[cpt]/.test(n)) return false; // sch, sp, st
      if (c === 'n' && /[gk]/.test(n)) return false; // ng, nk
      if (c === 'g' && p === 'n') return false;
      if ((c === 't' && n === 'z') || (c === 'z' && p === 't')) return false; // tz
      if (c === 'k' && p === 'c') return false; // ck
      if ((c === 'p' && n === 'f') || (c === 'f' && p === 'p')) return false; // pf
      if (c === 'r' && estV(p) && !estV(n)) return false; // r vocalisé (Tür, Tiger, Ohr)
      return true;
    }
    function exclusions(w, i) {
      switch (w[i]) {
        case 'd': return 't'; // Hund se prononce avec un t
        case 't': return 'd';
        case 'b': return 'p';
        case 'p': return 'b';
        case 'g': return 'kc';
        case 'k': return 'gc';
        case 'f': return 'v';
        case 'v': return 'fw';
        case 'w': return 'v';
        case 's': return 'zß';
        case 'z': return 'c';
        case 'e': return 'ä';
        case 'ä': return 'e';
        case 'i': return 'y';
        default: return '';
      }
    }
    function paire(w, i) {
      const p = w.substr(i, 2);
      if (p.length !== 2 || !Object.prototype.hasOwnProperty.call(PAIRES, p)) return null;
      const a = p[0];
      const b = p[1];
      let ok;
      if (estV(a) && estV(b)) ok = !estV(w[i - 1]) && !estV(w[i + 2]); // ei, ie, au, eu, ee (pas « eie »)
      else if (a === b || ['ch', 'ck', 'ng', 'pf', 'tz'].indexOf(p) !== -1) ok = true; // doubles et graphèmes
      else if (p === 'st' || p === 'sp') ok = w[i - 1] !== 's';
      else ok = consonne(w, i) && consonne(w, i + 1); // groupes de consonnes (tr, bl, nd…)
      return ok ? PAIRES[p] + exclusions(w, i) + exclusions(w, i + 1) : null;
    }
    return { voyelle, consonne, exclusions, paire, consonnes: 'bdfgklmnprstwz', rares: '' };
  })();

  // --- Luxembourgeois ---
  const LB = (function () {
    const estV = (c) => !!c && 'aeiouäéëy'.indexOf(c) !== -1;
    const premiere = (w) => w.search(/[aeiouäéë]/);
    // ei et äi se prononcent pareil ; ck / kk, tz / zz aussi.
    const PAIRES = {
      ei: 'ä', ou: '', au: '', ie: '', ue: '', aa: '', ee: '', ii: '', uu: '', oo: '',
      ch: '', ck: 'k', ng: '', tz: 'z', st: '', sp: '',
      ll: '', mm: '', nn: '', pp: '', ss: '', tt: '', ff: '', rr: '',
      bl: '', br: '', dr: '', fl: '', fr: '', gl: '', gr: '', kl: '', kr: '', pl: '', pr: '', tr: '',
      nd: '', nt: '', ld: '', lt: '', ft: '', mp: '',
    };
    // Voyelle simple : pas dans une voyelle longue (aa, ee, ii, uu) ni une diphtongue (ou, ei, ie, ue, äi, éi…),
    // et le e seulement comme première voyelle (Bett), jamais le e réduit des fins de mots (Fliger, Wollek).
    function voyelle(w, i) {
      const c = w[i];
      if ('aeiou'.indexOf(c) === -1) return false;
      if (estV(w[i - 1]) || estV(w[i + 1])) return false;
      return c !== 'e' || i === premiere(w);
    }
    function consonne(w, i) {
      const c = w[i];
      const p = w[i - 1] || '';
      const n = w[i + 1] || '';
      if ('bdfgklmnprstwz'.indexOf(c) === -1) return false; // c, h, j, q, v (Vull / Vëlo), x, y : ambigus
      if (c === p || c === n) return false; // lettres doubles (Ball, Bett, Kaddo)
      if (c === 's' && /[cpt]/.test(n)) return false; // sch, sp, st
      if (c === 'n' && /[gk]/.test(n)) return false; // ng, nk
      if (c === 'g' && (p === 'n' || !n)) return false; // ng ; g final (Bierg) se prononce autrement
      if ((c === 't' && n === 'z') || (c === 'z' && p === 't')) return false; // tz
      if (c === 'k' && p === 'c') return false; // ck
      if (c === 'w' && !n) return false; // w final (Léiw) se prononce f
      if (c === 'r' && estV(p) && !estV(n)) return false; // r vocalisé (Dier, Stär, Fliger)
      return true;
    }
    function exclusions(w, i) {
      switch (w[i]) {
        case 'd': return 't';
        case 't': return 'd';
        case 'b': return 'p';
        case 'p': return 'b';
        case 'g': return 'kc';
        case 'k': return 'gc';
        case 'f': return 'vw';
        case 'v': return 'fw';
        case 'w': return 'fv';
        case 's': return 'z';
        case 'z': return 'cs';
        case 'e': return 'äë';
        case 'ä': return 'e';
        case 'i': return 'y';
        default: return '';
      }
    }
    function paire(w, i) {
      const p = w.substr(i, 2);
      if (p.length !== 2 || !Object.prototype.hasOwnProperty.call(PAIRES, p)) return null;
      const a = p[0];
      const b = p[1];
      let ok;
      if (estV(a) && estV(b)) ok = !estV(w[i - 1]) && !estV(w[i + 2]); // ou, ei, ie, ue, aa, ee… (pas « éie »)
      else if (a === b || ['ch', 'ck', 'ng', 'tz'].indexOf(p) !== -1) ok = true;
      else if (p === 'st' || p === 'sp') ok = w[i - 1] !== 's';
      else ok = consonne(w, i) && consonne(w, i + 1);
      return ok ? PAIRES[p] + exclusions(w, i) + exclusions(w, i + 1) : null;
    }
    return { voyelle, consonne, exclusions, paire, consonnes: 'bdfgklmnprstwz', rares: '' };
  })();

  const REGLES = { fr: FR, en: EN, de: DE, lb: LB };

  // Lettres spéciales (niveau 3) et leurs « cousines » proposées comme distracteurs, pour les langues
  // où ce sont des lettres à part entière (le pack n'a pas de familles d'accents) : Bär / Bar, Fuß / Fus.
  // Le français utilise les familles d'accents de son pack (e é è ê…).
  const SPECIALES = {
    de: { ä: 'ae', ö: 'oe', ü: 'ui', ß: 'sz' },
    lb: { ä: 'ae', é: 'eë', ë: 'eé' },
  };

  // Vrais mots qu'un distracteur pourrait fabriquer à partir des mots illustrés du pack (une lettre changée
  // n'importe où, ou deux lettres voisines dans les mots de 5 lettres ou plus ; en chinois, un caractère
  // remplacé par un caractère d'un autre mot illustré), absents des autres listes du pack.
  // Générés hors ligne à partir de dictionnaires libres (npm : an-array-of-french-words, wordlist-english
  // jusqu'à la taille 60 avec les variantes britanniques, an-array-of-german-words et dictionary-de,
  // dictionary-lb, cc-cedict), écrits sous forme « pliée » (Ile.plier : minuscules sans accents ; ä ö ü ß
  // gardés en allemand, ä é ë en luxembourgeois). À régénérer si les mots illustrés d'un pack changent ;
  // en attendant, les mots du pack restent toujours vérifiés à l'exécution. Pour régénérer : lire aussi les
  // entrées hunspell sans drapeau (« vill po:adverb ») et vérifier aussi les combinaisons de deux lettres.
  const AUTRES_MOTS = {
    fr: (
      'acere acore adage adore adule agace agenouille agrafe ahanas ahuris aiche aigre aisee alaise ale alpin ' +
      'alunas ambon ambre amenas amere amuie amure anale anales anche ancra ancras angle angon animas anime ' +
      'anion anisas anise annee anode ansee antre apaise apion apure arabe arase arche arcon ardue arene arete ' +
      'argon argue aride arien arise armee armon arome arque arrise aryle astre aune autre avere avide aviez ' +
      'avili avina avinas avine avisa avise aviso aviva avive azure babine bache badine baigne bain balane ' +
      'balcon baleina balevre ballai ballas ballat baller balles ballet ballez ballot balzan balzane banale ' +
      'banaux banche bandee banlon bannee bannie banque barbon baryon basane basee bassine basson batees batela ' +
      'batele batent batera becane beche bedane bedeau beige ber biaise biche bichon billon bimane binard ' +
      'biseau bisee biveau ble bleche boche boire boisson bondon boston boucan boucau boucha boucla boucle ' +
      'boudee bouffe bougee bougie bougon bougre bouille boule boulee boulon bourbe bourde bourre bourse boutee ' +
      'bouton boutre braise breche broche bruche bruie brule buche buire buisson bureau butane cabot cabre ' +
      'cadets cadette cafard caisson calette calotte camard campe canada canais canait canant canape canari ' +
      'canaux canette canyon capron carafe cardite cariste carotta caroube carpeau casee cauris caveau cent ' +
      'cerame cercle cerite cerium cernee ceruse chah chai chameau change chapees chapela chapele char chas ' +
      'chenal chenil chevet cheveu chevre chiai chias chiat chics chiee chier chies chiez china chine chiot ' +
      'chipa chipe chips chomage choral chut ciseau citrin citronne citrus clayon cochai cochas cochat coche ' +
      'cochee cocher coches cochez cochonne cocotte combe comme conard conge conissant consonne copeau ' +
      'coquillais coquillait coquillant coquillart coquin cordon cordonne coteau cotise cotissant couard couche ' +
      'couille coule coup coupon couronna courtine crack crado craie crama crame crana cranas crane crans crase ' +
      'crash crave crawl creee creme crene crepe crepon crete creve criard criee crime crise croassant croie ' +
      'croise croissais croissait croissent cruche cueille cuire cuisson cuveau cytise deche demate deni der ' +
      'djain dont dosee douche douille drain drapeau duche dune durs ecule effile egrise elbot elire emanas ' +
      'empile emule encre enfile ensile epige eprise equille erige escarbot esche etable etage etain eteule ' +
      'ethyle etiole etire etoffe etoila etonne etoupe eveille exige fache fadee faire fait famee famille fanee ' +
      'fange faquin fauche faxee fee felee femelle femme femur fer fermage fetee feuilla feuillu feule fez ' +
      'fibre fiche fifille figee filee filmage fion fixee flair flanas fleau flein fluer fluor foire foison ' +
      'foree formage fou fouee fouie fouille foule foxee fraies fraisa framee frange frappe fraude frayage ' +
      'frayee frire frisage frison frisse frisson friture froissant fugue fuite fumee furie fusai fusas fusat ' +
      'fusel fuser fuses fusez fusil fusille fusse futee futur gache gain gallon gamme garce gatees gatent ' +
      'gatera gateux gauche gaule gemeau gemme gent gerce givre glaca glaise glana glanas gland glane glapi ' +
      'glass glati glebe glene globe glome glose glume gomme goule grace grain grange grenadille grenouilla ' +
      'gribouille grison guipure hache haire hait haleine hampe harpent havre heu hoche homme houille houle ' +
      'houp houris huche hune ide ilien ilion ils image inule ioule ire ive jabot jain joule juche jusee kache ' +
      'khat labie labre lacee lache lacte ladin ladre lagon laic laid laide laie laine lais laite laize lamai ' +
      'lamas lamat lamee lamer lames lamez lamie lampa lance lande lange lapai lapas lapat lapee laper lapes ' +
      'lapez lapis laque larde large larme larve lasse latin latte laure lavee layee layon leche lemme lepre ' +
      'lesee leu levre liage liane libre liche lied liege lien lieue lieur lifte ligie ligne ligue limbe limee ' +
      'linge lippe lisse liste liteau litee litre litron liure lives livet livra loche longe lopin loua louche ' +
      'loue loupe loure lucre lueur luge luire luit lupin lusin lute lutin luxe lysee mache macre maia maie ' +
      'mail maire mais maquillage marotte marron mec meche melo men ment menton merise mes met meule miche mir ' +
      'misee miston mitron moche moire moisson montages montante montasse morion motion mouche moule moulin ' +
      'mouron mouronne muche mur murs musee nabot nacre nain nait natron navre nee nef nefle negre neo nes net ' +
      'nette neume neuve niaise niche nichon noire nomme nouille nuais nuait nuant nuire nuise nuite nulle ' +
      'nuque nurse odeur ole ombre oracle orage orante orants ormille oronge orpin oscille oseille otage ouche ' +
      'ouis outille ouvre ovule pagre paie paien pair paire pais pait paix palie pallier palme palmiez palmite ' +
      'paloter panard paon papilles parafe parie parle parme pastelle patre patron paume paumier pavie pavillon ' +
      'peage peche pegre pelle penard pepie pepin pequin perle pesee petre peu phage pies pieta pieu pieutee ' +
      'piffa pigea pilla pille pinard pinca pinta pion piqua pissa pista pitie pitre place plaie plaise planas ' +
      'plane plate playon plebe pleur pliee ploie ploye pluche pluma plume plumier pluton poche pochon poele ' +
      'poeme poete pogne poids poila poile poils poilu poincon poing poins point poise poison poissai poissas ' +
      'poissat poissee poisser poisses poissez poivron polie pomma pommier pompa pompe ponce ponde ponge ponte ' +
      'poque posee poste poteau potee pouah pouce poufs pouille pouls poupe prame prele premier prime prison ' +
      'proie puche punie purs quenouille rabbin rabot radeau ragot raison rambour rampe range rapin rasee reche ' +
      'recoin regain renard requis requit riche rideau rieur risee robai robas robat robee rober robes robez ' +
      'robin roche rodat rompe rompt ronge rosat rosee rosit rotat rotit rouat rouet rouie rouille rouit roule ' +
      'ruche rupin rusee sabot sabre sache sacre sain saison sait sauge saule sauris sbire scion seche sechent ' +
      'segment sellent sent sentent sequin serge sergent seriant serient serment serrant serrent servant ' +
      'servent seule sevre sevrent sicle sied siege sieur sigle signe sillon sinon sinus situe sixte sobre ' +
      'solens somme songe sortis souche soucis soudas soudes souille soulas soule soules soumis soupas soupent ' +
      'soupes source sourde sourds sourie sourit soutes spire squille stage suage sucre sueur supin surs tabou ' +
      'tache tain taire tait tanin tapin taquin tarin taule telephona tempe teteau tetin teuton tisee toaste ' +
      'toison toiture tolite tomais tomait tomant tombee tomme tondue torche tordre tordue torque tortil tortis ' +
      'touche touille touque tourte trabe traca trace tracs tract trahi traie trais trait trama trame trams ' +
      'trapu trayon treille tressa tresse triture truie tuage tueur tune tussor ulule union unisson usage uvule ' +
      'vague vain vaine vaire vallon valse value valve vampe vanne vante vaque varie varve vaste vela veld vele ' +
      'velu venge vent ver veto veuille veule vibre vieille visee vivre voilure voire voisin voitura wallon ' +
      'zabre zain zebre zonard ' +
      // Relecture (dictionary-fr, formes absentes de la liste ci-dessus) :
      'ardre arion baston bion bonbec bourge busee carogne citral coire comate couton crade cre fallon flyer ' +
      'lapon lez line loire lomme lompe lure lyne meg mel montable ouds oups outs paule peule picta pigna pune ' +
      'rait rebot rez ried rune sion soire soufis ter'
    ),
    en: (
      'abbot abuse ace act addle aft again age agile aim aisle ale aloud alter amble ample amuse and anger ' +
      'angle anion ankle any apace ape appal apply apt apter arc ark art aster ate auger author awake awe awl ' +
      'aye bake band bar basket baster bead beak beam bean beat beau beer beg bet bey biddy bis biter bleep ' +
      'blower boa boar boas bob bod bog bolt boo boon boot booth bop bout bow brake brat broth bub buck bucked ' +
      'buckle bud buddy budget buffet bug bugle bullet bum bumpkin bur buster bustle but butterfat buy bye cab ' +
      'cackle cad cafe cage cajole cam came candle cane cap cape care case caster castes castor cater cattle ' +
      'cause cave caw cay change cheeky cheep cheery cheesy cherub chesty cite cloak clods clogs clomp clone ' +
      'clonk clops close cloth clots clout clove clown cloys clued cob cod cog coke con coo coon coot cop cot ' +
      'cox coy creep cruse cur curse cut cuter daddy dater dauphin deaf dear deck deter dick dire dob doc dock ' +
      'docket doe don dos dose dot doter douse drain drake duct dug dun dunk duple dusk duster dwell dye eager ' +
      'eat eater eave edger ego eke enter envelops ere erg err ester eve ewe exile faddy fake fare fastball ' +
      'faster fat fave fax fear fee fen fester fey fife file fine firm firs fist five fix flake flange flog ' +
      'flowed fob foe font food fool footfall foothill fop for fore fort foster fountain free frig from froth ' +
      'fuck gar gave gear gee giddy gig gite glower goon gorse grain grange grog grower guider gun had hag haj ' +
      'hake ham hang hank hap hater have haw hear hedgehop hem hep her hes hew hex hey hind hire hit hobbit hog ' +
      'hoot horas horde horns horny hound houri hours how huger hustle hut inane inland inter jabot jacket ' +
      'jester jig jostle juster keg ken kike kinda kine kith kits knell lager land later lave lead leak lean ' +
      'leap leas lee lire lite locket loon loot louse low lox luck lye maintain make manana manse maple mar ' +
      'master mater meter mickey middy mire mirth mister mite moan moat moire monody month mooch mood moor moos ' +
      'moose moot morn motley moues mould moult mound mount mourn mousy movie mow moxie muck muddy muster muter ' +
      'nave nee nestle noddy node none nope nosh nosy now nun nurse nus oar oat ocker odder offer ogler oil ' +
      'older oracle order osier other ouster outer owe own owner packet paddy pager panel panes pangs panic ' +
      'pansy panto pants par parka parse pasha pasta pat pause pave peak peal peas peat pee peer peg pester ' +
      'pestle peter phone pic picket pie pin pip pis pit pitta pix place plaid plain plait plank plans plant ' +
      'plash plate plats platy plays plaza plebe plume pocket pose poster potash pow pox prion prone proud ' +
      'prune puck pug pun purse pus quake quell quoth rabbet rabbis rabble racket rand raster rater rave rear ' +
      'recycle reuse rig rite roast robed robes robin rocked rocker roger roost root roster rouse roust row ' +
      'ruck ruddy rustle rye sabot sager sake sand sat save scale scar scion scull sear shale shall sharp shawl ' +
      'sheaf shear sheds sheen sheer sheet sherry shewn shews shill shoal shower sin sire site skill skull ' +
      'slain slake sleep sloth slower smoke snack snafu snags snaky snaps snare snarl snide snipe snore soar ' +
      'socket son soon soot sooth souse south sow spake spar spike spill spoke stab stag stain stake stale ' +
      'stall stay steep still stir stoke strep sub suck sue sum sup swain sweep swell swill taker tamer taper ' +
      'tar tardy taser taster tat tater taxer tear teary tee teeny telly tenth terry terse tester testy thane ' +
      'thee ticket tight tiler timer tiptop tire toady toddy tog toner tools toot toots touch tough tow tower ' +
      'toxin trace track tract trade trail trait tramp trams traps trash trawl trays trek trey trier troth true ' +
      'truer truth tuber tuck tun tuner tuple twain twee ulster union unshorn upland utter vaster vat verse ' +
      'voter vow wade wage wager wake wale wand wane war ware waster wavy wear wee wen whack whams wharf whats ' +
      'where wherry while whine whore whose wicket wire wish wive wog worse wove wow wroth yen youth yow yuck ' +
      'zoster ' +
      // Relecture (wordlist-english jusqu'à la taille 70 : mots rares, mais de vrais mots) :
      'ane arb becket ben blain bouse bree bucker burnet cade cantle cate checky chemmy cherty cig corse couth ' +
      'crake cree cuke dey dor dow dree eagre fash fere footwall frag frow frug gat gree guck hin horme hun kat ' +
      'kibe kish knower lar leal ley liger mig mog morse nog pampa panga pinna pinta pish poon rabbin rester ' +
      'shend stane sud swale torse tucket vanda vire wame wat wester wite yester'
    ),
    de: (
      'aage abbel abge achel ackel addel adge afael affa aige akto alge alto amael ammel ampel amsel amuel ' +
      'anales ananen andel ange angel aniel anitas ankel antel anto antras anuel anzel apfen apfer apoll ' +
      'april apsel apsöl arcel arge arkel arkise arvel assel astel atanas attel aube auce auco aufe augr ' +
      'augs augt auke aule aulo aume aune aupe aupo aurel auro ause aute auth bach bahne bald bali balz ' +
      'banale bangte banne bannte bar barone baud baue baus baut bbott bbs beat bebt been beet beil beim ' +
      'beiß beln ber bern bert best beta bete betz bfälle biege biegt biere biers biest biete bifie bilde ' +
      'bill binde binge binse birgt birke bis bisse black blage blähe bläht blair blanc blank blass blast ' +
      'bläst blaue blauf blecke bleie blicke blocke blöcke blöde blöke blökt blöse bloße blöße blues bluff ' +
      'blühe blüht bluse blute blüte bluts bogt böhme bohne bont book boom boome bos bös bout boxt brat ' +
      'brät briefe briese bringe brisse brodle brösle brüche brügge brühte brülle brüske brüste brut bts ' +
      'bub bug bühne buk bull bun bush buß but bütt buy bzocke ea eale eate eb ebs ebte ec ed edle eds ee ' +
      'eele ees eete ef efle efte eg eh ehle eia eib eid eif eig eih eil eile eim einhold einhole einholt ' +
      'eiß eit eite eiz ej ek ekle eks ekte el elegant element elevant elle els elte em ems en enae ence ' +
      'enge enie enje enke enne enre ens ense enth ento entr ents enze eo eos ep er erle ers erte ess eß ' +
      'este et ets ette eu euge euli eure eus eute ev ew ews ex exte ey eys ez fachs fader fahrend faser ' +
      'feder feger feier fesch feure ffner fixer flohs fluch flyer forsch foyer frech frisch frosta frosts ' +
      'fuder fug fui fujis fun funds funks fur fusch fußes futsch fuw geschehe gescheit geschert gescheut ' +
      'geschick habby habe habs hace hade haggy hahn haie hais hake hals hame hana hanau hance handi hanel ' +
      'hanen hanf hanfs hang hange hangs hank hanne hanoi hans hanse haos happy haps hard hare harry hars ' +
      'hass hast hät hats haue haun haut have hdt hese heus hind hit hlt hnt hohn hot höt hrt huan hub huf ' +
      'hui hul hun hur ichel ickel iebel iedel iegel iesel iffel iguel immel impel indel ineal inkel inmal ' +
      'insam insch insen inser insey intel inzel ipfel ippel irbel irkel istel ittel itzel kabel kable ' +
      'kabul kacke kahle kähne kalte kamai kamen kamin kamms kampf kamst kanal kante kappe kaputte karge ' +
      'kargste karosse karotin karre karte kasse katen kater katia kaufe kaure kaute kegel keine kenne ' +
      'kerne kerze keusche kinne kizze klammer klapper klebe klebt klees kleie klemm kleve klone klöne ' +
      'kochen köchen könne kotze krähe krähen krake krame kräne krise kroch kröne kropf kross kröte krude ' +
      'krüge krume kübel küchen kucken kugel kuhlen kühne kuj kulten kunden kuppen kur kurden kurien kursen ' +
      'kurten kurven kurze kürze kurzen kut kutsche kutten löde löse löße löte maas macs mads mags mais ' +
      'mans maos maps mars mass maul maut meuchel mind mong monk mons mont mood morchel mord münd muschis ' +
      'nabe nade nage nahe nake nasa nast oar odr oer ohe ohl ohm ohn oho oir oor our pfand pfeil pfelb ' +
      'pfeln pfels pfern pfers pflanzer pflastre pfund rachte raffte rahmte rakeln rakels rakern rammte ' +
      'ranate ranite rankte rannte rarste raubte raufte raunte rauste redete rodete ruckzuck sahne schad ' +
      'schaff schah schal scham schar scharf schau schauf scheiss schem schen scher scherin scheu schick ' +
      'schieb schied schief schien schier schieß schiit schild schilf schilt schily schirm schirr schis ' +
      'schiss schlacke schlaf schlaffe schlafs schlags schlampe schlangt schlanke schlappe schlaufe ' +
      'schlecke schlinge schlips schliss schlote schlots schlucke schlücke schluss schlüssen schlüssig ' +
      'schlüssle schmecke schmiss schmu schmucke schmücke schneide schneise schneite schnelle schnepfe ' +
      'schno schob schöh schon schön schopf schor schorf schoß schrecke schrein schub schuf schuh schul ' +
      'schur schwe schwebe schwebt schwede schweif schweig schweiß schweiz schwele schwelt schwenk schwere ' +
      'schwert schwinge segne sehne seine sinne socke softe sohle sohne söhne solde solle solve sonde songs ' +
      'sonja sonnt sonor sonst sonys sorbe sorge sorte souce sowie späne sporn statn steak steal stech ' +
      'steck stege stegs stehe steht steif steig steil steiß stell stemm stend steng stens stete stets ' +
      'steve stirn straf sühne szene taler täler tar täter ter tiber tigma toaste tollte toner tor törer ' +
      'toter tourte töver trier tur tyler vogts vogue vokal vulven walke wanke wecke welke werke wicke ' +
      'winke wirke woche wohle wohne wolfs wolga wolle wollt wonne worte zahl zahm zaun zehn zeit zell zig ' +
      'zitrate zog zue zuf zum zun zur zvg'
    ),
    lb: (
      'acel ad adder adler ae affer ah aker akter aler aller alter amber an aner ar arel auder bac bach bad ' +
      'baden bai bäi bail bak baken bal balcon bale balen ballad balleg ballek ballen baller ballet banal ' +
      'bande bands banjo banke banne bannt bar baren baron bas basen bau bauzon bech bee beel bees beet ' +
      'beez béi beit ben bern beut bich biede bief biefs bieft biere bierk biet biets biewe bilan bill bilt ' +
      'bind bitt bléck block bloem blot bluff bluse blutt boch boer boll bom bott bpm bräit braut break ' +
      'bréch bréie bréif bréim bréit bréiz bréng brest briet britt broch brock brode bromm brong bross ' +
      'brote brown bruck bruet brumm buer buerg bull bush dellen el elegant element em es ex faass fader ' +
      'fäeger faker fanger fäsch faser fauch féier feige feiger feigt feile feils feilt feine feint fénger ' +
      'féxer fiiss fixer fläch flëss flich fligel floss flouer fluch fluer flyer fochs focks fodes fokus ' +
      'fonds fooss forms fouen fouer foule fouls foult foung foyer frech frëss funks fusch gronner haarz ' +
      'häerd häert hals hang hank hauf haul heem hees hief hiel hien hier hierz hong honn huel huet huss ' +
      'iesel iewel iwwel kach käch kader kaf kairo kan kane kann kant kanu kap kar kasko kaweechelcher ' +
      'kazoo kleet klein klemm klett knascht kokosnëss kol konscht kor kox koz kredo kréin krinn kroat ' +
      'krock krome kromm krope krunn kuck kultus kuuscht léie léif léin léis léit maart mäert maus mauschel ' +
      'mëndchen mien moer moert monad mono moos mops mord moss mouch moud moude mouer mouk mouke moul moule ' +
      'mouls moult mount muede muele muels muelt muerd muere muerg mufft musst nuef nuen oder oper reebéi ' +
      'sanéier schaaf schaf schaff schäff schah schäi schäiss schal schaler schär schauer schei schéi ' +
      'scheier scheif schéine schéint schéiss schëld schënn schëpp schëss schëtt schëtz scheuer schief ' +
      'schif schifer schiff schilf schlaaks schlag schlake schlamm schlamp schlank schlapp schlaue schlauf ' +
      'schlaui schlaut schleis schléis schlëss schlësseg schlips schlo schlof schluss schmier schmu schna ' +
      'schnéimass schnëss schnuer schock schold scholl schonn schoss schoter schott schotz schoun schous ' +
      'schräin schro schroer schwäch schwäif schwäiz schwämm schwänz schwärm schwätz schwier schwoer sënn ' +
      'sezéier sinn soen soin sond sony soulaang spär spigel spiral sprong star strof taser täter téier ' +
      'timer toast tompe tomps tompt toobt toopt toozt toppt touft tourt tower trakten trakter tuler wollef ' +
      'wolleg wollen wollte wollts zank zell zuck ' +
      // Relecture : mots de dictionary-lb sans drapeau d'affixe (« vill », « ab »…), oubliés à la génération.
      'ab adel fiess vill zoch'
    ),
    zh: (
      '三星 三条 乌云 乌亮 乌日 乌青 乌鱼 乌鸡 乌龙 云朵 云阳 云龙 亮星 人子 人球 人龙 仙人球 仙子 仙山 企望 克山 克星 公子 公猫 公车 冰山 冰球 冰糕 冰花 冰蛋 出山 出车 出饭 ' +
      '刀子 包子 包机 包车 包饭 南山 南瓜 南阳 叶门 叶面 向阳 嘴子 圈子 堡子 大三 大人 大仙 大公 大刀 大力 大器 大大 大头 大小 大山 大指 大提琴 大月 大条 大棒 大汉 大火 大牛 ' +
      '大王 大球 大笔 大米 大红 大脑 大脚 大萝卜 大虫 大西 大足 大门 大雨 大雪 大马 大鹿 大鼠 太公 太太 太子 头子 头条 头球 头脑 奶子 子鼠 小子 小毛虫 小球 小脑 小花 小车 小鹅 ' +
      '小鼠 小龙 山子 山猫 山莓 山阳 彩云 彩头 彩电 彩蛋 彩车 恐鸟 手机 手球 指南车 掌机 提子 提花 提车 救星 日子 明亮 明子 明山 明星 星云 星月 星汉 星火 星球 星相 星象 星马 ' +
      '月子 月月 月牙 月球 月琴 月相 月租 月老 望花 机子 机车 条子 果子 柿子 树莓 树蛙 桃子 桃山 桃花 梨子 梨果 棒子 棒棒机 棒球 椰奶 椰子 椰子猫 椰果 橘树 橘红 毛子 毛条 ' +
      '毛毛雨 毛象 汉堡王 汉子 汉阳 火力 火器 火大 火星 火机 火爆 火球 火电 火眼 火红 火花 火鸡 火龙 烈山 照亮 熊包 熊掌 熊熊 熊猴 熊蜂 熊鹰 爆花 牙人 牙子 牙床 牙行 牛人 牛头 ' +
      '牛子 牛毛 牛米 牛羊 牛蛙 牛角 牛马 狗子 独子 独山 独龙 猫条 猫饭 猴王 猴米 王子 球星 瓜子 瓜果 甜瓜 电力 电器 电子 电机 电棒 电椅 电眼 电笔 电车 电门 电鸡 直球 相山 ' +
      '相机 眼力 眼圈 眼球 眼红 眼花 眼虫 眼角 眼镜 租子 租车 米兔 米果 米虫 米面 糖果 糖瓜 红山 红星 红机 红果 红花 红萝卜 红蛋 纸条 纸花 羊奶 老人 老公 老大 老太 老头 老子 ' +
      '老小 老手 老汉 老老 老花 老远 老鸟 老鹰 耳力 耳子 耳机 耳针 胡子 胡瓜 胡花 胡蜂 胡蝶 脑子 脑瓜 脑花 船山 花子 花山 花朵 花猫 花车 草书 草包 草山 草帽 草果 草纸 草草 ' +
      '草鱼 草鸡 落子 葡糖 葵花 葵鼠 虎子 虎虎 虫子 蛇果 蛇莓 蛋包 蛋蛋 蛋鸡 蜂糕 蜜月 蜜桃 蜜糖 行星 行车 袜裤 裙裤 裤头 裤袜 裤裙 西南 西子 西汉 西米 西西 西门 西青 西面 ' +
      '角子 角球 角龙 象山 足月 足足 足金 车子 车机 车条 车龙 金子 金山 金星 金条 金瓜 金阳 金龟 镜子 镜花 长子 长机 长条 长虹 长阳 长颈龙 长龙 门子 门球 门齿 阳伞 阳山 雨人 ' +
      '雨山 雨果 雨花 雨蛙 雪亮 雪人 雪克 雪山 雪条 雪梨 雪球 雪糕 雪耳 雪车 雪青 霸机 霸王树 青云 青山 青果 青瓜 青眼 青羊 青花 青草 青阳 青鱼 青龙 面交 面包 面向 面子 面瓜 ' +
      '面相 面纸 面镜 面面 颈子 飞出 飞刀 飞升 飞手 飞红 飞船 飞虫 飞行 飞车 飞雪 飞马 飞鱼 飞鸟 飞鹰 飞鼠 飞龙 饭匙 香包 香叶 香火 香瓜 香甜 香花 香草 马子 马山 马球 马虎 ' +
      '马蜂 马车 鱼子 鱼花 鱼蛋 鱼龙 鸟机 鸡公 鸡毛 鸡眼 鸡米花 鸡脚 鸡西 鸡鸡 鸭掌 鸭梨 鸭霸 鹅莓 鼻头 鼻毛 齿条 龙山 龙猫 龙虎 龙车 龙阳'
    ),
  };

  // Lettres qui se ressemblent (à l'oreille ou à l'œil) : un distracteur proche est proposé en priorité.
  const PROCHES = {
    a: 'oe', e: 'ai', i: 'eu', o: 'au', u: 'oi',
    b: 'dpv', c: 'gt', d: 'btp', f: 'vt', g: 'cq', j: 'gi', k: 'ct', l: 'rt', m: 'nr', n: 'mu',
    p: 'bdq', q: 'pg', r: 'ln', s: 'zf', t: 'dl', v: 'fb', w: 'vm', z: 's', h: 'nb',
  };

  // ---------------------------------------------------------------------------
  // Lexique de la langue courante : tous les mots du pack (exacts) + formes pliées + AUTRES_MOTS.
  // ---------------------------------------------------------------------------
  const lexiques = {};
  const plie = (s) => Array.from(String(s)).map(Ile.plier).join('');
  function lexique() {
    const lang = Ile.getLang();
    if (lexiques[lang]) return lexiques[lang];
    const P = Ile.L();
    const exact = new Set();
    const add = (s) => String(s || '').toLowerCase().normalize('NFC').split(/[^\p{L}]+/u).forEach((w) => { if (w) exact.add(w); });
    (P.MOTS || []).forEach((m) => { add(m.mot); add(m.pluriel); });
    Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach(add));
    (P.RIMES || []).forEach((r) => r.mots.forEach(add));
    (P.CONTRAIRES || []).forEach((c) => { add(c.a); add(c.b); });
    (P.PHRASES || []).forEach((p) => add(p.texte));
    (P.PLURIELS || []).forEach((p) => { add(p.s); add(p.p); });
    const plies = new Set((AUTRES_MOTS[lang] || '').split(' ').filter(Boolean));
    exact.forEach((w) => plies.add(plie(w)));
    lexiques[lang] = { exact, plies };
    return lexiques[lang];
  }
  // Chinois : ensemble des mots connus (le chinois s'écrit sans espace : on garde les mots tels quels).
  function lexiqueZh() {
    const lang = Ile.getLang();
    if (lexiques[lang]) return lexiques[lang];
    const P = Ile.L();
    const mots = new Set((AUTRES_MOTS[lang] || '').split(' ').filter(Boolean));
    (P.MOTS || []).forEach((m) => mots.add(m.mot));
    Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach((w) => mots.add(w)));
    (P.RIMES || []).forEach((r) => r.mots.forEach((w) => mots.add(w)));
    (P.CONTRAIRES || []).forEach((c) => { mots.add(c.a); mots.add(c.b); });
    (P.PHRASES || []).forEach((p) => (p.mots || []).forEach((w) => mots.add(w)));
    Object.keys(P.PINYIN || {}).forEach((w) => mots.add(w));
    lexiques[lang] = mots;
    return mots;
  }

  // Remplit les cases du mot avec des lettres données.
  function remplir(w, positions, lettres) {
    const a = Array.from(w);
    positions.forEach((p, k) => { a[p] = lettres[k]; });
    return a.join('');
  }
  // Une combinaison de tuiles forme-t-elle un AUTRE mot réel ? Pour les accents français, on compare
  // exactement (« vélo » ≠ « velo ») ; sinon, sur les formes pliées (« tache », « tâche »… ne doivent jamais
  // apparaître ; en allemand, « Bar » pour « Bär » non plus).
  function autreMot(w, positions, lettres, exacte) {
    const s = remplir(w, positions, lettres);
    if (s === w) return false;
    const X = lexique();
    if (exacte) return X.exact.has(s);
    const ps = plie(s);
    return ps !== plie(w) && X.plies.has(ps);
  }
  // Toutes les façons de placer des tuiles (distinctes) dans les cases.
  function combinaisonPiege(w, positions, tuiles) {
    const n = positions.length;
    const essai = (choix, utilises) => {
      if (choix.length === n) return autreMot(w, positions, choix, false);
      for (let k = 0; k < tuiles.length; k++) {
        if (utilises.indexOf(k) !== -1) continue;
        if (essai(choix.concat([tuiles[k]]), utilises.concat([k]))) return true;
      }
      return false;
    };
    return essai([], []);
  }

  // ---------------------------------------------------------------------------
  // Fabrication des questions (langues à alphabet)
  // ---------------------------------------------------------------------------
  function moteur() {
    const P = Ile.L();
    const lang = Ile.getLang();
    const R = REGLES[lang] || REGLES.en;
    const voyelles = P.voyelles;
    const estVoyelle = (c) => voyelles.indexOf(c) !== -1;
    // Voyelles simples (sans accent ni tréma) : les seules proposées aux niveaux 1 et 2.
    const voyellesSimples = voyelles.filter((v) => Ile.sansAccents(v) === v);
    const speciales = SPECIALES[lang] || null;
    const familles = P.familles || {};
    const aDesSpeciales = !!speciales || Object.keys(familles).length > 0;
    // Famille d'une lettre spéciale : elle-même et ses cousines (ä a e, ß s z ; en français e é è ê, trémas
    // seulement si c'est la réponse).
    function famille(c) {
      if (speciales) return speciales[c] ? [c].concat(speciales[c].split('')) : null;
      for (const base in familles) {
        if (familles[base].indexOf(c) === -1) continue;
        const autres = [base].concat(familles[base].split(''))
          .filter((x) => x !== c && !/̈/.test(x.normalize('NFD')));
        return [c].concat(autres.slice(0, 3));
      }
      return null;
    }
    const indices = (w) => Array.from(w).map((_, k) => k);
    const accentuees = (w) => indices(w).filter((i) => famille(w[i]) !== null);

    // Tuiles : bonnes lettres + distracteurs du même genre (voyelle / consonne), sans jamais créer un autre mot.
    function choisirTuiles(w, positions, nbDistracteurs, excluSupp, poolVoyelles) {
      const bonnes = positions.map((p) => w[p]);
      let exclu = excluSupp || '';
      positions.forEach((p) => { exclu += R.exclusions(w, p); });
      const listes = positions.map((p) => {
        const pool = estVoyelle(w[p]) ? poolVoyelles : R.consonnes.split('');
        // Une seule lettre « proche » au plus, le reste au hasard : l'ensemble des tuiles ne trahit pas la réponse.
        const proche = Ile.shuffle((PROCHES[w[p]] || '').split('').filter((x) => pool.indexOf(x) !== -1)).slice(0, 1);
        const rare = (x) => (R.rares.indexOf(x) !== -1 ? 1 : 0);
        return proche.concat(Ile.shuffle(pool).sort((a, b) => rare(a) - rare(b)));
      });
      const choisis = [];
      const ok = (x) => bonnes.indexOf(x) === -1 && exclu.indexOf(x) === -1 && choisis.indexOf(x) === -1 &&
        !combinaisonPiege(w, positions, bonnes.concat(choisis, [x]));
      for (let tour = 0; choisis.length < nbDistracteurs && tour < nbDistracteurs * 2 + 2; tour++) {
        const d = listes[tour % listes.length].find(ok);
        if (d) choisis.push(d);
      }
      if (choisis.length < nbDistracteurs) return null;
      return Ile.shuffle(bonnes.concat(choisis));
    }

    // Une question pour un mot, ou null si ce mot ne convient pas à ce type de question.
    // w garde la majuscule du nom (Katze) pour l'affichage ; les règles travaillent sur lw (minuscules).
    function fabriquer(m, type, lettreVoulue) {
      const w = m.mot.normalize('NFC');
      const lw = w.toLowerCase();
      if (/[^\p{L}]/u.test(lw) || Array.from(w).length !== Array.from(lw).length) return null; // tiret, espace…
      const cachable = (i) => w[i] === lw[i]; // jamais la majuscule d'un nom
      const base = { m, w, lw, type, units: Array.from(w) };
      if (type === 'voyelle' || type === 'lettre') {
        const cands = Ile.shuffle(indices(lw).filter((i) => cachable(i) &&
          ((estVoyelle(lw[i]) && R.voyelle(lw, i, type === 'voyelle' ? 1 : 2)) ||
            (type === 'lettre' && !estVoyelle(lw[i]) && R.consonne(lw, i)))));
        for (const i of cands) {
          const t = choisirTuiles(lw, [i], type === 'voyelle' ? 2 : 3, '', voyellesSimples);
          if (t) return Object.assign(base, { trous: [i], tuiles: t });
        }
        return null;
      }
      if (type === 'accent') {
        const pos = Ile.shuffle(accentuees(lw).filter(cachable)).sort((a, b) => (lw[b] === lettreVoulue) - (lw[a] === lettreVoulue));
        for (const i of pos) {
          const t = famille(lw[i]).filter((x) => x === lw[i] || !autreMot(lw, [i], [x], !speciales));
          if (t.length >= 2) return Object.assign(base, { trous: [i], tuiles: Ile.shuffle(t) });
        }
        return null;
      }
      // Deux lettres voisines (au moins 5 lettres dans le mot) : ch, ou, an, sh, ee, ei, ck, ll, tr…
      if (Array.from(lw).length < 5) return null;
      const speciale = (k) => accentuees(lw).indexOf(k) !== -1;
      const paires = Ile.shuffle(indices(lw).slice(0, -1))
        .filter((i) => cachable(i) && cachable(i + 1) && !speciale(i) && !speciale(i + 1))
        .map((i) => ({ i, exclu: R.paire(lw, i) }))
        .filter((o) => o.exclu !== null);
      for (const o of paires) {
        const t = choisirTuiles(lw, [o.i, o.i + 1], 4, o.exclu, voyelles);
        if (t) return Object.assign(base, { trous: [o.i, o.i + 1], tuiles: t });
      }
      return null;
    }

    // Les 10 questions de la partie.
    function preparer(level) {
      const pris = new Set();
      const serie = (mots, type, n) => {
        const out = [];
        for (const m of mots) {
          if (out.length >= n) break;
          if (pris.has(m)) continue;
          const q = fabriquer(m, type);
          if (q) { out.push(q); pris.add(m); }
        }
        return out;
      };
      if (level === 1) return serie(Ile.motsPourPartie(1, 999), 'voyelle', TOTAL);
      if (level === 2) return serie(Ile.motsPourPartie(2, 999), 'lettre', TOTAL);

      // Niveau 3 : lettres spéciales variées (une lettre différente à chaque fois tant que possible).
      const accents = [];
      if (aDesSpeciales) {
        const minuscules = (m) => m.mot.normalize('NFC').toLowerCase();
        const mots = Ile.motsPourPartie(3, 999, (m) => accentuees(minuscules(m)).length > 0);
        const vues = new Set();
        for (let tour = 0; tour < 2 && accents.length < TOTAL / 2; tour++) {
          for (const m of mots) {
            if (accents.length >= TOTAL / 2) break;
            if (pris.has(m)) continue;
            const lw = minuscules(m);
            const nouvelle = accentuees(lw).map((i) => lw[i]).find((c) => !vues.has(c));
            if (tour === 0 && !nouvelle) continue;
            const q = fabriquer(m, 'accent', nouvelle);
            if (q) { accents.push(q); pris.add(m); vues.add(lw[q.trous[0]]); }
          }
        }
      }
      const deux = serie(Ile.motsPourPartie(3, 999), 'deux', TOTAL - accents.length);
      // On alterne : lettre spéciale, deux lettres, lettre spéciale… (en commençant au hasard).
      const a = Ile.shuffle(accents);
      const d = Ile.shuffle(deux);
      const out = [];
      let tourAccent = Math.random() < 0.5;
      while (a.length || d.length) {
        out.push((tourAccent && a.length) || !d.length ? a.shift() : d.shift());
        tourAccent = !tourAccent;
      }
      return out.slice(0, TOTAL);
    }

    return { preparer };
  }

  // ---------------------------------------------------------------------------
  // Fabrication des questions (chinois) : un caractère manque, les choix sont des caractères.
  // ---------------------------------------------------------------------------
  function moteurZh() {
    const P = Ile.L();
    const MOTS = P.MOTS || [];
    const X = lexiqueZh();
    const syllabes = (m) => String(m.pinyin || '').trim().split(/\s+/);
    // Syllabe de chaque caractère, telle qu'on la lit dans le premier mot illustré qui le contient.
    const syllabe = new Map();
    MOTS.forEach((m) => {
      const s = syllabes(m);
      Array.from(m.mot).forEach((c, k) => { if (!syllabe.has(c) && s[k]) syllabe.set(c, s[k]); });
    });
    // Syllabe sans ton : deux choix qui se lisent pareil (猫 māo, 毛 máo) ne sont jamais proposés ensemble.
    const son = (p) => String(p || '').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
    const niveauCar = new Map(); // niveau le plus bas où un caractère apparaît
    const themeCar = new Map(); // thèmes des mots qui contiennent ce caractère
    MOTS.forEach((m) => Array.from(m.mot).forEach((c) => {
      niveauCar.set(c, Math.min(niveauCar.get(c) || 9, m.niveau));
      if (!themeCar.has(c)) themeCar.set(c, new Set());
      themeCar.get(c).add(m.theme);
    }));
    const tousCar = Array.from(niveauCar.keys());

    function fabriquer(m, level) {
      const car = Array.from(m.mot);
      const py = syllabes(m);
      if (car.length < 2 || py.length !== car.length) return null;
      const nbDistracteurs = level === 1 ? 2 : 3;
      // On ne cache ni le suffixe 子 (兔子, 鼻子) ni un caractère répété (大猩猩 : l'autre 猩 donnerait la
      // réponse) quand un autre caractère peut l'être.
      let positions = Ile.shuffle(car.map((_, k) => k));
      const pleines = positions.filter((k) => car[k] !== '子' && car.indexOf(car[k]) === car.lastIndexOf(car[k]));
      if (pleines.length) positions = pleines;
      for (const p of positions) {
        const bon = car[p];
        const sons = new Set([son(py[p])]);
        // Niveau 1 : caractères des mots du niveau 1 ; ensuite, ceux du même thème d'abord (fruits, animaux…).
        const pool = Ile.shuffle(tousCar.filter((c) => c !== '子' && car.indexOf(c) === -1 && (level > 1 || niveauCar.get(c) === 1)));
        const memeTheme = (c) => (level > 1 && !themeCar.get(c).has(m.theme) ? 2 : 0);
        // Un caractère lu au ton neutre dans son mot (睛 dans 眼睛 : jing) vient en dernier : seul, il
        // n'afficherait pas son vrai ton.
        const neutre = (c) => (syllabe.get(c) === son(syllabe.get(c)) ? 1 : 0);
        pool.sort((a, b) => (memeTheme(a) + neutre(a)) - (memeTheme(b) + neutre(b)));
        const choisis = [];
        for (const c of pool) {
          if (choisis.length >= nbDistracteurs) break;
          const s = son(syllabe.get(c));
          if (!s || sons.has(s)) continue;
          if (X.has(remplir(m.mot, [p], [c]))) continue; // jamais un autre vrai mot (大人, 火车…)
          choisis.push(c);
          sons.add(s);
        }
        if (choisis.length < nbDistracteurs) continue;
        const pyTuiles = {};
        choisis.forEach((c) => { pyTuiles[c] = syllabe.get(c); });
        pyTuiles[bon] = py[p];
        return { m, w: m.mot, lw: m.mot, type: 'caractere', units: car, py, trous: [p], tuiles: Ile.shuffle([bon].concat(choisis)), pyTuiles };
      }
      return null;
    }

    function preparer(level) {
      const n = (m) => Array.from(m.mot).length;
      const filtre = level === 3 ? (m) => n(m) >= 3 && n(m) <= 4 : (m) => n(m) >= 2;
      // Un mot fait d'un seul caractère répété (星星) se devine sans réfléchir : l'autre caractère donne la
      // réponse. On ne le prend qu'en dernier recours.
      const redouble = (m) => (new Set(Array.from(m.mot)).size === 1 ? 1 : 0);
      const mots = Ile.motsPourPartie(level, 999, filtre).sort((a, b) => redouble(a) - redouble(b));
      const out = [];
      for (const m of mots) {
        if (out.length >= TOTAL) break;
        const q = fabriquer(m, level);
        if (q) out.push(q);
      }
      return out;
    }
    return { preparer };
  }

  // ---------------------------------------------------------------------------
  // Le jeu
  // ---------------------------------------------------------------------------
  let clavier = null; // écouteur clavier de la partie en cours

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const hanzi = Ile.L().ecriture === 'hanzi';
      const questions = (hanzi ? moteurZh() : moteur()).preparer(level);
      // Chinois : pinyin sous le mot aux niveaux 1 et 2, sous chaque choix au niveau 1.
      const pinyinMot = hanzi && level < 3;
      const pinyinTuiles = hanzi && level === 1;
      let i = 0;
      let score = 0;

      const panel = el('section', { class: 'panel lettre' + (hanzi ? ' lettre--zh' : ''), 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);

      // Clavier : taper une lettre choisit la tuile correspondante (é, ä, ß… selon le clavier de l'enfant).
      if (clavier) document.removeEventListener('keydown', clavier);
      const surTouche = (e) => {
        if (!panel.isConnected) { document.removeEventListener('keydown', surTouche); return; }
        if (e.ctrlKey || e.metaKey || e.altKey || e.repeat || e.isComposing || document.querySelector('.result')) return;
        const cible = e.target;
        if (cible && cible.closest && cible.closest('input, select, textarea, [contenteditable]')) return;
        const k = String(e.key || '').toLowerCase().normalize('NFC');
        if (!/^\p{L}$/u.test(k)) return;
        const b = Array.from(panel.querySelectorAll('.lettre-tuile:not(:disabled)')).find((x) => x.dataset.lettre === k);
        if (b) { e.preventDefault(); b.click(); }
      };
      clavier = surTouche;
      document.addEventListener('keydown', surTouche);

      function fin() {
        Ile.progress(panel, questions.length, questions.length, score);
        let message;
        if (score === questions.length) message = tx.parfait;
        else message = Ile.canSpeak && !Ile.isMuted() ? tx.astuceEcoute : tx.astuceLis;
        Ile.showResult({ id: ID, score, total: questions.length, message });
      }

      // Syllabe de pinyin (petit texte sous un caractère).
      const syllabeEl = (texte, cache) => el('span', { class: 'lettre-py', lang: 'zh-Latn-pinyin', text: texte, hidden: cache || null });

      function next() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= questions.length) { fin(); return; }

        const q = questions[i];
        const w = q.w;
        let etape = 0; // case à remplir
        let firstTry = true;
        let done = false;
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);

        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        Ile.progress(panel, i, questions.length, score);
        panel.dataset.question = String(i);
        panel.dataset.type = q.type;

        // Le mot avec ses cases vides.
        const cases = [];
        const trou = () => {
          const box = el('span', { class: 'lettre-trou' + (cases.length === 0 ? ' is-active' : '') }, [
            el('span', { class: 'sr-only', text: tx.caseVide }),
            NB,
          ]);
          cases.push(box);
          return box;
        };
        let motEl;
        if (hanzi) {
          // Un caractère par colonne, sa syllabe de pinyin dessous (cachée au niveau 3 jusqu'à la réponse).
          motEl = el('p', { class: 'mot-affiche lettre-mot lettre-mot--zh' + (pinyinMot ? '' : ' is-sans-pinyin') });
          q.units.forEach((c, k) => {
            motEl.appendChild(el('span', { class: 'lettre-zi' }, [
              q.trous.indexOf(k) === -1 ? el('span', { class: 'lettre-car', text: c }) : trou(),
              syllabeEl(q.py[k], !pinyinMot),
            ]));
          });
        } else {
          // Mots longs (montagne, Schmetterling, Waassermeloun…) : police réduite pour garder le bouton écouter sur la ligne.
          const long = q.units.length;
          motEl = el('p', { class: 'mot-affiche lettre-mot' + (long >= 11 ? ' lettre-mot--long lettre-mot--tres-long' : long >= 8 ? ' lettre-mot--long' : '') });
          q.units.forEach((c, k) => {
            motEl.appendChild(q.trous.indexOf(k) === -1 ? document.createTextNode(c) : trou());
          });
        }

        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
        // L'étiquette de l'image ne donne le mot qu'après la réponse (sinon elle souffle l'orthographe).
        const image = el('div', { class: 'big-emoji', role: 'img', 'aria-label': tx.image, text: q.m.emoji });
        const tiles = el('div', {
          class: 'tiles lettre-tiles lettre-tiles--' + q.tuiles.length + (q.type === 'accent' ? ' lettre-tiles--accents' : ''),
          role: 'group',
          'aria-label': tx.tuiles,
        });

        function reussi() {
          done = true;
          if (firstTry) score++;
          tiles.classList.add('is-fini');
          motEl.querySelectorAll('.lettre-py[hidden]').forEach((s) => { s.hidden = false; }); // pinyin révélé
          motEl.classList.remove('is-sans-pinyin');
          image.setAttribute('aria-label', tx.imageDe(w));
          Ile.flash(motEl, 'good');
          Ile.sfx('good');
          const debut = (firstTry ? Ile.pick(Ile.t('bravo'), 1)[0] : tx.oui).replace(/ ([!?:;])/g, NB + '$1');
          Ile.feedback(fb, true, debut + (hanzi ? '' : ' ') + tx.onEcrit(w));
          Ile.say(w);
          Ile.progress(panel, i + 1, questions.length, score);
          i++;
          setTimeout(next, PAUSE);
        }

        function erreur(b, lettre) {
          firstTry = false;
          Ile.flash(b, 'bad');
          Ile.flash(cases[etape], 'bad');
          Ile.sfx('bad');
          // Bonne lettre, mais pour la case suivante : on ne la grise pas.
          if (q.trous.slice(etape + 1).some((p) => q.lw[p] === lettre)) {
            Ile.feedback(fb, false, tx.plusTard(lettre));
            return;
          }
          b.disabled = true;
          b.classList.add('is-wrong');
          let conseil = pinyinMot ? tx.conseilLis : hanzi ? tx.conseilImage : tx.conseilLis;
          if (q.type === 'accent') conseil = tx.conseilAccent(q.lw[q.trous[0]]);
          else if (Ile.canSpeak && !Ile.isMuted()) conseil = tx.conseilEcoute;
          Ile.feedback(fb, false, tx.pasCa(lettre) + (hanzi ? '' : ' ') + conseil);
          const reste = tiles.querySelector('.lettre-tuile:not(:disabled)');
          if (reste && !panel.contains(document.activeElement)) reste.focus({ preventScroll: true });
        }

        q.tuiles.forEach((lettre) => {
          const nom = tx.nomLettre[lettre];
          const py = pinyinTuiles ? q.pyTuiles[lettre] : '';
          const b = el('button', {
            type: 'button',
            class: 'tile lettre-tuile' + (hanzi ? ' lettre-tuile--zh' : ''),
            'data-lettre': lettre,
            'aria-label': nom ? lettre + ', ' + nom : py ? lettre + ' ' + py : null,
            title: nom || null,
          }, hanzi ? [
            el('span', { class: 'lettre-tuile__car', text: lettre }),
            py ? el('span', { class: 'lettre-tuile__py', lang: 'zh-Latn-pinyin', 'aria-hidden': 'true', text: py }) : null,
          ] : [lettre]);
          b.addEventListener('click', () => {
            if (done || b.disabled || !panel.isConnected) return;
            if (lettre !== q.lw[q.trous[etape]]) { erreur(b, lettre); return; }
            // Bonne lettre pour la case en cours.
            const box = cases[etape];
            box.textContent = lettre;
            box.classList.remove('is-active');
            box.classList.add('is-rempli');
            Ile.flash(box, 'good');
            b.disabled = true;
            b.classList.add(q.trous.length > 1 ? 'is-used' : 'is-correct');
            etape++;
            if (etape < q.trous.length) {
              cases[etape].classList.add('is-active');
              Ile.sfx('click');
              Ile.feedback(fb, true, tx.suivante);
              const reste = tiles.querySelector('.lettre-tuile:not(:disabled)');
              if (reste && !panel.contains(document.activeElement)) reste.focus({ preventScroll: true });
              return;
            }
            reussi();
          });
          tiles.appendChild(b);
        });

        panel.append(
          el('p', { class: 'consigne', text: q.type === 'accent' ? tx.consigne.accent(q.lw[q.trous[0]]) : tx.consigne[q.type] }),
          image,
          el('div', { class: 'lettre-ligne' }, [motEl, Ile.speakButton(w, tx.ecouterMot)]),
          tiles,
          fb
        );
        if (focusDansLeJeu && i > 0) tiles.firstElementChild.focus({ preventScroll: true });
      }

      next();
    },
  });
})();
