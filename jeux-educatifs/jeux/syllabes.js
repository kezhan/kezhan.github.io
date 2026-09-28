/* Le pont des syllabes : touche les syllabes dans l'ordre pour construire le pont et former le mot.
 * Niveau 1 : mots de 2 syllabes, aucun piège. Niveau 2 : 2–3 syllabes, 1 piège.
 * Niveau 3 : 3–4 syllabes, 2 pièges, image cachée jusqu'à la fin (un indice de thème la remplace).
 * Chinois : une syllabe = un caractère ; chaque tuile et chaque planche montre le caractère et son
 * pinyin ; les pièges sont des caractères d'autres mots. Allemand, luxembourgeois : la première
 * syllabe d'un nom garde sa majuscule (Kat·ze, Ba·nan) ; un piège garde celle de son mot d'origine.
 * Syllabes identiques (bon·bon, 星星) : l'une ou l'autre convient.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'syllabes';
  const TOTAL = 8;
  const PAUSE = 2300; // temps pour voir le pont fini et entendre le mot

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      consigne: 'Tap the syllables in order to build the word.',
      consigneCachee: 'Which word is hiding here? Tap the syllables in order.',
      ecouterMot: 'Listen to the word',
      image: 'Picture of the word to build',
      imageCachee: 'Hidden picture: you’ll see it at the end',
      imageDe: (g) => 'Picture: ' + g,
      pont: (k, n) => 'The bridge: ' + k + ' of ' + n + ' planks in place',
      tuiles: 'Syllables to place',
      syllabe: (s) => 'Syllable “' + s + '”',
      syllabePosee: 'Syllable already placed',
      syllabeEcartee: (s) => 'Syllable “' + s + '”: not in this word',
      pasDansMot: (s) => '“' + s + '” isn’t in this word.',
      plusLoin: (s) => '“' + s + '” comes later in the word.',
      essaieBrille: 'Try the one that’s glowing!',
      pasEncoreBrille: 'Not yet! Try the glowing syllable.',
      reussi: 'You did it!',
      cest: (g) => 'It’s ' + g + '.',
      themes: {
        animaux: '🐾 It’s an animal.',
        nourriture: '🍽️ You can eat it.',
        nature: '🌿 You find it in nature.',
        objets: '🧰 It’s a thing.',
        corps: '🖐️ It’s a part of the body.',
        personnes: '🧑 It’s a person.',
      },
      themeAutre: '🔎 Guess the word!',
      finParfaite: 'Every bridge built without a single mistake: what a talent!',
      finNormale: 'You get a point for each word built with no mistakes. Look carefully at each syllable and have another go!',
    },
    zh: {
      consigne: '按顺序点汉字，组成这个词。',
      consigneCachee: '藏起来的是哪个词？按顺序点汉字。',
      ecouterMot: '听一听这个词',
      image: '要组成的词的图片',
      imageCachee: '图片藏起来了，最后才会出现',
      imageDe: (g) => '图片：' + g,
      pont: (k, n) => '小桥：已经放好 ' + k + ' 块木板，一共 ' + n + ' 块',
      tuiles: '要放的汉字',
      syllabe: (s) => '汉字“' + s + '”',
      syllabePosee: '这个汉字已经放好了',
      syllabeEcartee: (s) => '汉字“' + s + '”：不在这个词里',
      pasDansMot: (s) => '“' + s + '”不在这个词里。',
      plusLoin: (s) => '“' + s + '”要放在后面。',
      essaieBrille: '试试发光的那个！',
      pasEncoreBrille: '还没轮到它！试试发光的汉字。',
      reussi: '你成功了！',
      cest: (g) => '这是' + g + '。',
      themes: {
        animaux: '🐾 这是一种动物。',
        nourriture: '🍽️ 这是能吃的东西。',
        nature: '🌿 在大自然里能看到它。',
        objets: '🧰 这是一样东西。',
        corps: '🖐️ 这是身体的一部分。',
        personnes: '🧑 这是一个人。',
      },
      themeAutre: '🔎 猜猜是哪个词！',
      finParfaite: '每座小桥都一次搭好，一个错也没有，真了不起！',
      finNormale: '一次就搭对的词，每个得一分。仔细看看每个汉字，再试一次吧！',
    },
    de: {
      consigne: 'Tippe die Silben in der richtigen Reihenfolge an.',
      consigneCachee: 'Welches Wort versteckt sich hier? Tippe die Silben der Reihe nach an.',
      ecouterMot: 'Wort anhören',
      image: 'Bild zum Wort',
      imageCachee: 'Verstecktes Bild: Du siehst es am Ende.',
      imageDe: (g) => 'Bild: ' + g,
      pont: (k, n) => 'Die Brücke: ' + k + ' von ' + n + ' Brettern gelegt',
      tuiles: 'Silben zum Legen',
      syllabe: (s) => 'Silbe „' + s + '“',
      syllabePosee: 'Silbe schon gelegt',
      syllabeEcartee: (s) => 'Silbe „' + s + '“: nicht in diesem Wort',
      pasDansMot: (s) => '„' + s + '“ ist nicht in diesem Wort.',
      plusLoin: (s) => '„' + s + '“ kommt später im Wort.',
      essaieBrille: 'Probier die Silbe, die leuchtet!',
      pasEncoreBrille: 'Noch nicht! Probier die Silbe, die leuchtet.',
      reussi: 'Geschafft!',
      cest: (g) => 'Das ist ' + g + '.',
      themes: {
        animaux: '🐾 Es ist ein Tier.',
        nourriture: '🍽️ Man kann es essen.',
        nature: '🌿 Man findet es in der Natur.',
        objets: '🧰 Es ist ein Gegenstand.',
        corps: '🖐️ Es ist ein Körperteil.',
        personnes: '🧑 Es ist ein Mensch.',
      },
      themeAutre: '🔎 Rate das Wort!',
      finParfaite: 'Alle Brücken ohne einen einzigen Fehler gebaut: Klasse!',
      finNormale: 'Für jedes Wort ohne Fehler gibt es einen Punkt. Schau dir die Silben genau an und versuch es noch mal!',
    },
    lb: {
      consigne: 'Setz d’Silben an déi richteg Reiefolleg.',
      consigneCachee: 'Wat fir e Wuert ass hei verstoppt? Setz d’Silben an déi richteg Reiefolleg.',
      ecouterMot: 'D’Wuert lauschteren',
      image: 'Bild vum Wuert',
      imageCachee: 'Verstoppt Bild: Du gesäis et um Enn.',
      imageDe: (g) => 'Bild: ' + g,
      pont: (k, n) => 'D’Bréck: ' + k + ' / ' + n + ' Silben',
      tuiles: 'Silbe fir d’Bréck', // règle de l'n : Silben → Silbe devant f
      syllabe: (s) => 'Silb „' + s + '“',
      syllabePosee: 'Silb schonn op der Bréck',
      syllabeEcartee: (s) => 'Silb „' + s + '“: net an dësem Wuert',
      pasDansMot: (s) => '„' + s + '“ ass net an dësem Wuert.',
      plusLoin: (s) => '„' + s + '“ kënnt méi spéit am Wuert.',
      essaieBrille: 'Probéier d’Silb, déi blénkt!',
      pasEncoreBrille: 'Nach net! Probéier d’Silb, déi blénkt.',
      reussi: 'Gutt gemaach!',
      cest: (g) => 'Dat ass ' + g + '.',
      themes: {
        animaux: '🐾 Et ass en Déier.',
        nourriture: '🍽️ Dat kann een iessen.',
        nature: '🌿 Dat fënnt een an der Natur.',
        objets: '🧰 Et ass eng Saach.',
        corps: '🖐️ Et ass en Deel vum Kierper.',
        personnes: '🧑 Et ass e Mënsch.',
      },
      themeAutre: '🔎 Fann d’Wuert!',
      finParfaite: 'Du hues all d’Wierder ouni Feeler gebaut. Bravo!',
      finNormale: 'Fir all Wuert ouni Feeler gëtt et e Punkt. Kuck d’Silbe gutt un a probéier nach eng Kéier!',
    },
    fr: {
      consigne: 'Touche les syllabes dans l’ordre pour former le mot.',
      consigneCachee: 'Quel mot se cache\u00a0? Touche les syllabes dans l’ordre.',
      ecouterMot: 'Écouter le mot',
      image: 'Image du mot à former',
      imageCachee: 'Image cachée\u00a0: elle apparaîtra à la fin',
      imageDe: (g) => 'Image\u00a0: ' + g,
      pont: (k, n) => 'Le pont\u00a0: ' + k + ' planche' + (k > 1 ? 's' : '') + ' posée' + (k > 1 ? 's' : '') + ' sur ' + n,
      tuiles: 'Syllabes à poser',
      syllabe: (s) => 'Syllabe «\u00a0' + s + '\u00a0»',
      syllabePosee: 'Syllabe déjà posée',
      syllabeEcartee: (s) => 'Syllabe «\u00a0' + s + '\u00a0»\u00a0: pas dans ce mot',
      pasDansMot: (s) => '«\u00a0' + s + '\u00a0» n’est pas dans ce mot.',
      plusLoin: (s) => '«\u00a0' + s + '\u00a0» vient plus loin dans le mot.',
      essaieBrille: 'Essaie celle qui brille\u00a0!',
      pasEncoreBrille: 'Pas encore\u00a0! Essaie la syllabe qui brille.',
      reussi: 'Tu as réussi\u00a0!',
      cest: (g) => 'C’est ' + g + '.',
      themes: {
        animaux: '🐾 C’est un animal.',
        nourriture: '🍽️ Ça se mange.',
        nature: '🌿 C’est dans la nature.',
        objets: '🧰 C’est un objet.',
        corps: '🖐️ C’est une partie du corps.',
        personnes: '🧑 C’est une personne.',
      },
      themeAutre: '🔎 Devine le mot\u00a0!',
      finParfaite: 'Tous les ponts construits sans une seule erreur\u00a0: quel talent\u00a0!',
      finNormale: 'Un point par mot construit sans erreur. Regarde bien chaque syllabe et réessaie\u00a0!',
    },
  };

  // Typographie française : espace insécable avant ! ? : ; » (les textes du pack commun en ont besoin aussi).
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, '\u00a0$1').replace(/« /g, '«\u00a0');
  }

  const hanzi = () => Ile.L().ecriture === 'hanzi';
  // Deux phrases à la suite : une espace, sauf en chinois (pas d'espace après ！。).
  const suite = (a, b) => a + (hanzi() ? '' : ' ') + b;
  const norm = (s) => Ile.sansAccents(String(s).normalize('NFC'));
  // Pinyin sans tons (ü conservé) : deux caractères qui ont le même se prononcent presque pareil.
  const sansTon = (p) => String(p || '').normalize('NFD').replace(/[\u0300-\u0307\u0309-\u036f]/g, '')
    .normalize('NFC').replace(/\s+/g, '').toLowerCase();

  // Nombre de syllabes pièges par niveau.
  const PIEGES = { 1: 0, 2: 1, 3: 2 };
  // Répartition des mots par nombre de syllabes : [nombre de syllabes, nombre de mots].
  const REPARTITION = {
    1: [[2, 8]],
    2: [[2, 5], [3, 3]],
    3: [[3, 5], [4, 3]],
  };

  // Clé phonétique simplifiée, propre à chaque langue : deux syllabes qui se prononcent pareil ont
  // la même clé. Un piège ne doit jamais sonner comme une vraie syllabe : l'enfant l'entend quand il la touche.
  const SONS = {
    fr: (s) => norm(s)
      .replace(/eau|au/g, 'o')
      .replace(/ph/g, 'f')
      .replace(/qu/g, 'k')
      .replace(/gu(?=[eiy])/g, 'G').replace(/g(?=[eiy])/g, 'j').replace(/G/g, 'g')
      .replace(/c(?=[eiy])/g, 's').replace(/c(?!h)/g, 'k')
      .replace(/(ai|ei)(?![nm])/g, 'e')
      .replace(/(ain|ein|in|im|un)(?![aeiouy])/g, '1')
      .replace(/(an|am|en|em)(?![aeiouy])/g, '2')
      .replace(/(on|om)(?![aeiouy])/g, '3')
      .replace(/([^aeiouy])\1/g, '$1')
      .replace(/[tdsxpz]$/, ''),
    // Anglais britannique : ph = f, ck = k, c doux, consonnes doublées, voyelle faible devant r final.
    en: (s) => norm(s)
      .replace(/ph/g, 'f').replace(/wh/g, 'w').replace(/ck/g, 'k').replace(/q/g, 'k')
      .replace(/c(?=[eiy])/g, 's').replace(/c/g, 'k')
      .replace(/([^aeiou])\1/g, '$1')
      .replace(/[aeiou]+r$/, 'R').replace(/re$/, 'R')
      .replace(/([^aeiou])e$/, '$1'),
    // Allemand : ä ≈ e, äu = eu, ei = ai, ie = i long, v = f, ß = ss, h de longueur muet,
    // voyelles et consonnes doublées, consonne finale durcie (Hund = Hunt).
    de: (s) => String(s).normalize('NFC').toLowerCase()
      .replace(/ß/g, 's')
      .replace(/äu|eu/g, 'Y').replace(/ei|ai|ey|ay/g, 'W').replace(/ie/g, 'i').replace(/ä/g, 'e')
      .replace(/ph/g, 'f').replace(/v/g, 'f').replace(/dt|th/g, 't')
      .replace(/ck/g, 'k').replace(/tz/g, 'z').replace(/qu/g, 'kw')
      .replace(/([aeiouöü])h(?![aeiouöüYW])/g, '$1')
      .replace(/([aeiouöü])\1/g, '$1')
      .replace(/([^aeiouöüYW])\1/g, '$1')
      .replace(/b$/, 'p').replace(/d$/, 't').replace(/g$/, 'k'),
    // Luxembourgeois : voyelles longues écrites doublées (aa, ee, ii, uu), v = f, consonnes
    // doublées, consonne finale durcie ; é, ë, ä restent distincts de e.
    lb: (s) => String(s).normalize('NFC').toLowerCase()
      .replace(/ph/g, 'f').replace(/v/g, 'f')
      .replace(/ck/g, 'k').replace(/tz/g, 'z').replace(/qu/g, 'kw')
      .replace(/([aeiou])\1/g, '$1')
      .replace(/([^aeiouäéëöüy])\1/g, '$1')
      .replace(/b$/, 'p').replace(/d$/, 't').replace(/g$/, 'ch'),
  };
  // Clé d'une tuile { s, py } : en chinois, le pinyin sans tons.
  function son(t) {
    if (hanzi()) return sansTon(t.py) || t.s;
    return (SONS[Ile.getLang()] || norm)(t.s);
  }

  const aUnTon = (p) => !!p && sansTon(p) !== String(p).normalize('NFC').toLowerCase();

  // Pinyin syllabe par syllabe d'un mot chinois (une syllabe par caractère), '' ailleurs.
  function pinyinSyl(m) {
    if (!hanzi()) return m.syl.map(() => '');
    const p = String(Ile.aide(m) || '').trim().split(/\s+/);
    return p.length === m.syl.length ? p : m.syl.map((s) => Ile.pinyin(s) || '');
  }

  // Mots connus de la langue courante : un piège ne doit jamais permettre d'écrire un autre vrai mot.
  const dicos = {};
  function dictionnaire() {
    const code = Ile.getLang();
    if (dicos[code]) return dicos[code];
    const P = Ile.L();
    const liste = [];
    (P.MOTS || []).forEach((m) => liste.push(m.mot));
    Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach((w) => liste.push(w)));
    (P.RIMES || []).forEach((r) => r.mots.forEach((w) => liste.push(w)));
    (P.CONTRAIRES || []).forEach((c) => liste.push(c.a, c.b));
    (P.PLURIELS || []).forEach((p) => liste.push(p.s, p.p));
    Object.keys(P.PINYIN || {}).forEach((w) => liste.push(w)); // chinois : mots des phrases, etc.
    dicos[code] = new Set(liste.map(norm));
    return dicos[code];
  }

  function syllabesOk(m) {
    return Array.isArray(m.syl) && m.syl.length >= 2 && m.syl.join('') === m.mot
      && m.syl.every((s) => /^\p{L}+$/u.test(s));
  }

  // Vrai si les vraies syllabes, dans un autre ordre, écrivent un autre mot connu.
  function autreOrdre(m) {
    const d = dictionnaire();
    const cible = norm(m.mot);
    const syl = m.syl;
    const pris = new Array(syl.length).fill(false);
    let trouve = false;
    (function explorer(s, n) {
      if (n === syl.length) { const x = norm(s); if (x !== cible && d.has(x)) trouve = true; return; }
      for (let k = 0; k < syl.length && !trouve; k++) {
        if (pris[k]) continue;
        pris[k] = true;
        explorer(s + syl[k], n + 1);
        pris[k] = false;
      }
    })('', 0);
    return trouve;
  }

  function choisirMots(level) {
    const ok = (m) => syllabesOk(m) && !autreOrdre(m);
    const groupes = REPARTITION[level] || REPARTITION[1];
    let mots = [];
    const deja = () => new Set(mots.map((m) => m.mot));
    groupes.forEach(([nb, combien]) => {
      const d = deja();
      mots = mots.concat(Ile.motsPourPartie(level, combien, (m) => ok(m) && !d.has(m.mot) && m.syl.length === nb));
    });
    const min = groupes[0][0];
    const max = groupes[groupes.length - 1][0];
    if (mots.length < TOTAL) { // on complète dans la fourchette du niveau
      const d = deja();
      mots = mots.concat(Ile.motsPourPartie(level, TOTAL - mots.length,
        (m) => ok(m) && !d.has(m.mot) && m.syl.length >= min && m.syl.length <= max));
    }
    if (mots.length < TOTAL) { // sinon : les mots les plus proches de la fourchette
      const d = deja();
      const ecart = (m) => (m.syl.length < min ? min - m.syl.length : Math.max(0, m.syl.length - max));
      const reste = Ile.shuffle(Ile.motsNiveau(level, (m) => ok(m) && !d.has(m.mot)))
        .sort((a, b) => ecart(a) - ecart(b));
      mots = mots.concat(reste.slice(0, TOTAL - mots.length));
    }
    // Du plus court au plus long : la difficulté monte doucement.
    return mots.slice(0, TOTAL).sort((a, b) => a.syl.length - b.syl.length);
  }

  // Réserve de pièges : les syllabes des autres mots du pack ({ s, py }), avec leur majuscule
  // d'origine (allemand, luxembourgeois). Chinois : un caractère et son pinyin dans ce mot-là,
  // seulement s'il porte un ton (子 de 兔子 se lit zi, mais zǐ tout seul : on l'écarte).
  function reservePieges(cible) {
    const forme = hanzi() ? /^\p{Script=Han}$/u : /^\p{L}{2,5}$/u;
    const res = new Map();
    (Ile.L().MOTS || []).forEach((m) => {
      if (m.mot === cible.mot || !Array.isArray(m.syl)) return;
      const py = pinyinSyl(m);
      m.syl.forEach((x, k) => {
        if (forme.test(x) && !res.has(norm(x)) && (!hanzi() || aUnTon(py[k]))) res.set(norm(x), { s: x, py: py[k] });
      });
    });
    return [...res.values()];
  }

  // Ressemblance entre un piège et une vraie syllabe (même début, même fin, longueur proche) ;
  // en chinois, d'après le pinyin sans tons (même initiale, même finale).
  function ressemblance(t, v) {
    const a = hanzi() ? sansTon(t.py) : t.s.toLowerCase();
    const b = hanzi() ? sansTon(v.py) : v.s.toLowerCase();
    let r = 0;
    if (a[0] === b[0]) r += 2;
    if (a[a.length - 1] === b[b.length - 1]) r += 1;
    if (Math.abs(a.length - b.length) <= 1) r += 1;
    return r;
  }

  // Vrai si, avec au moins un piège, on peut écrire un autre mot connu.
  function formeUnAutreMot(vraies, pieges, cibleN) {
    const d = dictionnaire();
    const tuiles = vraies.concat(pieges);
    const nV = vraies.length;
    const pris = new Array(tuiles.length).fill(false);
    let trouve = false;
    (function explorer(s, avecPiege) {
      if (avecPiege) {
        const n = norm(s);
        if (n !== cibleN && d.has(n)) { trouve = true; return; }
      }
      for (let k = 0; k < tuiles.length && !trouve; k++) {
        if (pris[k]) continue;
        pris[k] = true;
        explorer(s + tuiles[k], avecPiege || k >= nV);
        pris[k] = false;
      }
    })('', false);
    return trouve;
  }

  function choisirPieges(cible, vraies, n) {
    if (!n) return [];
    const vraiesN = vraies.map((v) => norm(v.s));
    const vraiesSon = vraies.map(son);
    const motN = norm(cible.mot);
    // Jamais une vraie syllabe du mot (même sans les accents ni la majuscule), ni un morceau du mot,
    // ni une syllabe qui se prononce comme une vraie.
    const candidats = reservePieges(cible).filter((t) => {
      const tn = norm(t.s);
      return vraiesN.indexOf(tn) === -1 && motN.indexOf(tn) === -1 && vraiesSon.indexOf(son(t)) === -1;
    });
    const note = (t) => Math.max.apply(null, vraies.map((v) => ressemblance(t, v)));
    const tries = Ile.shuffle(candidats).sort((a, b) => note(b) - note(a)); // tri stable : hasard entre ex æquo
    const meilleurs = tries.slice(0, Math.max(10, n * 5));
    const textes = vraies.map((v) => v.s);
    for (let essai = 0; essai < 80; essai++) {
      const choix = Ile.pick(essai < 50 ? meilleurs : tries, n);
      const distincts = new Set(choix.map(son)).size === choix.length; // deux pièges qui sonnent pareil : non
      if (choix.length === n && distincts && !formeUnAutreMot(textes, choix.map((t) => t.s), motN)) return choix;
    }
    return []; // jamais bloquant : au pire, pas de piège
  }

  // Mélange des tuiles : à partir de 3 tuiles, jamais déjà rangées dans l'ordre du mot.
  function melanger(tuiles, vraies) {
    let m = Ile.shuffle(tuiles);
    const range = (arr) => arr.slice(0, vraies.length).every((t, k) => t.s === vraies[k]);
    for (let essai = 0; essai < 20 && tuiles.length >= 3 && range(m); essai++) m = Ile.shuffle(tuiles);
    return m;
  }

  // Contenu d'une tuile, d'une planche ou d'un bloc du mot : la syllabe, et son pinyin dessous en chinois.
  function contenuSyllabe(s, py) {
    const noeuds = [el('span', { class: 'syl-txt', text: s })];
    if (py) noeuds.push(el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin', text: py }));
    return noeuds;
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const mots = choisirMots(level);
      const total = mots.length;
      const nbPieges = PIEGES[level] || 0;
      const avecPinyin = hanzi();
      let i = 0;
      let score = 0;
      let q = null; // question en cours
      let clavier = false; // l'enfant joue au clavier : on garde le focus dans le jeu

      const panel = el('section', { class: 'panel syllabes' + (avecPinyin ? ' syllabes--hanzi' : ''), 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      panel.addEventListener('pointerdown', () => { clavier = false; });
      panel.addEventListener('keydown', (e) => { if (e.key === 'Tab' || e.key === 'Enter' || e.key === ' ') clavier = true; });

      function next() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= total) {
          q = null;
          // Score honnête : un point par mot construit sans erreur, comme l'annonce le message de fin.
          Ile.progress(panel, total, total, score);
          Ile.showResult({ id: ID, score, total, message: typo(score === total ? tx.finParfaite : tx.finNormale) });
          return;
        }
        q = question(mots[i]);
      }

      function question(cible) {
        const ctrl = {};
        const py = pinyinSyl(cible);
        const vraies = cible.syl.map((s, k) => ({ s, py: py[k] }));
        const n = vraies.length;
        const nom = Ile.groupe(Ile.un(cible), cible.mot);
        const pieges = choisirPieges(cible, vraies, nbPieges);
        const tuiles = melanger(
          vraies.map((v) => ({ s: v.s, py: v.py, piege: false })).concat(pieges.map((t) => ({ s: t.s, py: t.py, piege: true }))),
          cible.syl
        );
        let etape = 0; // nombre de syllabes posées
        let erreur = false;
        let erreursEtape = 0;
        let fini = false;
        const cachee = level >= 3;

        panel.querySelectorAll(':scope > :not(.progress)').forEach((node) => node.remove());
        panel.dataset.question = String(i);
        Ile.progress(panel, i, total, score);

        // --- Image (cachée jusqu'à la fin au niveau 3) ---
        const image = el('div', { class: 'big-emoji syl-image', role: 'img' });
        function montrerImage(trouve) {
          image.textContent = cible.emoji;
          image.classList.remove('is-mystere');
          image.setAttribute('aria-label', trouve ? tx.imageDe(nom) : tx.image);
        }
        if (cachee) {
          image.textContent = '?';
          image.classList.add('is-mystere');
          image.setAttribute('aria-label', tx.imageCachee);
        } else montrerImage(false);
        const figure = el('div', { class: 'syl-figure' + (cachee ? ' syl-figure--cachee' : '') }, [image]);
        if (cachee) figure.appendChild(el('p', { class: 'syl-theme', text: typo(tx.themes[cible.theme] || tx.themeAutre) }));
        else figure.appendChild(Ile.speakButton(cible.mot, tx.ecouterMot));

        // --- Le pont : une planche par syllabe ---
        // Chaque planche est d'autant plus large que sa syllabe (ou son pinyin) est longue.
        const poids = vraies.map((v) => Math.max(Array.from(v.s).length * (avecPinyin ? 2 : 1), v.py ? v.py.length * 0.8 : 0) + 2);
        const poidsTotal = poids.reduce((a, b) => a + b, 0);
        const singe = el('span', { class: 'pont__singe', 'aria-hidden': 'true', text: '🐒' });
        const planches = vraies.map((_, k) => el('span', { class: 'planche is-vide', style: 'flex-grow:' + poids[k] }));
        const tablier = el('div', { class: 'pont__tablier' }, [singe].concat(planches));
        const pont = el('div', { class: 'pont', role: 'group', style: '--n:' + n }, [
          el('div', { class: 'pont__rive pont__rive--g', 'aria-hidden': 'true' }),
          tablier,
          el('div', { class: 'pont__rive pont__rive--d', 'aria-hidden': 'true' }),
        ]);
        function majPont() {
          pont.setAttribute('aria-label', tx.pont(etape, n));
          if (fini) singe.style.left = 'calc(100% + var(--rive) / 2)';
          else if (etape === 0) singe.style.left = 'calc(var(--rive) / -2)';
          else { // au milieu de la dernière planche posée
            const avant = poids.slice(0, etape - 1).reduce((a, b) => a + b, 0);
            singe.style.left = ((avant + poids[etape - 1] / 2) / poidsTotal) * 100 + '%';
          }
        }

        // --- Tuiles-syllabes ---
        const tuilesEl = el('div', { class: 'tiles syl-tuiles', role: 'group', 'aria-label': tx.tuiles });
        tuiles.forEach((t) => {
          t.btn = el('button', { type: 'button', class: 'tile syl-tuile', 'data-syl': t.s, 'aria-label': tx.syllabe(t.s) },
            contenuSyllabe(t.s, t.py));
          t.btn.addEventListener('click', () => toucher(t));
          tuilesEl.appendChild(t.btn);
        });

        const fb = el('p', { class: 'feedback syl-feedback', 'aria-live': 'polite' });
        // Lecteurs d'écran : chaque planche posée est annoncée (le message visible, lui, s'efface).
        const annonce = el('p', { class: 'sr-only', 'aria-live': 'polite' });

        panel.append(
          el('p', { class: 'consigne', text: typo(cachee ? tx.consigneCachee : tx.consigne) }),
          figure,
          pont,
          tuilesEl,
          fb,
          annonce
        );
        majPont();

        // Le bouton touché va être désactivé : on garde le focus sur une syllabe voisine.
        function garderFocus(btn) {
          if (document.activeElement !== btn) return;
          const libres = tuiles.filter((t) => !t.pose && !t.ecartee && t.btn !== btn).map((t) => t.btn);
          const apres = libres.find((b) => btn.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING);
          const autre = apres || libres[libres.length - 1];
          if (autre) autre.focus({ preventScroll: true });
        }

        function toucher(t) {
          if (fini || t.pose || t.ecartee || q !== ctrl || !panel.isConnected) return;
          Ile.say(t.s);
          const attendu = vraies[etape];
          if (t.s === attendu.s) { // syllabes identiques (bon·bon, 星星) : n'importe laquelle convient
            garderFocus(t.btn);
            t.pose = true;
            t.btn.disabled = true;
            t.btn.classList.add('is-used');
            t.btn.classList.remove('is-aide');
            t.btn.setAttribute('aria-label', tx.syllabePosee);
            const p = planches[etape];
            // La planche montre le pinyin de SA place (星星 : xīng puis xing).
            p.replaceChildren(...contenuSyllabe(attendu.s, attendu.py));
            p.classList.remove('is-vide');
            p.classList.add('is-posee');
            Ile.flash(p, 'good');
            etape++;
            erreursEtape = 0;
            tuiles.forEach((x) => x.btn.classList.remove('is-aide'));
            if (etape === n) { reussite(); return; }
            majPont();
            Ile.sfx('click');
            fb.textContent = '';
            fb.className = 'feedback syl-feedback';
            annonce.textContent = typo(tx.pont(etape, n));
            return;
          }
          erreur = true;
          erreursEtape++;
          Ile.flash(t.btn, 'bad');
          Ile.sfx('bad');
          let msg;
          if (t.piege) { // le piège est écarté : la partie ne peut jamais bloquer
            garderFocus(t.btn);
            t.ecartee = true;
            t.btn.disabled = true;
            t.btn.classList.add('is-wrong');
            t.btn.setAttribute('aria-label', tx.syllabeEcartee(t.s));
            msg = tx.pasDansMot(t.s);
          } else {
            msg = tx.plusLoin(t.s);
          }
          if (erreursEtape >= 2) { // coup de pouce : la bonne syllabe brille
            const bonne = tuiles.find((x) => !x.pose && !x.piege && x.s === attendu.s);
            if (bonne) {
              bonne.btn.classList.add('is-aide');
              msg = t.piege ? suite(msg, tx.essaieBrille) : tx.pasEncoreBrille;
            }
          }
          Ile.feedback(fb, false, typo(msg));
          fb.classList.add('syl-feedback');
        }

        function reussite() {
          fini = true;
          const parfait = !erreur;
          if (parfait) score++;
          majPont();
          pont.classList.add('is-fini');
          montrerImage(true);
          Ile.flash(image, 'good');
          Ile.sfx('good');
          // Le mot entier, syllabes en couleurs alternées (et pinyin sous chaque caractère en chinois).
          const motEl = el('p', { class: 'mot-affiche syl-mot', 'data-mot': cible.mot },
            vraies.map((v, k) => el('span', { class: 'syl-bloc ' + (k % 2 ? 'syl-b' : 'syl-a') }, contenuSyllabe(v.s, v.py))));
          tuilesEl.replaceWith(motEl);
          const bravo = parfait ? Ile.pick(Ile.t('bravo'), 1)[0] : tx.reussi;
          Ile.feedback(fb, true, typo(suite(bravo, tx.cest(nom))));
          fb.classList.add('syl-feedback');
          setTimeout(() => { if (panel.isConnected && q === ctrl) Ile.say(nom); }, 600);
          i++;
          Ile.progress(panel, i, total, score);
          setTimeout(next, PAUSE);
        }

        if (clavier) {
          const premiere = tuiles.find((t) => !t.pose && !t.ecartee);
          if (premiere) premiere.btn.focus({ preventScroll: true });
        }
        return ctrl;
      }

      next();
    },
  });
})();
