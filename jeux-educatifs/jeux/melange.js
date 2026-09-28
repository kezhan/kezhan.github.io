/* Lettres en vrac : les lettres du mot sont mélangées, remets-les dans l'ordre.
 * Niveau 1 : mots de 3 à 5 lettres, image visible ; niveau 2 : 4 à 7 lettres ; niveau 3 : 7 lettres et plus,
 * image cachée (indice de thème, bouton « voir l'image »).
 * Allemand et luxembourgeois : la majuscule du nom reste sur sa tuile (Katze : K, a, t, z, e) : indice naturel.
 * Chinois : on remet dans l'ordre les lettres du pinyin sans tons (Ile.epeler : 熊猫 → xiongmao) ; les caractères
 * restent affichés comme aide (avec le bouton écouter, à tous les niveaux) et le pinyin avec ses tons apparaît
 * dessous après la réussite.
 * Score : un point par mot trouvé sans indice ni erreur.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'melange';
  const TOTAL = 8;

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      consigne: 'Put the letters in the right order to spell the word.',
      consigneCachee: 'Which word is hiding in these letters?',
      astuceMajuscule: '',
      ecouterMot: 'Listen to the word',
      image: 'Picture of the word to spell',
      imageCachee: 'Hidden picture',
      imageDe: (g) => 'Picture: ' + g,
      cases: 'The word to spell',
      reserve: 'Jumbled letters',
      caseVide: (k) => 'Space ' + k + ', empty',
      casePosee: (k, c) => 'Space ' + k + ': ' + c + ', tap to take it out',
      caseBloquee: (k, c) => 'Space ' + k + ': ' + c + ', in the right place',
      lettre: (c) => 'Letter ' + c,
      lettrePosee: 'Letter already used',
      indice: '💡 Hint',
      voirImage: '🖼️ Show the picture',
      presque: 'Nearly! The green letters are in the right place.',
      pasEncore: 'Not yet! Try a different order.',
      reussi: 'You did it!',
      cest: (g) => 'It’s ' + g + '.',
      coupDePouce: (c) => '💡 Here’s a clue: the letter “' + c + '”.',
      voiciImage: '🖼️ Here’s the picture to help you!',
      themes: {
        animaux: '🐾 It’s an animal.',
        nourriture: '🍽️ You can eat it.',
        nature: '🌿 You find it in nature.',
        objets: '🧰 It’s a thing.',
        corps: '🖐️ It’s a part of the body.',
        personnes: '🧑 It’s a person.',
      },
      themeAutre: '🔎 Guess the word!',
      finParfaite: 'Every word found without any help: well done, letter pirate!',
      finNormale: 'You get a point for each word found with no hints and no mistakes. Have another go!',
    },
    zh: {
      consigne: '把字母排好，拼出这个词的拼音。',
      consigneCachee: '把字母排好，拼出这个词的拼音。',
      astuceMajuscule: '',
      ecouterMot: '听一听这个词',
      image: '要拼的词的图片',
      imageCachee: '藏起来的图片',
      imageDe: (g) => '图片：' + g,
      cases: '要拼的拼音',
      reserve: '打乱的字母',
      caseVide: (k) => '第 ' + k + ' 格，空的',
      casePosee: (k, c) => '第 ' + k + ' 格：' + c + '，点一下拿走',
      caseBloquee: (k, c) => '第 ' + k + ' 格：' + c + '，放对了',
      lettre: (c) => '字母 ' + c,
      lettrePosee: '这个字母已经放好了',
      indice: '💡 提示',
      voirImage: '🖼️ 看图片',
      presque: '差一点！绿色的字母放对了。',
      pasEncore: '还不对！换个顺序试试。',
      reussi: '你拼出来了！',
      cest: (g) => '这是' + g + '。',
      coupDePouce: (c) => '💡 提示：这里是字母“' + c + '”。',
      voiciImage: '🖼️ 看看图片，它能帮你！',
      themes: {
        animaux: '🐾 这是一种动物。',
        nourriture: '🍽️ 这是可以吃的东西。',
        nature: '🌿 这是大自然里的东西。',
        objets: '🧰 这是人们用的东西。',
        corps: '🖐️ 这是身体的一部分。',
        personnes: '🧑 这是一个人。',
      },
      themeAutre: '🔎 猜一猜是什么词！',
      finParfaite: '每个词都没用提示就拼出来了：你是拼音小海盗！',
      finNormale: '没用提示、也没出错的词，每个得一分。再试一次吧！',
    },
    de: {
      consigne: 'Bring die Buchstaben in die richtige Reihenfolge.',
      consigneCachee: 'Welches Wort versteckt sich in diesen Buchstaben?',
      astuceMajuscule: 'Tipp: Der große Buchstabe kommt nach vorn.',
      ecouterMot: 'Wort anhören',
      image: 'Bild zum gesuchten Wort',
      imageCachee: 'Verstecktes Bild',
      imageDe: (g) => 'Bild: ' + g,
      cases: 'Das gesuchte Wort',
      reserve: 'Durcheinandergewürfelte Buchstaben',
      caseVide: (k) => 'Feld ' + k + ', leer',
      casePosee: (k, c) => 'Feld ' + k + ': ' + c + ', antippen zum Entfernen',
      caseBloquee: (k, c) => 'Feld ' + k + ': ' + c + ', richtig',
      lettre: (c) => 'Buchstabe ' + c,
      lettrePosee: 'Buchstabe schon gelegt',
      indice: '💡 Tipp',
      voirImage: '🖼️ Bild zeigen',
      presque: 'Fast! Die grünen Buchstaben stehen schon richtig.',
      pasEncore: 'Noch nicht! Probier eine andere Reihenfolge.',
      reussi: 'Geschafft!',
      cest: (g) => 'Das ist ' + g + '.',
      coupDePouce: (c) => '💡 Kleiner Tipp: Hier kommt der Buchstabe „' + c + '“.',
      voiciImage: '🖼️ Hier ist das Bild als Hilfe!',
      themes: {
        animaux: '🐾 Es ist ein Tier.',
        nourriture: '🍽️ Man kann es essen.',
        nature: '🌿 Es kommt in der Natur vor.',
        objets: '🧰 Es ist ein Gegenstand.',
        corps: '🖐️ Es ist ein Körperteil.',
        personnes: '🧑 Es ist ein Mensch.',
      },
      themeAutre: '🔎 Rate das Wort!',
      finParfaite: 'Alle Wörter ohne Hilfe gefunden: Bravo, Buchstabenpirat!',
      finNormale: 'Für jedes Wort ohne Tipp und ohne Fehler gibt es einen Punkt. Versuch es noch einmal!',
    },
    lb: {
      consigne: 'Setz d’Buschtawen an déi richteg Reiefolleg.',
      consigneCachee: 'Wat fir e Wuert verstoppt sech an dëse Buschtawen?',
      astuceMajuscule: 'Tipp: De grousse Buschtaf kënnt no vir.',
      ecouterMot: 'D’Wuert lauschteren',
      image: 'Bild vum Wuert',
      imageCachee: 'Verstoppt Bild',
      imageDe: (g) => 'Bild: ' + g,
      cases: 'D’Wuert',
      reserve: 'Buschtawen',
      caseVide: (k) => 'Feld ' + k + ', eidel',
      casePosee: (k, c) => 'Feld ' + k + ': ' + c,
      caseBloquee: (k, c) => 'Feld ' + k + ': ' + c + ', richteg',
      lettre: (c) => 'Buschtaf ' + c,
      lettrePosee: 'Buschtaf scho gesat',
      indice: '💡 Tipp',
      voirImage: '🖼️ Bild weisen',
      presque: 'Bal! Déi gréng Buschtawe sinn op der richteger Plaz.',
      pasEncore: 'Nach net! Probéier eng aner Reiefolleg.',
      reussi: 'Du hues et gepackt!',
      cest: (g) => 'Dat ass ' + g + '.',
      coupDePouce: (c) => '💡 E klengen Tipp: hei ass de Buschtaf „' + c + '“.',
      voiciImage: '🖼️ Hei ass d’Bild, fir der ze hëllefen!',
      themes: {
        animaux: '🐾 Et ass en Déier.',
        nourriture: '🍽️ Dat kann een iessen.',
        nature: '🌿 Et ass an der Natur.',
        objets: '🧰 Et ass eng Saach.',
        corps: '🖐️ Et ass en Deel vum Kierper.',
        personnes: '🧑 Et ass e Mënsch.',
      },
      themeAutre: '🔎 Fann d’Wuert!',
      finParfaite: 'All d’Wierder ouni Hëllef fonnt: Bravo, Buschtawe-Pirat!',
      finNormale: 'Fir all Wuert ouni Tipp an ouni Feeler gëtt et e Punkt. Probéier nach eng Kéier!',
    },
    fr: {
      consigne: 'Remets les lettres dans l’ordre pour écrire le mot.',
      consigneCachee: 'Quel mot se cache dans ces lettres ?',
      astuceMajuscule: '',
      ecouterMot: 'Écouter le mot',
      image: 'Image du mot à écrire',
      imageCachee: 'Image cachée',
      imageDe: (g) => 'Image : ' + g,
      cases: 'Le mot à écrire',
      reserve: 'Lettres mélangées',
      caseVide: (k) => 'Case ' + k + ', vide',
      casePosee: (k, c) => 'Case ' + k + ' : ' + c + ', touche pour l’enlever',
      caseBloquee: (k, c) => 'Case ' + k + ' : ' + c + ', bien placée',
      lettre: (c) => 'Lettre ' + c,
      lettrePosee: 'Lettre déjà posée',
      indice: '💡 Indice',
      voirImage: '🖼️ Voir l’image',
      presque: 'Presque ! Les lettres vertes sont à la bonne place.',
      pasEncore: 'Pas encore ! Essaie un autre ordre.',
      reussi: 'Tu as réussi !',
      cest: (g) => 'C’est ' + g + '.',
      coupDePouce: (c) => '💡 Coup de pouce : voici la lettre « ' + c + ' ».',
      voiciImage: '🖼️ Voici l’image pour t’aider !',
      themes: {
        animaux: '🐾 C’est un animal.',
        nourriture: '🍽️ Ça se mange.',
        nature: '🌿 C’est dans la nature.',
        objets: '🧰 C’est un objet.',
        corps: '🖐️ C’est une partie du corps.',
        personnes: '🧑 C’est une personne.',
      },
      themeAutre: '🔎 Devine le mot !',
      finParfaite: 'Tous les mots trouvés sans aide : bravo, pirate des lettres !',
      finNormale: 'Un point par mot trouvé sans indice ni erreur. Tu peux réessayer !',
    },
  };

  // Typographie française : espace insécable avant ! ? : ; » (les textes du pack commun en ont besoin aussi).
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, ' $1').replace(/« /g, '« ');
  }

  const lettresDe = (mot) => Array.from(String(mot).normalize('NFC'));
  // Ce qu'on épelle : le mot, ou son pinyin sans tons en chinois (熊猫 → xiongmao).
  const forme = (m) => String(Ile.epeler(m)).normalize('NFC');
  const minuscule = (c) => c === c.toLowerCase();

  // Longueur des mots selon le niveau.
  const FILTRES = {
    1: (n) => n >= 3 && n <= 5,
    2: (n) => n >= 4 && n <= 7,
    3: (n) => n >= 7,
  };

  // Tous les mots connus de la langue, sous la forme qu'on épelle (pour écarter les anagrammes quand
  // l'image est cachée). En chinois : le pinyin sans tons des mots du pack.
  const lexiques = {};
  function lexique() {
    const code = Ile.getLang();
    if (lexiques[code]) return lexiques[code];
    const P = Ile.L();
    const liste = [];
    if (P.ecriture === 'hanzi') {
      const sansTon = (p) => String(p).normalize('NFD').replace(/[̀-̇̉-ͯ]/g, '').normalize('NFC').replace(/\s+/g, '');
      (P.MOTS || []).forEach((m) => liste.push(forme(m)));
      Object.keys(P.PINYIN || {}).forEach((k) => liste.push(sansTon(P.PINYIN[k])));
    } else {
      (P.MOTS || []).forEach((m) => liste.push(m.mot));
      Object.keys(P.MOTS_SIMPLES || {}).forEach((k) => P.MOTS_SIMPLES[k].forEach((w) => liste.push(w)));
      (P.RIMES || []).forEach((r) => r.mots.forEach((w) => liste.push(w)));
      (P.CONTRAIRES || []).forEach((c) => liste.push(c.a, c.b));
      (P.PLURIELS || []).forEach((p) => liste.push(p.s, p.p));
    }
    lexiques[code] = liste.map((w) => String(w).normalize('NFC').toLowerCase());
    return lexiques[code];
  }
  const cleAnagramme = (w) => lettresDe(w.toLowerCase()).sort().join('');
  function aUnAnagramme(mot) {
    const k = cleAnagramme(mot);
    const m = mot.toLowerCase();
    return lexique().some((w) => w !== m && cleAnagramme(w) === k);
  }

  // Un mot jouable : uniquement des lettres, et au moins deux lettres différentes
  // (un mot fait d'une seule lettre répétée ne peut pas être mélangé).
  function jouable(m) {
    const f = forme(m);
    return /^\p{L}+$/u.test(f) && new Set(lettresDe(f)).size > 1;
  }

  function choisirMots(level) {
    const f = FILTRES[level] || FILTRES[1];
    // Image cachée (niveau 3) : le mot doit être le seul possible avec ces lettres.
    const ok = (m) => jouable(m) && (level < 3 || !aUnAnagramme(forme(m)));
    let mots = Ile.motsPourPartie(level, TOTAL, (m) => ok(m) && f(lettresDe(forme(m)).length));
    if (mots.length < TOTAL) { // sécurité : on complète sans contrainte de longueur
      const deja = new Set(mots.map((m) => m.mot));
      mots = mots.concat(Ile.motsPourPartie(level, TOTAL, (m) => ok(m) && !deja.has(m.mot))).slice(0, TOTAL);
    }
    // Du plus court au plus long : la difficulté monte doucement.
    return mots.sort((a, b) => lettresDe(forme(a)).length - lettresDe(forme(b)).length);
  }

  // Mélange qui ne redonne jamais le mot, et laisse le moins possible de lettres à leur place.
  function melanger(lettres) {
    const mot = lettres.join('');
    if (new Set(lettres).size < 2) return lettres.slice(); // aucun autre ordre possible (mot écarté en amont)
    let meilleur = null;
    let fixes = Infinity;
    for (let essai = 0; essai < 60 && fixes > 0; essai++) {
      const m = Ile.shuffle(lettres);
      if (m.join('') === mot) continue;
      const f = m.reduce((n, c, k) => n + (c === lettres[k] ? 1 : 0), 0);
      if (f < fixes) { meilleur = m; fixes = f; }
    }
    if (meilleur) return meilleur;
    // Filet de sécurité : un décalage d'un cran qui change le mot (il existe, car au moins deux lettres diffèrent).
    for (let d = 1; d < lettres.length; d++) {
      const m = lettres.slice(d).concat(lettres.slice(0, d));
      if (m.join('') !== mot) return m;
    }
    return lettres.slice();
  }

  const px = (v) => parseFloat(v) || 0;

  // Répartit les cases en lignes équilibrées quand le mot ne tient pas sur une ligne (mobile).
  function disposer(conteneur, nb) {
    const parent = conteneur.parentElement;
    if (!parent) return;
    const ps = getComputedStyle(parent);
    const cs = getComputedStyle(conteneur);
    const cell = px(cs.getPropertyValue('--cell')) || 52;
    const gap = px(cs.columnGap);
    const dispo = parent.clientWidth - px(ps.paddingLeft) - px(ps.paddingRight)
      - px(cs.paddingLeft) - px(cs.paddingRight) - px(cs.borderLeftWidth) - px(cs.borderRightWidth);
    const parLigne = Math.max(1, Math.floor((dispo + gap) / (cell + gap)));
    const lignes = Math.ceil(nb / parLigne);
    conteneur.style.setProperty('--cols', String(Math.ceil(nb / lignes)));
  }

  // Écouteurs globaux de la partie en cours (retirés à chaque nouvelle partie).
  let ecouteurs = null;

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const hanzi = Ile.L().ecriture === 'hanzi';
      if (ecouteurs) ecouteurs.retirer();

      const mots = choisirMots(level);
      const total = mots.length;
      let i = 0;
      let score = 0;
      let q = null; // question en cours
      let clavier = false; // l'enfant joue au clavier : on garde le focus dans le jeu

      const panel = el('section', { class: 'panel melange' + (hanzi ? ' melange--zh' : '') });
      root.appendChild(panel);

      function onKey(e) {
        if (!panel.isConnected) { retirer(); return; }
        if (e.key === 'Tab') { clavier = true; return; }
        if (!q || document.querySelector('.result')) return;
        if (e.ctrlKey || e.metaKey || e.altKey || e.isComposing) return;
        const t = e.target;
        if (t && (/^(INPUT|SELECT|TEXTAREA)$/.test(t.tagName) || t.isContentEditable)) return;
        if (e.key === 'Backspace' || e.key === 'Delete') {
          e.preventDefault();
          clavier = true;
          q.effacerDerniere();
        } else if (e.key.length === 1 && /\p{L}/u.test(e.key)) {
          e.preventDefault();
          if (e.repeat) return;
          clavier = true;
          q.taper(e.key.normalize('NFC').toLowerCase());
        }
      }
      function onResize() {
        if (!panel.isConnected) { retirer(); return; }
        if (q) q.disposer();
      }
      function retirer() {
        document.removeEventListener('keydown', onKey);
        window.removeEventListener('resize', onResize);
      }
      document.addEventListener('keydown', onKey);
      window.addEventListener('resize', onResize);
      ecouteurs = { retirer };
      panel.addEventListener('pointerdown', () => { clavier = false; });

      function next() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= total) {
          q = null;
          // Score honnête : un point par mot trouvé sans indice ni erreur (le message le dit).
          Ile.progress(panel, total, total, score);
          Ile.showResult({ id: ID, score, total, message: typo(score === total ? tx.finParfaite : tx.finNormale) });
          return;
        }
        q = question(mots[i]);
      }

      function question(cible) {
        const lettres = lettresDe(forme(cible));
        const n = lettres.length;
        const nom = Ile.groupe(Ile.un(cible), cible.mot);
        const tuiles = melanger(lettres).map((c) => ({ c, pos: -1, btn: null }));
        const cases = new Array(n).fill(null); // tuile posée dans chaque case
        const bloquee = new Array(n).fill(false); // lettre validée (bien placée ou indice)
        const aidee = new Array(n).fill(false); // lettre posée par un indice
        let aide = false; // indice ou image utilisés
        let erreur = false;
        let occupe = false; // animation en cours : entrées bloquées
        let fini = false;
        let imageVue = level < 3;

        panel.querySelectorAll(':scope > :not(.progress)').forEach((node) => node.remove());
        panel.dataset.question = String(i);
        Ile.progress(panel, i, total, score);

        // --- Image (cachée au niveau 3) ---
        const image = el('div', { class: 'big-emoji mel-image', role: 'img' });
        function montrerImage(trouve) {
          image.textContent = cible.emoji;
          image.classList.remove('is-mystere');
          image.setAttribute('aria-label', trouve ? tx.imageDe(nom) : tx.image);
        }
        if (imageVue) montrerImage(false);
        else {
          image.textContent = '?';
          image.classList.add('is-mystere');
          image.setAttribute('aria-label', tx.imageCachee);
        }
        const figure = el('div', { class: 'mel-figure' + (level < 3 ? '' : ' mel-figure--cachee') }, [image]);
        // Chinois : les caractères restent affichés (aide), le pinyin avec ses tons apparaît après la réussite.
        let pinyin = null;
        if (hanzi) {
          pinyin = el('span', { class: 'mel-pinyin', lang: 'zh-Latn-pinyin', text: Ile.aide(cible), hidden: true });
          figure.appendChild(el('p', { class: 'mel-hanzi' }, [el('span', { class: 'mel-hanzi__mot', text: cible.mot }), pinyin]));
        }
        // Écouter : aux niveaux 1 et 2, et toujours en chinois (les caractères sont affichés, la voix aide
        // à retrouver le pinyin). Au niveau 3 des autres langues, elle donnerait le mot caché.
        if (level < 3 || hanzi) figure.appendChild(Ile.speakButton(cible.mot, tx.ecouterMot));
        if (level >= 3) figure.appendChild(el('p', { class: 'mel-theme', text: typo(tx.themes[cible.theme] || tx.themeAutre) }));

        // --- Cases du mot ---
        const casesEl = el('div', { class: 'slots mel-cases', role: 'group', 'aria-label': tx.cases });
        const caseBtns = lettres.map((_, k) => {
          const b = el('button', { type: 'button', class: 'mel-case' });
          b.addEventListener('click', () => retirerCase(k, true));
          casesEl.appendChild(b);
          return b;
        });

        // --- Lettres mélangées ---
        const reserve = el('div', { class: 'tiles mel-reserve', role: 'group', 'aria-label': tx.reserve });
        tuiles.forEach((t) => {
          t.btn = el('button', { type: 'button', class: 'tile mel-tuile', text: t.c });
          t.btn.addEventListener('click', () => poser(t));
          reserve.appendChild(t.btn);
        });

        // --- Aides ---
        const btnIndice = el('button', { type: 'button', class: 'btn btn--sun', text: tx.indice });
        btnIndice.addEventListener('click', indice);
        const actions = el('div', { class: 'actions mel-actions' }, [btnIndice]);
        let btnImage = null;
        if (!imageVue) {
          btnImage = el('button', { type: 'button', class: 'btn', text: tx.voirImage });
          btnImage.addEventListener('click', voirImage);
          actions.appendChild(btnImage);
        }

        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });

        // Allemand, luxembourgeois (niveau 1) : rappel que le nom commence par sa majuscule.
        const majuscule = level === 1 && tx.astuceMajuscule && !minuscule(lettres[0]);
        panel.append(...[
          el('p', { class: 'consigne', text: typo(level < 3 ? tx.consigne : tx.consigneCachee) }),
          majuscule ? el('p', { class: 'mel-astuce', text: tx.astuceMajuscule }) : null,
          figure,
          casesEl,
          reserve,
          actions,
          fb,
        ].filter(Boolean));

        function info(texte) {
          fb.textContent = typo(texte);
          fb.className = 'feedback mel-info';
          montrerFeedback();
        }
        // Mobile, mots longs : le message peut tomber sous le bord de l'écran. On le fait remonter,
        // seulement s'il est caché (les cases restent visibles).
        function montrerFeedback() {
          requestAnimationFrame(() => {
            if (!panel.isConnected || !fb.textContent) return;
            const r = fb.getBoundingClientRect();
            if (r.bottom > window.innerHeight) fb.scrollIntoView({ block: 'end', behavior: Ile.reduceMotion ? 'auto' : 'smooth' });
          });
        }

        function rendu() {
          caseBtns.forEach((b, k) => {
            const t = cases[k];
            b.textContent = t ? t.c : '';
            b.classList.toggle('is-filled', !!t);
            b.classList.toggle('is-locked', bloquee[k] && !aidee[k]);
            b.classList.toggle('is-aidee', aidee[k]);
            b.disabled = !t || bloquee[k] || fini;
            b.setAttribute('aria-label', !t ? tx.caseVide(k + 1)
              : bloquee[k] ? tx.caseBloquee(k + 1, t.c) : tx.casePosee(k + 1, t.c));
          });
          tuiles.forEach((t) => {
            const posee = t.pos !== -1;
            t.btn.disabled = posee || fini;
            t.btn.classList.toggle('is-used', posee);
            t.btn.setAttribute('aria-label', posee ? tx.lettrePosee : tx.lettre(t.c));
          });
          btnIndice.disabled = fini;
          if (btnImage) btnImage.disabled = fini || imageVue;
        }

        // Le bouton touché va être désactivé : on garde le focus sur une lettre voisine.
        function garderFocus(btn) {
          if (document.activeElement !== btn) return;
          const libres = tuiles.filter((t) => t.pos === -1 && t.btn !== btn).map((t) => t.btn);
          const apres = libres.find((b) => btn.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING);
          const autre = apres || libres[libres.length - 1];
          if (autre) autre.focus({ preventScroll: true });
          else btnIndice.focus({ preventScroll: true });
        }

        function poser(t) {
          if (occupe || fini || t.pos !== -1) return;
          const k = cases.indexOf(null);
          if (k === -1) return;
          garderFocus(t.btn);
          cases[k] = t;
          t.pos = k;
          Ile.sfx('click');
          rendu();
          Ile.flash(caseBtns[k], 'good');
          if (cases.indexOf(null) === -1) verifier();
        }

        function retirerCase(k, parClic) {
          if (occupe || fini || bloquee[k] || !cases[k]) return;
          const t = cases[k];
          const avaitFocus = document.activeElement === caseBtns[k];
          cases[k] = null;
          t.pos = -1;
          Ile.sfx('flip');
          rendu();
          if (parClic && avaitFocus) t.btn.focus({ preventScroll: true });
        }

        function effacerDerniere() {
          if (occupe || fini) return;
          for (let k = n - 1; k >= 0; k--) {
            if (cases[k] && !bloquee[k]) { retirerCase(k, false); return; }
          }
        }

        // Clavier physique : la lettre exacte, sinon la même lettre de base (e → é en français ; en allemand,
        // a ne donne jamais ä : ce sont deux lettres de l'alphabet). La majuscule d'un nom se tape en
        // minuscule : elle est choisie pour la première case, jamais ailleurs. Pinyin : v tape ü (claviers chinois).
        function taper(ch) {
          if (occupe || fini) return;
          const k = cases.indexOf(null);
          if (hanzi && ch === 'v' && !tuiles.some((x) => x.c === 'v')) ch = 'ü';
          const base = Ile.plier(ch);
          const cands = tuiles.filter((x) => x.pos === -1 && (x.c === ch || Ile.plier(x.c) === base));
          const rang = (x) => ((k === 0) === minuscule(x.c) ? 2 : 0) + (x.c === ch ? 0 : 1);
          const t = cands.sort((a, b) => rang(a) - rang(b))[0];
          if (t) poser(t);
          else { Ile.flash(reserve, 'bad'); Ile.sfx('bad'); }
        }

        function verifier() {
          if (cases.map((t) => t.c).join('') === lettres.join('')) { reussite(); return; }
          erreur = true;
          occupe = true;
          const faux = [];
          let nouvellesJustes = 0;
          cases.forEach((t, k) => {
            if (t.c !== lettres[k]) faux.push(k);
            else if (!bloquee[k]) { bloquee[k] = true; nouvellesJustes++; }
          });
          rendu();
          faux.forEach((k) => caseBtns[k].classList.add('is-wrong'));
          Ile.flash(casesEl, 'bad');
          Ile.sfx('bad');
          Ile.feedback(fb, false, typo(nouvellesJustes ? tx.presque : tx.pasEncore));
          montrerFeedback();
          setTimeout(() => {
            if (!panel.isConnected || q !== ctrl) return;
            faux.forEach((k) => {
              const t = cases[k];
              cases[k] = null;
              t.pos = -1;
              caseBtns[k].classList.remove('is-wrong');
            });
            occupe = false;
            rendu();
            if (clavier && !panel.contains(document.activeElement)) focusPremiere();
          }, 1000);
        }

        function reussite() {
          fini = true;
          occupe = true;
          const parfait = !aide && !erreur;
          if (parfait) score++;
          for (let k = 0; k < n; k++) bloquee[k] = true;
          rendu();
          casesEl.classList.add('is-win');
          reserve.classList.add('is-done');
          montrerImage(true);
          if (pinyin) pinyin.hidden = false;
          Ile.flash(casesEl, 'good');
          Ile.flash(image, 'good');
          Ile.sfx('good');
          const bravo = parfait ? Ile.pick(Ile.t('bravo'), 1)[0] : tx.reussi;
          Ile.feedback(fb, true, typo(bravo + (hanzi ? '' : ' ') + tx.cest(nom)));
          montrerFeedback();
          Ile.say(nom);
          i++;
          Ile.progress(panel, i, total, score);
          setTimeout(next, 1700);
        }

        // Place la prochaine bonne lettre (de gauche à droite).
        function indice() {
          if (occupe || fini) return;
          const pos = lettres.findIndex((c, k) => !cases[k] || cases[k].c !== c);
          if (pos === -1) return;
          aide = true;
          for (let k = 0; k < pos; k++) bloquee[k] = true; // déjà justes
          if (cases[pos]) { cases[pos].pos = -1; cases[pos] = null; }
          let t = tuiles.find((x) => x.pos === -1 && x.c === lettres[pos]);
          if (!t) {
            // La lettre attendue est posée ailleurs, dans une case où elle est fausse : on la reprend.
            let k2 = cases.findIndex((x, k) => x && !bloquee[k] && x.c === lettres[pos] && x.c !== lettres[k]);
            if (k2 === -1) k2 = cases.findIndex((x, k) => x && !bloquee[k] && x.c === lettres[pos]);
            if (k2 === -1) return;
            t = cases[k2];
            cases[k2] = null;
            t.pos = -1;
          }
          cases[pos] = t;
          t.pos = pos;
          bloquee[pos] = true;
          aidee[pos] = true;
          Ile.sfx('flip');
          rendu();
          Ile.flash(caseBtns[pos], 'good');
          info(tx.coupDePouce(lettres[pos]));
          if (cases.indexOf(null) === -1) verifier();
        }

        function voirImage() {
          if (fini || imageVue) return;
          aide = true;
          imageVue = true;
          montrerImage(false);
          Ile.flash(image, 'good');
          Ile.sfx('flip');
          if (document.activeElement === btnImage) btnIndice.focus({ preventScroll: true });
          rendu();
          info(tx.voiciImage);
        }

        function focusPremiere() {
          const t = tuiles.find((x) => x.pos === -1);
          if (t) t.btn.focus({ preventScroll: true });
        }

        const ctrl = {
          taper,
          effacerDerniere,
          disposer() { disposer(casesEl, n); disposer(reserve, n); },
        };

        rendu();
        ctrl.disposer();
        if (clavier) focusPremiere();
        return ctrl;
      }

      next();
    },
  });
})();
