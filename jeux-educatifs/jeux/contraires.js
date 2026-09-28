/* Les contraires : 10 paires du pack (Ile.L().CONTRAIRES), niveau choisi d'abord, puis niveaux inférieurs.
 * Niveaux 1–2 : « Quel est le contraire de … ? », 3 puis 4 propositions de même nature (adjectif, verbe,
 * nom, adverbe), dans un sens ou dans l'autre ; jamais un contraire acceptable parmi les distracteurs
 * (facile / dur, bruyant / calme : voir T.*.proches). Niveau 1 : une image par mot quand la table en a une.
 * Niveau 3 : deux manches « relie les contraires » de 5 paires ; une paire trouvée prend une couleur et un
 * symbole ; une erreur secoue les deux mots et coûte le point de la paire.
 * Chinois : pinyin sous chaque mot. Allemand, luxembourgeois : les noms gardent leur majuscule (Tag, Dag).
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'contraires';
  const TOTAL = 10;
  const PAUSE = 2000; // temps pour lire et entendre la paire
  const PAR_MANCHE = 5;
  const PAUSE_MANCHE = 1300;
  const MARQUES = ['●', '■', '▲', '◆', '★'];

  // Textes propres au jeu, par langue. {p} = la paire « mot ↔ contraire », mise en valeur.
  // proches : mots de sens voisin (dur ≈ difficile) ; un mot voisin de la réponse, ou le contraire d'un mot
  // voisin de la question, n'est jamais proposé comme distracteur, et deux paires voisines ne sont pas
  // reliées dans la même manche.
  // emoji : image de chaque mot du niveau 1 (affichée au niveau 1 seulement).
  const T = {
    en: {
      consigne: 'What is the opposite of this word?',
      cible: 'Word',
      choix: 'Choose the opposite',
      ecouter: (m) => 'Listen: ' + m,
      bravo: 'Well done! {p}',
      oui: 'Yes! {p}',
      non: (x) => 'No, “' + x + '” isn’t the opposite. Try again!',
      consigneRelie: 'Match the opposites: tap a word on the left, then its opposite on the right.',
      manche: (n, t) => 'Round ' + n + ' of ' + t,
      paires: (n, t) => n + ' / ' + t + ' pairs',
      pairesAria: 'Pairs found',
      gauche: 'Words',
      droite: 'Opposites',
      choisisD: 'Now tap its opposite on the right.',
      choisisG: 'Now tap its opposite on the left.',
      paire: 'Well done! {p}',
      pasPaire: (a, b) => '“' + a + '” and “' + b + '” aren’t opposites. Try again!',
      dire: (a, b) => a + ', ' + b,
      proches: [
        ['hard', 'difficult'], ['quiet', 'silent', 'soft'], ['loud', 'noisy'], ['in', 'inside', 'entrance'],
        ['out', 'outside', 'exit'], ['old', 'ancient'], ['up', 'above'], ['down', 'below'], ['rude', 'mean'],
        ['take', 'accept'], ['happy', 'laugh'], ['sad', 'cry'], ['early', 'before'], ['late', 'after'],
        ['small', 'short'],
      ],
      emoji: {
        big: '🐘', small: '🐭', hot: '🔥', cold: '❄️', day: '☀️', night: '🌙', up: '⬆️', down: '⬇️',
        full: '🍲', empty: '🍽️', happy: '😀', sad: '😢', clean: '🧼', dirty: '🐷', long: '🐍', short: '🐛',
        open: '📖', shut: '📕', wet: '💧', dry: '🌵', in: '📥', out: '📤', laugh: '😂', cry: '😭',
        push: '🛒', pull: '🧲',
      },
    },
    zh: {
      consigne: '这个词的反义词是什么？',
      cible: '词语',
      choix: '选出反义词',
      ecouter: (m) => '听：' + m,
      bravo: '真棒！{p}',
      oui: '对了！{p}',
      non: (x) => '不对，“' + x + '”不是它的反义词。再试一次！',
      consigneRelie: '把反义词连起来：先点左边的词，再点右边的反义词。',
      manche: (n, t) => '第 ' + n + ' 轮，共 ' + t + ' 轮',
      paires: (n, t) => n + ' / ' + t + ' 对',
      pairesAria: '已找到的对数',
      gauche: '词语',
      droite: '反义词',
      choisisD: '现在点右边的反义词。',
      choisisG: '现在点左边的反义词。',
      paire: '真棒！{p}',
      pasPaire: (a, b) => '“' + a + '”和“' + b + '”不是反义词。再试一次！',
      dire: (a, b) => a + '，' + b,
      proches: [
        ['坏', '错'], ['好', '对'], ['开', '开始'], ['关', '结束'], ['高兴', '笑'], ['难过', '哭'],
        ['白', '白天'], ['黑', '黑夜'], ['进', '入口', '里面'], ['出', '出口', '外面'],
      ],
      emoji: {
        大: '🐘', 小: '🐭', 多: '🍬🍬🍬', 少: '🍬', 高: '🦒', 矮: '🐧', 长: '🐍', 短: '🐛',
        快: '🐇', 慢: '🐢', 冷: '❄️', 热: '🔥', 黑: '⚫', 白: '⚪', 好: '👍', 坏: '👎',
        上: '⬆️', 下: '⬇️', 左: '⬅️', 右: '➡️', 开: '🔓', 关: '🔒', 哭: '😭', 笑: '😂', 来: '🤗', 去: '👋',
      },
    },
    de: {
      consigne: 'Was ist das Gegenteil von diesem Wort?',
      cible: 'Wort',
      choix: 'Wähle das Gegenteil',
      ecouter: (m) => 'Anhören: ' + m,
      bravo: 'Super! {p}',
      oui: 'Ja! {p}',
      non: (x) => 'Nein, „' + x + '“ ist nicht das Gegenteil. Versuch es noch mal!',
      consigneRelie: 'Verbinde die Gegenteile: Tippe links auf ein Wort, dann rechts auf sein Gegenteil.',
      manche: (n, t) => 'Runde ' + n + ' von ' + t,
      paires: (n, t) => n + ' / ' + t + ' Paare',
      pairesAria: 'Gefundene Paare',
      gauche: 'Wörter',
      droite: 'Gegenteile',
      choisisD: 'Tippe jetzt rechts auf das Gegenteil.',
      choisisG: 'Tippe jetzt links auf das Gegenteil.',
      paire: 'Super! {p}',
      pasPaire: (a, b) => '„' + a + '“ und „' + b + '“ sind keine Gegenteile. Versuch es noch mal!',
      dire: (a, b) => a + ', ' + b,
      proches: [
        ['dünn', 'schmal'], ['dick', 'breit'], ['nie', 'selten'], ['immer', 'oft'], ['fröhlich', 'lachen'],
        ['traurig', 'weinen'], ['drinnen', 'Eingang'], ['draußen', 'Ausgang'], ['Tag', 'hell'], ['Nacht', 'dunkel'],
        ['heiß', 'Sommer'], ['kalt', 'Winter'], ['klein', 'kurz'],
      ],
      emoji: {
        groß: '🐘', klein: '🐭', heiß: '🔥', kalt: '❄️', Tag: '☀️', Nacht: '🌙', oben: '⬆️', unten: '⬇️',
        voll: '🍲', leer: '🍽️', lang: '🐍', kurz: '🐛', nass: '💧', trocken: '🌵', jung: '👶', alt: '👴',
        schnell: '🐇', langsam: '🐢', hell: '💡', dunkel: '🌑', dick: '📚', dünn: '📄', fröhlich: '😀',
        traurig: '😢', lachen: '😂', weinen: '😭', kommen: '🤗', gehen: '👋',
      },
    },
    lb: {
      consigne: 'Wat ass d’Géigendeel vun dësem Wuert?',
      cible: 'Wuert',
      choix: 'Wiel d’Géigendeel',
      ecouter: (m) => 'Lauschteren: ' + m,
      bravo: 'Super! {p}',
      oui: 'Jo! {p}',
      non: (x) => 'Nee, „' + x + '“ ass net d’Géigendeel. Probéier nach eng Kéier!',
      consigneRelie: 'Verbann d’Géigendeeler: dréck lénks op e Wuert, dann riets op säi Géigendeel.',
      manche: (n, t) => 'Ronn ' + n + ' vun ' + t,
      paires: (n, t) => n + ' / ' + t + ' Pairen',
      pairesAria: 'Pairen, déi fonnt sinn',
      gauche: 'Wierder',
      droite: 'Géigendeeler',
      choisisD: 'Dréck elo riets op d’Géigendeel.',
      choisisG: 'Dréck elo lénks op d’Géigendeel.',
      paire: 'Super! {p}',
      pasPaire: (a, b) => '„' + a + '“ an „' + b + '“ sinn keng Géigendeeler. Probéier nach eng Kéier!',
      dire: (a, b) => a + ', ' + b,
      proches: [
        ['dënn', 'schmuel'], ['déck', 'breet'], ['ni', 'seelen'], ['ëmmer', 'dacks'], ['frou', 'laachen'],
        ['traureg', 'kräischen'], ['op', 'opmaachen'], ['zou', 'zoumaachen'], ['dobannen', 'Agang'],
        ['dobaussen', 'Ausgang'], ['Dag', 'hell'], ['Nuecht', 'däischter'], ['waarm', 'Summer'], ['kal', 'Wanter'],
        ['hell', 'liicht'], ['kleng', 'kuerz'], ['mëll', 'lues'],
      ],
      emoji: {
        grouss: '🐘', kleng: '🐭', waarm: '🔥', kal: '❄️', Dag: '☀️', Nuecht: '🌙', uewen: '⬆️', ënnen: '⬇️',
        voll: '🍲', eidel: '🍽️', laang: '🐍', kuerz: '🐛', naass: '💧', dréchen: '🌵', al: '👴', jonk: '👶',
        frou: '😀', traureg: '😢', op: '🔓', zou: '🔒', déck: '📚', dënn: '📄', hell: '💡', däischter: '🌑',
        laachen: '😂', kräischen: '😭', kommen: '🤗', goen: '👋',
      },
    },
    fr: {
      consigne: 'Quel est le contraire de ce mot ?',
      cible: 'Mot',
      choix: 'Choisis le contraire',
      ecouter: (m) => 'Écouter : ' + m,
      bravo: 'Bravo ! {p}',
      oui: 'Oui ! {p}',
      non: (x) => 'Non, « ' + x + ' » n’est pas le contraire. Essaie encore !',
      consigneRelie: 'Relie les contraires : touche un mot à gauche, puis son contraire à droite.',
      manche: (n, t) => 'Manche ' + n + ' sur ' + t,
      paires: (n, t) => n + ' / ' + t + ' paires',
      pairesAria: 'Paires trouvées',
      gauche: 'Mots',
      droite: 'Contraires',
      choisisD: 'Touche maintenant son contraire à droite.',
      choisisG: 'Touche maintenant son contraire à gauche.',
      paire: 'Bravo ! {p}',
      pasPaire: (a, b) => '« ' + a + ' » et « ' + b + ' » ne sont pas des contraires. Essaie encore !',
      dire: (a, b) => a + ', ' + b,
      proches: [
        ['dur', 'difficile'], ['vieux', 'ancien'], ['calme', 'silencieux'], ['agité', 'bruyant'],
        ['petit', 'court'], ['grand', 'long'], ['content', 'rire'], ['triste', 'pleurer'], ['jour', 'clair'],
        ['nuit', 'sombre'], ['dedans', 'entrer'], ['dehors', 'sortir'], ['haut', 'monter'], ['bas', 'descendre'],
        ['toujours', 'fréquent'], ['jamais', 'rare'],
      ],
      emoji: {
        grand: '🐘', petit: '🐭', chaud: '🔥', froid: '❄️', jour: '☀️', nuit: '🌙', haut: '⬆️', bas: '⬇️',
        plein: '🍲', vide: '🍽️', content: '😀', triste: '😢', propre: '🧼', sale: '🐷', long: '🐍', court: '🐛',
        ouvert: '📖', fermé: '📕', monter: '⤴️', descendre: '⤵️', rire: '😂', pleurer: '😭', entrer: '📥', sortir: '📤',
      },
    },
  };

  const hanzi = () => Ile.L().ecriture === 'hanzi';
  const oppose = (p, w) => (p.a === w ? p.b : p.a);

  // Paires du pack, index mot → paire, et mots de sens voisin.
  function donnees(tx) {
    const paires = (Ile.L().CONTRAIRES || []).filter((p) => p && p.a && p.b);
    const pairDe = new Map();
    paires.forEach((p) => { pairDe.set(p.a, p); pairDe.set(p.b, p); });
    const groupes = Array.isArray(tx.proches) ? tx.proches : [];
    const proches = (w) => {
      const s = new Set();
      groupes.forEach((g) => { if (g.indexOf(w) !== -1) g.forEach((x) => { if (x !== w) s.add(x); }); });
      return s;
    };
    return { paires, pairDe, proches };
  }

  // Paires de la partie : niveau exact en priorité, puis niveaux inférieurs.
  function choisirPaires(paires, level, n) {
    const exact = Ile.shuffle(paires.filter((p) => p.niveau === level));
    const bas = Ile.shuffle(paires.filter((p) => p.niveau < level));
    let liste = exact.concat(bas);
    if (!liste.length) liste = Ile.shuffle(paires);
    const out = liste.slice(0, n);
    while (out.length < n && liste.length) out.push(liste[out.length % liste.length]);
    return out;
  }

  // Niveaux 1–2 : questions à choix multiple.
  function construireQcm(level, D) {
    const nb = level === 1 ? 2 : 3;
    return choisirPaires(D.paires, level, TOTAL).map((p) => {
      const mot = Math.random() < 0.5 ? p.a : p.b;
      const bonne = oppose(p, mot);
      // Jamais un contraire acceptable : ni un mot voisin de la réponse (dur pour facile), ni le contraire
      // d'un mot voisin de la question (moderne pour vieux, car vieux ≈ ancien).
      const possible = (w) => {
        if (w === mot || w === bonne || D.proches(bonne).has(w)) return false;
        const q = D.pairDe.get(w);
        const autre = oppose(q, w);
        return autre !== mot && !D.proches(mot).has(autre);
      };
      const mots = (liste) => [].concat(...liste.map((q) => [{ w: q.a, p: q }, { w: q.b, p: q }])).filter((o) => possible(o.w));
      const autres = D.paires.filter((q) => q !== p);
      const meme = (q) => q.nature === p.nature;
      const facile = (q) => q.niveau <= level;
      // Même nature et même niveau d'abord ; au niveau 1, des mots connus plutôt que des mots difficiles.
      const paliers = level === 1
        ? [autres.filter((q) => meme(q) && facile(q)), autres.filter((q) => !meme(q) && facile(q)), autres.filter((q) => meme(q) && !facile(q)), autres]
        : [autres.filter((q) => meme(q) && facile(q)), autres.filter((q) => meme(q) && !facile(q)), autres.filter((q) => !meme(q) && facile(q)), autres];
      const distr = [];
      const prises = new Set();
      paliers.forEach((palier) => {
        const cands = Ile.shuffle(mots(palier));
        cands.forEach((o) => { if (distr.length < nb && !prises.has(o.p) && distr.indexOf(o.w) === -1) { distr.push(o.w); prises.add(o.p); } });
        cands.forEach((o) => { if (distr.length < nb && distr.indexOf(o.w) === -1) { distr.push(o.w); prises.add(o.p); } });
      });
      return { p, mot, bonne, options: Ile.shuffle([bonne].concat(distr)) };
    });
  }

  // Niveau 3 : deux manches de 5 paires, sans deux paires voisines dans la même manche
  // (sinon « calme » pourrait se relier à « bruyant » comme à « agité »).
  function construireManches(level, D) {
    const conflit = (p, q) => [p.a, p.b].some((x) => [q.a, q.b].some((y) => D.proches(x).has(y)));
    const liste = choisirPaires(D.paires, level, D.paires.length);
    const manches = [[], []];
    liste.forEach((p) => {
      const m = manches.find((x) => x.length < PAR_MANCHE && !x.some((q) => q === p || conflit(p, q)));
      if (m) m.push(p);
    });
    liste.forEach((p) => { // repli (pack trop petit) : on complète sans vérifier les voisins
      const m = manches.find((x) => x.length < PAR_MANCHE);
      if (m && !manches.some((x) => x.indexOf(p) !== -1)) m.push(p);
    });
    return manches.filter((m) => m.length).map((m) => {
      const items = m.map((p) => {
        const g = Math.random() < 0.5 ? p.a : p.b;
        return { p, g, d: oppose(p, g) };
      });
      const gauche = Ile.shuffle(items);
      // À droite, aucun contraire en face de son mot.
      let droite = Ile.shuffle(items);
      for (let essai = 0; essai < 30 && droite.some((x, k) => x === gauche[k]); essai++) droite = Ile.shuffle(items);
      return { gauche, droite };
    });
  }

  const pinyinEl = (texte) => el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin', text: texte });
  // Le mot, et son pinyin dessous en chinois.
  function noeudsMot(w) {
    const py = hanzi() ? Ile.pinyin(w) : '';
    return [el('span', { class: 'ct-texte', text: w }), py ? pinyinEl(py) : null];
  }
  // « 🔥 chaud ↔ froid ❄️ » (images au niveau 1 seulement).
  function recap(a, b, tx, avecImages) {
    const img = (w) => (avecImages && tx.emoji && tx.emoji[w] ? el('span', { class: 'ct-emoji', 'aria-hidden': 'true', text: tx.emoji[w] }) : null);
    return el('span', { class: 'ct-recap' }, [
      img(a),
      el('span', { class: 'ct-recap__mot' }, noeudsMot(a)),
      el('span', { class: 'ct-recap__fleche', 'aria-hidden': 'true', text: '↔' }),
      el('span', { class: 'sr-only', text: ' – ' }),
      el('span', { class: 'ct-recap__mot' }, noeudsMot(b)),
      img(b),
    ]);
  }
  // Message « Bravo ! {p} » : le texte et la paire mise en valeur.
  function message(fb, modele, noeud, extraClass) {
    const [avant, apres] = modele.split('{p}');
    fb.className = 'feedback ' + extraClass + ' feedback--good';
    fb.replaceChildren(avant, noeud, apres || '');
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const D = donnees(tx);
      const panel = el('section', { class: 'panel contraires', 'aria-label': Ile.game(ID).titre, tabindex: '-1' });
      root.appendChild(panel);
      if (level >= 3) jouerRelie(); else jouerQcm();

      // ---------------------------------------------------------------------
      // Niveaux 1–2 : « Quel est le contraire de … ? »
      // ---------------------------------------------------------------------
      function jouerQcm() {
        const questions = construireQcm(level, D);
        let i = 0;
        let score = 0;

        function next() {
          if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
          if (i >= questions.length) {
            Ile.progress(panel, questions.length, questions.length, score);
            Ile.showResult({ id: ID, score, total: questions.length });
            return;
          }
          const q = questions[i];
          let firstTry = true;
          let done = false;
          const images = level === 1 && tx.emoji;
          const imagesChoix = images && q.options.every((w) => tx.emoji[w]);
          const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);

          panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
          Ile.progress(panel, i, questions.length, score);

          const carte = el('div', { class: 'ct-cible', role: 'group', 'aria-label': tx.cible }, [
            images && tx.emoji[q.mot] ? el('span', { class: 'ct-emoji', 'aria-hidden': 'true', text: tx.emoji[q.mot] }) : null,
            el('div', { class: 'ct-cible__mot', 'data-mot': q.mot }, noeudsMot(q.mot)),
            Ile.speakButton(q.mot, tx.ecouter(q.mot)),
          ]);
          const fb = el('p', { class: 'feedback ct-feedback', 'aria-live': 'polite' });
          const grid = el('div', { class: 'ct-options ct-options--' + q.options.length, role: 'group', 'aria-label': tx.choix });

          function trouve(b, ligne) {
            done = true;
            if (firstTry) score++;
            b.classList.add('is-correct');
            ligne.classList.add('is-bonne');
            grid.classList.add('is-fini');
            grid.querySelectorAll('.ct-choice').forEach((x) => { if (x !== b) x.disabled = true; });
            Ile.flash(b, 'good');
            Ile.flash(carte, 'good');
            Ile.sfx('good');
            message(fb, firstTry ? tx.bravo : tx.oui, recap(q.mot, q.bonne, tx, images), 'ct-feedback');
            Ile.say(tx.dire(q.mot, q.bonne));
            Ile.progress(panel, i + 1, questions.length, score);
            i++;
            setTimeout(next, PAUSE);
          }

          q.options.forEach((w) => {
            const b = el('button', { type: 'button', class: 'btn choice ct-choice', 'data-mot': w }, [
              imagesChoix ? el('span', { class: 'ct-emoji', 'aria-hidden': 'true', text: tx.emoji[w] }) : null,
              el('span', { class: 'ct-choice__mot' }, noeudsMot(w)),
            ]);
            const ligne = el('div', { class: 'ct-option' }, [b, Ile.speakButton(w, tx.ecouter(w))]);
            b.addEventListener('click', () => {
              if (done || b.disabled || !panel.isConnected) return;
              if (w === q.bonne) { trouve(b, ligne); return; }
              firstTry = false;
              b.classList.add('is-wrong');
              b.disabled = true;
              Ile.flash(b, 'bad');
              Ile.sfx('bad');
              Ile.feedback(fb, false, tx.non(w));
              fb.classList.add('ct-feedback');
              const reste = grid.querySelector('.ct-choice:not(:disabled)');
              if (reste) reste.focus({ preventScroll: true });
            });
            grid.appendChild(ligne);
          });

          panel.append(el('p', { class: 'consigne', text: tx.consigne }), carte, grid, fb);
          if (focusDansLeJeu && i > 0) grid.querySelector('.ct-choice').focus({ preventScroll: true });
          Ile.say(q.mot);
        }

        next();
      }

      // ---------------------------------------------------------------------
      // Niveau 3 : relier les contraires, deux manches de 5 paires.
      // ---------------------------------------------------------------------
      function jouerRelie() {
        const manches = construireManches(level, D);
        const total = manches.reduce((s, m) => s + m.gauche.length, 0);
        let score = 0;
        let trouvees = 0;
        let m = 0;

        // Barre de progression en paires trouvées (« 3 / 10 paires ») plutôt qu'en questions.
        function avancer() {
          const bar = Ile.progress(panel, trouvees, total, score);
          bar.setAttribute('aria-label', tx.pairesAria);
          bar.querySelector('.progress__label').textContent = tx.paires(trouvees, total) + '  ·  ✅ ' + score;
        }

        function manche() {
          if (!panel.isConnected) return;
          if (m >= manches.length) {
            avancer();
            Ile.showResult({ id: ID, score, total });
            return;
          }
          const { gauche, droite } = manches[m];
          const erreurs = new Set(); // paires qui ont coûté leur point
          let sel = null;
          let bloque = false;
          let faites = 0;
          const itemDe = new WeakMap(); // bouton → { p, g, d }

          panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
          avancer();

          const fb = el('p', { class: 'feedback ct-feedback', 'aria-live': 'polite' });
          const neutre = (texte) => { fb.className = 'feedback ct-feedback ct-aide'; fb.textContent = texte; };

          function choisir(b) {
            if (sel) { sel.classList.remove('is-sel'); sel.setAttribute('aria-pressed', 'false'); }
            sel = b;
            if (b) {
              b.classList.add('is-sel');
              b.setAttribute('aria-pressed', 'true');
              neutre(b.getAttribute('data-cote') === 'g' ? tx.choisisD : tx.choisisG);
            }
          }

          function clic(b, item) {
            if (bloque || b.disabled || !panel.isConnected) return;
            Ile.say(b.getAttribute('data-mot'));
            if (!sel) { choisir(b); return; }
            if (sel === b) { choisir(null); neutre(''); return; }
            if (sel.getAttribute('data-cote') === b.getAttribute('data-cote')) { choisir(b); return; }
            const premier = sel;
            const itemPremier = itemDe.get(premier);
            const [bg, bd] = premier.getAttribute('data-cote') === 'g' ? [premier, b] : [b, premier];
            choisir(null);
            if (itemPremier.p === item.p) {
              // Paire trouvée : même couleur et même symbole des deux côtés. Les deux boutons se désactivent :
              // le focus clavier passe au prochain mot libre de la colonne de gauche.
              const auClavier = panel.contains(document.activeElement);
              const k = faites % MARQUES.length;
              faites++;
              trouvees++;
              if (!erreurs.has(item.p)) score++;
              [bg, bd].forEach((x) => {
                x.disabled = true;
                x.removeAttribute('aria-pressed');
                x.classList.add('is-paire');
                x.style.setProperty('--ct-c', 'var(--ct-c' + (k + 1) + ')');
                x.style.setProperty('--ct-b', 'var(--ct-b' + (k + 1) + ')');
                x.appendChild(el('span', { class: 'ct-marque', 'aria-hidden': 'true', text: MARQUES[k] }));
                Ile.flash(x, 'good');
              });
              Ile.sfx('good');
              message(fb, tx.paire, recap(item.g, item.d, tx, false), 'ct-feedback');
              Ile.say(tx.dire(item.g, item.d));
              avancer();
              if (faites >= gauche.length) {
                bloque = true;
                m++;
                if (auClavier) panel.focus({ preventScroll: true }); // le focus reste dans le jeu pendant la pause
                setTimeout(manche, PAUSE_MANCHE);
              } else {
                const suivant = panel.querySelector('.ct-col--g .ct-mot:not(:disabled)');
                if (suivant && auClavier) suivant.focus({ preventScroll: true });
              }
            } else {
              // Erreur : les deux mots tremblent, la paire du premier mot touché perd son point.
              erreurs.add(itemPremier.p);
              Ile.flash(bg, 'bad');
              Ile.flash(bd, 'bad');
              Ile.sfx('bad');
              Ile.feedback(fb, false, tx.pasPaire(bg.getAttribute('data-mot'), bd.getAttribute('data-mot')));
              fb.classList.add('ct-feedback');
            }
          }

          function bouton(item, cote) {
            const w = cote === 'g' ? item.g : item.d;
            const b = el('button', { type: 'button', class: 'btn ct-mot', 'data-mot': w, 'data-cote': cote, 'aria-pressed': 'false' }, noeudsMot(w));
            itemDe.set(b, item);
            b.addEventListener('click', () => clic(b, item));
            return b;
          }

          const colonne = (cote, titre, items) => el('div', { class: 'ct-col ct-col--' + cote, role: 'group', 'aria-label': titre }, [
            el('p', { class: 'ct-col__titre', 'aria-hidden': 'true', text: titre }),
          ].concat(items.map((x) => bouton(x, cote))));

          panel.append(
            el('p', { class: 'consigne', text: tx.consigneRelie }),
            el('p', { class: 'ct-manche', text: tx.manche(m + 1, manches.length) }),
            el('div', { class: 'ct-plateau' }, [colonne('g', tx.gauche, gauche), colonne('d', tx.droite, droite)]),
            fb
          );
          if (m > 0 && (!document.activeElement || document.activeElement === document.body || document.activeElement === panel)) {
            panel.querySelector('.ct-col--g .ct-mot').focus({ preventScroll: true });
          }
        }

        manche();
      }
    },
  });
})();
