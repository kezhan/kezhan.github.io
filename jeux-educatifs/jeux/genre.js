/* Un ou une ? / A or an? / 选量词 / Der, die oder das? / Den, déi oder dat?
 * Choisis le bon article (ou le bon classificateur en chinois) devant chaque mot.
 * Tout vient du pack : Ile.L().articles[niveau] = { champ, choix, affiche?, filtrer? }.
 *   - bonne réponse = mot[champ] ; boutons = choix ;
 *   - mots posés : niveau ≤ niveau de jeu et, si filtrer, mot[champ] ∈ choix ;
 *   - après la réponse, on affiche et on prononce Ile.groupe(mot[affiche || champ], mot.mot)
 *     (a cat, 一只猫, die Katze, de Bam, la lune).
 * Niveau 2 : l'image n'apparaît qu'après la réponse (on se fie au mot).
 * Niveau 3 : un ou deux pièges (unicorn, 一只手, der Hase, dat Meedchen, le hibou).
 * Chinois : pinyin sous le mot et sous chaque choix ; « 一 » reste affiché devant le trou.
 * Allemand, luxembourgeois : une couleur par genre (bleu, rouge, vert), sur les boutons et la réponse.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'genre';
  const TOTAL = 12;
  const PAUSE = 1800; // temps pour voir et entendre le groupe nominal complet
  const NB = ' ';
  // Suffixe cité (-chen, -ling, -e) : le trait d'union reste collé au suffixe (pas de retour à la ligne après « - »).
  const suf = (s) => '-\u2060' + s;
  const elide = (article) => /[’']$/.test(String(article));
  const finE = (m) => /e$/.test(m.mot);
  // -chen qui rend petit (das Eichhörnchen, dat Meedchen) : toujours neutre. Un nom en -chen qui n'est
  // pas neutre n'est pas un diminutif (der Kuchen, lb. d'Kichen) : c'est un piège, pas un exemple de la règle.
  const finChen = (m) => /chen$/.test(m.mot);
  const diminutif = (m) => finChen(m) && m.genre === 'n';
  const fauxDiminutif = (m) => finChen(m) && m.genre !== 'n';
  // « “a”, “b” ou “c” » dans chaque langue.
  const liste = (choix, cite, ou) => choix.map(cite).join(', ').replace(/, ([^,]*)$/, ou + '$1');

  // Allemand : règles de genre utiles à un apprenant (textes dans T.de.regles).
  //   toujours = rappel même après une réponse du premier coup ;
  //   leurre   = article que la « fausse règle » ferait choisir (message d'erreur ciblé).
  function regleDe(m) {
    const R = T.de.regles;
    if (diminutif(m)) return { texte: R.chen, toujours: true };
    if (fauxDiminutif(m)) return { texte: R.pasChen(m.mot), toujours: true, leurre: 'das' };
    if (/ling$/.test(m.mot)) return { texte: R.ling, toujours: true };
    if (finE(m) && m.genre !== 'f') return { texte: R.pasE(m.mot), toujours: true, leurre: 'die' };
    if (finE(m) && m.genre === 'f') return { texte: R.e, toujours: false };
    return null;
  }

  // Textes et règles propres à chaque langue.
  //   reussite(debut, gn, m, bonne, conf, premier) : message après la bonne réponse ;
  //   indice(m, choisi, bonne, conf)                : message après une erreur ;
  //   convient(m, champ) : le mot s'emploie-t-il naturellement avec ces articles ?
  //   piege(m, champ)    : l'article ne suit pas la règle « simple » (posé 1 ou 2 fois au niveau 3) ;
  //   couleurs           : une couleur par genre sur les boutons (langues à trois genres).
  const T = {
    en: {
      cite: (t) => '“' + String(t).replace(/ /g, NB) + '”',
      consigne: (choix) => 'Choose ' + liste(choix, T.en.cite, ' or ') + '.',
      aide: 'Read the word carefully: the picture will appear after you answer.',
      imageCachee: 'Hidden picture: it will appear after you answer',
      image: (mot) => 'Picture: ' + mot,
      aTrouver: 'missing word',
      ecouterMot: 'Listen to the word',
      groupeChoix: 'Choose the missing word',
      oui: 'Yes!',
      reussite(debut, gn, m, bonne) {
        let pourquoi = '';
        if (T.en.piege(m)) {
          pourquoi = /^h/i.test(m.mot)
            ? ', because the h in ' + T.en.cite(m.mot) + ' is silent'
            : ', because ' + T.en.cite(m.mot) + ' starts with a “yoo” sound';
        } else if (bonne === 'an') pourquoi = ', because ' + T.en.cite(m.mot) + ' starts with a vowel sound';
        return debut + ' We say ' + T.en.cite(gn) + pourquoi + '.';
      },
      indice(m, choisi, bonne) {
        if (T.en.piege(m)) {
          return /^h/i.test(m.mot)
            ? 'Hint: the h in ' + T.en.cite(m.mot) + ' is silent. Say it out loud!'
            : 'Hint: ' + T.en.cite(m.mot) + ' starts with a “yoo” sound, like “you”.';
        }
        return bonne === 'an'
          ? 'Hint: ' + T.en.cite(m.mot) + ' starts with a vowel sound.'
          : 'Hint: ' + T.en.cite(m.mot) + ' starts with a consonant sound.';
      },
      parfait: 'You really know when to use “a” and “an”!',
      astuce: () => 'Tip: use “an” before a vowel sound (an owl, an egg) and “a” before a consonant sound (a cat, a unicorn).',
      convient: () => true,
      // L'article ne suit pas la première lettre : a unicorn, an hourglass.
      piege: (m) => m.art !== (/^[aeiou]/i.test(m.mot) ? 'an' : 'a'),
      couleurs: false,
    },
    zh: {
      cite: (t) => '“' + t + '”',
      consigne: (choix) => (choix.length === 2 ? '用' + T.zh.cite(choix[0]) + '还是' + T.zh.cite(choix[1]) + '？' : '用哪个量词？'),
      aide: '仔细读一读这个词：回答以后，图片才会出现。',
      imageCachee: '图片藏起来了：回答以后就会出现',
      image: (mot) => '图片：' + mot,
      aTrouver: '缺少的量词',
      ecouterMot: '听一听这个词',
      groupeChoix: '选择量词',
      oui: '对了！',
      // À quoi sert chaque classificateur (message après une erreur ou une réussite hésitante).
      usage: {
        '个': '“个”是最常用的量词。',
        '只': '“只”多用在动物和成对的东西上。',
        '本': '“本”用在书上。',
        '条': '“条”用在长长的东西上。',
        '辆': '“辆”用在车上。',
        '朵': '“朵”用在花和云上。',
        '张': '“张”用在纸、床这样平平的东西上，也用在“嘴”上。',
        '把': '“把”用在有把手的东西上。',
      },
      reussite(debut, gn, m, bonne, conf, premier) {
        let s = debut + '我们说' + T.zh.cite(gn) + '。';
        if (T.zh.piege(m, conf.champ)) s += '身体上成对的部分，比如手和脚，用“只”。';
        else if (!premier && T.zh.usage[bonne]) s += T.zh.usage[bonne];
        return s;
      },
      indice(m, choisi, bonne, conf) {
        if (T.zh.piege(m, conf.champ)) return '提示：它是身体上成对的部分。再试一次！';
        if (choisi === '个') return '“个”很常用，不过这里有更合适的量词。再试一次！';
        return (T.zh.usage[choisi] || '') + '再试一次！';
      },
      parfait: '你的量词学得真好！',
      astuce: () => '小窍门：学新词的时候，把量词也一起记住，比如一只猫、一本书、一条鱼。',
      convient: () => true,
      // 手、脚、眼睛、耳朵 : 一只, et non 一个 (partie d'une paire).
      piege: (m, champ) => champ === 'cl' && m.theme === 'corps' && m.cl === '只',
      couleurs: false,
    },
    de: {
      cite: (t) => '„' + String(t).replace(/ /g, NB) + '“',
      consigne: (choix) => 'Wähle ' + liste(choix, T.de.cite, ' oder ') + '.',
      aide: 'Lies das Wort genau: Das Bild erscheint erst nach deiner Antwort.',
      imageCachee: 'Verstecktes Bild: Es erscheint nach deiner Antwort',
      image: (mot) => 'Bild: ' + mot,
      aTrouver: 'fehlender Artikel',
      ecouterMot: 'Wort anhören',
      groupeChoix: 'Wähle den Artikel',
      oui: 'Richtig!',
      // Règles de genre (voir regleDe).
      regles: {
        chen: 'Wenn ' + suf('chen') + ' etwas klein macht, heißt es immer „das“.',
        pasChen: (mot) => 'Achtung: Bei „' + mot + '“ macht ' + suf('chen') + ' nichts klein. Darum ist es kein „das“-Wort.',
        ling: 'Wörter mit ' + suf('ling') + ' am Ende sind „der“-Wörter.',
        pasE: (mot) => 'Achtung: „' + mot + '“ endet auf ' + suf('e') + ', ist aber kein „die“-Wort.',
        e: 'Viele Wörter mit ' + suf('e') + ' am Ende sind „die“-Wörter.',
      },
      reussite(debut, gn, m, bonne, conf, premier) {
        const r = regleDe(m);
        return debut + ' Es heißt ' + T.de.cite(gn) + '.' + (r && (r.toujours || !premier) ? ' ' + r.texte : '');
      },
      indice(m, choisi) {
        const r = regleDe(m);
        if (r && r.leurre) return choisi === r.leurre ? r.texte : 'Nicht ganz. Probier einen anderen Artikel!';
        if (r) return 'Tipp: ' + r.texte;
        return 'Nicht ganz. Probier einen anderen Artikel!';
      },
      parfait: 'Du kennst die Artikel richtig gut!',
      astuce: () => 'Tipp: Lerne jedes Wort immer mit seinem Artikel: der Hund, die Katze, das Haus.',
      convient: () => true,
      // der Hase, das Auge (-e mais pas « die ») ; das Eichhörnchen (-chen) ; der Kuchen (-chen mais pas « das »).
      piege: (m, champ) => champ === 'def' && ((finE(m) && m.genre !== 'f') || finChen(m)),
      couleurs: true,
    },
    lb: {
      cite: (t) => '„' + String(t).replace(/ /g, NB) + '“',
      consigne: (choix) => 'Wiel ' + liste(choix, T.lb.cite, ' oder ') + '.',
      aide: 'Lies d’Wuert gutt: D’Bild kënnt eréischt no denger Äntwert.',
      imageCachee: 'D’Bild ass verstoppt: Et kënnt no denger Äntwert',
      image: (mot) => 'Bild: ' + mot,
      aTrouver: 'Artikel, deen feelt',
      ecouterMot: 'D’Wuert lauschteren',
      groupeChoix: 'Wiel den Artikel',
      oui: 'Richteg!',
      // Forme affichée différente de la réponse (règle de l'n : de Bam) : on l'explique.
      // Féminin et neutre : on rappelle la forme courte de tous les jours (d’Kaz, d’Haus).
      reussite(debut, gn, m, bonne, conf) {
        const aff = m[conf.affiche || conf.champ];
        let s = debut + ' Et heescht ' + T.lb.cite(gn) + '.';
        if (aff !== bonne) s += ' Virun „' + m.mot[0] + '“ schreift een „' + aff + '“ amplaz „' + bonne + '“.';
        else if (diminutif(m)) s += ' ' + T.lb.chen;
        else if (m.def && m.def !== aff) s += ' Méi kuerz: ' + T.lb.cite(Ile.groupe(m.def, m.mot)) + '.';
        return s;
      },
      // -chen qui rend petit : toujours « dat » (mais d'Kichen, la cuisine, est féminin).
      chen: 'Wann ' + suf('chen') + ' eppes kleng mécht, ass et ëmmer „dat“.',
      indice(m) {
        if (diminutif(m)) return 'Tipp: ' + T.lb.chen;
        return 'Net ganz. Probéier en aneren Artikel!';
      },
      parfait: 'Bravo, dat war perfekt!',
      astuce: () => 'Tipp: Léier all Wuert mat sengem Artikel: den Hond, déi Kaz, dat Haus.',
      convient: () => true,
      // dat Meedchen, dat Kaweechelchen (-chen : toujours neutre).
      piege: (m, champ) => champ === 'artG' && diminutif(m),
      couleurs: true,
    },
    fr: {
      cite: (t) => '«' + NB + String(t).replace(/ /g, NB) + NB + '»',
      consigne: (choix) => 'Choisis ' + liste(choix, T.fr.cite, ' ou ') + '.',
      aide: 'Lis bien le mot' + NB + ': l’image apparaîtra après ta réponse.',
      imageCachee: 'Image cachée' + NB + ': elle apparaîtra après ta réponse',
      image: (mot) => 'Image' + NB + ': ' + mot,
      aTrouver: 'article à trouver',
      ecouterMot: 'Écouter le mot',
      groupeChoix: 'Choisis l’article',
      oui: 'Oui' + NB + '!',
      reussite(debut, gn, m, bonne, conf) {
        let pourquoi = '';
        if (conf.champ === 'def') {
          if (elide(bonne)) pourquoi = /^h/i.test(m.mot) ? ', car le h est muet' : ', car ' + T.fr.cite(m.mot) + ' commence par une voyelle';
          else if (/^h/i.test(m.mot)) pourquoi = ', sans apostrophe' + NB + ': ce h est aspiré';
        }
        return debut + ' On dit ' + T.fr.cite(gn) + pourquoi + '.';
      },
      indice(m, choisi, bonne, conf) {
        const i = (t) => 'Indice' + NB + ': ' + t;
        if (conf.champ !== 'def') return 'Pas tout à fait… Dis le mot tout haut et essaie encore' + NB + '!';
        if (elide(bonne)) {
          return /^h/i.test(m.mot)
            ? i('dans ' + T.fr.cite(m.mot) + ', le h ne se prononce pas.')
            : i(T.fr.cite(m.mot) + ' commence par une voyelle.');
        }
        if (elide(choisi)) {
          return /^h/i.test(m.mot)
            ? i('devant ' + T.fr.cite(m.mot) + ', on ne met pas d’apostrophe.')
            : i(T.fr.cite(m.mot) + ' commence par une consonne.');
        }
        return i('on dit ' + T.fr.cite(Ile.groupe(Ile.un(m), m.mot)) + '.');
      },
      parfait: 'Tu connais très bien tes articles' + NB + '!',
      astuce: (champ) => (champ === 'def'
        ? 'Astuce' + NB + ': devant une voyelle ou un h muet, ' + T.fr.cite('le') + ' et ' + T.fr.cite('la') + ' deviennent ' + T.fr.cite('l’') + '.'
        : 'Astuce' + NB + ': dis le mot tout haut avec ' + T.fr.cite('un') + ' puis avec ' + T.fr.cite('une') + ', et écoute ce qui sonne juste.'),
      // On ne dit pas naturellement « un lait », « une neige », « une pluie ».
      convient: (m, champ) => champ !== 'art' || ['lait', 'neige', 'pluie'].indexOf(m.mot) === -1,
      piege: (m, champ) => champ === 'def' && /^h/i.test(m.mot),
      couleurs: false,
    },
  };

  // Tirage des 12 mots : réponses équilibrées entre les choix (sinon « a » ou « 只 » gagneraient
  // toujours), un ou deux pièges au niveau 3, pas de longue série de la même réponse.
  function choisirMots(level, conf, tx) {
    const { champ, choix } = conf;
    const ok = (m) => tx.convient(m, champ) && choix.indexOf(m[champ]) !== -1;
    const liste = [];
    const pris = (m) => liste.indexOf(m) !== -1;

    const quotas = {};
    choix.forEach((c) => { quotas[c] = Math.floor(TOTAL / choix.length); });
    for (let r = TOTAL - choix.length * Math.floor(TOTAL / choix.length); r > 0; r--) quotas[Ile.pick(choix, 1)[0]]++;
    if (choix.length === 2 && Math.random() < 0.5) { // 5 / 7, 6 / 6 ou 7 / 5
      const [a, b] = Ile.shuffle(choix);
      quotas[a]++; quotas[b]--;
    }

    if (level === 3) {
      const pieges = Ile.shuffle(Ile.motsNiveau(level, (m) => ok(m) && tx.piege(m, champ)));
      pieges.slice(0, Ile.randInt(1, 2)).forEach((m) => {
        if (quotas[m[champ]] > 0) { liste.push(m); quotas[m[champ]]--; }
      });
    }
    choix.forEach((c) => {
      liste.push(...Ile.motsPourPartie(level, quotas[c], (m) => ok(m) && m[champ] === c && !pris(m)));
    });
    // Une catégorie trop petite (peu de mots en « an » au niveau 1, un seul mot en « 本 ») : on complète.
    if (liste.length < TOTAL) liste.push(...Ile.motsPourPartie(level, TOTAL - liste.length, (m) => ok(m) && !pris(m)));

    let best = null;
    for (let essai = 0; essai < 30; essai++) {
      const s = Ile.shuffle(liste);
      let serie = 1;
      let max = 1;
      for (let k = 1; k < s.length; k++) {
        serie = s[k][champ] === s[k - 1][champ] ? serie + 1 : 1;
        max = Math.max(max, serie);
      }
      if (!best || max < best.max) best = { s, max };
      if (max <= 3) break;
    }
    return best.s.slice(0, TOTAL);
  }

  const pinyinEl = (p) => el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin', text: p || NB });

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const L = Ile.L();
      const conf = L.articles[level];
      const { champ, choix } = conf;
      const hanzi = L.ecriture === 'hanzi';
      const questions = choisirMots(level, conf, tx);
      const imageCachee = level === 2;
      // Genre associé à chaque choix (der → m, die → f, das → n) pour les couleurs.
      const genreDe = (c) => { const w = (L.MOTS || []).find((x) => x[champ] === c); return w ? w.genre : null; };
      let i = 0;
      let score = 0;

      const panel = el('section', { class: 'panel genre' + (hanzi ? ' genre--hanzi' : ''), 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);

      function fin() {
        Ile.progress(panel, questions.length, questions.length, score);
        Ile.showResult({ id: ID, score, total: questions.length, message: score === questions.length ? tx.parfait : tx.astuce(champ) });
      }

      function next() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= questions.length) { fin(); return; }

        const m = questions[i];
        const bonne = m[champ];
        const aff = m[conf.affiche || champ]; // article écrit devant le mot après la réponse
        const gn = Ile.groupe(aff, m.mot);
        // Partie fixe de l'article, affichée avant le trou (le « 一 » de « 一只 »).
        const prefixe = aff !== bonne && aff.endsWith(bonne) ? aff.slice(0, aff.length - bonne.length) : '';
        let firstTry = true;
        let done = false;
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);

        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        Ile.progress(panel, i, questions.length, score);

        const image = imageCachee
          ? el('div', { class: 'big-emoji genre-image is-cachee', role: 'img', 'aria-label': tx.imageCachee }, [
            el('span', { class: 'genre-image__mystere', 'aria-hidden': 'true', text: '?' }),
          ])
          : el('div', { class: 'big-emoji genre-image', role: 'img', 'aria-label': tx.image(m.mot), text: m.emoji });

        // Groupe nominal : [préfixe] [trou] [mot] ; en chinois, chaque partie a son pinyin dessous.
        const article = el('span', { class: 'genre-article' }, [
          el('span', { class: 'sr-only', text: tx.aTrouver }),
          el('span', { 'aria-hidden': 'true', text: '?' }),
        ]);
        const motEl = el('span', { class: 'genre-mot', text: m.mot });
        // Groupes longs (das Geschenk, der Schmetterling, dat Kaweechelchen) : police un peu plus petite,
        // pour garder l'article, le mot et le bouton écouter sur une ligne, même sur un petit téléphone.
        const n = Array.from(gn).length;
        const phrase = el('p', { class: 'mot-affiche genre-phrase' + (hanzi ? ' genre-phrase--hanzi' : n >= 15 ? ' genre-phrase--xlong' : n >= 11 ? ' genre-phrase--long' : '') });
        let espace = null;
        let pyPrefixe = null;
        let pyArticle = null;
        if (hanzi) {
          pyPrefixe = pinyinEl('');
          pyArticle = pinyinEl('');
          if (prefixe) phrase.appendChild(el('span', { class: 'genre-seg' }, [el('span', { class: 'genre-prefixe', text: prefixe }), pyPrefixe]));
          phrase.append(
            el('span', { class: 'genre-seg' }, [article, pyArticle]),
            el('span', { class: 'genre-seg' }, [motEl, pinyinEl(Ile.aide(m))])
          );
        } else {
          // Vraie espace entre l'article et le mot (lecteurs d'écran), retirée en cas d'élision (l’étoile).
          espace = document.createTextNode(L.sepMots);
          phrase.append(article, espace, motEl);
        }
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
        const nbCol = Math.min(choix.length, 4);
        const grid = el('div', {
          class: 'choices genre-choices genre-choices--' + nbCol,
          role: 'group',
          'aria-label': tx.groupeChoix,
          style: '--nb:' + nbCol,
        });

        function trouve(b) {
          done = true;
          if (firstTry) score++;
          b.classList.add('is-correct');
          grid.classList.add('is-fini');
          grid.querySelectorAll('.genre-choice').forEach((x) => { if (x !== b) x.disabled = true; });
          Ile.flash(b, 'good');
          Ile.sfx('good');

          // Le groupe nominal complet s'affiche, puis se prononce.
          phrase.classList.add('is-complete');
          if (tx.couleurs && m.genre) phrase.setAttribute('data-genre', m.genre);
          if (hanzi) {
            article.textContent = aff.slice(prefixe.length);
            const syl = (Ile.pinyin(aff) || '').split(/\s+/).filter(Boolean);
            const nPre = Array.from(prefixe).length;
            if (syl.length === Array.from(aff).length) {
              pyPrefixe.textContent = syl.slice(0, nPre).join(' ') || NB;
              pyArticle.textContent = syl.slice(nPre).join(' ') || NB;
            } else {
              pyArticle.textContent = Ile.pinyin(bonne) || NB;
            }
          } else {
            article.textContent = aff;
            if (elide(aff)) { phrase.classList.add('is-elision'); espace.textContent = ''; }
          }
          Ile.flash(phrase, 'good');
          if (imageCachee) {
            image.classList.remove('is-cachee');
            image.textContent = m.emoji;
            image.setAttribute('aria-label', tx.image(m.mot));
            Ile.flash(image, 'good');
          }
          // Pas de retour à la ligne avant « ! » (typographie française des textes du pack).
          const debut = (firstTry ? Ile.pick(Ile.t('bravo'), 1)[0] : tx.oui).replace(/ ([!?:;])/g, NB + '$1');
          Ile.feedback(fb, true, tx.reussite(debut, gn, m, bonne, conf, firstTry));
          Ile.say(gn);
          Ile.progress(panel, i + 1, questions.length, score);
          i++;
          setTimeout(next, PAUSE);
        }

        choix.forEach((rep) => {
          const b = el('button', {
            type: 'button',
            class: 'btn choice genre-choice',
            'data-choix': rep,
            'data-genre': tx.couleurs ? genreDe(rep) : null,
          }, hanzi ? [el('span', { class: 'genre-choice__txt', text: rep }), pinyinEl(Ile.pinyin(rep))] : [rep]);
          b.addEventListener('click', () => {
            if (done || b.disabled || !panel.isConnected) return;
            if (rep === bonne) { trouve(b); return; }
            firstTry = false;
            b.disabled = true;
            b.classList.add('is-grise');
            Ile.flash(b, 'bad');
            Ile.sfx('bad');
            Ile.feedback(fb, false, tx.indice(m, rep, bonne, conf));
            // Le focus reste dans le jeu pour les joueurs au clavier.
            const reste = grid.querySelector('.genre-choice:not(:disabled)');
            if (reste) reste.focus({ preventScroll: true });
          });
          grid.appendChild(b);
        });

        panel.append(...[
          el('p', { class: 'consigne', text: tx.consigne(choix) }),
          imageCachee ? el('p', { class: 'genre-aide', text: tx.aide }) : null,
          image,
          el('div', { class: 'genre-ligne' }, [phrase, Ile.speakButton(() => (done ? gn : m.mot), tx.ecouterMot)]),
          grid,
          fb,
        ].filter(Boolean));
        if (focusDansLeJeu && i > 0) grid.firstElementChild.focus({ preventScroll: true });
      }

      next();
    },
  });
})();
