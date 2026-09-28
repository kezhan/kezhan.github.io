/* Le mot et l'image : une image, plusieurs mots, trouve le bon.
 * Après la bonne réponse, le groupe « article + mot » s'affiche et se prononce
 * (a cat, eine Katze, eng Kaz, 一只猫, un chat) : on apprend le mot avec son article.
 * Chinois : pinyin sous chaque choix aux niveaux 1–2 (au niveau 3, on lit les caractères seuls),
 * et toujours sous la réponse trouvée. Allemand, luxembourgeois : les noms gardent leur majuscule.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'images';
  const TOTAL = 10;
  const PAUSE = 1700; // temps pour lire et entendre la réponse

  // Textes propres au jeu, par langue. {g} = groupe « article + mot » (mis en valeur).
  const T = {
    en: {
      consigne: 'Which word matches the picture?',
      image: 'Picture to name',
      choix: 'Choose a word',
      bravo: 'Well done! It’s {g}.',
      oui: 'Yes! It’s {g}.',
      non: (mot) => 'No, it isn’t “' + mot + '”. Try again!',
    },
    zh: {
      consigne: '图上画的是什么？',
      image: '要猜的图片',
      choix: '选一个词',
      bravo: '真棒！这是{g}。',
      oui: '对了！这是{g}。',
      non: (mot) => '不对，不是“' + mot + '”。再试一次！',
    },
    de: {
      consigne: 'Welches Wort passt zum Bild?',
      image: 'Bild zum Benennen',
      choix: 'Wähle ein Wort',
      bravo: 'Super! Das ist {g}.',
      oui: 'Ja! Das ist {g}.',
      non: (mot) => 'Nein, das ist nicht „' + mot + '“. Versuch es noch mal!',
    },
    lb: {
      consigne: 'Wat fir e Wuert passt bei d’Bild?',
      image: 'Wat ass op dësem Bild?',
      choix: 'Wiel e Wuert',
      bravo: 'Super! Dat ass {g}.',
      oui: 'Jo! Dat ass {g}.',
      non: (mot) => 'Nee, dat ass net „' + mot + '“. Probéier nach eng Kéier!',
    },
    fr: {
      consigne: 'Quel mot correspond à l’image ?',
      image: 'Image à nommer',
      choix: 'Choisis un mot',
      bravo: 'Bravo ! C’est {g}.',
      oui: 'Oui ! C’est {g}.',
      non: (mot) => 'Non, ce n’est pas « ' + mot + ' ». Essaie encore !',
    },
  };

  // Mots génériques qui nomment AUSSI une image plus précise : jamais l'un comme distracteur de
  // l'autre (🦖 霸王龙 est aussi un 恐龙, 🚁 un 飞机, 🌋 une montagne, 🦉 un oiseau, 🚌 un 车…).
  // Clés et valeurs : emojis des packs, sans le sélecteur de variante U+FE0F.
  const GENERIQUES = {
    '🐦': '🦉🦜🐧🐔🦆🦩🦅🦢🐓', // oiseau
    '🌳': '🌴🌲', // arbre
    '🌸': '🌻🌷🌹🌺', // fleur
    '⛰': '🌋🏔', // montagne
    '🦕': '🦖', // dinosaure
    '🐉': '🦕🦖', // 龙 / 恐龙 : les enfants appellent souvent les dinosaures « 龙 »
    '🐟': '🦈🐠🐡🐳', // poisson (et Walfësch)
    '🚗': '🚕🚓🚙🚌🚑🚒🚲🚂🚜🏍', // voiture ; en chinois, 车 désigne tout véhicule
    '✈': '🚁', // 飞机 / 直升机 (直升飞机)
    '⛵': '🚢', // bateau / navire
    '🐒': '🦍', // singe
    '🐳': '🐬', // baleine / dauphin
    '🐚': '🦪', // coquillage / huître
    '🐻': '🧸', // ours / ours en peluche
    '☁': '🌧⛈🌩', // nuage / pluie
    '🏝': '🌴🌊', // l'île de l'image a un palmier et la mer
    '🧒': '👧👦👸', // enfant
    '👧': '👸', // fille / princesse
  };
  const sansVariante = (e) => String(e).replace(/\uFE0F/g, '');
  function ambigu(a, b) {
    const x = sansVariante(a.emoji);
    const y = sansVariante(b.emoji);
    const inclut = (g, s) => !!GENERIQUES[g] && Array.from(GENERIQUES[g]).indexOf(s) !== -1;
    return x === y || inclut(x, y) || inclut(y, x);
  }

  const SHY = '\u00AD';
  const hanzi = () => Ile.L().ecriture === 'hanzi';
  const pinyinEl = (p) => el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin', text: p });
  // Mot affiché dans un bouton : les mots longs (Schmetterling, Waassermeloun) ne se coupent qu'entre
  // deux syllabes du pack (césure douce), jamais au hasard.
  function motCoupable(m) {
    if (hanzi() || !Array.isArray(m.syl) || m.syl.join('') !== m.mot) return m.mot;
    return m.syl.join(SHY);
  }
  function taille(m) {
    const n = Array.from(m.mot).length;
    return n >= 12 ? ' images-choice--xlong' : n >= 9 ? ' images-choice--long' : '';
  }

  // Ressemblance entre deux mots, pour des distracteurs plus fins aux niveaux 2 et 3 :
  // même thème, même début (même première lettre, ou même premier caractère en chinois),
  // même fin en chinois (自行车 / 公交车 / 出租车), longueur voisine.
  function ressemblance(m, cible, level) {
    const a = Array.from(m.mot.toLowerCase());
    const b = Array.from(cible.mot.toLowerCase());
    const poids = level === 3 ? 3 : 1;
    let s = m.theme === cible.theme ? 2 : 0;
    if (a[0] === b[0]) s += poids;
    if (hanzi() && a.length > 1 && a[a.length - 1] === b[b.length - 1]) s += poids;
    if (Math.abs(a.length - b.length) <= (hanzi() ? 0 : 1)) s += 1;
    return s;
  }
  function distracteurs(cible, pool, n, level) {
    let autres = Ile.shuffle(pool.filter((m) => m.mot !== cible.mot && !ambigu(m, cible)));
    if (level >= 2) autres = autres.sort((x, y) => ressemblance(y, cible, level) - ressemblance(x, cible, level));
    return autres.slice(0, n);
  }

  // Phrase de réussite : le groupe nominal en gras (avec son pinyin dessous en chinois).
  function phraseReussite(modele, cible) {
    const art = Ile.un(cible);
    const gn = Ile.groupe(art, cible.mot);
    const fort = el('strong', { class: 'images-groupe' }, [el('span', { class: 'images-groupe__txt', text: gn })]);
    if (hanzi()) {
      const py = [Ile.pinyin(art), Ile.aide(cible)].filter(Boolean).join(' ');
      if (py) { fort.classList.add('images-groupe--py'); fort.appendChild(pinyinEl(py)); }
    }
    // Le groupe et la ponctuation qui le suit restent sur la même ligne.
    const [avant, apres] = modele.split('{g}');
    return { gn, noeuds: [avant, el('span', { class: 'images-fin' }, [fort, apres || ''])] };
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const nbChoix = level === 1 ? 3 : 4;
      const avecPinyin = hanzi() && level <= 2;
      const pool = Ile.motsNiveau(level);
      const questions = Ile.motsPourPartie(level, TOTAL);
      let i = 0;
      let score = 0;

      const panel = el('section', { class: 'panel images', 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);

      function next() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= questions.length) {
          Ile.progress(panel, questions.length, questions.length, score);
          Ile.showResult({ id: ID, score, total: questions.length });
          return;
        }
        const cible = questions[i];
        const options = Ile.shuffle([cible].concat(distracteurs(cible, pool, nbChoix - 1, level)));
        let firstTry = true;
        let done = false;
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);

        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        Ile.progress(panel, i, questions.length, score);

        const fb = el('p', { class: 'feedback images-feedback', 'aria-live': 'polite' });
        const emoji = el('div', { class: 'big-emoji images-emoji', role: 'img', 'aria-label': tx.image, text: cible.emoji });
        const grid = el('div', { class: 'choices images-choices images-choices--' + options.length, role: 'group', 'aria-label': tx.choix });

        function trouve(b) {
          done = true;
          if (firstTry) score++;
          b.classList.add('is-correct');
          grid.classList.add('is-fini');
          grid.querySelectorAll('.choice').forEach((x) => { if (x !== b) x.disabled = true; });
          Ile.flash(b, 'good');
          Ile.flash(emoji, 'good');
          Ile.sfx('good');
          const r = phraseReussite(firstTry ? tx.bravo : tx.oui, cible);
          fb.className = 'feedback images-feedback feedback--good';
          fb.replaceChildren(...r.noeuds);
          Ile.say(r.gn);
          Ile.progress(panel, i + 1, questions.length, score);
          i++;
          setTimeout(next, PAUSE);
        }

        options.forEach((opt) => {
          const b = el('button', { type: 'button', class: 'btn choice images-choice' + taille(opt), 'data-mot': opt.mot }, [
            el('span', { class: 'images-choice__mot', text: motCoupable(opt) }),
            avecPinyin ? pinyinEl(Ile.aide(opt)) : null,
          ]);
          b.addEventListener('click', () => {
            if (done || b.disabled || !panel.isConnected) return;
            if (opt === cible) { trouve(b); return; }
            firstTry = false;
            b.classList.add('is-wrong');
            b.disabled = true;
            Ile.flash(b, 'bad');
            Ile.sfx('bad');
            Ile.feedback(fb, false, tx.non(opt.mot));
            // Le focus reste dans le jeu pour les joueurs au clavier.
            const reste = grid.querySelector('.choice:not(:disabled)');
            if (reste) reste.focus({ preventScroll: true });
          });
          grid.appendChild(b);
        });

        panel.append(
          el('p', { class: 'consigne', text: tx.consigne }),
          emoji,
          grid,
          fb
        );
        if (focusDansLeJeu && i > 0) grid.firstElementChild.focus({ preventScroll: true });
      }

      next();
    },
  });
})();
