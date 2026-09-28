/* La chasse aux rimes : un mot (image, voix), trouve celui qui rime parmi 3 (niveau 1) ou 4 propositions.
 * Les familles viennent du pack (Ile.L().RIMES : { son, mots }) ; les distracteurs sont pris dans d'autres
 * familles et ne finissent jamais comme la cible (bear / deer, 水 shuǐ / 鸡 jī) ; au niveau 3, ils commencent
 * comme la bonne réponse, pour qu'on écoute vraiment la fin du mot.
 * Après la réponse, la terminaison commune est surlignée (c·at, h·at ; en chinois dans le pinyin : m·āo, b·āo)
 * et rappelée dans le message (« 🎵 -at » ; « 🎵 -ey / -ee » quand les deux mots l'écrivent différemment).
 * Sans voix pour la langue (souvent le luxembourgeois), le jeu se joue à l'écrit : les mots restent affichés
 * et, aux niveaux 1–2, la terminaison du mot à faire rimer est surlignée dès le début (K·ou → w·ou).
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'rimes';
  const TOTAL = 10;
  const PAUSE = 2300; // temps pour voir la terminaison surlignée et entendre les deux mots

  // Textes propres au jeu, par langue. {s} = la rime (« 🎵 -at », « 🎵 ao »), mise en valeur.
  // graphies : terminaisons écrites à surligner en entier quand elles finissent par le son de la famille
  // (famille « in » : p·ain, m·ain, et non pa·in, car « ain » s'écrit et se lit d'un bloc).
  const T = {
    en: {
      consigne: 'Which word rhymes with this one?',
      cible: 'Word to rhyme with',
      choix: 'Choose a word',
      ecouter: (m) => 'Listen: ' + m,
      bravo: 'Well done! They rhyme: {s}',
      oui: 'Yes! They rhyme: {s}',
      non: (x) => 'No, “' + x + '” doesn’t rhyme. Try again!',
      son: (s) => '-' + s,
      sansVoix: 'No voice for this language: look at the end of the words!',
      dire: (a, b) => a + ', ' + b,
    },
    zh: {
      consigne: '哪个字和它押韵？',
      cible: '要押韵的字',
      choix: '选一个字',
      ecouter: (m) => '听：' + m,
      bravo: '真棒！韵母一样：{s}',
      oui: '对了！韵母一样：{s}',
      non: (x) => '不对，“' + x + '”不押韵。再试一次！',
      son: (s) => s,
      sansVoix: '没有中文语音：看看拼音的结尾！',
      dire: (a, b) => a + '，' + b,
    },
    de: {
      consigne: 'Welches Wort reimt sich darauf?',
      cible: 'Das Wort zum Reimen',
      choix: 'Wähle ein Wort',
      ecouter: (m) => 'Anhören: ' + m,
      bravo: 'Super! Das reimt sich: {s}',
      oui: 'Ja! Das reimt sich: {s}',
      non: (x) => 'Nein, „' + x + '“ reimt sich nicht. Versuch es noch mal!',
      son: (s) => '-' + s,
      sansVoix: 'Keine Stimme für diese Sprache: Schau dir das Ende der Wörter an!',
      dire: (a, b) => a + ', ' + b,
    },
    lb: {
      consigne: 'Fann d’Wuert, dat sech reimt!',
      cible: 'Dëst Wuert',
      choix: 'Wiel e Wuert',
      ecouter: (m) => 'Lauschteren: ' + m,
      bravo: 'Super! Dat reimt sech: {s}',
      oui: 'Jo! Dat reimt sech: {s}',
      non: (x) => 'Nee, „' + x + '“ reimt sech net. Probéier nach eng Kéier!',
      son: (s) => '-' + s,
      sansVoix: 'Keng Stëmm fir dës Sprooch: kuck op d’Enn vun de Wierder!',
      dire: (a, b) => a + ', ' + b,
    },
    fr: {
      consigne: 'Quel mot rime avec celui-ci ?',
      cible: 'Mot à faire rimer',
      choix: 'Choisis un mot',
      ecouter: (m) => 'Écouter : ' + m,
      bravo: 'Bravo ! Ça rime : {s}',
      oui: 'Oui ! Ça rime : {s}',
      non: (x) => 'Non, « ' + x + ' » ne rime pas. Essaie encore !',
      son: (s) => '-' + s,
      sansVoix: 'Pas de voix pour cette langue : regarde la fin des mots !',
      dire: (a, b) => a + ', ' + b,
      graphies: ['ain', 'ein'],
    },
  };

  const hanzi = () => Ile.L().ecriture === 'hanzi';
  // Pinyin sans tons (ü conservé), en minuscules.
  const sansTon = (p) => String(p).normalize('NFD').replace(/[̀-̇̉-ͯ]/g, '').normalize('NFC').toLowerCase();
  const VOYELLES = 'aeiouyäöüéèêëàâîïôûùœæáíóú';
  const estVoyelle = (c) => VOYELLES.indexOf(c) !== -1;

  // Début de la terminaison qui rime dans un mot écrit en lettres : le son de la famille s'il termine
  // le mot (c·at, Ha·us), sinon le dernier groupe de voyelles, e muet final compris
  // (k·ite pour la famille « ight », b·ear pour « air », Sch·atz pour « az », b·eurre pour « eur »).
  // Une graphie de la langue qui finit par le son est surlignée en entier (p·ain pour la famille « in »).
  function debutRime(mot, son, graphies) {
    const l = mot.toLowerCase();
    const s = String(son).toLowerCase();
    const g = (graphies || []).find((x) => x.length > s.length && x.endsWith(s) && l.endsWith(x) && l.length > x.length);
    if (s && g) return mot.length - g.length;
    if (s && l.endsWith(s)) return mot.length - s.length;
    let j = l.length - 1;
    if (j >= 2 && l[j] === 'e' && !estVoyelle(l[j - 1])) j--;
    while (j >= 0 && !estVoyelle(l[j])) j--;
    if (j < 0) return 0;
    let k = j;
    while (k > 0 && estVoyelle(l[k - 1])) k--;
    if (k > 0 && k < j && l[k] === 'u' && l[k - 1] === 'q') k++; // qu : le u ne se prononce pas seul (squ·are)
    return k;
  }

  // Tout ce qu'il faut savoir d'un mot de rime.
  function infoMot(mot, fam, graphies) {
    const L = Ile.L();
    const m = (L.MOTS || []).find((x) => x.mot === mot);
    const zh = hanzi();
    const py = zh ? Ile.pinyin(mot) : '';
    const lettres = zh ? sansTon(py).replace(/\s+/g, '') : mot.toLowerCase();
    const initiale = zh
      ? (lettres.match(/^(zh|ch|sh|[bpmfdtnlgkhjqxrzcsyw])/) || [''])[0]
      : Ile.plier(Array.from(mot)[0] || '');
    const debut = zh ? 0 : debutRime(mot, fam.son, graphies);
    return {
      mot, fam, py, lettres, initiale, debut,
      emoji: m ? m.emoji : '',
      niveau: m ? m.niveau : 0,
      rime: zh ? fam.son : mot.slice(debut).toLowerCase(),
    };
  }

  // Un distracteur ne doit jamais sembler rimer : autre famille, et il ne finit ni par le son de la
  // famille (bear pour la famille « ear », 狗 gǒu pour la famille « u »), ni comme la cible ou la réponse
  // (grey / key, dont les terminaisons écrites sont identiques).
  function distracteurPossible(d, cible, bonne) {
    if (d.fam === cible.fam) return false;
    if (d.mot.toLowerCase() === cible.mot.toLowerCase() || d.mot.toLowerCase() === bonne.mot.toLowerCase()) return false;
    if (d.lettres.endsWith(String(cible.fam.son).toLowerCase())) return false;
    if (!hanzi() && (d.rime === cible.rime || d.rime === bonne.rime)) return false;
    return true;
  }

  // Les 10 questions de la partie : une famille différente par question (tant qu'il y en a assez).
  function construire(level, graphies) {
    const familles = (Ile.L().RIMES || []).filter((f) => f && f.mots && f.mots.length >= 2);
    const tous = [];
    const parFamille = familles.map((f) => {
      const mots = f.mots.map((w) => infoMot(w, f, graphies));
      tous.push(...mots);
      return { f, mots };
    });
    // Les familles qui ont un mot illustré (du niveau choisi) passent d'abord : la cible a une image.
    const illustree = (x) => x.mots.some((w) => w.emoji && w.niveau <= level) ? 2 : x.mots.some((w) => w.emoji) ? 1 : 0;
    const ordre = Ile.shuffle(parFamille).sort((a, b) => illustree(b) - illustree(a));
    const nbDistr = level === 1 ? 2 : 3;
    const vus = new Set();
    const questions = [];
    for (let q = 0; q < TOTAL && ordre.length; q++) {
      const { mots } = ordre[q % ordre.length];
      const libres = mots.filter((w) => !vus.has(w.mot));
      const base = libres.length >= 2 ? libres : mots;
      const rang = (w) => (w.emoji && w.niveau <= level ? 2 : w.emoji ? 1 : 0);
      const cible = Ile.shuffle(base).sort((a, b) => rang(b) - rang(a))[0];
      const bonne = Ile.shuffle(base.filter((w) => w !== cible))[0];
      // Distracteurs : une famille chacun ; au niveau 3, même initiale que la bonne réponse si possible.
      const score = (d) => (level === 3 && d.initiale && d.initiale === bonne.initiale ? 4 : 0) + (vus.has(d.mot) ? 0 : 1);
      const candidats = Ile.shuffle(tous.filter((d) => distracteurPossible(d, cible, bonne)))
        .sort((a, b) => score(b) - score(a));
      const distr = [];
      candidats.forEach((d) => {
        if (distr.length < nbDistr && !distr.some((x) => x.fam === d.fam)) distr.push(d);
      });
      candidats.forEach((d) => { if (distr.length < nbDistr && distr.indexOf(d) === -1) distr.push(d); });
      [cible, bonne].concat(distr).forEach((w) => vus.add(w.mot));
      questions.push({ cible, bonne, options: Ile.shuffle([bonne].concat(distr)) });
    }
    return questions;
  }

  const pinyinEl = (children) => el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin' }, children);
  const marque = (texte) => el('mark', { class: 'rm-fin', text: texte });

  // Pinyin d'un caractère, la finale surlignée (m·āo) : on compte les lettres de la finale sur la syllabe
  // avec ses tons (une lettre accentuée = un caractère).
  function pinyinSurligne(w) {
    const syl = w.py.split(/\s+/);
    const derniere = Array.from(syl.pop().normalize('NFC'));
    const n = String(w.fam.son).length;
    const avant = syl.length ? syl.join(' ') + ' ' : '';
    if (!sansTon(derniere.join('')).endsWith(String(w.fam.son).toLowerCase()) || n > derniere.length) {
      return [avant, marque(derniere.join(''))];
    }
    return [avant + derniere.slice(0, derniere.length - n).join(''), marque(derniere.slice(derniere.length - n).join(''))];
  }

  // Le mot (et son pinyin en chinois), avec ou sans la terminaison surlignée.
  function noeudsMot(w, surligne) {
    if (hanzi()) {
      return [
        el('span', { class: 'rm-texte', text: w.mot }),
        w.py ? pinyinEl(surligne ? pinyinSurligne(w) : [w.py]) : null,
      ];
    }
    if (!surligne) return [el('span', { class: 'rm-texte', text: w.mot })];
    return [el('span', { class: 'rm-texte' }, [w.mot.slice(0, w.debut), marque(w.mot.slice(w.debut))])];
  }

  // La liste des voix arrive parfois après le chargement : l'aide « regarde la fin des mots » apparaît ou
  // disparaît avec les boutons « écouter ». Un seul écouteur pour la page : il prévient la partie en cours
  // (celle d'une partie remplacée n'est plus appelée).
  let surVoix = null;
  try {
    window.speechSynthesis.addEventListener('voiceschanged', () => { if (surVoix) surVoix(); });
  } catch (e) { /* pas de synthèse vocale */ }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const questions = construire(level, tx.graphies);
      let i = 0;
      let score = 0;
      let majVoix = null; // met la question affichée à jour quand la voix apparaît / disparaît

      const panel = el('section', { class: 'panel rimes', 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      surVoix = () => { if (panel.isConnected && majVoix) majVoix(); };

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
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);

        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        Ile.progress(panel, i, questions.length, score);

        const motCible = el('div', { class: 'rm-cible__mot', 'data-mot': q.cible.mot });
        const carte = el('div', { class: 'rm-cible', role: 'group', 'aria-label': tx.cible }, [
          q.cible.emoji ? el('span', { class: 'rm-emoji', 'aria-hidden': 'true', text: q.cible.emoji }) : null,
          motCible,
          Ile.speakButton(q.cible.mot, tx.ecouter(q.cible.mot)),
        ]);
        const note = el('p', { class: 'rm-note', text: '👀 ' + tx.sansVoix });
        // Sans voix, on ne peut pas entendre la rime : aux niveaux 1–2, la terminaison du mot à faire
        // rimer est surlignée dès le début (le jeu se fait à l'écrit, en comparant les fins de mots).
        majVoix = () => {
          note.hidden = Ile.canSpeak;
          if (!done) motCible.replaceChildren(...noeudsMot(q.cible, !Ile.canSpeak && level <= 2).filter(Boolean));
        };
        majVoix();
        const fb = el('p', { class: 'feedback rm-feedback', 'aria-live': 'polite' });
        const grid = el('div', { class: 'rm-options rm-options--' + q.options.length, role: 'group', 'aria-label': tx.choix });

        function trouve(b, ligne, motEl) {
          done = true;
          if (firstTry) score++;
          b.classList.add('is-correct');
          ligne.classList.add('is-bonne');
          grid.classList.add('is-fini');
          grid.querySelectorAll('.rm-choice').forEach((x) => { if (x !== b) x.disabled = true; });
          // La terminaison commune, surlignée dans les deux mots.
          motCible.replaceChildren(...noeudsMot(q.cible, true).filter(Boolean));
          motEl.replaceChildren(...noeudsMot(q.bonne, true).filter(Boolean));
          Ile.flash(b, 'good');
          Ile.flash(carte, 'good');
          Ile.sfx('good');
          // La rime telle qu'elle est surlignée : « -at » ; « -ite » pour kite / white (famille « ight ») ;
          // « -ey / -ee » pour key / bee : un même son, deux écritures. En chinois, la finale du pinyin.
          const fins = hanzi() ? [q.cible.fam.son] : [q.cible.rime, q.bonne.rime].filter((x, k, a) => a.indexOf(x) === k);
          const puce = el('span', { class: 'rm-son', lang: hanzi() ? 'zh-Latn-pinyin' : null }, [
            el('span', { 'aria-hidden': 'true', text: '🎵 ' }), fins.map(tx.son).join(' / '),
          ]);
          const [avant, apres] = (firstTry ? tx.bravo : tx.oui).split('{s}');
          fb.className = 'feedback rm-feedback feedback--good';
          fb.replaceChildren(avant, puce, apres || '');
          Ile.say(tx.dire(q.cible.mot, q.bonne.mot));
          Ile.progress(panel, i + 1, questions.length, score);
          i++;
          setTimeout(next, PAUSE);
        }

        q.options.forEach((opt) => {
          const motEl = el('span', { class: 'rm-choice__mot' }, noeudsMot(opt, false));
          const b = el('button', { type: 'button', class: 'btn choice rm-choice', 'data-mot': opt.mot }, [
            opt.emoji ? el('span', { class: 'rm-emoji', 'aria-hidden': 'true', text: opt.emoji }) : null,
            motEl,
          ]);
          const ligne = el('div', { class: 'rm-option' }, [b, Ile.speakButton(opt.mot, tx.ecouter(opt.mot))]);
          b.addEventListener('click', () => {
            if (done || b.disabled || !panel.isConnected) return;
            if (opt === q.bonne) { trouve(b, ligne, motEl); return; }
            firstTry = false;
            b.classList.add('is-wrong');
            b.disabled = true;
            Ile.flash(b, 'bad');
            Ile.sfx('bad');
            Ile.feedback(fb, false, tx.non(opt.mot));
            fb.classList.add('rm-feedback');
            // Le focus reste dans le jeu pour les joueurs au clavier.
            const reste = grid.querySelector('.rm-choice:not(:disabled)');
            if (reste) reste.focus({ preventScroll: true });
          });
          grid.appendChild(ligne);
        });

        panel.append(el('p', { class: 'consigne', text: tx.consigne }), carte, note, grid, fb);
        if (focusDansLeJeu && i > 0) grid.querySelector('.rm-choice').focus({ preventScroll: true });
        Ile.say(q.cible.mot);
      }

      next();
    },
  });
})();
