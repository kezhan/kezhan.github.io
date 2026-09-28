/* La phrase en désordre : les mots d'une phrase sont mélangés, touche-les dans l'ordre pour la réécrire
 * sur le parchemin du message en bouteille.
 * 6 phrases du pack (niveau choisi en priorité, complété par les niveaux inférieurs). Jetons = p.mots s'il
 * existe, sinon texte.split(' ') (la ponctuation reste collée à son mot) ; phrase = jetons.join(sepMots).
 * Toucher un mot posé le renvoie. Quand tout est posé : juste => bravo et phrase lue ; faux => les mots bien
 * placés depuis le début restent (en vert), les autres reviennent. Deux mots identiques sont interchangeables.
 * Un point par phrase remise en ordre sans erreur ni indice. Si le pack donne p.variantes (autres ordres justes
 * avec les mêmes mots), ils sont acceptés aussi.
 * Chinois : la ponctuation finale (。？) est déjà posée au bout du parchemin ; pinyin sous chaque mot aux
 * niveaux 1–2, et sous la phrase trouvée à tous les niveaux.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'phrase';
  const TOTAL = 6;
  const PONCT_FINALE = /^[。！？…]+$/;

  // Textes propres au jeu, par langue (mêmes clés partout).
  const T = {
    en: {
      consigne: 'Tap the words in the right order to make the sentence.',
      ecouterPhrase: 'Listen to the sentence',
      astuce: 'Tip: the word with the capital letter comes first, and the word with the full stop comes last.',
      astuce2: '',
      parchemin: 'Your sentence',
      reserve: 'Words to use',
      motPose: (m) => '“' + m + '”, tap to take it back',
      motBloque: (m) => '“' + m + '”, in the right place',
      indice: '💡 Hint',
      presque: 'Nearly! The green words are in the right place.',
      pasEncore: 'Not yet! Try a different order.',
      coupDePouce: (m) => '💡 Here’s the next word: “' + m + '”.',
      finParfaite: 'Every sentence right first time, with no hints: brilliant!',
      finNormale: 'You get a point for each sentence with no hints and no mistakes. Have another go!',
    },
    zh: {
      consigne: '按顺序点词语，把句子排好。',
      ecouterPhrase: '听一听句子',
      astuce: '提示：句子最后的标点已经放好了。',
      astuce2: '',
      parchemin: '你的句子',
      reserve: '词语',
      motPose: (m) => '“' + m + '”，点一下拿回去',
      motBloque: (m) => '“' + m + '”，放对了',
      indice: '💡 提示',
      presque: '差一点！绿色的词语放对了。',
      pasEncore: '还不对！换个顺序试试。',
      coupDePouce: (m) => '💡 下一个词是“' + m + '”。',
      finParfaite: '每个句子都排对了，也没用提示：太棒了！',
      finNormale: '没用提示、也没出错的句子，每个得一分。再试一次吧！',
    },
    de: {
      consigne: 'Tippe die Wörter in der richtigen Reihenfolge an.',
      ecouterPhrase: 'Satz anhören',
      astuce: 'Tipp: Das Wort mit dem Punkt kommt ans Ende.',
      astuce2: 'Tipp: Das Verb steht an zweiter Stelle, direkt nach dem ersten Satzteil.',
      parchemin: 'Dein Satz',
      reserve: 'Die Wörter',
      motPose: (m) => '„' + m + '“, antippen zum Zurücklegen',
      motBloque: (m) => '„' + m + '“, richtig',
      indice: '💡 Tipp',
      presque: 'Fast! Die grünen Wörter stehen schon richtig.',
      pasEncore: 'Noch nicht! Probier eine andere Reihenfolge.',
      coupDePouce: (m) => '💡 Das nächste Wort ist „' + m + '“.',
      finParfaite: 'Alle Sätze ohne Fehler und ohne Tipp: super!',
      finNormale: 'Für jeden Satz ohne Tipp und ohne Fehler gibt es einen Punkt. Versuch es noch einmal!',
    },
    lb: {
      consigne: 'Dréck op d’Wierder an der richteger Reiefolleg.',
      ecouterPhrase: 'De Saz lauschteren',
      astuce: 'Tipp: D’Wuert mam Punkt steet um Enn.',
      astuce2: 'Tipp: D’Verb steet op der zweeter Plaz, direkt nom éischten Deel vum Saz.',
      parchemin: 'Däi Saz',
      reserve: 'D’Wierder',
      motPose: (m) => '„' + m + '“, dréck drop, fir et ewechzehuelen',
      motBloque: (m) => '„' + m + '“, richteg',
      indice: '💡 Tipp',
      presque: 'Bal! Déi gréng Wierder sinn op der richteger Plaz.',
      pasEncore: 'Nach net! Probéier eng aner Reiefolleg.',
      coupDePouce: (m) => '💡 Dat nächst Wuert ass „' + m + '“.',
      finParfaite: 'All d’Sätz ouni Feeler an ouni Tipp: Bravo!',
      finNormale: 'Fir all Saz ouni Tipp an ouni Feeler gëtt et e Punkt. Probéier nach eng Kéier!',
    },
    fr: {
      consigne: 'Touche les mots dans le bon ordre pour former la phrase.',
      ecouterPhrase: 'Écouter la phrase',
      astuce: 'Astuce : le mot avec la majuscule vient en premier, le mot avec le point vient en dernier.',
      astuce2: '',
      parchemin: 'Ta phrase',
      reserve: 'Les mots',
      motPose: (m) => '« ' + m + ' », touche pour le reprendre',
      motBloque: (m) => '« ' + m + ' », bien placé',
      indice: '💡 Indice',
      presque: 'Presque ! Les mots en vert sont à la bonne place.',
      pasEncore: 'Pas encore ! Essaie un autre ordre.',
      coupDePouce: (m) => '💡 Coup de pouce : le mot suivant est « ' + m + ' ».',
      finParfaite: 'Toutes les phrases remises en ordre sans erreur ni indice : bravo !',
      finNormale: 'Un point par phrase remise en ordre sans erreur ni indice. Tu peux réessayer !',
    },
  };

  // Typographie française : espace insécable avant ! ? : ; » et après «.
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, ' $1').replace(/« /g, '« ');
  }

  // 6 phrases : niveau exact en priorité, complété par les niveaux inférieurs.
  function choisirPhrases(level) {
    const all = Ile.L().PHRASES || [];
    const exact = Ile.shuffle(all.filter((p) => p.niveau === level));
    const autres = Ile.shuffle(all.filter((p) => p.niveau < level));
    return exact.concat(autres).slice(0, TOTAL);
  }
  const jetonsDe = (p) => (Array.isArray(p.mots) ? p.mots.slice() : p.texte.split(' '));

  // Tuiles mélangées, jamais dans un ordre juste (mots identiques comparés par leur texte).
  function melange(tuiles, ordres) {
    const justes = ordres.map((o) => o.join('\u0001'));
    for (let k = 0; k < 30; k++) {
      const m = Ile.shuffle(tuiles);
      if (justes.indexOf(m.map((t) => t.texte).join('\u0001')) === -1) return m;
    }
    return tuiles.slice().reverse();
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const P = Ile.L();
      const zh = P.ecriture === 'hanzi';
      const sep = P.sepMots === undefined ? ' ' : P.sepMots;
      const phrases = choisirPhrases(level);
      const total = phrases.length;
      let q = 0;
      let score = 0;

      const panel = el('section', { class: 'panel phr', 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      const actif = () => panel.isConnected;
      const pinyinEl = (p) => el('span', { class: 'pinyin', lang: 'zh-Latn-pinyin', text: p });

      function fin() {
        if (!actif()) return;
        Ile.progress(panel, total, total, score);
        Ile.showResult({ id: ID, score, total, message: typo(score === total ? tx.finParfaite : tx.finNormale) });
      }

      function suivante() {
        if (!actif()) return;
        if (q >= total) { fin(); return; }
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body || panel.contains(document.activeElement);
        panel.querySelectorAll(':scope > :not(.progress)').forEach((x) => x.remove());
        Ile.progress(panel, q, total, score);
        manche(phrases[q]);
        if (focusDansLeJeu && q > 0) {
          const b = panel.querySelector('.phr-tuile:not(:disabled)');
          if (b) b.focus({ preventScroll: true });
        }
      }

      function manche(p) {
        const numero = q;
        const jetons = jetonsDe(p);
        // Chinois : la ponctuation finale est un jeton fixe, déjà posé au bout de la phrase.
        const fixe = zh && jetons.length > 1 && PONCT_FINALE.test(jetons[jetons.length - 1]) ? jetons.pop() : null;
        const attendus = jetons;
        // Ordres acceptés : celui du pack, et ses variantes éventuelles (p.variantes : autres phrases justes
        // écrites avec les mêmes mots, textes ou listes de mots), pour ne jamais refuser une bonne réponse.
        const cleMots = (arr) => arr.slice().sort().join('\u0001');
        const autres = Array.isArray(p.variantes) ? p.variantes : [];
        const ordres = [attendus].concat(autres.map((v) => {
          const j = Array.isArray(v) ? v.slice() : String(v).split(' ');
          if (fixe && j[j.length - 1] === fixe) j.pop();
          return j;
        }).filter((j) => cleMots(j) === cleMots(attendus)));
        const tuiles = melange(attendus.map((texte, k) => ({ k, texte, py: zh ? Ile.pinyin(texte) : '', pose: false, btn: null })), ordres);
        const pose = []; // tuiles posées sur le parchemin, dans l'ordre
        let verrou = 0; // nombre de mots bloqués (bien placés depuis le début)
        let erreur = false;
        let indiceUtilise = false;
        let occupe = false; // transition en cours : entrées bloquées
        let fini = false;
        const encore = () => actif() && q === numero && !fini;

        const bulle = el('div', { class: 'phr-manche', 'data-manche': String(numero) });
        const tete = el('div', { class: 'phr-tete' }, [
          el('p', { class: 'consigne', text: typo(tx.consigne) }),
          level <= 2 ? Ile.speakButton(() => p.texte, tx.ecouterPhrase) : null,
        ]);
        const astuce = level === 1 ? tx.astuce : level === 2 ? tx.astuce2 : '';
        const ligne = el('div', { class: 'phr-ligne' });
        const parchemin = el('div', { class: 'phr-parchemin', role: 'group', 'aria-label': tx.parchemin }, [ligne]);
        const reserve = el('div', { class: 'phr-reserve', role: 'group', 'aria-label': tx.reserve });
        const btnIndice = el('button', { type: 'button', class: 'btn btn--sun phr-indice', text: tx.indice });
        const fb = el('p', { class: 'feedback phr-feedback', 'aria-live': 'polite' });

        // Contenu d'un mot : le mot, et son pinyin dessous en chinois.
        function contenu(t, avecPinyin) {
          const mot = el('span', { class: 'phr-texte', text: t.texte });
          return avecPinyin && t.py ? [mot, pinyinEl(t.py)] : [mot];
        }

        function dessiner(faux) {
          const avecPinyin = zh && (level <= 2 || fini);
          ligne.replaceChildren();
          pose.forEach((t, k) => {
            const bloque = fini || k < verrou;
            const b = el('button', {
              type: 'button',
              class: 'phr-mot' + (bloque ? ' is-bloque' : '') + (faux && k >= faux.debut ? ' is-faux' : faux && k < faux.debut ? ' is-bloque' : ''),
              'data-mot': t.texte,
              'aria-label': bloque ? tx.motBloque(t.texte) : tx.motPose(t.texte),
            }, contenu(t, avecPinyin));
            if (bloque) b.disabled = true;
            b.addEventListener('click', (ev) => {
              if (ev.detail > 1 || occupe || !encore() || k < verrou) return; // double clic : un seul coup
              retirer(k);
            });
            ligne.appendChild(b);
          });
          for (let k = pose.length; k < attendus.length; k++) ligne.appendChild(el('span', { class: 'phr-trou', 'aria-hidden': 'true' }));
          // Ponctuation fixe, alignée sur les caractères (une ligne vide à la place du pinyin).
          if (fixe) {
            ligne.appendChild(el('span', { class: 'phr-ponct' }, [
              el('span', { class: 'phr-texte', text: fixe }),
              avecPinyin ? el('span', { class: 'pinyin', 'aria-hidden': 'true', text: '\u00A0' }) : null,
            ]));
          }
        }

        function focusLibre(pref) {
          if (!panel.contains(document.activeElement) && document.activeElement !== document.body) return;
          const b = pref && !pref.disabled ? pref : reserve.querySelector('.phr-tuile:not(:disabled)');
          if (b) b.focus({ preventScroll: true });
        }

        function rendre(t) {
          t.pose = false;
          t.btn.disabled = false;
          t.btn.classList.remove('is-placee');
        }
        function retirer(k) {
          const t = pose.splice(k, 1)[0];
          rendre(t);
          Ile.sfx('flip');
          fb.textContent = '';
          fb.className = 'feedback phr-feedback';
          dessiner();
          focusLibre(t.btn);
        }
        function placer(t, parIndice) {
          pose.push(t);
          t.pose = true;
          t.btn.disabled = true;
          t.btn.classList.add('is-placee');
          Ile.sfx('click');
          dessiner();
          if (pose.length === attendus.length) valider();
          else if (!parIndice) focusLibre(); // indice demandé au clavier : le focus reste sur le bouton
        }
        // Nombre de mots bien placés depuis le début (comparés par leur texte), et l'ordre juste suivi.
        function prefixe() {
          let best = { k: 0, ordre: attendus };
          ordres.forEach((o) => {
            let k = 0;
            while (k < pose.length && pose[k].texte === o[k]) k++;
            if (k > best.k) best = { k, ordre: o };
          });
          return best;
        }

        function reussite() {
          fini = true;
          occupe = true;
          btnIndice.disabled = true;
          if (!erreur && !indiceUtilise) score++;
          const texte = pose.map((t) => t.texte).join(sep) + (fixe || '');
          parchemin.classList.add('is-reussie');
          parchemin.setAttribute('aria-label', texte);
          dessiner();
          Ile.flash(parchemin, 'good');
          Ile.sfx('good');
          Ile.feedback(fb, true, typo(Ile.pick(Ile.t('bravo'), 1)[0]));
          Ile.say(texte);
          Ile.progress(panel, numero + 1, total, score);
          q++;
          // On laisse lire la phrase jusqu'au bout (au plus quelques secondes de plus) : la fenêtre de fin
          // lit son titre et couperait la dernière phrase.
          const limite = Date.now() + 8000;
          const tic = () => {
            if (!actif() || q !== numero + 1) return;
            let parle = false;
            try { parle = !!window.speechSynthesis.speaking; } catch (e) { parle = false; }
            if (parle && Date.now() < limite) { setTimeout(tic, 150); return; }
            suivante();
          };
          setTimeout(tic, Math.min(3800, 1700 + 180 * attendus.length));
        }

        function valider() {
          const { k } = prefixe();
          if (k === attendus.length) { reussite(); return; }
          erreur = true;
          occupe = true;
          dessiner({ debut: k });
          Ile.flash(ligne, 'bad');
          Ile.sfx('bad');
          Ile.feedback(fb, false, typo(k > 0 ? tx.presque : tx.pasEncore));
          setTimeout(() => {
            if (!encore()) return;
            pose.splice(k).forEach(rendre);
            verrou = k;
            occupe = false;
            dessiner();
            focusLibre();
          }, 1100);
        }

        // Indice : les mots mal placés reviennent, le mot suivant se pose (et le point de la phrase est perdu).
        function indice() {
          if (occupe || !encore()) return;
          const { k, ordre } = prefixe();
          if (k >= ordre.length) return;
          indiceUtilise = true;
          pose.splice(k).forEach(rendre);
          const t = tuiles.find((x) => !x.pose && x.texte === ordre[k]);
          verrou = k + 1;
          Ile.feedback(fb, true, typo(tx.coupDePouce(ordre[k])));
          fb.className = 'feedback phr-feedback phr-feedback--indice';
          placer(t, true);
        }
        btnIndice.addEventListener('click', (ev) => { if (ev.detail > 1) return; indice(); });

        tuiles.forEach((t) => {
          const b = el('button', { type: 'button', class: 'tile phr-tuile', 'data-mot': t.texte, 'aria-label': t.texte }, contenu(t, zh && level <= 2));
          b.addEventListener('click', (ev) => {
            if (ev.detail > 1 || occupe || t.pose || !encore()) return; // double clic : un seul coup
            placer(t);
          });
          t.btn = b;
          reserve.appendChild(b);
        });

        bulle.append(tete);
        if (astuce) bulle.appendChild(el('p', { class: 'phr-astuce', text: typo(astuce) }));
        // Le message s'affiche juste sous le parchemin, là où l'enfant regarde (visible aussi sur téléphone).
        bulle.append(parchemin, fb, reserve, el('div', { class: 'actions phr-actions' }, [btnIndice]));
        panel.appendChild(bulle);
        dessiner();
      }

      suivante();
    },
  });
})();
