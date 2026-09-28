/* Mémo des mots : des cartes face cachée, chaque paire = une image + son mot.
 * Niveau 1 : 4 paires (8 cartes), niveau 2 : 6 paires (12), niveau 3 : 8 paires (16).
 * Une paire trouvée reste visible et le mot est prononcé avec son article (a cat, eine Katze, 一只猫).
 * Chinois : la carte-mot montre les caractères et leur pinyin dessous. Allemand, luxembourgeois :
 * les noms gardent leur majuscule ; un mot trop long se coupe entre deux syllabes (Schmet-terling).
 * Score (sur le nombre de paires) : une partie en « paires + 2 » coups ou moins donne tous les points ;
 * chaque petit groupe de coups en plus retire un point, sans jamais descendre sous 1.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'memory';
  const PAIRES = { 1: 4, 2: 6, 3: 8 };
  const DELAI_RETOUR = 1100; // deux cartes différentes restent visibles ~1 s, clics bloqués
  const DELAI_FIN = 1300;
  // Le dos des cartes montre un palmier : aucune paire n'utilise cette image, pour ne pas tromper l'œil.
  const EMOJI_DOS = '🌴';

  // Textes propres au jeu, par langue (mêmes clés partout).
  // fin(p, n) et objectif(n) reçoivent des nombres : chaque langue accorde « coup » à sa façon.
  // points(s, t) : ligne de score de la fenêtre de fin (le texte commun parle de « bonnes réponses »,
  // alors qu'ici toutes les paires sont trouvées et le score dépend du nombre de coups).
  const T = {
    en: {
      consigne1: 'Turn over two cards: find the picture and its word!',
      consigne: 'Turn over two cards at a time: match each picture with its word!',
      grille: 'Memory cards',
      pairesAria: 'Pairs found',
      paires: (n, t) => n + ' / ' + t + ' pairs',
      coups: (n) => n + (n === 1 ? ' turn' : ' turns'),
      carteCachee: (k) => 'Card ' + k + ', face down',
      carteImage: (k, mot) => 'Card ' + k + ', picture: ' + mot,
      carteMot: (k, mot) => 'Card ' + k + ', word: ' + mot,
      trouvee: ', pair found',
      paire: (g, emoji) => 'Well done! It’s ' + g + ' ' + emoji,
      pasPareil: 'Not a pair… Try to remember where they are!',
      toutes: 'Well done! You found all the pairs!',
      fin: (p, n) => 'You found all ' + p + ' pairs in ' + n + (n === 1 ? ' turn.' : ' turns.'),
      sansErreur: 'Not a single mistake: what a memory!',
      memoire: 'What a memory!',
      objectif: (n) => 'For 3 stars, try to finish in ' + n + ' turns or fewer!',
      points: (s, t) => s + ' / ' + t + (s === 1 ? ' point' : ' points'),
    },
    zh: {
      consigne1: '翻开两张卡片：找出图片和它的词语！',
      consigne: '每次翻两张卡片，给图片找到它的词语！', // 18 caractères : une seule ligne sur téléphone
      grille: '记忆卡片',
      pairesAria: '已找到的对数',
      paires: (n, t) => n + ' / ' + t + ' 对',
      coups: (n) => n + ' 步',
      carteCachee: (k) => '第 ' + k + ' 张卡片，背面朝上',
      carteImage: (k, mot) => '第 ' + k + ' 张卡片，图片：' + mot,
      carteMot: (k, mot) => '第 ' + k + ' 张卡片，词语：' + mot,
      trouvee: '，已配对',
      paire: (g, emoji) => '真棒！这是' + g + ' ' + emoji,
      pasPareil: '这两张不是一对……记住它们的位置！',
      toutes: '真棒！所有的卡片都配好对了！',
      fin: (p, n) => '你用了 ' + n + ' 步，找到了全部 ' + p + ' 对卡片。',
      sansErreur: '一次也没有翻错，记性真好！',
      memoire: '记性真好！',
      objectif: (n) => '想得 3 颗星，试试在 ' + n + ' 步以内完成！',
      points: (s, t) => '得分：' + s + ' / ' + t,
    },
    de: {
      consigne1: 'Dreh zwei Karten um: Finde das Bild und sein Wort!',
      consigne: 'Dreh immer zwei Karten um: Finde zu jedem Bild das passende Wort!',
      grille: 'Memo-Karten',
      pairesAria: 'Gefundene Paare',
      paires: (n, t) => n + ' / ' + t + ' Paare',
      coups: (n) => n + (n === 1 ? ' Zug' : ' Züge'),
      carteCachee: (k) => 'Karte ' + k + ', verdeckt',
      carteImage: (k, mot) => 'Karte ' + k + ', Bild: ' + mot,
      carteMot: (k, mot) => 'Karte ' + k + ', Wort: ' + mot,
      trouvee: ', Paar gefunden',
      paire: (g, emoji) => 'Super! Das ist ' + g + ' ' + emoji,
      pasPareil: 'Das ist kein Paar … Merk dir gut, wo die Karten liegen!',
      toutes: 'Super! Du hast alle Paare gefunden!',
      fin: (p, n) => 'Du hast alle ' + p + ' Paare in ' + n + (n === 1 ? ' Zug' : ' Zügen') + ' gefunden.',
      sansErreur: 'Ohne einen einzigen Fehler: Was für ein Gedächtnis!',
      memoire: 'Was für ein Gedächtnis!',
      objectif: (n) => 'Für 3 Sterne: Versuch es mit ' + n + ' Zügen oder weniger!',
      points: (s, t) => s + ' / ' + t + (s === 1 ? ' Punkt' : ' Punkte'),
    },
    lb: {
      consigne1: 'Dréi zwou Kaarten ëm: Fann d’Bild an d’Wuert!',
      consigne: 'Dréi all Kéier zwou Kaarten ëm: Fann d’Bild an d’Wuert, déi zesummepassen!',
      grille: 'Memo-Kaarten',
      pairesAria: 'Pairen, déi fonnt sinn',
      paires: (n, t) => n + ' / ' + t + ' Pairen',
      coups: (n) => n + (n === 1 ? ' Kéier' : ' Kéieren'),
      carteCachee: (k) => 'Kaart ' + k + ', verstoppt',
      carteImage: (k, mot) => 'Kaart ' + k + ', Bild: ' + mot,
      carteMot: (k, mot) => 'Kaart ' + k + ', Wuert: ' + mot,
      trouvee: ', Pair fonnt',
      paire: (g, emoji) => 'Super! Dat ass ' + g + ' ' + emoji,
      pasPareil: 'Dat ass kee Pair … Mierk der gutt, wou d’Kaarte leien!',
      toutes: 'Bravo! Du hues all d’Paire fonnt!', // règle de l'n : Pairen → Paire devant f
      // Règle de l'n : Kéieren → Kéiere devant « gebraucht » ; Pairen garde son n devant « ze ».
      fin: (p, n) => 'Du hues ' + n + (n === 1 ? ' Kéier' : ' Kéiere') + ' gebraucht, fir all ' + p + ' Pairen ze fannen.',
      sansErreur: 'Ouni Feeler: Wat fir e Gedächtnes!',
      memoire: 'Wat fir e Gedächtnes!',
      objectif: (n) => 'Fir 3 Stären: Probéier et mat ' + n + ' Kéieren oder manner!',
      points: (s, t) => s + ' / ' + t + (s === 1 ? ' Punkt' : ' Punkten'),
    },
    fr: {
      consigne1: 'Retourne deux cartes\u00a0: trouve l’image et son mot\u00a0!',
      consigne: 'Retourne deux cartes à la fois\u00a0: associe chaque image à son mot\u00a0!',
      grille: 'Cartes du mémo',
      pairesAria: 'Paires trouvées',
      paires: (n, t) => n + ' / ' + t + ' paires',
      coups: (n) => n + (n > 1 ? ' coups' : ' coup'),
      carteCachee: (k) => 'Carte ' + k + ', cachée',
      carteImage: (k, mot) => 'Carte ' + k + ', image\u00a0: ' + mot,
      carteMot: (k, mot) => 'Carte ' + k + ', mot\u00a0: ' + mot,
      trouvee: ', paire trouvée',
      paire: (g, emoji) => 'Bravo\u00a0! C’est ' + g + ' ' + emoji,
      pasPareil: 'Ce n’est pas une paire… Retiens bien leur place\u00a0!',
      toutes: 'Bravo\u00a0! Tu as trouvé toutes les paires\u00a0!',
      fin: (p, n) => 'Tu as trouvé les ' + p + ' paires en ' + n + (n > 1 ? ' coups.' : ' coup.'),
      sansErreur: 'Sans une seule erreur\u00a0: quelle mémoire\u00a0!',
      memoire: 'Quelle mémoire\u00a0!',
      objectif: (n) => 'Pour gagner 3 étoiles, essaie en ' + n + ' coups ou moins\u00a0!',
      points: (s, t) => s + ' / ' + t + (s > 1 ? ' points' : ' point'),
    },
  };

  // Deux images dont l'une nomme AUSSI l'autre ne vont jamais dans la même partie : l'enfant qui
  // associe 🦉 au mot « oiseau », 🚌 à « 车 » ou 🌴 à « arbre » a raison, et le jeu lui dirait
  // « ce n'est pas une paire ». Table commune : Ile.ambigu (js/common.js).
  // Mots de la partie : ceux du niveau d'abord (Ile.motsPourPartie), sans deux images ambiguës.
  function choisirMots(level, n) {
    const sansVariante = (e) => String(e).replace(/\uFE0F/g, '');
    const tous = Ile.motsPourPartie(level, Infinity, (m) => sansVariante(m.emoji) !== sansVariante(EMOJI_DOS));
    const choix = [];
    tous.forEach((m) => { if (choix.length < n && !choix.some((c) => Ile.ambigu(c, m))) choix.push(m); });
    return choix;
  }

  const hanzi = () => Ile.L().ecriture === 'hanzi';

  // Règles de coupure des mots trop longs pour une carte, par langue.
  // Partout : entre deux syllabes écrites (pack), jamais une lettre seule sur une ligne,
  // jamais entre deux voyelles (hiatus : Feu-er, Schéi-er). En français : jamais avant une syllabe
  // muette finale (-ble, -que…).
  const VOYELLE = /[aeiouyàâäáéèêëíîïóôöúùûüœæ]/i;
  const MUETTE = /^(?:qu|gu|[^aeiouyàâäáéèêëíîïóôöúùûüœæ])+e$/;
  const COUPE_AVANT_MUETTE = { fr: false };

  // Score : total = nombre de paires. Jusqu'à 2 coups inutiles, aucun point perdu ;
  // ensuite 1 point de moins tous les `pas` coups inutiles (3 pour 4 ou 6 paires, 4 pour 8) ; toujours au moins 1.
  function calculScore(paires, nbCoups) {
    const inutiles = Math.max(0, nbCoups - paires);
    const pas = Math.max(3, Math.round(paires / 2));
    const penalite = Math.ceil(Math.max(0, inutiles - 2) / pas);
    return Math.max(1, paires - penalite);
  }

  function coupures(m) {
    const syl = m.syl || [];
    if (syl.join('') !== m.mot) return [];
    const muetteOk = COUPE_AVANT_MUETTE[Ile.getLang()] !== false;
    const res = [];
    let pos = 0;
    for (let k = 0; k < syl.length - 1; k++) {
      pos += syl[k].length;
      if (pos < 2 || m.mot.length - pos < 2) continue;
      if (!muetteOk && k === syl.length - 2 && MUETTE.test(syl[k + 1])) continue;
      if (VOYELLE.test(m.mot[pos - 1]) && VOYELLE.test(m.mot[pos])) continue;
      res.push(pos);
    }
    return res;
  }

  // Recto d'une carte-mot : le mot ; en chinois, les caractères et leur pinyin dessous.
  function contenuMot(m) {
    if (!hanzi()) return el('span', { class: 'memo-mot', 'data-mot': m.mot, text: m.mot });
    return el('span', { class: 'memo-mot memo-mot--hanzi', 'data-mot': m.mot }, [
      el('span', { class: 'memo-car', text: m.mot }),
      el('span', { class: 'memo-py pinyin', lang: 'zh-Latn-pinyin', text: Ile.aide(m) }),
    ]);
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const nbPaires = PAIRES[level] || 4;
      const mots = choisirMots(level, nbPaires);
      const total = mots.length;
      const nbLignes = Math.ceil((total * 2) / 4);
      const avecPinyin = hanzi();

      let nbCoups = 0;
      let trouvees = 0;
      let premiere = null; // première carte retournée de l'essai en cours
      let bloque = false; // pendant le retour des cartes et la fin de partie

      const panel = el('section', { class: 'panel memo' + (avecPinyin ? ' memo--hanzi' : ''), 'aria-label': Ile.game(ID).titre });
      root.appendChild(panel);
      const actif = () => panel.isConnected;

      const compteurNb = el('span');
      const compteur = el('span', { class: 'memo-coups', 'data-coups': '0' }, [el('span', { 'aria-hidden': 'true', text: '👆 ' }), compteurNb]);
      function majStatut() {
        const bar = Ile.progress(panel, trouvees, total);
        bar.setAttribute('aria-label', tx.pairesAria);
        bar.querySelector('.progress__label').textContent = tx.paires(trouvees, total);
        if (!compteur.isConnected) bar.appendChild(compteur);
        compteurNb.textContent = tx.coups(nbCoups);
        compteur.setAttribute('data-coups', String(nbCoups));
      }

      const fb = el('p', { class: 'feedback memo-feedback', 'aria-live': 'polite' });
      const annonce = el('p', { class: 'sr-only', 'aria-live': 'polite' }); // contenu des cartes, pour les lecteurs d'écran
      const grille = el('div', { class: 'memo-grille memo-grille--' + nbLignes, role: 'group', 'aria-label': tx.grille });
      const mesure = el('span', { class: 'memo-mesure', 'aria-hidden': 'true' });

      // Deux cartes par mot : l'image et le mot écrit.
      const cartes = Ile.shuffle(mots.reduce((acc, m) => acc.concat([
        { m, type: 'image' },
        { m, type: 'mot' },
      ]), []));

      function nomCarte(c) {
        return c.type === 'image' ? tx.carteImage(c.num, c.m.mot) : tx.carteMot(c.num, c.m.mot);
      }

      cartes.forEach((c, k) => {
        c.num = k + 1;
        const contenu = c.type === 'image'
          ? el('span', { class: 'memo-emoji', text: c.m.emoji })
          : contenuMot(c.m);
        c.btn = el('button', {
          type: 'button',
          class: 'memo-carte memo-carte--' + c.type,
          'data-type': c.type,
          'aria-label': tx.carteCachee(c.num),
        }, [
          el('span', { class: 'memo-inner', 'aria-hidden': 'true' }, [
            el('span', { class: 'memo-face memo-dos' }, [el('span', { class: 'memo-dos__ile', text: EMOJI_DOS })]),
            el('span', { class: 'memo-face memo-recto' }, [contenu]),
          ]),
        ]);
        c.contenu = contenu;
        c.visible = false;
        c.trouvee = false;
        c.btn.addEventListener('click', () => retourner(c));
        grille.appendChild(c.btn);
      });

      function montrer(c, oui) {
        c.visible = oui;
        c.btn.classList.toggle('is-visible', oui);
        c.btn.setAttribute('aria-label', oui ? nomCarte(c) : tx.carteCachee(c.num));
      }

      function retourner(c) {
        if (!actif() || bloque || c.visible || c.trouvee) return;
        Ile.sfx('flip');
        montrer(c, true);
        annonce.textContent = nomCarte(c);

        if (!premiere) {
          premiere = c;
          fb.textContent = '';
          fb.className = 'feedback memo-feedback';
          return;
        }

        const a = premiere;
        premiere = null;
        nbCoups++;

        if (a.m === c.m) {
          // Une paire : les deux cartes restent visibles, en vert, et on entend le mot.
          trouvees++;
          [a, c].forEach((x) => {
            x.trouvee = true;
            x.btn.classList.add('is-trouvee');
            x.btn.setAttribute('aria-disabled', 'true');
            x.btn.setAttribute('aria-label', nomCarte(x) + tx.trouvee);
            Ile.flash(x.btn, 'good');
          });
          Ile.sfx('good');
          const gn = Ile.groupe(Ile.un(c.m), c.m.mot);
          Ile.say(gn);
          majStatut();
          if (trouvees === total) {
            bloque = true;
            Ile.feedback(fb, true, tx.toutes);
            setTimeout(fin, DELAI_FIN);
          } else {
            Ile.feedback(fb, true, tx.paire(gn, c.m.emoji));
          }
          fb.classList.add('memo-feedback');
          return;
        }

        // Pas la même paire : on les regarde un instant, puis elles se retournent.
        majStatut();
        bloque = true;
        [a, c].forEach((x) => { x.btn.classList.add('is-rate'); Ile.flash(x.btn, 'bad'); });
        Ile.feedback(fb, false, tx.pasPareil);
        fb.classList.add('memo-feedback');
        setTimeout(() => {
          if (!actif()) return; // partie remplacée (niveau, langue, rejouer)
          [a, c].forEach((x) => { x.btn.classList.remove('is-rate'); montrer(x, false); });
          bloque = false;
        }, DELAI_RETOUR);
      }

      function fin() {
        if (!actif()) return;
        const score = calculScore(total, nbCoups);
        const sep = avecPinyin ? '' : ' ';
        let message = tx.fin(total, nbCoups);
        if (nbCoups === total) message += sep + tx.sansErreur;
        else if (score === total) message += sep + tx.memoire;
        else message += sep + tx.objectif(total + 2);
        Ile.showResult({ id: ID, score, total, message, scoreTexte: tx.points(score, total) });
      }

      // Flèches du clavier : se déplacer de carte en carte dans la grille
      // (d'après la position à l'écran, car le nombre de colonnes dépend de la largeur).
      grille.addEventListener('keydown', (e) => {
        if (['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].indexOf(e.key) === -1) return;
        const btns = cartes.map((c) => c.btn);
        const i = btns.indexOf(document.activeElement);
        if (i === -1) return;
        e.preventDefault();
        let cible = null;
        if (e.key === 'ArrowLeft') cible = btns[i - 1];
        else if (e.key === 'ArrowRight') cible = btns[i + 1];
        else {
          const r = btns[i].getBoundingClientRect();
          const x = r.left + r.width / 2;
          const bas = e.key === 'ArrowDown';
          const autres = btns.map((b) => ({ b, r: b.getBoundingClientRect() }))
            .filter((o) => (bas ? o.r.top > r.top + r.height / 2 : o.r.top < r.top - r.height / 2));
          if (autres.length) {
            const ligne = bas ? Math.min(...autres.map((o) => o.r.top)) : Math.max(...autres.map((o) => o.r.top));
            const proches = autres.filter((o) => Math.abs(o.r.top - ligne) < 2);
            proches.sort((p, q) => Math.abs(p.r.left + p.r.width / 2 - x) - Math.abs(q.r.left + q.r.width / 2 - x));
            cible = proches[0].b;
          }
        }
        if (cible) cible.focus();
      });

      /* Taille des mots : le plus grand possible sans déborder de la carte.
       * Si un mot long devient trop petit sur une seule ligne, on le coupe entre deux syllabes.
       * Chinois : caractères sur une ligne, pinyin dessous (sur deux lignes s'il est trop long). */
      const SEUIL = 15; // px : en dessous, on préfère couper le mot entre deux syllabes
      const PY_RATIO = 0.55; // taille du pinyin par rapport aux caractères
      const PY_MIN = 11; // px : en dessous, le pinyin passe sur deux lignes
      function largeur(t) {
        mesure.textContent = t;
        return mesure.getBoundingClientRect().width || 1;
      }
      function dimensions() {
        const exemple = cartes.find((c) => c.type === 'mot');
        if (!exemple) return null;
        const face = exemple.contenu.parentNode;
        const cs = getComputedStyle(face);
        const W = face.clientWidth - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight) - 2;
        const H = face.clientHeight - parseFloat(cs.paddingTop) - parseFloat(cs.paddingBottom) - 2;
        return W > 0 && H > 0 ? { W, H } : null;
      }
      function lignes(c, liste, cls) {
        return liste.map((l, k) => el('span', { class: 'memo-ligne' + (cls && k < liste.length - 1 ? ' ' + cls : ''), text: l }));
      }
      function ajusterMots() {
        if (!actif()) return;
        const d = dimensions();
        if (!d) return;
        if (avecPinyin) { ajusterHanzi(d); return; }
        const { W, H } = d;
        const max = Math.min(W * 0.3, H * 0.4, 44);
        // Pour chaque mot : la plus grande taille sur une ligne ; si c'est trop petit, coupé entre deux
        // syllabes sur deux lignes (Schmet-terling), ou sur trois si deux restent trop petites (Eich-hörn-chen).
        const choix = cartes.filter((c) => c.type === 'mot').map((c) => {
          const mot = c.m.mot;
          const seul = { taille: Math.min(max, (W * 100) / largeur(mot)), morceaux: [mot] };
          if (seul.taille >= SEUIL) return Object.assign({ c }, seul);
          const cuts = coupures(c.m);
          const options = cuts.map((p) => [p]);
          cuts.forEach((p, a) => cuts.slice(a + 1).forEach((q) => options.push([p, q])));
          const meilleures = { 2: null, 3: null };
          options.forEach((pos) => {
            const bornes = [0].concat(pos, [mot.length]);
            const morceaux = bornes.slice(0, -1).map((b, k) => mot.slice(b, bornes[k + 1]));
            if (morceaux.some((x) => x.length < 2)) return;
            const w = Math.max(...morceaux.map((x, k) => largeur(x + (k < morceaux.length - 1 ? '-' : ''))));
            const t = Math.min(max, (W * 100) / w, H / (morceaux.length * 1.15 + 0.1));
            const longueurs = morceaux.map((x) => x.length);
            const e = Math.max(...longueurs) - Math.min(...longueurs);
            const m = meilleures[morceaux.length];
            // À taille presque égale, la coupure la plus équilibrée (para-pluie plutôt que pa-rapluie).
            if (!m || t > m.taille + 0.5 || (t > m.taille - 0.5 && e < m.e)) meilleures[morceaux.length] = { taille: t, e, morceaux };
          });
          let res = seul;
          if (meilleures[2] && meilleures[2].taille > res.taille) res = meilleures[2];
          if (meilleures[3] && res.taille < SEUIL && meilleures[3].taille > res.taille * 1.15) res = meilleures[3];
          return { c, taille: res.taille, morceaux: res.morceaux };
        });
        // Tailles harmonisées : un mot court ne dépasse pas de beaucoup le plus petit.
        const plafond = Math.max(SEUIL, Math.min(...choix.map((o) => o.taille))) * 1.4;
        choix.forEach(({ c, taille, morceaux }) => {
          taille = Math.min(taille, plafond);
          c.contenu.style.fontSize = Math.floor(taille * 10) / 10 + 'px';
          c.contenu.replaceChildren(...lignes(c, morceaux, 'memo-ligne--coupe'));
        });
      }
      function ajusterHanzi({ W, H }) {
        const max = Math.min(W * 0.46, H * 0.42, 46);
        const choix = cartes.filter((c) => c.type === 'mot').map((c) => {
          const py = Ile.aide(c.m);
          const tc = Math.min(max, (W * 100) / largeur(c.m.mot));
          let morceaux = [py];
          let tp = Math.min(tc * PY_RATIO, (W * 100) / largeur(py));
          const syl = py.split(' ');
          if (tp < PY_MIN && syl.length > 1) { // pinyin trop long : deux lignes, coupées entre deux syllabes
            const k = Math.ceil(syl.length / 2);
            const m2 = [syl.slice(0, k).join(' '), syl.slice(k).join(' ')];
            const t2 = Math.min(tc * PY_RATIO, (W * 100) / Math.max(largeur(m2[0]), largeur(m2[1])));
            if (t2 > tp) { tp = t2; morceaux = m2; }
          }
          // Hauteur : caractères + lignes de pinyin doivent tenir dans la carte.
          const h = tc * 1.2 + morceaux.length * tp * 1.2 + 2;
          const f = h > H ? H / h : 1;
          return { c, tc: tc * f, tp: tp * f, morceaux };
        });
        // Tailles harmonisées d'une carte à l'autre (caractères, et pinyin à part).
        const plafond = Math.max(SEUIL, Math.min(...choix.map((o) => o.tc))) * 1.5;
        const plafondPy = Math.max(PY_MIN, Math.min(...choix.map((o) => o.tp))) * 1.2;
        choix.forEach(({ c, tc, tp, morceaux }) => {
          const r = Math.min(1, plafond / tc);
          c.contenu.style.fontSize = Math.floor(tc * r * 10) / 10 + 'px';
          const pyEl = c.contenu.querySelector('.memo-py');
          pyEl.style.fontSize = Math.floor(Math.min(tp, tc * r * PY_RATIO, plafondPy) * 10) / 10 + 'px';
          pyEl.replaceChildren(...lignes(c, morceaux));
        });
      }

      panel.append(
        el('p', { class: 'consigne', text: level === 1 ? tx.consigne1 : tx.consigne }),
        grille,
        fb,
        annonce,
        mesure
      );
      majStatut();
      ajusterMots();

      // Réajuste quand la taille des cartes change (rotation, fenêtre) ou quand la police arrive.
      let prevu = false;
      const replanifier = () => {
        if (prevu || !actif()) return;
        prevu = true;
        requestAnimationFrame(() => { prevu = false; ajusterMots(); });
      };
      if (typeof ResizeObserver === 'function') {
        const ro = new ResizeObserver(() => {
          if (!actif()) { ro.disconnect(); return; }
          replanifier();
        });
        ro.observe(grille);
      } else {
        const onResize = () => {
          if (!actif()) { window.removeEventListener('resize', onResize); return; }
          replanifier();
        };
        window.addEventListener('resize', onResize);
      }
      if (document.fonts && document.fonts.ready) document.fonts.ready.then(replanifier).catch(() => {});
    },
  });
})();
