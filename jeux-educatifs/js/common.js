/*
 * L'Île aux mots — boîte à outils commune à tous les jeux (window.Ile).
 * Aucune dépendance, aucun build : chaque page charge les packs de langue (data/lang/<code>.js,
 * qui remplissent window.ILE_LANGS) puis ce fichier, puis le script de son jeu.
 */
(function () {
  'use strict';

  // ---------------------------------------------------------------------------
  // Registre des jeux (ordre de l'île). Titres, lieux et descriptions : pack de langue, clé jeux[id].
  // ---------------------------------------------------------------------------
  const GAMES = [
    { id: 'images', emoji: '🖼️' },
    { id: 'genre', emoji: '⚖️' },
    { id: 'lettre', emoji: '🔍' },
    { id: 'melange', emoji: '🔀' },
    { id: 'syllabes', emoji: '🌉' },
    { id: 'memory', emoji: '🃏' },
    { id: 'pendu', emoji: '🥥' },
    { id: 'dictee', emoji: '🦜' },
    { id: 'mots-caches', emoji: '🔎' },
    { id: 'rimes', emoji: '🎵' },
    { id: 'contraires', emoji: '🔄' },
    { id: 'pluriel', emoji: '👥' },
    { id: 'phrase', emoji: '📜' },
    { id: 'alphabet', emoji: '🔤' },
  ];
  const LEVELS = [1, 2, 3];

  // ---------------------------------------------------------------------------
  // Stockage local (toujours protégé : navigation privée, stockage bloqué…)
  // ---------------------------------------------------------------------------
  const PREFIX = 'ile-aux-mots:';
  const store = {
    get(key, fallback) {
      try {
        const raw = window.localStorage.getItem(PREFIX + key);
        return raw === null ? fallback : JSON.parse(raw);
      } catch (e) {
        return fallback;
      }
    },
    set(key, value) {
      try {
        window.localStorage.setItem(PREFIX + key, JSON.stringify(value));
      } catch (e) { /* stockage indisponible : on continue sans sauvegarder */ }
    },
  };

  // ---------------------------------------------------------------------------
  // Langues : packs déclarés dans data/lang/<code>.js (window.ILE_LANGS)
  // Le site sert à apprendre des langues étrangères : l'ordre est celui de l'île
  // (le français en dernier) et chaque langue peut être masquée dans les paramètres.
  // ---------------------------------------------------------------------------
  const ORDRE_LANGUES = ['en', 'zh', 'de', 'lb', 'fr'];
  const LANGS = window.ILE_LANGS || {};
  const LANG_CODES = ORDRE_LANGUES.filter((c) => LANGS[c])
    .concat(Object.keys(LANGS).filter((c) => ORDRE_LANGUES.indexOf(c) === -1));
  // Repli pour une clé manquante (ne doit pas arriver : tests/packs.js le vérifie).
  const FALLBACK_LANG = LANGS.en ? 'en' : LANG_CODES[0];

  // Langues masquées par l'adulte (paramètres). Au moins une langue reste visible.
  function hiddenLangs() {
    const h = store.get('languesMasquees', []);
    return Array.isArray(h) ? h.filter((c) => LANGS[c]) : [];
  }
  function visibleLangs() {
    const hidden = hiddenLangs();
    const v = LANG_CODES.filter((c) => hidden.indexOf(c) === -1);
    return v.length ? v : LANG_CODES.slice(0, 1);
  }
  function isLangVisible(code) { return visibleLangs().indexOf(code) !== -1; }

  function detectLang() {
    try {
      const q = new URLSearchParams(window.location.search).get('lang');
      if (q && LANGS[q] && isLangVisible(q)) { store.set('langue', q); return q; }
    } catch (e) { /* URL illisible */ }
    const saved = store.get('langue', null);
    if (saved && LANGS[saved] && isLangVisible(saved)) return saved;
    return visibleLangs()[0];
  }
  let lang = detectLang();

  function applyLangToDocument() {
    const P = LANGS[lang];
    document.documentElement.lang = (P && P.htmlLang) || lang;
    document.documentElement.dir = (P && P.dir) || 'ltr';
  }
  applyLangToDocument();

  function getLang() { return lang; }
  // Pack de la langue courante.
  function L() { return LANGS[lang] || LANGS[FALLBACK_LANG]; }
  function setLang(code) {
    if (!LANGS[code] || code === lang) return;
    lang = code;
    store.set('langue', code);
    applyLangToDocument();
    pickVoice();
    if (canSpeak) { try { window.speechSynthesis.cancel(); } catch (e) { /* rien */ } }
    document.dispatchEvent(new CustomEvent('ile:langue', { detail: { lang: code } }));
  }
  // Masque / affiche une langue. Si la langue courante est masquée, on passe à la première visible.
  function setLangHidden(code, hidden) {
    let h = hiddenLangs().filter((c) => c !== code);
    if (hidden) h.push(code);
    if (LANG_CODES.every((c) => h.indexOf(c) !== -1)) return false; // jamais toutes
    store.set('languesMasquees', h);
    if (hidden && code === lang) setLang(visibleLangs()[0]);
    document.dispatchEvent(new CustomEvent('ile:parametres'));
    return true;
  }
  function langInfo(c) { return { code: c, nom: LANGS[c].nom, drapeau: LANGS[c].drapeau, htmlLang: LANGS[c].htmlLang || c }; }
  // Langues proposées aux enfants (visibles), dans l'ordre de l'île.
  function langues() { return visibleLangs().map(langInfo); }
  // Toutes les langues installées (pour les paramètres).
  function toutesLangues() { return LANG_CODES.map(langInfo); }

  // Texte de l'interface commune : Ile.t('cle', ...args).
  function t(key) {
    const args = Array.prototype.slice.call(arguments, 1);
    let v = L().ui[key];
    if (v === undefined && LANGS[FALLBACK_LANG]) v = LANGS[FALLBACK_LANG].ui[key];
    if (v === undefined) return key;
    return typeof v === 'function' ? v.apply(null, args) : v;
  }
  // Textes propres à un jeu : const T = { en: {…}, zh: {…}, de: {…}, lb: {…}, fr: {…} } ; const tx = Ile.txt(T).
  function txt(dict) {
    const cur = dict[lang] || {};
    const base = dict[FALLBACK_LANG] || {};
    return Object.assign({}, base, cur);
  }
  // Un jeu existe-t-il dans la langue courante ? (pack.jeux[id] === null => non, ex. pluriel en chinois)
  function gameAvailable(id, code) {
    const P = LANGS[code || lang];
    return !!(P && P.jeux && P.jeux[id]);
  }
  // Infos d'un jeu dans la langue courante : { id, emoji, titre, lieu, competence, desc, disponible }.
  // Jeu absent de la langue (jeux[id] === null) : titre générique traduit (« 没有中文版 »), jamais
  // le titre d'une autre langue.
  function game(id) {
    const g = GAMES.find((x) => x.id === id);
    if (!g) return null;
    if (!gameAvailable(id)) {
      return Object.assign({}, g, { titre: t('jeuIndisponibleTitre'), lieu: '', competence: '', desc: '', disponible: false });
    }
    return Object.assign({}, g, L().jeux[id], { disponible: true });
  }
  // Jeux disponibles dans la langue courante, dans l'ordre de l'île.
  function jeuxDisponibles() { return GAMES.filter((g) => gameAvailable(g.id)).map((g) => game(g.id)); }
  function niveaux() { return L().niveaux; }

  function getLevel() {
    const n = Number(store.get('niveau', 1));
    return n === 2 || n === 3 ? n : 1;
  }
  function setLevel(n) { store.set('niveau', n); }

  // Étoiles : meilleur résultat (0 à 3) par langue, par jeu et par niveau.
  function starsFor(score, total) {
    if (!total) return 0;
    const r = score / total;
    if (r >= 0.9) return 3;
    if (r >= 0.6) return 2;
    if (r > 0) return 1;
    return 0;
  }
  function allStars() {
    const all = store.get('etoiles', {});
    return all && typeof all === 'object' ? all : {};
  }
  function getBest(gameId, level) {
    const all = allStars();
    return (all[lang] && all[lang][gameId] && all[lang][gameId][level]) || 0;
  }
  function saveResult(gameId, level, score, total) {
    const stars = starsFor(score, total);
    const all = allStars();
    all[lang] = all[lang] || {};
    all[lang][gameId] = all[lang][gameId] || {};
    const previous = all[lang][gameId][level] || 0;
    if (stars > previous) all[lang][gameId][level] = stars;
    store.set('etoiles', all);
    return { stars, record: stars > previous };
  }
  function resetStars() {
    const all = allStars();
    delete all[lang];
    store.set('etoiles', all);
  }
  function gameStars(gameId) {
    return LEVELS.reduce((s, n) => s + getBest(gameId, n), 0);
  }
  function totalStars() {
    return GAMES.reduce((s, g) => s + gameStars(g.id), 0);
  }
  const MAX_STARS = GAMES.length * LEVELS.length * 3;

  // ---------------------------------------------------------------------------
  // Utilitaires aléatoires / texte
  // ---------------------------------------------------------------------------
  function shuffle(arr) {
    const a = arr.slice();
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
    }
    return a;
  }
  function pick(arr, n) { return shuffle(arr).slice(0, n); }
  function randInt(min, max) { return min + Math.floor(Math.random() * (max - min + 1)); }

  // Minuscule sans accents ni ligatures (comparaisons tolérantes).
  function sansAccents(s) {
    return String(s).normalize('NFD').replace(/[\u0300-\u036f]/g, '')
      .replace(/œ/g, 'oe').replace(/Œ/g, 'OE').replace(/æ/g, 'ae').replace(/Æ/g, 'AE').toLowerCase();
  }
  // Réponse saisie : on ignore la casse et les espaces autour.
  function clean(s) { return String(s).trim().toLowerCase().replace(/[\u2019']/g, '\u2019').replace(/\s+/g, ' '); }

  // Lettre de base d'une lettre (minuscule) selon la langue : é → e en français,
  // mais ä reste ä en allemand (lettre du clavier de la langue : pack.alphabet ou lettresClavier).
  function plier(ch) {
    const c = String(ch).toLowerCase();
    const P = L();
    if (P.alphabet.indexOf(c) !== -1 || (P.lettresClavier || []).indexOf(c) !== -1) return c;
    if (P.ligatures && P.ligatures[c]) return P.ligatures[c];
    const fam = P.familles || {};
    for (const base in fam) { if (fam[base].indexOf(c) !== -1) return base; }
    return sansAccents(c);
  }
  // Lettres d'un mot pour une grille / un clavier : majuscules pliées, ligatures décomposées.
  // Renvoie null si le mot contient autre chose que des lettres (tiret, espace, apostrophe).
  function lettresGrille(mot) {
    if (/[^\p{L}]/u.test(mot)) return null;
    return Array.from(mot).map(plier).join('').toUpperCase().split('');
  }
  // Tri alphabétique dans la langue courante.
  function compare(a, b) { return String(a).localeCompare(String(b), lang, { sensitivity: 'base' }); }

  // Articles (fournis par le pack : champs art et def de chaque mot).
  function un(mot) { return mot.art; }
  function le(mot) { return mot.def; }
  // « article + mot » sans espace après une élision (l’étoile), avec espace sinon (la lune).
  function groupe(article, mot) {
    const a = String(article);
    if (!a) return mot;
    const sep = L().sepMots === undefined ? ' ' : L().sepMots;
    return /[\u2019']$/.test(a) ? a + mot : a + sep + mot;
  }
  // Aide à la lecture affichée sous un mot (pinyin en chinois, rien ailleurs).
  function aide(mot) { return (mot && mot.pinyin) || ''; }
  // Pinyin d'un texte chinois quelconque : mot illustré, sinon dictionnaire PINYIN du pack.
  function pinyin(texte) {
    const P = L();
    if (P.ecriture !== 'hanzi') return '';
    const m = (P.MOTS || []).find((x) => x.mot === texte);
    if (m && m.pinyin) return m.pinyin;
    return (P.PINYIN && P.PINYIN[texte]) || '';
  }
  // Forme « à épeler » d'un mot pour les jeux de lettres : le mot lui-même, ou son
  // pinyin sans tons en chinois (mot.epeler, fourni par le pack).
  function epeler(mot) { return (mot && mot.epeler) || (mot && mot.mot) || ''; }

  // Images dont le mot nomme AUSSI une image plus précise (🦖 霸王龙 est aussi un 恐龙, 🚁 un 飞机,
  // 🌋 une montagne, 🦉 un oiseau, 🚌 un 车…) : jamais l'une comme mauvaise réponse de l'autre,
  // ni les deux dans une même partie de mémo. Clés et valeurs : emojis des packs, sans U+FE0F.
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
  // Ile.ambigu(a, b) : deux mots (ou deux emojis) qu'un enfant peut confondre à bon droit.
  function ambigu(a, b) {
    const x = sansVariante(a && a.emoji !== undefined ? a.emoji : a);
    const y = sansVariante(b && b.emoji !== undefined ? b.emoji : b);
    const inclut = (g, e) => !!GENERIQUES[g] && Array.from(GENERIQUES[g]).indexOf(e) !== -1;
    return x === y || inclut(x, y) || inclut(y, x);
  }

  // Mots disponibles pour un niveau (niveau <= demandé), filtrables.
  function motsNiveau(level, filter) {
    const all = L().MOTS || [];
    const list = all.filter((m) => m.niveau <= level && (!filter || filter(m)));
    return list;
  }
  // Mots du niveau exact en priorité, complétés par les niveaux inférieurs.
  function motsPourPartie(level, n, filter) {
    const exact = shuffle(motsNiveau(level, (m) => m.niveau === level && (!filter || filter(m))));
    const autres = shuffle(motsNiveau(level, (m) => m.niveau < level && (!filter || filter(m))));
    return exact.concat(autres).slice(0, n);
  }

  // ---------------------------------------------------------------------------
  // Son : synthèse vocale + petits bruitages Web Audio
  // ---------------------------------------------------------------------------
  let muted = !!store.get('muet', false);
  let voice = null;
  let voicesKnown = false; // la liste des voix du système est-elle connue ?
  function pickVoice() {
    if (!('speechSynthesis' in window)) return;
    let voices = [];
    try { voices = window.speechSynthesis.getVoices() || []; } catch (e) { voices = []; }
    voicesKnown = voices.length > 0;
    const tts = (L() && L().tts) || 'en-GB';
    const exact = new RegExp('^' + tts.replace('-', '[-_]') + '$', 'i');
    const prefix = new RegExp('^' + tts.split('-')[0] + '([-_]|$)', 'i');
    voice = voices.find((v) => exact.test(v.lang) && /google|natural|premium|enhanced/i.test(v.name))
      || voices.find((v) => exact.test(v.lang))
      || voices.find((v) => prefix.test(v.lang)) || null;
    // Affiche / masque les boutons « écouter » selon la voix disponible pour la langue.
    if (typeof document !== 'undefined') {
      document.querySelectorAll('.speak').forEach((b) => { b.hidden = !voixDisponible(); });
    }
  }
  const speechApi = 'speechSynthesis' in window && typeof window.SpeechSynthesisUtterance === 'function';
  // Une voix existe-t-elle pour la langue courante ? (souvent aucune pour le luxembourgeois :
  // on ne lit jamais un mot avec la voix d'une autre langue.) Liste inconnue => on essaie.
  function voixDisponible() { return speechApi && (!voicesKnown || !!voice); }
  const canSpeak = speechApi; // API présente (pour annuler une lecture en cours)
  if ('speechSynthesis' in window) {
    pickVoice();
    try { window.speechSynthesis.addEventListener('voiceschanged', pickVoice); } catch (e) { /* ancien navigateur */ }
  }

  // Ile.say(texte, { rate?, pitch?, file? }) : lit le texte avec la voix de la langue courante.
  // Par défaut, coupe la lecture en cours ; file: true met le texte à la suite (sans rien couper).
  function say(text, opts) {
    if (!voixDisponible() || muted) return false;
    try {
      if (!(opts && opts.file)) window.speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(text);
      u.lang = L().tts;
      if (voice) u.voice = voice;
      u.rate = (opts && opts.rate) || (L().ttsRate || 0.9);
      u.pitch = (opts && opts.pitch) || 1.05;
      window.speechSynthesis.speak(u);
      return true;
    } catch (e) {
      return false;
    }
  }

  let audioCtx = null;
  function tone(freq, start, dur, type, gain) {
    const ctx = audioCtx;
    const o = ctx.createOscillator();
    const g = ctx.createGain();
    o.type = type || 'sine';
    o.frequency.value = freq;
    g.gain.setValueAtTime(0.0001, ctx.currentTime + start);
    g.gain.exponentialRampToValueAtTime(gain || 0.18, ctx.currentTime + start + 0.02);
    g.gain.exponentialRampToValueAtTime(0.0001, ctx.currentTime + start + dur);
    o.connect(g).connect(ctx.destination);
    o.start(ctx.currentTime + start);
    o.stop(ctx.currentTime + start + dur + 0.05);
  }
  const SFX = {
    good: [[660, 0, 0.12], [880, 0.1, 0.18]],
    bad: [[220, 0, 0.18, 'triangle'], [180, 0.14, 0.22, 'triangle']],
    click: [[520, 0, 0.06, 'triangle', 0.08]],
    win: [[523, 0, 0.14], [659, 0.12, 0.14], [784, 0.24, 0.14], [1047, 0.36, 0.35]],
    flip: [[440, 0, 0.05, 'triangle', 0.06]],
  };
  function sfx(name) {
    if (muted) return;
    try {
      const Ctx = window.AudioContext || window.webkitAudioContext;
      if (!Ctx) return;
      audioCtx = audioCtx || new Ctx();
      if (audioCtx.state === 'suspended') audioCtx.resume();
      (SFX[name] || []).forEach(([f, s, d, t, g]) => tone(f, s, d, t, g));
    } catch (e) { /* audio indisponible */ }
  }
  function isMuted() { return muted; }
  function setMuted(v) {
    muted = !!v;
    store.set('muet', muted);
    if (muted && canSpeak) { try { window.speechSynthesis.cancel(); } catch (e) { /* rien */ } }
    document.querySelectorAll('[data-ile-mute]').forEach(updateMuteButton);
  }
  function updateMuteButton(btn) {
    btn.textContent = muted ? '🔇' : '🔊';
    btn.setAttribute('aria-label', muted ? t('sonActiver') : t('sonCouper'));
    btn.setAttribute('aria-pressed', String(muted));
    btn.title = btn.getAttribute('aria-label');
  }

  // ---------------------------------------------------------------------------
  // Effets visuels
  // ---------------------------------------------------------------------------
  const reduceMotion = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  function confetti(count) {
    if (reduceMotion) return;
    const layer = document.createElement('div');
    layer.className = 'ile-confetti';
    layer.setAttribute('aria-hidden', 'true');
    const bits = ['🐚', '⭐', '🌴', '🐠', '✨', '🌺', '🥥'];
    const n = count || 28;
    for (let i = 0; i < n; i++) {
      const s = document.createElement('span');
      s.textContent = bits[i % bits.length];
      s.style.left = Math.random() * 100 + 'vw';
      s.style.animationDelay = Math.random() * 0.6 + 's';
      s.style.animationDuration = 1.6 + Math.random() * 1.4 + 's';
      s.style.fontSize = 16 + Math.random() * 18 + 'px';
      layer.appendChild(s);
    }
    document.body.appendChild(layer);
    setTimeout(() => layer.remove(), 3600);
  }

  // Anime un élément : 'good' (rebond vert) ou 'bad' (secousse rouge).
  function flash(el, kind) {
    if (!el) return;
    const cls = kind === 'good' ? 'is-good' : 'is-bad';
    el.classList.remove('is-good', 'is-bad');
    void el.offsetWidth; // relance l'animation
    el.classList.add(cls);
  }

  // Message court de feedback dans une zone aria-live.
  function feedback(el, ok, text) {
    if (!el) return;
    el.textContent = text || pick(ok ? t('bravo') : t('encore'), 1)[0];
    el.className = 'feedback ' + (ok ? 'feedback--good' : 'feedback--bad');
  }

  // ---------------------------------------------------------------------------
  // Mise en page commune d'un jeu
  // ---------------------------------------------------------------------------
  function el(tag, attrs, children) {
    const node = document.createElement(tag);
    if (attrs) {
      Object.keys(attrs).forEach((k) => {
        const v = attrs[k];
        if (v === undefined || v === null || v === false) return;
        if (k === 'class') node.className = v;
        else if (k === 'text') node.textContent = v;
        else if (k === 'html') node.innerHTML = v;
        else if (k.startsWith('on') && typeof v === 'function') node.addEventListener(k.slice(2), v);
        else node.setAttribute(k, v === true ? '' : v);
      });
    }
    (children || []).forEach((c) => {
      if (c === null || c === undefined || c === false) return;
      node.appendChild(typeof c === 'string' ? document.createTextNode(c) : c);
    });
    return node;
  }

  // Sélecteur de langue (réutilisé par l'accueil et les jeux).
  function langSelect() {
    const select = el('select', { class: 'lang-select', 'aria-label': t('choisisLangue'), title: t('choisisLangue') },
      langues().map((l) => el('option', { value: l.code, text: l.drapeau + ' ' + l.nom })));
    select.value = lang;
    select.addEventListener('change', () => { sfx('click'); setLang(select.value); });
    return select;
  }

  /*
   * Ile.mountGame({ id, onStart(level, root) })
   * Construit l'en-tête (retour, titre, langue, niveau, étoiles, son), puis appelle
   * onStart(level, root) au chargement, à chaque changement de niveau ou de langue et sur « Rejouer ».
   * Le jeu dessine dans root (#jeu) et lit ses textes avec Ile.t / Ile.txt, ses données avec Ile.L().
   */
  function mountGame(opts) {
    const header = document.getElementById('topbar') || document.body.insertBefore(el('header', { id: 'topbar' }), document.body.firstChild);
    const root = document.getElementById('jeu');
    let starsEl = null;

    function refreshStars() {
      if (starsEl) starsEl.textContent = '⭐ ' + gameStars(opts.id) + ' / 9';
    }

    function renderHeader() {
      const g = game(opts.id);
      document.title = (g ? g.titre : '') + ' · ' + t('titreSite');
      header.className = 'topbar';
      header.innerHTML = '';

      const select = el('select', { id: 'niveau', 'aria-label': t('niveau'), title: t('niveau') },
        niveaux().map((l) => el('option', { value: String(l.n), text: l.emoji + ' ' + l.nom + ' (' + l.classe + ')' })));
      select.value = String(getLevel());
      select.addEventListener('change', () => {
        setLevel(Number(select.value));
        sfx('click');
        start();
      });

      const muteBtn = el('button', { type: 'button', class: 'icon-btn', 'data-ile-mute': '' });
      updateMuteButton(muteBtn);
      muteBtn.addEventListener('click', () => setMuted(!muted));

      starsEl = el('span', { class: 'topbar__stars', title: t('etoilesJeu') });
      refreshStars();

      header.append(
        el('a', { href: '../index.html', class: 'icon-btn back', 'aria-label': t('retourIle'), title: t('retourIle'), text: '🏝️' }),
        el('h1', { class: 'topbar__title' }, [el('span', { 'aria-hidden': 'true', text: g ? g.emoji + ' ' : '' }), g ? g.titre : '']),
        el('div', { class: 'topbar__tools' }, [langSelect(), select, starsEl, muteBtn])
      );
    }

    function start() {
      closeResult();
      if (canSpeak) { try { window.speechSynthesis.cancel(); } catch (e) { /* rien */ } }
      root.innerHTML = '';
      refreshStars();
      if (!gameAvailable(opts.id)) { renderUnavailable(); return; }
      opts.onStart(getLevel(), root);
    }

    // Ce jeu n'existe pas dans la langue choisie (ex. le pluriel en chinois) : on propose les autres.
    function renderUnavailable() {
      const list = el('div', { class: 'choices' }, jeuxDisponibles().map((g) =>
        el('a', { class: 'btn choice', href: g.id + '.html', text: g.emoji + ' ' + g.titre })));
      root.appendChild(el('section', { class: 'panel' }, [
        el('div', { class: 'big-emoji', 'aria-hidden': 'true', text: '🧭' }),
        el('p', { class: 'consigne', text: t('jeuIndisponible') }),
        list,
      ]));
    }

    document.addEventListener('ile:langue', () => { renderHeader(); start(); });
    Ile._restart = start;
    Ile._refreshStars = refreshStars;
    renderHeader();
    start();
    return { restart: start };
  }

  /*
   * Ile.progress(container, index, total, score)
   * Barre de progression + score, à placer en haut de la zone de jeu.
   */
  function progress(container, index, total, score) {
    let bar = container.querySelector('.progress');
    if (!bar) {
      bar = el('div', { class: 'progress', role: 'progressbar', 'aria-valuemin': '0' }, [
        el('div', { class: 'progress__track' }, [el('div', { class: 'progress__fill' })]),
        el('span', { class: 'progress__label' }),
      ]);
      container.prepend(bar);
    }
    bar.setAttribute('aria-valuemax', String(total));
    bar.setAttribute('aria-valuenow', String(index));
    bar.querySelector('.progress__fill').style.width = (total ? (index / total) * 100 : 0) + '%';
    bar.querySelector('.progress__label').textContent =
      t('question', Math.min(index + 1, total), total) + (score !== undefined ? '  ·  ✅ ' + score : '');
    return bar;
  }

  /*
   * Ile.showResult({ id, score, total, message?, scoreTexte? })
   * Enregistre le résultat, affiche la fenêtre de fin (étoiles, rejouer, jeu suivant).
   * scoreTexte : ligne de score écrite par le jeu (« 3 / 4 points »), à la place de « 3 / 4 bonnes réponses ».
   * Le titre est lu APRÈS la lecture en cours (dernière réponse), sans la couper.
   */
  function showResult(opts) {
    const level = getLevel();
    const res = saveResult(opts.id, level, opts.score, opts.total);
    if (Ile._refreshStars) Ile._refreshStars();
    closeResult();

    // Jeu suivant de l'île qui existe dans la langue courante (le pluriel n'existe pas en chinois).
    const idx = GAMES.findIndex((g) => g.id === opts.id);
    let next = null;
    for (let k = 1; k <= GAMES.length && !next; k++) {
      const suivant = GAMES[(idx + k) % GAMES.length];
      if (gameAvailable(suivant.id)) next = game(suivant.id);
    }
    next = next || game(opts.id);
    const titres = t('resultTitres');
    const starsRow = el('div', { class: 'result__stars', role: 'img', 'aria-label': t('etoilesSur3', res.stars) },
      [1, 2, 3].map((i) => el('span', { class: 'result__star' + (i <= res.stars ? ' is-on' : ''), 'aria-hidden': 'true', text: '⭐', style: 'animation-delay:' + (i * 0.18) + 's' })));

    const replay = el('button', { type: 'button', class: 'btn btn--primary', text: t('rejouer') });
    replay.addEventListener('click', () => { sfx('click'); if (Ile._restart) Ile._restart(); });

    const dialog = el('div', { class: 'result', role: 'dialog', 'aria-modal': 'true', 'aria-labelledby': 'result-title' }, [
      el('div', { class: 'result__card' }, [
        el('h2', { id: 'result-title', text: titres[res.stars] }),
        starsRow,
        el('p', { class: 'result__score', text: opts.scoreTexte || t('score', opts.score, opts.total) }),
        opts.message ? el('p', { class: 'result__msg', text: opts.message }) : null,
        res.record && res.stars > 0 ? el('p', { class: 'result__record', text: t('record') }) : null,
        el('div', { class: 'result__actions' }, [
          replay,
          el('a', { class: 'btn', href: next.id + '.html', text: next.emoji + ' ' + t('jeuSuivant') }),
          el('a', { class: 'btn btn--ghost', href: '../index.html', text: '🏝️ ' + t('ile') }),
        ]),
      ]),
    ]);
    document.body.appendChild(dialog);
    replay.focus();
    if (res.stars >= 2) { sfx('win'); confetti(); } else { sfx('click'); }
    say(titres[res.stars], { file: true });
    return res;
  }
  function closeResult() {
    document.querySelectorAll('.result').forEach((n) => n.remove());
  }

  // Bouton « écouter » réutilisable.
  function speakButton(text, label) {
    const b = el('button', { type: 'button', class: 'icon-btn speak', 'aria-label': label || t('ecouter'), title: label || t('ecouter'), text: '🔈' });
    b.addEventListener('click', () => { if (!say(typeof text === 'function' ? text() : text)) flash(b, 'bad'); });
    if (!voixDisponible()) b.hidden = true;
    return b;
  }

  const Ile = {
    // Jeux, niveaux, langues
    GAMES, LEVELS, MAX_STARS, game, niveaux,
    LANGS, ORDRE_LANGUES, langues, toutesLangues, visibleLangs, hiddenLangs, setLangHidden,
    getLang, setLang, L, t, txt, langSelect, gameAvailable, jeuxDisponibles,
    // Progression
    store, getLevel, setLevel, getBest, saveResult, resetStars, gameStars, totalStars, starsFor,
    // Aléatoire, texte, mots
    shuffle, pick, randInt, sansAccents, clean, plier, lettresGrille, compare,
    un, le, groupe, aide, pinyin, epeler, motsNiveau, motsPourPartie, ambigu,
    // Son
    say, voixDisponible, sfx, isMuted, setMuted, updateMuteButton,
    // Interface
    confetti, flash, feedback, el, mountGame, progress, showResult, closeResult, speakButton,
    reduceMotion,
  };
  // Ile.canSpeak : une voix est-elle disponible pour la langue courante (valeur à jour à chaque lecture).
  Object.defineProperty(Ile, 'canSpeak', { get: voixDisponible, enumerable: true });
  window.Ile = Ile;
})();
