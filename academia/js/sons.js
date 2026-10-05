/* Académia : la musique et les bruitages. Une musique douce au village (et à l'accueil), vive au combat, un thème pour
   le chef de maison ; des fanfares courtes pour la victoire, le badge, le compagnon libéré, l'évolution et le niveau ;
   des bruitages pour les boutons, les pas, les coups, le bouclier, la porte, les rires. Les fichiers sont dans
   assets/audio/musique et assets/audio/sons, fabriqués par outils/sons/ (sources CC0 et leurs licences, mesures).
   1. Tout passe par Web Audio, dans le contexte des petits sons de js/outils.js. Un navigateur ne joue rien avant le
      premier toucher : la musique voulue part à ce moment-là.
   2. Une musique boucle sans blanc : chaque fichier porte après sa boucle une copie de sa première seconde, et la
      lecture tourne entre DEBUT_BOUCLE et DEBUT_BOUCLE plus la longueur de la boucle.
   3. La musique baisse pendant que la voix parle (événement « voix » de js/voix.js) et pendant une fanfare ; le coin
      Parents la coupe pour un Gardien (P().musique, gardé avec sa partie).
   4. Le jeu appelle jouerAmbiance(nom) et jouerSon(nom) ; les bips de sfx (js/outils.js) prennent la voix des
      bruitages dès qu'ils sont chargés, et la gardent pour un son qui manque.
   5. Les grands moments (js/moments.js) sont suivis par leurs classes : chaque battement de l'évolution, chaque étoile
      du badge, la bulle qui éclate sonnent avec le dessin, même quand un toucher accélère la scène.
   Sans Web Audio, en mode #test, ou quand un fichier ne vient pas, rien ne joue et rien ne casse. */
(() => {
  const DOSSIER = "assets/audio/";
  // <pistes> written by outils/sons/scripts/preparer.py
  const BOUCLES = {village: 90.000000, combat: 64.000000, boss: 54.857143}, DEBUT_BOUCLE = 0.5;
  // </pistes>
  // each music: its volume, how long it takes to come in (s)
  const PISTES = {village: {v: .5, entree: 1.6}, combat: {v: .55, entree: .4}, boss: {v: .58, entree: .4}};
  // each sound: its files (one drawn at random), its volume, how much its pitch may vary; a fanfare makes the music
  // step back while it plays
  const SONS = {
    bouton: {f: ["bouton"], v: .4}, juste: {f: ["juste"], v: .5}, faux: {f: ["faux"], v: .45},
    coup: {f: ["coup1", "coup2", "coup3"], v: .55, varie: .08}, critique: {f: ["critique"], v: .6}, bouclier: {f: ["bouclier"], v: .5},
    pas: {f: ["pas1", "pas2", "pas3", "pas4"], v: .14, varie: .12}, rencontre: {f: ["rencontre"], v: .5},
    rire: {f: ["rire"], v: .4, varie: .05}, joie: {f: ["joie"], v: .4, varie: .05}, scintille: {f: ["scintille"], v: .35},
    magie: {f: ["magie"], v: .45}, tic: {f: ["tic"], v: .28}, etoile: {f: ["etoile"], v: .4},
    porte: {f: ["porte"], v: .55, fanfare: true}, niveau: {f: ["niveau"], v: .5, fanfare: true}, repos: {f: ["repos"], v: .5, fanfare: true},
    victoire: {f: ["victoire"], v: .55, fanfare: true}, badge: {f: ["badge"], v: .55, fanfare: true},
    compagnon: {f: ["compagnon"], v: .55, fanfare: true}, eclosion: {f: ["eclosion"], v: .6, fanfare: true}
  };
  // the game's names (jouerSon) that are not sounds of their own: the end card's confetti (the fanfare came with
  // jouerAmbiance("victoire")), the start of the evolution and of the freed companion (their big sound comes later)
  const DU_JEU = {victoire: "scintille", evolution: "magie", compagnon: "magie"};
  // the computed bips of js/outils.js and the sounds that replace them
  const DES_BIPS = {tap: "bouton", juste: "juste", faux: "faux", coup: "coup", bouclier: "bouclier", niveau: "niveau", evolution: "magie",
    critique: () => ecranCourant === "monde" ? "rencontre" : "critique",          // in the village: an Ombre was touched
    victoire: () => ecranCourant === "combat" || ecranCourant === "fin" ? null : "compagnon"};   // a won fight has its fanfare
  const ORDRE = ["bouton", "juste", "faux", "pas1", "pas2", "pas3", "pas4", "coup1", "coup2", "coup3", "critique", "bouclier", "rencontre",
    "rire", "joie", "porte", "victoire", "scintille", "repos", "niveau", "badge", "compagnon", "magie", "tic", "etoile", "eclosion"];
  const MUSIQUES_GARDEES = 2;          // decoded music in memory at most (a 90 s loop, mono, 48 kHz: 17 MB)

  let ctx = null, debloque = false, voulue = null, courante = null, parle = false, fanfares = 0, cible = 1;
  const canal = {}, tampons = new Map(), prets = new Map(), dernier = {}, sources = {}, joues = [];
  let musiquesDecodees = [];
  const journal = (type, nom) => { joues.push({t: Math.round(performance.now()), type, nom}); if (joues.length > 80) joues.shift(); };
  const musiqueOn = () => { const p = typeof P === "function" ? P() : null; return !(p && p.musique === false); };

  // ---- the audio context: born at the first touch (shared with the bips), three channels into one output
  function contexte(){
    if (ctx || TEST || !(window.AudioContext || window.webkitAudioContext)) return ctx;
    try {
      ctx = (typeof audioCtx !== "undefined" && audioCtx) || new (window.AudioContext || window.webkitAudioContext)();
      if (typeof audioCtx !== "undefined") audioCtx = ctx;
      const sortie = ctx.createGain(); sortie.gain.value = .9; sortie.connect(ctx.destination);
      ["musique", "fanfare", "bruit"].forEach(k => { canal[k] = ctx.createGain(); canal[k].connect(sortie); });
    } catch (e) { ctx = null; }
    return ctx;
  }
  function reveiller(){ try { if (ctx && ctx.state !== "running" && !document.hidden) ctx.resume().catch(() => {}); } catch (e) {} }
  function debloquer(){
    if (!contexte()) return;
    reveiller();
    if (debloque) return;
    debloque = true; journal("debloque", ctx.state);
    regler();
    if (voulue) jouerPiste(voulue);
    setTimeout(() => precharger().catch(() => {}), 1200);    // the pictures of the village first
  }
  ["pointerdown", "pointerup", "mousedown", "touchend", "keydown", "click"].forEach(t => addEventListener(t, debloquer, {capture: true, passive: true}));
  document.addEventListener("visibilitychange", () => {
    if (!ctx) return;
    try { if (document.hidden) ctx.suspend().catch(() => {}); else if (debloque) reveiller(); } catch (e) {}
  });

  // ---- files: fetched, decoded once; a file that does not come is asked again 20 s later (the network may be back)
  const decoder = octets => new Promise((ok, ko) => { const r = ctx.decodeAudioData(octets, ok, ko); if (r && r.catch) r.catch(ko); });
  function charger(fichier){
    if (tampons.has(fichier)) return tampons.get(fichier);
    const p = (async () => {
      try {
        const r = await fetch(DOSSIER + fichier);
        if (!r.ok) return null;
        const octets = await r.arrayBuffer();
        return await Promise.race([decoder(octets), new Promise(ok => setTimeout(ok, 20000, null))]);   // a decoder that never answers
      } catch (e) { return null; }
    })();
    tampons.set(fichier, p);
    p.then(b => {
      if (b && tampons.get(fichier) === p) prets.set(fichier, b);
      else if (!b) setTimeout(() => { if (tampons.get(fichier) === p) tampons.delete(fichier); }, 20000);
    });
    return p;
  }
  // the music decoded stays for the next time, two at most (the one playing is never the one let go)
  function chargerMusique(nom){
    const f = `musique/${nom}.mp3`;
    musiquesDecodees = musiquesDecodees.filter(x => x !== f).concat(f);
    while (musiquesDecodees.length > MUSIQUES_GARDEES) {
      const vieille = musiquesDecodees.find(x => !courante || x !== `musique/${courante.nom}.mp3`);
      musiquesDecodees = musiquesDecodees.filter(x => x !== vieille);
      tampons.delete(vieille); prets.delete(vieille);
    }
    return charger(f);
  }
  async function precharger(){
    for (const f of ORDRE) await charger(`sons/${f}.mp3`);
    if (!courante || courante.nom === "village") chargerMusique("combat");   // the first fight starts at once
  }

  // ---- levels: the music steps back while the voice speaks and while a fanfare plays; cut for this Gardien
  function regler(){
    if (!ctx || !canal.musique) return;
    const t = ctx.currentTime, m = cible = musiqueOn() ? (parle ? .3 : 1) * (fanfares > 0 ? .4 : 1) : 0;
    canal.musique.gain.setTargetAtTime(m, t, parle || fanfares ? .05 : .4);
    canal.fanfare.gain.setTargetAtTime(parle ? .65 : 1, t, .08);
  }
  let finVoix = 0;
  document.addEventListener("voix", e => {
    clearTimeout(finVoix);
    if (e.detail === "debut") { parle = true; return regler(); }
    // the end, once nothing is heard any more (a long sentence may outlast the voice's own timer)
    const verifier = (n = 0) => {
      const encore = ("speechSynthesis" in window && speechSynthesis.speaking) || (typeof audioLb !== "undefined" && audioLb && !audioLb.paused);
      if (encore && n < 40) finVoix = setTimeout(() => verifier(n + 1), 300);
      else { parle = false; regler(); }
    };
    finVoix = setTimeout(verifier, 200);
  });

  // ---- music
  function jouerPiste(nom){
    voulue = nom;
    if (!debloque || !contexte()) return;
    if (!nom || !musiqueOn()) return arreterPiste(.5);
    if (courante && courante.nom === nom) return;
    arreterPiste(nom === "village" ? .9 : .35);
    chargerMusique(nom).then(b => {
      if (!b || !ctx || voulue !== nom || (courante && courante.nom === nom) || !musiqueOn()) return;
      const src = ctx.createBufferSource(), g = ctx.createGain(), t = ctx.currentTime, p = PISTES[nom];
      src.buffer = b; src.loop = true;
      src.loopStart = DEBUT_BOUCLE; src.loopEnd = Math.min(b.duration, DEBUT_BOUCLE + BOUCLES[nom]);
      g.gain.setValueAtTime(.0001, t); g.gain.linearRampToValueAtTime(p.v, t + p.entree);
      src.connect(g); g.connect(canal.musique); src.start(t);
      courante = {nom, src, g}; journal("piste", nom);
    }).catch(() => {});
  }
  function arreterPiste(duree = .6){
    const c = courante; courante = null;
    if (!c || !ctx) return;
    try {
      const t = ctx.currentTime;
      c.g.gain.cancelScheduledValues(t); c.g.gain.setValueAtTime(c.g.gain.value, t); c.g.gain.linearRampToValueAtTime(.0001, t + duree);
      c.src.stop(t + duree + .05);
    } catch (e) {}
  }

  // ---- sounds. true: played (or just played: a bip and its jouerSon at once make one sound); false: not ready
  // yet, the caller's bip plays instead (a fanfare not ready comes a moment later)
  function jouer(nom, o = {}){
    const s = SONS[nom];
    if (!s || !debloque || !ctx) return false;
    const t = performance.now();
    if (!o.toujours && t - (dernier[nom] || 0) < 70) return true;
    if (nom === "coup" && t - (dernier.critique || 0) < 70) return true;     // a critical hit has its own blow
    dernier[nom] = t;
    if (nom === "critique" && sources.coup && t - (dernier.coup || 0) < 70) try { sources.coup.stop(); } catch (e) {}
    const fichier = `sons/${s.f[Math.floor(Math.random() * s.f.length)]}.mp3`, b = prets.get(fichier);
    if (b) { lancer(nom, s, b, o); return true; }
    const p = charger(fichier);
    if (s.fanfare) p.then(x => { if (x && performance.now() - t < 1500) lancer(nom, s, x, o); });
    return !!s.fanfare;
  }
  function lancer(nom, s, b, o){
    try {
      const src = ctx.createBufferSource(), g = ctx.createGain(), vitesse = (o.vitesse || 1) * (s.varie ? 1 + (Math.random() * 2 - 1) * s.varie : 1);
      src.buffer = b; src.playbackRate.value = vitesse;
      g.gain.value = s.v * (o.volume || 1);
      src.connect(g);
      let bout = g;
      if (o.pan && ctx.createStereoPanner) { const pan = ctx.createStereoPanner(); pan.pan.value = o.pan; g.connect(pan); bout = pan; }
      bout.connect(canal[s.fanfare ? "fanfare" : "bruit"]);
      src.onended = () => { try { bout.disconnect(); } catch (e) {} };
      src.start(ctx.currentTime + (o.dans || 0));
      sources[nom] = src; journal("joue", nom);
      if (s.fanfare) {   // the music steps back for as long as the fanfare lasts
        fanfares++; regler();
        setTimeout(() => { fanfares = Math.max(0, fanfares - 1); regler(); }, ((o.dans || 0) + b.duration / vitesse) * 1000);
      }
    } catch (e) {}
  }

  // ---- the big moments, followed by their classes (js/evolution.js, js/liberation.js)
  const GAMME = [0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21];
  let suivi = null, tics = 0, dernierTic = 0, prochaineEtoile = 0;
  function suivreMoments(){
    const m = document.getElementById("moment");
    if (!m || suivi || typeof MutationObserver !== "function") return;
    suivi = new MutationObserver(liste => liste.forEach(r => {
      const e = r.target, c = e.classList, avant = ` ${r.oldValue || ""} `, theme = window.__moment;
      const vient = k => c.contains(k) && !avant.includes(` ${k} `), part = k => !c.contains(k) && avant.includes(` ${k} `);
      if (theme === "evolution" && c.contains("forme") && c.contains("avant") && (vient("cachee") || part("cachee"))) tic();
      else if (theme === "evolution" && c.contains("apres") && vient("nee")) jouer("eclosion");
      else if (theme === "badge" && c.contains("etoile-badge") && c.contains("pleine") && vient("vu")) etoile(e);
      else if (theme === "liberation" && c.contains("bulle-ombre") && vient("eclate")) jouer("compagnon");
      else if (theme === "liberation" && c.contains("libre") && vient("content")) jouer("joie");
    }));
    suivi.observe(m, {subtree: true, attributes: true, attributeFilter: ["class"], attributeOldValue: true});
  }
  // the old and the new form alternate faster and faster: one bell each time, higher and higher
  function tic(){
    const t = performance.now();
    if (calme() || t - dernierTic < 30) return;     // steps passed all at once: no rattle
    if (t - dernierTic > 2500) tics = 0;
    dernierTic = t;
    const k = Math.min(tics++, GAMME.length - 1);
    jouer("tic", {vitesse: 2 ** (GAMME[k] / 12), volume: .8 + .04 * k, toujours: true});
  }
  // the stars of the badge: do, mi, sol
  function etoile(e){
    const i = Math.max(0, [...e.parentNode.children].indexOf(e)), maintenant = ctx ? ctx.currentTime : 0;
    const dans = Math.max(0, prochaineEtoile - maintenant);
    prochaineEtoile = maintenant + dans + .09;
    jouer("etoile", {vitesse: [1, 1.26, 1.5][Math.min(i, 2)], dans, toujours: true});
  }

  // ---- what the game calls
  function jouerAmbiance(nom){
    journal("ambiance", nom);
    if (nom === "victoire") { voulue = null; arreterPiste(.35); if (!jouer("victoire")) bips.victoire && bips.victoire(); return; }
    if (nom === "defaite") { voulue = null; arreterPiste(1.2); jouer("repos", {dans: .5}); return; }
    if (nom === "accueil") nom = "village";
    if (PISTES[nom]) jouerPiste(nom);
  }
  let pasGauche = false;
  function jouerSon(nom){
    journal("son", nom);
    if (nom === "evolution" || nom === "badge" || nom === "compagnon") suivreMoments();
    if (nom === "pas") { pasGauche = !pasGauche; return void jouer("pas", {pan: pasGauche ? -.15 : .15}); }
    jouer(DU_JEU[nom] || nom);
  }
  // screens without a call of their own: the welcome screens have the village's music; a fight that did not ask for
  // its music gets it (a chief's fight: the boss's theme)
  const estBoss = () => { try { const s = JEU.scene.getScene("combat"); return !!(s && s.d && s.d.o && s.d.o.boss); } catch (e) { return false; } };
  document.addEventListener("ecran", e => {
    const id = e.detail;
    if (["accueil", "creation", "choix"].includes(id) || (id === "monde" && voulue !== "village")) jouerPiste("village");
    else if (id === "combat") setTimeout(() => {
      if (ecranCourant === "combat" && voulue !== "combat" && voulue !== "boss") jouerPiste(estBoss() ? "boss" : "combat");
    }, 80);
    regler();                                          // another Gardien may have another setting
    if (!musiqueOn()) arreterPiste(.4); else if (voulue && !courante) jouerPiste(voulue);
  });
  // the bips become the sounds once these are loaded (the bip stays for a sound that is missing)
  const bips = typeof sfx === "object" ? {...sfx} : {};
  Object.entries(DES_BIPS).forEach(([k, v]) => {
    if (typeof bips[k] !== "function") return;
    sfx[k] = () => { const nom = typeof v === "function" ? v() : v; if (nom && !jouer(nom)) bips[k](); };
  });

  // the grown-ups' corner (js/parents.js, Réglages): the music of this Gardien, on or off
  function boutonMusique(){
    const b = el("button", "joker"); b.id = "btnMusique";
    const maj = () => {   // the drawn speaker (js/icones.js), the words say what the button does
      const texte = musiqueOn() ? "🔊 Couper la musique" : "🔊 Remettre la musique";
      if (typeof ecrireIcones === "function") ecrireIcones(b, texte); else b.textContent = texte;
    };
    b.onclick = () => {
      const p = P(); if (!p) return;
      sfx.tap(); p.musique = !musiqueOn(); sauver(); maj();
      regler(); if (!musiqueOn()) arreterPiste(.4); else if (voulue) jouerPiste(voulue);
    };
    maj(); return b;
  }

  Object.assign(window, {jouerAmbiance, jouerSon, boutonMusique});
  // read by the checks (outils/sons/scripts/essai.py): nothing here changes the game
  window.__sons = {etat: () => ({contexte: ctx ? ctx.state : "aucun", debloque, voulue, piste: courante && courante.nom, parle, fanfares,
    musique: musiqueOn(), cible, niveau: canal.musique ? Math.round(canal.musique.gain.value * 100) / 100 : null,
    prets: [...prets.keys()], joues: joues.slice(-40)})};
})();
