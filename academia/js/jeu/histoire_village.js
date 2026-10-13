/* Académia : le village raconte (js/jeu/monde.js).
   1. Le but : réveiller l'Arbre-Monde, un badge après l'autre. Le grand l'entend dans le mot de bienvenue du Sage
      (js/accueil.js) ; la petite, et une partie d'avant la place, l'entendent après un premier combat (la caméra va
      montrer l'arbre s'il n'est pas à l'écran). Puis, chaque jour, les trois missions du Sage (js/missions.js), et
      chaque mission tenue annoncée avec son autocollant.
   2. Un chef consolé : la caméra glisse jusqu'à l'Arbre-Monde qui reverdit, puis jusqu'à ce que le chef rend à la place
      (js/jeu/arbre_monde.js), et revient au héros. Plusieurs cadeaux à rendre d'un coup (une partie d'avant la place) :
      un seul regard sur la place, une seule phrase. Au sixième badge, la fête sur la place.
   3. Les habitants parlent de la partie : le Sage montre la prochaine maison (la caméra y va pendant qu'il en parle) et
      ouvre les missions, Hugo fête la dernière évolution, Paco dit qui reste à libérer.
   4. Après une vingtaine de minutes de jeu dans la journée (P().temps, compté ici), le soleil se couche doucement, le
      compagnon bâille, le Sage dit au revoir ; le jeu reste jouable.
   Les annonces passent dans une bulle qui ne bloque rien (#bulleVillage) : la voix la lit, le jeu continue dessous.
   Ce qui a été vu est noté dans P().histoire (id -> date ISO) : but, missions, soir, fete, arbre_<n>, cadeau_<région>,
   evolution_<famille>_<stade>, missions_vues. */
const SOIR_MS = 20 * 60000, BAILLE_MS = 45000, BULLE_MIN_MS = 3500, BULLE_MAX_MS = 9000, REGARD_MS = 650;
const LE_REGION = {dojo: "le ", albion: "le ", germania: "la ", duche: "la ", jardin: "le ", observatoire: "l'"};
const EN_LETTRES = ["zéro", "un", "deux", "trois", "quatre", "cinq", "six"];
const badgesSurSix = k => `${EN_LETTRES[k]} badge${k > 1 ? "s" : ""} sur six`;   // « trois badges sur six », as the voice says it
const AU_REGION = {dojo: "au ", albion: "au ", germania: "à la ", duche: "à la ", jardin: "au ", observatoire: "à l'"};
// the gifts said together (several given back at once)
const CADEAUX_COURTS = {dojo: "les lanternes se rallument", albion: "la fontaine coule", germania: "l'horloge repart",
  duche: "les oiseaux reviennent", jardin: "les bacs fleurissent", observatoire: "des étoiles brillent autour de l'Arbre-Monde"};
const histoireDe = (p = P()) => { if (p && (!p.histoire || typeof p.histoire !== "object")) p.histoire = {}; return p ? p.histoire : {}; };
const noterHistoire = id => { histoireDe()[id] = new Date().toISOString(); sauver(); };
const vuAujourdhui = id => duJour(histoireDe()[id]);

// time played today (P().temps, "2026-10-12" -> ms). When the game counts it itself (js/temps.js, tempsDuJour), it is
// read there and nothing is counted here; otherwise counted here while the child plays (the page in sight, a touch in
// the last two minutes), added to what the save held for today when the count began, and written as the larger of the
// two counts (a count of the same time elsewhere never adds to it twice)
const TEMPS_LOCAL = {cle: "", ms: 0, base: 0, geste: -Infinity, avant: Date.now(), ecrit: 0};
const tempsAilleurs = () => typeof tempsDuJour === "function";
["pointerdown", "keydown"].forEach(k => addEventListener(k, () => { TEMPS_LOCAL.geste = Date.now(); }, {capture: true, passive: true}));
setInterval(() => {
  const t = Date.now(), d = Math.min(t - TEMPS_LOCAL.avant, 10000), p = P();
  TEMPS_LOCAL.avant = t;
  if (!p || tempsAilleurs() || document.hidden || t - TEMPS_LOCAL.geste > 120000 || ["accueil", "creation", "parents"].includes(ecranCourant)) return;
  const j = jour(), cle = p.id + j;
  if (!p.temps || typeof p.temps !== "object") p.temps = {};
  if (cle !== TEMPS_LOCAL.cle) Object.assign(TEMPS_LOCAL, {cle, ms: 0, base: Number(p.temps[j]) || 0});
  TEMPS_LOCAL.ms += d;
  p.temps[j] = Math.max(Number(p.temps[j]) || 0, Math.round(TEMPS_LOCAL.base + TEMPS_LOCAL.ms));
  if (t - TEMPS_LOCAL.ecrit > 60000) { TEMPS_LOCAL.ecrit = t; sauver(); }   // the other saves of the game carry it too
}, 5000);
function tempsJoue(p = P()){
  if (!p) return 0;
  if (tempsAilleurs()) return Number(tempsDuJour(p)) || 0;
  return Math.max(TEMPS_LOCAL.cle === p.id + jour() ? TEMPS_LOCAL.base + TEMPS_LOCAL.ms : 0, (p.temps && Number(p.temps[jour()])) || 0);
}

// a villager (or the tree) says something without stopping the game; the promise ends when the bubble goes
function bulleVillage(cle, nom, texte){
  const b = $("bulleVillage"); if (!b) return Promise.resolve();
  const moi = bulleVillage.n = (bulleVillage.n || 0) + 1, im = $("bvPortrait");
  if (cle === "arbre") im.src = `${dossierPlace()}arbre_monde_${Math.max(0, etatArbre())}.png`; else imageReessayee(im, `${DOSSIER_HD}/portraits/${cle}.png`);
  $("bvNom").textContent = nom; ecrireIcones($("bvTexte"), insecables(texte)); b.hidden = false;
  const t0 = performance.now(), voix = dire(texte);
  return Promise.race([Promise.resolve(voix), wait(BULLE_MAX_MS)])
    .then(() => wait(Math.max(700, BULLE_MIN_MS - (performance.now() - t0))))
    .then(() => { if (bulleVillage.n === moi) b.hidden = true; });
}
const bulleVillageOuverte = () => !!$("bulleVillage") && !$("bulleVillage").hidden;
document.addEventListener("ecran", e => { if (e.detail !== "monde" && $("bulleVillage")) $("bulleVillage").hidden = true; });

class HistoireVillage {
  constructor(scene){
    this.s = scene; this.anim = !calme(); this.occupee = false; this.prochain = performance.now() + 1200; this.baille = 0;
    preparerMissions();
    this.evolutions();   // what evolved while the village was away is noted (Hugo talks about it)
    if (vuAujourdhui("soir")) this.crepuscule(false);
    scene.events.once("shutdown", () => { if ($("bulleVillage")) $("bulleVillage").hidden = true; });
  }
  // every frame (Monde.update); what the village has to tell comes one thing at a time, when nothing else happens
  update(){
    const s = this.s, now = performance.now();
    if (dialogueOuvert() && bulleVillageOuverte()) $("bulleVillage").hidden = true;
    if (now < this.prochain) return;
    this.prochain = now + 400;
    if (ecranCourant !== "monde" || this.occupee || s.moment || s.verrou || dialogueOuvert() || !P()) return;
    if (this.aReveiller()) return this.raconter(() => this.reveiller());
    verifierMissions();
    const m = missionADire();
    if (m) return this.raconter(() => annoncerMission(m).then(() => s.place && s.place.majPanneau()));
    const ev = this.evolutions();
    if (ev) return this.raconter(() => this.hugoFete(ev));
    if (s.enMarche || now - s.guide.dernier < 1500) return;   // the rest when the child stops a moment
    const p = P();
    if (!p.hist.length) return;
    if (!histoireDe().but) return this.raconter(() => this.but());
    if (!vuAujourdhui("missions")) return this.raconter(() => this.annoncerMissions());
    if (!vuAujourdhui("soir") && tempsJoue() >= SOIR_MS) return this.raconter(() => this.coucher());
    if (this.voile && now > this.baille) { this.baille = now + BAILLE_MS; this.bailler(); }
  }
  raconter(f){
    this.occupee = true;
    Promise.resolve().then(f).catch(e => console.error(e)).then(() => { this.occupee = false; this.prochain = performance.now() + 900; });
  }
  parole(cle, texte){ return bulleVillage(cle, {sage: "Le Sage", hugo: "Hugo", paco: "Paco", arbre: "L'Arbre-Monde"}[cle] || "", texte); }
  // ---------- the goal, the day's missions ----------
  // the tree out of sight (the hero far from the square, a narrow screen): the camera goes and shows it while the Sage
  // names it, then comes back to the hero and the child plays again (the bubble goes on)
  async but(){
    noterHistoire("but");
    const s = this.s, pl = s.place, k = etatArbre(), a = pl && pl.im ? pl.arbrePied : null;
    const voir = !!a && !s.cameras.main.worldView.contains(a.x, a.y - 30);
    if (voir) { s.moment = true; s.chemin = []; s.cameras.main.stopFollow(); }
    try {
      if (voir) await this.regarder(a.x, a.y - 28);
      // the tree stirs in its sleep while the Sage names it
      if (a && this.anim) s.tweens.add({targets: pl.im, scaleY: pl.im.scaleX * 1.03, duration: 320, yoyo: true, repeat: 1, ease: "Sine.easeInOut"});
      const dit = this.parole("sage", k === 0 ? "Regarde l'Arbre-Monde, au milieu de la place : il dort, tout gris. Chaque badge que tu gagnes chez un chef des Ombres le réveille un peu !"
        : `Regarde l'Arbre-Monde, au milieu de la place : chaque badge que tu gagnes chez un chef des Ombres le réveille un peu. Encore ${EN_LETTRES[6 - k]}, et il sera en fête !`);
      if (voir) await Promise.race([dit, wait(3200)]);
      return dit;
    } finally {
      if (voir) { await this.regarder(s.heros.x, s.heros.y - 8); s.moment = false; s.suivreHeros(); }
    }
  }
  annoncerMissions(){
    noterHistoire("missions");
    if (this.s.place) this.s.place.majPanneau();
    const p = P(), restent = missionsDuJour().filter(m => !m.fait).length;
    if (!restent) return Promise.resolve();
    const attend = restent > 1 ? `${EN_LETTRES[restent]} missions t'attendent` : "une mission t'attend";
    return this.parole("sage", `${vuAujourdhui("but") ? `${p.nom}, ${attend}` : `Bonjour, ${p.nom} ! ${majuscule(attend)}`} aujourd'hui sur mon panneau, à côté de moi. Touche-le pour les voir !`);
  }
  // ---------- a chief consoled: the tree grows, the square gets its gift back ----------
  aReveiller(){ const pl = this.s.place; return !!pl && !!pl.im && (etatArbre() > pl.etat || chefsConsoles().some(r => !pl.rendus.has(r))); }
  regarder(x, y){
    const cam = this.s.cameras.main;
    if (!this.anim) { cam.centerOn(x, y); return Promise.resolve(); }
    return new Promise(ok => cam.pan(x, y, REGARD_MS, "Sine.easeInOut", true, (c, k) => { if (k >= 1) ok(); }));
  }
  async reveiller(){
    const s = this.s, pl = s.place, k = etatArbre(), cadeaux = chefsConsoles().filter(r => !pl.rendus.has(r));
    s.moment = true; s.chemin = []; s.cameras.main.stopFollow();
    try {
      if (cadeaux.length > 1) {   // a game from before the square: the tree and all the gifts at once, one look, one word
        const ps = [{x: pl.arbrePied.x, y: pl.arbrePied.y - 28}, ...cadeaux.map(r => pl.lieuCadeau(r))], moy = c => ps.reduce((t, q) => t + q[c], 0) / ps.length;
        await this.regarder(moy("x"), moy("y"));
        const h = histoireDe(), t = new Date().toISOString(), arbre = k > pl.etat;
        if (arbre) { pl.grandir(k); h["arbre_" + k] = t; }
        cadeaux.forEach(r => { pl.rendre(r); h["cadeau_" + r] = t; });
        sauver();
        const l = cadeaux.map(r => CADEAUX_COURTS[r]), fin = l.pop();
        await this.parole("sage", `Grâce à tes badges, ${arbre ? "l'Arbre-Monde se réveille et " : ""}la place revit : ${l.join(", ")} et ${fin} !`);
      } else {
        if (k > pl.etat) {
          await this.regarder(pl.arbrePied.x, pl.arbrePied.y - 28);
          pl.grandir(k); noterHistoire("arbre_" + k);
          await this.parole("arbre", ["", "L'Arbre-Monde se réveille : de nouvelles feuilles poussent !", "L'Arbre-Monde est tout vert !",
            "Des fleurs s'ouvrent sur l'Arbre-Monde !", "Des lumières s'allument dans l'Arbre-Monde !", "Des fruits d'or poussent sur l'Arbre-Monde !",
            "L'Arbre-Monde est tout à fait réveillé !"][k]);
        }
        for (const r of cadeaux) {
          const l = pl.lieuCadeau(r);
          await this.regarder(l.x, l.y);
          pl.rendre(r); noterHistoire("cadeau_" + r);
          await this.parole("sage", CADEAUX[r]);
        }
      }
      if (k >= ARBRE_ETATS - 1 && !histoireDe().fete) await this.fete();
    } finally {
      await this.regarder(s.heros.x, s.heros.y - 8);
      s.moment = false; s.suivreHeros();
      if (s.guide) s.guide.retour();
    }
  }
  // the six badges: a party on the square (confetti, fireworks over the tree, everybody jumps for joy)
  async fete(){
    noterHistoire("fete");
    const s = this.s, p = s.place.arbrePied, gens = [...s.pnj.map(n => n.s), s.heros].filter(x => x && x.active);
    if (typeof jouerSon === "function") jouerSon("badge");
    if (typeof pleuvoir === "function") pleuvoir({pieces: [...CONFETTIS, "etoile_p"], n: 50, taille: [12, 24], etale: 2600});
    gens.forEach((g, i) => {
      poseHumain(g, "joie");
      if (this.anim) s.tweens.add({targets: g, y: g.y - 4, duration: 230, yoyo: true, repeat: 5, delay: i * 80, ease: "Quad.easeOut"});
    });
    if (s.compagnon && this.anim) s.tweens.add({targets: s.compagnon, y: s.compagnon.y - 5, duration: 200, yoyo: true, repeat: 6});
    for (let i = 0; i < 6 && this.anim; i++) s.time.delayedCall(i * 420, () => effetHD(s, "evolution", p.x + (Math.random() - .5) * 60, p.y - 30 - Math.random() * 40, 24, 9600));
    await this.parole("sage", `Les six badges ! L'Arbre-Monde est réveillé : c'est la fête à Académia ! Merci, ${P().nom} !`);
    gens.forEach(g => { if (g.active) reposHumain(g, g === s.heros ? s.dir : g.dirRepos); });
  }
  // the tree touched (js/jeu/arbre_monde.js): it says where it is
  parlerArbre(){   // the child asked: it answers at once, over whatever was being said
    const k = etatArbre(), n = 6 - k;
    if (k >= ARBRE_ETATS - 1 && typeof pleuvoir === "function") pleuvoir({pieces: [...CONFETTIS, "etoile_p"], n: 16, taille: [12, 22], etale: 800});
    this.prochain = performance.now() + 4000;
    this.parole("arbre", k === 0 ? "L'Arbre-Monde dort, tout gris. Gagne un badge chez un chef des Ombres pour le réveiller !"
      : k < ARBRE_ETATS - 1 ? `L'Arbre-Monde se réveille : ${badgesSurSix(k)}. Encore ${EN_LETTRES[n]} badge${n > 1 ? "s" : ""}, et il sera en fête !`
      : "L'Arbre-Monde est réveillé. Merci, Gardien !");
  }
  // ---------- evolutions, for Hugo ----------
  // the companions' stages not seen yet are noted; the newest one comes back (to cheer it), or null. The first look,
  // as the village opens, notes in silence what evolved while it was not there, dated by the first fight that shows
  // the new stage (P().hist), else by the game's creation: Hugo never cheers an old evolution as a new one
  evolutions(){
    const p = P(), h = histoireDe(p), silence = !this.dejaLa;
    const quand = (famille, st) => { if (!silence) return new Date().toISOString();
      const x = (p.hist || []).find(e => e && e.compagnon === famille && e.niveau >= PALIERS[st - 1] && e.t);
      return x ? String(x.t) : String(p.cree || "2026-01-01T00:00:00.000Z"); };
    let neuve = null;
    (p ? p.compagnons : []).forEach(c => {
      for (let st = 2; st <= stade(c); st++) {
        const id = `evolution_${c.famille}_${st}`;
        if (!h[id]) { h[id] = quand(c.famille, st); neuve = {famille: c.famille, stade: st}; }
      }
    });
    if (neuve) sauver();
    return neuve && this.dejaLa ? neuve : (this.dejaLa = true, null);
  }
  derniereEvolution(){
    const h = histoireDe(), l = Object.keys(h).filter(k => /^evolution_/.test(k) && Date.now() - Date.parse(h[k]) < 3 * 864e5);
    if (!l.length) return null;
    const [, famille, st] = l.reduce((a, b) => h[b] > h[a] ? b : a).match(/^evolution_(.+)_(\d)$/) || [];
    const F = FAMILLES[famille];
    return F ? {avant: F.noms[st - 2], apres: F.noms[st - 1]} : null;
  }
  hugoFete(ev){
    const s = this.s, n = s.pnj.find(q => q.cle === "hugo"), F = FAMILLES[ev.famille];
    if (n && n.s.active) {
      poseHumain(n.s, "joie");
      if (this.anim) s.tweens.add({targets: n.s, y: n.s.y - 4, duration: 220, yoyo: true, repeat: 3, onComplete: () => reposHumain(n.s, n.s.dirRepos)});
    }
    return this.parole("hugo", `Hourra ! ${F.noms[ev.stade - 2]} a évolué : c'est maintenant ${F.noms[ev.stade - 1]} !`);
  }
  // ---------- what the villagers say (Monde.parler) ----------
  lignes(n){
    if (n.cle === "sage") return this.lignesSage();
    const p = P(), petit = p.age <= 5, c = actif(), l = [];
    if (n.cle === "hugo") {
      const ev = this.derniereEvolution(), palier = c && PALIERS[stade(c)], reste = palier && palier - c.niveau;
      if (ev) l.push(`Hourra ! ${ev.avant} a évolué : c'est maintenant ${ev.apres} ! Tu l'as bien aidé.`);
      if (c) l.push(palier ? `${nomCompagnon(c)} est au niveau ${c.niveau}. Encore ${reste} niveau${reste > 1 ? "x" : ""}, et il évolue !` : `${nomCompagnon(c)} est à sa plus grande forme. Quelle force !`);
      return {lignes: [...l, n.dit[0]]};
    }
    if (n.cle === "paco") {
      const reste = REGIONS.filter(r => !p.compagnons.some(x => x.famille === r.famille)), k = reste.length;
      l.push(!k ? "Tous les compagnons sont libres : ton équipe est au complet. Bravo !"
        : petit ? `Encore ${k} ami${k > 1 ? "s" : ""} à libérer dans les maisons !`
        : `Il reste ${k} compagnon${k > 1 ? "s" : ""} à libérer : ${reste.map(r => FAMILLES[r.famille].noms[0] + " " + (AU_REGION[r.id] || "à ") + r.nom).join(", ")}.`);
      return {lignes: [...l, ...n.dit]};
    }
    return {lignes: n.dit};
  }
  // the Sage: the tree, the next house (the camera goes and looks at it while he speaks), the day's missions
  lignesSage(){
    const p = P(), k = etatArbre(), r = prochaineMaison(p), l = [], s = this.s;
    l.push(k === 0 ? "Regarde l'Arbre-Monde, au milieu de la place : il dort, tout gris. Chaque badge que tu gagnes le réveille un peu !"
      : k < ARBRE_ETATS - 1 ? `L'Arbre-Monde se réveille : ${badgesSurSix(k)}. Encore ${EN_LETTRES[6 - k]}, et il sera en fête !`
      : "Grâce à toi, l'Arbre-Monde est réveillé et Académia fait la fête. Merci, Gardien !");
    let ligneMaison = -1;
    if (r) {
      const reste = ETAPES - Math.min(regionEtat(r.id).etape, ETAPES);
      ligneMaison = l.length;
      l.push(reste <= 0 ? `${majuscule(porteDe(r))} est ouverte : ${r.chef} t'attend ! Regarde, c'est là-bas.`
        : `La prochaine maison, c'est ${LE_REGION[r.id] || ""}${r.nom} : console encore ${reste} Ombre${reste > 1 ? "s" : ""} près d'elle, et sa porte s'ouvrira. Regarde, c'est là-bas !`);
    }
    const restent = missionsDuJour().filter(m => !m.fait).length;
    l.push(restent ? `Et sur mon panneau, ${restent > 1 ? `tes ${restent} missions` : "ta mission"} du jour !` : "Tes missions du jour sont toutes réussies. Bravo !");
    // the camera at the house while the Sage speaks of it, back on the hero (and the board) with the next line
    let regard = "";
    const retour = () => { regard = "revenu"; this.regarder(s.heros.x, s.heros.y - 8).then(() => { s.moment = false; s.suivreHeros(); }); };
    return {lignes: l, choix: [{texte: "🌟 Mes missions", dit: "Touche l'étoile pour voir tes missions.", action: () => ouvrirMissions()},
      {texte: "↩️ Plus tard", action: () => {}}],
      ligne: i => {
        if (regard === "la" && i !== ligneMaison) return retour();
        if (i !== ligneMaison || regard || !s.portes) return;
        const q = s.portes.portes[r.id]; if (!q) return;
        regard = "la"; s.moment = true; s.cameras.main.stopFollow();
        this.regarder(q.x, q.y - 6).then(() => { if (this.anim && regard === "la") effetHD(s, "evolution", q.x, q.y, 18, 9600); });
      },
      fin: () => { if (regard === "la") retour(); }};
  }
  // ---------- the evening ----------
  coucher(){
    noterHistoire("soir");
    const p = P(), c = actif();
    this.crepuscule(true);
    return wait(2400).then(() => { this.baille = performance.now() + BAILLE_MS; this.bailler(); return wait(1200); })
      .then(() => this.parole("sage", `Le soleil se couche sur Académia. Bravo pour aujourd'hui, ${p.nom} !${c ? ` ${nomCompagnon(c)} bâille : il va bientôt dormir.` : ""} À demain !`));
  }
  // a warm dusk over the village (a colour multiplied over the whole view), the lanterns glowing through it
  crepuscule(anime){
    const s = this.s; if (this.voile) return;
    const v = this.voile = s.add.rectangle(0, 0, 100, 100, 0xFFFFFF).setScrollFactor(0).setDepth(9990).setBlendMode(Phaser.BlendModes.MULTIPLY);
    const caler = () => { const c = s.cameras.main; v.setPosition(c.width / 2, c.height / 2).setScale((c.width / c.zoom + 8) / 100, (c.height / c.zoom + 8) / 100); };
    caler(); s.scale.on("resize", caler); s.events.once("shutdown", () => s.scale.off("resize", caler));
    const DE = Phaser.Display.Color.ValueToColor(0xFFFFFF), A = Phaser.Display.Color.ValueToColor(0xE89E8E);
    const teinte = t => { const k = Phaser.Display.Color.Interpolate.ColorWithColor(DE, A, 100, t); v.setFillStyle(Phaser.Display.Color.GetColor(k.r, k.g, k.b)); };
    if (anime && this.anim) s.tweens.addCounter({from: 0, to: 100, duration: 7000, ease: "Sine.easeInOut", onUpdate: tw => teinte(tw.getValue())});
    else teinte(100);
    // the lit lanterns shine through the dusk
    (s.villageHD ? s.villageHD.lumieres : []).forEach(l => {
      if (!l.im || !s.place || !s.place.rendus.has("dojo")) return;
      const g = VILLAGE_HD.poser(s, "lueur", l.x, l.y, 9991, {echelle: 1.6});
      if (!g) return;
      g.setBlendMode(Phaser.BlendModes.ADD).setTint(0xFFD08A).setAlpha(0);
      s.tweens.add({targets: g, alpha: .5, duration: anime && this.anim ? 5000 : 1, onComplete: () => { if (this.anim) s.tweens.add({targets: g, alpha: .3, duration: 1400, yoyo: true, repeat: -1, ease: "Sine.easeInOut"}); }});
    });
  }
  // the companion yawns: it stretches slowly, a little sleepy cloud rises over it
  bailler(){
    const s = this.s, k = s.compagnon; if (!k || !k.active || !dessine(k) || s.enMarche) return;
    if (this.anim) {
      arreterSouffle(k);
      s.tweens.add({targets: k, scaleY: k.base * 1.12, scaleX: k.base * .94, duration: 650, hold: 450, yoyo: true, ease: "Sine.easeInOut", onComplete: () => { if (k.active) reposCreature(k); }});
    }
    if (!s.textures.exists("ico_dodo")) return;
    const z = s.add.image(k.x + 5, k.y - k.displayHeight * .9, "ico_dodo").setDepth(9995), t = 7 / z.height;
    z.setScale(this.anim ? 0 : t);
    if (this.anim) s.tweens.add({targets: z, scale: t, y: z.y - 9, x: z.x + 3, alpha: {from: 1, to: 0}, duration: 2400, ease: "Sine.easeOut", onComplete: () => z.destroy()});
    else s.time.delayedCall(1500, () => z.destroy());
  }
}
