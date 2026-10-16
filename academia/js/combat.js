/* Académia : le combat au tour par tour (GDD §4). Chaque attaque est une question de la matière de la région :
   attaque rapide (révision, sûre), frappe massive (la notion du moment ; chez le boss, le défi du chef), combo ultime
   (trois bonnes réponses de suite, en temps limité à partir de 6 ans), et au tour de l'Ombre, le bouclier Vrai/Faux.
   La vie de l'Ombre se compte en bonnes réponses (js/compagnons.js, ombre()) : une erreur ne la touche pas, un coup
   critique brille sans raccourcir le combat. La charge du combo vit sur le compagnon (c.charge, sauvegardée) : chaque
   bonne réponse la remplit, une erreur la vide, d'un combat à l'autre. Au bout de quelques questions, le compagnon
   achève l'Ombre ; à partir de 6 ans, sans une seule bonne réponse, elle s'enfuit dans les herbes.
   L'Ombre est celle touchée au village, teinte et parée par sa région (js/jeu/ombres_regions.js). Une Ombre brillante
   joue sa petite musique et scintille ; à partir de 6 ans, l'attaque rapide glisse sur elle : la frappe massive la
   touche. Au début d'un combat, le Sage souffle parfois quel compagnon du sac est super efficace ici (son élément).
   Chez le chef, la revanche s'annonce, et chaque erreur réparée reçoit son tampon et sa ligne dans le carnet du sac.
   Chaque bonne réponse rapporte des étincelles tout de suite : « +n » vole jusqu'au compteur, en haut à gauche sous la
   case de l'Ombre (à la place de la barre du village, cachée pendant le combat). Consolée, l'Ombre dit merci dans la
   langue de sa maison (« Thank you! », « Danke! », « Merci »), pendant qu'elle s'envole : le combat n'en dure pas plus. */
const COMBO_PLEIN = 3;
// questions before the end (shields and combo questions count): the companion then finishes the fight
const plafondQuestions = boss => P().age <= 5 ? (boss ? 7 : 5) : (boss ? 12 : 7);
const majuscule = t => t.charAt(0).toUpperCase() + t.slice(1);
// étincelles for a right answer: 2, one more when the companion is super efficient here, two more for a critical hit
const ETINCELLES_JUSTE = 2;

// `vue`: the Ombre touched in the village ({cle, brillante}): the fight is against that very one
async function lancerCombat(regionId, boss, vue0 = null){
  const region = regionDe(regionId), c = actif(), p = P(), petit = p.age <= 5;
  const o = ombre(region, boss, vue0); o.pv = o.pvMax;
  // its region's colours (gold when shining); its other poses once it has come into the arena
  if (!boss) o.cle = ombreTeinte(o.espece, regionId, {brillante: o.brillante, ensuite: 1200});
  if (o.brillante) { albumDe(p).brillantes[o.espece] = true; sauver(); }        // met: it shines in the album
  if (boss) new Image().src = pieceDom("tampon");   // the stamp of a mistake repaired is ready (js/effets_dom.js)
  c.pv = pvMax(c);                         // a companion always starts rested: no punishment for children
  c.charge = Math.min(COMBO_PLEIN, c.charge || 0);
  const bilan = {region: regionId, boss, gagne: false, fuite: false, xp: 0, bonnes: 0, total: 0, revanches: 0, defis: 0, etincelles: 0,
    espece: o.espece, nom: o.nom, brillante: o.brillante};
  let toursOmbre = 0, tours = 0, phase2 = false, superDit = false, apresErreur = false, revancheDite = false, glisseDite = false;
  montrer("combat");
  const moi = ++combatEnCours, abandon = () => moi !== combatEnCours;
  if (typeof jouerAmbiance === "function") jouerAmbiance(boss ? "boss" : "combat");
  $("combat").toggleAttribute("data-brillante", o.brillante && !petit);   // the massive strike glows (style_combat.css)
  const vue = monterArene(c, o, region), compteur = compteurCombat(p);
  $("etatLui").classList.toggle("brillante", !!o.brillante);   // its box glows gold, a drawn sparkle before its name
  // a message in the text box, its emojis drawn (js/icones.js)
  const bulle = (texte, ms) => { const fin = vue.bulle(texte, ms); iconiser($("bulleCombat")); return fin; };
  vue.maj();
  const panneau = $("panneau"), cacher = () => panneau.classList.add("cache");
  panneau.innerHTML = ""; cacher();
  await vue.entree();                      // the arena opens; a boss falls onto its stand under its banner
  if (abandon()) return;
  if (o.brillante) {                       // a shining Ombre: its little tune, and the sentence said to its end
    if (typeof jouerSon === "function") jouerSon("brillante");
    // from 6, the massive strike by its button's own name (« frappe massive » is written nowhere on screen), that
    // button glowing (style_combat.css): one sentence, so that the fight starts soon
    await messageCombat(`✨ Oh ! Une Ombre brillante : ${o.nom} ! ` + (petit ? "Comme elle scintille !" : `Pour la toucher, choisis 💥 ${attaquesDe(c)[1]} !`),
      2000, petit ? 4500 : 6000);
  } else {
    const intro = boss ? `${region.icone} ${o.nom} apparaît ! C'est le chef de la maison !` : `${region.icone} ${region.nom} : une Ombre approche, ${o.nom} !`;
    dire(intro); await bulle(intro, 1800);
  }
  if (abandon()) return;
  const astuce = astuceFaiblesse(c, region);   // the region's companion waits in the bag: the Sage whispers it, sometimes
  if (astuce) { await soufflerAstuce(astuce); if (abandon()) return; }
  if (c.charge >= COMBO_PLEIN) {           // the combo charged in an earlier fight is ready at once
    const t = `🌟 ${nomCompagnon(c)} peut lancer ${attaquesDe(c)[2]} !`;
    dire(t); await bulle(t, 1500);
    if (abandon()) return;
  }

  // the étincelles won fly from (x, y) to the counter, which counts them when they arrive
  let montre = p.etincelles;
  const gagner = (n, [x, y]) => {
    p.etincelles += n; bilan.etincelles += n; sauver();
    volerEtincelles(n, x, y, compteur, () => { montre += n; compteur.querySelector("b").textContent = montre; });
  };
  const ouJuste = () => { const b = panneau.querySelector(".reponse.juste"); return b ? centre(b) : vue.ou("moi"); };
  // one question; after a mistake, the Sage shows the potion the first time (js/astuces.js). The chief announces the
  // first revanche of the fight, naming it when it is a short calculation (« Tu te souviens de 8 + 7 ? »)
  const poser = async (q, titre, opts = {}) => {
    if (q.revanche && !revancheDite) {
      revancheDite = true;
      const calc = /^([\d\s+−×÷-]+?)\s*=\s*\?$/.exec(String(q.question || "").trim()), rappel = calc && calc[1].length <= 14 ? ` de ${calc[1].trim()}` : "";
      await messageCombat(`${majuscule(region.chef)} : « Je connais tes erreurs, Gardien ! Tu te souviens${rappel} ? »`, 2000, 5000,
        boss ? imageHD(o.cle, "face", 64) : null);   // his face before his words
      if (abandon()) return {juste: false, ms: 0};
    }
    bilan.total++;
    panneau.classList.remove("choix");
    const attente = poserQuestion(panneau, q, {titre: affiche(titre), ...opts});
    const astucePot = apresErreur && opts.jokers !== false ? astucePotion(panneau) : null;
    const r = await attente;
    if (astucePot) astucePot.fermer();
    if (opts.jokers !== false) apresErreur = false;   // the potion was offered: the next mistake shows it again
    if (!r.juste) apresErreur = true;
    return r;
  };
  // a revanche won: the stamp on the arena, the mistake written in the notebook (js/adaptatif.js); the stamp lands at
  // once, its voice follows the word found said again (js/question_vue.js), and what comes next waits for both (the
  // voice 3 s at most)
  const reparee = q => {
    noterReparee(q);
    const voix = apresRedit(() => dire("Erreur réparée !"));
    const [x, y] = vue.ou("arene");
    return Promise.all([tamponDom("Erreur réparée !", x, y), Promise.race([voix, wait(3000)])]);
  };
  const charger = juste => { c.charge = juste ? Math.min(COMBO_PLEIN, (c.charge || 0) + 1) : 0; };
  const tirage = async mecanique => {
    let q = tirer(region.matiere, mecanique, boss);
    if (!q) { await chargerQuestions(); q = tirer(region.matiere, mecanique, boss); }
    return q;
  };
  const sup = superEfficace(c, region);
  // the companion's blow: n right answers' worth (never more than what is left)
  const frapper = async (n, type = "", ultime = false) => {
    const d = Math.min(o.pv, n * o.coup), crit = type === "critique";
    sfx.coup(); if (crit) sfx.critique();
    if (typeof jouerSon === "function") jouerSon(crit ? "critique" : "coup");
    await vue.frappe({degats: d, type, ultime});
    o.pv -= d; vue.maj();
    if (crit) bilan.xp += 4;
    if (sup) bilan.xp += 3;
    if (sup && !superDit && o.pv > 0) {   // said too: the little one does not read (and hears no « géométrie » at 4)
      superDit = true;
      const t = petit ? `C'est super efficace : ${nomCompagnon(c)} est super fort ici !` : `C'est super efficace : ${elementDe(c)} !`;
      // after the word just found, never over it (js/question_vue.js), and never over the next question; the voice of
      // the attack's choice waits for it in turn (its promise holds the line)
      apresRedit(() => !panneau.classList.contains("cache") && panneau.querySelector(".question") ? null : dire(t)); await bulle(t, 1300);
    }
    else if (crit && !ultime && o.pv > 0) await bulle("Coup critique !", 800);
    if (boss && !phase2 && o.pv > 0 && o.pv <= Math.ceil(o.coups / 2) * o.coup) {   // half its life gone: the boss gets cross
      phase2 = true;
      const t = `${majuscule(region.chef)} se fâche ! Courage, ${p.nom} !`;
      apresRedit(() => dire(t)); await vue.phase2(); await bulle(t, 1500);   // after the word found, never over it
    }
    await wait(300);
  };

  while (c.pv > 0 && o.pv > 0) {
    if (bilan.total >= plafondQuestions(boss)) {
      if (!petit && !bilan.bonnes) {       // not one right answer: the Ombre runs back into the grass (no door step)
        const t = boss ? `${majuscule(region.chef)} s'enfuit dans sa maison ! Reviens le défier.` : `${o.nom} s'enfuit dans les herbes ! Elle reviendra.`;
        dire(t); await vue.fuite(); await bulle(t, 1700);
        bilan.fuite = true; break;
      }
      const t = `✨ ${nomCompagnon(c)} donne le coup final !`;
      apresRedit(() => dire(t)); await bulle(t, 1200);   // after the word found, never over it
      await frapper(o.coups, "critique"); o.pv = 0; vue.maj();
      break;
    }
    const choix = await choisirAttaque(panneau, c, {boss, premier: tours++ === 0, brillante: o.brillante && !petit});
    if (abandon()) return;
    if (choix.action === "fuite") {
      const t = "On rentre au village. L'Ombre t'attend encore dans les herbes !";
      dire(t); await bulle(t, 1600);
      if (abandon()) return;
      taire(); await fermerEtoile();
      retourMonde({gagne: false}); ouvrirEtoile(); return;
    }
    if (choix.action === "combo") {
      const A = attaquesDe(c), limite = petit ? 0 : 14000 + (choix.sablier ? 10000 : 0);
      c.charge = 0;
      if (choix.sablier) { p.jokers.sablier = Math.max(0, (p.jokers.sablier || 0) - 1); sauver(); }
      let justes = 0;
      for (let k = 0; k < 3; k++) {
        const q = await tirage("rapide"); if (!q) break;
        const r = await poser(q, `🌟 Combo ${k + 1}/3` + (choix.sablier ? " · ⏳ +10 s" : ""), {limite, jokers: false});
        if (abandon()) return;
        noter(q, r.juste);
        if (!r.juste) break;
        justes++; bilan.bonnes++; bilan.xp += 8 * (q.revanche ? 3 : 1);
        const depuis = ouJuste();
        // its stamp before the next question (whose time only starts then): « Erreur réparée ! » is heard, not cut;
        // the « +n » flies once the stamp has gone, never across it
        if (q.revanche) { bilan.revanches++; cacher(); await reparee(q); }
        gagner(ETINCELLES_JUSTE + (sup ? 1 : 0), depuis);
        if (abandon()) return;
      }
      cacher();
      if (justes === 3) {                  // the ultimate attack: a fatal blow for an Ombre, three right answers for a boss
        await vue.ultime(A[2]);
        await frapper(boss ? 3 : o.coups, "critique", true); bilan.xp += 25;
        gagner(2, vue.ou("lui"));
      } else if (justes) await frapper(justes);
      else await vue.rate();
    } else {
      const q = await tirage(choix.action);
      if (abandon()) return;
      if (!q) { toast("Pas de question pour cette matière pour l'instant"); return retourMonde(); }   // not a defeat
      const A = attaquesDe(c);
      const titre = q.defi ? "👑 Défi du chef" : (q.revanche ? "🔁 Revanche ! " : "") + {rapide: "⚡ " + A[0], massive: "💥 " + A[1]}[choix.action];
      const r = await poser(q, titre);
      if (abandon()) return;
      const depuis = r.juste ? ouJuste() : null;
      cacher(); noter(q, r.juste); charger(r.juste);
      if (r.juste) {
        bilan.bonnes++; if (q.revanche) bilan.revanches++; if (q.defi) bilan.defis++;
        bilan.xp += (q.defi ? 20 : choix.action === "massive" ? 14 : 6) * (q.revanche ? 3 : 1);
        // a quick right answer is a critical hit: the massive from 6, any attack before (an effect, never a shorter fight)
        const crit = petit ? r.ms < 5000 : choix.action === "massive" && r.ms < 6000;
        const glisse = o.brillante && !petit && choix.action === "rapide";   // a shining Ombre: only the massive strike touches it
        if (q.revanche) await reparee(q);   // its stamp first: the « +n » flies once it has gone, never across it
        gagner(ETINCELLES_JUSTE + (sup ? 1 : 0) + (crit && !glisse ? 2 : 0), depuis);
        if (abandon()) return;
        if (glisse) {
          await vue.glisse();
          if (!glisseDite) { glisseDite = true; await messageCombat(`⚡ ${A[0]} glisse sur elle : choisis 💥 ${A[1]} !`, 1800, 4500); }
        } else await frapper(1, crit ? "critique" : choix.action === "massive" || q.defi ? "fort" : "");
      } else await vue.rate();             // a mistake: the right answer was shown, the attack misses
    }
    if (abandon()) return;
    if (o.pv <= 0) break;
    // the Ombre's turn: every other round for the little ones, with no question; the True/False shield every other turn from 6
    toursOmbre++;
    if (petit && toursOmbre % 2) continue;
    await bulle(`${o.nom} prépare une attaque !`, 900);
    const q = !petit && toursOmbre % 2 && bilan.total < plafondQuestions(boss) ? tirer(region.matiere, "bouclier", false) : null;
    if (q) {
      const r = await poser(q, estVraiFaux(q) ? "🛡️ Bouclier\u00A0! Vrai ou faux\u00A0?" : "🛡️ Bouclier\u00A0!", {jokers: false});
      if (abandon()) return;
      const depuis = r.juste ? ouJuste() : null;
      cacher(); noter(q, r.juste); charger(r.juste);
      if (r.juste) { bilan.bonnes++; bilan.xp += 4; gagner(ETINCELLES_JUSTE, depuis); sfx.bouclier(); await vue.esquive(); continue; }
    }
    sfx.coup(); if (typeof jouerSon === "function") jouerSon("coup");
    await vue.coupOmbre(o.force);
    c.pv -= o.force; vue.maj();
    await wait(500);
  }
  if (abandon()) return;
  bilan.gagne = o.pv <= 0;
  if (bilan.gagne) {
    remercier(o, region, bilan);   // « Danke! »: said while it floats away, in the time the fight already waits
    await vue.ko("lui"); sfx.victoire(); vue.joie();
    if (typeof jouerAmbiance === "function") jouerAmbiance("victoire");
  } else if (!bilan.fuite) {
    await vue.ko("moi");
    if (typeof jouerAmbiance === "function") jouerAmbiance("defaite");
  }
  await wait(700);
  if (abandon()) return;
  return terminerCombat(bilan);
}

// the consoled Ombre thanks the child in its house's language (« Thank you! » at Albion, « Danke! » at Germania,
// « Merci » in the Grand-Duché; the day's language elsewhere): its recording, and the word in the text box with its
// meaning in small, while it floats away (the KO and the 700 ms after it: the fight is not longer). The little one
// hears the meaning at the start of the next voice, the first times (bilan.merci, js/progression.js)
function remercier(o, region, bilan){
  const m = typeof motDuJeu === "function" ? motDuJeu("merci", langueDeMaison(region.id)) : null;
  if (!m) return;
  bilan.merci = {...m, direSens: sensADire(m)};
  // the last word found ends first, and the badge's or the card's voice waits for this one (js/question_vue.js)
  apresRedit(() => dire(m.mot, m.langue));
  const b = $("bulleCombat"), mot = el("b", "mot-etranger"), sens = el("small", "sens-mot");
  mot.lang = m.langue; mot.textContent = m.mot; sens.textContent = m.sens;
  $("panneau").classList.add("cache");
  b.classList.remove("sage", "parle"); b.replaceChildren(mot, " ", sens); b.hidden = false;
  setTimeout(() => { if (b.firstChild === mot) b.hidden = true; }, TEST ? 0 : 1900);
}
// the album of the bag (js/etat.js keeps its shape)
const albumDe = p => p.album = p.album && typeof p.album === "object" ? p.album : {ombres: {}, brillantes: {}, autocollants: [], reparees: []};
// the étincelles counter of the fight, top left like the village's bar (hidden during a fight): just under the Ombre's
// box, which it follows when a long name wraps or the screen turns (never over a creature)
function compteurCombat(p){
  let c = $("etincellesCombat");
  if (!c) {
    c = el("div", "pastille etincelles-combat"); c.id = "etincellesCombat";
    c.setAttribute("aria-label", "Étincelles"); c.append(iconeDom("etincelle"), el("b"));
    $("combat").append(c);
    const lui = $("etatLui"), placer = () => { if (lui.offsetHeight) Object.assign(c.style, {top: lui.offsetTop + lui.offsetHeight + 8 + "px", left: lui.offsetLeft + "px"}); };
    if (window.ResizeObserver) new ResizeObserver(placer).observe(lui);
    addEventListener("resize", () => { placer(); setTimeout(placer, 400); });   // and once the arena has its new layout
    requestAnimationFrame(placer);
  }
  c.querySelector("b").textContent = p.etincelles;
  return c;
}
// a long message goes on with a touch on the text box, once it has been seen a moment (the children are quick)
function toucherPourPasser(b, apres = 900){
  const fin = new AbortController();
  const touche = new Promise(f => setTimeout(() => { if (!fin.signal.aborted) b.addEventListener("pointerdown", () => f(), {signal: fin.signal}); }, apres));
  return {touche, fini: () => fin.abort()};
}
// a message in the text box that stays until it has been said: at least `min` ms, at most `max` (or a touch on it);
// `qui`: the drawing of who speaks, before the sentence (the little one sees who it is)
async function messageCombat(texte, min = 1500, max = 5000, qui = null){
  const b = $("bulleCombat"), passer = toucherPourPasser(b);
  $("panneau").classList.add("cache");
  b.classList.remove("sage"); b.classList.toggle("parle", !!qui);
  if (qui) { const t = el("p"); ecrireIcones(t, affiche(texte)); b.replaceChildren(qui, t); }
  else { b.textContent = affiche(texte); iconiser(b); }
  b.hidden = false;
  await Promise.race([Promise.all([dire(texte), wait(min)]), wait(max), passer.touche]);
  passer.fini(); b.hidden = true; b.classList.remove("parle");
}
// the Sage's tip: his portrait (shown once it is there, 0.8 s at most), the sentence, and the companion he means (the
// little one sees whom to choose)
async function soufflerAstuce({texte, compagnon}){
  const b = $("bulleCombat");
  const sage = el("img", "portrait-sage"); sage.alt = ""; sage.src = `${DOSSIER_HD}/portraits/sage.png`;
  await Promise.race([sage.decode().catch(() => {}), wait(800)]);
  $("panneau").classList.add("cache");
  b.textContent = ""; b.classList.add("sage");
  const t = el("p"); ecrireIcones(t, affiche(texte));
  b.append(sage, t, spriteCompagnon(compagnon, 64)); b.hidden = false;
  const passer = toucherPourPasser(b);   // two sentences, said slowly to the little one: a touch goes on
  await Promise.race([Promise.all([dire(texte), wait(2400)]), wait(9500), passer.touche]);
  passer.fini(); b.hidden = true; b.classList.remove("sage"); b.textContent = "";
}
