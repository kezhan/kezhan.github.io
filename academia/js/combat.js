/* Académia : le combat au tour par tour (GDD §4). Chaque attaque est une question de la matière de la région :
   attaque rapide (révision, sûre), frappe massive (la notion du moment ; chez le boss, le défi du chef), combo ultime
   (trois bonnes réponses de suite, en temps limité à partir de 6 ans), et au tour de l'Ombre, le bouclier Vrai/Faux.
   La vie de l'Ombre se compte en bonnes réponses (js/compagnons.js, ombre()) : une erreur ne la touche pas, un coup
   critique brille sans raccourcir le combat. La charge du combo vit sur le compagnon (c.charge, sauvegardée) : chaque
   bonne réponse la remplit, une erreur la vide, d'un combat à l'autre. Au bout de quelques questions, le compagnon
   achève l'Ombre ; à partir de 6 ans, sans une seule bonne réponse, elle s'enfuit dans les herbes. */
const COMBO_PLEIN = 3;
// questions before the end (shields and combo questions count): the companion then finishes the fight
const plafondQuestions = boss => P().age <= 5 ? (boss ? 7 : 5) : (boss ? 12 : 7);
const majuscule = t => t.charAt(0).toUpperCase() + t.slice(1);

async function lancerCombat(regionId, boss){
  const region = regionDe(regionId), c = actif(), p = P(), petit = p.age <= 5;
  const o = ombre(region, boss); o.pv = o.pvMax;
  c.pv = pvMax(c);                         // a companion always starts rested: no punishment for children
  c.charge = Math.min(COMBO_PLEIN, c.charge || 0);
  const bilan = {region: regionId, boss, gagne: false, fuite: false, xp: 0, bonnes: 0, total: 0, revanches: 0, defis: 0, etincelles: 0};
  let toursOmbre = 0, tours = 0, phase2 = false, superDit = false, apresErreur = false;
  montrer("combat");
  const moi = ++combatEnCours, abandon = () => moi !== combatEnCours;
  if (typeof jouerAmbiance === "function") jouerAmbiance(boss ? "boss" : "combat");
  const vue = monterArene(c, o, region);
  // a message in the text box, its emojis drawn (js/icones.js)
  const bulle = (texte, ms) => { const fin = vue.bulle(texte, ms); iconiser($("bulleCombat")); return fin; };
  vue.maj();
  const panneau = $("panneau"), cacher = () => panneau.classList.add("cache");
  panneau.innerHTML = ""; cacher();
  await vue.entree();                      // the arena opens; a boss falls onto its stand under its banner
  if (abandon()) return;
  const intro = boss ? `${region.icone} ${o.nom} apparaît ! C'est le chef de la maison !` : `${region.icone} ${region.nom} : une Ombre approche, ${o.nom} !`;
  dire(intro); await bulle(intro, 1800);
  if (abandon()) return;
  if (c.charge >= COMBO_PLEIN) {           // the combo charged in an earlier fight is ready at once
    const t = `🌟 ${nomCompagnon(c)} peut lancer ${attaquesDe(c)[2]} !`;
    dire(t); await bulle(t, 1500);
    if (abandon()) return;
  }

  // one question; after a mistake, the Sage shows the potion the first time (js/astuces.js)
  const poser = async (q, titre, opts = {}) => {
    bilan.total++;
    panneau.classList.remove("choix");
    const attente = poserQuestion(panneau, q, {titre: affiche(titre), ...opts});
    const astuce = apresErreur && opts.jokers !== false ? astucePotion(panneau) : null;
    const r = await attente;
    if (astuce) astuce.fermer();
    if (opts.jokers !== false) apresErreur = false;   // the potion was offered: the next mistake shows it again
    if (!r.juste) apresErreur = true;
    return r;
  };
  const charger = juste => { c.charge = juste ? Math.min(COMBO_PLEIN, (c.charge || 0) + 1) : 0; };
  const tirage = async mecanique => {
    let q = tirer(region.matiere, mecanique, boss);
    if (!q) { await chargerQuestions(); q = tirer(region.matiere, mecanique, boss); }
    return q;
  };
  // the companion's blow: n right answers' worth (never more than what is left)
  const frapper = async (n, type = "", ultime = false) => {
    const d = Math.min(o.pv, n * o.coup), crit = type === "critique", sup = superEfficace(c, region);
    sfx.coup(); if (crit) sfx.critique();
    if (typeof jouerSon === "function") jouerSon(crit ? "critique" : "coup");
    await vue.frappe({degats: d, type, ultime});
    o.pv -= d; vue.maj();
    if (crit) { bilan.xp += 4; bilan.etincelles += 2; }
    if (sup) { bilan.xp += 3; bilan.etincelles += 1; }
    if (sup && !superDit && o.pv > 0) { superDit = true; await bulle("C'est super efficace !", 900); }
    else if (crit && !ultime && o.pv > 0) await bulle("Coup critique !", 800);
    if (boss && !phase2 && o.pv > 0 && o.pv <= Math.ceil(o.coups / 2) * o.coup) {   // half its life gone: the boss gets cross
      phase2 = true;
      const t = `${majuscule(region.chef)} se fâche ! Courage, ${p.nom} !`;
      dire(t); await vue.phase2(); await bulle(t, 1500);
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
      dire(t); await bulle(t, 1200);
      await frapper(o.coups, "critique"); o.pv = 0; vue.maj();
      break;
    }
    const choix = await choisirAttaque(panneau, c, {boss, premier: tours++ === 0});
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
        justes++; bilan.bonnes++; bilan.xp += 8 * (q.revanche ? 3 : 1); if (q.revanche) bilan.revanches++;
      }
      cacher();
      if (justes === 3) {                  // the ultimate attack: a fatal blow for an Ombre, three right answers for a boss
        await vue.ultime(A[2]);
        await frapper(boss ? 3 : o.coups, "critique", true); bilan.xp += 25;
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
      cacher(); noter(q, r.juste); charger(r.juste);
      if (r.juste) {
        bilan.bonnes++; if (q.revanche) bilan.revanches++; if (q.defi) bilan.defis++;
        bilan.xp += (q.defi ? 20 : choix.action === "massive" ? 14 : 6) * (q.revanche ? 3 : 1);
        // a quick right answer is a critical hit: the massive from 6, any attack before (an effect, never a shorter fight)
        const crit = petit ? r.ms < 5000 : choix.action === "massive" && r.ms < 6000;
        await frapper(1, crit ? "critique" : choix.action === "massive" || q.defi ? "fort" : "");
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
      cacher(); noter(q, r.juste); charger(r.juste);
      if (r.juste) { bilan.bonnes++; bilan.xp += 4; sfx.bouclier(); await vue.esquive(); continue; }
    }
    sfx.coup(); if (typeof jouerSon === "function") jouerSon("coup");
    await vue.coupOmbre(o.force);
    c.pv -= o.force; vue.maj();
    await wait(500);
  }
  if (abandon()) return;
  bilan.gagne = o.pv <= 0;
  if (bilan.gagne) {
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
