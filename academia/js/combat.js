/* Académia : le combat au tour par tour (GDD §4). Chaque attaque est une question de la matière de la région :
   attaque rapide (révision, sûre), frappe massive (la notion du moment, gros dégâts), combo ultime (trois bonnes
   réponses de suite, en temps limité à partir de 6 ans), et au tour de l'Ombre, le bouclier Vrai/Faux pour esquiver.
   Un combat reste court : une erreur fait quand même un petit coup, et au bout de quelques questions le compagnon
   achève l'Ombre. Les plus petits n'ont ni bouclier ni fuite. */
const COMBO_PLEIN = 3;
const plafondQuestions = () => P().age <= 5 ? 5 : 7;

async function lancerCombat(regionId, boss){
  const region = regionDe(regionId), c = actif(), petit = P().age <= 5;
  const o = ombre(region, boss); o.pv = o.pvMax;
  c.pv = pvMax(c);                         // a companion always starts rested: no punishment for children
  const bilan = {region: regionId, boss, gagne: false, xp: 0, bonnes: 0, total: 0, revanches: 0};
  let charge = 0, toursOmbre = 0;
  montrer("combat");
  const moi = ++combatEnCours, abandon = () => moi !== combatEnCours;
  const vue = monterArene(c, o, region);
  vue.maj();
  const panneau = $("panneau"), cacher = () => panneau.classList.add("cache");
  panneau.innerHTML = ""; cacher();
  const intro = boss ? `${region.icone} ${o.nom} apparaît ! C'est le boss de la région !` : `${region.icone} ${region.nom} : une Ombre approche, ${o.nom} !`;
  dire(intro); await vue.bulle(intro, 1800);

  const tour = async mecanique => {
    let q = tirer(region.matiere, mecanique, boss);
    if (!q) { await chargerQuestions(); q = tirer(region.matiere, mecanique, boss); }
    if (!q) { toast("Pas de question pour cette matière pour l'instant"); return false; }
    bilan.total++;
    const titre = {rapide: "⚡ " + FAMILLES[c.famille].attaques[0], massive: "💥 " + FAMILLES[c.famille].attaques[1]}[mecanique] || "";
    const r = await poserQuestion(panneau, q, {titre: (q.revanche ? "🔁 Revanche ! " : "") + titre});
    cacher(); noter(q, r.juste);
    if (r.juste) { bilan.bonnes++; if (q.revanche) bilan.revanches++; }
    return {...r, revanche: q.revanche};
  };
  const frapper = async (mult, type = "") => {
    const crit = type === "critique";
    const d = Math.max(1, Math.round(force(c) * mult * (superEfficace(c, region) ? 1.5 : 1) * (crit ? 1.5 : 1)));
    await vue.anim("moi", "attaque"); sfx.coup(); if (crit) sfx.critique();
    o.pv -= d; vue.degats("lui", d, crit ? "critique" : ""); vue.anim("lui", "touche"); vue.maj();
    if (superEfficace(c, region) && !TEST && Math.random() < .35) vue.bulle("C'est super efficace !", 900);
    await wait(500);
  };

  while (c.pv > 0 && o.pv > 0) {
    if (bilan.total >= plafondQuestions()) {   // enough questions: the companion finishes the Ombre
      await vue.bulle(`✨ ${nomCompagnon(c)} donne le coup final !`, 1200);
      await vue.anim("moi", "attaque"); sfx.critique();
      vue.degats("lui", Math.max(1, o.pv), "critique"); o.pv = 0; vue.anim("lui", "touche"); vue.maj();
      await wait(500); break;
    }
    const choix = await choisirAttaque(panneau, c, charge);
    if (abandon()) return;
    if (choix === "fuite") {
      const t = "On rentre au village. L'Ombre t'attend encore dans les herbes !";
      dire(t); await vue.bulle(t, 1600);
      if (abandon()) return;
      taire(); return retourMonde({gagne: false});
    }
    if (choix === "combo") {
      charge = 0;
      const limite = P().age >= 6 ? 14000 : 0;
      let justes = 0;
      for (let k = 0; k < 3 && o.pv > 0; k++) {
        const q = tirer(region.matiere, "rapide", boss); if (!q) break;
        bilan.total++;
        const r = await poserQuestion(panneau, q, {titre: `🌟 Combo ${k + 1}/3`, limite, jokers: false});
        if (abandon()) return;
        noter(q, r.juste);
        if (!r.juste) break;
        justes++; bilan.bonnes++; bilan.xp += 8;
      }
      cacher();
      if (justes === 3) { await vue.bulle("🌟 " + FAMILLES[c.famille].attaques[2] + " !", 1200); await frapper(5, "critique"); bilan.xp += 25; }
      else await frapper(Math.max(.5, justes));
    } else {
      const r = await tour(choix);
      if (abandon()) return;
      if (!r) return retourMonde();   // no question for this subject yet: back to the village, not a defeat
      if (r.juste) {
        const mult = choix === "massive" ? 2.3 : 1.4;
        // a fast right answer from a reader is a critical hit
        await frapper(mult, P().age >= 6 && r.ms < 6000 && choix === "massive" ? "critique" : "");
        bilan.xp += (choix === "massive" ? 14 : 6) * (r.revanche ? 3 : 1);
        charge = Math.min(COMBO_PLEIN, charge + 1);
      } else {   // a mistake: the right answer was shown, and the fight still moves on with a small blow
        charge = 0; await frapper(.5);
      }
    }
    if (o.pv <= 0) break;
    // the Ombre's turn: every other round for the little ones, with no question; the True/False shield every other turn from 6
    toursOmbre++;
    if (petit && toursOmbre % 2) continue;
    await vue.bulle(`${o.nom} prépare une attaque !`, 900);
    const q = !petit && toursOmbre % 2 ? tirer(region.matiere, "bouclier", false) : null;
    if (q) {
      bilan.total++;
      const r = await poserQuestion(panneau, q, {titre: estVraiFaux(q) ? "🛡️ Bouclier ! Vrai ou faux ?" : "🛡️ Bouclier !", jokers: false});
      if (abandon()) return;
      cacher();
      noter(q, r.juste);
      if (r.juste) { bilan.bonnes++; bilan.xp += 4; sfx.bouclier(); vue.degats("moi", 0, "esquive"); await wait(700); continue; }
    }
    await vue.anim("lui", "attaque"); sfx.coup();
    c.pv -= o.force; vue.degats("moi", o.force); vue.anim("moi", "touche"); vue.maj();
    await wait(600);
  }
  if (abandon()) return;
  bilan.gagne = o.pv <= 0;
  await vue.anim(bilan.gagne ? "lui" : "moi", "ko");
  if (bilan.gagne) { sfx.victoire(); vue.anim("moi", "joie"); }
  await wait(700);
  if (abandon()) return;
  return terminerCombat(bilan);
}

// the attack buttons; the combo appears once three right answers are charged; leaving asks twice, and only from 6
function choisirAttaque(panneau, c, charge){
  return new Promise(fin => {
    const f = FAMILLES[c.famille];
    panneau.innerHTML = ""; panneau.classList.remove("cache");
    const g = el("div", "attaques");
    let choisi = false;
    const bouton = (cls, ico, nom, aide, val) => {
      const b = el("button", "attaque " + cls, `<span class="ico">${ico}</span><span><b></b><small>${aide}</small></span>`);
      b.querySelector("b").textContent = nom;
      b.onclick = () => { if (choisi) return; choisi = true; sfx.tap(); dire(nom); fin(val); };
      g.append(b); return b;
    };
    markOk(bouton("rapide", "⚡", f.attaques[0], "Une question facile", "rapide"));
    bouton("massive", "💥", f.attaques[1], "Plus dur, gros dégâts !", "massive");
    const combo = bouton("combo", "🌟", f.attaques[2], `Trois bonnes réponses de suite`, "combo");
    const j = el("div", "jauge-combo", `<i style="width:${100 * charge / COMBO_PLEIN}%"></i>`);
    combo.querySelector("span:last-child").append(j);
    combo.disabled = charge < COMBO_PLEIN;
    panneau.append(g);
    if (P().age < 6) return;
    const fuite = el("button", "joker", "🏃 S'enfuir"); fuite.style.marginTop = "10px";
    fuite.onclick = () => {
      sfx.tap();
      if (fuite.dataset.sur) { if (!choisi) { choisi = true; fin("fuite"); } return; }
      fuite.dataset.sur = "1"; fuite.textContent = "🏃 Partir ? Touche encore"; dire("Tu veux partir ? Touche encore.");
      setTimeout(() => { delete fuite.dataset.sur; fuite.textContent = "🏃 S'enfuir"; }, 3000);
    };
    const l = el("div", "jokers"); l.append(fuite); panneau.append(l);
  });
}
