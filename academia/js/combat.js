/* Académia : le combat au tour par tour (GDD §4). Chaque attaque est une question de la matière de la région :
   attaque rapide (révision, sûre), frappe massive (la notion du moment, gros dégâts), combo ultime (trois bonnes
   réponses de suite, en temps limité à partir de 6 ans), et au tour de l'Ombre, le bouclier Vrai/Faux pour esquiver. */
const COMBO_PLEIN = 3;

async function lancerCombat(regionId, boss){
  const region = regionDe(regionId), c = actif();
  const o = ombre(region, boss); o.pv = o.pvMax;
  c.pv = pvMax(c);                         // a companion always starts rested: no punishment for children
  const bilan = {region: regionId, boss, gagne: false, xp: 0, bonnes: 0, total: 0, revanches: 0};
  let charge = 0;
  montrer("combat");
  const moi = ++combatEnCours, abandon = () => moi !== combatEnCours;
  const vue = monterArene(c, o, region);
  vue.maj();
  const panneau = $("panneau"), cacher = () => panneau.classList.add("cache");
  panneau.innerHTML = ""; cacher();
  const intro = boss ? `${o.nom} apparaît ! C'est le boss de la région !` : `Une Ombre approche : ${o.nom} !`;
  dire(intro); await vue.bulle(intro, 1800);

  const tour = async mecanique => {
    const q = tirer(region.matiere, mecanique, boss);
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
    let d = Math.round(force(c) * mult * (superEfficace(c, region) ? 1.5 : 1) * (crit ? 1.5 : 1));
    await vue.anim("moi", "attaque"); sfx.coup(); if (crit) sfx.critique();
    o.pv -= d; vue.degats("lui", d, crit ? "critique" : ""); vue.anim("lui", "touche"); vue.maj();
    if (superEfficace(c, region) && !TEST && Math.random() < .35) vue.bulle("C'est super efficace !", 900);
    await wait(500);
  };

  while (c.pv > 0 && o.pv > 0) {
    const choix = await choisirAttaque(panneau, c, charge);
    if (abandon()) return;
    if (choix === "fuite") { taire(); return retourMonde(); }
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
      else if (justes) await frapper(justes);
      else { vue.degats("lui", 0, "rate"); await wait(600); }
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
      } else {
        vue.degats("lui", 0, "rate"); charge = 0; await wait(500);
      }
    }
    if (o.pv <= 0) break;
    // the Ombre's turn: the True/False shield (GDD §4)
    await vue.bulle(`${o.nom} prépare une attaque !`, 900);
    const q = tirer(region.matiere, "bouclier", false);
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
  return terminerCombat(bilan);
}

// the attack buttons; the combo appears once three right answers are charged
function choisirAttaque(panneau, c, charge){
  return new Promise(fin => {
    const f = FAMILLES[c.famille];
    panneau.innerHTML = ""; panneau.classList.remove("cache");
    const g = el("div", "attaques");
    const bouton = (cls, ico, nom, aide, val) => {
      const b = el("button", "attaque " + cls, `<span class="ico">${ico}</span><span><b></b><small>${aide}</small></span>`);
      b.querySelector("b").textContent = nom;
      b.onclick = () => { sfx.tap(); dire(nom); fin(val); };
      g.append(b); return b;
    };
    markOk(bouton("rapide", "⚡", f.attaques[0], "Une question facile", "rapide"));
    bouton("massive", "💥", f.attaques[1], "Plus dur, gros dégâts !", "massive");
    const combo = bouton("combo", "🌟", f.attaques[2], `Trois bonnes réponses de suite`, "combo");
    const j = el("div", "jauge-combo", `<i style="width:${100 * charge / COMBO_PLEIN}%"></i>`);
    combo.querySelector("span:last-child").append(j);
    combo.disabled = charge < COMBO_PLEIN;
    panneau.append(g);
    const fuite = el("button", "joker", "🏃 S'enfuir"); fuite.style.marginTop = "10px";
    fuite.onclick = () => fin("fuite");
    const l = el("div", "jokers"); l.append(fuite); panneau.append(l);
  });
}
