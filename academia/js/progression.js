/* Académia : ce que rapporte un combat (XP, étincelles, jokers, badge et ses étoiles), les niveaux, les évolutions
   aux paliers, et le compagnon libéré quand le boss d'une région tombe. Puis la fin, dans l'ordre : le badge qui
   tombe et le compagnon libéré (js/liberation.js), la carte du bilan (js/bilan.js), l'évolution (js/evolution.js),
   et le retour au village par l'étoile (js/transition.js). Chaque grand moment a sa scène : aucun toucher ne le saute. */
const JOKER_MAX = 5;   // a joker no longer piles up past five
const NIVEAU_LIBERE = 5;   // a freed companion starts low and lives both its evolutions in front of the child

function gagnerXP(c, xp){
  const avant = {niveau: c.niveau, stade: stade(c)};
  c.xp += xp;
  while (c.xp >= xpPour(c.niveau)) { c.xp -= xpPour(c.niveau); c.niveau++; }
  return {niveaux: c.niveau - avant.niveau, evolue: stade(c) > avant.stade, stadeAvant: avant.stade};
}
// a companion well behind the strongest of the team learns twice as fast (a freed one catches up on its first fights)
function rattrapage(c){
  const max = Math.max(...P().compagnons.map(x => x.niveau));
  return c.niveau <= max - 4 ? 2 : 1;
}
// a boss badge: one to three stars by the share of right answers (always three before 6 years)
function etoilesDe(b){
  if (P().age <= 5) return 3;
  const part = b.total ? b.bonnes / b.total : 0;
  return part >= .8 ? 3 : part >= .5 ? 2 : 1;
}

async function terminerCombat(b){
  const p = P(), c = actif(), region = regionDe(b.region), st = regionEtat(b.region);
  const fin = {libere: null, badge: null, etoiles: 0, etoilesAvant: st.etoiles || 0, joker: null, cadeau: false};
  const plus = k => { if ((p.jokers[k] || 0) < JOKER_MAX) { p.jokers[k] = (p.jokers[k] || 0) + 1; return true; } return false; };
  if (b.gagne) {
    b.xp += 10 + 4 * st.niveau + (b.boss ? 20 : 0);
    p.etincelles += 2 * b.bonnes + (b.boss ? 20 : 5) + (b.etincelles || 0);
    if (b.boss) {
      st.badges++; st.niveau++; st.etape = 0; fin.badge = region.badge;
      fin.etoiles = etoilesDe(b); st.etoiles = Math.max(fin.etoilesAvant, fin.etoiles);
      fin.libere = ajouterCompagnon(region.famille, NIVEAU_LIBERE);   // the region's companion joins the team
      if (!fin.libere) { fin.cadeau = plus("potion") | plus("indice"); }   // already in the team: presents instead
    } else st.etape++;
    if (!fin.cadeau && Math.random() < .45) {   // sometimes a joker; the Hourglass only serves the timed combo (from 6)
      const k = pick(["potion", "indice", ...(p.age >= 6 ? ["sablier"] : [])].filter(j => (p.jokers[j] || 0) < JOKER_MAX), 1)[0];
      if (k && plus(k)) fin.joker = k;
    }
  } else if (b.fuite) b.xp = Math.max(4, Math.round(b.xp * .5));   // the Ombre ran away: a little XP, no door step, no joker
  else b.xp = Math.round(b.xp * .6);
  fin.rattrapage = rattrapage(c);
  b.xp *= fin.rattrapage;
  const xpAvant = {niveau: c.niveau, xp: c.xp};
  const gain = gagnerXP(c, b.xp);
  noterCombat({region: b.region, boss: b.boss, gagne: b.gagne, fuite: !!b.fuite, xp: b.xp, bonnes: b.bonnes, total: b.total,
    revanches: b.revanches, compagnon: c.famille, niveau: c.niveau, etoiles: fin.etoiles || undefined});
  sauver();
  prechargerMoments([fin.libere && cleDe(fin.libere), gain.evolue && cleDe(c), b.boss && CLES_BOSS[region.id]].filter(Boolean),
    fin.badge ? ["badge_" + region.id, "etoile"] : []);
  // the big moments first, each one its own scene; then the card; then the evolution
  if (fin.badge) await sceneBadge(region, fin.etoiles, fin.etoilesAvant, !!fin.libere);
  if (fin.libere) await sceneLiberation(fin.libere, region);
  await carteBilan(b, c, region, gain, xpAvant, fin);
  if (gain.evolue) await sceneEvolution(c, gain.stadeAvant);
  await fermerEtoile();
  taire(); retourMonde({gagne: b.gagne});
  ouvrirEtoile();
}
