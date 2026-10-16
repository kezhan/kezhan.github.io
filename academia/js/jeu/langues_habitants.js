/* Académia : les habitants parlent leurs langues (Kezhan, 13/10/2026 : « les personnages aussi, ils peuvent parler
   d'autres langues »). Chacun a la sienne : le Sage le luxembourgeois (la langue du pays où vivent les enfants), Hugo
   l'anglais, Paco l'allemand. À chaque visite, il commence par un mot ou une petite phrase de sa langue, dite par sa
   voix enregistrée (l'anglais et l'allemand : donnees/voix_etrangeres.json ; le luxembourgeois : lod.lu), puis le sens
   en français, pour que la petite comprenne : « « Hello! » : c'est « bonjour », en anglais ! ». Le mot change d'une
   visite à l'autre. Un mot sans enregistrement n'est jamais pris (pas de voix approximative). Les mots sont ceux de la
   liste du jeu (js/mots_du_jeu.js), que les outils des voix relisent.
   Chaque visite du jour est notée dans P().histoire (visite_sage, visite_hugo, visite_paco : la mission « Dis bonjour »,
   js/missions.js). Juste après un éternuement du compagnon, l'habitant qui a dit « Gesundheit! » le redit quand on le
   touche, avec son sens (js/jeu/vie_village.js). */
const LANGUES_HABITANTS = {
  sage: {langue: "lb", nom: "luxembourgeois", mots: ["bonjour", "bonjour_matin", "merci", "au_revoir"]},
  hugo: {langue: "en", nom: "anglais", mots: ["bonjour", "comment_vas_tu", "bonne_journee", "bravo", "merci", "au_revoir"]},
  paco: {langue: "de", nom: "allemand", mots: ["bonjour", "comment_vas_tu", "bonne_journee", "bravo", "merci", "au_revoir"]}
};
const VISITES_HABITANTS = {};   // in this sitting: the next word of each villager
const MOT_EN_ATTENTE = {};      // a word just said by a villager (« Gesundheit! » after a sneeze): said again at the visit

// the words of a villager that have their recording here, as [word, meaning] (the Luxembourgish lexicon loads with
// the game)
function motsDits(L){
  return (typeof motsDuJeu === "function" ? motsDuJeu(L.mots, L.langue) : []).map(m => [m.mot, m.sens]);
}
// the first line of a villager: a word of their language and its meaning; null when they have none (or none recorded)
function salutHabitant(n){
  const L = LANGUES_HABITANTS[n.cle]; if (!L) return null;
  const attente = MOT_EN_ATTENTE[n.cle]; delete MOT_EN_ATTENTE[n.cle];
  let mot, sens;
  if (attente) ({mot, sens} = attente);
  else {
    const mots = motsDits(L); if (!mots.length) return null;
    const i = VISITES_HABITANTS[n.cle] = ((VISITES_HABITANTS[n.cle] ?? -1) + 1) % mots.length;
    [mot, sens] = mots[i];
  }
  if (typeof noterHistoire === "function") noterHistoire("visite_" + n.cle);   // the day's visit (the « Dis bonjour » mission)
  return {langue: L.langue, ligne: `« ${mot} » : c'est « ${sens} », en ${L.nom} !`};
}
