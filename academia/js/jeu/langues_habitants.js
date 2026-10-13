/* Académia : les habitants parlent leurs langues (Kezhan, 13/10/2026 : « les personnages aussi, ils peuvent parler
   d'autres langues »). Chacun a la sienne : le Sage le luxembourgeois (la langue du pays où vivent les enfants), Hugo
   l'anglais, Paco l'allemand. À chaque visite, il commence par un mot ou une petite phrase de sa langue, dite par sa
   voix enregistrée (l'anglais et l'allemand : donnees/voix_etrangeres.json ; le luxembourgeois : lod.lu), puis le sens
   en français, pour que la petite comprenne : « « Hello! » : c'est « bonjour », en anglais ! ». Le mot change d'une
   visite à l'autre. Un mot sans enregistrement n'est jamais pris (pas de voix approximative). */
const LANGUES_HABITANTS = {
  sage: {langue: "lb", nom: "luxembourgeois", mots: [["Moien", "bonjour"], ["Gudde Moien", "bonjour, le matin"], ["Merci", "merci"], ["Äddi", "au revoir"]]},
  hugo: {langue: "en", nom: "anglais", mots: [["Hello!", "bonjour"], ["How are you?", "comment vas-tu ?"], ["Have a nice day!", "bonne journée"],
    ["Well done!", "bravo"], ["Thank you!", "merci"], ["Goodbye!", "au revoir"]]},
  paco: {langue: "de", nom: "allemand", mots: [["Hallo!", "bonjour"], ["Wie geht's?", "comment vas-tu ?"], ["Einen schönen Tag!", "bonne journée"],
    ["Gut gemacht!", "bravo"], ["Danke!", "merci"], ["Tschüss!", "au revoir"]]}
};
const VISITES_HABITANTS = {};   // in this sitting: the next word of each villager

// the words of a villager that have their recording here (the Luxembourgish lexicon loads with the game)
function motsDits(L){
  return L.mots.filter(([mot]) => L.langue === "lb" ? typeof couvertLb === "function" && couvertLb(mot) : typeof aSaVoix === "function" && aSaVoix(mot, L.langue));
}
// the first line of a villager: a word of their language and its meaning; null when they have none (or none recorded)
function salutHabitant(n){
  const L = LANGUES_HABITANTS[n.cle]; if (!L) return null;
  const mots = motsDits(L); if (!mots.length) return null;
  const i = VISITES_HABITANTS[n.cle] = ((VISITES_HABITANTS[n.cle] ?? -1) + 1) % mots.length;
  const [mot, sens] = mots[i];
  return {langue: L.langue, ligne: `« ${mot} » : c'est « ${sens} », en ${L.nom} !`};
}
