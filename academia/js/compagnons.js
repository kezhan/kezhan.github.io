/* Académia : les familles de compagnons (une par matière, trois stades, noms du document de conception)
   et les régions du village. Créatures du pack Ninja Adventure (CC0) ; on libère un compagnon d'une Ombre. */
const FAMILLES = {
  matty:    {matiere: "maths", sprite: 18, noms: ["Matty", "Calculor", "Titan-Maths"], element: "géométrie",
             couleurs: ["#4FA3F7", "#2C6FC9", "#C9E8FF"], attaques: ["Coup de cube", "Pluie de nombres", "Tempête fractale"],
             desc: "Un petit golem cubique, joyeux et malin."},
  plumix:   {matiere: "anglais", sprite: 7, noms: ["Plumix", "Plumator", "Plumo-Céleste"], element: "vent",
             couleurs: ["#9B6BF2", "#6A3FC4", "#FFD45C"], attaques: ["Bourrasque", "Plumes filantes", "Vol céleste"],
             desc: "Un louveteau à plumes qui aime voyager."},
  barli:    {matiere: "allemand", sprite: 5, noms: ["Bärli", "Bärfurst", "Kaiserbär"], element: "pierre",
             couleurs: ["#B07A4A", "#7D5130", "#7EE0A0"], attaques: ["Patte de pierre", "Rune qui roule", "Avalanche"],
             desc: "Un ourson aux runes douces comme la pierre."},
  leiwchen: {matiere: "luxembourgeois", sprite: 13, noms: ["Léiwchen", "Léiwor", "Melusino"], element: "lumière",
             couleurs: ["#F7B733", "#E07A1F", "#EF3340"], attaques: ["Rugissement", "Crinière dorée", "Ailes de Mélusine"],
             desc: "Un lionceau doré, fier et gentil."},
  cavalin:  {matiere: "logique", sprite: 14, noms: ["Cavalin", "Taktik", "Roi-Tactik"], element: "stratégie",
             couleurs: ["#F4EBD9", "#2B2B3A", "#C9A227"], attaques: ["Saut en L", "Tour de garde", "Échec et mat"],
             desc: "Un petit poney-pion, rapide et espiègle."},
  poussik:  {matiere: "sciences", sprite: 12, noms: ["Poussik", "Floradrag", "Sylvanor"], element: "nature",
             couleurs: ["#5CCB6B", "#2F8F45", "#FF8FB8"], attaques: ["Graine éclair", "Souffle de feuilles", "Forêt éternelle"],
             desc: "Une pousse sautillante qui deviendra dragon des bois."}
};
// stages at levels 1, 15 and 30 (GDD §3)
const PALIERS = [1, 15, 30];
const stade = c => c.niveau >= PALIERS[2] ? 3 : c.niveau >= PALIERS[1] ? 2 : 1;
const nomCompagnon = c => FAMILLES[c.famille].noms[stade(c) - 1];
const spriteDe = c => FAMILLES[c.famille].sprite;
const echelleStade = s => [1, 1.3, 1.6][s - 1];   // a companion grows at each evolution
const pvMax = c => 24 + 4 * c.niveau;
const force = c => 6 + c.niveau;
// XP needed to go from level n to n+1: quick at first, slower later
const xpPour = n => 10 + 6 * n;

const REGIONS = [
  {id: "dojo", spriteBoss: 8, nom: "Dojo des Nombres", icone: "🥋", matiere: "maths", famille: "matty", desc: "Compter et calculer",
   boss: "Grand Vizir des Nombres", badge: "Badge de Pythagore"},
  {id: "albion", spriteBoss: 15, nom: "Port d'Albion", icone: "⛵", matiere: "anglais", famille: "plumix", desc: "Parler anglais",
   boss: "Roi du Quotidien", badge: "Badge de la Boussole"},
  {id: "germania", spriteBoss: 16, nom: "Montagne de Germania", icone: "⛰️", matiere: "allemand", famille: "barli", desc: "Deutsch sprechen",
   boss: "Dragon du Temps", badge: "Badge des Runes"},
  {id: "duche", spriteBoss: 2, nom: "Vallée du Grand-Duché", icone: "🏰", matiere: "luxembourgeois", famille: "leiwchen", desc: "Lëtzebuergesch schwätzen",
   boss: "Esprit des Traditions", badge: "Badge du Lion rouge"},
  {id: "jardin", spriteBoss: 17, nom: "Jardin des Éclats", icone: "🌸", matiere: "logique", famille: "cavalin", desc: "Formes, couleurs, suites",
   boss: "Gardien des Énigmes", badge: "Badge du Cristal"},
  {id: "observatoire", spriteBoss: 21, nom: "Observatoire", icone: "🔭", matiere: "sciences", famille: "poussik", desc: "Le monde qui nous entoure",
   boss: "Maître des Astres", badge: "Badge de l'Étoile"}
];
const ETAPES = 4;   // fights before the region boss
const regionDe = id => REGIONS.find(r => r.id === id);
// the Ombres: grumpy but never frightening for a 4-year-old
const OMBRES = ["Néantik", "Ombre-Chagrin", "Grisouille", "Brumichon", "Ronchonnet"];
const SPRITES_OMBRES = [1, 3, 11, 22];
function ombre(region, boss){
  const r = regionDe(region.id), st = regionEtat(region.id), k = st.niveau, a = actif();
  const base = a ? a.niveau : 1;
  if (boss) return {nom: r.boss, sprite: r.spriteBoss, niveau: base + 1, pvMax: 34 + 7 * base + 6 * k, force: 4 + Math.ceil(base * .7) + k, boss: true};
  const n = st.etape;
  return {nom: OMBRES[(n + k) % OMBRES.length], sprite: SPRITES_OMBRES[(n + k) % SPRITES_OMBRES.length], niveau: base,
          pvMax: 24 + 4 * base + 3 * n + 3 * k, force: 3 + Math.ceil(base * .5) + Math.floor(k / 2), boss: false};
}
// the region's own family hits harder there (GDD §4: weaknesses)
const superEfficace = (c, region) => c && FAMILLES[c.famille].matiere === regionDe(region.id).matiere;
