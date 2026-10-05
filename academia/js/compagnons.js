/* Académia : les familles de compagnons (une par matière, trois stades, noms du document de conception)
   et les régions du village. Leurs dessins : assets/hd/creatures ; on libère un compagnon d'une Ombre. */
const FAMILLES = {
  matty:    {matiere: "maths", noms: ["Matty", "Calculor", "Titan-Maths"], element: "géométrie",
             couleurs: ["#4FA3F7", "#2C6FC9", "#C9E8FF"], attaques: ["Coup de cube", "Pluie de nombres", "Tempête fractale"],
             desc: "Un petit golem cubique, joyeux et malin."},
  plumix:   {matiere: "anglais", noms: ["Plumix", "Plumator", "Plumo-Céleste"], element: "vent",
             couleurs: ["#9B6BF2", "#6A3FC4", "#FFD45C"], attaques: ["Bourrasque", "Plumes filantes", "Vol céleste"],
             desc: "Un louveteau à plumes qui aime voyager."},
  barli:    {matiere: "allemand", noms: ["Bärli", "Bärfurst", "Kaiserbär"], element: "pierre",
             couleurs: ["#B07A4A", "#7D5130", "#7EE0A0"], attaques: ["Patte de pierre", "Rune qui roule", "Avalanche"],
             desc: "Un ourson aux runes douces comme la pierre."},
  leiwchen: {matiere: "luxembourgeois", noms: ["Léiwchen", "Léiwor", "Melusino"], element: "lumière",
             couleurs: ["#F7B733", "#E07A1F", "#EF3340"], attaques: ["Rugissement", "Crinière dorée", "Ailes de Mélusine"],
             desc: "Un lionceau doré, fier et gentil."},
  cavalin:  {matiere: "logique", noms: ["Cavalin", "Taktik", "Roi-Tactik"], element: "stratégie",
             couleurs: ["#F4EBD9", "#2B2B3A", "#C9A227"], attaques: ["Saut en L", "Tour de garde", "Échec et mat"],
             desc: "Un petit poney-pion, rapide et espiègle."},
  poussik:  {matiere: "sciences", noms: ["Poussik", "Floradrag", "Sylvanor"], element: "nature",
             couleurs: ["#5CCB6B", "#2F8F45", "#FF8FB8"], attaques: ["Graine éclair", "Souffle de feuilles", "Forêt éternelle"],
             desc: "Une pousse sautillante qui deviendra dragon des bois."}
};
// stages at levels 1, 15 and 30 (GDD §3)
const PALIERS = [1, 15, 30];
const stade = c => c.niveau >= PALIERS[2] ? 3 : c.niveau >= PALIERS[1] ? 2 : 1;
const nomCompagnon = c => FAMILLES[c.famille].noms[stade(c) - 1];
const cleDe = c => c.famille + stade(c);   // the high-definition drawing of a companion at its stage (assets/hd/creatures)
const pvMax = c => 24 + 4 * c.niveau;
const force = c => 6 + c.niveau;
// XP needed to go from level n to n+1: quick at first, slower later
const xpPour = n => 10 + 6 * n;

const REGIONS = [
  {id: "dojo", herbes: "à droite du chemin du Dojo", nom: "Dojo des Nombres", icone: "🥋", matiere: "maths", famille: "matty", desc: "Compter et calculer",
   boss: "Grand Vizir des Nombres", badge: "Badge de Pythagore"},
  {id: "albion", herbes: "sous le port, près de la mare", nom: "Port d'Albion", icone: "⛵", matiere: "anglais", famille: "plumix", desc: "Parler anglais",
   boss: "Roi du Quotidien", badge: "Badge de la Boussole"},
  {id: "germania", herbes: "en bas, près des rochers", nom: "Montagne de Germania", icone: "⛰️", matiere: "allemand", famille: "barli", desc: "Deutsch sprechen",
   boss: "Dragon du Temps", badge: "Badge des Runes"},
  {id: "duche", herbes: "à droite de la maison rouge", nom: "Vallée du Grand-Duché", icone: "🏰", matiere: "luxembourgeois", famille: "leiwchen", desc: "Lëtzebuergesch schwätzen",
   boss: "Esprit des Traditions", badge: "Badge du Lion rouge"},
  {id: "jardin", herbes: "à gauche du pavillon", nom: "Jardin des Éclats", icone: "🌸", matiere: "logique", famille: "cavalin", desc: "Formes, couleurs, suites",
   boss: "Gardien des Énigmes", badge: "Badge du Cristal"},
  {id: "observatoire", herbes: "à droite de l'observatoire", nom: "Observatoire", icone: "🔭", matiere: "sciences", famille: "poussik", desc: "Le monde qui nous entoure",
   boss: "Maître des Astres", badge: "Badge de l'Étoile"}
];
const ETAPES = 4;   // fights before the region boss
const regionDe = id => REGIONS.find(r => r.id === id);
// the Ombres: grumpy but never frightening for a 4-year-old
const OMBRES = ["Néantik", "Ombre-Chagrin", "Grisouille", "Brumichon", "Ronchonnet"];
const CLES_OMBRES = {"Néantik": "neantik", "Ombre-Chagrin": "ombre", "Grisouille": "grisouille", "Brumichon": "brumichon", "Ronchonnet": "ronchonnet"};
const CLES_BOSS = {dojo: "vizir", albion: "roi", germania: "dragon", duche: "esprit", jardin: "gardien", observatoire: "maitre"};
// life counted in quick attacks of the companion, not in levels, so that a fight stays short at every level:
// an Ombre falls in 2 quick attacks before 6 years, 3 after; a boss in 3, then 4
function ombre(region, boss){
  const r = regionDe(region.id), st = regionEtat(region.id), k = st.niveau, a = actif(), p = P();
  const base = a ? a.niveau : 1, coup = Math.round((6 + base) * 1.4), petit = !!p && p.age <= 5;
  if (boss) return {nom: r.boss, cle: CLES_BOSS[r.id], niveau: base + 1, pvMax: Math.round(coup * (petit ? 2.6 : 3.6)), force: 4 + Math.ceil(base * .7) + k, boss: true};
  const n = st.etape + REGIONS.indexOf(r);   // each region has its own Ombre, and the next one changes
  const nom = OMBRES[(n + k) % OMBRES.length];
  return {nom, cle: CLES_OMBRES[nom], niveau: base,
          pvMax: Math.round(coup * (petit ? 1.8 : 2.6)), force: 3 + Math.ceil(base * .5) + Math.floor(k / 2), boss: false};
}
// the region's own family hits harder there (GDD §4: weaknesses)
const superEfficace = (c, region) => c && FAMILLES[c.famille].matiere === regionDe(region.id).matiere;
