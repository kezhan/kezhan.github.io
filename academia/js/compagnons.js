/* Académia : les familles de compagnons (une par matière, trois stades, noms du document de conception)
   et les régions du village. Leurs dessins : assets/hd/creatures ; on libère un compagnon d'une Ombre. */
const FAMILLES = {
  matty:    {matiere: "maths", noms: ["Matty", "Calculor", "Titan-Maths"], element: "géométrie",
             couleurs: ["#4FA3F7", "#2C6FC9", "#C9E8FF"], attaques: ["Coup de cube", "Pluie de nombres", "Tempête fractale"],
             nouvelles: ["Bras géométriques", "Colosse d'énergie"],
             desc: "Un petit golem cubique, joyeux et malin."},
  plumix:   {matiere: "anglais", noms: ["Plumix", "Plumator", "Plumo-Céleste"], element: "vent",
             couleurs: ["#9B6BF2", "#6A3FC4", "#FFD45C"], attaques: ["Bourrasque", "Plumes filantes", "Vol céleste"],
             nouvelles: ["Glyphes lumineux", "Tornade de plumes"],
             desc: "Un louveteau à plumes qui aime voyager."},
  barli:    {matiere: "allemand", noms: ["Bärli", "Bärfurst", "Kaiserbär"], element: "pierre",
             couleurs: ["#B07A4A", "#7D5130", "#7EE0A0"], attaques: ["Patte de pierre", "Rune qui roule", "Avalanche"],
             nouvelles: ["Armure de runes", "Montagne qui gronde"],
             desc: "Un ourson aux runes douces comme la pierre."},
  leiwchen: {matiere: "luxembourgeois", noms: ["Léiwchen", "Léiwor", "Melusino"], element: "lumière",
             couleurs: ["#F7B733", "#E07A1F", "#EF3340"], attaques: ["Rugissement", "Crinière dorée", "Ailes de Mélusine"],
             nouvelles: ["Rugissement royal", "Soleil royal"],
             desc: "Un lionceau doré, fier et gentil."},
  cavalin:  {matiere: "logique", noms: ["Cavalin", "Taktik", "Roi-Tactik"], element: "stratégie",
             couleurs: ["#F4EBD9", "#2B2B3A", "#C9A227"], attaques: ["Saut en L", "Tour de garde", "Échec et mat"],
             nouvelles: ["Fourchette", "Grand roque"],
             desc: "Un petit poney-pion, rapide et espiègle."},
  poussik:  {matiere: "sciences", noms: ["Poussik", "Floradrag", "Sylvanor"], element: "nature",
             couleurs: ["#5CCB6B", "#2F8F45", "#FF8FB8"], attaques: ["Graine éclair", "Souffle de feuilles", "Forêt éternelle"],
             nouvelles: ["Feuilles lumineuses", "Racines géantes"],
             desc: "Une pousse sautillante qui deviendra dragon des bois."}
};
// stages at levels 1, 15 and 30 (GDD §3)
const PALIERS = [1, 15, 30];
const stade = c => c.niveau >= PALIERS[2] ? 3 : c.niveau >= PALIERS[1] ? 2 : 1;
const nomCompagnon = c => FAMILLES[c.famille].noms[stade(c) - 1];
const cleDe = c => c.famille + stade(c);   // the high-definition drawing of a companion at its stage (assets/hd/creatures)
// its three attacks: quick, massive (a new one is learnt at each evolution, GDD §9) and the ultimate combo
function attaquesDe(c, st = stade(c)){
  const F = FAMILLES[c.famille];
  return [F.attaques[0], st > 1 ? F.nouvelles[st - 2] : F.attaques[1], F.attaques[2]];
}
const pvMax = c => 24 + 4 * c.niveau;
const force = c => 6 + c.niveau;
// XP needed to go from level n to n+1: quick at first, slower later
const xpPour = n => 10 + 6 * n;

// the houses; `icone` is written in the texts and shown as the house's drawn symbol (js/icones.js, EMOJI_ICONES)
const REGIONS = [
  {id: "dojo", herbes: "à droite du chemin du Dojo", nom: "Dojo des Nombres", icone: "🥋", matiere: "maths", famille: "matty", desc: "Compter et calculer",
   boss: "Grand Vizir des Nombres", chef: "le Vizir", badge: "Badge de Pythagore"},
  {id: "albion", herbes: "sous le port, près de la mare", nom: "Port d'Albion", icone: "⛵", matiere: "anglais", famille: "plumix", desc: "Parler anglais",
   boss: "Roi du Quotidien", chef: "le Roi", badge: "Badge de la Boussole"},
  {id: "germania", herbes: "en bas, près des rochers", nom: "Montagne de Germania", icone: "⛰️", matiere: "allemand", famille: "barli", desc: "Deutsch sprechen",
   boss: "Dragon du Temps", chef: "le Dragon", badge: "Badge des Runes"},
  {id: "duche", herbes: "à droite de la maison rouge", nom: "Vallée du Grand-Duché", icone: "🏰", matiere: "luxembourgeois", famille: "leiwchen", desc: "Lëtzebuergesch schwätzen",
   boss: "Esprit des Traditions", chef: "l'Esprit", badge: "Badge du Lion rouge"},
  {id: "jardin", herbes: "à gauche du pavillon", nom: "Jardin des Éclats", icone: "🌸", matiere: "logique", famille: "cavalin", desc: "Formes, couleurs, suites",
   boss: "Gardien des Énigmes", chef: "le Gardien", badge: "Badge du Cristal"},
  {id: "observatoire", herbes: "à droite de l'observatoire", nom: "Observatoire", icone: "🔭", matiere: "sciences", famille: "poussik", desc: "Le monde qui nous entoure",
   boss: "Maître des Astres", chef: "le Maître", badge: "Badge de l'Étoile"}
];
const ETAPES = 4;   // fights before the region boss
const regionDe = id => REGIONS.find(r => r.id === id);
// the Ombres: grumpy but never frightening for a 4-year-old
const OMBRES = ["Néantik", "Ombre-Chagrin", "Grisouille", "Brumichon", "Ronchonnet"];
const CLES_OMBRES = {"Néantik": "neantik", "Ombre-Chagrin": "ombre", "Grisouille": "grisouille", "Brumichon": "brumichon", "Ronchonnet": "ronchonnet"};
const CLES_BOSS = {dojo: "vizir", albion: "roi", germania: "dragon", duche: "esprit", jardin: "gardien", observatoire: "maitre"};
// life counted in right answers, whatever the level, the attack or a lucky critical hit: an Ombre falls after 3 right
// answers (2 before 6 years), a boss after 5 (3 before 6). Each right answer takes off one `coup`, the companion's force
// (the number seen on screen); the Ombre's own blows still grow with the companion
const VIES = {ombre: [2, 3], boss: [3, 5]};
function ombre(region, boss){
  const r = regionDe(region.id), st = regionEtat(region.id), k = st.niveau, a = actif(), p = P();
  const base = a ? a.niveau : 1, coup = force(a || {niveau: 1}), petit = !!p && p.age <= 5;
  const coups = VIES[boss ? "boss" : "ombre"][petit ? 0 : 1];
  if (boss) return {nom: r.boss, cle: CLES_BOSS[r.id], niveau: base + 1, coups, coup, pvMax: coups * coup,
                    force: 3 + Math.ceil(base * .5) + k, boss: true};
  const n = st.etape + REGIONS.indexOf(r);   // each region has its own Ombre, and the next one changes
  const nom = OMBRES[(n + k) % OMBRES.length];
  return {nom, cle: CLES_OMBRES[nom], niveau: base, coups, coup, pvMax: coups * coup,
          force: 3 + Math.ceil(base * .5) + Math.floor(k / 2), boss: false};
}
// the region's own family hits with more sparkle there (GDD §4: weaknesses): an effect and XP, never a shorter fight
const superEfficace = (c, region) => c && FAMILLES[c.famille].matiere === regionDe(region.id).matiere;

// text shown on screen: a name never breaks at its hyphen (« Ombre- / Chagrin »: a word joiner after it), a number
// stays with its word, and « ! ? : ; » keep their French space without ever starting a line (a no-break space: the
// thin one is nearly invisible in Fredoka and Nunito). The voice reads the plain text (js/voix.js: spaces are spaces)
function affiche(t){
  return String(t == null ? "" : t)
    .replace(/(\p{L})-(\p{L})/gu, "$1-\u2060$2")
    .replace(/\b(niveau|Niv\.|sur)\s+(?=\d)/g, "$1\u00A0")
    .replace(/[ \u00A0\u202F]*([!?:;»])/g, (m, p, i) => i ? "\u00A0" + p : p)
    .replace(/«[ \u00A0\u202F]*/g, "«\u00A0");
}
