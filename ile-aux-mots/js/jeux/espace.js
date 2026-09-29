/* L'Île aux Mots : jeu « L'espace » (catégorie Le monde).
   The Sun, the Moon, the Earth and the eight planets drawn in SVG: true colours, Saturn's big rings, Uranus's thin ring standing upright
   (the planet lies on its side), Jupiter's bands and Great Red Spot, Mars's white polar cap, sizes in the right order.
   Everything the child sees or hears is in English, German, Luxembourgish or Chinese, never French.
   1: sun, moon, star, rocket, astronaut: touch the picture that is said (3 pictures; lexicon words, so Luxembourgish plays the lod.lu recordings)
   2: the planets by their colour and shape: the red one, the big rings, the biggest, the smallest, our home, the dark blue one, not a planet, around the Earth
   3: the order of the planets from the Sun: the child builds the solar system one planet at a time (names written under the pictures)
   4: short sentences about the sky, true or false; the big one reads them alone (🔊 reads them, counted as a hint); what is true instead is shown after
   Facts: 8 planets (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune); Jupiter the biggest, Mercury the smallest and the closest, Neptune the farthest;
   Venus hotter than Mercury although farther from the Sun; Jupiter, Uranus and Neptune have faint rings too; the Earth turns once a day and goes round the Sun in a year;
   the Moon is about a quarter as wide as the Earth; nobody lives on Mars; no air in space.
   Luxembourgish checked on lod.lu (entries PLANEIT1, AERD1, MERKUR1…NEPTUN1, RANK1 plural Réng, DONKELBLO1, NO2, LOFT1, WELTALL1, SAIT1, ACHS1, OBWUEL1, KAUM1):
   Planéit m. (pl. Planéiten), d'Äerd, de Merkur, d'Venus, de Mars, de Jupiter, de Saturn, den Uranus, den Neptun; lod.lu example sentences reused:
   "de Merkur ass de klengste Planéit", "de Jupiter ass de gréisste…", "de Saturn erkennt een u senge Réng", "den Neptun ass dee Planéit, deen am wäitste vun der Sonn ewech ass",
   "de Mound dréit ëm d'Äerd an d'Äerd dréit ëm d'Sonn", "d'Äerdkugel dréit sech ëm hir eegen Achs", "um Mound", "ootmen".
   To have proofread in Luxembourgish: "am nooste bei der Sonn", "méi no bei der Sonn wéi d'Äerd", "Wat ass hei kee Planéit?", "nom Merkur / no der Venus",
   "mee een gesäit se kaum", "Dofir droen d'Astronauten en Helm", "Déi aacht Planéite sinn all do!", "D'Äerd dréit an engem Joer eemol ëm d'Sonn",
   "obwuel si méi wäit vun der Sonn ewech ass", "Um Mars wunne Leit". */
(() => {
const ID = "espace";
const LG = () => ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
const calm = () => fx.calm();
let nid = 0;
const cid = () => `esp${++nid}x${rnd(1e6)}`; // clip paths need an id of their own in every picture

/* ---------- the pictures: a 100 x 100 box, details of a planet drawn in a unit circle scaled to its radius ---------- */
function disc(R, base, inner){
  const id = cid();
  return `<clipPath id="${id}"><circle cx="50" cy="50" r="${R}"/></clipPath><circle cx="50" cy="50" r="${R}" fill="${base}"/>
    <g clip-path="url(#${id})"><g transform="translate(50 50) scale(${R})">${inner}
    <circle cx=".6" cy=".6" r="1.05" fill="#000" opacity=".2"/><circle cx="-.4" cy="-.42" r=".42" fill="#fff" opacity=".16"/></g></g>`;
}
const band = (y0, y1, c, o = 1) => `<rect x="-1.2" y="${y0}" width="2.4" height="${(y1 - y0).toFixed(2)}" fill="${c}" opacity="${o}"/>`;
const blob = (x, y, rx, ry, c, o = 1, rot = 0) => `<ellipse cx="${x}" cy="${y}" rx="${rx}" ry="${ry}" fill="${c}" opacity="${o}" transform="rotate(${rot} ${x} ${y})"/>`;
const INK = `stroke="#1B2D45" stroke-linejoin="round"`;
const ART = {
  // radii in the right order: Mercury < Mars < Venus < Earth < Neptune < Uranus < Saturn < Jupiter
  mercury: () => disc(13, "#A9A9A9", blob(-.4, -.2, .24, .22, "#7E7E7E") + blob(.3, .35, .2, .18, "#858585") + blob(.35, -.45, .13, .12, "#8C8C8C") + blob(-.1, .55, .12, .1, "#7E7E7E") + blob(-.55, .35, .1, .09, "#C4C4C4")),
  venus: () => disc(22, "#E8CB82", blob(0, -.38, 1.3, .16, "#F4E1AE", 1, -10) + blob(0, .05, 1.3, .13, "#D8B86A", 1, -8) + blob(0, .45, 1.3, .15, "#F1DCA2", 1, -12)),
  earth: () => disc(23, "#2F7FD8",
    `<path d="M-.62 -.72 C-.22 -.84 -.02 -.58 -.2 -.36 C-.36 -.16 -.1 0 -.16 .22 C-.22 .46 -.36 .7 -.46 .88 C-.62 .52 -.8 .12 -.86 -.2 C-.84 -.46 -.76 -.62 -.62 -.72 Z" fill="#4CAF50"/>
     <path d="M.18 -.66 C.46 -.76 .74 -.54 .7 -.28 C.8 0 .62 .3 .42 .58 C.3 .32 .14 .12 .2 -.08 C.04 -.24 .04 -.52 .18 -.66 Z" fill="#5DBB63"/>`
    + blob(.45, -.1, .12, .08, "#C9A86A", .9) + blob(0, -.97, .6, .2, "#fff", .95) + blob(0, .98, .55, .18, "#fff", .95)
    + blob(-.25, -.1, .38, .06, "#fff", .8, -15) + blob(.4, .35, .3, .05, "#fff", .75, 10)),
  mars: () => disc(16, "#C9532C", blob(-.3, .1, .35, .2, "#9E3B1F", .9, 20) + blob(.35, .4, .25, .14, "#A5421F", .9) + blob(.3, -.3, .18, .12, "#E07A4F", .8) + blob(0, -.92, .45, .2, "#fff", .95)),
  jupiter: () => disc(44, "#E6CBA3", band(-.66, -.52, "#B98559") + band(-.36, -.2, "#C99469") + band(-.08, .02, "#EFDDBE") + band(.1, .24, "#A96F47")
    + band(.38, .5, "#D1A57A") + band(.64, .76, "#B98559") + `<ellipse cx=".32" cy=".3" rx=".22" ry=".12" fill="#C4553A" stroke="#8E3A26" stroke-width=".03"/>`),
  saturn: () => {
    // the back of the rings, the planet, then the front of the rings (with the Cassini gap between the two rings)
    const rings = front => `<g transform="rotate(-18 50 50)" fill="none">${front
      ? `<path d="M3 50 A47 12 0 0 0 97 50" stroke="#C8AE74" stroke-width="5"/><path d="M13 50 A37 9 0 0 0 87 50" stroke="#EADBAA" stroke-width="4"/>`
      : `<ellipse cx="50" cy="50" rx="47" ry="12" stroke="#C8AE74" stroke-width="5"/><ellipse cx="50" cy="50" rx="37" ry="9" stroke="#EADBAA" stroke-width="4"/>`}</g>`;
    return rings(false) + disc(24, "#E6CF92", `<g transform="rotate(-18)">${band(-.5, -.32, "#D3B26E") + band(-.05, .08, "#D9BC7E") + band(.3, .44, "#F0DFAE")}</g>`) + rings(true);
  },
  uranus: () => {
    // Uranus spins lying on its side: its thin rings stand almost upright
    const ring = front => `<g transform="rotate(8 50 50)" fill="none" stroke="#E4F7F9" stroke-width="1.6" opacity=".8">${front ? `<path d="M50 5 A7 45 0 0 1 50 95"/>` : `<ellipse cx="50" cy="50" rx="7" ry="45"/>`}</g>`;
    return ring(false) + disc(30, "#A6E0E6", blob(-.1, -.15, .55, .5, "#C4EEF2", .5)) + ring(true);
  },
  neptune: () => disc(29, "#3456C8", band(-.5, -.38, "#4A6FE3", .8) + band(.18, .3, "#2A46A8") + blob(-.25, .02, .22, .11, "#1C2C7A") + blob(-.05, -.2, .22, .045, "#fff", .85, -6) + blob(.35, .45, .15, .035, "#fff", .7)),
  sun: () => {
    let rays = "";
    for (let k = 0; k < 12; k++) {
      const a = k * Math.PI / 6, p = (r, d) => `${(50 + r * Math.cos(a + d)).toFixed(1)},${(50 + r * Math.sin(a + d)).toFixed(1)}`;
      rays += `<polygon points="${p(31, -.2)} ${p(47, 0)} ${p(31, .2)}"/>`;
    }
    return `<g fill="#FFB300" stroke="#F57C00" stroke-width="1.5" stroke-linejoin="round">${rays}</g><circle cx="50" cy="50" r="31" fill="#FFD54F" stroke="#F57C00" stroke-width="2.5"/>
      <circle cx="50" cy="50" r="23" fill="#FFE27A"/><circle cx="42" cy="41" r="8" fill="#fff" opacity=".35"/>`;
  },
  // a crescent lit by the Sun; the rest of the disc stays faintly visible
  moon: (R = 34) => {
    const t = 50 - R, b = 50 + R, ri = R * .35, c = (x, y, r) => `<circle cx="${(50 + R * x).toFixed(1)}" cy="${(50 + R * y).toFixed(1)}" r="${(R * r).toFixed(1)}" fill="#D9D4C2"/>`;
    return `<circle cx="50" cy="50" r="${R}" fill="#3A4A66" stroke="#5A6B8A" stroke-width="1.5"/>
      <path d="M50 ${t} A${R} ${R} 0 0 1 50 ${b} A${ri.toFixed(1)} ${R} 0 0 0 50 ${t} Z" fill="#F3F0E2"/>${c(.62, -.3, .09) + c(.7, .2, .07) + c(.5, .55, .06)}`;
  },
  star: () => {
    const pts = [...Array(10)].map((_, k) => { const r = k % 2 ? 17 : 40, a = -Math.PI / 2 + k * Math.PI / 5; return `${(50 + r * Math.cos(a)).toFixed(1)},${(52 + r * Math.sin(a)).toFixed(1)}`; }).join(" ");
    return `<polygon points="${pts}" fill="#FFD54F" stroke="#F9A825" stroke-width="3" stroke-linejoin="round"/><circle cx="43" cy="40" r="4" fill="#fff" opacity=".6"/>`;
  },
  rocket: () => `<path d="M43 68 Q50 97 57 68 Z" fill="#FF9800"/><path d="M46.5 68 Q50 86 53.5 68 Z" fill="#FFEB3B"/>
    <path d="M36 50 L23 72 L37 67 Z" fill="#E53935" ${INK} stroke-width="2"/><path d="M64 50 L77 72 L63 67 Z" fill="#E53935" ${INK} stroke-width="2"/>
    <path d="M50 7 C62 19 66 38 63.5 66 L36.5 66 C34 38 38 19 50 7 Z" fill="#F5F5F5" ${INK} stroke-width="2.5"/>
    <path d="M50 7 C56.5 13.5 60 21 61.5 28 L38.5 28 C40 21 43.5 13.5 50 7 Z" fill="#E53935" ${INK} stroke-width="2.5"/>
    <circle cx="50" cy="42" r="7" fill="#4FC3F7" stroke="#1B2D45" stroke-width="2.5"/><circle cx="47.5" cy="39.5" r="2" fill="#fff"/>
    <rect x="43" y="66" width="14" height="4" rx="1" fill="#78909C" stroke="#1B2D45" stroke-width="1.5"/>`,
  astronaut: () => `<rect x="31" y="44" width="38" height="28" rx="7" fill="#B0BEC5" stroke="#1B2D45" stroke-width="2"/>
    <rect x="22" y="50" width="12" height="22" rx="6" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2" transform="rotate(20 28 52)"/>
    <rect x="66" y="50" width="12" height="22" rx="6" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2" transform="rotate(-20 72 52)"/>
    <rect x="37" y="72" width="11" height="18" rx="4" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2"/><rect x="52" y="72" width="11" height="18" rx="4" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2"/>
    <rect x="36" y="85" width="13" height="7" rx="3" fill="#78909C" stroke="#1B2D45" stroke-width="2"/><rect x="51" y="85" width="13" height="7" rx="3" fill="#78909C" stroke="#1B2D45" stroke-width="2"/>
    <rect x="35" y="46" width="30" height="30" rx="9" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2"/>
    <rect x="43" y="55" width="14" height="9" rx="2" fill="#CFD8DC" stroke="#1B2D45" stroke-width="1.5"/><circle cx="47" cy="59.5" r="1.8" fill="#E53935"/><circle cx="53" cy="59.5" r="1.8" fill="#43AA8B"/>
    <circle cx="50" cy="29" r="19" fill="#F5F5F5" stroke="#1B2D45" stroke-width="2.5"/>
    <ellipse cx="50" cy="30" rx="13" ry="10" fill="#1E3A5F" stroke="#1B2D45" stroke-width="2"/><path d="M42 26 Q46 22 51 23" stroke="#fff" stroke-width="2.5" fill="none" stroke-linecap="round" opacity=".8"/>`
};
const svgOf = (id, R) => `<svg class="esp-svg" viewBox="0 0 100 100" aria-hidden="true">${R ? ART[id](R) : ART[id]()}</svg>`;

/* ---------- words ---------- */
const PLANETS = ["mercury","venus","earth","mars","jupiter","saturn","uranus","neptune"]; // from the Sun outwards
// level 1: the lexicon words (donnees.js), so Luxembourgish plays their lod.lu recordings
const L1W = {
  sun: {en:"sun", de:"die Sonne", lb:"d'Sonn", zh:"太阳"}, moon: {en:"moon", de:"der Mond", lb:"de Mound", zh:"月亮"},
  star: {en:"star", de:"der Stern", lb:"de Stär", zh:"星星"}, rocket: {en:"rocket", de:"die Rakete", lb:"d'Rakéit", zh:"火箭"},
  astronaut: {en:"astronaut", de:"der Astronaut", lb:"den Astronaut", zh:"宇航员"}
};
// [in a sentence, under the picture, after "after"]: German "nach dem Merkur", Luxembourgish "nom Merkur" (no + dem), "no der Venus"
const N = {
  sun: {en:["the Sun","Sun"], de:["die Sonne","Sonne"], lb:["d'Sonn","Sonn"], zh:["太阳","太阳"]},
  moon: {en:["the Moon","Moon"], de:["der Mond","Mond"], lb:["de Mound","Mound"], zh:["月亮","月亮"]},
  mercury: {en:["Mercury","Mercury"], de:["der Merkur","Merkur","dem Merkur"], lb:["de Merkur","Merkur","nom Merkur"], zh:["水星","水星"]},
  venus: {en:["Venus","Venus"], de:["die Venus","Venus","der Venus"], lb:["d'Venus","Venus","no der Venus"], zh:["金星","金星"]},
  earth: {en:["Earth","Earth"], de:["die Erde","Erde","der Erde"], lb:["d'Äerd","Äerd","no der Äerd"], zh:["地球","地球"]},
  mars: {en:["Mars","Mars"], de:["der Mars","Mars","dem Mars"], lb:["de Mars","Mars","nom Mars"], zh:["火星","火星"]},
  jupiter: {en:["Jupiter","Jupiter"], de:["der Jupiter","Jupiter","dem Jupiter"], lb:["de Jupiter","Jupiter","nom Jupiter"], zh:["木星","木星"]},
  saturn: {en:["Saturn","Saturn"], de:["der Saturn","Saturn","dem Saturn"], lb:["de Saturn","Saturn","nom Saturn"], zh:["土星","土星"]},
  uranus: {en:["Uranus","Uranus"], de:["der Uranus","Uranus","dem Uranus"], lb:["den Uranus","Uranus","nom Uranus"], zh:["天王星","天王星"]},
  neptune: {en:["Neptune","Neptune"], de:["der Neptun","Neptun","dem Neptun"], lb:["den Neptun","Neptun","nom Neptun"], zh:["海王星","海王星"]}
};
const TX = {
  en: {again:"Again", thats:n => `That's ${n}.`, first:"Which planet is closest to the Sun?", after:(n, d) => `Which planet comes after ${n}?`,
       all:"All eight planets are in place!", tf:"True or false?", t:"True", f:"False", yesT:"Yes, it's true!", yesF:"Yes, it's false!", noT:"Oops! It's true.", noF:"Oops! It's false.", sep:", "},
  de: {again:"Nochmal", thats:n => `Das ist ${n}.`, first:"Welcher Planet ist der Sonne am nächsten?", after:(n, d) => `Welcher Planet kommt nach ${d}?`,
       all:"Alle acht Planeten sind da!", tf:"Richtig oder falsch?", t:"Richtig", f:"Falsch", yesT:"Ja, das stimmt!", yesF:"Ja, das ist falsch!", noT:"Hoppla! Das stimmt.", noF:"Hoppla! Das ist falsch.", sep:", "},
  lb: {again:"Nach eng Kéier", thats:n => `Dat ass ${n}.`, first:"Wéi ee Planéit ass am nooste bei der Sonn?", after:(n, d) => `Wéi ee Planéit kënnt ${d}?`,
       all:"Déi aacht Planéite sinn all do!", tf:"Richteg oder falsch?", t:"Richteg", f:"Falsch", yesT:"Jo, dat stëmmt!", yesF:"Jo, dat ass falsch!", noT:"Neen! Dat stëmmt.", noF:"Neen! Dat ass falsch.", sep:", "},
  zh: {again:"再听一次", thats:n => `这是${n}。`, first:"哪颗行星离太阳最近？", after:(n, d) => `${n}后面是哪颗行星？`,
       all:"八颗行星都到齐了！", tf:"对还是错？", t:"对", f:"错", yesT:"没错，这是对的！", yesF:"没错，这是错的！", noT:"哎呀，这句话是对的。", noF:"哎呀，这句话是错的。", sep:"、"}
};
// level 2: a planet by its colour or its shape; pool = the other pictures shown (never one that also fits: no Jupiter for "red", no Uranus for "rings")
const Q2 = [
  {key:"red planet", ans:"mars", pool:["mercury","venus","earth","saturn","uranus","neptune"],
   q:{en:"Which planet is red?", de:"Welcher Planet ist rot?", lb:"Wéi ee Planéit ass rout?", zh:"哪颗行星是红色的？"},
   ok:{en:"Yes! Mars is the red planet.", de:"Ja! Der Mars ist der rote Planet.", lb:"Jo! De Mars ass de roude Planéit.", zh:"对！火星是红色的行星。"}},
  {key:"big rings", ans:"saturn", pool:["mercury","venus","earth","mars","jupiter","neptune"],
   q:{en:"Which planet has big rings?", de:"Welcher Planet hat große Ringe?", lb:"Wéi ee Planéit huet grouss Réng?", zh:"哪颗行星有很大的光环？"},
   ok:{en:"Yes! Saturn has big rings.", de:"Ja! Der Saturn hat große Ringe.", lb:"Jo! De Saturn huet grouss Réng.", zh:"对！土星有很大的光环。"}},
  {key:"biggest planet", ans:"jupiter", pool:["mercury","venus","earth","mars","uranus","neptune"],
   q:{en:"Which is the biggest planet?", de:"Welcher ist der größte Planet?", lb:"Wat ass de gréisste Planéit?", zh:"哪颗行星最大？"},
   ok:{en:"Yes! Jupiter is the biggest planet.", de:"Ja! Der Jupiter ist der größte Planet.", lb:"Jo! De Jupiter ass de gréisste Planéit.", zh:"对！木星是最大的行星。"}},
  {key:"smallest planet", ans:"mercury", pool:["venus","earth","jupiter","saturn","uranus","neptune"],
   q:{en:"Which is the smallest planet?", de:"Welcher ist der kleinste Planet?", lb:"Wat ass de klengste Planéit?", zh:"哪颗行星最小？"},
   ok:{en:"Yes! Mercury is the smallest planet.", de:"Ja! Der Merkur ist der kleinste Planet.", lb:"Jo! De Merkur ass de klengste Planéit.", zh:"对！水星是最小的行星。"}},
  {key:"our planet", ans:"earth", pool:["mercury","venus","mars","jupiter","saturn","uranus","neptune"],
   q:{en:"Which planet do we live on?", de:"Auf welchem Planeten leben wir?", lb:"Op wéi engem Planéit liewe mir?", zh:"我们住在哪颗行星上？"},
   ok:{en:"Yes! We live on Earth.", de:"Ja! Wir leben auf der Erde.", lb:"Jo! Mir liewen op der Äerd.", zh:"对！我们住在地球上。"}},
  {key:"dark blue planet", ans:"neptune", pool:["mercury","venus","mars","jupiter","saturn"],
   q:{en:"Which planet is dark blue?", de:"Welcher Planet ist dunkelblau?", lb:"Wéi ee Planéit ass donkelblo?", zh:"哪颗行星是深蓝色的？"},
   ok:{en:"Yes! That's Neptune. It is the farthest planet from the Sun.", de:"Ja! Das ist der Neptun. Er ist am weitesten von der Sonne entfernt.",
       lb:"Jo! Dat ass den Neptun. Hien ass am wäitste vun der Sonn ewech.", zh:"对！这是海王星，它离太阳最远。"}},
  {key:"not a planet", ans:"sun", pool:PLANETS,
   q:{en:"Which one is not a planet?", de:"Was ist hier kein Planet?", lb:"Wat ass hei kee Planéit?", zh:"哪一个不是行星？"},
   ok:{en:"Yes! The Sun is not a planet: it is a star!", de:"Ja! Die Sonne ist kein Planet, sie ist ein Stern!", lb:"Jo! D'Sonn ass kee Planéit, si ass e Stär!", zh:"对！太阳不是行星，它是一颗恒星！"}},
  {key:"around the Earth", ans:"moon", pool:["sun","mercury","venus","mars","jupiter","saturn"],
   q:{en:"Which one goes around the Earth?", de:"Was kreist um die Erde?", lb:"Wat dréit ëm d'Äerd?", zh:"哪一个绕着地球转？"},
   ok:{en:"Yes! The Moon goes around the Earth.", de:"Ja! Der Mond kreist um die Erde.", lb:"Jo! De Mound dréit ëm d'Äerd.", zh:"对！月亮绕着地球转。"}}
];
// level 4: one sentence per topic; t = true or false; fix = what is true instead; v = the pictures ([id, radius] draws the Moon to scale beside the Earth)
const FACTS = [
  {topic:"sun", t:true, v:["sun"], en:"The Sun is a star.", de:"Die Sonne ist ein Stern.", lb:"D'Sonn ass e Stär.", zh:"太阳是一颗恒星。"},
  {topic:"sun", t:false, v:["sun"], en:"The Sun is a planet.", de:"Die Sonne ist ein Planet.", lb:"D'Sonn ass e Planéit.", zh:"太阳是一颗行星。",
   fix:{en:"The Sun is not a planet, it is a star.", de:"Die Sonne ist kein Planet, sondern ein Stern.", lb:"D'Sonn ass kee Planéit, mee e Stär.", zh:"太阳不是行星，而是一颗恒星。"}},
  {topic:"rings", t:true, v:["saturn"], en:"Saturn has rings.", de:"Der Saturn hat Ringe.", lb:"De Saturn huet Réng.", zh:"土星有光环。"},
  {topic:"rings", t:false, v:["saturn","uranus"], en:"Only Saturn has rings.", de:"Nur der Saturn hat Ringe.", lb:"Nëmmen de Saturn huet Réng.", zh:"只有土星有光环。",
   fix:{en:"Jupiter, Uranus and Neptune have rings too, but they are hard to see.", de:"Jupiter, Uranus und Neptun haben auch Ringe, aber man sieht sie kaum.",
        lb:"De Jupiter, den Uranus an den Neptun hunn och Réng, mee een gesäit se kaum.", zh:"木星、天王星和海王星也有光环，只是很难看到。"}},
  {topic:"jupiter", t:true, v:["jupiter","earth"], en:"Jupiter is the biggest planet.", de:"Der Jupiter ist der größte Planet.", lb:"De Jupiter ass de gréisste Planéit.", zh:"木星是最大的行星。"},
  {topic:"jupiter", t:false, v:["jupiter","earth"], en:"Jupiter is smaller than the Earth.", de:"Der Jupiter ist kleiner als die Erde.", lb:"De Jupiter ass méi kleng wéi d'Äerd.", zh:"木星比地球小。",
   fix:{en:"Jupiter is much bigger than the Earth.", de:"Der Jupiter ist viel größer als die Erde.", lb:"De Jupiter ass vill méi grouss wéi d'Äerd.", zh:"木星比地球大得多。"}},
  {topic:"mars", t:true, v:["mars"], en:"Mars is red.", de:"Der Mars ist rot.", lb:"De Mars ass rout.", zh:"火星是红色的。"},
  {topic:"mars", t:false, v:["mars"], en:"People live on Mars.", de:"Auf dem Mars wohnen Menschen.", lb:"Um Mars wunne Leit.", zh:"火星上住着人。",
   fix:{en:"No one lives on Mars.", de:"Auf dem Mars wohnt kein Mensch.", lb:"Um Mars wunnt kee Mënsch.", zh:"火星上没有人住。"}},
  {topic:"orbit", t:true, v:["sun","earth"], en:"The Earth goes around the Sun in one year.", de:"Die Erde kreist in einem Jahr einmal um die Sonne.", lb:"D'Äerd dréit an engem Joer eemol ëm d'Sonn.", zh:"地球一年绕太阳转一圈。"},
  {topic:"orbit", t:false, v:["sun","earth"], en:"The Sun goes around the Earth.", de:"Die Sonne kreist um die Erde.", lb:"D'Sonn dréit ëm d'Äerd.", zh:"太阳绕着地球转。",
   fix:{en:"The Earth goes around the Sun.", de:"Die Erde kreist um die Sonne.", lb:"D'Äerd dréit ëm d'Sonn.", zh:"是地球绕着太阳转。"}},
  {topic:"moon", t:true, v:["earth",["moon", 7]], en:"The Moon goes around the Earth.", de:"Der Mond kreist um die Erde.", lb:"De Mound dréit ëm d'Äerd.", zh:"月亮绕着地球转。"},
  {topic:"moon", t:false, v:["earth",["moon", 7]], en:"The Moon is a planet.", de:"Der Mond ist ein Planet.", lb:"De Mound ass e Planéit.", zh:"月亮是一颗行星。",
   fix:{en:"The Moon is not a planet. It goes around the Earth.", de:"Der Mond ist kein Planet. Er kreist um die Erde.", lb:"De Mound ass kee Planéit. Hien dréit ëm d'Äerd.", zh:"月亮不是行星，它绕着地球转。"}},
  {topic:"moon", t:false, v:["earth",["moon", 7]], en:"The Moon is bigger than the Earth.", de:"Der Mond ist größer als die Erde.", lb:"De Mound ass méi grouss wéi d'Äerd.", zh:"月亮比地球大。",
   fix:{en:"The Moon is much smaller than the Earth.", de:"Der Mond ist viel kleiner als die Erde.", lb:"De Mound ass vill méi kleng wéi d'Äerd.", zh:"月亮比地球小得多。"}},
  {topic:"count", t:true, v:PLANETS, en:"Eight planets go around the Sun.", de:"Acht Planeten kreisen um die Sonne.", lb:"Aacht Planéiten dréien ëm d'Sonn.", zh:"有八颗行星绕着太阳转。"},
  {topic:"near", t:true, v:["sun","mercury"], en:"Mercury is the closest planet to the Sun.", de:"Der Merkur ist der Sonne am nächsten.", lb:"De Merkur ass am nooste bei der Sonn.", zh:"水星离太阳最近。"},
  {topic:"near", t:false, v:["sun","earth","neptune"], en:"Neptune is closer to the Sun than the Earth.", de:"Der Neptun ist näher an der Sonne als die Erde.", lb:"Den Neptun ass méi no bei der Sonn wéi d'Äerd.", zh:"海王星比地球离太阳更近。",
   fix:{en:"Neptune is the planet farthest from the Sun.", de:"Der Neptun ist der Planet, der am weitesten von der Sonne entfernt ist.",
        lb:"Den Neptun ass dee Planéit, deen am wäitste vun der Sonn ewech ass.", zh:"海王星是离太阳最远的行星。"}},
  {topic:"hot", t:true, v:["venus","mercury"], en:"Venus is hotter than Mercury.", de:"Die Venus ist heißer als der Merkur.", lb:"D'Venus ass méi waarm wéi de Merkur.", zh:"金星比水星还热。"},
  {topic:"hot", t:false, v:["venus","mercury"], en:"Mercury is hotter than Venus.", de:"Der Merkur ist heißer als die Venus.", lb:"De Merkur ass méi waarm wéi d'Venus.", zh:"水星比金星还热。",
   fix:{en:"Venus is hotter than Mercury, even though it is farther from the Sun.", de:"Die Venus ist heißer als der Merkur, obwohl sie weiter von der Sonne entfernt ist.",
        lb:"D'Venus ass méi waarm wéi de Merkur, obwuel si méi wäit vun der Sonn ewech ass.", zh:"金星比水星还热，虽然它离太阳更远。"}},
  {topic:"day", t:true, v:["earth"], en:"The Earth spins around once a day.", de:"Die Erde dreht sich an einem Tag einmal um sich selbst.", lb:"D'Äerd dréit sech an engem Dag eemol ëm hir eegen Achs.", zh:"地球一天自转一圈。"},
  {topic:"day", t:false, v:["sun","earth"], en:"It is daytime everywhere on Earth at the same time.", de:"Es ist überall auf der Erde gleichzeitig Tag.", lb:"Et ass iwwerall op der Äerd gläichzäiteg Dag.", zh:"地球上所有地方同时都是白天。",
   fix:{en:"When it is day on one side of the Earth, it is night on the other side.", de:"Wenn auf der einen Seite der Erde Tag ist, ist auf der anderen Seite Nacht.",
        lb:"Wann et op enger Säit vun der Äerd Dag ass, ass et op der anerer Säit Nuecht.", zh:"地球一边是白天的时候，另一边是黑夜。"}},
  {topic:"stars", t:true, v:["star","sun"], en:"The stars in the sky are suns, very far away.", de:"Die Sterne am Himmel sind Sonnen, sehr weit weg.", lb:"D'Stären um Himmel si Sonnen, ganz wäit ewech.", zh:"天上的星星是很远很远的太阳。"},
  {topic:"space", t:false, v:["astronaut"], en:"In space, there is air to breathe.", de:"Im Weltall gibt es Luft zum Atmen.", lb:"Am Weltall gëtt et Loft fir ze ootmen.", zh:"太空里有空气可以呼吸。",
   fix:{en:"There is no air in space. That's why astronauts wear a helmet.", de:"Im Weltall gibt es keine Luft. Darum tragen Astronauten einen Helm.",
        lb:"Am Weltall gëtt et keng Loft. Dofir droen d'Astronauten en Helm.", zh:"太空里没有空气，所以宇航员要戴头盔。"}}
];

addStyle(`
.esp-card{background:radial-gradient(circle at 30% 25%, #2A4178 0, #13224A 60%, #0B1530 100%); position:relative; overflow:hidden}
.esp-card::before{content:""; position:absolute; inset:0; pointer-events:none; opacity:.85;
  background-image:radial-gradient(circle at 12% 18%, #fff 1.2px, transparent 2px), radial-gradient(circle at 80% 12%, #fff 1px, transparent 1.6px),
  radial-gradient(circle at 88% 72%, #fff 1.2px, transparent 2px), radial-gradient(circle at 18% 84%, #fff 1px, transparent 1.6px), radial-gradient(circle at 56% 92%, #fff 1px, transparent 1.6px)}
.esp-card .esp-svg{width:74%; height:auto; position:relative; overflow:visible}
.esp-card .w{color:#fff; position:relative; min-height:1.2em; text-shadow:1px 1px 0 #0B1530}
.esp-card.ok{background:radial-gradient(circle, #2F8F6B 0, #13224A 78%)}
.esp-card.ko{background:radial-gradient(circle, #9C3B4B 0, #13224A 78%)}
.esp-fact{margin:0; text-align:center; font-family:var(--display); font-size:20px; font-weight:600; background:#fff; border:3px solid var(--ink); border-radius:16px; padding:8px 12px}
.esp-strip{display:flex; align-items:center; gap:3px; padding:8px 8px 8px 0; background:linear-gradient(90deg, #1E2F5C, #0B1530); border:3px solid var(--ink); border-radius:18px; overflow:hidden}
.esp-sunedge{flex:0 0 22px; height:56px; background:#FFC43D; border-radius:0 56px 56px 0; box-shadow:0 0 16px 4px #FFB300}
.esp-slot{flex:1 1 0; min-width:0; max-width:48px; aspect-ratio:1; border-radius:50%; display:grid; place-items:center; color:#FFC43D; font-family:var(--display); font-weight:700; font-size:18px}
.esp-slot.empty{border:2px dashed rgba(255,255,255,.3)}
.esp-slot.next{border:2px dashed #FFC43D; animation:esp-pulse 1s ease-in-out infinite}
.esp-slot svg{width:100%; height:100%; overflow:visible}
.esp-slot.pop svg{animation:esp-pop .5s ease-out}
.esp-strip.wave .esp-slot{animation:esp-wave .8s ease-in-out 2; animation-delay:calc(var(--k) * 80ms)}
.esp-vis{display:flex; justify-content:center; align-items:center; flex-wrap:wrap; gap:4px; padding:8px; background:radial-gradient(circle at 30% 25%, #2A4178 0, #0B1530 90%); border:3px solid var(--ink); border-radius:18px}
.esp-mini{width:clamp(64px,20vw,96px); display:block} .esp-mini.tiny{width:clamp(34px,9.5vw,52px)}
.esp-mini svg{width:100%; height:auto; display:block; overflow:visible}
.esp-sent{font-size:clamp(21px,4.6vw,30px); margin:0}
.esp-sent b{font-weight:600}
.esp-tf{display:grid; grid-template-columns:1fr 1fr; gap:12px}
.esp-tf button{font-family:var(--display); font-size:24px; font-weight:600; padding:14px 8px; background:#fff; display:flex; flex-direction:column; align-items:center; gap:2px}
.esp-tf button.ok{background:#C9F2DF} .esp-tf button.ko{background:#FFD6CF; animation:shake .4s}
.esp-tf .esp-tfe{font-size:40px; line-height:1}
.esp-fb{background:#fff; border:3px solid var(--ink); border-radius:18px; padding:10px 14px; font-family:var(--display); font-size:20px; text-align:center}
.esp-fb b{display:block; margin-top:4px; font-weight:600; color:var(--sea-deep)}
.esp-next{align-self:center; font-size:30px; padding:8px 30px}
.esp-comet{position:fixed; left:0; top:0; font-size:44px; pointer-events:none; z-index:60}
.esp-go-launch{animation:esp-launch 1.1s ease-in forwards}
.esp-go-spin{animation:esp-spin 1.2s ease-in-out}
.esp-go-rock{animation:esp-rock .9s ease-in-out 2}
.esp-go-twinkle{animation:esp-twinkle .7s ease-in-out 2}
.esp-go-float{animation:esp-float 1.2s ease-in-out}
.esp-go-joy{animation:esp-joy .8s ease-in-out}
@keyframes esp-launch{0%{transform:none} 15%{transform:translateY(4%) rotate(-2deg)} 30%{transform:translateY(2%) rotate(2deg)} 100%{transform:translateY(-170%); opacity:0}}
@keyframes esp-spin{to{transform:rotate(360deg) scale(1)}}
@keyframes esp-rock{0%,100%{transform:rotate(0)} 25%{transform:rotate(-16deg)} 75%{transform:rotate(16deg)}}
@keyframes esp-twinkle{0%,100%{transform:scale(1)} 30%{transform:scale(1.25) rotate(14deg)} 60%{transform:scale(.9)}}
@keyframes esp-float{0%,100%{transform:none} 40%{transform:translateY(-14%) rotate(-12deg)} 70%{transform:translateY(-6%) rotate(8deg)}}
@keyframes esp-joy{0%,100%{transform:none} 30%{transform:scale(1.16) rotate(-8deg)} 60%{transform:scale(.95) rotate(5deg)}}
@keyframes esp-pop{0%{transform:scale(0)} 70%{transform:scale(1.3)} 100%{transform:scale(1)}}
@keyframes esp-pulse{50%{transform:scale(1.12)}}
@keyframes esp-wave{0%,100%{transform:none} 50%{transform:translateY(-8px)}}
@media (prefers-reduced-motion: reduce){.esp-slot.next, .esp-strip.wave .esp-slot, .esp-slot.pop svg{animation:none}}
`);

registerGame({id:ID, em:"🪐", name:"L'espace", desc:"Le Soleil, la Lune et les planètes", multi:true, cat:"monde",
  title:{en:"Space", de:"Im Weltall", lb:"Am Weltall", zh:"太空"},
  sub:{en:"The Sun, the Moon and the planets", de:"Sonne, Mond und Planeten", lb:"Sonn, Mound a Planéiten", zh:"太阳、月亮和行星"}}, function () {
  const L = LG(), lvl = levelOf(ID), X = TX[L], P = PHRASES[L] || PHRASES.en;
  const speak = t => say(t, L);
  const praise = () => P.praise[rnd(P.praise.length)];
  const nom = id => N[id][L][0], short = id => N[id][L][1], dat = id => N[id][L][2];
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? Math.min(ms, 120) : ms)));
  const body = $("gameBody"), res = [];
  const noOk = () => body.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok); // the round is over: nothing left to find

  // a surprise: a comet crosses the screen
  const comet = () => {
    if (calm()) return;
    const d = el("div", "esp-comet", "☄️"); document.body.append(d);
    d.animate([{transform:`translate(${innerWidth + 40}px, 40px)`}, {transform:`translate(-80px, ${Math.round(innerHeight * .35)}px)`}], {duration:1500, easing:"ease-in"}).onfinish = () => d.remove();
  };
  // each picture has its own little show when it is the right answer
  const JOY = {rocket:"esp-go-launch", sun:"esp-go-spin", moon:"esp-go-rock", star:"esp-go-twinkle", astronaut:"esp-go-float"};
  const celebrate = (card, id) => {
    if (!TEST && id === "rocket") tone([160, 220, 300, 420, 600, 850], .09, "triangle");
    if (!TEST && id === "star") tone([1568, 2093, 2637], .08, "sine");
    if (calm()) return;
    const s = card.querySelector("svg"); if (s) s.classList.add(JOY[id] || "esp-go-joy");
  };

  /* levels 1 to 3: touch the right picture */
  function ask(o){
    return new Promise(done => {
      body.innerHTML = "";
      if (o.top) body.append(o.top);
      const p = el("p", "prompt", o.prompt); body.append(p);
      const row = el("div", "row"); row.style.justifyContent = "center"; row.append(speakBtn(() => o.voice, X.again, L)); body.append(row);
      const grid = el("div", "choices");
      let tries = 0, locked = false;
      shuffle(o.ids).forEach(id => {
        const c = el("button", "choice chunky esp-card", `${svgOf(id)}<span class="w">${o.labels ? short(id) : ""}</span>`);
        if (id === o.good) markOk(c);
        c.onclick = async () => {
          if (locked || !alive(gen)) return; G.taps++;
          if (id === o.good) {
            locked = true; noOk(); c.classList.add("ok"); sfx.ok();
            const first = tries === 0; if (first) addStar();
            logRound(o.key, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
            c.querySelector(".w").textContent = o.label(id);
            celebrate(c, id);
            await o.win(c, p);
            if (alive(gen)) done();
          } else {
            tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko");
            c.querySelector(".w").textContent = o.label(id);
            await speak(o.wrongSay(id));
            if (o.again && alive(gen) && !locked) speak(o.voice); // the little one hears the question again
          }
        };
        grid.append(c);
      });
      body.append(grid);
      speak(o.voice);
    });
  }

  /* level 3: the solar system being built, the Sun on the left */
  function strip(placed, next){
    const s = el("div", "esp-strip"); s.append(el("div", "esp-sunedge"));
    PLANETS.forEach((id, k) => {
      const slot = el("div", "esp-slot " + (k < placed ? "" : k === next ? "next" : "empty"), k < placed ? svgOf(id) : k === next ? "?" : "");
      slot.style.setProperty("--k", k); s.append(slot);
    });
    return s;
  }

  /* level 4: true or false */
  function tfRound(f){
    return new Promise(done => {
      body.innerHTML = "";
      const sentence = f[L], readAlone = G.kid === "p7"; // the big one reads alone, the little one hears it
      const vis = el("div", "esp-vis");
      f.v.forEach(v => { const [id, R] = Array.isArray(v) ? v : [v]; vis.append(el("span", "esp-mini" + (f.v.length > 4 ? " tiny" : ""), svgOf(id, R))); });
      body.append(vis, el("p", "prompt esp-sent", `<b>${sentence}</b><small>🤔 ${X.tf}</small>`));
      const row = el("div", "row"); row.style.justifyContent = "center";
      if (readAlone) { const b = el("button", "chip", "🔊"); b.onclick = () => { G.hints++; speak(sentence); }; row.append(b); }
      else row.append(speakBtn(() => sentence + (L === "zh" ? "" : " ") + X.tf, X.again, L));
      body.append(row);
      const tf = el("div", "esp-tf"); let locked = false;
      [true, false].forEach(val => {
        const b = el("button", "chunky", `<span class="esp-tfe">${val ? "✅" : "❌"}</span><span>${val ? X.t : X.f}</span>`);
        if (val === f.t) markOk(b);
        b.onclick = async () => {
          if (locked || !alive(gen)) return; locked = true; G.taps++; noOk();
          const right = val === f.t;
          if (right) { b.classList.add("ok"); sfx.ok(); addStar(); if (rnd(2)) comet(); }
          else { b.classList.add("ko"); sfx.ko(); tf.querySelectorAll("button").forEach(x => { if (x !== b) x.classList.add("ok"); }); }
          logRound(f.en, right, right ? 1 : 2, {lvl}); res.push(right ? 1 : 0); renderDots(res, total, -1);
          const head = right ? (f.t ? X.yesT : X.yesF) : (f.t ? X.noT : X.noF), fix = f.fix ? f.fix[L] : "";
          body.append(el("div", "esp-fb", `${right ? "🌟" : "💡"} ${head}${fix ? `<b>${fix}</b>` : ""}`));
          const next = el("button", "bigbtn chunky esp-next", "➡️"); markOk(next);
          next.onclick = () => { if (!alive(gen)) return; G.taps++; sfx.tap(); next.disabled = true; done(); };
          body.append(next);
          await speak(head + (fix ? (L === "zh" ? "" : " ") + fix : ""));
        };
        tf.append(b);
      });
      body.append(tf);
      speak(readAlone ? X.tf : sentence + (L === "zh" ? "" : " ") + X.tf);
    });
  }

  // one topic per sentence, about as many true as false
  const pickFacts = () => {
    const topics = pick([...new Set(FACTS.map(f => f.topic))], 8), want = shuffle([true, true, true, true, false, false, false, false]);
    return topics.map((t, k) => { const all = FACTS.filter(f => f.topic === t), good = all.filter(f => f.t === want[k]); return pick(good.length ? good : all, 1)[0]; });
  };
  const L1 = Object.keys(L1W);
  const plan = lvl === 1 ? (() => { const a = shuffle(L1); a.push(pick(L1.filter(x => x !== a[a.length - 1]), 1)[0]); return a; })()
    : lvl === 2 ? pick(Q2, 7) : lvl === 3 ? PLANETS.slice() : pickFacts();
  const total = plan.length;
  startSession(ID, null, total); const gen = GEN;

  const play = async k => {
    const it = plan[k];
    if (lvl === 1) {
      const q = P.find(L1W[it][L]);
      return ask({prompt:"🔭 " + q, voice:q, ids:[it, ...pick(L1.filter(x => x !== it), 2)], good:it, key:it, again:true,
        label:x => L1W[x][L], wrongSay:x => P.thats(L1W[x][L]),
        win:async () => { await Promise.all([speak(praise()), wait(it === "rocket" ? 1200 : 900)]); }});
    }
    if (lvl === 2) {
      return ask({prompt:"🪐 " + it.q[L], voice:it.q[L], ids:[it.ans, ...pick(it.pool, 3)], good:it.ans, key:it.key,
        label:short, wrongSay:x => X.thats(nom(x)),
        win:async (c, p) => { p.after(el("p", "esp-fact", "✨ " + it.ok[L])); if (rnd(2)) comet(); await Promise.all([speak(it.ok[L]), wait(2200)]); }});
    }
    if (lvl === 3) {
      const q = k === 0 ? X.first : X.after(nom(PLANETS[k - 1]), dat(PLANETS[k - 1])), top = strip(k, k);
      return ask({top, prompt:"☀️ " + q, voice:q, ids:[it, ...pick(PLANETS.filter(x => x !== it), 3)], good:it, labels:true,
        key:k === 0 ? "closest to the Sun" : `after ${PLANETS[k - 1]}`, label:short, wrongSay:x => X.thats(nom(x)),
        win:async (c, p) => {
          const slot = top.children[k + 1]; slot.className = "esp-slot pop"; slot.innerHTML = svgOf(it);
          const chain = PLANETS.slice(0, k + 1).map(short).join(X.sep), line = `${praise()} ${chain}${L === "zh" ? "！" : "!"}`;
          p.after(el("p", "esp-fact", line));
          await Promise.all([speak(line), wait(1500)]);
        }});
    }
    return tfRound(it);
  };

  (async () => {
    for (let k = 0; k < total; k++) {
      if (!alive(gen)) return;
      renderDots(res, total, k);
      await play(k);
      if (!alive(gen)) return;
      await wait(250);
    }
    if (!alive(gen)) return;
    if (lvl === 3) { // the whole solar system waves goodbye
      body.innerHTML = "";
      const s = strip(8, -1); if (!calm()) s.classList.add("wave");
      body.append(s, el("p", "prompt", "🚀 " + X.all));
      comet(); fx.sparkle(innerWidth / 2, innerHeight / 3, 20);
      await Promise.all([speak(X.all), wait(1800)]);
      if (!alive(gen)) return;
    }
    finish();
  })();
});
})();
