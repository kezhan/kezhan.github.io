/* L'Île aux Mots : jeu « Formes », dans la langue apprise (anglais, allemand, luxembourgeois, chinois ; jamais de français).
   1 : trouver la forme, 3 images · 2 : 4 images · 3 : couleur et forme ensemble, 6 images · 4 : plus de pièges de même couleur ou de même forme.
   La question est toujours écrite, en plus de la voix (Kezhan : « c'est l'occasion de lire »).
   Formes dessinées en SVG : toutes les couleurs vont avec toutes les formes (il n'existe pas d'émoji losange vert).
   Accords : das blaue Quadrat, der rote Kreis, die grüne Raute ; de roude Krees, den Dräieck, déi gréng Raut, dat blot Häerz ; 蓝色的正方形.
   Luxembourgeois vérifié sur lod.lu : KREES1 et DRAIECK1 masculins, QUADRAT1 masculin ou neutre, RAUT1 féminin, STAR1, HAERZ1 ;
   formes des adjectifs relevées dans lod.lu : « de Roude Léiw », « e bloe Vëlo », « eng gréng Kusch », blot, gréngt, gielt (neutre). */
addStyle(`.choice svg.forme{width:62%; height:auto; overflow:visible}
.choice.ok svg.forme.vive{animation:formeTwirl .8s cubic-bezier(.3,1.4,.5,1)}
@keyframes formeTwirl{0%{transform:none} 50%{transform:rotate(200deg) scale(1.3)} 100%{transform:rotate(360deg)}}`);

const FORMES = {
  // g: gender of the noun (m, f, n)
  shapes: [
    {k:"circle", svg:'<circle cx="50" cy="50" r="41"/>', en:"circle", de:["m","Kreis"], lb:["m","Krees"], zh:"圆形"},
    {k:"square", svg:'<rect x="11" y="11" width="78" height="78" rx="4"/>', en:"square", de:["n","Quadrat"], lb:["m","Quadrat"], zh:"正方形"},
    {k:"triangle", svg:'<polygon points="50,7 94,88 6,88"/>', en:"triangle", de:["n","Dreieck"], lb:["m","Dräieck"], zh:"三角形"},
    {k:"star", svg:`<polygon points="${Array.from({length: 10}, (_, i) => { const r = i % 2 ? 19 : 47, a = Math.PI / 5 * i - Math.PI / 2; return `${(50 + r * Math.cos(a)).toFixed(1)},${(54 + r * Math.sin(a)).toFixed(1)}`; }).join(" ")}"/>`,
      en:"star", de:["m","Stern"], lb:["m","Stär"], zh:"五角星"},
    {k:"heart", svg:'<path d="M50 88 C22 67 5 51 8 31 C11 12 36 8 50 27 C64 8 89 12 92 31 C95 51 78 67 50 88 Z"/>', en:"heart", de:["n","Herz"], lb:["n","Häerz"], zh:"心形"},
    {k:"diamond", svg:'<polygon points="50,4 91,50 50,96 9,50"/>', en:"diamond", de:["f","Raute"], lb:["f","Raut"], zh:"菱形"}
  ],
  // colour words: German after "der, die, das"; Luxembourgish by gender (masculine before the n-rule)
  colours: {
    red: {de:"rote", lb:{m:"rouden", f:"rout", n:"rout"}}, blue: {de:"blaue", lb:{m:"bloen", f:"blo", n:"blot"}},
    green: {de:"grüne", lb:{m:"gréngen", f:"gréng", n:"gréngt"}}, yellow: {de:"gelbe", lb:{m:"gielen", f:"giel", n:"gielt"}}
  },
  // the whole name: "the blue square", "das blaue Quadrat", "de roude Krees", "蓝色的正方形"
  name(L, s, c){
    const col = c && THEMES.colors.words.find(w => w.en === c);
    if (L === "en") return c ? `${c} ${s.en}` : s.en;
    if (L === "zh") return c ? `${col.zh}的${s.zh}` : s.zh;
    const [g, noun] = s[L];
    if (L === "de") return `${{m:"der", f:"die", n:"das"}[g]} ${c ? FORMES.colours[c].de + " " : ""}${noun}`;
    const keepN = w => /^[aeiouäéëöüdtzhn]/i.test(w); // Luxembourgish n-rule
    if (!c) return g === "m" ? `${keepN(noun) ? "den" : "de"} ${noun}` : `d'${noun}`;
    const adj = FORMES.colours[c].lb[g], a = g === "m" && !keepN(noun) ? adj.slice(0, -1) : adj;
    return `${{m:"de", f:"déi", n:"dat"}[g]} ${a} ${noun}`;
  },
  txt: {
    en: {find: x => `Find the ${x}!`, that: (y, x) => `That's the ${y}. Find the ${x}!`},
    de: {find: x => `Wo ist ${x}?`, that: (y, x) => `Das ist ${y}. Wo ist ${x}?`},
    lb: {find: x => `Wou ass ${x}?`}, // only written: the voice plays a recorded word (de Stär, d'Häerz, rout, blo…)
    zh: {find: x => `找到${x}！`, that: (y, x) => `这是${y}。找到${x}！`}
  }
};

registerGame({id:"formes", em:"🔺", name:"Formes", desc:"Cercle, carré, étoile…", multi:true,
  title:{en:"Shapes", de:"Formen", lb:"Formen", zh:"图形"},
  sub:{en:"Circle, square, star…", de:"Kreis, Quadrat, Stern…", lb:"Krees, Quadrat, Stär…", zh:"圆形、正方形、五角星……"}}, function () {
  const lvl = levelOf("formes"), combos = lvl >= 3, total = combos ? 8 : 6, n = [3, 4, 6, 6][lvl - 1];
  const COL = ["red", "blue", "green", "yellow"], hex = c => THEMES.colors.words.find(w => w.en === c).e;
  const pool = combos ? FORMES.shapes.flatMap(s => COL.map(c => ({s, c}))) : FORMES.shapes.map(s => ({s, c: null}));
  const lbRec = new Set(Object.values(THEMES).flatMap(t => t.words).filter(w => w.lb && w.lod).map(w => w.lb));
  // Luxembourgish voice: the recorded shape (de Stär, d'Häerz), or the recorded colour of a coloured shape
  const heard = t => t.c ? THEMES.colors.words.find(w => w.en === t.c).lb : lbRec.has(FORMES.name("lb", t.s)) ? FORMES.name("lb", t.s) : "";
  const vive = typeof fx !== "undefined" && fx.calm() ? "" : " vive";
  // shapes alone come in any colour: only the shape counts
  const draw = (t, L) => `<svg class="forme${vive}" viewBox="0 0 100 100" role="img" aria-label="${FORMES.name(L, t.s, t.c)}">
    <g fill="${hex(t.c || COL[rnd(COL.length)])}" stroke="#1B2D45" stroke-width="5" stroke-linejoin="round">${t.s.svg}</g></svg>`;
  // traps: the same colour or the same shape, so both words count
  const others = t => {
    if (!combos) return pick(pool.filter(x => x !== t), n - 1);
    const k = lvl === 3 ? 1 : 2, shape = pick(pool.filter(x => x.s === t.s && x !== t), k), colour = pick(pool.filter(x => x.c === t.c && x !== t), k);
    return shape.concat(colour, pick(pool.filter(x => x !== t && !shape.includes(x) && !colour.includes(x)), n - 1 - 2 * k));
  };
  const plans = {};
  const plan = L => plans[L] = plans[L] || pick(pool, total).map(t => ({t, o: others(t)}));

  function build(i){
    const L = langOf(), T = FORMES.txt[L], {t, o} = plan(L)[i], x = FORMES.name(L, t.s, t.c);
    const choice = y => ({html: draw(y, L), ok: y === t, label: FORMES.name(L, y.s, y.c)});
    // the question is always written, with the voice on top (Kezhan: « c'est l'occasion de lire »)
    const r = {lang: L, show: "🔍 " + T.find(`<b>${x}</b>`), word: `${t.c ? t.c + " " : ""}${t.s.en}`};
    if (L === "lb") return {...r, say: heard(t), choices: [t, ...o].map(y => ({...choice(y), sayWrong: heard(y) || " "}))};
    return {...r, say: T.find(x), choices: [t, ...o].map(y => ({...choice(y), sayWrong: T.that(FORMES.name(L, y.s, y.c), x)}))};
  }

  // each round is written when it starts, in the language of that moment
  const lazy = i => { let r = null; return new Proxy({}, {get: (_, k) => (r = r || build(i))[k]}); };
  runQuiz("formes", null, Array.from({length: total}, (_, i) => lazy(i)));
});
