/* L'Île aux Mots : jeu « Contraires », dans la langue apprise (anglais, allemand, luxembourgeois, chinois ; jamais de français).
   1 : trouver le mot, 3 images, paires faciles · 2 : toutes les paires, 4 images · 3 : le contraire d'un mot · 4 : réponses écrites, à lire.
   La question est toujours écrite, mot en gras, en plus de la voix (Kezhan : « c'est l'occasion de lire »).
   Luxembourgeois vérifié sur lod.lu : GROUSS2 et GEIGENDEEL1 (« de Géigendeel vu grouss ass kleng »), KLENG1, WAARM1, KAL1, FROU1,
   TRAUREG1, SEIER1, LUES1, HELL1, DAISCHTER1, UEWEN1, ENNEN1, OP1, ZOU1, LENKS2, RIETS2, SEISS1, SAUER1.
   « lues » veut dire lent et doucement : pas de paire fort / doucement en luxembourgeois.
   Chaque manche s'écrit au moment où elle commence : un drapeau touché en cours de partie vaut dès la manche suivante. */
addStyle(`.choice.ok .kontra.vive{display:inline-block; animation:kontraFlip .8s ease-out}
@keyframes kontraFlip{0%{transform:none} 45%{transform:rotateY(180deg) scale(1.3)} 100%{transform:rotateY(360deg)}}`);

// the two pictures, then the pair in each language; a language without a clear pair of its own has none.
// g: pairs close in meaning (happy and laugh), never offered as each other's wrong answers
const CONTRAIRES = [
  {e:["🐘","🐭"], easy:1, g:"size", en:["big","small"], de:["groß","klein"], lb:["grouss","kleng"], zh:["大","小"]},
  {e:["🔥","🧊"], easy:1, en:["hot","cold"], de:["heiß","kalt"], lb:["waarm","kal"], zh:["热","冷"]},
  {e:["😀","😢"], easy:1, g:"mood", en:["happy","sad"], de:["fröhlich","traurig"], lb:["frou","traureg"], zh:["开心","难过"]},
  {e:["🐇","🐢"], easy:1, en:["fast","slow"], de:["schnell","langsam"], lb:["séier","lues"], zh:["快","慢"]},
  {e:["☀️","🌙"], easy:1, g:"light", en:["day","night"], de:["hell","dunkel"], lb:["hell","däischter"], zh:["白天","黑夜"]},
  {e:["⬆️","⬇️"], easy:1, en:["up","down"], de:["oben","unten"], lb:["uewen","ënnen"], zh:["上","下"]},
  {e:["🔓","🔒"], easy:1, en:["open","closed"], de:["offen","zu"], lb:["op","zou"], zh:["开","关"]},
  {e:["📢","🤫"], en:["loud","quiet"], de:["laut","leise"], zh:["大声","小声"]},
  {e:["🦒","🐁"], g:"size", en:["tall","short"], zh:["高","矮"]}, // German and Luxembourgish say groß / grouss, already big
  {e:["⬛","⬜"], g:"light", en:["black","white"], de:["schwarz","weiß"], lb:["schwaarz","wäiss"], zh:["黑","白"]},
  {e:["⬅️","➡️"], en:["left","right"], de:["links","rechts"], lb:["lénks","riets"], zh:["左","右"]},
  {e:["😆","😭"], g:"mood", en:["laugh","cry"], de:["lachen","weinen"], lb:["laachen","kräischen"], zh:["笑","哭"]},
  {e:["🍬","🍋"], en:["sweet","sour"], de:["süß","sauer"], lb:["séiss","sauer"], zh:["甜","酸"]}
];
// Luxembourgish n-rule: "vun" keeps its n before a vowel or d, t, z, h, n (vun uewen, vu grouss)
const CONTRAIRES_VUN = w => /^[aeiouäéëöüdtzhn]/i.test(w) ? "vun" : "vu";
// w: the word; html: written on screen, in bold
const CONTRAIRES_B = (w, html) => html ? `<b>${w}</b>` : w;
const CONTRAIRES_TXT = {
  en: {find: (w, h) => `Find: ${CONTRAIRES_B(w, h)}!`, that: (x, w) => `That's ${x}. Find: ${w}!`,
    opp: (w, h) => `What is the opposite of ${CONTRAIRES_B(w, h)}?`, yes: (a, b) => `Yes! ${a} and ${b}!`},
  de: {find: (w, h) => `Zeig mir: ${CONTRAIRES_B(w, h)}!`, that: (x, w) => `Das heißt ${x}. Zeig mir: ${w}!`,
    opp: (w, h) => `Was ist das Gegenteil von ${CONTRAIRES_B(w, h)}?`, yes: (a, b) => `Ja! ${a} und ${b}!`},
  // Luxembourgish is only written: the voice plays the lod.lu recording when the word is in the lexicon
  lb: {find: (w, h) => `Weis mer: ${CONTRAIRES_B(w, h)}!`, opp: (w, h) => `Wat ass de Géigendeel ${CONTRAIRES_VUN(w)} ${CONTRAIRES_B(w, h)}?`},
  zh: {find: (w, h) => `找一找：${CONTRAIRES_B(w, h)}！`, that: (x, w) => `这是${x}。找一找：${w}！`,
    opp: (w, h) => `${CONTRAIRES_B(w, h)}的反义词是什么？`, yes: (a, b) => `对了！${a}和${b}！`}
};

registerGame({id:"contraires", em:"↔️", name:"Contraires", desc:"Grand, petit, chaud, froid", multi:true,
  title:{en:"Opposites", de:"Gegenteile", lb:"Géigendeeler", zh:"反义词"},
  sub:{en:"Big, small, hot, cold", de:"Groß, klein, heiß, kalt", lb:"Grouss, kleng, waarm, kal", zh:"大和小，冷和热"}}, function () {
  const lvl = levelOf("contraires"), total = 8, n = lvl === 1 ? 3 : 4;
  // Luxembourgish words that have a recording (a longer text would play the first lexicon word found inside it: "grouss" holds "gro")
  const lbRec = new Set(Object.values(THEMES).flatMap(t => t.words).filter(w => w.lb && w.lod).map(w => w.lb));
  const heard = w => lbRec.has(w) ? w : "";
  const vive = typeof fx !== "undefined" && fx.calm() ? "" : " vive";
  // the cards of a language, {e, w, opp}, made once so that the rounds compare the same objects
  const decks = {}, plans = {};
  const cardsOf = L => decks[L] = decks[L] || CONTRAIRES.filter(p => p[L] && (lvl > 1 || p.easy))
    .flatMap(p => [{e: p.e[0], w: p[L][0], opp: p[L][1], g: p.g}, {e: p.e[1], w: p[L][1], opp: p[L][0], g: p.g}]);
  const plan = L => plans[L] = plans[L] || pick(cardsOf(L), total);

  function build(i){
    const L = langOf(), T = CONTRAIRES_TXT[L], cards = cardsOf(L), c = plan(L)[i % plan(L).length];
    const opp = cards.find(x => x.w === c.opp), lb = L === "lb";
    const face = x => `<span class="kontra${vive}">${x.e}</span>`;
    const others = pick(cards.filter(x => x !== c && x !== opp && !(c.g && x.g === c.g)), n - 1);
    // the question is always written, with the voice on top (Kezhan: « c'est l'occasion de lire »)
    if (lvl <= 2) // find the word
      return {lang: L, say: lb ? heard(c.w) : T.find(c.w), show: "👀 " + T.find(c.w, true), word: c.w,
        choices: [c, ...others].map(x => ({html: face(x), ok: x === c, label: x.w, sayWrong: lb ? heard(x.w) || " " : T.that(x.w, c.w)}))};
    // level 4: the answers are written words, to read
    return {lang: L, say: lb ? heard(c.w) : T.opp(c.w), visual: c.e, word: `${c.w}/${opp.w}`,
      show: "↔️ " + T.opp(c.w, true), praise: lb ? heard(opp.w) : T.yes(c.w, opp.w),
      choices: [opp, ...others].map(x => ({html: lvl >= 4 ? x.w : face(x), small: lvl >= 4, ok: x === opp, label: lvl >= 4 ? "" : x.w,
        sayWrong: lb ? heard(x.w) || " " : lvl >= 4 ? x.w : undefined}))};
  }

  // each round is written when it starts, in the language of that moment
  const lazy = i => { let r = null; return new Proxy({}, {get: (_, k) => (r = r || build(i))[k]}); };
  runQuiz("contraires", null, Array.from({length: total}, (_, i) => lazy(i)));
});
