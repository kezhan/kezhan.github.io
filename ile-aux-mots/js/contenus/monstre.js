/* L'Île aux Mots : contenus du jeu « monstre » (Le Monstre affamé) : textes en anglais, allemand, luxembourgeois et chinois,
   grammaire de chaque langue, dessins (aliments, monstre), sons drôles et style.
   Luxembourgeois vérifié sur lod.lu : « Gëff mer » (ginn), « wannechgelift », « Ech hätt gär », « fir d'éischt »,
   pluriels (Äppel, Banannen, Pijen, Eeër), adjectifs (de rouden Apel, déi rout Tomat, dat blot Ee), zwou devant un nom féminin. */
const MONSTRE_TXT = {
  title: {en:"The Hungry Monster", de:"Das hungrige Monster", lb:"D'Monster huet Honger", zh:"饿肚子的小怪兽"},
  sub: {en:"Feed the monster!", de:"Füttere das Monster!", lb:"Fidder d'Monster!", zh:"喂小怪兽吃东西！"},
  again: {en:"Again", de:"Nochmal", lb:"Nach eng Kéier", zh:"再听一次"},
  done: {en:"Done!", de:"Fertig!", lb:"Fäerdeg!", zh:"好了！"},
  // x: what the monster wants, already in the right case (German accusative, "den Käse")
  ask: {
    en: [x => `Give me ${x}!`, x => `I want ${x}!`, x => `I'm hungry! Give me ${x}!`, x => `Mmm, ${x}, please!`, x => `Feed me ${x}!`],
    de: [x => `Gib mir ${x}!`, x => `Ich will ${x}!`, x => `Ich habe Hunger! Gib mir ${x}!`, x => `Mmm, ${x}, bitte!`, x => `Ich möchte ${x}!`],
    lb: [x => `Gëff mer ${x}!`, x => `Ech wëll ${x}!`, x => `Ech hunn Honger! Gëff mer ${x}!`, x => `Mmm, ${x}, wannechgelift!`, x => `Ech hätt gär ${x}!`],
    zh: [x => `给我${x}！`, x => `我要${x}！`, x => `我饿了！给我${x}！`, x => `嗯，我想要${x}！`, x => `请给我${x}！`]
  },
  // level 3: two foods, in this order
  order: {
    en: [(a, b) => `First ${a}, then ${b}!`, (a, b) => `${a.charAt(0).toUpperCase() + a.slice(1)} first, and then ${b}!`],
    de: [(a, b) => `Zuerst ${a}, dann ${b}!`, (a, b) => `Erst ${a} und dann ${b}!`],
    lb: [(a, b) => `Fir d'éischt ${a}, dann ${b}!`, (a, b) => `Ech wëll fir d'éischt ${a} an dann ${b}!`],
    zh: [(a, b) => `先给我${a}，再给我${b}！`, (a, b) => `先给我${a}，然后给我${b}！`]
  },
  firstThe: {en: a => `No, no! First ${a}!`, de: a => `Nein, nein! Zuerst ${a}!`, lb: a => `Neen, neen! Fir d'éischt ${a}!`, zh: a => `不对不对！先给我${a}！`},
  // w in the nominative: "Das ist die Birne"
  yuck: {en: w => `Yuck! That's ${w}!`, de: w => `Igitt! Das ist ${w}!`, lb: w => `Eekleg! Dat ass ${w}!`, zh: w => `呸呸！这是${w}！`},
  yum: {
    en: ["Yummy!", "Mmm, delicious!", "Yum yum yum!", "Thank you!", "So tasty!", "Mmm, I love it!"],
    de: ["Lecker!", "Mmm, lecker!", "Mjam mjam!", "Danke!", "Köstlich!", "Hmm, das schmeckt!"],
    lb: ["Mmm, lecker!", "Merci!", "Dat schmaacht!", "Bravo!", "Hmm, dat ass gutt!", "Super lecker!"],
    zh: ["好吃！", "真好吃！", "谢谢你！", "嗯，好香啊！", "太美味了！", "吧唧吧唧，好吃！"]
  },
  burp: {en:["BURP!", "Oops! Excuse me!"], de:["RÜLPS!", "Ups! Entschuldigung!"], lb:["REPS!", "Pardon!"], zh:["嗝～", "哎呀，不好意思！"]},
  hungry: {en:"I'm hungry!", de:"Ich habe Hunger!", lb:"Ech hunn Honger!", zh:"我饿了！"},
  tickle: {
    en: ["Hee hee! That tickles!", "Ha ha ha! Stop it!"], de: ["Hihi! Das kitzelt!", "Haha! Hör auf!"],
    lb: ["Hihi! Dat kribbelt!", "Hahaha! Dat kribbelt esou!"], zh: ["嘻嘻，好痒！", "哈哈哈！别闹了！"]
  },
  belly: {en:"My tummy!", de:"Mein Bauch!", lb:"Mäi Bauch!", zh:"我的肚子！"},
  tooMany: {en:"Oh no! Too many! My tummy hurts!", de:"Oh nein! Zu viele! Mein Bauch tut weh!", lb:"Oh neen! Ze vill! Mäi Bauch deet wéi!", zh:"哎呀！太多了！我肚子疼！"},
  more: {en:"More, please!", de:"Mehr, bitte!", lb:"Nach méi, wannechgelift!", zh:"还要，还要！"},
  and: {en:"and", de:"und", lb:"an", zh:"和"} // Luxembourgish "an" follows the Eifel rule (a véier, an aacht)
};

// level 2: foods drawn in the colour asked; de and lb = gender for the article (pl = plural noun); colours right in every language
const MONSTRE_COLOR_FOODS = [
  {en:"apple", svg:"apple", de:"m", lb:"m", cols:["red","green","yellow"]},
  {en:"ice cream", svg:"icecream", de:"n", lb:"f", cols:["pink","brown","white","green","yellow","blue","purple"]},
  {en:"cake", svg:"cake", de:"m", lb:"m", cols:["white","brown","yellow","blue","green"]},
  {en:"egg", svg:"egg", de:"n", lb:"n", cols:["white","brown","blue","green","yellow","red"]},
  {en:"milk", svg:"milk", de:"f", lb:"f", cols:["white","pink","brown"]},
  {en:"grapes", svg:"grapes", de:"pl", lb:"pl", cols:["green","purple"]},
  {en:"carrot", svg:"carrot", de:"f", lb:"f", cols:["purple","yellow","white"]},
  {en:"tomato", svg:"tomato", de:"f", lb:"f", cols:["red","green","yellow"]},
  {en:"pear", svg:"pear", de:"f", lb:"f", cols:["green","yellow","brown"]},
  {en:"lemon", svg:"lemon", de:"f", lb:"f", cols:["yellow","green"]}
];
// Luxembourgish colour adjectives (lod.lu): masculine, feminine and plural, neuter
const MONSTRE_LB_ADJ = {
  red:["rouden","rout","rout"], blue:["bloen","blo","blot"], green:["gréngen","gréng","gréngt"], yellow:["gielen","giel","gielt"],
  pink:["rosaen","rosa","rosat"], purple:["mofen","mof","mooft"], white:["wäissen","wäiss","wäisst"], brown:["brongen","brong","brongt"]
};

// level 4: w = lexicon word; plurals; lb gender for zwee / zwou; zh classifier
const MONSTRE_COUNT = [
  {w:"apple", pl:"apples", de:"Äpfel", lb:"Äppel", g:"m", cl:"个"},
  {w:"banana", pl:"bananas", de:"Bananen", lb:"Banannen", g:"f", cl:"根"},
  {w:"strawberry", pl:"strawberries", de:"Erdbeeren", lb:"Äerdbieren", g:"f", cl:"个"},
  {w:"orange", pl:"oranges", de:"Orangen", lb:"Orangen", g:"f", cl:"个"},
  {w:"pear", pl:"pears", de:"Birnen", lb:"Bieren", g:"f", cl:"个"},
  {w:"cherries", pl:"cherries", de:"Kirschen", lb:"Kiischten", g:"f", cl:"颗"},
  {w:"egg", pl:"eggs", de:"Eier", lb:"Eeër", g:"n", cl:"个"},
  {w:"carrot", pl:"carrots", de:"Karotten", lb:"Muerten", g:"f", cl:"根"},
  {w:"tomato", pl:"tomatoes", de:"Tomaten", lb:"Tomaten", g:"f", cl:"个"},
  {w:"lemon", pl:"lemons", de:"Zitronen", lb:"Zitrounen", g:"f", cl:"个"},
  {w:"peach", pl:"peaches", de:"Pfirsiche", lb:"Pijen", g:"f", cl:"个"},
  {w:"mushroom", pl:"mushrooms", de:"Pilze", lb:"Champignonen", g:"m", cl:"个"},
  {w:"potato", pl:"potatoes", de:"Kartoffeln", lb:"Gromperen", g:"f", cl:"个"},
  {w:"cucumber", pl:"cucumbers", de:"Gurken", lb:"Concomberen", g:"f", cl:"根"},
  {w:"sandwich", pl:"sandwiches", de:"Sandwiches", lb:"Sandwichen", g:"m", cl:"个"},
  {w:"watermelon", pl:"watermelons", de:"Wassermelonen", lb:"Waassermelounen", g:"f", cl:"个"}
];

// the grammar of each language: German cases, Luxembourgish Eifel rule and adjective endings, Chinese classifiers
const MONSTRE_GRAM = (() => {
  const food = en => THEMES.food.words.find(w => w.en === en);
  // Luxembourgish Eifel rule: a final n stays only before a vowel or d, t, z, h, n
  const eifel = (w, next) => /n$/.test(w) && !/^[aeiouäéëèdtzhn]/i.test(next) ? w.slice(0, -1) : w;
  // "the banana"; German accusative unless nom (der Käse → den Käse)
  const the = (w, lang, nom) => lang === "en" ? "the " + w.en : lang === "de" && !nom ? w.de.replace(/^der /, "den ") : w[lang];
  // "the red apple": German weak endings, Luxembourgish de / déi / dat with the lod.lu adjective forms
  function coloured(f, c, g, lang, nom){
    if (lang === "en") return `the ${c.en} ${f.en}`;
    if (lang === "zh") return `${c.zh}的${f.zh}`;
    if (lang === "de") {
      const noun = f.de.replace(/^\S+ /, ""), fixed = /a$/.test(c.de); // rosa, lila never change
      const art = g.de === "m" ? (nom ? "der" : "den") : g.de === "n" ? "das" : "die";
      return `${art} ${fixed ? c.de : c.de + (g.de === "pl" || (g.de === "m" && !nom) ? "en" : "e")} ${noun}`;
    }
    const noun = f.lb.replace(/^(den |de |d')/, ""), [m, fe, n] = MONSTRE_LB_ADJ[c.en];
    if (g.lb === "m") return `${eifel("den", m)} ${eifel(m, noun)} ${noun}`;
    return g.lb === "n" ? `dat ${n} ${noun}` : `déi ${fe} ${noun}`;
  }
  // "three strawberries": 两 before a classifier, zwou before a feminine Luxembourgish noun
  function counted(n, c, lang){
    if (lang === "en") return `${numberWords(n)} ${c.pl}`;
    if (lang === "de") return `${numberIn(n, "de")} ${c.de}`;
    if (lang === "zh") return `${n === 2 ? "两" : numberIn(n, "zh")}${c.cl}${food(c.w).zh}`;
    return `${eifel(n === 2 && c.g === "f" ? "zwou" : numberIn(n, "lb"), c.lb)} ${c.lb}`;
  }
  const two = (a, b, lang) => a + (lang === "zh" ? "" : " ") + b; // two sentences in a row
  const andJoin = (p, lang) => p.length < 2 ? p[0] : lang === "zh" ? p.join(MONSTRE_TXT.and.zh) : `${p[0]} ${lang === "lb" ? eifel(MONSTRE_TXT.and.lb, p[1]) : MONSTRE_TXT.and[lang]} ${p[1]}`;
  return {the, coloured, counted, andJoin, two};
})();

// level 2 drawings, 64 x 64, filled with the colour c
const MONSTRE_SVG = (() => {
  const K = `stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"`, shine = (x, y) => `<ellipse cx="${x}" cy="${y}" rx="3.5" ry="6.5" fill="#fff" opacity=".5"/>`;
  const svg = s => `<svg viewBox="0 0 64 64" aria-hidden="true">${s}</svg>`;
  return {
    apple: c => svg(`<path d="M34 20c-2-6 1-11 6-13" stroke="#5D4037" stroke-width="4" fill="none" stroke-linecap="round"/>
      <path d="M32 20c-7-6-22-5-24 10-2 14 8 28 16 28 4 0 5-2 8-2s4 2 8 2c8 0 18-14 16-28-2-15-17-16-24-10z" fill="${c}" ${K}/>
      <path d="M38 12c6-6 14-4 16-2-5 6-12 6-16 2z" fill="#66BB6A" ${K} stroke-width="2"/>${shine(20, 31)}`),
    icecream: c => svg(`<path d="M20 34L32 61L44 34Z" fill="#E6A95A" ${K}/><path d="M25 39L38 50M40 39L28 53" stroke="#B9782F" stroke-width="2"/>
      <path d="M14 35c-4-14 6-27 18-27s22 13 18 27c-3 3-6-1-9 1-3 3-6 0-9 1-3 1-6-2-9 0-3 1-6-1-9-2z" fill="${c}" ${K}/>${shine(24, 19)}`),
    cake: c => svg(`<rect x="10" y="30" width="44" height="26" rx="5" fill="#F6D9A8" ${K}/><path d="M11 44H53" stroke="#E1A96B" stroke-width="3"/>
      <path d="M10 36c0-10 10-14 22-14s22 4 22 14v2c-3 5-6 5-8 0-2 5-6 5-8 0-2 5-6 5-8 0-2 5-6 5-8 0-2 5-5 5-8 0z" fill="${c}" ${K}/>
      <circle cx="32" cy="17" r="6" fill="#E53935" ${K} stroke-width="2.5"/><path d="M32 11c1-4 4-6 7-6" stroke="#2E7D32" stroke-width="2.5" fill="none"/>`),
    egg: c => svg(`<ellipse cx="32" cy="36" rx="17" ry="22" fill="${c}" ${K}/>${shine(25, 28)}`),
    milk: c => svg(`<path d="M36 3L41 30" stroke="#FF6F59" stroke-width="4" stroke-linecap="round"/><path d="M16 9L21 58H43L48 9Z" fill="#BFE3F5" ${K}/>
      <path d="M18.3 22L21.9 55.5H42.1L45.7 22Z" fill="${c}"/><path d="M18.3 22H45.7" stroke="#1B2D45" stroke-width="2"/>`),
    grapes: c => svg(`<path d="M32 12C32 7 35 4 39 2" stroke="#5D4037" stroke-width="3" fill="none"/><path d="M34 10c6-4 12-2 14 1-6 3-10 2-14-1z" fill="#66BB6A" ${K} stroke-width="2"/>`
      + [[22,20],[32,18],[42,20],[17,31],[27,30],[37,30],[47,31],[22,41],[32,41],[42,41],[27,51],[37,51]].map(([x, y]) => `<circle cx="${x}" cy="${y}" r="7" fill="${c}" ${K} stroke-width="2.5"/>`).join("")),
    carrot: c => svg(`<path d="M30 18L25 4M34 18L36 3M38 19L47 7" stroke="#43A047" stroke-width="4" stroke-linecap="round"/>
      <path d="M19 20Q32 12 46 22L27 60Q24 62 22 58Z" fill="${c}" ${K}/><path d="M24 31h8M26 41h6M25 50h4" stroke="#1B2D45" stroke-width="2" opacity=".4"/>`),
    tomato: c => svg(`<circle cx="32" cy="37" r="22" fill="${c}" ${K}/>${shine(22, 31)}
      <path d="M32 15l4 6 7-2-4 6 6 4h-8l-5 5-3-7-7 1 4-6-5-5 7 1z" fill="#2E7D32" ${K} stroke-width="2"/>`),
    pear: c => svg(`<path d="M32 12c0-4 2-7 5-9" stroke="#5D4037" stroke-width="3" fill="none" stroke-linecap="round"/>
      <path d="M32 12c-6 0-8 6-8 12 0 6-10 10-10 22 0 9 8 14 18 14s18-5 18-14c0-12-10-16-10-22 0-6-2-12-8-12z" fill="${c}" ${K}/>${shine(23, 42)}`),
    lemon: c => svg(`<path d="M5 33Q10 29 13 26C18 15 46 15 51 26Q54 29 59 33Q54 37 51 40C46 51 18 51 13 40Q10 37 5 33Z" fill="${c}" ${K}/>${shine(22, 28)}`)
  };
})();

// the monster, an original drawing; each moving part has its class (arms, tuft, eyes, pupils, brows, cheeks, mouth, tongue, nose, belly)
const MONSTRE_SKINS = ["#9B7BF7", "#5CC97B", "#FF9A5C", "#4FB3F6", "#F77FB8", "#35C2B0"];
const MONSTRE_BODY = (() => {
  const INK = "#1B2D45", BODY = "M120 38C180 38 216 82 216 130C216 182 174 212 120 212C66 212 24 182 24 130C24 82 60 38 120 38Z";
  const LINE = `stroke="${INK}" stroke-width="4" stroke-linejoin="round"`;
  const arm = (d, c, cls) => `<g class="mo-arm ${cls}"><path d="${d}" stroke="${INK}" stroke-width="18" stroke-linecap="round" fill="none"/><path d="${d}" stroke="${c}" stroke-width="10" stroke-linecap="round" fill="none"/></g>`;
  const eye = x => `<g class="mo-eye"><circle cx="${x}" cy="92" r="24" fill="#fff" ${LINE}/><g class="mo-pupil"><circle cx="${x}" cy="95" r="11" fill="${INK}"/><circle cx="${x - 4}" cy="90" r="4" fill="#fff"/></g></g>`;
  const brow = d => `<path class="mo-brow" d="${d}" stroke="${INK}" stroke-width="6" stroke-linecap="round" fill="none"/>`;
  return c => `<svg class="mo-svg" viewBox="0 -10 240 234" aria-hidden="true"><g class="mo-fat"><g class="mo-breath"><g class="mo-all">
  ${arm("M46 142Q14 134 16 100", c, "mo-armL")}${arm("M194 142Q226 134 224 100", c, "mo-armR")}
  <ellipse cx="84" cy="208" rx="26" ry="12" fill="${c}" ${LINE}/><ellipse cx="156" cy="208" rx="26" ry="12" fill="${c}" ${LINE}/>
  <path d="M74 60Q48 24 66 10Q74 36 96 48Z" fill="#FFE08A" ${LINE}/><path d="M166 60Q192 24 174 10Q166 36 144 48Z" fill="#FFE08A" ${LINE}/>
  <path class="mo-tuft" d="M100 46L108 18L118 40L126 14L134 40L144 20L146 48Z" fill="${c}" ${LINE}/>
  <path d="${BODY}" fill="${c}" stroke="${INK}" stroke-width="5"/><path class="mo-sick" d="${BODY}" fill="#8BC34A" opacity="0"/>
  <ellipse class="mo-belly" cx="120" cy="186" rx="54" ry="20" fill="#fff" opacity=".35"/>
  ${eye(86)}${eye(154)}${brow("M64 60Q86 50 106 60")}${brow("M134 60Q154 50 176 60")}
  <circle class="mo-cheek" cx="58" cy="138" r="14" fill="#FF6B8B" opacity=".7"/><circle class="mo-cheek" cx="182" cy="138" r="14" fill="#FF6B8B" opacity=".7"/>
  <g class="mo-mouth"><ellipse cx="120" cy="150" rx="40" ry="24" fill="#5B1330" ${LINE}/><ellipse class="mo-tongue" cx="120" cy="163" rx="20" ry="8" fill="#FF6F8E"/>
    <path d="M103 127l7 11 7-11ZM123 127l7 11 7-11Z" fill="#fff" stroke="${INK}" stroke-width="2" stroke-linejoin="round"/></g>
  <ellipse class="mo-nose" cx="120" cy="117" rx="10" ry="7" fill="#FF7FA5" stroke="${INK}" stroke-width="3"/>
</g></g></g></svg>`;
})();

// the monster's funny sounds (Web Audio): burp, munch, boing, honk, giggle; silent in #test
const MONSTRE_SND = (() => {
  let actx = null;
  function audio(){ if (TEST) return null; try { actx = actx || new (window.AudioContext || window.webkitAudioContext)(); if (actx.state === "suspended") actx.resume(); return actx; } catch(e) { return null; } }
  // one voice gliding from f0 to f1; wob = wobble speed (burp, raspberry); lp = muffled
  function glide(type, f0, f1, dur, vol = .2, at = 0, wob = 0, lp = 0){
    const a = audio(); if (!a) return;
    const t = a.currentTime + at, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    if (wob) { const l = a.createOscillator(), d = a.createGain(); l.frequency.value = wob; d.gain.value = f0 * .3; l.connect(d); d.connect(o.frequency); l.start(t); l.stop(t + dur); }
    let out = g;
    if (lp) { const f = a.createBiquadFilter(); f.type = "lowpass"; f.frequency.value = lp; g.connect(f); out = f; }
    o.connect(g); out.connect(a.destination); o.start(t); o.stop(t + dur + .05);
  }
  function hiss(dur, at = 0, vol = .3, freq = 800){
    const a = audio(); if (!a) return;
    const n = Math.floor(a.sampleRate * dur), b = a.createBuffer(1, n, a.sampleRate), d = b.getChannelData(0);
    for (let k = 0; k < n; k++) d[k] = (Math.random() * 2 - 1) * (1 - k / n);
    const s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = b; f.type = "lowpass"; f.frequency.value = freq; g.gain.value = vol;
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(a.currentTime + at);
  }
  return {
    munch: () => [0, .2, .4].forEach(t => hiss(.09, t, .45, 650)), gulp: () => glide("sine", 520, 130, .22, .25),
    burp: () => glide("sawtooth", 110, 58, .75, .25, 0, 26, 700), yuck: () => glide("sawtooth", 220, 140, .55, .2, 0, 32, 1200),
    boing: () => { glide("sine", 160, 520, .12, .22); glide("sine", 520, 240, .4, .18, .12, 11); }, whoosh: () => glide("sine", 300, 1000, .25, .1),
    spit: () => { glide("square", 900, 200, .18, .08); hiss(.15, 0, .3, 2500); }, pop: () => glide("square", 700, 1400, .06, .08),
    giggle: () => [880, 1050, 920, 1120, 990].forEach((f, k) => glide("sine", f, f * .8, .09, .14, k * .1)),
    honk: () => { glide("square", 520, 490, .14, .1, 0, 0, 1500); glide("square", 440, 410, .22, .1, .16, 0, 1500); },
    rumble: () => glide("sine", 75, 48, .9, .35, 0, 9), heart: () => [0, .12, .24].forEach((t, k) => glide("triangle", 800 + k * 200, 1300 + k * 200, .12, .08, t))
  };
})();

// its look: the monster's parts move with transform and opacity only; nothing moves in #test (.mo-calm)
const MONSTRE_CSS = `
.mo{display:flex; flex-direction:column; gap:12px}
.mo-bubble{position:relative; align-self:center; max-width:100%; min-height:58px; background:#fff; border:3px solid var(--ink); border-radius:22px; box-shadow:3px 4px 0 var(--ink); padding:10px 18px; font-family:var(--display); font-size:clamp(20px,5.6vw,28px); line-height:1.2; text-align:center; display:flex; align-items:center; justify-content:center}
.mo-bubble::after{content:""; position:absolute; left:50%; bottom:-12px; width:20px; height:20px; margin-left:-10px; background:#fff; border-right:3px solid var(--ink); border-bottom:3px solid var(--ink); transform:rotate(45deg)}
.mo-bubble b{font-weight:600} .mo-ear{font-size:36px; display:inline-block; animation:mo-ear 1.2s ease-in-out infinite}
.mo-stage{display:flex; justify-content:center; padding-top:8px} .mo-box{position:relative; width:min(230px,62vw); cursor:pointer}
.mo-svg{display:block; width:100%; height:auto; overflow:visible}
.mo-svg g,.mo-svg path,.mo-svg ellipse,.mo-svg circle{transform-box:fill-box; transform-origin:50% 50%}
.mo-svg .mo-fat,.mo-svg .mo-breath,.mo-svg .mo-all{transform-origin:50% 100%} .mo-svg .mo-fat{transition:transform .45s cubic-bezier(.3,1.6,.5,1)}
.mo-svg .mo-breath{animation:mo-breathe 2.8s ease-in-out infinite} .mo-svg .mo-eye{animation:mo-blink 4.4s infinite}
.mo-svg .mo-mouth{transform-origin:50% 0; transform:scaleY(.5); transition:transform .14s}
.mo-svg .mo-armL{transform-origin:100% 100%; animation:mo-waveL 3.2s ease-in-out infinite} .mo-svg .mo-armR{transform-origin:0 100%; animation:mo-waveR 3.2s ease-in-out infinite}
.mo-svg .mo-tuft{transform-origin:50% 100%; animation:mo-tuft 1.9s ease-in-out infinite}
@keyframes mo-breathe{50%{transform:scale(1.03,.97)}} @keyframes mo-blink{0%,92%,100%{transform:scaleY(1)} 95%{transform:scaleY(.08)}}
@keyframes mo-waveL{50%{transform:rotate(14deg)}} @keyframes mo-waveR{50%{transform:rotate(-14deg)}}
@keyframes mo-tuft{50%{transform:skewX(10deg)}} @keyframes mo-ear{50%{transform:scale(1.15) rotate(-8deg)}}
.mo-calm,.mo-calm *{animation:none!important; transition:none!important}
.mo-tray{display:flex; flex-wrap:wrap; justify-content:center; gap:8px; align-self:center}
.mo-food{position:relative; width:72px; min-height:80px; padding:4px 2px; background:#fff; border:3px solid var(--ink); border-radius:18px; box-shadow:3px 4px 0 var(--ink); font-size:44px; line-height:1; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:2px; touch-action:none}
.mo-food:active{transform:scale(.92)} .mo-food svg{width:52px; height:52px; display:block}
.mo-food .w{font-family:var(--display); font-size:12px; font-weight:600; line-height:1.1; text-align:center} .mo-food .w:empty{display:none}
.mo-food.mo-drag{z-index:30; box-shadow:8px 12px 0 rgba(27,45,69,.3)}
.mo-done{font-family:var(--display); font-size:22px; font-weight:700; background:var(--leaf); color:#fff; min-height:64px; padding:10px 22px}
.mo-fly{position:fixed; left:0; top:0; z-index:60; pointer-events:none; font-size:46px; line-height:1; width:56px; text-align:center; will-change:transform,opacity}
.mo-fly svg{display:block; width:100%; height:auto}
.mo-tummy{position:absolute; left:50%; top:80%; width:44%; transform:translate(-50%,-50%); display:flex; flex-wrap:wrap; justify-content:center; font-size:15px; line-height:1; pointer-events:none}
.mo-tummy svg{width:15px; height:15px}
.mo-pop{position:absolute; left:50%; top:-10px; transform:translateX(-50%); width:max-content; max-width:min(300px,82vw); background:var(--sun); border:3px solid var(--ink); border-radius:16px; padding:4px 12px; font-family:var(--display); font-weight:700; font-size:18px; text-align:center; pointer-events:none; z-index:5}
.mo-burp{position:absolute; left:50%; top:52%; transform:translate(-50%,0); font-family:var(--display); font-size:34px; font-weight:800; color:#fff; -webkit-text-stroke:2px var(--ink); pointer-events:none; white-space:nowrap}
.mo-num{position:absolute; right:0; top:48%; font-family:var(--display); font-size:44px; font-weight:800; color:var(--coral); -webkit-text-stroke:2px var(--ink); display:flex; flex-direction:column; align-items:center; pointer-events:none}
.mo-num small{font-size:16px; -webkit-text-stroke:0; color:var(--ink)}
`;
