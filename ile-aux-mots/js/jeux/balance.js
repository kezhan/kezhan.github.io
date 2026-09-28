/* L'Île aux Mots : jeu « La balance » (Heavy or light?).
   A two-pan balance drawn in SVG, with a little face on top. It waits on two stools; the child answers,
   the stools slide away and the balance tips with a wobble (the light thing hops, the heavy one lands with a thud).
   1: two things far apart (elephant and mouse): which one is heavy? which one is light? (touch a pan; picture, icon and voice)
   2: two things close together, or misleading (a big balloon, a small stone): which is heavier / lighter? guess, then it tips
   3: iron weights with numbers, 2 or 3 per pan, sums differing by 0 to 2: which side is heavier / lighter, or the same?
   4: balance it: 24 kg on one pan, cubes of 3 kg; how many cubes? then two weights to add first,
      or a weight already on the cubes' pan (subtract, then divide). The cubes fall one by one, counted aloud.
   English, German, Luxembourgish or Chinese, never French. German: nominative after "als"; Chinese: 比 for comparisons, 两 before 公斤.
   Luxembourgish checked on lod.lu (API, 28/09/2026): schwéier, liicht, méi (comparative), ewéi (than), Säit (f), gläich, béid,
   Wierfel (plural Wierfelen, n-rule form Wierfele), weien ("dëse Brot weit 740 Gramm"), Wo (f, the scales), Gläichgewiicht (n), Kilo,
   kippen, Ballon (m), Steen (m), Fieder (f). Eifel rule kept: "si méi", "si béid", "Wierfelen?" at the end of a sentence.
   To have reread by a Luxembourgish speaker: "gläich schwéier" (as heavy), "D'Wo soll am Gläichgewiicht sinn!", "Kuck, wéi d'Wo kippt!",
   "Lénks … Kilo, riets … Kilo", "Wien/Wat ass méi schwéier: … oder …?", "Grouss ass net ëmmer schwéier!". */
(() => {
const ID = "balance", INK = "#1B2D45";
const LG = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
const zk = n => n === 2 ? "两" : String(n); // 两公斤, not 二公斤
const sum = a => a.reduce((s, x) => s + x, 0);

const TX = {
  title: {en: "Heavy or light?", de: "Schwer oder leicht?", lb: "Schwéier oder liicht?", zh: "谁重谁轻？"},
  sub: {en: "Watch the balance tip!", de: "Schau, wie die Waage kippt!", lb: "Kuck, wéi d'Wo kippt!", zh: "看看天平往哪边沉！"},
  again: {en: "Again", de: "Nochmal", lb: "Nach eng Kéier", zh: "再听一次"},
  // level 1: who (two animals) or what
  q1: {
    h: {en: () => "Which one is heavy?", de: who => who ? "Wer ist schwer?" : "Was ist schwer?", lb: who => who ? "Wien ass schwéier?" : "Wat ass schwéier?", zh: () => "哪个重？"},
    l: {en: () => "Which one is light?", de: who => who ? "Wer ist leicht?" : "Was ist leicht?", lb: who => who ? "Wien ass liicht?" : "Wat ass liicht?", zh: () => "哪个轻？"}
  },
  heavy: {en: x => `The ${x} is heavy!`, de: x => `${cap(x)} ist schwer!`, lb: x => `${cap(x)} ass schwéier!`, zh: x => `${x}很重！`},
  light: {en: x => `The ${x} is light!`, de: x => `${cap(x)} ist leicht!`, lb: x => `${cap(x)} ass liicht!`, zh: x => `${x}很轻！`},
  // level 2: the two names in the question
  q2: {
    h: {en: (a, b) => `Which is heavier, the ${a} or the ${b}?`, de: (a, b, who) => `${who ? "Wer" : "Was"} ist schwerer: ${a} oder ${b}?`,
        lb: (a, b, who) => `${who ? "Wien" : "Wat"} ass méi schwéier: ${a} oder ${b}?`, zh: (a, b) => `${a}和${b}，哪个更重？`},
    l: {en: (a, b) => `Which is lighter, the ${a} or the ${b}?`, de: (a, b, who) => `${who ? "Wer" : "Was"} ist leichter: ${a} oder ${b}?`,
        lb: (a, b, who) => `${who ? "Wien" : "Wat"} ass méi liicht: ${a} oder ${b}?`, zh: (a, b) => `${a}和${b}，哪个更轻？`}
  },
  heavier: {en: (h, l) => `The ${h} is heavier than the ${l}!`, de: (h, l) => `${cap(h)} ist schwerer als ${l}!`, lb: (h, l) => `${cap(h)} ass méi schwéier ewéi ${l}!`, zh: (h, l) => `${h}比${l}重！`},
  lighter: {en: (l, h) => `The ${l} is lighter than the ${h}!`, de: (l, h) => `${cap(l)} ist leichter als ${h}!`, lb: (l, h) => `${cap(l)} ass méi liicht ewéi ${h}!`, zh: (l, h) => `${l}比${h}轻！`},
  big: {en: "Big is not always heavy!", de: "Groß ist nicht immer schwer!", lb: "Grouss ass net ëmmer schwéier!", zh: "大的不一定重！"},
  // level 3: weights with numbers
  q3: {
    h: {en: "Which side is heavier?", de: "Welche Seite ist schwerer?", lb: "Wéi eng Säit ass méi schwéier?", zh: "哪边更重？"},
    l: {en: "Which side is lighter?", de: "Welche Seite ist leichter?", lb: "Wéi eng Säit ass méi liicht?", zh: "哪边更轻？"}
  },
  orSame: {en: "Or do they weigh the same?", de: "Oder sind beide gleich schwer?", lb: "Oder si béid gläich schwéier?", zh: "还是一样重？"},
  same: {en: "The same", de: "Gleich schwer", lb: "Gläich schwéier", zh: "一样重"},
  kgHeavier: {en: (a, b) => `${a} kilos is heavier than ${b} kilos!`, de: (a, b) => `${a} Kilo sind schwerer als ${b} Kilo!`,
              lb: (a, b) => `${a} Kilo si méi schwéier ewéi ${b} Kilo!`, zh: (a, b) => `${zk(a)}公斤比${zk(b)}公斤重！`},
  kgLighter: {en: (a, b) => `${a} kilos is lighter than ${b} kilos!`, de: (a, b) => `${a} Kilo sind leichter als ${b} Kilo!`,
              lb: (a, b) => `${a} Kilo si méi liicht ewéi ${b} Kilo!`, zh: (a, b) => `${zk(a)}公斤比${zk(b)}公斤轻！`},
  kgSame: {en: a => `${a} kilos on each side: they weigh the same!`, de: a => `${a} Kilo auf jeder Seite: gleich schwer!`,
           lb: a => `Lénks ${a} Kilo, riets ${a} Kilo: gläich schwéier!`, zh: a => `两边都是${zk(a)}公斤，一样重！`},
  // level 4: balance it with cubes
  q4: {en: "How many cubes?", de: "Wie viele Würfel?", lb: "Wéi vill Wierfelen?", zh: "要放几个方块？"},
  cube: {en: c => `One cube weighs ${c} kilos. Make the scale balance!`, de: c => `Ein Würfel wiegt ${c} Kilo. Bring die Waage ins Gleichgewicht!`,
         lb: c => `E Wierfel weit ${c} Kilo. D'Wo soll am Gläichgewiicht sinn!`, zh: c => `一个方块重${zk(c)}公斤。让天平平衡！`},
  formula: {en: (p, n, c, t) => `${p ? p + " plus " : ""}${n} times ${c} is ${t}.`, de: (p, n, c, t) => `${p ? p + " plus " : ""}${n} mal ${c} ist ${t}.`,
            lb: (p, n, c, t) => `${p ? p + " plus " : ""}${n} mol ${c} ass ${t}.`, zh: (p, n, c, t) => `${p ? p + "加" : ""}${n}乘${c}等于${t}。`},
  balanced: {en: "Balanced!", de: "Gleichgewicht!", lb: "Gläichgewiicht!", zh: "平衡了！"},
  tooLight: {en: s => `${s} kilos: too light!`, de: s => `${s} Kilo: zu leicht!`, lb: s => `${s} Kilo: ze liicht!`, zh: s => `${zk(s)}公斤，太轻了！`},
  tooHeavy: {en: s => `${s} kilos: too heavy!`, de: s => `${s} Kilo: zu schwer!`, lb: s => `${s} Kilo: ze schwéier!`, zh: s => `${zk(s)}公斤，太重了！`}
};

// things to weigh: the lexicon gives the words with their article (and the Luxembourgish recording); three more for the surprises
const EXTRA = {
  balloon: {e: "🎈", en: "balloon", de: "der Luftballon", lb: "de Ballon", zh: "气球"},
  stone: {e: "🪨", en: "stone", de: "der Stein", lb: "de Steen", zh: "石头"},
  feather: {e: "🪶", en: "feather", de: "die Feder", lb: "d'Fieder", zh: "羽毛"}
};
const KG = {elephant: 5000, cow: 600, horse: 500, bear: 300, car: 1200, pig: 100, lion: 190, tiger: 200, giraffe: 1000, zebra: 350, bus: 12000,
  sheep: 70, kangaroo: 60, dolphin: 150, penguin: 25, dog: 20, monkey: 10, cat: 4, rabbit: 2, chicken: 2.5, duck: 1.2, bird: .03, frog: .05,
  mouse: .02, butterfly: .0005, watermelon: 5, pineapple: 1.5, bread: .5, apple: .2, banana: .12, lemon: .1, egg: .06, strawberry: .02,
  bike: 12, key: .02, balloon: .004, stone: 2, feather: .001};
const ANIMALS = ["elephant", "cow", "horse", "bear", "pig", "lion", "tiger", "giraffe", "zebra", "sheep", "kangaroo", "dolphin", "penguin",
  "dog", "monkey", "cat", "rabbit", "chicken", "duck", "bird", "frog", "mouse", "butterfly"];
const item = en => {
  const w = EXTRA[en] || Object.values(THEMES).map(t => t.words.find(x => x.en === en)).find(Boolean) || {e: "❓", en};
  return {...w, kg: KG[en] || 1, animal: ANIMALS.includes(en)};
};
const L1_HEAVY = ["elephant", "cow", "horse", "bear", "car", "pig", "lion"];
const L1_LIGHT = ["mouse", "bird", "frog", "butterfly", "strawberry", "egg", "apple", "rabbit"];
const L2_PAIRS = [["dog", "cat"], ["dog", "rabbit"], ["cow", "sheep"], ["horse", "pig"], ["lion", "monkey"], ["bear", "dog"], ["giraffe", "zebra"],
  ["pig", "duck"], ["tiger", "cat"], ["chicken", "bird"], ["sheep", "chicken"], ["kangaroo", "rabbit"], ["dolphin", "penguin"], ["watermelon", "apple"],
  ["pineapple", "lemon"], ["apple", "strawberry"], ["banana", "egg"], ["bread", "egg"], ["car", "bike"], ["bus", "car"]];
// the lighter one is drawn big: big is not always heavy
const L2_SURPRISE = [["stone", "balloon"], ["key", "balloon"], ["apple", "balloon"], ["stone", "feather"]];

/* funny sounds, made with the Web Audio API (silent in the recette) */
const snd = (() => {
  let c = null, noise = null;
  const ctx = () => { if (TEST) return null; try { c = c || new (window.AudioContext || window.webkitAudioContext)(); if (c.state === "suspended") c.resume(); return c; } catch(e) { return null; } };
  function note(type, f0, f1, dur, vol = .15, at = 0, lfo){
    const a = ctx(); if (!a) return;
    const t = a.currentTime + at, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination);
    if (lfo) { const l = a.createOscillator(), lg = a.createGain(); l.frequency.value = lfo[0]; lg.gain.value = lfo[1]; l.connect(lg); lg.connect(o.frequency); l.start(t); l.stop(t + dur); }
    o.start(t); o.stop(t + dur + .05);
  }
  function hiss(dur, vol, at, f0, f1){
    const a = ctx(); if (!a) return;
    if (!noise) { noise = a.createBuffer(1, a.sampleRate, a.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    const t = a.currentTime + at, s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = noise; f.type = "bandpass"; f.frequency.setValueAtTime(f0, t); f.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(t); s.stop(t + dur);
  }
  return {
    creak: () => note("sawtooth", 150, 105, .5, .04, 0, [16, 10]),
    thud: big => { hiss(.2, big ? .7 : .4, 0, 600, 80); note("sine", big ? 110 : 170, 42, big ? .45 : .28, big ? .4 : .22); },
    whoosh: () => hiss(.32, .3, 0, 350, 2600),
    boing: () => note("sine", 210, 640, .45, .14, 0, [14, 60]),
    tick: k => note("triangle", 440 * Math.pow(2, Math.min(k, 14) / 7), 460 * Math.pow(2, Math.min(k, 14) / 7), .12, .12),
    chime: () => [0, 4, 7, 12].forEach((s, k) => note("triangle", 523 * Math.pow(2, s / 12), 523 * Math.pow(2, s / 12), .3, .1, k * .09))
  };
})();

addStyle(`
.bl-scene{align-self:center; width:100%; max-width:480px; background:linear-gradient(#D6F1FF,#F6FCFF); border:3px solid var(--ink); border-radius:18px; overflow:hidden}
.bl-scene svg{display:block; width:100%; height:auto; touch-action:manipulation}
.bl-pan{cursor:pointer; outline:none}
.bl-pan .bl-dish{transition:fill .25s}
.bl-pan.bl-good .bl-dish{fill:#7EDDB0}
.bl-pan.bl-bad .bl-dish{fill:#FF9C8A}
.bl-pan:focus-visible .bl-dish{stroke:var(--coral); stroke-width:5}
.bl-load,.bl-stool{transform-box:fill-box; transform-origin:50% 100%}
.bl-pup{transition:transform .5s}
.bl-t{font-family:var(--display); font-weight:700}
.bl-sums{align-self:center; display:flex; flex-wrap:wrap; align-items:center; justify-content:center; gap:6px 10px; max-width:100%;
  font-family:var(--display); font-weight:700; font-size:clamp(16px,4.3vw,24px); visibility:hidden}
.bl-sums.on{visibility:visible}
.bl-sums span{background:#fff; border:3px solid var(--ink); border-radius:14px; padding:3px 10px; white-space:nowrap}
.bl-sums b{font-size:1.5em; line-height:1}
.bl-same{align-self:center; font-family:var(--display); font-size:22px; font-weight:600; padding:12px 22px; background:#fff}
.bl-same.ok{background:#C9F2DF} .bl-same.ko{background:#FFD6CF; animation:shake .4s}
.bl-nums{align-self:center; display:grid; gap:10px; grid-template-columns:repeat(4,minmax(0,1fr)); width:100%; max-width:480px}
.bl-nums .num{font-size:34px; padding:8px 0}
.bl-nums .num[disabled]{opacity:.45}
.bl-say{margin:0; min-height:1.3em; text-align:center; font-family:var(--display); font-size:clamp(18px,4.4vw,24px); font-weight:600}
`);

const MOUTH = {calm: "M195 41 Q200 44.5 205 41", wow: "M197 41.5 a3 3.5 0 1 0 6 0 a3 3.5 0 1 0 -6 0", happy: "M192.5 39.5 Q200 49 207.5 39.5"};
const ARM = 140; // from the pivot (200, 64) to each hook (60 and 340)

// the balance: stand, dial, beam with its needle, two hanging pans, a face on top; stools hold it level until the answer
function scene(stools){
  const pan = (side, x) => `<g transform="translate(${x} 64)"><g class="bl-pan" data-side="${side}" role="button" tabindex="0"><g class="bl-move">
    <path d="M0 0 L-52 118 M0 0 L52 118" stroke="${INK}" stroke-width="2" fill="none"/><g class="bl-load"></g>
    <path class="bl-dish" d="M-58 118 Q0 150 58 118 Z" fill="#FFC43D" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>
    <rect x="-62" y="14" width="124" height="126" fill="transparent"/></g></g></g>`;
  const stool = (side, x) => `<g class="bl-stool" data-side="${side}"><rect x="${x - 24}" y="208" width="8" height="42" rx="2" fill="#A86F3C" stroke="${INK}" stroke-width="2"/>
    <rect x="${x + 16}" y="208" width="8" height="42" rx="2" fill="#A86F3C" stroke="${INK}" stroke-width="2"/><rect x="${x - 32}" y="200" width="64" height="10" rx="4" fill="#C98B4F" stroke="${INK}" stroke-width="2.5"/></g>`;
  const ticks = [-20, -10, 0, 10, 20].map(a => {
    const s = Math.sin(a * Math.PI / 180), c = Math.cos(a * Math.PI / 180);
    return `<line x1="${(200 + 49 * s).toFixed(1)}" y1="${(64 + 49 * c).toFixed(1)}" x2="${(200 + 58 * s).toFixed(1)}" y2="${(64 + 58 * c).toFixed(1)}" stroke="${a ? INK : "#2F9E6E"}" stroke-width="${a ? 2 : 3.5}" stroke-linecap="round"/>`;
  }).join("");
  const wrap = el("div", "bl-scene");
  wrap.innerHTML = `<svg viewBox="-8 0 416 262" aria-hidden="false">
    <rect x="-8" y="248" width="416" height="14" fill="#9ED36A"/><path d="M-8 248 H408" stroke="${INK}" stroke-width="2.5"/>
    ${stools ? stool("l", 60) + stool("r", 340) : ""}
    <rect x="193" y="40" width="14" height="196" rx="5" fill="#C98B4F" stroke="${INK}" stroke-width="3"/>
    <path d="M150 249 Q152 232 172 232 H228 Q248 232 250 249 Z" fill="#C98B4F" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>
    <rect x="170" y="104" width="60" height="24" rx="9" fill="#fff" stroke="${INK}" stroke-width="2.5"/>${ticks}
    ${pan("l", 60)}${pan("r", 340)}
    <g class="bl-beam" style="transform-box:view-box; transform-origin:200px 64px">
      <path d="M200 64 L200 119" stroke="${INK}" stroke-width="3.5" stroke-linecap="round"/><circle cx="200" cy="120" r="4" fill="#FF6F59" stroke="${INK}" stroke-width="1.5"/>
      <rect x="50" y="58" width="300" height="12" rx="6" fill="#FFC43D" stroke="${INK}" stroke-width="3"/>
      <path d="M70 61.5 H130 M270 61.5 H330" stroke="#fff" stroke-opacity=".6" stroke-width="2.5" stroke-linecap="round"/>
      <circle cx="60" cy="64" r="4.5" fill="#fff" stroke="${INK}" stroke-width="2.5"/><circle cx="340" cy="64" r="4.5" fill="#fff" stroke="${INK}" stroke-width="2.5"/>
      <circle cx="200" cy="64" r="8" fill="#FF6F59" stroke="${INK}" stroke-width="3"/></g>
    <circle cx="200" cy="34" r="17" fill="#FFE3A3" stroke="${INK}" stroke-width="3"/>
    <circle cx="188" cy="39" r="3.5" fill="#FFB3C1"/><circle cx="212" cy="39" r="3.5" fill="#FFB3C1"/>
    <circle cx="193.5" cy="30" r="5" fill="#fff" stroke="${INK}" stroke-width="1.5"/><circle cx="206.5" cy="30" r="5" fill="#fff" stroke="${INK}" stroke-width="1.5"/>
    <g class="bl-pup"><circle cx="193.5" cy="31" r="2.5" fill="${INK}"/><circle cx="206.5" cy="31" r="2.5" fill="${INK}"/></g>
    <path class="bl-mouth" d="${MOUTH.calm}" stroke="${INK}" stroke-width="2.2" fill="none" stroke-linecap="round"/>
  </svg>`;
  const svg = wrap.querySelector("svg"), q = s => svg.querySelector(s);
  const panEl = {l: q('.bl-pan[data-side="l"]'), r: q('.bl-pan[data-side="r"]')};
  const move = {l: panEl.l.querySelector(".bl-move"), r: panEl.r.querySelector(".bl-move")};
  const load = {l: panEl.l.querySelector(".bl-load"), r: panEl.r.querySelector(".bl-load")};
  const beam = q(".bl-beam"), pup = q(".bl-pup"), mouth = q(".bl-mouth");
  let cur = 0, released = !stools;
  // the beam turns around the pivot; each pan follows its hook and stays upright
  const tf = a => {
    const r = a * Math.PI / 180, dx = ARM * (1 - Math.cos(r)), dy = ARM * Math.sin(r);
    return {b: `rotate(${a}deg)`, l: `translate(${dx.toFixed(2)}px, ${(-dy).toFixed(2)}px)`, r: `translate(${(-dx).toFixed(2)}px, ${dy.toFixed(2)}px)`};
  };
  const mood = m => mouth.setAttribute("d", MOUTH[m]);
  const set = a => {
    const t = tf(a); cur = a;
    beam.style.transform = t.b; move.l.style.transform = t.l; move.r.style.transform = t.r;
    pup.style.transform = a ? `translate(${a > 0 ? 2.6 : -2.6}px, 1px)` : ""; // the face looks at the side going down
  };
  const anim = (n, frames, o) => { if (!n || calm()) return null; try { return n.animate(frames, o); } catch(e) { return null; } };
  const hop = side => { // the light one is thrown up a little
    snd.boing();
    anim(load[side], [{transform: "translateY(0)"}, {transform: "translateY(-30px) rotate(-8deg)", offset: .35}, {transform: "translateY(0)", offset: .62},
      {transform: "translateY(-9px) rotate(4deg)", offset: .8}, {transform: "translateY(0)"}], {duration: 760, easing: "ease-out"});
  };
  // o.soft: a short glide (a cube lands); otherwise a swing that overshoots and settles
  const tip = (a, o = {}) => {
    const from = cur; set(a);
    if (calm() || Math.abs(a - from) < .01) { mood(a || !o.balance ? "calm" : "happy"); return Promise.resolve(); }
    const d = a - from, dur = o.soft ? 420 : 1300;
    const seq = o.soft ? [from, a] : [from, a + d * .28, a - d * .13, a + d * .05, a - d * .015, a];
    const off = o.soft ? [0, 1] : [0, .32, .54, .72, .87, 1];
    const frames = k => seq.map((v, j) => ({transform: tf(v)[k], offset: off[j], easing: o.soft ? "ease-out" : "ease-in-out"}));
    anim(beam, frames("b"), {duration: dur}); anim(move.l, frames("l"), {duration: dur}); anim(move.r, frames("r"), {duration: dur});
    if (!o.soft) { mood("wow"); snd.creak(); loops.push(setTimeout(() => snd.thud(o.big), dur * .3)); }
    if (o.hop) loops.push(setTimeout(() => hop(o.hop), dur * .22));
    return new Promise(res => loops.push(setTimeout(() => { mood(a === 0 && o.balance ? "happy" : "calm"); res(); }, dur)));
  };
  // the stools slide out, and the balance is free to move
  const release = () => {
    if (released) return Promise.resolve(); released = true;
    const st = svg.querySelectorAll(".bl-stool");
    if (calm()) { st.forEach(s => s.style.display = "none"); return Promise.resolve(); }
    snd.whoosh();
    st.forEach(s => { const k = s.dataset.side === "l" ? -1 : 1; anim(s, [{transform: "none", opacity: 1}, {transform: `translate(${k * 90}px, 8px) rotate(${k * 25}deg)`, opacity: 0}], {duration: 380, easing: "ease-in", fill: "forwards"}); });
    return new Promise(res => loops.push(setTimeout(res, 300)));
  };
  const flash = (side, cls) => { panEl[side].classList.add(cls); if (cls === "bl-bad") loops.push(setTimeout(() => panEl[side].classList.remove(cls), 800)); };
  panEl.l.addEventListener("keydown", e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); panEl.l.onclick && panEl.l.onclick(); } });
  panEl.r.addEventListener("keydown", e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); panEl.r.onclick && panEl.r.onclick(); } });
  return {wrap, pan: panEl, load, set, tip, release, flash, anim, mood};
}

// how far the balance leans: nothing when equal, at least 5 degrees otherwise, 16 at most (right heavier: positive, right pan down)
const tiltOf = (l, r) => l === r ? 0 : Math.sign(r - l) * (5 + 11 * Math.min(1, Math.abs(r - l) / Math.max(l, r)));

// drawn things: an emoji, an iron weight with its number, a small cube
const emo = (it, s) => `<text x="0" y="121" text-anchor="middle" font-size="${Math.round(58 * s)}">${it.e}</text>`;
// small weights (level 3) show only their big number, larger ones add "kg"
function weight(x, w, h, n){
  const kg = w >= 40, fs = Math.min(h * (kg ? .52 : .66), w / ((String(n).length + (kg ? 1.4 : .2)) * .6));
  return `<g transform="translate(${x} 121)"><rect x="${-w * .17}" y="${-h * 1.34}" width="${w * .34}" height="${h * .5}" rx="${w * .14}" fill="none" stroke="${INK}" stroke-width="3"/>
    <path d="M${-w / 2} 0 L${-w * .37} ${-h} H${w * .37} L${w / 2} 0 Z" fill="#5B6B80" stroke="${INK}" stroke-width="2.5" stroke-linejoin="round"/>
    <path d="M${-w * .26} ${-h * .82} H${w * .05}" stroke="#fff" stroke-opacity=".35" stroke-width="2.5" stroke-linecap="round"/>
    <text class="bl-t" x="0" y="${(-h * (kg ? .26 : .2)).toFixed(1)}" text-anchor="middle" font-size="${fs.toFixed(1)}" fill="#fff">${n}${kg ? `<tspan font-size="${(fs * .6).toFixed(1)}">kg</tspan>` : ""}</text></g>`;
}
const weightsRow = (ws, w, h) => { const gap = 3, tot = ws.length * w + (ws.length - 1) * gap; return ws.map((n, k) => weight(-tot / 2 + w / 2 + k * (w + gap), w, h, n)).join(""); };
const CUBE_COLORS = ["#FF6F59", "#43AA8B", "#36B5D8", "#FFC43D"];
const cube = (x, y, c, k) => `<g transform="translate(${x} ${y})"><g class="bl-cube"><rect x="-11" y="-22" width="22" height="22" rx="4" fill="${CUBE_COLORS[k % 4]}" stroke="${INK}" stroke-width="2.2"/>
  <path d="M-7 -18 h8" stroke="#fff" stroke-opacity=".6" stroke-width="2.5" stroke-linecap="round"/><text class="bl-t" x="0" y="-6.5" text-anchor="middle" font-size="13" fill="${INK}">${c}</text></g></g>`;

/* the rounds of each level */
function plan(lvl){
  const coin = () => Math.random() < .5;
  if (lvl === 1) {
    const H = shuffle(L1_HEAVY), L = shuffle(L1_LIGHT);
    return ["h", ...shuffle(["h", "h", "l", "l", "l"])].map((q, k) => ({h: item(H[k % H.length]), l: item(L[k % L.length]), q, heavyLeft: coin(), sz: [1.25, .7]}));
  }
  if (lvl === 2) {
    const chosen = [], used = new Set();
    for (const p of shuffle(L2_PAIRS)) {
      if (chosen.length >= 5) break;
      if (used.has(p[0]) || used.has(p[1])) continue;
      chosen.push(p); used.add(p[0]); used.add(p[1]);
    }
    chosen.splice(1 + rnd(chosen.length), 0, [...pick(L2_SURPRISE, 1)[0], true]);
    return chosen.map(([h, l, surprise]) => ({h: item(h), l: item(l), q: coin() ? "h" : "l", heavyLeft: coin(), surprise: !!surprise, sz: surprise ? [.75, 1.35] : [1, 1]}));
  }
  if (lvl === 3) { // sums close together: counting the weights or looking at them is not enough
    const eqAt = new Set(pick([1, 2, 3, 4, 5], 2));
    const side = () => [...Array(2 + rnd(2))].map(() => 1 + rnd(9));
    return [...Array(6)].map((_, k) => {
      let L = [5, 4], R = [2, 7];
      for (let t = 0; t < 500; t++) {
        const a = side(), b = side(), d = sum(a) - sum(b);
        const twin = a.slice().sort().join() === b.slice().sort().join();
        if (eqAt.has(k) ? d === 0 && !twin : d !== 0 && Math.abs(d) <= 2) { L = a; R = b; break; }
      }
      return {L, R, q: coin() ? "h" : "l"};
    });
  }
  // level 4: A one weight, B two weights to add, C a weight already beside the cubes
  return ["A", ...shuffle(["A", "B", "B", "C", "C"])].map(t => {
    const c = 2 + rnd(8);
    if (t === "C") { const n = 2 + rnd(7), pre = 2 + rnd(12); return {c, n, pre, T: pre + n * c, parts: [pre + n * c]}; }
    const n = 3 + rnd(8), T = n * c;
    if (t === "A") return {c, n, pre: 0, T, parts: [T]};
    const a = Math.max(2, Math.min(T - 2, Math.round(T * (.3 + Math.random() * .4))));
    return {c, n, pre: 0, T, parts: [a, T - a]};
  });
}

registerGame({id: ID, em: "⚖️", name: "La balance", title: TX.title, sub: TX.sub, multi: true, cat: "nombres"}, function () {
  const lang = LG(), lvl = levelOf(ID), total = 6, res = [], rounds = plan(lvl);
  const t = k => TX[k][lang];
  const nm = it => it[lang] || it.en;
  const speak = s => say(s, lang);
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 5 : ms)));
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };
  startSession(ID, null, total); const gen = GEN;
  const body = $("gameBody");
  let i = 0;

  const frame = (html, ask, stools) => {
    body.innerHTML = "";
    body.append(el("p", "prompt", html));
    const row = el("div", "row"); row.style.justifyContent = "center"; row.append(speakBtn(() => ask, t("again"), lang)); body.append(row);
    const sc = scene(stools); body.append(sc.wrap);
    return sc;
  };
  const sumsRow = () => {
    const node = el("div", "bl-sums", "<span></span><b></b><span></span>"), [a, s, b] = node.children;
    return {node, show(l, sign, r){ a.textContent = l; s.textContent = sign; b.textContent = r; node.classList.add("on");
      if (!calm()) try { node.animate([{transform: "scale(.6)", opacity: 0}, {transform: "scale(1.06)", opacity: 1, offset: .7}, {transform: "none"}], {duration: 380, easing: "ease-out"}); } catch(e) {} },
      hide(){ node.classList.remove("on"); }};
  };
  const end = async (key, first, tries) => {
    logRound(key, first, tries + 1, {lvl});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    i++; await wait(900);
    next();
  };
  const cheer = sc => { // a burst of sparkles above the balance
    if (calm()) return;
    const r = sc.wrap.getBoundingClientRect();
    fx.sparkle(r.left + r.width / 2, r.top + r.height * .25, 16);
  };

  // levels 1 and 2: two things, which one is heavy (heavier) or light (lighter)?
  const compare = async r => {
    const [a, b] = r.heavyLeft ? [r.h, r.l] : [r.l, r.h];
    const who = r.h.animal && r.l.animal;
    const q = lvl === 1 ? TX.q1[r.q][lang](who) : TX.q2[r.q][lang](nm(a), nm(b), who);
    const sc = frame(`${r.q === "h" ? "🏋️" : "🪶"} ${q}`, q, true);
    const size = it => it === r.h ? r.sz[0] : r.sz[1];
    sc.load.l.innerHTML = emo(a, size(a)); sc.load.r.innerHTML = emo(b, size(b));
    sc.pan.l.setAttribute("aria-label", nm(a)); sc.pan.r.setAttribute("aria-label", nm(b));
    const good = (r.q === "h" ? r.h : r.l) === a ? "l" : "r";
    markOk(sc.pan[good]);
    const line = el("p", "bl-say", ""); body.append(line);
    const fb = lvl === 1 ? (r.q === "h" ? `${TX.heavy[lang](nm(r.h))} ${TX.light[lang](nm(r.l))}` : `${TX.light[lang](nm(r.l))} ${TX.heavy[lang](nm(r.h))}`)
      : (r.q === "h" ? TX.heavier[lang](nm(r.h), nm(r.l)) : TX.lighter[lang](nm(r.l), nm(r.h))) + (r.surprise ? " " + t("big") : "");
    let tries = 0, over = false, busy = false, tipped = false;
    const answer = async side => {
      if (over || busy) return; busy = true; G.taps++;
      if (!tipped) {
        tipped = true;
        await sc.release(); if (!alive(gen)) return;
        await sc.tip(tiltOf(a.kg, b.kg), {hop: r.heavyLeft ? "r" : "l", big: lvl === 1 || r.h.kg >= 100}); if (!alive(gen)) return;
      }
      line.textContent = fb;
      if (side === good) {
        over = true; delete sc.pan[good].dataset.ok; sc.flash(good, "bl-good"); sfx.ok();
        const first = tries === 0; if (first) addStar();
        await speak(`${praise()} ${fb}`); if (!alive(gen)) return;
        return end(`${r.h.en}>${r.l.en}`, first, tries);
      }
      tries++; sc.flash(side, "bl-bad"); sfx.ko();
      await speak(fb);
      busy = false;
    };
    sc.pan.l.onclick = () => answer("l"); sc.pan.r.onclick = () => answer("r");
    await speak(q);
  };

  // level 3: add the weights of each pan; heavier, lighter or the same?
  const sums = async r => {
    const sL = sum(r.L), sR = sum(r.R), q = TX.q3[r.q][lang], ask = `${q} ${t("orSame")}`;
    const good = sL === sR ? "same" : (r.q === "h") === (sL > sR) ? "l" : "r";
    const sc = frame(`⚖️ ${q}<small>${t("orSame")}</small>`, ask, true);
    sc.load.l.innerHTML = weightsRow(r.L, 36, 34); sc.load.r.innerHTML = weightsRow(r.R, 36, 34);
    const row = sumsRow(); body.append(row.node);
    const same = el("button", "bl-same chunky", `⚖️ ${t("same")}`); body.append(same);
    markOk(good === "same" ? same : sc.pan[good]);
    const line = el("p", "bl-say", ""); body.append(line);
    const hi = Math.max(sL, sR), lo = Math.min(sL, sR);
    const fb = sL === sR ? TX.kgSame[lang](sL) : r.q === "h" ? TX.kgHeavier[lang](hi, lo) : TX.kgLighter[lang](lo, hi);
    let tries = 0, over = false, busy = false, tipped = false;
    const answer = async pickd => {
      if (over || busy) return; busy = true; G.taps++;
      if (!tipped) {
        tipped = true;
        await sc.release(); if (!alive(gen)) return;
        row.show(`${r.L.join(" + ")} = ${sL}`, sL > sR ? ">" : sL < sR ? "<" : "=", `${r.R.join(" + ")} = ${sR}`);
        await sc.tip(tiltOf(sL, sR), {balance: true}); if (!alive(gen)) return;
      }
      line.textContent = fb;
      if (pickd === good) {
        over = true;
        if (good === "same") { delete same.dataset.ok; same.classList.add("ok"); } else { delete sc.pan[good].dataset.ok; sc.flash(good, "bl-good"); }
        sfx.ok(); if (good === "same") cheer(sc);
        const first = tries === 0; if (first) addStar();
        await speak(`${praise()} ${fb}`); if (!alive(gen)) return;
        return end(`${sL}:${sR}`, first, tries);
      }
      tries++; sfx.ko();
      if (pickd === "same") { same.classList.remove("ko"); void same.offsetWidth; same.classList.add("ko"); } else sc.flash(pickd, "bl-bad");
      await speak(fb);
      busy = false;
    };
    sc.pan.l.onclick = () => answer("l"); sc.pan.r.onclick = () => answer("r"); same.onclick = () => answer("same");
    await speak(ask);
  };

  // level 4: how many cubes balance the weights? they fall one by one, counted aloud
  const cubes = async r => {
    const {c, n, pre, T, parts} = r, small = TX.cube[lang](c), big = t("q4");
    const sc = frame(`⚖️ ${big}<small>${small}</small>`, `${small} ${big}`, false);
    sc.load.l.innerHTML = parts.length > 1 ? weightsRow(parts, 48, 40) : weightsRow(parts, 64, 48);
    if (pre) sc.load.r.innerHTML = weight(-32, 40, 34, pre);
    const pile = document.createElementNS("http://www.w3.org/2000/svg", "g"); sc.load.r.append(pile);
    const cols = pre ? 3 : 4, x0 = pre ? 0 : -36;
    const slot = j => [x0 + (j % cols) * 24, 121 - Math.floor(j / cols) * 24];
    const row = sumsRow(); body.append(row.node);
    const pool = [n - 2, n - 1, n + 1, n + 2, n + 3].filter(x => x >= 2 && x <= 12);
    const choices = [n, ...pick(pool, 3)].sort((x, y) => x - y);
    const grid = el("div", "bl-nums"); body.append(grid);
    const line = el("p", "bl-say", ""); body.append(line);
    let tries = 0, over = false, busy = false;
    const answer = async (k, btn) => {
      if (over || busy || btn.disabled) return; busy = true; G.taps++; line.textContent = "";
      for (let j = 0; j < k; j++) { // each cube lands, the balance moves a little, the total is said
        const [x, y] = slot(j);
        pile.insertAdjacentHTML("beforeend", cube(x, y, c, j));
        sc.anim(pile.lastElementChild.firstElementChild, [{transform: "translateY(-120px)", opacity: 0}, {transform: "translateY(3px)", opacity: 1, offset: .7},
          {transform: "translateY(-3px)", offset: .85}, {transform: "none"}], {duration: 380, easing: "ease-in"});
        snd.tick(j);
        const s = pre + (j + 1) * c;
        sc.tip(tiltOf(T, s), {soft: true});
        await Promise.all([speak(String(s)), wait(420)]); if (!alive(gen)) return;
      }
      const S = pre + k * c;
      row.show(parts.length > 1 ? `${parts.join(" + ")} = ${T}` : `${T}`, T > S ? ">" : T < S ? "<" : "=", `${pre ? pre + " + " : ""}${k} × ${c} = ${S}`);
      if (S === T) {
        over = true; delete btn.dataset.ok; btn.classList.add("ok");
        await sc.tip(0, {balance: true}); if (!alive(gen)) return;
        sc.mood("happy"); snd.chime(); sfx.ok(); cheer(sc);
        const fb = `${TX.formula[lang](pre, k, c, S)} ${t("balanced")}`; line.textContent = fb;
        const first = tries === 0; if (first) addStar();
        await speak(`${praise()} ${fb}`); if (!alive(gen)) return;
        return end(`${T}=${pre ? pre + "+" : ""}${n}x${c}`, first, tries);
      }
      tries++; sfx.ko(); btn.classList.remove("ko"); void btn.offsetWidth; btn.classList.add("ko"); btn.disabled = true;
      const fb = S < T ? TX.tooLight[lang](S) : TX.tooHeavy[lang](S); line.textContent = fb;
      await speak(fb); await wait(500); if (!alive(gen)) return;
      // the cubes fly away, the balance goes back
      [...pile.children].forEach((g, j) => sc.anim(g.firstElementChild, [{transform: "none", opacity: 1}, {transform: "translateY(-140px) rotate(40deg)", opacity: 0}],
        {duration: 420, delay: j * 40, easing: "ease-in", fill: "forwards"}));
      await wait(420 + k * 40); if (!alive(gen)) return;
      pile.innerHTML = ""; row.hide();
      await sc.tip(tiltOf(T, pre)); if (!alive(gen)) return;
      busy = false;
    };
    choices.forEach(k => {
      const b = el("button", "num chunky", String(k)); if (k === n) markOk(b);
      b.onclick = () => answer(k, b); grid.append(b);
    });
    sc.tip(tiltOf(T, pre)); // the empty side goes up
    await speak(`${small} ${big}`);
  };

  const next = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    (lvl <= 2 ? compare : lvl === 3 ? sums : cubes)(rounds[i]);
  };
  next();
});
})();
