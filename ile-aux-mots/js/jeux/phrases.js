/* L'Île aux Mots : jeu « phrases » (Word Parade), l'ordre des mots.
   Word creatures jump into line in the right order, then the picture acts the sentence out.
   1: three words, pictures on the tags · 2: four or five words, adjectives · 3: six words and more, prepositions, words only
   4: picture only (no voice for the reader), one tag too many. In English, German, Luxembourgish or Chinese, never French. */
(() => {
const D = phrasesData, LG = ["en", "de", "lb", "zh"];
const cur = () => LG.includes(langOf()) ? langOf() : "en";
const tx = (k, L) => D.txt[L || cur()][k];
const calm = () => fx.calm();

addStyle(`
.ph{display:flex; flex-direction:column; gap:12px}
.ph-q{margin:0; text-align:center; font-family:var(--display); font-size:21px; font-weight:600; line-height:1.2}
.ph-q small{display:block; margin-top:3px; font-family:var(--body); font-size:15px; font-weight:800; color:var(--coral)}
.ph-scene{position:relative; height:150px; border:3px solid var(--ink); border-radius:18px; overflow:hidden; cursor:pointer;
  background:radial-gradient(circle at 88% 16%, #FFF3B8 0 16px, transparent 17px), linear-gradient(#BDEBFF 0 66%, #A5DC8E 66%)}
.ph-pos{position:absolute; transform:translate(-50%,-50%); line-height:1}
.ph-e{display:inline-block; transform-origin:50% 85%}
.ph-main{animation:ph-idle 4.6s ease-in-out infinite}
@keyframes ph-idle{0%,80%,100%{transform:none} 86%{transform:translateY(-12px) scale(.94,1.08)} 92%{transform:scale(1.1,.9)}}
.ph-pt{position:absolute; margin:-14px 0 0 -14px; font-size:26px; line-height:1; pointer-events:none; z-index:3}
.ph-bub{position:absolute; right:8px; top:8px; z-index:4; background:#fff; border:3px solid var(--ink); border-radius:16px 16px 16px 4px; padding:2px 10px; font-family:var(--display); font-size:22px; font-weight:700}
.ph-strip{display:flex; flex-wrap:wrap; justify-content:center; align-items:flex-end; gap:20px 8px; min-height:100px; padding:22px 8px 12px; background:#FFF6DC; border:3px dashed #C9B98E; border-radius:18px}
.ph-slot{display:flex; min-width:58px; min-height:64px; border:3px dashed #C9B98E; border-radius:22px; background:rgba(255,255,255,.6)}
.ph-slot.full{border:0; background:none; min-width:0}
.ph-dot{align-self:flex-end; font-family:var(--display); font-size:36px; font-weight:700; line-height:1}
.ph.won .ph-strip{border-style:solid; border-color:var(--leaf); background:#E3F7EC}
.ph-pool{display:flex; flex-wrap:wrap; justify-content:center; gap:26px 10px; padding-top:14px; min-height:92px}
.ph-ghost{display:inline-block; visibility:hidden}
.ph-w{position:relative; padding:0; border-radius:22px; animation:ph-pop .45s cubic-bezier(.2,1.5,.4,1) both; animation-delay:calc(var(--i,0) * 70ms)}
@keyframes ph-pop{from{transform:scale(.2); opacity:0}}
.ph-in{position:relative; display:flex; flex-direction:column; align-items:center; justify-content:center; min-width:64px; min-height:64px; padding:14px 12px 8px;
  background:var(--c,#fff); border:3px solid var(--ink); border-radius:22px; box-shadow:2px 4px 0 var(--ink); font-family:var(--display); font-size:24px; font-weight:600;
  line-height:1.1; white-space:nowrap; animation:ph-bob 2.8s ease-in-out infinite; animation-delay:var(--d,0s)}
@keyframes ph-bob{50%{transform:translateY(-5px) rotate(-2deg)}}
.ph-slot .ph-w,.ph-slot .ph-in{animation:none}
.ph-p{font-size:26px; line-height:1.1}
.ph-eyes{position:absolute; top:-12px; left:50%; margin-left:-20px; display:flex; gap:3px}
.ph-eye{position:relative; display:block; width:19px; height:19px; background:#fff; border:2.5px solid var(--ink); border-radius:50%; animation:ph-blink 4.2s infinite; animation-delay:var(--d,0s)}
.ph-eye b{position:absolute; left:4px; top:5px; width:7px; height:7px; background:var(--ink); border-radius:50%; animation:ph-look 5s ease-in-out infinite; animation-delay:var(--d,0s)}
@keyframes ph-blink{0%,92%,100%{transform:none} 95%{transform:scaleY(.1)}}
@keyframes ph-look{0%,25%,100%{transform:none} 35%,55%{transform:translate(-3px,1px)} 65%,85%{transform:translate(3px,-1px)}}
.ph-w.ko .ph-in{background:#FFD6CF}
.ph-w.ko .ph-eye,.ph-w.yay .ph-eye{background:none; border-color:transparent; animation:none}
.ph-w.ko .ph-eye b,.ph-w.yay .ph-eye b{display:none}
.ph-w.ko .ph-eye::before{position:absolute; inset:-3px 0 0; text-align:center; font:900 18px/1 var(--body); color:var(--ink)}
.ph-w.ko .ph-eye:first-child::before{content:">"} .ph-w.ko .ph-eye:last-child::before{content:"<"}
.ph-w.yay .ph-eye{height:11px; margin-top:6px; border:3.5px solid var(--ink); border-bottom:0; border-radius:12px 12px 0 0} /* happy closed eyes */
.ph-tg{position:absolute; left:50%; bottom:-14px; width:16px; height:15px; margin-left:-8px; background:#FF7B9C; border:2.5px solid var(--ink); border-top:0;
  border-radius:0 0 9px 9px; transform:scaleY(0); transform-origin:top; transition:transform .12s}
.ph-w.ko .ph-tg{transform:scaleY(1)}
.ph-w.hint .ph-in{box-shadow:0 0 0 5px var(--sun), 2px 4px 0 var(--ink); animation:ph-wig .5s ease-in-out infinite}
@keyframes ph-wig{25%{transform:rotate(-7deg) scale(1.06)} 75%{transform:rotate(7deg) scale(1.06)}}
.ph-oops{position:fixed; z-index:60; pointer-events:none; font-family:var(--display); font-size:22px; font-weight:700; color:var(--bad); text-shadow:2px 2px 0 #fff}
.ph-calm *{animation:none!important; transition:none!important}
`);

/* ---------- words ---------- */
const parse = s => {
  s = s.trim(); let p = "";
  if (/[.!?。！？]$/.test(s)) { p = s.slice(-1); s = s.slice(0, -1).trim(); }
  return {toks: s.split(/\s+/).map(t => t.replace(/_/g, " ")), p};
};
const joinS = (toks, p, L) => (L === "zh" ? toks.join("") : toks.join(" ")) + p;
const bare = s => s.replace(/^(der|die|das|den|dem|de)\s+/i, "").replace(/^d'/i, "");
let PICS = null, LBW = null;
function picOf(w, L) {
  if (!PICS) {
    PICS = new Map();
    Object.values(THEMES).forEach(t => t.words.forEach(x => {
      if (!x.e.startsWith("#")) [x.en, bare(x.de || ""), bare(x.lb || ""), x.zh].forEach(k => { k = (k || "").toLowerCase(); if (k && !PICS.has(k)) PICS.set(k, x.e); });
    }));
    D.pics.forEach(line => { const [e, ...ks] = line.split(/\s+/); ks.forEach(k => PICS.set(k.replace(/_/g, " ").toLowerCase(), e)); });
  }
  const k = w.toLowerCase();
  return L === "lb" && k === "a" ? "" : PICS.get(k) || PICS.get(bare(k)) || ""; // lb "a" is "in" (a mengem Rucksak)
}
// Luxembourgish has no voice: play the lod.lu recording of a lexicon word found in the text (nouns first, plurals too: Kazen → d'Kaz)
function lbWord(text) {
  if (!LBW) LBW = Object.entries(THEMES).flatMap(([k, t]) => t.words.filter(w => w.lb && w.lod).map(w => [bare(w.lb).toLowerCase(), w.lb, k === "colors" || k === "actions" ? 1 : 0]));
  const toks = text.toLowerCase().split(/[\s.!?,]+/).map(t => t.replace(/^d'/, "")).filter(Boolean);
  const ends = ["e", "n", "en", "er", "ën", "ër"];
  for (const rank of [0, 1]) for (const t of toks) {
    const hit = LBW.find(([b, , r]) => r === rank && t === b) || LBW.find(([b, , r]) => r === rank && b.length > 1 && t.startsWith(b) && ends.includes(t.slice(b.length)));
    if (hit) return hit[1];
  }
  return null;
}
function sayIn(text, L) {
  if (L === "lb") { const w = lbWord(text); return w ? say(w, "lb") : Promise.resolve(); }
  return say(text, L, L === "en" ? undefined : (S.kid === "p4" ? .8 : .9));
}

/* ---------- funny sounds (Web Audio, silent in the recette) ---------- */
const au = () => { try { return ac = ac || new (window.AudioContext || window.webkitAudioContext)(); } catch (e) { return null; } };
function snd(f0, f1, dur, type = "sine", vol = .2, o = {}) {
  const c = !TEST && au(); if (!c) return;
  try {
    const t = c.currentTime + (o.at || 0), osc = c.createOscillator(), g = c.createGain();
    osc.type = type; osc.frequency.setValueAtTime(f0, t); osc.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    let out = osc;
    if (o.lp) { const f = c.createBiquadFilter(); f.type = "lowpass"; f.frequency.value = o.lp; osc.connect(f); out = f; }
    out.connect(g); g.connect(c.destination);
    if (o.lfo) { const l = c.createOscillator(), lg = c.createGain(); l.frequency.value = o.lfo[0]; lg.gain.value = o.lfo[1]; l.connect(lg); lg.connect(osc.frequency); l.start(t); l.stop(t + dur + .05); }
    osc.start(t); osc.stop(t + dur + .05);
  } catch (e) {}
}
let noise = null;
function hiss(dur, freq, vol = .25, at = 0) {
  const c = !TEST && au(); if (!c) return;
  try {
    if (!noise) { noise = c.createBuffer(1, c.sampleRate / 2, c.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    const t = c.currentTime + at, s = c.createBufferSource(), f = c.createBiquadFilter(), g = c.createGain();
    s.buffer = noise; f.type = "bandpass"; f.frequency.value = freq; f.Q.value = .8;
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(c.destination); s.start(t); s.stop(t + dur + .02);
  } catch (e) {}
}
const NOTES = [523, 587, 659, 784, 880, 1047, 1175, 1319, 1568, 1760];
const SND = {
  note: k => snd(NOTES[k % 10] * .7, NOTES[k % 10], .22, "triangle", .2, {lfo: [16, 10]}), // each word one step higher: the sentence sings
  boing: () => snd(160, 540, .35, "sine", .25, {lfo: [22, 40]}),
  pouet: () => snd(220, 105, .45, "sawtooth", .16, {lfo: [26, 24], lp: 900}),
  burp: (at = 0) => snd(115, 60, .55, "sawtooth", .3, {lfo: [19, 18], lp: 480, at}),
  up: () => snd(420, 1500, .4, "sine", .16),
  down: () => snd(1400, 260, .55, "sine", .16),
  sneeze: () => { snd(300, 560, .5, "sine", .1); hiss(.35, 2600, .4, .6); },
  vroom: () => snd(70, 200, .9, "sawtooth", .14, {lp: 700, lfo: [30, 15]}),
  roar: () => snd(150, 70, .9, "sawtooth", .25, {lp: 600, lfo: [35, 30]}),
  bark: () => { snd(520, 300, .12, "square", .14, {lp: 1500}); snd(540, 280, .12, "square", .14, {lp: 1500, at: .22}); },
  glug: () => [0, 1, 2].forEach(k => snd(320 - k * 40, 180 - k * 30, .12, "sine", .2, {at: .2 + k * .2})),
  crunch: () => [0, 1, 2].forEach(k => hiss(.07, 1800, .3, k * .2)),
  plop: (at = 0) => snd(900, 180, .16, "sine", .25, {at}),
  snore: () => snd(80, 110, 1, "sawtooth", .08, {lp: 300, lfo: [12, 10]}),
  tune: f => !TEST && tone(f, .12, "sine")
};

/* ---------- the picture: placed as the sentence says, and it acts ---------- */
const LAY = {
  one: [[50, 54, 82]], two: [[32, 54, 70], [68, 54, 70]],
  on: [[50, 30, 54], [50, 72, 66]], under: [[50, 78, 44], [50, 36, 74]], next: [[32, 56, 62], [68, 56, 62]],
  in: [[50, 56, 40], [50, 52, 86]], over: [[50, 20, 46], [50, 70, 66]], front: [[50, 70, 54], [50, 40, 78]],
  between: [[50, 62, 44], [20, 58, 58], [80, 58, 58]]
};
function scene(it) {
  const sc = el("div", "ph-scene"), ems = it.e.split(" "), lay = LAY[it.o] || (ems.length > 1 ? LAY.two : LAY.one);
  const nodes = ems.map((e, k) => {
    const [x, y, s] = lay[k] || lay[lay.length - 1], pos = el("span", "ph-pos");
    pos.style.cssText = `left:${x}%; top:${y}%; font-size:${s}px; z-index:${k ? 1 : 2}`;
    const inner = el("span", "ph-e" + (k ? "" : " ph-main"), e); pos.append(inner); sc.append(pos);
    return inner;
  });
  sc._o = lay[0];
  return {sc, main: nodes[0]};
}
const anim = (n, frames, duration = 1200, o = {}) => n.animate(frames, Object.assign({duration, easing: "ease-in-out"}, o));
const F = (...ts) => ts.map(t => typeof t === "string" ? {transform: t} : {transform: t[0], offset: t[1]});
// emoji thrown from the picture: up, down (rain), out (burst), left (dust)
function spray(sc, em, n, how, at = 0) {
  const [ox, oy] = sc._o;
  for (let k = 0; k < n; k++) {
    const d = el("span", "ph-pt", em); d.style.left = ox + "%"; d.style.top = oy + "%"; sc.append(d);
    const r = Math.random() - .5, a = Math.PI * 2 * k / n;
    const end = how === "down" ? `translate(${r * 240}px,90px)` : how === "out" ? `translate(${Math.cos(a) * 95}px,${Math.sin(a) * 60}px) scale(1.3)`
      : how === "left" ? `translate(${-50 - k * 28}px,${r * 30 + 20}px) scale(1.5)` : `translate(${r * 100}px,-85px) scale(1.3)`;
    const start = how === "down" ? `translate(${r * 240}px,-110px)` : "translate(0,0) scale(.5)";
    d.animate([{transform: start, opacity: 1}, {transform: end, opacity: 0}], {duration: 900 + k * 60, delay: at + k * 110, easing: "ease-out", fill: "backwards"}).onfinish = () => d.remove();
  }
}
function bubble(sc, text, at = 0) {
  const b = el("span", "ph-bub"); b.textContent = text; sc.append(b);
  b.animate([{transform: "scale(0)", opacity: 0}, {transform: "scale(1.2)", opacity: 1, offset: .15}, {transform: "scale(1)", opacity: 1, offset: .75}, {transform: "scale(1)", opacity: 0}],
    {duration: 1400, delay: at, fill: "backwards"}).onfinish = () => b.remove();
}
const HOP = F("none", ["scale(1.25,.75)", .12], ["translateY(-48px) scale(.9,1.12)", .32], ["scale(1.2,.8)", .5], ["translateY(-30px) scale(.95,1.06)", .68], ["scale(1.1,.9)", .85], "none");
const DASH = w => F("none", [`translateX(${w}px) skewX(-14deg)`, .42], [`translateX(${-w}px) skewX(-14deg)`, .43], "none");
const ACTS = {
  hop: m => { anim(m, HOP); SND.boing(); },
  run: (m, sc, w) => { anim(m, DASH(w), 1500); spray(sc, "💨", 3, "left"); hiss(.5, 800, .15); },
  drive: (m, sc, w) => { anim(m, DASH(w), 1600); spray(sc, "💨", 4, "left"); SND.vroom(); },
  sleep: (m, sc) => { anim(m, F("none", ["rotate(-16deg) translateY(8px)", .15], ["rotate(-16deg) translateY(8px) scale(1.06,.94)", .5], ["rotate(-16deg) translateY(8px)", .85], "none"), 2200); spray(sc, "💤", 3, "up"); SND.snore(); },
  sing: (m, sc) => { anim(m, F("none", "rotate(-12deg) scale(1.1)", "rotate(12deg) scale(1.1)", "rotate(-12deg) scale(1.1)", "none"), 1400); spray(sc, "🎵", 4, "up"); SND.tune([784, 988, 1175, 988, 784]); },
  swim: (m, sc) => { anim(m, F("none", "translate(-40px,-8px) rotate(-8deg)", "translate(40px,8px) rotate(8deg)", "translate(-20px,-4px)", "none"), 1800); spray(sc, "💧", 4, "up"); SND.plop(); SND.plop(.5); },
  splash: (m, sc) => { anim(m, HOP); spray(sc, "💦", 6, "out", 350); SND.plop(.4); },
  roar: (m, sc) => { anim(m, F("none", ["scale(1.6) rotate(-5deg)", .3], ["scale(1.6) rotate(5deg)", .45], ["scale(1.6) rotate(-5deg)", .6], "none"), 1300); spray(sc, "💥", 3, "out", 300); SND.roar(); },
  eat: (m, sc) => { anim(m, F("none", "scale(1.12,.84)", "none", "scale(1.12,.84)", "none", "scale(1.12,.84)", "none"), 1100); spray(sc, "😋", 1, "up", 700); SND.crunch(); },
  drink: m => { anim(m, F("none", ["rotate(-25deg)", .2], ["rotate(-25deg) scale(1.05)", .8], "none"), 1400); SND.glug(); },
  love: (m, sc) => { anim(m, F("none", "scale(1.25)", "none", "scale(1.25)", "none"), 1100); spray(sc, "❤️", 5, "up"); SND.tune([659, 784, 1047]); },
  dance: (m, sc) => { anim(m, F("none", "rotate(-15deg) translateY(-12px)", "rotate(15deg)", "rotate(-15deg) translateY(-12px)", "rotate(15deg)", "none"), 1400); spray(sc, "🎉", 4, "out"); SND.tune([523, 659, 784, 659, 784, 1047]); },
  fly: m => { anim(m, F("none", "translate(-70px,-30px) rotate(-12deg)", "translate(60px,-40px) rotate(12deg)", "translate(-20px,-15px)", "none"), 1800); SND.up(); },
  grow: m => { anim(m, F("none", ["scale(1.8)", .45], ["scale(.85)", .7], ["scale(1.08)", .85], "none"), 1300); SND.boing(); },
  stretch: m => { anim(m, F("none", ["translateY(-18px) scale(.8,1.6)", .45], ["scale(1.1,.85)", .7], "none"), 1300); SND.up(); },
  shrink: m => { anim(m, F("none", ["scale(.3)", .35], ["scale(.3)", .6], ["scale(1.25)", .8], "none"), 1400); SND.down(); SND.plop(.8); },
  wiggle: m => { anim(m, F("none", "skewX(22deg)", "skewX(-22deg)", "skewX(22deg)", "skewX(-22deg)", "none"), 1300); hiss(1, 5000, .1); },
  roll: (m, sc, w) => { anim(m, F("none", [`translateX(${w}px) rotate(360deg)`, .45], [`translateX(${-w}px) rotate(0deg)`, .46], "none"), 1600); SND.up(); },
  crawl: m => { anim(m, F("none", "translateX(8px) rotate(3deg)", "translateX(16px)", "translateX(24px) rotate(3deg)", "translateX(12px)", "none"), 2400); SND.tune([196, 175, 165]); },
  hot: (m, sc) => { anim(m, F("none", "rotate(-6deg) scale(1.1)", "rotate(6deg) scale(1.1)", "rotate(-6deg) scale(1.1)", "none"), 1000); spray(sc, "💦", 5, "out"); hiss(.6, 3000, .12); },
  cold: (m, sc) => { anim(m, F("none", "translateX(-4px)", "translateX(4px)", "translateX(-4px)", "translateX(4px)", "translateX(-4px)", "translateX(4px)", "none"), 1000); spray(sc, "❄️", 5, "down"); SND.tune([1568, 1480, 1568, 1480]); },
  shine: (m, sc) => { anim(m, F("none", "rotate(180deg) scale(1.3)", "rotate(360deg)"), 1400); spray(sc, "✨", 6, "out"); SND.tune([1047, 1319, 1568, 2093]); },
  laugh: (m, sc) => { anim(m, F("none", "translateY(-8px) rotate(-6deg)", "none", "translateY(-8px) rotate(6deg)", "none", "translateY(-8px)", "none"), 1100); spray(sc, "😂", 3, "up"); SND.tune([660, 590, 660, 590]); },
  bark: (m, sc) => { anim(m, F("none", "translateY(-16px) scale(1.12)", "none", "translateY(-16px) scale(1.12)", "none"), 800); spray(sc, "🗯️", 2, "out"); SND.bark(); },
  lay: (m, sc) => { anim(m, F("none", ["scale(1.2,.78)", .3], ["translateY(-24px) scale(.92,1.1)", .5], "none"), 1100); spray(sc, "🥚", 1, "down", 450); SND.plop(.45); },
  slide: (m, sc, w) => { anim(m, F(`translateX(${-w}px) rotate(-12deg)`, [`translateX(${w}px) rotate(-12deg)`, .5], [`translateX(${-w}px) rotate(12deg)`, .51], "none"), 1800); SND.up(); snd(1500, 400, .6, "sine", .16, {at: .9}); },
  sway: (m, sc) => { anim(m, F("none", "rotate(-10deg)", "rotate(10deg)", "rotate(-10deg)", "none"), 1600); spray(sc, "🦋", 1, "up"); SND.tune([880, 1047, 880]); },
  spin: m => { anim(m, F("none", "rotate(360deg) scale(1.2)", "none"), 900); SND.boing(); },
  rain: (m, sc) => { spray(sc, "💧", 9, "down"); anim(m, F("none", "scale(1.1,.9)", "none", "scale(1.1,.9)", "none"), 1400); SND.tune([1319, 1175, 1319]); }
};
// a random gag when the picture is touched, sometimes on its own: sneeze, burp, boing, spin
const GAGS = [
  (m, sc, L) => { anim(m, F("none", ["scale(1.18,1.1) rotate(-6deg)", .45], ["scale(.8,1.2) translateY(6px) rotate(8deg)", .55], ["scale(1.1,.92)", .7], "none"), 1300); bubble(sc, tx("sneeze", L), 650); spray(sc, "💦", 5, "out", 700); SND.sneeze(); },
  (m, sc, L) => { anim(m, F("none", ["scale(1.3,1.22)", .4], ["scale(.92,1.05)", .52], "none"), 1100); bubble(sc, tx("burp", L), 480); SND.burp(.45); },
  m => { anim(m, HOP, 1100); SND.boing(); },
  m => { anim(m, F("none", "rotate(360deg) scale(1.2)"), 700); SND.up(); }
];
const gag = (m, sc, L) => calm() ? SND.boing() : GAGS[rnd(GAGS.length)](m, sc, L);

/* ---------- a word creature flies into its place ---------- */
function fly(b, slot, done) {
  const a = b.getBoundingClientRect(), g = el("span", "ph-ghost");
  g.style.width = a.width + "px"; g.style.height = a.height + "px";
  b.replaceWith(g); slot.append(b); slot.classList.add("full");
  if (calm()) return done();
  const z = b.getBoundingClientRect(), dx = a.left - z.left, dy = a.top - z.top;
  b.style.zIndex = 5;
  b.animate(F(`translate(${dx}px,${dy}px)`, [`translate(${dx / 2}px,${dy / 2 - 70}px) scale(1.2) rotate(-10deg)`, .5], ["scale(1.25,.72)", .82], "none"),
    {duration: 520, easing: "ease-in-out"}).onfinish = () => { b.style.zIndex = ""; done(); };
}
function oops(b, L) {
  if (calm()) return;
  const r = b.getBoundingClientRect(), d = el("div", "ph-oops"); d.textContent = tx("oops", L);
  d.style.left = (r.left + r.width / 2) + "px"; d.style.top = r.top + "px"; document.body.append(d);
  d.animate([{transform: "translate(-50%,-20%) scale(.6)", opacity: 0}, {transform: "translate(-50%,-110%) scale(1.1)", opacity: 1, offset: .3}, {transform: "translate(-50%,-170%)", opacity: 0}],
    {duration: 900, easing: "ease-out"}).onfinish = () => d.remove();
}
const COLS = ["#FFE3A3", "#BDEBDD", "#FFD0C6", "#D4ECFF", "#F4E1FF", "#E6F5C2"];
const wait = ms => new Promise(r => loops.push(setTimeout(r, ms)));

registerGame({id: "phrases", em: "🎺", name: "Phrases", desc: "Remets les mots dans l'ordre", multi: true, title: D.title, sub: D.sub}, function () {
  const lvl = levelOf("phrases"), p4 = S.kid === "p4";
  const pool = D.items.filter(it => lvl === 4 ? it.l >= 2 : it.l === lvl);
  const total = Math.min([8, 7, 6, 6][lvl - 1], pool.length), items = pick(pool, total), res = [];
  const pics = lvl <= 2 || p4, voice = lvl < 4 || p4, extra = lvl === 4; // she cannot read: pictures and voice at every level
  startSession("phrases", null, total); const gen = GEN;
  let i = 0, rid = 0;
  const round = again => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const me = ++rid, it = items[i], L = cur(), {toks, p} = parse(it[L]), text = joinS(toks, p, L);
    // every right order with the same tags (it.alt: "next to", "between" go both ways); those still possible so far
    let ways = [toks, ...[].concat((it.alt || {})[L] || []).map(s => parse(s).toks)];
    const live = () => me === rid && alive(gen);
    let pos = 0, tries = again ? again.tries : 0, miss = 0, locked = false, idle = 0;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    const root = el("div", "ph" + (calm() ? " ph-calm" : ""));
    const q = el("p", "ph-q", voice ? `👂 ${tx("listen", L)}` : `👀 ${tx("look", L)}`);
    if (extra) q.append(el("small", "", tx("extra", L)));
    const {sc, main} = scene(it);
    const row = el("div", "row"); row.style.justifyContent = "center";
    const sp = el("button", "speak chunky", `🔊 <span>${voice ? tx("again", L) : ""}</span>`);
    // the flags in the game bar press this button: same sentence, new language
    sp.onclick = () => {
      if (cur() !== L) { if (!locked) round({tries}); return; }
      if (voice) G.replays++; else G.hints++;
      if (!calm()) fx.bounce(main);
      sayIn(text, L);
    };
    row.append(sp);
    const strip = el("div", "ph-strip"), slots = toks.map(() => strip.appendChild(el("span", "ph-slot")));
    const dot = strip.appendChild(el("span", "ph-dot"));
    const poolEl = el("div", "ph-pool");
    root.append(q, sc, row, strip, poolEl); body.append(root);
    sc.onclick = () => { if (!live()) return; G.taps++; gag(main, sc, L); };

    const words = toks.slice(), xw = extra && (it.x || "").split("|")[LG.indexOf(L)];
    if (xw && !toks.includes(xw)) words.push(xw);
    let order = shuffle(words);
    for (let k = 0; k < 9 && order.join("|") === words.join("|"); k++) order = shuffle(words);
    const next = () => tiles.find(t => !t.dataset.placed && t._w === ways[0][pos]);
    const markNext = () => { if (!TEST) return; tiles.forEach(t => delete t.dataset.ok); markOk(next()); };
    const hint = () => { const n = next(); if (n) n.classList.add("hint"); };
    // the little one gets a nudge when nothing happens for a while
    const nudge = () => {
      clearTimeout(idle); if (!p4 || TEST) return;
      idle = setTimeout(() => { if (live() && !locked) { hint(); sayIn(ways[0][pos], L); nudge(); } }, 9000); loops.push(idle);
    };
    const tiles = order.map((w, k) => {
      const b = el("button", "ph-w"), pic = pics ? picOf(w, L) : ""; b._w = w;
      b.style.setProperty("--c", COLS[k % COLS.length]); b.style.setProperty("--d", (-Math.random() * 5).toFixed(2) + "s"); b.style.setProperty("--i", k);
      b.innerHTML = `<span class="ph-in"><span class="ph-eyes"><i class="ph-eye"><b></b></i><i class="ph-eye"><b></b></i></span>${pic ? `<span class="ph-p">${pic}</span>` : ""}<span class="ph-t"></span><span class="ph-tg"></span></span>`;
      b.querySelector(".ph-t").textContent = w; b.setAttribute("aria-label", w);
      b.onclick = () => tap(b);
      poolEl.append(b); return b;
    });
    const tap = b => {
      if (!live()) return;
      G.taps++; nudge();
      if (b.dataset.placed) { if (!calm()) fx.bounce(b); sayIn(b._w, L); return; }
      if (locked) return;
      const fit = ways.filter(o => o[pos] === b._w);
      if (fit.length) {
        ways = fit; b.dataset.placed = "1"; delete b.dataset.ok; b.classList.remove("hint", "ko"); b.classList.add("yay");
        SND.note(pos);
        const slot = slots[pos++]; miss = 0;
        fly(b, slot, () => { if (!calm()) { const r = b.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 6); } });
        sayIn(b._w, L); markNext();
        if (pos === toks.length) { locked = true; complete(); }
      } else {
        tries++; miss++;
        b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
        loops.push(setTimeout(() => b.classList.remove("ko"), 900));
        fx.wrong(); SND.pouet(); oops(b, L);
        if (p4 || miss >= 2) { hint(); if (p4) sayIn(ways[0][pos], L); }
      }
    };
    const complete = async () => {
      clearTimeout(idle);
      const first = tries === 0;
      if (first) addStar();
      logRound(it.en.replace(/_/g, " "), first, tries + 1, {lvl, lang: L});
      res.push(first ? 1 : 0); renderDots(res, total, -1);
      await wait(TEST ? 0 : 560); // the last word lands
      if (!live()) return;
      dot.textContent = p; root.classList.add("won");
      q.textContent = "🎉 " + pick(tx("praise", L), 1)[0];
      if (!calm()) {
        slots.forEach((s, k) => s.firstChild && s.firstChild.animate(F("none", "translateY(-22px) scale(1.08,.94) rotate(-4deg)", "scale(1.1,.9)", "none"), {duration: 520, delay: k * 110, easing: "ease-out"}));
        tiles.filter(t => !t.dataset.placed).forEach(t => { t.animate(F("none", ["translateY(-20px) rotate(-10deg)", .2], "translateY(260px) rotate(40deg)").map((f, k) => k === 2 ? {...f, opacity: 0} : f), {duration: 900, easing: "ease-in", fill: "forwards"}); SND.down(); });
        (ACTS[it.a] || ACTS.hop)(main, sc, sc.clientWidth * .75);
        if (Math.random() < .3) loops.push(setTimeout(() => live() && gag(main, sc, L), 1900));
      }
      await sayIn(joinS(ways[0], p, L), L); // the sentence as the child built it
      if (!live()) return;
      await wait(TEST ? 0 : 1000);
      if (!live()) return;
      i++; loops.push(setTimeout(round, TEST ? 30 : 250));
    };
    markNext();
    if (voice) (async () => {
      if (i === 0 && !again) { await sayIn(tx("listen", L), L); if (!live()) return; }
      await sayIn(text, L);
    })();
    nudge();
  };
  round();
});
})();
