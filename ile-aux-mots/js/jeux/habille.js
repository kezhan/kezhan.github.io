/* L'Île aux Mots : jeu « Habille-moi ! » (ticket 2026-09-27_jeu-habille).
   A chilly little critter asks for clothes; the child picks them, the critter spins into them and sneezes at a wrong one.
   1: one garment among three · 2: garment and colour · 3: two garments after a spoken situation · 4: written order, voice on request.
   English, German, Luxembourgish or Chinese, never French. Words and colours from the lexicon, texts in js/contenus/habille.js. */
(() => {
const H = HABILLE_TXT, INK = "#1B2D45", S3 = `stroke="${INK}" stroke-width="3" stroke-linejoin="round"`;
const LIGHT = "rgba(255,255,255,.5)", DARK = "rgba(0,0,0,.22)", PINK = "#FFB3C1";
const mir = m => m + `<g transform="translate(200 0) scale(-1 1)">${m}</g>`; // right side = left side mirrored
const oneOf = a => a[rnd(a.length)], cap = s => s.charAt(0).toUpperCase() + s.slice(1);
const hbLang = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const byEn = en => THEMES.clothes.words.find(w => w.en === en), slotOf = g => H.gear[g.en].slot;
let hbSync = null; // language flags in the game bar repaint the bubble

addStyle(`
.hb{display:flex; flex-direction:column; gap:12px; width:100%; max-width:560px; margin:0 auto}
.hb-top{display:flex; gap:10px; align-items:center}
.hb .hb-say{flex:none; width:64px; height:64px; padding:0; font-size:30px; background:var(--sun); display:grid; place-items:center; align-self:center}
.hb-bubble{flex:1; min-height:64px; background:#fff; border:3px solid var(--ink); border-radius:18px; padding:8px 12px; font-family:var(--display); font-size:20px; font-weight:600; text-align:center; display:flex; flex-direction:column; justify-content:center; gap:2px; overflow-wrap:anywhere}
.hb-bubble small{font-family:var(--body); font-size:15px; font-weight:800; color:var(--ink-soft)}
.hb-bubble .hb-ear{font-size:30px}
.hb-stage{position:relative; height:252px; border:3px solid var(--ink); border-radius:18px; overflow:hidden; background:linear-gradient(#BFE9FA, #EAF8FF 76%, #fff 76%); display:flex; justify-content:center; align-items:flex-end}
.hb-svg{height:238px; width:auto; max-width:100%; display:block; cursor:pointer; position:relative; z-index:1}
.hb-sun{position:absolute; top:6px; right:10px; font-size:42px; opacity:0; transition:opacity .6s}
.hb-sun.go{animation:hb-turn 14s linear infinite}
.hb-snow{position:absolute; inset:0; pointer-events:none; transition:opacity .6s}
.hb-snow span{position:absolute; top:-24px; font-size:18px}
.hb-snow.go span{animation:hb-fall linear infinite}
@keyframes hb-fall{from{transform:translateY(0) rotate(0)} to{transform:translateY(290px) rotate(360deg)}}
@keyframes hb-turn{to{transform:rotate(360deg)}}
.hb-pop{position:absolute; left:50%; top:8px; transform:translateX(-50%); z-index:3; max-width:92%; width:max-content; text-align:center; font-family:var(--display); font-size:20px; font-weight:700; background:var(--sun); border:3px solid var(--ink); border-radius:18px; padding:2px 12px; opacity:0; pointer-events:none; transition:opacity .25s}
.hb-fx{position:absolute; pointer-events:none; z-index:2}
.hb-grid{display:grid; gap:10px; grid-template-columns:repeat(3,1fr); width:100%; max-width:470px; margin:0 auto}
.hb-grid.n4{grid-template-columns:repeat(4,1fr)}
.hb-opt{position:relative; aspect-ratio:1; min-height:64px; background:#fff}
.hb-opt svg{position:absolute; inset:8px; width:calc(100% - 16px); height:calc(100% - 16px); pointer-events:none}
.hb-opt.ok{background:#C9F2DF} .hb-opt.no{background:#FFD6CF} .hb-opt[disabled]{opacity:.6}
.hb-kid{transform-box:view-box; transform-origin:100px 240px}
.hb-head{transform-box:view-box; transform-origin:100px 130px}
.hb-armL{transform-box:view-box; transform-origin:64px 148px}
.hb-armR{transform-box:view-box; transform-origin:136px 148px}
.hb-eye,.hb-cheek,.hb-svg [data-slot],.hb-svg [data-arm]{transform-box:fill-box; transform-origin:center}
.hb-mouth{transition:opacity .12s}
`);

/* ---------- drawings: every garment in the critter's own coordinates (viewBox 200 × 250) ---------- */
const sleeve = (c, x, y) => `<line x1="64" y1="148" x2="${x}" y2="${y}" stroke="${INK}" stroke-width="25" stroke-linecap="round"/><line x1="64" y1="148" x2="${x}" y2="${y}" stroke="${c}" stroke-width="19" stroke-linecap="round"/>`;
// a mitten drawn upright around (0,0): fingers up, thumb on the left, cuff at the wrist
const mitten = c => `<ellipse cx="-10" cy="1" rx="5" ry="8" transform="rotate(-25 -10 1)" fill="${c}" ${S3}/><path d="M-9 12 V-6 Q-9 -16 0 -16 Q9 -16 9 -6 V12 Z" fill="${c}" ${S3}/><rect x="-11" y="10" width="22" height="8" rx="3" fill="${c}" ${S3}/><rect x="-9.5" y="12" width="19" height="3" fill="${LIGHT}"/>`;
const glove = c => `<g transform="translate(41 191) rotate(210)">${mitten(c)}</g>`; // along the arm, thumb towards the body
const DRAW = {
  hat: {vb: "46 2 108 62", body: c => `<rect x="70" y="8" width="60" height="44" rx="7" fill="${c}" ${S3}/><rect x="71.5" y="35" width="57" height="10" fill="${DARK}"/><ellipse cx="100" cy="52" rx="50" ry="9" fill="${c}" ${S3}/>`},
  cap: {vb: "16 22 132 60", body: c => `<path d="M58 68 Q58 30 100 30 Q142 30 142 68 Z" fill="${c}" ${S3}/><path d="M100 32 V66 M80 36 Q88 50 86 66 M120 36 Q112 50 114 66" stroke="${DARK}" stroke-width="3" fill="none"/><path d="M62 64 Q38 60 20 72 Q40 80 74 70 Z" fill="${c}" ${S3}/><circle cx="100" cy="30" r="5" fill="${c}" ${S3}/>`},
  glasses: {vb: "52 64 96 42", body: c => { const f = `<circle cx="84" cy="85" r="14"/><circle cx="116" cy="85" r="14"/><path d="M98 83 Q100 79 102 83 M70 82 L58 77 M130 82 L142 77"/>`;
    return `<g fill="rgba(255,255,255,.3)" stroke="${INK}" stroke-width="9" stroke-linecap="round">${f}</g><g fill="none" stroke="${c}" stroke-width="4.5" stroke-linecap="round">${f}</g>`; }},
  scarf: {vb: "54 116 92 76", body: c => `<path d="M104 134 L124 132 L128 178 L108 180 Z" fill="${c}" ${S3}/><path d="M109 146 L125 145 M110 160 L126 159" stroke="${LIGHT}" stroke-width="5"/><path d="M111 181 v7 M116 181 v7 M121 180 v7 M126 179 v7" stroke="${INK}" stroke-width="2.5" stroke-linecap="round"/><path d="M60 122 Q100 136 140 122 Q145 131 140 140 Q100 154 60 140 Q55 131 60 122 Z" fill="${c}" ${S3}/><path d="M80 131 v14 M100 134 v14 M120 131 v14" stroke="${LIGHT}" stroke-width="6"/>`},
  tie: {vb: "84 124 32 78", body: c => `<path d="M96 139 L104 139 L111 184 L100 197 L89 184 Z" fill="${c}" ${S3}/><path d="M92 127 L108 127 L104 141 L96 141 Z" fill="${c}" ${S3}/><path d="M98 152 H103 M96 166 H105 M95 180 H106" stroke="${LIGHT}" stroke-width="3"/>`},
  "T-shirt": {vb: "34 122 132 90", arm: c => sleeve(c, 57, 161), body: c => `<path d="M58 146 Q60 130 84 128 Q100 138 116 128 Q140 130 142 146 L146 194 Q100 208 54 194 Z" fill="${c}" ${S3}/><path d="M84 129 Q100 141 116 129" fill="none" stroke="${INK}" stroke-width="3"/><path d="M78 172 Q100 182 122 172" stroke="${LIGHT}" stroke-width="5" fill="none"/>`},
  dress: {vb: "42 126 116 106", body: c => `<path d="M70 132 Q100 142 130 132 Q148 148 146 170 L156 216 Q100 232 44 216 L54 170 Q52 148 70 132 Z" fill="${c}" ${S3}/><path d="M56 170 Q100 182 144 170" stroke="${LIGHT}" stroke-width="7" fill="none"/><path d="M50 206 Q100 222 150 206" stroke="${LIGHT}" stroke-width="4" stroke-dasharray="1 9" stroke-linecap="round" fill="none"/>`},
  coat: {vb: "30 120 140 114", arm: c => sleeve(c, 46, 179), body: c => `<path d="M56 146 Q58 126 84 126 L100 138 L116 126 Q142 126 144 146 L150 216 Q100 230 50 216 Z" fill="${c}" ${S3}/><path d="M100 138 V223" stroke="${INK}" stroke-width="3"/><path d="M84 126 L92 152 L100 138 L108 152 L116 126" fill="${DARK}" stroke="${INK}" stroke-width="2.5" stroke-linejoin="round"/><g fill="${DARK}" stroke="${INK}" stroke-width="2"><circle cx="108" cy="166" r="4"/><circle cx="108" cy="184" r="4"/><circle cx="108" cy="202" r="4"/></g>`},
  trousers: {vb: "52 174 96 66", body: c => `<path d="M57 180 Q100 198 143 180 L141 200 L132 236 L104 236 L102 208 L98 208 L96 236 L68 236 L59 200 Z" fill="${c}" ${S3}/><path d="M58 186 Q100 204 142 186" stroke="${DARK}" stroke-width="6" fill="none"/>`},
  socks: {vb: "64 202 72 44", body: c => mir(`<rect x="76" y="206" width="20" height="28" rx="5" fill="${c}" ${S3}/><ellipse cx="84" cy="235" rx="14" ry="7.5" fill="${c}" ${S3}/><rect x="77.5" y="210" width="17" height="5" fill="${LIGHT}"/>`)},
  shoes: {vb: "60 220 80 28", body: c => mir(`<path d="M64 238 Q64 224 80 224 Q96 224 99 236 Q99 243 84 243 L68 243 Q64 243 64 238 Z" fill="${c}" ${S3}/><path d="M64 239 H99" stroke="${INK}" stroke-width="2.5"/><path d="M80 228 l6 2 M79 233 l7 1" stroke="${LIGHT}" stroke-width="2.5" stroke-linecap="round"/>`)},
  boots: {vb: "58 198 84 48", body: c => mir(`<path d="M73 206 H97 V232 Q99 243 84 243 L68 243 Q63 243 63 237 Q63 230 73 230 Z" fill="${c}" ${S3}/><rect x="70" y="200" width="30" height="9" rx="4" fill="${c}" ${S3}/><path d="M63 238 H98" stroke="${INK}" stroke-width="2.5"/>`)},
  gloves: {vb: "4 4 92 42", arm: glove, icon: c => `<g transform="translate(30 24)">${mitten(c)}</g><g transform="translate(70 24) scale(-1 1)">${mitten(c)}</g>`}
};
const AT = {head: [100, 34], eyes: [100, 86], neck: [100, 134], top: [100, 166], outer: [100, 170], legs: [100, 212], socks: [100, 228], feet: [100, 236], hands: [42, 188]};
const icon = (en, c) => { const d = DRAW[en]; return `<svg viewBox="${d.vb}" aria-hidden="true">${d.icon ? d.icon(c) : (d.body ? d.body(c) : "") + (d.arm ? mir(d.arm(c)) : "")}</svg>`; };

const EARS = {
  bunny: sk => mir(`<ellipse cx="78" cy="32" rx="11" ry="30" transform="rotate(-14 78 32)" fill="${sk}" ${S3}/><ellipse cx="78" cy="34" rx="5" ry="20" transform="rotate(-14 78 34)" fill="${PINK}"/>`),
  bear: sk => mir(`<circle cx="63" cy="54" r="15" fill="${sk}" ${S3}/><circle cx="63" cy="54" r="7" fill="${PINK}"/>`),
  cat: sk => mir(`<path d="M58 72 L60 30 L92 50 Z" fill="${sk}" ${S3}/><path d="M65 60 L66 42 L82 52 Z" fill="${PINK}"/>`),
  bug: () => mir(`<path d="M88 50 Q82 28 70 18" fill="none" stroke="${INK}" stroke-width="3" stroke-linecap="round"/><circle cx="69" cy="16" r="7" fill="#FFC43D" ${S3}/>`),
  horns: () => mir(`<path d="M68 58 Q58 36 70 22 Q74 42 86 50 Z" fill="#FFF3D6" ${S3}/>`),
  tuft: sk => `<path d="M94 48 Q92 24 110 24 Q100 30 104 46 Z" fill="${sk}" ${S3}/><path d="M100 47 Q106 32 120 34 Q110 38 108 48 Z" fill="${sk}" ${S3}/>`
};
const CRITTERS = [["#C9B6FF", "bunny"], ["#A8E6CF", "bug"], ["#FFD3B6", "bear"], ["#FFF1A8", "cat"], ["#BDE0FE", "horns"], ["#FFC8DD", "tuft"]];
function critter([sk, ears]){
  const arm = `<line x1="64" y1="148" x2="42" y2="186" stroke="${INK}" stroke-width="17" stroke-linecap="round"/><line x1="64" y1="148" x2="42" y2="186" stroke="${sk}" stroke-width="11" stroke-linecap="round"/><g data-arm="top"></g><g data-arm="outer"></g><circle cx="41" cy="189" r="10" fill="${sk}" ${S3}/><g data-arm="hands"></g>`;
  const eye = x => `<g class="hb-eye"><ellipse cx="${x}" cy="85" rx="11" ry="13" fill="#fff" ${S3}/><g class="hb-pupil"><circle cx="${x + 1}" cy="87" r="6" fill="${INK}"/><circle cx="${x + 3.5}" cy="84" r="2.2" fill="#fff"/></g></g>`;
  const mouths = `<path class="hb-mouth" data-m="smile" d="M89 111 Q100 121 111 111" fill="none" stroke="${INK}" stroke-width="3" stroke-linecap="round"/>
    <path class="hb-mouth" data-m="cold" d="M86 114 l4.7 -4 l4.7 4 l4.7 -4 l4.7 4 l4.7 -4 l4.7 4" fill="none" stroke="${INK}" stroke-width="3" stroke-linecap="round" stroke-linejoin="round"/>
    <ellipse class="hb-mouth" data-m="open" cx="100" cy="115" rx="7" ry="8" fill="#7A2230" ${S3}/>
    <path class="hb-mouth" data-m="grin" d="M85 108 Q100 130 115 108 Z" fill="#7A2230" ${S3}/>`;
  return `<svg class="hb-svg" viewBox="0 0 200 250" aria-hidden="true">
  <ellipse cx="100" cy="245" rx="54" ry="5" fill="rgba(27,45,69,.18)"/>
  <g class="hb-kid">${mir(`<rect x="77" y="196" width="18" height="40" rx="9" fill="${sk}" ${S3}/><ellipse cx="84" cy="236" rx="13" ry="7" fill="${sk}" ${S3}/>`)}
   <g data-slot="socks"></g><ellipse cx="100" cy="168" rx="44" ry="42" fill="${sk}" ${S3}/><ellipse cx="100" cy="178" rx="26" ry="24" fill="rgba(255,255,255,.35)"/>
   <g data-slot="legs"></g><g data-slot="feet"></g><g data-slot="top"></g><g data-slot="outer"></g><g data-slot="neck"></g>
   <g class="hb-armL">${arm}</g><g class="hb-armR"><g transform="translate(200 0) scale(-1 1)">${arm}</g></g>
   <g class="hb-head">${EARS[ears](sk)}<circle cx="100" cy="88" r="44" fill="${sk}" ${S3}/><circle class="hb-cold" cx="100" cy="88" r="42.5" fill="#8FD3FF" opacity=".55"/>
    ${eye(84)}${eye(116)}<ellipse class="hb-cheek" cx="67" cy="104" rx="9" ry="6" fill="#FF8FA3" opacity=".65"/><ellipse class="hb-cheek" cx="133" cy="104" rx="9" ry="6" fill="#FF8FA3" opacity=".65"/>
    <ellipse cx="100" cy="100" rx="5.5" ry="4.5" fill="#FF8C7A" stroke="${INK}" stroke-width="2"/>${mouths}<g data-slot="eyes"></g><g data-slot="head"></g></g>
  </g></svg>`;
}

/* ---------- grammar: "the red hat", "den roten Hut", "de rouden Hutt", "那顶红色的帽子" ---------- */
const bare = s => s.replace(/^(der |die |das |den |de |d')/, "");
function npDe(t, acc, colour){
  const g = H.gear[t.g.en].de, art = {m: acc ? "den" : "der", f: "die", n: "das", pl: "die"}[g], n = bare(t.g.de);
  if (!colour) return `${art} ${n}`;
  const a = t.c.de, end = H.deFixed.includes(a) ? "" : g === "pl" || (g === "m" && acc) ? "en" : "e";
  return `${art} ${a}${end} ${n}`;
}
// Eifel rule: a final n stays only before a vowel or d, t, z, h, n; "orangen" then becomes "orangë" (lod.lu: en orangë Pullover)
const eifel = (w, next) => /n$/.test(w) && !/^[aeiouäëéèdtzhn]/i.test(next) ? (w === "orangen" ? "orangë" : w.slice(0, -1)) : w;
function npLb(t, colour){
  if (!colour) return t.g.lb;
  const g = H.gear[t.g.en].lb, a = H.lbAdj[t.c.en][["m", "f", "n", "pl"].indexOf(g)], n = bare(t.g.lb);
  return g === "m" ? `${eifel("den", a)} ${eifel(a, n)} ${n}` : `${g === "n" ? "dat" : "déi"} ${a} ${n}`;
}
const npZh = (t, colour, clf) => (clf ? "那" + H.gear[t.g.en].zh[0] : "") + (colour ? t.c.zh + "的" : "") + t.g.zh;
const npEn = (t, colour) => `the ${colour ? t.c.en + " " : ""}${t.g.en}`;
function npList(ts, L, colour){
  const ps = ts.map(t => L === "de" ? npDe(t, true, colour) : L === "lb" ? npLb(t, colour) : L === "zh" ? npZh(t, colour, true) : npEn(t, colour));
  return ps.length === 1 ? ps[0] : ps.slice(0, -1).join(H.list[L]) + H.and[L] + ps[ps.length - 1];
}
function gotLine(t, L, colour){
  if (L === "zh") return `我${H.gear[t.g.en].zh[1]}上${npZh(t, colour, false)}啦！`;
  return cap(L === "de" ? npDe(t, false, colour) : L === "lb" ? npLb(t, colour) : npEn(t, colour)) + "!";
}

/* ---------- funny sounds (Web Audio, silent in the recette) ---------- */
const snd = (() => {
  const ctx = () => { try { return (ac = ac || new (window.AudioContext || window.webkitAudioContext)()); } catch (e) { return null; } };
  function glide(f1, f2, dur, type = "sine", at = 0, vol = .2, lp = 0, wob = 0){
    const c = TEST ? null : ctx(); if (!c) return;
    const t = c.currentTime + at, o = c.createOscillator(), g = c.createGain(); let out = o;
    o.type = type; o.frequency.setValueAtTime(f1, t); o.frequency.exponentialRampToValueAtTime(f2, t + dur);
    if (wob) { const l = c.createOscillator(), lg = c.createGain(); l.frequency.value = wob; lg.gain.value = f2 * .25; l.connect(lg); lg.connect(o.frequency); l.start(t); l.stop(t + dur + .05); }
    if (lp) { const f = c.createBiquadFilter(); f.type = "lowpass"; f.frequency.value = lp; o.connect(f); out = f; }
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    out.connect(g); g.connect(c.destination); o.start(t); o.stop(t + dur + .05);
  }
  function noise(dur, at, freq, vol){
    const c = TEST ? null : ctx(); if (!c) return;
    const b = c.createBuffer(1, Math.floor(c.sampleRate * dur), c.sampleRate), d = b.getChannelData(0);
    for (let k = 0; k < d.length; k++) d[k] = (Math.random() * 2 - 1) * (1 - k / d.length);
    const s = c.createBufferSource(), f = c.createBiquadFilter(), g = c.createGain();
    s.buffer = b; f.type = "bandpass"; f.frequency.value = freq; f.Q.value = .8; g.gain.value = vol;
    s.connect(f); f.connect(g); g.connect(c.destination); s.start(c.currentTime + at);
  }
  return {
    whoosh: () => glide(900, 300, .3, "triangle", 0, .12),
    spin: () => { glide(300, 1200, .3, "triangle", 0, .15); glide(200, 520, .16, "sine", .32, .2); glide(520, 260, .2, "sine", .46, .18); },
    sneeze: () => { glide(260, 420, .28, "sine", 0, .14); glide(300, 540, .3, "sine", .32, .16); noise(.35, .66, 2600, .5); },
    giggle: () => [990, 1250, 1050, 1320, 1100].forEach((f, k) => glide(f, f * 1.15, .08, "triangle", k * .09, .13)),
    burp: () => glide(150, 60, .6, "sawtooth", 0, .3, 500, 18),
    toot: () => glide(190, 90, .6, "sawtooth", 0, .3, 650, 28),
    hic: () => { glide(600, 1100, .09, "sine", 0, .2); glide(600, 1100, .09, "sine", .45, .2); },
    chatter: () => { for (let k = 0; k < 6; k++) glide(160, 130, .04, "square", k * .07, .06, 900); },
    tada: () => tone([523, 659, 784, 1047, 1319], .12)
  };
})();

registerGame({id: "habille", em: "🧥", name: "Habille-moi !", desc: "Habille le petit frileux", multi: true, title: H.title, sub: H.sub}, function () {
  const lvl = levelOf("habille"), total = 6, res = [], pool = wordsOf("clothes", lvl), colour = lvl > 1;
  const COLS = [["red", "blue", "green", "yellow", "pink", "purple"], ["red", "blue", "green", "yellow", "pink", "purple", "orange", "black"]][lvl - 1];
  const cols = THEMES.colors.words.filter(c => (c.lvl || 1) === 1 && (!COLS || COLS.includes(c.en)));
  startSession("habille", null, total); const gen = GEN;
  let i = 0, cur = null, busy = false, deck = shuffle(pool), btns = [], popT = 0; const worn = {};
  const body = $("gameBody"); body.innerHTML = "";
  const wrap = el("div", "hb"), top = el("div", "hb-top"), bubble = el("div", "hb-bubble"), grid = el("div", "hb-grid");
  const rep = el("button", "hb-say chunky" + (lvl < 4 ? " speak" : ""), "🔊");
  const flakes = [...Array(7)].map((_, k) => `<span style="left:${6 + k * 13}%; animation-duration:${4 + rnd(4)}s; animation-delay:-${rnd(5)}s">❄️</span>`).join("");
  const stage = el("div", "hb-stage", `<div class="hb-sun">☀️</div><div class="hb-snow">${flakes}</div>${critter(oneOf(CRITTERS))}<div class="hb-pop"></div>`);
  top.append(rep, bubble); wrap.append(top, stage, grid); body.append(wrap);
  const svg = stage.querySelector("svg"), kid = svg.querySelector(".hb-kid"), head = svg.querySelector(".hb-head");
  const eyes = [...svg.querySelectorAll(".hb-eye")], pupils = [...svg.querySelectorAll(".hb-pupil")], cheeks = [...svg.querySelectorAll(".hb-cheek")];
  const armL = svg.querySelector(".hb-armL"), armR = svg.querySelector(".hb-armR"), snow = stage.querySelector(".hb-snow"), sun = stage.querySelector(".hb-sun"), popEl = stage.querySelector(".hb-pop");
  if (!fx.calm()) { snow.classList.add("go"); sun.classList.add("go"); }
  const box = $("flagBar"); // the header flags: a tap repaints the clothes in the new language
  if (hbSync) box.removeEventListener("click", hbSync);
  hbSync = () => { if (alive(gen)) paint(); }; box.addEventListener("click", hbSync);

  /* ---------- faces and moves (transform and opacity only, nothing in the recette) ---------- */
  const A = (e, k, o) => fx.calm() || !e ? Promise.resolve() : e.animate(k, o).finished.catch(() => {});
  const wait = ms => new Promise(r => loops.push(setTimeout(r, ms)));
  const warmth = () => Math.min(1, Object.keys(worn).length / 5);
  const mouth = m => svg.querySelectorAll(".hb-mouth").forEach(p => p.style.opacity = p.dataset.m === m ? 1 : 0);
  const rest = () => { if (alive(gen)) mouth(warmth() < .6 ? "cold" : "smile"); };
  function warm(){ const w = warmth(); svg.querySelector(".hb-cold").style.opacity = (.55 * (1 - w)).toFixed(2); snow.style.opacity = (1 - w).toFixed(2); sun.style.opacity = w.toFixed(2); }
  function pop(t){
    popEl.textContent = t; popEl.style.opacity = 1; clearTimeout(popT);
    A(popEl, [{transform: "translateX(-50%) scale(.3)"}, {transform: "translateX(-50%) scale(1.15)", offset: .6}, {transform: "translateX(-50%) scale(1)"}], {duration: 320, easing: "ease-out"});
    popT = setTimeout(() => popEl.style.opacity = 0, 2000); loops.push(popT);
  }
  // an emoji flying out of the critter (sneeze drops, burp bubble, music notes)
  function puff(txt, vx, vy, dx, dy, ms = 700, size = 24){
    if (fx.calm()) return;
    const k = svg.getBoundingClientRect(), s = stage.getBoundingClientRect(), d = el("span", "hb-fx", txt);
    d.style.cssText = `left:${k.left - s.left + vx / 200 * k.width}px; top:${k.top - s.top + vy / 250 * k.height}px; font-size:${size}px`;
    stage.append(d);
    d.animate([{transform: "translate(-50%,-50%) scale(.4)", opacity: 1}, {transform: `translate(calc(-50% + ${dx}px), calc(-50% + ${dy}px)) scale(1.2)`, opacity: 0}], {duration: ms, easing: "ease-out"}).onfinish = () => d.remove();
  }
  const blink = () => eyes.forEach(e => A(e, [{transform: "scaleY(1)"}, {transform: "scaleY(.1)"}, {transform: "scaleY(1)"}], {duration: 180}));
  const shiver = () => A(kid, [0, -3, 3, -3, 3, -2, 0].map(x => ({transform: `translateX(${x}px)`})), {duration: 450});
  const wave = (n = 1) => { A(armL, [{transform: "none"}, {transform: "rotate(55deg)"}, {transform: "rotate(15deg)"}, {transform: "rotate(55deg)"}, {transform: "none"}], {duration: 900, iterations: n}); A(armR, [{transform: "none"}, {transform: "rotate(-55deg)"}, {transform: "rotate(-15deg)"}, {transform: "rotate(-55deg)"}, {transform: "none"}], {duration: 900, iterations: n}); };
  const puffCheeks = () => cheeks.forEach(c => A(c, [{transform: "scale(1)"}, {transform: "scale(1.7)"}, {transform: "scale(1)"}], {duration: 700}));
  const spin = () => A(kid, [{transform: "none"}, {transform: "translateY(-30px) scaleX(.1)", offset: .25}, {transform: "translateY(-40px) scaleX(-1)", offset: .5}, {transform: "translateY(-30px) scaleX(.1)", offset: .75}, {transform: "scale(1.14,.86)", offset: .9}, {transform: "none"}], {duration: 700, easing: "ease-in-out"});
  function idle(){
    if (!alive(gen) || fx.calm()) return;
    if (rnd(3) || busy) blink(); else if (warmth() < .6) shiver(); else A(head, [{transform: "none"}, {transform: "rotate(-8deg)"}, {transform: "rotate(6deg)"}, {transform: "none"}], {duration: 900});
    loops.push(setTimeout(idle, 1800 + rnd(2400)));
  }
  function look(e){
    if (fx.calm()) return;
    const k = svg.getBoundingClientRect(), dx = e.clientX - (k.left + k.width / 2), dy = e.clientY - (k.top + k.height * .34), d = Math.hypot(dx, dy) || 1;
    pupils.forEach(p => p.style.transform = `translate(${(dx / d * 3.5).toFixed(1)}px,${(dy / d * 3).toFixed(1)}px)`);
  }
  wrap.addEventListener("pointermove", look); wrap.addEventListener("pointerdown", look);
  function happy(){
    if (fx.calm()) return rest();
    mouth("grin"); wave(); puffCheeks();
    const k = svg.getBoundingClientRect(); fx.sparkle(k.left + k.width / 2, k.top + k.height * .4, 10);
    loops.push(setTimeout(rest, 1100));
  }
  function sneeze(){
    snd.sneeze();
    if (fx.calm()) return;
    mouth("open");
    A(head, [{transform: "none"}, {transform: "rotate(-12deg) translateY(-5px)", offset: .55}, {transform: "rotate(14deg) translateY(6px)", offset: .7}, {transform: "rotate(-4deg)", offset: .85}, {transform: "none"}], {duration: 1000});
    A(kid, [{transform: "none"}, {transform: "scale(.95,1.07)", offset: .55}, {transform: "scale(1.15,.85)", offset: .72}, {transform: "none"}], {duration: 1000});
    eyes.forEach(e => A(e, [{transform: "scaleY(1)"}, {transform: "scaleY(.15)", offset: .5}, {transform: "scaleY(.15)", offset: .8}, {transform: "scaleY(1)"}], {duration: 1000}));
    loops.push(setTimeout(() => { for (let k = 0; k < 6; k++) puff("💦", 106, 104, 30 + rnd(70), rnd(60) - 25, 650, 14 + rnd(10)); }, 700));
    loops.push(setTimeout(rest, 1100));
  }
  function tickle(){
    if (!alive(gen)) return;
    const L = hbLang(), line = oneOf(H.tickle[L]);
    snd.giggle(); pop(line);
    if (L !== "lb" && !busy) say(line, L);
    if (fx.calm()) return;
    mouth("grin"); puffCheeks();
    A(kid, [0, -7, 7, -6, 6, 0].map(r => ({transform: `rotate(${r}deg)`})), {duration: 520});
    eyes.forEach(e => A(e, [{transform: "scaleY(1)"}, {transform: "scaleY(.35)"}, {transform: "scaleY(1)"}], {duration: 520}));
    loops.push(setTimeout(rest, 800));
  }
  svg.addEventListener("click", tickle);
  // a surprise after some rounds: a burp, a toot or hiccups
  async function gag(){
    const L = hbLang(), k = rnd(3);
    if (k === 0) { snd.burp(); mouth("open"); puff("🫧", 100, 112, -10, -90, 900, 28); A(head, [{transform: "none"}, {transform: "translateY(-6px) rotate(-6deg)"}, {transform: "none"}], {duration: 600}); }
    else if (k === 1) { snd.toot(); puff("💨", 56, 212, -70, 8, 800, 32); A(kid, [{transform: "none"}, {transform: "translateY(-20px) rotate(-5deg)"}, {transform: "none"}], {duration: 420}); eyes.forEach(e => A(e, [{transform: "scale(1)"}, {transform: "scale(1.35)"}, {transform: "scale(1)"}], {duration: 600})); }
    else { snd.hic(); pop(H.pop.hic[L]); [0, 450].forEach(t => loops.push(setTimeout(() => A(kid, [{transform: "none"}, {transform: "translateY(-16px)"}, {transform: "none"}], {duration: 260}), t))); }
    await wait(650); if (!alive(gen)) return;
    if (k < 2) { const line = oneOf(H.sorry[L]); pop(line); await say(L === "lb" ? "" : line, L); }
    await wait(250); rest();
  }
  function fly(b, s){
    if (fx.calm()) return Promise.resolve();
    const ic = b.querySelector("svg"), r = ic.getBoundingClientRect(), k = svg.getBoundingClientRect(), [ax, ay] = AT[s], c = ic.cloneNode(true);
    c.style.cssText = `position:fixed; left:${r.left}px; top:${r.top}px; width:${r.width}px; height:${r.height}px; z-index:60; pointer-events:none`;
    document.body.append(c); snd.whoosh();
    const dx = k.left + ax / 200 * k.width - r.left - r.width / 2, dy = k.top + ay / 250 * k.height - r.top - r.height / 2;
    return c.animate([{transform: "none"}, {transform: `translate(${dx / 2}px,${dy / 2 - 70}px) rotate(-25deg) scale(1.15)`, offset: .5}, {transform: `translate(${dx}px,${dy}px) scale(.45)`, opacity: .4}], {duration: 450, easing: "ease-in-out"}).finished.catch(() => {}).then(() => c.remove());
  }
  function wear(t){
    const d = DRAW[t.g.en], s = slotOf(t.g), parts = []; worn[s] = t;
    const g = svg.querySelector(`[data-slot="${s}"]`); if (g) { g.innerHTML = d.body ? d.body(t.c.e) : ""; parts.push(g); }
    svg.querySelectorAll(`[data-arm="${s}"]`).forEach(a => { a.innerHTML = d.arm ? d.arm(t.c.e) : ""; parts.push(a); });
    parts.forEach(p => A(p, [{transform: "scale(.2)", opacity: 0}, {transform: "scale(1.2)", opacity: 1, offset: .6}, {transform: "scale(1)"}], {duration: 380, easing: "ease-out"}));
    warm();
  }

  /* ---------- voice and bubble ---------- */
  async function sayLb(parts){ for (const p of parts) { await say(p, "lb"); if (!alive(gen)) return; } }
  // Luxembourgish has no voice: the lod.lu recordings of the garment, then of its colour
  const lbParts = ts => ts.flatMap(t => colour ? [t.g.lb, t.c.lb] : [t.g.lb]);
  const speak = (text, parts) => hbLang() === "lb" ? sayLb(parts) : say(text, hbLang());
  const askText = (L, ts) => H.ask[L][cur.frame % H.ask[L].length].replace("{x}", npList(ts, L, colour));
  const lead = L => cur.lead ? cur.lead[L] + " " : "";
  function paint(){
    if (!cur) return;
    const L = hbLang();
    // always written, the little one included (Kezhan: a chance to read); level 4 is read without the voice
    bubble.innerHTML = (cur.lead ? `<small>${cur.lead[L]}</small>` : "") + `<b>${askText(L, cur.ts)}</b>`;
  }
  rep.onclick = () => {
    if (!cur || !alive(gen)) return;
    if (lvl === 4) G.hints++; else G.replays++;
    paint(); const L = hbLang(), ts = cur.todo.length ? cur.todo : cur.ts;
    speak(lead(L) + askText(L, ts), lbParts(ts));
  };

  /* ---------- rounds ---------- */
  function makeRound(){
    const r = {frame: rnd(8), lead: null, tries: 0};
    if (lvl >= 3) {
      const ld = oneOf(H.lead), k = lvl === 4 && rnd(2) ? 3 : 2, ts = [];
      shuffle(ld.gear.map(byEn).filter(g => pool.includes(g))).forEach(g => { if (ts.length < k && !ts.some(t => slotOf(t.g) === slotOf(g))) ts.push({g}); });
      r.lead = ld; r.ts = ts;
    } else {
      if (!deck.length) deck = shuffle(pool);
      r.ts = [{g: deck.pop()}];
    }
    pick(cols, r.ts.length).forEach((c, k) => r.ts[k].c = c);
    r.todo = r.ts.slice(); return r;
  }
  function choices(r){
    const n = [3, 4, 6, 8][lvl - 1], out = r.ts.slice(), seen = new Set(out.map(o => o.g.en + o.c.en));
    const add = (g, c) => { if (out.length < n && !seen.has(g.en + c.en) && !(lvl === 1 && out.some(o => o.g === g))) { seen.add(g.en + c.en); out.push({g, c}); } };
    // traps: the right garment in another colour, another garment in the right colour
    if (lvl > 1) r.ts.forEach(t => { add(t.g, oneOf(cols.filter(c => c !== t.c))); add(oneOf(pool.filter(g => g !== t.g)), t.c); });
    for (let k = 0; out.length < n && k < 300; k++) add(oneOf(pool), oneOf(cols));
    return shuffle(out);
  }
  const matches = (o, t) => o.g === t.g && o.c === t.c;
  const btnOf = t => btns.find(b => !b.disabled && matches(b.hbo, t));
  function mark(){ if (!TEST || !cur) return; btns.forEach(b => delete b.dataset.ok); const b = cur.todo[0] && btnOf(cur.todo[0]); if (b) markOk(b); }
  function next(){
    if (!alive(gen)) return;
    if (i >= total) return show();
    cur = makeRound(); busy = false;
    renderDots(res, total, i); paint();
    const opts = choices(cur);
    grid.className = "hb-grid" + (opts.length % 4 ? "" : " n4"); grid.innerHTML = "";
    btns = opts.map(o => { const b = el("button", "hb-opt chunky", icon(o.g.en, o.c.e)); b.hbo = o; b.onclick = () => tap(b); grid.append(b); return b; });
    mark();
    const L = hbLang(), brr = i === 0 && lvl < 3 ? oneOf(H.cold[L]) : ""; // levels 3 and 4 open with the situation
    if (warmth() < .6) { pop(brr || "Brrr!"); shiver(); snd.chatter(); }
    if (lvl < 4) speak((brr ? brr + " " : "") + (lvl === 3 ? lead(L) : "") + askText(L, cur.ts), lbParts(cur.ts));
  }
  async function tap(b){
    if (!alive(gen) || busy || b.disabled || !cur) return;
    G.taps++;
    const r = cur, t = r.todo.find(x => matches(b.hbo, x));
    if (!t) return miss(b);
    r.todo = r.todo.filter(x => x !== t); const last = !r.todo.length;
    if (last) busy = true;
    b.disabled = true; b.classList.add("ok"); mark(); sfx.pop();
    await fly(b, slotOf(t.g)); if (!alive(gen)) return;
    wear(t); snd.spin(); await spin(); if (!alive(gen)) return;
    if (last) return done(r);
    if (!fx.calm()) fx.sparkle();
    speak(gotLine(t, hbLang(), colour), [t.g.lb]);
  }
  function miss(b){
    const r = cur; r.tries++; sfx.ko(); sneeze();
    b.classList.add("no"); loops.push(setTimeout(() => b.classList.remove("no"), 700));
    const L = hbLang(), line = `${H.pop.sneeze[L]} ${oneOf(H.oops[L])}`;
    pop(line);
    if (lvl < 4) speak(line + " " + askText(L, r.todo), lbParts(r.todo));
    // the little one gets a nudge after two misses so the game never stalls
    if (r.tries >= 2 && S.kid === "p4") { const k = btnOf(r.todo[0]); if (k) k.classList.add("bob"); }
  }
  async function done(r){
    const first = r.tries === 0; if (first) addStar(); sfx.ok();
    logRound(r.ts.map(t => (colour ? t.c.en + " " : "") + t.g.en).join(" + "), first, r.tries + 1, {lvl});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    happy();
    const L = hbLang(), line = (r.ts.length === 1 ? gotLine(r.ts[0], L, colour) + " " : "") + oneOf(H.thanks[L]);
    pop(line);
    await speak(line, r.ts.length === 1 ? [r.ts[0].g.lb] : []); if (!alive(gen)) return;
    if (!fx.calm() && rnd(10) < 3) { await gag(); if (!alive(gen)) return; }
    i++; loops.push(setTimeout(next, fx.calm() ? 300 : 500));
  }
  // the end: a little fashion show
  async function show(){
    busy = true;
    if (!fx.calm()) {
      const L = hbLang(), line = oneOf(H.show[L]);
      pop(line); snd.tada(); mouth("grin"); wave(2); puffCheeks();
      A(kid, [{transform: "none"}, {transform: "translateY(-24px) rotate(-8deg)"}, {transform: "none"}, {transform: "translateY(-24px) rotate(8deg)"}, {transform: "none"}, {transform: "translateY(-34px) scaleX(-1)"}, {transform: "none"}], {duration: 1600, easing: "ease-in-out"});
      for (let k = 0; k < 7; k++) loops.push(setTimeout(() => puff(oneOf(["🎵", "🎶", "⭐", "💖"]), k % 2 ? 40 : 160, 90, rnd(50) - 25, -100, 1000, 24), k * 220));
      await Promise.all([speak(line, []), wait(1700)]); if (!alive(gen)) return;
    }
    finish();
  }
  warm(); rest(); loops.push(setTimeout(idle, 1500));
  next();
});
})();
