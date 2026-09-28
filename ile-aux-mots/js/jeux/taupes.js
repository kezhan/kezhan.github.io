/* L'Île aux Mots : jeu « taupes » (Tape-taupes).
   Moles pop out of their holes holding a picture; the voice says a word: bop the mole that holds it.
   1: 3 holes, slow moles · 2: 5 holes, faster · 3: 7 holes, two moles at once, mixed themes, sound-alike traps
   4: the word is written on a sign, no voice, two words at the end (the little one keeps the voice).
   English, German, Luxembourgish or Chinese, never French. Texts in js/contenus/taupes.js. */
(() => {
const C = TAUPES_C, ID = "taupes";
const LG = () => ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
const tx = o => o[LG()] || o.en;
const any = a => a[rnd(a.length)];
const calm = () => fx.calm();
const anim = (n, frames, o) => { if (!n || calm()) return null; try { return n.animate(frames, o); } catch(e) { return null; } };
const THEMES_OK = ["animals","food","body","things","clothes","jobs"]; // nouns a mole can hold
const LAYOUT = {3:[3], 5:[3,2], 7:[2,3,2]};
const PARTY = `<svg viewBox="0 0 30 34" width="1em" height="1.1em"><path d="M15 2 L27 32 H3 Z" fill="#FF6F59" stroke="#1B2D45" stroke-width="2.5"/><path d="M9 17 L21 17 M6 25 L24 25" stroke="#FFC43D" stroke-width="3"/><circle cx="15" cy="3" r="3.5" fill="#FFC43D" stroke="#1B2D45" stroke-width="2"/></svg>`;
const HATS = ["🎀", "👑", "🌱", PARTY]; // never when the word itself is a hat or a cap

// the word inside a sentence: German nominative or accusative, the others as the lexicon gives them
function noun(w, form){
  const lang = LG();
  if (lang === "de" && form === "a") return C.acc[w.de] || w.de.replace(/^der /, "den ");
  return w[lang] || w.en;
}
function fill(t, w){
  if (Array.isArray(t)) t = t[(C.plural[LG()] || []).includes(w.en) ? 1 : 0];
  return t.replace(/\{[wn]\}/g, () => noun(w, "n")).replace("{a}", noun(w, "a"));
}

/* funny sounds, made with the Web Audio API */
const snd = (() => {
  let c = null, noise = null;
  const ctx = () => { if (TEST) return null; try { c = c || new (window.AudioContext || window.webkitAudioContext)(); if (c.state === "suspended") c.resume(); return c; } catch(e) { return null; } };
  // a sliding note; lfo = [speed, depth, "gain" for a trembling volume, else the pitch wobbles]
  function note(type, f0, f1, dur, vol = .2, at = 0, lfo){
    const a = ctx(); if (!a) return;
    const t = a.currentTime + at, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination);
    if (lfo) { const l = a.createOscillator(), lg = a.createGain(); l.frequency.value = lfo[0]; lg.gain.value = lfo[1]; l.connect(lg); lg.connect(lfo[2] === "gain" ? g.gain : o.frequency); l.start(t); l.stop(t + dur); }
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
    pop: () => note("sine", 260, 760, .09, .1), hi: () => note("triangle", 600, 950, .14, .08),
    bonk: () => { hiss(.08, .6, 0, 1200, 300); note("triangle", 200, 70, .14, .3); },
    thud: () => { hiss(.12, .4, 0, 500, 150); note("sine", 120, 50, .16, .25); },
    boing: () => note("sine", 170, 540, .5, .22, .05, [15, 80]),
    raspberry: () => note("sawtooth", 130, 85, .55, .12, .05, [30, .08, "gain"]),
    giggle: () => [0, 1, 2, 3, 4].forEach(k => note("triangle", 780 + (k % 2) * 200, 950 + (k % 2) * 160, .08, .09, k * .11)),
    sneeze: () => { note("triangle", 300, 560, .3, .08); hiss(.25, .5, .34, 2500, 6000); },
    burp: () => note("sawtooth", 95, 55, .5, .16, 0, [22, 16]), yawn: () => note("sine", 520, 230, .9, .07, 0, [5, 25]),
    coin: () => { note("square", 988, 988, .08, .07); note("square", 1319, 1319, .32, .07, .08); }
  };
})();

// the mole, drawn: ears, body, eyes with lids (blink) and crosses (dizzy), cheeks, whiskers, buck teeth, pink nose
const SVG = `<svg class="tp-svg" viewBox="0 0 100 128" aria-hidden="true"><circle class="tp-ear" cx="19" cy="27" r="9"/><circle class="tp-ear" cx="81" cy="27" r="9"/>
<path class="tp-body" d="M8 132 L8 52 Q8 8 50 8 Q92 8 92 52 L92 132 Z"/><ellipse class="tp-belly" cx="50" cy="104" rx="31" ry="28"/>
<g class="tp-eyes"><circle class="tp-white" cx="36" cy="38" r="10.5"/><circle class="tp-white" cx="64" cy="38" r="10.5"/><g class="tp-pup"><circle cx="37" cy="40" r="5.2"/><circle cx="65" cy="40" r="5.2"/>
<circle class="tp-shine" cx="35" cy="37.5" r="1.8"/><circle class="tp-shine" cx="63" cy="37.5" r="1.8"/></g><ellipse class="tp-lid" cx="36" cy="38" rx="11.8" ry="11.8"/><ellipse class="tp-lid" cx="64" cy="38" rx="11.8" ry="11.8"/></g>
<path class="tp-xe" d="M29 31 L43 45 M43 31 L29 45 M57 31 L71 45 M71 31 L57 45"/><circle class="tp-ck" cx="22" cy="55" r="7"/><circle class="tp-ck" cx="78" cy="55" r="7"/>
<path class="tp-wh" d="M30 57 L10 53 M30 61 L10 63 M70 57 L90 53 M70 61 L90 63"/><path class="tp-mouth" d="M41 61 Q50 69 59 61"/><path class="tp-teeth" d="M45.5 64.5 h9 v6 h-9 Z M50 64.5 v6"/>
<ellipse class="tp-o" cx="50" cy="66" rx="6" ry="7.5"/><ellipse class="tp-nose" cx="50" cy="54" rx="10" ry="7"/><ellipse cx="47" cy="52" rx="3" ry="1.8" fill="#fff" opacity=".7"/></svg>`;

addStyle(`
.tp-top{display:flex; gap:8px 10px; align-items:center; justify-content:center; flex-wrap:wrap}
.tp-top .speak{font-size:19px; padding:10px 16px}
.tp-sign{min-width:150px; max-width:100%; min-height:58px; padding:6px 18px; display:flex; flex-direction:column; justify-content:center; gap:2px; text-align:center; line-height:1.15;
  background:repeating-linear-gradient(0deg,#C98B4F 0 9px,#BC7F45 9px 11px); border:3px solid var(--ink); border-radius:14px; box-shadow:3px 4px 0 var(--ink);
  font-family:var(--display); font-size:clamp(22px,5.5vw,30px); font-weight:700; color:#fff; text-shadow:2px 2px 0 var(--ink)}
.tp-sign small{font-family:var(--body); font-size:15px; font-weight:800; color:#FFF3DA; text-shadow:1px 1px 0 var(--ink)}
.tp-sign .tp-w{display:inline-block; padding:0 5px} .tp-sign .tp-w.done{opacity:.5; text-decoration:line-through}
.tp-chip{font-size:20px; min-width:56px; min-height:48px}
.tp-garden{--tp-w:min(112px,22.5vw); position:relative; padding:14px 4px 6px; border:3px solid var(--ink); border-radius:22px;
  background:radial-gradient(circle at 20% 30%,rgba(255,255,255,.2) 0 5px,transparent 6px) 0 0/90px 70px, radial-gradient(circle at 70% 75%,rgba(40,110,40,.25) 0 6px,transparent 7px) 0 0/110px 80px, linear-gradient(#9BDB6A,#5DB548)}
.tp-row{display:flex; justify-content:center; gap:4px} .tp-row + .tp-row{margin-top:-52px}
.tp-n3{--tp-w:min(150px,22.5vw)} .tp-n5{--tp-w:min(130px,22.5vw)}
.tp-cell{position:relative; width:calc(var(--tp-w) * 1.1); height:calc(var(--tp-w) * 1.28 + 44px); pointer-events:none}
.tp-pit{position:absolute; left:50%; bottom:16px; width:calc(var(--tp-w) * 1.02); height:34px; transform:translateX(-50%); border-radius:50%; border:3px solid var(--ink); background:radial-gradient(ellipse at 50% 60%,#1d120a 50%,#4a2e1b 80%)}
.tp-clip{position:absolute; left:-4px; right:-4px; top:0; bottom:30px; overflow:hidden}
.tp-mole{position:absolute; bottom:0; left:50%; width:var(--tp-w); height:calc(var(--tp-w) * 1.28); margin-left:calc(var(--tp-w) / -2); will-change:transform}
.tp-btn{position:absolute; inset:0; padding:0; border:0; background:none; pointer-events:auto; touch-action:manipulation; cursor:pointer}
.tp-sq{position:absolute; inset:0; transform-origin:50% 100%}
.tp-svg{position:absolute; inset:0; width:100%; height:100%; overflow:visible; transform-origin:50% 100%; animation:tp-breathe 2.6s ease-in-out infinite}
.tp-lip{position:absolute; left:50%; bottom:0; width:calc(var(--tp-w) * 1.1); height:36px; transform:translateX(-50%); border:3px solid var(--ink); border-radius:50%; pointer-events:auto;
  background:radial-gradient(circle at 30% 40%,#9A6437 0 3px,transparent 4px) 0 0/16px 12px, #7B4A2A}
.tp-ear{fill:#7A4A2B; stroke:#1B2D45; stroke-width:3} .tp-body{fill:#8D5B3A; stroke:#1B2D45; stroke-width:3} .tp-belly{fill:#B98159}
.tp-white{fill:#fff; stroke:#1B2D45; stroke-width:2.5} .tp-shine{fill:#fff} .tp-pup{fill:#1B2D45; transform-box:fill-box; animation:tp-look 5s var(--bd,0s) infinite}
.tp-lid{fill:#8D5B3A; transform-box:fill-box; transform-origin:50% 0; transform:scaleY(0); animation:tp-blink var(--bl,4s) var(--bd,0s) infinite}
.tp-xe{stroke:#1B2D45; stroke-width:4; stroke-linecap:round; opacity:0}
.tp-ck{fill:#FF9EB0; opacity:.75; transform-box:fill-box; transform-origin:center; transition:transform .15s}
.tp-wh,.tp-mouth{fill:none; stroke:#1B2D45; stroke-width:1.8; stroke-linecap:round} .tp-mouth{stroke-width:2.6}
.tp-teeth{fill:#fff; stroke:#1B2D45; stroke-width:1.5} .tp-o{fill:#5A1F2B; stroke:#1B2D45; stroke-width:2; opacity:0}
.tp-nose{fill:#FF7A93; stroke:#1B2D45; stroke-width:2.5; transform-box:fill-box; transform-origin:center; animation:tp-sniff 2.4s var(--bd,0s) infinite}
.gold .tp-body,.gold .tp-lid{fill:#F4C542} .gold .tp-ear{fill:#E0A92B} .gold .tp-belly{fill:#FBE38A}
.dizzy .tp-eyes{opacity:0} .dizzy .tp-xe,.dizzy .tp-ring{opacity:1} .puff .tp-ck{transform:scale(1.7)} .o .tp-mouth,.o .tp-teeth{opacity:0} .o .tp-o{opacity:1}
.tp-card{position:absolute; left:17%; right:17%; top:55%; height:37%; z-index:1; display:grid; place-items:center; background:#fff; border:3px solid var(--ink); border-radius:12px; font-size:calc(var(--tp-w) * .4); line-height:1}
.tp-card::before,.tp-card::after{content:""; position:absolute; top:28%; width:30%; aspect-ratio:1; background:#7A4A2B; border:2.5px solid var(--ink); border-radius:50%}
.tp-card::before{left:-20%} .tp-card::after{right:-20%} .gold .tp-card::before,.gold .tp-card::after{background:#E0A92B}
.tp-tongue{position:absolute; left:42%; width:16%; top:51%; height:18%; z-index:2; background:#FF5C7A; border:2.5px solid var(--ink); border-top:0; border-radius:0 0 50% 50% / 0 0 70% 70%; transform-origin:50% 0; transform:scaleY(0); transition:transform .15s}
.tongue .tp-tongue{transform:scaleY(1); animation:tp-lick .2s .15s 5 alternate ease-in-out}
.tp-hat{position:absolute; left:50%; top:-7%; z-index:2; font-size:calc(var(--tp-w) * .3); line-height:1; transform:translateX(-50%) rotate(-10deg)}
.tp-ring{position:absolute; left:50%; top:6%; width:0; height:0; z-index:3; opacity:0}
.tp-ring i{position:absolute; font-style:normal; font-size:16px; animation:tp-orbit .9s linear infinite} .tp-ring i:nth-child(2){animation-delay:-.3s} .tp-ring i:nth-child(3){animation-delay:-.6s}
.tp-glow{position:absolute; inset:-14% -18%; border-radius:50%; background:radial-gradient(circle,rgba(255,236,120,.95) 30%,rgba(255,236,120,0) 70%); opacity:0}
.hint .tp-glow{opacity:.8; animation:tp-glow .8s ease-in-out infinite alternate}
.tp-bw{position:absolute; bottom:calc(var(--tp-w) * 1.28 + 26px); z-index:5; width:max-content; max-width:150px; pointer-events:none}
.tp-bw.l{left:-2px} .tp-bw.r{right:-2px} .tp-bw.m{left:50%; transform:translateX(-50%)}
.tp-bub{position:relative; padding:4px 9px; background:#fff; border:3px solid var(--ink); border-radius:14px; box-shadow:2px 3px 0 var(--ink); font-family:var(--display); font-weight:600; font-size:15px; line-height:1.15; text-align:center; opacity:0; transition:opacity .25s}
.tp-bub::after{content:""; position:absolute; bottom:-8px; left:calc(50% - 6px); width:10px; height:10px; background:#fff; border-right:3px solid var(--ink); border-bottom:3px solid var(--ink); transform:rotate(45deg)}
.l .tp-bub::after{left:26%} .r .tp-bub::after{left:auto; right:26%}
.tp-mallet{position:absolute; right:-6px; top:0; z-index:6; font-size:calc(var(--tp-w) * .52); line-height:1; transform-origin:85% 85%; opacity:0; pointer-events:none}
.tp-dust{position:absolute; left:50%; bottom:40px; width:9px; height:9px; z-index:4; border-radius:50%; background:#7B4A2A; border:1.5px solid var(--ink); pointer-events:none}
.tp-still *{animation:none!important; transition:none!important}
@keyframes tp-blink{0%,90%,100%{transform:scaleY(0)} 94%{transform:scaleY(1)}}
@keyframes tp-look{0%,35%,100%{transform:translate(0,0)} 45%,60%{transform:translate(-2.5px,.5px)} 70%,90%{transform:translate(2.5px,0)}}
@keyframes tp-sniff{0%,84%,100%{transform:scale(1)} 88%{transform:scale(1.2,.85)} 92%{transform:scale(.9,1.12)} 96%{transform:scale(1.08,.94)}}
@keyframes tp-breathe{50%{transform:scale(.99,1.025)}}
@keyframes tp-lick{from{transform:scaleY(1) rotate(-12deg)} to{transform:scaleY(1.12) rotate(12deg)}}
@keyframes tp-glow{from{transform:scale(.9)} to{transform:scale(1.08)}}
@keyframes tp-orbit{0%,100%{transform:translate(-50%,-50%) translate(-28px,0) scale(.9)} 12.5%{transform:translate(-50%,-50%) translate(-20px,4px)} 25%{transform:translate(-50%,-50%) translate(0,6px) scale(1.2)}
  37.5%{transform:translate(-50%,-50%) translate(20px,4px)} 50%{transform:translate(-50%,-50%) translate(28px,0) scale(.9)} 62.5%{transform:translate(-50%,-50%) translate(20px,-4px) scale(.75)}
  75%{transform:translate(-50%,-50%) translate(0,-6px) scale(.6)} 87.5%{transform:translate(-50%,-50%) translate(-20px,-4px) scale(.75)}}
`);

// one hole: the pit, a mole in a clipping box (it sinks into the ground), the dirt lip in front, a speech bubble
function makeHole(col){
  const cell = el("div", "tp-cell");
  cell.innerHTML = `<div class="tp-pit"></div><div class="tp-clip"><div class="tp-mole"><div class="tp-glow"></div><button class="tp-btn" aria-label="mole"><div class="tp-sq">`
    + `<div class="tp-hat"></div>${SVG}<div class="tp-tongue"></div><div class="tp-card"></div><div class="tp-ring"><i>⭐</i><i>💫</i><i>✨</i></div></div></button></div></div>`
    + `<div class="tp-lip"></div><div class="tp-bw ${col}"><div class="tp-bub"></div></div>`;
  const q = s => cell.querySelector(s);
  const h = {cell, mole:q(".tp-mole"), btn:q(".tp-btn"), lip:q(".tp-lip"), sq:q(".tp-sq"), card:q(".tp-card"), hat:q(".tp-hat"), bub:q(".tp-bub"), busy:false, tok:0};
  h.mole.style.setProperty("--bl", (2.6 + Math.random() * 2.4).toFixed(2) + "s");
  h.mole.style.setProperty("--bd", (-Math.random() * 4).toFixed(2) + "s");
  h.mole.style.transform = "translateY(110%)";
  return h;
}
const SQUASH = [{transform:"scale(1,1)"}, {transform:"scale(1.3,.6)", offset:.18}, {transform:"scale(.82,1.22)", offset:.42}, {transform:"scale(1.08,.94)", offset:.64}, {transform:"scale(.97,1.03)", offset:.82}, {transform:"scale(1,1)"}];
const WIGGLE = [{transform:"rotate(0)"}, {transform:"rotate(-11deg)"}, {transform:"rotate(10deg)"}, {transform:"rotate(-7deg)"}, {transform:"rotate(4deg)"}, {transform:"rotate(0)"}];
const HOP = [{transform:"translateY(0)"}, {transform:"translateY(-14%) scale(.95,1.06)", offset:.35}, {transform:"translateY(0) scale(1.08,.92)", offset:.7}, {transform:"translateY(0)"}];

registerGame({id:ID, em:"🔨", name:"Tape-taupes", desc:"Tape la taupe qui tient le mot", multi:true, title:C.title, sub:C.sub}, function () {
  const lvl = levelOf(ID), little = S.kid === "p4", reading = lvl === 4 && !little;
  const cfg = [{holes:3, up:4200, every:1500, max:2, pT:.55}, {holes:5, up:3000, every:1150, max:3, pT:.45},
               {holes:7, up:2400, every:1100, max:4, pT:.4, pair:true}, {holes:7, up:2100, every:1000, max:4, pT:.4, pair:true}][lvl - 1];
  const up0 = cfg.up * (little ? 1.35 : 1), every = cfg.every * (little ? 1.2 : 1), total = [6, 8, 8, 8][lvl - 1];
  // one theme at levels 1 and 2 (enough easy words), all themes mixed from level 3
  const themes = lvl <= 2 ? [any(THEMES_OK.filter(k => lvl > 1 || k !== "jobs"))] : THEMES_OK;
  const seen = new Set(), pool = themes.flatMap(k => wordsOf(k, lvl)).filter(w => !seen.has(w.e) && seen.add(w.e));
  const byEn = {}; THEMES_OK.forEach(k => THEMES[k].words.forEach(w => { byEn[w.en] = byEn[w.en] || w; }));
  const queue = shuffle(pool); let qi = 0;
  const next = () => queue[qi++ % queue.length];
  const rounds = [...Array(total)].map((_, r) => reading && r >= total - 3 ? [next(), next()] : [next()]);
  const askK = rounds.map(() => rnd(60)), res = [];
  let i = 0, tries = 0, misses = 0, found = [], locked = true, sinceT = 9, upMs = up0, gold = false, hint = false, spawner = 0, shown = "";
  startSession(ID, null, total); const gen = GEN;
  const later = (fn, ms) => loops.push(setTimeout(() => { if (alive(gen)) fn(); }, ms));
  const soon = (h, ms, fn) => { const tok = h.tok; later(() => { if (h.tok === tok) fn(); }, ms); }; // cancelled if the mole moved on

  const body = $("gameBody"); body.innerHTML = "";
  const top = el("div", "tp-top"), sign = el("div", "tp-sign"), tools = el("div", "row");
  tools.style.justifyContent = "center"; top.append(sign, tools);
  const garden = el("div", `tp-garden tp-n${cfg.holes}` + (TEST || calm() ? " tp-still" : ""));
  const holes = [];
  LAYOUT[cfg.holes].forEach(n => {
    const row = el("div", "tp-row");
    for (let k = 0; k < n; k++) { const h = makeHole(k === 0 ? "l" : k === n - 1 ? "r" : "m"); holes.push(h); row.append(h.cell); }
    garden.append(row);
  });
  body.append(top, garden);
  holes.forEach(h => {
    // pointerdown answers at once; click stays for keyboards and the recette; the dirt in front counts too
    h.btn.onpointerdown = h.lip.onpointerdown = e => { e.preventDefault(); h.pd = Date.now(); hit(h); };
    h.btn.onclick = () => { if (Date.now() - (h.pd || 0) > 700) hit(h); };
  });

  const target = () => rounds[i];
  const promptText = () => {
    const t = target(), a = tx(C.ask);
    return t.length === 2 ? t.map(w => noun(w, "n")).join(" … ") : fill(a[askK[i] % a.length], t[0]);
  };
  function drawSign(){
    shown = LG();
    const t = target();
    sign.innerHTML = reading
      ? `<small>${tx(t.length === 2 ? C.two : C.read)}</small><b>${t.map(w => `<span class="tp-w${found.includes(w) ? " done" : ""}">${noun(w, "n")}</span>`).join(" ")}</b>`
      : `<small>👂 ${tx(C.listen)}</small><b>${promptText()}</b>`; // what the voice says is written too (Kezhan: a chance to read)
  }
  function drawTools(){
    tools.innerHTML = "";
    if (reading) { const b = el("button", "chip tp-chip", "🔊"); b.onclick = () => { G.hints++; say(promptText(), LG()); }; tools.append(b); }
    else tools.append(speakBtn(promptText, tx(C.again), LG));
    if (little) { const b = el("button", "chip tp-chip", "🐢"); b.onclick = () => { G.hints++; say(promptText(), LG(), .55); }; tools.append(b); }
    else if (lvl === 3) { // help in the language he already knows: Chinese, or English when he learns Chinese
      const hl = LG() === "zh" ? "en" : "zh", b = el("button", "chip tp-chip", hl === "zh" ? "中文 ?" : "English ?");
      b.onclick = () => { G.hints++; b.textContent = target().map(w => w[hl]).join(" · "); say(b.textContent, hl); };
      tools.append(b);
    }
  }
  const flip = n => anim(n, [{transform:"perspective(300px) rotateX(85deg)"}, {transform:"perspective(300px) rotateX(-12deg)", offset:.6}, {transform:"none"}], {duration:380, easing:"ease-out"});
  function bubble(h, text, ms = 1500){
    const b = h.bub, tok = b.tok = (b.tok || 0) + 1;
    b.textContent = text; b.style.opacity = 1;
    anim(b, [{transform:"scale(.3)", opacity:0}, {transform:"scale(1.12)", opacity:1, offset:.6}, {transform:"scale(1)", opacity:1}], {duration:260, easing:"ease-out"});
    later(() => { if (b.tok === tok) b.style.opacity = 0; }, ms);
  }
  function dust(h){
    if (calm()) return;
    for (let k = 0; k < 6; k++) {
      const d = el("i", "tp-dust"), x = (k - 2.5) * 13 + rnd(8), y = 12 + rnd(24); h.cell.append(d);
      const a = anim(d, [{transform:"translate(-50%,0) scale(1)", opacity:1}, {transform:`translate(calc(-50% + ${x}px), ${-y}px) scale(.35)`, opacity:0}], {duration:420 + rnd(220), easing:"cubic-bezier(.2,.8,.3,1)"});
      if (a) a.onfinish = () => d.remove(); else d.remove();
    }
  }
  function mallet(h){
    if (calm()) return;
    const m = el("div", "tp-mallet", "🔨"); h.cell.append(m);
    const a = anim(m, [{transform:"rotate(45deg)", opacity:0}, {transform:"rotate(50deg)", opacity:1, offset:.2}, {transform:"rotate(-42deg)", opacity:1, offset:.45},
      {transform:"rotate(-30deg)", opacity:1, offset:.7}, {transform:"rotate(-30deg)", opacity:0}], {duration:430, easing:"ease-in"});
    if (a) a.onfinish = () => m.remove(); else m.remove();
  }

  function pop(h, c){
    h.tok++; h.busy = true; h.out = h.hit = false; h.word = c.w || null; h.gold = !!c.gold;
    const t = target() || [], isT = !!c.w && t.includes(c.w); // no round any more at the final party
    h.mole.className = "tp-mole" + (h.gold ? " gold" : "") + (hint && isT ? " hint" : "");
    h.card.innerHTML = h.gold ? "💎" : c.w ? wordFace(c.w) : c.party ? "🎉" : "👋";
    h.btn.setAttribute("aria-label", c.w ? noun(c.w, "n") : "mole");
    h.hat.innerHTML = h.gold ? "👑" : Math.random() < .4 && !t.some(w => ["hat", "cap"].includes(w.en)) ? any(HATS) : "";
    delete h.btn.dataset.ok;
    if (isT) markOk(h.btn);
    h.mole.style.transform = "translateY(0)";
    if (TEST) return;
    anim(h.mole, [{transform:"translateY(110%)"}, {transform:"translateY(-9%)", offset:.55}, {transform:"translateY(3%)", offset:.78}, {transform:"translateY(0)"}], {duration:430, easing:"ease-out"});
    dust(h); snd.pop();
    if (c.peek || c.party) return;
    if (Math.random() < .15) soon(h, 480, () => quirk(h));
    soon(h, upMs * (h.gold ? .75 : 1), () => leave(h));
  }
  function duck(h){
    if (!h.busy || h.out) return;
    h.out = true; const tok = ++h.tok;
    delete h.btn.dataset.ok;
    h.mole.style.transform = "translateY(110%)";
    const free = () => { if (h.tok !== tok) return; h.busy = h.out = h.gold = false; h.word = null; h.mole.className = "tp-mole"; };
    if (TEST || calm()) return free();
    anim(h.mole, [{transform:"translateY(0)"}, {transform:"translateY(-7%)", offset:.3}, {transform:"translateY(110%)"}], {duration:280, easing:"ease-in"});
    later(free, 300);
  }
  // time is up: the wanted mole laughs and goes, and comes back slower
  function leave(h){
    if (!locked && h.word && target().includes(h.word) && !found.includes(h.word)) {
      misses++; upMs = Math.min(upMs * 1.15, up0 * 2); sinceT = 9;
      if (misses >= (little ? 2 : 3)) hint = true;
      snd.giggle(); bubble(h, any(tx(C.slow))); h.mole.classList.add("tongue");
      anim(h.sq, [0, 1, 2, 3, 4, 5, 6].map(k => ({transform:`translateY(${k % 2 ? -5 : 0}px) rotate(${k % 2 ? 4 : -4}deg)`})), {duration:560});
      soon(h, 750, () => duck(h));
    } else duck(h);
  }
  // a mole that just came out may say hello, sneeze, burp or yawn
  function quirk(h){
    const k = any(["hi", "sneeze", "burp", "yawn"]);
    bubble(h, any(tx(C.quirk[k]))); snd[k]();
    if (k === "hi") anim(h.sq, HOP, {duration:500, easing:"ease-out"});
    if (k === "sneeze") {
      h.mole.classList.add("o"); later(() => dust(h), 380);
      anim(h.sq, [{transform:"none"}, {transform:"scale(.94,1.08) rotate(-5deg)", offset:.45}, {transform:"scale(1.15,.84) translateY(5px) rotate(4deg)", offset:.6}, {transform:"none"}], {duration:750});
    }
    if (k === "burp") { h.mole.classList.add("puff", "o"); anim(h.sq, [{transform:"none"}, {transform:"scale(1.1,.92)", offset:.3}, {transform:"scale(.96,1.05)", offset:.6}, {transform:"none"}], {duration:600}); }
    if (k === "yawn") { h.mole.classList.add("o"); anim(h.sq, [{transform:"none"}, {transform:"scale(.95,1.1)", offset:.5}, {transform:"none"}], {duration:1000, easing:"ease-in-out"}); }
    soon(h, 1000, () => h.mole.classList.remove("o", "puff"));
  }

  function hit(h){
    if (locked) return;
    if (!h.busy || h.out || h.hit) { mallet(h); dust(h); snd.thud(); return; } // bonk on an empty hole: only dirt
    G.taps++; h.hit = true; h.tok++;
    mallet(h); snd.bonk();
    if (h.gold) { // the golden mole: a bonus star
      snd.coin(); addStar(); bubble(h, tx(C.bonus));
      anim(h.sq, [{transform:"rotate(0) scale(1)"}, {transform:"rotate(360deg) scale(1.25)"}, {transform:"rotate(360deg) scale(1)"}], {duration:700, easing:"ease-out"});
      return soon(h, 900, () => duck(h));
    }
    if (target().includes(h.word) && !found.includes(h.word)) good(h); else wrong(h);
  }
  function good(h){
    const w = h.word; found.push(w); delete h.btn.dataset.ok;
    snd.boing(); h.mole.classList.remove("hint", "tongue"); h.mole.classList.add("dizzy");
    anim(h.sq, SQUASH, {duration:650, easing:"ease-out"});
    bubble(h, any(tx(C.ouch)));
    const bye = o => TEST ? duck(o) : soon(o, 1100, () => duck(o));
    if (found.length < target().length) { drawSign(); sayT(any(tx(C.praise))); return bye(h); } // two words: one more to go
    locked = true; clearInterval(spawner);
    const first = tries === 0; if (first) addStar(); else fx.sparkle();
    logRound(target().map(x => x.en).join("+"), first, tries + 1, {lvl, misses});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    const pr = any(tx(C.praise)), line = fill(any(tx(C.got)), w);
    sign.innerHTML = `<b>⭐ ${pr}</b><small>${line}</small>`; flip(sign);
    holes.forEach(o => { if (o !== h && o.busy) { o.tok++; if (TEST) duck(o); else later(() => duck(o), 200 + rnd(300)); } });
    bye(h);
    const r = i;
    say(pr + (LG() === "zh" ? "" : " ") + line, LG()).then(() => { if (alive(gen) && r === i) later(() => { i++; ask(); }, TEST ? 150 : 600); });
  }
  function wrong(h){
    tries++; fx.wrong(); snd.raspberry();
    const line = fill(any(tx(C.nope)), h.word);
    h.mole.classList.add("tongue", "puff");
    anim(h.sq, WIGGLE, {duration:600, easing:"ease-in-out"});
    bubble(h, line, 1900); sayT(line);
    if (tries >= (little ? 2 : 3)) { hint = true; sinceT = 9; holes.forEach(o => { if (o.busy && !o.hit && target().includes(o.word)) o.mole.classList.add("hint"); }); }
    soon(h, 1300, () => { h.mole.classList.remove("tongue", "puff"); duck(h); });
  }

  function tick(){
    if (!alive(gen) || locked) return;
    if (shown !== LG()) { drawSign(); drawTools(); } // a flag was touched in the game bar
    for (let n = cfg.pair ? 2 : 1; n > 0; n--) {
      const free = holes.filter(h => !h.busy);
      if (!free.length || holes.length - free.length >= cfg.max) return;
      pop(any(free), choose());
    }
  }
  function choose(){
    const t = target(), upW = holes.filter(h => h.busy).map(h => h.word);
    const want = t.filter(w => !found.includes(w) && !upW.includes(w));
    if (want.length && (sinceT >= (lvl === 1 ? 1 : 2) || Math.random() < cfg.pT)) { sinceT = 0; return {w:any(want)}; }
    sinceT++;
    if (!gold && i >= 1 && Math.random() < .07) { gold = true; return {gold:true}; }
    return {w:other(upW)};
  }
  // a wrong picture; from level 3 often one that sounds or looks like the wanted word
  function other(upW = []){
    const t = target(), ok = w => w && !t.includes(w) && !upW.includes(w);
    const twins = lvl >= 3 ? t.flatMap(w => (C.twins[LG()] || []).filter(g => g.includes(w.en)).flat()).map(k => byEn[k]).filter(ok) : [];
    if (twins.length && Math.random() < .5) return any(twins);
    const o = pool.filter(ok);
    return o.length ? any(o) : any(pool.filter(w => !t.includes(w)));
  }

  function ask(){
    if (!alive(gen)) return;
    if (i >= total) return finale();
    tries = 0; misses = 0; found = []; sinceT = 9; upMs = up0; hint = false; locked = false;
    renderDots(res, total, i);
    holes.forEach(h => { if (h.busy) duck(h); });
    drawSign(); drawTools(); flip(sign);
    if (TEST) { // the recette: wanted moles up at once and still, plus one wrong one
      const free = shuffle(holes);
      target().forEach(w => pop(free.pop(), {w}));
      return pop(free.pop(), {w:other()});
    }
    if (!reading) sayT(promptText());
    const r = i;
    clearInterval(spawner);
    later(() => { if (r !== i || locked) return; tick(); spawner = setInterval(tick, every); loops.push(spawner); }, reading ? 500 : 1100);
  }
  // hello at the start, a party at the end
  function intro(){
    if (TEST || calm()) return ask();
    sign.innerHTML = `<b>🔨 ${tx(C.ready)}</b>`;
    holes.forEach((h, k) => later(() => pop(h, {peek:true}), 100 + k * 90));
    const h = any(holes);
    later(() => { snd.hi(); bubble(h, any(tx(C.quirk.hi)), 1200); anim(h.sq, HOP, {duration:500}); }, 700);
    later(() => holes.forEach(o => duck(o)), 1700);
    later(ask, 2150);
  }
  function finale(){
    if (TEST) return finish();
    locked = true; clearInterval(spawner); tools.innerHTML = "";
    sign.innerHTML = `<b>🎉 ${any(tx(C.cheer))}</b>`; flip(sign);
    holes.forEach((h, k) => later(() => { if (!h.busy) pop(h, {party:true}); later(() => anim(h.sq, HOP, {duration:450}), 450); }, 350 + k * 80));
    later(() => { const h = any(holes); snd.hi(); bubble(h, any(tx(C.cheer)), 1200); }, 900);
    later(finish, 2000);
  }
  intro();
});
})();
