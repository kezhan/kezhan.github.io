/* L'Île aux Mots : jeu « Les tambours à rythmes » (catégorie Bouger et jouer).
   Drums of colour, each with its own Web Audio sound. The game plays a sequence and names every colour in the language
   being learnt (words of the "colors" theme of the lexicon); the child plays it back; the sequence grows after each success.
   1: 3 drums that light up, 2 to 3 beats · 2: 3 drums, the sequence grows up to 5
   3: 4 drums, the colours are only said (no light, one neutral tick for every beat), up to 6
   4: the sequence is written (red, blue, red…), no voice: read it, the words hide at the first tap, up to 6 (the little one keeps the voice)
   English, German, Luxembourgish or Chinese, never French.
   Luxembourgish checked on lod.lu: "d'Tromm, d'Trommen", "spill no!" (nospillen), "lauschter!" (transitive: "lauschter d'Trommen").
   To have proofread: "Lauschter d'Trommen a spill datselwecht no!", "D'Wierder verstoppe sech, soubal s de ufänks!",
   "Folleg dem Liicht!", the title "Rhythmus-Trommen". */
(() => {
const ID = "rythmes";
const RY = {
  en:{listen:"Listen!", turn:"Your turn!", read:"Read!", again:"Again",
    intro:{drums:"Listen to the drums, then play the same!", colours:"Listen to the colours, then play them!", read:"Read the colours, then play them!"},
    hide:"The words hide when you start!", oops:"Oops! Listen again.", oopsRead:"Oops! Read again.", follow:"Follow the light!"},
  de:{listen:"Hör zu!", turn:"Du bist dran!", read:"Lies!", again:"Nochmal",
    intro:{drums:"Hör den Trommeln zu und spiel dasselbe nach!", colours:"Hör dir die Farben an und spiel sie nach!", read:"Lies die Farben und spiel sie nach!"},
    hide:"Die Wörter verstecken sich, sobald du anfängst!", oops:"Hoppla! Hör noch mal zu.", oopsRead:"Hoppla! Lies noch mal.", follow:"Folge dem Licht!"},
  lb:{listen:"Lauschter!", turn:"Elo bass du drun!", read:"Lies!", again:"Nach eng Kéier",
    intro:{drums:"Lauschter d'Trommen a spill datselwecht no!", colours:"Lauschter d'Faarwen a spill se no!", read:"Lies d'Faarwen a spill se no!"},
    hide:"D'Wierder verstoppe sech, soubal s de ufänks!", oops:"Hoppla! Lauschter nach eng Kéier.", oopsRead:"Hoppla! Lies nach eng Kéier.", follow:"Folleg dem Liicht!"},
  zh:{listen:"听！", turn:"轮到你了！", read:"读一读！", again:"再听一次",
    intro:{drums:"听小鼓，然后照着敲！", colours:"听颜色，然后照着敲！", read:"读颜色，然后照着敲！"},
    hide:"你一开始敲，字就会藏起来！", oops:"哎呀！再听一次。", oopsRead:"哎呀！再读一次。", follow:"跟着亮光敲！"}
};
// colours a drum can wear (never white on the white page), more of them at higher levels
const COLOR_SETS = [
  ["red","blue","yellow","green"],
  ["red","blue","yellow","green","orange","pink"],
  ["red","blue","yellow","green","orange","pink","purple"],
  ["red","blue","yellow","green","orange","pink","purple","brown","grey"]
];
const CFG = [{n:3, rounds:6, start:2, max:3}, {n:3, rounds:7, start:2, max:5}, {n:4, rounds:7, start:3, max:6}, {n:4, rounds:7, start:3, max:6}];

/* drum sounds, made with the Web Audio API (silent in test mode), on the shared audio context that son.js wakes at the first tap */
const snd = (() => {
  let noise = null;
  const ctx = () => { if (TEST) return null; try { ac = ac || new (window.AudioContext || window.webkitAudioContext)(); if (ac.state === "suspended") ac.resume(); return ac; } catch(e) { return null; } };
  function note(type, f0, f1, dur, vol, at = 0, lfo){
    const a = ctx(); if (!a) return;
    const t = a.currentTime + at, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .008); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination);
    if (lfo) { const l = a.createOscillator(), lg = a.createGain(); l.frequency.value = lfo[0]; lg.gain.value = lfo[1]; l.connect(lg); lg.connect(o.frequency); l.start(t); l.stop(t + dur); }
    o.start(t); o.stop(t + dur + .05);
  }
  function hiss(dur, vol, at, f0, f1, type = "bandpass"){
    const a = ctx(); if (!a) return;
    if (!noise) { noise = a.createBuffer(1, a.sampleRate, a.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    const t = a.currentTime + at, s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = noise; f.type = type; f.frequency.setValueAtTime(f0, t); f.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(t); s.stop(t + dur);
  }
  // four drums that do not sound alike: a deep tom, a high bongo, a crackling snare, a round conga
  const KIT = [
    () => { note("sine", 170, 52, .5, .6); hiss(.06, .4, 0, 900, 250, "lowpass"); },
    () => { note("triangle", 480, 340, .16, .38); hiss(.025, .25, 0, 4200, 2600); },
    () => { hiss(.22, .55, 0, 2600, 1400); note("triangle", 240, 170, .1, .25); },
    () => { note("sine", 330, 240, .3, .45); note("sine", 660, 470, .1, .1); hiss(.03, .18, 0, 2200, 900); }
  ];
  return {
    unlock: () => { ctx(); },
    drum: k => KIT[k % KIT.length](),
    tick: () => { note("square", 1500, 1380, .04, .05); hiss(.03, .18, 0, 3600, 3000); },
    crash: at => { hiss(1.2, .3, at, 8000, 5000, "highpass"); note("sine", 170, 52, .5, .5, at); },
    boing: () => note("sine", 170, 540, .5, .22, .02, [15, 80])
  };
})();

// a drum with a face: coloured shell, cream skin on top, a smile that opens when it sings
const lum = hex => { const n = parseInt(hex.slice(1), 16); return (.299 * (n >> 16) + .587 * (n >> 8 & 255) + .114 * (n & 255)) / 255; };
function drumSVG(col){
  const line = lum(col) < .45 ? "#FFF7E0" : "#1B2D45";
  return `<svg class="ry-svg" viewBox="0 0 100 100" aria-hidden="true"><ellipse cx="50" cy="90" rx="36" ry="6" fill="#1B2D45" opacity=".15"/>
<path d="M13 32 V76 A37 11 0 0 0 87 76 V32 Z" fill="${col}" stroke="#1B2D45" stroke-width="3"/>
<path d="M13 70 A37 11 0 0 0 87 70" fill="none" stroke="${line}" stroke-width="3" opacity=".55"/>
<circle cx="37" cy="51" r="7" fill="#fff" stroke="#1B2D45" stroke-width="2"/><circle cx="63" cy="51" r="7" fill="#fff" stroke="#1B2D45" stroke-width="2"/>
<g class="ry-pup"><circle cx="38" cy="52" r="3.4" fill="#1B2D45"/><circle cx="64" cy="52" r="3.4" fill="#1B2D45"/></g>
<path class="ry-smile" d="M42 62 Q50 69 58 62" fill="none" stroke="${line}" stroke-width="2.8" stroke-linecap="round"/>
<ellipse class="ry-o" cx="50" cy="64" rx="5.5" ry="6.5" fill="#5A1F2B" stroke="${line}" stroke-width="2"/>
<ellipse class="ry-skin" cx="50" cy="32" rx="37" ry="11" fill="#FFF7E0" stroke="#1B2D45" stroke-width="3"/>
<ellipse cx="40" cy="29.5" rx="12" ry="3" fill="#fff" opacity=".8"/></svg>`;
}

addStyle(`
.ry-strip{display:flex; flex-wrap:wrap; justify-content:center; align-items:center; gap:6px; min-height:60px; padding:8px; background:#FFF3DA; border:3px dashed var(--ink); border-radius:18px}
.ry-slot{min-width:44px; height:44px; padding:0 9px; display:flex; align-items:center; justify-content:center; gap:5px; background:#fff; border:3px solid var(--ink); border-radius:14px;
  font-family:var(--display); font-weight:600; font-size:clamp(16px,4.4vw,20px); line-height:1; color:var(--ink); white-space:nowrap}
.ry-slot:empty::before{content:"♪"; font-size:22px; color:var(--ink-soft); opacity:.6} /* one slot = one beat still to play */
.ry-slot.on{background:#FFFBEA} .ry-slot.miss{background:#FFD6CF}
.ry-dot{width:20px; height:20px; flex:none; border-radius:50%; border:2px solid var(--ink)}
.ry-drums{display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:10px; width:100%; max-width:560px; margin:0 auto}
.ry-drums.n4{grid-template-columns:repeat(4,minmax(0,1fr))}
@media (max-width:600px){ .ry-drums.n4{grid-template-columns:repeat(2,minmax(0,1fr)); max-width:300px} }
.ry-drum{position:relative; display:flex; flex-direction:column; align-items:center; gap:2px; padding:4px 2px 6px; background:none; border:0; cursor:pointer; font:inherit; color:inherit;
  touch-action:manipulation; -webkit-tap-highlight-color:transparent; user-select:none; -webkit-user-select:none}
.ry-svg{position:relative; z-index:1; display:block; width:100%; max-width:150px; aspect-ratio:1; overflow:visible; transform-origin:50% 90%}
.ry-halo{position:absolute; left:50%; top:40%; width:112%; max-width:170px; aspect-ratio:1; transform:translate(-50%,-50%); border-radius:50%; pointer-events:none;
  background:radial-gradient(circle,rgba(255,226,90,.95) 28%,rgba(255,226,90,0) 68%); opacity:0; transition:opacity .12s}
.ry-lit .ry-halo,.ry-help .ry-halo{opacity:1} .ry-help .ry-halo{animation:ry-pulse .75s ease-in-out infinite alternate}
.ry-lit .ry-skin,.ry-help .ry-skin{fill:#FFE45C}
.ry-o{opacity:0} .ry-sing .ry-o,.ry-dizzy .ry-o{opacity:1} .ry-sing .ry-smile,.ry-dizzy .ry-smile{opacity:0}
.ry-pup{transition:transform .15s} .ry-dizzy .ry-pup{transform:translate(0,-3px)}
.ry-lab{position:relative; z-index:1; max-width:100%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; padding:1px 10px; background:#fff; border:2px solid var(--ink); border-radius:999px;
  font-family:var(--display); font-weight:600; font-size:clamp(15px,4vw,20px)}
.ry-note{position:fixed; z-index:60; font-size:30px; pointer-events:none; transform:translate(-50%,-50%)}
.ry-still *{animation:none!important; transition:none!important}
@keyframes ry-pulse{from{transform:translate(-50%,-50%) scale(.86)} to{transform:translate(-50%,-50%) scale(1.08)}}
`);

registerGame({id:ID, em:"🥁", name:"Les tambours à rythmes", multi:true, cat:"musique",
  title:{en:"Rhythm drums", de:"Rhythmus-Trommeln", lb:"Rhythmus-Trommen", zh:"节奏小鼓"},
  sub:{en:"Listen and play it back!", de:"Hör zu und spiel nach!", lb:"Lauschter a spill no!", zh:"听一听，敲一敲！"}}, function () {
  const lang = ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en", X = RY[lang];
  const lvl = levelOf(ID), little = S.kid === "p4";
  const reading = lvl === 4 && !little, lit = lvl <= 2, labels = lvl <= 2; // the little one never has to read
  const kind = reading ? "read" : lit ? "drums" : "colours";
  const cons = L => RY[L].intro[kind] + (reading ? " " + RY[L].hide : "");
  const consigne = cons(lang), consAll = {en:cons("en"), de:cons("de"), lb:cons("lb"), zh:cons("zh")};
  const cfg = CFG[lvl - 1], total = cfg.rounds;
  const byEn = en => THEMES.colors.words.find(w => w.en === en);
  const drums = pick(COLOR_SETS[lvl - 1], cfg.n).map(byEn).filter(Boolean).map((w, k) => ({w, k}));
  // a new beat never makes the same drum three times in a row
  const beat = s => { let k; do k = rnd(drums.length); while (s.length >= 2 && s[s.length - 1] === k && s[s.length - 2] === k); return k; };
  const fresh = n => { const s = []; while (s.length < n) s.push(beat(s)); return s; };
  let seq = fresh(cfg.start), i = 0, pos = 0, tries = 0, phase = "wait", help = false, hidden = false;
  const res = [];
  snd.unlock(); // still inside the tap that opened the game: the sound may start
  startSession(ID, null, total); const gen = GEN;
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? Math.min(ms, 15) : ms)));
  const calm = () => fx.calm();

  const body = $("gameBody"); body.innerHTML = "";
  const prompt = el("p", "prompt"), tools = el("div", "row"), strip = el("div", "ry-strip");
  tools.style.justifyContent = "center";
  const grid = el("div", `ry-drums n${drums.length}` + (TEST || calm() ? " ry-still" : ""));
  drums.forEach(d => {
    const b = el("button", "ry-drum", `<span class="ry-halo"></span>${drumSVG(d.w.e)}${labels ? `<span class="ry-lab"></span>` : ""}`);
    b.setAttribute("aria-label", d.w[lang]);
    if (labels) b.querySelector(".ry-lab").textContent = d.w[lang];
    // pointerdown answers at once (a drum is hit, not clicked); click stays for keyboards and the recette
    b.onpointerdown = e => { e.preventDefault(); b.pd = Date.now(); tap(d); };
    b.onclick = () => { if (Date.now() - (b.pd || 0) > 700) tap(d); };
    d.b = b; d.svg = b.querySelector(".ry-svg"); grid.append(b);
  });
  body.append(prompt, tools, strip, grid);

  /* ---------- little animations (none in test mode or when the system asks for calm) ---------- */
  const hop = d => { if (!calm()) d.svg.animate([{transform:"scale(1)"}, {transform:"scale(1.18,.8)", offset:.25}, {transform:"scale(.92,1.1) translateY(-6%)", offset:.6}, {transform:"scale(1)"}], {duration:320, easing:"ease-out"}); };
  const flyNote = (d, txt) => {
    if (calm()) return;
    const r = d.b.getBoundingClientRect(), n = el("div", "ry-note"); n.textContent = txt || ["🎵","🎶","♪"][rnd(3)];
    n.style.left = r.left + r.width / 2 + "px"; n.style.top = r.top + r.height * .3 + "px"; document.body.append(n);
    n.animate([{transform:"translate(-50%,-50%) scale(.6)", opacity:1}, {transform:`translate(calc(-50% + ${rnd(60) - 30}px), calc(-50% - 80px)) scale(1.25) rotate(${rnd(40) - 20}deg)`, opacity:0}],
      {duration:850, easing:"ease-out"}).onfinish = () => n.remove();
  };
  const face = (d, cls, ms) => { d.b.classList.add(cls); loops.push(setTimeout(() => d.b.classList.remove(cls), ms)); };
  const hit = d => { snd.drum(d.k); hop(d); face(d, "ry-sing", 300); flyNote(d); };

  /* ---------- screen parts ---------- */
  const setPrompt = main => { prompt.textContent = main; const s = el("small"); s.textContent = consigne; prompt.append(s); };
  const drawStrip = words => {
    strip.innerHTML = "";
    seq.forEach(k => { const s = el("span", "ry-slot"); if (words) s.textContent = drums[k].w[lang]; strip.append(s); });
  };
  const fillSlot = (j, d, dot) => {
    const s = strip.children[j]; if (!s) return;
    s.className = "ry-slot on"; s.innerHTML = dot ? `<i class="ry-dot" style="background:${d.w.e}"></i>` : "";
    s.append(document.createTextNode(d.w[lang]));
    if (!calm()) s.animate([{transform:"scale(.5)"}, {transform:"scale(1.15)"}, {transform:"scale(1)"}], {duration:260, easing:"ease-out"});
  };
  // the drum to hit next: glowing after two misses, and marked for the recette
  const mark = () => {
    const want = phase === "play" ? drums[seq[pos]] : null;
    drums.forEach(d => {
      d.b.classList.toggle("ry-help", help && d === want);
      if (d === want) markOk(d.b); else delete d.b.dataset.ok;
    });
  };
  const drawTools = () => {
    tools.innerHTML = "";
    // levels 1 to 3: the drums play the sequence again; level 4: the words come back and are read aloud (a hint)
    const again = el("button", "speak chunky", `🔊 <span></span>`); again.querySelector("span").textContent = X.again;
    again.onclick = () => { if (phase !== "play" || !alive(gen)) return; if (reading) G.hints++; else G.replays++; play(null, true); };
    tools.append(again, bridgeBtn(consAll));
  };

  /* ---------- one round: the drums play, then the child ---------- */
  const flash = async (d, j) => {
    if (lit) { d.b.classList.add("ry-lit"); hit(d); } else snd.tick(); // level 3: the same tick for every drum, only the word tells
    fillSlot(j, d, lit);
    await Promise.all([say(d.w[lang], lang), wait(lit ? 520 : 600)]);
    d.b.classList.remove("ry-lit");
    await wait(170);
  };
  const demo = async aloud => {
    pos = 0; hidden = false;
    if (reading) {
      drawStrip(true);
      if (!aloud) return true;
      for (let j = 0; j < seq.length; j++) { // the 🔊 hint reads the words one by one
        const s = strip.children[j]; s.classList.add("on");
        await say(drums[seq[j]].w[lang], lang); if (!alive(gen)) return false;
        await wait(250); if (!alive(gen)) return false;
      }
      drawStrip(true);
      return true;
    }
    drawStrip(false);
    for (let j = 0; j < seq.length; j++) { await flash(drums[seq[j]], j); if (!alive(gen)) return false; }
    await wait(400); if (!alive(gen)) return false;
    drawStrip(false); // now from memory
    return true;
  };
  const play = async (line, aloud) => {
    phase = "demo"; mark();
    setPrompt(reading ? "👀 " + X.read : "👂 " + X.listen);
    if (line) { await say(line, lang); if (!alive(gen)) return; await wait(250); if (!alive(gen)) return; }
    if (!(await demo(aloud))) return;
    phase = "play"; pos = 0; mark();
    setPrompt(help ? "✨ " + X.follow : reading ? "👀 " + X.read : "👉 " + X.turn);
    if (!reading || help) say(help ? X.follow : X.turn, lang);
  };
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    tries = 0; help = false;
    drawTools();
    play(i === 0 ? consigne : reading ? X.read : X.listen, false);
  };

  function tap(d){
    if (!alive(gen)) return;
    G.taps++;
    if (phase !== "play") return; // while the drums play, they only listen
    if (d !== drums[seq[pos]]) return miss(d);
    hit(d); say(d.w[lang], lang);
    // level 4: the words hide as soon as the child starts (read first, then play)
    if (reading && !hidden && !help) { hidden = true; [...strip.children].forEach(s => { s.className = "ry-slot"; s.textContent = ""; }); }
    fillSlot(pos, d, true);
    pos++;
    if (pos >= seq.length) return win();
    mark();
  }
  async function miss(d){
    tries++; phase = "wait"; mark();
    snd.boing(); fx.wrong(); face(d, "ry-dizzy", 800); flyNote(d, "💫");
    const s = strip.children[pos]; if (s) { s.className = "ry-slot miss"; s.textContent = "❓"; }
    if (tries >= 2) help = true; // from now on the right drum glows
    const line = help ? X.follow : reading ? X.oopsRead : X.oops;
    setPrompt("💫 " + line);
    await say(line, lang); if (!alive(gen)) return;
    await wait(500); if (!alive(gen)) return;
    play(null, false);
  }
  async function win(){
    phase = "done"; mark();
    const first = tries === 0;
    if (first) addStar();
    logRound(seq.map(k => drums[k].w.en).join(" "), first, tries + 1, {lvl, len:seq.length, lang});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    const p = (PHRASES[lang] || PHRASES.en).praise, praise = p[rnd(p.length)];
    setPrompt("🎉 " + praise);
    // the drums celebrate: a little solo in a wave, then the cymbal
    if (!TEST) drums.forEach((dd, k) => loops.push(setTimeout(() => { if (alive(gen)) hit(dd); }, 130 * k)));
    snd.crash(.13 * drums.length);
    if (!calm()) { const r = strip.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 14); }
    await wait(150 * drums.length); if (!alive(gen)) return;
    await say(praise, lang); if (!alive(gen)) return;
    // the sequence grows after a success on the first try (up to the level's maximum); after a miss, a new one as long
    seq = !first ? fresh(seq.length) : seq.length < cfg.max ? [...seq, beat(seq)] : fresh(cfg.max);
    i++;
    loops.push(setTimeout(round, TEST ? 20 : 700));
  }
  round();
});
})();
