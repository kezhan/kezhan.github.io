/* L'Île aux Mots : jeu « Devinettes » (ticket 2026-09-27_jeu-devinettes).
   A funny monster whispers clues in the first person ("I am yellow. I am long."), the child touches the answer.
   1: two simple clues with pictograms, 4 pictures from every theme · 2: three clues, 4 pictures of the same theme
   3: two rich clues, 6 pictures · 4: the clues are written, the voice only on request (a hint).
   English, German, Luxembourgish or Chinese, never French. Riddles: js/contenus/devinettes.js. */
(() => {
const ID = "devinettes", LANGS4 = ["en", "de", "lb", "zh"], POOL = ["animals", "food", "body", "things", "clothes", "jobs"];
const lng = () => LANGS4.includes(langOf()) ? langOf() : "en";
const ui = (k, w) => DEVINETTES_UI[k][lng()].replace("{w}", w || "");
const word = w => w[lng()] || w.en;
let LEX = null; // English word -> {w, t}: the lexicon entry and its theme
const lex = () => { if (!LEX) { LEX = {}; POOL.forEach(t => THEMES[t].words.forEach(w => { if (!LEX[w.en]) LEX[w.en] = {w, t}; })); } return LEX; };
const seen = new Set(); // riddles met since the page opened: the next game brings new ones

/* a wrong picture must never fit the clues: not in the "no" list, not drawn as a pictogram, not named in any language */
const noun = s => s.replace(/^(d'|de |den |der |die |das )/, "");
const names = (text, w, l) => {
  if (!w) return false;
  if (l === "zh") return text.includes(w);
  const n = (l === "en" ? w : noun(w)).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  return new RegExp("(?<!\\p{L})" + n + (l === "en" ? "(?:s|es)?" : "\\p{L}{0,3}") + "(?!\\p{L})", l === "en" ? "iu" : "u").test(text);
};
function options(d, lvl){
  const a = lex()[d.w], no = (d.no || "").split(","), n = lvl >= 3 ? 5 : 3;
  const ban = w => w.en === d.w || w.e === a.w.e || no.includes(w.en) || d.i.includes(w.e) || LANGS4.some(l => names(d[l], w[l], l));
  const ok = themes => [...new Map(themes.flatMap(t => wordsOf(t, lvl)).filter(w => !ban(w)).map(w => [w.en, w])).values()];
  const same = ok([a.t]), other = ok(POOL.filter(t => t !== a.t));
  let got = lvl === 1 ? pick(ok(POOL), n) : pick(same, lvl === 2 ? n : lvl === 3 ? 3 : 4);
  got = got.concat(pick(other.filter(w => !got.includes(w)), n - got.length));
  return shuffle([a.w, ...got]);
}

/* funny sounds, made on the fly */
const snd = (() => {
  const c = () => { try { ac = ac || new (window.AudioContext || window.webkitAudioContext)(); if (ac.state === "suspended") ac.resume(); return ac; } catch(e) { return null; } };
  const env = (x, g, t, vol, dur) => { g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur); g.connect(x.destination); };
  function glide(type, f0, f1, t0, dur, vol){
    const x = c(); if (!x) return; const t = x.currentTime + t0, o = x.createOscillator(), g = x.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    env(x, g, t, vol, dur); o.connect(g); o.start(t); o.stop(t + dur + .02);
  }
  // an oscillator wobbled by another one: raspberry, burp, boing
  function wob(type, f, dur, rate, depth, vol, cut){
    const x = c(); if (!x) return; const t = x.currentTime, o = x.createOscillator(), l = x.createOscillator(), lg = x.createGain(), fl = x.createBiquadFilter(), g = x.createGain();
    o.type = type; o.frequency.setValueAtTime(f, t); o.frequency.linearRampToValueAtTime(f * .8, t + dur);
    l.frequency.value = rate; lg.gain.setValueAtTime(depth, t); lg.gain.exponentialRampToValueAtTime(1, t + dur); l.connect(lg); lg.connect(o.frequency);
    fl.type = "lowpass"; fl.frequency.value = cut; o.connect(fl); fl.connect(g); env(x, g, t, vol, dur);
    o.start(t); l.start(t); o.stop(t + dur + .02); l.stop(t + dur + .02);
  }
  function hiss(t0, dur, freq, vol){
    const x = c(); if (!x) return; const n = Math.floor(x.sampleRate * dur), b = x.createBuffer(1, n, x.sampleRate), v = b.getChannelData(0);
    for (let k = 0; k < n; k++) v[k] = (Math.random() * 2 - 1) * (1 - k / n);
    const s = x.createBufferSource(), fl = x.createBiquadFilter(), g = x.createGain();
    s.buffer = b; fl.type = "bandpass"; fl.frequency.value = freq; g.gain.value = vol; s.connect(fl); fl.connect(g); g.connect(x.destination); s.start(x.currentTime + t0);
  }
  const quiet = f => (...a) => { if (!TEST) f(...a); };
  return {
    boing: quiet(() => wob("sine", 260, .55, 14, 140, .25, 3000)),
    pfff: quiet(() => { wob("sawtooth", 95, .45, 30, 40, .2, 900); hiss(0, .35, 500, .3); }),
    burp: quiet(() => wob("sawtooth", 78, .6, 10, 20, .3, 480)),
    sneeze: quiet(() => { glide("sine", 420, 700, 0, .28, .1); glide("sine", 480, 900, .32, .3, .1); hiss(.68, .35, 2600, .8); }),
    giggle: quiet(() => tone([900, 1150, 950, 1200, 1000, 1260], .07, "sine")),
    blip: quiet(() => glide("triangle", 600, 1200, 0, .08, .07)),
    slide: quiet(() => glide("triangle", 300, 1400, 0, .35, .12))
  };
})();

/* a picture flies from one element to another; a puff rises above an element */
function fly(from, to, html, ms){
  const a = from.getBoundingClientRect(), b = to.getBoundingClientRect(), d = el("div", "dv-fly", html);
  const dx = b.left + b.width / 2 - a.left - a.width / 2, dy = b.top + b.height / 2 - a.top - a.height / 2;
  d.style.left = a.left + a.width / 2 + "px"; d.style.top = a.top + a.height / 2 + "px"; document.body.append(d);
  d.animate([{transform: "translate(-50%,-50%) scale(1)"}, {transform: `translate(calc(-50% + ${dx / 2}px), calc(-50% + ${dy / 2 - 70}px)) scale(1.5) rotate(-20deg)`, offset: .5},
    {transform: `translate(calc(-50% + ${dx}px), calc(-50% + ${dy}px)) scale(.5) rotate(15deg)`}], {duration: ms, easing: "ease-in-out"}).onfinish = () => d.remove();
}
function puff(from, txt){
  const a = from.getBoundingClientRect(), d = el("div", "dv-fly", txt);
  d.style.left = a.left + a.width / 2 + "px"; d.style.top = a.top + 10 + "px"; d.style.fontSize = "30px"; document.body.append(d);
  d.animate([{transform: "translate(-50%,0) scale(.4)", opacity: 1}, {transform: "translate(-50%,-50px) scale(1.2) rotate(30deg)", opacity: 1, offset: .5}, {transform: "translate(-50%,-90px) rotate(90deg)", opacity: 0}], {duration: 900, easing: "ease-out"}).onfinish = () => d.remove();
}

/* the monster: an original round creature that blinks, talks, laughs and blows raspberries */
const SKINS = [["#8E7CFF", "#C9C1FF"], ["#3DC1A5", "#B3EEDF"], ["#FF8A5B", "#FFD3BF"], ["#5AA9FF", "#C4E1FF"], ["#FF7EB6", "#FFD3E7"]];
const INK = `stroke="#1B2D45" stroke-width="3"`;
const SVG = `<svg viewBox="0 0 120 132" aria-hidden="true"><g class="dv-body">
  <g class="dv-ant"><path d="M60 24Q56 12 66 6" fill="none" ${INK} stroke-linecap="round"/><circle cx="67" cy="6" r="6" fill="#FFC43D" ${INK}/></g>
  <ellipse class="dv-skin" cx="40" cy="124" rx="14" ry="7" ${INK}/><ellipse class="dv-skin" cx="80" cy="124" rx="14" ry="7" ${INK}/>
  <ellipse class="dv-skin dv-arm" cx="13" cy="82" rx="9" ry="17" ${INK}/><ellipse class="dv-skin dv-arm" cx="107" cy="82" rx="9" ry="17" ${INK}/>
  <path class="dv-skin" d="M60 22C94 22 108 50 108 82C108 110 88 124 60 124C32 124 12 110 12 82C12 50 26 22 60 22Z" ${INK}/>
  <ellipse class="dv-belly" cx="60" cy="102" rx="28" ry="16"/>
  <g class="dv-eyes"><circle cx="44" cy="60" r="14" fill="#fff" ${INK}/><circle cx="76" cy="60" r="14" fill="#fff" ${INK}/>
    <g class="dv-pupils"><circle cx="45" cy="62" r="6.5" fill="#1B2D45"/><circle cx="77" cy="62" r="6.5" fill="#1B2D45"/><circle cx="47.5" cy="59" r="2.2" fill="#fff"/><circle cx="79.5" cy="59" r="2.2" fill="#fff"/></g>
    <g class="dv-lids"><circle class="dv-skin" cx="44" cy="60" r="15.5"/><circle class="dv-skin" cx="76" cy="60" r="15.5"/></g></g>
  <g class="dv-happy" fill="none" stroke="#1B2D45" stroke-width="4" stroke-linecap="round"><path d="M34 64Q44 50 54 64"/><path d="M66 64Q76 50 86 64"/></g>
  <ellipse class="dv-cheek" cx="29" cy="81" rx="8" ry="5" fill="#FF8FA3"/><ellipse class="dv-cheek" cx="91" cy="81" rx="8" ry="5" fill="#FF8FA3"/>
  <g class="dv-mouth"><path d="M47 83Q60 98 73 83Z" fill="#1B2D45"/><ellipse cx="60" cy="90" rx="6" ry="3" fill="#FF6F91"/></g>
  <path class="dv-tongue" d="M53 87Q52 106 60 106Q68 106 67 87Z" fill="#FF6F91" stroke="#1B2D45" stroke-width="2.5"/></g></svg>`;
function monster(calm){
  const box = el("div", "dv-monw"), btn = el("button", "dv-mon", SVG), think = el("div", "dv-think", "❓"), bub = el("div", "dv-pop");
  const [c1, c2] = SKINS[rnd(SKINS.length)];
  btn.style.setProperty("--dv-c", c1); btn.style.setProperty("--dv-c2", c2); btn.setAttribute("aria-label", "🙂");
  box.append(btn, think, bub);
  const $$ = s => btn.querySelector(s), body = $$(".dv-body");
  const later = (ms, f) => loops.push(setTimeout(f, ms));
  const flash = (cls, ms) => { btn.classList.add(cls); later(ms, () => btn.classList.remove(cls)); };
  const say2 = e => { if (calm) return; bub.textContent = e; bub.animate([{opacity: 0, transform: "translate(-50%,0) scale(.4)"}, {opacity: 1, transform: "translate(-50%,-40px) scale(1.2)", offset: .3}, {opacity: 0, transform: "translate(-50%,-80px) scale(1)"}], {duration: 1100, easing: "ease-out"}); };
  const move = (frames, ms) => { if (!calm) body.animate(frames, {duration: ms, easing: "ease-out"}); };
  const m = {
    box, think,
    talk: on => btn.classList.toggle("talk", on),
    look(x, y){ const r = btn.getBoundingClientRect(), dx = x - (r.left + r.width / 2), dy = y - (r.top + r.height * .45), k = Math.hypot(dx, dy) || 1;
      $$(".dv-pupils").style.transform = `translate(${(dx / k * 4).toFixed(1)}px, ${(dy / k * 3.5).toFixed(1)}px)`; },
    lookAt(n){ const r = n.getBoundingClientRect(); m.look(r.left + r.width / 2, r.top + r.height / 2); },
    win(face, ms){
      flash("happy", 1600); later(ms, snd.boing);
      move([{transform: "none"}, {transform: "scale(1.18,.78)", offset: .15}, {transform: "translateY(-30px) scale(.88,1.15)", offset: .45}, {transform: "scale(1.12,.88)", offset: .75}, {transform: "none"}], 800);
      if (!calm) btn.querySelectorAll(".dv-arm").forEach((a, k) => a.animate([{transform: "none"}, {transform: `rotate(${k ? 40 : -40}deg)`}, {transform: "none"}], {duration: 400, iterations: 2}));
      later(ms, () => { think.innerHTML = face; if (!calm) think.animate([{transform: "scale(.2) rotate(-40deg)"}, {transform: "scale(1.4) rotate(10deg)"}, {transform: "scale(1)"}], {duration: 500, easing: "ease-out"}); });
    },
    pfff(){
      snd.pfff(); flash("puff", 260); later(calm ? 0 : 220, () => flash("pfff", 700)); say2("💨");
      move([{transform: "none"}, {transform: "rotate(-9deg)"}, {transform: "rotate(8deg)"}, {transform: "rotate(-5deg)"}, {transform: "none"}], 650);
    }
  };
  // touch the monster: it sneezes, giggles, burps or bounces
  const tricks = [
    () => { snd.sneeze(); say2("🤧"); move([{transform: "none"}, {transform: "rotate(-10deg) scale(1.06)", offset: .6}, {transform: "rotate(12deg) translateX(6px) scale(.95)", offset: .75}, {transform: "none"}], 1100); },
    () => { snd.giggle(); flash("happy", 900); say2("😆"); move([{transform: "none"}, {transform: "rotate(6deg)"}, {transform: "rotate(-6deg)"}, {transform: "rotate(6deg)"}, {transform: "none"}], 600); },
    () => { flash("puff", 350); later(300, () => { snd.burp(); flash("talk", 500); say2("💭"); }); },
    () => { snd.boing(); say2("✨"); move([{transform: "none"}, {transform: "scale(1.2,.75)", offset: .2}, {transform: "translateY(-40px) scale(.85,1.2)", offset: .5}, {transform: "none"}], 700); }
  ];
  btn.onclick = () => { m.look(innerWidth / 2, innerHeight); tricks[rnd(tricks.length)](); };
  return m;
}

addStyle(`
.dv{display:flex; flex-direction:column; gap:14px}
.dv-scene{display:flex; gap:12px; align-items:flex-start}
.dv-monw{position:relative; flex:0 0 100px; height:112px; margin-top:8px}
.dv-mon{width:100%; height:100%; padding:0; display:block; touch-action:manipulation}
.dv-mon svg{width:100%; height:100%; overflow:visible; display:block}
.dv-mon svg *{transform-box:fill-box}
.dv-skin{fill:var(--dv-c)} .dv-belly{fill:var(--dv-c2)}
.dv-body{transform-origin:50% 100%; animation:dv-breathe 2.8s ease-in-out infinite}
.dv-lids{transform-origin:50% 0; transform:scaleY(0); animation:dv-blink 4.2s infinite}
.dv-ant{transform-origin:0 100%; animation:dv-wob 1.9s ease-in-out infinite}
.dv-arm{transform-origin:50% 10%}
.dv-mouth{transform-origin:50% 0} .dv-mon.talk .dv-mouth{animation:dv-talk .17s ease-in-out infinite alternate}
.dv-tongue{transform-origin:50% 0; transform:scaleY(0); transition:transform .15s} .dv-mon.pfff .dv-tongue{transform:scaleY(1)}
.dv-cheek{transform-origin:50% 50%; transition:transform .15s} .dv-mon.puff .dv-cheek{transform:scale(1.8)}
.dv-happy{opacity:0} .dv-mon.happy .dv-happy{opacity:1} .dv-mon.happy .dv-eyes{opacity:0}
.dv-pupils{transition:transform .15s}
.dv-think{position:absolute; right:-14px; top:-14px; width:50px; height:50px; border-radius:50%; background:#fff; border:3px solid var(--ink); box-shadow:2px 3px 0 var(--ink); display:grid; place-items:center; font-size:28px; pointer-events:none}
.dv-think .swatch{width:30px}
.dv-fly{position:fixed; z-index:60; pointer-events:none; font-size:56px; line-height:1; will-change:transform}
.dv-fly .swatch{display:block; width:56px}
.dv-pop{position:absolute; left:50%; top:20%; font-size:34px; pointer-events:none; opacity:0}
.dv-clues{flex:1; min-width:0; display:flex; flex-wrap:wrap; gap:8px; align-content:flex-start}
.dv-clue{min-width:64px; min-height:64px; padding:6px 10px; background:#fff; border:3px solid var(--ink); border-radius:18px 18px 18px 4px; box-shadow:2px 3px 0 var(--ink); display:flex; align-items:center; gap:8px; text-align:left; font-weight:700; font-size:16px; line-height:1.25; transition:transform .35s cubic-bezier(.3,1.6,.5,1), opacity .25s, background .2s}
.dv-clue.wide{flex:1 1 100%}
.dv-clue .ic{font-size:30px; line-height:1; display:flex; gap:3px; flex:none}
.dv-clue .dot{width:26px; height:26px; border-radius:50%; border:3px solid var(--ink)}
.dv-clue.now{background:var(--sun); transform:scale(1.04)}
.dv-clue.hid{opacity:0; transform:translateX(-30px) scale(.3)}
.dv-ask{flex:1 1 100%; font-family:var(--display); font-weight:700; font-size:20px}
.dv-ask small{display:block; font-family:var(--body); font-size:14px; color:var(--ink-soft)}
.dv-grid{display:grid; gap:12px; grid-template-columns:repeat(4,1fr)}
.dv-grid.n6{grid-template-columns:repeat(6,1fr)}
@media (max-width:640px){ .dv-grid{grid-template-columns:repeat(2,1fr)} .dv-grid.n6{grid-template-columns:repeat(3,1fr)} }
.dv-card{aspect-ratio:1; min-height:64px; background:#fff; padding:4px; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:2px; transition:opacity .3s, background .2s}
.dv-card .em{display:block; font-size:clamp(46px,12vw,72px); line-height:1.05}
.dv-grid.n6 .dv-card .em{font-size:clamp(38px,10vw,60px)}
.dv-card .swatch{display:block; width:56px}
.dv-card .w{font-family:var(--display); font-weight:600; font-size:15px; line-height:1.1; text-align:center; min-height:1.1em}
.dv-card.ok{background:#C9F2DF} .dv-card.no{background:#FFD6CF; opacity:.6} .dv-card.fade{opacity:.3}
.dv-calm *{animation:none!important; transition:none!important}
@keyframes dv-breathe{50%{transform:scale(1.03,.97)}}
@keyframes dv-blink{0%,90%,100%{transform:scaleY(0)} 94%{transform:scaleY(1)}}
@keyframes dv-wob{50%{transform:rotate(-9deg)}}
@keyframes dv-talk{from{transform:scaleY(.35)} to{transform:scaleY(1.15)}}
`);

const icon = s => s.split(" ").map(x => x.startsWith("#") ? `<i class="dot" style="background:${x}"></i>` : x).join("");

registerGame({id:ID, em:"🔮", name:"Devinettes", desc:"Écoute les indices, trouve qui parle", multi:true, title:DEVINETTES_UI.title, sub:DEVINETTES_UI.sub}, function () {
  const lvl = levelOf(ID), p4 = S.kid === "p4", total = lvl === 1 ? 6 : 8, calm = typeof fx === "undefined" || fx.calm();
  const all = DEVINETTES.filter(d => lex()[d.w] && (lex()[d.w].w.lvl || 1) <= Math.min(3, lvl));
  if (all.filter(d => !seen.has(d.w)).length < total) all.forEach(d => seen.delete(d.w));
  const list = pick(all.filter(d => !seen.has(d.w)), total), res = [];
  list.forEach(d => seen.add(d.w));
  startSession(ID, null, list.length); const gen = GEN;
  const wait = ms => new Promise(r => TEST || !ms ? r() : loops.push(setTimeout(r, ms)));
  let i = 0;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= list.length) return finish();
    const d = list[i], a = lex()[d.w].w, opts = options(d, lvl), idx = lvl >= 3 ? [3, 4] : lvl === 2 ? [0, 1, 2] : [0, 1];
    const icons = d.i.split("|"), clue = k => d[lng()].split("|")[idx[k]], ask = () => DEVINETTES_UI[d.who ? "who" : "what"][lng()];
    let tries = 0, locked = false, token = 0;
    renderDots(res, list.length, i);
    const body = $("gameBody"); body.innerHTML = "";
    const wrap = el("div", "dv" + (calm ? " dv-calm" : "")), scene = el("div", "dv-scene"), mon = monster(calm), box = el("div", "dv-clues");
    const bubbles = idx.map((_, k) => {
      const b = el("button", "dv-clue" + (lvl < 4 && !calm ? " hid" : ""));
      b.onclick = () => { G.replays++; token++; sayClue(k, token); };
      box.append(b); return b;
    });
    const q = el("div", "dv-ask"); box.append(q);
    const again = el("button", "speak chunky");
    const grid = el("div", "dv-grid" + (opts.length > 4 ? " n6" : ""));
    scene.append(mon.box, box); wrap.append(scene, again, grid); body.append(wrap);
    // texts in the language chosen now (the flags in the bar may switch it mid-round)
    const reads = () => true; // clues and question always written, the little one included (Kezhan: a chance to read)
    const paint = () => {
      bubbles.forEach((b, k) => {
        const ic = lvl <= 2 ? icon(icons[idx[k]]) : reads() ? "" : "👂";
        b.classList.toggle("wide", reads());
        b.innerHTML = (ic ? `<span class="ic">${ic}</span>` : "") + (reads() ? `<span>${clue(k)}</span>` : "");
      });
      q.innerHTML = (reads() ? ask() : "❓") + (lvl === 4 ? `<small>${ui("read")}</small>` : "");
      again.innerHTML = lvl === 4 ? "🔊" : `🔊 <span>${ui("again")}</span>`;
      cards.forEach((c, k) => { if (c.matches(".ok,.no")) c.querySelector(".w").textContent = word(opts[k]); });
    };
    const sayClue = async (k, my) => {
      const b = bubbles[k];
      bubbles.forEach(x => x.classList.toggle("now", x === b));
      if (b.classList.contains("hid")) { b.classList.remove("hid"); snd.blip(); }
      mon.talk(true); mon.lookAt(b);
      await Promise.all([say(clue(k), lng()), wait(lng() === "lb" ? 1600 : 500)]);
      if (my === token && alive(gen)) { mon.talk(false); b.classList.remove("now"); }
    };
    const readAll = async () => {
      const my = ++token;
      for (let k = 0; k < idx.length; k++) {
        if (!alive(gen) || my !== token || locked) return;
        await sayClue(k, my);
      }
      if (!alive(gen) || my !== token || locked) return;
      mon.talk(true); await say(ask(), lng()); if (my === token) mon.talk(false);
    };
    again.onclick = e => {
      if (!alive(gen)) return;
      paint();
      if (!e.isTrusted && lvl === 4) return; // a flag switched the language: new texts, still silent
      if (e.isTrusted) { if (lvl === 4) G.hints++; else G.replays++; }
      bubbles.forEach(b => b.classList.remove("hid"));
      readAll();
    };
    const tap = async (c, w) => {
      if (locked || !alive(gen) || c.classList.contains("no")) return;
      G.taps++;
      const em = c.querySelector(".em");
      if (w === a) {
        locked = true; token++; mon.talk(false);
        c.classList.add("ok"); c.querySelector(".w").textContent = word(w);
        const first = tries === 0; if (first) addStar(); else if (!calm) fx.sparkle();
        logRound(d.w, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, list.length, -1);
        sfx.ok(); snd.slide();
        if (!calm) fly(em, mon.think, wordFace(a), 520);
        mon.win(wordFace(a), calm ? 0 : 500);
        cards.forEach(x => { if (x !== c) x.classList.add("fade"); });
        if (!calm) em.animate([{transform: "none"}, {transform: "scale(1.35) rotate(-10deg)"}, {transform: "scale(.9) rotate(6deg)"}, {transform: "scale(1.15)"}, {transform: "none"}], {duration: 750, easing: "ease-out"});
        await say(ui("yes", word(a)), lng());
        if (!alive(gen)) return;
        i++; loops.push(setTimeout(round, TEST ? 50 : 700));
      } else {
        const my = ++token; // stops the clues being read, so the "no" line is heard
        tries++; mon.talk(false); c.classList.add("no"); c.querySelector(".w").textContent = word(w);
        if (typeof fx !== "undefined") fx.wrong();
        mon.pfff(); if (!calm) puff(c, "💫");
        if (!calm) em.animate([{transform: "none"}, {transform: "rotate(-25deg) scale(.8)"}, {transform: "rotate(20deg) scale(.85)"}, {transform: "rotate(-10deg)"}, {transform: "none"}], {duration: 600});
        if (tries >= 2 && p4) cards[opts.indexOf(a)].classList.add("bob"); // never let the little one stall
        await say(ui("no", word(w)), lng());
        if (my === token && lvl <= 2 && !locked && alive(gen)) readAll(); // the little ones hear the clues again
      }
    };
    const cards = opts.map((w, k) => {
      const c = el("button", "dv-card chunky", `<span class="em">${wordFace(w)}</span><span class="w"></span>`);
      if (w === a) markOk(c);
      c.addEventListener("pointerdown", e => mon.look(e.clientX, e.clientY));
      c.onclick = () => tap(c, w);
      grid.append(c);
      if (!calm) c.animate([{transform: "translateY(-40px) scale(.5) rotate(-8deg)", opacity: 0}, {transform: "translateY(6px) scale(1.06) rotate(3deg)", opacity: 1, offset: .7}, {transform: "none", opacity: 1}],
        {duration: 420, delay: 70 * k, easing: "ease-out", fill: "backwards"});
      return c;
    });
    paint();
    if (lvl === 4) { if (!calm) bubbles.forEach((b, k) => b.animate([{transform: "scale(.3)", opacity: 0}, {transform: "none", opacity: 1}], {duration: 380, delay: 150 * k, easing: "cubic-bezier(.3,1.6,.5,1)", fill: "backwards"})); }
    else if (calm) readAll(); else loops.push(setTimeout(readAll, 450));
  };
  round();
});
})();
