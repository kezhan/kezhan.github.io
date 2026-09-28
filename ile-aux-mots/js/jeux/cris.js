/* L'Île aux Mots : jeu « Qui fait ce bruit ? ». Each language has its own animal sounds: woof woof, wau wau, 汪汪.
   1: hear the sound, find the animal among 3 · 2: among 5, a silly question, a band of two to tap in order
   3: "What does the duck say?", written sounds, "In which language?", a band of three · 4: all written, the voice only on 🔊, verbs ("Who barks?").
   What the child sees and hears is in the language being learnt (en, de, lb, zh), never French. Luxembourgish has no voice:
   the lod.lu recording of the animal is played, and its sound (spelt as German reads it: wau wau, mu, quak) by the German voice. */
addStyle(`
.cris-head{display:flex; align-items:center; gap:14px}
.cris-mas{position:relative; flex:0 0 auto; width:84px; height:84px; padding:0}
.cris-mas svg{width:100%; height:100%; overflow:visible}
.cris-mas .m-lid{transform-box:fill-box; transform-origin:50% 0; transform:scaleY(0)}
.cris-mas .m-mouth,.cris-mas .m-cheek,.cris-mas .m-cup,.cris-mas .m-tongue{transform-box:fill-box; transform-origin:center}
.cris-mas .m-body{transform-origin:60px 112px}
.cris-say{flex:1; min-width:0; position:relative; background:#fff; padding:10px 12px; text-align:center; font-family:var(--display); font-size:clamp(19px,5vw,26px); font-weight:600; line-height:1.15; overflow-wrap:anywhere}
.cris-say::before{content:""; position:absolute; left:-10px; top:calc(50% - 8px); width:14px; height:14px; background:#fff; border-left:3px solid var(--ink); border-bottom:3px solid var(--ink); transform:rotate(45deg)}
.cris-say small{display:block; margin-top:4px; font-family:var(--body); font-size:15px; font-weight:800; color:var(--ink-soft)}
.cris-again{min-height:64px}
.cris-hint{min-width:64px; min-height:64px; font-size:28px; align-self:center}
.cris-grid{display:grid; gap:10px; grid-template-columns:repeat(3,1fr)}
.cris-grid.two{grid-template-columns:repeat(2,1fr)}
.cris-card{position:relative; aspect-ratio:1; min-height:64px; background:#fff; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:2px; padding:4px}
.cris-emo{display:inline-block; font-size:clamp(44px,13vw,72px); line-height:1.1; transform-origin:50% 85%}
.cris-card .w,.cris-yn .w{font-family:var(--display); font-weight:600; font-size:15px; min-height:1.1em; line-height:1.05; text-align:center; overflow-wrap:anywhere}
.cris-card.ok,.cris-snd.ok,.cris-yn.ok{background:#C9F2DF} .cris-card.ko,.cris-snd.ko,.cris-yn.ko{background:#FFD6CF}
.cris-snd{min-height:72px; background:#fff; padding:8px; border-radius:26px; font-family:var(--display); font-weight:600; font-size:21px; line-height:1.1; overflow-wrap:anywhere}
.cris-yn{min-height:104px; background:#fff; font-size:48px; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:4px; padding:6px}
.cris-yn .w{font-size:18px}
.cris-big{position:relative; align-self:center; background:none; padding:40px 16px 0; font-size:96px; line-height:1}
.cris-big .cris-bw{top:0} .cris-mas .cris-bw{top:-10px; left:-4px; right:auto; justify-content:flex-start}
.cris-say.pic b{font-size:34px}
.cris-big .cris-emo{font-size:96px}
.cris-bw{position:absolute; left:-18px; right:-18px; top:-20px; display:flex; justify-content:center; pointer-events:none; z-index:5}
.cris-bub{max-width:170px; background:#fff; border:3px solid var(--ink); border-radius:14px; box-shadow:2px 3px 0 var(--ink); padding:2px 9px; font-family:var(--display); font-weight:700; font-size:17px; line-height:1.15; text-align:center}
.cris-fly{position:fixed; pointer-events:none; z-index:60; font-size:26px}
.cris-strip{display:flex; flex-wrap:wrap; gap:6px; justify-content:center}
.cris-strip span{background:var(--sand); border:2px solid var(--ink); border-radius:999px; padding:3px 10px; font-weight:800; font-size:15px}
.cris-num{position:absolute; top:4px; left:6px; width:26px; height:26px; display:grid; place-items:center; background:var(--sun); border:2px solid var(--ink); border-radius:50%; font-family:var(--display); font-weight:700; font-size:15px}
`);
(() => {
  const TX = CRIS_TXT, L4 = ["en", "de", "lb", "zh"];
  const lng = () => L4.includes(langOf()) ? langOf() : "en";
  const calm = () => typeof fx === "undefined" || fx.calm();
  const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
  const mark = (s, L, m) => s + (L === "zh" ? {"!": "！", "?": "？"}[m] : m); // Chinese punctuation is full width
  const LEX = {}; THEMES.animals.words.forEach(w => { LEX[w.en] = w; });
  const ANI = CRIS_DATA.animals.map(a => { const w = LEX[a.lex] || {}; return Object.assign({n: {en: w.en, de: w.de, lb: w.lb, zh: w.zh}}, a); });
  const nm = (a, L) => L === "en" ? "the " + a.n.en : L === "zh" ? a.zk || a.n.zh : a.n[L]; // in a sentence
  const lab = (a, L) => L === "zh" ? a.zk || a.n.zh : a.n[L];                                // under a picture

  // cartoon noises with Web Audio, one per animal (nz in the content) and a few for the game
  const NZ = Object.assign({}, CRIS_DATA.fx); CRIS_DATA.animals.forEach(a => { NZ[a.k] = a.nz; });
  let white = null;
  function noise(k){ // returns its length in ms
    const seq = NZ[k]; if (TEST || !seq) return 0;
    try {
      ac = ac || new (window.AudioContext || window.webkitAudioContext)();
      if (ac.state === "suspended") ac.resume();
      let t = ac.currentTime + .02, end = t;
      seq.forEach(([type, f0, f1, dur, vr = 0, vd = 0, vol = .2, gap = 0]) => {
        const g = ac.createGain(), f = ac.createBiquadFilter();
        f.type = type === "noise" ? "bandpass" : "lowpass"; f.frequency.value = type === "noise" ? f0 : Math.min(8000, Math.max(f0, f1) * 3);
        g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.setValueAtTime(vol, t + dur * .7); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
        let src;
        if (type === "noise") {
          if (!white) { white = ac.createBuffer(1, ac.sampleRate * 1.5, ac.sampleRate); const d = white.getChannelData(0); for (let j = 0; j < d.length; j++) d[j] = Math.random() * 2 - 1; }
          src = ac.createBufferSource(); src.buffer = white;
        } else {
          src = ac.createOscillator(); src.type = type;
          src.frequency.setValueAtTime(f0, t); src.frequency.exponentialRampToValueAtTime(f1, t + dur);
          if (vr) { const lfo = ac.createOscillator(), lg = ac.createGain(); lfo.frequency.value = vr; lg.gain.value = vd; lfo.connect(lg); lg.connect(src.frequency); lfo.start(t); lfo.stop(t + dur); }
        }
        src.connect(f); f.connect(g); g.connect(ac.destination); src.start(t); src.stop(t + dur + .02);
        end = Math.max(end, t + dur); t += dur + gap;
      });
      return (end - ac.currentTime) * 1000;
    } catch (e) { return 0; }
  }

  // dances: squash and stretch on the emoji, transform only
  const MV = {
    hop: [{transform: "none"}, {transform: "scale(1.25,.75)", offset: .15}, {transform: "translateY(-40px) scale(.85,1.2)", offset: .45}, {transform: "scale(1.2,.8)", offset: .75}, {transform: "none"}],
    wag: [{transform: "none"}, {transform: "rotate(-16deg)"}, {transform: "rotate(16deg)"}, {transform: "rotate(-12deg)"}, {transform: "rotate(12deg)"}, {transform: "none"}],
    shake: [{transform: "none"}, {transform: "translateX(-9px) rotate(-8deg)"}, {transform: "translateX(9px) rotate(8deg)"}, {transform: "translateX(-7px) rotate(-6deg)"}, {transform: "translateX(7px) rotate(6deg)"}, {transform: "none"}],
    spin: [{transform: "none"}, {transform: "rotate(180deg) scale(1.3)"}, {transform: "rotate(360deg)"}],
    fly: [{transform: "none"}, {transform: "translate(18px,-34px) rotate(12deg)"}, {transform: "translate(-18px,-46px) rotate(-12deg)"}, {transform: "translate(0,-20px)"}, {transform: "none"}],
    slither: [{transform: "none"}, {transform: "skewX(22deg) translateX(-8px)"}, {transform: "skewX(-22deg) translateX(8px)"}, {transform: "skewX(14deg)"}, {transform: "none"}],
    stomp: [{transform: "none"}, {transform: "scale(1.3,.7)"}, {transform: "translateY(-14px) scale(.9,1.15)"}, {transform: "scale(1.3,.7)"}, {transform: "none"}],
    gallop: [{transform: "none"}, {transform: "translateX(-12px) rotate(-12deg)"}, {transform: "translate(12px,-10px) rotate(10deg)"}, {transform: "translateX(-6px) rotate(-6deg)"}, {transform: "none"}],
    puff: [{transform: "none"}, {transform: "scale(1.5)", offset: .35}, {transform: "scale(1.5) rotate(-6deg)", offset: .6}, {transform: "scale(.85)", offset: .8}, {transform: "none"}],
    zoom: [{transform: "none"}, {transform: "translateX(-30px) scale(.9)"}, {transform: "translateX(30px) scale(1.1)"}, {transform: "translateX(-12px)"}, {transform: "none"}],
    no: [{transform: "none"}, {transform: "rotate(-14deg)"}, {transform: "rotate(14deg)"}, {transform: "rotate(-10deg)"}, {transform: "none"}]
  };
  const move = (e, m, n = 1) => { if (e && !calm()) e.animate(MV[m] || MV.hop, {duration: 720, iterations: n, easing: "ease-in-out"}); };
  function bubble(box, text, keep){
    const w = el("div", "cris-bw"), b = el("span", "cris-bub"); b.textContent = text; w.append(b); box.append(w);
    if (!calm()) b.animate([{transform: "scale(0) rotate(-14deg)", opacity: 0}, {transform: "scale(1.15) rotate(4deg)", opacity: 1, offset: .6}, {transform: "none", opacity: 1}], {duration: 380, easing: "ease-out"});
    if (!keep) loops.push(setTimeout(() => w.remove(), 1900));
  }
  // emoji rising from an element: music notes, a sneeze, a burp
  function puff(txt, box, n = 1, rise = 110){
    if (calm() || !box) return;
    const r = box.getBoundingClientRect();
    for (let k = 0; k < n; k++) {
      const d = el("div", "cris-fly"); d.textContent = Array.isArray(txt) ? txt[k % txt.length] : txt;
      d.style.left = (r.left + r.width * (.25 + Math.random() * .5)) + "px"; d.style.top = (r.top + r.height * .3) + "px";
      document.body.append(d);
      const dx = rnd(60) - 30;
      d.animate([{transform: "translate(-50%,0) scale(.4)", opacity: 0}, {transform: `translate(calc(-50% + ${dx / 3}px),-${rise / 4}px) scale(1.1)`, opacity: 1, offset: .25},
        {transform: `translate(calc(-50% + ${dx}px),-${rise}px) scale(.9) rotate(${dx}deg)`, opacity: 0}], {duration: 1100 + rnd(400), delay: k * 140, easing: "ease-out", fill: "backwards"}).onfinish = () => d.remove();
    }
  }

  // the host: a round listener with headphones who blinks, puffs its cheeks, sticks its tongue out and burps
  const MAS = `<svg viewBox="0 0 120 120" aria-hidden="true"><g class="m-body">
    <path d="M20 62 Q60 -8 100 62" fill="none" stroke="#1B2D45" stroke-width="7" stroke-linecap="round"/>
    <ellipse cx="60" cy="72" rx="42" ry="38" fill="#FFB84D" stroke="#1B2D45" stroke-width="4"/>
    <rect class="m-cup" x="8" y="54" width="18" height="30" rx="8" fill="#FF6F59" stroke="#1B2D45" stroke-width="4"/>
    <rect class="m-cup" x="94" y="54" width="18" height="30" rx="8" fill="#FF6F59" stroke="#1B2D45" stroke-width="4"/>
    <ellipse cx="46" cy="64" rx="10" ry="12" fill="#fff" stroke="#1B2D45" stroke-width="3"/><ellipse cx="74" cy="64" rx="10" ry="12" fill="#fff" stroke="#1B2D45" stroke-width="3"/>
    <circle cx="48" cy="66" r="5" fill="#1B2D45"/><circle cx="76" cy="66" r="5" fill="#1B2D45"/>
    <ellipse class="m-lid" cx="46" cy="64" rx="11.5" ry="13.5" fill="#FFB84D" stroke="#1B2D45" stroke-width="3"/>
    <ellipse class="m-lid" cx="74" cy="64" rx="11.5" ry="13.5" fill="#FFB84D" stroke="#1B2D45" stroke-width="3"/>
    <circle class="m-cheek" cx="32" cy="84" r="7" fill="#FF8FA3"/><circle class="m-cheek" cx="88" cy="84" r="7" fill="#FF8FA3"/>
    <ellipse class="m-tongue" cx="60" cy="96" rx="6" ry="7" fill="#FF6F8F" stroke="#1B2D45" stroke-width="2" opacity="0"/>
    <ellipse class="m-mouth" cx="60" cy="89" rx="9" ry="4" fill="#7A2531"/></g></svg>`;
  function mas(m, what, ms = 900){
    if (!m || calm()) return;
    const an = (s, k, o) => m.querySelectorAll(s).forEach(x => x.animate(k, o));
    if (what === "blink") an(".m-lid", [{transform: "scaleY(0)"}, {transform: "scaleY(1)"}, {transform: "scaleY(0)"}], {duration: 220});
    if (what === "talk") {
      const n = Math.max(2, Math.round(ms / 260));
      an(".m-mouth", [{transform: "none"}, {transform: "scale(1.2,3)"}, {transform: "none"}], {duration: 260, iterations: n});
      an(".m-cheek", [{transform: "none"}, {transform: "scale(1.7)"}, {transform: "none"}], {duration: 520, iterations: Math.ceil(n / 2)});
      an(".m-cup", [{transform: "none"}, {transform: "scale(1.2)"}, {transform: "none"}], {duration: 300, iterations: n});
    }
    if (what === "happy") an(".m-body", [{transform: "none"}, {transform: "scale(1.15,.85)", offset: .2}, {transform: "translateY(-22px) scale(.92,1.1)", offset: .5}, {transform: "scale(1.1,.9)", offset: .8}, {transform: "none"}], {duration: 700, easing: "ease-out"});
    if (what === "oops") {
      an(".m-tongue", [{opacity: 0, transform: "translateY(-6px)"}, {opacity: 1, transform: "translateY(3px)", offset: .2}, {opacity: 1, transform: "translateY(3px) scaleY(1.3)", offset: .8}, {opacity: 0, transform: "translateY(-6px)"}], {duration: 1200});
      an(".m-lid", [{transform: "scaleY(0)"}, {transform: "scaleY(.6)", offset: .2}, {transform: "scaleY(.6)", offset: .8}, {transform: "scaleY(0)"}], {duration: 1200});
      an(".m-body", MV.no, {duration: 650});
    }
    if (what === "burp") {
      an(".m-cheek", [{transform: "none"}, {transform: "scale(2.2)", offset: .6}, {transform: "none"}], {duration: 700});
      an(".m-mouth", [{transform: "none"}, {transform: "scale(.5)", offset: .6}, {transform: "scale(1.6,4)", offset: .75}, {transform: "none"}], {duration: 900});
      an(".m-body", [{transform: "none"}, {transform: "scale(1.12)", offset: .6}, {transform: "scale(.95,1.05)", offset: .75}, {transform: "none"}], {duration: 900});
    }
  }

  registerGame({id: "cris", em: "🐮", name: "Qui fait ce bruit ?", desc: "Les cris des animaux, différents dans chaque langue", multi: true,
    title: {en: "Who says moo?", de: "Wer macht muh?", lb: "Wien seet mu?", zh: "谁在哞哞叫？"},
    sub: {en: "Woof! Meow! Moo!", de: "Wau wau! Miau! Muh!", lb: "Wau wau! Miau! Mu!", zh: "汪汪！喵喵！哞哞！"}}, function () {
    const lvl = levelOf("cris"), p4 = S.kid === "p4";
    const TYPES = [["hear", "hear", "silly", "hear", "hear", "hear"],
      ["hear", "hear", "silly", "hear", "hear", "silly", "hear", "band"],
      ["what", "who", "what", "silly", "lang", "who", "what", "band"],
      ["what", "who", "verb", "lang", "what", "silly", "who", "verb", "lang", "band"]][lvl - 1];
    const total = TYPES.length, res = [];
    let L = lng(), plan = makePlan(L), i = 0, tries = 0, locked = false, step = 0, M = null, big = null, row = null, cards = [];
    startSession("cris", null, total); const gen = GEN;
    const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));
    // Luxembourgish: the animal's lod.lu recording, then its sound read by the German voice
    async function talk(text, a, s){
      if (L !== "lb") return say(text, L);
      if (a && a.lex) await say(a.n.lb, "lb");
      if (s && alive(gen)) await say(s, "de");
    }

    function makePlan(L, from = 0){
      const pool = ANI.filter(a => a.s[L] && a.lvl <= Math.min(lvl, 3)), used = new Set(), snd = a => a.s[L];
      const fresh = list => { const c = list.filter(a => !used.has(a.k)), l = c.length ? c : list, a = l[rnd(l.length)]; used.add(a.k); return a; };
      // animals whose sound (or verb) differs from the ones already taken: never two right answers
      const apart = (list, n, f, taken) => { const seen = new Set(taken), out = []; shuffle(list).forEach(b => { if (out.length < n && !seen.has(f(b))) { out.push(b); seen.add(f(b)); } }); return out; };
      const others = (a, n, f = snd) => apart(pool.filter(b => b !== a), n, f, [f(a)]);
      return TYPES.map((t, k) => {
        if (k < from) return null;
        if (t === "verb" && !TX[L].verbQ) t = "who";
        if (t === "hear" || t === "who") { const a = fresh(pool), n = t === "hear" ? (lvl === 1 ? 3 : 5) : lvl === 3 ? 4 : 6; return {t, a, o: shuffle(others(a, n - 1).concat(a))}; }
        if (t === "what") { const a = fresh(pool); return {t, a, o: shuffle(others(a, lvl === 3 ? 2 : 3).concat(a))}; }
        if (t === "verb") { const a = fresh(pool.filter(b => b.v && b.v[L])); return {t, a, o: shuffle(others(a, 5, b => (b.v && b.v[L]) || b.k).concat(a))}; }
        if (t === "silly") { const a = fresh(pool), yes = rnd(2) === 0; return {t, a, yes, claim: yes ? snd(a) : snd(others(a, 1)[0])}; }
        if (t === "lang") { // a sound heard in one language only: the cow says moo, muh, mu, 哞哞
          const alone = (a, x) => a.s[x] && L4.every(y => y === x || a.s[y] !== a.s[x]);
          const ok = pool.filter(a => L4.some(x => x !== L && alone(a, x))), a = fresh(ok.length ? ok : pool), xs = L4.filter(x => x !== L && alone(a, x));
          return {t, a, x: xs.length ? xs[rnd(xs.length)] : L};
        }
        const seq = apart(pool, lvl === 2 ? 2 : 3, snd, []);
        return {t: "band", seq, o: shuffle(seq.concat(apart(pool.filter(b => !seq.includes(b)), (lvl === 2 ? 4 : lvl === 3 ? 5 : 6) - seq.length, snd, seq.map(snd))))};
      });
    }

    function headline(r, tx){
      const a = r.a, s = a && a.s[L];
      // the question is always written, the little one included (Kezhan: a chance to read)
      if (r.t === "hear") return ["🔊 " + mark(cap(s), L, "!"), tx.who(s)];
      if (r.t === "silly") return ["🤔 " + cap(tx.silly(nm(a, L), r.claim)), "👍 👎"];
      if (r.t === "what") return [cap(tx.what(nm(a, L))), ""];
      if (r.t === "who") return [cap(tx.who(s)), ""];
      if (r.t === "verb") return [cap(tx.verbQ(a.v[L])), ""];
      if (r.t === "lang") return [cap(tx.langQ(nm(a, L), a.s[r.x])), ""];
      return [tx.band, tx.bandAsk];
    }
    function card(b){
      const c = el("button", "cris-card chunky", `<span class="cris-emo">${b.e}</span><span class="w"></span>`);
      c.dataset.k = b.k; if (lvl >= 3) c.querySelector(".w").textContent = lab(b, L);
      return c;
    }
    function draw(){
      const r = plan[i], tx = TX[L], body = $("gameBody"); body.innerHTML = ""; cards = []; big = null; step = 0;
      const head = el("div", "cris-head"), bub = el("div", "cris-say chunky"), [top, sub] = headline(r, tx);
      M = el("button", "cris-mas", MAS); M.setAttribute("aria-label", "🎧");
      M.onclick = () => { G.taps++; noise("hihi"); mas(M, "happy"); };
      const b0 = el("b"); b0.textContent = top; bub.append(b0); if (!/\p{L}/u.test(top)) bub.classList.add("pic"); // pictures only: bigger
      if (sub) { const s = el("small"); s.textContent = sub; bub.append(s); }
      head.append(M, bub); body.append(head);
      row = el("div", "row"); row.style.justifyContent = "center";
      const btn = lvl < 4 ? el("button", "speak chunky cris-again", "🔊 <span></span>") : el("button", "chip cris-hint", "🔊");
      if (lvl < 4) btn.querySelector("span").textContent = tx.again;
      btn.onclick = () => { if (relang()) return; if (lvl < 4) G.replays++; else G.hints++; cue(); };
      row.append(btn); body.append(row);
      if (r.t === "band" && lvl >= 4) { const s = el("div", "cris-strip"); r.seq.forEach((b, k) => { const sp = el("span"); sp.textContent = `${k + 1}. ${b.s[L]}`; s.append(sp); }); body.append(s); }
      if (r.t === "silly" || r.t === "what" || r.t === "lang") {
        big = el("button", "cris-big", `<span class="cris-emo">${r.a.e}</span>`);
        big.onclick = () => { G.taps++; hop(big, r.a, r.t === "lang" ? r.a.s[r.x] : null); };
        body.append(big);
        if (r.t === "silly") bubble(big, mark(r.claim, L, "?"), true);
      }
      const grid = el("div", "cris-grid" + (["silly", "what", "lang"].includes(r.t) ? " two" : ""));
      if (r.t === "silly") [[true, "👍", tx.yes], [false, "👎", tx.no]].forEach(([v, em, w]) => {
        const b = el("button", "cris-yn chunky", `${em}<span class="w"></span>`); b.querySelector(".w").textContent = w;
        if (v === r.yes) markOk(b); b.onclick = () => answer(b, v === r.yes, null); grid.append(b);
      });
      else if (r.t === "what") r.o.forEach(b => {
        const s = el("button", "cris-snd chunky"); s.textContent = b.s[L];
        if (b === r.a) markOk(s); s.onclick = () => answer(s, b === r.a, b); grid.append(s);
      });
      else if (r.t === "lang") L4.forEach(x => {
        const f = LANGS[x] || {flag: "", label: x}, b = el("button", "cris-yn chunky", `${f.flag}<span class="w"></span>`); b.querySelector(".w").textContent = f.label;
        if (x === r.x) markOk(b); b.onclick = () => answer(b, x === r.x, x); grid.append(b);
      });
      else r.o.forEach(b => {
        const c = card(b); if (r.t !== "band" && b === r.a) markOk(c);
        c.onclick = () => r.t === "band" ? bandTap(c, b) : answer(c, b === r.a, b); grid.append(c); cards.push(c);
      });
      body.append(grid);
      if (r.t === "band") marks();
    }
    // an animal dances, makes its noise and says it in a bubble, music notes fly
    function hop(box, a, text){ move(box.querySelector(".cris-emo"), a.mv); noise(a.k); bubble(box, text || a.s[L]); puff(["🎵", "🎶"], box, 2); }
    // the sound in the four languages, where it is checked
    function strip(a){
      const s = el("div", "cris-strip");
      L4.forEach(x => { if (a.s[x]) { const sp = el("span"); sp.textContent = `${(LANGS[x] || {}).flag || x} ${a.s[x]}`; s.append(sp); } });
      row.innerHTML = ""; row.append(s);
      if (!calm()) [...s.children].forEach((sp, k) => sp.animate([{transform: "scale(0)", opacity: 0}, {transform: "none", opacity: 1}], {duration: 320, delay: k * 120, fill: "backwards", easing: "cubic-bezier(.2,1.4,.4,1)"}));
    }

    async function cue(){
      const r = plan[i], tx = TX[L], a = r.a, g = i, still = () => alive(gen) && i === g;
      mas(M, "talk");
      if (r.t === "band") {
        if (L !== "lb") await talk(tx.band);
        for (const b of r.seq) {
          if (!still()) return;
          mas(M, "talk", 700); bubble(M, b.s[L]);
          await wait(noise(b.k) + 150); if (!still()) return;
          await talk(cap(b.s[L]) + "!", null, b.s[L]); await wait(250);
        }
        return;
      }
      if (r.t === "hear") { await wait(noise(a.k) + 150); if (!still()) return; return talk(`${cap(a.s[L])}! ${tx.who(a.s[L])}`, null, a.s[L]); }
      if (r.t === "silly") return talk(cap(tx.silly(nm(a, L), r.claim)), a, r.claim);
      if (r.t === "what") return talk(cap(tx.what(nm(a, L))), a, null);
      if (r.t === "who") return talk(cap(tx.who(a.s[L])), null, a.s[L]);
      if (r.t === "verb") return L === "lb" ? noise(a.k) : talk(cap(tx.verbQ(a.v[L])));
      return talk(cap(tx.langQ(nm(a, L), a.s[r.x])), a, null);
    }
    function round(){
      if (!alive(gen)) return;
      if (i >= total) return finish();
      tries = 0; locked = false; renderDots(res, total, i);
      draw();
      if (lvl < 4) cue();
    }
    // flags in the game bar: the rest of the game is made again in the new language
    function relang(){
      if (lng() === L) return false;
      L = lng();
      const from = locked ? i + 1 : i;
      plan = plan.slice(0, from).concat(makePlan(L, from).slice(from));
      if (!locked && i < total) { draw(); if (lvl < 4) cue(); }
      return true;
    }

    function praiseFor(r){
      const tx = TX[L], a = r.a, p = tx.praise[rnd(tx.praise.length)];
      if (r.t === "band") return talk(tx.bandDone);
      if (r.t === "lang") return talk(p).then(() => alive(gen) && say(a.s[r.x], r.x === "lb" ? "de" : r.x)); // the sound in its own language
      const s = a.s[L], line = r.t === "verb" ? `${cap(tx.verbA(nm(a, L), a.v[L]))} ${cap(s)}!` : cap(tx.says(nm(a, L), s));
      return talk((r.t === "silly" ? (r.yes ? tx.yes : tx.no) + "! " : p + " ") + line, a, s);
    }
    async function success(btn, word){
      const r = plan[i]; locked = true; btn.classList.add("ok"); sfx.ok();
      const first = !tries; if (first) addStar(); else if (!calm()) fx.sparkle();
      logRound(word, first, tries + 1, {lvl, t: r.t}); res.push(first ? 1 : 0); renderDots(res, total, -1);
      mas(M, "happy");
      if (r.t === "band") { // the whole band plays, then the host burps
        cards.filter(c => r.seq.some(b => b.k === c.dataset.k)).forEach((c, k) => loops.push(setTimeout(() => { move(c.querySelector(".cris-emo"), ANI.find(b => b.k === c.dataset.k).mv, 2); puff(["🎵", "🎶", "♪"], c, 3); }, k * 150)));
        loops.push(setTimeout(() => { noise("burp"); mas(M, "burp"); puff("💨", M, 1, 60); }, 1000));
        await wait(1600);
      } else {
        hop(big || btn, r.a, r.t === "lang" ? r.a.s[r.x] : null);
        const w = btn.querySelector(".w"); if (w && ["hear", "who", "verb"].includes(r.t)) w.textContent = lab(r.a, L);
        if (lvl >= 3) strip(r.a);
        await wait(500);
      }
      if (!alive(gen)) return;
      await praiseFor(r);
      if (!alive(gen)) return;
      i++; loops.push(setTimeout(round, TEST ? 100 : 700));
    }
    async function answer(btn, ok, b){
      if (locked) return; G.taps++;
      const r = plan[i], a = r.a, tx = TX[L];
      if (ok) return success(btn, r.t === "lang" ? a.k + ":" + r.x : a.k);
      tries++; btn.classList.add("ko"); mas(M, "oops");
      if (typeof fx !== "undefined") fx.wrong();
      if (r.t === "silly") { noise("pouet"); return talk(cap(tx.says(nm(a, L), a.s[L])), a, a.s[L]); } // the truth
      if (r.t === "what") { noise("pouet"); btn.textContent = `${b.e} ${b.s[L]}`; return talk(cap(tx.says(nm(b, L), b.s[L])), b, b.s[L]); }
      if (r.t === "lang") { noise("pouet"); btn.querySelector(".w").textContent = a.s[b] || "…"; if (a.s[b]) say(a.s[b], b === "lb" ? "de" : b); return; }
      // another animal: it shakes its head and says what it really says
      btn.querySelector(".w").textContent = lab(b, L); move(btn.querySelector(".cris-emo"), "no"); noise(b.k);
      if (p4 && tries >= 2) { const c = cards.find(c => c.dataset.k === a.k); if (c) c.classList.add("bob"); }
      const line = r.t === "verb" && b.v && b.v[L] ? cap(tx.verbA(nm(b, L), b.v[L])) : `${cap(tx.thats(nm(b, L)))} ${cap(tx.says(nm(b, L), b.s[L]))}`;
      await talk(line, b, b.s[L]);
      if (lvl <= 2 && alive(gen) && !locked) talk(`${cap(a.s[L])}?`, null, a.s[L]);
    }
    function marks(){
      if (!TEST) return;
      const r = plan[i]; cards.forEach(c => delete c.dataset.ok);
      const next = r.seq[step] && cards.find(c => c.dataset.k === r.seq[step].k); if (next) next.dataset.ok = "1";
    }
    function bandTap(c, b){
      if (locked) return; G.taps++;
      const r = plan[i], at = r.seq.indexOf(b);
      if (at > -1 && at < step) return hop(c, b); // already playing: it dances again
      if (at === step) {
        c.append(el("span", "cris-num", String(step + 1))); c.classList.add("ok"); hop(c, b); step++; marks();
        if (step === r.seq.length) success(c, "band:" + r.seq.map(x => x.k).join("+"));
        return;
      }
      tries++; c.classList.add("ko"); mas(M, "oops"); move(c.querySelector(".cris-emo"), "no"); noise(b.k);
      if (typeof fx !== "undefined") fx.wrong();
      loops.push(setTimeout(() => c.classList.remove("ko"), 700));
      if (lvl < 4) { const want = r.seq[step]; talk(`${cap(want.s[L])}!`, null, want.s[L]); }
    }

    // life between turns: the host blinks, an animal sneezes or burps now and then
    function blink(){ if (!alive(gen)) return; mas(M, "blink"); loops.push(setTimeout(blink, 2200 + rnd(2600))); }
    function antic(){
      if (!alive(gen)) return;
      const busy = "speechSynthesis" in window && speechSynthesis.speaking;
      if (!locked && cards.length && !busy) {
        const c = cards[rnd(cards.length)], e = c.querySelector(".cris-emo");
        if (rnd(3)) {
          e.animate([{transform: "none"}, {transform: "scale(1.15,1.1) rotate(-8deg)", offset: .6}, {transform: "translateY(5px) scale(.9,1.05) rotate(6deg)", offset: .72}, {transform: "none"}], {duration: 1000});
          loops.push(setTimeout(() => { noise("sneeze"); puff("💦", c, 2, 50); }, 600));
        } else {
          e.animate([{transform: "none"}, {transform: "scale(1.2)", offset: .5}, {transform: "scale(.95)", offset: .7}, {transform: "none"}], {duration: 700});
          loops.push(setTimeout(() => { noise("burp"); puff("💨", c, 1, 60); }, 350));
        }
      }
      loops.push(setTimeout(antic, 6500 + rnd(5000)));
    }
    loops.push(setInterval(() => { if (alive(gen)) relang(); }, 500));
    if (!calm()) { loops.push(setTimeout(blink, 1500)); loops.push(setTimeout(antic, 6000)); }
    round();
  });
})();
