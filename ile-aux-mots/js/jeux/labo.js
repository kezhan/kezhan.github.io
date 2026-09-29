/* L'Île aux Mots : jeu « Le Labo des monstres » (ticket du 27/09/2026, atelier de conception).
   The voice describes a monster, the child builds it from the drawers, then touches the lightning: "It's alive!".
   1: one order at a time (Make it blue!) · 2: three traits heard at once · 3: five traits with no, big/small, left/right
   4: six traits to read (the big one), or which of three monsters it is. Every third monster, a free one the game describes.
   In English, German, Luxembourgish or Chinese (js/contenus/labo.js), never French. */
addStyle(`
.labo{display:flex; flex-direction:column; gap:10px} .labo .lab-top{display:flex; flex-direction:column; align-items:center; gap:8px}
.labo .lab-txt{font-family:var(--display); font-size:19px; line-height:1.3; text-align:center; max-width:100%} .labo .lab-txt p{margin:2px 0}
.labo .lab-txt .hd{font-size:15px; color:var(--ink-soft)} .labo .lab-txt p.ok{color:var(--good)} .labo .lab-txt p.ko{color:var(--bad); text-decoration:underline wavy}
.labo .lab-ear{font-size:30px} .labo .lab-hint{font-size:22px; padding:8px 18px; background:#fff; align-self:center}
.labo .lab-chips{display:flex; gap:6px; flex-wrap:wrap; justify-content:center} .labo .lab-chips span.ko{background:#FFD6CF}
.labo .lab-chips span{font-size:20px; padding:1px 8px; border:2px solid var(--ink); border-radius:999px; background:#C9F2DF}
.labo .lab-grid{display:grid; gap:12px} .labo .lab-col{display:flex; flex-direction:column; gap:10px; min-width:0}
.labo .lab-stage{position:relative; height:260px; border:3px solid var(--ink); border-radius:18px; overflow:hidden;
  background:radial-gradient(ellipse 70% 16% at 50% 97%, #C9F2DF 60%, transparent 62%), linear-gradient(#E3F1FF, #FBFDFF)}
@media (min-width:760px){.labo .lab-grid{grid-template-columns:1fr 1fr; align-items:start} .labo .lab-stage{height:340px}}
.labo .lab-mon{position:absolute; inset:4px 0 0; display:flex; justify-content:center; align-items:flex-end}
.labo .lab-mon > svg{height:100%; width:auto; max-width:100%; transform-origin:50% 100%; cursor:pointer}
.labo .mon g{transform-box:fill-box; transform-origin:50% 50%} .labo .mon .bd{transform-origin:50% 100%; animation:labBreath 2.8s ease-in-out infinite}
.labo .mon .eye{animation:labBlink 4.5s infinite} .labo .mon .arm{transform-box:view-box; animation:labWave 3.2s ease-in-out infinite}
.labo .lab-3{display:grid; grid-template-columns:repeat(3,1fr); gap:6px; padding:8px; align-items:center}
.labo .lab-one{background:#fff; padding:2px; min-height:64px} .labo .lab-one svg{width:100%; height:auto; display:block}
.labo .lab-alive{position:absolute; right:8px; bottom:8px; width:86px; height:86px; border-radius:50%; background:var(--sun); z-index:2;
  display:flex; flex-direction:column; align-items:center; justify-content:center; font-family:var(--display); font-weight:700; font-size:13px; line-height:1.05; padding:4px}
.labo .lab-alive b{font-size:34px; line-height:1} .labo .lab-alive.go{animation:labPulse .9s ease-in-out infinite; background:#FFE066}
.labo .lab-bub{position:absolute; left:50%; top:8px; transform:translateX(-50%); max-width:86%; background:#fff; border:3px solid var(--ink); border-radius:16px;
  padding:6px 12px; font-family:var(--display); font-size:18px; text-align:center; opacity:0; pointer-events:none; z-index:3; transition:opacity .25s}
.labo .lab-deco{position:absolute; left:8px; bottom:6px; font-size:30px; opacity:.8; pointer-events:none} .labo .lab-deco.r{left:auto; right:12px; top:6px; bottom:auto}
.labo .lab-fx{position:absolute; pointer-events:none; font-size:34px; z-index:4; transform:translate(-50%,-50%)} .labo .lab-fx.big{font-size:90px}
.labo .lab-fx i{display:block; width:18px; height:18px; border-radius:50%; border:2px solid var(--ink)} .labo .lab-flash{position:absolute; inset:0; background:#fff; opacity:0; pointer-events:none; z-index:3}
.labo .lab-tabs,.labo .lab-drawer{display:grid; grid-template-columns:repeat(auto-fill,minmax(64px,1fr)); gap:7px}
.labo .lab-tab{min-height:64px; font-size:32px; background:#fff; display:grid; place-items:center}
.labo .lab-tab[aria-pressed="true"]{background:var(--sun); transform:translateY(-3px); box-shadow:3px 7px 0 var(--ink)}
.labo .lab-drawer{background:var(--sand); border:3px solid var(--ink); border-radius:16px; padding:8px}
.labo .lab-pc{min-height:64px; background:#fff; font-size:34px; display:flex; flex-direction:column; align-items:center; justify-content:center; padding:2px; line-height:1.1}
.labo .lab-pc svg{width:54px; height:54px} .labo .lab-pc .mon *{animation:none} .labo .lab-pc small{font-size:13px; font-family:var(--display); font-weight:700}
.labo .lab-pc[aria-pressed="true"]{background:#C9F2DF; outline:3px solid var(--leaf)}
.labo .lab-sw{border-radius:50%; aspect-ratio:1} .labo .lab-sw[aria-pressed="true"]{outline:4px solid var(--ink); outline-offset:3px}
.labo .lab-zoo{display:flex; gap:6px; align-items:center; min-height:64px; overflow:hidden} .labo .lab-home{font-size:30px}
.labo .lab-z{width:58px; height:64px; background:#fff; border:2px solid var(--ink); border-radius:14px; padding:2px} .labo .lab-z svg{width:100%; height:100%}
.labo.calm *{animation:none!important; transition:none!important}
@keyframes labBreath{50%{transform:scale(1.035,.965)}} @keyframes labBlink{0%,92%,100%{transform:none} 95%{transform:scaleY(.1)}}
@keyframes labWave{50%{transform:rotate(var(--w))}} @keyframes labPulse{50%{transform:scale(1.12) rotate(-8deg)}}
`);
(() => {
const K = "labo", snd = LABO.snd;
const langL = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const anim = (e, k, o) => e && !calm() ? e.animate(k, typeof o === "number" ? {duration: o, easing: "cubic-bezier(.3,1.4,.5,1)"} : o) : null;
/* the monster's own voice: pitch and speed carry the mood; Luxembourgish plays the lexicon word of the line */
function voice(l, tone){
  const L = langL();
  if (L === "lb") return l.k ? say(l.k, "lb") : Promise.resolve();
  // a recorded Azure voice wins over the monster's tone: fluid on every device
  if (!tone || TEST || !("speechSynthesis" in window) || (typeof voixFiles === "function" && voixFiles(l.t, L))) return say(l.t, L);
  return new Promise(res => {
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(l.t), v = voiceFor(L); let done = false; const fin = () => { if (!done) { done = true; res(); } };
      u.lang = LANG[L]; if (v) u.voice = v; u.pitch = tone.p; u.rate = tone.r * voiceRate() / 0.8; u.onend = u.onerror = fin; loops.push(setTimeout(fin, 3000 + l.t.length * 110 / u.rate));
      speechSynthesis.speak(u);
    } catch (e) { res(); }
  });
}
/* ---------- the monster and its traits live in js/contenus/labo.js ---------- */
const {svg, MAX, SZK, BLANK, sat, put, key, traits, vary, describe, one} = LABO;
const TABS = [["skin", "🎨"], ["eyes", "👀"], ["mood", "😀"], ["arms", "💪"], ["legs", "🦵"], ["ears", "👂"], ["hat", "🎩"], ["hold", "🍌"]];
const TAB_OF = {col: "skin", pat: "skin", eyes: "eyes", es: "eyes", mood: "mood", arms: "arms", legs: "legs", ears: "ears", rs: "ears", hat: "hat", hold: "hold", side: "hold"};
const ICON = {col: "🎨", pat: "✨", eyes: "👀", mood: "😀", arms: "💪", legs: "🦵", ears: "👂", hat: "🎩", hold: "✋"};
/* the zoo: every monster made, per child, in this browser */
const ZK = "iam.labo", zooAll = () => { try { return JSON.parse(localStorage.getItem(ZK) || "{}") || {}; } catch (e) { return {}; } };
const zooAdd = (kid, z) => { try { const a = zooAll(); a[kid] = [...(a[kid] || []), z].slice(-12); localStorage.setItem(ZK, JSON.stringify(a)); } catch (e) {} };
registerGame({id: K, em: "🧪", name: "Le labo des monstres", desc: "Écoute, fabrique le monstre, il prend vie", multi: true,
  title: {en: "Monster Lab", de: "Monsterlabor", lb: "Monsterlabo", zh: "怪兽实验室"},
  sub: {en: "Build the monster you hear!", de: "Bau das Monster, das du hörst!", lb: "Bau d'Monster, dat s de héiers!", zh: "做出你听到的怪兽！"}}, function () {
  const lvl = levelOf(K), kid = S.kid, gen = GEN, reading = lvl === 4 && kid === "p7";
  const plan = lvl === 4 ? ["build", "which", "build", "which"] : ["build", "build", "build", "build"], total = plan.length, res = [];
  startSession(K, null, total);
  let i = 0, wins = 0, freeNext = false, busy = false, cur = null, m = {...BLANK}, tab = "skin", said = "", rep0 = 0, talkId = 0;
  const W = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));
  const body = $("gameBody"); body.innerHTML = "";
  const root = el("div", "labo" + (calm() ? " calm" : "")), top = el("div", "lab-top"), txt = el("div", "lab-txt"), chips = el("div", "lab-chips");
  const again = el("button", reading ? "speak chip lab-hint" : "speak chunky");
  const stage = el("div", "lab-stage"), mon = el("div", "lab-mon"), bub = el("div", "lab-bub"), bolt = el("button", "lab-alive chunky");
  const tabsBox = el("div", "lab-tabs"), drawer = el("div", "lab-drawer"), zoo = el("div", "lab-zoo"), grid = el("div", "lab-grid"), c1 = el("div", "lab-col"), c2 = el("div", "lab-col");
  stage.append(el("span", "lab-deco", "🧪"), el("span", "lab-deco r", "⚗️"), mon, bub, bolt);
  c1.append(stage); c2.append(tabsBox, drawer, zoo); top.append(again, txt, chips); grid.append(c1, c2); root.append(top, grid); body.append(root);

  /* ---------- funny moves ---------- */
  const spawn = (h, x, y, kf, d, cls = "lab-fx") => {
    if (calm()) return;
    const e = el("div", cls, h); e.style.left = x + "%"; e.style.top = y + "%"; stage.append(e);
    e.animate(kf, {duration: d, easing: "cubic-bezier(.2,.8,.3,1)", fill: "forwards"}).onfinish = () => e.remove();
  };
  const RISE = [{transform: "translate(-50%,-50%) scale(.5)", opacity: 1}, {transform: "translate(-50%,-260%) scale(1.3)", opacity: 0}];
  const POP = [{transform: "translate(-50%,-50%) scale(0)"}, {transform: "translate(-50%,-50%) scale(1.3)", offset: .3}, {transform: "translate(-50%,-50%) scale(1)", opacity: 1, offset: .8}, {transform: "translate(-50%,-50%) scale(1)", opacity: 0}];
  const SHAKE = [{transform: "none"}, {transform: "rotate(-9deg)"}, {transform: "rotate(9deg)"}, {transform: "rotate(-6deg)"}, {transform: "none"}];
  const squash = s => anim(s, [{transform: "none"}, {transform: "scale(1.2,.78)"}, {transform: "scale(.9,1.12)"}, {transform: "scale(1.04,.97)"}, {transform: "none"}], 600);
  const jump = (s, h) => anim(s, [{transform: "none"}, {transform: "scale(1.15,.85)", offset: .2}, {transform: `translateY(-${h}px) scale(.92,1.1)`, offset: .5}, {transform: "scale(1.1,.9)", offset: .8}, {transform: "none"}], 650);
  const splash = c => { for (let j = 0; j < 9; j++) { const a = j / 9 * 6.28, d = 70 + rnd(40); spawn(`<i style="background:${c}"></i>`, 50, 55, [{transform: "translate(-50%,-50%)"}, {transform: `translate(calc(-50% + ${Math.cos(a) * d}px), calc(-50% + ${Math.sin(a) * d}px)) scale(.3)`, opacity: 0}], 550); } };
  const bubble = (h, ms = 1900) => {
    bub.innerHTML = h; if (TEST) return;
    bub.style.opacity = 1; anim(bub, [{transform: "translateX(-50%) scale(.3)"}, {transform: "translateX(-50%) scale(1.1)", offset: .6}, {transform: "translateX(-50%) scale(1)"}], 350);
    clearTimeout(bub.h); bub.h = setTimeout(() => { bub.style.opacity = 0; }, ms); loops.push(bub.h);
  };
  const center = () => { const r = stage.getBoundingClientRect(); return [r.left + r.width / 2, r.top + r.height * .45]; };
  function poke(){
    if (busy) return;
    G.taps++; const s = mon.firstChild;
    one([
      () => { snd("giggle"); squash(s); anim(s.querySelector(".ch"), [{transform: "none"}, {transform: "scale(1.8)"}, {transform: "none"}], 600); bubble(LABO.UI.giggle[langL()], 900); },
      () => { snd("burp"); jump(s, 16); spawn("🫧", 50, 60, RISE, 1000); },
      () => { snd("sneeze"); anim(s, [{transform: "none"}, {transform: "rotate(-6deg) scale(1.05)", offset: .45}, {transform: "translateY(6px) scale(1.12,.85)", offset: .55}, {transform: "none"}], 700); spawn("🤧", 70, 30, POP, 900); },
      () => { snd("honk"); jump(s, 50); },
      () => { snd("slide"); anim(s, [{transform: "none", transformOrigin: "50% 55%"}, {transform: "rotate(360deg) scale(.8)", transformOrigin: "50% 55%"}, {transform: "rotate(360deg)", transformOrigin: "50% 55%"}], {duration: 800, easing: "ease-in-out"}); }
    ])();
  }
  // draw the monster again; the part just changed pops in with a sound
  function draw(k){
    mon.innerHTML = svg(m); const s = mon.firstChild; s.onclick = poke;
    if (!k) return;
    const part = {col: ".bd", pat: ".bd", eyes: ".ey", es: ".ey", mood: ".mo", arms: ".am", legs: ".lg", ears: ".er", rs: ".er", hat: ".ht", hold: ".ho", side: ".ho"}[k];
    snd({col: "splat", pat: "splat", mood: "boing", arms: "boing", legs: "boing", hat: "slide", side: "slide"}[k] || "pop");
    if (part !== ".bd") anim(s.querySelector(part), [{transform: "scale(.2)", opacity: 0}, {transform: "scale(1.25)", opacity: 1, offset: .6}, {transform: "scale(.95)"}, {transform: "none"}], 450);
    squash(s);
    if (k === "col" && m.col) splash(LABO.hex(m.col));
  }

  /* ---------- tabs and drawers ---------- */
  const tabIds = lvl === 1 ? ["skin", "eyes", "mood", "hat", "hold"] : lvl === 2 ? ["skin", "eyes", "mood", "arms", "legs", "hat", "hold"] : TABS.map(x => x[0]);
  const tabBtn = {};
  TABS.filter(([id]) => tabIds.includes(id)).forEach(([id, e]) => {
    const b = el("button", "lab-tab chunky", e); b.onclick = () => { G.taps++; snd("pop"); openTab(id); };
    tabBtn[id] = b; tabsBox.append(b);
  });
  function openTab(id){
    tab = id;
    Object.entries(tabBtn).forEach(([k, b]) => { b.setAttribute("aria-pressed", String(k === id)); b.classList.remove("bob"); });
    fillDrawer(); mark();
    anim(drawer, [{transform: "scaleY(.7)", opacity: .4}, {transform: "none", opacity: 1}], 260);
  }
  const VB = {eyes: "58 46 124 96", arms: "0 50 240 150", legs: "58 172 124 60", ears: "10 0 220 150", pat: "46 44 148 164"};
  const range = k => { const a = []; for (let n = lvl >= 3 ? 0 : 1; n <= (lvl === 1 ? 3 : MAX[k]); n++) a.push(n); return a; };
  const pressed = () => drawer.querySelectorAll("[data-k]").forEach(b => b.setAttribute("aria-pressed", String(String(m[b.dataset.k] ?? "none") === b.dataset.v)));
  function fillDrawer(){
    drawer.innerHTML = ""; const L = langL();
    const add = (k, v, h, cls = "") => { const b = el("button", "lab-pc chunky " + cls, h); b.dataset.k = k; b.dataset.v = v; b.onclick = () => tapPiece(k, v); drawer.append(b); return b; };
    const mini = (x, k) => svg({...BLANK, arms: 0, legs: 0, ...x}, VB[k]);
    if (tab === "skin") {
      (lvl === 1 ? LABO.COLS.slice(0, 4) : LABO.COLS).forEach(c => { const b = add("col", c, "", "lab-sw"); b.style.background = LABO.hex(c); b.setAttribute("aria-label", LABO.colW(c, L)); });
      if (lvl >= 3) ["none", ...Object.keys(LABO.PAT)].forEach(p => add("pat", p, p === "none" ? "✖" : mini({col: "blue", pat: p}, "pat")));
    } else if (MAX[tab]) {
      range(tab).forEach(n => add(tab, n, mini({[tab]: n}, tab)));
      if (lvl >= 3 && SZK[tab]) ["big", "small"].forEach(s => add(SZK[tab], s, mini({[tab]: 1, [SZK[tab]]: s}, tab) + `<small>${LABO.SIZE[s][L]}</small>`));
    } else if (tab === "mood") Object.keys(LABO.MOOD).slice(0, lvl === 1 ? 3 : lvl === 2 ? 4 : 5).forEach(v => add("mood", v, LABO.MOOD[v].e));
    else if (tab === "hat") ["none", ...Object.keys(LABO.HAT)].forEach(v => add("hat", v, v === "none" ? "✖" : LABO.HAT[v].e));
    else {
      if (lvl >= 3) ["L", "R"].forEach(s => add("side", s, `${s === "L" ? "👈" : "👉"}<small>${LABO.SIDE[s].w[L]}</small>`));
      ["none", ...Object.keys(LABO.HOLD)].forEach(v => add("hold", v, v === "none" ? "✖" : LABO.HOLD[v].e));
    }
    pressed();
  }
  function tapPiece(k, v){
    if (busy || !cur || cur.mode === "which") return;
    G.taps++;
    m[k] = v === "none" ? null : MAX[k] ? +v : v;
    draw(k); pressed();
    if (cur.mode === "orders") order(k);
    mark();
  }

  /* ---------- what is said and shown ---------- */
  const ui = o => ({t: o[langL()], hd: true});
  function lines(){
    const L = langL();
    if (!cur) return [];
    // level 1: happy, sad and sleepy are heard in the tone of the voice too
    if (cur.mode === "orders") { const t = cur.traits[cur.idx], a = t && t.k === "mood" && LABO.ACT[t.v]; return t ? [{...LABO.line(t, L, "do"), tone: a && {p: a.p, r: a.r * .9}}] : [ui(LABO.UI.ready)]; }
    if (cur.mode === "free") return [ui(LABO.UI.free)];
    const head = cur.mode === "which" ? LABO.UI.which : reading ? LABO.UI.read : LABO.UI.listen;
    return [ui(head), ...cur.traits.map(t => LABO.line(t, L))];
  }
  function showTxt(list){
    const L = langL(); said = L;
    again.innerHTML = reading ? "🔊" : `🔊 <span>${LABO.UI.again[L]}</span>`;
    bolt.innerHTML = `<b>⚡</b><span>${LABO.UI.alive[L]}</span>`;
    // always written, for both children and at every level (Kezhan: a chance to read), with the voice on top below level 4
    txt.innerHTML = (list || lines()).map((l, j) => `<p class="${l.hd ? "hd" : ""}" data-j="${j}">${l.t}</p>`).join("");
  }
  async function talk(list){
    const my = ++talkId;
    for (const l of list || lines()) { if (!alive(gen) || my !== talkId) return; await voice(l, l.tone); }
  }
  again.onclick = () => {
    if (langL() !== said) { showTxt(); if (cur && cur.mode !== "which") fillDrawer(); if (reading) return; } // a flag in the bar: new language
    if (reading) G.hints++; else G.replays++;
    talk();
  };

  /* ---------- the recette touches the next useful thing ---------- */
  function pieceSel(t){
    if (t.n != null) return m[t.k] !== t.n ? `[data-k="${t.k}"][data-v="${t.n}"]` : `[data-k="${SZK[t.k]}"][data-v="${t.s}"]`;
    if (t.k === "hold" && m.hold === t.v) return `[data-k="side"][data-v="${t.side}"]`;
    return `[data-k="${t.k}"][data-v="${t.v}"]`;
  }
  function mark(){
    if (!TEST) return;
    root.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok);
    if (busy || !cur) return;
    if (cur.mode === "which") return markOk(cur.right);
    const need = cur.mode === "orders" ? cur.traits.slice(cur.idx, cur.idx + 1) : cur.mode === "desc" ? cur.traits.filter(t => !sat(t, m)) : [];
    if (!need.length) return markOk(bolt);
    const tb = TAB_OF[need[0].k];
    markOk(tab === tb ? drawer.querySelector(pieceSel(need[0])) : tabBtn[tb]);
  }

  /* ---------- level 1: one order at a time ---------- */
  function order(k){
    const t = cur.traits[cur.idx];
    if (!t) return;
    if (sat(t, m)) {
      cur.log.push([t, cur.tries]); cur.tries = 0; cur.idx++;
      sfx.ok(); if (!calm()) fx.sparkle(...center(), 10);
      busy = true;
      loops.push(setTimeout(() => {
        if (!alive(gen)) return;
        busy = false;
        if (cur.idx < cur.traits.length) openTab(TAB_OF[cur.traits[cur.idx].k]); else bolt.classList.add("go");
        showTxt(); talk(); mark();
      }, TEST ? 30 : 800));
    } else if (TAB_OF[k] === TAB_OF[t.k]) {
      cur.tries++; sfx.ko(); anim(mon.firstChild, SHAKE, 500);
      if (kid === "p4" && cur.tries >= 2) { const b = drawer.querySelector(pieceSel(t)); if (b) b.classList.add("bob"); }
      loops.push(setTimeout(() => { if (alive(gen)) talk(); }, 600));
    }
  }

  /* ---------- "It's alive!" ---------- */
  bolt.onclick = async () => {
    if (busy || !cur || cur.mode === "which") return;
    G.taps++;
    if (cur.mode === "orders" && cur.idx < cur.traits.length) { snd("fizzle"); bubble(LABO.UI.notyet[langL()], 1200); anim(mon.firstChild, SHAKE, 500); return; }
    if (cur.mode === "desc") {
      cur.checks++;
      const bad = cur.traits.filter(t => !sat(t, m));
      bad.forEach(t => { t.f = (t.f || 0) + 1; });
      // every trait asked lights up green or red
      chips.innerHTML = cur.traits.map(t => `<span class="${sat(t, m) ? "ok" : "ko"}">${ICON[t.k]}</span>`).join("");
      txt.querySelectorAll("p[data-j]").forEach(p => { const t = cur.traits[p.dataset.j - 1]; if (t) p.className = sat(t, m) ? "ok" : "ko"; });
      if (bad.length) return fizzle(bad[0]);
    }
    celebrate();
  };
  // not quite: the lightning fizzles and the monster says only what is wrong ("I've got THREE eyes!")
  async function fizzle(t){
    busy = true; mark();
    sfx.ko(); snd("fizzle"); spawn("⚡", 50, 22, POP, 600); anim(mon.firstChild, SHAKE, 700);
    const l = LABO.line(t, langL(), "me", t.k === "hold" && m.hold === t.v);
    bubble(l.h, 3200);
    await voice(ui(LABO.UI.almost), {p: 1.3, r: 1}); if (!alive(gen)) return;
    await voice(l, {p: 1.5, r: .95}); if (!alive(gen)) return;
    busy = false;
    if (kid === "p4" && cur.checks >= 2 && tabBtn[TAB_OF[t.k]]) tabBtn[TAB_OF[t.k]].classList.add("bob");
    mark();
  }
  async function moodShow(s){
    const a = LABO.ACT[m.mood], L = langL();
    bubble(a[L], 2400);
    ({
      happy: () => { snd("giggle"); jump(s, 30); if (!calm()) fx.sparkle(...center(), 14); },
      sad: () => { snd("sad"); [42, 58].forEach((x, j) => spawn("💧", x, 40, [{transform: "translate(-50%,0)", opacity: 1}, {transform: "translate(-50%,90px)", opacity: 0}], 1100 + j * 200)); anim(s, [{transform: "none"}, {transform: "scale(1.03,.9)"}, {transform: "none"}], 1200); },
      angry: () => { snd("growl"); spawn("💢", 70, 22, POP, 1000); anim(s, {transform: ["none", "translateX(-6px)", "translateX(6px)", "none"]}, {duration: 120, iterations: 6}); },
      scared: () => { snd("squeak"); spawn("‼️", 70, 20, POP, 900); anim(s, {transform: ["none", "translate(-3px,1px)", "translate(3px,-1px)", "none"]}, {duration: 90, iterations: 10}); },
      sleepy: () => { snd("snore"); [0, 1, 2].forEach(j => spawn("💤", 62 + j * 6, 30, RISE, 1400 + j * 250)); anim(s.querySelector(".mo"), [{transform: "none"}, {transform: "scale(1.9)"}, {transform: "none"}], 1300); }
    })[m.mood]();
    await voice({t: a[L], k: a.k}, {p: a.p, r: a.r});
  }
  // it comes to life: flash, jump, blink, burp or joke, dance, its mood, its silly name, then off to the zoo
  async function liveShow(seed){
    const s = mon.firstChild, L = langL(), ok = () => alive(gen);
    snd("zap");
    const fl = el("div", "lab-flash"); stage.append(fl); if (anim(fl, [{opacity: 0}, {opacity: .9}, {opacity: 0}], {duration: 380})) loops.push(setTimeout(() => fl.remove(), 400)); else fl.remove();
    spawn("⚡", 50, 30, [{transform: "translate(-50%,-160%) scale(2)", opacity: 0}, {transform: "translate(-50%,-50%) scale(1.2)", opacity: 1, offset: .4}, {transform: "translate(-50%,-50%)", opacity: 0}], 700, "lab-fx big");
    anim(s, [{transform: "none"}, {transform: "scale(1.3,.6)", offset: .3}, {transform: "translateY(-60px) scale(.85,1.2)", offset: .6}, {transform: "scale(1.15,.85)", offset: .85}, {transform: "none"}], 900);
    await W(450); if (!ok()) return; snd("boing");
    s.querySelectorAll(".eye").forEach(e => anim(e, [{transform: "none"}, {transform: "scaleY(.1)"}, {transform: "none"}], 260));
    await W(500); if (!ok()) return;
    // a burp, a little dance, then its mood played out, or a line of its own
    snd("burp"); spawn("🫧", 56, 52, RISE, 1000);
    anim(s, [{transform: "none"}, {transform: "rotate(-12deg) translateY(-12px)"}, {transform: "rotate(12deg)"}, {transform: "rotate(-12deg) translateY(-12px)"}, {transform: "rotate(10deg)"}, {transform: "none"}], {duration: 1400, easing: "ease-in-out"});
    s.querySelectorAll(".leg").forEach((g, j) => anim(g, [{transform: "none"}, {transform: "translateY(-10px)"}, {transform: "none"}], {duration: 350, iterations: 4, delay: j * 80}));
    await W(600); if (!ok()) return;
    if (m.mood) await moodShow(s);
    else { const q = one(LABO.SAYS); bubble(q[L], 2200); await voice({t: q[L], k: q.k}, {p: 1.5, r: 1.05}); }
    if (!ok()) return;
    const hi = LABO.hello(seed, L);
    bubble(`<b>${hi}</b>`, 2400); snd("tada"); if (!calm()) fx.sparkle(...center(), 16);
    await voice({t: hi}, {p: 1.6, r: 1});
  }
  async function celebrate(){
    busy = true; mark(); bolt.classList.remove("go");
    const free = cur.mode === "free", seed = [rnd(99), rnd(99), rnd(99)];
    if (!free) {
      let first;
      if (cur.mode === "orders") { cur.log.forEach(([t, n]) => logRound(key(t), !n, n + 1, {lvl})); first = cur.log.every(([, n]) => !n); }
      else if (cur.mode === "which") { logRound("which monster", !cur.tries, cur.tries + 1, {lvl}); first = !cur.tries; }
      else { cur.traits.forEach(t => logRound(key(t), !t.f, (t.f || 0) + 1, {lvl})); first = cur.checks === 1; }
      if (first) addStar();
      // bonus star for holding the whole description in mind: heard once, or twice at most
      if (first && (lvl === 2 || lvl === 3) && G.replays - rep0 <= 1) addStar();
      res.push(first ? 1 : 0); renderDots(res, total, -1);
      wins++; i++;
      if (wins % 3 === 0 && i < total) freeNext = true;
    }
    await liveShow(seed); if (!alive(gen)) return;
    if (free) {
      // the game tells what the child made
      const L = langL(), list = [ui(LABO.UI.look), ...describe(m).map(t => LABO.line(t, L))];
      showTxt(list); await talk(list); if (!alive(gen)) return;
    }
    const s = mon.firstChild; snd("slide");
    anim(s, [{transform: "none"}, {transform: "translateY(40px) scale(.15)", opacity: 0}], {duration: 520, easing: "ease-in", fill: "forwards"});
    await W(520); if (!alive(gen)) return;
    zooAdd(kid, {...m, seed}); drawZoo(true);
    loops.push(setTimeout(next, TEST ? 30 : 500));
  }

  /* ---------- the zoo: touch a monster, it tells who it is ---------- */
  function drawZoo(fresh){
    zoo.innerHTML = "<span class='lab-home'>🏡</span>";
    const list = (zooAll()[kid] || []).slice(-4);
    list.forEach((z, j) => {
      const b = el("button", "lab-z", svg(z)); b.setAttribute("aria-label", LABO.name(z.seed, langL()));
      b.onclick = async () => {
        if (busy) return;
        G.taps++; snd("boing"); anim(b, [{transform: "none"}, {transform: "translateY(-14px) scale(1.15)"}, {transform: "none"}], 450);
        const L = langL(), hi = LABO.hello(z.seed, L), my = ++talkId;
        bubble(`<b>${hi}</b>`, 2400);
        for (const l of [{t: hi}, ...describe(z).slice(0, 4).map(t => LABO.line(t, L))]) { if (!alive(gen) || my !== talkId) return; await voice(l, {p: 1.5, r: 1}); }
      };
      zoo.append(b);
      if (fresh && j === list.length - 1) anim(b, [{transform: "scale(0)"}, {transform: "scale(1.4)"}, {transform: "none"}], 500);
    });
  }

  /* ---------- level 4: which of the three monsters is it? ---------- */
  function which(heard){
    cur = {mode: "which", traits: traits(3, 5, heard), tries: 0};
    const sh = rnd(LABO.SHAPES), make = ts => ts.reduce(put, {...BLANK, sh});
    const opts = [{m: make(cur.traits)}, ...pick(cur.traits, 2).map(t => { const u = vary(t); return {m: make(cur.traits.map(x => x === t ? u : x)), u, side: u.k === "hold" && u.v === t.v}; })];
    tabsBox.hidden = drawer.hidden = bolt.hidden = true;
    mon.innerHTML = ""; mon.classList.add("lab-3");
    shuffle(opts).forEach(o => {
      const b = el("button", "lab-one chunky", svg(o.m));
      if (!o.u) cur.right = b;
      b.onclick = () => {
        if (busy) return;
        G.taps++;
        if (!o.u) { m = o.m; mon.classList.remove("lab-3"); bolt.hidden = false; draw(); return celebrate(); }
        // the wrong one says what makes it different
        cur.tries++; sfx.ko(); anim(b, SHAKE, 450);
        const l = LABO.line(o.u, langL(), "me", o.side); bubble(l.h, 2600); voice(l, {p: 1.5, r: 1});
      };
      mon.append(b);
    });
    showTxt(); if (!reading) talk(); mark();
  }

  function next(){
    if (!alive(gen)) return;
    busy = false; chips.innerHTML = ""; bolt.classList.remove("go");
    tabsBox.hidden = drawer.hidden = bolt.hidden = false; mon.classList.remove("lab-3");
    if (freeNext) { freeNext = false; cur = {mode: "free"}; }
    else {
      if (i >= total) return finish();
      renderDots(res, total, i);
      const heard = kid === "p4" && langL() === "lb";
      if (plan[i] === "which") return which(heard);
      cur = lvl === 1 ? {mode: "orders", traits: traits(1, 3, heard), idx: 0, tries: 0, log: []}
        : {mode: "desc", traits: traits(lvl, [0, 0, 3, 5, 6][lvl], heard), checks: 0};
    }
    m = {...BLANK, sh: rnd(LABO.SHAPES)}; rep0 = G.replays;
    draw(); snd("slide");
    anim(mon.firstChild, [{transform: "translateY(-160px) scale(.5)"}, {transform: "translateY(0) scale(1.15,.85)", offset: .6}, {transform: "none"}], 650);
    openTab(cur.mode === "orders" ? TAB_OF[cur.traits[0].k] : "skin");
    showTxt(); if (!reading || cur.mode === "free") talk();
    mark();
  }
  drawZoo(false);
  next();
});
})();
