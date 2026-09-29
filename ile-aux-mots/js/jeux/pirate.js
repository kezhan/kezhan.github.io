/* L'Île aux Mots : jeu « Le Robot pirate » (ticket _tickets/ouvert/2026-09-27_jeu-robot-pirate.md). Texts, drawings, sounds and islands: js/contenus/pirate.js.
   A little pirate robot looks for treasure on a grid island. In English, German, Luxembourgish or Chinese, never French.
   1: drive it with the finger to the place the voice names · 2: the parrot dictates the way, arrows then dig
   3: a clue with landmarks (left of, between…), then a program of cards, "three times" included, fewest cards for a bonus star
   4: a robot that turns (forward, turn left, turn right), a key before the chest, compass clues, and a buggy program to predict and fix. */
(() => {
const P = PIRATE, SND = P.snd, {D, O, at, same, markAt, path, sim, picto} = P.isle;
let lang = "en";
const L = () => (lang = ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en");
const t = (o, ...a) => { const v = o[lang] || o.en; return typeof v === "function" ? v(...a) : v; };
const clueText = r => t(P.clue, t(P.rel[r.kind], r.a, r.b));
const calm = () => fx.calm();
const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));
// Luxembourgish has no voice: the lod.lu recordings of the lexicon words are played instead
const speak = (text, lbw) => lang !== "lb" ? say(text, lang) : (lbw || []).reduce((p, w) => p.then(() => say(w, "lb")), Promise.resolve());
// some voices never say they have finished: never wait for them longer than max
const talk = (text, lbw, max = 3000) => Promise.race([speak(text, lbw), wait(max)]);

addStyle(`
.pir{display:flex; flex-direction:column; gap:12px} .pir-top{display:flex; align-items:center; gap:14px; position:relative}
.pir-parrot{font-size:50px; width:68px; height:68px; flex:none; display:grid; place-items:center; animation:pir-bob 2.2s ease-in-out infinite}
.pir-bubble{flex:1; min-width:0; min-height:68px; position:relative; background:#fff; border:3px solid var(--ink); border-radius:18px; padding:8px 12px; text-align:center; font-family:var(--display); font-weight:600; font-size:20px; line-height:1.25; display:flex; flex-direction:column; justify-content:center; gap:4px; word-break:keep-all; overflow-wrap:anywhere} /* Chinese: 两格 stays whole, the line breaks after ！ */
.pir-bubble::before{content:""; position:absolute; left:-15px; top:50%; margin-top:-9px; border:9px solid transparent; border-right:12px solid var(--ink); border-left:0}
.pir-bubble b{font-weight:600} .pir-bubble small{font-size:15px; color:var(--ink-soft)} .pir-pic{font-size:30px; line-height:1.15}
.pir-psay{position:absolute; left:36px; top:-16px; z-index:5; max-width:220px; background:var(--sun); border:2px solid var(--ink); border-radius:14px; padding:4px 10px; font-family:var(--display); font-weight:600; font-size:16px; opacity:0; pointer-events:none}
.pir-row{display:flex; align-items:center; justify-content:center; gap:12px; flex-wrap:wrap} .pir-bag{font-size:26px; letter-spacing:2px}
.pir-goal{margin:0; text-align:center; font-family:var(--display); font-weight:600; font-size:17px; color:var(--ink-soft)} .pir-goal:empty{display:none}
.pir-board{--cw:min(100vw - 70px, 440px); position:relative; align-self:center; width:min(100%, 440px); aspect-ratio:1; display:grid; grid-template-columns:repeat(var(--n),1fr); grid-template-rows:repeat(var(--n),1fr); background:linear-gradient(160deg,#5CCBEA,#2E9FC7); border:3px solid var(--ink); border-radius:18px}
.pir-cell{position:relative; margin:2px; border-radius:10px; display:grid; place-items:center; font-size:calc(var(--cw) / var(--n) * .5); line-height:1}
.pir-cell > *{grid-area:1/1; pointer-events:none} .pir-cell.pir-s{background:#F7DC92; box-shadow:inset 0 -4px 0 #E3BD62}
.pir-cell.pir-w::after{content:"〰"; color:#fff; font-size:.55em; opacity:.7; animation:pir-wave 2.8s ease-in-out infinite}
.pir-cell.pir-start{background:#FBE7B4; outline:3px dashed rgba(27,45,69,.3); outline-offset:-7px}
.pir-cell .pir-m{animation:pir-sway 3.2s ease-in-out infinite} .pir-cell .pir-key{animation:pir-bob 1.4s ease-in-out infinite}
.pir-trace{font-size:.38em; opacity:.6} .pir-trace.pir-old{opacity:.22} .pir-hole{font-size:.8em} .pir-x{font-size:.7em; opacity:.85} .pir-pin{font-size:.8em} .pir-shovel{font-size:.8em}
.pir-chest{width:76%} .pir-chest svg{width:100%; display:block; overflow:visible} .pir-trash{width:64px; height:64px; font-size:28px; background:#fff}
.pir-lid{transform-box:fill-box; transform-origin:0% 100%; transition:transform .35s cubic-bezier(.3,1.5,.5,1)} .pir-lid.pir-open{transform:rotate(-40deg)}
.pir-bot{position:absolute; top:0; left:0; width:calc(100% / var(--n)); height:calc(100% / var(--n)); pointer-events:none; z-index:2}
.pir-body{position:absolute; inset:4%; pointer-events:auto; cursor:pointer; animation:pir-bob 1.6s ease-in-out infinite} .pir-body svg{width:100%; height:100%; display:block; overflow:visible}
.pir-dir{position:absolute; inset:-10%; display:none; transition:transform .35s ease; pointer-events:none} .pir-turn .pir-dir{display:block}
.pir-dir i{position:absolute; left:50%; top:-4%; margin-left:-12px; border:12px solid transparent; border-bottom:19px solid var(--coral); border-top:0}
.pir-hat{position:absolute; left:20%; top:-42%; font-size:calc(var(--cw) / var(--n) * .56); opacity:0; pointer-events:none} .pir-wet .pir-hat{opacity:1; animation:pir-sway 1s ease-in-out infinite}
.pir-haskey .pir-body::after{content:"🔑"; position:absolute; right:-10%; bottom:-6%; font-size:calc(var(--cw) / var(--n) * .3); pointer-events:none}
.pir-say{position:absolute; bottom:96%; left:0; z-index:3; width:max-content; max-width:160px; background:#fff; border:2px solid var(--ink); border-radius:12px; padding:3px 8px; font-family:var(--display); font-weight:600; font-size:15px; opacity:0; pointer-events:none} .pir-say.pir-right{left:auto; right:0}
.pir-eye{transform-box:fill-box; transform-origin:center; animation:pir-blink 4s infinite} .pir-ant{transform-box:fill-box; transform-origin:50% 100%; animation:pir-ant 1.8s ease-in-out infinite}
.pir-cheek{transform-box:fill-box; transform-origin:center} .pir-pup{transition:transform .2s}
.pir-board.pir-drive{touch-action:none} .pir-ctl{display:flex; flex-direction:column; gap:10px; align-items:center}
.pir-pad{display:grid; grid-template-columns:repeat(3,78px); grid-template-rows:repeat(2,74px); gap:8px} .pir-arrow.a-up{grid-area:1/2} .pir-arrow.a-left{grid-area:2/1} .pir-arrow.a-down{grid-area:2/2} .pir-arrow.a-right{grid-area:2/3}
.pir-arrow{background:#fff; display:flex; flex-direction:column; align-items:center; justify-content:center; font-family:var(--display); font-weight:600; line-height:1.05} .pir-arrow span{font-size:30px} .pir-arrow small{font-size:13px}
.pir-pal{display:grid; grid-template-columns:repeat(auto-fit,minmax(64px,1fr)); gap:6px; width:100%}
.pir-prog{display:flex; flex-wrap:wrap; gap:6px; width:100%; min-height:82px; padding:6px; border:3px dashed var(--ink-soft); border-radius:16px; background:rgba(255,255,255,.55); align-content:flex-start}
.pir-card{min-width:64px; min-height:64px; padding:4px 6px; border:3px solid var(--ink); border-radius:14px; box-shadow:2px 3px 0 var(--ink); display:flex; flex-direction:column; align-items:center; justify-content:center; text-align:center; font-family:var(--display); font-weight:600; line-height:1.05; transition:transform .15s}
.pir-card span{font-size:26px} .pir-card small{font-size:14px} .pir-prog .pir-card{max-width:96px}
.pir-card.pir-now{transform:translateY(-6px) scale(1.12); box-shadow:0 0 0 4px var(--sun), 2px 3px 0 var(--ink)} .pir-card.pir-sel{box-shadow:0 0 0 4px var(--coral)}
.pir .c-up,.pir .c-fwd{background:#C9F2DF} .pir .c-down{background:#FFD6CF} .pir .c-left,.pir .c-tl{background:#D4ECFF} .pir .c-right,.pir .c-tr{background:#FFE3A3} .pir .c-rep{background:#F4E1FF}
.pir-fly{position:fixed; pointer-events:none; z-index:60; transform:translate(-50%,-50%)} .pir-sand{color:#C9993F}
.pir-still *, .pir-still *::after{animation:none!important; transition:none!important} @keyframes pir-ant{50%{transform:rotate(14deg)}}
@keyframes pir-bob{50%{transform:translateY(-5%)}} @keyframes pir-sway{25%{transform:rotate(-5deg)} 75%{transform:rotate(5deg)}}
@keyframes pir-wave{50%{transform:translateX(18%); opacity:.35}} @keyframes pir-blink{0%,90%,100%{transform:none} 94%{transform:scaleY(.1)}}
`);

registerGame({id:"pirate", em:"🏴‍☠️", name:"Le Robot pirate", desc:"Guide le robot jusqu'au trésor", multi:true,
  title:{en:"The Pirate Robot", de:"Der Piratenroboter", lb:"De Piratenroboter", zh:"海盗机器人"},
  sub:{en:"Find the treasure!", de:"Finde den Schatz!", lb:"Fann de Schatz!", zh:"快去找宝藏！"}}, function () {
  L();
  const lvl = levelOf("pirate"), little = S.kid === "p4", total = lvl === 1 ? 5 : 6, res = [], gen = GEN, loot = shuffle(P.treasures);
  startSession("pirate", null, total);
  const body = $("gameBody"); body.innerHTML = "";
  const root = el("div", "pir" + (TEST ? " pir-still" : "")), top = el("div", "pir-top"), parrot = el("button", "pir-parrot", "🦜");
  const bub = el("div", "pir-bubble"), psayEl = el("div", "pir-psay"), again = el("button", "speak chunky", "🔊 <span></span>");
  const bagEl = el("div", "pir-bag", "🎒"), row = el("div", "pir-row"), goalEl = el("p", "pir-goal"), board = el("div", "pir-board"), ctl = el("div", "pir-ctl");
  parrot.setAttribute("aria-label", "🦜");
  top.append(parrot, bub, psayEl); row.append(again, bagEl); root.append(top, row, goalEl, board, ctl); body.append(root);
  let k = 0, R = null, isl = null, cells = [], bot = null, bx = 0, by = 0, bd = 0, rot = 0, busy = false, tries = 0, dragged = false;
  // every text on screen is a function of the language, so the flags in the bar switch it mid-game
  const labs = [], lab = (e, f) => { labs.push([e, f]); e.innerHTML = f(); return e; };
  const relabel = () => { labs.forEach(([e, f]) => e.innerHTML = f()); if (R && R.redraw) R.redraw(); };
  lab(again.querySelector("span"), () => t(P.ui.again)); lab(bub, () => R ? R.bubble() : ""); lab(goalEl, () => R && R.goal ? R.goal() : "");
  const BASE = labs.length;
  again.onclick = () => { G.replays++; L(); relabel(); flap(); if (R) R.say(); };
  const mark = e => { root.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok); if (e) markOk(e); };
  const marks = () => { if (TEST) R && !busy ? R.mark() : mark(null); };
  const cellOf = p => cells[p.y * isl.n + p.x], here = () => cellOf({x:bx, y:by});
  const part = s => bot.querySelector(s), bodyEl = () => part(".pir-body");
  const anim = (e, frames, o) => calm() || !e ? null : e.animate(frames, typeof o === "number" ? {duration:o, easing:"ease-out"} : o);
  const shake = e => anim(e, [{transform:"none"}, {transform:"translateX(-8px) rotate(-3deg)"}, {transform:"translateX(7px) rotate(3deg)"}, {transform:"none"}], 350);

  /* ---------- the parrot, the robot and their jokes ---------- */
  const flap = () => anim(parrot, [{transform:"none"}, {transform:"rotate(-14deg) scale(1.12)"}, {transform:"rotate(10deg)"}, {transform:"rotate(-6deg) scale(1.05)"}, {transform:"none"}], 600);
  const pop = (e, ms = 1700) => anim(e, [{opacity:0, transform:"translateY(8px) scale(.6)"}, {opacity:1, transform:"none", offset:.12}, {opacity:1, offset:.85}, {opacity:0}], ms);
  const psay = text => { psayEl.textContent = text; flap(); pop(psayEl, 2000); };
  const bsay = text => { const s = part(".pir-say"); s.textContent = text; pop(s); };
  parrot.onclick = () => { const l = pick(P.parrot, 1)[0]; G.taps++; SND.squawk(); psay(t(l)); if (lang !== "lb") say(t(l), lang); };
  const oh = on => { part(".pir-oh").setAttribute("opacity", on ? 1 : 0); part(".pir-smile").setAttribute("opacity", on ? 0 : 1); };
  function gag(e){
    e.stopPropagation(); if (busy || dragged) return;
    const g = pick(P.gags, 1)[0], b = bodyEl(); G.taps++; SND[g.s](); bsay(t(g));
    if (g.s === "sneeze" || g.s === "toot") burst(["💨"], here(), 2, {spread:40, size:24});
    ({puff:() => { bot.querySelectorAll(".pir-cheek").forEach(c => anim(c, [{transform:"none"}, {transform:"scale(2.3)"}, {transform:"none"}], 650)); anim(b, [{transform:"none"}, {transform:"scale(1.16,.88)"}, {transform:"none"}], 650); },
      jump:() => anim(b, [{transform:"none"}, {transform:"translateY(-45%) scale(.88,1.16)", offset:.4}, {transform:"scale(1.22,.8)", offset:.8}, {transform:"none"}], 600),
      spin:() => anim(b, [{transform:"rotate(0)"}, {transform:"rotate(360deg) scale(1.2)"}], {duration:650, easing:"ease-in-out"}),
      wobble:() => anim(b, [{transform:"none"}, {transform:"rotate(-15deg) scale(1.06)"}, {transform:"rotate(13deg)"}, {transform:"rotate(-8deg)"}, {transform:"none"}], 700)})[g.a]();
  }
  // emoji particles from a cell: sparkles, splashes, coins, bubbles, sand
  function burst(list, cellEl, n, o = {}){
    if (calm() || !cellEl) return;
    const r = cellEl.getBoundingClientRect(), x0 = r.left + r.width / 2, y0 = r.top + r.height / 2, sp = o.spread || 80, up = o.up || 0;
    const to = (x, y) => `translate(calc(-50% + ${x}px), calc(-50% + ${y}px))`;
    for (let i = 0; i < n; i++) {
      const d = el("div", "pir-fly " + (o.cls || ""), list[i % list.length]); d.style.cssText = `left:${x0}px; top:${y0}px; font-size:${o.size || 26}px`; document.body.append(d);
      const dx = (Math.random() * 2 - 1) * sp, dy = up ? -up * (.6 + Math.random() * .6) : (Math.random() * 2 - 1) * sp;
      const frames = o.arc ? [{transform:to(0, 0) + " scale(.3)"}, {transform:to(dx / 2, dy) + " scale(1.1)", offset:.45}, {transform:to(dx, 30) + ` scale(.8) rotate(${rnd(360)}deg)`, opacity:0}]
        : [{transform:to(0, 0) + " scale(.3)", opacity:1}, {transform:to(dx, dy) + " scale(1.1)", opacity:0}];
      d.animate(frames, {duration:(o.ms || 800) + rnd(300), delay:i * (o.gap || 0), easing:"ease-out", fill:"backwards"}).onfinish = () => d.remove();
    }
  }

  /* ---------- the board and the robot's moves ---------- */
  function draw(){
    board.innerHTML = ""; board.style.setProperty("--n", isl.n); board.classList.toggle("pir-drive", lvl === 1); cells = [];
    for (let y = 0; y < isl.n; y++) for (let x = 0; x < isl.n; x++) {
      const g = at(isl, x, y), c = el("div", "pir-cell " + (g === "~" ? "pir-w" : "pir-s") + (same({x, y}, isl.start) ? " pir-start" : ""));
      if (g === "#") c.append(el("span", "pir-m", "🪨"));
      cells.push(c); board.append(c);
    }
    isl.marks.forEach(m => cellOf(m).append(el("span", "pir-m", m.m.e)));
    if (isl.key) cellOf(isl.key).append(el("span", "pir-key", "🔑"));
    bot = el("div", "pir-bot" + (isl.turn ? " pir-turn" : ""), `<div class="pir-body"><div class="pir-dir"><i></i></div>${P.robot}<span class="pir-hat">🐙</span></div><div class="pir-say"></div>`);
    board.append(bot); bodyEl().onclick = gag; home();
  }
  function put(x, y){ bx = x; by = y; bot.style.transform = `translate(${x * 100}%, ${y * 100}%)`; part(".pir-say").classList.toggle("pir-right", x >= isl.n / 2); }
  const look = dir => { part(".pir-pup").style.transform = `translate(${D[dir][0] * 3}px, ${D[dir][1] * 3}px)`; };
  function face(d, turn){ rot = turn ? rot + ((d - bd + 4) % 4 === 1 ? 90 : -90) : d * 90; bd = d; part(".pir-dir").style.transform = `rotate(${rot}deg)`; look(O[d]); }
  function home(){ put(isl.start.x, isl.start.y); face(isl.start.d || 0); bot.classList.remove("pir-haskey"); const ke = board.querySelector(".pir-key"); if (ke) { ke.getAnimations().forEach(a => a.cancel()); ke.style.opacity = ""; } }
  const popIn = () => anim(bodyEl(), [{transform:"scale(0) rotate(-90deg)"}, {transform:"scale(1.2) rotate(10deg)", offset:.7}, {transform:"none"}], 450);
  async function hop(x, y, dir, ms = 260){
    const from = bot.style.transform; put(x, y); look(dir); SND.hop();
    anim(bot, [{transform:from}, {transform:bot.style.transform}], {duration:ms, easing:"cubic-bezier(.3,1.25,.5,1)"});
    anim(bodyEl(), [{transform:"none"}, {transform:"translateY(-24%) scale(.9,1.12)", offset:.45}, {transform:"scale(1.14,.84)", offset:.8}, {transform:"none"}], ms + 40);
    await wait(ms + 20);
  }
  async function bump(dir, g){
    SND.beep(); oh(true); bsay(t(g === "#" ? P.ui.rock : P.ui.beep));
    anim(bodyEl(), [{transform:"none"}, {transform:`translate(${D[dir][0] * 22}%, ${D[dir][1] * 22}%) scale(.88,1.1)`}, {transform:"rotate(-9deg)"}, {transform:"rotate(7deg)"}, {transform:"none"}], 520);
    burst(["💫", "⭐"], here(), 4, {spread:40, up:50, size:18});
    await wait(560); oh(false);
  }
  // into the water: splash, bubbles, and back to the start with an octopus on the head
  async function splash(x, y, dir){
    const from = bot.style.transform; put(x, y); look(dir); oh(true);
    anim(bot, [{transform:from}, {transform:bot.style.transform}], 240); await wait(240);
    SND.splash(); sfx.ko();
    const sink = anim(bodyEl(), [{transform:"none", opacity:1}, {transform:"translateY(30%) scale(.5) rotate(25deg)", opacity:0}], {duration:500, fill:"forwards"});
    burst(["💦", "💧"], here(), 7, {spread:60, up:70, arc:true, size:22}); burst(["🫧"], here(), 6, {spread:25, up:130, ms:1200, gap:90, size:20});
    await wait(950); if (sink) sink.cancel();
    home(); bot.classList.add("pir-wet"); popIn(); SND.boing(); oh(false); bsay(t(P.ui.splash));
    await wait(500);
  }
  async function gotKey(){
    const ke = board.querySelector(".pir-key"); SND.key(); bsay(t(P.ui.key)); bot.classList.add("pir-haskey");
    if (ke) { anim(ke, [{transform:"none"}, {transform:"translateY(-50%) scale(1.7) rotate(20deg)", offset:.5}, {transform:"translateY(-80%) scale(.3)", opacity:0}], 500); ke.style.opacity = 0; }
    await wait(450);
  }
  async function shovel(){
    const c = here(), s = el("span", "pir-shovel", "🪏"); c.append(s); SND.dig();
    anim(s, [{transform:"none"}, {transform:"rotate(-40deg) translateY(-12%)"}, {transform:"rotate(12deg) translateY(10%)"}, {transform:"rotate(-40deg) translateY(-12%)"}, {transform:"rotate(12deg) translateY(10%)"}, {transform:"none"}], 650);
    anim(bodyEl(), [{transform:"none"}, {transform:"scale(1.08,.9)"}, {transform:"none"}, {transform:"scale(1.08,.9)"}, {transform:"none"}], 650);
    burst(["●"], c, 8, {spread:45, up:45, arc:true, size:11, cls:"pir-sand"});
    await wait(700); s.remove(); return c;
  }
  async function hole(){ const c = await shovel(); c.append(el("span", "pir-hole", "🕳️")); SND.wah(); sfx.ko(); oh(true); shake(bodyEl()); psay(t(P.ui.notHere)); await talk(t(P.ui.notHere), []); await wait(300); oh(false); }
  function toBag(e, from){
    const s = el("span", "", e);
    if (calm()) return bagEl.append(s);
    const a = from.getBoundingClientRect(), b = bagEl.getBoundingClientRect(), x0 = a.left + a.width / 2, y0 = a.top + a.height / 2;
    const d = el("div", "pir-fly", e); d.style.cssText = `left:${x0}px; top:${y0}px; font-size:44px`; document.body.append(d);
    d.animate([{transform:"translate(-50%,-50%) scale(.2)"}, {transform:"translate(-50%,-170%) scale(1.8) rotate(-10deg)", offset:.35},
      {transform:`translate(calc(-50% + ${b.right - 16 - x0}px), calc(-50% + ${b.top + b.height / 2 - y0}px)) scale(.6)`}], {duration:1100, easing:"ease-in-out"})
      .onfinish = () => { d.remove(); bagEl.append(s); anim(s, [{transform:"scale(0)"}, {transform:"scale(1.6)"}, {transform:"none"}], 350); };
  }
  async function treasure(){
    const c = await shovel(); if (!alive(gen)) return null;
    let chest = c.querySelector(".pir-chest");
    if (!chest) { chest = el("span", "pir-chest", P.chest); c.append(chest); anim(chest, [{transform:"translateY(40%) scale(0)"}, {transform:"translateY(-20%) scale(1.3)", offset:.6}, {transform:"none"}], 450); await wait(420); }
    chest.querySelector(".pir-lid").classList.add("pir-open"); SND.coins(); sfx.ok();
    burst(["🪙", "✨", "💰"], c, 12, {spread:110, up:150, arc:true, gap:25, ms:900});
    const item = loot[k % loot.length]; toBag(item.e, c); psay(t(P.ui.cheer));
    anim(bodyEl(), [{transform:"none"}, {transform:"translateY(-40%) rotate(-10deg)"}, {transform:"none"}, {transform:"translateY(-25%) rotate(10deg)"}, {transform:"none"}], 800);
    await wait(900); return item;
  }
  // the treasure is found: stars, the round is logged, next island
  async function win(n, pre, lbw){
    busy = true; mark(null);
    const said = pre ? speak(pre, lbw) : null, item = await treasure();
    if (said) await Promise.race([said, wait(3000)]);
    if (!item || !alive(gen)) return;
    const first = tries === 0; if (first) addStar();
    if (n && n <= R.min) { addStar(); psay(t(P.ui.bonus, n)); }
    logRound(R.key, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
    await talk(t(P.ui.found, item), []); if (!alive(gen)) return;
    await wait(700); k++; next();
  }
  const card = c => (little ? `<span>${P.cards[c].i}</span>` : "") + `<small>${t(P.cards[c])}</small>`; // the big one reads, no arrows

  /* ---------- level 1: drive with the finger (a touch on any cell moves the robot one step towards it) ---------- */
  function drive(){
    const r = P.isle.drive(), tgt = r.tgt.m; isl = r.isl; draw();
    let dragging = false, seen = null;
    const toward = async (i, drag) => {
      if (busy) return;
      const dx = i % isl.n - bx, dy = Math.floor(i / isl.n) - by; if (!dx && !dy) return;
      const dir = Math.abs(dx) >= Math.abs(dy) ? (dx > 0 ? "right" : "left") : (dy > 0 ? "down" : "up"), nx = bx + D[dir][0], ny = by + D[dir][1], g = at(isl, nx, ny);
      G.taps++; if (drag) dragged = true; busy = true;
      if (g !== ".") await bump(dir, g); else await hop(nx, ny, dir, 190);
      busy = false; if (!alive(gen)) return;
      const m = markAt(isl, bx, by);
      if (m === r.tgt) return win(0, t(P.here, tgt), [tgt.lb]);
      if (m && m !== seen) { tries++; sfx.ko(); oh(true); shake(bodyEl()); psay(t(P.notThis, m.m, tgt)); speak(t(P.notThis, m.m, tgt), [m.m.lb, tgt.lb]); loops.push(setTimeout(() => oh(false), 900)); }
      seen = m; marks();
    };
    board.onpointerdown = () => { dragging = true; dragged = false; };
    board.onpointermove = e => { if (!dragging) return; const c = document.elementFromPoint(e.clientX, e.clientY), i = cells.indexOf(c && c.closest(".pir-cell")); if (i >= 0) toward(i, true); };
    board.onpointerup = board.onpointercancel = () => { dragging = false; };
    cells.forEach((c, i) => c.onclick = () => { if (!dragged) toward(i, false); });
    return {key:r.key,
      bubble:() => `<span class="pir-pic">🤖 ➜ ${tgt.e}</span><b>${t(P.drive, tgt)}</b>`, // always written, the little one included (Kezhan: a chance to read)
      say:() => speak(t(P.drive, tgt), [tgt.lb]),
      mark:() => { const w = path(isl, {x:bx, y:by}, r.tgt, p => at(isl, p.x, p.y) === "." && (!markAt(isl, p.x, p.y) || same(p, r.tgt))); mark(w && w.length ? cellOf(w[0]) : null); }};
  }

  /* ---------- level 2: the parrot dictates, arrows then dig; footprints show the way ---------- */
  function listen(){
    const r = P.isle.listen(k < 3 ? 2 : 3); isl = r.isl; draw();
    const sp = () => lang === "zh" ? "" : " "; // Chinese puts no space between sentences
    const text = () => r.plan.map(([d, n]) => t(P.step, d, n)).join(sp()) + sp() + t(P.dig), pic = r.plan.map(([d, n]) => P.cards[d].i.repeat(n)).join(" ") + " 🪏";
    const pad = {}, padEl = el("div", "pir-pad"), digBtn = el("button", "bigbtn chunky");
    const R2 = {key:r.key, hint:false,
      // the words stay hidden to be remembered, until a miss (in Luxembourgish, without a voice, always written)
      bubble:() => R2.hint || lang === "lb" ? `<span class="pir-pic">${pic}</span><b>${text()}</b>` : `<span class="pir-pic">🦜 👂</span><b>${t(P.listen)}</b>`,
      say:() => lang === "lb" ? (SND.squawk(), Promise.resolve()) : say(text(), lang),
      mark:() => { const w = path(isl, {x:bx, y:by}, isl.goal, p => at(isl, p.x, p.y) === "."); mark(!w ? null : w.length ? pad[O.find(d => bx + D[d][0] === w[0].x && by + D[d][1] === w[0].y)] : digBtn); }};
    const miss = () => { tries++; R2.hint = true; relabel(); };
    ["up", "left", "down", "right"].forEach(d => {
      const b = el("button", "pir-arrow chunky a-" + d); lab(b, () => `<span>${P.cards[d].i}</span><small>${t(P.cards[d])}</small>`); pad[d] = b; padEl.append(b);
      b.onclick = async () => {
        if (busy) return; G.taps++; busy = true;
        const nx = bx + D[d][0], ny = by + D[d][1], g = at(isl, nx, ny);
        if (lang !== "lb") say(t(P.cards[d]), lang);
        if (g === "~") { await splash(nx, ny, d); root.querySelectorAll(".pir-trace").forEach(s => s.remove()); miss(); }
        else if (g !== ".") await bump(d, g);
        else { here().append(el("span", "pir-trace", "👣")); await hop(nx, ny, d); }
        if (!alive(gen)) return;
        busy = false; marks();
      };
    });
    lab(digBtn, () => "🪏 " + t(P.dig));
    digBtn.onclick = async () => {
      if (busy) return; G.taps++;
      if (same({x:bx, y:by}, isl.goal)) return win(0);
      busy = true; miss(); await hole(); await wait(700); if (!alive(gen)) return;
      root.querySelectorAll(".pir-trace").forEach(s => s.classList.add("pir-old")); home(); popIn(); busy = false; marks();
    };
    ctl.append(padEl, digBtn);
    return R2;
  }

  /* ---------- levels 3 and 4: a clue, then a program of cards ---------- */
  function program(r){
    isl = r.isl; draw();
    const set = isl.turn ? ["fwd", "tl", "tr", "rep"] : ["up", "down", "left", "right", "rep"], pal = el("div", "pir-pal"), bar = el("div", "pir-prog"), run = el("div", "pir-row");
    const trash = el("button", "pir-trash chunky", "🗑️"), go = el("button", "bigbtn chunky"), pals = {};
    const R3 = {key:r.key, min:r.sol.length, prog:[],
      bubble:() => (little || lang === "lb" ? `<span class="pir-pic">${picto(r)}</span>` : "") + `<b>${clueText(r)}</b>` + (isl.turn ? `<small>${t(P.compass)}</small>` : ""),
      goal:() => `🃏 ${t(P.ui.challenge, r.sol.length)}`,
      say:() => speak(clueText(r), r.b ? [r.a.lb, r.b.lb] : [r.a.lb]),
      redraw:() => drawBar(),
      mark:() => { const p = R3.prog, bad = p.findIndex((c, j) => c !== r.sol[j]); mark(bad >= 0 ? bar.children[bad] : p.length < r.sol.length ? pals[r.sol[p.length]] : go); }};
    function drawBar(popLast){
      bar.innerHTML = "";
      R3.prog.forEach((c, j) => {
        const b = el("button", "pir-card c-" + c, card(c)); bar.append(b);
        b.onclick = () => { if (busy) return; G.taps++; R3.prog.splice(j, 1); drawBar(); SND.pop(); marks(); }; // a card touched in the bar goes away
        if (popLast && j === R3.prog.length - 1) anim(b, [{transform:"scale(0) rotate(-20deg)"}, {transform:"scale(1.2) rotate(6deg)"}, {transform:"none"}], 300);
      });
    }
    set.forEach(c => {
      const b = el("button", "pir-card c-" + c); lab(b, () => card(c)); pals[c] = b; pal.append(b);
      b.onclick = () => {
        if (busy) return; G.taps++;
        if (R3.prog.length >= 10) { shake(bar); SND.beep(); return; }
        R3.prog.push(c); drawBar(true); SND.pop(); if (lang !== "lb") say(t(P.cards[c]), lang); marks();
      };
    });
    trash.onclick = () => { if (busy) return; G.taps++; R3.prog = []; drawBar(); SND.toot(); marks(); };
    lab(go, () => "▶ " + t(P.ui.go));
    go.onclick = async () => {
      if (busy) return; if (!R3.prog.length) return shake(go);
      G.taps++; busy = true; mark(null);
      const out = sim(isl, R3.prog); if (!await play(out.ev, bar)) return;
      if (!out.wet && same(out.s, isl.goal) && out.s.k) return win(R3.prog.length);
      tries++;
      if (!out.wet) { // the program stays, to be corrected
        if (same(out.s, isl.goal)) { oh(true); shake(bodyEl()); SND.wah(); psay(t(P.ui.locked)); await talk(t(P.ui.locked), [t(P.ui.locked)]); }
        else await hole();
        await wait(600); if (!alive(gen)) return;
        home(); popIn();
      }
      if (tries >= (little ? 1 : 2) && !cellOf(isl.goal).querySelector(".pir-x")) cellOf(isl.goal).append(el("span", "pir-x", "❌"));
      busy = false; marks();
    };
    run.append(trash, go); ctl.append(pal, bar, run);
    return R3;
  }
  // run a program: the card in progress lights up and is said
  async function play(ev, bar){
    const els = [...bar.children];
    for (const e of ev) {
      if (!alive(gen)) return false;
      if ("j" in e) { els.forEach((b, j) => b.classList.toggle("pir-now", j === e.j)); SND.pop(); await Promise.all([lang !== "lb" ? talk(t(P.cards[e.c]), [], 1100) : null, wait(300)]); continue; }
      if ("turn" in e) { face(e.turn, true); SND.hop(); anim(bodyEl(), [{transform:"none"}, {transform:`rotate(${e.c === "tl" ? -12 : 12}deg) scale(1.06)`}, {transform:"none"}], 320); await wait(380); continue; }
      if (e.bump) { await bump(e.dir, e.bump); continue; }
      if (e.key) { await gotKey(); continue; }
      if (e.wet) { await splash(e.x, e.y, e.dir); continue; }
      await hop(e.x, e.y, e.dir, 320);
    }
    els.forEach(b => b.classList.remove("pir-now"));
    return alive(gen);
  }

  /* ---------- level 4, every other island: where will it stop, then fix the wrong card ---------- */
  function debug(r){
    isl = r.isl; draw(); cellOf(isl.goal).append(el("span", "pir-chest", P.chest));
    const bar = el("div", "pir-prog"), chooser = el("div", "pir-row");
    const R4 = {key:r.key, phase:"predict", prog:r.bug,
      bubble:() => (little ? `<span class="pir-pic">${R4.phase === "predict" ? "🤖 ❓ 📍" : "🃏 ❓"}</span>` : "") + `<b>${t(R4.phase === "predict" ? P.ui.where : P.ui.wrongCard)}</b>`,
      say:() => speak(t(R4.phase === "predict" ? P.ui.where : P.ui.wrongCard), []),
      redraw:() => drawBar(R4.prog),
      mark:() => mark(R4.phase === "predict" ? cellOf(r.end) : chooser.children.length ? [...chooser.children].find(b => b.dataset.c === r.sol[r.j]) : bar.children[r.j])};
    function drawBar(prog){
      bar.innerHTML = "";
      prog.forEach((c, j) => { const b = el("button", "pir-card c-" + c, card(c)); b.onclick = () => choose(j); bar.append(b); });
    }
    cells.forEach((c, i) => c.onclick = async () => {
      if (busy || R4.phase !== "predict") return;
      busy = true; G.taps++;
      const ok = same({x:i % isl.n, y:Math.floor(i / isl.n)}, r.end), pin = el("span", "pir-pin", "📍"); c.append(pin);
      anim(pin, [{transform:"translateY(-120%)", opacity:0}, {transform:"none", opacity:1}], 300);
      if (ok) sfx.ok(); else { tries++; sfx.ko(); }
      if (!await play(sim(isl, r.bug).ev, bar)) return;
      if (ok) burst(["✨", "⭐"], here(), 8);
      psay(t(ok ? P.ui.yes : P.ui.look)); await talk(t(ok ? P.ui.yes : P.ui.look), []);
      await wait(700); if (!alive(gen)) return;
      pin.remove(); home(); popIn(); R4.phase = "fix"; relabel(); R4.say(); busy = false; marks();
    });
    function choose(j){
      if (busy || R4.phase !== "fix") return; G.taps++; SND.pop();
      [...bar.children].forEach((b, x) => b.classList.toggle("pir-sel", x === j));
      chooser.innerHTML = ""; psay(t(P.ui.which)); if (lang !== "lb") say(t(P.ui.which), lang);
      ["fwd", "tl", "tr", "rep"].filter(c => c !== R4.prog[j]).forEach(c => {
        const b = el("button", "pir-card c-" + c, card(c)); b.dataset.c = c; chooser.append(b);
        anim(b, [{transform:"scale(0)"}, {transform:"scale(1.15)"}, {transform:"none"}], 250);
        b.onclick = () => fix(j, c);
      });
      marks();
    }
    async function fix(j, c){
      if (busy) return; busy = true; G.taps++; chooser.innerHTML = "";
      const prog = R4.prog.slice(); prog[j] = c; drawBar(prog);
      const out = sim(isl, prog); if (!await play(out.ev, bar)) return;
      if (!out.wet && same(out.s, isl.goal) && out.s.k) return win(0);
      tries++; if (!out.wet) await hole();
      await wait(600); if (!alive(gen)) return;
      home(); popIn(); drawBar(R4.prog); busy = false; marks(); // back to the program with its one wrong card
    }
    drawBar(R4.prog); ctl.append(bar, chooser);
    return R4;
  }

  function next(){
    if (!alive(gen)) return;
    if (k >= total) return finish();
    renderDots(res, total, k); tries = 0; busy = false; dragged = false; labs.length = BASE; ctl.innerHTML = "";
    board.onpointerdown = board.onpointermove = board.onpointerup = board.onpointercancel = null;
    R = lvl === 1 ? drive() : lvl === 2 ? listen() : lvl === 3 ? program(P.isle.clue(["left", "right", "above", "below", "between"], false))
      : k % 2 ? debug(P.isle.debug()) : program(P.isle.clue(["north", "south", "east", "west"], true));
    relabel(); flap(); R.say(); marks();
  }
  next();
});
})();
