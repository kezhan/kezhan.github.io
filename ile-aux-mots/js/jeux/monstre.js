/* L'Île aux Mots : jeu « monstre », le Monstre affamé (ticket 2026-09-27_jeu-monstre-affame).
   A funny monster asks for food: drag it (or touch it) to its mouth. It chews, puffs its cheeks, burps and sends hearts;
   the wrong food is spat out with a "yuck". Touch the monster too: it giggles, its nose honks, its belly goes boing.
   1: one food out of three · 2: foods drawn in colours ("the red apple") · 3: two foods in order · 4: count them, the belly fills (the big one reads).
   English, German, Luxembourgish or Chinese, never French. Texts, grammar, drawings, sounds and style: js/contenus/monstre.js. */
(() => {
const TX = MONSTRE_TXT, LEARNT = ["en", "de", "lb", "zh"];
const L = () => LEARNT.includes(langOf()) ? langOf() : "en";
const food = en => THEMES.food.words.find(w => w.en === en);
const colour = en => THEMES.colors.words.find(w => w.en === en);
const one = a => a[rnd(a.length)];
const calm = () => typeof fx === "undefined" || fx.calm();
const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));
const go = (n, kf, o) => calm() ? null : n.animate(kf, typeof o === "number" ? {duration:o, easing:"ease-in-out"} : o);
const kf = (prop, vals) => vals.map(v => ({transform:`${prop}(${v})`}));
const {the, coloured, counted, andJoin, two} = MONSTRE_GRAM, SND = MONSTRE_SND;

/* ---------- flying things: crumbs, hearts, the food itself ---------- */
function bits(txt, at, n, size, up){
  if (calm()) return;
  for (let k = 0; k < n; k++) {
    const d = el("div", "mo-fly", txt); d.style.cssText = `left:${at.x}px; top:${at.y}px; width:${size}px; font-size:${size}px`;
    document.body.append(d);
    const a = up ? -Math.PI / 2 + (Math.random() - .5) * 1.4 : Math.random() * Math.PI * 2, r = 50 + Math.random() * 60;
    d.animate([{transform:"translate(-50%,-50%) scale(.4)", opacity:1},
      {transform:`translate(calc(-50% + ${Math.cos(a) * r}px), calc(-50% + ${Math.sin(a) * r + (up ? -20 : 50)}px)) scale(1) rotate(${rnd(360) - 180}deg)`, opacity:0}],
      {duration:750 + rnd(450), easing:"cubic-bezier(.2,.7,.4,1)"}).onfinish = () => d.remove();
  }
}
// along an arc from a to b, spinning and shrinking
function fly(html, a, b, o = {}){
  if (calm()) return Promise.resolve();
  const {spin = 0, from = 1, to = .3, arc = -70, dur = 430} = o, dx = b.x - a.x, dy = b.y - a.y;
  const d = el("div", "mo-fly", html); d.style.left = a.x + "px"; d.style.top = a.y + "px";
  document.body.append(d);
  const T = (x, y, s, r) => `translate(calc(-50% + ${x}px), calc(-50% + ${y}px)) scale(${s}) rotate(${r}deg)`;
  return new Promise(res => {
    const fin = () => { d.remove(); res(); };
    d.animate([{transform:T(0, 0, from, 0)}, {transform:T(dx / 2, dy / 2 + arc, (from + to) / 2 + .15, spin / 2), offset:.5}, {transform:T(dx, dy, to, spin)}],
      {duration:dur, easing:"ease-in-out"}).onfinish = fin;
    loops.push(setTimeout(fin, dur + 150));
  });
}

/* ---------- the monster: each part moves on its own ---------- */
function monster(stage){
  stage.innerHTML = `<div class="mo-box">${MONSTRE_BODY(one(MONSTRE_SKINS))}<div class="mo-tummy"></div><div class="mo-pop" hidden></div></div>`;
  const box = stage.firstChild, q = s => box.querySelector(s), qa = s => [...box.querySelectorAll(s)];
  const mouth = q(".mo-mouth"), all = q(".mo-all"), sick = q(".mo-sick"), belly = q(".mo-belly"), nose = q(".mo-nose"), tongue = q(".mo-tongue"), pop = q(".mo-pop");
  const eyes = qa(".mo-eye"), pupils = qa(".mo-pupil"), cheeks = qa(".mo-cheek"), brows = qa(".mo-brow"), arms = qa(".mo-arm");
  const mouthAt = () => { const r = mouth.getBoundingClientRect(); return {x:r.left + r.width / 2, y:r.top + r.height / 2}; };
  const puff = (s, d) => cheeks.forEach(c => go(c, kf("scale", [1, s, 1.2, s, 1]), d));
  const squint = d => eyes.forEach(e => go(e, [{transform:"scaleY(1)"}, {transform:"scaleY(.3)", offset:.2}, {transform:"scaleY(.3)", offset:.8}, {transform:"scaleY(1)"}], d));
  const wobble = (deg, d) => go(all, kf("rotate", [0, -deg, deg, -deg, deg * .6, 0].map(r => r + "deg")), d);
  const glow = (op, d) => go(sick, [{opacity:0}, {opacity:op, offset:.2}, {opacity:op, offset:.8}, {opacity:0}], d);
  let popT = 0, eyeT = 0;
  const pupilsBack = ms => { clearTimeout(eyeT); eyeT = setTimeout(() => pupils.forEach(p => p.removeAttribute("transform")), ms); loops.push(eyeT); };
  const pupilsAt = (a, b) => { pupils.forEach((p, k) => p.setAttribute("transform", `translate(${k ? b : a})`)); pupilsBack(900); };
  const M = {
    box, mouth: mouthAt,
    open(s = .5){ mouth.style.transform = `scaleY(${s})`; },
    // the pupils follow the finger, then look ahead again
    look(x, y){
      pupils.forEach((p, k) => {
        const r = eyes[k].getBoundingClientRect(), dx = x - r.left - r.width / 2, dy = y - r.top - r.height / 2, d = Math.hypot(dx, dy) || 1, m = Math.min(9, d / 10);
        p.setAttribute("transform", `translate(${(dx / d * m).toFixed(1)} ${(dy / d * m).toFixed(1)})`);
      });
      pupilsBack(1500);
    },
    say(t){
      pop.textContent = t; pop.hidden = false;
      go(pop, [{transform:"translateX(-50%) scale(.2)", opacity:0}, {transform:"translateX(-50%) scale(1.1)", opacity:1, offset:.6}, {transform:"translateX(-50%) scale(1)", opacity:1}], {duration:260, easing:"ease-out"});
      clearTimeout(popT); popT = setTimeout(() => pop.hidden = true, 2200); loops.push(popT);
    },
    hungry(){
      go(mouth, kf("scaleY", [.5, 1.15, .5]), 650); go(tongue, [{transform:"none"}, {transform:"translateY(9px) scaleX(1.25)"}, {transform:"none"}], 650);
      go(all, [{transform:"none"}, {transform:"translateY(-14px) scale(.95,1.06)"}, {transform:"scale(1.05,.95)"}, {transform:"none"}], 550);
    },
    chew(html){
      SND.munch(); loops.push(setTimeout(SND.gulp, TEST ? 0 : 640));
      go(mouth, kf("scaleY", [.12, .7, .12, .7, .12, .5]), 800); puff(1.6, 800);
      go(all, kf("scale", ["1", "1.06,.94", ".98,1.03", "1.06,.94", "1"]), 800);
      bits(html, mouthAt(), 5, 14);
      M.open(); return wait(850);
    },
    gulp(html){ SND.gulp(); go(mouth, kf("scaleY", [.12, .7, .5]), 300); puff(1.4, 300); bits(html, mouthAt(), 3, 12); },
    happy(){
      SND.heart(); squint(900);
      const r = box.getBoundingClientRect();
      bits("💖", {x:r.left + r.width / 2, y:r.top + r.height * .2}, 5, 26, true);
      arms.forEach((a, k) => go(a, kf("rotate", [0, 1, 0, 1, 0].map(u => u * (k ? -35 : 35) + "deg")), 800));
      go(all, [{transform:"none"}, {transform:"translateY(-22px) scale(.95,1.06)"}, {transform:"scale(1.06,.94)"}, {transform:"none"}], {duration:650, easing:"ease-out"});
    },
    burp(txt, big){
      SND.burp();
      go(mouth, [{transform:"scaleY(.3)"}, {transform:"scaleY(1.25)", offset:.3}, {transform:"scaleY(1.1)", offset:.8}, {transform:"scaleY(.5)"}], 900);
      go(all, [{transform:"none"}, {transform:`scale(${big ? 1.15 : 1.08},${big ? .87 : .93})`, offset:.25}, {transform:"scale(.96,1.06)", offset:.6}, {transform:"none"}], 900);
      const d = el("div", "mo-burp", txt); box.append(d);
      go(d, [{transform:"translate(-50%,0) scale(.3)", opacity:0}, {transform:"translate(-50%,-30px) scale(1.3)", opacity:1, offset:.35}, {transform:"translate(-50%,-80px) scale(1.5)", opacity:0}], 1300);
      loops.push(setTimeout(() => d.remove(), calm() ? 900 : 1300));
      bits("💨", mouthAt(), big ? 6 : 3, 24);
    },
    // green face, angry brows, the tongue out
    yuck(){
      SND.yuck(); puff(1.5, 500); wobble(8, 800); glow(.55, 1400);
      brows.forEach((b, k) => { const t = `translateY(5px) rotate(${k ? -16 : 16}deg)`; go(b, [{transform:"none"}, {transform:t, offset:.15}, {transform:t, offset:.8}, {transform:"none"}], 1400); });
      go(mouth, [{transform:"scaleY(.12)"}, {transform:"scaleY(.12)", offset:.45}, {transform:"scaleY(1)", offset:.6}, {transform:"scaleY(.5)"}], 1000);
      go(tongue, [{transform:"none"}, {transform:"none", offset:.5}, {transform:"translateY(14px) scaleY(1.4)", offset:.65}, {transform:"translateY(14px) scaleY(1.4)", offset:.9}, {transform:"none"}], 1000);
    },
    spray(){ SND.spit(); bits("💦", mouthAt(), 5, 18); },
    tickle(){ SND.giggle(); squint(700); puff(1.3, 600); go(all, [0, -7, 7, -7, 7, 0].map(r => ({transform:`rotate(${r}deg) scale(${1 + Math.abs(r) / 90})`})), 600); },
    // the nose honks and the eyes cross
    honk(){ SND.honk(); go(nose, kf("scale", ["1", "1.6,.6", ".8,1.3", "1"]), 350); pupilsAt("7 4", "-7 4"); },
    boing(){ SND.boing(); go(belly, kf("scale", ["1", "1.3,.75", ".88,1.2", "1.08,.94", "1"]), 650); go(all, kf("scale", ["1", "1.06,.94", ".97,1.03", "1"]), 500); },
    rumble(){ SND.rumble(); go(belly, [0, 1, -1, 1, -1, 0].map(k => ({transform:`translateX(${k * 6}px) scale(${1 + Math.abs(k) * .08})`})), 900); },
    ache(){ SND.rumble(); wobble(6, 900); puff(1.6, 900); pupilsAt("-6 6", "6 -6"); glow(.6, 1800); },
    // a big number rises from the belly: 3, "three"
    count(k, word){
      const d = el("div", "mo-num", `${k}<small>${word}</small>`); box.append(d);
      go(d, [{transform:"translateY(20px) scale(.3)", opacity:0}, {transform:"translateY(0) scale(1.2)", opacity:1, offset:.3}, {transform:"translateY(-40px) scale(1)", opacity:0}], 1100);
      loops.push(setTimeout(() => d.remove(), calm() ? 700 : 1100));
    },
    fat(k){ q(".mo-fat").style.transform = `scale(${k})`; },
    quiet(){ pop.hidden = true; },
    tummy(list){ q(".mo-tummy").innerHTML = list.join(""); }
  };
  return M;
}

addStyle(MONSTRE_CSS);

registerGame({id:"monstre", em:"😋", name:"Le Monstre affamé", desc:"Donne à manger au monstre", multi:true, title:TX.title, sub:TX.sub}, function () {
  const lvl = levelOf("monstre"), p4 = S.kid === "p4", reading = lvl === 4 && !p4;
  const total = [6, 6, 5, 5][lvl - 1], res = [];
  startSession("monstre", "food", total); const gen = GEN;
  const body = $("gameBody"); body.innerHTML = "";
  const root = el("div", "mo" + (calm() ? " mo-calm" : ""));
  const bar = el("div", "row"), bubble = el("div", "mo-bubble"), stage = el("div", "mo-stage"), tray = el("div", "mo-tray");
  const again = el("button", "speak chunky", "🔊 <span></span>");
  bar.style.justifyContent = "center"; bar.append(again);
  root.append(bar, bubble, stage, tray); body.append(root);
  const M = monster(stage);
  let i = 0, R = null, done = null, lastLine = 0;
  const live = r => alive(gen) && R === r;
  // Luxembourgish has no voice: play the lod.lu recording of each lexicon word instead
  async function talk(text, lbw){
    const lang = L();
    if (lang !== "lb") return say(text, lang);
    for (const w of lbw || []) { if (!alive(gen)) return; await say(w, "lb"); }
  }

  /* ---------- rounds ---------- */
  const wordItem = w => ({key:w.en, html:w.e, name:(lang, nom) => the(w, lang, nom), label:lang => w[lang], lbw:[w.lb]});
  const pairs = MONSTRE_COLOR_FOODS.flatMap(g => g.cols.map(c => ({g, f:food(g.en), c:colour(c)})));
  const firsts = pick(wordsOf("food", 1), total), dishes = pick(MONSTRE_COLOR_FOODS, total);
  function build(k){
    const t = rnd(p4 && lvl === 1 ? 2 : TX.ask.en.length);
    if (lvl === 1) {
      const w = firsts[k], items = shuffle([w, ...pick(wordsOf("food", 1).filter(x => x !== w), 2)].map(wordItem)), it = items.find(x => x.key === w.en);
      return {items, queue:[it], key:w.en, lbw:it.lbw, ask:lang => TX.ask[lang][t](the(w, lang))};
    }
    if (lvl === 2) {
      const tp = one(pairs.filter(p => p.g === dishes[k])), n = p4 ? 4 : 5;
      // the same food in other colours and other foods in the same colour: both words count
      const opts = [tp, ...shuffle([...pick(pairs.filter(p => p.g === tp.g && p !== tp), 2), ...pick(pairs.filter(p => p.c === tp.c && p.g !== tp.g), 2)]).slice(0, n - 1)];
      const rest = shuffle(pairs.filter(p => !opts.includes(p)));
      while (opts.length < n) opts.push(rest.pop());
      const items = shuffle(opts.map(p => ({key:`${p.c.en} ${p.f.en}`, html:MONSTRE_SVG[p.g.svg](p.c.e), lbw:[p.c.lb, p.f.lb],
        name:(lang, nom) => coloured(p.f, p.c, p.g, lang, nom), label:lang => lang === "en" ? `${p.c.en} ${p.f.en}` : coloured(p.f, p.c, p.g, lang, true)})));
      const it = items.find(x => x.key === `${tp.c.en} ${tp.f.en}`);
      return {items, queue:[it], key:it.key, lbw:it.lbw, ask:lang => TX.ask[lang][t](it.name(lang))};
    }
    if (lvl === 3) {
      const ws = pick(wordsOf("food", 3), p4 ? 4 : 6), items = shuffle(ws.map(wordItem)), o = rnd(2);
      const [a, b] = ws.slice(0, 2).map(w => items.find(x => x.key === w.en));
      return {items, queue:[a, b], key:`${a.key} > ${b.key}`, lbw:[...a.lbw, ...b.lbw], ask:lang => TX.order[lang][o](a.name(lang), b.name(lang))};
    }
    // level 4: how many? the big one may get two kinds at once
    const cs = pick(MONSTRE_COUNT, 4), counts = !p4 && rnd(2) ? [2 + rnd(4), 2 + rnd(3)] : [2 + rnd(p4 ? 4 : 8)];
    const items = shuffle(cs.map(c => Object.assign(wordItem(food(c.w)), {c})));
    const groups = counts.map((n, j) => ({item:items.find(x => x.c === cs[j]), n, got:0}));
    return {items, groups, eaten:[], key:groups.map(g => `${g.n} ${g.item.c.pl}`).join(" + "), lbw:groups.map(g => g.item.lbw[0]),
      ask:lang => TX.ask[lang][t](andJoin(groups.map(g => counted(g.n, g.item.c, lang)), lang))};
  }
  function next(){
    if (!alive(gen)) return;
    if (i >= total) return end();
    R = Object.assign(build(i), {tries:0, busy:false, btn:new Map()});
    renderDots(res, total, i);
    tray.innerHTML = ""; tray.style.maxWidth = R.items.length > 4 ? "250px" : "";
    R.items.forEach((it, k) => {
      const b = el("button", "mo-food", `${it.html}<span class="w"></span>`);
      b.setAttribute("aria-label", it.label(L())); R.btn.set(it, b); grab(b, it); tray.append(b);
      go(b, [{transform:"scale(0) rotate(-25deg)", opacity:0}, {transform:"scale(1.15) rotate(6deg)", opacity:1, offset:.6}, {transform:"none", opacity:1}], {duration:420, delay:k * 70, easing:"ease-out", fill:"backwards"});
    });
    done = null;
    if (R.groups) { done = el("button", "mo-done chunky", "✋ <span></span>"); done.onclick = count; tray.append(done); }
    M.fat(R.groups ? 1 : 1 + i * .03); M.tummy([]); M.quiet(); M.open(); M.hungry();
    ask();
  }
  // the monster's words in the language chosen now (the flags can change it mid-game)
  function paint(){
    const lang = L(), text = R.ask(lang);
    bubble.innerHTML = ""; bubble.append(el("b", "", text)); // always written, the little one included (Kezhan: a chance to read)
    again.querySelector("span").textContent = reading ? "" : TX.again[lang];
    if (done) done.querySelector("span").textContent = TX.done[lang];
    R.items.forEach(it => { const b = R.btn.get(it); if (b.dataset.named) b.querySelector(".w").textContent = it.label(lang); });
    return text;
  }
  function ask(){ const text = paint(); marks(); if (!reading) talk(text, R.lbw); idle(); }
  again.onclick = e => { if (!R) return; if (reading && e.isTrusted) G.hints++; else G.replays++; talk(paint(), R.lbw); M.hungry(); };
  // #test: only the food (or the Done button) that moves the game on
  function marks(){
    if (!TEST || !R) return;
    root.querySelectorAll("[data-ok]").forEach(b => delete b.dataset.ok);
    const g = R.groups && R.groups.find(x => x.got < x.n), need = R.groups ? g && g.item : R.queue[0];
    markOk(need ? R.btn.get(need) : done);
  }
  // nothing happens for a while: the belly rumbles and the monster asks again
  function idle(){
    const r = R; clearTimeout(r.idle);
    if (TEST || reading) return;
    r.idle = setTimeout(() => { if (!live(r) || r.busy) return; const lang = L(); M.rumble(); M.say(TX.hungry[lang]); talk(two(TX.hungry[lang], r.ask(lang), lang), r.lbw); }, 10000);
    loops.push(r.idle);
  }

  /* ---------- touch or drag a food to the mouth ---------- */
  const centre = b => { const r = b.getBoundingClientRect(); return {x:r.left + r.width / 2, y:r.top + r.height / 2}; };
  function grab(b, it){
    let st = null, dragEnd = 0;
    b.addEventListener("pointerdown", e => {
      if (!R || R.busy) return;
      b.classList.remove("bob"); M.open(.85); SND.pop();
      st = {id:e.pointerId, x:e.clientX, y:e.clientY, moved:false};
      try { b.setPointerCapture(e.pointerId); } catch(err) {}
    });
    b.addEventListener("pointermove", e => {
      if (!st || e.pointerId !== st.id) return;
      const dx = e.clientX - st.x, dy = e.clientY - st.y;
      if (!st.moved && Math.hypot(dx, dy) < 10) return;
      st.moved = true; b.classList.add("mo-drag");
      b.style.transform = `translate(${dx}px,${dy}px) scale(1.15) rotate(${Math.max(-20, Math.min(20, dx / 6))}deg)`;
      const m = M.mouth(); M.look(e.clientX, e.clientY); M.open(Math.hypot(e.clientX - m.x, e.clientY - m.y) < 150 ? 1.2 : .85);
    });
    const up = cancel => e => {
      if (!st || e.pointerId !== st.id) return;
      const moved = st.moved; st = null; b.classList.remove("mo-drag");
      if (!moved) return;
      dragEnd = performance.now();
      const r = M.box.getBoundingClientRect(), from = b.style.transform;
      b.style.transform = "";
      if (!cancel && e.clientX > r.left - 20 && e.clientX < r.right + 20 && e.clientY > r.top - 20 && e.clientY < r.bottom + 30) return feed(it, {x:e.clientX, y:e.clientY});
      go(b, [{transform:from}, {transform:"none"}], {duration:420, easing:"cubic-bezier(.3,1.7,.5,1)"}); SND.boing(); M.open();
    };
    b.addEventListener("pointerup", up(false)); b.addEventListener("pointercancel", up(true));
    b.addEventListener("click", () => { if (performance.now() - dragEnd > 400) feed(it, centre(b)); });
  }
  // the wrong food: green face, then it flies back to its place and shows its name
  async function spit(r, it, b, line, lbw){
    r.tries++; sfx.ko(); M.yuck(); M.say(line);
    await wait(450); if (!live(r)) return false;
    M.spray(); await fly(it.html, M.mouth(), centre(b), {spin:-540, from:.35, to:1, arc:-110, dur:520});
    if (!live(r)) return false;
    b.style.opacity = ""; b.dataset.named = "1"; b.querySelector(".w").textContent = it.label(L());
    go(b, [{transform:"scale(.6)"}, {transform:"scale(1.2) rotate(-8deg)"}, {transform:"none"}], {duration:380, easing:"ease-out"});
    r.busy = false; M.open(); talk(line, lbw); idle();
    return true;
  }
  async function feed(it, from){
    const r = R; if (!r || !alive(gen)) return;
    if (r.groups) return feedCount(r, it, from);
    if (r.busy) return;
    G.taps++; r.busy = true; clearTimeout(r.idle);
    const b = r.btn.get(it), lang = L(), want = r.queue[0];
    b.classList.remove("bob"); b.style.opacity = "0"; SND.whoosh(); M.open(1.2);
    await fly(it.html, from, M.mouth(), {spin:360});
    if (!live(r)) return;
    if (it === want) {
      r.queue.shift(); marks(); b.style.visibility = "hidden"; b.disabled = true;
      await M.chew(it.html);
      if (!live(r)) return;
      if (r.queue.length) { M.say(one(TX.yum[lang])); r.busy = false; idle(); return; }
      return win(r);
    }
    const early = r.queue.includes(it); // level 3: the right food, too soon
    const line = early ? TX.firstThe[lang](want.name(lang)) : two(TX.yuck[lang](it.name(lang, true)), TX.ask[lang][1](want.name(lang)), lang);
    if (await spit(r, it, b, line, early ? want.lbw : [...it.lbw, ...want.lbw]) && p4 && r.tries >= 2) r.btn.get(want).classList.add("bob");
  }
  async function feedCount(r, it, from){
    if (r.busy) return;
    G.taps++; clearTimeout(r.idle);
    const b = r.btn.get(it), lang = L(), g = r.groups.find(x => x.item === it);
    if (g && g.got < g.n) { // one more: counted aloud, and into the belly
      g.got++; marks(); M.count(g.got, numberIn(g.got, lang)); say(numberIn(g.got, lang), lang);
      if (p4 && r.groups.every(x => x.got === x.n)) done.classList.add("bob");
      fly(it.html, from, M.mouth(), {spin:300, dur:380}).then(() => {
        if (!live(r)) return;
        r.eaten.push(it.html); M.gulp(it.html); M.tummy(r.eaten); M.fat(1 + Math.min(r.eaten.length, 8) * .03);
      });
      return idle();
    }
    r.busy = true; SND.whoosh(); M.open(1.2);
    await fly(it.html, from, M.mouth(), {spin:300});
    if (!live(r)) return;
    if (!g) return spit(r, it, b, TX.yuck[lang](it.name(lang, true)), it.lbw);
    // one too many: tummy ache, a big burp, and everything starts again
    r.tries++; sfx.ko(); M.ache(); M.say(TX.tooMany[lang]); done.classList.remove("bob");
    r.groups.forEach(x => x.got = 0); r.eaten = [];
    await talk(TX.tooMany[lang], []); await wait(500);
    if (!live(r)) return;
    M.burp(TX.burp[lang][0], true); M.tummy([]); M.fat(1);
    await wait(900);
    if (!live(r)) return;
    r.busy = false; ask();
  }
  function count(){
    const r = R; if (!r || r.busy || !alive(gen)) return;
    G.taps++; clearTimeout(r.idle);
    if (r.groups.every(x => x.got === x.n)) { r.busy = true; return win(r); }
    const lang = L(); r.tries++; sfx.ko(); M.rumble(); M.say(TX.more[lang]);
    talk(two(TX.more[lang], r.ask(lang), lang), r.lbw); idle();
  }
  async function win(r){
    const first = r.tries === 0, lang = L();
    root.querySelectorAll("[data-ok]").forEach(b => delete b.dataset.ok);
    sfx.ok(); if (first) addStar(); else if (typeof fx !== "undefined") fx.sparkle();
    logRound(r.key, first, r.tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
    M.happy();
    const line = one(TX.yum[lang]); M.say(line);
    await talk(line, []);
    if (!live(r)) return;
    if (r.groups || !rnd(3)) { // a burp, then good manners
      const [noise, sorry] = TX.burp[lang];
      M.burp(noise); await wait(800); if (!live(r)) return;
      M.say(sorry); await talk(sorry, []); if (!live(r)) return;
    }
    await wait(lang === "lb" ? 800 : 250);
    if (!live(r)) return;
    i++; next();
  }
  async function end(){
    if (!TEST) { M.burp(TX.burp[L()][0], true); M.happy(); await wait(1300); if (!alive(gen)) return; }
    finish();
  }

  /* ---------- poke the monster ---------- */
  M.box.addEventListener("click", e => {
    if (!R || R.busy) return;
    G.taps++;
    const lang = L(), line = t => { if (Date.now() - lastLine < 2500) return; lastLine = Date.now(); M.say(t); if (lang !== "lb") say(t, lang); };
    if (e.target.closest(".mo-nose")) return M.honk();
    if (e.target.closest(".mo-belly")) { M.boing(); return line(TX.belly[lang]); }
    M.tickle(); line(one(TX.tickle[lang]));
  });
  root.addEventListener("pointermove", e => M.look(e.clientX, e.clientY));
  // now and then it looks around
  loops.push(setInterval(() => { if (!alive(gen) || calm()) return; const r = M.box.getBoundingClientRect(); M.look(r.left - r.width + rnd(r.width * 3), r.top + rnd(r.height)); }, 3800));
  next();
});
})();
