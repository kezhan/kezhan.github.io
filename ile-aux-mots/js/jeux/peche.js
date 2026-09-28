/* L'Île aux Mots : jeu « La Pêche aux mots » (ticket 2026-09-27_jeu-peche).
   Des poissons nagent, chacun porte une image. « Catch the octopus! » : toucher le bon, la canne le remonte
   en tournoyant jusqu'au seau glouton ; un mauvais poisson éclabousse, tire la langue et file.
   1 : trois poissons lents, un étang au choix · 2 : cinq poissons plus vifs et un vieux déchet ·
   3 : les poissons font demi-tour, étangs mélangés, couleurs, actions, nombres pour le grand ·
   4 : la consigne est écrite sur la bouée, sans voix, tables de multiplication pour le grand.
   Anglais, allemand, luxembourgeois ou chinois, jamais de français ; textes et dessins dans js/contenus/peche.js. */
(() => {
const {sky: SKY, fw: FW, fh: FH} = PECHE_SIZE;
const L = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const X = l => PECHE_TXT[l || L()];
const still = () => fx.calm();
const settle = a => a ? a.finished.catch(() => {}) : Promise.resolve();
const rate = (sw, r) => (sw._a || []).forEach(a => a.updatePlaybackRate ? a.updatePlaybackRate(r) : (a.playbackRate = r));
const order = (r, l = L()) => pecheQuiz.order(r, l), that = (r, it, l = L()) => pecheQuiz.that(r, it, l);

/* ---------- sounds: Web Audio glides and noise bursts ---------- */
function audio(){ try { ac = ac || new (window.AudioContext || window.webkitAudioContext)(); return ac; } catch(e) { return null; } }
function glide(f1, f2, dur, type = "sine", vol = .2, delay = 0){
  const a = audio(); if (!a) return;
  try {
    const t = a.currentTime + delay, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f1, t); o.frequency.exponentialRampToValueAtTime(f2, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination); o.start(t); o.stop(t + dur + .05);
  } catch(e) {}
}
function noise(dur, freq, vol, delay = 0){
  const a = audio(); if (!a) return;
  try {
    const n = Math.floor(a.sampleRate * dur), buf = a.createBuffer(1, n, a.sampleRate), d = buf.getChannelData(0);
    for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * (1 - i / n);
    const s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = buf; f.type = "bandpass"; f.frequency.value = freq; g.gain.value = vol;
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(a.currentTime + delay);
  } catch(e) {}
}
const snd = {
  splash: () => { noise(.4, 900, .6); glide(700, 180, .3, "sine", .1); },
  boing: () => { glide(160, 640, .12, "sine", .25); glide(640, 280, .2, "sine", .2, .12); },
  reel: () => glide(250, 1400, .55, "sawtooth", .045),
  plop: () => glide(1100, 160, .15, "sine", .3),
  burp: () => { glide(130, 62, .42, "sawtooth", .16); glide(98, 55, .36, "square", .07, .04); },
  bleh: () => { glide(430, 140, .45, "square", .07); glide(300, 90, .4, "sawtooth", .05, .1); },
  bloop: () => glide(380, 920, .1, "sine", .2),
  sneeze: () => { glide(380, 900, .28, "triangle", .08); noise(.3, 3200, .5, .28); },
  giggle: () => [0, .11, .22].forEach((d, k) => glide(700 + k * 90, 900 + k * 90, .08, "triangle", .1, d)),
  clack: () => { glide(1800, 1500, .04, "square", .08); glide(1800, 1500, .04, "square", .08, .1); }
};

/* ---------- swimming: a looping path, the fish turning round at each end ---------- */
function swim(sw, y, min, max, v, turny){
  let pts = [min, max, min];
  // levels 3-4: random stretches, the fish turns back anywhere
  const next = p => { const lo = Math.max(0, p - 60 - min), hi = Math.max(0, max - p - 60), r = Math.random() * (lo + hi); return r < lo ? min + r : p + 60 + (r - lo); };
  if (turny) for (let t = 0; t < 30; t++) {
    const p = [min + Math.random() * (max - min)];
    for (let j = 0; j < 4; j++) p.push(next(p[j]));
    if (Math.abs(p[4] - p[0]) >= 60) { pts = [...p, p[0]]; break; }
  }
  const seg = pts.slice(1).map((q, j) => Math.abs(q - pts[j])), len = seg.reduce((a, b) => a + b, 0) || 1, n = seg.length;
  const off = [0]; seg.forEach(s => off.push(Math.min(1, off[off.length - 1] + s / len))); off[n] = 1;
  const dur = len / v * 1000, dir = pts.slice(1).map((q, j) => q > pts[j] ? 1 : -1);
  const e = Math.min(90 / dur, Math.min(...seg) / len / 2.5);
  const anims = [sw.animate(pts.map((q, j) => ({transform: `translate(${q}px,${y}px)`, offset: off[j], easing: "ease-in-out"})), {duration: dur, iterations: Infinity})];
  const turn = [];
  dir.forEach((d, j) => {
    const before = dir[(j + n - 1) % n], after = dir[(j + 1) % n];
    turn.push({offset: off[j] + (before !== d ? e : 0), transform: `scaleX(${d})`}, {offset: off[j + 1] - (after !== d ? e : 0), transform: `scaleX(${d})`});
  });
  if (dir[n - 1] !== dir[0]) { turn.unshift({offset: 0, transform: "scaleX(0)"}); turn.push({offset: 1, transform: "scaleX(0)"}); }
  const flip = sw.querySelector(".pc-flip");
  if (flip) anims.push(flip.animate(turn, {duration: dur, iterations: Infinity}));
  // the picture never shows mirrored: it only narrows while the fish turns round
  const plate = sw.querySelector(".pc-plate"), wrap = dir[n - 1] !== dir[0];
  if (plate) {
    const kf = [{offset: 0, transform: `scaleX(${wrap ? 0 : 1})`}, {offset: wrap ? e : 0, transform: "scaleX(1)"}];
    dir.forEach((d, j) => { if (j && dir[j - 1] !== d) kf.push({offset: off[j] - e, transform: "scaleX(1)"}, {offset: off[j], transform: "scaleX(0)"}, {offset: off[j] + e, transform: "scaleX(1)"}); });
    kf.push({offset: wrap ? 1 - e : 1, transform: "scaleX(1)"}, {offset: 1, transform: `scaleX(${wrap ? 0 : 1})`});
    anims.push(plate.animate(kf, {duration: dur, iterations: Infinity}));
  }
  const phase = Math.random() * dur; anims.forEach(a => { a.currentTime = phase; });
  return anims;
}

/* ---------- the scene: sky, captain, bucket, water, and every animation in it ---------- */
function makeScene(box, gen){
  const scene = el("div", "pc-scene");
  scene.innerHTML = `<div class="pc-water"><div class="pc-sand"></div></div><div class="pc-waves"></div><div class="pc-pier"></div>
    <div class="pc-pile"></div><div class="pc-bucket">${PECHE_ART.bucket}</div><div class="pc-cap">${PECHE_ART.captain}</div><div class="pc-line"></div>`;
  box.append(scene);
  const q = s => scene.querySelector(s);
  const water = q(".pc-water"), cap = q(".pc-cap"), bucket = q(".pc-bucket"), pile = q(".pc-pile"), line = q(".pc-line"), tip = q(".pc-tip"), idle = q(".pc-idle");
  const at = node => { const r = node.getBoundingClientRect(), s = scene.getBoundingClientRect(); return {x: r.left + r.width / 2 - s.left - scene.clientLeft, y: r.top + r.height / 2 - s.top - scene.clientTop}; };
  const P = {scene, cap, at};
  let tok = 0;
  // the captain's mouth moves while the voice speaks; false when another line took over
  P.talk = async (text, l, r) => { const my = ++tok; cap.classList.add("pc-talking"); await say(text, l, r); if (my === tok) cap.classList.remove("pc-talking"); return my === tok && alive(gen); };
  P.mood = (cls, ms) => { cap.classList.remove("pc-happy", "pc-oops", "pc-yuck"); cap.classList.add(cls); loops.push(setTimeout(() => cap.classList.remove(cls), ms)); };
  P.jump = (node, kf, ms) => { if (!still()) node.animate(kf, {duration: ms, easing: "ease-out"}); };
  P.speech = (text, x, y) => {
    const b = el("div", "pc-say"); b.textContent = text; scene.append(b);
    b.style.left = Math.max(4, Math.min(x - b.offsetWidth / 2, scene.clientWidth - b.offsetWidth - 4)) + "px";
    b.style.top = Math.max(2, y - b.offsetHeight) + "px";
    if (!still()) b.animate([{transform: "scale(.3)", opacity: 0}, {transform: "scale(1.1)", opacity: 1, offset: .6}, {transform: "none", opacity: 1}], {duration: 260, easing: "ease-out"});
    loops.push(setTimeout(() => b.remove(), 1700));
  };
  P.spray = (x, y, n, ch = "💧") => {
    if (still()) return;
    for (let k = 0; k < n; k++) {
      const d = el("div", "pc-fx", ch); d.style.left = x + "px"; d.style.top = y + "px"; scene.append(d);
      const a = -Math.PI / 2 + (n > 1 ? (k / (n - 1) - .5) * 2.4 : 0), r = 36 + rnd(34), dx = Math.cos(a) * r, dy = Math.sin(a) * r;
      d.animate([{transform: "translate(-50%,-50%) scale(.4)", opacity: 1}, {transform: `translate(calc(-50% + ${dx}px),calc(-50% + ${dy}px)) scale(1)`, opacity: 1, offset: .55},
        {transform: `translate(calc(-50% + ${dx * 1.3}px),calc(-50% + ${dy + 40}px)) scale(.7)`, opacity: 0}], {duration: 700, easing: "ease-out"}).onfinish = () => d.remove();
    }
  };
  P.ripple = (x, y) => {
    if (still()) return;
    const r = el("div", "pc-ring"); r.style.left = x + "px"; r.style.top = y + "px"; scene.append(r);
    r.animate([{transform: "scale(.3)", opacity: 1}, {transform: "scale(3.2)", opacity: 0}], {duration: 650, easing: "ease-out"}).onfinish = () => r.remove();
  };
  P.pileAdd = face => {
    const s = el("span", "", face); pile.append(s);
    while (pile.children.length > 3) pile.firstChild.remove();
    P.jump(s, [{transform: "translateY(24px)", opacity: 0}, {transform: "translateY(-8px)", opacity: 1, offset: .6}, {transform: "none", opacity: 1}], 420);
  };
  P.burp = () => {
    snd.burp(); bucket.classList.add("pc-talking"); loops.push(setTimeout(() => bucket.classList.remove("pc-talking"), 450));
    const b = at(bucket); P.spray(b.x, b.y - 26, 1, "💨");
    P.jump(bucket, [{transform: "none"}, {transform: "scale(1.12,.86) rotate(-5deg)"}, {transform: "scale(.95,1.08) rotate(3deg)"}, {transform: "none"}], 420);
  };
  P.gulp = face => {
    snd.plop(); const b = at(bucket); P.spray(b.x, b.y - 24, 5);
    P.jump(bucket, [{transform: "none"}, {transform: "scale(1.25,.78)"}, {transform: "scale(.9,1.15)"}, {transform: "none"}], 420);
    P.pileAdd(face);
    if (Math.random() < .35) loops.push(setTimeout(P.burp, 520));
  };
  P.spawn = (it, k, n, v, turny) => {
    const top = SKY + 8, bot = scene.clientHeight - 26 - FH, y = n > 1 ? top + k * (bot - top) / (n - 1) : (top + bot) / 2;
    const sw = el("div", "pc-swim" + (it.junk ? " pc-junk" : "")), bob = el("div", "pc-bob"), b = el("button", "pc-fish");
    b.type = "button"; b.setAttribute("aria-label", it.label);
    if (it.junk) b.textContent = it.e;
    else {
      const f = el("div", "pc-flip", PECHE_ART.fish(it.col, it.dark)); b.append(f);
      f.querySelector(".pc-eye").style.animationDelay = -Math.random() * 4 + "s";
      f.querySelector(".pc-tail").style.animationDelay = -Math.random() + "s";
      if (it.face) b.append(el("span", "pc-plate" + (it.num ? " pc-num" : ""), `<i>${it.face}</i>`));
    }
    bob.style.animationDelay = -Math.random() * 2 + "s";
    bob.append(b); sw.append(bob); scene.append(sw);
    const min = 6, max = Math.max(min + 80, scene.clientWidth - FW - 6);
    // test and calm mode: still fish, in two columns so none covers another
    if (still()) sw.style.transform = `translate(${k % 2 ? max : min}px,${y}px)`;
    else {
      sw._a = swim(sw, y, min, max, v * (it.junk ? .5 : .85 + Math.random() * .3), turny);
      bob.animate([{opacity: 0, transform: "scale(.2)"}, {opacity: 1, transform: "scale(1.12)", offset: .7}, {opacity: 1, transform: "none"}], {duration: 380, delay: k * 70, fill: "backwards", easing: "ease-out"});
    }
    return {sw, b, it};
  };
  P.leave = sw => { if (still()) return sw.remove(); sw.style.pointerEvents = "none"; sw.animate([{opacity: 1}, {opacity: 0}], {duration: 220, fill: "forwards"}).onfinish = () => sw.remove(); };
  P.flee = sw => { rate(sw, 4.5); loops.push(setTimeout(() => rate(sw, sw.classList.contains("pc-hint") ? .3 : 1), 650)); };
  P.hint = sw => { sw.classList.add("pc-hint"); rate(sw, .3); };
  // hooked: the line drops, the fish wriggles, spins up to the rod tip, then flies into the bucket (or away, for junk)
  P.reel = async (sw, toBucket, face) => {
    if (still()) { sw.remove(); if (toBucket) P.pileAdd(face); return; }
    const t = at(tip), m = new DOMMatrix(getComputedStyle(sw).transform), x = m.m41, y = m.m42, flip = sw.querySelector(".pc-flip");
    if (flip) flip.style.transform = getComputedStyle(flip).transform;
    (sw._a || []).forEach(a => a.cancel()); sw._a = null;
    const from = `translate(${x}px,${y}px)`; sw.style.transform = from; sw.style.zIndex = 7;
    const dx = x + FW / 2 - t.x, dy = y + FH / 2 - t.y, k = Math.hypot(dx, dy) / 100, r = `rotate(${Math.atan2(-dx, dy)}rad)`;
    line.style.left = t.x + "px"; line.style.top = t.y + "px"; idle.style.opacity = 0;
    await settle(line.animate([{transform: `${r} scaleY(0)`}, {transform: `${r} scaleY(${k})`}], {duration: 240, easing: "ease-out", fill: "forwards"}));
    snd.boing(); P.spray(x + FW / 2, y + 4, 1, "❗");
    await settle(sw.firstChild.animate([{transform: "none"}, {transform: "rotate(-22deg) scale(1.15)"}, {transform: "rotate(16deg) scale(1.05)"}, {transform: "none"}], {duration: 300}));
    snd.reel();
    const spin = toBucket ? 720 : 360, up = `translate(${t.x - FW / 2}px,${t.y - FH / 2}px) rotate(${spin}deg) scale(.7)`;
    line.animate([{transform: `${r} scaleY(${k})`}, {transform: `${r} scaleY(0)`}], {duration: 520, easing: "ease-in", fill: "forwards"});
    await settle(sw.animate([{transform: from}, {transform: up}], {duration: 520, easing: "ease-in", fill: "forwards"}));
    idle.style.opacity = 1;
    if (!toBucket) {
      await settle(sw.animate([{transform: up}, {transform: `translate(-160px,-30px) rotate(${spin + 720}deg) scale(.4)`, opacity: 0}], {duration: 650, easing: "ease-in", fill: "forwards"}));
      return sw.remove();
    }
    const b = at(bucket);
    await settle(sw.animate([{transform: up}, {transform: `translate(${(t.x + b.x) / 2 - FW / 2}px,${14 - FH / 2}px) rotate(${spin + 180}deg) scale(.55)`, offset: .5},
      {transform: `translate(${b.x - FW / 2}px,${b.y - 18 - FH / 2}px) rotate(${spin + 360}deg) scale(.3)`, opacity: .3}], {duration: 520, easing: "ease-in-out", fill: "forwards"}));
    sw.remove(); P.gulp(face);
  };
  // life in the water: sea-weed, a crab to poke, bubbles, ripples where the water is touched
  ["12%", "48%", "80%"].forEach((left, k) => { const w = el("div", "pc-weed", "🌿"); w.style.left = left; w.style.animationDelay = -k * .9 + "s"; water.append(w); });
  const crab = el("div", "pc-crab", "<span>🦀</span>"); water.append(crab);
  if (!still()) {
    crab.animate([{transform: "translateX(0)"}, {transform: `translateX(${Math.max(0, scene.clientWidth - 64)}px)`}], {duration: 12000, direction: "alternate", iterations: Infinity, easing: "ease-in-out"});
    loops.push(setInterval(() => {
      if (!alive(gen)) return;
      const s = 6 + rnd(11), b = el("div", "pc-rise");
      b.style.cssText = `width:${s}px; height:${s}px; left:${rnd(95)}%; animation-duration:${3 + Math.random() * 2}s`;
      b.onanimationend = () => b.remove(); water.append(b);
    }, 750));
  }
  crab.addEventListener("pointerdown", e => {
    e.stopPropagation(); snd.clack();
    P.jump(crab.firstChild, [{transform: "none"}, {transform: "translateY(-44px) rotate(25deg) scale(1.2)"}, {transform: "translateY(-10px) rotate(-10deg)"}, {transform: "none"}], 520);
    const c = at(crab); P.spray(c.x, c.y - 24, 2, "💢");
  });
  water.addEventListener("pointerdown", e => { const s = scene.getBoundingClientRect(); P.ripple(e.clientX - s.left, e.clientY - s.top); snd.bloop(); });
  bucket.addEventListener("click", P.burp);
  return P;
}

/* ---------- the pond picker (levels 1-2): pictures only ---------- */
function picker(body, gen, go){
  const box = el("div", "pc" + (still() ? " pc-still" : ""));
  const buoy = el("div", "pc-buoy", PECHE_ART.ring), bt = el("span"); buoy.append(bt);
  const top = el("div", "pc-top"), row = el("div", "row");
  const spk = el("button", "speak chunky", "🔊 <span></span>");
  const texts = () => {
    const pics = S.kid === "p4" && L() !== "lb";
    bt.textContent = pics ? "🎣 ❓" : X().where; buoy.classList.toggle("pc-big", pics); spk.querySelector("span").textContent = X().again;
  };
  spk.onclick = () => { texts(); say(X().where, L()); };
  row.append(spk);
  const grid = el("div", "pc-ponds"); let busy = false;
  [["sea", "🌊"], ...Object.keys(THEMES).map(k => [k, k === "colors" ? "🎨" : THEMES[k].icon])].forEach(([k, icon], j) => {
    const b = el("button", "pc-pond chunky", icon);
    b.style.animationDelay = -j * .35 + "s";
    if (!j) markOk(b);
    b.onclick = async () => {
      if (busy) return; busy = true; snd.splash();
      if (!still()) await settle(b.animate([{transform: "none"}, {transform: "scale(1.25) rotate(-12deg)"}, {transform: "scale(.2) rotate(220deg)", opacity: 0}], {duration: 420, easing: "ease-in"}));
      if (alive(gen)) go(k);
    };
    grid.append(b);
  });
  top.append(buoy, row); box.append(top, grid); body.append(box);
  texts(); say(X().where, L());
}

registerGame({id: "peche", em: "🎣", name: "La Pêche aux mots", desc: "Attrape le bon poisson", multi: true,
  title: Object.fromEntries(Object.entries(PECHE_TXT).map(([l, x]) => [l, x.title])),
  sub: Object.fromEntries(Object.entries(PECHE_TXT).map(([l, x]) => [l, x.sub]))}, function (theme0) {
  const lvl = levelOf("peche"), gen = GEN, body = $("gameBody");
  body.innerHTML = "";
  if (lvl >= 3) return play(null);
  if (theme0 === "sea" || THEMES[theme0]) return play(theme0);
  picker(body, gen, play);

  function play(theme){
    const total = lvl === 1 ? 6 : 8, n = [3, 5, 5, 6][lvl - 1], speed = [38, 62, 74, 92][lvl - 1] * (S.kid === "p4" ? .75 : 1);
    const rs = pecheQuiz.rounds(theme, lvl, total, n, L(), S.kid), res = [], voiced = lvl < 4;
    startSession("peche", theme, total);
    body.innerHTML = "";
    const box = el("div", "pc" + (still() ? " pc-still" : ""));
    const buoy = el("div", "pc-buoy", PECHE_ART.ring), bt = el("span"); buoy.append(bt);
    const top = el("div", "pc-top"), row = el("div", "row");
    top.append(buoy, row); box.append(top); body.append(box);
    const P = makeScene(box, gen);
    let i = 0, cur = null, tries = 0, locked = false, fish = [], spk = null, hb = null;
    const helpLang = () => L() === "zh" ? "en" : "zh"; // the big one's help: Chinese, or English when he learns Chinese
    // Luxembourgish has no voice: its sentence is always written; level 4 is read, never heard first
    const texts = () => {
      const l = L(), written = lvl === 4 || l === "lb";
      bt.textContent = written ? order(cur, l) : "";
      buoy.hidden = !written; // heard only: the voice button is enough
      if (spk && voiced) spk.querySelector("span").textContent = X(l).again;
      if (hb) hb.textContent = helpLang() === "zh" ? "中文 ?" : "English ?";
    };
    const buildRow = () => {
      row.innerHTML = ""; hb = null;
      spk = el("button", "speak chunky", "🔊 <span></span>");
      spk.onclick = e => {
        texts();
        if (!voiced && !e.isTrusted) return; // a language flag only rewrites the buoy at level 4
        if (!voiced) G.hints++; else if (e.isTrusted) G.replays++;
        P.talk(order(cur), L());
      };
      row.append(spk);
      if (S.kid === "p4") { const s = el("button", "chip", "🐢"); s.onclick = () => { G.replays++; P.talk(order(cur), L(), .55); }; row.append(s); }
      if (S.kid === "p7" && lvl <= 3 && cur.t.en) {
        hb = el("button", "chip");
        hb.onclick = () => { const h = helpLang(), w = cur.t[h] || cur.t.en; G.hints++; hb.textContent = w; say(w, h); };
        row.append(hb);
      }
    };
    const next = () => {
      if (!alive(gen)) return;
      if (i >= total) return finish();
      cur = rs[i]; tries = 0; locked = false;
      renderDots(res, total, i);
      fish.forEach(f => P.leave(f.sw));
      const items = shuffle(cur.items);
      fish = items.map((it, k) => { const f = P.spawn(it, k, items.length, speed, lvl >= 3); if (it.ok) markOk(f.b); bind(f); return f; });
      buildRow(); texts();
      P.jump(buoy, [{transform: "translateY(-40px) rotate(-10deg)", opacity: 0}, {transform: "translateY(6px) rotate(3deg)", opacity: 1, offset: .7}, {transform: "none", opacity: 1}], 450);
      if (voiced) P.talk(order(cur), L());
    };
    // the touch answers at once (pointerdown); a lone click (recette fallback) works too
    const bind = f => {
      let down = 0;
      f.b.addEventListener("pointerdown", () => { down = Date.now(); P.jump(f.b, [{transform: "none"}, {transform: "scale(.82,1.18)"}, {transform: "scale(1.12,.9)"}, {transform: "none"}], 300); hit(f); });
      f.b.addEventListener("click", () => { if (Date.now() - down > 700) hit(f); });
    };
    const hit = f => {
      if (locked || !alive(gen) || !fish.includes(f)) return; // a fish of the last round, still fading out, is not in play
      G.taps++;
      if (f.it.junk) return junk(f);
      if (f.it.ok) return caught(f);
      const l = L(), x = X(l), c = P.at(f.sw);
      tries++; sfx.ko(); snd.splash();
      P.spray(c.x, c.y - 10, 7); P.speech(x.nope[rnd(x.nope.length)], c.x, c.y - FH / 2);
      f.b.classList.add("pc-grr"); loops.push(setTimeout(() => f.b.classList.remove("pc-grr"), 900));
      P.mood("pc-oops", 800); P.flee(f.sw);
      const r = cur;
      (async () => { if (await P.talk(that(r, f.it, l), l) && voiced && !locked && r === cur) P.talk(order(r), L()); })();
      // the little one gets a nudge after two misses so the game never stalls
      if (tries >= 2 && S.kid === "p4") { const g = fish.find(q => q.it.ok); if (g) P.hint(g.sw); }
    };
    const caught = async f => {
      locked = true; sfx.ok();
      const first = tries === 0; if (first) addStar();
      logRound(cur.key, first, tries + 1, {lvl, kind: cur.kind}); res.push(first ? 1 : 0); renderDots(res, total, -1);
      const l = L(), x = X(l), p = x.praise[rnd(x.praise.length)];
      P.mood("pc-happy", 1500); P.speech(p, 118, 36);
      P.jump(P.cap, [{transform: "none"}, {transform: "translateY(-14px) rotate(-5deg)"}, {transform: "translateY(2px) rotate(3deg)"}, {transform: "none"}], 480);
      fish = fish.filter(q => q !== f);
      await Promise.all([P.talk(`${p} ${that(cur, f.it, l)}`, l), P.reel(f.sw, true, f.it.pile)]);
      if (!alive(gen)) return;
      i++; loops.push(setTimeout(next, 350));
    };
    // junk on the hook: a miss, but a funny one
    const junk = async f => {
      locked = true; tries++; snd.bleh();
      const t = X().junk[f.it.k];
      P.mood("pc-yuck", 1700); P.speech(t, 118, 36); P.talk(t, L());
      fish = fish.filter(q => q !== f);
      await P.reel(f.sw, false);
      if (alive(gen) && fish.length) locked = false;
    };
    P.cap.addEventListener("click", () => {
      G.taps++;
      const x = X();
      if (Math.random() < .35) {
        snd.sneeze(); P.speech(x.sneeze, 118, 36); P.mood("pc-oops", 600);
        P.jump(P.cap, [{transform: "none"}, {transform: "translate(-6px,4px) rotate(-7deg)", offset: .6}, {transform: "translate(8px,-3px) rotate(6deg)", offset: .75}, {transform: "none"}], 700);
      } else {
        const t = x.captain[rnd(x.captain.length)];
        snd.giggle(); P.speech(t, 118, 36); P.mood("pc-happy", 700);
        if (!locked) P.talk(t, L());
      }
    });
    if (!still()) P.speech(X().hello, 118, 36);
    next();
  }
});
})();
