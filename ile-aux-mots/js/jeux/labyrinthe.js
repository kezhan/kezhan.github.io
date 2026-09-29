/* L'Île aux Mots : jeu « Le labyrinthe dicté » (ticket _tickets/ouvert/2026-09-28_jeu-labyrinthe.md), catégorie nombres et logique.
   Un petit animal rejoint un trésor sur une grille en suivant des directions dans la langue apprise (en, de, lb, zh), jamais en français.
   1 : une direction, grille 3×3, trois flèches (le trésor est sur la case voisine : l'image et la voix disent la même chose)
   2 : deux directions enchaînées, grille 4×4, un arbre là où l'ordre inverse mènerait
   3 : directions avec nombre de pas (« two steps up, then three steps right »), grille 5×5 avec obstacles, une touche par pas, comptée à voix haute
   4 : lire quatre chemins écrits et choisir celui qui mène au trésor ; un mauvais chemin est parcouru pour de vrai, jusqu'à l'arbre ou la mauvaise case.
   Luxembourgeois vérifié sur lod.lu : Géi, no uewen, no ënnen, no lénks, no riets (déjà dans pirate), Schrëtt (SCHRETT1, masculin : ee Schrëtt),
   dann (DANN2, adverbe), Wee (WEE1, masculin : wéi ee Wee), Labyrinth (LABYRINTH1, masculin ou neutre : de Labyrinth).
   À faire relire en luxembourgeois : le pluriel « zwee Schrëtt » (lod.lu ne donne pas de forme), « Géi zwee Schrëtt no uewen, dann dräi Schrëtt no riets! »,
   « Oh! Dat ass lénks! », « Bum! », « Wéi ee Wee ass richteg? ». Aucun de ces mots n'a d'enregistrement lod.lu dans le lexique : en luxembourgeois, tout se lit.
   À faire relire en allemand : « Hoppla! Das ist oben! » et « Bums! ». */
(() => {
addStyle(`
.lab{display:flex; flex-direction:column; align-items:center; gap:12px}
.lab .prompt{margin:0; max-width:100%}
.lab .prompt.lab-long{font-size:clamp(19px,4.4vw,28px); line-height:1.3}
.lab-board{--n:3; position:relative; width:min(100%,340px); aspect-ratio:1; display:grid; grid-template-columns:repeat(var(--n),1fr); grid-template-rows:repeat(var(--n),1fr);
  background:#C8F0AE; border:3px solid var(--ink); border-radius:18px; box-shadow:3px 4px 0 var(--ink); overflow:hidden; container-type:inline-size}
.lab-board.lab-small{width:min(100%,290px)}
.lab-cell{position:relative; display:grid; place-items:center; line-height:1; font-size:calc(190px / var(--n)); font-size:calc(56cqw / var(--n)); box-shadow:inset 0 0 0 1px #5E9E4A33}
.lab-cell.lab-alt{background:#B4E596}
.lab-cell.lab-trail::after{content:""; position:absolute; width:24%; height:24%; border-radius:50%; background:#E9A15A; opacity:.75}
.lab-goal{display:inline-block; position:relative; z-index:3; animation:lab-glow 1.4s ease-in-out infinite}
.lab-got .lab-goal{animation:lab-got .8s ease-out forwards}
@keyframes lab-glow{50%{transform:scale(1.12) rotate(-6deg)}}
@keyframes lab-got{0%{transform:none} 45%{transform:translateY(-30%) scale(1.5) rotate(12deg)} 100%{transform:translate(34%,-30%) scale(.6)}}
.lab-hero{position:absolute; left:0; top:0; width:calc(100% / var(--n)); height:calc(100% / var(--n)); display:grid; place-items:center; z-index:2; pointer-events:none;
  line-height:1; font-size:calc(210px / var(--n)); font-size:calc(62cqw / var(--n)); transition:transform .28s cubic-bezier(.3,.7,.4,1.25)}
.lab-hero > span{display:inline-block; animation:bob 1.6s ease-in-out infinite}
.lab-hero .lab-dizzy{position:absolute; top:-8%; right:4%; font-size:45%; animation:none}
.lab-pad{display:grid; grid-template-columns:repeat(3,66px); grid-template-rows:repeat(3,66px); gap:8px; justify-content:center}
.lab-arrow{font-size:34px; background:#fff; padding:0; display:grid; place-items:center}
.lab-mid{display:grid; place-items:center; font-size:34px}
.lab-opts{width:100%; display:grid; gap:10px; grid-template-columns:repeat(auto-fit,minmax(170px,1fr))}
.lab-opt{background:#fff; text-align:left; font-family:var(--display); font-size:18px; font-weight:600; line-height:1.4; padding:10px 12px}
.lab-opt .lab-no{display:inline-grid; place-items:center; width:1.4em; height:1.4em; margin-right:6px; border-radius:50%; background:var(--sun); font-size:14px; vertical-align:1px}
.lab-opt.ok{background:#C9F2DF} .lab-opt.ko{background:#FFD6CF}
.lab-opt[disabled]{opacity:.6}
`);

const DIRS = {up:[0, -1], down:[0, 1], left:[-1, 0], right:[1, 0]}, KEYS = ["up", "right", "down", "left"];
const OPP = {up:"down", down:"up", left:"right", right:"left"};
const ARROW = {up:"⬆️", down:"⬇️", left:"⬅️", right:"➡️"};
const perp = d => d === "up" || d === "down" ? ["left", "right"] : ["up", "down"];
const key = (x, y) => x + "," + y;
const same = (a, b) => a[0] === b[0] && a[1] === b[1];
const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
const one = a => a[rnd(a.length)];
const HEROES = ["🐭", "🐰", "🐶", "🐱", "🐼", "🐸", "🐧", "🐢", "🦊", "🐨"];
const GEMS = ["💎", "👑", "💰", "🏆", "🎁"];
const SURPRISES = ["🧸", "🎈", "🍭", "🦄", "🍩", "🚀"];
const TREES = ["🌳", "🌲", "🌵", "🪨"];

// everything the child sees or hears; "dir" goes into the sentences, "where" names the arrow tapped by mistake
const LAB = {
  en:{dir:{up:"up", down:"down", left:"left", right:"right"}, where:{up:"up", down:"down", left:"left", right:"right"},
    one:d => `Go ${d}!`, two:(a, b) => `Go ${a}, then ${b}!`,
    seg:(d, n) => `${n === 1 ? "one step" : numberWords(n) + " steps"} ${d}`, many:s => `Go ${s.join(", then ")}!`,
    tap:d => `${cap(d)}!`, oops:w => `Oops! That's ${w}!`, found:"You found the treasure!",
    which:"Which way leads to the treasure?", bonk:"Bonk!", notHere:"Not here!", retry:"Try again!", again:"Again"},
  // German: "einen Schritt" (accusative), "zwei Schritte"
  de:{dir:{up:"nach oben", down:"nach unten", left:"nach links", right:"nach rechts"}, where:{up:"oben", down:"unten", left:"links", right:"rechts"},
    one:d => `Geh ${d}!`, two:(a, b) => `Geh ${a}, dann ${b}!`,
    seg:(d, n) => `${n === 1 ? "einen Schritt" : ["", "", "zwei", "drei", "vier"][n] + " Schritte"} ${d}`, many:s => `Geh ${s.join(", dann ")}!`,
    tap:d => `${cap(d)}!`, oops:w => `Hoppla! Das ist ${w}!`, found:"Du hast den Schatz gefunden!",
    which:"Welcher Weg führt zum Schatz?", bonk:"Bums!", notHere:"Nicht hier!", retry:"Versuch es noch mal!", again:"Nochmal"},
  // Luxembourgish: Schrëtt is masculine, "ee Schrëtt" (Eifel rule before S), "zwee Schrëtt" (not zwou)
  lb:{dir:{up:"no uewen", down:"no ënnen", left:"no lénks", right:"no riets"}, where:{up:"uewen", down:"ënnen", left:"lénks", right:"riets"},
    one:d => `Géi ${d}!`, two:(a, b) => `Géi ${a}, dann ${b}!`,
    seg:(d, n) => `${["", "ee", "zwee", "dräi", "véier"][n]} Schrëtt ${d}`, many:s => `Géi ${s.join(", dann ")}!`,
    tap:d => `${cap(d)}!`, oops:w => `Oh! Dat ass ${w}!`, found:"Du hues de Schatz fonnt!",
    which:"Wéi ee Wee ass richteg?", bonk:"Bum!", notHere:"Net hei!", retry:"Probéier nach eng Kéier!", again:"Nach eng Kéier"},
  // Chinese: 两 before the measure word 步; 先…再…最后… for the order
  zh:{dir:{up:"上", down:"下", left:"左", right:"右"}, where:{up:"上面", down:"下面", left:"左边", right:"右边"},
    one:d => `往${d}走！`, two:(a, b) => `先往${a}走，再往${b}走！`,
    seg:(d, n) => `往${d}走${["", "一", "两", "三", "四"][n]}步`,
    many:s => s.length === 1 ? `${s[0]}！` : s.length === 2 ? `先${s[0]}，再${s[1]}！` : `先${s[0]}，再${s.slice(1, -1).join("，再")}，最后${s[s.length - 1]}！`,
    tap:d => `往${d}！`, oops:w => `哎呀！这是${w}！`, found:"你找到宝藏了！",
    which:"哪条路能走到宝藏？", bonk:"咚！", notHere:"不在这里！", retry:"再试一次！", again:"再听一次"}
};

// walks a list of [direction, steps] from start; stops at the edge or at an obstacle
function walk(start, segs, N, trees){
  let p = start.slice(); const cells = [p];
  for (const [d, n] of segs) for (let k = 0; k < n; k++) {
    const q = [p[0] + DIRS[d][0], p[1] + DIRS[d][1]];
    if (q[0] < 0 || q[1] < 0 || q[0] >= N || q[1] >= N || trees.has(key(...q))) return {cells, end:p, hit:q, dir:d, ok:false};
    p = q; cells.push(p);
  }
  return {cells, end:p, hit:null, ok:true};
}

// level 4: wrong ways that look right (order reversed, numbers swapped, left for right, one step too many), none reaching the treasure
function distractors(r){
  const sig = s => s.map(([d, n]) => d + n).join(" "), seen = new Set([sig(r.segs)]), groups = [[], [], []];
  const copy = () => r.segs.map(s => s.slice());
  const add = (s, g) => {
    const k = sig(s); if (seen.has(k)) return; seen.add(k);
    const w = walk(r.start, s, r.N, r.trees);
    if (w.ok && same(w.end, r.goal)) return; // another good way: not a wrong answer
    groups[g].push(s);
  };
  add(copy().reverse(), 0); // the same steps in the other order: a tree stands in the way
  r.segs.forEach((a, x) => r.segs.forEach((b, y) => { if (x < y && a[1] !== b[1]) { const c = copy(); c[x][1] = b[1]; c[y][1] = a[1]; add(c, 0); } }));
  r.segs.forEach((s, x) => { const c = copy(); c[x][0] = OPP[s[0]]; add(c, 1); });
  r.segs.forEach((s, x) => [-1, 1].forEach(dn => { const c = copy(); c[x][1] += dn; if (c[x][1] >= 1 && c[x][1] <= 4) add(c, 1); }));
  r.segs.forEach((s, x) => perp(s[0]).forEach(p => { const c = copy(); c[x][0] = p; add(c, 2); }));
  const out = shuffle(groups[0]).slice(0, 2), rest = shuffle(groups[1]).concat(shuffle(groups[2]));
  while (out.length < 3 && rest.length) out.push(rest.shift());
  return out;
}

// a grid, a start, the dictated way and the obstacles
function makeRound(lvl){
  const N = lvl === 1 ? 3 : lvl === 2 ? 4 : 5;
  for (;;) {
    const start = [rnd(N), rnd(N)];
    let segs;
    if (lvl === 1) segs = [[one(KEYS), 1]];
    else if (lvl === 2) { const a = one(KEYS); segs = [[a, 1], [one(perp(a)), 1]]; }
    else {
      segs = []; let d = one(KEYS);
      for (let s = rnd(2) ? 3 : 2; s > 0; s--) { segs.push([d, 1 + rnd(3)]); d = one(perp(d)); }
      const steps = segs.reduce((t, [, n]) => t + n, 0);
      if (steps < 3 || steps > (lvl === 3 ? 6 : 7)) continue;
    }
    const w = walk(start, segs, N, new Map());
    if (!w.ok) continue;
    const onPath = new Set(w.cells.map(c => key(...c)));
    if (onPath.size !== w.cells.length) continue; // never twice on the same square
    const trees = new Map();
    const addTree = c => { const k = key(...c); if (!onPath.has(k) && !trees.has(k)) trees.set(k, one(TREES)); };
    if (lvl >= 2) {
      // an obstacle on the way of the reversed order: the order of the directions matters
      const back = walk(start, segs.slice().reverse(), N, new Map()).cells.slice(1).filter(c => !onPath.has(key(...c)));
      if (back.length) addTree(back[0]);
      const want = lvl === 2 ? 2 : lvl === 3 ? 5 : 6;
      for (let t = 0; trees.size < want && t < 60; t++) addTree([rnd(N), rnd(N)]);
    }
    const r = {N, start, segs, goal:w.end, trees};
    if (lvl === 1) r.choices = [segs[0][0], ...pick(KEYS.filter(k => k !== segs[0][0]), 2)];
    if (lvl === 4) { r.wrong = distractors(r); if (r.wrong.length < 3) continue; }
    return r;
  }
}

registerGame({id:"labyrinthe", em:"🧭", name:"Le labyrinthe dicté", desc:"Suis les directions jusqu'au trésor", multi:true, cat:"nombres",
  title:{en:"The Maze", de:"Das Labyrinth", lb:"De Labyrinth", zh:"迷宫"},
  sub:{en:"Up, down, left, right", de:"Oben, unten, links, rechts", lb:"Uewen, ënnen, lénks, riets", zh:"上、下、左、右"}}, function () {
  const lang = ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en"; // never French for the children
  const X = LAB[lang], lvl = levelOf("labyrinthe");
  const total = lvl === 3 ? 5 : 6, res = [];
  startSession("labyrinthe", null, total); const gen = GEN;
  const speak = t => say(t, lang);
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));
  // some voices never say they have finished: never wait for them too long
  const talk = (t, max = 2500) => Promise.race([speak(t), wait(max)]);
  const calm = () => typeof fx === "undefined" || fx.calm();
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };
  const segTxt = ([d, n]) => X.seg(X.dir[d], n);
  const sentence = segs => lvl === 1 ? X.one(X.dir[segs[0][0]]) : lvl === 2 ? X.two(X.dir[segs[0][0]], X.dir[segs[1][0]]) : X.many(segs.map(segTxt));
  let i = 0;

  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const R = makeRound(lvl), hero = one(HEROES), gem = one(GEMS);
    const text = lvl === 4 ? X.which : sentence(R.segs);
    let pos = R.start.slice(), tries = 0, busy = false, done = false;
    const body = $("gameBody"); body.innerHTML = "";
    const wrap = el("div", "lab");
    // the instruction is always written (a chance to read) and said
    const prompt = el("p", "prompt" + (lvl >= 3 ? " lab-long" : ""), `${lvl === 4 ? "🗺️" : hero} ${text}`);
    const tools = el("div", "row"); tools.style.justifyContent = "center";
    tools.append(speakBtn(() => text, X.again, lang));

    // the grid: grass squares, obstacles, the treasure, and the hero sliding on top
    const board = el("div", "lab-board" + (lvl === 4 ? " lab-small" : "")); board.style.setProperty("--n", R.N);
    const cells = [];
    for (let y = 0; y < R.N; y++) for (let x = 0; x < R.N; x++) {
      const c = el("div", "lab-cell" + ((x + y) % 2 ? " lab-alt" : ""));
      const t = R.trees.get(key(x, y)); if (t) c.textContent = t;
      if (same([x, y], R.goal)) c.innerHTML = `<span class="lab-goal">${gem}</span>`;
      cells.push(c); board.append(c);
    }
    const cellAt = p => cells[p[1] * R.N + p[0]];
    const heroEl = el("div", "lab-hero", `<span>${hero}</span>`); board.append(heroEl);
    const place = () => { heroEl.style.transform = `translate(${pos[0] * 100}%, ${pos[1] * 100}%)`; };
    place();
    const stepTo = p => { cellAt(pos).classList.add("lab-trail"); pos = p; place(); sfx.tap(); };
    // a wrong way: the hero bumps and comes back; an obstacle or the edge makes it dizzy
    const bump = d => {
      if (calm()) return;
      const [dx, dy] = DIRS[d], q = [pos[0] + dx, pos[1] + dy];
      heroEl.firstChild.animate([{transform:"none"}, {transform:`translate(${dx * 30}%, ${dy * 30}%) rotate(${(dx || dy) * 8}deg)`}, {transform:"none"}], {duration:360, easing:"ease-out"});
      const blocked = q[0] < 0 || q[1] < 0 || q[0] >= R.N || q[1] >= R.N || R.trees.has(key(...q));
      if (R.trees.has(key(...q))) cellAt(q).animate([{transform:"none"}, {transform:"rotate(-9deg)"}, {transform:"rotate(8deg)"}, {transform:"none"}], {duration:420});
      if (blocked) { const z = el("span", "lab-dizzy", "💫"); heroEl.append(z); setTimeout(() => z.remove(), 900); }
    };
    const arrive = async () => {
      await wait(320); if (!alive(gen)) return;
      const gc = cellAt(R.goal); gc.classList.add("lab-got"); sfx.ok();
      if (gem === "🎁") gc.firstChild.textContent = one(SURPRISES); // the present opens
      if (!calm()) {
        const b = gc.getBoundingClientRect(); fx.sparkle(b.left + b.width / 2, b.top + b.height / 2, 16);
        heroEl.firstChild.animate([{transform:"none"}, {transform:"translateY(-35%) scale(1.15)"}, {transform:"none"}, {transform:"translateY(-18%)"}, {transform:"none"}], {duration:700});
      }
      const first = tries === 0; if (first) addStar();
      logRound(R.segs.map(([d, n]) => (n > 1 ? n + " " : "") + d).join(", "), first, tries + 1, {lvl});
      res.push(first ? 1 : 0); renderDots(res, total, -1);
      await talk(`${praise()} ${X.found}`, 3500);
      if (!alive(gen)) return;
      i++; loops.push(setTimeout(round, TEST ? 30 : 600));
    };

    wrap.append(prompt, tools, board);
    if (lvl <= 3) {
      // levels 1 to 3: one tap on an arrow per step, in the order dictated
      const moves = R.segs.flatMap(([d, n]) => Array(n).fill(d));
      const count = R.segs.flatMap(([, n]) => [...Array(n)].map((_, k) => k + 1)); // the step number inside its direction
      let k = 0;
      const pad = el("div", "lab-pad"), arrows = {};
      const mark = () => { if (!TEST) return; Object.values(arrows).forEach(b => delete b.dataset.ok); if (!done) markOk(arrows[moves[k]]); };
      [["up", "1 / 2"], ["left", "2 / 1"], ["right", "2 / 3"], ["down", "3 / 2"]].forEach(([d, area]) => {
        const b = el("button", "lab-arrow chunky", ARROW[d]); b.style.gridArea = area; b.setAttribute("aria-label", X.dir[d]);
        if (lvl === 1 && !R.choices.includes(d)) b.style.visibility = "hidden"; // three arrows for the little one
        b.onclick = async () => {
          if (busy || done) return; G.taps++;
          if (d === moves[k]) {
            stepTo([pos[0] + DIRS[d][0], pos[1] + DIRS[d][1]]); k++;
            if (k >= moves.length) { done = true; mark(); return arrive(); }
            mark();
            speak(lvl === 3 ? numberIn(count[k - 1], lang) : X.tap(X.dir[d])); // level 3: the steps are counted aloud
            return;
          }
          tries++; busy = true; sfx.ko(); bump(d);
          await talk(X.oops(X.where[d]), 2200);
          if (!alive(gen)) return;
          busy = false; speak(text);
        };
        arrows[d] = b; pad.append(b);
      });
      const mid = el("div", "lab-mid", hero); mid.style.gridArea = "2 / 2"; pad.append(mid);
      wrap.append(pad);
      mark();
    } else {
      // level 4: read the written ways, choose the one that leads to the treasure; the hero walks the chosen one
      const box = el("div", "lab-opts");
      shuffle([{segs:R.segs, ok:true}, ...R.wrong.map(s => ({segs:s}))]).forEach(o => {
        const b = el("button", "lab-opt chunky", o.segs.map((s, n) => `<span class="lab-no">${n + 1}</span>${cap(segTxt(s))}`).join("<br>"));
        if (o.ok) markOk(b);
        b.onclick = async () => {
          if (busy || done || b.disabled) return; G.taps++; busy = true;
          if (o.ok) { done = true; b.classList.add("ok"); box.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok); }
          speak(X.many(o.segs.map(segTxt)));
          const w = walk(R.start, o.segs, R.N, R.trees);
          for (const c of w.cells.slice(1)) { await wait(380); if (!alive(gen)) return; stepTo(c); }
          if (o.ok) return arrive();
          tries++; b.classList.add("ko"); b.disabled = true;
          await wait(250); if (!alive(gen)) return;
          sfx.ko(); if (w.hit) bump(w.dir);
          await talk(w.hit ? X.bonk : X.notHere, 1800);
          if (!alive(gen)) return;
          await wait(400); if (!alive(gen)) return;
          cells.forEach(c => c.classList.remove("lab-trail")); pos = R.start.slice(); place();
          busy = false; talk(X.retry);
        };
        box.append(b);
      });
      wrap.append(box);
    }
    body.append(wrap);
    speak(text);
  };
  round();
});
})();
