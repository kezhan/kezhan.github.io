/* L'Île aux Mots : jeu « Le sudoku en images » (Nombres et logique).
   A Latin square of pictures (animals, fruit from the lexicon): every row and every column holds each picture once; find the missing one.
   The grid is random, and the empty cell asked is always forced by what the grid shows, so it has exactly one right answer.
   1: 2×2, one empty cell, 3 pictures to choose from, the voice names the pictures · 2: 3×3, two empty cells, 4 choices
   3: 4×4, three empty cells, 4 choices · 4: 4×4 with written words instead of pictures, five empty cells, and (in almost every grid)
      one asked first that two words could fill, found only across its row or column (a "hidden single": the right word fits nowhere else);
      the words are not read aloud unless a word is tapped (counted as a hint).
   Tapping a picture in the grid says its name. English, German, Luxembourgish or Chinese, never French.
   À faire relire en luxembourgeois : « Oh nee, … ass schonn an dëser Rei / Kolonn! » (Rei et Kolonn vérifiés sur lod.lu : féminins,
   REI2 et KOLONN1), « An all Rei an an all Kolonn: all Bild nëmmen eemol! », « Wéi ee Wuert feelt? », « Net …! Kuck nach eng Kéier. »,
   « Alles passt! ». En luxembourgeois, seuls les mots du lexique sont entendus (enregistrements lod.lu) : le reste est écrit à l'écran. */
(() => {
const ID = "sudoku";
addStyle(`
.sdk-top{display:flex; gap:8px 10px; align-items:center; justify-content:center; flex-wrap:wrap}
.sdk-top .speak{font-size:19px; padding:10px 16px}
.sdk-msg{min-height:1.4em; text-align:center; font-family:var(--display); font-weight:600; font-size:clamp(17px,4.4vw,22px); line-height:1.2}
.sdk-msg.good{color:var(--good)} .sdk-msg.bad{color:var(--bad)}
.sdk-grid{display:grid; gap:6px; padding:6px; margin:0 auto; width:min(100%, var(--sdk-w)); background:var(--ink); border-radius:18px; box-shadow:4px 5px 0 rgba(27,45,69,.3)}
.sdk-n2{--sdk-w:280px; --sdk-fs:clamp(56px,17vw,84px)} .sdk-n3{--sdk-w:330px; --sdk-fs:clamp(40px,12vw,64px)} .sdk-n4{--sdk-w:370px; --sdk-fs:clamp(30px,9vw,52px)}
.sdk-cell{aspect-ratio:1; min-width:0; display:grid; place-items:center; padding:2px; border:0; border-radius:11px; background:#fff; color:var(--ink); font-size:var(--sdk-fs); line-height:1; position:relative; transition:background .2s}
.sdk-cell.lane{background:#FFF3C4}
.sdk-cell.hole{background:#DDF1F8; box-shadow:inset 0 0 0 3px #9CCFE3}
.sdk-cell.ask{background:var(--sun); box-shadow:inset 0 0 0 4px var(--coral)}
.sdk-cell.ask .sdk-q{display:inline-block; font-size:.8em; animation:sdk-hop 1s ease-in-out infinite}
.sdk-cell.ko{box-shadow:inset 0 0 0 4px var(--bad); animation:shake .4s}
.sdk-hand{position:absolute; top:-10px; right:-8px; font-size:26px; line-height:1; animation:sdk-wave .45s ease-in-out 3; pointer-events:none}
.sdk-w{display:flex; flex-direction:column; align-items:center; font-family:var(--display); font-weight:600; font-size:clamp(13px,3.6vw,19px); line-height:1.05; max-width:100%; overflow-wrap:anywhere; text-align:center}
.sdk-w small{font-family:var(--body); font-weight:800; font-size:.7em; color:var(--ink-soft)}
.sdk-pal{display:flex; flex-wrap:wrap; justify-content:center; gap:10px}
.sdk-pal .sdk-ch{width:clamp(60px,17vw,96px); aspect-ratio:1; display:grid; place-items:center; padding:0; background:#fff; font-size:clamp(34px,10vw,56px); line-height:1}
.sdk-pal.big .sdk-ch{width:clamp(80px,24vw,120px); font-size:clamp(48px,14vw,76px)}
.sdk-pal.words{display:grid; grid-template-columns:repeat(2,minmax(0,170px)); justify-content:center}
.sdk-pal.words .sdk-ch{width:100%; aspect-ratio:auto; min-height:62px; padding:6px 10px}
.sdk-pal.words .sdk-w{font-size:clamp(18px,5vw,24px)}
.sdk-ch.ko{background:#FFD6CF; animation:shake .4s} .sdk-ch.ok{background:#C9F2DF}
.sdk-fly{position:fixed; pointer-events:none; z-index:60; line-height:1; will-change:transform}
@keyframes sdk-hop{50%{transform:translateY(-6px) scale(1.12)}}
@keyframes sdk-wave{50%{transform:rotate(-22deg)}}
`);

// the lexicon words used as pictures: singular nouns only, so that "the cat" / "die Katze" / "d'Kaz" / "猫" reads well in every sentence
const ANIMALS = ["cat","dog","cow","pig","duck","horse","lion","frog","fish","bird","rabbit","monkey","elephant","bear","mouse","sheep","chicken",
  "tiger","giraffe","zebra","snake","penguin","turtle","owl","whale"];
const FRUITS = ["apple","banana","strawberry","orange","pear","watermelon","lemon","peach","pineapple"];
const LOOKALIKE = [["orange","peach"]]; // 🍊 and 🍑 look alike on some screens: never in the same grid
const clash = (a, b) => LOOKALIKE.some(([x, y]) => (a.en === x && b.en === y) || (a.en === y && b.en === x));
// n pictures for the grid, then the extra ones for the choices, with no two look-alikes together
function pickApart(pool, n, more){
  const out = [];
  for (const w of shuffle(pool)) if (out.length < n + more && !out.some(o => clash(o, w))) out.push(w);
  return [out.slice(0, n), out.slice(n)];
}

/* ---------- the grid: a random Latin square, and an order of empty cells each forced by what is shown ---------- */
const range = n => [...Array(n).keys()];
function latin(n){
  // cyclic square, or for 4 the Klein square (i xor j), with rows, columns and pictures shuffled
  const base = n === 4 && Math.random() < 0.5 ? (i, j) => i ^ j : (i, j) => (i + j) % n;
  const R = shuffle(range(n)), C = shuffle(range(n)), P = shuffle(range(n));
  return R.map(r => C.map(c => P[base(r, c)]));
}
// Fills the empty cells one by one: each step is a naked single (one picture left for that cell) or, when hard,
// a hidden single (the picture fits in no other empty cell of that row or column). Returns null if the grid gets stuck.
function solveOrder(sol, holes, hard){
  const n = sol.length, g = sol.map(r => r.slice());
  holes.forEach(([r, c]) => { g[r][c] = -1; });
  let left = holes.slice(), hidden = false;
  const seq = [];
  const cand = ([r, c]) => range(n).filter(s => !g[r].includes(s) && !g.some(row => row[c] === s));
  const isHidden = x => {
    const s = sol[x[0]][x[1]], others = o => o !== x && cand(o).includes(s);
    return !left.some(o => o[0] === x[0] && others(o)) || !left.some(o => o[1] === x[1] && others(o));
  };
  while (left.length) {
    const naked = left.filter(x => cand(x).length === 1);
    const hid = hard ? left.filter(x => cand(x).length > 1 && isHidden(x)) : [];
    let next;
    if (hid.length && (!hidden || !naked.length)) { next = hid[rnd(hid.length)]; hidden = true; }
    else if (naked.length) next = naked[rnd(naked.length)];
    else return null;
    seq.push(next); g[next[0]][next[1]] = sol[next[0]][next[1]];
    left = left.filter(x => x !== next);
  }
  return {seq, hidden};
}
function makePuzzle(n, holes, hard){
  const all = range(n * n).map(i => [Math.floor(i / n), i % n]);
  for (let t = 0; t < 500; t++) {
    const sol = latin(n), o = solveOrder(sol, pick(all, holes), hard);
    if (o && (!hard || o.hidden || t > 400)) return {sol, order: o.seq};
  }
  // never reached in practice: one empty cell per row, always a naked single
  const sol = latin(n);
  return {sol, order: shuffle(range(n)).slice(0, Math.min(holes, n)).map((c, r) => [r, c])};
}

registerGame({id:ID, em:"🧩", name:"Sudoku en images", multi:true, cat:"nombres",
  title:{en:"Picture sudoku", de:"Bilder-Sudoku", lb:"Biller-Sudoku", zh:"图片数独"},
  sub:{en:"Which one is missing?", de:"Was fehlt?", lb:"Wat feelt?", zh:"少了哪一个？"}}, function () {
  const lvl = levelOf(ID), lang = ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
  const CFG = {1:{n:2, holes:1, extra:1, grids:6}, 2:{n:3, holes:2, extra:1, grids:6}, 3:{n:4, holes:3, extra:0, grids:6}, 4:{n:4, holes:5, extra:0, grids:5}}[lvl];
  const words = lvl === 4;
  const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
  // German nominative from the lexicon ("die Katze ist schon…"); Luxembourgish never starts a sentence, so "d'Kaz" is found in the lod.lu recordings
  const TX = {
    en:{ask:"Which one is missing?", askW:"Which word is missing?", rule:"Every row ➡️ and every column ⬇️: each picture only once!", ruleW:"Every row ➡️ and every column ⬇️: each word only once!",
      yes:w => `Yes! The ${w.en}!`, row:w => `Oops, the ${w.en} is already in this row!`, col:w => `Oops, the ${w.en} is already in this column!`, no:w => `Not the ${w.en}! Look again.`,
      full:"Everything fits!", again:"Again"},
    de:{ask:"Was fehlt hier?", askW:"Welches Wort fehlt?", rule:"In jeder Reihe ➡️ und jeder Spalte ⬇️: jedes Bild nur einmal!", ruleW:"In jeder Reihe ➡️ und jeder Spalte ⬇️: jedes Wort nur einmal!",
      yes:w => `Ja! ${cap(w.de)}!`, row:w => `Hoppla, ${w.de} ist schon in dieser Reihe!`, col:w => `Hoppla, ${w.de} ist schon in dieser Spalte!`, no:w => `Nicht ${w.de}! Schau noch mal.`,
      full:"Alles passt!", again:"Nochmal"},
    lb:{ask:"Wat feelt hei?", askW:"Wéi ee Wuert feelt?", rule:"An all Rei ➡️ an an all Kolonn ⬇️: all Bild nëmmen eemol!", ruleW:"An all Rei ➡️ an an all Kolonn ⬇️: all Wuert nëmmen eemol!",
      yes:w => `Jo, dat ass ${w.lb}!`, row:w => `Oh nee, ${w.lb} ass schonn an dëser Rei!`, col:w => `Oh nee, ${w.lb} ass schonn an dëser Kolonn!`, no:w => `Net ${w.lb}! Kuck nach eng Kéier.`,
      full:"Alles passt!", again:"Nach eng Kéier"},
    zh:{ask:"这里少了什么？", askW:"这里少了哪个词？", rule:"每一行➡️、每一列⬇️，每个图片只出现一次！", ruleW:"每一行➡️、每一列⬇️，每个词只出现一次！",
      yes:w => `对了！是${w.zh}！`, row:w => `哎呀，${w.zh}已经在这一行了！`, col:w => `哎呀，${w.zh}已经在这一列了！`, no:w => `不是${w.zh}哦，再看看！`,
      full:"全都对了！", again:"再听一次"}
  }[lang];
  const askTxt = words ? TX.askW : TX.ask, ruleTxt = words ? TX.ruleW : TX.rule;
  const plain = s => s.replace(/\s?[➡⬇]️?/g, ""); // the arrows ➡️ ⬇️ are for the eyes, not for the voice
  const speak = t => say(t, lang);
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };

  // level 4 shows words: the article small above the noun, and only nouns short enough for a 4×4 cell on a phone
  const ART = /^(der|die|das|den|de|d')\s?(.+)$/;
  const noun = w => (lang === "de" || lang === "lb") ? w[lang].replace(ART, "$2") : w[lang];
  const short = w => lang === "zh" ? w.zh.length <= 3 : noun(w).length <= 8;
  const wordHtml = w => {
    const m = (lang === "de" || lang === "lb") && w[lang].match(ART);
    return m ? `<span class="sdk-w"><small>${m[1]}</small>${m[2]}</span>` : `<span class="sdk-w">${w[lang]}</span>`;
  };
  const face = w => words ? wordHtml(w) : w.e;
  const poolOf = kind => {
    const theme = kind ? "food" : "animals", names = kind ? FRUITS : ANIMALS;
    const ws = THEMES[theme].words.filter(w => names.includes(w.en) && (w.lvl || 1) <= Math.min(3, lvl) && (!words || short(w)));
    return ws.length > CFG.n + CFG.extra ? ws : THEMES.animals.words.filter(w => ANIMALS.includes(w.en) && (!words || short(w)));
  };

  const DANCES = [
    [{transform:"none"}, {transform:"translateY(-18px) scale(1.15)"}, {transform:"none"}],
    [{transform:"rotate(0)"}, {transform:"rotate(360deg) scale(1.1)"}, {transform:"rotate(360deg)"}],
    [{transform:"none"}, {transform:"scale(1.25,.8)"}, {transform:"scale(.85,1.2)"}, {transform:"none"}]
  ];
  // the right picture flies from its button into the cell
  const fly = (from, to, html) => new Promise(res => {
    if (fx.calm()) return res();
    const a = from.getBoundingClientRect(), b = to.getBoundingClientRect();
    const x0 = a.left + a.width / 2, y0 = a.top + a.height / 2, dx = b.left + b.width / 2 - x0, dy = b.top + b.height / 2 - y0;
    const d = el("div", "sdk-fly", html);
    d.style.left = x0 + "px"; d.style.top = y0 + "px"; d.style.fontSize = getComputedStyle(to).fontSize;
    document.body.append(d);
    let over = false; const end = () => { if (!over) { over = true; d.remove(); res(); } };
    try {
      d.animate([{transform:"translate(-50%,-50%) scale(1)"},
        {transform:`translate(calc(-50% + ${dx * .5}px), calc(-50% + ${dy * .5 - 60}px)) scale(1.35)`, offset:.5},
        {transform:`translate(calc(-50% + ${dx}px), calc(-50% + ${dy}px)) scale(1)`}], {duration:460, easing:"ease-in-out"}).onfinish = end;
    } catch(e) { end(); }
    setTimeout(end, 900);
  });

  const total = CFG.grids, res = [], firstKind = rnd(2);
  let k = 0;
  startSession(ID, null, total); const gen = GEN;

  const nextGrid = () => {
    if (!alive(gen)) return;
    if (k >= total) return finish();
    renderDots(res, total, k);
    const n = CFG.n, pool = poolOf((k + firstKind) % 2);
    const [syms, extra] = pickApart(pool, n, CFG.extra);
    const {sol, order} = makePuzzle(n, CFG.holes, words);
    const shown = sol.map(r => r.slice());
    order.forEach(([r, c]) => { shown[r][c] = -1; });
    const firstGrid = k === 0, lanes = lvl <= 3;
    let step = 0, errors = 0, busy = false, voice = 0;

    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("p", "prompt", `🧩 ${askTxt}<small>${ruleTxt}</small>`));
    const top = el("div", "sdk-top"), again = el("button", "speak chunky", `🔊 <span>${TX.again}</span>`);
    top.append(again); body.append(top);
    const msg = el("div", "sdk-msg"); msg.setAttribute("aria-live", "polite");
    const setMsg = (t, cls) => { msg.textContent = t; msg.className = "sdk-msg" + (cls ? " " + cls : ""); };
    const grid = el("div", `sdk-grid sdk-n${n}`); grid.style.gridTemplateColumns = `repeat(${n},1fr)`;
    const cells = range(n).map(r => range(n).map(c => {
      const cell = el("button", "sdk-cell");
      cell.onclick = () => {
        const v = shown[r][c], cur = order[step]; voice++;
        if (v < 0) { if (cur && cur[0] === r && cur[1] === c) speak(askTxt); return; }
        G.taps++; if (words) G.hints++; // at level 4 hearing a word is a reading help
        fx.bounce(cell); speak(syms[v][lang]);
      };
      grid.append(cell);
      return cell;
    }));
    const pal = el("div", "sdk-pal" + (lvl === 1 ? " big" : words ? " words" : ""));
    const opts = shuffle([...syms.map((w, s) => ({w, s})), ...extra.map(w => ({w, s: -1}))]);
    body.append(msg, grid, pal);

    const paint = () => {
      const cur = order[step];
      cells.forEach((row, r) => row.forEach((cell, c) => {
        const v = shown[r][c], isAsk = !!cur && cur[0] === r && cur[1] === c;
        const lane = lanes && cur && !isAsk && (cur[0] === r || cur[1] === c);
        cell.className = "sdk-cell" + (v < 0 ? (isAsk ? " ask" : " hole") : lane ? " lane" : "");
        const key = v + (isAsk ? "?" : "");
        if (cell.dataset.v !== key) { cell.dataset.v = key; cell.innerHTML = v < 0 ? (isAsk ? `<span class="sdk-q">❓</span>` : "") : face(syms[v]); }
      }));
    };
    const ask = () => {
      paint();
      opts.forEach(o => { delete o.b.dataset.ok; o.b.classList.remove("ko"); });
      const [r, c] = order[step];
      markOk(opts.find(o => o.s === sol[r][c]).b);
    };
    const bounceSym = v => cells.forEach((row, r) => row.forEach((cell, c) => { if (shown[r][c] === v) fx.bounce(cell); }));
    // levels 1 to 3: the voice names the pictures of the grid, then asks; the rule is said on the first grid (level 2 and up)
    const talk = async () => {
      const my = ++voice, go = () => my === voice && alive(gen);
      if (!words) {
        const seen = [...new Set(shown.flat().filter(v => v >= 0))];
        for (const v of seen) { if (!go()) return; bounceSym(v); await speak(syms[v][lang]); }
      }
      if (!go()) return;
      if (firstGrid && lvl >= 2) { await speak(plain(ruleTxt)); if (!go()) return; }
      await speak(askTxt);
    };
    again.onclick = () => { G.replays++; talk(); };

    const done = async w => {
      const first = errors === 0;
      if (first) addStar();
      logRound(`${n}x${n} ${order.map(([r, c]) => syms[sol[r][c]].en).join(" ")}`, first, errors + 1, {lvl});
      res.push(first ? 1 : 0); renderDots(res, total, -1);
      const cheer = `${praise()} ${TX.full}`;
      setMsg(cheer, "good");
      tone([523, 659, 784, 1047, 1319], .07);
      if (!fx.calm()) {
        const f = DANCES[rnd(DANCES.length)];
        cells.forEach((row, r) => row.forEach((cell, c) => { try { cell.animate(f, {duration:650, delay:(r + c) * 90, easing:"ease-in-out"}); } catch(e) {} }));
        try { grid.animate([{background:"#1B2D45"}, {background:"#FFC43D"}, {background:"#43AA8B"}, {background:"#1B2D45"}], {duration:1300}); } catch(e) {}
        const g = grid.getBoundingClientRect(); fx.sparkle(g.left + g.width / 2, g.top + g.height / 2, 16);
      }
      await speak(TX.yes(w)); if (!alive(gen)) return;
      await speak(cheer); if (!alive(gen)) return;
      k++; loops.push(setTimeout(nextGrid, TEST ? 50 : 700));
    };

    opts.forEach(o => {
      const b = el("button", "sdk-ch chunky", face(o.w)); o.b = b;
      b.onclick = async () => {
        if (busy) return;
        G.taps++; voice++;
        const [r, c] = order[step], right = sol[r][c];
        if (o.s === right) {
          busy = true; opts.forEach(x => delete x.b.dataset.ok);
          b.classList.add("ok"); sfx.ok(); setMsg(TX.yes(o.w), "good");
          const cell = cells[r][c];
          await fly(b, cell, face(o.w)); if (!alive(gen)) return;
          b.classList.remove("ok");
          shown[r][c] = right; step++; paint();
          fx.bounce(cell);
          if (!fx.calm()) { const q = cell.getBoundingClientRect(); fx.sparkle(q.left + q.width / 2, q.top + q.height / 2, 8); }
          if (step >= order.length) return done(o.w);
          speak(TX.yes(o.w)); busy = false; ask();
          return;
        }
        // wrong: say why when the picture is already in the row or the column, and let that one wave "I'm here!"
        errors++; sfx.ko(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
        let text = TX.no(o.w), there = null;
        if (o.s >= 0) {
          const inRow = shown[r].indexOf(o.s), inCol = shown.findIndex(row => row[c] === o.s);
          if (inRow >= 0) { text = TX.row(o.w); there = cells[r][inRow]; }
          else if (inCol >= 0) { text = TX.col(o.w); there = cells[inCol][c]; }
        }
        if (there) {
          there.classList.remove("ko"); void there.offsetWidth; there.classList.add("ko");
          const hand = el("span", "sdk-hand", "✋"); there.append(hand);
          loops.push(setTimeout(() => { hand.remove(); there.classList.remove("ko"); }, 1600));
        }
        setMsg(text, "bad"); speak(text);
      };
      pal.append(b);
    });

    ask();
    talk();
  };
  nextGrid();
});
})();
