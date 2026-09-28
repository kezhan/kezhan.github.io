/* L'Île aux Mots : jeu « Suites logiques ». What comes next? Logic needs no reading, so the little one can play.
   1: AB AB with colours, 3 choices · 2: ABC / AAB with animals and fruit, 4 choices · 3: growing patterns and a gap in the middle
   4: number sequences (by 2, by 3, doubles, counting down), read aloud by the voice of the language being learnt. */
addStyle(`.suite{display:flex; flex-wrap:wrap; justify-content:center; align-items:center; gap:8px; font-size:clamp(34px,8vw,54px); padding:10px; background:#fff; border:3px dashed var(--ink); border-radius:18px}
.suite .trou{display:inline-grid; place-items:center; min-width:1.2em; border-radius:12px; background:var(--sun); animation:flotte 1.4s ease-in-out infinite}
.suite .n{font-family:var(--display); font-weight:700; font-size:.8em}`);
registerGame({id:"suites", em:"🔁", name:"Patterns", desc:"What comes next?", multi:true}, function () {
  const lvl = levelOf("suites"), lang = langOf() === "fr" ? "en" : langOf(); // never French for the children
  const ASK = {en:"What comes next?", de:"Was kommt als Nächstes?", lb:"Wat kënnt duerno?", zh:"下一个是什么？"};
  const GAP = {en:"What is missing?", de:"Was fehlt?", lb:"Wat feelt?", zh:"少了什么？"};
  const COLORS = ["🔴","🔵","🟢","🟡","🟣","🟠"], THINGS = ["🐱","🐶","🐸","🍎","🍌","🍓","⭐","🌙","🚗","⚽"];
  const hole = `<span class="trou">❓</span>`;
  const show = (items, gapAt) => `<div class="suite">${items.map((x, k) => k === gapAt ? hole : `<span>${x}</span>`).join("")}</div>`;
  const choicesFor = (right, pool, n) => [right, ...pick(pool.filter(x => x !== right), n - 1)];
  const round = () => {
    if (lvl === 1) { // AB AB AB ?
      const [a, b] = pick(COLORS, 2), seq = [a, b, a, b, a, b, a];
      return {items: seq.slice(0, 6).concat(["?"]), gap: 6, right: seq[6], choices: choicesFor(seq[6], COLORS, 3)};
    }
    if (lvl === 2) { // ABC ABC or AAB AAB
      const [a, b, c] = pick(THINGS, 3), unit = Math.random() < 0.5 ? [a, b, c] : [a, a, b];
      const seq = unit.concat(unit, unit), cut = 5 + rnd(3);
      return {items: seq.slice(0, cut).concat(["?"]), gap: cut, right: seq[cut], choices: choicesFor(seq[cut], THINGS, 4)};
    }
    if (lvl === 3) { // growing (⭐, ⭐⭐, ⭐⭐⭐, ?) or a gap in the middle of ABB ABB
      if (Math.random() < 0.5) {
        const t = pick(THINGS, 1)[0], k = 1 + rnd(2), seq = [1, 2, 3, 4].map(n => t.repeat(n + k - 1));
        const wrong = [t.repeat(k + 2), t.repeat(k + 4), t.repeat(Math.max(1, k - 1))].filter(x => x !== seq[3]);
        return {items: seq.slice(0, 3).concat(["?"]), gap: 3, right: seq[3], choices: [seq[3], ...wrong.slice(0, 3)]};
      }
      const [a, b] = pick(THINGS, 2), seq = [a, b, b, a, b, b, a, b, b], gap = 1 + rnd(7);
      return {items: seq, gap, right: seq[gap], choices: choicesFor(seq[gap], THINGS, 4), mid: true};
    }
    // level 4: numbers; the voice reads the digits in the language being learnt
    const kinds = [
      () => { const s = 2 + rnd(8), d = 2 + rnd(5); return [0, 1, 2, 3, 4].map(k => s + d * k); },   // + d
      () => { const s = 1 + rnd(3), f = 2; return [0, 1, 2, 3, 4].map(k => s * f ** k); },           // doubles
      () => { const s = 30 + rnd(20), d = 2 + rnd(4); return [0, 1, 2, 3, 4].map(k => s - d * k); },  // counting down
      () => { const t = 2 + rnd(8); return [1, 2, 3, 4, 5].map(k => t * k); }                          // a times table
    ];
    const seq = pick(kinds, 1)[0](), right = seq[4];
    const near = [right + 1, right - 1, right + (seq[1] - seq[0]), right + 10, right - 2].filter(x => x !== right && x > 0);
    return {items: seq.slice(0, 4).map(n => `<span class="n">${n}</span>`).concat(["?"]), gap: 4, right: `<span class="n">${right}</span>`,
      choices: [right, ...pick([...new Set(near)], 3)].map(n => `<span class="n">${n}</span>`), spoken: seq.slice(0, 4).join(", ")};
  };
  const rounds = [...Array(8)].map(() => {
    const r = round(), ask = r.mid ? GAP[lang] : ASK[lang];
    return {lang, say: r.spoken ? `${r.spoken}… ${ask}` : ask, show: `🤔 ${ask}${show(r.items, r.gap)}`, // the question is written too (Kezhan: a chance to read)
      word: r.items.join(" ").replace(/<[^>]+>/g, ""),
      choices: r.choices.map(c => ({html: c, ok: c === r.right}))};
  });
  runQuiz("suites", null, rounds);
});
