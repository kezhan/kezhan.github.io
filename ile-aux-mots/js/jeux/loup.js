/* L'Île aux Mots : jeu « Quelle heure est-il, Monsieur le Loup ? » (atelier de conception, 7,0/10).
   The rabbit walks to the wolf; the wolf says a time, the child picks the right clock and the rabbit hops that many squares,
   counted aloud. Sometimes the wolf turns round: "DINNER TIME!", run to the burrow.
   1: "Three steps!", tap the rabbit that many times · 2: o'clock · 3: half past, quarter past, quarter to · 4: read the clock, choose the written time.
   Everything the child sees or hears is in the language being learnt (en, de, lb, zh), never French. */
addStyle(`
.pre{display:flex; flex-wrap:wrap; gap:4px; padding:10px; background:#BDEBA0; border:3px solid var(--ink); border-radius:18px; position:relative}
.pre .case{width:clamp(22px,6vw,34px); height:clamp(22px,6vw,34px); border-radius:8px; background:#8BC34A55; display:grid; place-items:center; font-size:clamp(18px,5vw,28px)}
.pre .case.depart{background:#795548; color:#fff}
.pre .case.loup{background:#FFD6CF}
.loup-dos{display:inline-block; transform:scaleX(-1)}
.bulle{font-family:var(--display); font-size:22px; font-weight:600; background:#fff; padding:12px 18px; align-self:center}
.horloges{display:grid; gap:12px; grid-template-columns:repeat(auto-fit,minmax(110px,1fr))}
.horloges .choice{aspect-ratio:1; padding:8px}
.horloges svg{width:100%; height:100%}
.horloges .choice.phrase{aspect-ratio:auto; min-height:80px; font-size:20px; font-family:var(--display); font-weight:600}
.lapin{font-size:64px; background:none; border:0; align-self:center}
.terrier{font-size:26px; font-family:var(--display); font-weight:700; background:var(--coral); color:#fff; padding:14px 22px; align-self:center; animation:flotte .5s ease-in-out infinite}
`);
registerGame({id:"loup", em:"🐺", name:"Mr Wolf", desc:"What's the time?", multi:true}, function () {
  const lvl = levelOf("loup"), lang = langOf() === "fr" ? "en" : langOf(); // never French for the children
  const DE = ["null","eins","zwei","drei","vier","fünf","sechs","sieben","acht","neun","zehn","elf","zwölf"];
  const LB = ["null","eent","zwee","dräi","véier","fënnef","sechs","siwen","aacht","néng","zéng","eelef","zwielef"];
  const ZH = ["零","一","二","三","四","五","六","七","八","九","十","十一","十二"];
  const num = n => ({en: numberWords(n), de: DE[n], lb: LB[n], zh: ZH[n]})[lang];
  const nxt = h => h % 12 + 1;
  // time phrases; German and Luxembourgish "halb vier" is 3:30, Chinese two o'clock is 两点
  const TIME = {
    en: (h, m) => m === 0 ? `${numberWords(h)} o'clock` : m === 30 ? `half past ${numberWords(h)}` : m === 15 ? `quarter past ${numberWords(h)}` : m === 45 ? `quarter to ${numberWords(nxt(h))}`
      : m < 30 ? `${numberWords(m)} past ${numberWords(h)}` : `${numberWords(60 - m)} to ${numberWords(nxt(h))}`,
    de: (h, m) => m === 0 ? `${h === 1 ? "ein" : DE[h]} Uhr` : m === 30 ? `halb ${DE[nxt(h)]}` : m === 15 ? `Viertel nach ${DE[h]}` : `Viertel vor ${DE[nxt(h)]}`,
    lb: (h, m) => m === 0 ? `${h === 1 ? "eng" : LB[h]} Auer` : m === 30 ? `hallwer ${LB[nxt(h)]}` : m === 15 ? `Véierel op ${LB[h]}` : `Véierel vir ${LB[nxt(h)]}`,
    zh: (h, m) => `${h === 2 ? "两" : ZH[h]}点${m === 0 ? "" : m === 30 ? "半" : m === 15 ? "一刻" : "三刻"}`
  };
  const TXT = {
    en: {ask: "What's the time, Mr Wolf?", its: t => `It's ${t}!`, steps: n => n === 1 ? "One step!" : `${numberWords(n)} steps!`, dinner: "It's DINNER TIME!", run: "Run home! 🏠", pick: "Which clock?", read: "What time is it?", tap: "Tap the rabbit!", safe: "Phew! Safe!", caught: "Caught! Tickle tickle!", win: "You got Mr Wolf!"},
    de: {ask: "Wie spät ist es, Herr Wolf?", its: t => `Es ist ${t}!`, steps: n => n === 1 ? "Ein Schritt!" : `${DE[n]} Schritte!`, dinner: "Essenszeit!", run: "Lauf nach Hause! 🏠", pick: "Welche Uhr?", read: "Wie spät ist es?", tap: "Tipp auf den Hasen!", safe: "Puh! Gerettet!", caught: "Erwischt! Kille kille!", win: "Du hast Herrn Wolf erwischt!"},
    lb: {ask: "Wéi vill Auer ass et, Här Wollef?", its: t => `Et ass ${t}!`, steps: n => n === 1 ? "Ee Schrëtt!" : `${LB[n]} Schrëtt!`, dinner: "Et ass Iessenszäit!", run: "Laf heem! 🏠", pick: "Wéi eng Auer?", read: "Wéi vill Auer ass et?", tap: "Tipp op den Hues!", safe: "Ouf! Gerett!", caught: "Erwëscht!", win: "Du hues den Här Wollef erwëscht!"},
    zh: {ask: "狼先生，几点了？", its: t => `${t}！`, steps: n => `走${n === 2 ? "两" : ZH[n]}步！`, dinner: "吃饭时间到了！", run: "快跑回家！🏠", pick: "哪个钟？", read: "几点了？", tap: "点一点小兔子！", safe: "呼！安全了！", caught: "抓到了！", win: "你抓到狼先生了！"}
  };
  const tx = TXT[lang];
  const speak = t => say(t, lang);
  // a clock drawn in SVG, so any time can be shown (emoji only have o'clock and half past)
  const clock = (h, m) => {
    const ah = ((h % 12) + m / 60) * 30, am = m * 6;
    const ticks = [...Array(12)].map((_, k) => { const a = k * 30 * Math.PI / 180; return `<line x1="${50 + 38 * Math.sin(a)}" y1="${50 - 38 * Math.cos(a)}" x2="${50 + 44 * Math.sin(a)}" y2="${50 - 44 * Math.cos(a)}" stroke="#1B2D45" stroke-width="${k % 3 ? 2 : 4}"/>`; }).join("");
    return `<svg viewBox="0 0 100 100" aria-label="${h}:${String(m).padStart(2, "0")}"><circle cx="50" cy="50" r="47" fill="#FFFBF2" stroke="#1B2D45" stroke-width="4"/>${ticks}
      <line x1="50" y1="50" x2="${50 + 24 * Math.sin(ah * Math.PI / 180)}" y2="${50 - 24 * Math.cos(ah * Math.PI / 180)}" stroke="#1B2D45" stroke-width="6" stroke-linecap="round"/>
      <line x1="50" y1="50" x2="${50 + 36 * Math.sin(am * Math.PI / 180)}" y2="${50 - 36 * Math.cos(am * Math.PI / 180)}" stroke="#E53935" stroke-width="4" stroke-linecap="round"/>
      <circle cx="50" cy="50" r="4" fill="#1B2D45"/></svg>`;
  };
  const minutes = lvl <= 2 ? [0] : lvl === 3 ? [0, 30, 15, 45] : lang === "en" ? [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55] : [0, 30, 15, 45];
  const randTime = () => [1 + rnd(12), pick(minutes, 1)[0]];
  const board = lvl === 1 ? 12 : 30;
  let pos = 0, turn = 0, tries = 0, busy = false;
  const res = [];
  startSession("loup", null, lvl === 1 ? 6 : 5); const gen = GEN;
  const body = $("gameBody");
  const draw = (wolfFacing) => {
    body.innerHTML = "";
    const pre = el("div", "pre");
    for (let k = 0; k <= board; k++) {
      const c = el("div", "case" + (k === 0 ? " depart" : "") + (k === board ? " loup" : ""), k === pos ? "🐰" : k === 0 ? "🕳️" : k === board ? `<span class="${wolfFacing ? "" : "loup-dos"}">🐺</span>` : "");
      pre.append(c);
    }
    body.append(pre);
    return pre;
  };
  const hop = async (n, dir = 1) => { // the rabbit hops square by square, counted aloud
    for (let k = 1; k <= n && alive(gen); k++) {
      pos = Math.max(0, Math.min(board, pos + dir)); draw(false); sfx.tap();
      await speak(num(Math.min(k, 12)) || String(k));
    }
  };
  const dinner = async () => { // the wolf turns round: two seconds to reach the burrow
    draw(true); sfx.ko(); speak(tx.dinner);
    const b = el("button", "terrier chunky", tx.run); markOk(b); body.append(b);
    const safe = await new Promise(r => { b.onclick = () => r(true); loops.push(setTimeout(() => r(false), TEST ? 1500 : 2600)); });
    if (!alive(gen)) return;
    if (safe) { sfx.ok(); await speak(tx.safe); }
    else { await speak(tx.caught); pos = Math.max(0, pos - 3); }
  };
  const next = async () => {
    if (!alive(gen)) return;
    if (pos >= board) {
      draw(true); if (typeof fx !== "undefined") fx.sparkle(innerWidth / 2, innerHeight / 3, 20);
      await speak(tx.win); return finish();
    }
    draw(false); busy = false; tries = 0; turn++;
    renderDots(res, Math.max(res.length + 1, 5), res.length);
    const ask = el("button", "bulle chunky", "🐰 " + tx.ask); markOk(ask); body.append(ask);
    ask.onclick = async () => {
      if (busy) return; busy = true; G.taps++; ask.remove();
      await speak(tx.ask);
      if (lvl === 1) return stepsRound();
      const [h, m] = randTime(), phrase = TIME[lang](h, m);
      if (lvl === 4) return readRound(h, m, phrase);
      await speak(tx.its(phrase));
      clockRound(h, m, phrase);
    };
  };
  const scored = async (ok, word, steps) => {
    if (ok && tries === 0) addStar();
    logRound(word, ok && tries === 0, tries + 1, {lvl}); res.push(ok && tries === 0 ? 1 : 0);
    if (ok) await hop(steps);
    if (!alive(gen)) return;
    // the closer to the wolf, the more often he turns round
    if (pos < board && Math.random() < 0.15 + 0.4 * pos / board) await dinner();
    next();
  };
  const choices = (items, good, render, isText) => {
    const grid = el("div", "horloges");
    shuffle(items).forEach(it => {
      const c = el("button", "choice chunky" + (isText ? " phrase" : ""), render(it));
      if (it === good) markOk(c);
      c.onclick = () => {
        if (c.dataset.done) return; G.taps++;
        if (it === good) { grid.querySelectorAll("button").forEach(x => x.dataset.done = 1); c.classList.add("ok"); sfx.ok(); good.go(); }
        else { tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko"); if (!isText) speak(TIME[lang](it[0], it[1])); }
      };
      grid.append(c);
    });
    return grid;
  };
  const clockRound = (h, m, phrase) => {
    const good = [h, m]; good.go = () => scored(true, phrase, h);
    const others = []; while (others.length < (lvl === 2 ? 2 : 3)) { const t = randTime(); if (!(t[0] === h && t[1] === m) && !others.some(o => o[0] === t[0] && o[1] === t[1])) others.push(t); }
    body.append(el("p", "prompt", `${tx.its(phrase)}<small>${tx.pick}</small>`), choices([good, ...others], good, t => clock(t[0], t[1]), false));
  };
  const readRound = (h, m, phrase) => { // level 4: the clock is shown, choose the written time
    const good = [h, m]; good.go = () => scored(true, phrase, h);
    const others = []; while (others.length < 3) { const t = randTime(); if (!(t[0] === h && t[1] === m) && !others.some(o => o[0] === t[0] && o[1] === t[1])) others.push(t); }
    const face = el("div", "", clock(h, m)); face.style.cssText = "width:min(46vw,200px); align-self:center";
    body.append(el("p", "prompt", tx.read), face, choices([good, ...others], good, t => TIME[lang](t[0], t[1]), true));
    speak(tx.read);
  };
  const stepsRound = async () => { // level 1: "Three steps!", tap the rabbit three times
    const n = 1 + rnd(pos > board - 4 ? board - pos : 4);
    body.append(el("p", "prompt", `${tx.steps(n)}<small>${tx.tap}</small>`));
    const lapin = el("button", "lapin", "🐰"); markOk(lapin); body.append(lapin);
    await speak(tx.steps(n));
    let count = 0, timer = null;
    const check = async () => {
      lapin.disabled = true;
      if (count === n) { sfx.ok(); scored(true, `${n} steps`, 0); }
      else { tries++; sfx.ko(); await speak(tx.steps(n)); pos = Math.max(0, pos - count); count = 0; draw(false); scored(false, `${n} steps`, 0); }
    };
    lapin.onclick = async () => {
      G.taps++; clearTimeout(timer);
      if (TEST) { count = n; pos = Math.min(board, pos + n); return check(); } // the recette taps once for the whole count
      count++; pos = Math.min(board, pos + 1); if (typeof fx !== "undefined") fx.bounce(lapin);
      speak(num(Math.min(count, 12)));
      // the count is validated after 1.5 s without a tap
      timer = setTimeout(check, 1500); loops.push(timer);
    };
  };
  next();
});
