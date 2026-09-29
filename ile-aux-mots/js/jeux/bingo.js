/* L'Île aux Mots : jeu « bingo » (Bingo des mots).
   A card of pictures; the parrot turns the bingo machine and calls a word; the child puts a counter on that picture.
   A full line (row, column, and the diagonals from 3×3) is BINGO, with a big party. Every word called is one round.
   1: 2×2 card, easy words, one theme · 2: 3×3 card, one theme per card
   3: 3×3, themes mixed, sound-alike traps on the card, and some words called are NOT on the card (the red 🙅 button)
   4: 4×4, the word is only written, no voice (the little one keeps the voice), traps and words not on the card.
   English, German, Luxembourgish or Chinese, never French.
   Luxembourgish checked on lod.lu: leeën (lee!), drécken (dréck!), kucken (kuck!), fannen (fann!), sichen (sich!), oppassen (opgepasst),
   lauschteren (lauschter!), liesen (lies!), Jeton (m.), Kaart (f.), Rei (f., a line of things), Bild (n.), Knäppchen (m.), nei, voll, elo, rout.
   Eifel rule kept: "e Jeton", "a lee", "a fann", "de roude Knäppchen". Still to be read by a speaker: the calls "Elo: …" and "Wien huet …?",
   "Dréck op de roude Knäppchen!", German "Chip" for the counter, Chinese "盖住" (cover), and the sound-alike pairs of each language. */
(() => {
const ID = "bingo";
const LG = () => ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
const calm = () => fx.calm();
const anim = (n, frames, o) => { if (!n || calm()) return null; try { return n.animate(frames, o); } catch(e) { return null; } };
const any = a => a[rnd(a.length)];
const NS = "http://www.w3.org/2000/svg";
const THEMES_OK = ["animals","food","body","things","clothes","jobs"]; // nouns with a clear picture
const COLORS = ["#FF6F59","#36B5D8","#43AA8B","#FFC43D","#B388EB","#FF8FB1"];

const TX = {
  title: {en:"Picture Bingo", de:"Bilder-Bingo", lb:"Biller-Bingo", zh:"看图宾果"},
  sub: {en:"Listen and find the picture!", de:"Hör zu und finde das Bild!", lb:"Lauschter a fann d'Bild!", zh:"听一听，找图片！"},
  listen: {en:"Listen and put a counter on the picture!", de:"Hör zu und leg einen Chip auf das Bild!", lb:"Lauschter a lee e Jeton op d'Bild!", zh:"听一听，把听到的图片盖住！"},
  read: {en:"Read and put a counter on the picture!", de:"Lies und leg einen Chip auf das Bild!", lb:"Lies a lee e Jeton op d'Bild!", zh:"读一读，把对的图片盖住！"},
  goal: {en:"A full line is BINGO!", de:"Eine volle Reihe ist BINGO!", lb:"Eng voll Rei ass BINGO!", zh:"连成一条线就是宾果！"},
  nopeHelp: {en:"Not on your card? Tap the red button!", de:"Nicht auf deiner Karte? Tipp auf den roten Knopf!", lb:"Net op denger Kaart? Dréck op de roude Knäppchen!", zh:"卡上没有？按红色的按钮！"},
  nope: {en:"Not on my card!", de:"Nicht auf meiner Karte!", lb:"Net op menger Kaart!", zh:"我的卡上没有！"},
  isThere: {en:"Look again! It's on your card!", de:"Schau noch mal! Es ist auf deiner Karte!", lb:"Kuck nach eng Kéier! Et ass op denger Kaart!", zh:"再看看！你的卡上有！"},
  notThere: {en:"Well spotted! It's not on your card.", de:"Gut aufgepasst! Das ist nicht auf deiner Karte.", lb:"Gutt opgepasst! Dat ass net op denger Kaart.", zh:"眼力真好！你的卡上没有。"},
  line: {en:"A full line! Press BINGO!", de:"Eine volle Reihe! Drück auf BINGO!", lb:"Eng voll Rei! Dréck op BINGO!", zh:"连成一条线啦！快按宾果！"},
  bingo: {en:"BINGO!", de:"BINGO!", lb:"BINGO!", zh:"宾果！"},
  newCard: {en:"A new card!", de:"Eine neue Karte!", lb:"Eng nei Kaart!", zh:"换一张新卡！"},
  again: {en:"Again", de:"Nochmal", lb:"Nach eng Kéier", zh:"再听一次"},
  // what the parrot calls; {w} the word as the lexicon gives it, German {n} nominative and {a} accusative (den Hund, den Löwen)
  call: {
    en:["Find the {w}!", "Next one: the {w}!", "Who has the {w}?", "Cover the {w}!"],
    de:["Such {a}!", "Und jetzt: {n}!", "Wer hat {a}?", "Leg einen Chip auf {a}!"],
    lb:["Sich {w}!", "Elo: {w}!", "Wien huet {w}?", "Lee e Jeton op {w}!"],
    zh:["找一找{w}！", "下一个：{w}！", "谁有{w}？", "把{w}盖住！"]
  }
};
// words that sound alike in each language (English keys of the lexicon): traps from level 3
const SOUND = {
  en:[["mouse","house","mouth"], ["bear","pear","hair","ear"], ["car","star"], ["cake","snake"], ["cat","hat","cap"], ["boat","coat"],
      ["cow","owl"], ["leg","egg"], ["frog","dog"], ["tree","key"], ["arm","farmer"], ["book","cook"], ["rice","ice cream"], ["plane","train"]],
  de:[["mouse","house"], ["mouth","moon","dog"], ["hat","dog","hand"], ["leg","pig"], ["cow","shoes"], ["ball","whale"], ["ice cream","rice","egg"],
      ["cat","cap"], ["fish","frog"], ["nose","trousers"], ["book","cake"]],
  // bear and pear are both "Bier" in Luxembourgish: never together (see stem below)
  lb:[["mouse","house"], ["mouth","moon"], ["cat","cap"], ["ball","whale"], ["dog","hand","chicken"], ["fish","frog"],
      ["bread","boat"], ["egg","leg"], ["cake","book","cook"], ["train","tongue"]],
  zh:[["glasses","eyes"], ["book","tree"], ["cat","hat","owl"], ["chicken","egg"], ["duck","tooth","cap"], ["cow","milk"],
      ["mouse","tiger","teacher"], ["elephant","brain"], ["watermelon","tomato","broccoli"], ["plane","pilot"], ["bus","car","train","bike"],
      ["fish","whale","octopus","crocodile"], ["hand","finger","gloves"], ["rocket","train"], ["bread","noodles"], ["shoes","socks"], ["rabbit","potato"]]
};
// pictures that look alike, in every language
const LOOK = [["tiger","lion"], ["dolphin","whale"], ["orange","lemon","peach"], ["apple","tomato"], ["hat","cap"], ["shoes","boots"],
  ["socks","gloves"], ["bus","train","car"], ["rocket","plane","helicopter"], ["cow","sheep"], ["owl","bird"], ["frog","turtle","crocodile"],
  ["doctor","scientist"], ["cucumber","carrot"], ["bread","sandwich"], ["dress","T-shirt"]];

// a see-through counter (the picture still shows through it) with a little smiling face on its edge
const TOKEN = c => `<svg viewBox="0 0 40 40" aria-hidden="true"><circle cx="20" cy="20" r="16" fill="${c}" fill-opacity=".3" stroke="#1B2D45" stroke-width="7"/>
<circle cx="20" cy="20" r="16" fill="none" stroke="${c}" stroke-width="3.6"/><circle cx="20" cy="20" r="16" fill="none" stroke="#fff" stroke-opacity=".75" stroke-width="1.2" stroke-dasharray="2.5 3"/>
<g transform="translate(31.5 8.5)"><circle r="7.5" fill="${c}" stroke="#1B2D45" stroke-width="2"/><circle cx="-2.5" cy="-1.3" r="1.2" fill="#1B2D45"/><circle cx="2.5" cy="-1.3" r="1.2" fill="#1B2D45"/>
<path d="M-3.2 1.8 Q0 5.2 3.2 1.8" fill="none" stroke="#1B2D45" stroke-width="1.5" stroke-linecap="round"/></g></svg>`;
const HOP = [{transform:"translateY(0)"}, {transform:"translateY(-12px) scale(1.06)", offset:.4}, {transform:"translateY(0) scale(.96)", offset:.75}, {transform:"none"}];

addStyle(`
.bg-game{display:flex; flex-direction:column; gap:12px}
.bg-say{margin:0; text-align:center; font-family:var(--display); font-size:clamp(18px,4.6vw,24px); font-weight:600; line-height:1.2}
.bg-say small{display:block; margin-top:3px; font-family:var(--body); font-size:15px; font-weight:800; color:var(--ink-soft)}
.bg-top{display:flex; align-items:center; justify-content:center; gap:8px; min-height:88px}
.bg-caller{flex:none; font-size:40px; line-height:1}
.bg-mach{flex:none; display:flex; flex-direction:column; align-items:center}
.bg-globe{position:relative; width:66px; height:66px; border-radius:50%; overflow:hidden; border:3px solid var(--ink); box-shadow:2px 3px 0 var(--ink);
  background:radial-gradient(circle at 34% 28%,#fff 0 9%,#E6F7FF 30%,#BFE6F5 78%)}
.bg-mix{position:absolute; inset:0; animation:bg-spin 14s linear infinite}
.bg-mix i{position:absolute; width:17px; height:17px; border-radius:50%; border:2px solid var(--ink)}
.bg-globe.spin .bg-mix{animation-duration:.45s} .bg-globe.spin .bg-mix i{animation:bg-jig .18s ease-in-out infinite alternate}
.bg-base{width:44px; height:13px; margin-top:-3px; background:var(--coral); border:3px solid var(--ink); border-radius:4px 4px 9px 9px}
.bg-ball{flex:1 1 auto; min-width:0; max-width:250px; min-height:68px; display:flex; align-items:center; justify-content:center; text-align:center;
  padding:6px 16px; background:#fff; border:6px solid var(--bc,#FFC43D); border-radius:999px; box-shadow:0 0 0 3px var(--ink), 3px 5px 0 3px var(--ink);
  font-family:var(--display); font-weight:600; font-size:clamp(17px,4.4vw,22px); line-height:1.15; overflow-wrap:break-word}
.bg-ball > span{min-width:0; max-width:100%}
.bg-ball b{font-size:1.2em} .bg-ball.empty{color:var(--ink-soft); font-size:30px}
.bg-tools{display:flex; gap:8px; justify-content:center; align-items:center; flex-wrap:wrap; min-height:48px}
.bg-tools .speak{font-size:18px; padding:8px 16px} .bg-chip{font-size:20px; min-width:56px; min-height:46px}
.bg-wrap{position:relative; align-self:center; width:100%; max-width:var(--bg-max,420px)}
.bg-card{background:#FFE08A; border:3px solid var(--ink); border-radius:20px; box-shadow:4px 5px 0 var(--ink); padding:8px}
.bg-head{display:flex; justify-content:center; gap:5px; margin-bottom:7px}
.bg-head span{width:30px; height:30px; border-radius:50%; display:grid; place-items:center; border:2.5px solid var(--ink); color:#fff;
  font-family:var(--display); font-weight:700; font-size:18px; text-shadow:1px 1px 0 var(--ink)}
.bg-grid{position:relative; display:grid; gap:6px; grid-template-columns:repeat(var(--n),minmax(0,1fr))}
.bg-cell{position:relative; aspect-ratio:1; min-width:0; padding:0; background:#fff; border:3px solid var(--ink); border-radius:14px;
  display:grid; place-items:center; font-size:var(--fs); line-height:1; touch-action:manipulation; transition:transform .12s, background .2s}
.bg-cell:active{transform:scale(.94)}
.bg-pic{display:grid; place-items:center; transition:opacity .2s} .bg-on .bg-pic{opacity:.85}
.bg-tok{position:absolute; inset:5%; pointer-events:none; opacity:0} .bg-on .bg-tok{opacity:1}
.bg-tok svg{display:block; width:100%; height:100%; filter:drop-shadow(2px 3px 0 rgba(27,45,69,.35))}
.bg-lab{position:absolute; left:50%; bottom:4px; transform:translateX(-50%); max-width:96%; padding:1px 6px; background:#fff; border:2px solid var(--ink);
  border-radius:10px; font-family:var(--display); font-size:13px; font-weight:600; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
  opacity:0; transition:opacity .2s; pointer-events:none}
.bg-lab.show{opacity:1}
.bg-cell.bg-ko{background:#FFD6CF; animation:shake .4s}
.bg-hint{animation:bg-glow .8s ease-in-out infinite alternate}
.bg-cell.bg-win{background:#FFF3B0; border-color:#C98B00}
.bg-lines{position:absolute; inset:0; width:100%; height:100%; pointer-events:none; overflow:visible}
.bg-lines line{stroke:var(--coral); stroke-width:9; stroke-linecap:round; stroke-dasharray:1; stroke-dashoffset:0; opacity:.85}
.bg-nope{align-self:center; display:flex; align-items:center; gap:8px; padding:10px 18px; background:var(--coral); color:#fff;
  font-family:var(--display); font-size:20px; font-weight:600; text-shadow:1px 1px 0 var(--ink)}
.bg-nope.bg-ko{animation:shake .4s}
.bg-go{position:absolute; left:50%; top:50%; z-index:5; transform:translate(-50%,-50%); white-space:nowrap; padding:12px 30px;
  background:var(--coral); color:#fff; border:4px solid var(--ink); border-radius:999px; box-shadow:4px 6px 0 var(--ink);
  font-family:var(--display); font-size:clamp(34px,10vw,56px); font-weight:700; text-shadow:2px 2px 0 var(--ink); animation:bg-pulse .6s ease-in-out infinite alternate}
.bg-party{position:absolute; left:50%; top:50%; z-index:6; transform:translate(-50%,-50%); display:flex; gap:5px; pointer-events:none}
.bg-party span{width:min(60px,15vw); aspect-ratio:1; border-radius:50%; display:grid; place-items:center; border:3px solid var(--ink); box-shadow:2px 3px 0 var(--ink);
  color:#fff; font-family:var(--display); font-weight:700; font-size:min(34px,8.5vw); text-shadow:2px 2px 0 var(--ink)}
.bg-still,.bg-still *{animation:none!important; transition:none!important}
@keyframes bg-spin{to{transform:rotate(360deg)}}
@keyframes bg-jig{to{transform:translateY(-5px)}}
@keyframes bg-glow{from{box-shadow:0 0 0 0 #FFC43D} to{box-shadow:0 0 0 8px #FFC43D}}
@keyframes bg-pulse{from{transform:translate(-50%,-50%) scale(1)} to{transform:translate(-50%,-50%) scale(1.1) rotate(-3deg)}}
`);

// the lines of an n×n card: rows, columns and, from 3×3, the two diagonals (on a 2×2 card a diagonal would win at once)
function linesOf(n){
  const r = [...Array(n).keys()], L = [];
  r.forEach(a => { L.push(r.map(b => a * n + b)); L.push(r.map(b => b * n + a)); });
  if (n > 2) { L.push(r.map(k => k * n + k)); L.push(r.map(k => k * n + n - 1 - k)); }
  return L;
}
// the cells called, in order: no full line before the last one, a full line with it
function plan(n, real){
  const lines = linesOf(n), full = set => lines.some(l => l.every(i => set.has(i)));
  for (let t = 0; t < 400; t++) {
    const line = any(lines), last = any(line);
    const others = shuffle([...Array(n * n).keys()].filter(i => !line.includes(i))).slice(0, real - n);
    const before = shuffle([...others, ...line.filter(i => i !== last)]);
    if (!full(new Set(before))) return [...before, last];
  }
  return shuffle(any(lines));
}

registerGame({id:ID, em:"🎱", name:"Bingo des mots", desc:"Écoute, pose un jeton, fais une ligne", multi:true, cat:"mots", title:TX.title, sub:TX.sub}, function () {
  const lang = LG(), t = o => o[lang] || o.en, lvl = levelOf(ID), little = S.kid === "p4", reading = lvl === 4 && !little, traps = lvl >= 3;
  const n = [2, 3, 3, 4][lvl - 1];
  // cards of the game: k words called, d of them not on the card
  const cards = lvl === 1 ? any([[2, 2, 2], [3, 3]]).map(k => ({k, d:0}))
    : lvl === 2 ? any([[3, 4], [4, 3]]).map(k => ({k, d:0}))
    : lvl === 3 ? (rnd(2) ? [{k:4, d:1}, {k:4, d:rnd(2)}] : [{k:4, d:rnd(2)}, {k:4, d:1}])
    : [{k:8, d:2}];
  const total = cards.reduce((s, c) => s + c.k, 0);
  const word = w => w[lang] || w.en;
  // "d'Bier" (pear) and "de Bier" (bear) sound the same: two words with one stem never share a card
  const stem = w => word(w).toLowerCase().replace(/^(d'|den |de |der |die |das |the )/, "");
  const uniq = ws => { const seen = new Set(); return ws.filter(w => !seen.has(w.e) && !seen.has(w.en) && seen.add(w.e) && seen.add(w.en)); };
  const themes = shuffle(THEMES_OK.filter(k => (lvl > 1 || k !== "jobs") && uniq(wordsOf(k, lvl)).length >= n * n + 2));
  const poolFor = c => uniq(lvl <= 2 ? wordsOf(themes[c % themes.length], lvl) : THEMES_OK.flatMap(k => wordsOf(k, lvl)));
  const twinsOf = (w, byKey, groups) => {
    const out = [];
    groups.forEach(g => { if (g.includes(w.en)) g.forEach(k => { const x = byKey.get(k); if (x && x.en !== w.en && !out.includes(x)) out.push(x); }); });
    return out;
  };
  const seen = new Set(); // words of the cards already played
  function makeCard({k, d}, all){
    const fresh = all.filter(w => !seen.has(w.en)), pool = fresh.length >= n * n + d + 4 ? fresh : all;
    const byKey = new Map(pool.map(w => [w.en, w]));
    const order = plan(n, k - d), words = Array(n * n).fill(null), used = [];
    const fits = w => !!w && !used.some(u => u.en === w.en || stem(u) === stem(w));
    const put = (i, w) => { words[i] = w; used.push(w); };
    if (traps) { // a word called and its sound-alike (or look-alike) twin on the same card
      const free = shuffle([...Array(n * n).keys()].filter(i => !order.includes(i))), drawn = shuffle(order);
      const pairs = g => shuffle(pool.flatMap(a => twinsOf(a, byKey, g).map(b => [a, b])));
      let placed = 0;
      for (const [a, b] of [...pairs(SOUND[lang] || []), ...pairs(LOOK)]) {
        if (placed === 2 || !free.length) break;
        if (!fits(a) || !fits(b) || stem(a) === stem(b)) continue;
        put(drawn.pop(), a); put(free.pop(), b); placed++;
      }
    }
    const rest = shuffle(pool);
    for (let i = 0; i < n * n; i++) if (!words[i]) put(i, rest.find(fits) || any(pool));
    // words called that are not on the card: a twin of a word on it when there is one
    const groups = (SOUND[lang] || []).concat(LOOK), decoys = [];
    for (let j = 0; j < d; j++) {
      const w = shuffle(words.flatMap(x => twinsOf(x, byKey, groups))).find(fits) || shuffle(pool).find(fits) || shuffle(all).find(fits);
      used.push(w); decoys.push(w);
    }
    const seq = order.map(i => ({w:words[i], cell:i}));
    decoys.forEach(w => seq.splice(1 + rnd(seq.length - 1), 0, {w, cell:-1})); // never the first nor the last call
    used.forEach(w => seen.add(w.en));
    return {words, seq};
  }
  const fill = (tpl, w, html) => tpl.replace(/\{([wna])\}/g, (m, key) => {
    const s = key === "a" ? deAkk(w.de) : word(w);
    return html ? `<b>${s}</b>` : s;
  });
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };

  const res = [];
  let ci = 0, di = 0, cur = null, card = null, covered = [], tokN = rnd(COLORS.length);
  startSession(ID, null, total); const gen = GEN;
  const later = ms => new Promise(r => loops.push(setTimeout(r, TEST ? Math.min(ms, 40) : ms)));
  const speak = s => say(s, lang);

  const body = $("gameBody"); body.innerHTML = "";
  const game = el("div", "bg-game" + (TEST || calm() ? " bg-still" : ""));
  const intro = `${t(reading ? TX.read : TX.listen)} ${t(TX.goal)}${traps ? " " + t(TX.nopeHelp) : ""}`;
  const head = el("p", "bg-say", `${reading ? "📖" : "👂"} ${t(reading ? TX.read : TX.listen)}<small>🎯 ${t(TX.goal)}</small>${traps ? `<small>🙅 ${t(TX.nopeHelp)}</small>` : ""}`);
  const top = el("div", "bg-top"), caller = el("div", "bg-caller bob", "🦜"), mach = el("div", "bg-mach");
  const SPOTS = [[8, 18], [40, 8], [66, 26], [22, 50], [56, 58], [38, 34], [10, 66], [70, 62]];
  const globe = el("div", "bg-globe", `<div class="bg-mix">${SPOTS.map(([x, y], k) => `<i style="left:${x}%;top:${y}%;background:${COLORS[k % COLORS.length]}"></i>`).join("")}</div>`);
  mach.append(globe, el("div", "bg-base"));
  const ball = el("div", "bg-ball empty", "?");
  top.append(caller, mach, ball);
  const tools = el("div", "bg-tools"), wrap = el("div", "bg-wrap");
  wrap.style.setProperty("--bg-max", {2:"330px", 3:"420px", 4:"480px"}[n]);
  const nope = traps ? el("button", "bg-nope chunky", `🙅 <span>${t(TX.nope)}</span>`) : null;
  game.append(head, top, tools, wrap); if (nope) game.append(nope);
  body.append(game);

  // sounds, made with the tone() of the core
  const snd = {
    rattle: () => tone([...Array(8)].map(() => 700 + rnd(900)), .05, "triangle"),
    plop: () => tone([520, 300], .06, "sine"),
    whoosh: () => tone([900, 700, 520, 380], .05, "triangle"),
    fanfare: () => tone([523, 659, 784, 1047, 784, 1047, 1319], .12, "triangle")
  };

  function drawCard(){
    covered = Array(n * n).fill(false);
    wrap.innerHTML = "";
    const box = el("div", "bg-card");
    const letters = el("div", "bg-head", "BINGO".split("").map((ch, k) => `<span style="background:${COLORS[k]}">${ch}</span>`).join(""));
    const grid = el("div", "bg-grid");
    grid.style.setProperty("--n", n);
    grid.style.setProperty("--fs", {2:"clamp(56px,17vw,84px)", 3:"clamp(38px,11vw,62px)", 4:"clamp(28px,8vw,48px)"}[n]);
    card.cells = card.words.map((w, i) => {
      const b = el("button", "bg-cell", `<span class="bg-pic">${wordFace(w)}</span><span class="bg-tok"></span><span class="bg-lab"></span>`);
      b.setAttribute("aria-label", word(w));
      b.onclick = () => tapCell(i);
      grid.append(b);
      return b;
    });
    const svg = document.createElementNS(NS, "svg"); svg.setAttribute("class", "bg-lines"); svg.setAttribute("aria-hidden", "true");
    grid.append(svg);
    box.append(letters, grid); wrap.append(box);
    Object.assign(card, {grid, svg, box});
    card.cells.forEach((b, i) => anim(b, [{transform:"scale(0) rotate(-25deg)"}, {transform:"scale(1.1)", offset:.7}, {transform:"none"}], {duration:380, delay:i * 45, easing:"ease-out", fill:"backwards"}));
    letters.querySelectorAll("span").forEach((s, k) => anim(s, [{transform:"translateY(-40px)", opacity:0}, {transform:"translateY(4px)", opacity:1, offset:.7}, {transform:"none"}], {duration:450, delay:k * 80, easing:"ease-out", fill:"backwards"}));
  }
  function clearOk(){ game.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok); }

  async function startCard(){
    if (!alive(gen)) return;
    card = makeCard(cards[ci], poolFor(ci)); di = 0; cur = null;
    ball.className = "bg-ball empty"; ball.textContent = "?"; tools.innerHTML = "";
    drawCard();
    await speak(ci === 0 ? intro : t(TX.newCard));
    if (!alive(gen)) return;
    await later(ci === 0 ? 300 : 500);
    next();
  }
  function next(){
    if (!alive(gen)) return;
    if (di >= card.seq.length) { ci++; return ci >= cards.length ? finish() : startCard(); } // safety net: the plan always ends on a line
    const c = card.seq[di++], tpl = little || lvl === 1 ? t(TX.call)[0] : any(t(TX.call));
    cur = Object.assign({tries:0, done:false, ready:false, hinted:false, say:fill(tpl, c.w, false), html:fill(tpl, c.w, true)}, c);
    renderDots(res, total, res.length);
    draw(cur);
  }
  // the parrot turns the machine, a ball rolls out with the word
  async function draw(c){
    ball.className = "bg-ball empty"; ball.textContent = "…"; tools.innerHTML = "";
    globe.classList.add("spin"); snd.rattle();
    anim(mach, [{transform:"rotate(0)"}, {transform:"rotate(-8deg)"}, {transform:"rotate(8deg)"}, {transform:"rotate(-5deg)"}, {transform:"none"}], {duration:600});
    await later(650);
    if (!alive(gen) || c !== cur) return;
    globe.classList.remove("spin");
    ball.className = "bg-ball"; ball.style.setProperty("--bc", any(COLORS));
    ball.innerHTML = `<span>${reading ? `<b>${word(c.w)}</b>` : c.html}</span>`; // what the voice says is written too (a chance to read)
    anim(ball, [{transform:"translateX(-70px) scale(.2) rotate(-200deg)", opacity:0}, {transform:"scale(1.08)", opacity:1, offset:.7}, {transform:"none", opacity:1}], {duration:480, easing:"ease-out"});
    anim(caller, HOP, {duration:500, easing:"ease-out"});
    sfx.pop();
    tools.innerHTML = "";
    if (reading) { const b = el("button", "chip bg-chip", "🔊"); b.onclick = () => { G.hints++; speak(c.say); }; tools.append(b); }
    else tools.append(speakBtn(() => c.say, t(TX.again), lang));
    if (little || lvl >= 3) tools.append(bridgeBtn(c.w)); // 中文 for the big one, 🐢 slowly for the little one
    c.ready = true;
    markOk(c.cell < 0 ? nope : card.cells[c.cell]);
    if (!reading) speak(c.say);
  }

  function tapCell(i){
    const c = cur;
    if (!c || c.done || !c.ready) return;
    const b = card.cells[i];
    if (covered[i]) { sfx.tap(); anim(b, [{transform:"rotate(0)"}, {transform:"rotate(-6deg)"}, {transform:"rotate(6deg)"}, {transform:"none"}], {duration:300}); return; }
    G.taps++;
    if (i === c.cell) return good(i);
    c.tries++; sfx.ko();
    b.classList.remove("bg-ko"); void b.offsetWidth; b.classList.add("bg-ko");
    const lab = b.querySelector(".bg-lab"); lab.textContent = word(card.words[i]); lab.classList.add("show");
    loops.push(setTimeout(() => { lab.classList.remove("show"); b.classList.remove("bg-ko"); }, 1700));
    // the picture says its own name, then the word called again
    speak(word(card.words[i])).then(() => { if (alive(gen) && cur === c && !c.done && !reading) speak(c.say); });
    hint(c);
  }
  if (nope) nope.onclick = () => {
    const c = cur;
    if (!c || c.done || !c.ready) return;
    G.taps++;
    if (c.cell < 0) return good(-1);
    c.tries++; sfx.ko();
    nope.classList.remove("bg-ko"); void nope.offsetWidth; nope.classList.add("bg-ko");
    speak(t(TX.isThere));
    hint(c);
  };
  // after two misses (the little one) or three: the right answer glows
  function hint(c){
    if (c.hinted || c.tries < (little ? 2 : 3)) return;
    c.hinted = true;
    (c.cell < 0 ? nope : card.cells[c.cell]).classList.add("bg-hint");
  }

  async function good(i){
    const c = cur; c.done = true;
    clearOk();
    const first = c.tries === 0;
    if (first) addStar(); else fx.sparkle();
    logRound(c.w.en, first, c.tries + 1, i < 0 ? {lvl, absent:true} : {lvl});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    sfx.ok();
    if (i < 0) { // not on the card: the ball rolls away
      nope.classList.remove("bg-hint");
      snd.whoosh();
      const a = anim(ball, [{transform:"none", opacity:1}, {transform:"translateX(160px) rotate(260deg)", opacity:0}], {duration:520, easing:"ease-in"});
      if (a) a.onfinish = () => { if (cur === c) { ball.className = "bg-ball empty"; ball.textContent = "?"; } };
      await speak(t(TX.notThere));
    } else {
      cover(i);
      const won = linesOf(n).filter(l => l.includes(i) && l.every(k => covered[k]));
      if (won.length) return bingo(won);
      await speak(praise());
    }
    if (!alive(gen)) return;
    await later(450);
    next();
  }
  // the counter falls on the picture
  function cover(i){
    covered[i] = true;
    const b = card.cells[i];
    b.classList.remove("bg-hint", "bg-ko"); b.classList.add("bg-on");
    const tok = b.querySelector(".bg-tok");
    tok.innerHTML = TOKEN(COLORS[tokN++ % COLORS.length]);
    anim(tok, [{transform:"translateY(-90px) scale(1.7) rotate(-120deg)", opacity:0}, {transform:"translateY(6px) scale(.9)", opacity:1, offset:.65},
      {transform:"translateY(-4px) scale(1.05)", offset:.85}, {transform:"none", opacity:1}], {duration:520, easing:"ease-out"});
    loops.push(setTimeout(snd.plop, calm() ? 0 : 330));
  }
  // a stroke crosses the full line, cell by cell it lights up, and the BINGO button waits to be pressed
  function bingo(won){
    const g = card.grid.getBoundingClientRect(), mid = r => [r.left + r.width / 2 - g.left, r.top + r.height / 2 - g.top];
    card.svg.setAttribute("viewBox", `0 0 ${Math.max(1, g.width)} ${Math.max(1, g.height)}`);
    won.forEach(l => {
      const [x1, y1] = mid(card.cells[l[0]].getBoundingClientRect()), [x2, y2] = mid(card.cells[l[l.length - 1]].getBoundingClientRect());
      const ln = document.createElementNS(NS, "line");
      [["x1", x1], ["y1", y1], ["x2", x2], ["y2", y2], ["pathLength", 1]].forEach(([k, v]) => ln.setAttribute(k, v));
      card.svg.append(ln);
      anim(ln, [{strokeDashoffset:1}, {strokeDashoffset:0}], {duration:650, delay:250, easing:"ease-out", fill:"backwards"});
    });
    [...new Set(won.flat())].forEach((k, j) => { card.cells[k].classList.add("bg-win"); anim(card.cells[k], HOP, {duration:450, delay:j * 90, easing:"ease-out"}); });
    tools.innerHTML = "";
    const go = el("button", "bg-go", t(TX.bingo));
    wrap.append(go); markOk(go);
    anim(go, [{transform:"translate(-50%,-50%) scale(0) rotate(-30deg)"}, {transform:"translate(-50%,-50%) scale(1.2)", offset:.7}, {transform:"translate(-50%,-50%) scale(1)"}], {duration:500, delay:500, easing:"ease-out", fill:"backwards"});
    snd.fanfare();
    speak(t(TX.line));
    go.onclick = async () => {
      if (go.dataset.done) return;
      go.dataset.done = "1"; delete go.dataset.ok; G.taps++;
      party(won, go);
      await speak(t(TX.bingo) + (lang === "zh" ? "" : " ") + praise());
      if (!alive(gen)) return;
      await later(1300);
      if (!alive(gen)) return;
      ci++;
      if (ci >= cards.length) return finish();
      startCard();
    };
  }
  // the party: the letters B I N G O jump over the card, the counters dance, the parrot spins
  function party(won, go){
    snd.fanfare(); loops.push(setTimeout(() => sfx.win(), 900));
    confetti(36);
    anim(caller, [{transform:"rotate(0) scale(1)"}, {transform:"rotate(360deg) scale(1.5)", offset:.6}, {transform:"rotate(720deg) scale(1)"}], {duration:1100, easing:"ease-in-out"});
    const a = anim(go, [{transform:"translate(-50%,-50%) scale(1)", opacity:1}, {transform:"translate(-50%,-50%) scale(1.8)", opacity:0}], {duration:420, easing:"ease-in"});
    if (a) a.onfinish = () => go.remove(); else go.remove();
    [...new Set(won.flat())].forEach((k, j) => {
      const tok = card.cells[k].querySelector(".bg-tok");
      anim(tok, [{transform:"none"}, {transform:"translateY(-22px) rotate(180deg) scale(1.2)", offset:.5}, {transform:"rotate(360deg)"}], {duration:700, delay:j * 110, iterations:2, easing:"ease-in-out"});
      if (!calm()) loops.push(setTimeout(() => { const r = card.cells[k].getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 8); }, 200 + j * 140));
    });
    if (calm()) return;
    const show = el("div", "bg-party", "BINGO".split("").map((ch, k) => `<span style="background:${COLORS[k]}">${ch}</span>`).join(""));
    wrap.append(show);
    show.querySelectorAll("span").forEach((s, k) => anim(s, [{transform:"translateY(-160px) scale(.4)", opacity:0}, {transform:"translateY(10px) scale(1.15)", opacity:1, offset:.55},
      {transform:"translateY(-18px)", offset:.75}, {transform:"none", opacity:1}], {duration:650, delay:k * 110, easing:"ease-out", fill:"backwards"}));
    const out = anim(show, [{opacity:1}, {opacity:1, offset:.8}, {opacity:0}], {duration:2300});
    if (out) out.onfinish = () => show.remove();
  }

  startCard();
});
})();
