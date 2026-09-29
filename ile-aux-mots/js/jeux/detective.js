/* L'Île aux Mots : jeu « Détective » : qui a mangé le gâteau ? (ticket 2026-09-27_jeu-detective)
   1: 4 suspects, "The thief has a hat.": touch the thief · 2: 6 suspects, colours or two things at once
   3: 9 suspects, spoken and written clues (not, or, and, he/she): touch who is NOT the thief; or ask the owl, tile by tile
   4: 12 suspects and written clues (neither…nor), or 16 suspects to unmask in three questions.
   English, German, Luxembourgish or Chinese, never French. Words, sentences, drawings and sounds: js/contenus/detective.js. */
(() => {
const D = detectiveData, TR = D.traits, {face, OWL, icon, snd, SKIN, SCARF, COL, HAIR} = detectiveArt;
const lang = () => ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
const U = () => D.ui[lang()];
const one = a => a[rnd(a.length)];
const cat = (a, b) => lang() === "zh" ? a + b : a + " " + b;
const wait = ms => new Promise(r => { if (TEST) r(); else loops.push(setTimeout(r, ms)); });
const move = (n, k, o) => n && !fx.calm() ? n.animate(k, o) : null;
const has = (s, t) => s[t.k] === t.v;
const hear = t => lang() !== "lb" || !!t.key; // Luxembourgish is heard only through lexicon recordings
const keysOf = c => [c.a, c.b].flatMap(t => t && t.key || []);
// does suspect s fit clue c? (he/she in the clue also rules out the other gender)
function fits(s, c){
  if (c.subj && c.subj !== "thief" && s.g !== c.subj) return false;
  const A = has(s, c.a), B = c.b && has(s, c.b);
  return {pos:A, neg:!A, and:A && B, or:A || B, nor:!A && !B}[c.t];
}
const clueKey = c => ({pos:c.a.id, neg:"not " + c.a.id, and:`${c.a.id} and ${c.b && c.b.id}`, or:`${c.a.id} or ${c.b && c.b.id}`, nor:`neither ${c.a.id} nor ${c.b && c.b.id}`})[c.t];

/* ---------- voice: the owl, the suspects and the thief each get their own pitch ---------- */
function talk(text, who = "owl", keys){
  const L = lang();
  if (L === "lb") return (keys && keys.length ? keys : [text]).reduce((p, k) => p.then(() => say(k, "lb")), Promise.resolve());
  // a recorded Azure voice is fluid on every device: it wins over the character's pitch (in #test, say() notes the line)
  if (TEST || (typeof voixFiles === "function" && voixFiles(text, L)) || !("speechSynthesis" in window)) return say(text, L);
  return new Promise(res => {
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(text), v = voiceFor(L);
      u.lang = LANG[L]; if (v) u.voice = v;
      u.pitch = {owl:1.3, kid:1.6, thief:1.9}[who] || 1.1;
      u.rate = voiceRate() * (who === "kid" ? 1.1 : 1);   // the speed the child chose (🐌 🐢 🐇)
      let done = false; const fin = () => { if (!done) { done = true; res(); } };
      u.onend = fin; u.onerror = fin; loops.push(setTimeout(fin, 3000 + text.length * 110 / u.rate));
      speechSynthesis.speak(u);
    } catch(e) { res(); }
  });
}

/* ---------- suspects and cases, all generated ---------- */
function gallery(n, p, build){
  const names = {he:shuffle(D.names.he), she:shuffle(D.names.she)}, out = [], seen = new Set();
  for (let k = 0; out.length < n && k < 800; k++) {
    const g = build ? build(out.length).g : out.length % 2 ? "he" : "she";
    const s = {g, skin:one(SKIN), style:one(g === "she" ? ["long","bun","curly","bob"] : ["short","spiky","curly","short"]), hair:one(Object.keys(HAIR)),
      head:Math.random() < p ? one(["hat","crown"]) : "none", glasses:Math.random() < p * .8, scarf:Math.random() < p * .8, sc:one(SCARF),
      shirt:one(Object.keys(COL)), hold:Math.random() < p ? one(["ball","book","flower","umbrella","key"]) : "none", ...(build ? build(out.length) : {})};
    const sig = [s.g, s.hair, s.head, s.glasses, s.scarf, s.shirt, s.hold].join();
    if (seen.has(sig)) continue;
    seen.add(sig); s.name = names[g].pop(); out.push(s);
  }
  return shuffle(out);
}
// levels 1 and 2: one clue, only the thief fits it
function caseFind(lvl){
  for (let a = 0; a < 500; a++) {
    const sus = gallery(lvl === 1 ? 4 : 6, lvl === 1 ? .3 : .45), th = one(sus), cands = [];
    const only = c => sus.filter(s => fits(s, c)).length === 1;
    const mine = TR.filter(t => has(th, t) && hear(t) && (lvl === 1 ? t.big : true));
    if (lvl === 1) mine.forEach(t => cands.push({t:"pos", a:t}));
    else {
      mine.filter(t => t.col).forEach(t => cands.push({t:"pos", a:t}));
      // two things at once: each alone fits somebody else too
      mine.forEach((x, i) => mine.slice(i + 1).forEach(y => { if (x.k !== y.k && [x, y].every(t => sus.filter(s => has(s, t)).length > 1)) cands.push({t:"and", a:x, b:y}, {t:"and", a:x, b:y}); }));
    }
    const ok = cands.filter(only);
    if (ok.length) return {sus, thief:th, clues:[{...one(ok), subj:"thief"}]};
  }
  const sus = gallery(4, 0), th = sus[0]; th.head = "hat"; // never reached in practice: a plain gallery, one hat
  return {sus, thief:th, clues:[{t:"pos", a:TR[0], subj:"thief"}]};
}
// levels 3 and 4: two to four clues, each rules out at least one suspect, until only the thief is left
function caseElim(n, types){
  for (let a = 0; a < 3000; a++) {
    const sus = gallery(a < 2000 ? n : 6, .45), th = one(sus), clues = [], used = new Set(); let R = sus;
    while (R.length > 1 && clues.length < 4) {
      const subj = clues.length ? th.g : "thief", byT = {};
      types.forEach(t => {
        const list = t === "pos" || t === "neg" ? TR.map(x => ({t, a:x})) : TR.flatMap((x, i) => TR.slice(i + 1).filter(y => t === "or" ? x.k === y.k : x.k !== y.k).map(y => ({t, a:x, b:y})));
        byT[t] = list.map(c => ({...c, subj})).filter(c => !used.has(c.a.id) && !(c.b && used.has(c.b.id)) && fits(th, c) && R.some(s => !fits(s, c))
          && (clues.length >= 1 || R.filter(s => fits(s, c)).length >= 2));
      });
      const ts = Object.keys(byT).filter(t => byT[t].length); if (!ts.length) break;
      const c = one(byT[one(ts)]);
      clues.push(c); used.add(c.a.id); if (c.b) used.add(c.b.id); R = R.filter(s => fits(s, c));
    }
    if (R.length === 1 && clues.length >= 2) return {sus, thief:th, clues};
  }
  return caseFind(2); // never reached in practice: one clue that leaves only the thief
}
// questions: 12 suspects, or 16 built on four yes/no traits so that three questions always do it
function caseAsk(lvl){
  if (lvl < 4) { const sus = gallery(12, .5); return {sus, thief:one(sus)}; }
  const sus = gallery(16, 0, i => ({g:i & 1 ? "he" : "she", head:i & 2 ? "hat" : "none", glasses:!!(i & 4), shirt:i & 8 ? "red" : "blue", scarf:false, hold:"none"}));
  return {sus, thief:one(sus)};
}

registerGame({id:"detective", em:"🕵️", name:"Détective", desc:"Qui a mangé le gâteau ?", multi:true, title:D.title, sub:D.sub}, function () {
  const lvl = levelOf("detective"), gen = GEN, res = [];
  const plan = [0, ["find","find","find","find","find"], ["find","find","find","find","find","find"], ["elim","ask","elim","ask"], ["read","ask","read","ask"]][lvl];
  startSession("detective", null, plan.length);
  const root = el("div", "dt" + (fx.calm() ? " calm" : "")), top = el("div", "dt-top"), owl = el("div", "dt-owl", OWL), bub = el("div", "dt-bub");
  const again = el("button", "speak chunky"), gw = el("div", "dt-gw"), grid = el("div", "dt-grid"), ctl = el("div", "dt-ctl");
  top.append(owl, bub, again); gw.append(grid); root.append(top, gw, ctl); $("gameBody").append(root);
  let ci = 0, cs = null;
  const reads = () => S.kid === "p7" || lvl >= 3 || lang() === "lb";
  const clue = () => cs.clues[cs.k];
  const aliveS = () => cs.sus.filter(s => !cs.out.has(s));
  const nameOf = s => lang() === "zh" ? s.name[1] : s.name[0];
  // a flag in the game bar clicks this button: the page is redrawn in the new language
  again.onclick = e => {
    if (e.isTrusted) { G.replays++; if (cs.kind === "read") G.hints++; } else refresh();
    if (cs.line) owlSay(cs.line());
  };

  /* ---------- the owl ---------- */
  function owlSay(l){
    move(owl.querySelector(".dt-owlb"), [{transform:"rotate(0)"}, {transform:"rotate(-7deg)"}, {transform:"rotate(6deg)"}, {transform:"rotate(0)"}], {duration:650, iterations:2});
    move(owl.querySelector(".dt-bk"), [{transform:"scaleY(1)"}, {transform:"scaleY(1.6)"}, {transform:"scaleY(1)"}], {duration:170, iterations:6});
    return talk(l.text, "owl", l.keys);
  }
  const flap = () => [[".dt-wl", -35], [".dt-wr", 35]].forEach(([q, d]) => move(owl.querySelector(q), [{transform:"rotate(0)"}, {transform:`rotate(${d}deg)`}, {transform:"rotate(0)"}], {duration:260, iterations:3}));
  function setLine(f, speak = true){ cs.line = f; paint(); if (speak) return owlSay(f()); }
  function paint(){
    const L = lang(), l = cs.line ? cs.line() : null, intro = cat(D.openers[L][cs.op], D.crime(L, cs.w, cs.eat));
    again.innerHTML = cs.kind === "read" ? "<b>🔊</b>" : `<b>🔊</b><span>${U().again}</span>`;
    const n = (cs.kind === "elim" || cs.kind === "read") && cs.stage === "elim" ? `<span class="dt-n">🔍 ${U().clue} ${cs.k + 1}/${cs.clues.length}</span>` : "";
    // always written, the little one included (Kezhan: a chance to read); she also gets the pictures
    const pics = reads() ? "" : `<span><span class="dt-ev">${cs.w.e}</span> ${cs.hint ? `<span class="dt-hi">${[clue().a, clue().b].filter(Boolean).map(icon).join("")}</span>` : ""}</span>`;
    bub.innerHTML = `${pics}${cs.stage === "intro" ? "" : `<small>${intro}</small>`}${n}${l ? `<b>${l.text}</b>` : ""}${cs.note ? `<small class="dt-note">${cs.note()}</small>` : ""}`;
  }

  /* ---------- the suspects ---------- */
  function drawGrid(){
    grid.innerHTML = ""; grid.style.setProperty("--c", cs.sus.length <= 4 ? 2 : cs.sus.length <= 9 ? 3 : 4);
    cs.sus.forEach(s => {
      const b = el("button", "dt-s" + (cs.out.has(s) ? " out" : ""), `${face(s)}<span class="dt-nm">${nameOf(s)}</span>`);
      b.style.setProperty("--d", -rnd(40) / 10 + "s"); b.setAttribute("aria-label", nameOf(s));
      b.onpointerdown = () => move(b.firstChild, [{transform:"scale(1)"}, {transform:"scale(1.08,.86)"}, {transform:"scale(1)"}], {duration:170});
      b.onclick = () => tap(s);
      s.b = b; grid.append(b);
    });
  }
  function pop(s, text, ms = 1800){
    const b = s.b; b.querySelectorAll(".dt-say").forEach(x => x.remove());
    const d = el("div", "dt-say"); d.textContent = text; b.append(d);
    move(d, [{transform:"translate(-50%,0) scale(.2)", opacity:0}, {transform:"translate(-50%,0) scale(1.12)", opacity:1, offset:.6}, {transform:"translate(-50%,0) scale(1)"}], {duration:260, easing:"ease-out"});
    loops.push(setTimeout(() => d.remove(), ms));
  }
  // turned round: shows the back of the head
  async function out(s, line){
    cs.out.add(s); const svg = s.b.firstChild;
    const a = move(svg, [{transform:"scaleX(1)"}, {transform:"scaleX(0)"}], {duration:130, easing:"ease-in"});
    if (a) await a.finished.catch(() => {});
    s.b.classList.add("out");
    move(svg, [{transform:"scaleX(0) translateY(-6px)"}, {transform:"scaleX(1.12) translateY(-12px)"}, {transform:"none"}], {duration:280, easing:"ease-out"});
    if (line) pop(s, line, 1500);
  }
  const shake = s => move(s.b.firstChild, [0, -14, 12, -10, 8, 0].map(d => ({transform:`rotate(${d}deg)`})), {duration:520});
  const wiggle = s => move(s.b, [0, -6, 6, -4, 0].map(d => ({transform:`translateX(${d}px)`})), {duration:420, iterations:2});
  function giggle(s){ snd.hee(); move(s.b.firstChild, [{transform:"none"}, {transform:"translateY(-10px) scale(1.05,.95)"}, {transform:"none"}], {duration:300}); }
  function puff(s){
    const ch = s.b.querySelector(".dt-ch");
    move(ch, [{transform:"scale(1)", opacity:.45}, {transform:"scale(1.9)", opacity:1}, {transform:"scale(1.9)", opacity:1, offset:.8}, {transform:"scale(1)", opacity:.45}], {duration:1100});
    move(s.b.firstChild, [0, -5, 5, -5, 5, 0].map(d => ({transform:`translate(${d}px,${-Math.abs(d)}px)`})), {duration:450});
  }

  function mark(){
    if (!TEST) return;
    root.querySelectorAll("[data-ok]").forEach(n => n.removeAttribute("data-ok"));
    const st = cs.stage;
    if (st === "find" || st === "catch") markOk(cs.thief.b);
    else if (st === "elim") { const s = aliveS().find(x => !fits(x, clue())); markOk(s ? s.b : cs.nextB); }
    else if (st === "chips" || st === "tiles") markOk(cs.best);
  }
  function refresh(){
    if (!cs) return;
    drawGrid(); if (cs.stage === "elim") drawNext(); else if (cs.stage === "chips" || cs.stage === "tiles") chips();
    paint(); mark();
  }

  /* ---------- a case ---------- */
  async function next(){
    if (!alive(gen)) return;
    if (ci >= plan.length) return finish();
    renderDots(res, plan.length, ci);
    const kind = plan[ci], [we, eat] = one(D.crimes);
    cs = kind === "find" ? caseFind(lvl) : kind === "ask" ? caseAsk(lvl) : caseElim(kind === "read" ? 12 : 9, kind === "read" ? ["neg","nor","or","and","nor"] : ["pos","neg","and","or"]);
    Object.assign(cs, {kind, eat, out:new Set(), ok:true, k:0, tries:0, err:false, op:rnd(6), stage:"intro", line:null,
      w:Object.values(THEMES).flatMap(t => t.words).find(x => x.en === we)});
    ctl.innerHTML = ""; gw.querySelectorAll(".dt-stamp").forEach(x => x.remove()); drawGrid(); paint(); snd.hoot(); flap();
    move(grid, [{transform:"translateY(30px) scale(.9)", opacity:0}, {transform:"none", opacity:1}], {duration:420, easing:"cubic-bezier(.2,1.4,.4,1)"});
    await setLine(() => ({text:cat(D.openers[lang()][cs.op], D.crime(lang(), cs.w, cs.eat))}));
    if (!alive(gen)) return;
    if (kind === "ask") return askStart();
    cs.stage = kind === "find" ? "find" : "elim";
    if (cs.stage === "elim") drawNext();
    setClue();
  }
  function setClue(){
    const c = clue();
    setLine(() => ({text:D.sentence(lang(), c, c.subj), keys:keysOf(c)}), cs.kind !== "read"); // level 4: read alone, voice on request
    mark();
  }
  async function tap(s){
    G.taps++;
    const st = cs.stage;
    if ((st === "find" || st === "catch") && s === cs.thief) return caught();
    if (st === "find") return miss(s);
    if (st === "elim" && !cs.out.has(s)) return fits(s, clue()) ? protest(s) : rule(s);
    if (!cs.out.has(s) && st !== "done") giggle(s);
  }
  // levels 1 and 2: a wrong suspect says what it lacks
  async function miss(s){
    cs.tries++; cs.ok = false; snd.honk(); fx.wrong(); shake(s);
    const c = clue(), lack = [c.a, c.b].find(t => t && !has(s, t)) || c.a;
    const line = cat(U().no, D.sentence(lang(), {t:"neg", a:lack}, "me"));
    pop(s, line, 2400);
    if (S.kid === "p4" && cs.tries >= 2) { cs.hint = true; paint(); cs.thief.b.classList.add("dt-hint"); }
    await talk(line, "kid", lack.key);
    if (alive(gen) && cs.stage === "find") owlSay(cs.line());
  }
  // levels 3 and 4: rightly ruled out
  function rule(s){
    const c = clue(), byG = c.subj !== "thief" && s.g !== c.subj && fits(s, {...c, subj:"thief"});
    const line = byG ? cat(U().no, s.g === "he" ? U().boy : U().girl) : one(D.notMe[lang()]);
    snd.pouet(); out(s, line); talk(line, "kid");
    if (!aliveS().some(x => !fits(x, c))) {
      if (aliveS().length === 1) { endClue(); return startCatch(); }
      cs.nextB.classList.add("dt-go");
    }
    mark();
  }
  // an innocent who fits the clue comes back and protests; the clue is read again
  async function protest(s){
    cs.tries++; cs.err = true; cs.ok = false; snd.boing(); fx.wrong(); puff(s);
    const c = clue(), pc = c.t === "or" ? {t:"pos", a:has(s, c.a) ? c.a : c.b} : c;
    const line = cat(U().hey, D.sentence(lang(), pc, "me"));
    pop(s, line, 2600);
    await talk(line, "kid", keysOf(pc));
    if (alive(gen) && cs.stage === "elim") owlSay(cs.line());
  }
  function drawNext(){
    ctl.innerHTML = "";
    const b = el("button", "chunky dt-next", `🔍 ${U().next}`);
    if (!aliveS().some(x => !fits(x, clue()))) b.classList.add("dt-go");
    b.onclick = () => {
      G.taps++; if (cs.stage !== "elim") return;
      const left = aliveS().filter(x => !fits(x, clue()));
      if (left.length) { cs.err = true; cs.ok = false; snd.honk(); left.forEach(wiggle); cs.note = () => U().look; paint(); return owlSay({text:U().look}); }
      snd.pop(); endClue(); cs.k++; b.classList.remove("dt-go"); setClue();
    };
    cs.nextB = b; ctl.append(b);
  }
  function endClue(){
    const c = clue();
    logRound(clueKey(c), !cs.err, cs.tries + 1, {lvl, who:c.subj});
    if (!cs.err) addStar();
    cs.err = false; cs.tries = 0; cs.note = null;
  }

  /* ---------- the owl answers questions ---------- */
  async function askStart(){
    const g = cs.thief.g;
    cs.stage = "saw"; cs.asked = 0;
    await setLine(() => ({text:U().saw[g]}));
    if (!alive(gen)) return;
    for (const s of aliveS().filter(x => x.g !== g)) { if (!alive(gen)) return; out(s, s.g === "he" ? U().boy : U().girl); snd.pouet(); await wait(110); }
    if (!alive(gen)) return;
    setLine(() => ({text:lvl === 4 ? cat(U().ask, U().challenge) : U().ask}));
    chips();
  }
  function chips(){
    cs.stage = "chips"; cs.best = null; ctl.innerHTML = "";
    const R = aliveS(), row = el("div", "dt-chips"); let bd = 99;
    TR.filter(t => R.some(s => has(s, t))).forEach(t => {
      const b = el("button", "dt-chip", icon(t)), n = R.filter(s => has(s, t)).length, d = Math.abs(n - R.length / 2);
      b.setAttribute("aria-label", t.en);
      b.onpointerdown = () => move(b, [{transform:"scale(1)"}, {transform:"scale(.85)"}, {transform:"scale(1)"}], {duration:160});
      b.onclick = () => { G.taps++; snd.pop(); const Q = D.ask[lang()]; cs.q = {t, c:[], Q, slots:Q.slots(t)}; tiles(); };
      if (n < R.length && d < bd) { bd = d; cs.best = b; }
      row.append(b);
    });
    ctl.append(row); mark();
  }
  function tiles(){
    const {t, c, Q, slots} = cs.q;
    cs.stage = "tiles"; ctl.innerHTML = "";
    const line = el("div", "dt-q"), row = el("div", "dt-tiles"), want = Q.best(Q.pron[cs.thief.g])[c.length];
    Q.frame(t).forEach(p => line.append(typeof p === "number" ? el("span", "sl" + (p === c.length ? " now" : ""), c[p] || "&nbsp;") : el("span", "", p)));
    line.firstChild.before(el("span", "dt-ev", icon(t)));
    shuffle(slots[c.length]).forEach(w => {
      const b = el("button", "dt-tile chunky", w);
      b.onclick = () => { G.taps++; snd.pop(); fx.bounce(b); c.push(w); c.length === slots.length ? ask() : tiles(); };
      if (w === want) cs.best = b;
      row.append(b);
    });
    const back = el("button", "dt-chip", "↩"); back.setAttribute("aria-label", "back"); back.onclick = () => { snd.pop(); chips(); };
    ctl.append(line, row, back); mark();
  }
  async function ask(){
    const {t, c, Q} = cs.q, g = cs.thief.g, pron = Q.pron[g], yes = has(cs.thief, t);
    const text = Q.frame(t).map(p => typeof p === "number" ? c[p] : p).join(lang() === "zh" ? "" : " ");
    const good = Q.good(c, t) && c[Q.pi] === pron;
    cs.stage = "wait"; cs.asked++; ctl.innerHTML = "";
    const mine = el("div", "dt-q dt-mine", "🙋 " + text); ctl.append(mine);
    move(mine, [{transform:"scale(.3)", opacity:0}, {transform:"scale(1.08)", opacity:1}, {transform:"none"}], {duration:300});
    // a question built wrong is kept as it was said, for the parents' notes
    logRound("q:" + t.id, good, 1, {lvl, q:text});
    if (good) addStar(); else cs.ok = false;
    await talk(text, "kid", t.key);
    if (!alive(gen)) return;
    if (!good) {
      move(owl.querySelector(".dt-owlb"), [{transform:"rotate(0)"}, {transform:"rotate(18deg)"}, {transform:"rotate(18deg)", offset:.8}, {transform:"rotate(0)"}], {duration:1400});
      await setLine(() => ({text:cat(U().mean, D.ask[lang()].right(D.ask[lang()].pron[g], t)), keys:t.key}));
      if (!alive(gen)) return;
    }
    const isForm = good && c[0] === "Is";
    await setLine(() => { const A = D.ask[lang()]; return {text:A.reply(isForm && lang() === "en", A.pron[g], t, yes), keys:t.key}; });
    if (!alive(gen)) return;
    flap();
    aliveS().filter(s => has(s, t) !== yes).forEach(s => out(s, one(D.notMe[lang()])));
    snd.pouet();
    await wait(800);
    if (!alive(gen)) return;
    if (aliveS().length > 1) { setLine(() => ({text:U().ask}), false); return chips(); }
    if (lvl === 4 && cs.asked <= 3) { addStar(); confetti(20); await owlSay({text:U().super}); if (!alive(gen)) return; }
    startCatch();
  }

  /* ---------- catch the thief ---------- */
  function startCatch(){
    cs.stage = "catch"; ctl.innerHTML = "";
    setLine(() => ({text:U().catch})); mark();
    if (fx.calm()) return;
    let x = 0, y = 0;
    const hop = () => {
      if (cs.stage !== "catch") return;
      const b = cs.thief.b, g = grid.getBoundingClientRect(), r = b.getBoundingClientRect();
      const nx = x + g.left - r.left + Math.random() * (g.width - r.width), ny = y + g.top - r.top + Math.random() * (g.height - r.height);
      b.style.zIndex = 5;
      b.animate([{transform:`translate(${x}px,${y}px)`}, {transform:`translate(${(x + nx) / 2}px,${(y + ny) / 2 - 30}px) rotate(${rnd(2) ? 14 : -14}deg)`}, {transform:`translate(${nx}px,${ny}px)`}],
        {duration:420, easing:"ease-in-out", fill:"forwards"});
      x = nx; y = ny; snd.step();
    };
    loops.push(setTimeout(() => { if (cs.stage !== "catch") return; hop(); cs.hop = setInterval(hop, 1300); loops.push(cs.hop); }, 900));
  }
  async function caught(){
    const find = cs.stage === "find", s = cs.thief, b = s.b;
    cs.stage = "done"; mark(); clearInterval(cs.hop);
    b.getAnimations().forEach(a => a.cancel()); b.classList.remove("dt-hint"); b.style.zIndex = 5;
    if (find) { logRound(clueKey(clue()), cs.tries === 0, cs.tries + 1, {lvl}); if (cs.tries === 0) addStar(); else sfx.ok(); }
    snd.siren(); flap();
    const light = el("div", "dt-siren", "🚨"); b.append(light);
    move(light, [{opacity:1}, {opacity:.2}], {duration:250, iterations:8, direction:"alternate"});
    move(b, [{transform:"scale(1)"}, {transform:"scale(1.3) rotate(-4deg)"}, {transform:"scale(1.18) rotate(3deg)"}, {transform:"scale(1.2)"}], {duration:700, easing:"ease-out", fill:"forwards"});
    await setLine(() => ({text:U().got}));
    if (!alive(gen)) return;
    // blushing, with crumbs on the chin (or the stolen thing pops out)
    move(b.querySelector(".dt-ch"), [{opacity:.45, transform:"scale(1)"}, {opacity:1, transform:"scale(1.6)"}], {duration:500, fill:"forwards"});
    b.querySelector(".dt-ch").setAttribute("opacity", 1);
    b.querySelector(".dt-mo").setAttribute("d", "M44 66Q50 60 56 66Q50 71 44 66Z");
    if (cs.eat) { b.querySelector(".dt-cr").setAttribute("opacity", 1); await wait(350); snd.burp(); pop(s, "💨", 900); await wait(700); }
    else { pop(s, cs.w.e, 900); snd.boing(); await wait(700); }
    if (!alive(gen)) return;
    const conf = one((cs.eat ? D.confessEat : D.confessTake)[lang()]);
    pop(s, conf, 3200);
    await talk(conf, "thief");
    if (!alive(gen)) return;
    const st = el("div", "dt-stamp", U().closed); gw.append(st); snd.thud();
    move(st, [{transform:"translate(-50%,-50%) rotate(-30deg) scale(3)", opacity:0}, {transform:"translate(-50%,-50%) rotate(-12deg) scale(1)", opacity:1}], {duration:320, easing:"cubic-bezier(.3,1.5,.5,1)"});
    res.push(cs.ok ? 1 : 0); renderDots(res, plan.length, -1);
    await setLine(() => ({text:U().praise[cs.op % 4]}));
    await wait(900);
    if (!alive(gen)) return;
    ci++; next();
  }
  next();
});
})();
