/* L'Île aux Mots : jeu « Calcul », compter puis calculer dans la langue apprise (en, de, lb, zh).
   1: count 1 to 5, answer in digits · 2: count to 20, answer in words, look-alike traps (fourteen, forty)
   3: sums and differences said in the language, result to 20 · 4: times tables said, answer to 100 in words.
   The little one always answers in digits; the big one starts at level 3 (he knows his tables). */
Object.assign(ACTS.find(a => a.id === "compte"), {multi:true, start:{p7:3},
  title:{en:"Maths", de:"Rechnen", lb:"Rechnen", zh:"算一算"},
  sub:{en:"Count and calculate", de:"Zählen und rechnen", lb:"Zielen a rechnen", zh:"数数和计算"}});

const CP_TXT = {
  howMany:{en:"How many?", de:"Wie viele?", lb:"Wéi vill?", zh:"有几个？"},
  tap:{en:"Tap them to count", de:"Tipp sie an und zähl mit", lb:"Tipp se un an zielt mat", zh:"点一点，数一数"},
  again:{en:"Again", de:"Nochmal", lb:"Nach eng Kéier", zh:"再听一次"},
  op:{en:{"+":"plus", "−":"minus", "×":"times"}, de:{"+":"plus", "−":"minus", "×":"mal"}, lb:{"+":"plus", "−":"minus", "×":"mol"}, zh:{"+":"加", "−":"减", "×":"乘"}},
  ask:{en:q => `What is ${q}?`, de:q => `Wie viel ist ${q}?`, lb:q => `Wéi vill ass ${q}?`, zh:q => `${q}等于几？`},
  yes:{en:n => `Yes! ${n}!`, de:n => `Ja! ${n}!`, lb:n => `Jo! ${n}!`, zh:n => `对！${n}！`},
  no:{en:n => `No, that's ${n}.`, de:n => `Nein, das ist ${n}.`, lb:n => `Neen, dat ass ${n}.`, zh:n => `不对，那是${n}。`}
};
const CP_ITEMS = ["🐱","🐶","🐷","🦆","🐸","🐰","🍎","🍌","🍓","🥕","⭐","🚗","🎈","🐟","🌸","🍪"];
addStyle(`.cp-pile{display:flex; flex-wrap:wrap; justify-content:center; gap:6px; max-width:420px; margin:0 auto}
  .cp-pile button{position:relative; font-size:40px; width:56px; height:56px; background:none; border:0; cursor:pointer; padding:0}
  .cp-pile button.done{opacity:.45}
  .cp-bub{position:absolute; left:50%; top:-8px; transform:translate(-50%,-100%); font:600 15px var(--display); background:#fff;
    border:2px solid var(--ink); border-radius:12px; padding:1px 7px; white-space:nowrap; pointer-events:none; z-index:2}
  .cp-q{font-family:var(--display); font-size:clamp(26px,7vw,40px); text-align:center; line-height:1.2}
  .cp-q small{display:block; font-size:.55em; opacity:.7}
  .cp-ans{display:grid; grid-template-columns:repeat(2, minmax(0,1fr)); gap:10px}
  .cp-ans .num{font-size:22px; min-height:64px; padding:8px; word-break:break-word}`);

registerGame({id:"compte"}, function () {
  const lang = ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
  const L = o => o[lang], small = S.kid === "p4", lvl = levelOf("compte");
  const words = S.kid === "p7" && lvl >= 2;              // the little one cannot read: digits only
  const say$ = t => say(t, lang), word = n => numberIn(n, lang), shown = n => words ? word(n) : String(n);
  const total = small ? 5 : 8, res = [];
  // look-alikes: 14 and 40, 56 and 65, 7 and 17
  const twins = n => [n + 1, n - 1, n < 10 ? n + 10 : n - 10, n >= 13 && n <= 19 ? (n - 10) * 10 : 0,
    n % 10 && n >= 10 ? (n % 10) * 10 + Math.floor(n / 10) : 0, n % 10 === 0 && n >= 30 ? n / 10 + 10 : 0];
  const qs = [...Array(total)].map(() => {
    if (lvl <= 2) return {kind:"count", n:1 + rnd(lvl === 1 ? 5 : 20), it:CP_ITEMS[rnd(CP_ITEMS.length)]};
    if (lvl === 3) {
      if (rnd(2)) { const a = 1 + rnd(10), b = 1 + rnd(10); return {kind:"calc", a, b, op:"+", n:a + b}; }
      const a = 5 + rnd(16), b = 1 + rnd(a - 1); return {kind:"calc", a, b, op:"−", n:a - b};
    }
    const a = 2 + rnd(9), b = 2 + rnd(9); return {kind:"calc", a, b, op:"×", n:a * b};
  });
  startSession("compte", null, total);
  let i = 0; const gen = GEN;

  function choices(q){
    const max = lvl === 4 ? 100 : 20;
    const extra = lvl === 4 ? [q.n + q.a, q.n - q.a, q.n + q.b, q.n - q.b] : [q.n + 2, q.n - 2];
    // level 1 stays within what a 4-year-old counts; above, traps may leave the range on purpose (fourteen, forty)
    const pool = lvl === 1 ? [1, 2, 3, 4, 5, 6].filter(v => v !== q.n)
      : [...new Set([...twins(q.n), ...extra])].filter(v => v >= 1 && v <= Math.max(max, 90) && v !== q.n);
    return shuffle([q.n, ...shuffle(pool).slice(0, small ? 2 : 3)]);
  }
  const question = q => q.kind === "count" ? L(CP_TXT.howMany) : L(CP_TXT.ask)(`${word(q.a)} ${L(CP_TXT.op)[q.op]} ${word(q.b)}`);
  const pop = (el, k) => { if (!fx.calm() && el) el.animate(k, {duration:450, easing:"cubic-bezier(.3,1.6,.5,1)"}); };

  function round(){
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const q = qs[i]; let tries = 0, locked = false, counted = 0;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    if (q.kind === "count") {
      body.append(el("p", "prompt", `${L(CP_TXT.howMany)}<small>${L(CP_TXT.tap)}</small>`));
      const pile = el("div", "cp-pile");
      for (let j = 0; j < q.n; j++) {
        const s = el("button", "", q.it); s.style.setProperty("--d", j);
        if (!fx.calm()) s.animate([{transform:"scale(0) rotate(-40deg)"}, {transform:"scale(1.2)"}, {transform:"scale(1)"}], {duration:420, delay:j * 45, easing:"ease-out", fill:"backwards"});
        s.onclick = () => {
          if (s.classList.contains("done")) return;
          s.classList.add("done"); G.taps++; counted++;
          const b = el("span", "cp-bub"); b.textContent = word(counted); s.append(b);   // the number word, seen even without a voice
          pop(s, [{transform:"translateY(0)"}, {transform:"translateY(-18px) rotate(10deg)"}, {transform:"translateY(0)"}]);
          say$(word(counted));
        };
        pile.append(s);
      }
      body.append(pile);
    } else {
      const expr = words ? `${word(q.a)} ${L(CP_TXT.op)[q.op]} ${word(q.b)}` : `${q.a} ${q.op} ${q.b}`;
      body.append(el("div", "cp-q", `${expr}<small>${q.a} ${q.op} ${q.b} = ?</small>`));
    }
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => question(q), L(CP_TXT.again), lang)); body.append(row);
    const ans = el("div", "cp-ans");
    choices(q).forEach(v => {
      const b = el("button", "num chunky", shown(v));
      if (v === q.n) markOk(b);
      b.onclick = async () => {
        if (locked) return; G.taps++;
        if (v === q.n) {
          locked = true; b.classList.add("ok"); sfx.ok(); fx.star();
          const first = tries === 0; if (first) addStar();
          const key = q.kind === "count" ? String(q.n) : `${q.a}${q.op}${q.b}`;
          logRound(key, first, tries + 1); res.push(first ? 1 : 0); renderDots(res, total, -1);
          pop(b, [{transform:"scale(1)"}, {transform:"scale(1.25) rotate(-4deg)"}, {transform:"scale(1)"}]);
          await say$(L(CP_TXT.yes)(word(q.n)));
          if (!alive(gen)) return;
          i++; loops.push(setTimeout(round, 350));
        } else {
          tries++; sfx.ko(); fx.wrong(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
          say$(L(CP_TXT.no)(word(v)));
        }
      };
      ans.append(b);
    });
    body.append(ans);
    say$(question(q));
  }
  round();
});
