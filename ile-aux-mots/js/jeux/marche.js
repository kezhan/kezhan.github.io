/* L'Île aux Mots : jeu « Le Marché de l'île » (atelier de conception, 6,7/10). The child keeps the stall.
   1: one item, up to 3 · 2: two items, up to 6 · 3: prices on the stall, how much is it? · 4: the customer pays with a note, what is the change?
   In English, German, Luxembourgish or Chinese, never French, with real grammar: plurals, German accusative, Luxembourgish "zwou", Chinese measure words. */
addStyle(`
.etal{display:grid; grid-template-columns:repeat(auto-fit,minmax(90px,1fr)); gap:10px; padding:12px; background:#FFE3A3; border:3px solid var(--ink); border-radius:18px}
.etal .article{font-size:44px; background:#fff; padding:8px 4px; display:flex; flex-direction:column; align-items:center; gap:2px}
.etal .prix{font-size:15px; font-weight:800; font-family:var(--display)}
.panier{min-height:64px; display:flex; flex-wrap:wrap; justify-content:center; align-items:center; gap:4px; font-size:34px; padding:8px; background:#fff; border:3px dashed var(--ink); border-radius:18px}
.client{font-size:64px; text-align:center; line-height:1}
.bulle-client{align-self:center; background:#fff; border:3px solid var(--ink); border-radius:18px; padding:10px 16px; font-family:var(--display); font-size:20px; font-weight:600; max-width:100%}
.caisse{display:flex; gap:12px; justify-content:center; flex-wrap:wrap}
`);
registerGame({id:"marche", em:"🧺", name:"Market", desc:"Sell fruit and count money", multi:true}, function () {
  const lvl = levelOf("marche"), lang = langOf() === "fr" ? "en" : langOf();
  const DE = ["null","ein","zwei","drei","vier","fünf","sechs","sieben","acht","neun","zehn"];
  const LB = ["null","een","zwee","dräi","véier","fënnef","sechs","siwen","aacht","néng","zéng"];
  const ZH = ["零","一","两","三","四","五","六","七","八","九","十"];
  const ITEMS = [
    {e:"🍎", en:["apple","apples"], de:["m","Apfel","Äpfel"], lb:["m","Apel","Äppel"], zh:["个","苹果"]},
    {e:"🍌", en:["banana","bananas"], de:["f","Banane","Bananen"], lb:["f","Banann","Bananen"], zh:["根","香蕉"]},
    {e:"🍐", en:["pear","pears"], de:["f","Birne","Birnen"], lb:["f","Bir","Biren"], zh:["个","梨"]},
    {e:"🍓", en:["strawberry","strawberries"], de:["f","Erdbeere","Erdbeeren"], lb:["f","Äerdbier","Äerdbieren"], zh:["颗","草莓"]},
    {e:"🥕", en:["carrot","carrots"], de:["f","Karotte","Karotten"], lb:["f","Muert","Muerten"], zh:["根","胡萝卜"]},
    {e:"🥚", en:["egg","eggs"], de:["n","Ei","Eier"], lb:["n","Ee","Eeër"], zh:["个","鸡蛋"]}
  ];
  // "three apples" / "einen Apfel" / "zwou Bananen" / "两根香蕉"
  const qty = (it, n) => {
    if (lang === "en") return `${numberWords(n)} ${it.en[n === 1 ? 0 : 1]}`;
    if (lang === "zh") return `${ZH[n]}${it.zh[0]}${it.zh[1]}`;
    const [g, sg, pl] = it[lang];
    if (n === 1) return lang === "de" ? `${g === "m" ? "einen" : g === "f" ? "eine" : "ein"} ${sg}` : `${g === "f" ? "eng" : "een"} ${sg}`;
    const num = lang === "de" ? DE[n] : (n === 2 && g === "f" ? "zwou" : LB[n]);
    return `${num} ${pl}`;
  };
  const T2 = {
    en: {order: l => `Can I have ${l.join(" and ")}, please?`, much: "How much is it?", change: p => `I pay with ${p} euros. What is my change?`, eur: n => `${n} €`, thanks: "Thank you! Bye!", oops: "Oops! That's not what I asked for."},
    de: {order: l => `Ich hätte gern ${l.join(" und ")}, bitte.`, much: "Wie viel kostet das?", change: p => `Ich zahle mit ${p} Euro. Wie viel bekomme ich zurück?`, eur: n => `${n} €`, thanks: "Danke! Tschüss!", oops: "Oh! Das habe ich nicht bestellt."},
    lb: {order: l => `Ech hätt gär ${l.join(" an ")}, wann ech gelift.`, much: "Wat kascht dat?", change: p => `Ech bezuelen mat ${p} Euro. Wéi vill kréien ech zréck?`, eur: n => `${n} €`, thanks: "Merci! Äddi!", oops: "Oh! Dat hunn ech net bestallt."},
    zh: {order: l => `请给我${l.join("和")}。`, much: "一共多少钱？", change: p => `我付${p}欧元，要找我多少钱？`, eur: n => `${n}欧元`, thanks: "谢谢！再见！", oops: "哎呀！这不是我要的。"}
  }[lang];
  const speak = t => say(t, lang);
  const CUSTOMERS = ["🐻","🐰","🐷","🦊","🐼","🐸","🐨","🦁"];
  const stall = pick(ITEMS, lvl === 1 ? 4 : 6);
  const prices = new Map(stall.map(it => [it, 1 + rnd(3)])); // 1 to 3 euros each
  const total = 5, res = [];
  startSession("marche", null, total); const gen = GEN;
  let i = 0;
  const round = async () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const kinds = pick(stall, lvl === 1 ? 1 : 2);
    const order = new Map(kinds.map(it => [it, 1 + rnd(lvl === 1 ? 3 : 5)]));
    const text = T2.order([...order].map(([it, n]) => qty(it, n)));
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("div", "client bob", pick(CUSTOMERS, 1)[0]));
    // the little one hears the order first; the picture comes into the bubble after the voice
    const bulle = el("div", "bulle-client chunky", lvl === 1 ? "💬" : text);
    body.append(bulle);
    const row = el("div", "row"); row.style.justifyContent = "center"; row.append(speakBtn(() => text, "", () => lang)); body.append(row);
    const basket = new Map(); let tries = 0, done = false;
    const panier = el("div", "panier"); const etal = el("div", "etal");
    const bell = el("button", "bigbtn chunky", "🔔"); bell.style.alignSelf = "center";
    const clear = el("button", "chip", "↩️");
    const drawBasket = () => {
      panier.innerHTML = [...basket].map(([it, n]) => it.e.repeat(n)).join(" ") || "🧺";
      if (TEST) { // the recette taps whatever is still missing, then the bell
        etal.querySelectorAll(".article").forEach((b, k) => { const it = stall[k]; if ((basket.get(it) || 0) < (order.get(it) || 0)) b.dataset.ok = "1"; else delete b.dataset.ok; });
        const full = [...order].every(([it, n]) => basket.get(it) === n); if (full) bell.dataset.ok = "1"; else delete bell.dataset.ok;
      }
    };
    stall.forEach(it => {
      const b = el("button", "article chunky", `${it.e}${lvl >= 3 ? `<span class="prix">${prices.get(it)} €</span>` : ""}`);
      b.onclick = () => { if (done) return; G.taps++; const n = (basket.get(it) || 0) + 1; basket.set(it, n); sfx.tap(); if (typeof fx !== "undefined") fx.bounce(b); speak(lang === "zh" ? ZH[Math.min(n, 10)] : lang === "en" ? numberWords(n) : (lang === "de" ? DE : LB)[Math.min(n, 10)]); drawBasket(); };
      etal.append(b);
    });
    clear.onclick = () => { basket.clear(); drawBasket(); };
    const cash = async () => { // levels 3-4: the price, then the change
      const sum = [...order].reduce((s, [it, n]) => s + n * prices.get(it), 0);
      const pay = [5, 10, 20, 50].find(p => p > sum);
      const ask = lvl === 3 ? T2.much : T2.change(pay), right = lvl === 3 ? sum : pay - sum;
      body.append(el("p", "prompt", ask));
      const choices = [right, ...pick([...new Set([right + 1, right - 1, right + 2, right + 10, right - 2].filter(x => x >= 0 && x !== right))], 3)];
      const caisse = el("div", "caisse");
      shuffle(choices).forEach(v => {
        const c = el("button", "num chunky", T2.eur(v)); if (v === right) markOk(c);
        c.onclick = async () => {
          if (c.dataset.done) return; G.taps++;
          if (v === right) { caisse.querySelectorAll("button").forEach(x => x.dataset.done = 1); c.classList.add("ok"); sfx.ok(); await close(true); }
          else { tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko"); speak(T2.eur(v)); }
        };
        caisse.append(c);
      });
      body.append(caisse); speak(ask);
    };
    const close = async ok => {
      document.querySelectorAll("#gameBody [data-ok]").forEach(x => delete x.dataset.ok); // the round is over: no answer left to find
      const first = ok && tries === 0; if (first) addStar();
      logRound(`${[...order].map(([it, n]) => n + " " + it.en[1]).join(" + ")}${lvl >= 3 ? " €" : ""}`, first, tries + 1, {lvl}); res.push(first ? 1 : 0);
      await speak(T2.thanks); i++; loops.push(setTimeout(round, 500));
    };
    bell.onclick = async () => {
      if (done) return; G.taps++;
      const right = [...order].every(([it, n]) => basket.get(it) === n) && [...basket].every(([it, n]) => order.get(it) === n);
      if (!right) { tries++; sfx.ko(); await speak(T2.oops); speak(text); return; }
      done = true; sfx.ok(); bell.remove(); clear.remove();
      if (lvl <= 2) return close(true);
      cash();
    };
    const tools = el("div", "row"); tools.style.justifyContent = "center"; tools.append(clear);
    body.append(etal, panier, tools, bell);
    drawBasket();
    await speak(text);
    if (lvl === 1 && alive(gen)) bulle.innerHTML = [...order].map(([it, n]) => it.e.repeat(n)).join(" ");
  };
  round();
});
