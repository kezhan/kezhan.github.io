/* L'Île aux Mots : jeu « Plus grand, plus petit » (id plusgrand), en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   Une balance à deux plateaux : lequel est le plus grand, le plus petit, ou sont-ils pareils ? Une fois la réponse trouvée, la balance
   penche du côté du plus grand, le signe >, < ou = apparaît au milieu et la voix dit la phrase entière (« Fifty-six is bigger than fifty. »).
   1 : deux tas d'images jusqu'à 5, plus, moins ou pareil (3 boutons, tout par l'image et la voix)
   2 : deux nombres jusqu'à 20, dits et écrits en lettres (en chiffres pour la petite), pièges 6 et 16, 12 et 20
   3 : nombres jusqu'à 100 écrits en lettres ou en chiffres, à lire (56 ou 65, 14 ou 40, 39 ou 40, sechsundfünfzig ou 65),
       et « quel nombre fait dix de plus / dix de moins que … ? »
   4 : calculs à comparer (7 × 8 ou 50 ?, 36 + 9 ou 44 ?, 4 × 9 ou 6 × 6 ?) et phrases écrites à choisir (« 7 × 8 ist größer als 50 »)
   Luxembourgeois vérifié sur lod.lu : GROUSS2 et KLENG1 (comparatifs « méi grouss », « méi kleng »), MEI1 (« méi al ewéi mäi Brudder »),
   EWEI1 (« than » : « esou grouss ewéi ech »), MANNER1 (« manner ewéi »), ESOU1 (« esou vill »), SELWECHT1 (« d'selwecht (ewéi) »),
   SAIT1 (d'Säit), ZUEL1 (d'Zuel), SAZ1 (de Saz), VERGLAICHEN1. Règle de l'Eifel : « siwe mol aacht », « Zuele vergläichen », « wéi ee Saz ».
   À faire relire en luxembourgeois : « Wéi eng Säit huet méi / manner? », « Wéi ee Saz stëmmt? », « X ass esou grouss ewéi Y » pour
   l'égalité d'un calcul, « Wéi eng Zuel ass zéng méi ewéi … ? ». Le luxembourgeois n'a pas de voix pour les nombres : tout est écrit. */
addStyle(`
.plusgrand{display:flex; flex-direction:column; gap:12px}
.plusgrand .plusgrand-scale{display:flex; flex-direction:column; align-items:center; padding-top:4px}
.plusgrand .plusgrand-pans{display:grid; grid-template-columns:minmax(0,1fr) 40px minmax(0,1fr); gap:8px; align-items:end; width:100%; max-width:460px}
.plusgrand .plusgrand-hang{min-width:0; transition:transform .7s cubic-bezier(.3,1.5,.5,1); animation:plusgrand-rockL 2.6s ease-in-out infinite}
.plusgrand .plusgrand-hang.r{animation-name:plusgrand-rockR}
.plusgrand .plusgrand-pan{position:relative; width:100%; min-height:118px; background:#fff; padding:8px 6px; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:2px; font-family:var(--display); color:var(--ink); text-align:center}
.plusgrand div.plusgrand-pan{border-style:dashed}
.plusgrand .plusgrand-big{font-weight:700; line-height:1.1; max-width:100%; overflow-wrap:anywhere}
.plusgrand .plusgrand-big.d{font-size:44px}
.plusgrand .plusgrand-big.w{font-size:clamp(18px,5vw,26px)}
.plusgrand .plusgrand-big.w.z{font-size:34px}
.plusgrand .plusgrand-big.x{font-size:clamp(24px,7vw,34px); white-space:nowrap}
.plusgrand .plusgrand-pile{display:flex; flex-wrap:wrap; justify-content:center; gap:2px 4px; max-width:112px; font-size:30px; line-height:1.15}
.plusgrand .plusgrand-pile i{font-style:normal}
.plusgrand .plusgrand-sub{font-size:14px; font-weight:700; color:var(--ink-soft); max-width:100%; overflow-wrap:anywhere}
.plusgrand .plusgrand-rev{font-size:17px; font-weight:700; color:var(--sea-deep); opacity:0; transform:scale(.4); transition:opacity .3s, transform .4s cubic-bezier(.3,1.6,.5,1)}
.plusgrand .plusgrand-rev.on{opacity:1; transform:none}
.plusgrand .plusgrand-sign{position:relative; align-self:center; width:40px; height:40px; border-radius:50%; background:var(--sun); border:3px solid var(--ink); display:grid; place-items:center; font-family:var(--display); font-size:24px; font-weight:700}
.plusgrand .plusgrand-sign.on{background:var(--leaf); color:#fff; animation:plusgrand-pop .5s ease-out}
.plusgrand .plusgrand-float{position:absolute; left:50%; top:-8px; font-size:30px; pointer-events:none; opacity:0}
.plusgrand .plusgrand-beam{width:92%; max-width:440px; height:10px; margin-top:6px; background:var(--ink); border-radius:6px; transition:transform .7s cubic-bezier(.3,1.5,.5,1); animation:plusgrand-rock 2.6s ease-in-out infinite}
.plusgrand .plusgrand-foot{width:0; height:0; border-left:18px solid transparent; border-right:18px solid transparent; border-bottom:26px solid var(--ink)}
.plusgrand.done .plusgrand-hang, .plusgrand.done .plusgrand-beam, .plusgrand.still .plusgrand-hang, .plusgrand.still .plusgrand-beam{animation:none}
.plusgrand .plusgrand-line{min-height:1.4em; text-align:center; font-family:var(--display)}
.plusgrand .plusgrand-line b{display:block; font-size:24px}
.plusgrand .plusgrand-line small{display:block; font-size:16px; color:var(--ink-soft); font-weight:600}
.plusgrand .plusgrand-answers{display:flex; flex-wrap:wrap; justify-content:center; gap:10px}
.plusgrand .plusgrand-answers.grid{display:grid; grid-template-columns:repeat(2,minmax(0,1fr))}
.plusgrand .plusgrand-answers.col{display:grid; grid-template-columns:minmax(0,1fr)}
.plusgrand .plusgrand-same{background:#fff; font-family:var(--display); font-size:22px; font-weight:600; padding:10px 20px; display:flex; align-items:center; gap:8px}
.plusgrand .plusgrand-same b{font-size:28px}
.plusgrand .plusgrand-card{background:#fff; font-family:var(--display); font-size:34px; font-weight:700; padding:10px; min-height:64px}
.plusgrand .plusgrand-card.t{font-size:20px; font-weight:600; text-align:left; padding:12px 14px; overflow-wrap:anywhere}
.plusgrand .ok{background:#C9F2DF}
.plusgrand .ko{background:#FFD6CF; animation:plusgrand-shake .4s}
@keyframes plusgrand-rock{0%,100%{transform:rotate(-1.5deg)} 50%{transform:rotate(1.5deg)}}
@keyframes plusgrand-rockL{0%,100%{transform:translateY(4px)} 50%{transform:translateY(-4px)}}
@keyframes plusgrand-rockR{0%,100%{transform:translateY(-4px)} 50%{transform:translateY(4px)}}
@keyframes plusgrand-pop{0%{transform:scale(.3)} 60%{transform:scale(1.35)} 100%{transform:scale(1)}}
@keyframes plusgrand-shake{0%,100%{transform:translateX(0)} 25%{transform:translateX(-8px)} 75%{transform:translateX(8px)}}
`);

// everything the child sees or hears, in the language being learnt; each question has one or two wordings, so the instruction changes
const PLUSGRAND_TXT = {
  en: {
    more: ["Which side has more?", "Where are there more?"], less: ["Which side has fewer?", "Where are there fewer?"],
    big: ["Which number is bigger?", "Tap the bigger number!"], small: ["Which number is smaller?", "Tap the smaller number!"],
    bigX: ["Which is bigger?", "Which side is bigger?"], smallX: ["Which is smaller?", "Which side is smaller?"],
    tenMore: n => `Which number is ten more than ${n}?`, tenLess: n => `Which number is ten less than ${n}?`,
    truth: "Which sentence is true?", same: "the same", sameQ: "the same", or: (a, b) => `${a} or ${b}?`,
    gt: (a, b) => `${a} is bigger than ${b}`, lt: (a, b) => `${a} is smaller than ${b}`, eq: (a, b) => `${a} is the same as ${b}`,
    moreThan: (a, b) => `${a} is more than ${b}`, lessThan: (a, b) => `${a} is less than ${b}`, eqQ: (a, b) => `${a} is the same as ${b}`,
    ten: {more: (a, b) => `${a} is ten more than ${b}`, less: (a, b) => `${a} is ten less than ${b}`},
    is: (x, v) => `${x} is ${v}`, notSame: "Not the same!", no: v => `No, that's ${v}.`, again: "Again",
    op: {"+": "plus", "−": "minus", "×": "times"}
  },
  // German: "Tipp auf die größere Zahl" (auf + accusative, die Zahl), "ist gleich" for equal values
  de: {
    more: ["Welche Seite hat mehr?", "Wo sind mehr?"], less: ["Welche Seite hat weniger?", "Wo sind weniger?"],
    big: ["Welche Zahl ist größer?", "Tipp auf die größere Zahl!"], small: ["Welche Zahl ist kleiner?", "Tipp auf die kleinere Zahl!"],
    bigX: ["Was ist größer?", "Welche Seite ist größer?"], smallX: ["Was ist kleiner?", "Welche Seite ist kleiner?"],
    tenMore: n => `Welche Zahl ist zehn mehr als ${n}?`, tenLess: n => `Welche Zahl ist zehn weniger als ${n}?`,
    truth: "Welcher Satz stimmt?", same: "gleich", sameQ: "gleich viele", or: (a, b) => `${a} oder ${b}?`,
    gt: (a, b) => `${a} ist größer als ${b}`, lt: (a, b) => `${a} ist kleiner als ${b}`, eq: (a, b) => `${a} ist gleich ${b}`,
    moreThan: (a, b) => `${a} ist mehr als ${b}`, lessThan: (a, b) => `${a} ist weniger als ${b}`, eqQ: (a, b) => `${a} ist gleich ${b}`,
    ten: {more: (a, b) => `${a} ist zehn mehr als ${b}`, less: (a, b) => `${a} ist zehn weniger als ${b}`},
    is: (x, v) => `${x} ist ${v}`, notSame: "Das ist nicht gleich!", no: v => `Nein, das ist ${v}.`, again: "Nochmal",
    op: {"+": "plus", "−": "minus", "×": "mal"}
  },
  // Luxembourgish: comparative "méi grouss ewéi" (lod.lu MEI1, EWEI1), "manner ewéi" (MANNER1), "d'selwecht" (SELWECHT1);
  // feminine "wéi eng Säit / Zuel", masculine "wéi ee Saz" (n dropped before S)
  lb: {
    more: ["Wéi eng Säit huet méi?"], less: ["Wéi eng Säit huet manner?"],
    big: ["Wéi eng Zuel ass méi grouss?", "Wat ass méi grouss?"], small: ["Wéi eng Zuel ass méi kleng?", "Wat ass méi kleng?"],
    bigX: ["Wat ass méi grouss?", "Wéi eng Säit ass méi grouss?"], smallX: ["Wat ass méi kleng?", "Wéi eng Säit ass méi kleng?"],
    tenMore: n => `Wéi eng Zuel ass zéng méi ewéi ${n}?`, tenLess: n => `Wéi eng Zuel ass zéng manner ewéi ${n}?`,
    truth: "Wéi ee Saz stëmmt?", same: "d'selwecht", sameQ: "d'selwecht", or: (a, b) => `${a} oder ${b}?`,
    gt: (a, b) => `${a} ass méi grouss ewéi ${b}`, lt: (a, b) => `${a} ass méi kleng ewéi ${b}`, eq: (a, b) => `${a} ass esou grouss ewéi ${b}`,
    moreThan: (a, b) => `${a} ass méi ewéi ${b}`, lessThan: (a, b) => `${a} ass manner ewéi ${b}`, eqQ: (a, b) => `${a} ass esou vill ewéi ${b}`,
    ten: {more: (a, b) => `${a} ass zéng méi ewéi ${b}`, less: (a, b) => `${a} ass zéng manner ewéi ${b}`},
    is: (x, v) => `${x} ass ${v}`, notSame: "Dat ass net d'selwecht!", no: v => `Neen, dat ass ${v}.`, again: "Nach eng Kéier",
    op: {"+": "plus", "−": "minus", "×": "mol"}
  },
  // Chinese: 比…大 / 比…小, 和…一样大; 多 / 少 for things that are counted
  zh: {
    more: ["哪边多？", "哪边的东西多？"], less: ["哪边少？", "哪边的东西少？"],
    big: ["哪个数大？", "点一下比较大的数！"], small: ["哪个数小？", "点一下比较小的数！"],
    bigX: ["哪边大？", "哪个大？"], smallX: ["哪边小？", "哪个小？"],
    tenMore: n => `比${n}多十的数是多少？`, tenLess: n => `比${n}少十的数是多少？`,
    truth: "哪句话是对的？", same: "一样大", sameQ: "一样多", or: (a, b) => `${a}还是${b}？`,
    gt: (a, b) => `${a}比${b}大`, lt: (a, b) => `${a}比${b}小`, eq: (a, b) => `${a}和${b}一样大`,
    moreThan: (a, b) => `${a}比${b}多`, lessThan: (a, b) => `${a}比${b}少`, eqQ: (a, b) => `${a}和${b}一样多`,
    ten: {more: (a, b) => `${a}比${b}多十`, less: (a, b) => `${a}比${b}少十`},
    is: (x, v) => `${x}等于${v}`, notSame: "不一样！", no: v => `不对，那是${v}。`, again: "再听一次",
    op: {"+": "加", "−": "减", "×": "乘"}
  }
};
const PLUSGRAND_ITEMS = ["🍎", "🍓", "🐟", "⭐", "🍪", "🐞", "🦆", "🎈", "🌸", "🐸", "🍌", "🚗"];

registerGame({id:"plusgrand", em:"🐘", name:"Plus grand, plus petit", desc:"Comparer des quantités, des nombres, des calculs", multi:true, cat:"nombres",
  title:{en:"Bigger or smaller?", de:"Größer oder kleiner?", lb:"Méi grouss oder méi kleng?", zh:"比大小"},
  sub:{en:"More, less or the same", de:"Mehr, weniger oder gleich", lb:"Méi, manner oder d'selwecht", zh:"谁大，谁小，还是一样？"}}, function () {
  const lang = ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en"; // never French for the children
  const T = PLUSGRAND_TXT[lang], lvl = levelOf("plusgrand"), little = S.kid === "p4";
  const total = lvl === 1 ? 6 : 8, res = [];
  const num = n => numberIn(n, lang), sp = lang === "zh" ? "" : " ";
  const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
  const sent = s => lang === "zh" ? s + "。" : cap(s) + ".";
  // Luxembourgish n-rule: "siwen" loses its n before a consonant other than d, t, z, h, n ("siwe mol aacht")
  const fix = s => lang === "lb" ? s.replace(/\b(siwe)n (?=[^aeiouäéëöüdtzhn\s\d])/gi, "$1 ") : s;
  const speak = t => say(fix(t), lang);
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };
  const snd = {clunk: () => tone([196, 131], .11, "sine"), even: () => tone([523, 659, 523, 659], .07), boing: () => tone([440, 880], .07, "square")};
  // a soft hyphen before the tens, so that "siebenundneunzig" or "siwenanzwanzeg" can break inside a narrow pan
  const soft = n => {
    const w = num(n), d = NUMS_IN[lang];
    if (!d || !d.tens || n < 21 || n % 10 === 0 || n === 100) return w;
    const t = d.tens[Math.floor(n / 10)];
    if (!w.endsWith(t)) return w;
    const head = w.slice(0, -t.length);
    return (lang === "de" ? head.replace(/und$/, "­und") : head) + "­" + t;
  };

  /* the two sides: v (value), big (what the pan shows), rev (shown once answered), spk (said), txt (written in a sentence) */
  const nSide = (n, words) => ({v:n, key:String(n), spk:num(n), txt:String(n), big:words ? soft(n) : String(n), cls:words ? (lang === "zh" ? "w z" : "w") : "d", rev:words ? String(n) : ""});
  const qSide = (n, it) => ({v:n, key:String(n), spk:num(n), txt:String(n), qty:true, cls:"q", rev:String(n),
    big:`<span class="plusgrand-pile">${Array(n).fill(`<i>${it}</i>`).join("")}</span>`});
  const eSide = (a, op, b) => {
    const v = op === "×" ? a * b : op === "+" ? a + b : a - b;
    return {v, key:`${a}${op}${b}`, spk:[num(a), T.op[op], num(b)].join(sp), txt:`${a} ${op} ${b}`, big:`${a} ${op} ${b}`, cls:"x", expr:true, rev:`= ${v}`};
  };

  /* level 2: to 20, look-alikes (sechs / sechzehn, zwölf / zwanzig) */
  const pair2 = () => {
    const g = rnd(4);
    if (g === 0) { const n = 1 + rnd(9); return [n, n + 10]; }
    if (g === 1) { const n = 1 + rnd(19); return [n, n + 1]; }
    if (g === 2) return pick([[12, 20], [2, 12], [13, 3], [11, 7], [19, 9], [20, 2]], 1)[0];
    let a, b; do { a = 1 + rnd(20); b = 1 + rnd(20); } while (a === b); return [a, b];
  };
  /* level 3: to 100, traps for readers of number words (56 / 65, 14 / 40, 47 / 57, 39 / 40) */
  const pair3 = () => {
    const g = rnd(5);
    if (g === 0) { let t, u; do { t = 1 + rnd(9); u = 1 + rnd(9); } while (t === u); return [t * 10 + u, u * 10 + t]; }
    if (g === 1) { const n = 3 + rnd(7); return [10 + n, n * 10]; }
    if (g === 2) { const n = 11 + rnd(80); return [n, n + 10]; }
    if (g === 3) { const n = 2 + rnd(9); return [n * 10 - 1, n * 10]; }
    let a, b; do { a = 10 + rnd(91); b = 10 + rnd(91); } while (a === b); return [a, b];
  };
  const swapDigits = x => x >= 10 && x < 100 && x % 10 ? (x % 10) * 10 + Math.floor(x / 10) : 0;
  const findRound = more => {
    const n = 12 + rnd(77), right = more ? n + 10 : n - 10;
    const near = [...new Set([more ? n + 1 : n - 1, more ? n - 10 : n + 10, swapDigits(right), right + 1, right - 1])].filter(x => x > 0 && x <= 100 && x !== right && x !== n);
    return {type:"find", ask:more ? "more10" : "less10", n, right, L:nSide(n, true), R:nSide(right, false), choices:shuffle([right, ...near.slice(0, 3)])};
  };
  /* level 4: sums with a carry, differences with a borrow, times tables close to each other, and the same value in two shapes */
  const N = n => nSide(n, false);
  const pair4 = kind => {
    if (kind === "mul") {
      const a = 3 + rnd(7), b = 3 + rnd(7), p = a * b, tens = Math.round(p / 10) * 10;
      return [eSide(a, "×", b), N(pick([p - 1, p + 1, p - 2, p + 2, tens !== p ? tens : p + 3], 1)[0])];
    }
    if (kind === "add") {
      let a, b; do { a = 21 + rnd(69); b = 4 + rnd(6); } while (a % 10 + b < 10 || a + b > 100);
      const s = a + b; return [eSide(a, "+", b), N(pick([s - 1, s + 1, s - 10, s + 2], 1)[0])];
    }
    if (kind === "sub") {
      let a, b; do { a = 31 + rnd(65); b = 4 + rnd(6); } while (a % 10 >= b);
      const s = a - b; return [eSide(a, "−", b), N(pick([s - 1, s + 1, s + 10, s - 2], 1)[0])];
    }
    let a, b, c, d;
    if (kind === "mm") { // 7 × 8 or 6 × 9? no shared factor, at most 4 apart
      do { a = 2 + rnd(8); b = 2 + rnd(8); c = 2 + rnd(8); d = 2 + rnd(8); }
      while (a * b === c * d || Math.abs(a * b - c * d) > 4 || [c, d].includes(a) || [c, d].includes(b));
      return [eSide(a, "×", b), eSide(c, "×", d)];
    }
    if (rnd(2)) { // 4 × 9 and 6 × 6
      do { a = 2 + rnd(8); b = 2 + rnd(8); c = 2 + rnd(8); d = 2 + rnd(8); }
      while (a * b !== c * d || (a === c && b === d) || (a === d && b === c));
      return [eSide(a, "×", b), eSide(c, "×", d)];
    }
    a = 3 + rnd(7); b = 3 + rnd(7); return [eSide(a, "×", b), N(a * b)];
  };
  const ask2 = () => pick(["big", "small"], 1)[0];
  const swap = p => rnd(2) ? [p[1], p[0]] : p;

  /* the rounds, planned at the start */
  let plan;
  if (lvl === 1) {
    plan = ["more", ...shuffle(["more", "less", "less", "same", "more"])].map(k => {
      let a, b;
      if (k === "same") a = b = 2 + rnd(4);
      else do { a = 1 + rnd(5); b = 1 + rnd(5); } while (a === b || (Math.max(a, b) > 3 && Math.abs(a - b) < 2));
      const it = pick(PLUSGRAND_ITEMS, 1)[0];
      return {type:"cmp", qty:true, ask:k === "same" ? pick(["more", "less"], 1)[0] : k, L:qSide(a, it), R:qSide(b, it)};
    });
  } else if (lvl === 2) {
    // the little one reads digits (the word under it), the big one reads the words; "the same" hides behind a word and its digits
    const side = (n, words) => little ? {...nSide(n, false), sub:num(n)} : nSide(n, words);
    plan = ["big", ...shuffle(["big", "big", "small", "small", "small", "big", "same"])].map(k => {
      const [a, b] = k === "same" ? (n => [n, n])(2 + rnd(19)) : swap(pair2());
      const [L, R] = swap([side(a, true), side(b, k !== "same")]);
      return {type:"cmp", ask:k === "same" ? ask2() : k, L, R};
    });
  } else if (lvl === 3) {
    plan = ["cmp", ...shuffle(["cmp", "cmp", "cmp", "cmp", "same", "more10", "less10"])].map(k => {
      if (k === "more10" || k === "less10") return findRound(k === "more10");
      const [a, b] = k === "same" ? (n => [n, n])(21 + rnd(79)) : swap(pair3());
      const wa = k === "same" || rnd(3) > 0, wb = k === "same" ? false : !wa || rnd(2) === 0; // at least one side in words
      const [L, R] = swap([nSide(a, wa), nSide(b, wb)]);
      return {type:"cmp", ask:ask2(), L, R};
    });
  } else {
    const bal = ["mul", ...shuffle(["add", "mm", "sub", "eq"])].map(k => ({k, truth:false}));
    const tru = shuffle(["mul", "add", "sub", "mm", "eq"]).slice(0, 3).map(k => ({k, truth:true}));
    plan = [bal[0], ...shuffle([...bal.slice(1), ...tru])].map(({k, truth}) => {
      const [L, R] = swap(pair4(k));
      if (!truth) return {type:"cmp", ask:ask2(), L, R};
      const rel = L.v > R.v ? "gt" : L.v < R.v ? "lt" : "eq";
      return {type:"truth", L, R, choices:shuffle(["gt", "lt", "eq"].map(x => ({txt:T[x](L.txt, R.txt), ok:x === rel})))};
    });
  }
  plan.forEach(r => { r.var = rnd(2); });

  const question = r => r.type === "find" ? T[r.ask === "more10" ? "tenMore" : "tenLess"](num(r.n)) : r.type === "truth" ? T.truth
    : (a => a[r.var % a.length])(T[r.ask + (lvl >= 4 ? "X" : "")]);
  const emoji = r => r.type === "find" ? "🔟" : r.type === "truth" ? "🧐" : ["big", "more"].includes(r.ask) ? "🐘" : "🐭";
  // "Seven is more than three." / "Fifty-six is bigger than fifty."
  const relTxt = (a, b, qty) => (a.v > b.v ? (qty ? T.moreThan : T.gt) : a.v < b.v ? (qty ? T.lessThan : T.lt) : (qty ? T.eqQ : T.eq))(a.spk, b.spk);
  // the answer in a full sentence, the side asked for first
  const answerTxt = r => {
    if (r.type === "find") return T.ten[r.ask === "more10" ? "more" : "less"](num(r.right), num(r.n));
    if (r.type === "truth" || r.L.v === r.R.v) return relTxt(r.L, r.R, r.qty);
    return ["big", "more"].includes(r.ask) === (r.L.v > r.R.v) ? relTxt(r.L, r.R, r.qty) : relTxt(r.R, r.L, r.qty);
  };
  const panHtml = s => `<span class="plusgrand-big ${s.cls}">${s.big}</span>${s.sub ? `<small class="plusgrand-sub">${s.sub}</small>` : ""}<small class="plusgrand-rev">${s.rev || "&nbsp;"}</small>`;
  const mathTxt = s => s.expr ? `${s.txt} (${s.v})` : String(s.v);

  startSession("plusgrand", null, total); const gen = GEN;
  const later = (fn, ms) => loops.push(setTimeout(() => { if (alive(gen)) fn(); }, ms));
  let i = 0;

  function round(){
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const r = plan[i]; let tries = 0, locked = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    const wrap = el("div", "plusgrand" + (fx.calm() ? " still" : ""));
    const q = question(r);
    // level 2 hears the two numbers too; from level 3 he reads them (🔊 🔢 reads them aloud, counted as a hint)
    const spoken = lvl === 2 && r.type === "cmp" ? q + sp + T.or(cap(r.L.spk), r.R.spk) : q;
    wrap.append(el("p", "prompt", `${emoji(r)} ${q}`));
    const tools = el("div", "row"); tools.style.justifyContent = "center";
    tools.append(speakBtn(() => fix(spoken), T.again, lang));
    if (lvl >= 3 && r.type !== "find") {
      const h = el("button", "chip", "🔊 🔢");
      h.onclick = () => { G.hints++; speak(T.or(cap(r.L.spk), r.R.spk)); };
      tools.append(h);
    }
    wrap.append(tools);

    // the balance: two pans, the sign between them, the beam and its foot
    const clickable = r.type === "cmp";
    const hangL = el("div", "plusgrand-hang l"), hangR = el("div", "plusgrand-hang r");
    const panL = el(clickable ? "button" : "div", "plusgrand-pan chunky", panHtml(r.L));
    const panR = el(clickable ? "button" : "div", "plusgrand-pan chunky", r.type === "find" ? panHtml({big:"?", cls:"d"}) : panHtml(r.R));
    hangL.append(panL); hangR.append(panR);
    const sign = el("div", "plusgrand-sign", "?");
    const pans = el("div", "plusgrand-pans"); pans.append(hangL, sign, hangR);
    const beam = el("div", "plusgrand-beam"), scale = el("div", "plusgrand-scale");
    scale.append(pans, beam, el("div", "plusgrand-foot"));
    const line = el("div", "plusgrand-line");
    wrap.append(scale, line);

    const reveal = () => wrap.querySelectorAll(".plusgrand-rev").forEach(x => x.classList.add("on"));
    const floaty = (host, txt) => {
      if (fx.calm()) return;
      const f = el("span", "plusgrand-float", txt); host.append(f);
      const a = f.animate([{transform:"translate(-50%,0) scale(.5)", opacity:1}, {transform:"translate(-50%,-64px) scale(1.3)", opacity:0}], {duration:950, easing:"ease-out"});
      a.onfinish = () => f.remove();
    };
    // the heavier side goes down with a bounce, the lighter one flies up and its load hops
    function tilt(lv, rv){
      const d = lv > rv ? -1 : lv < rv ? 1 : 0; // 1: the right side goes down
      sign.textContent = d < 0 ? ">" : d > 0 ? "<" : "=";
      sign.classList.add("on"); wrap.classList.add("done");
      beam.style.transform = `rotate(${d * 6}deg)`;
      hangL.style.transform = `translateY(${-d * 16}px)`;
      hangR.style.transform = `translateY(${d * 16}px)`;
      if (d) {
        snd.clunk();
        const light = d > 0 ? panL : panR, load = light.querySelector(".plusgrand-big");
        if (!fx.calm() && load) load.animate([{transform:"translateY(0)"}, {transform:"translateY(-22px) rotate(-8deg)", offset:.4}, {transform:"translateY(0)"}], {duration:560, delay:250, easing:"ease-out"});
        later(snd.boing, 300); floaty(light, "💨");
      } else { snd.even(); fx.bounce(panL); fx.bounce(panR); floaty(sign, "🤝"); }
      if (!fx.calm()) { const b = sign.getBoundingClientRect(); fx.sparkle(b.left + b.width / 2, b.top + b.height / 2, 8); }
    }
    function miss(b, text){
      tries++; sfx.ko(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
      if (text) speak(text);
    }
    // the values of the sums: "Seven times eight is fifty-six."
    const values = () => [r.L, r.R].filter(s => s.expr).map(s => sent(T.is(s.spk, num(s.v)))).join(sp);
    async function win(b){
      locked = true;
      wrap.querySelectorAll("[data-ok]").forEach(x => delete x.dataset.ok); // the round is over: nothing left to find
      if (b) b.classList.add("ok");
      sfx.ok();
      const first = tries === 0; if (first) addStar(); else fx.sparkle();
      const key = r.type === "find" ? `${r.n}${r.ask === "more10" ? "+" : "−"}10` : `${r.L.key}|${r.R.key}:${r.type === "truth" ? "true" : r.ask}`;
      logRound(key, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
      if (r.type === "find") panR.innerHTML = panHtml(r.R);
      reveal();
      tilt(r.L.v, r.R.v);
      const s = sent(answerTxt(r));
      line.innerHTML = "<b></b><small></small>";
      line.querySelector("b").textContent = r.type === "find" ? `${r.n} ${r.ask === "more10" ? "+" : "−"} 10 = ${r.right}` : `${mathTxt(r.L)} ${sign.textContent} ${mathTxt(r.R)}`;
      line.querySelector("small").textContent = fix(s); // written too: Luxembourgish has no voice for numbers
      await speak(praise() + sp + s);
      if (!alive(gen)) return;
      i++; later(round, TEST ? 60 : 1300);
    }

    const answers = el("div", "plusgrand-answers");
    if (r.type === "cmp") {
      const same = el("button", "plusgrand-same chunky", `<span>⚖️</span><b>=</b><span>${r.qty ? T.sameQ : T.same}</span>`);
      const want = r.L.v === r.R.v ? "same" : ["big", "more"].includes(r.ask) === (r.L.v > r.R.v) ? "L" : "R";
      const btns = {L:panL, R:panR, same};
      markOk(btns[want]);
      Object.entries(btns).forEach(([k, b]) => b.onclick = () => {
        if (locked) return; G.taps++;
        if (k === want) return win(b);
        let t;
        if (lvl >= 4) { reveal(); t = (k === "same" ? T.notSame + sp : "") + values(); }
        else {
          const [X, Y] = k === "R" ? [r.R, r.L] : [r.L, r.R];
          t = (k === "same" ? T.notSame + sp : "") + sent(relTxt(X, Y, r.qty));
          reveal(); // the digits under the words, the count under the pictures
        }
        if (want === "same") fx.bounce(same);
        miss(b, t);
      });
      answers.append(same);
    } else if (r.type === "find") {
      answers.classList.add("grid");
      r.choices.forEach(v => {
        const b = el("button", "plusgrand-card chunky", String(v));
        if (v === r.right) markOk(b);
        b.onclick = () => { if (locked) return; G.taps++; if (v === r.right) win(b); else miss(b, T.no(num(v))); };
        answers.append(b);
      });
    } else {
      answers.classList.add("col");
      r.choices.forEach(c => {
        const b = el("button", "plusgrand-card t chunky"); b.textContent = c.txt;
        if (c.ok) markOk(b);
        b.onclick = () => { if (locked) return; G.taps++; if (c.ok) return win(b); reveal(); miss(b, values()); };
        answers.append(b);
      });
    }
    wrap.append(answers);
    body.append(wrap);
    speak(spoken);
  }
  round();
});
