/* L'Île aux Mots : jeu « Jacques a dit », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   Simon says / Simon sagt / De Simon seet / 西蒙说; every word, the parent's included, is in the language being learnt.
   1: always "Simon says" · 2: traps without "Simon says" · 3: two actions in a row · 4: two actions, counted moves ("Jump three times"), more traps
   Luxembourgish imperatives from lod.lu: beréier, klapp, sprang, dréin dech, setz dech, stéi op, wénk, trampel, maach zou, streck. */
const SIMON_MOVES = [ // no final "!": it is added after joining two moves
  {e:"👃", en:"Touch your nose", de:"Fass dir an die Nase", lb:"Beréier deng Nues", zh:"摸摸你的鼻子"},
  {e:"👏", en:"Clap your hands", de:"Klatsch in die Hände", lb:"Klapp an d'Hänn", zh:"拍拍手",
    n:{en:n => `Clap your hands ${n} times`, de:n => `Klatsch ${n}mal in die Hände`, lb:n => `Klapp ${n}mol an d'Hänn`, zh:n => `拍${n}下手`}},
  {e:"🦘", en:"Jump", de:"Spring", lb:"Sprang", zh:"跳一跳",
    n:{en:n => `Jump ${n} times`, de:n => `Spring ${n}mal`, lb:n => `Sprang ${n}mol`, zh:n => `跳${n}下`}},
  {e:"🔄", en:"Turn around", de:"Dreh dich im Kreis", lb:"Dréin dech am Krees", zh:"转个圈",
    n:{en:n => `Turn around ${n} times`, de:n => `Dreh dich ${n}mal im Kreis`, lb:n => `Dréin dech ${n}mol am Krees`, zh:n => `转${n}圈`}},
  {e:"🪑", en:"Sit down", de:"Setz dich hin", lb:"Setz dech", zh:"坐下"},
  {e:"🧍", en:"Stand up", de:"Steh auf", lb:"Stéi op", zh:"站起来"},
  {e:"👋", en:"Wave hello", de:"Wink mit der Hand", lb:"Wénk mat der Hand", zh:"挥挥手",
    n:{en:n => `Wave hello ${n} times`, de:n => `Wink ${n}mal mit der Hand`, lb:n => `Wénk ${n}mol mat der Hand`, zh:n => `挥${n}下手`}},
  {e:"🦶", en:"Stomp your feet", de:"Stampf mit den Füßen", lb:"Trampel mat de Féiss", zh:"跺跺脚",
    n:{en:n => `Stomp your feet ${n} times`, de:n => `Stampf ${n}mal mit den Füßen`, lb:n => `Trampel ${n}mol mat de Féiss`, zh:n => `跺${n}下脚`}},
  {e:"🙆", en:"Touch your head", de:"Fass dir an den Kopf", lb:"Beréier däi Kapp", zh:"摸摸你的头"},
  {e:"😑", en:"Close your eyes", de:"Mach die Augen zu", lb:"Maach d'Aen zou", zh:"闭上眼睛"},
  {e:"🙌", en:"Put your hands up", de:"Heb die Hände hoch", lb:"Streck d'Hänn an d'Luucht", zh:"举起双手"},
  {e:"😝", en:"Stick out your tongue", de:"Streck die Zunge raus", lb:"Streck d'Zong eraus", zh:"吐吐舌头"}
];
// counted moves: "three times" / "dreimal" / "dräimol" / "三下" (两 before a measure word)
const SIMON_TIMES = {en:n => numberIn(n, "en"), de:n => numberIn(n, "de"), lb:n => numberIn(n, "lb"), zh:n => n === 2 ? "两" : numberIn(n, "zh")};
const SIMON_TXT = {
  en:{says:"Simon says: ", and:"and", bang:"!", again:"Again", done:"✅ Done!", notYet:"🔁 Not yet",
    trap:"Grown-up: it's a trap! Without “Simon says”, don't move.", two:"Grown-up: both moves, in the right order.", first:"Grown-up: do it together the first time.",
    caught:"Good! Simon didn't say!", fooled:"Oops! Simon didn't say!", tryAgain:"Nice try!", referee:"Play with a grown-up: they are the referee!"},
  de:{says:"Simon sagt: ", and:"und", bang:"!", again:"Nochmal", done:"✅ Geschafft!", notYet:"🔁 Noch nicht",
    trap:"Eltern: Falle! Ohne „Simon sagt“ bewegt man sich nicht.", two:"Eltern: beide Bewegungen, in der richtigen Reihenfolge.", first:"Eltern: Macht beim ersten Mal mit.",
    caught:"Gut! Simon hat es nicht gesagt!", fooled:"Hoppla! Simon hat es nicht gesagt!", tryAgain:"Guter Versuch!", referee:"Spiel mit einem Erwachsenen: Er ist der Schiedsrichter!"},
  lb:{says:"De Simon seet: ", and:"an", bang:"!", again:"Nach eng Kéier", done:"✅ Gepackt!", notYet:"🔁 Nach net",
    trap:"Elteren: Fal! Ouni „De Simon seet“ beweegt een sech net.", two:"Elteren: déi zwou Beweegungen, eng no der anerer.", first:"Elteren: Maacht déi éischte Kéier mat.",
    caught:"Gutt! De Simon huet et net gesot!", fooled:"Hoppla! De Simon huet et net gesot!", tryAgain:"Gutt probéiert!", referee:"Spill mat engem Erwuessenen: hien ass den Arbitter!"},
  zh:{says:"西蒙说：", and:"，然后", bang:"！", again:"再听一次", done:"✅ 做到了！", notYet:"🔁 还没有",
    trap:"家长：这是陷阱！没有说“西蒙说”，就不能动。", two:"家长：两个动作，按顺序做。", first:"家长：第一次和孩子一起做。",
    caught:"真棒！西蒙没有说！", fooled:"哎呀！西蒙没有说！", tryAgain:"差一点点！", referee:"和大人一起玩：大人当裁判！"}
};
// the island is declared in ACTS (donnees.js): its title follows the language being learnt
Object.assign(ACTS.find(a => a.id === "simon"), {multi:true,
  title:{en:"Simon says", de:"Simon sagt", lb:"De Simon seet", zh:"西蒙说"},
  sub:{en:"Move your body!", de:"Beweg dich!", lb:"Beweeg dech!", zh:"动一动身体！"}});
Object.defineProperty(ACTS.find(a => a.id === "simon"), "badge", {configurable:true, enumerable:true,
  get: () => ({en:"with a grown-up", de:"mit Erwachsenen", lb:"mat Erwuessenen", zh:"和大人一起"})[langOf()] || "with a grown-up"});

GAMES.simon = function () {
  const lvl = levelOf("simon"), total = 8, res = [];
  const trapRate = [0, 0.3, 0.3, 0.4][lvl - 1];
  // one move, counted at level 4 when it can be ("Jump three times")
  const one = () => {
    const m = pick(SIMON_MOVES, 1)[0];
    if (lvl === 4 && m.n) {
      const k = 2 + rnd(3);
      return {e: m.e.repeat(Math.min(k, 3)), key: `${m.en} x${k}`, txt: L => m.n[L](SIMON_TIMES[L](k))};
    }
    return {e: m.e, key: m.en, txt: L => m[L]};
  };
  // two moves joined: "Touch your nose and clap your hands!"; German and Luxembourgish imperatives lose their capital
  const lower = s => s.charAt(0).toLowerCase() + s.slice(1);
  const joinWord = (L, next) => L !== "lb" ? SIMON_TXT[L].and : /^[aeiouäéëdtzhn]/i.test(next) ? "an" : "a"; // lb n-rule: "a klapp", "an dréin"
  const orders = [...Array(total)].map(() => {
    let o = one();
    if (lvl >= 3) {
      let b = one(); while (b.key === o.key) b = one();
      const a = o;
      o = {e: a.e + b.e, key: `${a.key} + ${b.key}`, txt: L => L === "zh" ? a.txt(L) + SIMON_TXT.zh.and + b.txt(L)
        : `${a.txt(L)} ${joinWord(L, b.txt(L))} ${lower(b.txt(L))}`};
    }
    return {...o, trick: Math.random() < trapRate};
  });
  // the help button (中文 / English / 🐢) reads the move in each language
  const bridgeWord = o => ({en: o.txt("en") + "!", de: o.txt("de") + "!", lb: o.txt("lb") + "!", zh: o.txt("zh") + "！"});
  // a flag tapped during the game starts it again in the new language (drapeaux.js)
  const lang = SIMON_TXT[langOf()] ? langOf() : "en", X = SIMON_TXT[lang];
  startSession("simon", null, total);
  if (!S.present) toast(X.referee);
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const o = orders[i], line = (o.trick ? "" : X.says) + o.txt(lang) + X.bang;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    const order = el("div", "order bob", o.e);
    // the order is always written, for every child and level (Kezhan: every screen is a chance to read)
    const p = el("p", "prompt", ""); p.textContent = "🗣️ " + line;
    p.append(el("small", "", o.trick ? X.trap : lvl >= 3 ? X.two : X.first));
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => line, X.again, lang), bridgeBtn(bridgeWord(o)));
    const judge = el("div", "judge");
    const ok = el("button", "chunky", X.done); ok.style.background = "#C9F2DF";
    const ko = el("button", "chunky", X.notYet); ko.style.background = "#FFD6CF";
    const next = async good => {
      if (!alive(gen) || judge.dataset.done) return; judge.dataset.done = "1";
      if (good) { sfx.ok(); addStar(); if (!fx.calm()) order.animate([{transform: "none"}, {transform: "rotate(-12deg) scale(1.2)"}, {transform: "rotate(12deg) scale(1.2)"}, {transform: "none"}], {duration: 600}); }
      else sfx.ko();
      logRound(o.key, good, 1, {lvl, trick: o.trick, lang}); res.push(good ? 1 : 0); i++;
      await say(o.trick ? (good ? X.caught : X.fooled) : good ? praiseT() : X.tryAgain, lang);
      round();
    };
    ok.onclick = () => next(true);
    ko.onclick = () => next(false);
    judge.append(ok, ko); body.append(order, p, row, judge);
    say(line, lang);
  };
  round();
};
