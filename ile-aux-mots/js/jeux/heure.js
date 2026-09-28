/* L'Île aux Mots : jeu « Quelle heure ? », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   Each language tells the time its own way: half past seven, halb acht (7:30!), hallwer aacht, 七点半.
   1: o'clock, 3 clocks · 2: o'clock, 4 clocks · 3: half hours too · 4: read the clock, choose the written time */
addStyle(`
.hr-clk{display:inline-block}
.choice.ok .hr-clk, .order .hr-clk{animation:hr-ring .6s ease-in-out 2}
@keyframes hr-ring{0%,100%{transform:rotate(0)} 20%{transform:rotate(-14deg) scale(1.1)} 40%{transform:rotate(12deg) scale(1.1)} 60%{transform:rotate(-8deg)} 80%{transform:rotate(5deg)}}
`);
// the hour after h, for "halb acht" / "hallwer aacht" (7:30) and "halb eins" (12:30)
const heureNext = h => h % 12 + 1;
// Luxembourgish "Auer" is feminine: eng Auer, zwou Auer, hallwer eng, hallwer zwou (luxembourgishwithanne.lu, lod.lu)
const heureLb = h => h === 1 ? "eng" : h === 2 ? "zwou" : numberIn(h, "lb");
const HEURE_TXT = {
  en:{time:(h, half) => half ? `half past ${numberIn(h, "en")}` : `${numberIn(h, "en")} o'clock`,
    find:t => `It's ${t}. Find the clock!`, what:"What time is it?", yes:t => `Yes! It's ${t}!`, no:t => `No, that's ${t}!`},
  de:{time:(h, half) => half ? `halb ${numberIn(heureNext(h), "de")}` : `${h === 1 ? "ein" : numberIn(h, "de")} Uhr`,
    find:t => `Es ist ${t}. Finde die Uhr!`, what:"Wie spät ist es?", yes:t => `Ja! Es ist ${t}!`, no:t => `Nein, das ist ${t}!`},
  lb:{time:(h, half) => half ? `hallwer ${heureLb(heureNext(h))}` : `${heureLb(h)} Auer`,
    find:t => `Et ass ${t}. Fann d'Auer!`, what:"Wéi vill Auer ass et?", yes:t => `Jo! Et ass ${t}!`, no:t => `Neen, dat ass ${t}!`},
  // 两点, never 二点
  zh:{time:(h, half) => `${h === 2 ? "两" : numberIn(h, "zh")}点${half ? "半" : ""}`,
    find:t => `现在是${t}。找一找，是哪个钟？`, what:"现在几点？", yes:t => `对了！现在是${t}！`, no:t => `不对，这是${t}！`}
};
registerGame({id:"heure", em:"🕒", name:"Quelle heure ?", desc:"Lire l'heure", ages:["p7"], multi:true,
  title:{en:"What time is it?", de:"Wie spät ist es?", lb:"Wéi vill Auer ass et?", zh:"现在几点？"},
  sub:{en:"Read the clock", de:"Lies die Uhr", lb:"Lies d'Auer", zh:"认一认钟表"}}, function () {
  const lvl = levelOf("heure"), ring = fx.calm() ? "" : "hr-clk";
  // clock emoji: U+1F550 + h-1 for o'clock, U+1F55C + h-1 for half past
  const clock = (h, half) => `<span class="${ring}">${String.fromCodePoint((half ? 0x1F55C : 0x1F550) + h - 1)}</span>`;
  const times = [];
  for (let h = 1; h <= 12; h++) { times.push([h, false]); if (lvl >= 3) times.push([h, true]); }
  const n = lvl === 1 ? 3 : 4;
  // a flag tapped during the game starts it again in the new language (drapeaux.js)
  const lang = HEURE_TXT[langOf()] ? langOf() : "en", X = HEURE_TXT[lang];
  const rounds = pick(times, 8).map(t => {
    const all = [t, ...pick(times.filter(x => x !== t), n - 1)];
    const said = X.time(...t), word = HEURE_TXT.en.time(...t);
    // the question is always written too (Kezhan: every screen is a chance to read), never the answer
    if (lvl < 4) return {lang, say: X.find(said), word, praise: X.yes(said), show: "🔎 " + X.find(`<b>${said}</b>`),
      choices: all.map(x => ({html: clock(...x), ok: x === t, label: X.time(...x), sayWrong: X.no(X.time(...x))}))};
    // level 4: read the clock, choose the written time
    return {lang, say: X.what, show: "⏰ " + X.what, visual: clock(...t), word, praise: X.yes(said),
      choices: all.map(x => ({html: X.time(...x), small: true, ok: x === t}))};
  });
  runQuiz("heure", null, rounds);
});
