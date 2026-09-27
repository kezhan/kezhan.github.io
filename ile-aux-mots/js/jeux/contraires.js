/* L'Île aux Mots : jeu « contraires ». */
registerGame({id:"contraires", em:"↔️", name:"Contraires", desc:"Grand, petit, chaud, froid"}, function () {
  const lvl = levelOf("contraires");
  const PAIRS = [["🐘","big","🐭","small"],["🔥","hot","🧊","cold"],["😀","happy","😢","sad"],["🐇","fast","🐢","slow"],
    ["☀️","day","🌙","night"],["⬆️","up","⬇️","down"],["🔓","open","🔒","closed"],["📢","loud","🤫","quiet"],["🦒","tall","🐁","short"]];
  const cards = PAIRS.flatMap(p => [[p[0], p[1], p[3]], [p[2], p[3], p[1]]]); // [emoji, word, opposite]
  const n = lvl === 1 ? 3 : 4;
  const rounds = pick(cards, 8).map(c => {
    const opp = cards.find(x => x[1] === c[2]);
    const others = pick(cards.filter(x => x !== c && x !== opp), n - 1);
    if (lvl <= 2) // find the word
      return {say: `Find: ${c[1]}!`, word: c[1], choices: [c, ...others].map(x => ({html: x[0], ok: x === c, label: x[1], sayWrong: `That's ${x[1]}. Find: ${c[1]}!`}))};
    return {say: `What is the opposite of ${c[1]}?`, word: `${c[1]}/${opp[1]}`, visual: c[0],
      show: lvl >= 4 ? `The opposite of <b>${c[1]}</b>?` : "Le contraire ?", praise: `Yes! ${c[1]} and ${opp[1]}!`,
      choices: [opp, ...others.filter(x => x !== c)].slice(0, n).map(x => ({html: x[0], ok: x === opp, label: x[1]}))};
  });
  runQuiz("contraires", null, rounds);
});
