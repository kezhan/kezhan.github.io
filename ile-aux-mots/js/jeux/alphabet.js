/* L'Île aux Mots : jeu « ABC ». */
registerGame({id:"alphabet", em:"🔤", name:"ABC", desc:"Les lettres et leurs sons"}, function () {
  const lvl = levelOf("alphabet");
  const letters = "ABCDEFGHIJKLMNOPRSTW".split("");
  if (lvl <= 2) { // hear a letter, touch it
    const n = lvl === 1 ? 3 : 4;
    const rounds = pick(letters, 8).map(L => ({say: `Find the letter ${L}!`, word: L,
      choices: [L, ...pick(letters.filter(x => x !== L), n - 1)].map(x => ({html: `<span style="font-family:var(--display)">${x}</span>`, ok: x === L, sayWrong: `That's ${x}. Find ${L}!`}))}));
    return runQuiz("alphabet", null, rounds);
  }
  // levels 3-4: something that starts with the letter
  const words = Object.values(THEMES).filter(t => t !== THEMES.colors).flatMap(t => t.words);
  const byFirst = {}; words.forEach(w => { const f = w.en[0].toUpperCase(); (byFirst[f] = byFirst[f] || []).push(w); });
  const usable = Object.keys(byFirst);
  const rounds = pick(usable, 8).map(L => {
    const w = pick(byFirst[L], 1)[0];
    const others = pick(words.filter(x => x.en[0].toUpperCase() !== L), lvl === 3 ? 2 : 3);
    return {say: `Find something that starts with ${L}!`, show: lvl >= 4 ? `Starts with <b>${L}</b>` : undefined, word: `${L}:${w.en}`,
      praise: `Yes! ${w.en} starts with ${L}!`,
      choices: [w, ...others].map(x => ({html: x.e, ok: x === w, label: x.en, sayWrong: `${x.en} starts with ${x.en[0].toUpperCase()}.`}))};
  });
  runQuiz("alphabet", null, rounds);
});
