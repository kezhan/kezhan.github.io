/* L'Île aux Mots : jeu « heure ». */
registerGame({id:"heure", em:"🕒", name:"Quelle heure ?", desc:"Lire l'heure en anglais", ages:["p7"]}, function () {
  const lvl = levelOf("heure");
  // clock emoji: U+1F550 + h-1 for o'clock, U+1F55C + h-1 for half past
  const clock = (h, half) => String.fromCodePoint((half ? 0x1F55C : 0x1F550) + h - 1);
  const words = (h, half) => half ? `half past ${numberWords(h)}` : `${numberWords(h)} o'clock`;
  const times = [];
  for (let h = 1; h <= 12; h++) { times.push([h, false]); if (lvl >= 3) times.push([h, true]); }
  const n = lvl === 1 ? 3 : 4;
  const rounds = pick(times, 8).map(t => {
    const others = pick(times.filter(x => x !== t), n - 1), all = [t, ...others];
    if (lvl < 4) return {say: `It's ${words(...t)}. Find the clock!`, word: words(...t),
      choices: all.map(x => ({html: clock(...x), ok: x === t, label: words(...x)}))};
    // level 4: read the clock, choose the written time
    return {say: "What time is it?", visual: clock(...t), show: "What time is it?", word: words(...t), praise: `Yes! It's ${words(...t)}!`,
      choices: all.map(x => ({html: words(...x), small: true, ok: x === t}))};
  });
  runQuiz("heure", null, rounds);
});
