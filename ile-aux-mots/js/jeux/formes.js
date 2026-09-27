/* L'Île aux Mots : jeu « formes ». */
registerGame({id:"formes", em:"🔺", name:"Formes", desc:"Cercle, carré, étoile…"}, function () {
  const lvl = levelOf("formes");
  const SHAPES = [["🔴","circle"],["🟦","square"],["🔺","triangle"],["⭐","star"],["❤️","heart"],["🔷","diamond"]];
  // level 3+: colour and shape together
  const COMBOS = [["🔴","red circle"],["🔵","blue circle"],["🟢","green circle"],["🟥","red square"],["🟦","blue square"],["🟩","green square"],["🟨","yellow square"],["🟡","yellow circle"]];
  const n = lvl === 1 ? 3 : lvl === 2 ? 4 : 6;
  const pool = lvl >= 3 ? COMBOS : SHAPES;
  const rounds = pick(pool, Math.min(8, pool.length)).map(t => ({
    say: `Find the ${t[1]}!`, word: t[1],
    show: lvl >= 4 ? `Find the <b>${t[1]}</b>` : undefined,
    choices: [t, ...pick(pool.filter(x => x !== t), n - 1)].map(x => ({html: x[0], ok: x === t, label: x[1], sayWrong: `That's the ${x[1]}. Find the ${t[1]}!`}))
  }));
  runQuiz("formes", null, rounds);
});
