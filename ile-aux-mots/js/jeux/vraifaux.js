/* L'Île aux Mots : jeu « vrai ou faux ». */
registerGame({id:"vraifaux", em:"✅", name:"Vrai ou faux ?", desc:"Écoute ou lis la phrase"}, function () {
  const lvl = levelOf("vraifaux");
  const FACTS = [["🍌","banana","yellow"],["🍎","apple","red"],["🍓","strawberry","red"],["🥕","carrot","orange"],["🐸","frog","green"],
    ["🐷","pig","pink"],["🥛","milk","white"],["🍇","grapes","purple"],["🌳","tree","green"],["☀️","sun","yellow"],["🐻","bear","brown"],["🍊","orange","orange"]];
  const COLORS = ["red","blue","green","yellow","orange","pink","purple","white","brown","black"];
  const ANIMALS = [["🐱","cats"],["🐶","dogs"],["🐟","fish"],["🐦","birds"],["🐰","rabbits"]];
  const rounds = [...Array(8)].map((_, k) => {
    const truth = Math.random() < 0.5;
    let sentence, visual;
    if (lvl >= 3 && k % 2) { // counting sentences
      const a = ANIMALS[rnd(ANIMALS.length)], n = 2 + rnd(lvl >= 4 ? 8 : 4), said = truth ? n : (n > 2 && rnd(2) ? n - 1 : n + 1); // never "one cats"
      sentence = `There are ${numberWords(said)} ${a[1]}.`; visual = a[0].repeat(n);
    } else {
      const f = FACTS[rnd(FACTS.length)], col = truth ? f[2] : pick(COLORS.filter(c => c !== f[2]), 1)[0];
      sentence = `The ${f[1]} ${f[1] === "grapes" ? "are" : "is"} ${col}.`; visual = f[0];
    }
    // the little one cannot read: she always hears it; the big one reads alone from level 3, with a listen button
    const readAlone = S.kid === "p7" && lvl >= 3;
    const shown = S.kid === "p7" ? `<b>${sentence}</b>` : "Vrai ou faux ?<small>Écoute la phrase</small>";
    return {say: readAlone ? "" : sentence + " True or false?", show: shown, visual, word: sentence, listen: readAlone ? sentence : "",
      praise: truth ? "Yes, that's true!" : "Yes, that's false!",
      choices: [{html: "✅<span class='w'>True</span>", ok: truth, sayWrong: `No! ${sentence} That's ${truth ? "true" : "false"}.`},
                {html: "❌<span class='w'>False</span>", ok: !truth, sayWrong: `No! ${sentence} That's ${truth ? "true" : "false"}.`}]};
  });
  runQuiz("vraifaux", null, rounds);
});
