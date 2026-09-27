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
    const heard = lvl <= 2; // levels 3-4: read it yourself
    return {say: heard ? sentence : "", show: heard ? "Vrai ou faux ?" : `<b>${sentence}</b>`, visual, word: sentence,
      praise: truth ? "Yes, that's true!" : "Yes, that's false!",
      choices: [{html: "✅ True", small: true, ok: truth}, {html: "❌ False", small: true, ok: !truth}]};
  });
  runQuiz("vraifaux", null, rounds);
});
