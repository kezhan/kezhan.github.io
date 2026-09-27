/* L'Île aux Mots : jeu « La Machine à mots » (atelier de conception, 6,0/10), sons et rimes de l'anglais.
   1: which picture rhymes (3 pictures, heard) · 2: the same with 4 pictures · 3: picture + "_at", choose the first letter
   4: read: the right spelling among vowel traps (hat, hot, hit, hut), no voice. English phonics: the game stays in English. */
registerGame({id:"machine", em:"🎰", name:"Word Machine", desc:"Rhymes and sounds"}, function () {
  const lvl = levelOf("machine");
  const RHYMES = [["🐱","cat","🎩","hat"],["🐝","bee","🌳","tree"],["⭐","star","🚗","car"],["🐭","mouse","🏠","house"],["🐍","snake","🍰","cake"],
    ["🐐","goat","⛵","boat"],["🐸","frog","🪵","log"],["🌙","moon","🥄","spoon"],["🐻","bear","🍐","pear"],["🧦","sock","⏰","clock"],
    ["👑","king","💍","ring"],["🌧️","rain","🚂","train"],["🐟","fish","🍽️","dish"],["🦊","fox","📦","box"]];
  const CVC = [["🐱","cat"],["🎩","hat"],["🦇","bat"],["🐀","rat"],["🐷","pig"],["🐶","dog"],["🪵","log"],["☀️","sun"],["🚌","bus"],["🐔","hen"],
    ["🖊️","pen"],["📦","box"],["🦊","fox"],["🛏️","bed"],["🚐","van"],["🐛","bug"],["🕸️","web"],["🥅","net"],["🛖","hut"],["🗺️","map"],["🧢","cap"],["👜","bag"],["🦵","leg"]];
  const big = t => `<span style="font-family:var(--display)">${t}</span>`;
  let rounds;
  if (lvl <= 2) { // rhyme by ear: "Which one rhymes with cat?"
    const n = lvl === 1 ? 3 : 4;
    rounds = pick(RHYMES, 8).map(r => {
      const [ae, a, be, b] = Math.random() < 0.5 ? r : [r[2], r[3], r[0], r[1]];
      // one word from each other pair: they never rhyme with the target
      const others = pick(RHYMES.filter(x => x !== r), n - 1).map(x => rnd(2) ? [x[0], x[1]] : [x[2], x[3]]);
      return {say: `Which one rhymes with ${a}?`, show: `Which one rhymes with <b>${a}</b>?`, visual: ae, word: `${a}/${b}`,
        praise: `Yes! ${a}, ${b}!`, choices: [{html: be, ok: true, label: b}, ...others.map(o => ({html: o[0], label: o[1], sayWrong: `${o[1]}? ${a}, ${o[1]}... no! Which one rhymes with ${a}?`}))]};
    });
  } else if (lvl === 3) { // onset: 🎩 + "_at" → h
    const letters = "bcdfghjlmnprstvw".split("");
    rounds = pick(CVC, 8).map(([e, w]) => ({
      say: `Make the word ${w}!`, show: `Make the word! ${big("_" + w.slice(1))}`, visual: e, word: w, praise: `Yes! ${w}!`,
      choices: [w[0], ...pick(letters.filter(l => l !== w[0]), 3)].map(l => ({html: big(l), ok: l === w[0], sayWrong: `${l}${w.slice(1)}? No! Make ${w}!`}))
    }));
  } else { // read: the right spelling among vowel traps, no voice
    rounds = pick(CVC, 8).map(([e, w]) => {
      const traps = "aeiou".split("").filter(v => v !== w[1]).map(v => w[0] + v + w[2]);
      return {say: "", show: "Which word?", visual: e, word: w, praise: `Yes! ${w}!`,
        choices: [w, ...pick(traps, 3)].map(x => ({html: big(x), small: true, ok: x === w}))};
    });
  }
  runQuiz("machine", null, rounds);
});
