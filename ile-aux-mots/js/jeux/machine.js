/* L'Île aux Mots : jeu « La Machine à mots » (atelier de conception, 6,0/10) : les sons de la langue apprise, jamais du français.
   Each language has its own sound game (contents: js/contenus/machine.js):
   en, de, lb · 1: which picture rhymes (3 pictures) · 2: the same with 4 · 3: picture + "_at", choose the first letter · 4: the right spelling among traps, no voice
   zh · 1-2: 押韵 (which one has the same final) · 3: 韵母 (m + ?) · 4: 声调 (māo, máo, mǎo or mào?) */
addStyle(`
.mc-reel{display:inline-block; animation:mc-reel .55s cubic-bezier(.3,1.5,.5,1)}
@keyframes mc-reel{from{transform:translateY(-70px) scale(.6); opacity:0} to{transform:none; opacity:1}}
.mc-name{display:block; font-family:var(--display); font-size:18px; line-height:1.1}
`);
const MACHINE_TONES = {a:"āáǎà", e:"ēéěè", i:"īíǐì", o:"ōóǒò", u:"ūúǔù", "ü":"ǖǘǚǜ"};
// the tone mark goes on a, else e, else the o of "ou", else the last vowel (niú, guǐ, huǒ)
function machinePinyin(fin, t){
  const at = fin.includes("a") ? fin.indexOf("a") : fin.includes("e") ? fin.indexOf("e") : fin.includes("ou") ? fin.indexOf("o")
    : Math.max(...[..."iouü"].map(v => fin.lastIndexOf(v)));
  return fin.slice(0, at) + MACHINE_TONES[fin[at]][t - 1] + fin.slice(at + 1);
}
// every language as {face, name, speak}: the picture, the written word, what the voice says
function machineItems(lang){
  const D = MACHINE_DATA[lang];
  if (lang === "lb") {
    const all = Object.values(THEMES).flatMap(t => t.words);
    // a colour is a swatch; the big picture of the round needs its own size (an inline swatch has no width)
    const lex = en => { const w = all.find(x => x.en === en); return {face: wordFace(w), name: w.lb.replace(/^(d'|den |de )/, ""), speak: w.lb,
      vis: w.e.startsWith("#") ? `<span class="swatch" style="display:inline-block; width:.9em; vertical-align:middle; background:${w.e}"></span>` : w.e}; };
    return {rhymes: D.rhymes.map(p => p.map(lex)), words: D.words.map(lex), spell: D.spell};
  }
  if (lang === "zh") {
    const syl = D.syl.map(([e, c, ini, fin, t]) => ({face: e, name: `${c} ${ini}${machinePinyin(fin, t)}`, speak: c, c, ini, fin, t}));
    return {syl, rhymes: D.rhymes.map(p => [...p].map(c => syl.find(s => s.c === c)))};
  }
  const item = ([e, w]) => ({face: e, name: w, speak: w});
  return {rhymes: D.rhymes.map(r => [item(r.slice(0, 2)), item(r.slice(2))]), words: D.words.map(item), spell: D.spell};
}
registerGame({id:"machine", em:"🎰", name:"Machine à mots", desc:"Rimes et sons", multi:true,
  title:{en:"Word Machine", de:"Wörtermaschine", lb:"Wierdermaschinn", zh:"词语机器"},
  sub:{en:"Rhymes and sounds", de:"Reime und Laute", lb:"Reimen a Lauter", zh:"押韵、韵母和声调"}}, function () {
  const lvl = levelOf("machine"), reel = fx.calm() ? "" : "mc-reel";
  const big = t => `<span style="font-family:var(--display)">${t}</span>`;
  const pic = f => `<span class="${reel}">${f}</span>`;
  // a picture with its written word under it: the children read as they play
  const card = it => `${it.face}<span class="mc-name">${it.name}</span>`;
  // a flag tapped during the game starts it again in the new language (drapeaux.js)
  const lang = MACHINE_DATA[langOf()] ? langOf() : "en", U = MACHINE_DATA.ui[lang], I = machineItems(lang);
  // one shuffled deck per list: the rounds of a game never repeat a word
  const decks = {}, at = (key, list, j) => (decks[key] = decks[key] || shuffle(list))[j % list.length];
  const rounds = [...Array(8)].map((_, k) => {
    if (lvl <= 2) { // rhyme by ear: "Which one rhymes with cat?" / 哪个和“猫”押韵？
      const n = lvl === 1 ? 3 : 4, r = at("rhymes", I.rhymes, k), [a, b] = rnd(2) ? r : [r[1], r[0]];
      // one word from each other pair: they never rhyme with the target
      const others = pick(I.rhymes.filter(x => x !== r), n - 1).map(x => x[rnd(2)]);
      return {lang, say: U.rhyme(a.speak), show: "👂 " + U.rhymeShow(a.name), visual: pic(a.vis || a.face), word: `${a.name}/${b.name}`, praise: U.yes(a.speak, b.speak),
        choices: [{html: card(b), ok: true}, ...others.map(o => ({html: card(o), sayWrong: U.nope(a.speak, o.speak)}))]};
    }
    if (lang === "zh") {
      const s = at("syl", I.syl, k);
      if (lvl === 3) { // 韵母: 猫 = m + āo
        const fins = [...new Set(I.syl.map(x => x.fin).filter(f => f !== s.fin))];
        return {lang, say: U.fin(s.c), show: `🧩 <b>${s.c}</b> ${big(s.ini + " + ?")}<small>${U.finShow}</small>`, visual: pic(s.face), word: s.name, praise: U.yes1(s.c),
          choices: [s.fin, ...pick(fins, 3)].map(f => ({html: big(machinePinyin(f, s.t)), small: true, ok: f === s.fin, sayWrong: s.c}))};
      }
      // 声调: māo, máo, mǎo, mào
      return {lang, say: U.tone(s.c), show: `🎵 <b>${s.c}</b> ${big(s.ini + s.fin)}<small>${U.toneShow}</small>`, visual: pic(s.face), word: s.name, praise: U.toneYes(s.c, s.t),
        choices: [1, 2, 3, 4].map(t => ({html: big(s.ini + machinePinyin(s.fin, t)), small: true, ok: t === s.t, sayWrong: s.c}))};
    }
    if (lvl === 3) { // onset: 🎩 + "_at" → h; never a letter that makes another word of the game (Hund, Mund)
      const w = at("words", I.words, k), first = w.name[0], rest = w.name.slice(1), up = first === first.toUpperCase();
      const taken = I.words.filter(x => x.name.slice(1) === rest).map(x => x.name[0].toLowerCase());
      const letters = (up ? "BDFGHKLMNPRSTWZ" : "bcdfghjlmnprstvw").split("").filter(l => !taken.includes(l.toLowerCase()));
      return {lang, say: U.make(w.speak), show: `🧩 ${U.makeShow} ${big("_" + rest)}`, visual: pic(w.vis || w.face), word: w.name, praise: U.yes1(w.speak),
        choices: [first, ...pick(letters, 3)].map(l => ({html: big(l), ok: l === first, sayWrong: U.makeNo(l + rest, w.speak)}))};
    }
    // read: the right spelling among traps, no voice (English: vowel traps, hat, hot, hit, hut)
    const sp = lang === "en" ? (([e, w]) => [e, w, ...pick("aeiou".split("").filter(v => v !== w[1]).map(v => w[0] + v + w[2]), 3)])(at("cvc", MACHINE_DATA.en.words, k)) : at("spell", I.spell, k);
    const [e, right, ...traps] = sp;
    return {lang, say: "", show: `📖 ${U.which}`, visual: pic(e), word: right, praise: U.yes1(lang === "lb" ? machineLbSpeak(right) : right),
      choices: [right, ...traps].map(x => ({html: big(x), small: true, ok: x === right, sayWrong: U.retry}))};
  });
  runQuiz("machine", null, rounds);
});
// Luxembourgish praise at the reading level: the recording of the word when the lexicon has it
function machineLbSpeak(name){
  const w = Object.values(THEMES).flatMap(t => t.words).find(x => x.lb && x.lb.replace(/^(d'|den |de )/, "") === name);
  return w ? w.lb : name;
}
