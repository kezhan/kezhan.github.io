/* L'Île aux Mots : jeu « Vrai ou faux ? », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   Funny sentences to judge: colours (Die Banane ist blau.), animal noises (D'Kou mécht wau wau.), counting (Et sinn dräi Kazen. 有三只猫。)
   1: colours · 2: colours and noises · 3: counting too, up to 5, the big one reads alone · 4: counting up to 9
   Luxembourgish plurals and noises from lod.lu (Hënn, Vullen, Kanéngercher; the n-rule: "Et si véier Hënn", "D'Drauwe si mof"). */
addStyle(`
.vf-giggle{display:inline-block; animation:vf-giggle .9s ease-in-out 2}
@keyframes vf-giggle{0%,100%{transform:none} 25%{transform:rotate(-10deg) scale(1.12)} 75%{transform:rotate(10deg) scale(1.12)}}
`);
const VF_COLORS = {
  red:{en:"red", de:"rot", lb:"rout", zh:"红色"}, blue:{en:"blue", de:"blau", lb:"blo", zh:"蓝色"}, green:{en:"green", de:"grün", lb:"gréng", zh:"绿色"},
  yellow:{en:"yellow", de:"gelb", lb:"giel", zh:"黄色"}, orange:{en:"orange", de:"orange", lb:"orange", zh:"橙色"}, pink:{en:"pink", de:"rosa", lb:"rosa", zh:"粉色"},
  purple:{en:"purple", de:"lila", lb:"mof", zh:"紫色"}, white:{en:"white", de:"weiß", lb:"wäiss", zh:"白色"}, brown:{en:"brown", de:"braun", lb:"brong", zh:"棕色"},
  black:{en:"black", de:"schwarz", lb:"schwaarz", zh:"黑色"}
};
// c: the true colour; no: colours that are plainly silly for it (never a real variety: no green apple, no black bear, no "blaue Trauben")
const VF_THINGS = [
  {e:"🍌", c:"yellow", no:["blue","pink","purple"], en:"The banana", de:"Die Banane", lb:"D'Banann", zh:"香蕉"},
  {e:"🍎", c:"red", no:["blue","purple","white"], en:"The apple", de:"Der Apfel", lb:"Den Apel", zh:"苹果"},
  {e:"🍓", c:"red", no:["blue","black","purple"], en:"The strawberry", de:"Die Erdbeere", lb:"D'Äerdbier", zh:"草莓"},
  {e:"🥕", c:"orange", no:["blue","pink"], en:"The carrot", de:"Die Karotte", lb:"D'Muert", zh:"胡萝卜"},
  {e:"🐸", c:"green", no:["pink","purple","white"], en:"The frog", de:"Der Frosch", lb:"De Fräsch", zh:"青蛙"},
  {e:"🐷", c:"pink", no:["blue","green","purple"], en:"The pig", de:"Das Schwein", lb:"D'Schwäin", zh:"小猪"},
  {e:"🥛", c:"white", no:["blue","green","black","red"], en:"The milk", de:"Die Milch", lb:"D'Mëllech", zh:"牛奶"},
  {e:"🍇", c:"purple", no:["orange","pink"], pl:true, en:"The grapes", de:"Die Trauben", lb:"D'Drauwe", zh:"葡萄"},
  {e:"🌳", c:"green", no:["blue","purple"], en:"The tree", de:"Der Baum", lb:"De Bam", zh:"树"},
  {e:"☀️", c:"yellow", no:["blue","green","purple","black"], en:"The sun", de:"Die Sonne", lb:"D'Sonn", zh:"太阳"},
  {e:"🐻", c:"brown", no:["blue","green","pink","purple"], en:"The bear", de:"Der Bär", lb:"De Bier", zh:"熊"},
  {e:"🍊", c:"orange", no:["blue","purple","pink"], en:"The orange", de:"Die Orange", lb:"D'Orange", zh:"橙子"}
];
// noises checked for js/contenus/cris.js (lod.lu verbs: muen → mu, miauen → miau…)
const VF_NOISES = [
  {e:"🐶", en:["The dog","woof woof"], de:["Der Hund","wau wau"], lb:["Den Hond","wau wau"], zh:["小狗","汪汪"]},
  {e:"🐱", en:["The cat","meow"], de:["Die Katze","miau"], lb:["D'Kaz","miau"], zh:["小猫","喵喵"]},
  {e:"🐮", en:["The cow","moo"], de:["Die Kuh","muh"], lb:["D'Kou","mu"], zh:["奶牛","哞哞"]},
  {e:"🐷", en:["The pig","oink oink"], de:["Das Schwein","grunz grunz"], lb:["D'Schwäin","grunz grunz"], zh:["小猪","哼哼"]},
  {e:"🦆", en:["The duck","quack quack"], de:["Die Ente","quak quak"], lb:["D'Int","quak quak"], zh:["小鸭子","嘎嘎"]},
  {e:"🐑", en:["The sheep","baa"], de:["Das Schaf","mäh"], lb:["D'Schof","mä"], zh:["小羊","咩咩"]}
];
// counting: German plural, Luxembourgish gender (zwou before a feminine noun) and plural, Chinese measure word
const VF_COUNT = [
  {e:"🐱", en:"cats", de:"Katzen", lb:["f","Kazen"], zh:["只","猫"]},
  {e:"🐶", en:"dogs", de:"Hunde", lb:["m","Hënn"], zh:["只","狗"]},
  {e:"🐟", en:"fish", de:"Fische", lb:["m","Fësch"], zh:["条","鱼"]},
  {e:"🐦", en:"birds", de:"Vögel", lb:["m","Vullen"], zh:["只","鸟"]},
  {e:"🐰", en:"rabbits", de:"Kaninchen", lb:["f","Kanéngercher"], zh:["只","兔子"]}
];
// Luxembourgish n-rule: the final n stays before a vowel or d, t, z, n, h (sinn → si, siwen → siwe)
const vfN = (full, short, next) => /^[aeiouäéëdtznh]/i.test(next) ? full : short;
const VF_SAY = {
  en:{color:(t, c) => `${t.en} ${t.pl ? "are" : "is"} ${c}.`, noise:(a, s) => `${a} says ${s}.`, count:(it, n) => `There are ${numberIn(n, "en")} ${it.en}.`,
    q:"True or false?", t:"True", f:"False", isT:"Yes, that's true!", isF:"Yes, that's false!", no:(s, ok) => `No! ${s} That's ${ok ? "true" : "false"}.`},
  de:{color:(t, c) => `${t.de} ${t.pl ? "sind" : "ist"} ${c}.`, noise:(a, s) => `${a} macht ${s}.`, count:(it, n) => `Da sind ${numberIn(n, "de")} ${it.de}.`,
    q:"Richtig oder falsch?", t:"Richtig", f:"Falsch", isT:"Ja, das stimmt!", isF:"Ja, das ist falsch!", no:(s, ok) => `Nein! ${s} Das ${ok ? "stimmt" : "ist falsch"}.`},
  lb:{color:(t, c) => t.pl ? `${t.lb} ${vfN("sinn", "si", c)} ${c}.` : `${t.lb} ass ${c}.`, noise:(a, s) => `${a} mécht ${s}.`,
    count:(it, n) => { const [g, pl] = it.lb, num = n === 2 && g === "f" ? "zwou" : n === 7 ? vfN("siwen", "siwe", pl) : numberIn(n, "lb"); return `Et ${vfN("sinn", "si", num)} ${num} ${pl}.`; },
    q:"Richteg oder falsch?", t:"Richteg", f:"Falsch", isT:"Jo, dat stëmmt!", isF:"Jo, dat ass falsch!", no:(s, ok) => `Neen! ${s} Dat ${ok ? "stëmmt" : "ass falsch"}.`},
  // 两 before a measure word: 有两只猫
  zh:{color:(t, c) => `${t.zh}是${c}的。`, noise:(a, s) => `${a}${s}叫。`, count:(it, n) => `有${n === 2 ? "两" : numberIn(n, "zh")}${it.zh[0]}${it.zh[1]}。`,
    q:"对还是错？", t:"对", f:"错", isT:"没错，这是对的！", isF:"没错，这是错的！", no:(s, ok) => `不对！${s}这是${ok ? "对" : "错"}的。`}
};
registerGame({id:"vraifaux", em:"✅", name:"Vrai ou faux ?", desc:"Écoute ou lis la phrase", multi:true,
  title:{en:"True or false?", de:"Richtig oder falsch?", lb:"Richteg oder falsch?", zh:"对还是错？"},
  sub:{en:"Listen or read the sentence", de:"Hör zu oder lies den Satz", lb:"Lauschter oder lies de Saz", zh:"听一听或读一读句子"}}, function () {
  const lvl = levelOf("vraifaux"), giggle = fx.calm() ? "" : "vf-giggle";
  const things = shuffle(VF_THINGS), beasts = shuffle(VF_NOISES), counts = shuffle(VF_COUNT);
  // a flag tapped during the game starts it again in the new language (drapeaux.js)
  const lang = VF_SAY[langOf()] ? langOf() : "en", X = VF_SAY[lang];
  const rounds = [...Array(8)].map((_, k) => {
    const truth = Math.random() < 0.5;
    const kind = lvl >= 3 && k % 2 ? "count" : lvl >= 2 && rnd(2) ? "noise" : "color";
    let sentence, word, visual;
    if (kind === "count") {
      const it = counts[k % counts.length], n = 2 + rnd(lvl >= 4 ? 8 : 4), said = truth ? n : (n > 2 && rnd(2) ? n - 1 : n + 1); // never "one cats"
      sentence = X.count(it, said); word = VF_SAY.en.count(it, said); visual = it.e.repeat(n);
    } else if (kind === "noise") {
      const a = beasts[k % beasts.length], [who, snd] = a[lang];
      // a wrong noise must sound different in this language (a duck and a frog both say "quak" in German)
      const other = pick(VF_NOISES.filter(b => b[lang][1] !== snd), 1)[0];
      const said = truth ? snd : other[lang][1];
      sentence = X.noise(who, said); word = VF_SAY.en.noise(a.en[0], truth ? a.en[1] : other.en[1]); visual = a.e + "💬";
    } else {
      const t = things[k % things.length], col = truth ? t.c : pick(t.no, 1)[0];
      sentence = X.color(t, VF_COLORS[col][lang]); word = VF_SAY.en.color(t, col); visual = t.e;
    }
    // the sentence is always written (Kezhan: every screen is a chance to read); the big one reads it without the voice from level 3
    const readAlone = S.kid === "p7" && lvl >= 3;
    const shown = `<b>${sentence}</b><small>🤔 ${X.q}</small>`;
    const wrong = X.no(sentence, truth);
    return {lang, say: readAlone ? "" : sentence + (lang === "zh" ? "" : " ") + X.q, show: shown, visual: `<span class="${giggle}">${visual}</span>`, word,
      listen: readAlone ? sentence : "", praise: truth ? X.isT : X.isF,
      choices: [{html: `✅<span class='w'>${X.t}</span>`, ok: truth, sayWrong: wrong}, {html: `❌<span class='w'>${X.f}</span>`, ok: !truth, sayWrong: wrong}]};
  });
  runQuiz("vraifaux", null, rounds);
});
