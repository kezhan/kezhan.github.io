/* L'Île aux Mots : jeu « ABC », dans la langue apprise (anglais, allemand, luxembourgeois, chinois ; jamais de français).
   1-2 : entendre une lettre et la toucher. Allemand : noms des lettres (J « Jott », V « Vau »), Ä Ö Ü ß au niveau 2.
         Chinois : les initiales du pinyin (声母), dites comme à l'école (b 玻, p 坡, m 摸…).
         Luxembourgeois : aucune voix ne dit les lettres, donc un mot enregistré sur lod.lu montre sa lettre
         (« den Af » pour A ; au niveau 2 Ä, et É, Ë dans « de Léiw », « de Fësch »).
   3-4 : trouver ce qui commence par la lettre (le mot sans son article : « der Hund » commence par H ;
         en chinois, l'initiale de la première syllabe : 苹果 píng guǒ commence par p).
   La question est toujours écrite, lettre en gras, en plus de la voix (Kezhan : « c'est l'occasion de lire »).
   Chaque manche s'écrit au moment où elle commence : un drapeau touché en cours de partie vaut dès la manche suivante. */
addStyle(`.choice .lettre{font-family:var(--display); line-height:1}
.choice.ok .lettre.vive{animation:abcHop .7s cubic-bezier(.3,1.6,.5,1)}
@keyframes abcHop{0%{transform:none} 40%{transform:translateY(-22px) scale(1.3) rotate(-10deg)} 70%{transform:translateY(0) scale(.92,1.08)} 100%{transform:none}}
.prompt .abc-mot b{color:var(--coral); font-size:1.25em}`);

const ABC = {
  // letters heard at levels 1-2: the first list from level 1, the second added at level 2 (and always met there)
  letters: {en: ["ABCDEFGHIJKLMNOPRSTW", "UVYZ"], de: ["ABDEFGHIJKLMNOPRSTUVWZ", "ÄÖÜß"],
    zh: ["b p m f d t n l g k h", "j q x zh ch sh r z c s y w"]},
  // German letter names, for the German voice
  deName: {A:"A", B:"Be", C:"Ze", D:"De", E:"E", F:"Eff", G:"Ge", H:"Ha", I:"I", J:"Jott", K:"Ka", L:"Ell", M:"Emm", N:"Enn", O:"O",
    P:"Pe", Q:"Ku", R:"Err", S:"Ess", T:"Te", U:"U", V:"Vau", W:"We", X:"Ix", Y:"Ypsilon", Z:"Zett", Ä:"Ä", Ö:"Ö", Ü:"Ü", ß:"Eszett"},
  // pinyin initials as Chinese teachers say them (呼读音)
  zhSound: {b:"玻", p:"坡", m:"摸", f:"佛", d:"得", t:"特", n:"讷", l:"勒", g:"哥", k:"科", h:"喝", j:"基", q:"欺", x:"希",
    zh:"知", ch:"蚩", sh:"诗", r:"日", z:"资", c:"雌", s:"思", y:"衣", w:"乌"},
  // letters that look or sound alike: the traps of level 2 (kept only when the language has them)
  traps: {A:"Ä", B:"D P", D:"B O", P:"R B", R:"P K", M:"N W", N:"M H", W:"M V", E:"F É Ë", F:"E", C:"G O", G:"C", O:"Q C Ö",
    I:"J L", J:"I", U:"V Ü", V:"U", Ä:"A", Ö:"O", Ü:"U", ß:"B S", S:"ß Z", É:"E Ë", Ë:"E É",
    b:"d p", d:"b", p:"b q", q:"p j", m:"n", n:"m", h:"n", f:"t", t:"f", z:"zh", zh:"z", c:"ch", ch:"c", s:"sh", sh:"s", j:"q", x:"s"},
  // pinyin of the lexicon's Chinese words; words that start with a vowel (鳄鱼 è yú, 耳朵 ěr duo) have no initial and stay out
  py: Object.fromEntries(`猫 māo|狗 gǒu|奶牛 nǎi niú|猪 zhū|鸭子 yā zi|马 mǎ|狮子 shī zi|青蛙 qīng wā|鱼 yú|鸟 niǎo|兔子 tù zi|猴子 hóu zi
大象 dà xiàng|熊 xióng|老鼠 lǎo shǔ|羊 yáng|鸡 jī|老虎 lǎo hǔ|长颈鹿 cháng jǐng lù|斑马 bān mǎ|蛇 shé|蝴蝶 hú dié|企鹅 qǐ é|乌龟 wū guī
猫头鹰 māo tóu yīng|海豚 hǎi tún|鲸鱼 jīng yú|章鱼 zhāng yú|袋鼠 dài shǔ|苹果 píng guǒ|香蕉 xiāng jiāo|草莓 cǎo méi|葡萄 pú tao
橙子 chéng zi|梨 lí|樱桃 yīng táo|西瓜 xī guā|面包 miàn bāo|奶酪 nǎi lào|牛奶 niú nǎi|鸡蛋 jī dàn|蛋糕 dàn gāo|胡萝卜 hú luó bo
披萨 pī sà|西红柿 xī hóng shì|冰淇淋 bīng qí lín|土豆 tǔ dòu|柠檬 níng méng|桃子 táo zi|三明治 sān míng zhì|面条 miàn tiáo|米饭 mǐ fàn
黄瓜 huáng guā|蘑菇 mó gu|菠萝 bō luó|西兰花 xī lán huā|眼睛 yǎn jing|鼻子 bí zi|嘴巴 zuǐ ba|手 shǒu|脚 jiǎo|牙齿 yá chǐ|胳膊 gē bo
腿 tuǐ|头发 tóu fa|舌头 shé tou|手指 shǒu zhǐ|脸 liǎn|骨头 gǔ tou|心脏 xīn zàng|大脑 dà nǎo|球 qiú|汽车 qì chē|自行车 zì xíng chē
船 chuán|飞机 fēi jī|火车 huǒ chē|书 shū|房子 fáng zi|树 shù|花 huā|太阳 tài yáng|月亮 yuè liang|星星 xīng xing|公交车 gōng jiāo chē
云 yún|雨伞 yǔ sǎn|彩虹 cǎi hóng|钥匙 yào shi|眼镜 yǎn jìng|铅笔 qiān bǐ|剪刀 jiǎn dāo|钟 zhōng|火箭 huǒ jiàn|雪人 xuě rén
直升机 zhí shēng jī|书包 shū bāo|帽子 mào zi|裙子 qún zi|鞋子 xié zi|袜子 wà zi|外套 wài tào|围巾 wéi jīn|手套 shǒu tào
鸭舌帽 yā shé mào|靴子 xuē zi|裤子 kù zi|领带 lǐng dài|医生 yī shēng|老师 lǎo shī|农民 nóng mín|厨师 chú shī|警察 jǐng chá
消防员 xiāo fáng yuán|飞行员 fēi xíng yuán|歌手 gē shǒu|宇航员 yǔ háng yuán|画家 huà jiā|科学家 kē xué jiā|修理工 xiū lǐ gōng
跑步 pǎo bù|睡觉 shuì jiào|吃 chī|喝 hē|游泳 yóu yǒng|唱歌 chàng gē|跳舞 tiào wǔ|哭 kū|笑 xiào|写字 xiě zì|爬 pá`
    .split(/[|\n]/).map(s => s.trim().split(/ (.+)/).slice(0, 2))),
  // everything the child sees or hears: s is the letter (bold on screen, its name for the voice), w a word
  txt: {
    en: {find: s => `Find the letter ${s}!`, that: (x, s) => `That's ${x}. Find ${s}!`, starts: s => `Find something that starts with ${s}!`,
      yes: (w, s) => `Yes! ${w} starts with ${s}!`, is: (w, s) => `${w} starts with ${s}.`},
    de: {find: s => `Zeig mir das ${s}!`, that: (x, s) => `Das ist das ${x}. Zeig mir das ${s}!`, starts: s => `Such etwas, das mit ${s} anfängt!`,
      yes: (w, s) => `Ja! ${w} fängt mit ${s} an!`, is: (w, s) => `${w} fängt mit ${s} an.`},
    // lod.lu: FANNEN1, BUSCHTAF1, UFANKEN1 (« en neie Saz fänkt ni mat engem klenge Buschtaf un »)
    lb: {find: "Fann de Buschtaf!", starts: s => `Wat fänkt mat ${s} un?`},
    zh: {find: s => `声母${s}在哪里？`, that: (x, s) => `这是${x}。声母${s}在哪里？`, starts: s => `哪个是${s}开头的？`,
      yes: (w, s) => `对了！${w}是${s}开头的！`, is: (w, s) => `${w}是${s}开头的。`}
  }
};

registerGame({id:"alphabet", em:"🔤", name:"ABC", desc:"Les lettres et leurs sons", multi:true,
  title:{en:"ABC", de:"ABC", lb:"ABC", zh:"拼音"},
  sub:{en:"Letters and their sounds", de:"Buchstaben und ihre Laute", lb:"Buschtawen a Wierder", zh:"认识声母 b p m f"}}, function () {
  const lvl = levelOf("alphabet"), total = 8;
  const strip = s => s.replace(/^(der |die |das |den |dem |de |d')/, "");
  const words = (() => { const seen = new Set(); return Object.entries(THEMES).filter(([k]) => k !== "colors").flatMap(([, t]) => t.words).filter(w => !seen.has(w.e) && seen.add(w.e)); })();
  const initial = w => { const m = (ABC.py[w.zh] || "").match(/^(zh|ch|sh|[bpmfdtnlgkhjqxrzcsyw])/); return m ? m[1] : null; };
  // the letter as the voice says it
  const nameIn = (L, l) => L === "de" ? ABC.deName[l] : L === "zh" ? ABC.zhSound[l] : l;
  // the question is always written (Kezhan: « c'est l'occasion de lire »): the letter in bold
  const bold = (L, l) => L === "zh" ? ` <b>${l}</b> ` : `<b>${l}</b>`;
  const tile = l => `<span class="lettre${typeof fx !== "undefined" && fx.calm() ? "" : " vive"}">${l}</span>`;
  const others = (l, set, n) => {
    const traps = lvl === 2 ? (ABC.traps[l] || "").split(" ").filter(x => x && x !== l && set.includes(x)) : [];
    return pick(traps, Math.min(2, n)).concat(pick(set.filter(x => x !== l && !traps.includes(x)), n)).slice(0, n);
  };

  // Luxembourgish levels 1-2: letters shown by nouns that have a lod.lu recording
  function lbLetters(){
    const by = {};
    const add = (l, w) => (by[l] = by[l] || []).push(w);
    words.filter(w => w.lb && w.lod && /^[A-ZÄÉ]/.test(strip(w.lb))).forEach(w => {
      const s = strip(w.lb); add(s[0], w);
      if (s.includes("é")) add("É", w);
      if (s.includes("ë")) add("Ë", w);
    });
    return by;
  }
  const mark = (w, l) => {
    const s = strip(w.lb), art = w.lb.slice(0, w.lb.length - s.length), i = "ÉË".includes(l) ? s.indexOf(l.toLowerCase()) : 0;
    return `${art}${s.slice(0, i)}<b>${s[i]}</b>${s.slice(i + 1)}`;
  };

  // levels 3-4: words of the lexicon by their first letter in the language (Chinese: initial of the first syllable)
  function wordsBy(L){
    const items = words.map(w => {
      if (L === "zh") { const i = initial(w); return i && {w, first: i, name: w.zh, label: `${w.zh} ${ABC.py[w.zh]}`}; }
      if (L === "lb" && !w.lod) return null;
      const name = L === "en" ? w.en : strip(w[L]);
      return {w, first: name[0].toUpperCase(), name, label: name};
    }).filter(Boolean);
    const by = {}; items.forEach(x => (by[x.first] = by[x.first] || []).push(x));
    return {items, by};
  }

  // what each round asks, per language, chosen once the first time the language is played
  const plans = {};
  function plan(L){
    if (plans[L]) return plans[L];
    let targets;
    if (lvl >= 3) targets = pick(Object.keys(wordsBy(L).by), total);
    else if (L === "lb") {
      const by = lbLetters(), more = ["Ä", "É", "Ë"].filter(l => by[l]), base = Object.keys(by).filter(l => !more.includes(l));
      targets = lvl === 1 ? pick(base, total) : pick(more, 2).concat(pick(base, total - 2));
    } else {
      const [base, more] = ABC.letters[L].map(s => s.split(L === "zh" ? " " : ""));
      const k = lvl === 1 ? 0 : L === "zh" ? 3 : 2;
      targets = pick(more, k).concat(pick(base, total - k));
    }
    return (plans[L] = shuffle(targets));
  }

  function build(i){
    const L = langOf(), p = plan(L), l = p[i % p.length], T = ABC.txt[L], n = lvl === 1 ? 3 : 4;
    if (lvl >= 3) { // something that starts with the letter
      const {items, by} = wordsBy(L), x = pick(by[l], 1)[0];
      const choices = [x, ...pick(items.filter(y => y.first !== l), lvl === 3 ? 2 : 3)];
      if (L === "lb") // no voice for letters: the words are heard through their recording
        return {lang: L, show: "🔍 " + T.starts(bold(L, l)), word: `${l}:${x.w.en}`, praise: x.w.lb,
          choices: choices.map(y => ({html: y.w.e, ok: y === x, label: y.label, sayWrong: y.w.lb}))};
      return {lang: L, say: T.starts(nameIn(L, l)), show: "🔍 " + T.starts(bold(L, l)), word: `${l}:${x.w.en}`,
        praise: T.yes(x.name, nameIn(L, l)),
        choices: choices.map(y => ({html: y.w.e, ok: y === x, label: y.label, sayWrong: T.is(y.name, nameIn(L, y.first))}))};
    }
    if (L === "lb") { // a recorded word shows the letter; a wrong tap plays the word again
      const by = lbLetters(), set = Object.keys(by).filter(x => lvl === 2 || !"ÄÉË".includes(x)), w = pick(by[l], 1)[0];
      return {lang: L, say: w.lb, show: `${w.e} <span class="abc-mot">${mark(w, l)}</span><small>🔤 ${T.find}</small>`, word: l,
        choices: [l, ...others(l, set, n - 1)].map(x => ({html: tile(x), ok: x === l}))};
    }
    const [base, more] = ABC.letters[L].map(s => s.split(L === "zh" ? " " : "")), set = lvl === 1 ? base : base.concat(more);
    return {lang: L, say: T.find(nameIn(L, l)), show: "🔤 " + T.find(bold(L, l)), word: l,
      choices: [l, ...others(l, set, n - 1)].map(x => ({html: tile(x), ok: x === l, sayWrong: T.that(nameIn(L, x), nameIn(L, l))}))};
  }

  // each round is written when it starts, in the language of that moment
  const lazy = i => { let r = null; return new Proxy({}, {get: (_, k) => (r = r || build(i))[k]}); };
  runQuiz("alphabet", null, Array.from({length: total}, (_, i) => lazy(i)));
});
