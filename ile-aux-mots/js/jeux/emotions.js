/* L'Île aux Mots : jeu « les émotions », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   1: find the face, 3 faces · 2: eight feelings, 4 faces · 3: a short situation heard, how do I feel? · 4: the situation is written, answer with the word */
registerGame({id:"emotions", em:"😀", name:"Feelings", desc:"How do you feel?", multi:true}, function () {
  const lvl = levelOf("emotions"), lang = langOf() === "fr" ? "en" : langOf();
  const F = [
    {e:"😀", en:"happy", de:"fröhlich", lb:"frou", zh:"开心"},
    {e:"😢", en:"sad", de:"traurig", lb:"traureg", zh:"难过"},
    {e:"😠", en:"angry", de:"wütend", lb:"rosen", zh:"生气"},
    {e:"😨", en:"scared", de:"ängstlich", lb:"bang", zh:"害怕"},
    {e:"😮", en:"surprised", de:"überrascht", lb:"iwwerrascht", zh:"惊讶"},
    {e:"😴", en:"tired", de:"müde", lb:"midd", zh:"累"},
    {e:"🤒", en:"sick", de:"krank", lb:"krank", zh:"生病了"},
    {e:"😋", en:"hungry", de:"hungrig", lb:"hongereg", zh:"饿"}
  ];
  const by = en => F.find(f => f.en === en);
  // short situations, each with the feeling it brings
  const SIT = [
    ["happy", {en:"It's my birthday!", de:"Heute ist mein Geburtstag!", lb:"Haut ass mäi Gebuertsdag!", zh:"今天是我的生日！"}],
    ["sad", {en:"My ice cream fell on the floor.", de:"Mein Eis ist auf den Boden gefallen.", lb:"Meng Glace ass op de Buedem gefall.", zh:"我的冰淇淋掉在地上了。"}],
    ["angry", {en:"My brother broke my toy!", de:"Mein Bruder hat mein Spielzeug kaputt gemacht!", lb:"Mäi Brudder huet mäi Spillsaach futti gemaach!", zh:"哥哥把我的玩具弄坏了！"}],
    ["scared", {en:"There is a big spider on my bed!", de:"Auf meinem Bett ist eine große Spinne!", lb:"Op mengem Bett ass eng grouss Spann!", zh:"我的床上有一只大蜘蛛！"}],
    ["surprised", {en:"Look! A present for me!", de:"Schau! Ein Geschenk für mich!", lb:"Kuck! E Kaddo fir mech!", zh:"看！给我的礼物！"}],
    ["tired", {en:"It is very late. I want to sleep.", de:"Es ist sehr spät. Ich will schlafen.", lb:"Et ass ganz spéit. Ech wëll schlofen.", zh:"很晚了，我想睡觉。"}],
    ["sick", {en:"I have a cold and a fever.", de:"Ich habe Schnupfen und Fieber.", lb:"Ech hunn de Schnapp an Hëtzt.", zh:"我感冒了，还发烧。"}],
    ["hungry", {en:"I didn't eat lunch today.", de:"Ich habe heute nicht zu Mittag gegessen.", lb:"Ech hunn haut net zu Mëtteg giess.", zh:"我今天没吃午饭。"}]
  ];
  const Q = {
    en: {find: w => `Who is ${w}?`, how: "How do I feel?"},
    de: {find: w => `Wer ist ${w}?`, how: "Wie fühle ich mich?"},
    lb: {find: w => `Wien ass ${w}?`, how: "Wéi fillen ech mech?"},
    zh: {find: w => `谁${w}？`, how: "我感觉怎么样？"}
  }[lang];
  const face = f => ({html: f.e, label: f[lang]});
  const rounds = pick(lvl <= 2 ? F.slice(0, lvl === 1 ? 4 : 8) : SIT, 8).map(x => {
    if (lvl <= 2) {
      const others = pick(F.slice(0, lvl === 1 ? 4 : 8).filter(f => f !== x), lvl === 1 ? 2 : 3);
      return {lang, say: Q.find(x[lang]), show: Q.find(x[lang]), word: x.en, choices: [x, ...others].map(f => ({...face(f), ok: f === x}))};
    }
    const [feel, s] = x, right = by(feel), others = pick(F.filter(f => f !== right), 3);
    if (lvl === 3) return {lang, say: `${s[lang]} ${Q.how}`, show: `${s[lang]}<small>${Q.how}</small>`, word: feel, choices: [right, ...others].map(f => ({...face(f), ok: f === right}))};
    return {lang, say: "", listen: `${s[lang]} ${Q.how}`, show: `<b>${s[lang]}</b><small>${Q.how}</small>`, word: feel,
      choices: [right, ...others].map(f => ({html: `<span style="font-family:var(--display)">${f[lang]}</span>`, small: true, ok: f === right}))};
  });
  runQuiz("emotions", null, rounds);
});
