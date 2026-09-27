/* L'Île aux Mots : jeu « pays, drapeaux, langues et capitales », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   1: find a flag among 3 very different ones · 2: eleven countries, 4 flags · 3: where do people speak …? · 4: capitals, written answers */
registerGame({id:"pays", em:"🌍", name:"Flags", desc:"Countries of the world", multi:true}, function () {
  const lvl = levelOf("pays"), lang = langOf() === "fr" ? "en" : langOf();
  const C = [
    {f:"🇱🇺", en:"Luxembourg", de:"Luxemburg", lb:"Lëtzebuerg", zh:"卢森堡", cap:{en:"Luxembourg", de:"Luxemburg", lb:"Lëtzebuerg", zh:"卢森堡市"}, speak:"lb"},
    {f:"🇫🇷", en:"France", de:"Frankreich", lb:"Frankräich", zh:"法国", cap:{en:"Paris", de:"Paris", lb:"Paräis", zh:"巴黎"}},
    {f:"🇩🇪", en:"Germany", de:"Deutschland", lb:"Däitschland", zh:"德国", cap:{en:"Berlin", de:"Berlin", lb:"Berlin", zh:"柏林"}},
    {f:"🇧🇪", en:"Belgium", de:"Belgien", lb:"der Belsch", zh:"比利时", cap:{en:"Brussels", de:"Brüssel", lb:"Bréissel", zh:"布鲁塞尔"}},
    {f:"🇨🇳", en:"China", de:"China", lb:"China", zh:"中国", cap:{en:"Beijing", de:"Peking", lb:"Peking", zh:"北京"}, speak:"zh"},
    {f:"🇬🇧", en:"the United Kingdom", de:"Großbritannien", lb:"Groussbritannien", zh:"英国", cap:{en:"London", de:"London", lb:"London", zh:"伦敦"}},
    {f:"🇮🇹", en:"Italy", de:"Italien", lb:"Italien", zh:"意大利", cap:{en:"Rome", de:"Rom", lb:"Roum", zh:"罗马"}, speak:"it"},
    {f:"🇪🇸", en:"Spain", de:"Spanien", lb:"Spuenien", zh:"西班牙", cap:{en:"Madrid", de:"Madrid", lb:"Madrid", zh:"马德里"}, speak:"es"},
    {f:"🇵🇹", en:"Portugal", de:"Portugal", lb:"Portugal", zh:"葡萄牙", cap:{en:"Lisbon", de:"Lissabon", lb:"Lissabon", zh:"里斯本"}, speak:"pt"},
    {f:"🇯🇵", en:"Japan", de:"Japan", lb:"Japan", zh:"日本", cap:{en:"Tokyo", de:"Tokio", lb:"Tokio", zh:"东京"}, speak:"ja"},
    {f:"🇺🇸", en:"America", de:"Amerika", lb:"Amerika", zh:"美国", cap:{en:"Washington", de:"Washington", lb:"Washington", zh:"华盛顿"}}
  ];
  // languages spoken in only one country of the game, so the answer is never ambiguous
  const LANGUES = {lb:{en:"Luxembourgish", de:"Luxemburgisch", lb:"Lëtzebuergesch", zh:"卢森堡语"}, zh:{en:"Chinese", de:"Chinesisch", lb:"Chinesesch", zh:"中文"},
    it:{en:"Italian", de:"Italienisch", lb:"Italienesch", zh:"意大利语"}, es:{en:"Spanish", de:"Spanisch", lb:"Spuenesch", zh:"西班牙语"},
    pt:{en:"Portuguese", de:"Portugiesisch", lb:"Portugisesch", zh:"葡萄牙语"}, ja:{en:"Japanese", de:"Japanisch", lb:"Japanesch", zh:"日语"}};
  // Luxembourgish "vu/vun" follows the Eifel rule: the n stays before a vowel or h, n, d, t, z
  const vun = x => /^[aeiouäëéhndtz]/i.test(x) ? "vun" : "vu";
  const Q = {
    en: {flag: c => `Find the flag of ${c}!`, speak: l => `Where do people speak ${l}?`, cap: c => `What is the capital of ${c}?`},
    de: {flag: c => `Zeig mir die Flagge von ${c}!`, speak: l => `Wo spricht man ${l}?`, cap: c => `Was ist die Hauptstadt von ${c}?`},
    lb: {flag: c => `Weis mer de Fändel ${vun(c)} ${c}!`, speak: l => `Wou schwätzt een ${l}?`, cap: c => `Wat ass d'Haaptstad ${vun(c)} ${c}?`},
    zh: {flag: c => `找到${c}的国旗！`, speak: l => `哪个国家说${l}？`, cap: c => `${c}的首都是哪里？`}
  }[lang];
  const flagChoice = (right, n) => [right, ...pick(C.filter(c => c !== right), n - 1)].map(c => ({html: c.f, label: c[lang], ok: c === right}));
  const FIRST = [C[0], C[4], C[1], C[2]]; // the little one starts with Luxembourg, China, France, Germany
  const rounds = [...Array(8)].map((_, k) => {
    if (lvl === 1) { const c = FIRST[k % 4]; return {lang, say: Q.flag(c[lang]), show: "🇱🇺 🇨🇳 🇫🇷 🇩🇪", word: c.en, choices: flagChoice(c, 3)}; }
    if (lvl === 2) { const c = pick(C, 1)[0]; return {lang, say: Q.flag(c[lang]), show: Q.flag(c[lang]), word: c.en, choices: flagChoice(c, 4)}; }
    if (lvl === 3) {
      const c = pick(C.filter(x => x.speak), 1)[0], l = LANGUES[c.speak][lang];
      return {lang, say: Q.speak(l), show: Q.speak(l), word: `speak ${c.speak}`, choices: flagChoice(c, 4)};
    }
    const c = pick(C, 1)[0], others = pick(C.filter(x => x !== c), 3);
    return {lang, say: "", listen: Q.cap(c[lang]), show: `${c.f} <b>${Q.cap(c[lang])}</b>`, word: `capital ${c.en}`,
      choices: [c, ...others].map(x => ({html: `<span style="font-family:var(--display)">${x.cap[lang]}</span>`, small: true, ok: x === c}))};
  });
  runQuiz("pays", null, rounds);
});
