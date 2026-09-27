/* L'Île aux Mots : jeu « jours, mois et saisons », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   1: the four seasons in pictures · 2: the day after · 3: the day before, today and tomorrow (real date) · 4: months in order and months of the year's feasts */
registerGame({id:"calendrier", em:"📅", name:"Calendar", desc:"Days, months, seasons", multi:true}, function () {
  const lvl = levelOf("calendrier"), lang = langOf() === "fr" ? "en" : langOf();
  const DAYS = {
    en: ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],
    de: ["Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"],
    lb: ["Méindeg","Dënschdeg","Mëttwoch","Donneschdeg","Freideg","Samschdeg","Sonndeg"],
    zh: ["星期一","星期二","星期三","星期四","星期五","星期六","星期天"]
  }[lang];
  const MONTHS = {
    en: ["January","February","March","April","May","June","July","August","September","October","November","December"],
    de: ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"],
    lb: ["Januar","Februar","Mäerz","Abrëll","Mee","Juni","Juli","August","September","Oktober","November","Dezember"],
    zh: ["一月","二月","三月","四月","五月","六月","七月","八月","九月","十月","十一月","十二月"]
  }[lang];
  const SEASONS = [["🌸", {en:"spring", de:"der Frühling", lb:"de Fréijoer", zh:"春天"}], ["☀️", {en:"summer", de:"der Sommer", lb:"de Summer", zh:"夏天"}],
    ["🍂", {en:"autumn", de:"der Herbst", lb:"den Hierscht", zh:"秋天"}], ["⛄", {en:"winter", de:"der Winter", lb:"de Wanter", zh:"冬天"}]];
  const Q = {
    en: {find: s => `Find ${s}!`, after: d => `What comes after ${d}?`, before: d => `What comes before ${d}?`, today: "What day is it today?", tomorrow: "What day is it tomorrow?", monthAfter: m => `Which month comes after ${m}?`, feast: f => `In which month is ${f}?`},
    de: {find: s => `Zeig mir ${s}!`, after: d => `Was kommt nach ${d}?`, before: d => `Was kommt vor ${d}?`, today: "Welcher Tag ist heute?", tomorrow: "Welcher Tag ist morgen?", monthAfter: m => `Welcher Monat kommt nach ${m}?`, feast: f => `In welchem Monat ist ${f}?`},
    lb: {find: s => `Weis mer ${s}!`, after: d => `Wat kënnt no ${d}?`, before: d => `Wat kënnt virun ${d}?`, today: "Wéi een Dag ass haut?", tomorrow: "Wéi een Dag ass muer?", monthAfter: m => `Wéi ee Mount kënnt no ${m}?`, feast: f => `A wéi engem Mount ass ${f}?`},
    zh: {find: s => `找到${s}！`, after: d => `${d}后面是哪一天？`, before: d => `${d}前面是哪一天？`, today: "今天是星期几？", tomorrow: "明天是星期几？", monthAfter: m => `${m}后面是几月？`, feast: f => `${f}在几月？`}
  }[lang];
  const FEASTS = [[11, "🎄", {en:"Christmas", de:"Weihnachten", lb:"Chrëschtdag", zh:"圣诞节"}], [0, "🎆", {en:"New Year", de:"Neujahr", lb:"Neijoerschdag", zh:"新年"}],
    [8, "🎒", {en:"the first day of school", de:"der erste Schultag", lb:"den éischte Schouldag", zh:"开学"}], [9, "🎃", {en:"Halloween", de:"Halloween", lb:"Halloween", zh:"万圣节"}]];
  const txt = t => `<span style="font-family:var(--display)">${t}</span>`;
  const opts = (right, pool, n) => [right, ...pick(pool.filter(x => x !== right), n - 1)].map(x => ({html: txt(x), small: true, ok: x === right}));
  const todayIdx = (new Date().getDay() + 6) % 7; // Monday = 0
  const rounds = [...Array(8)].map((_, k) => {
    if (lvl === 1) {
      const s = pick(SEASONS, 1)[0], others = pick(SEASONS.filter(x => x !== s), 2);
      return {lang, say: Q.find(s[1][lang]), show: "🌸 ☀️ 🍂 ⛄", word: s[1].en, choices: [s, ...others].map(x => ({html: x[0], ok: x === s}))};
    }
    if (lvl === 2) {
      const d = rnd(7), right = DAYS[(d + 1) % 7];
      return {lang, say: Q.after(DAYS[d]), show: Q.after(DAYS[d]), word: `after ${DAYS[d]}`, choices: opts(right, DAYS, 4)};
    }
    if (lvl === 3) {
      if (k % 4 === 0) return {lang, say: Q.today, show: Q.today, word: "today", choices: opts(DAYS[todayIdx], DAYS, 4)};
      if (k % 4 === 1) return {lang, say: Q.tomorrow, show: Q.tomorrow, word: "tomorrow", choices: opts(DAYS[(todayIdx + 1) % 7], DAYS, 4)};
      const d = rnd(7), right = DAYS[(d + 6) % 7];
      return {lang, say: Q.before(DAYS[d]), show: Q.before(DAYS[d]), word: `before ${DAYS[d]}`, choices: opts(right, DAYS, 4)};
    }
    if (k % 3 === 2) { // the month of a feast
      const [m, e, f] = pick(FEASTS, 1)[0];
      return {lang, say: Q.feast(f[lang]), show: `${e} ${Q.feast(f[lang])}`, word: `feast ${f.en}`, choices: opts(MONTHS[m], MONTHS, 4)};
    }
    const m = rnd(12), right = MONTHS[(m + 1) % 12];
    return {lang, say: Q.monthAfter(MONTHS[m]), show: Q.monthAfter(MONTHS[m]), word: `after ${MONTHS[m]}`, choices: opts(right, MONTHS, 4)};
  });
  runQuiz("calendrier", null, rounds);
});
