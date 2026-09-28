/* L'Île aux Mots : contenu du « Labo des monstres » (jeu labo).
   Pièces du monstre et phrases générées en anglais, allemand, luxembourgeois et chinois, jamais en français.
   Allemand : accusatif après haben, geben, tragen, halten (einen Hut, ein großes Auge), datif après in (in der linken Hand).
   Luxembourgeois vérifié sur lod.lu (Aen, Äerm, Been, Oueren, grousst, lénkser, Kroun, Punkten, Sträifen, repsen...),
   règle de l'Eifel ; k = mot du lexique que say(k, "lb") fait entendre (enregistrement lod.lu).
   Chinois : 两 devant un classificateur (两只眼睛, 两条腿), 一顶帽子, 一副眼镜, 一根香蕉. */
const LABO = (() => {
  const COLS = ["red", "blue", "green", "yellow", "orange", "pink", "purple", "brown"];
  const lex = en => { for (const t of Object.values(THEMES)) { const w = t.words.find(x => x.en === en); if (w) return w; } return {}; };
  // colours from the colour theme only ("orange" is also a fruit)
  const col = c => THEMES.colors.words.find(w => w.en === c) || {};
  const colW = (c, L) => col(c)[L] || c;
  const hex = c => col(c).e || "#B8BEC6";
  // counted parts: de [gender, one, many], lb [one, many] (all masculine or neuter: zwee, een), zh [noun, classifier]
  const PART = {
    eyes: {en: ["eye", "eyes"], de: ["n", "Auge", "Augen"], lb: ["A", "Aen"], k: "d'Aen", zh: ["眼睛", "只"]},
    arms: {en: ["arm", "arms"], de: ["m", "Arm", "Arme"], lb: ["Aarm", "Äerm"], k: "den Aarm", zh: ["胳膊", "条"]},
    legs: {en: ["leg", "legs"], de: ["n", "Bein", "Beine"], lb: ["Been", "Been"], k: "d'Been", zh: ["腿", "条"]},
    ears: {en: ["ear", "ears"], de: ["n", "Ohr", "Ohren"], lb: ["Ouer", "Oueren"], k: "d'Ouer", zh: ["耳朵", "只"]}
  };
  const SIZE = {big: {en: "big", de: "groß", lb: "grouss", zh: "大"}, small: {en: "small", de: "klein", lb: "kleng", zh: "小"}};
  // moods: lb heard through a lexicon verb (laachen, kräischen, schlofen)
  const MOOD = {
    happy:  {e: "😀", en: "happy", de: "fröhlich", lb: "frou", zh: "开心", k: "laachen"},
    sad:    {e: "😢", en: "sad", de: "traurig", lb: "traureg", zh: "难过", k: "kräischen"},
    sleepy: {e: "😴", en: "sleepy", de: "müde", lb: "midd", zh: "困", k: "schlofen"},
    angry:  {e: "😠", en: "angry", de: "wütend", lb: "rosen", zh: "生气"},
    scared: {e: "😨", en: "scared", de: "Angst", lb: "Angscht", zh: "害怕"}
  };
  const HAT = {
    hat:     {e: "🎩", en: "a hat", de: "einen Hut", lb: "en Hutt", zh: "一顶帽子", k: "den Hutt"},
    cap:     {e: "🧢", en: "a cap", de: "eine Kappe", lb: "eng Kap", zh: "一顶鸭舌帽", k: "d'Kap"},
    glasses: {e: "👓", en: "glasses", de: "eine Brille", lb: "e Brëll", zh: "一副眼镜", k: "de Brëll"},
    crown:   {e: "👑", en: "a crown", de: "eine Krone", lb: "eng Kroun", zh: "一顶皇冠"},
    helmet:  {e: "⛑️", en: "a helmet", de: "einen Helm", lb: "en Helm", zh: "一顶头盔"}
  };
  // things held in a hand: all in the lexicon, so Luxembourgish is heard (k = lexicon word)
  const HOLD = {
    banana:      {e: "🍌", en: "a banana", de: "eine Banane", lb: "eng Banann", zh: "一根香蕉"},
    apple:       {e: "🍎", en: "an apple", de: "einen Apfel", lb: "en Apel", zh: "一个苹果"},
    flower:      {e: "🌸", en: "a flower", de: "eine Blume", lb: "eng Blumm", zh: "一朵花"},
    fish:        {e: "🐟", en: "a fish", de: "einen Fisch", lb: "e Fësch", zh: "一条鱼"},
    ball:        {e: "⚽", en: "a ball", de: "einen Ball", lb: "e Ball", zh: "一个球"},
    umbrella:    {e: "☂️", en: "an umbrella", de: "einen Regenschirm", lb: "e Prabbeli", zh: "一把雨伞"},
    carrot:      {e: "🥕", en: "a carrot", de: "eine Karotte", lb: "eng Muert", zh: "一根胡萝卜"},
    cake:        {e: "🍰", en: "a cake", de: "einen Kuchen", lb: "e Kuch", zh: "一块蛋糕"},
    "ice cream": {e: "🍦", en: "an ice cream", de: "ein Eis", lb: "eng Glace", zh: "一个冰淇淋"},
    key:         {e: "🔑", en: "a key", de: "einen Schlüssel", lb: "e Schlëssel", zh: "一把钥匙"}
  };
  const PAT = {
    spots:   {en: "spots", de: "Punkte", lb: "Punkten", zh: "圆点"},
    stripes: {en: "stripes", de: "Streifen", lb: "Sträifen", zh: "条纹"},
    stars:   {en: "stars", de: "Sterne", lb: "Stären", zh: "星星", k: "de Stär"}
  };
  // side of the hand: as seen on the screen; w = the word on the button
  const SIDE = {
    L: {en: "left", de: "linken", lb: "lénkser", zh: "左", w: {en: "left", de: "links", lb: "lénks", zh: "左"}},
    R: {en: "right", de: "rechten", lb: "rietser", zh: "右", w: {en: "right", de: "rechts", lb: "riets", zh: "右"}}
  };
  // Eifel rule: a final n stays only before a vowel or d, t, z, h, n (hunn → hu, sinn → si, een → ee)
  const eif = (w, next) => /^[aeiouäëéöüdtzhn]/i.test(next) ? w : w.replace(/n?n$/, "");
  const zhNum = n => n === 2 ? "两" : numberIn(n, "zh");
  // "three big eyes", "ein großes Auge", "ee klengt Ouer", "两只大眼睛"; zero gives "no eyes" (zh: the noun alone)
  function np(t, L){
    const P = PART[t.k], n = t.n, s = t.s && SIZE[t.s];
    if (L === "de") {
      const [g, one, many] = P.de;
      if (!n) return `keine ${many}`;
      if (n === 1) return g === "m" ? `einen ${s ? s.de + "en " : ""}${one}` : `ein ${s ? s.de + "es " : ""}${one}`;
      return `${numberIn(n, "de")} ${s ? s.de + "e " : ""}${many}`;
    }
    if (L === "lb") {
      const [one, many] = P.lb;
      if (!n) return `keng ${many}`;
      if (n === 1) return s ? `${eif("een", s.lb)} ${s.lb}t ${one}` : `${eif("een", one)} ${one}`;
      return `${numberIn(n, "lb")} ${s ? s.lb + " " : ""}${many}`;
    }
    if (L === "zh") return n ? `${zhNum(n)}${P.zh[1]}${s ? s.zh : ""}${P.zh[0]}` : P.zh[0];
    return n ? `${numberWords(n)} ${s ? s.en + " " : ""}${P.en[n === 1 ? 0 : 1]}` : `no ${P.en[1]}`;
  }
  const COUNT = ["eyes", "arms", "legs", "ears"];
  /* one sentence about one trait, as [before, focus, after]
     mode "it": the description · "me": the monster says what is wrong · "do": a level 1 order; side: only the side is wrong */
  function parts(t, L, mode, side){
    const r = t.r || 0, cnt = COUNT.includes(t.k);
    const w = t.k === "col" ? colW(t.v, L) : cnt ? np(t, L) : ({mood: MOOD, hat: HAT, hold: HOLD, pat: PAT}[t.k] || {})[t.v][L];
    const S = t.side && SIDE[t.side][L];
    if (L === "de") {
      if (t.k === "col") return mode === "do" ? ["Mach es ", w, "!"] : mode === "me" ? ["Ich bin ", w, "!"] : ["Es ist ", w, "."];
      if (cnt) return mode === "do" ? ["Gib ihm ", w, "!"] : mode === "me" ? ["Ich habe ", w, "!"] : [["Es hat ", "Das Monster hat "][r % 2], w, "."];
      if (t.k === "mood") { const a = t.v === "scared";
        return mode === "do" ? [a ? "Mach ihm " : "Mach es ", w, "!"] : mode === "me" ? [a ? "Ich habe " : "Ich bin ", w, "!"] : [a ? "Es hat " : "Es ist ", w, "."]; }
      if (t.k === "hat") return mode === "do" ? ["Gib ihm ", w, "!"] : mode === "me" ? ["Ich trage ", w, "!"] : [["Es trägt ", "Es hat "][r % 2], w, "."];
      if (t.k === "hold") return S ? (side ? [`${mode === "me" ? "Ich habe" : "Es hat"} ${w} in der `, S, ` Hand${mode === "me" ? "!" : "."}`] : [mode === "me" ? "Ich habe " : "Es hat ", w, ` in der ${S} Hand${mode === "me" ? "!" : "."}`])
        : mode === "do" ? ["Gib ihm ", w, "!"] : mode === "me" ? ["Ich halte ", w, "!"] : [["Es hält ", "Es hat "][r % 2], w, "."];
      return mode === "me" ? ["Ich habe ", w, "!"] : ["Es hat ", w, "."];
    }
    if (L === "lb") {
      const me = v => `Ech ${eif(v, w)} `;
      if (t.k === "col") return mode === "do" ? ["Maach et ", w, "!"] : mode === "me" ? [me("sinn"), w, "!"] : ["Et ass ", w, "."];
      if (cnt) return mode === "do" ? ["Gëff him ", w, "!"] : mode === "me" ? [me("hunn"), w, "!"] : [["Et huet ", "D'Monster huet "][r % 2], w, "."];
      if (t.k === "mood") {
        if (mode === "do") return {happy: ["Et soll ", "laachen", "!"], sad: ["Et soll ", "kräischen", "!"], sleepy: ["Et soll ", "schlofen", "!"], angry: ["Maach et ", w, "!"], scared: ["Maach him ", w, "!"]}[t.v];
        if (mode === "me") return t.v === "scared" ? ["Ech hunn ", w, "!"] : [me("sinn"), w, "!"];
        return t.v === "scared" ? ["Et huet ", w, "."] : ["Et ass ", w, MOOD[t.v].k ? ` a wëll ${MOOD[t.v].k}.` : "."];
      }
      if (t.k === "hat") return mode === "do" ? ["Gëff him ", w, "!"] : mode === "me" ? [me("hunn"), w, " un!"] : ["Et huet ", w, " un."];
      if (t.k === "hold") return S ? (side ? [`${mode === "me" ? me("hunn") : "Et huet "}${w} an der `, S, ` Hand${mode === "me" ? "!" : "."}`] : [mode === "me" ? me("hunn") : "Et huet ", w, ` an der ${S} Hand${mode === "me" ? "!" : "."}`])
        : mode === "do" ? ["Gëff him ", w, "!"] : mode === "me" ? [me("halen"), w, "!"] : ["Et hält ", w, "."];
      return mode === "me" ? [me("hunn"), w, "!"] : [["Et huet ", "D'Monster huet "][r % 2], w, "."];
    }
    if (L === "zh") {
      const end = mode === "it" ? "。" : "！", who = mode === "me" ? "我" : "它";
      if (t.k === "col") return mode === "do" ? ["把它变成", w, "！"] : [who + "是", w, "的" + end];
      if (cnt) return mode === "do" ? ["给它加上", w, "！"] : t.n ? [mode === "it" ? ["它有", "这个怪兽有"][r % 2] : "我有", w, end] : [who, "没有" + w, end];
      if (t.k === "mood") return mode === "do" ? {happy: ["让它", w, "！"], sad: ["让它", w, "！"], angry: ["让它", w, "！"], scared: ["", "吓吓", "它！"], sleepy: ["让它", "打瞌睡", "！"]}[t.v] : [who + "很", w, end];
      if (t.k === "hat") return mode === "do" ? ["给它戴上", w, "！"] : [who + "戴着", w, end];
      if (t.k === "hold") return S ? (side ? [who, S + "手", `拿着${w}${end}`] : [`${who}${S}手拿着`, w, end]) : mode === "do" ? ["给它", w, "！"] : [who + "拿着", w, end];
      return [who + "身上有", w, end];
    }
    const has = ["It has ", "It has got ", "It's got "][r % 3];
    if (t.k === "col") return mode === "do" ? ["Make it ", w, "!"] : mode === "me" ? ["I'm ", w, "!"] : [["It is ", "It's "][r % 2], w, "."];
    if (cnt) return mode === "do" ? ["Give it ", w, "!"] : mode === "me" ? ["I've got ", w, "!"] : [has, w, "."];
    if (t.k === "mood") return mode === "do" ? ["Make it ", w, "!"] : mode === "me" ? ["I'm ", w, "!"] : [["It is ", "It looks ", "It's "][r % 3], w, "."];
    if (t.k === "hat") return mode === "do" ? ["Give it ", w, "!"] : mode === "me" ? ["I'm wearing ", w, "!"] : [["It is wearing ", "It has got "][r % 2], w, "."];
    if (t.k === "hold") return S ? (side ? [mode === "me" ? `I've got ${w} in my ` : `It has ${w} in its `, S, mode === "me" ? " hand!" : " hand."] : [mode === "me" ? "I've got " : ["It has ", "It is holding "][r % 2], w, ` in ${mode === "me" ? "my" : "its"} ${S} hand${mode === "me" ? "!" : "."}`])
      : mode === "do" ? ["Give it ", w, "!"] : mode === "me" ? ["I'm holding ", w, "!"] : [["It is holding ", "It has got "][r % 2], w, "."];
    return mode === "me" ? ["I've got ", w, "!"] : [has, w, "."];
  }
  // lb: the lexicon word to hear for a trait
  const key = t => t.k === "col" ? colW(t.v, "lb") : COUNT.includes(t.k) ? PART[t.k].k : t.k === "hold" ? lex(t.v).lb : (({mood: MOOD, hat: HAT, pat: PAT}[t.k] || {})[t.v] || {}).k;
  function line(t, L, mode = "it", side){
    const [a, b, c] = parts(t, L, mode, side);
    return {t: a + b + c, h: `${a}<b>${b}</b>${c}`, k: key(t)};
  }
  // does lb have a recording for this trait? (the 4-year-old cannot read)
  const heard = t => !!key(t);
  const UI = {
    again:  {en: "Again", de: "Nochmal", lb: "Nach eng Kéier", zh: "再听一次"},
    alive:  {en: "It's alive!", de: "Es lebt!", lb: "Et lieft!", zh: "它活了！"},
    almost: {en: "Almost!", de: "Fast!", lb: "Bal!", zh: "差一点！"},
    ready:  {en: "Now touch the lightning!", de: "Jetzt drück auf den Blitz!", lb: "Elo dréck op de Blëtz!", zh: "现在按一下闪电！"},
    notyet: {en: "Not yet!", de: "Noch nicht!", lb: "Nach net!", zh: "还没好呢！"},
    listen: {en: "Listen!", de: "Hör gut zu!", lb: "Lauschter gutt!", zh: "仔细听！"},
    read:   {en: "Read and build!", de: "Lies und bau!", lb: "Lies a bau!", zh: "读一读，做出来！"},
    which:  {en: "Which monster is it?", de: "Welches Monster ist es?", lb: "Wat fir e Monster ass et?", zh: "是哪一个怪兽？"},
    free:   {en: "Build any monster you like!", de: "Bau ein Monster, wie du willst!", lb: "Bau e Monster, wéi s de wëlls!", zh: "做一个你喜欢的怪兽吧！"},
    look:   {en: "Wow! Look at your monster!", de: "Wow! Schau dir dein Monster an!", lb: "Wow! Kuck däi Monster!", zh: "哇！看看你的怪兽！"},
    giggle: {en: "Hee hee!", de: "Hihi!", lb: "Hihi!", zh: "嘻嘻！"}
  };
  const HELLO = {en: "Hello! I'm {n}!", de: "Hallo! Ich bin {n}!", lb: "Moien! Ech sinn {n}!", zh: "你好！我叫{n}！"};
  // what a new monster says after its name (k: lb lexicon word inside the line)
  const SAYS = [
    {en: "Burp! Excuse me!", de: "Rülps! Entschuldigung!", lb: "Pardon, ech hu gerepst!", zh: "嗝！不好意思！"},
    {en: "I'm hungry! Where is my cake?", de: "Ich habe Hunger! Wo ist mein Kuchen?", lb: "Ech hunn Honger! Wou ass de Kuch?", zh: "我饿了！我的蛋糕在哪儿？", k: "de Kuch"},
    {en: "I love you!", de: "Ich hab dich lieb!", lb: "Ech hunn dech gär!", zh: "我喜欢你！"},
    {en: "Let's dance!", de: "Komm, wir tanzen!", lb: "Komm, mir danzen!", zh: "我们来跳舞吧！", k: "danzen"},
    {en: "Tickle me!", de: "Kitzel mich!", lb: "Këddel mech!", zh: "挠挠我！"},
    {en: "Boo! Are you scared?", de: "Buh! Hast du Angst?", lb: "Hues du Angscht?", zh: "哇！你害怕吗？"},
    {en: "Hooray! I'm alive!", de: "Hurra! Ich lebe!", lb: "Hurra! Ech liewen!", zh: "耶！我活了！"},
    {en: "I want to play!", de: "Ich will spielen!", lb: "Ech wëll spillen!", zh: "我想玩！"},
    {en: "Bye-bye! I'm going to the zoo!", de: "Tschüss! Ich gehe in den Zoo!", lb: "Äddi! Ech ginn an den Zoo!", zh: "拜拜！我去动物园啦！"},
    {en: "Look at me! I'm so beautiful!", de: "Schau mich an! Ich bin so schön!", lb: "Kuck mech! Ech si sou schéin!", zh: "看我！我好漂亮！"},
    {en: "Where is the banana?", de: "Wo ist die Banane?", lb: "Wou ass d'Banann?", zh: "香蕉在哪儿？", k: "d'Banann"},
    {en: "I want to go swimming!", de: "Ich will schwimmen!", lb: "Ech wëll schwammen!", zh: "我想去游泳！", k: "schwammen"}
  ];
  // the mood played out loud, with the pitch and speed of the monster's voice
  const ACT = {
    happy:  {en: "Ha ha ha! I'm so happy!", de: "Hahaha! Ich bin so fröhlich!", lb: "Ech wëll laachen! Hahaha!", zh: "哈哈哈！我好开心！", k: "laachen", p: 1.6, r: 1.1},
    sad:    {en: "Sniff... I'm so sad.", de: "Schnief... Ich bin so traurig.", lb: "Ech wëll kräischen...", zh: "呜呜……我好难过。", k: "kräischen", p: .6, r: .8},
    sleepy: {en: "Yawn... Good night!", de: "Gähn... Gute Nacht!", lb: "Ech wëll schlofen! Gutt Nuecht!", zh: "哈欠……晚安！", k: "schlofen", p: .8, r: .75},
    angry:  {en: "Grrr! I'm angry!", de: "Grrr! Ich bin wütend!", lb: "Grrr! Ech si rosen!", zh: "哼！我生气了！", p: .5, r: 1},
    scared: {en: "Eek! Help!", de: "Iiih! Hilfe!", lb: "Hëllef! Ech hunn Angscht!", zh: "啊！救命啊！", p: 1.9, r: 1.4}
  };
  // silly names: 40 syllables that rhyme (Blobby Wobby); in Chinese a doubled sound (圆泡泡怪)
  const ON = ["Bl", "W", "Z", "Sn", "Gr", "Fl", "Sp", "Kr", "P", "B", "G", "Schn", "Tr", "M", "Pl", "Sm", "D", "N", "Br", "Gl"];
  const RIME = ["obby", "iggle", "umpy", "oodle", "azzle", "ibbo", "onky", "ubble", "inky", "izzy", "ozzle", "ummy", "ipper", "ungo", "ello", "iffy", "orky", "eeble", "uffin", "ooky"];
  const ZA = ["胖", "圆", "毛", "泡", "咕", "噗", "啾", "嘟", "豆", "糖", "果", "球", "跳", "乐", "哈", "呼", "滚", "棉", "软", "闪"];
  const ZB = ["嘟嘟", "噜噜", "泡泡", "球球", "毛毛", "蛋蛋", "包包", "豆豆", "糖糖", "乐乐"];
  function name([a, b, c], L){
    if (L === "zh") return ZA[a % ZA.length] + ZB[b % ZB.length] + "怪";
    const bb = b % ON.length === a % ON.length ? b + 1 : b;
    return ON[a % ON.length] + RIME[c % RIME.length] + " " + ON[bb % ON.length] + RIME[c % RIME.length];
  }
  const hello = (seed, L) => { const n = name(seed, L); return HELLO[L].replace(L === "lb" ? "sinn {n}" : "{n}", L === "lb" ? `${eif("sinn", n)} ${n}` : n); };
  return {COLS, PART, SIZE, MOOD, HAT, HOLD, PAT, SIDE, UI, SAYS, ACT, hex, colW, line, heard, name, hello};
})();

/* Le catalogue des pièces (le monstre en SVG, un <g> par partie) et le générateur de traits.
   A trait is {k, v} (col, mood, hat, pat), {k, n, s} for a counted part (eyes, arms, legs, ears; s = big or small), {k:"hold", v, side}. */
Object.assign(LABO, (() => {
  const INK = "#1B2D45", GREY = "#B8BEC6";
  const SH = [
    "M120 50C170 50 194 92 190 134C186 180 160 204 120 204C80 204 50 182 50 134C50 90 72 50 120 50Z",
    "M96 58H144A42 42 0 0 1 186 100V162A42 42 0 0 1 144 204H96A42 42 0 0 1 54 162V100A42 42 0 0 1 96 58Z",
    "M120 52C174 52 198 100 184 146C174 180 160 204 120 204C80 204 66 180 56 146C42 100 66 52 120 52Z",
    "M120 46C182 46 204 96 188 128C178 150 176 204 120 204C64 204 62 150 52 128C36 96 58 46 120 46Z"
  ];
  const EYES = [[], [[120, 100]], [[98, 100], [142, 100]], [[92, 106], [120, 84], [148, 106]], [[94, 88], [146, 88], [94, 118], [146, 118]], [[80, 108], [99, 86], [120, 78], [141, 86], [160, 108]]];
  const EARS = [[], [-90], [-120, -60], [-135, -90, -45], [-150, -112, -68, -30]];
  const MOUTH = {
    none: `<path d="M104 150Q120 157 136 150" fill="none" stroke="${INK}" stroke-width="4" stroke-linecap="round"/>`,
    happy: `<path d="M94 140Q120 184 146 140Z" fill="#7A1F2B" stroke="${INK}" stroke-width="4" stroke-linejoin="round"/><ellipse cx="120" cy="160" rx="11" ry="6" fill="#FF8A80"/>`,
    sad: `<path d="M100 164Q120 138 140 164" fill="none" stroke="${INK}" stroke-width="5" stroke-linecap="round"/>`,
    angry: `<rect x="98" y="144" width="44" height="18" rx="5" fill="#7A1F2B" stroke="${INK}" stroke-width="4"/><path d="M101 146l5 7 5-7 5 7 5-7 5 7 5-7 5 7 5-7z" fill="#fff"/>`,
    scared: `<ellipse cx="120" cy="156" rx="11" ry="15" fill="#7A1F2B" stroke="${INK}" stroke-width="4"/>`,
    sleepy: `<ellipse cx="120" cy="154" rx="7" ry="6" fill="#7A1F2B" stroke="${INK}" stroke-width="3"/>`
  };
  const BADGE = {sleepy: ["💤", 184, 62, 24], scared: ["💧", 172, 76, 20], angry: ["💢", 176, 70, 22]};
  let uid = 0;
  const limb = (d, c) => `<path d="${d}" fill="none" stroke="${INK}" stroke-width="16" stroke-linecap="round"/><path d="${d}" fill="none" stroke="${c}" stroke-width="9" stroke-linecap="round"/>`;
  const em = (e, x, y, s) => `<text x="${x}" y="${y}" font-size="${s}" text-anchor="middle" dominant-baseline="central">${e}</text>`;
  function svg(m, box = "0 0 240 240"){
    const c = m.col ? LABO.hex(m.col) : GREY, id = "labc" + ++uid, sh = SH[m.sh || 0], md = m.mood;
    const pc = !m.col || ["yellow", "orange", "pink"].includes(m.col) ? "rgba(27,45,69,.3)" : "rgba(255,255,255,.6)";
    let legs = "", arms = "", ears = "", eyes = "", pat = "";
    for (let j = 0; j < m.legs; j++) { const x = 120 + (j - (m.legs - 1) / 2) * 22; legs += `<g class="leg">${limb(`M${x} 186V219`, c)}<ellipse cx="${x + 5}" cy="224" rx="13" ry="7" fill="${c}" stroke="${INK}" stroke-width="3"/></g>`; }
    for (let j = 0; j < m.arms; j++) {
      const s = j % 2 ? 1 : -1, row = j >> 1, sx = 120 + s * 56, sy = 126 + row * 34, hx = 120 + s * (100 - row * 8), hy = 102 + row * 50;
      arms += `<g class="arm" style="transform-origin:${sx}px ${sy}px;--w:${s * 10}deg">${limb(`M${sx} ${sy}Q${120 + s * 90} ${sy + 8} ${hx} ${hy}`, c)}<circle cx="${hx}" cy="${hy}" r="10" fill="${c}" stroke="${INK}" stroke-width="3"/></g>`;
    }
    const k = {small: .7, big: 1.5}[m.rs] || 1;
    (EARS[m.ears] || []).forEach(a => {
      const x = 120 + Math.cos(a * Math.PI / 180) * 76, y = 128 + Math.sin(a * Math.PI / 180) * 76;
      ears += `<g transform="translate(${x.toFixed(1)} ${y.toFixed(1)}) rotate(${a + 90})"><ellipse cy="${-14 * k}" rx="${12 * k}" ry="${20 * k}" fill="${c}" stroke="${INK}" stroke-width="3"/><ellipse cy="${-15 * k}" rx="${6 * k}" ry="${12 * k}" fill="#F8BBD0"/></g>`;
    });
    const r = {small: 8, big: 17}[m.es] || 12;
    (EYES[m.eyes] || []).forEach(([x, y], j) => {
      // eyebrows tell the mood: angry inner ends down, sad inner ends up, scared raised
      const by = y - r - (md === "scared" ? 12 : 6), ang = md === "scared" ? 0 : Math.sign(120 - x) * (md === "angry" ? 22 : -22);
      const brow = ["angry", "sad", "scared"].includes(md) ? `<path d="M${x - r} ${by}H${x + r}" stroke="${INK}" stroke-width="4" stroke-linecap="round" transform="rotate(${ang} ${x} ${by})"/>` : "";
      const lid = md === "sleepy" ? `<path d="M${x - r - 1} ${y + 1}A${r + 1} ${r + 1} 0 0 1 ${x + r + 1} ${y + 1}Z" fill="${c}" stroke="${INK}" stroke-width="3"/>` : "";
      eyes += `<g class="eye" style="animation-delay:${(j * .9 + Math.random() * 2).toFixed(1)}s"><circle cx="${x}" cy="${y}" r="${r}" fill="#fff" stroke="${INK}" stroke-width="3"/><circle cx="${x}" cy="${y + r * .15}" r="${md === "scared" ? r * .3 : r * .5}" fill="${INK}"/><circle cx="${x + r * .22}" cy="${y - r * .12}" r="${r * .18}" fill="#fff"/>${lid}</g>${brow}`;
    });
    if (m.pat === "spots") pat = [[82, 70, 9], [158, 76, 7], [68, 150, 11], [172, 150, 8], [100, 192, 9], [146, 194, 7], [60, 110, 6], [182, 112, 6]].map(([x, y, s]) => `<circle cx="${x}" cy="${y}" r="${s}" fill="${pc}"/>`).join("");
    if (m.pat === "stripes") pat = [60, 74, 178, 192, 206].map(y => `<path d="M30 ${y}Q120 ${y + 12} 210 ${y}" fill="none" stroke="${pc}" stroke-width="7"/>`).join("");
    if (m.pat === "stars") pat = [[80, 72], [160, 78], [66, 150], [174, 150], [104, 194], [146, 192]].map(([x, y]) => `<text x="${x}" y="${y}" font-size="22" text-anchor="middle" dominant-baseline="central" fill="${pc}">★</text>`).join("");
    const b = BADGE[md], ey = m.eyes ? EYES[m.eyes].reduce((a, p) => a + p[1], 0) / m.eyes : 100;
    const hat = m.hat === "glasses" ? em("👓", 120, ey + 2, 64) : m.hat ? em(LABO.HAT[m.hat].e, 120, 40, 54) : "";
    const hold = m.hold ? em(LABO.HOLD[m.hold].e, m.side === "L" ? 22 : 218, 92, 40) : "";
    return `<svg class="mon" viewBox="${box}" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="monster"><defs><clipPath id="${id}"><path d="${sh}"/></clipPath></defs>`
      + `<g class="lg">${legs}</g><g class="am">${arms}</g><g class="er">${ears}</g>`
      + `<g class="bd"><path d="${sh}" fill="${c}" stroke="${INK}" stroke-width="4"/><g clip-path="url(#${id})">${pat}</g></g>`
      + `<g class="ch"><ellipse cx="82" cy="142" rx="10" ry="7" fill="#FF8A80" opacity=".5"/><ellipse cx="158" cy="142" rx="10" ry="7" fill="#FF8A80" opacity=".5"/></g>`
      + `<g class="ey">${eyes}</g><g class="mo">${MOUTH[md || "none"]}</g>${b ? em(...b) : ""}<g class="ht">${hat}</g><g class="ho">${hold}</g></svg>`;
  }

  const MAX = {eyes: 5, arms: 4, legs: 5, ears: 4}, SZK = {eyes: "es", ears: "rs"};
  // the blank: a grey potato with two arms and two legs, no eyes yet
  const BLANK = {sh: 0, col: null, pat: null, eyes: 0, es: "mid", mood: null, arms: 2, legs: 2, ears: 0, rs: "mid", hat: null, hold: null, side: "R"};
  const ORDER = ["col", "pat", "eyes", "mood", "arms", "legs", "ears", "hat", "hold"];
  const T = (k, x) => Object.assign({k, r: rnd(6)}, x);
  const one = a => a[rnd(a.length)];
  const sat = (t, m) => t.n != null ? m[t.k] === t.n && (!t.s || m[SZK[t.k]] === t.s) : t.k === "hold" ? m.hold === t.v && (!t.side || m.side === t.side) : m[t.k] === t.v;
  const put = (m, t) => { if (t.n != null) { m[t.k] = t.n; if (t.s) m[SZK[t.k]] = t.s; } else m[t.k] = t.v; if (t.side) m.side = t.side; return m; };
  // the word kept in the feedback: "3 big eyes", "no arms", "banana left", "purple"
  const key = t => t.n != null ? `${t.n || "no"} ${t.s ? t.s + " " : ""}${t.k}` : t.side ? `${t.v} ${t.side === "L" ? "left" : "right"}` : t.v;
  // level 1: three orders, 2: three traits, 3: five, 4: six; heard: only what Luxembourgish can say aloud (for the 4-year-old)
  function traits(lvl, n, heard){
    const cols = lvl === 1 ? LABO.COLS.slice(0, 4) : LABO.COLS;
    const moods = Object.keys(LABO.MOOD).slice(0, heard || lvl === 1 ? 3 : lvl === 2 ? 4 : 5), hats = Object.keys(LABO.HAT).slice(0, heard ? 3 : 5);
    const mk = {col: () => T("col", {v: one(cols)}), mood: () => T("mood", {v: one(moods)}), hat: () => T("hat", {v: one(hats)}),
      hold: side => T("hold", {v: one(Object.keys(LABO.HOLD)), side}), pat: () => T("pat", {v: one(Object.keys(LABO.PAT))})};
    const cnt = (k, from, s) => T(k, {n: one(from), s});
    let out;
    if (heard) out = [mk.col(), ...pick(["mood", "hat", "hold"], Math.min(3, n - 1)).map(k => mk[k]())];
    else if (lvl === 1) out = [mk.col(), ...pick(["eyes", "mood", "hat"], 2).map(k => k === "eyes" ? cnt("eyes", [1, 2, 3]) : mk[k]())];
    else if (lvl === 2) { const k = one(["eyes", "arms", "legs"]); out = [mk.col(), cnt(k, {eyes: [1, 3, 4, 5], arms: [1, 3, 4], legs: [1, 3, 4, 5]}[k]), mk[one(["mood", "hat", "hold"])]()]; }
    else {
      // 3 and 4: a colour, a "no", a size, most of the time left or right, then more
      const side = rnd(10) < 7; out = [mk.col()];
      if (side) out.push(mk.hold(one(["L", "R"])));
      out.push(T(side ? "legs" : one(["arms", "legs"]), {n: 0}));
      const big = one(["eyes", "ears"]); out.push(cnt(big, big === "eyes" ? [1, 2, 3, 4, 5] : [1, 2, 3, 4], one(["big", "small"])));
      for (const k of shuffle(["mood", "hat", "pat", "eyes", "ears", "arms", "legs"])) {
        if (out.length >= n) break;
        if (out.some(t => t.k === k)) continue;
        out.push(k === "eyes" ? cnt(k, [1, 3, 4, 5]) : k === "ears" ? cnt(k, [1, 2, 3]) : k === "arms" ? cnt(k, side ? [3, 4] : [1, 3, 4]) : k === "legs" ? cnt(k, [1, 3, 4, 5]) : mk[k]());
      }
    }
    return out.sort((x, y) => ORDER.indexOf(x.k) - ORDER.indexOf(y.k));
  }
  // the same trait, a little different (spot the difference)
  function vary(t){
    const u = {...t}, other = (a, v) => one(a.filter(x => x !== v));
    if (t.k === "col") u.v = other(LABO.COLS, t.v);
    else if (t.n != null) { if (t.s && rnd(2)) u.s = t.s === "big" ? "small" : "big"; else u.n = !t.n ? 1 + rnd(2) : t.n === MAX[t.k] || rnd(2) ? t.n - 1 : t.n + 1; }
    else if (t.side) u.side = t.side === "L" ? "R" : "L";
    else u.v = other(Object.keys({mood: LABO.MOOD, hat: LABO.HAT, hold: LABO.HOLD, pat: LABO.PAT}[t.k]), t.v);
    return u;
  }
  // a monster told back: the free one, or one from the zoo
  function describe(z){
    const out = [];
    if (z.col) out.push(T("col", {v: z.col}));
    if (z.pat) out.push(T("pat", {v: z.pat}));
    out.push(T("eyes", {n: z.eyes, s: z.eyes && z.es !== "mid" ? z.es : null}));
    if (z.mood) out.push(T("mood", {v: z.mood}));
    if (z.arms !== 2) out.push(T("arms", {n: z.arms}));
    if (z.legs !== 2) out.push(T("legs", {n: z.legs}));
    if (z.ears) out.push(T("ears", {n: z.ears, s: z.rs !== "mid" ? z.rs : null}));
    if (z.hat) out.push(T("hat", {v: z.hat}));
    if (z.hold) out.push(T("hold", {v: z.hold, side: z.arms >= 2 ? z.side : null}));
    return out;
  }
  return {svg, SHAPES: SH.length, MAX, SZK, BLANK, sat, put, key, traits, vary, describe, one};
})());

/* Les bruitages du labo, en Web Audio : un zap, un boing, un rot, un fou rire, un ronflement. Rien en mode test. */
LABO.snd = (() => {
  let ac, noise;
  return function (kind){
    if (TEST) return;
    try {
      ac = ac || new (window.AudioContext || window.webkitAudioContext)();
      const t = ac.currentTime;
      const o = (type, f0, f1, d, v = .2, at = 0, trem = 0) => {
        const s = ac.createOscillator(), g = ac.createGain();
        s.type = type; s.frequency.setValueAtTime(f0, t + at); s.frequency.exponentialRampToValueAtTime(f1, t + at + d);
        g.gain.setValueAtTime(v, t + at); g.gain.exponentialRampToValueAtTime(.001, t + at + d);
        if (trem) { const l = ac.createOscillator(), lg = ac.createGain(); l.frequency.value = trem; lg.gain.value = v * .8; l.connect(lg); lg.connect(g.gain); l.start(t + at); l.stop(t + at + d); }
        s.connect(g); g.connect(ac.destination); s.start(t + at); s.stop(t + at + d + .02);
      };
      const hiss = (d, v = .25, at = 0) => {
        if (!noise) { noise = ac.createBuffer(1, ac.sampleRate / 2, ac.sampleRate); const c = noise.getChannelData(0); for (let i = 0; i < c.length; i++) c[i] = Math.random() * 2 - 1; }
        const s = ac.createBufferSource(), g = ac.createGain(), f = ac.createBiquadFilter();
        s.buffer = noise; f.type = "lowpass"; f.frequency.value = 1400; g.gain.setValueAtTime(v, t + at); g.gain.exponentialRampToValueAtTime(.001, t + at + d);
        s.connect(f); f.connect(g); g.connect(ac.destination); s.start(t + at); s.stop(t + at + d);
      };
      ({
        splat: () => { hiss(.18, .35); o("triangle", 320, 70, .18, .25); },
        pop: () => o("square", 500, 1300, .07, .12),
        boing: () => { o("sine", 140, 600, .16, .3); o("sine", 600, 180, .3, .25, .16, 18); },
        burp: () => o("sawtooth", 105, 55, .6, .22, 0, 28),
        zap: () => { o("sawtooth", 1900, 70, .55, .14); hiss(.4, .2); o("square", 900, 2400, .25, .06, .3); },
        giggle: () => [0, 1, 2, 3, 4].forEach(i => o("sine", 900 + i % 2 * 260, 1100 + i % 2 * 200, .08, .15, i * .09)),
        honk: () => o("square", 330, 260, .3, .15, 0, 12),
        fizzle: () => { hiss(.5, .2); o("sine", 700, 120, .5, .15); },
        sad: () => o("triangle", 520, 220, .7, .2, 0, 6),
        growl: () => o("sawtooth", 80, 60, .7, .25, 0, 32),
        squeak: () => { o("sine", 1200, 2200, .12, .2); o("sine", 1400, 2600, .12, .2, .16); },
        snore: () => { o("sawtooth", 60, 90, .7, .15, 0, 20); o("sine", 900, 1500, .35, .12, .75); },
        sneeze: () => { o("sine", 500, 900, .25, .12); hiss(.3, .45, .28); },
        slide: () => o("sine", 300, 1300, .35, .18),
        tada: () => [523, 659, 784, 1047].forEach((f, i) => o("triangle", f, f * 1.01, .14, .18, i * .1))
      })[kind]();
    } catch (e) {}
  };
})();
