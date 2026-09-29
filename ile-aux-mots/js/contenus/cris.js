/* L'Île aux Mots : contenu du jeu « Qui fait ce bruit ? » (cris d'animaux en anglais, allemand, luxembourgeois, chinois).
   lex: the word in THEMES.animals (names, and the lod.lu recording in Luxembourgish); n: names of animals missing from the lexicon.
   nz: its cartoon noise made with Web Audio, [wave, from Hz, to Hz, seconds, wobble Hz, wobble depth, volume, pause after (negative overlaps)].
   s: the sound in each language; v: the verb ("the dog barks"); zk: the name a Chinese child uses (小狗); mv: its dance.
   Luxembourgish sounds are checked, never guessed: the imperative of the lod.lu verb, as German does (miauen → miau,
   muen → mu, mäen → mä, quaken → quak, grunzen → grunz, piipsen → piips, jiipsen → jiips, summen → summ, wiheren → wiher, zischen → zisch),
   or lb.wikipedia.org (Wauwau, Kikeriki, Uhu). null: no checked sound, the animal stays out of the game in that language.
   Luxembourgish verbs from lod.lu: billen, miauen, muen, grunzen, quaken, mäen, gackeren, kréinen, wiheren, piipsen, jiipsen, summen, brëllen, zischen.
   Not on lod.lu nor lb.wikipedia: the hen's "gack gack" (lb.wikipedia only gives the verb gackeren). */
const CRIS_DATA = {animals: [
  {k:"dog", nz:[["square",420,200,.12,0,0,.2,.09],["square",460,210,.14]], e:"🐶", lvl:1, lex:"dog", zk:"小狗", mv:"wag",
    s:{en:"woof woof", de:"wau wau", lb:"wau wau", zh:"汪汪"}, v:{en:"barks", de:"bellt", lb:"billt"}},
  {k:"cat", nz:[["triangle",480,820,.22,0,0,.25],["triangle",820,430,.38,6,20,.25]], e:"🐱", lvl:1, lex:"cat", zk:"小猫", mv:"spin",
    s:{en:"meow", de:"miau", lb:"miau", zh:"喵喵"}, v:{en:"meows", de:"miaut", lb:"miaut"}},
  {k:"cow", nz:[["sawtooth",150,115,1,5,4,.2]], e:"🐮", lvl:1, lex:"cow", zk:"奶牛", mv:"stomp",
    s:{en:"moo", de:"muh", lb:"mu", zh:"哞哞"}, v:{en:"moos", de:"muht", lb:"mut"}},
  {k:"pig", nz:[["sawtooth",210,140,.14,40,30,.22,.06],["sawtooth",230,150,.16,40,30,.22]], e:"🐷", lvl:1, lex:"pig", zk:"小猪", mv:"hop",
    s:{en:"oink oink", de:"grunz grunz", lb:"grunz grunz", zh:"哼哼"}, v:{en:"grunts", de:"grunzt", lb:"grunzt"}},
  {k:"duck", nz:[["square",620,480,.13,0,0,.15,.07],["square",640,470,.15,0,0,.15]], e:"🦆", lvl:1, lex:"duck", zk:"小鸭子", mv:"wag",
    s:{en:"quack quack", de:"quak quak", lb:"quak quak", zh:"嘎嘎"}, v:{en:"quacks", de:"quakt", lb:"quaakt"}},
  {k:"sheep", nz:[["sawtooth",380,340,.7,9,30,.18]], e:"🐑", lvl:1, lex:"sheep", zk:"小羊", mv:"hop",
    s:{en:"baa", de:"mäh", lb:"mä", zh:"咩咩"}, v:{en:"bleats", de:"blökt", lb:"mät"}},
  {k:"hen", nz:[["square",700,950,.06,0,0,.14,.06],["square",720,980,.06,0,0,.14,.06],["square",650,1100,.2,0,0,.14]], e:"🐔", lvl:1, lex:"chicken", zk:"母鸡", mv:"hop",
    s:{en:"cluck cluck", de:"gack gack", lb:"gack gack", zh:"咯咯哒"}, v:{en:"clucks", de:"gackert", lb:"gackert"}},
  {k:"rooster", nz:[["triangle",520,700,.14,0,0,.25,.02],["triangle",700,620,.14,0,0,.25,.02],["triangle",660,880,.16,0,0,.25,.02],["triangle",900,640,.55,7,18,.25]], e:"🐓", lvl:1, n:{en:"rooster", de:"der Hahn", lb:"den Hunn", zh:"公鸡"}, zk:"大公鸡", mv:"puff",
    s:{en:"cock-a-doodle-doo", de:"kikeriki", lb:"kikeriki", zh:"喔喔喔"}, v:{en:"crows", de:"kräht", lb:"kréint"}},
  {k:"horse", nz:[["triangle",1100,480,.85,12,70,.22]], e:"🐴", lvl:1, lex:"horse", zk:"小马", mv:"gallop",
    s:{en:"neigh", de:"wieher", lb:"wiher", zh:"咴咴"}, v:{en:"neighs", de:"wiehert", lb:"wihert"}},
  {k:"frog", nz:[["square",150,110,.16,28,35,.25,.07],["square",160,115,.2,28,35,.25]], e:"🐸", lvl:1, lex:"frog", zk:"小青蛙", mv:"hop",
    s:{en:"ribbit", de:"quak quak", lb:"quak quak", zh:"呱呱"}, v:{en:"croaks", de:"quakt", lb:"quaakt"}},
  {k:"bird", nz:[["sine",2200,3300,.07,0,0,.18,.05],["sine",2400,3400,.07,0,0,.18,.05],["sine",2600,3600,.09,0,0,.18]], e:"🐦", lvl:1, lex:"bird", zk:"小鸟", mv:"fly",
    s:{en:"tweet tweet", de:"tschilp tschilp", lb:"piips piips", zh:"叽叽喳喳"}, v:{en:"tweets", de:"zwitschert", lb:"piipst"}},
  {k:"mouse", nz:[["sine",3000,3600,.06,0,0,.14,.07],["sine",3100,3700,.06,0,0,.14]], e:"🐭", lvl:1, lex:"mouse", zk:"小老鼠", mv:"zoom",
    s:{en:"squeak squeak", de:"piep piep", lb:"piips piips", zh:"吱吱"}, v:{en:"squeaks", de:"piepst", lb:"piipst"}},
  {k:"chick", nz:[["sine",2600,3100,.08,0,0,.15,.06],["sine",2700,3200,.08,0,0,.15]], e:"🐤", lvl:2, n:{en:"chick", de:"das Küken", lb:"d'Jippelchen", zh:"小鸡"}, zk:"小鸡", mv:"hop",
    s:{en:"cheep cheep", de:"piep piep", lb:"jiips jiips", zh:"叽叽"}, v:{en:"cheeps", de:"piepst", lb:"jiipst"}},
  {k:"lion", nz:[["sawtooth",140,70,1,22,18,.3,-1],["noise",300,300,.8,0,0,.12]], e:"🦁", lvl:2, lex:"lion", zk:"狮子", mv:"shake",
    s:{en:"roar", de:"grrr", lb:"grrr", zh:"嗷呜"}, v:{en:"roars", de:"brüllt", lb:"brëllt"}},
  {k:"owl", nz:[["sine",430,380,.3,0,0,.3,.12],["sine",430,360,.5,0,0,.3]], e:"🦉", lvl:2, lex:"owl", zk:"猫头鹰", mv:"spin",
    s:{en:"hoot hoot", de:"schuhu", lb:"uhu", zh:"咕咕"}, v:{en:"hoots"}},
  {k:"bee", nz:[["sawtooth",210,230,1.1,28,12,.12]], e:"🐝", lvl:2, n:{en:"bee", de:"die Biene", lb:"d'Bei", zh:"蜜蜂"}, zk:"小蜜蜂", mv:"fly",
    s:{en:"buzz", de:"summ summ", lb:"summ summ", zh:"嗡嗡"}, v:{en:"buzzes", de:"summt", lb:"summt"}},
  {k:"snake", nz:[["noise",5000,5000,1,0,0,.15]], e:"🐍", lvl:2, lex:"snake", zk:"小蛇", mv:"slither",
    s:{en:"hiss", de:"zisch", lb:"zisch", zh:"嘶嘶"}, v:{en:"hisses", de:"zischt", lb:"zischt"}},
  {k:"wolf", nz:[["sine",330,660,.6,0,0,.25],["sine",660,420,1,5,12,.25]], e:"🐺", lvl:2, n:{en:"wolf", de:"der Wolf", lb:"de Wollef", zh:"狼"}, zk:"大灰狼", mv:"puff",
    s:{en:"awoooo", de:"auuuu", lb:null, zh:"嗷呜"}, v:{en:"howls", de:"heult"}},
  {k:"bear", nz:[["sawtooth",95,70,.9,18,10,.3]], e:"🐻", lvl:2, lex:"bear", zk:"熊", mv:"stomp",
    s:{en:"grrr", de:"brumm brumm", lb:null, zh:null}, v:{en:"growls", de:"brummt"}},
  {k:"goat", nz:[["sawtooth",520,480,.55,15,45,.18]], e:"🐐", lvl:3, n:{en:"goat", de:"die Ziege", lb:"d'Geess", zh:"山羊"}, zk:"山羊", mv:"hop",
    s:{en:"maa", de:"meck meck", lb:null, zh:"咩咩"}, v:{en:"bleats", de:"meckert"}},
  {k:"monkey", nz:[["sine",500,1100,.12,0,0,.22,.04],["sine",520,1150,.12,0,0,.22,.04],["sine",600,1300,.2,0,0,.22]], e:"🐵", lvl:3, lex:"monkey", zk:"小猴子", mv:"shake",
    s:{en:"ooh ooh aah aah", de:"uh uh ah ah", lb:null, zh:"吱吱"}},
  {k:"cricket", nz:[["sine",4200,4300,.05,60,300,.1,.04],["sine",4200,4300,.05,60,300,.1,.04],["sine",4200,4300,.05,60,300,.1]], e:"🦗", lvl:3, n:{en:"cricket", de:"die Grille", lb:null, zh:"蟋蟀"}, zk:"小蟋蟀", mv:"hop",
    s:{en:"chirp chirp", de:"zirp zirp", lb:null, zh:"唧唧"}, v:{en:"chirps", de:"zirpt"}},
  {k:"dove", nz:[["sine",420,360,.3,6,10,.22,.08],["sine",400,340,.4,6,10,.22]], e:"🕊️", lvl:3, n:{en:"dove", de:"die Taube", lb:"d'Dauf", zh:"鸽子"}, zk:"小鸽子", mv:"fly",
    s:{en:"coo coo", de:"gurr gurr", lb:null, zh:"咕咕"}, v:{en:"coos", de:"gurrt"}}
], fx: { // funny noises of the game itself
  pouet: [["sawtooth",110,80,.45,28,40,.25]], burp: [["sawtooth",95,62,.5,14,14,.3]], hihi: [["triangle",900,1300,.08,0,0,.15,.05],["triangle",950,1400,.08,0,0,.15]],
  sneeze: [["noise",1800,1800,.12,0,0,.15,.03],["noise",3500,3500,.25,0,0,.3]]
}};

/* What the child hears and reads. n: the animal with its article (the dog, der Hund, den Hond; 小狗 in Chinese), s: a sound, v: a verb.
   German and Luxembourgish only need the nominative here (subject, or after "sein"/"sinn"). */
const CRIS_TXT = {
  en: {who: s => `Who says ${s}?`, what: n => `What does ${n} say?`, says: (n, s) => `${n} says ${s}!`, silly: (n, s) => `Does ${n} say ${s}?`,
    thats: n => `That's ${n}!`, verbQ: v => `Who ${v}?`, verbA: (n, v) => `${n} ${v}!`, langQ: (n, s) => `In which language does ${n} say ${s}?`,
    yes: "Yes", no: "No", band: "Listen to the band!", bandAsk: "Who is singing? Tap them in order!", bandDone: "What a concert!", again: "Again",
    praise: ["Great job!", "Well done!", "Yes!", "Super!", "Amazing!", "You got it!"]},
  de: {who: s => `Wer macht ${s}?`, what: n => `Wie macht ${n}?`, says: (n, s) => `${n} macht ${s}!`, silly: (n, s) => `Macht ${n} ${s}?`,
    thats: n => `Das ist ${n}!`, verbQ: v => `Wer ${v}?`, verbA: (n, v) => `${n} ${v}!`, langQ: (n, s) => `In welcher Sprache macht ${n} ${s}?`,
    yes: "Ja", no: "Nein", band: "Hör mal, die Tierband!", bandAsk: "Wer singt? Tippe sie der Reihe nach an!", bandDone: "Was für ein Konzert!", again: "Nochmal",
    praise: ["Super!", "Toll!", "Richtig!", "Prima!", "Sehr gut!"]},
  // lod.lu: wien (who), soen → seet, lauschteren → lauschter! (transitive: de Radio lauschteren), sangen → séngt, Sprooch (f), Concert (m),
  // Band (f, the music band), tippen → tipp! (to touch), der Rei no (one after the other)
  lb: {who: s => `Wien seet ${s}?`, what: n => `Wat seet ${n}?`, says: (n, s) => `${n} seet ${s}!`, silly: (n, s) => `Seet ${n} ${s}?`,
    thats: n => `Dat ass ${n}!`, verbQ: v => `Wien ${v}?`, verbA: (n, v) => `${n} ${v}!`, langQ: (n, s) => `A wéi enger Sprooch seet ${n} ${s}?`,
    yes: "Jo", no: "Neen", band: "Lauschter d'Band!", bandAsk: "Wien séngt? Tipp se der Rei no!", bandDone: "Wat e Concert!", again: "Nach eng Kéier",
    praise: ["Super!", "Bravo!", "Richteg!", "Flott!", "Ganz gutt!"]},
  zh: {who: s => `谁在${s}叫？`, what: n => `${n}怎么叫？`, says: (n, s) => `${n}${s}叫！`, silly: (n, s) => `${n}会${s}叫吗？`,
    thats: n => `这是${n}！`, verbQ: null, verbA: null, langQ: (n, s) => `哪种语言里，${n}这样叫：“${s}”？`,
    yes: "对", no: "不对", band: "听，动物乐队！", bandAsk: "谁在唱歌？按顺序点一点！", bandDone: "多棒的音乐会！", again: "再听一次",
    praise: ["太棒了！", "真棒！", "对了！", "好厉害！"]}
};
