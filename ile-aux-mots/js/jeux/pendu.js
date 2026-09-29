/* L'Île aux Mots : jeu « Le bonhomme de neige » (ticket 2026-09-28_jeu-pendu), un pendu gentil.
   A word of the lexicon is guessed letter by letter, the picture as a clue. Each wrong letter melts the snowman a little
   (hat askew, an arm drops, sunglasses, a puddle that grows), but he always smiles, and snow rebuilds him after every word.
   1: 3 letters, 3 letter keys at a time, ghost letters to match, the voice names each letter
   2: 4 to 5 letters, a keyboard of the useful letters and 3 more
   3: 6 letters and more, the whole alphabet of the language (ä ö ü ß in German, ä é ë in Luxembourgish), the picture only
   4: no picture, a riddle to read (and to hear in English and German), the whole alphabet
   Chinese: characters instead of letters. 1: two characters, 3 keys at a time, ghost characters, pinyin · 2: the characters and
   look-alikes, pinyin shown · 3: three-character words, 12 keys with look-alikes, no pinyin · 4: a riddle, a two-character word
   with one character given, choose the missing one among 6.
   English, German, Luxembourgish or Chinese, never French. Luxembourgish is heard only through the lod.lu recording of the word:
   its instructions and riddles are written.
   Luxembourgish checked on lod.lu (API lod.lu/api/lb): Hausdéier (n.), fänken, Maus/Mais, Bauerenhaff, Mëllech, Ouer/Oueren,
   Muert/Muerten, kloteren, Bam/Beem, Rüssel (m.), Kinnek, Déier/Déieren, Vull, Flillek/Flilleken, faarweg, virdrun, Raup, Mier,
   schlau, Floss, Zant/Zänn, Puppelchen (m.), Täsch, Bauch, Fruucht, Af/Afen, séiss, bannen, Bäcker, baken (baakt), Gebuertsdag,
   sauer, Kroun, Kapp, Fieder/Fiederen, leeën, Ee/Eeër, Reen, Himmel, Dier, Fënster/Fënsteren, Daach (m.), Rad/Rieder, Strooss,
   Hand/Hänn, Kand/Kanner, Wanter, Mound, Wollek/Wolleken, Schinn/Schinnen, Pull (m., puddle), Buschtaf/Buschtawen, Wuert,
   schneien (et schneit), rëm, and the present tense of wunnen, maachen, ginn, sprangen, iessen (du ëss), spillen, fléien,
   schwammen, droen, ruffen, schlofen, richen, héieren, botzen, reenen, bleiwen (du bleifs), opmaachen (du méchs ... op), sëtzen,
   gesinn (du gesäis), schreiwen (du schreifs), molen (du mools), schneiden, bauen, schéngen, liichten, fueren, hëllefen, schaffen,
   léieren, doen (du dees), halen, drénken (du drénks), oppassen (pass op). Eifel rule applied by hand.
   To be proofread by a Luxembourgish speaker: the riddles of level 4 (DEF, third text), "Fann d'Wuert, Buschtaf fir Buschtaf",
   "Ech si rëm do!", "Oh, et gëtt waarm!", "Ech sinn e Pull!". */
(() => {
const ID = "pendu";
const LV = {
  1: {len: [3, 3], zh: [2, 2], wl: 1, keys: "trio", pic: true, free: 1, zfree: 1, rounds: 6},
  2: {len: [4, 5], zh: [2, 3], wl: 2, keys: "few", pic: true, free: 2, zfree: 1, rounds: 6},
  3: {len: [6, 11], zh: [3, 3], wl: 3, keys: "all", pic: true, free: 3, zfree: 2, rounds: 5},
  4: {def: true, keys: "all", free: 3, zfree: 0, rounds: 6, zrounds: 8}
};
const ALPHA = {en: "abcdefghijklmnopqrstuvwxyz", de: "abcdefghijklmnopqrstuvwxyzäöüß", lb: "abcdefghijklmnopqrstuvwxyzäéë"};
const HAN = /^[一-鿿]+$/;
const MELT = 6, NO_AUDIO = ["GIRAFF1"]; // lexicon words whose lod.lu recording is not in audio/lb
const KEYC = ["#FFD0C6", "#D4ECFF", "#BDEBDD", "#FFE3A3", "#F4E1FF", "#E6F5C2"];
const TX = {
  en: {title: "Snowman", sub: "Guess the word letter by letter", go: "Find the letters!", guess: "What is it? Guess the word!", again: "Again", hear: "Listen",
    who: "Who am I?", what: "What am I?", pl: "What are we?", oops: ["Brr... it's getting warm!", "Drip, drop!", "Not in this word!", "Oops!"],
    cheer: ["You found it!", "Brilliant!", "Hooray!", "Super!"], back: "I'm back!", puddle: "I'm a puddle! Hee hee!", hihi: "Hee hee!"},
  de: {title: "Schneemann", sub: "Errate das Wort Buchstabe für Buchstabe", go: "Finde die Buchstaben!", guess: "Was ist das? Errate das Wort!", again: "Nochmal", hear: "Hör zu",
    who: "Wer bin ich?", what: "Was bin ich?", pl: "Was sind wir?", oops: ["Oje, es wird warm!", "Tropf, tropf!", "Nicht in diesem Wort!", "Hoppla!"],
    cheer: ["Gefunden!", "Super!", "Klasse!", "Spitze!"], back: "Ich bin wieder da!", puddle: "Ich bin eine Pfütze! Hihi!", hihi: "Hihi!"},
  lb: {title: "Schnéimännchen", sub: "Fann d'Wuert, Buschtaf fir Buschtaf", go: "Fann d'Buschtawe vum Wuert!", guess: "Wat ass dat? Fann d'Wuert!", again: "Nach eng Kéier", hear: "Lauschter",
    who: "Wien sinn ech?", what: "Wat sinn ech?", pl: "Wat si mir?", oops: ["Oh, et gëtt waarm!", "Hoppla!", "Oh nee!"],
    cheer: ["Super!", "Bravo!", "Richteg!", "Wonnerbar!"], back: "Ech si rëm do!", puddle: "Ech sinn e Pull! Hihi!", hihi: "Hihi!"},
  zh: {title: "雪人猜词", sub: "一个字一个字猜出词语", go: "找出这个词的字！", guess: "这是什么？猜一猜！", again: "再听一次", hear: "听一听",
    who: "我是谁？", what: "我是什么？", pl: "我们是什么？", miss: "选出缺少的字！", oops: ["哎呀，好热！", "滴答滴答！", "这个词里没有哦！", "哎呀！"],
    cheer: ["猜对了！", "真厉害！", "太棒了！", "好极了！"], back: "我又回来啦！", puddle: "我变成水坑啦！嘻嘻！", hihi: "嘻嘻！"}
};
// level 4 riddles, the word speaking about itself: [lexicon word (en), question, en, de, lb, zh]
// the Chinese riddle never contains a character of its word (one of them is the one to find)
const DEF = [
  ["cat", "who", "I am a small pet. I say meow and I love to chase mice.", "Ich bin ein kleines Haustier. Ich mache miau und fange gern Mäuse.", "Ech sinn e klengt Hausdéier. Ech maache miau an ech fänke gär Mais.", "我是一种小宠物，会喵喵叫，最喜欢抓老鼠。"],
  ["dog", "who", "I am a pet. I say woof and I love to play with a ball.", "Ich bin ein Haustier. Ich mache wau wau und spiele gern mit dem Ball.", "Ech sinn en Hausdéier. Ech maache wau wau an ech spille gär mam Ball.", "我是一种宠物，会汪汪叫，最喜欢玩球。"],
  ["cow", "who", "I live on a farm. I say moo and I give you milk.", "Ich wohne auf dem Bauernhof. Ich mache muh und gebe dir Milch.", "Ech wunnen um Bauerenhaff. Ech maache muh an ech ginn der Mëllech.", "我住在农场，身上有黑白花纹，会哞哞叫。"],
  ["chicken", "who", "I live on a farm. I have feathers and I lay eggs.", "Ich wohne auf dem Bauernhof. Ich habe Federn und lege Eier.", "Ech wunnen um Bauerenhaff. Ech hu Fiederen an ech leeën Eeër.", "我住在农场，身上有羽毛，会下蛋。"],
  ["frog", "who", "I am green. I jump and I say ribbit.", "Ich bin grün. Ich springe und mache quak.", "Ech si gréng. Ech sprangen an ech maache quak.", "我是绿色的，会跳，会呱呱叫。"],
  ["rabbit", "who", "I have long ears and I love carrots.", "Ich habe lange Ohren und fresse gern Karotten.", "Ech hu laang Oueren an ech iesse gär Muerten.", "我有长长的耳朵，最爱吃胡萝卜。"],
  ["monkey", "who", "I climb trees and I love bananas.", "Ich klettere auf Bäume und esse gern Bananen.", "Ech kloteren op d'Beem an ech iesse gär Bananen.", "我会爬树，最爱吃香蕉。"],
  ["elephant", "who", "I am very big and grey. I have big ears and a long trunk.", "Ich bin sehr groß und grau. Ich habe große Ohren und einen langen Rüssel.", "Ech si ganz grouss a gro. Ech hu grouss Oueren an e laange Rüssel.", "我很重很重，身体是灰色的，耳朵像扇子，鼻子长长的。"],
  ["lion", "who", "I am the king of the animals. I have a big mane and I roar.", "Ich bin der König der Tiere. Ich habe eine große Mähne und brülle laut.", "Ech sinn de Kinnek vun den Déieren.", "我是百兽之王，会大声吼叫。"],
  ["penguin", "who", "I am a black and white bird. I can't fly, but I swim very well.", "Ich bin ein schwarz-weißer Vogel. Ich kann nicht fliegen, aber sehr gut schwimmen.", "Ech sinn e schwaarzen a wäisse Vull. Ech kann net fléien, mee ech schwamme ganz gutt.", "我是黑白色的鸟，不会飞，但很会游泳。"],
  ["turtle", "who", "I am very slow and I carry my house on my back.", "Ich bin sehr langsam und trage mein Haus auf dem Rücken.", "Ech sinn net séier an ech droe mäin Haus um Réck.", "我走得很慢，把家背在背上。"],
  ["owl", "who", "I am a bird. I sleep in the day and I say hoo hoo at night.", "Ich bin ein Vogel. Ich schlafe am Tag und rufe nachts: Huhu!", "Ech sinn e Vull. Ech schlofen am Dag an ech ruffen an der Nuecht: Hu hu!", "我是一种鸟，白天睡觉，晚上咕咕叫。"],
  ["mouse", "who", "I am very small and grey. I love cheese.", "Ich bin ganz klein und grau. Ich esse gern Käse.", "Ech si ganz kleng a gro. Ech iesse gär Kéis.", "我很小，是灰色的，最爱吃奶酪。"],
  ["butterfly", "who", "I have beautiful colourful wings. Before, I was a caterpillar.", "Ich habe schöne bunte Flügel. Früher war ich eine Raupe.", "Ech hu schéin faarweg Flilleken. Virdru war ech eng Raup.", "我有美丽多彩的翅膀，以前是一条毛毛虫。"],
  ["dolphin", "who", "I live in the sea. I am very clever and I love to jump out of the water.", "Ich wohne im Meer. Ich bin sehr klug und springe gern aus dem Wasser.", "Ech wunnen am Mier. Ech si ganz schlau an ech sprange gär aus dem Waasser.", "我住在水里，很聪明，喜欢跳出水面。"],
  ["crocodile", "who", "I live in the river. I have a big mouth with lots of sharp teeth.", "Ich wohne im Fluss. Ich habe ein großes Maul mit vielen spitzen Zähnen.", "Ech wunnen am Floss an ech hu ganz vill Zänn.", "我住在河里，大嘴巴里有很多尖尖的牙齿。"],
  ["kangaroo", "who", "I jump very far and I carry my baby in a pocket on my tummy.", "Ich springe sehr weit und trage mein Baby in einem Beutel am Bauch.", "Ech sprange ganz wäit an ech droe mäi Puppelchen an enger Täsch um Bauch.", "我来自澳大利亚，跳得很远，还把宝宝带在身上。"],
  ["banana", "what", "I am a long yellow fruit. Monkeys love me.", "Ich bin eine lange gelbe Frucht. Affen essen mich sehr gern.", "Ech sinn eng laang giel Fruucht an d'Afen iesse mech gär.", "我是一种长长的黄色水果，猴子最爱吃我。"],
  ["strawberry", "what", "I am a small red fruit with tiny seeds on the outside.", "Ich bin eine kleine rote Frucht. Meine kleinen Kerne sitzen außen auf der Haut.", "Ech sinn eng kleng rout Fruucht an ech si séiss.", "我是红色的小水果，身上有很多小小的籽。"],
  ["watermelon", "what", "I am a big green fruit. Inside I am red and very juicy.", "Ich bin eine große grüne Frucht. Innen bin ich rot und voller Saft.", "Ech sinn eng grouss gréng Fruucht. Bannen sinn ech rout.", "我是很大的绿色水果，里面是红色的，夏天吃很解渴。"],
  ["bread", "what", "The baker bakes me every morning.", "Der Bäcker backt mich jeden Morgen.", "De Bäcker baakt mech all Moien.", "我是烤出来的，可以做三明治，早餐常常吃我。"],
  ["milk", "what", "I am white and you drink me. Cows give me.", "Ich bin weiß und du trinkst mich. Die Kuh gibt mich.", "Ech si wäiss an du drénks mech. D'Kou gëtt mech.", "我是白色的饮料，喝了长得高。"],
  ["cake", "what", "I am sweet. You eat me on your birthday, with candles on top.", "Ich bin süß. Du isst mich an deinem Geburtstag, mit Kerzen obendrauf.", "Ech si séiss. Du ëss mech op dengem Gebuertsdag.", "我甜甜的，过生日的时候吃，上面插着蜡烛。"],
  ["lemon", "what", "I am a yellow fruit and I am very, very sour.", "Ich bin eine gelbe Frucht und ich bin sehr, sehr sauer.", "Ech sinn eng giel Fruucht an ech si ganz sauer.", "我是黄色的水果，味道很酸很酸。"],
  ["pineapple", "what", "I am a fruit with a crown of green leaves on my head.", "Ich bin eine Frucht mit einer Krone aus grünen Blättern auf dem Kopf.", "Ech sinn eng Fruucht mat enger Kroun um Kapp.", "我是水果，头上戴着一顶绿叶做的王冠。"],
  ["eyes", "pl", "There are two of us in your face. We help you see.", "Wir sind zwei, in deinem Gesicht. Mit uns siehst du.", "Mir sinn zwee an dengem Gesiicht. Mat eis gesäis du.", "我们有两个，在你的脸上，帮你看东西。"],
  ["nose", "what", "I am in the middle of your face. I help you smell.", "Ich bin mitten in deinem Gesicht. Mit mir riechst du.", "Ech sinn an der Mëtt vun dengem Gesiicht. Mat mir richs du.", "我在你脸的中间，帮你闻味道。"],
  ["ear", "what", "You have one on each side of your head. I help you listen.", "Du hast eins an jeder Seite vom Kopf. Mit mir hörst du.", "Ech sinn um Kapp. Mat mir héiers du.", "我长在头的两边，帮你听声音。"],
  ["tooth", "what", "I am small and white and I live in your mouth. Brush me every day!", "Ich bin klein und weiß und wohne in deinem Mund. Putz mich jeden Tag!", "Ech si kleng a wäiss an ech wunnen an dengem Mond. Botz mech all Dag!", "我小小的，白白的，住在你的嘴巴里，每天都要刷我。"],
  ["umbrella", "what", "When it rains, you open me and you stay dry.", "Wenn es regnet, machst du mich auf und bleibst trocken.", "Wann et reent, méchs du mech op an du bleifs dréchen.", "天上掉水点的时候，你把我撑开，就不会淋湿。"],
  ["rainbow", "what", "After a shower, I appear in the sky with many colours.", "Nach einem Schauer stehe ich am Himmel, mit vielen Farben.", "Nom Reen sinn ech um Himmel, mat ville Faarwen.", "下雨以后，我出现在天上，有七种颜色。"],
  ["key", "what", "You need me to open the door.", "Du brauchst mich, um die Tür aufzumachen.", "Du brauchs mech, fir d'Dier opzemaachen.", "你需要我来开门。"],
  ["glasses", "what", "I sit on your nose and help you see better.", "Ich sitze auf deiner Nase und du siehst besser.", "Ech sëtzen op denger Nues an du gesäis besser.", "我坐在你的鼻子上，帮你看得更清楚。"],
  ["pencil", "what", "You write and draw with me.", "Mit mir schreibst und malst du.", "Mat mir schreifs a mools du.", "你用我写字和画画。"],
  ["scissors", "what", "I cut paper. Be careful with your fingers!", "Ich schneide Papier. Pass auf deine Finger auf!", "Ech schneiden de Pabeier. Pass op mat de Fangeren!", "我可以把纸变成两半，用的时候要小心手指。"],
  ["rocket", "what", "I fly very, very fast, all the way to the moon.", "Ich fliege ganz schnell, bis zum Mond.", "Ech fléie ganz séier, bis op de Mound.", "我飞得非常快，能飞到月亮上。"],
  ["snowman", "who", "Children build me in winter. I am white and my nose is a carrot.", "Kinder bauen mich im Winter. Ich bin weiß und meine Nase ist eine Karotte.", "D'Kanner baue mech am Wanter. Ech si wäiss a meng Nues ass eng Muert.", "冬天小朋友把我堆起来，我白白的，鼻子是胡萝卜。"],
  ["sun", "what", "I shine in the sky during the day. I am hot and bright.", "Ich scheine am Tag am Himmel. Ich bin heiß und hell.", "Ech schéngen am Dag um Himmel. Ech si waarm an hell.", "白天我在天上发光，又热又亮。"],
  ["moon", "what", "I shine in the sky at night. Sometimes I am round, sometimes I am thin.", "Ich leuchte nachts am Himmel. Manchmal bin ich rund, manchmal ganz dünn.", "Ech liichten an der Nuecht um Himmel. Heiansdo sinn ech ronn.", "晚上我在天上发光，有时圆，有时弯。"],
  ["train", "what", "I run on rails and I carry lots of people.", "Ich fahre auf Schienen und nehme viele Leute mit.", "Ech fueren op Schinnen.", "我在铁轨上跑得很快，带着很多乘客去远方。"],
  ["plane", "what", "I have two wings and I carry people above the clouds.", "Ich habe zwei Flügel und fliege mit vielen Leuten über den Wolken.", "Ech hunn zwee Flilleken an ech fléien iwwer d'Wolleken.", "我有两个翅膀，载着很多人在云上面旅行。"],
  ["house", "what", "You live in me. I have a door, windows and a roof.", "Du wohnst in mir. Ich habe eine Tür, Fenster und ein Dach.", "Du wunns an mir. Ech hunn eng Dier, Fënsteren an en Daach.", "你住在我里面，我有门、窗户和屋顶。"],
  ["car", "what", "I have four wheels and I drive on the road.", "Ich habe vier Räder und fahre auf der Straße.", "Ech hu véier Rieder an ech fueren op der Strooss.", "我有四个轮子，在马路上跑。"],
  ["doctor", "who", "When you are sick, I help you get better.", "Wenn du krank bist, helfe ich dir, wieder gesund zu werden.", "Wann s du krank bass, hëllefen ech dir.", "你不舒服的时候，我帮你检查身体。"],
  ["teacher", "who", "I work at school and I help you learn.", "Ich arbeite in der Schule und helfe dir beim Lernen.", "Ech schaffen an der Schoul an ech hëllefen dir beim Léieren.", "我在学校工作，帮你学习。"],
  ["hat", "what", "You put me on your head.", "Du setzt mich auf den Kopf.", "Du dees mech op de Kapp.", "你把我戴在头上。"],
  ["gloves", "pl", "When it is cold, we keep your hands warm.", "Wenn es kalt ist, halten wir deine Hände warm.", "Wann et kal ass, hale mir deng Hänn waarm.", "天冷的时候，你戴上我们，十个指头就暖和了。"]
].map(([en, q, e, d, l, z]) => ({en, q, t: {en: e, de: d, lb: l, zh: z}}));

const lang = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const one = a => a[rnd(a.length)];
const bare = (w, L) => L === "de" ? w.de.replace(/^(der|die|das) /, "") : L === "lb" ? w.lb.replace(/^(d'|(den|de|dat|déi) )/, "") : w[L];
const artOf = (w, L) => L === "de" || L === "lb" ? w[L].slice(0, w[L].length - bare(w, L).length) : "";
const up = c => c === "ß" ? c : c.toUpperCase(); // "ß".toUpperCase() is "SS"
const anim = (e, kf, ms, o) => { if (e && !calm()) return e.animate(kf, Object.assign({duration: ms, easing: "ease-in-out"}, o)); };
// pinyin and look-alike characters of the spelling game (js/contenus/epelle.js), when it is loaded
const ep = () => (typeof EPELLE !== "undefined" && EPELLE) || {};
const pinyin = w => { const p = (ep().py || {})[w.zh], s = p ? p.split(" ") : null; return s && s.length === [...w.zh].length ? s : null; };

let lex = null, han = null;
const words = () => lex || (lex = (() => {
  const seen = new Set(), out = [];
  Object.values(THEMES).flatMap(t => t.words).forEach(w => { if (!seen.has(w.en)) { seen.add(w.en); out.push(w); } });
  return out;
})());
const hanChars = () => han || (han = [...new Set(words().filter(w => HAN.test(w.zh)).flatMap(w => [...w.zh]))]);
const text = (w, L) => L === "zh" ? w.zh : bare(w, L);
const spellable = (s, L) => L === "zh" ? HAN.test(s) : !!s && [...s.toLowerCase()].every(c => ALPHA[L].includes(c));
// Luxembourgish is heard only through lod.lu: a word without its recording is left out
const usable = (w, L) => !!w && !!w[L] && (L !== "lb" || (!!w.lod && !NO_AUDIO.includes(w.lod))) && spellable(text(w, L), L);
const size = (w, L) => [...text(w, L)].length;
function choose(L, lvl, n, avoid){
  const lv = LV[lvl];
  let fit;
  if (lv.def) fit = DEF.map(d => ({d, w: words().find(w => w.en === d.en)})).filter(x => usable(x.w, L) && (L !== "zh" || size(x.w, L) === 2));
  else {
    const [lo, hi] = L === "zh" ? lv.zh : lv.len;
    const f = (a, b, wl) => words().filter(w => usable(w, L) && (w.lvl || 1) <= wl && size(w, L) >= a && size(w, L) <= b).map(w => ({w}));
    fit = f(lo, hi, lv.wl);
    if (fit.length < n + 2) fit = f(lo - 1, hi + 1, 3); // few words of that length in this language: a letter shorter or longer
  }
  const out = [], used = new Set(avoid);
  shuffle(fit).forEach(x => { const s = text(x.w, L); if (out.length < n && !used.has(s)) { used.add(s); out.push(x); } });
  return out;
}

/* ---------- sounds: Web Audio, silent in the recette ---------- */
function snd(kind){
  if (TEST) return;
  try {
    ac = ac || new (window.AudioContext || window.webkitAudioContext)();
    const t = ac.currentTime;
    const osc = (type, f0, f1, dur, vol = .16, at = 0) => {
      const o = ac.createOscillator(), g = ac.createGain();
      o.type = type; o.frequency.setValueAtTime(f0, t + at); o.frequency.exponentialRampToValueAtTime(f1, t + at + dur);
      g.gain.setValueAtTime(.0001, t + at); g.gain.exponentialRampToValueAtTime(vol, t + at + .02); g.gain.exponentialRampToValueAtTime(.0001, t + at + dur);
      o.connect(g); g.connect(ac.destination); o.start(t + at); o.stop(t + at + dur + .05);
    };
    ({
      ding: () => { osc("triangle", 880, 890, .12); osc("triangle", 1320, 1330, .2, .14, .09); },
      plip: () => { osc("sine", 1500, 320, .15, .2); osc("sine", 1100, 260, .12, .12, .22); },
      snow: () => [1568, 1319, 1760, 1568, 2093, 2637].forEach((f, k) => osc("triangle", f, f * 1.01, .16, .07, k * .11)),
      giggle: () => { for (let k = 0; k < 5; k++) osc("triangle", k % 2 ? 900 : 1150, k % 2 ? 860 : 1100, .07, .12, k * .08); },
      hop: () => osc("sine", 300, 700, .18, .12)
    })[kind]();
  } catch (e) {}
}

/* ---------- the snowman (original drawing): he melts with --m from 0 to 6, and always smiles ---------- */
const INK = `stroke="#1B2D45" stroke-width="3"`;
const ARM = `fill="none" stroke="#7A4B2A" stroke-width="3.5" stroke-linecap="round"`;
const MAN = `<svg viewBox="0 0 120 150" aria-hidden="true">
<ellipse class="pd-puddle" cx="60" cy="143" rx="34" ry="6" fill="#BFE6FF" stroke="#1B2D45" stroke-width="2"/>
<g class="pd-body">
<g class="pd-arm l"><path d="M41 80 L13 63 M22 68 L17 56 M22 68 L9 71" ${ARM}/></g>
<g class="pd-arm r"><path d="M79 80 L107 63 M98 68 L103 56 M98 68 L111 71" ${ARM}/></g>
<circle cx="60" cy="116" r="27" fill="#fff" ${INK}/><circle cx="60" cy="79" r="20" fill="#fff" ${INK}/>
<circle cx="60" cy="72" r="2.6" fill="#1B2D45"/><circle cx="60" cy="83" r="2.6" fill="#1B2D45"/><circle cx="60" cy="108" r="2.8" fill="#1B2D45"/><circle cx="60" cy="121" r="2.8" fill="#1B2D45"/>
<path d="M66 63 L73 81 L65 82 L61 65 Z" fill="#E53935" stroke="#1B2D45" stroke-width="2" stroke-linejoin="round"/>
<g class="pd-head">
<circle cx="60" cy="44" r="17" fill="#fff" ${INK}/>
<ellipse cx="49" cy="50" rx="3.6" ry="2.4" fill="#FF9EB0"/><ellipse cx="71" cy="50" rx="3.6" ry="2.4" fill="#FF9EB0"/>
<circle cx="54" cy="40" r="2.5" fill="#1B2D45"/><circle cx="66" cy="40" r="2.5" fill="#1B2D45"/>
<g class="pd-shades"><rect x="47.5" y="36" width="11" height="7.5" rx="3" fill="#1B2D45"/><rect x="61.5" y="36" width="11" height="7.5" rx="3" fill="#1B2D45"/><path d="M58 39 H62" stroke="#1B2D45" stroke-width="2"/><path d="M50 38 l3 0" stroke="#fff" stroke-width="1.4" stroke-linecap="round"/></g>
<path d="M52 52 Q60 59 68 52" fill="none" stroke="#1B2D45" stroke-width="2.6" stroke-linecap="round"/>
<path class="pd-nose" d="M60 44 L79 47.5 L60 50 Z" fill="#FF8A1F" stroke="#1B2D45" stroke-width="1.6" stroke-linejoin="round"/>
<g class="pd-hat"><rect x="43" y="25" width="34" height="5.5" rx="2" fill="#1B2D45"/><rect x="48" y="6" width="24" height="20" rx="2" fill="#1B2D45"/><rect x="48" y="19" width="24" height="4" fill="#E53935"/></g>
</g>
<path d="M43 58 Q60 67 77 58 L77 64 Q60 73 43 64 Z" fill="#E53935" stroke="#1B2D45" stroke-width="2" stroke-linejoin="round"/>
</g>
</svg>`;

addStyle(`
.pd-prompt{font-size:clamp(20px,5vw,30px); margin:0}
.pd-prompt.riddle{font-size:clamp(18px,4.4vw,24px); line-height:1.3; background:#fff; border:3px solid var(--ink); border-radius:18px; padding:10px 14px}
.pd-prompt.riddle small{margin-top:6px; font-size:17px; color:var(--sea-deep)}
.pd-scene{position:relative; height:180px; overflow:hidden; border:3px solid var(--ink); border-radius:18px; display:flex; align-items:flex-end; justify-content:center; gap:16px; padding:0 8px;
  background:linear-gradient(#BFE3FF, #EAF6FF 70%, #fff 70%)}
.pd-man{flex:none; width:132px; height:166px; padding:0; position:relative; z-index:2; transform-origin:50% 100%; animation:pd-sway 3.2s ease-in-out infinite}
.pd-man svg{width:100%; height:100%; overflow:visible; display:block}
.pd-pic{flex:none; align-self:center; width:104px; height:104px; margin-bottom:22px; padding:0; display:grid; place-items:center; font-size:60px; line-height:1;
  background:#fff; border:3px solid var(--ink); border-radius:50%; box-shadow:3px 4px 0 var(--ink); position:relative; z-index:2}
.pd-pic .swatch{width:62px}
.pd-pic.mys{font-family:var(--display); font-weight:700; font-size:54px; color:var(--sea-deep); animation:pd-wob 1.8s ease-in-out infinite}
.pd-sun{position:absolute; top:8px; right:10px; font-size:30px; line-height:1; z-index:1; transform-origin:100% 0; transform:scale(calc(.75 + var(--m, 0) * .14)); transition:transform .7s}
.pd-cloud{position:absolute; top:0; right:2px; font-size:46px; line-height:1; opacity:0; z-index:3; pointer-events:none}
.pd-say{position:absolute; left:8px; top:8px; max-width:calc(100% - 70px); z-index:5; background:#fff; border:3px solid var(--ink); border-radius:16px 16px 16px 4px;
  padding:5px 11px; font-family:var(--display); font-weight:600; font-size:18px; line-height:1.15; opacity:0; transform:scale(.5); transform-origin:0 100%;
  transition:opacity .15s, transform .25s cubic-bezier(.3,1.7,.5,1); pointer-events:none}
.pd-say.on{opacity:1; transform:none}
.pd-flake,.pd-drip{position:absolute; top:0; left:0; pointer-events:none; z-index:4; line-height:1}
.pd-flake{font-size:18px} .pd-drip{font-size:16px}
.pd-body{transform-box:view-box; transform-origin:60px 143px; transform:scale(calc(1 + var(--m, 0) * .055), calc(1 - var(--m, 0) * .075)); transition:transform .7s cubic-bezier(.3,1.5,.5,1)}
.pd-puddle{transform-box:fill-box; transform-origin:center; transform:scale(calc(1 + var(--m, 0) * .2), calc(1 + var(--m, 0) * .12)); transition:transform .7s}
.pd-hat,.pd-arm,.pd-nose,.pd-shades{transform-box:fill-box; transition:transform .6s cubic-bezier(.3,1.5,.5,1), opacity .4s}
.pd-hat{transform-origin:50% 100%}
.pd-scene.m1 .pd-hat{transform:rotate(-14deg) translate(-3px,1px)}
.pd-scene.m5 .pd-hat{transform:rotate(-7deg) translate(-2px,13px)}
.pd-arm.l{transform-origin:100% 100%} .pd-arm.r{transform-origin:0 100%}
.pd-scene.m2 .pd-arm.l{transform:rotate(-78deg)}
.pd-scene.m4 .pd-arm.r{transform:rotate(78deg)}
.pd-nose{transform-origin:0 50%}
.pd-scene.m3 .pd-nose{transform:rotate(32deg)}
.pd-shades{opacity:0; transform:translateY(-9px)}
.pd-scene.m3 .pd-shades{opacity:1; transform:none}
.pd-slots{display:flex; justify-content:center; gap:5px; padding:0 2px}
.pd-slot{flex:0 1 46px; min-width:0; height:58px; display:flex; flex-direction:column; align-items:center; justify-content:flex-end; padding-bottom:3px;
  border-bottom:5px solid var(--ink); border-radius:3px; font-family:var(--display); font-weight:700; line-height:1;
  font-size:min(34px, calc((100vw - 70px) / var(--n) * .78))}
.pd-slot.zh{flex-basis:66px; height:76px; justify-content:center; font-family:var(--body); font-size:40px; border:3px dashed rgba(27,45,69,.45); border-radius:14px; background:rgba(255,255,255,.75)}
.pd-slot.zh.full{border:3px solid var(--ink); background:#fff}
.pd-slot b{font-weight:700} .pd-slot .gh{color:rgba(27,45,69,.25)}
.pd-slot i{font-style:normal; font-family:var(--body); font-weight:800; font-size:13px; color:var(--ink-soft); margin-top:3px}
.pd-reveal{min-height:34px; text-align:center; font-family:var(--display); font-weight:700; font-size:28px; line-height:1.1}
.pd-reveal small{display:block; font-family:var(--body); font-size:15px; color:var(--ink-soft)}
.pd-reveal .art{color:var(--coral)} .pd-reveal .der{color:#1E88E5} .pd-reveal .die{color:#E53935} .pd-reveal .das{color:#2F9E6E}
.pd-keys{display:grid; grid-template-columns:repeat(auto-fill, minmax(40px, 1fr)); gap:6px; width:100%; max-width:600px; margin:0 auto}
.pd-keys.few{grid-template-columns:repeat(auto-fill, minmax(58px, 1fr)); max-width:460px; gap:9px}
.pd-keys.trio{grid-template-columns:repeat(3, minmax(0, 96px)); justify-content:center; gap:14px}
.pd-key{height:48px; min-width:0; padding:0; font-family:var(--display); font-weight:700; font-size:24px; line-height:1; background:#fff;
  border:3px solid var(--ink); border-radius:12px; box-shadow:2px 3px 0 var(--ink); transition:transform .08s, opacity .25s, background .25s}
.pd-key:active{transform:translate(2px,3px); box-shadow:0 0 0 var(--ink)}
.pd-keys.few .pd-key{height:60px; font-size:30px; background:var(--c)}
.pd-keys.trio .pd-key{height:96px; font-size:52px; border-radius:22px; background:var(--c)}
.pd-key.zh{font-family:var(--body)}
.pd-key.ok{background:#BDEBDD; opacity:.45; pointer-events:none}
.pd-key.no{background:#FFD0C6; opacity:.3; pointer-events:none}
.pd-key.hint{animation:pd-hint .8s ease-in-out infinite}
@keyframes pd-sway{50%{transform:rotate(-2.5deg)}}
@keyframes pd-wob{50%{transform:rotate(-6deg) scale(1.06)}}
@keyframes pd-hint{50%{transform:scale(1.16); background:#FFF3C4}}
`);

registerGame({id: ID, em: "⛄", name: "Bonhomme de neige", multi: true, cat: "mots",
  title: {en: TX.en.title, de: TX.de.title, lb: TX.lb.title, zh: TX.zh.title},
  sub: {en: TX.en.sub, de: TX.de.sub, lb: TX.lb.sub, zh: TX.zh.sub}}, function () {
  const lvl = levelOf(ID), lv = LV[lvl], small = S.kid === "p4";
  let L = lang();
  const rounds = () => L === "zh" && lv.zrounds ? lv.zrounds : lv.rounds;
  let targets = choose(L, lvl, rounds(), []);
  const total = targets.length, res = [], body = $("gameBody");
  startSession(ID, null, total); const gen = GEN;
  let i = 0, busy = false, lastTap = Date.now(), idle = null;
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));

  const round = () => {
    if (!alive(gen)) return;
    // a flag touched in the game bar: the words still to come are chosen again in the new language
    if (lang() !== L) { L = lang(); const past = targets.slice(0, i); targets = past.concat(choose(L, lvl, total - i, past.map(x => text(x.w, L)))); }
    if (i >= total || !targets[i]) return finish();
    busy = false;
    const {w, d} = targets[i], zh = L === "zh", tx = TX[L], word = text(w, L);
    const letters = [...word.normalize("NFC")], n = letters.length;
    const low = c => zh ? c : c.toLowerCase(), caps = lvl <= 2 && !zh, face = c => caps ? up(c) : c;
    const inWord = new Set(letters.map(low)), shown = letters.map(() => false), py = zh ? pinyin(w) : null;
    const ghost = lvl === 1 || (small && lvl === 2), sayLetters = lvl <= 2 && L !== "lb"; // no letter names are recorded in Luxembourgish
    let errs = 0, hinted = 0, keys = [];
    if (zh && lv.def) shown[rnd(n)] = true; // Chinese riddle: one character is given, the other is to find
    const full = L === "lb" ? w.lb : w[L]; // "die Katze"; in Luxembourgish, the text that plays the lod.lu recording
    const riddle = d ? `${d.t[L]} ${tx[d.q]}` : "";
    const ask = () => {
      if (lv.def) return L === "lb" ? Promise.resolve() : say(riddle, L);
      if (lvl <= 2) return say(L === "lb" ? full : zh ? `${full}！${tx.go}` : `${full}! ${tx.go}`, L);
      return L === "lb" ? Promise.resolve() : say(tx.guess, L);
    };
    // 🔊: the instruction again; at levels 3 and 4 hearing the word itself is a clue (the Luxembourgish riddle cannot be heard)
    const clueByEar = lvl === 3 || (lv.def && L === "lb");
    const listen = () => { G.replays++; lastTap = Date.now(); if (clueByEar) { G.hints++; say(full, L); } else ask(); };

    renderDots(res, total, i);
    body.innerHTML = "";
    const prompt = lv.def
      ? el("p", "prompt pd-prompt riddle", `🤔 ${d.t[L]}<small>${tx[d.q]}${zh ? " " + tx.miss : ""}</small>`)
      : el("p", "prompt pd-prompt", `⛄ ${lvl <= 2 ? tx.go : tx.guess}`); // always written (Kezhan: a chance to read)
    const row = el("div", "row"); row.style.justifyContent = "center";
    const again = el("button", "speak chunky", `🔊 <span>${clueByEar ? tx.hear : tx.again}</span>`);
    again.onclick = listen;
    const tip = el("button", "chip", "💡"); tip.style.cssText = "font-size:26px; min-width:64px; min-height:56px";
    tip.onclick = () => { if (busy) return; G.hints++; hinted++; lastTap = Date.now(); hintKey(); };
    row.append(again, tip);

    const scene = el("div", "pd-scene"), sun = el("div", "pd-sun", "☀️"), cloud = el("div", "pd-cloud", "🌨️"), bub = el("div", "pd-say");
    const man = el("button", "pd-man", MAN); man.setAttribute("aria-label", "⛄");
    const pic = el("button", "pd-pic" + (lv.pic ? "" : " mys"), lv.pic ? wordFace(w) : "?");
    scene.append(sun, cloud, bub, man, pic);
    const slotRow = el("div", "pd-slots"); slotRow.style.setProperty("--n", n);
    const slots = letters.map((ch, k) => {
      const s = el("div", "pd-slot" + (zh ? " zh" : ""), (ghost ? `<b class="gh">${face(ch)}</b>` : "") + (py && lvl <= 2 ? `<i>${py[k]}</i>` : ""));
      slotRow.append(s); return s;
    });
    const reveal = el("div", "pd-reveal"), box = el("div", "pd-keys " + (zh && lv.keys === "all" ? "few" : lv.keys)); // characters get big keys
    body.append(prompt, row, scene, slotRow, reveal, box);

    /* ---------- the snowman's moods ---------- */
    let tBub;
    const bubble = (t, ms = 1200) => { bub.textContent = t; bub.classList.add("on"); clearTimeout(tBub); loops.push(tBub = setTimeout(() => bub.classList.remove("on"), ms)); };
    const hop = (h = 16) => anim(man, [{transform: "none"}, {transform: `translateY(-${h}px) scale(.96,1.05)`, offset: .4}, {transform: "scale(1.06,.94)", offset: .75}, {transform: "none"}], 520, {easing: "ease-out"});
    const setMelt = m => {
      scene.style.setProperty("--m", m);
      for (let k = 1; k <= MELT; k++) scene.classList.toggle("m" + k, k <= m);
    };
    const drip = () => {
      if (calm()) return;
      const a = man.getBoundingClientRect(), s = scene.getBoundingClientRect(), dr = el("div", "pd-drip", "💧");
      dr.style.left = a.left - s.left + a.width * (.3 + Math.random() * .4) + "px"; dr.style.top = a.top - s.top + a.height * .3 + "px";
      scene.append(dr);
      dr.animate([{transform: "translateY(0) scale(.6)", opacity: 1}, {transform: `translateY(${a.height * .62}px) scale(1)`, opacity: .2}], {duration: 700, easing: "ease-in"}).onfinish = () => dr.remove();
    };
    const snowfall = () => {
      if (calm()) return;
      const W = scene.clientWidth, H = scene.clientHeight;
      for (let k = 0; k < 16; k++) {
        const f = el("div", "pd-flake", "❄️"); f.style.left = rnd(Math.max(20, W - 20)) + "px"; scene.append(f);
        f.animate([{transform: "translateY(-24px) rotate(0)", opacity: 1}, {transform: `translateY(${H + 10}px) rotate(${rnd(360)}deg)`, opacity: .3}],
          {duration: 1100 + rnd(700), delay: rnd(500), easing: "linear", fill: "backwards"}).onfinish = () => f.remove();
      }
      anim(cloud, [{opacity: 0, transform: "translateX(50px)"}, {opacity: 1, transform: "none", offset: .2}, {opacity: 1, transform: "none", offset: .8}, {opacity: 0, transform: "translateX(50px)"}], 2000);
    };

    /* ---------- keys ---------- */
    const need = () => { const k = shown.indexOf(false); return k < 0 ? null : low(letters[k]); };
    const tried = new Set();
    const decoys = (k, pref) => {
      const out = [], add = c => { if (c && out.length < k && !inWord.has(c) && !tried.has(c) && !out.includes(c)) out.push(c); };
      (pref || []).forEach(add);
      const spare = zh ? hanChars() : [...ALPHA[L]];
      for (let t = 0; t < 300 && out.length < k; t++) add(one(spare));
      return out;
    };
    const looks = () => shuffle(letters.flatMap(c => [...((ep().zhLook || {})[c] || "")])); // Chinese characters that look alike
    const draw = list => {
      box.innerHTML = "";
      keys = list.map((c, k) => {
        const b = el("button", "pd-key" + (zh ? " zh" : ""), face(c)), key = {c, b, used: false};
        b.style.setProperty("--c", KEYC[k % KEYC.length]);
        b.onclick = () => tap(key);
        box.append(b);
        if (lv.keys !== "all") anim(b, [{transform: "scale(.2)", opacity: 0}, {transform: "scale(1)", opacity: 1}], 320, {delay: 50 * k, easing: "cubic-bezier(.3,1.6,.5,1)", fill: "backwards"});
        return key;
      });
      marks();
    };
    const trio = () => draw(shuffle([need(), ...decoys(2)]));
    const firstKeys = () => {
      if (lv.keys === "trio") return trio();
      const mine = [...new Set(letters.filter((ch, k) => !shown[k]).map(low))]; // the letters still to find (the given Chinese character is not a key)
      if (lv.keys === "few") return draw(shuffle(mine.concat(decoys(zh ? 4 : 3, zh ? looks() : null))));
      if (zh) return draw(shuffle(mine.concat(decoys(lv.def ? 5 : 12 - mine.length, looks()))));
      draw([...ALPHA[L]]);
    };
    const keyOf = c => keys.find(k => k.c === c && !k.used);
    const marks = () => {
      keys.forEach(k => delete k.b.dataset.ok);
      if (!busy && need()) markOk((keyOf(need()) || {}).b);
    };
    const hintKey = () => { keys.forEach(k => k.b.classList.remove("hint")); const k = keyOf(need()); if (k) { k.b.classList.add("hint"); anim(man, [{transform: "none"}, {transform: "rotate(6deg)"}, {transform: "none"}], 500); } };
    const fill = j => {
      const s = slots[j];
      s.classList.add("full"); s.innerHTML = `<b>${face(letters[j])}</b>` + (py && lvl <= 2 ? `<i>${py[j]}</i>` : "");
      anim(s, [{transform: "scale(.3)"}, {transform: "scale(1.3,.8)", offset: .45}, {transform: "scale(.92,1.1)", offset: .75}, {transform: "none"}], 380, {easing: "ease-out"});
      if (!calm()) { const r = s.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 5); }
    };
    const tap = key => {
      if (busy || key.used || !alive(gen)) return;
      G.taps++; lastTap = Date.now(); key.used = true; tried.add(key.c);
      key.b.classList.remove("hint"); delete key.b.dataset.ok;
      if (sayLetters) say(zh ? key.c : up(key.c), L); // the voice names each letter (in Chinese, each character)
      const right = letters.some((ch, j) => !shown[j] && low(ch) === key.c);
      if (right) {
        key.b.classList.add("ok"); snd("ding"); hop(8);
        letters.forEach((ch, j) => { if (!shown[j] && low(ch) === key.c) { shown[j] = true; fill(j); } });
        if (shown.every(Boolean)) return complete();
        if (lv.keys === "trio") { keys.forEach(k => delete k.b.dataset.ok); loops.push(setTimeout(() => { if (alive(gen) && !busy) trio(); }, TEST ? 0 : 450)); }
        else marks();
        return;
      }
      // a wrong letter: the snowman melts a little, never sad
      errs++; key.b.classList.add("no");
      if (typeof fx !== "undefined") fx.wrong();
      setMelt(Math.min(errs, MELT)); snd("plip"); drip();
      if (errs === 3) bubble("😎", 1500);
      else if (errs === MELT) { bubble(tx.puddle, 2000); loops.push(setTimeout(() => snd("giggle"), 300)); }
      else bubble(one(tx.oops), 1100);
      if (errs >= MELT || (lvl === 1 && errs >= 2) || (small && errs >= 2)) hintKey(); // the right key blinks
      marks();
    };

    /* ---------- a word found: it is shown with its article, then snow rebuilds the snowman ---------- */
    const complete = async () => {
      busy = true; marks(); keys.forEach(k => k.b.classList.remove("hint"));
      const ok = errs + hinted <= (zh ? lv.zfree : lv.free);
      if (ok) addStar();
      logRound(w.en, ok, errs + 1, {lvl, lang: L}); res.push(ok ? 1 : 0); renderDots(res, total, -1);
      const a = artOf(w, L).trim(), pyAll = pinyin(w);
      reveal.innerHTML = (a ? `<span class="art ${a.replace("'", "")}">${a}</span>${artOf(w, L).endsWith(" ") ? " " : ""}` : "") + `<span>${word}</span>` + (zh && pyAll ? `<small>${pyAll.join(" ")}</small>` : "");
      anim(reveal, [{transform: "scale(.3)", opacity: 0}, {transform: "scale(1.15)", opacity: 1, offset: .6}, {transform: "none"}], 450, {easing: "ease-out"});
      if (!lv.pic) { pic.classList.remove("mys"); pic.innerHTML = wordFace(w); anim(pic, [{transform: "scale(.4) rotate(-20deg)"}, {transform: "scale(1.15)"}, {transform: "none"}], 500); }
      slots.forEach((s, k) => anim(s, [{transform: "none"}, {transform: "translateY(-12px)"}, {transform: "none"}], 400, {delay: 60 * k}));
      const cheer = one(tx.cheer);
      bubble(cheer, 1600); hop(20);
      await say(L === "lb" ? w.lb : zh ? cheer + w.zh : `${cheer} ${full}!`, L); if (!alive(gen)) return;
      if (errs) { // snow falls and he is whole again
        snowfall(); snd("snow"); setMelt(0);
        loops.push(setTimeout(() => { if (alive(gen)) { bubble(tx.back, 1400); hop(24); } }, TEST ? 0 : 700));
        await wait(1600); if (!alive(gen)) return;
      } else { snd("giggle"); anim(man, [{transform: "none"}, {transform: "rotate(-10deg)"}, {transform: "rotate(10deg)"}, {transform: "rotate(-6deg)"}, {transform: "none"}], 700); }
      await wait(900); if (!alive(gen)) return;
      i++; round();
    };

    man.onclick = () => { G.taps++; lastTap = Date.now(); snd("giggle"); bubble(tx.hihi, 900); hop(18); if (lvl <= 2) ask(); };
    pic.onclick = () => { lastTap = Date.now(); anim(pic, [{transform: "none"}, {transform: "scale(1.15) rotate(6deg)"}, {transform: "none"}], 380); if (lvl <= 2 || (lv.def && L !== "lb")) { G.replays++; ask(); } };
    // nobody has touched anything for a while: the little one is shown the right key and hears the word again, the big one sees a wave
    idle = () => {
      if (small || lvl === 1) { const k = keyOf(need()); if (k) anim(k.b, [{transform: "none"}, {transform: "scale(1.2) rotate(-6deg)"}, {transform: "none"}], 600); ask(); }
      else { anim(man.querySelector(".pd-arm.r"), [{transform: "none"}, {transform: "rotate(-25deg)"}, {transform: "rotate(10deg)"}, {transform: "none"}], 900); bubble("?", 900); }
    };
    letters.forEach((ch, j) => { if (shown[j]) fill(j); });
    firstKeys();
    anim(man, [{transform: "translateY(40px) scale(.6)", opacity: 0}, {transform: "none", opacity: 1}], 500, {easing: "cubic-bezier(.3,1.6,.5,1)"});
    ask();
  };
  if (!TEST) loops.push(setInterval(() => { if (alive(gen) && !busy && idle && Date.now() - lastTap > 9000) { lastTap = Date.now(); idle(); } }, 2000));
  round();
});
})();
