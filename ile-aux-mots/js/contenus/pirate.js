/* L'Île aux Mots : contenus du jeu « Le Robot pirate » (js/jeux/pirate.js), dans les quatre langues apprises, jamais en français :
   les phrases, les cartes, les trésors, les répliques du perroquet et du robot, les dessins, et en fin de fichier les îles
   (tirées au hasard, puis vérifiées par un solveur qui donne aussi le programme le plus court).
   Repères pris dans le lexique (donnees.js), pour que le luxembourgeois s'entende par l'enregistrement lod.lu du mot.
   Allemand : datif après les prépositions de lieu (links vom Baum, zwischen dem Haus und der Blume, zum/zur), accusatif après suchen.
   Luxembourgeois vérifié sur lod.lu : Schatz, Feld/Felder, no uewen, no ënnen, no lénks, no riets (LENKS2, RIETS2), riichtaus,
   dréinen, gruewen (gruef!), fueren (fuer!), goen (géi!), huelen (huel!), fannen (fann!, fonnt), bleiwen, stoen, Këscht, zou, Fiels,
   Kaart/Kaarten, dräimol, packen, nërdlech/südlech/ëstlech/westlech vun, iwwer, ënner, tëscht, Norden, Osten, Süden, Westen,
   Papagei, Pirat, Kapitän, Matrous, Kichelchen (m), Siicht, kribbelen, pardon, nolauschteren (lauschter no!), gescheit, allez, just (nur),
   dréinen (intransitif : die Richtung ändern), dohin, Teleskop (m), Coupe (Pokal), Mënz ; pas de Goldmënz dans le LOD, d'où « Mënz aus Gold » ;
   « Platsch », « Hatschi », « Ahoi » sont des onomatopées absentes du LOD. Le genre des repères (Kanéngchen est féminin), et les trésors : Kompass, Kroun, Diamant, Rank, Muschel, Anker, Teleskop, Klack,
   Teddybier, Trompett, Pärel, Mënz, Sandauer, Coupe, Fändel, Dinosaurier. Règle de l'Eifel : e Kompass, en Anker, e Fiels. */
const PIRATE = (() => {
  // landmarks: English key of the lexicon, Luxembourgish gender (lod.lu) for the dative
  const MARKS = [["tree","m"],["house","n"],["flower","f"],["boat","n"],["turtle","f"],["snake","f"],["frog","m"],["crocodile","m"],
    ["umbrella","m"],["star","m"],["ball","m"],["banana","f"],["bird","m"],["rocket","f"],["pig","n"],["duck","f"],["penguin","m"],
    ["cake","m"],["car","m"],["horse","n"],["dog","m"],["cat","f"],["chicken","n"],["snowman","m"],["apple","m"],["bike","m"],
    ["rabbit","f"],["cow","f"],["sheep","n"],["mouse","f"],["giraffe","f"]];
  const words = Object.values(THEMES).flatMap(t => t.words);
  const marks = MARKS.map(([en, g]) => {
    const w = words.find(x => x.en === en && x.lod);
    if (!w) return null;
    const [art, ...rest] = w.de.split(" "), de = rest.join(" "), dm = art !== "die", lbN = w.lb.replace(/^(den |de |d')/, ""), lm = g !== "f";
    return {en, e:w.e, lvl:w.lvl || 1, zh:w.zh, lb:w.lb, deNom:w.de, deAcc:art === "der" ? "den " + de : w.de,
      deDat:(dm ? "dem " : "der ") + de, deVon:dm ? "vom " + de : "von der " + de, deZu:(dm ? "zum " : "zur ") + de,
      lbDat:(lm ? "dem " : "der ") + lbN, lbVun:lm ? "vum " + lbN : "vun der " + lbN};
  }).filter(Boolean);

  const P = {
    marks,
    // level 1: drive to a landmark
    drive: {en:m => `Drive to the ${m.en}!`, de:m => `Fahr ${m.deZu}!`, lb:m => `Wou ass ${m.lb}? Fuer dohin!`, zh:m => `开到${m.zh}那里！`},
    notThis: {en:(m, t) => `This is the ${m.en}! We want the ${t.en}.`, de:(m, t) => `Das ist ${m.deNom}! Wir suchen ${t.deAcc}.`,
      lb:(m, t) => `Dat ass ${m.lb}! Mir sichen ${t.lb}.`, zh:(m, t) => `这是${m.zh}！我们要找${t.zh}。`},
    here: {en:m => `Yes, the ${m.en}! Dig!`, de:m => `Ja, ${m.deNom}! Grab!`, lb:m => `Jo, ${m.lb}! Gruef!`, zh:m => `对，是${m.zh}！挖吧！`},
    // level 2: the parrot dictates the way
    step: {
      en:(d, n) => `Go ${d} ${["", "one", "two", "three"][n]}!`,
      de:(d, n) => `Geh ${["", "ein Feld", "zwei Felder", "drei Felder"][n]} nach ${{up:"oben", down:"unten", left:"links", right:"rechts"}[d]}!`,
      lb:(d, n) => `Géi ${["", "ee Feld", "zwee Felder", "dräi Felder"][n]} no ${{up:"uewen", down:"ënnen", left:"lénks", right:"riets"}[d]}!`,
      zh:(d, n) => `往${{up:"上", down:"下", left:"左", right:"右"}[d]}走${["", "一", "两", "三"][n]}格！`
    },
    dig: {en:"Dig!", de:"Grab!", lb:"Gruef!", zh:"挖！"},
    listen: {en:"Listen carefully!", de:"Hör gut zu!", lb:"Lauschter gutt no!", zh:"仔细听！"},
    // levels 3 and 4: where the treasure is, next to the landmarks
    rel: {
      left:   {en:a => `to the left of the ${a.en}`, de:a => `links ${a.deVon}`, lb:a => `lénks ${a.lbVun}`, zh:a => `在${a.zh}的左边`},
      right:  {en:a => `to the right of the ${a.en}`, de:a => `rechts ${a.deVon}`, lb:a => `riets ${a.lbVun}`, zh:a => `在${a.zh}的右边`},
      above:  {en:a => `above the ${a.en}`, de:a => `über ${a.deDat}`, lb:a => `iwwer ${a.lbDat}`, zh:a => `在${a.zh}的上面`},
      below:  {en:a => `below the ${a.en}`, de:a => `unter ${a.deDat}`, lb:a => `ënner ${a.lbDat}`, zh:a => `在${a.zh}的下面`},
      between:{en:(a, b) => `between the ${a.en} and the ${b.en}`, de:(a, b) => `zwischen ${a.deDat} und ${b.deDat}`,
               lb:(a, b) => `tëscht ${a.lbDat} an ${b.lbDat}`, zh:(a, b) => `在${a.zh}和${b.zh}的中间`},
      north:  {en:a => `north of the ${a.en}`, de:a => `nördlich ${a.deVon}`, lb:a => `nërdlech ${a.lbVun}`, zh:a => `在${a.zh}的北边`},
      south:  {en:a => `south of the ${a.en}`, de:a => `südlich ${a.deVon}`, lb:a => `südlech ${a.lbVun}`, zh:a => `在${a.zh}的南边`},
      east:   {en:a => `east of the ${a.en}`, de:a => `östlich ${a.deVon}`, lb:a => `ëstlech ${a.lbVun}`, zh:a => `在${a.zh}的东边`},
      west:   {en:a => `west of the ${a.en}`, de:a => `westlich ${a.deVon}`, lb:a => `westlech ${a.lbVun}`, zh:a => `在${a.zh}的西边`}
    },
    clue: {en:s => `The treasure is ${s}.`, de:s => `Der Schatz ist ${s}.`, lb:s => `De Schatz ass ${s}.`, zh:s => `宝藏${s}。`},
    compass: {en:"↑ N  → E  ↓ S  ← W", de:"↑ N  → O  ↓ S  ← W", lb:"↑ N  → O  ↓ S  ← W", zh:"↑ 北  → 东  ↓ 南  ← 西"},
    // program cards: i is shown to the little one, who does not read
    cards: {
      up:    {i:"⬆️", en:"up", de:"nach oben", lb:"no uewen", zh:"往上"},
      down:  {i:"⬇️", en:"down", de:"nach unten", lb:"no ënnen", zh:"往下"},
      left:  {i:"⬅️", en:"left", de:"nach links", lb:"no lénks", zh:"往左"},
      right: {i:"➡️", en:"right", de:"nach rechts", lb:"no riets", zh:"往右"},
      fwd:   {i:"⏫", en:"forward", de:"geradeaus", lb:"riichtaus", zh:"向前走"},
      tl:    {i:"↩️", en:"turn left", de:"nach links drehen", lb:"no lénks dréinen", zh:"向左转"},
      tr:    {i:"↪️", en:"turn right", de:"nach rechts drehen", lb:"no riets dréinen", zh:"向右转"},
      rep:   {i:"🔁", en:"three times", de:"dreimal", lb:"dräimol", zh:"重复三次"}
    },
    ui: {
      again: {en:"Again", de:"Nochmal", lb:"Nach eng Kéier", zh:"再听一次"},
      go: {en:"Go!", de:"Los!", lb:"Allez!", zh:"出发！"}, // lod.lu: "lass" is an adjective (los), "allez" the call to start
      challenge: {en:n => `Can you do it with ${n} cards?`, de:n => `Schaffst du es mit ${n} Karten?`, lb:n => `Packs du et mat ${n} Kaarten?`, zh:n => `你能只用${n}张卡片吗？`},
      bonus: {en:n => `Only ${n} cards! Bonus star!`, de:n => `Nur ${n} Karten! Ein Extrastern!`, lb:n => `Just ${n} Kaarten! Nach e Stär!`, zh:n => `只用了${n}张卡片！再奖励一颗星！`},
      where: {en:"Where will the robot stop?", de:"Wo bleibt der Roboter stehen?", lb:"Wou bleift de Roboter stoen?", zh:"机器人会停在哪里？"},
      yes: {en:"Yes! Well spotted!", de:"Ja! Gut aufgepasst!", lb:"Jo! Richteg!", zh:"对！看得真准！"},
      look: {en:"Look where it stops!", de:"Schau, wo er stehen bleibt!", lb:"Kuck, wou de Roboter stoe bleift!", zh:"看，它停在这里！"},
      wrongCard: {en:"Find the wrong card!", de:"Finde die falsche Karte!", lb:"Fann déi falsch Kaart!", zh:"找出错的那张卡片！"},
      which: {en:"Which card is right?", de:"Welche Karte ist richtig?", lb:"Wéi eng Kaart ass richteg?", zh:"哪张卡片是对的？"},
      notHere: {en:"Arr! Not here!", de:"Arr! Nicht hier!", lb:"Arr! Net hei!", zh:"啊！不在这里！"},
      locked: {en:"The chest is locked! Get the key first!", de:"Die Truhe ist zu! Hol zuerst den Schlüssel!",
        lb:"D'Këscht ass zou! Huel fir d'éischt de Schlëssel!", zh:"箱子锁着呢！先去拿钥匙！"},
      key: {en:"I've got the key!", de:"Ich habe den Schlüssel!", lb:"Ech hunn de Schlëssel!", zh:"拿到钥匙了！"},
      rock: {en:"Beep beep! A rock!", de:"Piep piep! Ein Felsen!", lb:"Biip biip! E Fiels!", zh:"嘀嘀！有石头！"},
      beep: {en:"Beep beep!", de:"Piep piep!", lb:"Biip biip!", zh:"嘀嘀！"},
      splash: {en:"Splash! Hello, octopus!", de:"Platsch! Hallo, Oktopus!", lb:"Platsch! Moien, Oktopus!", zh:"扑通！你好，章鱼！"},
      found: {en:t => `You found ${t.en}!`, de:t => `Du hast ${t.de} gefunden!`, lb:t => `Du hues ${t.lb} fonnt!`, zh:t => `你找到了${t.zh}！`},
      cheer: {en:"Treasure! Treasure!", de:"Schatz! Schatz!", lb:"Schatz! Schatz!", zh:"宝藏！宝藏！"}
    },
    // treasures for the bag, with the article each sentence needs (German accusative, Luxembourgish Eifel rule, Chinese classifier)
    treasures: [
      {e:"🧭", en:"a compass", de:"einen Kompass", lb:"e Kompass", zh:"一个指南针"},
      {e:"👑", en:"a crown", de:"eine Krone", lb:"eng Kroun", zh:"一顶王冠"},
      {e:"💎", en:"a diamond", de:"einen Diamanten", lb:"en Diamant", zh:"一颗钻石"},
      {e:"💍", en:"a ring", de:"einen Ring", lb:"e Rank", zh:"一枚戒指"},
      {e:"🐚", en:"a shell", de:"eine Muschel", lb:"eng Muschel", zh:"一个贝壳"},
      {e:"⚓", en:"an anchor", de:"einen Anker", lb:"en Anker", zh:"一个锚"},
      {e:"🔭", en:"a telescope", de:"ein Fernrohr", lb:"en Teleskop", zh:"一个望远镜"},
      {e:"🔔", en:"a bell", de:"eine Glocke", lb:"eng Klack", zh:"一个铃铛"},
      {e:"🧸", en:"a teddy bear", de:"einen Teddybären", lb:"en Teddybier", zh:"一只泰迪熊"},
      {e:"🎺", en:"a trumpet", de:"eine Trompete", lb:"eng Trompett", zh:"一把小号"},
      {e:"🦪", en:"a pearl", de:"eine Perle", lb:"eng Pärel", zh:"一颗珍珠"},
      {e:"🪙", en:"a gold coin", de:"eine Goldmünze", lb:"eng Mënz aus Gold", zh:"一枚金币"},
      {e:"⏳", en:"an hourglass", de:"eine Sanduhr", lb:"eng Sandauer", zh:"一个沙漏"},
      {e:"🏆", en:"a trophy", de:"einen Pokal", lb:"eng Coupe", zh:"一个奖杯"},
      {e:"🚩", en:"a flag", de:"eine Fahne", lb:"e Fändel", zh:"一面旗子"},
      {e:"🦖", en:"a dinosaur", de:"einen Dinosaurier", lb:"en Dinosaurier", zh:"一只恐龙"},
      {e:"🍪", en:"a biscuit", de:"einen Keks", lb:"e Kichelchen", zh:"一块饼干"}
    ],
    // the parrot, when touched
    parrot: [
      {en:"Squawk! Pretty robot!", de:"Kraah! Schöner Roboter!", lb:"Kraah! Schéine Roboter!", zh:"嘎！好漂亮的机器人！"},
      {en:"Yo ho ho!", de:"Jo-ho-ho!", lb:"Jo-ho-ho!", zh:"哟嚯嚯！"},
      {en:"Where is the treasure?", de:"Wo ist der Schatz?", lb:"Wou ass de Schatz?", zh:"宝藏在哪里？"},
      {en:"I want a biscuit!", de:"Ich will einen Keks!", lb:"Ech wëll e Kichelchen!", zh:"我要饼干！"},
      {en:"Ahoy, captain!", de:"Ahoi, Kapitän!", lb:"Ahoi, Kapitän!", zh:"你好，船长！"},
      {en:"Land ho!", de:"Land in Sicht!", lb:"Land a Siicht!", zh:"看到陆地了！"},
      {en:"Gold! Gold! Gold!", de:"Gold! Gold! Gold!", lb:"Gold! Gold! Gold!", zh:"金子！金子！金子！"},
      {en:"Hello, sailor!", de:"Hallo, Matrose!", lb:"Moien, Matrous!", zh:"你好，水手！"},
      {en:"I love treasure!", de:"Ich liebe Schätze!", lb:"Ech hu Schätz gär!", zh:"我最爱宝藏了！"},
      {en:"Dig, dig, dig!", de:"Grab, grab, grab!", lb:"Gruef, gruef, gruef!", zh:"挖呀挖呀挖！"},
      {en:"What a clever robot!", de:"Was für ein kluger Roboter!", lb:"Wat e gescheite Roboter!", zh:"好聪明的机器人！"},
      {en:"Hello, hello!", de:"Hallo, hallo!", lb:"Moien, moien!", zh:"你好，你好！"}
    ],
    // the robot, when touched: s = its sound, a = its move
    gags: [
      {s:"burp", a:"puff", en:"Burp! Excuse me!", de:"Rülps! Entschuldigung!", lb:"Hoppla! Pardon!", zh:"嗝！不好意思！"},
      {s:"sneeze", a:"jump", en:"Achoo!", de:"Hatschi!", lb:"Hatschi!", zh:"阿嚏！"},
      {s:"hic", a:"jump", en:"Hic!", de:"Hicks!", lb:"Hick!", zh:"嗝儿！"},
      {s:"giggle", a:"wobble", en:"Hee hee, that tickles!", de:"Hihi, das kitzelt!", lb:"Hihi, dat kribbelt!", zh:"嘻嘻，好痒！"},
      {s:"spin", a:"spin", en:"Wheee!", de:"Juhuu!", lb:"Juhu!", zh:"哇哦！"},
      {s:"beep", a:"puff", en:"Beep boop!", de:"Piep bup!", lb:"Biip bup!", zh:"哔哔啵啵！"},
      {s:"toot", a:"wobble", en:"Pfft! Oops!", de:"Pfft! Hoppla!", lb:"Pfft! Hoppla!", zh:"噗！哎呀！"},
      {s:"boing", a:"jump", en:"Boing boing!", de:"Boing boing!", lb:"Boing boing!", zh:"蹦蹦跳！"}
    ],
    // drawings: the robot (a patch, a hat with a skull, one blinking eye) and the chest
    robot: `<svg viewBox="0 0 100 100" aria-hidden="true">
      <g class="pir-ant"><line x1="50" y1="24" x2="50" y2="9" stroke="#1B2D45" stroke-width="3"/><circle cx="50" cy="8" r="5" fill="#FF6F59" stroke="#1B2D45" stroke-width="2.5"/></g>
      <rect x="18" y="30" width="64" height="44" rx="15" fill="#9FD8EA" stroke="#1B2D45" stroke-width="3"/>
      <path d="M14 34 Q50 4 86 34 Q50 24 14 34Z" fill="#1B2D45"/><circle cx="50" cy="22" r="4.5" fill="#fff"/>
      <rect x="47" y="27" width="6" height="2.5" rx="1" fill="#fff"/>
      <path d="M19 44 L81 36" stroke="#1B2D45" stroke-width="2.5"/><ellipse cx="35" cy="50" rx="9" ry="8" fill="#1B2D45"/>
      <g class="pir-eye"><circle cx="64" cy="50" r="9.5" fill="#fff" stroke="#1B2D45" stroke-width="2.5"/><circle class="pir-pup" cx="64" cy="50" r="4.2" fill="#1B2D45"/></g>
      <circle class="pir-cheek" cx="27" cy="64" r="4.5" fill="#FF9AA2"/><circle class="pir-cheek" cx="73" cy="64" r="4.5" fill="#FF9AA2"/>
      <path class="pir-smile" d="M40 61 Q50 70 60 61" fill="none" stroke="#1B2D45" stroke-width="3" stroke-linecap="round"/>
      <ellipse class="pir-oh" cx="50" cy="65" rx="4.5" ry="5.5" fill="#1B2D45" opacity="0"/>
      <rect x="30" y="74" width="40" height="12" rx="4" fill="#FFC43D" stroke="#1B2D45" stroke-width="3"/><circle cx="50" cy="80" r="2.5" fill="#1B2D45"/>
      <rect x="22" y="85" width="56" height="11" rx="5.5" fill="#4A5B72" stroke="#1B2D45" stroke-width="2.5"/>
      <circle cx="32" cy="90.5" r="2.5" fill="#FFFBF2"/><circle cx="50" cy="90.5" r="2.5" fill="#FFFBF2"/><circle cx="68" cy="90.5" r="2.5" fill="#FFFBF2"/></svg>`,
    chest: `<svg viewBox="0 0 60 52" aria-hidden="true"><rect x="6" y="24" width="48" height="24" rx="3" fill="#8B4513" stroke="#1B2D45" stroke-width="3"/>
      <rect x="6" y="32" width="48" height="4" fill="#FFC43D"/><g class="pir-lid"><path d="M6 25 Q30 2 54 25Z" fill="#B5651D" stroke="#1B2D45" stroke-width="3"/></g>
      <rect x="26" y="22" width="8" height="11" rx="2" fill="#FFC43D" stroke="#1B2D45" stroke-width="2"/></svg>`
  };

  /* ---------- islands: n×n cells, "." sand, "~" water, "#" rock; landmarks, key and treasure sit on sand.
     Each island is drawn at random, then checked by a solver that also gives the shortest program. ---------- */
  const D = {up:[0,-1], right:[1,0], down:[0,1], left:[-1,0]}, O = ["up", "right", "down", "left"];
  const OFF = {left:[1,0], right:[-1,0], above:[0,1], below:[0,-1], west:[1,0], east:[-1,0], north:[0,1], south:[0,-1]}; // the landmark, seen from the treasure
  const at = (isl, x, y) => x < 0 || y < 0 || x >= isl.n || y >= isl.n ? "E" : isl.g[y * isl.n + x];
  const same = (a, b) => !!(a && b && a.x === b.x && a.y === b.y);
  const markAt = (isl, x, y) => isl.marks.find(m => m.x === x && m.y === y);
  const freeAt = (isl, p) => at(isl, p.x, p.y) === "." && !markAt(isl, p.x, p.y) && !same(p, isl.start) && !same(p, isl.goal) && !same(p, isl.key);
  const spot = (isl, ok = () => true) => shuffle([...Array(isl.n * isl.n).keys()]).map(i => ({x:i % isl.n, y:Math.floor(i / isl.n)})).find(p => freeAt(isl, p) && ok(p)) || null;
  const addMarks = (isl, list) => list.forEach(m => { const p = spot(isl); if (p) isl.marks.push({...p, m}); });
  function island(n, water, rocks, coast){
    const g = Array(n * n).fill(".");
    if (water) [[0, 0], [n - 1, 0], [0, n - 1], [n - 1, n - 1]].forEach(([x, y]) => g[y * n + x] = "~");
    for (let i = 0; i < n * n; i++) if (coast && (i % n === 0 || i % n === n - 1 || i < n || i >= n * n - n) && Math.random() < coast) g[i] = "~";
    for (let i = 0; i < water; i++) g[rnd(n * n)] = "~";
    for (let i = 0; i < rocks; i++) g[rnd(n * n)] = "#";
    return {n, g, marks:[]};
  }
  // breadth-first way from a to b through the cells ok() lets through (first step first)
  function path(isl, a, b, ok){
    const k = p => p.x + "," + p.y, prev = {[k(a)]: null}, q = [a];
    while (q.length) {
      const p = q.shift();
      if (same(p, b)) { const out = []; for (let c = p; prev[k(c)]; c = prev[k(c)]) out.unshift(c); return out; }
      O.forEach(d => { const nx = {x:p.x + D[d][0], y:p.y + D[d][1]}; if (!(k(nx) in prev) && ok(nx)) { prev[k(nx)] = p; q.push(nx); } });
    }
    return null;
  }
  function moves(isl, s){
    const walk = (dir, n) => { let {x, y, k} = s; for (let i = 0; i < n; i++) { x += D[dir][0]; y += D[dir][1]; if (at(isl, x, y) !== ".") return null; if (same({x, y}, isl.key)) k = 1; } return {x, y, d:s.d, k}; };
    const out = isl.turn ? [[["fwd"], walk(O[s.d], 1)], [["rep", "fwd"], walk(O[s.d], 3)], [["tl"], {...s, d:(s.d + 3) % 4}], [["tr"], {...s, d:(s.d + 1) % 4}]]
      : O.flatMap(d => [[[d], walk(d, 1)], [["rep", d], walk(d, 3)]]);
    return out.filter(m => m[1]);
  }
  // the shortest program (fewest cards), by cost over (cell, heading, key)
  function solve(isl){
    const id = s => [s.x, s.y, s.d, s.k].join(), s0 = {x:isl.start.x, y:isl.start.y, d:isl.start.d || 0, k:isl.key ? 0 : 1};
    const best = {[id(s0)]: []}, todo = [[s0]];
    for (let c = 0; c < 12; c++) for (const s of todo[c] || []) {
      const cards = best[id(s)];
      if (cards.length !== c) continue;
      if (same(s, isl.goal) && s.k) return cards;
      for (const [cs, nx] of moves(isl, s)) {
        const k = id(nx), nc = c + cs.length;
        if (best[k] && best[k].length <= nc) continue;
        best[k] = cards.concat(cs); (todo[nc] = todo[nc] || []).push(nx);
      }
    }
    return null;
  }
  // what a program does, step by step: events for the animation, and where the robot ends
  function sim(isl, prog){
    const s = {x:isl.start.x, y:isl.start.y, d:isl.start.d || 0, k:isl.key ? 0 : 1}, ev = [];
    let times = 1;
    for (let j = 0; j < prog.length; j++) {
      const c = prog[j]; ev.push({j, c});
      if (c === "rep") { times = 3; continue; }
      for (let i = 0; i < times; i++) {
        if (c === "tl" || c === "tr") { s.d = (s.d + (c === "tl" ? 3 : 1)) % 4; ev.push({turn:s.d, c}); continue; }
        const dir = c === "fwd" ? O[s.d] : c, x = s.x + D[dir][0], y = s.y + D[dir][1], g = at(isl, x, y);
        if (g === "~") { s.x = x; s.y = y; ev.push({wet:1, x, y, dir}); return {s, ev, wet:1}; }
        if (g !== ".") { ev.push({bump:g, dir}); s.bump = 1; continue; }
        s.x = x; s.y = y; ev.push({x, y, dir});
        if (!s.k && same(s, isl.key)) { s.k = 1; ev.push({key:1}); }
      }
      times = 1;
    }
    return {s, ev};
  }
  // level 1: a 4×4 island without water, three landmarks, one reachable without crossing the others
  function drive(){
    for (;;) {
      const isl = island(4, 0, 1 + rnd(2)); isl.start = spot(isl);
      addMarks(isl, pick(marks.filter(m => m.lvl === 1), 3));
      if (!isl.start || isl.marks.length < 3) continue;
      const tgt = isl.marks[rnd(3)], way = path(isl, isl.start, tgt, p => at(isl, p.x, p.y) === "." && (!markAt(isl, p.x, p.y) || same(p, tgt)));
      if (way && way.length >= 2) return {isl, tgt, key:tgt.m.en};
    }
  }
  // level 2: a way of 2 or 3 straight lines on dry land ("Go up two! Go right one! Dig!")
  const perp = d => d === "up" || d === "down" ? ["left", "right"] : ["up", "down"];
  function listen(segs){
    for (;;) {
      const isl = island(5, 1 + rnd(2), 1 + rnd(2)); isl.start = spot(isl);
      if (!isl.start) continue;
      let x = isl.start.x, y = isl.start.y, d = null, ok = true; const plan = [];
      for (let j = 0; j < segs && ok; j++) {
        d = pick(d ? perp(d) : O, 1)[0]; const len = 1 + rnd(j === 2 ? 2 : 3);
        for (let i = 0; i < len; i++) { x += D[d][0]; y += D[d][1]; if (at(isl, x, y) !== ".") ok = false; }
        plan.push([d, len]);
      }
      if (!ok || same({x, y}, isl.start)) continue;
      isl.goal = {x, y}; addMarks(isl, pick(marks.filter(m => m.lvl <= 2), 3));
      return {isl, plan, key:plan.map(([d, n]) => d + " " + n).join(", ")};
    }
  }
  // levels 3 and 4: the treasure next to one landmark (or between two), a program of 3 to 8 cards
  function clue(kinds, turn){
    for (;;) {
      const isl = turn ? island(5, 1, 1 + rnd(2)) : island(6, 1, 2 + rnd(2), .4);
      isl.turn = turn; isl.start = spot(isl); if (!isl.start) continue;
      isl.start.d = rnd(4);
      isl.goal = spot(isl, p => Math.abs(p.x - isl.start.x) + Math.abs(p.y - isl.start.y) >= 2); if (!isl.goal) continue;
      const kind = kinds[rnd(kinds.length)], [a, b] = pick(marks, 2), vert = !rnd(2);
      const cs = (kind === "between" ? (vert ? [[0, -1], [0, 1]] : [[-1, 0], [1, 0]]) : [OFF[kind]]).map(([dx, dy]) => ({x:isl.goal.x + dx, y:isl.goal.y + dy}));
      if (!cs.every(p => freeAt(isl, p))) continue;
      cs.forEach((p, i) => isl.marks.push({...p, m:[a, b][i]}));
      if (turn) isl.key = spot(isl);
      addMarks(isl, pick(marks.filter(m => m !== a && m !== b), turn ? 1 : 2));
      const sol = solve(isl);
      if (sol && sol.length >= 3 && sol.length <= (turn ? 8 : 7)) return {isl, kind, a, b:kind === "between" ? b : null, vert, sol, key:`${kind} ${a.en}` + (kind === "between" ? ` ${b.en}` : "")};
    }
  }
  // level 4: a good program with one wrong card, which stops somewhere else on dry land
  function debug(){
    for (;;) {
      const r = clue(["north", "south", "east", "west"], true);
      for (let n = 0; n < 20; n++) {
        const j = rnd(r.sol.length); if (r.sol[j] === "rep") continue;
        const bug = r.sol.slice(); bug[j] = pick(["fwd", "tl", "tr"].filter(c => c !== r.sol[j]), 1)[0];
        const out = sim(r.isl, bug);
        if (!out.wet && !out.s.bump && !same(out.s, r.isl.goal) && !same(out.s, r.isl.start)) return {...r, bug, j, end:{x:out.s.x, y:out.s.y}, key:"debug"};
      }
    }
  }
  // the clue in pictures, for the little one who does not read
  const picto = r => { const a = r.a.e, b = r.b && r.b.e, T = "💰";
    return {left:T + a, west:T + a, right:a + T, east:a + T, above:`${T}<br>${a}`, north:`${T}<br>${a}`, below:`${a}<br>${T}`, south:`${a}<br>${T}`,
      between:r.vert ? `${a}<br>${T}<br>${b}` : a + T + b}[r.kind]; };
  /* ---------- funny sounds (Web Audio): a sweep from f1 to f2, a noise burst, and what the robot and the parrot make of them ---------- */
  function au(f1, f2, dur, type = "sine", vol = .16, delay = 0){
    try {
      ac = ac || new (window.AudioContext || window.webkitAudioContext)();
      const s = ac.currentTime + delay, o = ac.createOscillator(), g = ac.createGain();
      o.type = type; o.frequency.setValueAtTime(f1, s); o.frequency.exponentialRampToValueAtTime(f2, s + dur);
      g.gain.setValueAtTime(.0001, s); g.gain.exponentialRampToValueAtTime(vol, s + .02); g.gain.exponentialRampToValueAtTime(.0001, s + dur);
      o.connect(g); g.connect(ac.destination); o.start(s); o.stop(s + dur + .05);
    } catch(e) {}
  }
  function hiss(dur, freq, delay = 0){
    try {
      ac = ac || new (window.AudioContext || window.webkitAudioContext)();
      const n = Math.floor(ac.sampleRate * dur), b = ac.createBuffer(1, n, ac.sampleRate), d = b.getChannelData(0), src = ac.createBufferSource(), f = ac.createBiquadFilter();
      for (let i = 0; i < n; i++) d[i] = (Math.random() * 2 - 1) * (1 - i / n);
      src.buffer = b; f.type = "bandpass"; f.frequency.value = freq; src.connect(f); f.connect(ac.destination); src.start(ac.currentTime + delay);
    } catch(e) {}
  }
  const SND = {
    hop: () => au(280, 560, .11, "triangle", .12), pop: () => au(700, 1400, .06, "square", .06), key: () => tone([1047, 1568, 2093], .08),
    beep: () => { au(900, 900, .08, "square", .07); au(900, 900, .08, "square", .07, .13); }, coins: () => tone([1319, 1568, 2093, 2637, 3136], .06),
    splash: () => { hiss(.7, 800); au(600, 110, .45, "sine", .2); }, dig: () => [0, .2, .4].forEach(a => hiss(.1, 350, a)),
    wah: () => { au(392, 370, .22, "sawtooth", .07); au(349, 330, .22, "sawtooth", .07, .25); au(330, 200, .5, "sawtooth", .07, .5); },
    squawk: () => { au(1500, 800, .12, "sawtooth", .08); au(1700, 900, .15, "sawtooth", .08, .14); }, burp: () => au(130, 55, .5, "sawtooth", .18),
    sneeze: () => { au(500, 1000, .3, "sine", .1); hiss(.35, 2800, .32); }, hic: () => au(500, 1300, .07, "square", .07),
    giggle: () => [0, 1, 2, 3].forEach(i => au(900 + i * 90, 1200 + i * 90, .07, "triangle", .12, i * .1)), spin: () => au(250, 1500, .5, "triangle", .14),
    toot: () => au(90, 65, .55, "sawtooth", .15), boing: () => { au(140, 620, .16, "sine", .22); au(620, 180, .3, "sine", .18, .16); }
  };
  P.isle = {D, O, at, same, markAt, path, sim, solve, drive, listen, clue, debug, picto};
  P.snd = SND;
  return P;
})();
