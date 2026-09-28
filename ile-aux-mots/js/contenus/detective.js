/* L'Île aux Mots : contenu du jeu « Détective » (en, de, lb, zh ; jamais de français côté enfant), puis son look et ses sons.
   German: objects in the accusative (einen Hut, keinen Hut, ein rotes T-Shirt).
   Luxembourgish checked on lod.lu (Déif, Kroun, iergendeen, giess, geklaut, erwëscht, weeder…nach, onschëlleg,
   Grimmelen, uff, ma, schold, Hiweis, Fro, packen, geléist, gewosst, weidersoen); accusative = nominative, Eifel rule written out
   (en Hutt, e Brëll, kee Brëll, keen Hutt, e rouden T-Shirt as in "Roude Wäin", plural adjectives bare: rout Hoer).
   key: the lexicon word(s) played for Luxembourgish, which has no voice. */
const detectiveData = {
  title: {en:"Detective", de:"Detektiv", lb:"Detektiv", zh:"小侦探"},
  sub: {en:"Who ate the cake?", de:"Wer hat den Kuchen gegessen?", lb:"Wien huet de Kuch giess?", zh:"谁吃了蛋糕？"},
  // [English, German and Luxembourgish name, Chinese name]
  names: {
    he: [["Tom","小刚"],["Ben","大伟"],["Max","小龙"],["Leo","阿杰"],["Sam","小虎"],["Jack","浩浩"],["Noah","东东"],["Oscar","小强"],["Finn","阿宝"],["Hugo","小军"],["Theo","小明"],["Paul","小勇"]],
    she: [["Lily","小美"],["Emma","小红"],["Mia","丽丽"],["Anna","小芳"],["Zoe","婷婷"],["Lucy","小雪"],["Ella","甜甜"],["Nora","小燕"],["Ruby","小梅"],["Clara","芳芳"],["Ivy","玲玲"],["Lea","小兰"]]
  },
  // what a suspect can have: k/v = attribute and value; de, lb, zh = [has, has not]
  traits: [
    {id:"hat", k:"head", v:"hat", big:1, icon:"🎩", en:"a hat", de:["einen Hut","keinen Hut"], lb:["en Hutt","keen Hutt"], zh:["戴着帽子","没戴帽子"], key:["den Hutt"]},
    {id:"crown", k:"head", v:"crown", big:1, icon:"👑", en:"a crown", de:["eine Krone","keine Krone"], lb:["eng Kroun","keng Kroun"], zh:["戴着皇冠","没戴皇冠"]},
    {id:"glasses", k:"glasses", v:true, big:1, icon:"👓", en:"glasses", de:["eine Brille","keine Brille"], lb:["e Brëll","kee Brëll"], zh:["戴着眼镜","没戴眼镜"], key:["de Brëll"]},
    {id:"scarf", k:"scarf", v:true, big:1, icon:"🧣", en:"a scarf", de:["einen Schal","keinen Schal"], lb:["e Schal","kee Schal"], zh:["围着围巾","没围围巾"], key:["de Schal"]},
    {id:"red T-shirt", k:"shirt", v:"red", col:"#E53935", en:"a red T-shirt", de:["ein rotes T-Shirt","kein rotes T-Shirt"], lb:["e rouden T-Shirt","kee rouden T-Shirt"], zh:["穿着红色的T恤","没穿红色的T恤"], key:["den T-Shirt","rout"]},
    {id:"blue T-shirt", k:"shirt", v:"blue", col:"#1E88E5", en:"a blue T-shirt", de:["ein blaues T-Shirt","kein blaues T-Shirt"], lb:["e bloen T-Shirt","kee bloen T-Shirt"], zh:["穿着蓝色的T恤","没穿蓝色的T恤"], key:["den T-Shirt","blo"]},
    {id:"green T-shirt", k:"shirt", v:"green", col:"#43A047", en:"a green T-shirt", de:["ein grünes T-Shirt","kein grünes T-Shirt"], lb:["e gréngen T-Shirt","kee gréngen T-Shirt"], zh:["穿着绿色的T恤","没穿绿色的T恤"], key:["den T-Shirt","gréng"]},
    {id:"yellow T-shirt", k:"shirt", v:"yellow", col:"#FDD835", en:"a yellow T-shirt", de:["ein gelbes T-Shirt","kein gelbes T-Shirt"], lb:["e gielen T-Shirt","kee gielen T-Shirt"], zh:["穿着黄色的T恤","没穿黄色的T恤"], key:["den T-Shirt","giel"]},
    {id:"brown hair", k:"hair", v:"brown", hair:1, col:"#7B4A26", en:"brown hair", de:["braune Haare","keine braunen Haare"], lb:["brong Hoer","keng brong Hoer"], zh:["有棕色的头发","没有棕色的头发"], key:["d'Hoer","brong"]},
    {id:"black hair", k:"hair", v:"black", hair:1, col:"#2A2A2A", en:"black hair", de:["schwarze Haare","keine schwarzen Haare"], lb:["schwaarz Hoer","keng schwaarz Hoer"], zh:["有黑色的头发","没有黑色的头发"], key:["d'Hoer","schwaarz"]},
    {id:"blond hair", k:"hair", v:"blond", hair:1, col:"#F3C747", en:"blond hair", de:["blonde Haare","keine blonden Haare"], lb:["blond Hoer","keng blond Hoer"], zh:["有金色的头发","没有金色的头发"]},
    {id:"red hair", k:"hair", v:"red", hair:1, col:"#E0662B", en:"red hair", de:["rote Haare","keine roten Haare"], lb:["rout Hoer","keng rout Hoer"], zh:["有红色的头发","没有红色的头发"], key:["d'Hoer","rout"]},
    {id:"ball", k:"hold", v:"ball", big:1, hold:1, icon:"⚽", en:"a ball", de:["einen Ball","keinen Ball"], lb:["e Ball","kee Ball"], zh:["拿着球","没拿球"], key:["de Ball"]},
    {id:"book", k:"hold", v:"book", big:1, hold:1, icon:"📖", en:"a book", de:["ein Buch","kein Buch"], lb:["e Buch","kee Buch"], zh:["拿着书","没拿书"], key:["d'Buch"]},
    {id:"flower", k:"hold", v:"flower", big:1, hold:1, icon:"🌸", en:"a flower", de:["eine Blume","keine Blume"], lb:["eng Blumm","keng Blumm"], zh:["拿着花","没拿花"], key:["d'Blumm"]},
    {id:"umbrella", k:"hold", v:"umbrella", big:1, hold:1, icon:"☂️", en:"an umbrella", de:["einen Regenschirm","keinen Regenschirm"], lb:["e Prabbeli","kee Prabbeli"], zh:["拿着雨伞","没拿雨伞"], key:["de Prabbeli"]},
    {id:"key", k:"hold", v:"key", big:1, hold:1, icon:"🔑", en:"a key", de:["einen Schlüssel","keinen Schlüssel"], lb:["e Schlëssel","kee Schlëssel"], zh:["拿着钥匙","没拿钥匙"], key:["de Schlëssel"]}
  ],
  // what went missing: lexicon words (English key); eat 0 = taken away
  crimes: [["cake",1],["cheese",1],["pizza",1],["banana",1],["cherries",1],["apple",1],["carrot",1],["bread",1],["watermelon",1],["grapes",1],["strawberry",1],["egg",1],
           ["socks",0],["shoes",0],["gloves",0],["rocket",0]],
  openers: {
    en: ["Oh no!","Help!","Oh dear!","Look!","Uh-oh!","Detective, come quick!"],
    de: ["Oh nein!","Hilfe!","Ach je!","Schau mal!","Oje!","Detektiv, komm schnell!"],
    lb: ["O nee!","Hëllef!","O jee!","Kuck!","Ups!","Detektiv, komm séier!"],
    zh: ["哎呀！","救命啊！","糟了！","快看！","不好了！","小侦探，快来！"]
  },
  // w: the lexicon word; German takes the accusative (der Kuchen → den Kuchen)
  crime: (L, w, eat) => ({
    en: `Someone ${eat ? "ate" : "took"} the ${w.en}! Who was it?`,
    de: `Jemand hat ${w.de.replace(/^der /, "den ")} ${eat ? "gegessen" : "geklaut"}! Wer war das?`,
    lb: `Iergendeen huet ${w.lb} ${eat ? "giess" : "geklaut"}! Wien war dat?`,
    zh: `有人把${w.zh}${eat ? "吃了" : "拿走了"}！是谁干的？`
  })[L],
  // clue sentences: the thief, he, she, or me (a suspect who protests)
  has: {
    en: {thief:"The thief has", he:"He has", she:"She has", me:"I've got"},
    enNot: {thief:"The thief hasn't got", he:"He hasn't got", she:"She hasn't got", me:"I haven't got"},
    de: {thief:"Der Dieb hat", he:"Er hat", she:"Sie hat", me:"Ich habe doch"},
    lb: {thief:"Den Déif huet", he:"Hien huet", she:"Si huet", me:"Ech hunn dach"},
    zh: {thief:"小偷", he:"他", she:"她", me:"我"}
  },
  // c = {t: pos|neg|and|or|nor, a, b}
  sentence(L, c, who){
    const A = c.a, B = c.b, me = who === "me";
    if (L === "zh") {
      const p = x => x.zh[0], n = x => x.zh[1];
      return this.has.zh[who] + {pos:p(A), neg:n(A), and:p(A) + "，还" + (B && p(B)), or:"要么" + p(A) + "，要么" + (B && p(B)), nor:"既" + n(A) + "，也" + (B && n(B))}[c.t] + (me ? "呀！" : "。");
    }
    const end = me ? "!" : ".";
    if (L === "en") {
      const has = this.has.en[who];
      return {pos:`${has} ${A.en}`, neg:`${this.has.enNot[who]} ${A.en}`, and:`${has} ${A.en} and ${B && B.en}`,
        or:`${has} ${A.en} or ${B && B.en}`, nor:`${has} neither ${A.en} nor ${B && B.en}`}[c.t] + end;
    }
    const o = x => x[L][0], de = L === "de";
    // Luxembourgish "an" keeps its n before a vowel or d, t, z, h, n
    const and = de ? "und" : B && /^[aeiouäéëdtzhn]/i.test(o(B)) ? "an" : "a";
    return this.has[L][who] + " " + {pos:o(A), neg:A[L][1], and:`${o(A)} ${and} ${B && o(B)}`, or:`${o(A)} oder ${B && o(B)}`,
      nor:`${de ? "weder" : "weeder"} ${o(A)} ${de ? "noch" : "nach"} ${B && o(B)}`}[c.t] + end;
  },
  // questions to the owl, built tile by tile: slots of two tiles each, frame = where the slots and the object go
  ask: {
    en: {pron:{she:"she", he:"he"}, pi:1, slots:t => [["Has","Is"], ["she","he"], ["got", t.hold ? "holding" : "wearing"]],
      best:g => ["Has", g, "got"], frame:t => [0, 1, 2, t.en + "?"],
      good:(c, t) => c[0] === "Has" ? c[2] === "got" : c[2] !== "got" && !t.hair,
      right:(g, t) => `Has ${g} got ${t.en}?`,
      reply:(isForm, g, t, yes) => isForm ? (yes ? `Yes, ${g} is!` : `No, ${g} isn't!`) : (yes ? `Yes, ${g} has!` : `No, ${g} hasn't!`)},
    de: {pron:{she:"sie", he:"er"}, pi:1, slots:() => [["Hat","Ist"], ["sie","er"]], best:g => ["Hat", g], frame:t => [0, 1, t.de[0] + "?"],
      good:c => c[0] === "Hat", right:(g, t) => `Hat ${g} ${t.de[0]}?`,
      reply:(x, g, t, yes) => yes ? `Ja, ${g} hat ${t.de[0]}!` : `Nein, ${g} hat ${t.de[1]}!`},
    lb: {pron:{she:"si", he:"hien"}, pi:1, slots:() => [["Huet","Ass"], ["si","hien"]], best:g => ["Huet", g], frame:t => [0, 1, t.lb[0] + "?"],
      good:c => c[0] === "Huet", right:(g, t) => `Huet ${g} ${t.lb[0]}?`,
      reply:(x, g, t, yes) => yes ? `Jo, ${g} huet ${t.lb[0]}!` : `Nee, ${g} huet ${t.lb[1]}!`},
    zh: {pron:{she:"她", he:"他"}, pi:0, slots:() => [["她","他"], ["吗","呢"]], best:g => [g, "吗"], frame:t => [0, t.zh[0], 1, "？"],
      good:c => c[1] === "吗", right:(g, t) => `${g}${t.zh[0]}吗？`,
      reply:(x, g, t, yes) => yes ? `对，${g}${t.zh[0]}！` : `不，${g}${t.zh[1]}！`}
  },
  // the thief owns up (same order in every language)
  confessEat: {
    en: ["Yes, it was me!","Sorry! It was so yummy!","Oops! You caught me!","I only wanted a tiny piece!","I was so hungry!","Crumbs? What crumbs?",
         "It wasn't me! … OK, it was me.","I'll never do it again! Maybe.","Is there any more?","My nose made me do it!","Mmm… it was delicious!","Sorry! Please don't tell anyone!"],
    de: ["Ja, ich war's!","Entschuldigung! Es war so lecker!","Ups! Du hast mich erwischt!","Ich wollte nur ein kleines Stück!","Ich hatte so einen Hunger!","Krümel? Welche Krümel?",
         "Ich war's nicht! … Na gut, ich war's.","Ich mach's nie wieder! Vielleicht.","Gibt es noch mehr?","Meine Nase war schuld!","Mmm… das war köstlich!","Tut mir leid! Nicht weitersagen!"],
    lb: ["Jo, ech war et!","Pardon! Et war esou lecker!","Ups! Du hues mech erwëscht!","Ech wollt nëmmen e klengt Stéck!","Ech hat esou en Honger!","Grimmelen? Wat fir Grimmelen?",
         "Ech war et net! … Majo, ech war et.","Ech maachen dat ni méi! Vläicht.","Gëtt et nach méi?","Meng Nues war schold!","Mmm… dat war lecker!","Pardon! Net weidersoen!"],
    zh: ["对，是我吃的！","对不起！太好吃了！","哎呀！被你抓到了！","我只想吃一小口！","我太饿了！","渣渣？什么渣渣？",
         "不是我！……好吧，是我。","我再也不敢了！大概吧。","还有吗？我还想吃！","都怪我的鼻子！","嗯……真好吃！","对不起！别告诉别人！"]
  },
  confessTake: {
    en: ["Yes, it was me! I just wanted to play!","Oops! I'll give it all back!","You caught me! You're a super detective!","It was a joke! A funny joke!","Sorry! I'll never do it again!","Hmm… how did you know?"],
    de: ["Ja, ich war's! Ich wollte nur spielen!","Ups! Ich gebe alles zurück!","Du hast mich erwischt! Du bist ein super Detektiv!","Das war ein Witz! Ein lustiger Witz!","Entschuldigung! Ich mach's nie wieder!","Hm… woher wusstest du das?"],
    lb: ["Jo, ech war et! Ech wollt just spillen!","Ups! Ech ginn alles zeréck!","Du hues mech erwëscht! Du bass e super Detektiv!","Dat war e Witz! E flotte Witz!","Pardon! Ech maachen dat ni méi!","Hm… wéi hues du dat gewosst?"],
    zh: ["对，是我！我只是想玩一玩！","哎呀！我全都还给你！","被你抓到了！你真是个超级侦探！","我只是开个玩笑！","对不起！我再也不敢了！","嗯……你怎么知道的？"]
  },
  // a suspect ruled out, rightly
  notMe: {
    en: ["Not me!","Phew!","It wasn't me!","I'm innocent!","Bye-bye!","Not me, not me!"],
    de: ["Ich nicht!","Puh!","Ich war's nicht!","Ich bin unschuldig!","Tschüss!","Ich nicht, ich nicht!"],
    lb: ["Ech net!","Uff!","Ech war et net!","Ech sinn onschëlleg!","Äddi!","Ech net, ech net!"],
    zh: ["不是我！","呼！","不是我干的！","我是无辜的！","拜拜！","不是我，不是我！"]
  },
  ui: {
    en: {again:"Again", next:"Next clue", clue:"Clue", notIt:"Who is it NOT?", who:"Who is it?", catch:"Catch the thief!", look:"Look again!",
      got:"Got you!", closed:"CASE CLOSED", ask:"Ask me a question!", challenge:"Can you do it with three questions?", super:"Super detective!",
      mean:"Hmm? You mean:", hey:"Hey!", no:"Not me!", boy:"I'm a boy!", girl:"I'm a girl!",
      saw:{she:"I saw the thief! It was a girl!", he:"I saw the thief! It was a boy!"},
      praise:["Well done, detective!","Great detective work!","Brilliant!","Super sleuth!"]},
    de: {again:"Nochmal", next:"Nächster Hinweis", clue:"Hinweis", notIt:"Wer ist es NICHT?", who:"Wer ist es?", catch:"Fang den Dieb!", look:"Schau noch mal!",
      got:"Erwischt!", closed:"FALL GELÖST", ask:"Stell mir eine Frage!", challenge:"Schaffst du es mit drei Fragen?", super:"Super-Detektiv!",
      mean:"Hm? Du meinst:", hey:"He!", no:"Ich nicht!", boy:"Ich bin ein Junge!", girl:"Ich bin ein Mädchen!",
      saw:{she:"Ich habe den Dieb gesehen! Es war ein Mädchen!", he:"Ich habe den Dieb gesehen! Es war ein Junge!"},
      praise:["Gut gemacht, Detektiv!","Super, Detektiv!","Klasse!","Toll gemacht!"]},
    lb: {again:"Nach eng Kéier", next:"Nächsten Hiweis", clue:"Hiweis", notIt:"Wien ass et NET?", who:"Wien ass et?", catch:"Fänk den Déif!", look:"Kuck nach eng Kéier!",
      got:"Erwëscht!", closed:"FALL GELÉIST", ask:"Fro mech eppes!", challenge:"Packs du et mat dräi Froen?", super:"Super Detektiv!",
      mean:"Hm? Du mengs:", hey:"Ma nee!", no:"Ech net!", boy:"Ech sinn e Jong!", girl:"Ech sinn e Meedchen!",
      saw:{she:"Ech hunn den Déif gesinn! Et war e Meedchen!", he:"Ech hunn den Déif gesinn! Et war e Jong!"},
      praise:["Gutt gemaach, Detektiv!","Super, Detektiv!","Flott!","Bravo!"]},
    zh: {again:"再听一次", next:"下一条线索", clue:"线索", notIt:"谁不是小偷？", who:"是谁呢？", catch:"快抓住小偷！", look:"再仔细看看！",
      got:"抓到了！", closed:"结案！", ask:"问我一个问题吧！", challenge:"你能只问三个问题就找到小偷吗？", super:"超级侦探！",
      mean:"嗯？你是想问：", hey:"喂！", no:"不是我！", boy:"我是男孩！", girl:"我是女孩！",
      saw:{she:"我看见小偷了！是个女孩！", he:"我看见小偷了！是个男孩！"},
      praise:["干得好，小侦探！","真是个好侦探！","太棒了！","真厉害！"]}
  }
};

/* ---------- look and sound: styles, original SVG drawings (no protected character), sound sweeps ---------- */
addStyle(`
.dt{display:flex; flex-direction:column; gap:10px}
.dt-top{display:flex; align-items:center; gap:8px}
.dt-owl{flex:none; width:62px}
.dt-owl svg{width:62px; height:66px; display:block; overflow:visible}
.dt g,.dt path,.dt svg{transform-box:fill-box; transform-origin:center}
.dt-owlb{transform-origin:50% 100%!important}
.dt-bub{flex:1; min-width:0; position:relative; background:#fff; border:3px solid var(--ink); border-radius:16px; padding:6px 10px; min-height:58px;
  display:flex; flex-direction:column; justify-content:center; font:600 18px/1.25 var(--display)}
.dt-bub::before{content:""; position:absolute; left:-11px; top:18px; border:9px solid transparent; border-right-color:var(--ink); border-left:0}
.dt-bub small{font:700 13px/1.2 var(--body); color:var(--ink-soft)}
.dt-bub .dt-n{font:800 12px var(--body); color:var(--sea-deep)}
.dt-bub .dt-note{color:var(--bad); font-size:15px}
.dt-ev{font-size:34px; line-height:1}
.dt-hi{display:inline-flex; gap:6px; align-items:center; font-size:30px; vertical-align:middle}
.dt-ic{width:34px; height:34px; display:block}
.dt .speak{flex:none; flex-direction:column; gap:0; padding:6px 8px; min-width:64px; max-width:92px; min-height:64px; font-size:13px; line-height:1.1; text-align:center}
.dt .speak b{font-size:24px}
.dt-gw{position:relative}
.dt-grid{display:grid; grid-template-columns:repeat(var(--c),1fr); gap:6px; width:100%; max-width:calc(var(--c) * 150px); margin:0 auto}
.dt-s{position:relative; background:#fff; border:2.5px solid var(--ink); border-radius:14px; box-shadow:2px 3px 0 var(--ink); padding:2px 2px 3px;
  display:flex; flex-direction:column; align-items:center; min-width:0; min-height:64px; touch-action:manipulation}
.dt-s svg{width:100%; height:auto; display:block; overflow:visible; transform-origin:50% 100%; animation:dtSway 3.2s ease-in-out infinite; animation-delay:var(--d,0s)}
.dt-nm{font:600 13px/1.1 var(--display); white-space:nowrap}
.dt-eyes,.dt-oe{animation:dtBlink 4.2s infinite; animation-delay:var(--d,0s)}
.dt-back{opacity:0} .dt-s.out{opacity:.42} .dt-s.out .dt-face{opacity:0} .dt-s.out .dt-back{opacity:1} .dt-s.out svg{animation:none}
.dt-say{position:absolute; left:50%; top:2px; transform:translate(-50%,0); background:#fff; border:2px solid var(--ink); border-radius:12px; padding:3px 7px;
  font:800 12px/1.2 var(--body); width:max-content; max-width:150px; z-index:6; pointer-events:none; box-shadow:2px 2px 0 var(--ink); text-align:center}
.dt-siren{position:absolute; top:-16px; left:50%; transform:translateX(-50%); font-size:26px; z-index:7; pointer-events:none}
.dt-stamp{position:absolute; left:50%; top:38%; transform:translate(-50%,-50%) rotate(-12deg); font:700 26px var(--display); color:#C62828; border:4px solid #C62828;
  border-radius:10px; padding:4px 12px; background:rgba(255,255,255,.88); pointer-events:none; white-space:nowrap; z-index:8}
.dt-hint{animation:dtHop .8s ease-in-out infinite}
.dt-ctl{display:flex; flex-direction:column; gap:10px; align-items:center}
.dt-next{font:600 20px var(--display); padding:10px 18px; min-height:64px; background:var(--leaf); color:#fff}
.dt-go{animation:dtPulse 1s ease-in-out infinite}
.dt-chips,.dt-tiles{display:flex; flex-wrap:wrap; gap:8px; justify-content:center}
.dt-chip{width:64px; height:64px; font-size:32px; background:#fff; border:3px solid var(--ink); border-radius:16px; box-shadow:2px 3px 0 var(--ink); display:grid; place-items:center}
.dt-tile{min-width:96px; min-height:64px; padding:6px 14px; font:600 26px var(--display); background:#fff}
.dt-q{display:flex; flex-wrap:wrap; gap:6px; justify-content:center; align-items:center; font:600 21px var(--display)}
.dt-q .sl{min-width:48px; min-height:34px; padding:2px 8px; border:2px dashed var(--ink); border-radius:10px; background:#fff}
.dt-q .sl.now{border-style:solid; background:var(--sun)}
.dt-mine{background:#E3F2FD; border:2px solid var(--ink); border-radius:14px; padding:8px 12px}
.dt.calm *{animation:none!important}
@keyframes dtBlink{0%,93%,100%{transform:scaleY(1)} 96%{transform:scaleY(.1)}}
@keyframes dtSway{50%{transform:rotate(2deg)}}
@keyframes dtHop{50%{transform:translateY(-8px)}}
@keyframes dtPulse{50%{transform:scale(1.07)}}
`);
const detectiveArt = (() => {
const INK = "#1B2D45", TR = detectiveData.traits;
/* ---------- drawings: original SVG, one suspect in a 100 × 112 box ---------- */
const SKIN = ["#FFE0C2","#F5C7A0","#E3A878","#B97A4E","#8A5A3B"], SCARF = ["#8E24AA","#FB8C00","#F48FB1"];
const COL = {red:"#E53935", blue:"#1E88E5", green:"#43A047", yellow:"#FDD835"}, HAIR = {brown:"#7B4A26", black:"#2A2A2A", blond:"#F3C747", red:"#E0662B"};
const HAIRDO = {short:"M25 48Q24 24 50 24Q76 24 75 48Q68 34 50 36Q32 34 25 48Z", bun:"M25 48Q24 24 50 24Q76 24 75 48Q68 34 50 36Q32 34 25 48Z",
  spiky:"M26 46L27 30L35 33L38 21L46 30L52 18L58 29L66 22L67 32L75 31L74 46Q64 34 50 36Q36 34 26 46Z",
  bob:"M22 64Q18 24 50 24Q82 24 78 64L70 64Q72 38 50 36Q28 38 30 64Z", long:"M26 50Q26 24 50 24Q74 24 74 50Q66 36 50 37Q34 36 26 50Z"};
const ST = `stroke="${INK}" stroke-width="3" stroke-linejoin="round"`;
function face(s){
  const h = HAIR[s.hair], sk = s.skin, t = TR.find(x => x.k === "hold" && x.v === s.hold);
  const back = s.style === "long" ? `<path d="M23 52Q20 22 50 22Q80 22 77 52L80 94Q66 100 58 90H42Q34 100 20 94Z" fill="${h}" ${ST}/>`
    : s.style === "bun" ? `<circle cx="50" cy="20" r="10" fill="${h}" ${ST}/>` : "";
  const front = s.style === "curly" ? `<g fill="${h}" stroke="${INK}" stroke-width="2.5">${[[26,42],[30,31],[38,26],[46,23],[54,23],[62,26],[70,31],[74,42]].map(([x, y]) => `<circle cx="${x}" cy="${y}" r="8"/>`).join("")}</g>`
    : `<path d="${HAIRDO[s.style]}" fill="${h}" ${ST}/>`;
  return `<svg viewBox="0 0 100 112" aria-hidden="true">${back}
<path d="M14 112Q16 82 50 80Q84 82 86 112Z" fill="${COL[s.shirt]}" ${ST}/>
${s.scarf ? `<path d="M30 76Q50 88 70 76L72 84Q50 98 28 84Z" fill="${s.sc}" ${ST}/><path d="M57 86H67L65 104L56 102Z" fill="${s.sc}" ${ST}/>` : ""}
<circle cx="25" cy="53" r="5" fill="${sk}" ${ST}/><circle cx="75" cy="53" r="5" fill="${sk}" ${ST}/><circle cx="50" cy="50" r="25" fill="${sk}" ${ST}/>${front}
<g class="dt-face"><g class="dt-eyes"><ellipse cx="41" cy="51" rx="3.3" ry="4.2" fill="${INK}"/><ellipse cx="59" cy="51" rx="3.3" ry="4.2" fill="${INK}"/><circle cx="42.2" cy="49.5" r="1.2" fill="#fff"/><circle cx="60.2" cy="49.5" r="1.2" fill="#fff"/></g>
${s.g === "she" ? `<path d="M37 47l-3-2.5M40 45.5l-1-3M63 47l3-2.5M60 45.5l1-3" stroke="${INK}" stroke-width="1.8" stroke-linecap="round"/>` : ""}
<g class="dt-ch" fill="#FF7B7B" opacity=".45"><ellipse cx="34" cy="60" rx="5" ry="3.2"/><ellipse cx="66" cy="60" rx="5" ry="3.2"/></g>
<path class="dt-mo" d="M43 63Q50 70 57 63" fill="none" stroke="${INK}" stroke-width="2.6" stroke-linecap="round"/>
<g class="dt-cr" opacity="0"><path d="M44 66Q50 72 57 66" stroke="#fff" stroke-width="3" fill="none"/><g fill="#B7793F"><circle cx="43" cy="70" r="1.8"/><circle cx="51" cy="73" r="1.5"/><circle cx="58" cy="70" r="1.4"/><circle cx="39" cy="66" r="1.2"/><circle cx="61" cy="75" r="1.1"/></g></g>
${s.glasses ? `<g fill="rgba(255,255,255,.3)" stroke="${INK}" stroke-width="2.4"><circle cx="41" cy="51" r="8"/><circle cx="59" cy="51" r="8"/><path d="M49 50h2M33 49l-8-2M67 49l8-2" fill="none"/></g>` : ""}</g>
<g class="dt-back"><circle cx="50" cy="50" r="26" fill="${h}" ${ST}/><path d="M50 36q9 5 1 12q-9 4-10-5M36 58q14 8 28 0" fill="none" stroke="${INK}" stroke-width="2" opacity=".45"/></g>
${s.head === "hat" ? `<g ${ST}><rect x="31" y="4" width="38" height="26" rx="4" fill="#37474F"/><rect x="31" y="21" width="38" height="6" fill="#FFC43D"/><rect x="21" y="27" width="58" height="7" rx="3.5" fill="#37474F"/></g>` : ""}
${s.head === "crown" ? `<path d="M30 33L28 12L39 21L50 7L61 21L72 12L70 33Z" fill="#FFD54F" ${ST}/><circle cx="50" cy="26" r="3" fill="#E53935"/><circle cx="38" cy="28" r="2.2" fill="#1E88E5"/><circle cx="62" cy="28" r="2.2" fill="#43A047"/>` : ""}
${t ? `<text x="85" y="104" font-size="24" text-anchor="middle">${t.icon}</text><circle cx="79" cy="102" r="5.5" fill="${sk}" ${ST}/>` : ""}</svg>`;
}
const OWL = `<svg viewBox="0 0 80 84" aria-hidden="true"><g class="dt-owlb">
<path d="M18 16L27 33L35 22ZM62 16L53 33L45 22Z" fill="#8D6E63" ${ST}/>
<path class="dt-wl" d="M18 42Q6 58 17 74Q24 60 24 46Z" fill="#8D6E63" ${ST}/><path class="dt-wr" d="M62 42Q74 58 63 74Q56 60 56 46Z" fill="#8D6E63" ${ST}/>
<ellipse cx="40" cy="48" rx="24" ry="28" fill="#A1887F" ${ST}/><ellipse cx="40" cy="61" rx="14" ry="13" fill="#EFE3D8"/>
<path d="M33 57q2 2 4 0M43 57q2 2 4 0M38 64q2 2 4 0" stroke="#A1887F" stroke-width="2" fill="none"/>
<circle cx="30" cy="38" r="11" fill="#fff" ${ST}/><circle cx="50" cy="38" r="11" fill="#fff" ${ST}/>
<g class="dt-oe"><circle cx="31" cy="39" r="5" fill="${INK}"/><circle cx="49" cy="39" r="5" fill="${INK}"/><circle cx="32.5" cy="37" r="1.6" fill="#fff"/><circle cx="50.5" cy="37" r="1.6" fill="#fff"/></g>
<path class="dt-bk" d="M36 47H44L40 55Z" fill="#FFB300" ${ST}/><path d="M31 76v5M35 76v5M45 76v5M49 76v5" stroke="#FFB300" stroke-width="3" stroke-linecap="round"/></g></svg>`;
// a trait as a picture: T-shirt and hair drawn in their colour
function icon(t){
  if (t.k === "shirt") return `<svg class="dt-ic" viewBox="0 0 40 40"><path d="M13 6L4 12L8 20L12 17V35H28V17L32 20L36 12L27 6Q20 12 13 6Z" fill="${t.col}" ${ST}/></svg>`;
  if (t.hair) return `<svg class="dt-ic" viewBox="0 0 40 40"><circle cx="20" cy="24" r="11" fill="#F5C7A0" ${ST}/><path d="M6 30Q3 6 20 6Q37 6 34 30Q30 17 20 17Q10 17 6 30Z" fill="${t.col}" ${ST}/></svg>`;
  return t.icon;
}
/* ---------- sounds: sweeps on the shared audio context ---------- */
function sweep(f1, f2, d, type = "sine", v = .18, at = 0){
  if (TEST) return;
  try {
    ac = ac || new (window.AudioContext || window.webkitAudioContext)();
    const t = ac.currentTime + at, o = ac.createOscillator(), g = ac.createGain();
    o.type = type; o.frequency.setValueAtTime(f1, t); o.frequency.exponentialRampToValueAtTime(f2, t + d);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(v, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + d);
    o.connect(g); g.connect(ac.destination); o.start(t); o.stop(t + d + .05);
  } catch(e) {}
}
const snd = {
  pouet: () => { sweep(620, 300, .22, "square", .07); },
  honk: () => { sweep(260, 230, .14, "square", .09); sweep(260, 200, .2, "square", .09, .17); },
  boing: () => { sweep(150, 620, .12); sweep(620, 180, .32, "sine", .2, .12); },
  siren: () => { for (let i = 0; i < 3; i++) { sweep(620, 980, .3, "sawtooth", .05, i * .6); sweep(980, 620, .3, "sawtooth", .05, i * .6 + .3); } },
  burp: () => { sweep(150, 55, .5, "sawtooth", .2); sweep(95, 60, .4, "square", .07, .06); },
  hoot: () => { sweep(440, 390, .2, "sine", .2); sweep(440, 350, .36, "sine", .2, .28); },
  thud: () => sweep(190, 45, .28, "triangle", .35),
  pop: () => sweep(380, 1300, .07, "square", .05),
  step: () => { sweep(900, 600, .04, "square", .03); sweep(800, 500, .04, "square", .03, .12); },
  hee: () => sweep(700, 1100, .1, "triangle", .1)
};
return {face, OWL, icon, snd, SKIN, SCARF, COL, HAIR};
})();
