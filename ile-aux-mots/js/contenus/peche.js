/* L'Île aux Mots : contenu de « La Pêche aux mots » (jeu peche).
   Ce que l'enfant voit et entend, en anglais, allemand, luxembourgeois et chinois, jamais en français ;
   les mots de la mer absents du lexique ; les dessins SVG (capitaine, poisson, seau, bouée).
   Allemand : le lexique donne le nominatif, pecheAcc fait l'accusatif avec deAkk (der → den, noms faibles : den Löwen).
   Luxembourgeois vérifié sur lod.lu (fänk!, huel!, wëll, wat fir e, Zuel, mol, Äntwert, Stiwwel, Dous, Krabb, Hai,
   Crevette, Tëntefësch, Muschel, Anker, Schëff, Insel, Well, Plage, Aangel, kribbelen, gefaangen, Fang) ;
   le mot du lexique reste entier dans la phrase pour que son enregistrement lod.lu soit joué, règle de l'Eifel par pecheEifel. */
const pecheAcc = w => deAkk(w); // German accusative: den Hund, and weak nouns den Löwen, den Affen (js/langues.js)
// Luxembourgish Eifel rule: a final n stays only before a vowel or d, t, z, h, n
const pecheEifel = (w, next) => /n$/.test(w) && !/^[aeiouäéëdtzhn]/i.test(next) ? w.slice(0, -1) : w;

const PECHE_TXT = {
  en: {
    title: "Word Fishing", sub: "Catch the right fish!", again: "Again",
    hello: "Ahoy! Let's go fishing!", where: "Where shall we fish?",
    catch: [w => `Catch the ${w}!`, w => `Quick, catch the ${w}!`, w => `Fish out the ${w}!`, w => `I want the ${w}!`],
    color: c => `Which fish is ${c}?`, action: v => `Which fish wants to ${v}?`,
    number: n => `Catch number ${n}!`, times: (a, b) => `What is ${a} times ${b}?`, result: (a, b, c) => `${a} times ${b} is ${c}!`,
    thats: w => `That's the ${w}.`, thatsColor: c => `This fish is ${c}.`, thatsAction: v => `This fish wants to ${v}.`, thatsNumber: n => `That's ${n}.`,
    praise: ["Great catch!", "What a catch!", "Got it!", "Super!", "Hooray!", "Yes!"],
    nope: ["Not me!", "Blub!", "Nope!", "That tickles!", "Hee hee!", "Wrong fish!"],
    captain: ["Ahoy!", "I love fishing!", "Hee hee, that tickles!", "Where are the fish?", "Blub, blub!", "What a lovely day!"],
    sneeze: "Atchoo!",
    junk: {boot: "Yuck! An old boot!", can: "Yuck! An old tin can!", umbrella: "Oh no! An old umbrella!"}
  },
  de: {
    title: "Wörterangeln", sub: "Fang den richtigen Fisch!", again: "Nochmal",
    hello: "Ahoi! Wir gehen angeln!", where: "Wo angeln wir?",
    catch: [w => `Fang ${w}!`, w => `Schnell, fang ${w}!`, w => `Hol ${w} aus dem Wasser!`, w => `Ich will ${w}!`],
    color: c => `Welcher Fisch ist ${c}?`, action: v => `Welcher Fisch will ${v}?`,
    number: n => `Fang die Zahl ${n}!`, times: (a, b) => `Wie viel ist ${a} mal ${b}?`, result: (a, b, c) => `${a} mal ${b} ist ${c}!`,
    thats: w => `Das ist ${w}.`, thatsColor: c => `Dieser Fisch ist ${c}.`, thatsAction: v => `Dieser Fisch will ${v}.`, thatsNumber: n => `Das ist ${n}.`,
    praise: ["Toller Fang!", "Gut gefangen!", "Super!", "Hurra!", "Petri Heil!", "Ja!"],
    nope: ["Ich nicht!", "Blubb!", "Nö!", "Das kitzelt!", "Hihi!", "Falscher Fisch!"],
    captain: ["Ahoi!", "Ich angle so gern!", "Hihi, das kitzelt!", "Wo sind die Fische?", "Blubb, blubb!", "Was für ein schöner Tag!"],
    sneeze: "Hatschi!",
    junk: {boot: "Igitt! Ein alter Stiefel!", can: "Igitt! Eine alte Dose!", umbrella: "Oje! Ein alter Regenschirm!"}
  },
  lb: {
    title: "Wierder fëschen", sub: "Fänk de richtege Fësch!", again: "Nach eng Kéier",
    hello: "Ahoi! Mir gi fëschen!", where: "Wou fësche mer?",
    catch: [w => `Fänk ${w}!`, w => `Séier, fänk ${w}!`, w => `Huel ${w} aus dem Waasser!`, w => `Ech wëll ${w}!`],
    color: c => `Wat fir e Fësch ass ${c}?`, action: v => `Wat fir e Fësch wëll ${v}?`,
    number: n => `Fänk d'Zuel ${n}!`, times: (a, b) => `Wéi vill ass ${pecheEifel(a, "mol")} mol ${b}?`,
    result: (a, b, c) => `${pecheEifel(a, "mol")} mol ${b} ass ${c}!`,
    thats: w => `Dat ass ${w}.`, thatsColor: c => `Dee Fësch ass ${c}.`, thatsAction: v => `Dee Fësch wëll ${v}.`, thatsNumber: n => `Dat ass ${n}.`,
    praise: ["Gutt gefaangen!", "Wat e Fang!", "Super!", "Bravo!", "Flott!", "Jo!"],
    nope: ["Ech net!", "Blubb!", "Nee!", "Dat kribbelt!", "Hihi!", "Ech sinn et net!"],
    captain: ["Ahoi!", "Ech fëschen esou gär!", "Hihi, dat kribbelt!", "Wou sinn d'Fësch?", "Blubb, blubb!", "Wat e schéinen Dag!"],
    sneeze: "Hatschi!",
    junk: {boot: "Eekleg! En ale Stiwwel!", can: "Eekleg! Eng al Dous!", umbrella: "Oh nee! En ale Prabbeli!"}
  },
  zh: {
    title: "钓单词", sub: "把对的鱼钓上来！", again: "再听一次",
    hello: "你好！我们去钓鱼吧！", where: "我们去哪儿钓鱼？",
    catch: [w => `把${w}钓上来！`, w => `快，把${w}钓上来！`, w => `把${w}从水里钓出来！`, w => `我要${w}！`],
    color: c => `哪条鱼是${c}的？`, action: v => `哪条鱼想${v}？`,
    number: n => `把数字${n}钓上来！`, times: (a, b) => `${a}乘${b}等于几？`, result: (a, b, c) => `${a}乘${b}等于${c}！`,
    thats: w => `这是${w}。`, thatsColor: c => `这条鱼是${c}的。`, thatsAction: v => `这条鱼想${v}。`, thatsNumber: n => `这是${n}。`,
    praise: ["钓到了！", "好厉害！", "太棒了！", "抓到啦！", "真棒！", "对了！"],
    nope: ["不是我！", "咕噜咕噜！", "才不是呢！", "好痒呀！", "嘻嘻！", "钓错啦！"],
    captain: ["你好！", "我最爱钓鱼！", "嘻嘻，好痒！", "鱼儿在哪儿？", "咕噜，咕噜！", "今天天气真好！"],
    sneeze: "阿嚏！",
    junk: {boot: "哎呀！一只旧靴子！", can: "哎呀！一个旧罐头！", umbrella: "哎呀！一把旧雨伞！"}
  }
};

// the sea pond: words the lexicon lacks, plus its sea animals (by English name); German and Luxembourgish with their article
const PECHE_SEA = [
  {e: "🦀", en: "crab", de: "die Krabbe", lb: "d'Krabb", zh: "螃蟹", lvl: 1},
  {e: "🦈", en: "shark", de: "der Hai", lb: "den Hai", zh: "鲨鱼", lvl: 1},
  {e: "🐚", en: "shell", de: "die Muschel", lb: "d'Muschel", zh: "贝壳", lvl: 1},
  {e: "🚢", en: "ship", de: "das Schiff", lb: "d'Schëff", zh: "轮船", lvl: 1},
  {e: "🏝️", en: "island", de: "die Insel", lb: "d'Insel", zh: "小岛", lvl: 2},
  {e: "🌊", en: "wave", de: "die Welle", lb: "d'Well", zh: "海浪", lvl: 2},
  {e: "⚓", en: "anchor", de: "der Anker", lb: "den Anker", zh: "船锚", lvl: 2},
  {e: "🏖️", en: "beach", de: "der Strand", lb: "d'Plage", zh: "沙滩", lvl: 2},
  {e: "🦐", en: "shrimp", de: "die Garnele", lb: "d'Crevette", zh: "虾", lvl: 2},
  {e: "🎣", en: "fishing rod", de: "die Angel", lb: "d'Aangel", zh: "鱼竿", lvl: 3},
  {e: "🦑", en: "squid", de: "der Tintenfisch", lb: "den Tëntefësch", zh: "鱿鱼", lvl: 3}
];
const PECHE_SEA_LEX = ["duck", "frog", "octopus", "dolphin", "whale", "turtle", "penguin"];
// never together in one round: they look or sound too much alike
const PECHE_CLASH = [["boat", "ship"], ["yellow", "gold"], ["grey", "silver"], ["white", "silver"]];
// what the rod may bring up instead of a fish
const PECHE_JUNK = [{e: "🥾", k: "boot"}, {e: "🥫", k: "can"}, {e: "☂️", k: "umbrella"}];
// body colours of the fish, with a darker shade for fins and tail
const PECHE_PAL = [["#FF8A65", "#E64A19"], ["#FFD54F", "#F9A825"], ["#81C784", "#388E3C"], ["#64B5F6", "#1976D2"],
  ["#BA68C8", "#7B1FA2"], ["#F06292", "#C2185B"], ["#4DD0E1", "#0097A7"], ["#AED581", "#689F38"]];

const PECHE_ART = {
  // a round fish facing right; its tail wiggles, its eye blinks, its tongue shows when it teases
  fish: (c, d) => `<svg viewBox="0 0 100 70" aria-hidden="true">
<g class="pc-tail"><path d="M22 35 L2 13 Q10 35 2 57Z" fill="${d}" stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"/></g>
<path d="M36 14 Q52 -2 70 13" fill="${d}" stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"/>
<ellipse cx="56" cy="36" rx="40" ry="25" fill="${c}" stroke="#1B2D45" stroke-width="3"/>
<ellipse cx="58" cy="47" rx="24" ry="7" fill="#fff" opacity=".22"/>
<circle cx="85" cy="42" r="4" fill="#FF8A80" opacity=".85"/>
<g class="pc-eye"><circle cx="82" cy="27" r="8" fill="#fff" stroke="#1B2D45" stroke-width="2.5"/><circle cx="84" cy="28" r="4" fill="#1B2D45"/><circle cx="85.5" cy="26.5" r="1.3" fill="#fff"/></g>
<path d="M88 42 Q93 46 97 40" fill="none" stroke="#1B2D45" stroke-width="2.5" stroke-linecap="round"/>
<ellipse class="pc-tongue" cx="95" cy="47" rx="4" ry="6" fill="#FF6F91" stroke="#1B2D45" stroke-width="2"/>
</svg>`,
  // the captain in his boat, rod in hand; faces: eyes, joy, ouch, talk, yuck
  captain: `<svg viewBox="0 0 170 110" aria-hidden="true">
<path d="M96 70 L164 20" stroke="#6D4C41" stroke-width="5" stroke-linecap="round"/>
<g class="pc-idle"><line x1="164" y1="20" x2="164" y2="98" stroke="#1B2D45" stroke-width="1.5"/><circle cx="164" cy="99" r="5" fill="#E53935" stroke="#1B2D45" stroke-width="2"/></g>
<circle class="pc-tip" cx="164" cy="20" r="2.5" fill="#1B2D45"/>
<ellipse cx="72" cy="82" rx="28" ry="20" fill="#FFC43D" stroke="#1B2D45" stroke-width="3"/>
<circle cx="72" cy="50" r="22" fill="#FFD9B3" stroke="#1B2D45" stroke-width="3"/>
<path d="M51 52 Q53 77 72 79 Q91 77 93 52 Q85 63 72 63 Q59 63 51 52Z" fill="#fff" stroke="#1B2D45" stroke-width="2.5" stroke-linejoin="round"/>
<path d="M49 40 Q72 12 95 40Z" fill="#1E88E5" stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"/>
<rect x="46" y="37" width="52" height="7" rx="3.5" fill="#fff" stroke="#1B2D45" stroke-width="2.5"/>
<circle cx="72" cy="17" r="4.5" fill="#E53935" stroke="#1B2D45" stroke-width="2"/>
<circle class="pc-cheek" cx="58" cy="57" r="4.5" fill="#FF8A80"/><circle class="pc-cheek" cx="86" cy="57" r="4.5" fill="#FF8A80"/>
<g class="pc-green"><circle cx="58" cy="57" r="6" fill="#9CCC65"/><circle cx="86" cy="57" r="6" fill="#9CCC65"/></g>
<g class="pc-eyes"><ellipse cx="64" cy="50" rx="3" ry="4" fill="#1B2D45"/><ellipse cx="80" cy="50" rx="3" ry="4" fill="#1B2D45"/></g>
<g class="pc-joy" fill="none" stroke="#1B2D45" stroke-width="2.5" stroke-linecap="round"><path d="M60 51 Q64 45 68 51"/><path d="M76 51 Q80 45 84 51"/></g>
<g class="pc-ouch" fill="none" stroke="#1B2D45" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><path d="M60 47 L67 50 L60 53"/><path d="M84 47 L77 50 L84 53"/></g>
<circle cx="72" cy="56" r="4.5" fill="#FF7043" stroke="#1B2D45" stroke-width="2"/>
<path class="pc-smile" d="M66 66 Q72 71 78 66" fill="none" stroke="#1B2D45" stroke-width="2.5" stroke-linecap="round"/>
<ellipse class="pc-talkm" cx="72" cy="67" rx="4.5" ry="3.5" fill="#7B1F1F" stroke="#1B2D45" stroke-width="2"/>
<ellipse class="pc-ctongue" cx="72" cy="72" rx="3.5" ry="5" fill="#FF6F91" stroke="#1B2D45" stroke-width="2"/>
<circle cx="96" cy="70" r="6" fill="#FFD9B3" stroke="#1B2D45" stroke-width="2.5"/>
<path d="M6 86 Q12 108 42 108 L112 108 Q138 108 146 86Z" fill="#C0643A" stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"/>
<path d="M14 95 L139 95" stroke="#FFE0B2" stroke-width="4"/>
</svg>`,
  // the hungry bucket: it gulps every fish, and sometimes burps
  bucket: `<svg viewBox="0 0 62 62" aria-hidden="true">
<path d="M9 18 Q31 -6 53 18" fill="none" stroke="#1B2D45" stroke-width="2.5"/>
<path d="M8 18 L54 18 L48 59 L14 59Z" fill="#90CAF9" stroke="#1B2D45" stroke-width="3" stroke-linejoin="round"/>
<ellipse cx="31" cy="18" rx="23" ry="6" fill="#1565C0" stroke="#1B2D45" stroke-width="3"/>
<g class="pc-eyes"><circle cx="23" cy="34" r="3" fill="#1B2D45"/><circle cx="39" cy="34" r="3" fill="#1B2D45"/></g>
<path class="pc-smile" d="M25 43 Q31 48 37 43" fill="none" stroke="#1B2D45" stroke-width="2.5" stroke-linecap="round"/>
<ellipse class="pc-talkm" cx="31" cy="45" rx="5" ry="4.5" fill="#7B1F1F" stroke="#1B2D45" stroke-width="2"/>
</svg>`,
  ring: `<svg viewBox="0 0 44 44" aria-hidden="true"><circle cx="22" cy="22" r="15" fill="none" stroke="#1B2D45" stroke-width="13"/>
<circle cx="22" cy="22" r="15" fill="none" stroke="#fff" stroke-width="8"/><circle cx="22" cy="22" r="15" fill="none" stroke="#E53935" stroke-width="8" stroke-dasharray="11.78 11.78"/></svg>`
};

/* the look of the pond, the drawings and their faces; sizes shared with js/jeux/peche.js */
const PECHE_SIZE = {sky: 110, fw: 96, fh: 68};
(({sky: SKY, fw: FW, fh: FH}) => addStyle(`
.pc{display:flex; flex-direction:column; gap:12px}
.pc-top{display:flex; flex-wrap:wrap; justify-content:center; align-items:center; gap:10px}
.pc-top .chip{min-width:64px; min-height:64px; font-size:25px; padding:6px 12px}
.pc-top .speak{font-size:19px; padding:10px 14px; min-height:64px}
.pc-buoy{display:flex; align-items:center; gap:10px; align-self:center; max-width:100%; min-height:58px; background:#fff; border:3px solid var(--ink); border-radius:30px; padding:5px 18px 5px 6px; box-shadow:3px 4px 0 var(--ink); animation:pc-tilt 3s ease-in-out infinite alternate}
.pc-buoy svg{width:44px; height:44px; flex:none}
.pc-buoy[hidden]{display:none}
.pc-buoy span{font-family:var(--display); font-weight:600; font-size:21px; line-height:1.2}
.pc-buoy.pc-big span{font-size:32px}
.pc-scene{position:relative; height:calc(${SKY}px + clamp(220px,100vh - 450px,420px)); height:calc(${SKY}px + clamp(220px,100svh - 450px,420px)); border:3px solid var(--ink); border-radius:18px; overflow:hidden; background:linear-gradient(#9EDCF7,#E3F6FF ${SKY}px); touch-action:manipulation; user-select:none; -webkit-user-select:none}
.pc-water{position:absolute; left:0; right:0; top:${SKY}px; bottom:0; overflow:hidden; background:linear-gradient(#4FC3F7,#0288D1 70%,#01579B)}
.pc-waves{position:absolute; left:-40px; top:${SKY - 14}px; width:calc(100% + 80px); height:26px; z-index:1; pointer-events:none; background:radial-gradient(circle at 20px 26px,#4FC3F7 16px,transparent 17px) 0 0/40px 26px repeat-x; animation:pc-wave 1.8s linear infinite}
.pc-sand{position:absolute; left:0; right:0; bottom:0; height:22px; background:#F6D98E; border-top:3px solid #D9B45A}
.pc-weed{position:absolute; bottom:10px; font-size:34px; transform-origin:50% 100%; pointer-events:none; animation:pc-sway 2.6s ease-in-out infinite alternate}
.pc-crab{position:absolute; left:0; bottom:2px; width:58px; height:50px; display:grid; place-items:center; font-size:32px; cursor:pointer}
.pc-crab span{animation:pc-sway .3s ease-in-out infinite alternate}
.pc-rise{position:absolute; bottom:24px; border:2px solid rgba(255,255,255,.85); border-radius:50%; pointer-events:none; animation:pc-rise 4s ease-in forwards}
.pc-cap{position:absolute; left:0; top:12px; width:170px; height:110px; z-index:3; cursor:pointer; transform-origin:45% 95%; animation:pc-rock 2.8s ease-in-out infinite alternate}
.pc-cap svg,.pc-bucket svg{width:100%; height:100%; overflow:visible}
.pc-bucket{position:absolute; right:12px; top:40px; width:62px; height:62px; z-index:3; cursor:pointer; transform-origin:50% 100%}
.pc-pier{position:absolute; right:0; top:100px; width:98px; height:12px; z-index:2; background:#8D6E63; border:3px solid var(--ink); border-right:none; border-radius:6px 0 0 6px}
.pc-pile{position:absolute; right:14px; top:38px; width:58px; display:flex; justify-content:center; font-size:22px; line-height:1; z-index:2; pointer-events:none}
.pc-pile b{font-family:var(--display); font-size:16px; background:#fff; border:2px solid var(--ink); border-radius:8px; padding:0 3px}
.pc-dot{display:inline-block; width:18px; height:18px; border-radius:50%; border:2px solid var(--ink)}
.pc-line{position:absolute; left:0; top:0; width:3px; height:100px; margin-left:-1.5px; background:var(--ink); transform-origin:50% 0; transform:scaleY(0); z-index:6; pointer-events:none}
.pc-swim{position:absolute; left:0; top:0; width:${FW}px; height:${FH}px; z-index:4}
.pc-bob{width:100%; height:100%; animation:pc-bob 1.7s ease-in-out infinite alternate}
.pc-fish{position:relative; display:block; width:100%; height:100%; padding:0; border:none; background:none; cursor:pointer; -webkit-tap-highlight-color:transparent}
.pc-flip{position:absolute; inset:0}
.pc-flip svg{width:100%; height:100%; overflow:visible}
.pc-plate{position:absolute; left:50%; top:52%; width:44px; height:44px; margin:-22px 0 0 -22px; pointer-events:none}
.pc-plate i{display:grid; place-items:center; box-sizing:border-box; width:100%; height:100%; border-radius:50%; background:#fff; border:2.5px solid var(--ink); font-style:normal; font-size:27px; line-height:1}
.pc-plate.pc-num i{font-family:var(--display); font-weight:700; font-size:21px; color:var(--ink)}
.pc-junk .pc-fish{font-size:48px; line-height:${FH}px; text-align:center}
.pc-tail{transform-box:fill-box; transform-origin:100% 50%; animation:pc-tail .45s ease-in-out infinite alternate}
.pc-eye,.pc-scene .pc-eyes{transform-box:fill-box; transform-origin:center; animation:pc-blink 4.2s infinite}
.pc-tongue{opacity:0; transition:opacity .12s}
.pc-fish.pc-grr .pc-tongue{opacity:1}
.pc-fish.pc-grr .pc-eye{transform:scaleY(.2); animation:none}
.pc-hint .pc-plate i{animation:pc-pulse .6s ease-in-out infinite alternate}
.pc-cap .pc-joy,.pc-cap .pc-ouch,.pc-cap .pc-green,.pc-cap .pc-ctongue,.pc-scene .pc-talkm{opacity:0; transition:opacity .12s}
.pc-cap.pc-happy .pc-eyes,.pc-cap.pc-oops .pc-eyes,.pc-cap.pc-yuck .pc-smile{opacity:0}
.pc-cap.pc-happy .pc-joy,.pc-cap.pc-oops .pc-ouch,.pc-cap.pc-yuck .pc-ouch,.pc-cap.pc-yuck .pc-green,.pc-cap.pc-yuck .pc-ctongue{opacity:1}
.pc-cap .pc-cheek{transform-box:fill-box; transform-origin:center; transition:transform .2s}
.pc-cap.pc-happy .pc-cheek{transform:scale(1.7)}
.pc-scene .pc-talkm{transform-box:fill-box; transform-origin:center}
.pc-scene .pc-talking .pc-talkm{opacity:1; animation:pc-talk .16s ease-in-out infinite alternate}
.pc-scene .pc-talking .pc-smile{opacity:0}
.pc-say{position:absolute; z-index:9; max-width:230px; background:#fff; border:3px solid var(--ink); border-radius:14px; padding:4px 10px; font-family:var(--display); font-weight:600; font-size:17px; text-align:center; pointer-events:none; box-shadow:2px 3px 0 var(--ink)}
.pc-fx{position:absolute; z-index:8; font-size:20px; pointer-events:none}
.pc-ring{position:absolute; width:24px; height:24px; margin:-12px 0 0 -12px; border:3px solid #fff; border-radius:50%; z-index:1; pointer-events:none}
.pc-ponds{display:grid; grid-template-columns:repeat(3,1fr); gap:12px; width:100%; max-width:360px; align-self:center}
.pc-pond{aspect-ratio:1; font-size:46px; background:#E3F6FF; display:grid; place-items:center; animation:pc-tilt 2.4s ease-in-out infinite alternate}
.pc.pc-still *{animation:none !important; transition:none !important}
@keyframes pc-bob{from{transform:translateY(-4px)}to{transform:translateY(5px)}}
@keyframes pc-tail{from{transform:rotate(-16deg)}to{transform:rotate(16deg)}}
@keyframes pc-blink{0%,91%,100%{transform:scaleY(1)}95%{transform:scaleY(.1)}}
@keyframes pc-wave{to{transform:translateX(40px)}}
@keyframes pc-sway{from{transform:rotate(-9deg)}to{transform:rotate(9deg)}}
@keyframes pc-rise{from{transform:translateY(0); opacity:.9}to{transform:translateY(-330px); opacity:0}}
@keyframes pc-pulse{from{transform:scale(1)}to{transform:scale(1.3)}}
@keyframes pc-talk{from{transform:scaleY(.35)}to{transform:scaleY(1.2)}}
@keyframes pc-tilt{from{transform:rotate(-2deg)}to{transform:rotate(2deg)}}
@keyframes pc-rock{from{transform:rotate(-2.5deg)}to{transform:rotate(2.5deg)}}
`))(PECHE_SIZE);

/* rounds and sentences: which words swim, how each language asks for them, what a touched fish says */
const pecheQuiz = (() => {
  const shade = h => "#" + [1, 3, 5].map(k => Math.round(parseInt(h.slice(k, k + 2), 16) * .7).toString(16).padStart(2, "0")).join("");
  const cap1 = s => s.charAt(0).toUpperCase() + s.slice(1);
  const uniq = ws => ws.filter((w, k) => w.en !== "fish" && ws.findIndex(v => v.e === w.e) === k); // every fish carries a fish: not a question
  const clash = (a, b) => PECHE_CLASH.some(([p, q]) => (a.en === p && b.en === q) || (a.en === q && b.en === p));
  function pond(key, lvl){
    if (key !== "sea") return uniq(wordsOf(key, lvl));
    const lex = Object.values(THEMES).flatMap(t => t.words).filter(w => PECHE_SEA_LEX.includes(w.en));
    const all = uniq([...PECHE_SEA, ...lex]), ws = all.filter(w => (w.lvl || 1) <= Math.min(3, lvl));
    return ws.length >= 4 ? ws : all;
  }
  const nouns = lvl => uniq(["sea", ...Object.keys(THEMES).filter(k => k !== "colors" && k !== "actions")].flatMap(k => pond(k, lvl)));
  // numbers that sound alike: the digits swapped first (siebenundvierzig / vierundsiebzig), then the neighbours
  function nearNums(t){
    const s = new Set(), rev = +String(t).split("").reverse().join("");
    if (rev >= 10) s.add(rev);
    [t + 1, t - 1, t + 10, t - 10, t + 2, t - 2].forEach(v => { if (v >= 10 && v <= 99) s.add(v); });
    s.delete(t); return [...s];
  }
  const timesNums = (a, b) => shuffle([...new Set([a * (b + 1), a * (b - 1), (a + 1) * b, (a - 1) * b, a * b + 1, a * b - 1, a * b + 10])].filter(v => v > 1 && v !== a * b));

  function rounds(theme, lvl, total, n, lang, kid){
    const kinds = lvl <= 2 ? Array(total).fill(theme === "colors" ? "color" : theme === "actions" ? "action" : "word")
      : shuffle(kid !== "p7" ? ["word", "word", "word", "color", "action", "word", "color", "word"]
        : lvl === 3 ? ["word", "word", "word", "color", "action", "number", "number", "word"]
        : ["word", "word", "color", "action", "number", "times", "times", "word"]);
    const pools = {word: lvl <= 2 ? pond(theme, lvl) : nouns(lvl), color: pond("colors", lvl), action: pond("actions", lvl)};
    // Luxembourgish is heard only through the lod.lu recordings: a word to catch by ear must have one
    const used = new Set(), heard = w => lang !== "lb" || lvl === 4 || !!w.lod;
    return kinds.map(kind => {
      const r = {kind, tpl: rnd(4)}, cs = shuffle(PECHE_PAL);
      const dress = (it, k) => Object.assign(it, {ok: !k, col: it.col || cs[k % cs.length][0], dark: it.dark || cs[k % cs.length][1]});
      if (kind === "number" || kind === "times") {
        if (kind === "number") { r.t = 11 + rnd(lvl === 3 ? 50 : 89); r.key = String(r.t); }
        else { r.a = 2 + rnd(8); r.b = 2 + rnd(8); r.t = r.a * r.b; r.key = `${r.a}x${r.b}`; }
        const ns = [r.t, ...(kind === "number" ? nearNums(r.t) : timesNums(r.a, r.b))].slice(0, n);
        while (ns.length < n) { const v = 10 + rnd(90); if (!ns.includes(v)) ns.push(v); }
        r.items = ns.map((v, k) => dress({n: v, face: String(v), num: true, pile: `<b>${v}</b>`, label: String(v)}, k));
      } else {
        const pool = pools[kind];
        const mix = shuffle(pool);
        r.t = mix.find(w => !used.has(w.en) && heard(w)) || mix.find(heard) || mix[0]; used.add(r.t.en); r.key = r.t.en;
        const ws = [r.t, ...shuffle(pool.filter(w => w !== r.t && w.e !== r.t.e && !clash(r.t, w))).slice(0, n - 1)];
        r.items = ws.map((w, k) => dress(kind === "color"
          ? {w, face: "", col: w.e, dark: shade(w.e), pile: `<i class="pc-dot" style="background:${w.e}"></i>`, label: w.en}
          : {w, face: w.e, pile: w.e, label: w.en}, k));
      }
      if (lvl >= 2) { const j = PECHE_JUNK[rnd(PECHE_JUNK.length)]; r.items.push({junk: true, e: j.e, k: j.k, label: j.k}); }
      return r;
    });
  }

  // what is said: the order, and what a touched fish carries
  const nameOf = (w, l) => w[l] || w.en;
  function order(r, l){
    const x = PECHE_TXT[l];
    if (r.kind === "word") return x.catch[r.tpl](l === "de" ? pecheAcc(nameOf(r.t, l)) : nameOf(r.t, l));
    if (r.kind === "color") return x.color(nameOf(r.t, l));
    if (r.kind === "action") return x.action(nameOf(r.t, l));
    if (r.kind === "number") return x.number(numberIn(r.t, l));
    return x.times(numberIn(r.a, l), numberIn(r.b, l));
  }
  function that(r, it, l){
    const x = PECHE_TXT[l];
    if (r.kind === "word") return x.thats(nameOf(it.w, l));
    if (r.kind === "color") return x.thatsColor(nameOf(it.w, l));
    if (r.kind === "action") return x.thatsAction(nameOf(it.w, l));
    if (r.kind === "times" && it.ok) return cap1(x.result(numberIn(r.a, l), numberIn(r.b, l), numberIn(r.t, l)));
    return x.thatsNumber(numberIn(it.n, l));
  }
  return {rounds, order, that};
})();
