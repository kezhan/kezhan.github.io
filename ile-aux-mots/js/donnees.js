/* L'Île aux Mots : mots, thèmes, jeux et constantes. */
const VERSION = "1.1";
/* ---------- words: emoji|english|français|中文 ---------- */
const P = s => s.trim().split("\n").map(l => { const [e,en,fr,zh] = l.trim().split("|"); return {e,en,fr,zh}; });
const THEMES = {
  animals:{label:"Animaux", icon:"🐱", words:P(`🐱|cat|chat|猫
🐶|dog|chien|狗
🐮|cow|vache|奶牛
🐷|pig|cochon|猪
🦆|duck|canard|鸭子
🐴|horse|cheval|马
🦁|lion|lion|狮子
🐸|frog|grenouille|青蛙
🐟|fish|poisson|鱼
🐦|bird|oiseau|鸟
🐰|rabbit|lapin|兔子
🐵|monkey|singe|猴子
🐘|elephant|éléphant|大象
🐻|bear|ours|熊
🐭|mouse|souris|老鼠
🐑|sheep|mouton|羊`)},
  food:{label:"Miam", icon:"🍎", words:P(`🍎|apple|pomme|苹果
🍌|banana|banane|香蕉
🍓|strawberry|fraise|草莓
🍇|grapes|raisin|葡萄
🍊|orange|orange|橙子
🍐|pear|poire|梨
🍒|cherries|cerises|樱桃
🍉|watermelon|pastèque|西瓜
🍞|bread|pain|面包
🧀|cheese|fromage|奶酪
🥛|milk|lait|牛奶
🥚|egg|œuf|鸡蛋
🍰|cake|gâteau|蛋糕
🥕|carrot|carotte|胡萝卜
🍕|pizza|pizza|比萨`)},
  colors:{label:"Couleurs", icon:"🎨", words:P(`#E53935|red|rouge|红色
#1E88E5|blue|bleu|蓝色
#43A047|green|vert|绿色
#FDD835|yellow|jaune|黄色
#FB8C00|orange|orange|橙色
#F48FB1|pink|rose|粉色
#8E24AA|purple|violet|紫色
#212121|black|noir|黑色
#FFFFFF|white|blanc|白色
#795548|brown|marron|棕色`)},
  body:{label:"Mon corps", icon:"👃", words:P(`👀|eyes|yeux|眼睛
👃|nose|nez|鼻子
👄|mouth|bouche|嘴巴
👂|ear|oreille|耳朵
✋|hand|main|手
🦶|foot|pied|脚
🦷|tooth|dent|牙齿
💪|arm|bras|胳膊
🦵|leg|jambe|腿
💇|hair|cheveux|头发`)},
  things:{label:"Mon monde", icon:"⚽", words:P(`⚽|ball|ballon|球
🚗|car|voiture|汽车
🚲|bike|vélo|自行车
⛵|boat|bateau|船
✈️|plane|avion|飞机
🚂|train|train|火车
📖|book|livre|书
🏠|house|maison|房子
🌳|tree|arbre|树
🌸|flower|fleur|花
☀️|sun|soleil|太阳
🌙|moon|lune|月亮
⭐|star|étoile|星星`)}
};
const NUMS = ["one","two","three","four","five","six","seven","eight","nine","ten"];
const NUMS_FR = ["un","deux","trois","quatre","cinq","six","sept","huit","neuf","dix"];
const NUMS_ZH = ["一","二","三","四","五","六","七","八","九","十"];
const SIMON = P(`👃|Touch your nose!|Touche ton nez|摸摸你的鼻子
👏|Clap your hands!|Tape dans tes mains|拍拍手
🦘|Jump!|Saute|跳一跳
🔄|Turn around!|Tourne sur toi|转个圈
🪑|Sit down!|Assieds-toi|坐下
🧍|Stand up!|Lève-toi|站起来
👋|Wave hello!|Fais coucou|挥挥手
🦶|Stomp your feet!|Tape des pieds|跺跺脚
🙆|Touch your head!|Touche ta tête|摸摸你的头
😑|Close your eyes!|Ferme les yeux|闭上眼睛
🙌|Hands up!|Lève les mains|举起手
😝|Stick out your tongue!|Tire la langue|吐舌头`);
const STICKERS = ["🦄","🚀","🦖","🐙","🌈","🏰","🍦","🎈","🦋","🐳","🦕","🚁","🧸","🍭","🐧","🎸","🦜","🛸","🐢","🍩","🐉","🎠","🧁","🪐"];
const PRAISE = ["Great job!","Well done!","Yes!","Super!","Amazing!","You got it!"];

const ACTS = [
  {id:"imagier", em:"📚", name:"Imagier", desc:"Touche une image, elle parle", themes:true},
  {id:"ecoute",  em:"👂", name:"Écoute & trouve", desc:"Trouve la bonne image", themes:true},
  {id:"ballons", em:"🎈", name:"Ballons", desc:"Éclate la bonne couleur"},
  {id:"compte",  em:"🔢", name:"Combien ?", desc:"Compte en anglais"},
  {id:"memory",  em:"🃏", name:"Memory", desc:"Retrouve les paires", themes:true},
  {id:"simon",   em:"🙆", name:"Jacques a dit", desc:"Bouge ton corps", badge:"avec un parent"},
  {id:"repete",  em:"🎤", name:"Répète !", desc:"Dis le mot au micro", themes:true, mic:true}
];
// speech recognition needs a secure page outside claude.ai: the PC at http://localhost, or a future https site
const SR = window.SpeechRecognition || window.webkitSpeechRecognition;
const MIC_OK = !!SR && !window.claude && window.isSecureContext;
const KID_DEFAULT = {
  p7:{name:"Le grand", age:7, ava:"🦊", bridge:"zh", choices:4, showWord:true},
  p4:{name:"Le petit", age:4, ava:"🐣", bridge:"fr", choices:3, showWord:false}
};
const FB_TAGS = ["Adoré","Trop facile","Trop dur","Ennui","Son / voix","Bug","Idée"];


const COUNT_ITEMS = [["🐱","cats"],["🐶","dogs"],["🐷","pigs"],["🦆","ducks"],["🐸","frogs"],["🐰","rabbits"],["🍎","apples"],["🍌","bananas"],["🍓","strawberries"],["🥕","carrots"],["⭐","stars"],["🚗","cars"],["🎈","balloons"]];
