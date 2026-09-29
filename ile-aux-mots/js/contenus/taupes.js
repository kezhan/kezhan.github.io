/* L'Île aux Mots : contenu du jeu « taupes » (Tape-taupes), en anglais, allemand, luxembourgeois et chinois.
   {w} the word as the lexicon gives it; German {n} nominative, {a} accusative.
   [singular, plural] when the sentence changes with the number (Where is / Where are).
   Luxembourgish checked on lod.lu: fänken (fänk!), sichen (sich!), weisen (weis!), hunn (hues, huet), fannen (fonnt),
   erwëschen (erwëscht), lauschteren (lauschter!), liesen (lies!), klappen (klapp!), Maulef, Wuert (Wierder), lues, ze, dach,
   neen, net, midd, juppi, hoppla, äddi, allez, prett, chapeau, salut, moien, wonnerbar, genial, bravo, richteg (de richtege Maulef),
   flott, falsch (falschen), klappen op (klopfen auf), fänken (gefaangen), Hoer (singulier et pluriel : Wou sinn d'Hoer?).
   Eifel rule kept: "Wou sinn d'…", "Ech hunn d'…" (n before d), "Ech si midd" (no n before m), "Falsche Maulef". */
const TAUPES_C = {
  title: {en:"Mole Bop!", de:"Hau den Maulwurf!", lb:"Klapp de Maulef!", zh:"打地鼠"},
  sub: {en:"Listen and bop the right mole!", de:"Hör zu und klopf auf den richtigen Maulwurf!", lb:"Lauschter a klapp op de richtege Maulef!", zh:"听一听，打对的地鼠！"},
  again: {en:"Again", de:"Nochmal", lb:"Nach eng Kéier", zh:"再听一次"},
  listen: {en:"Listen!", de:"Hör zu!", lb:"Lauschter!", zh:"听一听！"},
  read: {en:"Read and bop!", de:"Lies und klopf!", lb:"Lies a klapp!", zh:"读一读，打一打！"},
  two: {en:"Two words!", de:"Zwei Wörter!", lb:"Zwee Wierder!", zh:"两个词！"},
  ready: {en:"Ready? Go!", de:"Achtung, fertig, los!", lb:"Prett? Allez!", zh:"准备好了吗？开始！"},
  bonus: {en:"Bonus!", de:"Bonus!", lb:"Bonus!", zh:"奖励！"},

  // what the voice asks, one sentence per round
  ask: {
    en:[["Where is the {w}?", "Where are the {w}?"], "Find the {w}!", "Bop the mole with the {w}!", "Catch the {w}!", "Who's got the {w}?", "Quick! Get the {w}!"],
    de:[["Wo ist {n}?", "Wo sind {n}?"], "Such {a}!", "Fang {a}!", "Wer hat {a}?", "Zeig mir {a}!", "Tipp auf {a}!", "Schnapp dir {a}!"],
    lb:[["Wou ass {w}?", "Wou sinn {w}?"], "Sich {w}!", "Fänk {w}!", "Wien huet {w}?", "Weis mer {w}!"],
    zh:["{w}在哪里？", "找一找{w}！", "谁拿着{w}？", "快打拿着{w}的地鼠！", "哪只地鼠拿着{w}？", "{w}，{w}，在哪儿？"]
  },
  // said after the right mole, behind a word of praise
  got: {
    en:[["That's the {w}!", "Those are the {w}!"], "You got the {w}!", "You found the {w}!"],
    de:[["Das ist {n}!", "Das sind {n}!"], "Du hast {a}!", "Du hast {a} gefunden!"],
    lb:[["Dat ass {w}!", "Dat sinn {w}!"], "Du hues {w}!", "Du hues {w} fonnt!", "Du hues {w} gefaangen!"],
    zh:["是{w}！", "你找到了{w}！", "你抓到{w}了！"]
  },
  praise: {
    en:["Great job!", "Well done!", "Yes!", "Super!", "Amazing!", "Bullseye!", "Wow!", "Hooray!", "Brilliant!"],
    de:["Super!", "Toll!", "Treffer!", "Klasse!", "Prima!", "Genau!", "Spitze!", "Juhu!", "Bravo!"],
    lb:["Super!", "Bravo!", "Richteg!", "Flott!", "Ganz gutt!", "Genial!", "Wonnerbar!", "Juppi!", "Chapeau!"],
    zh:["太棒了！", "打中了！", "真厉害！", "对啦！", "好样的！", "真棒！", "漂亮！", "没错！", "你真行！"]
  },
  // the right mole, bopped on the head
  ouch: {
    en:["Ouch!", "Boing!", "Bonk!", "Oof!", "Ow ow ow!"],
    de:["Aua!", "Boing!", "Bonk!", "Uff!", "Autsch!"],
    lb:["Au!", "Boing!", "Bonk!", "Au au au!"],
    zh:["哎哟！", "嘣！", "咚！", "哎呀呀！"]
  },
  // a wrong mole sticks its tongue out and says what it holds
  nope: {
    en:["Nope! I've got the {w}!", "Ha ha! I've got the {w}!", "Not me! I've got the {w}!", "Wrong mole! I've got the {w}!"],
    de:["Nee! Ich hab {a}!", "Ätsch! Ich hab {a}!", "Ich nicht! Ich hab {a}!", "Falscher Maulwurf! Ich hab {a}!"],
    lb:["Neen! Ech hunn {w}!", "Hihi! Ech hunn {w}!", "Net ech! Ech hunn {w}!", "Falsche Maulef! Ech hunn {w}!"],
    zh:["不对！我拿的是{w}！", "嘿嘿，我拿的是{w}！", "不是我！我拿着{w}！", "打错啦！我拿的是{w}！"]
  },
  // the right mole went back down before the tap
  slow: {
    en:["Too slow!", "Hee hee!", "Missed me!", "Can't catch me!", "Bye-bye!"],
    de:["Zu langsam!", "Hihi!", "Nicht erwischt!", "Fang mich doch!", "Tschüss!"],
    lb:["Ze lues!", "Hihi!", "Net erwëscht!", "Fänk mech dach!", "Äddi!"],
    zh:["太慢啦！", "嘻嘻！", "没打着！", "来抓我呀！", "拜拜！"]
  },
  // little jokes of a mole that just came out
  quirk: {
    hi:     {en:["Hi!", "Hello!", "Peekaboo!"], de:["Hallo!", "Huhu!", "Kuckuck!"], lb:["Moien!", "Salut!"], zh:["你好！", "嗨！", "看这儿！"]},
    sneeze: {en:["Achoo!"], de:["Hatschi!"], lb:["Hatschi!"], zh:["阿嚏！"]},
    burp:   {en:["Burp! Oops!"], de:["Rülps! Hoppla!"], lb:["Hoppla!"], zh:["嗝！哎呀！"]},
    yawn:   {en:["Yaaawn… I'm sleepy!"], de:["Gääähn… Ich bin müde!"], lb:["Ech si midd!"], zh:["哈欠～好困啊！"]}
  },
  cheer: {en:["Hooray!", "Yippee!"], de:["Hurra!", "Juhu!"], lb:["Juppi!", "Bravo!"], zh:["耶！", "太好了！"]},

  // nouns said in the plural (keys: the English word of the lexicon)
  plural: {
    en:["grapes", "cherries", "eyes", "shoes", "socks", "gloves", "boots", "trousers", "glasses", "noodles", "scissors"],
    de:["grapes", "cherries", "eyes", "hair", "shoes", "socks", "gloves", "boots", "noodles"],
    lb:["grapes", "cherries", "eyes", "hair", "shoes", "socks", "gloves", "boots", "noodles"]
  },
  // German weak masculine nouns: the accusative takes -n or -en (den Löwen, den Affen)
  acc: {"der Löwe":"den Löwen", "der Affe":"den Affen", "der Elefant":"den Elefanten", "der Bär":"den Bären",
        "der Astronaut":"den Astronauten", "der Polizist":"den Polizisten", "der Pilot":"den Piloten", "der Bauer":"den Bauern"},
  // words that sound or look alike in each language: traps from level 3 (English keys of the lexicon).
  // Never two words that sound the same: de Bier (bear) and d'Bier (pear) have one lod.lu recording each, heard alike.
  twins: {
    en:[["mouse","house"], ["bear","pear","hair","ear"], ["car","star"], ["cake","snake"], ["cat","hat","cap"], ["boat","coat"],
        ["cow","owl"], ["leg","egg"], ["frog","dog"], ["tree","key"], ["arm","farmer"], ["book","cook"], ["rice","ice cream"]],
    de:[["mouse","house"], ["mouth","moon","dog"], ["hat","dog"], ["leg","pig"], ["cow","shoes"], ["ball","whale"], ["ice cream","rice"],
        ["cat","cap"], ["fish","frog"], ["nose","trousers"], ["book","cake"]],
    lb:[["mouse","house"], ["mouth","moon"], ["cat","cap"], ["ball","whale"], ["dog","hand","chicken"], ["fish","frog"],
        ["bread","boat"], ["egg","leg"], ["cake","book"], ["train","tongue"]],
    zh:[["glasses","eyes"], ["book","tree","hand"], ["cat","hat","owl"], ["chicken","egg"], ["duck","tooth","cap"], ["cow","milk"],
        ["mouse","tiger","teacher"], ["elephant","brain"], ["watermelon","tomato"], ["plane","pilot"], ["bus","car","train","bike"],
        ["fish","whale","octopus","crocodile"], ["hand","finger","gloves"], ["rocket","train"], ["bread","noodles"], ["shoes","socks"]]
  }
};
