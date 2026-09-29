/* L'Île aux Mots : contenus du jeu « habille » (Habille-moi !), en anglais, allemand, luxembourgeois et chinois.
   Luxembourgish checked on lod.lu: adjective declension (rout: rouden m, rout f/n/pl), noun genders, Eifel rule
   (orangen before a consonant is written orangë: en orangë Pullover), verbs (ech wëll, ech brauch, gëff mer, ech hätt gär,
   undoen: do mech un), situation words (schneit, reent, Bësch, Schnéimännchen m, Kiermes f, Schi...). */
const HABILLE_TXT = {
  title: {en: "Dress me up!", de: "Zieh mich an!", lb: "Do mech un!", zh: "帮我穿衣服！"},
  sub: {en: "Brr! I'm cold!", de: "Brr! Mir ist kalt!", lb: "Brr! Et ass mer kal!", zh: "嘶，我好冷！"},
  // per garment (English word of the lexicon): place on the body, German and Luxembourgish gender,
  // Chinese measure word and verb (戴 for what sits on the head or hands, 穿 for what you get into, 系 for a tie)
  gear: {
    hat:        {slot: "head",  de: "m",  lb: "m",  zh: ["顶", "戴"]},
    cap:        {slot: "head",  de: "f",  lb: "f",  zh: ["顶", "戴"]},
    glasses:    {slot: "eyes",  de: "f",  lb: "m",  zh: ["副", "戴"]},
    scarf:      {slot: "neck",  de: "m",  lb: "m",  zh: ["条", "围"]},
    tie:        {slot: "neck",  de: "f",  lb: "f",  zh: ["条", "系"]},
    "T-shirt":  {slot: "top",   de: "n",  lb: "m",  zh: ["件", "穿"]},
    dress:      {slot: "top",   de: "n",  lb: "n",  zh: ["条", "穿"]},
    coat:       {slot: "outer", de: "m",  lb: "m",  zh: ["件", "穿"]},
    trousers:   {slot: "legs",  de: "f",  lb: "f",  zh: ["条", "穿"]},
    socks:      {slot: "socks", de: "pl", lb: "pl", zh: ["双", "穿"]},
    shoes:      {slot: "feet",  de: "pl", lb: "pl", zh: ["双", "穿"]},
    boots:      {slot: "feet",  de: "pl", lb: "pl", zh: ["双", "穿"]},
    gloves:     {slot: "hands", de: "pl", lb: "pl", zh: ["双", "戴"]}
  },
  // Luxembourgish colour before a noun (lod.lu declension tables): masculine, feminine, neuter, plural
  lbAdj: {
    red: ["rouden", "rout", "rout", "rout"], blue: ["bloen", "blo", "blot", "blo"], green: ["gréngen", "gréng", "gréngt", "gréng"],
    yellow: ["gielen", "giel", "gielt", "giel"], orange: ["orangen", "orange", "oranget", "orange"], pink: ["rosaen", "rosa", "rosat", "rosa"],
    purple: ["mofen", "mof", "mooft", "mof"], black: ["schwaarzen", "schwaarz", "schwaarzt", "schwaarz"], white: ["wäissen", "wäiss", "wäisst", "wäiss"],
    brown: ["brongen", "brong", "brongt", "brong"], grey: ["groen", "gro", "grot", "gro"]
  },
  // German colours that do not decline in the standard language (Duden): das rosa Kleid, die lila Schuhe
  deFixed: ["orange", "rosa", "lila"],
  // the critter asks; {x} is the accusative noun phrase ("den roten Hut", "de rouden Hutt", "那顶红色的帽子")
  ask: {
    // the same order in every language: the flags repaint the bubble in the middle of a round
    en: ["I want {x}!", "Give me {x}, please!", "I need {x}!", "I'd like {x}, please!"],
    de: ["Ich will {x}!", "Gib mir bitte {x}!", "Ich brauche {x}!", "Ich hätte gern {x}!"],
    lb: ["Ech wëll {x}!", "Gëff mer {x}, wannechgelift!", "Ech brauch {x}!", "Ech hätt gär {x}!"],
    zh: ["我要{x}！", "请给我{x}！", "我需要{x}！", "我想要{x}！"]
  },
  and: {en: " and ", de: " und ", lb: " an ", zh: "和"},
  list: {en: ", ", de: ", ", lb: ", ", zh: "、"},
  thanks: {
    en: ["Thank you!", "Yay! I'm warm now!", "I love it!", "So cosy!", "Look at me!", "Wow, so pretty!", "Hooray!", "Perfect!"],
    de: ["Danke!", "Juhu! Jetzt ist mir warm!", "Das gefällt mir!", "Schön kuschelig!", "Schau mich an!", "Wow, wie schön!", "Hurra!", "Perfekt!"],
    lb: ["Merci!", "Hurra! Elo ass mer waarm!", "Dat gefält mer!", "Kuck mol!", "Wéi schéin!", "Hurra!", "Super!", "Merci villmools!"],
    zh: ["谢谢！", "耶！我暖和了！", "我好喜欢！", "好舒服啊！", "看看我！", "哇，真漂亮！", "太棒了！", "完美！"]
  },
  oops: {
    en: ["No, not that one!", "That's not it!", "Oops, wrong one!", "Hmm, no!"],
    de: ["Nein, nicht das!", "Das ist es nicht!", "Hoppla, falsch!", "Hm, nein!"],
    lb: ["Nee, dat net!", "Dat ass et net!", "Oh, falsch!", "Hm, nee!"],
    zh: ["不是这个！", "不对哦！", "哎呀，错啦！", "嗯，不是！"]
  },
  cold: {
    en: ["Brr! I'm cold!", "Brr! So cold!", "My nose is cold!", "I'm freezing!"],
    de: ["Brr! Mir ist kalt!", "Brr! So kalt!", "Meine Nase ist kalt!", "Ich friere!"],
    lb: ["Brr! Et ass mer kal!", "Brr! Sou kal!", "Meng Nues ass kal!", "Ech fréieren!"],
    zh: ["嘶，我好冷！", "好冷好冷！", "我的鼻子好冷！", "我快冻僵了！"]
  },
  tickle: {
    en: ["Hee hee! That tickles!", "Hee hee hee!", "Stop it! Hee hee!"],
    de: ["Hihi! Das kitzelt!", "Hihihi!", "Hör auf! Hihi!"],
    lb: ["Hihi! Dat kribbelt!", "Hihihi!", "Hal op! Hihi!"],
    zh: ["嘻嘻！好痒！", "嘻嘻嘻！", "别闹啦！嘻嘻！"]
  },
  sorry: {
    en: ["Oops! Excuse me!", "Pardon me!"],
    de: ["Hoppla! Entschuldigung!", "Oh, Verzeihung!"],
    lb: ["Pardon!", "Oh, pardon!"],
    zh: ["哎呀，不好意思！", "对不起！"]
  },
  show: {
    en: ["Ta-da! Look at me!", "Ta-da! I'm not cold any more!"],
    de: ["Tadaa! Schau mich an!", "Tadaa! Jetzt friere ich nicht mehr!"],
    lb: ["Tadaa! Kuck mech!", "Tadaa! Elo ass mer net méi kal!"],
    zh: ["当当！看看我！", "当当！我一点儿也不冷了！"]
  },
  pop: {
    sneeze: {en: "Achoo!", de: "Hatschi!", lb: "Hatschi!", zh: "阿嚏！"},
    hic: {en: "Hic!", de: "Hicks!", lb: "Hic!", zh: "嗝！"}
  },
  // levels 3 and 4: where the critter is going, and the clothes that fit
  lead: [
    ["Brr! It's snowing!", "Brr! Es schneit!", "Brr! Et schneit!", "好冷！下雪了！", "coat scarf gloves hat boots trousers socks cap"],
    ["Oh no, it's raining!", "Oh nein, es regnet!", "Oh nee, et reent!", "哎呀，下雨了！", "boots coat hat cap trousers socks"],
    ["Hooray, the sun is shining!", "Juhu, die Sonne scheint!", "Hurra, d'Sonn schéngt!", "太好了，出太阳了！", "T-shirt cap glasses dress shoes hat"],
    ["We're going to a party!", "Wir gehen auf eine Party!", "Mir ginn op eng Party!", "我们去参加派对！", "tie dress shoes hat glasses socks T-shirt trousers"],
    ["Time for school!", "Zeit für die Schule!", "Zäit fir d'Schoul!", "该上学了！", "T-shirt trousers shoes socks coat cap dress"],
    ["Whoosh! It's so windy!", "Huiii! Es ist so windig!", "Huiii! Wat e Wand!", "呼呼，风好大！", "scarf coat cap trousers gloves boots"],
    ["It's Grandma's birthday!", "Oma hat Geburtstag!", "D'Boma huet Gebuertsdag!", "今天是奶奶的生日！", "tie dress shoes hat glasses socks trousers T-shirt"],
    ["Let's go for a walk in the forest!", "Wir gehen im Wald spazieren!", "Mir ginn am Bësch spadséieren!", "我们去森林里散步吧！", "boots trousers coat cap socks scarf hat"],
    ["Let's play football!", "Wir spielen Fußball!", "Mir spille Fussball!", "我们去踢足球吧！", "T-shirt shoes socks cap trousers"],
    ["Let's go to the park!", "Wir gehen in den Park!", "Mir ginn an de Park!", "我们去公园玩吧！", "T-shirt trousers shoes cap coat dress glasses socks"],
    ["Let's go to the zoo!", "Wir gehen in den Zoo!", "Mir ginn an den Zoo!", "我们去动物园吧！", "cap T-shirt shoes glasses trousers dress hat"],
    ["Let's go shopping!", "Wir gehen einkaufen!", "Mir ginn akafen!", "我们去买东西吧！", "coat shoes hat trousers dress scarf glasses"],
    ["Phew, it's hot today!", "Puh, heute ist es heiß!", "Puh, haut ass et waarm!", "呼，今天好热！", "T-shirt cap glasses dress hat shoes"],
    ["Let's build a snowman!", "Wir bauen einen Schneemann!", "Mir bauen e Schnéimännchen!", "我们来堆雪人吧！", "gloves scarf hat boots coat cap trousers socks"],
    ["We're going to the seaside!", "Wir fahren ans Meer!", "Mir fueren un d'Mier!", "我们去海边吧！", "T-shirt cap glasses hat dress shoes"],
    ["We're going to a restaurant!", "Wir gehen ins Restaurant!", "Mir ginn an de Restaurant!", "我们去餐厅吃饭！", "tie dress shoes glasses trousers T-shirt socks"],
    ["It's winter!", "Es ist Winter!", "Et ass Wanter!", "冬天到了！", "coat scarf gloves hat boots trousers socks"],
    ["It's summer!", "Es ist Sommer!", "Et ass Summer!", "夏天到了！", "T-shirt dress cap glasses shoes hat"],
    ["It's autumn!", "Es ist Herbst!", "Et ass Hierscht!", "秋天到了！", "coat scarf boots cap trousers socks"],
    ["It's spring!", "Es ist Frühling!", "Et ass Fréijoer!", "春天到了！", "T-shirt trousers shoes cap dress glasses"],
    ["Let's go to the fair!", "Wir gehen auf die Kirmes!", "Mir ginn op d'Kiermes!", "我们去游乐场玩吧！", "cap T-shirt trousers shoes glasses dress coat"],
    ["Let's go skiing!", "Wir fahren Ski!", "Mir fuere Schi!", "我们去滑雪吧！", "gloves scarf coat boots glasses hat cap trousers"],
    ["Grandpa is coming!", "Opa kommt!", "De Bopa kënnt!", "爷爷来了！", "tie dress shoes T-shirt trousers glasses"],
    ["We're going to a wedding!", "Wir gehen auf eine Hochzeit!", "Mir ginn op eng Hochzäit!", "我们去参加婚礼！", "tie dress shoes hat glasses"],
    ["Brr, there's a storm!", "Brr, ein Sturm!", "Brr, e Stuerm!", "哎呀，暴风雨来了！", "coat boots hat scarf trousers"]
  ].map(([en, de, lb, zh, gear]) => ({en, de, lb, zh, gear: gear.split(" ")}))
};
