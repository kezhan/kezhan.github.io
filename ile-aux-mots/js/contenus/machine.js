/* L'Île aux Mots : contenu du jeu « La Machine à mots » : un vrai jeu de sons propre à chaque langue, jamais une traduction.
   en: English rhymes (cat, hat), onsets (_at) and vowel traps (hat, hot, hit).
   de: Reime (Haus, Maus; Hund, Mund), Anlaute (_und), Rechtschreibfallen (Fisch, Fich, Visch).
   lb: rhymes between words of the lexicon, so the lod.lu recording plays (Haus, Maus; Hond, Mond; Kuch, Buch),
       checked by their lod.lu IPA (hæːʊs/mæːʊs, hont/mont, kuχ/buχ…); onsets; spelling traps, German spellings included (Hund for Hond).
   zh: 押韵 (the same pinyin final: 猫 māo, 桃 táo), 韵母 (m + āo), 声调 (māo, máo, mǎo, mào). */
const MACHINE_DATA = {
  en: {
    rhymes: [["🐱","cat","🎩","hat"],["🐝","bee","🌳","tree"],["⭐","star","🚗","car"],["🐭","mouse","🏠","house"],["🐍","snake","🍰","cake"],
      ["🐐","goat","⛵","boat"],["🐸","frog","🪵","log"],["🌙","moon","🥄","spoon"],["🐻","bear","🍐","pear"],["🧦","sock","⏰","clock"],
      ["👑","king","💍","ring"],["🌧️","rain","🚂","train"],["🐟","fish","🍽️","dish"],["🦊","fox","📦","box"]],
    words: [["🐱","cat"],["🎩","hat"],["🦇","bat"],["🐀","rat"],["🐷","pig"],["🐶","dog"],["🪵","log"],["☀️","sun"],["🚌","bus"],["🐔","hen"],
      ["🖊️","pen"],["📦","box"],["🦊","fox"],["🛏️","bed"],["🚐","van"],["🐛","bug"],["🕸️","web"],["🥅","net"],["🛖","hut"],["🗺️","map"],["🧢","cap"],["👜","bag"],["🦵","leg"]]
  },
  de: {
    // pairs that never rhyme with another pair (Wal/Schal and Zahn/Hahn: aːl, aːn)
    rhymes: [["🏠","Haus","🐭","Maus"],["🐶","Hund","👄","Mund"],["🐮","Kuh","👞","Schuh"],["🐰","Hase","👃","Nase"],["🦵","Bein","🐷","Schwein"],
      ["🌹","Rose","👖","Hose"],["🍞","Brot","⛵","Boot"],["🐐","Ziege","🪰","Fliege"],["🐵","Affe","🦒","Giraffe"],["🐳","Wal","🧣","Schal"],
      ["🦔","Igel","🪞","Spiegel"],["🦷","Zahn","🐓","Hahn"],["🥜","Nuss","🚌","Bus"],["🚀","Rakete","🎺","Trompete"],["🦟","Mücke","🌉","Brücke"]],
    // one consonant, then the rest of the word: _und
    words: [["🐶","Hund"],["🏠","Haus"],["🐭","Maus"],["👄","Mund"],["🎩","Hut"],["🚌","Bus"],["⚽","Ball"],["🐻","Bär"],["🐮","Kuh"],["🐟","Fisch"],
      ["⛵","Boot"],["🌙","Mond"],["🦷","Zahn"],["🥜","Nuss"],["🦵","Bein"],["🌹","Rose"],["🐰","Hase"],["👃","Nase"],["🐳","Wal"],["🌳","Baum"],
      ["🐐","Ziege"],["🐓","Hahn"],["🦊","Fuchs"],["🦁","Löwe"],["🐯","Tiger"],["☀️","Sonne"],["📖","Buch"],["🍰","Kuchen"],["🍅","Tomate"],["🍄","Pilz"],
      ["🌽","Mais"],["💡","Lampe"],["🐱","Katze"],["☁️","Wolke"],["🍴","Gabel"],["☕","Tasse"],["👖","Hose"],["🐦","Vogel"],["🚀","Rakete"]],
    // the right spelling, then three traps (sch, ie, ä, ß, h, doubled letters)
    spell: [["🐟","Fisch","Fich","Visch","Fish"],["🐭","Maus","Mauss","Mous","Maos"],["🐻","Bär","Ber","Bähr","Bäa"],["🐶","Hund","Hunt","Hundt","Hond"],
      ["🏠","Haus","Hous","Haos","Hauss"],["🌙","Mond","Mont","Mohnd","Mund"],["🦷","Zahn","Zan","Tsahn","Zaan"],["⛵","Boot","Bot","Boht","Bood"],
      ["🍞","Brot","Brod","Broot","Brodt"],["🐍","Schlange","Shlange","Schlanke","Schlang"],["🧀","Käse","Kese","Käße","Kähse"],["🌳","Baum","Boum","Bawm","Baumm"],
      ["🦉","Eule","Oile","Äule","Eulle"],["🐸","Frosch","Frosh","Frohsch","Vrosch"],["🍎","Apfel","Abfel","Apfl","Appfel"],["🥛","Milch","Milsch","Millch","Mielch"],
      ["🐝","Biene","Bine","Bihne","Biehne"],["🦶","Fuß","Fus","Vuß","Fuhs"],["⭐","Stern","Schtern","Sten","Sterm"],["🐮","Kuh","Ku","Kuu","Khu"]]
  },
  lb: {
    // English keys of the lexicon (THEMES): the word, its picture and its recording come from there
    rhymes: [["house","mouse"],["dog","mouth"],["tooth","hand"],["cake","book"],["chicken","shoes"],["whale","scarf"],["bread","red"],["blue","grey"],["white","rice"]],
    words: ["house","mouse","dog","mouth","hand","cake","book","train","chicken","whale","ball","bus","boat","tree","sun","cat","cow","bird","fish","foot",
      "leg","hat","cap","trousers","nose","tooth","bear","moon","cheese","rice","pizza","tomato","lion","carrot","cloud","coat","tongue","finger","bike"],
    // German spellings are the first trap: the children meet both
    spell: [["🐶","Hond","Hund","Hont","Hoond"],["🐟","Fësch","Fisch","Fesch","Fäsch"],["🐦","Vull","Vogel","Full","Vul"],["🐸","Fräsch","Frosch","Fresch","Fräch"],
      ["🦷","Zant","Zahn","Sant","Zannt"],["🐔","Hong","Huhn","Hung","Honk"],["🍞","Brout","Brot","Braut","Brut"],["🧀","Kéis","Käse","Keis","Kéiss"],
      ["🌳","Bam","Baum","Bamm","Pam"],["🌙","Mound","Mund","Moond","Mount"],["⭐","Stär","Stern","Ster","Staer"],["🚂","Zuch","Zug","Zuuch","Tsuch"],
      ["🦶","Fouss","Fuß","Fous","Fuuss"],["👃","Nues","Nase","Nuess","Noes"],["🍎","Apel","Apfel","Aapel","Abel"],["🐭","Maus","Mous","Mauss","Maos"],
      ["🏠","Haus","Hous","Hauss","Haos"],["🐮","Kou","Kuh","Ku","Kow"]]
  },
  zh: {
    // picture, character, initial, written final without tone, tone
    syl: [["🐱","猫","m","ao",1],["🍑","桃","t","ao",2],["🐶","狗","g","ou",3],["✋","手","sh","ou",3],["🐟","鱼","y","u",2],["🌧️","雨","y","u",3],
      ["🐷","猪","zh","u",1],["📖","书","sh","u",1],["🐔","鸡","j","i",1],["🍐","梨","l","i",2],["🐍","蛇","sh","e",2],["🚗","车","ch","e",1],
      ["⭐","星","x","ing",1],["🧊","冰","b","ing",1],["⛰️","山","sh","an",1],["☂️","伞","s","an",3],["💡","灯","d","eng",1],["🌬️","风","f","eng",1],
      ["☁️","云","y","un",2],["👗","裙","q","un",2],["🌙","月","y","ue",4],["❄️","雪","x","ue",3],["🔥","火","h","uo",3],["🍲","锅","g","uo",1],
      ["🐑","羊","y","ang",2],["🐘","象","x","iang",4],["🐴","马","m","a",3],["🍵","茶","ch","a",2],["🌸","花","h","ua",1],["🐮","牛","n","iu",2],
      ["🐦","鸟","n","iao",3],["⚽","球","q","iu",2],["🚪","门","m","en",2],["🦷","牙","y","a",2],["👟","鞋","x","ie",2],["⛵","船","ch","uan",2],
      ["🥚","蛋","d","an",4],["🍜","面","m","ian",4],["🍚","饭","f","an",4],["🐰","兔","t","u",4],["🌳","树","sh","u",4],["🥬","菜","c","ai",4],
      ["🍖","肉","r","ou",4],["🐯","虎","h","u",3],["🦶","脚","j","iao",3]],
    // rhyming pairs (same final); no pair rhymes with another one (an / ang, ou / iu kept apart)
    rhymes: ["猫桃","狗手","鱼雨","猪书","鸡梨","蛇车","星冰","山伞","灯风","云裙","月雪","火锅","羊象","马茶"]
  },
  ui: {
    en: {rhyme:a => `Which one rhymes with ${a}?`, rhymeShow:a => `Which one rhymes with <b>${a}</b>?`, yes:(a, b) => `Yes! ${a}, ${b}!`,
      nope:(a, o) => `${o}? ${a}, ${o}... no! Which one rhymes with ${a}?`, make:w => `Make the word ${w}!`, makeShow:"Make the word!",
      makeNo:(x, w) => `${x}? No! Make ${w}!`, which:"Which word?", yes1:w => `Yes! ${w}!`, retry:"Try again!"},
    de: {rhyme:a => `Was reimt sich auf ${a}?`, rhymeShow:a => `Was reimt sich auf <b>${a}</b>?`, yes:(a, b) => `Ja! ${a}, ${b}!`,
      nope:(a, o) => `${o}? ${a}, ${o}… nein! Was reimt sich auf ${a}?`, make:w => `Womit fängt ${w} an?`, makeShow:"Womit fängt das Wort an?",
      makeNo:(x, w) => `${x}? Nein! ${w}!`, which:"Welches Wort ist richtig geschrieben?", yes1:w => `Ja! ${w}!`, retry:"Versuch es noch mal!"},
    // Luxembourgish has no voice: only the lod.lu recording of the word itself is played
    lb: {rhyme:a => a, rhymeShow:a => `Wat reimt sech op <b>${a}</b>?`, yes:(a, b) => b, nope:(a, o) => o,
      make:w => w, makeShow:"Mat wéi engem Buschtaf fänkt d'Wuert un?", makeNo:(x, w) => w, which:"Wéi schreift een dat Wuert?", yes1:w => w, retry:"Probéier nach eng Kéier!"},
    zh: {rhyme:a => `哪个和“${a}”押韵？`, rhymeShow:a => `哪个和 <b>${a}</b> 押韵？`, yes:(a, b) => `对了！${a}，${b}！`, nope:(a, o) => `${o}？${a}，${o}……不对！`,
      fin:c => `${c}，${c}的韵母是什么？`, finShow:"韵母是什么？", tone:c => `${c}，${c}是第几声？`, toneShow:"听一听，是第几声？",
      toneYes:(c, t) => `对了！${c}是第${["一","二","三","四"][t - 1]}声！`, yes1:c => `对了！${c}！`, retry:"再试一次！"}
  }
};
