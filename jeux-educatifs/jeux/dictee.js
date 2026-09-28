/*
 * La dictée du perroquet / Parrot dictation / 鹦鹉听写 / Das Papageien-Diktat / D’Dictée vum Papagei.
 * Le perroquet prononce un mot (Ile.say, voix de la langue du pack) et l'enfant l'écrit.
 * Mots : Ile.L().MOTS du niveau (l'emoji sert d'aide) et Ile.L().MOTS_SIMPLES[niveau]
 * (une petite phrase d'exemple, propre à chaque langue, donne le sens et lève les homophones).
 * Niveau 1 : tolérant (accents, ä ö ü ß, majuscule : accepté, mais on montre la bonne orthographe).
 * Niveaux 2 et 3 : exigeant, avec un message propre à chaque faute (accents, Umlaut, ß, majuscule
 * des noms en allemand et en luxembourgeois, mot en minuscules écrit avec une majuscule).
 * Deux essais par mot ; un point si c'est juste du premier coup.
 * Sans voix pour la langue (souvent le luxembourgeois), son coupé ou « Je n'entends rien » :
 * mode « Mémorise », le mot s'affiche 3 secondes avec son image, puis s'envole.
 * Chinois : niveau 1 = on entend le mot et on choisit ses caractères parmi 4 (pinyin masqué) ;
 * niveaux 2 et 3 = on tape le pinyin sans tons (Ile.epeler ; v ou u: pour ü, tons et espaces ignorés),
 * puis on voit les caractères et le pinyin avec ses tons.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'dictee';
  const TOTAL = 8;
  const NB_SANS_IMAGE = 3; // mots de MOTS_SIMPLES dans chaque partie
  const NB_CHOIX = 4; // chinois, niveau 1 : caractères proposés
  const MEMO_MS = 3000; // durée d'affichage du mot en mode « Mémorise »
  const DELAI_SUIVANT = 1500;
  const NB = '\u202F'; // espace fine insécable (typographie française)

  const listeEt = (et, ouvre, ferme) => (items) => {
    const q = items.map((c) => ouvre + c + ferme);
    return q.length < 2 ? q.join('') : q.slice(0, -1).join(', ') + et + q[q.length - 1];
  };

  // Textes propres au jeu, par langue (mêmes clés partout).
  // phrases : « ~ » marque le mot dicté (mots sans image) ; article : champ du mot dicté avec lui.
  const T = {
    en: {
      nom: 'Polly',
      article: null, // en anglais, on dicte le mot seul, comme dans un « spelling test »
      ouvre: '“',
      ferme: '”',
      bonjour: (nom) => 'Hello, I’m ' + nom + '!',
      introVoix: (n) => 'I’m going to say ' + n + ' words. Listen carefully and write them down!',
      introMemo: (n, s) => 'I’m going to show you ' + n + ' words. Look carefully: each word flies away after '
        + s + ' seconds. Then write it down!',
      introQcm: (n) => 'I’m going to say ' + n + ' words. Listen carefully and choose the right one!',
      introMemoQcm: (n, s) => 'I’m going to show you ' + n + ' words. Each one flies away after ' + s
        + ' seconds. Then choose the right one!',
      astuceVoix: 'You can listen to me again as many times as you like.',
      astuceMemo: 'You can look at the word again if you forget it.',
      astuceEssais: 'You have two tries for each word.',
      astuceAccents: 'Watch out for accents: they count!',
      astuceSansAccents: 'You don’t need the accents, but I’ll show you where they go.',
      astuceQcm: 'Listen, then choose the right word.',
      astucePinyin: 'Type the pinyin without the tones.',
      commencer: 'Start',
      consigneVoix: (nom) => 'Listen to ' + nom + ' and write the word.',
      consigneMemo: 'Remember the word, then write it.',
      consigneQcm: (nom) => 'Listen to ' + nom + ' and choose the right word.',
      consigneMemoQcm: 'Remember the word, then choose it.',
      perroquet: (nom) => nom + ' the parrot',
      image: 'Picture of the word to write',
      reecouter: 'Listen again',
      lentement: 'Slowly',
      revoir: 'See the word again',
      sourd: 'I can’t hear anything',
      sourdAide: 'Can’t hear the parrot? The word will be shown instead.',
      sonCoupe: (nom) => 'The sound is off: tap 🔇 at the top to hear ' + nom + '.',
      motAEcrire: '(the word to write)',
      ecrisIci: 'Type the word here',
      ecrisApres: (art) => 'Type the word that comes after “' + art + '”',
      ecrisPinyin: 'Type the pinyin here',
      touches: 'Special letters',
      inserer: (c) => 'Type “' + c + '”',
      valider: 'Check',
      regarde: 'Look carefully…',
      aToi: 'Your turn to write!',
      vide: 'Type the word in the box first.',
      accents: () => 'Watch out for the accents!',
      bravoAccents: () => 'Well done! Look carefully at the accents.',
      majuscule: () => 'Watch out for capital letters!',
      bravoMajuscule: () => 'Well done! Look carefully at the capital letters.',
      aussiMajuscule: () => 'And watch out for capital letters.',
      onEcrit: 'It’s spelt:',
      bravo: 'Well done! Perfect spelling.',
      oui: 'Yes, that’s right!',
      pasGrave: 'Never mind!',
      voici: 'Here’s how to spell it:',
      union: 'Watch out for the hyphen!',
      reessaie: 'Not quite… Try again!',
      uJqxy: 'After j, q, x and y, ü loses its dots: we write u!',
      choix: 'Choose a word',
      nonQcm: 'No, that’s not it. Try again!',
      suivant: 'Next word',
      resultat: 'See my score',
      aRevoir: (mots) => (mots.length > 1 ? 'Words to practise' : 'Word to practise') + ': ' + mots.join(', ') + '.',
      parfait: (nom) => 'No mistakes: ' + nom + ' is very proud of you!',
      phrases: {
        // Level 1
        mum: 'My ~ reads me a story.',
        dad: 'My ~ makes pancakes.',
        red: 'Strawberries are ~.',
        sit: 'Please ~ down on the mat.',
        hop: 'A rabbit can ~.',
        jam: 'I like ~ on my toast.',
        mud: 'My boots are covered in ~.',
        top: 'Climb to the ~ of the hill.',
        yes: 'Nod your head to say ~.',
        zip: 'Can you ~ up your coat?',
        lid: 'Put the ~ back on the jar.',
        net: 'The ball went into the ~.',
        tap: 'Turn off the ~, please.',
        bag: 'My school ~ is heavy.',
        pot: 'The plant grows in a ~.',
        // Level 2
        garden: 'We have a swing in the ~.',
        school: 'I walk to ~ every day.',
        beach: 'We play on the sandy ~.',
        table: 'Dinner is on the ~.',
        chair: 'Sit on the ~, please.',
        door: 'Please shut the ~.',
        window: 'The cat looks out of the ~.',
        friend: 'Tom is my best ~.',
        water: 'I drink a glass of ~.',
        music: 'I love listening to ~.',
        sister: 'My big ~ is ten.',
        brother: 'My little ~ is three.',
        hello: 'I say ~ to my teacher.',
        please: 'Can I have a biscuit, ~?',
        morning: 'I eat my breakfast in the ~.',
        // Level 3
        captain: 'The ~ is in charge of the ship.',
        adventure: 'We are going on a big ~.',
        compass: 'A ~ shows you which way is north.',
        storm: 'The wind blows hard in the ~.',
        pirate: 'The ~ buried a chest of gold.',
        sailor: 'The ~ climbs up the mast.',
        voyage: 'The ship sets off on a long ~.',
        library: 'I borrow books from the ~.',
        birthday: 'Happy ~ to you!',
        mysterious: 'We heard a ~ noise in the cave.',
        explorer: 'The ~ found a hidden cave.',
        treasure: 'The map shows where the ~ is buried.',
        crew: 'The whole ~ works hard on the ship.',
        telescope: 'I look at the stars through a ~.',
        horizon: 'The sun sets on the ~.',
      },
    },
    zh: {
      nom: '豆豆',
      article: null, // on dicte le mot seul (pas de classificateur)
      ouvre: '“',
      ferme: '”',
      bonjour: (nom) => '你好，我是' + nom + '！',
      introVoix: (n) => '我会说 ' + n + ' 个词。仔细听，写出它们的拼音！',
      introMemo: (n, s) => '我会给你看 ' + n + ' 个词。仔细看：拼音 ' + s + ' 秒后就会飞走。然后把拼音写出来！',
      introQcm: (n) => '我会说 ' + n + ' 个词。仔细听，选出正确的汉字！',
      introMemoQcm: (n, s) => '我会给你看 ' + n + ' 个词的拼音。拼音 ' + s + ' 秒后就会飞走，然后选出正确的汉字！',
      astuceVoix: '你想听几次都可以。',
      astuceMemo: '忘了的话，可以再看一次。',
      astuceEssais: '每个词你有两次机会。',
      astuceAccents: '只写拼音字母，不用写声调。ü\u00A0可以打\u00A0v。',
      astuceSansAccents: '只写拼音字母，不用写声调。ü\u00A0可以打\u00A0v。',
      astuceQcm: '听一听，再从四个词里选出你听到的那个。',
      astucePinyin: '只写拼音字母，不用写声调。ü\u00A0可以打\u00A0v。',
      commencer: '开始',
      consigneVoix: (nom) => '听' + nom + '说，写出这个词的拼音。',
      consigneMemo: '记住拼音，然后写出来。',
      consigneQcm: (nom) => '听' + nom + '说，选出正确的汉字。',
      consigneMemoQcm: '看拼音，记住它，然后选出正确的汉字。',
      perroquet: (nom) => '鹦鹉' + nom,
      image: '这个词的图片',
      reecouter: '再听一次',
      lentement: '慢一点',
      revoir: '再看一次',
      sourd: '我听不到',
      sourdAide: '听不到鹦鹉说话？那就把词显示出来。',
      sonCoupe: (nom) => '声音关掉了：点上面的 🔇，就能听到' + nom + '说话。',
      motAEcrire: '（要写的词）',
      ecrisIci: '在这里写拼音',
      ecrisApres: () => '在这里写拼音',
      ecrisPinyin: '在这里写拼音',
      touches: '特殊字母',
      inserer: (c) => '输入“' + c + '”',
      valider: '检查',
      regarde: '仔细看……',
      aToi: '现在轮到你了！',
      vide: '先在框里写拼音。',
      accents: () => '注意：这里要写\u00A0ü（可以打\u00A0v）！',
      bravoAccents: () => '对了！不过要注意：这里是 ü。',
      majuscule: () => '注意大小写！',
      bravoMajuscule: () => '对了！注意大小写。',
      aussiMajuscule: () => '还要注意大小写。',
      onEcrit: '正确的写法：',
      bravo: '太棒了！完全正确。',
      oui: '对了！',
      pasGrave: '没关系！',
      voici: '拼音是这样写的：',
      union: '注意：拼音要连在一起写！',
      reessaie: '不太对……再试一次！',
      uJqxy: 'j、q、x、y 后面的 ü 要去掉两点，写成 u！',
      choix: '选一个词',
      nonQcm: '不对，再试一次！',
      suivant: '下一个词',
      resultat: '看看我的成绩',
      aRevoir: (mots) => '要复习的词：' + mots.join('、') + '。',
      parfait: (nom) => '全都对了：' + nom + '为你感到骄傲！',
      phrases: {},
    },
    de: {
      nom: 'Lora',
      article: 'def', // on dicte « die Katze » : der, die, das font partie du mot qu'on apprend
      ouvre: '„',
      ferme: '“',
      bonjour: (nom) => 'Hallo, ich bin ' + nom + '!',
      introVoix: (n) => 'Ich sage dir ' + n + ' Wörter. Hör gut zu und schreib sie auf!',
      introMemo: (n, s) => 'Ich zeige dir ' + n + ' Wörter. Schau genau hin: Jedes Wort fliegt nach '
        + s + ' Sekunden weg. Dann schreibst du es auf!',
      introQcm: (n) => 'Ich sage dir ' + n + ' Wörter. Hör gut zu und wähle das richtige Wort!',
      introMemoQcm: (n, s) => 'Ich zeige dir ' + n + ' Wörter. Jedes Wort fliegt nach ' + s
        + ' Sekunden weg. Dann wählst du das richtige Wort!',
      astuceVoix: 'Du kannst mich so oft anhören, wie du willst.',
      astuceMemo: 'Wenn du das Wort vergisst, kannst du es noch einmal ansehen.',
      astuceEssais: 'Du hast für jedes Wort zwei Versuche.',
      astuceAccents: 'Achte auf ä, ö, ü, ß und auf große Anfangsbuchstaben: Sie zählen!',
      astuceSansAccents: 'Große Buchstaben, ä, ö, ü und ß musst du noch nicht können: Ich zeige sie dir.',
      astuceQcm: 'Hör zu und wähle das richtige Wort.',
      astucePinyin: 'Schreib das Pinyin ohne Töne.',
      commencer: 'Los geht’s',
      consigneVoix: (nom) => 'Hör ' + nom + ' zu und schreib das Wort.',
      consigneMemo: 'Merk dir das Wort und schreib es dann.',
      consigneQcm: (nom) => 'Hör ' + nom + ' zu und wähle das richtige Wort.',
      consigneMemoQcm: 'Merk dir das Wort und wähle es dann aus.',
      perroquet: (nom) => nom + ', der Papagei',
      image: 'Bild zum Wort',
      reecouter: 'Wiederholen',
      lentement: 'Langsam',
      revoir: 'Wort noch mal zeigen',
      sourd: 'Ich höre nichts',
      sourdAide: 'Du hörst den Papagei nicht? Dann wird das Wort angezeigt.',
      sonCoupe: (nom) => 'Der Ton ist aus: Tippe oben auf 🔇, um ' + nom + ' zu hören.',
      motAEcrire: '(das Wort zum Schreiben)',
      ecrisIci: 'Schreib das Wort hier',
      ecrisApres: (art) => 'Schreib das Wort nach „' + art + '“',
      ecrisPinyin: 'Schreib das Pinyin hier',
      touches: 'Besondere Buchstaben',
      inserer: (c) => '„' + c + '“ schreiben',
      valider: 'Prüfen',
      regarde: 'Schau genau hin …',
      aToi: 'Jetzt schreibst du!',
      vide: 'Schreib zuerst das Wort ins Feld.',
      accents: (diff) => 'Achte auf ' + (diff.length ? listeEt(' und ', '„', '“')(diff) : 'ä, ö, ü und ß') + '!',
      bravoAccents: (diff) => 'Gut! Aber achte auf ' + (diff.length ? listeEt(' und ', '„', '“')(diff) : 'ä, ö, ü und ß') + '.',
      majuscule: (nom) => (nom ? 'Achtung: Nomen schreibt man groß!' : 'Achtung: Dieses Wort schreibt man klein!'),
      bravoMajuscule: (nom) => (nom ? 'Gut! Aber Nomen schreibt man groß.' : 'Gut! Aber dieses Wort schreibt man klein.'),
      aussiMajuscule: (nom) => 'Und auf den ' + (nom ? 'großen' : 'kleinen') + ' Anfangsbuchstaben.',
      onEcrit: 'So schreibt man es:',
      bravo: 'Super! Alles richtig geschrieben.',
      oui: 'Ja, genau!',
      pasGrave: 'Nicht schlimm!',
      voici: 'So schreibt man das Wort:',
      union: 'Achte auf den Bindestrich!',
      reessaie: 'Nicht ganz … Versuch es noch mal!',
      uJqxy: 'Nach j, q, x und y verliert das ü seine Punkte: Man schreibt u!',
      choix: 'Wähle ein Wort',
      nonQcm: 'Nein, das ist es nicht. Versuch es noch mal!',
      suivant: 'Nächstes Wort',
      resultat: 'Mein Ergebnis',
      aRevoir: (mots) => 'Zum Üben: ' + mots.join(', ') + '.',
      parfait: (nom) => 'Kein einziger Fehler: ' + nom + ' ist sehr stolz auf dich!',
      phrases: {
        // Stufe 1
        Mama: 'Meine ~ liest mir eine Geschichte vor.',
        Papa: 'Mein ~ kocht heute Nudeln.',
        Oma: 'Meine ~ backt einen Kuchen.',
        Opa: 'Mein ~ arbeitet im Garten.',
        ja: 'Ich nicke und sage ~.',
        nein: 'Ich schüttle den Kopf und sage ~.',
        rot: 'Die Erdbeere ist ~.',
        blau: 'Der Himmel ist ~.',
        gut: 'Der Kuchen schmeckt ~.',
        Tag: 'Heute ist ein schöner ~.',
        Tisch: 'Das Essen steht auf dem ~.',
        Name: 'Mein ~ ist Lena.',
        hallo: 'Ich sage ~ zu meinem Freund.',
        eins: 'Nach null kommt ~.',
        zwei: 'Ich habe ~ Hände.',
        // Stufe 2
        Garten: 'Die Blumen wachsen im ~.',
        Schule: 'Ich gehe zu Fuß zur ~.',
        Strand: 'Wir bauen eine Sandburg am ~.',
        Wasser: 'Ich trinke ein Glas ~.',
        Freund: 'Tom ist mein bester ~.',
        Musik: 'Ich höre gern ~.',
        Bruder: 'Mein kleiner ~ ist drei Jahre alt.',
        Schwester: 'Meine große ~ ist zehn.',
        Zimmer: 'Mein ~ ist aufgeräumt.',
        Straße: 'Das Auto fährt auf der ~.',
        Familie: 'Ich habe meine ~ lieb.',
        Sonntag: 'Am ~ gehen wir in den Park.',
        Morgen: 'Am ~ esse ich mein Frühstück.',
        danke: 'Ich sage ~ für das Geschenk.',
        bitte: 'Gib mir ~ den Ball.',
        // Stufe 3
        Kapitän: 'Der ~ steuert das Schiff.',
        Abenteuer: 'Wir erleben ein großes ~.',
        Schatzkarte: 'Die ~ zeigt den Weg zum Schatz.',
        Sturm: 'Im ~ sind die Wellen hoch.',
        Pirat: 'Der ~ versteckt seinen Schatz.',
        Kompass: 'Der ~ zeigt nach Norden.',
        Reise: 'Wir machen eine lange ~.',
        Bibliothek: 'Ich leihe ein Buch in der ~ aus.',
        Geburtstag: 'Heute ist mein ~.',
        geheimnisvoll: 'Die alte Höhle ist ~.',
        Entdecker: 'Der ~ findet eine neue Insel.',
        Mannschaft: 'Unsere ~ gewinnt das Spiel.',
        Fernrohr: 'Mit dem ~ sehe ich weit.',
        Horizont: 'Die Sonne geht am ~ unter.',
        Leuchtturm: 'Der ~ steht am Meer.',
      },
    },
    lb: {
      nom: 'Coco',
      article: 'art', // on dicte « eng Kaz », « en Hond », « e Buch »
      ouvre: '„',
      ferme: '“',
      bonjour: (nom) => 'Moien, ech sinn de ' + nom + '!',
      introVoix: (n) => 'Ech soen der ' + n + ' Wierder. Lauschter gutt no a schreif se!',
      introMemo: (n, s) => 'Ech weisen der ' + n + ' Wierder. Kuck gutt: All Wuert flitt no '
        + s + ' Sekonnen ewech. Dann schreif et!',
      introQcm: (n) => 'Ech soen der ' + n + ' Wierder. Lauschter gutt no a wiel dat richtegt Wuert!',
      introMemoQcm: (n, s) => 'Ech weisen der ' + n + ' Wierder. All Wuert flitt no ' + s
        + ' Sekonnen ewech. Dann wiel dat richtegt Wuert!',
      astuceVoix: 'Du kanns d’Wuert ëmmer nach eng Kéier héieren.',
      astuceMemo: 'Du kanns d’Wuert nach eng Kéier kucken.',
      astuceEssais: 'Du hues zwee Versich fir all Wuert.',
      astuceAccents: 'Opgepasst op ä, é, ë a grouss Buschtawen: si zielen!',
      astuceSansAccents: 'Keng Suerg mat ä, é, ë a grousse Buschtawen: ech weisen der, wéi et richteg ass.',
      astuceQcm: 'Lauschter no a wiel dat richtegt Wuert.',
      astucePinyin: 'Schreif de Pinyin.',
      commencer: 'Lass geet et',
      consigneVoix: (nom) => 'Lauschter dem ' + nom + ' no a schreif d’Wuert.',
      consigneMemo: 'Mierk der d’Wuert a schreif et dann.',
      consigneQcm: (nom) => 'Lauschter dem ' + nom + ' no a wiel dat richtegt Wuert.',
      consigneMemoQcm: 'Mierk der d’Wuert a wiel et dann.',
      perroquet: (nom) => 'De Papagei ' + nom,
      image: 'Bild vum Wuert',
      reecouter: 'Nach eng Kéier',
      lentement: 'Lues',
      revoir: 'Nach eng Kéier kucken',
      sourd: 'Ech héieren näischt',
      sourdAide: 'Du héiers de Papagei net? Da gëtt d’Wuert gewisen.',
      sonCoupe: (nom) => 'Den Toun ass aus: dréck uewen op 🔇, fir de ' + nom + ' ze héieren.',
      motAEcrire: '(d’Wuert)',
      ecrisIci: 'Schreif d’Wuert hei',
      ecrisApres: (art) => 'Schreif d’Wuert no „' + art + '“',
      ecrisPinyin: 'Schreif de Pinyin hei',
      touches: 'Speziell Buschtawen',
      inserer: (c) => '„' + c + '“ schreiwen',
      valider: 'Iwwerpréiwen',
      regarde: 'Kuck gutt …',
      aToi: 'Elo bass du drun!',
      vide: 'Schreif d’Wuert fir d’éischt an d’Feld.',
      accents: (diff) => 'Opgepasst op ' + (diff.length ? listeEt(' an ', '„', '“')(diff) : 'ä, é an ë') + '!',
      bravoAccents: (diff) => 'Gutt! Mee opgepasst op ' + (diff.length ? listeEt(' an ', '„', '“')(diff) : 'ä, é an ë') + '.',
      majuscule: (nom) => 'Opgepasst: D’Wuert fänkt mat engem ' + (nom ? 'grousse' : 'klenge') + ' Buschtaf un!',
      bravoMajuscule: (nom) => 'Gutt! Mee d’Wuert fänkt mat engem ' + (nom ? 'grousse' : 'klenge') + ' Buschtaf un.',
      aussiMajuscule: (nom) => 'An op de ' + (nom ? 'grousse' : 'klenge') + ' Buschtaf.',
      onEcrit: 'Esou schreift een et:',
      bravo: 'Super! Alles richteg geschriwwen.',
      oui: 'Jo, genee!',
      pasGrave: 'Net schlëmm!',
      voici: 'Esou schreift een d’Wuert:',
      union: 'Opgepasst op de Bindestréch!',
      reessaie: 'Net ganz … Probéier nach eng Kéier!',
      uJqxy: 'No j, q, x an y schreift een ü ouni Punkten, also u!',
      choix: 'Wiel e Wuert',
      nonQcm: 'Nee, dat ass et net. Probéier nach eng Kéier!',
      suivant: 'Nächst Wuert',
      resultat: 'Mäi Resultat',
      aRevoir: (mots) => 'Fir ze iwwen: ' + mots.join(', ') + '.',
      parfait: (nom) => 'Kee Feeler: de ' + nom + ' ass ganz houfreg op dech!',
      // Phrases simples ; règle de l'n appliquée avec le mot mis à la place de « ~ ».
      phrases: {
        // Niveau 1
        Mamm: 'Meng ~ ass doheem.',
        Papp: 'Mäi ~ liest e Buch.',
        Bomi: 'Meng ~ erzielt eis eng spannend Geschicht.',
        Bopa: 'Mäi ~ huet e Gaart.',
        jo: 'Ech soe ~.',
        nee: 'Hie seet ~.',
        rout: 'D’Blumm ass ~.',
        blo: 'Den Himmel ass ~.',
        gutt: 'De Kuch ass ~.',
        Dësch: 'D’Iesse steet um ~.',
        Stull: 'D’Kaz sëtzt um ~.',
        Numm: 'Mäi ~ ass Lea.',
        moien: 'Ech soe ~ zu mengem Frënd.',
        eent: 'Ech zielen: ~, zwee, dräi.',
        zwee: 'Eent, ~, dräi!',
        // Niveau 2
        Gaart: 'D’Blumme wuessen am ~.',
        Schoul: 'Ech ginn an d’~.',
        Strooss: 'Den Auto fiert op der ~.',
        Waasser: 'Ech drénken e Glas ~.',
        Mëllech: 'D’Kou gëtt ~.',
        Kéis: 'D’Mous iesst gär ~.',
        Musek: 'Ech lauschtere gär ~.',
        Brudder: 'Mäi klenge ~ ass dräi Joer al.',
        Schwëster: 'Meng ~ molt en Haus.',
        Zëmmer: 'Mäi ~ ass ganz kleng.',
        Kichen: 'D’Mamm kacht an der ~.',
        Fënster: 'D’Kaz kuckt duerch d’~.',
        Famill: 'Ech hu meng ~ gär.',
        Sonndeg: 'Haut ass ~.',
        merci: 'Ech soe ~.',
        // Niveau 3
        Kapitän: 'De ~ steiert d’Schëff.',
        Matrous: 'De ~ klëmmt op de Mast.',
        Pirat: 'De ~ verstoppt säi Schatz.',
        Schatzkaart: 'De Kapitän kuckt op seng ~.',
        Stuerm: 'De ~ ass ganz staark.',
        Kompass: 'De ~ weist de Wee.',
        Rees: 'Mir maachen eng laang ~.',
        Bibliothéik: 'Ech léinen e Buch an der ~.',
        Gebuertsdag: 'Haut ass mäi ~.',
        Geheimnis: 'Dat ass mäi ~.',
        Entdecker: 'Den ~ fënnt eng nei Insel.',
        Equipe: 'Eis ~ gewënnt de Match.',
        Luuchttuerm: 'De ~ steet um Mier.',
        Horizont: 'D’Sonn geet um ~ ënner.',
        Abenteuer: 'Mir erliewen e grousst ~.',
      },
    },
    fr: {
      nom: 'Coco',
      article: 'def', // on dicte « la pomme », « l’île » : le genre aide à reconnaître le mot
      ouvre: '«' + NB,
      ferme: NB + '»',
      bonjour: (nom) => 'Bonjour, je suis ' + nom + ' !',
      introVoix: (n) => 'Je vais te dire ' + n + ' mots. Écoute bien et écris-les !',
      introMemo: (n, s) => 'Je vais te montrer ' + n + ' mots. Regarde bien : chaque mot s’envole au bout de '
        + s + NB + 'secondes. Ensuite, écris-le !',
      introQcm: (n) => 'Je vais te dire ' + n + ' mots. Écoute bien et choisis le bon !',
      introMemoQcm: (n, s) => 'Je vais te montrer ' + n + ' mots. Chaque mot s’envole au bout de ' + s + NB
        + 'secondes. Ensuite, choisis le bon !',
      astuceVoix: 'Tu peux me réécouter autant de fois que tu veux.',
      astuceMemo: 'Tu peux revoir le mot si tu l’as oublié.',
      astuceEssais: 'Tu as deux essais pour chaque mot.',
      astuceAccents: 'Attention aux accents : ils comptent !',
      astuceSansAccents: 'Les accents ne sont pas obligatoires, mais je te montrerai où ils vont.',
      astuceQcm: 'Écoute, puis choisis le bon mot.',
      astucePinyin: 'Écris le pinyin sans les tons.',
      commencer: 'Commencer',
      consigneVoix: (nom) => 'Écoute ' + nom + ' et écris le mot.',
      consigneMemo: 'Retiens bien le mot, puis écris-le.',
      consigneQcm: (nom) => 'Écoute ' + nom + ' et choisis le bon mot.',
      consigneMemoQcm: 'Retiens bien le mot, puis choisis-le.',
      perroquet: (nom) => nom + ', le perroquet',
      image: 'Image du mot à écrire',
      reecouter: 'Réécouter',
      lentement: 'Lentement',
      revoir: 'Revoir le mot',
      sourd: 'Je n’entends rien',
      sourdAide: 'Le perroquet ne parle pas ? Le mot s’affichera à la place.',
      sonCoupe: (nom) => 'Le son est coupé : touche 🔇 en haut pour entendre ' + nom + '.',
      motAEcrire: '(le mot à écrire)',
      ecrisIci: 'Écris le mot ici',
      ecrisApres: (art) => 'Écris le mot qui suit « ' + art + ' »',
      ecrisPinyin: 'Écris le pinyin ici',
      touches: 'Lettres spéciales',
      inserer: (c) => 'Écrire « ' + c + ' »',
      valider: 'Valider',
      regarde: 'Regarde bien…',
      aToi: 'À toi d’écrire !',
      vide: 'Écris d’abord le mot dans la case.',
      // diff : lettres du bon mot que l'enfant a écrites sans leur accent (ou accents en trop).
      accents: (diff) => (diff.length && diff.every((c) => c === 'ç') ? 'Attention à la cédille !' : 'Attention aux accents !'),
      bravoAccents: (diff) => 'Bravo ! Regarde bien ' + (diff.length && diff.every((c) => c === 'ç') ? 'la cédille.' : 'les accents.'),
      majuscule: () => 'Attention à la majuscule !',
      bravoMajuscule: () => 'Bravo ! Regarde bien la majuscule.',
      aussiMajuscule: () => 'Et attention à la majuscule.',
      onEcrit: 'On écrit :',
      bravo: 'Bravo ! C’est bien écrit.',
      oui: 'Oui, c’est ça !',
      pasGrave: 'Ce n’est pas grave !',
      voici: 'Voici comment on l’écrit :',
      union: 'Attention au trait d’union !',
      reessaie: 'Pas tout à fait… Réessaie !',
      uJqxy: 'Après j, q, x et y, le ü perd ses deux points : on écrit u !',
      choix: 'Choisis un mot',
      nonQcm: 'Non, ce n’est pas ça. Essaie encore !',
      suivant: 'Mot suivant',
      resultat: 'Voir mon résultat',
      aRevoir: (mots) => (mots.length > 1 ? 'Mots à revoir' : 'Mot à revoir') + ' : ' + mots.join(', ') + '.',
      parfait: (nom) => 'Zéro faute : ' + nom + ' est très fier de toi !',
      phrases: {
        // Niveau 1
        ami: 'Léo est mon ~.',
        papa: 'Mon ~ me raconte une histoire.',
        maman: 'Ma ~ chante une chanson.',
        moto: 'La ~ roule vite.',
        lit: 'Je dors dans mon ~.',
        sac: 'Mon ~ est très lourd.',
        bol: 'Je bois mon chocolat dans un ~.',
        joli: 'Quel ~ dessin !',
        rue: 'Je traverse la ~.',
        nid: 'L’oiseau dort dans son ~.',
        midi: 'Nous mangeons à ~.',
        tasse: 'Mamie boit une ~ de thé.',
        domino: 'Je pose un ~ sur la table.',
        jupe: 'Lina porte une ~ rouge.',
        mardi: 'Aujourd’hui, c’est ~.',
        // Niveau 2
        jardin: 'Les tomates poussent dans le ~.',
        forêt: 'Le loup vit dans la ~.',
        plage: 'Nous jouons sur la ~.',
        ville: 'Paris est une grande ~.',
        école: 'Je vais à l’~ à pied.',
        cahier: 'J’écris dans mon ~.',
        table: 'Le repas est sur la ~.',
        chaise: 'Assieds-toi sur la ~.',
        porte: 'Ferme la ~, s’il te plaît.',
        fenêtre: 'Le chat regarde par la ~.',
        musique: 'J’aime écouter de la ~.',
        copain: 'Tom est mon ~.',
        dimanche: 'Nous irons au parc ~.',
        bonjour: 'Je dis ~ à la maîtresse.',
        merci: 'Je dis ~ à la dame.',
        // Niveau 3
        capitaine: 'Le ~ dirige le bateau.',
        aventure: 'Nous partons pour une grande ~.',
        boussole: 'La ~ indique le nord.',
        tempête: 'Une ~ secoue le bateau.',
        pirate: 'Le ~ cache son coffre.',
        navire: 'Le ~ quitte le port.',
        marin: 'Le ~ hisse la voile.',
        bibliothèque: 'J’emprunte un livre à la ~.',
        anniversaire: 'Demain, c’est mon ~.',
        mystérieux: 'Un bruit ~ sort de la grotte.',
        explorateur: 'L’~ découvre une île.',
        lointain: 'Le bateau part vers un pays ~.',
        équipage: 'Tout l’~ monte sur le pont.',
        'longue-vue': 'J’observe la mer avec une ~.',
        horizon: 'Le soleil se couche à l’~.',
      },
    },
  };

  // Typographie française : espace fine insécable avant ! ? : ; et dans les guillemets.
  function typo(s) {
    if (Ile.getLang() !== 'fr') return String(s);
    return String(s).replace(/ ([!?:;»])/g, NB + '$1').replace(/« /g, '«' + NB);
  }

  // L'écran « Commencer » n'est montré qu'au premier lancement : les navigateurs
  // n'autorisent la voix qu'après un geste de l'enfant. Ensuite, « Rejouer » ou les
  // changements de niveau et de langue sont eux-mêmes des gestes.
  let premiereFois = true;
  // Écouteurs globaux de la partie en cours (retirés à chaque nouvelle partie).
  let ecouteurs = null;

  // --- Comparaison des mots ----------------------------------------------------
  const nfc = (s) => String(s).normalize('NFC');
  // net : espaces nettoyés, apostrophe typographique, ligatures décomposées (œ et oe acceptés).
  function net(s) {
    return nfc(s).trim().replace(/[\u2019']/g, '’').replace(/\s+/g, ' ')
      .replace(/œ/g, 'oe').replace(/Œ/g, 'Oe').replace(/æ/g, 'ae').replace(/Æ/g, 'Ae');
  }
  const minus = (s) => net(s).toLowerCase();
  // Sans accents, trémas ni ß (ß → ss) : Bär ~ Bar, Fësch ~ Fesch, forêt ~ foret.
  const sansMarques = (s) => nfc(String(s).replace(/ß/g, 'ss').replace(/ẞ/g, 'SS').normalize('NFD').replace(/[\u0300-\u036f]/g, ''));
  const souple = (s) => sansMarques(minus(s));
  const soupleCasse = (s) => sansMarques(net(s));
  const compact = (s) => souple(s).replace(/[\s’'-]/g, '');
  const estMarquee = (c) => sansMarques(c) !== c;
  // Pinyin tapé : minuscules, v ou u: → ü, tons (accents ou chiffres), espaces et apostrophes ignorés.
  function pinyinTape(s) {
    return nfc(String(s).toLowerCase().replace(/u:/g, 'ü').replace(/v/g, 'ü').normalize('NFD')
      .replace(/[\u0300-\u0307\u0309-\u036f]/g, '').normalize('NFC').replace(/[1-5\s’'-]/g, ''));
  }
  const sansTon = (p) => String(p || '').normalize('NFD').replace(/[\u0300-\u0307\u0309-\u036f]/g, '')
    .normalize('NFC').replace(/\s+/g, '').toLowerCase();

  // Lettres marquées du bon mot (é, ä, ß…) qui manquent dans la saisie.
  function lettresSansMarque(saisie, cible) {
    const compte = (s, c) => Array.from(s).filter((x) => x === c).length;
    const a = minus(saisie);
    const b = minus(cible);
    return Array.from(new Set(Array.from(b).filter((c) => estMarquee(c) && compte(a, c) < compte(b, c))));
  }

  // Lettres du bon mot absentes de la saisie (plus longue sous-suite commune).
  function lettresARevoir(cible, saisie) {
    const a = Array.from(cible);
    const b = Array.from(saisie || '');
    const M = a.map(() => new Array(b.length + 1).fill(0));
    M.push(new Array(b.length + 1).fill(0));
    for (let x = a.length - 1; x >= 0; x--) {
      for (let y = b.length - 1; y >= 0; y--) {
        M[x][y] = a[x] === b[y] ? M[x + 1][y + 1] + 1 : Math.max(M[x + 1][y], M[x][y + 1]);
      }
    }
    if (M[0][0] < Math.ceil(a.length / 2)) return null; // trop différent : on montre le mot entier
    const marque = new Array(a.length).fill(true);
    let x = 0;
    let y = 0;
    while (x < a.length && y < b.length) {
      if (a[x] === b[y]) { marque[x] = false; x++; y++; } else if (M[x + 1][y] >= M[x][y + 1]) x++; else y++;
    }
    return marque;
  }

  // Touches spéciales sur plusieurs lignes : lignes équilibrées (4 + 3 plutôt que 6 + 1).
  function equilibrer(clavier) {
    if (!clavier || !clavier.isConnected || clavier.hidden) return;
    clavier.style.maxWidth = '';
    const btns = clavier.querySelectorAll('button');
    if (btns.length < 2) return;
    const w = btns[0].getBoundingClientRect().width;
    const gap = parseFloat(window.getComputedStyle(clavier).columnGap) || 0;
    const parLigne = Math.max(1, Math.floor((clavier.clientWidth + gap) / (w + gap)));
    if (!w || parLigne >= btns.length) return;
    const n = Math.ceil(btns.length / Math.ceil(btns.length / parLigne));
    clavier.style.maxWidth = Math.ceil(n * (w + gap) - gap + 1) + 'px';
  }

  // Petit bouton avec un emoji décoratif.
  function bouton(emoji, texte, cls) {
    return el('button', { type: 'button', class: 'btn ' + (cls || '') }, [
      el('span', { 'aria-hidden': 'true', class: 'dictee-emoji', text: emoji }), ' ' + texte,
    ]);
  }
  const pinyinEl = (p, cls) => el('span', { class: 'pinyin' + (cls ? ' ' + cls : ''), lang: 'zh-Latn-pinyin', text: p });

  // Mots écrits à l'écran pendant la partie (titre, grades, consigne, boutons) :
  // on ne les dicte pas, leur orthographe serait sous les yeux de l'enfant.
  // Les mots composés sont coupés (« Papageien-Diktat » montre « Papagei »).
  // Chinois non concerné : on y écrit le pinyin, que l'interface n'affiche jamais.
  function motsAffiches(tx) {
    const g = Ile.game(ID);
    const textes = [g ? g.titre : '', tx.nom, tx.consigneVoix(tx.nom), tx.consigneMemo,
      tx.reecouter, tx.lentement, tx.revoir, tx.sourd, tx.valider, tx.suivant, tx.resultat]
      .concat(Ile.niveaux().map((n) => n.nom + ' ' + n.classe));
    const mots = new Set();
    textes.forEach((t) => String(t).split(/[^\p{L}]+/u).forEach((w) => { if (w) mots.add(souple(w)); }));
    return Array.from(mots);
  }
  // Au plus une lettre de différence (remplacée, ajoutée ou enlevée).
  function uneLettre(a, b) {
    if (Math.abs(a.length - b.length) > 1) return false;
    let k = 0;
    while (k < a.length && a[k] === b[k]) k++;
    return a.slice(k + (a.length >= b.length ? 1 : 0)) === b.slice(k + (b.length >= a.length ? 1 : 0));
  }
  // Même mot ou même famille : Papagei / Papageien, Schiff / Schiffsjunge, Kokosnoss / Kokosnëss.
  function proche(a, b) {
    const n = Math.min(a.length, b.length);
    return a === b || (n >= 4 && (a.startsWith(b) || b.startsWith(a))) || (n >= 6 && uneLettre(a, b));
  }
  const motsDe = (t) => String(t || '').split(/[^\p{L}’'-]+/u).filter(Boolean).map(souple);

  // --- Préparation de la partie ----------------------------------------------
  // Question : { mot (affiché), cible (à écrire : mot, ou pinyin sans tons), emoji, pinyin, article,
  //              articles (acceptés devant le mot), dit (texte prononcé), phrase }
  function preparerChinois(level) {
    const L = Ile.L();
    const vus = new Set();
    const garde = (w) => !!w && !vus.has(w) && vus.add(w);
    const avecImage = Ile.motsPourPartie(level, 999).filter((m) => m.pinyin && garde(m.mot)).map((m) => ({
      mot: m.mot, cible: m.epeler || sansTon(m.pinyin), emoji: m.emoji, pinyin: m.pinyin, articles: [], dit: m.mot,
    }));
    let simples = [];
    for (let n = level; n >= 1; n--) simples = simples.concat(Ile.shuffle((L.MOTS_SIMPLES && L.MOTS_SIMPLES[n]) || []));
    const sansImage = simples.filter((w) => Ile.pinyin(w) && garde(w)).map((w) => ({
      mot: w, cible: sansTon(Ile.pinyin(w)), emoji: null, pinyin: Ile.pinyin(w), articles: [], dit: w,
    }));
    return assembler(avecImage, sansImage);
  }

  function preparer(level, tx) {
    const L = Ile.L();
    const exclus = motsAffiches(tx);
    const vus = new Set();
    const garde = (w) => {
      const k = souple(w);
      if (!w || vus.has(k) || exclus.some((x) => proche(k, x))) return false;
      vus.add(k);
      return true;
    };
    // Mots illustrés : niveau exact d'abord, puis niveaux inférieurs.
    const avecImage = Ile.motsPourPartie(level, 999).filter((m) => garde(m.mot)).map((m) => {
      const art = tx.article ? m[tx.article] : null;
      return { mot: m.mot, cible: m.mot, emoji: m.emoji, article: art, articles: [m.art, m.def], dit: art ? Ile.groupe(art, m.mot) : m.mot };
    });
    // Mots sans image : niveau exact d'abord, avec leur phrase d'exemple.
    let simples = [];
    for (let n = level; n >= 1; n--) simples = simples.concat(Ile.shuffle((L.MOTS_SIMPLES && L.MOTS_SIMPLES[n]) || []));
    const sansImage = simples.filter(garde).map((w) => {
      const p = tx.phrases[w] || null;
      return { mot: w, cible: w, phrase: p, articles: [], dit: p ? w + '. ' + p.replace('~', w) : w };
    });
    return assembler(avecImage, sansImage);
  }

  // TOTAL mots dont NB_SANS_IMAGE sans image ; aucune phrase d'exemple n'écrit un autre mot de la partie.
  function assembler(avecImage, sansImage) {
    const liste = [];
    const conflit = (q) => liste.some((r) => motsDe(r.phrase).indexOf(souple(q.mot)) !== -1
      || motsDe(q.phrase).indexOf(souple(r.mot)) !== -1);
    const prendre = (source, max) => {
      for (const q of source) {
        if (liste.length >= TOTAL || max <= 0) return;
        if (liste.indexOf(q) !== -1 || conflit(q)) continue;
        liste.push(q);
        max--;
      }
    };
    prendre(sansImage, NB_SANS_IMAGE);
    prendre(avecImage, TOTAL - liste.length);
    prendre(sansImage, TOTAL - liste.length); // complète si une des deux listes est trop courte
    return Ile.shuffle(liste);
  }

  // Chinois, niveau 1 : trois autres mots, de même longueur si possible, jamais de même pinyin (书 / 树).
  function distracteurs(q, level) {
    const L = Ile.L();
    const pool = L.MOTS.filter((m) => m.niveau <= level && m.pinyin).map((m) => ({ mot: m.mot, cible: m.epeler, pinyin: m.pinyin }))
      .concat(((L.MOTS_SIMPLES && L.MOTS_SIMPLES[1]) || []).filter((w) => Ile.pinyin(w))
        .map((w) => ({ mot: w, cible: sansTon(Ile.pinyin(w)), pinyin: Ile.pinyin(w) })));
    const vus = new Set([q.mot]);
    const cibles = new Set([q.cible]);
    const n = Array.from(q.mot).length;
    const autres = Ile.shuffle(pool).sort((a, b) => (Array.from(a.mot).length === n ? 0 : 1) - (Array.from(b.mot).length === n ? 0 : 1));
    const choix = [];
    autres.forEach((x) => {
      if (choix.length >= NB_CHOIX - 1 || vus.has(x.mot) || cibles.has(x.cible)) return;
      vus.add(x.mot);
      cibles.add(x.cible);
      choix.push(x);
    });
    return choix;
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      if (ecouteurs) ecouteurs.retirer();
      const L = Ile.L();
      const lang = Ile.getLang();
      const zh = L.ecriture === 'hanzi';
      const qcm = zh && level === 1; // chinois débutant : on choisit les caractères
      const langueSaisie = zh ? 'zh-Latn-pinyin' : (L.htmlLang || lang);
      // Allemand, luxembourgeois : les noms ont une majuscule, exigée aux niveaux 2 et 3.
      const casse = lang === 'de' || lang === 'lb';
      const exigeant = level >= 2;
      const g = Ile.game(ID);
      const panel = el('section', { class: 'panel dictee' + (zh ? ' dictee--zh' : ''), 'aria-label': g ? g.titre : '' });
      root.appendChild(panel);
      // Rotation de la tablette : on rééquilibre les touches tant que la partie est affichée.
      const auRedimensionnement = () => {
        if (!panel.isConnected) { retirer(); return; }
        equilibrer(panel.querySelector('.dictee-touches'));
      };
      function retirer() { window.removeEventListener('resize', auRedimensionnement); }
      window.addEventListener('resize', auRedimensionnement);
      ecouteurs = { retirer };

      const liste = zh ? preparerChinois(level) : preparer(level, tx);
      const langueAccentuee = zh || casse || Object.keys(L.familles || {}).length > 0 || (L.touchesSpeciales || []).length > 0;
      // Touches spéciales : celles du pack, plus les signes des mots de la partie (trait d'union…).
      const touches = (L.touchesSpeciales || []).slice();
      if (!zh) {
        liste.forEach((q) => Array.from(q.cible).forEach((c) => {
          if (!/[\p{L}\s]/u.test(c) && touches.indexOf(c) === -1) touches.push(c);
        }));
      }
      let i = 0;
      let score = 0;
      let sourd = false; // l'enfant a signalé qu'il n'entend pas le perroquet
      const aRevoir = [];

      const modeMemo = () => sourd || !Ile.canSpeak || Ile.isMuted();
      const nomRevoir = (q) => (zh ? q.mot + '（' + q.pinyin + '）' : q.mot);

      function vider() {
        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
      }

      // --- Écran d'accueil ----------------------------------------------------
      function accueil() {
        const memo = modeMemo();
        panel.dataset.etat = 'accueil';
        const go = bouton('▶️', tx.commencer, 'btn--primary dictee-go');
        go.addEventListener('click', () => {
          if (go.disabled || !panel.isConnected) return;
          go.disabled = true;
          premiereFois = false;
          Ile.sfx('click');
          poser();
        });
        const astuces = [
          memo ? ['👀', tx.astuceMemo] : ['🔁', tx.astuceVoix],
          ['✌️', tx.astuceEssais],
        ];
        if (qcm) astuces.push(['👆', tx.astuceQcm]);
        else if (zh) astuces.push(['⌨️', tx.astucePinyin]);
        else if (langueAccentuee) astuces.push(exigeant ? ['⚠️', tx.astuceAccents] : ['✨', tx.astuceSansAccents]);
        const intro = qcm ? (memo ? tx.introMemoQcm(liste.length, MEMO_MS / 1000) : tx.introQcm(liste.length))
          : (memo ? tx.introMemo(liste.length, MEMO_MS / 1000) : tx.introVoix(liste.length));
        panel.append(el('div', { class: 'dictee-intro' }, [
          el('div', { class: 'dictee-perroquet dictee-emoji', 'aria-hidden': 'true', text: '🦜' }),
          el('h2', { text: typo(tx.bonjour(tx.nom)) }),
          el('p', { text: typo(intro) }),
          el('ul', { class: 'dictee-astuces' }, astuces.map(([e, t]) =>
            el('li', {}, [el('span', { 'aria-hidden': 'true', class: 'dictee-emoji', text: e }), el('span', { text: typo(t) })]))),
          el('div', { class: 'actions' }, [go]),
        ]));
        go.focus({ preventScroll: true });
      }

      // --- Une question -------------------------------------------------------
      // reprise : essais déjà faits sur ce mot (même mot reposé en mode « Mémorise ») ;
      // passer en mode « Mémorise » ne redonne pas un premier essai, donc pas de point en plus.
      function poser(reprise) {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= liste.length) { terminer(); return; }
        const q = liste[i];
        const memo = modeMemo();
        const jeton = {};
        panel._jeton = jeton;
        const actif = () => panel.isConnected && panel._jeton === jeton;
        // Le focus était dans le jeu (joueur au clavier) : il y reste pour la question suivante.
        const focusDansLeJeu = !document.activeElement || document.activeElement === document.body
          || panel.contains(document.activeElement);
        let essais = reprise || 0;
        let fini = false;
        let derniereSaisie = '';
        let memoTimer = null;
        let bloque = false; // mot affiché en mode « Mémorise » : réponses bloquées

        vider();
        Ile.progress(panel, i, liste.length, score);
        panel.dataset.question = String(i);
        panel.dataset.etat = 'question';
        panel.dataset.memo = memo ? '1' : '0';
        delete panel.dataset.verdict;

        const dire = (ok, texte) => Ile.feedback(fb, ok, typo(texte));
        const info = (texte) => { fb.className = 'feedback'; fb.textContent = typo(texte); };

        // Le perroquet et sa bulle (image du mot, ou haut-parleur pour les mots sans image).
        const perroquet = el('span', { class: 'dictee-perroquet dictee-emoji', role: 'img', 'aria-label': tx.perroquet(tx.nom), text: '🦜' });
        const bulle = el('div', { class: 'dictee-bulle' });
        const scene = el('div', { class: 'dictee-scene' }, [perroquet, bulle]);
        const emojiEl = (decoratif) => el('span', decoratif ? { class: 'dictee-emoji', 'aria-hidden': 'true', text: q.emoji }
          : { class: 'dictee-emoji', role: 'img', 'aria-label': tx.image, text: q.emoji });
        // Chinois : les caractères restent affichés après le mode « Mémorise » (niveaux 2–3).
        const hanziEl = (avecPinyin) => el('span', { class: 'dictee-hanzi', lang: 'zh-Hans' }, [
          el('span', { class: 'dictee-hanzi__mot', text: q.mot }),
          avecPinyin ? pinyinEl(q.pinyin) : null,
        ]);
        let hanziVu = false;
        function bulleRepos() {
          bulle.classList.remove('is-memo', 'is-solution');
          bulle.innerHTML = '';
          if (q.emoji) bulle.appendChild(emojiEl(false));
          if (hanziVu && !qcm) bulle.appendChild(hanziEl(false));
          else if (!q.emoji) bulle.appendChild(el('span', { class: 'dictee-emoji', 'aria-hidden': 'true', text: memo ? '🙈' : '🔊' }));
        }
        // Chinois : après la réponse, les caractères et le pinyin avec ses tons.
        function bulleSolution() {
          if (!zh) return;
          bulle.classList.remove('is-memo');
          bulle.classList.add('is-solution');
          bulle.innerHTML = '';
          if (q.emoji) bulle.appendChild(emojiEl(true));
          bulle.appendChild(hanziEl(true));
        }
        bulleRepos();

        const consigneTexte = qcm ? (memo ? tx.consigneMemoQcm : tx.consigneQcm(tx.nom)) : (memo ? tx.consigneMemo : tx.consigneVoix(tx.nom));
        const consigne = el('p', { class: 'consigne', text: typo(consigneTexte) });
        const ecoute = el('div', { class: 'actions dictee-ecoute' });
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });

        function parler(lent) {
          if (!actif()) return false;
          // Apostrophe droite pour la synthèse vocale (l’île → l'île).
          const ok = Ile.say(q.dit.replace(/’/g, '\''), lent ? { rate: 0.6 } : undefined);
          if (ok) {
            perroquet.classList.remove('is-talking');
            void perroquet.offsetWidth;
            perroquet.classList.add('is-talking');
          } else if (Ile.isMuted()) {
            info(tx.sonCoupe(tx.nom));
          }
          return ok;
        }
        perroquet.addEventListener('animationend', () => perroquet.classList.remove('is-talking'));
        // La liste des voix arrive parfois après le début de la partie : s'il n'y a finalement aucune
        // voix pour la langue (souvent le luxembourgeois), le même mot passe en mode « Mémorise ».
        function sansVoix() {
          if (fini || !actif() || Ile.canSpeak || Ile.isMuted()) return false;
          sourd = true;
          poser(essais);
          return true;
        }

        const focusSaisie = () => { if (input && !input.disabled) input.focus({ preventScroll: true }); };
        let revoir = null;
        if (memo) {
          revoir = bouton('👀', tx.revoir, 'dictee-revoir');
          revoir.addEventListener('click', () => { if (!fini && actif()) { Ile.sfx('flip'); montrer(); } });
          ecoute.append(revoir);
        } else {
          const re = bouton('🔁', tx.reecouter, 'dictee-reecouter');
          const lent = bouton('🐢', tx.lentement, 'dictee-lent');
          re.addEventListener('click', () => {
            if (fini || !actif()) return;
            if (!parler(false) && !sansVoix()) Ile.flash(re, 'bad');
            focusSaisie();
          });
          lent.addEventListener('click', () => {
            if (fini || !actif()) return;
            if (!parler(true) && !sansVoix()) Ile.flash(lent, 'bad');
            focusSaisie();
          });
          ecoute.append(re, lent);
        }

        // Phrase d'exemple pour les mots sans image, avec un blanc à la place du mot.
        let phrase = null;
        if (q.phrase) {
          const morceaux = q.phrase.split('~');
          // La ponctuation qui suit le blanc (jusqu'à la première espace) reste sur la même ligne.
          const apres = typo((morceaux[1] || '') + tx.ferme);
          const k = apres.indexOf(' ');
          const colle = k === -1 ? apres : apres.slice(0, k);
          phrase = el('p', { class: 'dictee-phrase', lang: langueSaisie }, [
            typo(tx.ouvre + morceaux[0]),
            el('span', { class: 'dictee-colle' }, [
              el('span', { class: 'dictee-blanc' }, [NB, el('span', { class: 'sr-only', text: tx.motAEcrire })]),
              colle,
            ]),
            k === -1 ? '' : apres.slice(k),
          ]);
        }

        // --- Réponse : caractères à choisir (chinois niveau 1) ou mot à écrire ---
        let input = null;
        let valider = null;
        let clavier = null;
        let zoneValider = null;
        let grille = null;
        // Annoncée aux lecteurs d'écran : la bonne orthographe suit le message (« Nicht schlimm! »).
        const zoneCorrection = el('div', { class: 'dictee-zone-correction', 'aria-live': 'polite' });
        let reponse;
        if (qcm) {
          const options = Ile.shuffle([q].concat(distracteurs(q, level)));
          grille = el('div', { class: 'choices dictee-choix', role: 'group', 'aria-label': tx.choix });
          options.forEach((o) => {
            const b = el('button', { type: 'button', class: 'btn choice dictee-choix__btn', 'data-mot': o.mot }, [
              el('span', { class: 'dictee-choix__han', lang: 'zh-Hans', text: o.mot }),
              pinyinEl(o.pinyin, 'dictee-choix__py is-cache'),
            ]);
            b.addEventListener('click', () => choisir(b, o));
            grille.appendChild(b);
          });
          reponse = el('div', { class: 'dictee-form' }, [grille, fb, zoneCorrection]);
        } else {
          input = el('input', {
            type: 'text', class: 'answer', lang: langueSaisie, autocomplete: 'off', autocapitalize: 'off', autocorrect: 'off',
            spellcheck: 'false', enterkeyhint: 'done', maxlength: '40',
            'aria-label': zh ? tx.ecrisPinyin : q.article ? tx.ecrisApres(q.article) : tx.ecrisIci,
          });
          const saisie = el('div', { class: 'dictee-saisie' }, [
            q.article ? el('span', { class: 'dictee-article', lang: langueSaisie, 'aria-hidden': 'true', text: q.article }) : null,
            input,
          ]);
          clavier = el('div', { class: 'dictee-touches', role: 'group', 'aria-label': tx.touches },
            touches.map((c) => {
              const b = el('button', { type: 'button', class: 'dictee-touche', lang: langueSaisie, 'aria-label': tx.inserer(c), title: tx.inserer(c), text: c });
              b.addEventListener('mousedown', (e) => e.preventDefault()); // garde le curseur dans la case
              b.addEventListener('click', () => inserer(c));
              return b;
            }));
          clavier.hidden = !touches.length;
          valider = el('button', { type: 'submit', class: 'btn btn--primary dictee-valider' }, [
            el('span', { 'aria-hidden': 'true', text: '✓' }), ' ' + tx.valider,
          ]);
          zoneValider = el('div', { class: 'actions' }, [valider]);
          // Le retour et la correction s'affichent juste sous la case, pour rester visibles
          // sur téléphone ; une fois le mot terminé, ils remplacent les touches spéciales.
          reponse = el('form', { class: 'dictee-form', novalidate: true, autocomplete: 'off' }, [
            saisie, fb, zoneCorrection, clavier, zoneValider,
          ]);
          reponse.addEventListener('submit', (e) => { e.preventDefault(); verifier(); });
        }

        const bas = el('div', { class: 'actions dictee-bas' });
        if (!memo) {
          const sourdBtn = bouton('🙉', tx.sourd, 'btn--ghost dictee-sourd');
          sourdBtn.title = tx.sourdAide;
          sourdBtn.addEventListener('click', () => {
            if (fini || !actif()) return;
            sourd = true;
            Ile.sfx('click');
            poser(essais); // même mot, en mode « Mémorise », sans rendre les essais déjà faits
          });
          bas.append(sourdBtn);
        }

        panel.append(scene, consigne, ecoute);
        if (phrase) panel.append(phrase);
        panel.append(reponse, bas);
        if (clavier) equilibrer(clavier);

        function inserer(c) {
          if (!input || input.disabled || fini) return;
          const debut = input.selectionStart == null ? input.value.length : input.selectionStart;
          const finSel = input.selectionEnd == null ? debut : input.selectionEnd;
          input.setRangeText(c, debut, finSel, 'end');
          input.focus({ preventScroll: true });
          Ile.sfx('click');
        }

        function bloquerSaisie(b) {
          bloque = b;
          if (input) {
            input.disabled = b;
            valider.disabled = b;
            clavier.querySelectorAll('button').forEach((x) => { x.disabled = b; });
          }
          if (grille) {
            grille.querySelectorAll('.choice').forEach((x) => { if (!x.classList.contains('is-wrong')) x.disabled = b; });
          }
        }

        // Mode Mémorise : le mot s'affiche quelques secondes (avec son image) puis s'envole.
        // Chinois : on montre le pinyin avec ses tons (et les caractères aux niveaux 2–3, qui restent ensuite).
        function montrer() {
          if (!actif() || fini) return;
          clearTimeout(memoTimer);
          bulle.innerHTML = '';
          if (q.emoji) bulle.appendChild(emojiEl(true));
          if (zh) {
            if (!qcm) hanziVu = true;
            bulle.appendChild(qcm ? pinyinEl(q.pinyin, 'dictee-memo-py') : hanziEl(true));
          } else {
            bulle.appendChild(el('span', { class: 'mot-affiche' + (Array.from(q.mot).length > 9 ? ' mot-affiche--long' : ''), lang: langueSaisie, text: q.mot }));
          }
          bulle.style.setProperty('--memo-ms', MEMO_MS + 'ms');
          bulle.classList.remove('is-memo', 'is-solution');
          void bulle.offsetWidth;
          bulle.classList.add('is-memo');
          bloquerSaisie(true);
          if (revoir) revoir.disabled = true;
          info(tx.regarde);
          memoTimer = setTimeout(() => {
            if (!actif() || fini) return;
            bulleRepos();
            bloquerSaisie(false);
            if (revoir) revoir.disabled = false;
            info(tx.aToi);
            if (input) input.focus({ preventScroll: true });
            else if (grille) { const b = grille.querySelector('.choice:not(:disabled)'); if (b) b.focus({ preventScroll: true }); }
          }, MEMO_MS);
        }

        function motCorrige() {
          const marque = lettresARevoir(q.cible, derniereSaisie);
          const span = el('span', { class: 'dictee-correction__mot', lang: langueSaisie });
          Array.from(q.cible).forEach((c, k) => {
            span.appendChild(marque && marque[k] ? el('mark', { text: c }) : document.createTextNode(c));
          });
          return span;
        }

        function montrerCorrection(texte) {
          if (clavier) clavier.hidden = true;
          if (zoneValider) zoneValider.hidden = true;
          zoneCorrection.innerHTML = '';
          zoneCorrection.appendChild(el('div', { class: 'dictee-correction' }, [
            el('span', { class: 'dictee-correction__label', text: typo(texte) }),
            motCorrige(),
          ]));
        }

        function boutonSuivant() {
          const dernier = i + 1 >= liste.length;
          const suite = bouton(dernier ? '🏁' : '➜', dernier ? tx.resultat : tx.suivant, 'btn--sun dictee-suivant');
          const cree = Date.now();
          suite.addEventListener('click', () => {
            // La touche Entrée qui a validé ne doit pas aussi passer au mot suivant.
            if (suite.disabled || !actif() || Date.now() - cree < 350) return;
            suite.disabled = true;
            Ile.sfx('click');
            i++;
            poser();
          });
          bas.innerHTML = '';
          zoneCorrection.appendChild(el('div', { class: 'actions' }, [suite]));
          setTimeout(() => {
            if (!actif()) return;
            suite.focus({ preventScroll: true });
            suite.scrollIntoView({ block: 'nearest', behavior: Ile.reduceMotion ? 'auto' : 'smooth' });
          }, 0);
        }

        function terminerMot() {
          fini = true;
          clearTimeout(memoTimer);
          panel.dataset.etat = 'reponse';
          bloquerSaisie(true);
          ecoute.querySelectorAll('button').forEach((b) => { b.disabled = true; });
          bas.innerHTML = '';
          bulleSolution();
          if (grille) {
            grille.classList.add('is-fini');
            grille.querySelectorAll('.dictee-choix__py').forEach((p) => p.classList.remove('is-cache'));
          }
        }

        // tolere : [message, faute] quand une faute tolérée au niveau 1 (accents, majuscule) a été acceptée.
        function reussi(tolere) {
          terminerMot();
          if (essais === 1) score++;
          else aRevoir.push(nomRevoir(q));
          if (input) Ile.flash(input, 'good');
          Ile.sfx('good');
          Ile.progress(panel, i + 1, liste.length, score);
          if (tolere) {
            // Accepté malgré les accents ou la majuscule (niveau 1) : on montre la bonne orthographe.
            dire(true, tolere);
            montrerCorrection(tx.onEcrit);
            boutonSuivant();
          } else {
            dire(true, essais === 1 ? tx.bravo : tx.oui);
            if (zh && !memo) setTimeout(() => { if (actif()) parler(false); }, 300);
            setTimeout(() => { if (actif()) { i++; poser(); } }, DELAI_SUIVANT + (zh ? 500 : 0));
          }
        }

        function rate() {
          terminerMot();
          aRevoir.push(nomRevoir(q));
          if (input) Ile.flash(input, 'bad');
          Ile.sfx('bad');
          Ile.progress(panel, i + 1, liste.length, score);
          dire(false, tx.pasGrave);
          if (input) montrerCorrection(tx.voici);
          if (!memo) setTimeout(() => { if (actif()) parler(false); }, 500);
          boutonSuivant();
        }

        // --- Chinois, niveau 1 : choisir les caractères ---
        function choisir(b, o) {
          if (fini || bloque || b.disabled || !actif()) return;
          essais++;
          if (o === q) {
            panel.dataset.verdict = 'exact';
            b.classList.add('is-correct');
            Ile.flash(b, 'good');
            reussi(null);
            return;
          }
          panel.dataset.verdict = 'faux';
          b.classList.add('is-wrong');
          b.disabled = true;
          Ile.flash(b, 'bad');
          if (essais >= 2) {
            const bon = grille.querySelector('.choice[data-mot="' + q.mot + '"]');
            if (bon) bon.classList.add('is-correct');
            rate();
            return;
          }
          Ile.sfx('bad');
          dire(false, tx.nonQcm);
          const reste = grille.querySelector('.choice:not(:disabled)');
          if (reste) reste.focus({ preventScroll: true });
          if (!memo) setTimeout(() => { if (actif() && !fini) parler(false); }, 600);
        }

        // --- Mot écrit ---
        // 'exact' | 'majuscule' (seule la casse diffère) | 'accents' (accents, ä ö ü, ß) | 'union' (tiret, espace)
        // | 'jqxy' (pinyin : ü écrit après j, q, x, y au lieu de u) | 'faux'
        function juger(brut) {
          if (zh) {
            const s = pinyinTape(brut);
            if (s === q.cible) return { verdict: 'exact', saisie: s };
            // ü (ou v) tapé après j, q, x, y : on entend bien ü dans yú, jú zi, xué xiào, mais il s'écrit u.
            if (/[jqxy]ü/.test(s) && s.replace(/([jqxy])ü/g, '$1u') === q.cible) return { verdict: 'jqxy', saisie: s };
            // u tapé là où le pinyin écrit ü (lǜ, nǚ) ; un ü en trop ailleurs est simplement faux.
            if (/ü/.test(q.cible) && s.replace(/ü/g, 'u') === q.cible.replace(/ü/g, 'u')) return { verdict: 'accents', diff: ['ü'], saisie: s };
            return { verdict: 'faux', saisie: s };
          }
          const s = net(brut);
          const essaisPossibles = [s];
          // L'enfant a pu écrire l'article devant le mot : « la pomme », « l’île », « die Katze », « eng Kaz ».
          q.articles.forEach((a) => {
            const art = minus(a || '');
            if (!art) return;
            const pref = /’$/.test(art) ? art : art + ' ';
            if (minus(s).startsWith(pref)) essaisPossibles.push(s.slice(pref.length).trim());
          });
          const cible = q.cible;
          const nom = /^\p{Lu}/u.test(cible);
          const exacte = essaisPossibles.find((c) => net(c) === net(cible));
          if (exacte !== undefined) return { verdict: 'exact', saisie: exacte };
          const casseSeule = essaisPossibles.find((c) => minus(c) === minus(cible));
          if (casseSeule !== undefined) return casse ? { verdict: 'majuscule', nom, saisie: casseSeule } : { verdict: 'exact', saisie: casseSeule };
          const accent = essaisPossibles.find((c) => souple(c) === souple(cible));
          if (accent !== undefined) {
            // Lettres marquées oubliées (Bar pour Bär) ou, à défaut, ajoutées à tort (Hünd pour Hund).
            const oubliees = lettresSansMarque(accent, cible);
            return { verdict: 'accents', diff: oubliees.length ? oubliees : lettresSansMarque(cible, accent), saisie: accent, nom,
              casse: casse && soupleCasse(accent) !== soupleCasse(cible) };
          }
          if (essaisPossibles.some((c) => compact(c) === compact(cible))) return { verdict: 'union' };
          return { verdict: 'faux' };
        }

        function verifier() {
          if (fini || bloque || !input || input.disabled || !actif()) return;
          const saisieNette = net(input.value);
          if (!saisieNette) {
            panel.dataset.verdict = 'vide';
            info(tx.vide);
            input.focus({ preventScroll: true });
            return;
          }
          essais++;
          const j = juger(saisieNette);
          derniereSaisie = j.saisie || (zh ? pinyinTape(saisieNette) : saisieNette);
          panel.dataset.verdict = j.verdict;
          if (j.verdict === 'exact') { reussi(null); return; }
          // Niveau 1 : accents et majuscule tolérés (on montre quand même la bonne orthographe).
          if (!exigeant && j.verdict === 'majuscule') { reussi(tx.bravoMajuscule(j.nom)); return; }
          if (!exigeant && j.verdict === 'accents') {
            reussi(tx.bravoAccents(j.diff) + (j.casse ? ' ' + tx.aussiMajuscule(j.nom) : ''));
            return;
          }
          if (essais >= 2) { rate(); return; }
          // Premier essai manqué : un indice précis, puis un deuxième essai.
          Ile.flash(input, 'bad');
          Ile.sfx('bad');
          let msg;
          if (j.verdict === 'majuscule') msg = tx.majuscule(j.nom);
          else if (j.verdict === 'accents') msg = tx.accents(j.diff) + (j.casse ? ' ' + tx.aussiMajuscule(j.nom) : '');
          else if (j.verdict === 'union') msg = tx.union;
          else if (j.verdict === 'jqxy') msg = tx.uJqxy;
          else msg = tx.reessaie;
          dire(false, msg);
          input.focus({ preventScroll: true });
          if (!memo && j.verdict === 'faux') setTimeout(() => { if (actif() && !fini) parler(false); }, 600);
        }

        // Lancement de la question
        if (memo) {
          montrer();
        } else {
          if (!parler(false) && sansVoix()) return;
          if (input) input.focus({ preventScroll: true });
          else if (grille && focusDansLeJeu && (i > 0 || reprise)) grille.querySelector('.choice').focus({ preventScroll: true });
        }
      }

      function terminer() {
        if (!panel.isConnected) return;
        panel._jeton = null;
        panel.dataset.etat = 'fin';
        Ile.progress(panel, liste.length, liste.length, score);
        const message = aRevoir.length ? tx.aRevoir(aRevoir) : tx.parfait(tx.nom);
        // Score exact : un point par mot écrit juste du premier coup (le message rappelle les autres).
        Ile.showResult({ id: ID, score, total: liste.length, message: typo(message) });
      }

      if (premiereFois) accueil();
      else poser();
    },
  });
})();
