/* L'Île aux Mots : cours « Flotte ou coule ? » (Float or sink?), catégorie 🔬 Sciences.
   A fun lesson first (under a minute, 4 or 5 animated steps, voice and a short written line, ⏭️ skips it), then 6 or 7 rounds
   in a big aquarium drawn in HTML: the thing hangs above the water, drops in with a splash, bobs on top or sinks with bubbles.
   1 (the little one, all by picture and voice): the lesson shows a duck that floats and a stone that sinks;
     then three things on a shelf, one floats: touch it and it drops into the water (a thing that sinks shows it by sinking).
   2: the lesson adds the rule with a seesaw (the thing against its water twin, same size but made of water);
     then guess "floats" or "sinks" for 6 things before they drop, and a 7th round sorts all six into two tubs (tap or drag).
   3 (surprises, read to answer): the ice cube, the apple, the fresh egg, the steel ship, a heavy piece of wood against a light key,
     the clay ball then the same clay made into a boat: guess, watch, then choose the written reason among three (tempting wrong ones:
     "the clay got lighter", "steel is lighter than water", "its engine holds it up", "heavy things always float", "because it is a fruit",
     "because it is too big"); a wrong reason is answered in writing as well as aloud.
   4 (experiments and written questions): make a sinking egg float (salt), a boat that carries at most C grams and coins of w grams
     (how many coins at most: division with a remainder, then one more coin sinks it), cubes of the same size as a 100 g cube of water
     (touch all the ones that float, then ✔️), and two written questions (oil, submarine, loading a ship, iceberg, sea or pool).
   English, German, Luxembourgish or Chinese, never French. Sounds made with the Web Audio API, silent in the recette.

   Facts used (all checked; kept simple, never wrong):
   1. Something floats if it is lighter than the same amount (the same size, the same volume) of water; heavier, it sinks (Archimedes).
   2. Air is very light: lots of air inside helps a thing float (ball, rubber duck, apple). Not "everything with air floats" (a steel tank of air sinks).
   3. Metal things sink: key, coin, anchor, paper clip dropped into water (steel ~7.9, copper alloys ~8.9 times as heavy as water).
   4. A piece of wood and a cork float ("this wood": most woods are lighter than water, a few very dense ones sink, so never "all wood").
   5. Ice floats: water gets bigger when it freezes (about 9 % more), so ice (0.92) is lighter than the same amount of water;
      most of an ice cube (about nine tenths) stays under the water. An iceberg in the sea: about nine tenths under water (0.917 / 1.025 ≈ 0.89).
   6. An apple floats: it has lots of air inside (about a fifth to a quarter of its volume).
   7. A fresh egg is a bit heavier than the same amount of tap water (about 1.03 to 1.09): it sinks; in very salty water it floats
      (lots of salt: salt water can reach 1.2). Salt water is heavier than the same amount of fresh water; in the sea (about 1.025) you float
      a bit higher than in a swimming pool (a floating body pushes aside its own weight of water: heavier water, less of it, less of you under water).
   8. A ball of modelling clay sinks (clay about 1.3 to 1.8); the same clay shaped as a boat floats: same weight, but the boat shape holds air,
      boat and air together are lighter than the same amount of water. Same idea for a hollow steel ship.
   9. Heavy or light alone does not decide: a big piece of wood (kilos) floats, a small key (grams) sinks.
   10. A boat sits deeper with every load; too heavy, water comes in and it sinks. The coins round is a stated premise (the boat carries
      at most C grams, C never a multiple of w): n = floor(C / w) coins float, n + 1 sink. Wrong answers are shown the same way.
   11. Cubes of the same size as a 100 g cube of water weigh, with real densities: cork 20 g (0.12 to 0.24), this wood 60 g (0.4 to 0.9),
      candle wax 90 g (0.88 to 0.92), ice 92 g (0.917), glass 250 g (2.4 to 2.8), stone 270 g (granite 2.6 to 2.8), iron 787 g (7.87),
      gold 1930 g (19.3). Under 100 g it floats, over 100 g it sinks.
   12. Cooking oil (about 0.92) floats on water. A submarine dives by letting water into its tanks (heavier) and comes up by blowing
      air into them to push the water out. More boxes on a ship: it sits deeper and deeper.
   13. A fresh grape sinks in tap water (level 2): sugary juice and no air inside make it a bit heavier than the same amount of water
      (about 1.05 to 1.1). A nice contrast with the apple, a fruit that floats: "because it is a fruit" is a wrong reason (level 3).
   14. An egg that has been kept a long time can float (its air pocket grows): level 3 says "this egg is fresh" so the answer is one.
   15. A steel ship floats with its engine stopped: the engine is not what holds it up (level 3 wrong reason).

   Luxembourgish checked on lod.lu (API, 29/09/2026): schwammen (hie schwëmmt, "schwammen um Waasser", example "et schwëmmt en dënne Film
   Mazout um Waasser"), ënnergoen (geet ënner, example "d'Boot ass ënnergaangen"), Waasser n., Loft f., Loftblos f., Salz n., salzeg,
   Séisswaasser n., Mier n., Schwämm f. (Schwimmbad), Fong m. (Grund eines Gewässers), uewen, ënnen, déif, huel, eidel, Stopp m. (Korken),
   Äiswierfel m., Klamer f. (Büroklammer), Schëff n., Unterseeboot n., Plastilinn m., Kugel f., Mënz f. (pl. Mënzen), Steen m., Holz n.,
   Stéck n., Anker m., Fieder f., Blat n., Ueleg m., Äisbierg m., Eisen n., Gold n., Wuess m. (Wachs), Kork m., Glas n., Wierfel m.,
   Gréisst f., gradesou (genauso), Deel m. ("de gréissten Deel"), Hallschent f., Zéngtel m., Spëtzt f., Gewiicht n., Form f., Tank m.,
   Këscht f., lueden (du luets), droen (hie dréit), bleiwen (bleift), réieren (umrühren), derbäimaachen (hinzugeben), fëllen, blosen (bléist),
   drécken (dréckt), loossen (léisst), kommen (kënnt), fréieren, vergläichen, erausfannen, roden, geheien, sortéieren, iwwersprangen,
   probéieren, manner, héchstens, ongeféier, zesummen, wichteg, mëll, ronn, wäiss, séier, waarm, Kapitän, Iwwerraschung f., Bidden f. (Wanne),
   Saach f., Luucht f. (Licht), ausmaachen, passéieren. Eifel rule kept (e kal, e schwëmmt, Mënze kanns, Wierfele vun, zesumme si, maache mer).
   To have reread by a Luxembourgish speaker: "gradesou vill Waasser" for "the same amount of water"; the capital after a colon
   ("…: Hie schwëmmt!", as in German); "Do ass vill Loft dran", "An engem Apel ass vill Loft"; "Wann d'Waasser zu Äis gëtt, gëtt et méi grouss";
   "De gréissten Deel vum Äiswierfel ass ënner Waasser"; "E frëscht Ee"; "Dëst grousst Schëff ass aus Stol"; "Et ass bannen huel";
   "An der Bootsform ass vill Loft"; "Deeselwechte Plastilinn, datselwecht Gewiicht"; "Et léisst Waasser eran"; "Fir erëm eropzekommen,
   bléist et Loft an d'Tanken"; "Wat d'Schëff méi schwéier ass, wat et méi déif läit"; "Wou dréit d'Waasser dech besser"; "D'Mierwaasser"; "Dofir schwëmms du am Mier méi héich"; "Et léisst Waasser a seng Tanken";
   "Méi Waasser derbäimaachen"; "Wat maache mer, fir datt d'Ee schwëmmt?"; "Denk drun"; "Sortéier se an déi richteg Bidden";
   added by the second reader (lod.lu checked: Fruucht f., haart, Motor m., halen "hie hält", frësch, Wierfel pl. Wierfelen, weien "et weit"):
   "Well en eng Fruucht ass"; "Eng Drauf ass och eng Fruucht, mee se geet ënner"; "Dëst Ee ass frësch"; "Well d'Schuel haart ass";
   "Well et ze grouss ass"; "De Motor hält et uewen"; "Denk drun: Ass et méi liicht ewéi gradesou vill Waasser, da schwëmmt et.
   Ass et méi schwéier, da geet et ënner."; "Tipp all d'Wierfelen un, déi schwammen, dann ✔️";
   "d'Drauf" for one single grape (the lexicon has the plural d'Drauwen, lod.lu DRAUF1; German "die Weintraube" in the everyday sense of one grape). */
(() => {
const ID = "flotte", INK = "#1B2D45", SURF = .3, FLOOR = .89;
const LG = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
const anim = (n, frames, o) => { if (!n || calm()) return null; try { return n.animate(frames, o); } catch(e) { return null; } };
const pause = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 5 : ms)));
const later = (ms, f) => loops.push(setTimeout(f, TEST ? 5 : ms));
// German and Luxembourgish pronouns after the verb ("Schwimmt der Apfel oder sinkt er?", "… oder geet en ënner?")
const PR = {de: {m: "er", f: "sie", n: "es"}, lb: {m: "en", f: "se", n: "et"}};

/* ---------- drawings (100 x 100 boxes) ---------- */
const DUCK = `<path d="M20 60 C18 80 34 90 56 90 C78 90 92 80 92 62 C92 52 90 44 86 40 C84 50 76 55 68 55 L60 55 C63 50 65 44 65 38 C65 24 55 16 43 16 C30 16 21 26 21 38 C21 46 25 52 31 55 C25 55 20 57 20 60 Z" fill="#FFD23F" stroke="${INK}" stroke-width="4" stroke-linejoin="round"/>
  <path d="M22 35 C13 33 6 37 6 41 C10 47 18 47 25 43 Z" fill="#FF8C42" stroke="${INK}" stroke-width="3.5" stroke-linejoin="round"/>
  <circle cx="39" cy="31" r="5" fill="${INK}"/><circle cx="40.8" cy="29.2" r="1.8" fill="#fff"/><circle cx="31" cy="45" r="4" fill="#FFB3C1" opacity=".8"/>
  <path d="M50 66 C60 60 72 62 78 68 C70 76 58 76 50 66 Z" fill="#F4B400" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>`;
const CORK = `<rect x="12" y="34" width="68" height="34" rx="7" fill="#D9A566" stroke="${INK}" stroke-width="4"/>
  <ellipse cx="80" cy="51" rx="9" ry="17" fill="#E9C08A" stroke="${INK}" stroke-width="4"/>
  ${[[28, 44, 2.6], [44, 57, 2.2], [58, 43, 2.4], [36, 60, 1.8], [66, 58, 2], [81, 46, 1.8], [78, 57, 1.6], [20, 55, 1.8]].map(([x, y, r]) => `<circle cx="${x}" cy="${y}" r="${r}" fill="#A8743A"/>`).join("")}`;
const CLAYBALL = `<circle cx="50" cy="56" r="31" fill="#E4572E" stroke="${INK}" stroke-width="4"/>
  <path d="M33 42 Q40 35 49 37" stroke="#fff" stroke-opacity=".5" stroke-width="5" fill="none" stroke-linecap="round"/>
  <path d="M56 64 q6 -4 12 0 M58 70 q5 -3 9 0" stroke="#A8321A" stroke-width="2.5" fill="none" stroke-linecap="round"/>`;
const CLAYBOAT = `<path d="M5 44 L95 44 Q91 75 50 77 Q9 75 5 44 Z" fill="#E4572E" stroke="${INK}" stroke-width="4" stroke-linejoin="round"/>
  <path d="M11 45 Q50 54 89 45" stroke="#A8321A" stroke-width="3" fill="none"/>
  <path d="M20 57 Q30 53 40 56" stroke="#fff" stroke-opacity=".45" stroke-width="4" fill="none" stroke-linecap="round"/>`;
const GRAPE = `<path d="M50 32 V14" stroke="#6D4C41" stroke-width="4.5" stroke-linecap="round"/>
  <path d="M52 19 C60 10 71 11 76 17 C67 24 58 24 52 19 Z" fill="#66BB6A" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>
  <ellipse cx="50" cy="59" rx="26" ry="29" fill="#8E44AD" stroke="${INK}" stroke-width="4"/>
  <path d="M37 47 Q41 39 50 38" stroke="#fff" stroke-opacity=".55" stroke-width="5" fill="none" stroke-linecap="round"/>`;
const SUBM = `<rect x="8" y="42" width="80" height="34" rx="17" fill="#FFC43D" stroke="${INK}" stroke-width="4"/>
  <rect x="40" y="27" width="22" height="19" rx="4" fill="#FFC43D" stroke="${INK}" stroke-width="4"/>
  <path d="M51 27 V15 H62" stroke="${INK}" stroke-width="4" fill="none" stroke-linecap="round"/>
  ${[28, 48, 68].map(x => `<circle cx="${x}" cy="59" r="6" fill="#9EE0FF" stroke="${INK}" stroke-width="3"/>`).join("")}
  <path d="M88 52 L97 45 V73 L88 66 Z" fill="#FF6F59" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>`;
// the toy boat of level 4 (200 x 100): coins go on board, water floods in when it is too heavy
const HULL = "M6 40 H194 L172 90 Q100 99 28 90 Z";
const BOATSVG = `<path d="M100 40 V6" stroke="${INK}" stroke-width="4"/><path d="M103 8 L130 16 L103 24 Z" fill="#FF6F59" stroke="${INK}" stroke-width="3" stroke-linejoin="round"/>
  <path d="${HULL}" fill="#43AA8B" stroke="${INK}" stroke-width="4.5" stroke-linejoin="round"/>
  <path d="M20 54 H180" stroke="#fff" stroke-opacity=".6" stroke-width="4" stroke-dasharray="10 9"/>
  <path class="fl-flood" d="${HULL}" fill="#2B8CBE" opacity="0" stroke="${INK}" stroke-width="4.5"/><g class="fl-coins"></g>`;

/* ---------- the things: f floats in tap water, sub part under water when floating, box [top, bottom] of the drawing, g gender ---------- */
const O = {
  duck: {svg: DUCK, f: 1, sub: .32, box: [.15, .91], g: "f", en: "rubber duck", de: "die Quietscheente", lb: "d'Int", zh: "小黄鸭"},
  ball: {e: "⚽", f: 1, sub: .12, g: "m", en: "ball", de: "der Ball", lb: "de Ball", zh: "球"},
  boat: {e: "⛵", f: 1, sub: .22, box: [.08, .86], g: "n", en: "boat", de: "das Boot", lb: "d'Boot", zh: "小船"},
  leaf: {e: "🍃", f: 1, sub: .2, g: "n", en: "leaf", de: "das Blatt", lb: "d'Blat", zh: "树叶"},
  feather: {e: "🪶", f: 1, sub: .15, g: "f", en: "feather", de: "die Feder", lb: "d'Fieder", zh: "羽毛"},
  wood: {e: "🪵", f: 1, sub: .55, box: [.2, .8], g: "n", en: "piece of wood", de: "das Stück Holz", lb: "d'Stéck Holz", zh: "木头"},
  cork: {svg: CORK, f: 1, sub: .22, box: [.34, .68], g: "m", en: "cork", de: "der Korken", lb: "de Stopp", zh: "软木塞"},
  apple: {e: "🍎", f: 1, sub: .8, g: "m", en: "apple", de: "der Apfel", lb: "den Apel", zh: "苹果"},
  ice: {e: "🧊", f: 1, sub: .9, g: "m", en: "ice cube", de: "der Eiswürfel", lb: "den Äiswierfel", zh: "冰块"},
  ship: {e: "🚢", f: 1, sub: .4, box: [.12, .86], g: "n", en: "ship", de: "das Schiff", lb: "d'Schëff", zh: "轮船"},
  clayBoat: {svg: CLAYBOAT, f: 1, sub: .55, box: [.44, .77], g: "n", en: "clay boat", de: "das Boot aus Knete", lb: "d'Boot aus Plastilinn", zh: "橡皮泥小船"},
  stone: {e: "🪨", f: 0, g: "m", en: "stone", de: "der Stein", lb: "de Steen", zh: "石头"},
  key: {e: "🔑", f: 0, g: "m", en: "key", de: "der Schlüssel", lb: "de Schlëssel", zh: "钥匙"},
  coin: {e: "🪙", f: 0, g: "f", en: "coin", de: "die Münze", lb: "d'Mënz", zh: "硬币"},
  anchor: {e: "⚓", f: 0, g: "m", en: "anchor", de: "der Anker", lb: "den Anker", zh: "锚"},
  clip: {e: "📎", f: 0, g: "f", en: "paper clip", de: "die Büroklammer", lb: "d'Klamer", zh: "回形针"},
  grape: {svg: GRAPE, f: 0, box: [.14, .88], g: "f", en: "grape", de: "die Weintraube", lb: "d'Drauf", zh: "葡萄"},
  egg: {e: "🥚", f: 0, sub: .92, g: "n", en: "egg", de: "das Ei", lb: "d'Ee", zh: "鸡蛋"},
  clayBall: {svg: CLAYBALL, f: 0, box: [.24, .88], g: "f", en: "clay ball", de: "die Kugel aus Knete", lb: "d'Kugel aus Plastilinn", zh: "橡皮泥球"},
  swimmer: {e: "🏊", f: 1, sub: .78, box: [.2, .8], g: "m"},
  sub: {svg: SUBM, f: 1, sub: .72, box: [.15, .77], g: "n"},
  toyBoat: {svg: BOATSVG, vb: "0 0 200 100", f: 1, sub: .22, box: [.4, .96], g: "n"}
};
const L1F = ["duck", "ball", "boat", "leaf", "feather"], L1S = ["stone", "key", "coin", "anchor"];
const L2F = ["cork", "wood", "apple", "duck", "ball", "feather"], L2S = ["key", "coin", "stone", "clip", "anchor", "grape"];
// cubes as big as a 100 g cube of water (level 4), weights from the real densities
const MAT = {
  cork: {g: 20, c: "#D9A566", en: "cork", de: "Kork", lb: "Kork", zh: "软木", sub: .2},
  wood: {g: 60, c: "#B7793F", en: "wood", de: "Holz", lb: "Holz", zh: "木头", sub: .6},
  wax: {g: 90, c: "#FFF1BF", en: "wax", de: "Wachs", lb: "Wuess", zh: "蜡", sub: .9},
  ice: {g: 92, c: "#D4F1FF", en: "ice", de: "Eis", lb: "Äis", zh: "冰", sub: .92},
  glass: {g: 250, c: "#A5E6E1", en: "glass", de: "Glas", lb: "Glas", zh: "玻璃"},
  stone: {g: 270, c: "#9AA3AD", en: "stone", de: "Stein", lb: "Steen", zh: "石头"},
  iron: {g: 787, c: "#5B6B80", en: "iron", de: "Eisen", lb: "Eisen", zh: "铁"},
  gold: {g: 1930, c: "#FFC43D", en: "gold", de: "Gold", lb: "Gold", zh: "金子"}
};

/* ---------- words ---------- */
const TX = {
  title: {en: "Float or sink?", de: "Schwimmen oder sinken?", lb: "Schwammen oder ënnergoen?", zh: "浮还是沉？"},
  sub: {en: "Guess, drop it in, and watch!", de: "Raten, ins Wasser werfen, zuschauen!", lb: "Roden, an d'Waasser geheien, kucken!", zh: "猜一猜，放进水里，看一看！"},
  learn: {en: "Let's learn!", de: "Wir lernen!", lb: "Mir léieren!", zh: "一起学一学！"},
  skip: {en: "Skip", de: "Überspringen", lb: "Iwwersprangen", zh: "跳过"},
  again: {en: "Again", de: "Nochmal", lb: "Nach eng Kéier", zh: "再听一次"},
  retry: {en: "Try again!", de: "Versuch es noch mal!", lb: "Probéier nach eng Kéier!", zh: "再试一次！"},
  hear: {en: "Read it to me", de: "Vorlesen", lb: "Virliesen", zh: "读给我听"},
  look: {en: "Look!", de: "Schau!", lb: "Kuck!", zh: "看！"},
  floatsBtn: {en: "Floats", de: "Schwimmt", lb: "Schwëmmt", zh: "浮"},
  sinksBtn: {en: "Sinks", de: "Sinkt", lb: "Geet ënner", zh: "沉"},
  q1: {en: "Which one floats?", de: "Was schwimmt?", lb: "Wat schwëmmt?", zh: "哪个会浮起来？"},
  q1s: {en: "Touch it!", de: "Tipp drauf!", lb: "Tipp drop!", zh: "点一点！"},
  predict: {en: n => `Will the ${n} float or sink?`, de: (n, g) => `Schwimmt ${n} oder sinkt ${PR.de[g]}?`,
            lb: (n, g) => `Schwëmmt ${n} oder geet ${PR.lb[g]} ënner?`, zh: n => `${n}会浮起来，还是沉下去？`},
  floats: {en: n => `The ${n} floats!`, de: n => `${cap(n)} schwimmt!`, lb: n => `${cap(n)} schwëmmt!`, zh: n => `${n}浮起来了！`},
  sinks: {en: n => `The ${n} sinks!`, de: n => `${cap(n)} sinkt!`, lb: n => `${cap(n)} geet ënner!`, zh: n => `${n}沉下去了！`},
  sortQ: {en: "Sort them into the right tub!", de: "Sortiere sie in die richtige Wanne!", lb: "Sortéier se an déi richteg Bidden!", zh: "把它们放进对的水盆里！"},
  sortS: {en: "Floats or sinks?", de: "Schwimmen oder sinken?", lb: "Schwammen oder ënnergoen?", zh: "浮还是沉？"}
};
// the lesson of each level: what is written, what is said (the written line is the spoken one unless said otherwise)
const LT = {
  rule1: {en: "Floats: it stays on top. Sinks: it goes to the bottom.", de: "Schwimmen: Es bleibt oben. Sinken: Es geht nach unten.",
          lb: "Schwammen: Et bleift uewen. Ënnergoen: Et geet op de Fong.", zh: "浮：待在上面。沉：沉到下面。"},
  turn1: {en: "Your turn!", de: "Jetzt du!", lb: "Elo du!", zh: "轮到你了！"},
  duck2: {en: "The rubber duck floats: it stays on top of the water.", de: "Die Quietscheente schwimmt: Sie bleibt oben auf dem Wasser.",
          lb: "D'Int schwëmmt: Si bleift uewen um Waasser.", zh: "小黄鸭浮起来了：它待在水面上。"},
  key2: {en: "The key sinks: it goes down to the bottom.", de: "Der Schlüssel sinkt: Er geht bis auf den Boden.",
         lb: "De Schlëssel geet ënner: Hie geet bis op de Fong.", zh: "钥匙沉下去了：它一直沉到水底。"},
  twin2: {en: "Why? Compare it with the same amount of water: same size, but made of water. The apple is lighter: it floats!",
          de: "Warum? Vergleiche mit der gleichen Menge Wasser: gleich groß, aber aus Wasser. Der Apfel ist leichter: Er schwimmt!",
          lb: "Firwat? Vergläich mat gradesou vill Waasser: gradesou grouss, mee aus Waasser. Den Apel ass méi liicht: Hie schwëmmt!",
          zh: "为什么？和同样大小的水比一比：一样大，但是水做的。苹果更轻，所以浮起来！"},
  keyTwin2: {en: "The key is heavier than the same amount of water: it sinks!", de: "Der Schlüssel ist schwerer als die gleiche Menge Wasser: Er sinkt!",
             lb: "De Schlëssel ass méi schwéier ewéi gradesou vill Waasser: Hie geet ënner!", zh: "钥匙比同样大小的水重，所以沉下去！"},
  turn2: {en: "Your turn: float or sink?", de: "Jetzt du: schwimmen oder sinken?", lb: "Elo du: schwammen oder ënnergoen?", zh: "轮到你了：浮还是沉？"},
  apple3: {en: "An apple is lighter than the same amount of water: it floats.", de: "Ein Apfel ist leichter als die gleiche Menge Wasser: Er schwimmt.",
           lb: "En Apel ass méi liicht ewéi gradesou vill Waasser: Hie schwëmmt.", zh: "苹果比同样大小的水轻：它会浮起来。"},
  key3: {en: "A key is heavier than the same amount of water: it sinks.", de: "Ein Schlüssel ist schwerer als die gleiche Menge Wasser: Er sinkt.",
         lb: "E Schlëssel ass méi schwéier ewéi gradesou vill Waasser: Hie geet ënner.", zh: "钥匙比同样大小的水重：它会沉下去。"},
  air3: {en: "Air is very light. Lots of air inside helps things float!", de: "Luft ist sehr leicht. Viel Luft im Inneren hilft beim Schwimmen!",
         lb: "Loft ass ganz liicht. Vill Loft bannen hëlleft beim Schwammen!", zh: "空气很轻。里面有很多空气，能帮东西浮起来！"},
  surprise3: {en: "Watch out, surprises! Guess first, then find out why.", de: "Achtung, Überraschungen! Erst raten, dann herausfinden, warum.",
              lb: "Et gëtt Iwwerraschungen! Fir d'éischt roden, dann erausfannen, firwat.", zh: "注意，有惊喜！先猜一猜，再想想为什么。"},
  rule4: {en: "Remember: lighter than the same amount of water, it floats. Heavier, it sinks.",
          de: "Merke: Ist es leichter als die gleiche Menge Wasser, schwimmt es. Ist es schwerer, sinkt es.",
          lb: "Denk drun: Ass et méi liicht ewéi gradesou vill Waasser, da schwëmmt et. Ass et méi schwéier, da geet et ënner.",
          zh: "记住：比同样大小的水轻，就浮；比它重，就沉。"},
  salt4: {en: "Salt makes water heavier. Salty water carries you better!", de: "Salz macht Wasser schwerer. Salzwasser trägt dich besser!",
          lb: "Salz mécht d'Waasser méi schwéier. Salzwaasser dréit dech besser!", zh: "盐让水变重。盐水能更好地托住你！"},
  boat4: {en: "Every coin pushes the boat deeper. Too heavy: it sinks!", de: "Jede Münze drückt das Boot tiefer. Zu schwer: Es sinkt!",
          lb: "All Mënz dréckt d'Boot méi déif. Ze schwéier: Et geet ënner!", zh: "每放一枚硬币，小船就往水里压下去一点。太重了，就沉到水底！"},
  go4: {en: "Now: experiments and tricky questions!", de: "Jetzt: Experimente und knifflige Fragen!", lb: "Elo: Experimenter a schwiereg Froen!", zh: "现在：做实验，答难题！"}
};
// level 3: why? (the right reason and two tempting wrong ones), then a short explanation
const WHY = {
  ice: {q: {en: "Why does the ice cube float?", de: "Warum schwimmt der Eiswürfel?", lb: "Firwat schwëmmt den Äiswierfel?", zh: "冰块为什么会浮起来？"},
    ok: {en: "Ice is lighter than the same amount of water.", de: "Eis ist leichter als die gleiche Menge Wasser.", lb: "Äis ass méi liicht ewéi gradesou vill Waasser.", zh: "冰比同样大小的水轻。"},
    no: [{en: "Because it is cold.", de: "Weil er kalt ist.", lb: "Well e kal ass.", zh: "因为它很冷。"},
         {en: "Because it is small.", de: "Weil er klein ist.", lb: "Well e kleng ass.", zh: "因为它很小。"}],
    more: {en: "When water turns into ice, it gets bigger. So ice is lighter than the same amount of water. Look: most of the ice cube is under the water!",
           de: "Wenn Wasser zu Eis wird, dehnt es sich aus. Darum ist Eis leichter als die gleiche Menge Wasser. Schau: Der größte Teil des Eiswürfels ist unter Wasser!",
           lb: "Wann d'Waasser zu Äis gëtt, gëtt et méi grouss. Dofir ass Äis méi liicht ewéi gradesou vill Waasser. Kuck: De gréissten Deel vum Äiswierfel ass ënner Waasser!",
           zh: "水变成冰的时候会变大，所以冰比同样大小的水轻。看：冰块的大部分都在水下面！"}},
  apple: {q: {en: "Why does the apple float?", de: "Warum schwimmt der Apfel?", lb: "Firwat schwëmmt den Apel?", zh: "苹果为什么会浮起来？"},
    ok: {en: "There is lots of air inside it.", de: "Er hat viel Luft in sich.", lb: "Do ass vill Loft dran.", zh: "它里面有很多空气。"},
    no: [{en: "Because it is a fruit.", de: "Weil er eine Frucht ist.", lb: "Well en eng Fruucht ass.", zh: "因为它是水果。"},
         {en: "Because it is round.", de: "Weil er rund ist.", lb: "Well e ronn ass.", zh: "因为它是圆的。"}],
    more: {en: "An apple has lots of air inside. So it is lighter than the same amount of water. Round is not the reason: a clay ball is round too, and it sinks! A grape is a fruit too, but it sinks.",
           de: "Ein Apfel hat viel Luft in sich. Darum ist er leichter als die gleiche Menge Wasser. Rund ist nicht der Grund: Eine Knetkugel ist auch rund und sinkt! Eine Weintraube ist auch eine Frucht, aber sie sinkt.",
           lb: "An engem Apel ass vill Loft. Dofir ass e méi liicht ewéi gradesou vill Waasser. Ronn ass net de Grond: Eng Kugel aus Plastilinn ass och ronn a geet ënner! Eng Drauf ass och eng Fruucht, mee se geet ënner.",
           zh: "苹果里面有很多空气，所以比同样大小的水轻。圆不是原因：橡皮泥球也是圆的，却会沉下去！葡萄也是水果，可它会沉下去。"}},
  egg: {pre: {en: "This egg is fresh.", de: "Dieses Ei ist frisch.", lb: "Dëst Ee ass frësch.", zh: "这是一个新鲜的鸡蛋。"},
    q: {en: "Why does the egg sink?", de: "Warum sinkt das Ei?", lb: "Firwat geet d'Ee ënner?", zh: "鸡蛋为什么会沉下去？"},
    ok: {en: "It is a bit heavier than the same amount of water.", de: "Es ist ein bisschen schwerer als die gleiche Menge Wasser.", lb: "Et ass e bësse méi schwéier ewéi gradesou vill Waasser.", zh: "它比同样大小的水重一点。"},
    no: [{en: "Because the shell is hard.", de: "Weil die Schale hart ist.", lb: "Well d'Schuel haart ass.", zh: "因为蛋壳很硬。"},
         {en: "Because it is too big.", de: "Weil es zu groß ist.", lb: "Well et ze grouss ass.", zh: "因为它太大了。"}],
    more: {en: "A fresh egg is a bit heavier than the same amount of water: it sinks. In very salty water, it floats!",
           de: "Ein frisches Ei ist ein bisschen schwerer als die gleiche Menge Wasser: Es sinkt. In sehr salzigem Wasser schwimmt es!",
           lb: "E frëscht Ee ass e bësse méi schwéier ewéi gradesou vill Waasser: Et geet ënner. A ganz salzegem Waasser schwëmmt et!",
           zh: "新鲜鸡蛋比同样大小的水重一点，所以会沉。在很咸的水里，它就会浮起来！"}},
  ship: {pre: {en: "This big ship is made of steel.", de: "Dieses große Schiff ist aus Stahl.", lb: "Dëst grousst Schëff ass aus Stol.", zh: "这艘大轮船是钢做的。"},
    q: {en: "Steel is heavy. Why does the ship float?", de: "Stahl ist schwer. Warum schwimmt das Schiff?", lb: "Stol ass schwéier. Firwat schwëmmt d'Schëff?", zh: "钢很重，轮船为什么能浮起来？"},
    ok: {en: "It is hollow: there is lots of air inside.", de: "Es ist innen hohl: Da ist viel Luft drin.", lb: "Et ass bannen huel: Do ass vill Loft dran.", zh: "它里面是空的，装着很多空气。"},
    no: [{en: "Steel is lighter than water.", de: "Stahl ist leichter als Wasser.", lb: "Stol ass méi liicht ewéi Waasser.", zh: "钢比水轻。"},
         {en: "Its engine holds it up.", de: "Der Motor hält es oben.", lb: "De Motor hält et uewen.", zh: "是发动机把它托起来的。"}],
    more: {en: "Steel is much heavier than water. But the ship is hollow and full of air: ship and air together are lighter than the same amount of water.",
           de: "Stahl ist viel schwerer als Wasser. Aber das Schiff ist innen hohl und voller Luft: Schiff und Luft zusammen sind leichter als die gleiche Menge Wasser.",
           lb: "Stol ass vill méi schwéier ewéi Waasser. Mee d'Schëff ass bannen huel a voller Loft: Schëff a Loft zesumme si méi liicht ewéi gradesou vill Waasser.",
           zh: "钢比水重得多。可是轮船里面是空的，装满了空气：船和空气加起来比同样大小的水轻。"}},
  log: {pre: {en: "The piece of wood is much heavier than the key.", de: "Das Stück Holz ist viel schwerer als der Schlüssel.", lb: "D'Stéck Holz ass vill méi schwéier ewéi de Schlëssel.", zh: "这块木头比钥匙重得多。"},
    ask: {en: "Which one floats?", de: "Was schwimmt?", lb: "Wat schwëmmt?", zh: "哪个会浮起来？"},
    q: {en: "Why does the wood float, even though it is so heavy?", de: "Warum schwimmt das Holz, obwohl es so schwer ist?", lb: "Firwat schwëmmt d'Holz, obwuel et esou schwéier ass?", zh: "木头这么重，为什么还能浮起来？"},
    ok: {en: "This wood is lighter than the same amount of water.", de: "Dieses Holz ist leichter als die gleiche Menge Wasser.", lb: "Dëst Holz ass méi liicht ewéi gradesou vill Waasser.", zh: "这种木头比同样大小的水轻。"},
    no: [{en: "Heavy things always float.", de: "Schwere Sachen schwimmen immer.", lb: "Schwéier Saache schwammen ëmmer.", zh: "重的东西总会浮起来。"},
         {en: "The key is too small.", de: "Der Schlüssel ist zu klein.", lb: "De Schlëssel ass ze kleng.", zh: "钥匙太小了。"}],
    more: {en: "Heavy or light alone does not decide! What counts: is it heavier or lighter than the same amount of water?",
           de: "Schwer oder leicht allein entscheidet nicht! Es zählt: Ist es schwerer oder leichter als die gleiche Menge Wasser?",
           lb: "Schwéier oder liicht eleng, dat ass net wichteg! Wichteg ass: Ass et méi schwéier oder méi liicht ewéi gradesou vill Waasser?",
           zh: "光看轻重不能决定！要看的是：它比同样大小的水重，还是轻？"}},
  clayBall: {q: {en: "Why does the clay ball sink?", de: "Warum sinkt die Kugel aus Knete?", lb: "Firwat geet d'Kugel aus Plastilinn ënner?", zh: "橡皮泥球为什么会沉下去？"},
    ok: {en: "Clay is heavier than the same amount of water.", de: "Knete ist schwerer als die gleiche Menge Wasser.", lb: "Plastilinn ass méi schwéier ewéi gradesou vill Waasser.", zh: "橡皮泥比同样大小的水重。"},
    no: [{en: "Because it is soft.", de: "Weil sie weich ist.", lb: "Well se mëll ass.", zh: "因为它很软。"},
         {en: "Because it is red.", de: "Weil sie rot ist.", lb: "Well se rout ass.", zh: "因为它是红色的。"}],
    more: {en: "Clay is heavier than the same amount of water. But wait: what if we change its shape?",
           de: "Knete ist schwerer als die gleiche Menge Wasser. Aber warte: Was passiert, wenn wir ihre Form ändern?",
           lb: "Plastilinn ass méi schwéier ewéi gradesou vill Waasser. Mee waart: Wat passéiert, wa mir seng Form änneren?",
           zh: "橡皮泥比同样大小的水重。等一等：要是换个形状呢？"}},
  clayBoat: {pre: {en: "Same clay, new shape: a boat!", de: "Dieselbe Knete, neue Form: ein Boot!", lb: "Deeselwechte Plastilinn, nei Form: e Boot!", zh: "同样的橡皮泥，捏成了小船！"},
    q: {en: "Same clay, but now it floats! Why?", de: "Dieselbe Knete, aber jetzt schwimmt sie! Warum?", lb: "Deeselwechte Plastilinn, mee elo schwëmmt en! Firwat?", zh: "还是那块橡皮泥，现在却浮起来了！为什么？"},
    ok: {en: "The boat shape holds lots of air.", de: "In der Bootsform ist viel Luft.", lb: "An der Bootsform ass vill Loft.", zh: "小船的形状里装着很多空气。"},
    no: [{en: "The clay got lighter.", de: "Die Knete ist leichter geworden.", lb: "De Plastilinn ass méi liicht ginn.", zh: "橡皮泥变轻了。"},
         {en: "The water got warmer.", de: "Das Wasser ist wärmer geworden.", lb: "D'Waasser ass méi waarm ginn.", zh: "水变热了。"}],
    more: {en: "Same clay, same weight! But the boat is big and full of air: boat and air together are lighter than the same amount of water.",
           de: "Gleiche Knete, gleiches Gewicht! Aber das Boot ist groß und voller Luft: Boot und Luft zusammen sind leichter als die gleiche Menge Wasser.",
           lb: "Deeselwechte Plastilinn, datselwecht Gewiicht! Mee d'Boot ass grouss a voller Loft: Boot a Loft zesumme si méi liicht ewéi gradesou vill Waasser.",
           zh: "同样的橡皮泥，一样重！可是小船又大又装满了空气：船和空气加起来比同样大小的水轻。"}}
};
// level 4: the egg, the coins, the cubes, the written questions
const EGG = {
  q: {en: "The egg sinks. How can you make it float?", de: "Das Ei sinkt. Wie bringst du es zum Schwimmen?", lb: "D'Ee geet ënner. Wat maache mer, fir datt et schwëmmt?", zh: "鸡蛋沉下去了。怎样才能让它浮起来？"},
  salt: {en: "🧂 Add lots of salt", de: "🧂 Viel Salz dazugeben", lb: "🧂 Vill Salz derbäimaachen", zh: "🧂 加很多盐"},
  water: {en: "💧 Add more water", de: "💧 Mehr Wasser dazugeben", lb: "💧 Méi Waasser derbäimaachen", zh: "💧 再加点水"},
  stir: {en: "🥄 Stir the water", de: "🥄 Das Wasser umrühren", lb: "🥄 D'Waasser réieren", zh: "🥄 把水搅一搅"},
  noWater: {en: "More water, but the same water: the egg stays at the bottom.", de: "Mehr Wasser, aber dasselbe Wasser: Das Ei bleibt unten.",
            lb: "Méi Waasser, mee datselwecht Waasser: D'Ee bleift ënnen.", zh: "水多了，可还是一样的水：鸡蛋还在水底。"},
  noStir: {en: "Stirring does not change the water: the egg stays at the bottom.", de: "Umrühren ändert das Wasser nicht: Das Ei bleibt unten.",
           lb: "Réieren ännert d'Waasser net: D'Ee bleift ënnen.", zh: "搅一搅，水还是一样的：鸡蛋还在水底。"},
  more: {en: "Salt water is heavier than the same amount of fresh water. Now the egg is lighter than the same amount of salt water: it floats!",
         de: "Salzwasser ist schwerer als die gleiche Menge Leitungswasser. Jetzt ist das Ei leichter als die gleiche Menge Salzwasser: Es schwimmt!",
         lb: "Salzwaasser ass méi schwéier ewéi gradesou vill Séisswaasser. Elo ass d'Ee méi liicht ewéi gradesou vill Salzwaasser: Et schwëmmt!",
         zh: "盐水比同样大小的清水重。现在鸡蛋比同样大小的盐水轻，所以浮起来了！"}
};
const COINS = {
  q: {en: (C, w) => `This boat can carry at most ${C} grams. One coin weighs ${w} grams.`, de: (C, w) => `Dieses Boot trägt höchstens ${C} Gramm. Eine Münze wiegt ${w} Gramm.`,
      lb: (C, w) => `D'Boot dréit héchstens ${C} Gramm. Eng Mënz weit ${w} Gramm.`, zh: (C, w) => `这条小船最多能载${C}克。一枚硬币重${w}克。`},
  ask: {en: "How many coins can it carry at most?", de: "Wie viele Münzen kann es höchstens tragen?", lb: "Wéi vill Mënzen dréit et héchstens?", zh: "它最多能装几枚硬币？"},
  ok: {en: (n, w, C) => `${n} × ${w} = ${n * w} grams, less than ${C}: it floats!`, de: (n, w, C) => `${n} × ${w} = ${n * w} Gramm, weniger als ${C}: Es schwimmt!`,
       lb: (n, w, C) => `${n} × ${w} = ${n * w} Gramm, manner ewéi ${C}: Et schwëmmt!`, zh: (n, w, C) => `${n} × ${w} = ${n * w}克，比${C}克少：船还浮着！`},
  oneMore: {en: (m, w, C) => `One more coin: ${m * w} grams, more than ${C}. It sinks!`, de: (m, w, C) => `Noch eine Münze: ${m * w} Gramm, mehr als ${C}. Es sinkt!`,
            lb: (m, w, C) => `Nach eng Mënz: ${m * w} Gramm, méi ewéi ${C}. Et geet ënner!`, zh: (m, w, C) => `再放一枚：${m * w}克，比${C}克多，船沉下去了！`},
  tooMany: {en: (m, w, C) => `${m} coins: ${m} × ${w} = ${m * w} grams, more than ${C}. Too heavy!`, de: (m, w, C) => `${m} Münzen: ${m} × ${w} = ${m * w} Gramm, mehr als ${C}. Zu schwer!`,
            lb: (m, w, C) => `${m} Mënzen: ${m} × ${w} = ${m * w} Gramm, méi ewéi ${C}. Ze schwéier!`, zh: (m, w, C) => `${m}枚硬币：${m} × ${w} = ${m * w}克，比${C}克多，太重了！`},
  tooFew: {en: (k, w) => `${k} × ${w} = ${k * w} grams. It floats, but one more coin still fits!`, de: (k, w) => `${k} × ${w} = ${k * w} Gramm. Es schwimmt, aber eine Münze passt noch dazu!`,
           lb: (k, w) => `${k} × ${w} = ${k * w} Gramm. Et schwëmmt, mee eng Mënz passt nach dobäi!`, zh: (k, w) => `${k} × ${w} = ${k * w}克。船还浮着，还能再放一枚！`}
};
const CUBES = {
  q: {en: "A cube of water weighs 100 grams. Which cubes of the same size float?", de: "Ein Würfel aus Wasser wiegt 100 Gramm. Welche gleich großen Würfel schwimmen?",
      lb: "E Wierfel aus Waasser weit 100 Gramm. Wéi eng Wierfele vun der selwechter Gréisst schwammen?", zh: "一个水做的方块重100克。同样大小的方块，哪些会浮起来？"},
  small: {en: "Touch all the ones that float, then ✔️", de: "Tippe alle schwimmenden Würfel an, dann ✔️", lb: "Tipp all d'Wierfelen un, déi schwammen, dann ✔️", zh: "把会浮起来的方块都点出来，再点✔️"},
  water: {en: "water", de: "Wasser", lb: "Waasser", zh: "水"},
  rule: {en: "Lighter than 100 grams: it floats. Heavier than 100 grams: it sinks.", de: "Leichter als 100 Gramm: Er schwimmt. Schwerer als 100 Gramm: Er sinkt.",
         lb: "Méi liicht ewéi 100 Gramm: Hie schwëmmt. Méi schwéier ewéi 100 Gramm: Hie geet ënner.", zh: "比100克轻：浮。比100克重：沉。"}
};
const QUIZ = {
  oil: {em: "🍳", q: {en: "You pour cooking oil into water. Where does the oil go?", de: "Du gießt Speiseöl ins Wasser. Wohin geht das Öl?", lb: "Du méchs Ueleg an d'Waasser. Wou geet den Ueleg hin?", zh: "把食用油倒进水里，油会跑到哪里？"},
    ok: {en: "It floats on top of the water.", de: "Es schwimmt oben auf dem Wasser.", lb: "Hie schwëmmt uewen um Waasser.", zh: "浮在水面上。"},
    no: [{en: "It sinks to the bottom.", de: "Es sinkt auf den Boden.", lb: "Hie geet op de Fong.", zh: "沉到水底。"},
         {en: "It turns into water.", de: "Es wird zu Wasser.", lb: "Hie gëtt zu Waasser.", zh: "变成了水。"}],
    more: {en: "Oil is lighter than the same amount of water: it floats on top.", de: "Öl ist leichter als die gleiche Menge Wasser: Es schwimmt oben.",
           lb: "Ueleg ass méi liicht ewéi gradesou vill Waasser: Hie schwëmmt uewen.", zh: "油比同样大小的水轻，所以浮在上面。"}},
  sub: {em: "🌊", q: {en: "How does a submarine dive down?", de: "Wie taucht ein U-Boot nach unten?", lb: "Wéi kënnt en Unterseeboot no ënnen?", zh: "潜水艇是怎么潜到水下的？"},
    ok: {en: "It lets water into its tanks: it gets heavier.", de: "Es lässt Wasser in seine Tanks: Es wird schwerer.", lb: "Et léisst Waasser a seng Tanken: Et gëtt méi schwéier.", zh: "它往水舱里灌水，变重了。"},
    no: [{en: "It gets lighter.", de: "Es wird leichter.", lb: "Et gëtt méi liicht.", zh: "它变轻了。"},
         {en: "It switches off its lights.", de: "Es macht das Licht aus.", lb: "Et mécht d'Luucht aus.", zh: "它把灯关了。"}],
    more: {en: "To come back up, it blows air into its tanks and pushes the water out.", de: "Um wieder aufzutauchen, bläst es Luft in seine Tanks und drückt das Wasser hinaus.",
           lb: "Fir erëm eropzekommen, bléist et Loft an d'Tanken an dréckt d'Waasser eraus.", zh: "要浮上来的时候，它往水舱里吹进空气，把水挤出去。"}},
  load: {em: "📦", q: {en: "You load more and more boxes onto a ship. What happens?", de: "Du lädst immer mehr Kisten auf ein Schiff. Was passiert?", lb: "Du luets ëmmer méi Këschten op e Schëff. Wat passéiert?", zh: "你往船上装越来越多的箱子，会怎么样？"},
    ok: {en: "It sits deeper and deeper in the water.", de: "Es liegt immer tiefer im Wasser.", lb: "Et läit ëmmer méi déif am Waasser.", zh: "船在水里越压越深。"},
    no: [{en: "It rises out of the water.", de: "Es steigt aus dem Wasser.", lb: "Et kënnt aus dem Waasser eraus.", zh: "船会升出水面。"},
         {en: "Nothing changes.", de: "Nichts ändert sich.", lb: "Et ännert sech näischt.", zh: "什么都不会变。"}],
    more: {en: "The heavier the ship, the deeper it sits. Too much, and water comes in!", de: "Je schwerer das Schiff, desto tiefer liegt es. Zu viel, und Wasser läuft hinein!",
           lb: "Wat d'Schëff méi schwéier ass, wat et méi déif läit. Ze vill, an d'Waasser leeft eran!", zh: "船越重，就沉得越深。装得太多，水就会灌进来！"}},
  berg: {em: "🧊", q: {en: "An iceberg floats in the sea. How much of it is under the water?", de: "Ein Eisberg schwimmt im Meer. Wie viel davon ist unter Wasser?", lb: "En Äisbierg schwëmmt am Mier. Wéi vill dovun ass ënner Waasser?", zh: "冰山浮在海上。它有多少在水下面？"},
    ok: {en: "Most of it", de: "Der größte Teil", lb: "De gréissten Deel", zh: "大部分"},
    no: [{en: "Only half", de: "Nur die Hälfte", lb: "Just d'Hallschent", zh: "只有一半"},
         {en: "None of it", de: "Gar nichts", lb: "Guer näischt", zh: "一点也没有"}],
    more: {en: "About nine tenths of an iceberg is under the water. We only see the tip!", de: "Etwa neun Zehntel eines Eisbergs sind unter Wasser. Wir sehen nur die Spitze!",
           lb: "Ongeféier néng Zéngtel vun engem Äisbierg sinn ënner Waasser. Mir gesinn nëmmen d'Spëtzt!", zh: "冰山大约十分之九都在水下，我们只看到它的尖儿！"}},
  sea: {em: "🏊", q: {en: "Where does the water carry you better: in the sea or in a swimming pool?", de: "Wo trägt dich das Wasser besser: im Meer oder im Schwimmbad?", lb: "Wou dréit d'Waasser dech besser: am Mier oder an der Schwämm?", zh: "在哪里水更能托住你：大海里还是游泳池里？"},
    ok: {en: "In the sea", de: "Im Meer", lb: "Am Mier", zh: "大海里"},
    no: [{en: "In the swimming pool", de: "Im Schwimmbad", lb: "An der Schwämm", zh: "游泳池里"},
         {en: "It is the same", de: "Überall gleich", lb: "Iwwerall gläich", zh: "都一样"}],
    more: {en: "Sea water is salty: it is heavier than the same amount of fresh water. So you float higher in the sea!", de: "Meerwasser ist salzig: Es ist schwerer als die gleiche Menge Süßwasser. Darum schwimmst du im Meer höher!",
           lb: "D'Mierwaasser ass salzeg: Et ass méi schwéier ewéi gradesou vill Séisswaasser. Dofir schwëmms du am Mier méi héich!", zh: "海水是咸的，比同样大小的淡水重，所以你在海里浮得更高！"}}
};

/* ---------- funny sounds, made with the Web Audio API (silent in the recette) ---------- */
const snd = (() => {
  let c = null, noise = null;
  const ctx = () => { if (TEST) return null; try { c = c || new (window.AudioContext || window.webkitAudioContext)(); if (c.state === "suspended") c.resume(); return c; } catch(e) { return null; } };
  function note(type, f0, f1, dur, vol = .15, at = 0){
    const a = ctx(); if (!a) return;
    const t = a.currentTime + at, o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination); o.start(t); o.stop(t + dur + .05);
  }
  function hiss(dur, vol, at, f0, f1){
    const a = ctx(); if (!a) return;
    if (!noise) { noise = a.createBuffer(1, a.sampleRate, a.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    const t = a.currentTime + at, s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = noise; f.type = "bandpass"; f.frequency.setValueAtTime(f0, t); f.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(t); s.stop(t + dur);
  }
  return {
    splash: () => { hiss(.4, .55, 0, 1800, 350); note("sine", 320, 90, .25, .18); },
    plop: () => note("sine", 380, 950, .14, .14),
    bubble: () => note("sine", 500 + Math.random() * 500, 1300 + Math.random() * 500, .07, .05),
    thud: () => note("sine", 150, 55, .22, .2),
    whoosh: () => hiss(.3, .25, 0, 400, 2400),
    salt: () => hiss(.9, .12, 0, 5000, 7500),
    clink: k => { note("square", 1800 + k * 50, 1800 + k * 50, .05, .04); note("triangle", 2600 + k * 40, 2500, .14, .05, .03); },
    glug: () => [0, 1, 2, 3].forEach(k => note("sine", 320 - k * 45, 140 - k * 20, .12, .12, k * .14)),
    creak: () => note("sawtooth", 150, 110, .45, .035),
    oops: () => note("triangle", 520, 300, .3, .08),
    jingle: () => [0, 4, 7, 4, 12].forEach((s, k) => note("triangle", 523 * Math.pow(2, s / 12), 523 * Math.pow(2, s / 12), .18, .08, k * .15)),
    chime: () => [0, 4, 7, 12].forEach((s, k) => note("triangle", 587 * Math.pow(2, s / 12), 587 * Math.pow(2, s / 12), .3, .09, k * .09))
  };
})();

addStyle(`
.fl-tank,.fl-see{position:relative; align-self:center; width:100%; max-width:520px; aspect-ratio:16/10; border:3px solid var(--ink); border-radius:18px; overflow:hidden; box-shadow:3px 4px 0 var(--ink); --s:clamp(34px,9vw,50px)}
.fl-tank{background:linear-gradient(#E6F6FF,#FFFFFF 30%)}
.fl-water{position:absolute; left:0; right:0; top:30%; bottom:0; background:linear-gradient(#66CCEE,#2B8CBE); transition:background 1s}
.fl-salty .fl-water{background:linear-gradient(#9FE0EE,#4FAFC4)}
.fl-sand{position:absolute; left:0; right:0; bottom:0; height:11%; background:#F2D59B; border-top:2px solid rgba(27,45,69,.25)}
.fl-weed{position:absolute; bottom:5%; font-size:calc(var(--s) * .9); line-height:1; transform-origin:50% 100%; animation:flSway 3.4s ease-in-out infinite}
.fl-shell{position:absolute; bottom:1%; left:58%; font-size:calc(var(--s) * .45); line-height:1}
.fl-fish{position:absolute; top:55%; left:10%; font-size:calc(var(--s) * .6); line-height:1; animation:flSwim 18s linear infinite}
.fl-objs{position:absolute; inset:0}
.fl-front{position:absolute; left:0; right:0; top:30%; bottom:0; background:rgba(160,225,250,.22); border-top:3px solid rgba(255,255,255,.9); pointer-events:none}
.fl-obj{position:absolute; left:50%; top:15%; width:calc(var(--s) * 1.15); height:calc(var(--s) * 1.15); margin:calc(var(--s) * -.575) 0 0 calc(var(--s) * -.575); font-size:var(--s); line-height:1; pointer-events:none}
.fl-in{width:100%; height:100%; display:grid; place-items:center}
.fl-in svg{width:100%; height:100%; overflow:visible}
.fl-big{--s:clamp(54px,15vw,82px)}
.fl-boatel{width:46%; height:auto; aspect-ratio:2/1; margin:-11.5% 0 0 -23%}
.fl-hang .fl-in{animation:flSwing 1.8s ease-in-out infinite; transform-origin:50% -30%}
.fl-bob .fl-in{animation:flBob 2.6s ease-in-out infinite}
.fl-calm .fl-weed,.fl-calm .fl-fish,.fl-calm .fl-in,.fl-calm .fl-beam{animation:none!important; transition:none!important}
@keyframes flSwing{0%,100%{transform:rotate(-6deg)} 50%{transform:rotate(6deg)}}
@keyframes flBob{50%{transform:translateY(-3px) rotate(-3deg)}}
@keyframes flSway{50%{transform:rotate(9deg)}}
@keyframes flSwim{0%{left:6%; transform:scaleX(-1)} 48%{left:84%; transform:scaleX(-1)} 50%{left:84%; transform:scaleX(1)} 98%{left:6%; transform:scaleX(1)} 100%{left:6%; transform:scaleX(-1)}}
.fl-drop,.fl-bub,.fl-grain,.fl-oilb{position:absolute; pointer-events:none; border-radius:50%}
.fl-drop{width:9px; height:9px; margin:-4px; background:#C8F0FF; border:2px solid #2B8CBE}
.fl-bub{width:9px; height:9px; margin:-4px; border:2px solid rgba(255,255,255,.95); background:rgba(255,255,255,.2)}
.fl-grain{width:5px; height:5px; margin:-2px; background:#fff; border:1px solid #8a9aa8; border-radius:1px}
.fl-oilb{width:12px; height:12px; margin:-6px; background:#FFD54A; border:2px solid #C8900E}
.fl-oil{position:absolute; left:0; right:0; top:calc(30% - 12px); height:12px; background:linear-gradient(#FFE58A,#F4C13A); border-bottom:2px solid #C8900E; opacity:0}
.fl-shaker{position:absolute; left:50%; top:1%; font-size:calc(var(--s) * .9); line-height:1; margin-left:calc(var(--s) * -.45); transform-origin:50% 40%}
.fl-mark{position:absolute; font-size:calc(var(--s) * .7); line-height:1; margin:calc(var(--s) * -.35) 0 0 calc(var(--s) * -.35); pointer-events:none}
.fl-berg{position:absolute; inset:0; pointer-events:none}
.fl-berg svg{width:100%; height:100%}
.fl-see{background:linear-gradient(#FFF6DC,#FFFFFF)}
.fl-beam{position:absolute; left:8%; right:8%; top:60%; height:14px; background:var(--sun); border:3px solid var(--ink); border-radius:8px; transition:transform 1.1s cubic-bezier(.3,1.5,.5,1)}
.fl-pan{position:absolute; bottom:calc(100% + 2px); width:1.15em; height:1.15em; font-size:clamp(46px,13vw,76px); line-height:1; display:grid; place-items:center}
.fl-pan svg{width:100%; height:100%}
.fl-pl{left:1%} .fl-pr{right:1%}
.fl-twin{width:100%; height:100%; display:grid; place-items:center; filter:grayscale(1) sepia(1) hue-rotate(165deg) saturate(4) brightness(1.05); opacity:.85}
.fl-pan i{position:absolute; right:-.12em; top:-.22em; font-size:.42em; font-style:normal}
.fl-pivot{position:absolute; left:42%; width:16%; top:calc(60% + 12px); bottom:12%; background:#C98B4F; clip-path:polygon(50% 0, 100% 100%, 0 100%)}
.fl-ground{position:absolute; left:0; right:0; bottom:0; height:12%; background:#9ED36A; border-top:3px solid var(--ink)}
.fl-shelf{display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; width:100%; max-width:520px; align-self:center}
.fl-item{background:#fff; font-size:clamp(44px,12vw,70px); line-height:1; padding:10px 0; display:grid; place-items:center}
.fl-item svg{width:1.15em; height:1.15em}
.fl-item.fl-used{opacity:.3}
.fl-item.ok{background:#C9F2DF; opacity:1}
.fl-big-i{font-size:clamp(56px,16vw,90px)}
.fl-two{display:grid; grid-template-columns:1fr 1fr; gap:12px; width:100%; max-width:520px; align-self:center}
.fl-ans{background:#fff; display:flex; align-items:center; justify-content:center; gap:8px; padding:10px 6px; font-family:var(--display); font-size:clamp(18px,5vw,26px); font-weight:700}
.fl-ans[disabled],.fl-why[disabled],.fl-item[disabled]{cursor:default}
.fl-ans.ok,.fl-why.ok{background:#C9F2DF}
.fl-ans.ko,.fl-why.ko{background:#FFD6CF}
.fl-ico{width:44px; height:44px; flex:none}
.fl-line{margin:0; min-height:1.3em; text-align:center; font-family:var(--display); font-size:clamp(17px,4.4vw,23px); font-weight:600}
.fl-whys{display:flex; flex-direction:column; gap:10px; width:100%; max-width:560px; align-self:center}
.fl-why{background:#fff; text-align:left; padding:12px 14px; font-size:clamp(16px,4.2vw,20px); font-weight:700; line-height:1.25}
.fl-why.ko{opacity:.55}
.fl-cap{margin:0; text-align:center; font-family:var(--display); font-size:clamp(19px,4.8vw,26px); font-weight:600; min-height:2.6em}
.fl-cap small{display:block; font-family:var(--body); font-size:.72em; color:var(--ink-soft); font-weight:700; margin-top:4px}
.fl-foot{justify-content:space-between; width:100%; max-width:520px; align-self:center}
.fl-steps{display:flex; gap:6px}
.fl-steps i{width:12px; height:12px; border-radius:50%; background:#fff; border:2px solid var(--ink)}
.fl-steps i.on{background:var(--sun)}
.fl-skip{font-size:30px; line-height:1; padding:8px 18px; background:#fff}
.fl-tubs{display:grid; grid-template-columns:1fr 1fr; gap:12px; width:100%; max-width:520px; align-self:center}
.fl-tub{background:#fff; padding:8px; display:flex; flex-direction:column; gap:6px; align-items:stretch; font-family:var(--display); font-weight:700; font-size:clamp(16px,4.4vw,22px)}
.fl-tub b{display:flex; align-items:center; justify-content:center; gap:6px}
.fl-tub .fl-ico{width:34px; height:34px}
.fl-tubw{position:relative; height:clamp(76px,21vw,116px); border:3px solid var(--ink); border-radius:10px 10px 18px 18px; background:linear-gradient(#EAF8FF 0 24%, #66CCEE 24%, #2B8CBE); overflow:hidden}
.fl-tubw span{position:absolute; font-size:clamp(22px,6.4vw,32px); line-height:1; width:1.15em; height:1.15em; margin:-.575em 0 0 -.575em; display:grid; place-items:center}
.fl-tubw svg,.fl-queue svg,.fl-cur svg{width:100%; height:100%}
.fl-cur{align-self:center; width:1.3em; height:1.3em; display:grid; place-items:center; font-size:clamp(58px,16vw,92px); line-height:1; background:#fff; border:3px dashed var(--ink); border-radius:18px; touch-action:none; cursor:grab}
.fl-queue{align-self:center; display:flex; gap:6px; min-height:30px; font-size:26px; line-height:1}
.fl-queue span{width:1.1em; height:1.1em; display:grid; place-items:center; opacity:.75}
.fl-nums{display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:10px; width:100%; max-width:520px; align-self:center}
.fl-nums .num{font-size:34px; padding:8px 0}
.fl-nums .num[disabled]{opacity:.45}
.fl-sum{align-self:center; margin:0; min-height:1.3em; font-family:var(--display); font-weight:700; font-size:clamp(18px,4.6vw,24px)}
.fl-ref{align-self:center; display:flex; align-items:center; gap:8px; font-family:var(--display); font-weight:700; font-size:clamp(18px,4.6vw,24px)}
.fl-ref svg{width:46px; height:46px}
.fl-cubes{display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:8px; width:100%; max-width:520px; align-self:center}
.fl-cube{position:relative; background:#fff; padding:6px 2px; display:flex; flex-direction:column; align-items:center; gap:2px; font-weight:800; font-size:clamp(12px,3.4vw,16px)}
.fl-cube svg{width:clamp(38px,11vw,60px); height:clamp(38px,11vw,60px)}
.fl-cube b{font-family:var(--display); font-size:1.2em}
.fl-cube.fl-sel{background:#FFE9A8}
.fl-cube.fl-sel::after{content:"✔️"; position:absolute; top:-8px; right:-2px; font-size:20px}
.prompt .fl-ico{width:1.3em; height:1.3em; vertical-align:middle}
.fl-cube.ok{background:#C9F2DF} .fl-cube.ko{background:#FFD6CF}
.fl-check{align-self:center; font-size:26px; padding:10px 36px}
.fl-check[disabled]{opacity:.5}
`);

const face = o => o.svg ? `<svg viewBox="${o.vb || "0 0 100 100"}" aria-hidden="true">${o.svg}</svg>` : o.e;
const ICON = f => `<svg class="fl-ico" viewBox="0 0 40 40" aria-hidden="true"><rect x="3" y="7" width="34" height="30" rx="5" fill="#fff" stroke="${INK}" stroke-width="3"/>
  <rect x="5.5" y="15" width="29" height="19.5" rx="3" fill="#66CCEE"/><circle cx="20" cy="${f ? 14.5 : 29}" r="5.5" fill="#FF6F59" stroke="${INK}" stroke-width="2.5"/></svg>`;
// a cube of a material, its weight written on it
const cubeSVG = (c, label, water) => `<svg viewBox="0 0 100 100" aria-hidden="true"><rect x="10" y="10" width="80" height="80" rx="12" fill="${c}" stroke="${INK}" stroke-width="5"${water ? ' fill-opacity=".8"' : ""}/>
  <path d="M22 26 H48" stroke="#fff" stroke-opacity=".55" stroke-width="6" stroke-linecap="round"/>${label ? `<text x="50" y="66" text-anchor="middle" font-size="24" font-weight="800" fill="${INK}" font-family="Nunito,sans-serif">${label}</text>` : ""}</svg>`;

/* ---------- the aquarium ---------- */
function tank(){
  const wrap = el("div", "fl-tank" + (calm() ? " fl-calm" : ""));
  wrap.innerHTML = `<div class="fl-water"></div><div class="fl-sand"></div><span class="fl-weed" style="left:4%">🌿</span>
    <span class="fl-weed" style="right:6%; animation-delay:-1.3s">🌿</span><span class="fl-shell">🐚</span><span class="fl-fish">🐟</span>
    <div class="fl-objs"></div><div class="fl-front"></div>`;
  const objs = wrap.querySelector(".fl-objs");
  const H = () => wrap.clientHeight || 210;
  const size = n => n.offsetHeight || 46;
  const band = o => o.box || [.1, .9];
  // the centre of a floating thing: the part `sub` of the drawing under the surface
  const floatY = (n, o, sub = o.sub) => { const [a, b] = band(o); return H() * SURF + (.5 - (b - sub * (b - a))) * size(n); };
  const bottomY = (n, o) => H() * FLOOR + size(n) * .04 - (band(o)[1] - .5) * size(n);
  const topOf = n => parseFloat(n.style.top) || H() * .15;
  function add(o, x, cls){
    const n = el("div", "fl-obj" + (cls ? " " + cls : ""), `<div class="fl-in">${face(o)}</div>`);
    n.style.left = x + "%"; objs.append(n);
    n.style.top = Math.max(size(n) * .55, H() * .15) + "px"; // above the water
    return n;
  }
  const place = (n, o, floats, sub) => { n.classList.remove("fl-hang"); n.style.top = (floats ? floatY(n, o, sub) : bottomY(n, o)) + "px"; if (floats) n.classList.add("fl-bob"); };
  function splash(x){
    if (calm()) return;
    const y = H() * SURF;
    for (let k = 0; k < 7; k++) {
      const d = el("span", "fl-drop"); d.style.left = x + "%"; d.style.top = y + "px"; objs.append(d);
      const dx = (k - 3) * 11 + Math.random() * 6, up = 22 + Math.random() * 26;
      const a = anim(d, [{transform: "translate(0,0)"}, {transform: `translate(${dx * .6}px,${-up}px)`, offset: .45}, {transform: `translate(${dx}px,${6}px)`, opacity: .2}],
        {duration: 620 + Math.random() * 200, easing: "ease-out"});
      if (a) a.onfinish = () => d.remove(); else d.remove();
    }
  }
  function bubbles(x, y, n = 4){
    if (calm()) return;
    for (let k = 0; k < n; k++) later(k * 170, () => {
      const b = el("span", "fl-bub"); b.style.left = (x + Math.random() * 6 - 3) + "%"; b.style.top = y + "px"; objs.append(b);
      const rise = Math.max(8, y - H() * SURF);
      const a = anim(b, [{transform: "translate(0,0) scale(.6)", opacity: 1}, {transform: `translate(${Math.random() * 10 - 5}px,${-rise}px) scale(1.1)`, opacity: .9, offset: .92}, {transform: `translate(0,${-rise}px) scale(1.6)`, opacity: 0}],
        {duration: 500 + rise * 6, easing: "ease-in"});
      snd.bubble();
      if (a) a.onfinish = () => b.remove(); else b.remove();
    });
  }
  // drop a thing that hangs above the water: it floats (bobbing, part under water) or sinks to the sand
  function drop(n, o, floats = !!o.f, sub = o.sub){
    n.classList.remove("fl-hang");
    const y0 = topOf(n), s = size(n), surf = H() * SURF, x = parseFloat(n.style.left) || 50;
    const end = floats ? floatY(n, o, sub) : bottomY(n, o);
    n.style.top = end + "px";
    if (calm()) { if (floats) n.classList.add("fl-bob"); return pause(250); }
    if (floats) {
      const dur = 1500;
      anim(n, [{top: y0 + "px", easing: "cubic-bezier(.55,0,1,.45)"}, {top: (Math.max(end, surf) + s * .35) + "px", offset: .4, easing: "ease-out"},
        {top: (end - s * .16) + "px", offset: .62, easing: "ease-in-out"}, {top: (end + s * .06) + "px", offset: .8, easing: "ease-in-out"}, {top: end + "px"}], {duration: dur});
      later(dur * .3, () => { splash(x); snd.splash(); });
      later(dur * .55, () => snd.plop());
      later(dur, () => n.classList.add("fl-bob"));
      return pause(dur + 100);
    }
    const dur = 2300;
    anim(n, [{top: y0 + "px", easing: "cubic-bezier(.55,0,1,.45)"}, {top: surf + "px", offset: .17, easing: "ease-out"},
      {top: (surf + (end - surf) * .3) + "px", offset: .38, easing: "linear"}, {top: end + "px", easing: "ease-in"}], {duration: dur});
    anim(n.firstElementChild, [{transform: "rotate(0)"}, {transform: "rotate(-11deg)"}, {transform: "rotate(8deg)"}, {transform: "rotate(-4deg)"}, {transform: "rotate(0)"}], {duration: dur});
    later(dur * .16, () => { splash(x); snd.splash(); });
    later(dur * .3, () => bubbles(x, surf + (end - surf) * .35, 4));
    later(dur, () => snd.thud());
    return pause(dur + 100);
  }
  // glide to another height (the egg rises, the boat goes deeper, the submarine dives)
  function move(n, y, dur = 900, easing = "ease-in-out"){
    const y0 = topOf(n); n.style.top = y + "px";
    anim(n, [{top: y0 + "px"}, {top: y + "px"}], {duration: dur, easing});
    return pause(calm() ? 60 : dur);
  }
  // salt poured from a shaker: the water becomes salty
  function salt(){
    const sh = el("span", "fl-shaker", "🧂"); wrap.append(sh);
    anim(sh, [{transform: "rotate(0)"}, {transform: "rotate(150deg)", offset: .2}, {transform: "rotate(130deg)", offset: .35}, {transform: "rotate(160deg)", offset: .5}, {transform: "rotate(140deg)", offset: .7}, {transform: "rotate(0)"}], {duration: 1800});
    snd.salt();
    if (!calm()) for (let k = 0; k < 22; k++) later(250 + k * 50, () => {
      const g = el("span", "fl-grain"); g.style.left = (46 + Math.random() * 10) + "%"; g.style.top = (H() * .12) + "px"; objs.append(g);
      const a = anim(g, [{transform: "translateY(0)", opacity: 1}, {transform: `translateY(${H() * (.35 + Math.random() * .35)}px)`, opacity: 0}], {duration: 900, easing: "ease-in"});
      if (a) a.onfinish = () => g.remove(); else g.remove();
    });
    later(900, () => wrap.classList.add("fl-salty"));
    later(1850, () => sh.remove());
    return pause(1900);
  }
  // an arrow or a mark written in the water, next to a thing
  function mark(txt, x, y){ const m = el("span", "fl-mark", txt); m.style.left = x + "%"; m.style.top = y + "px"; objs.append(m);
    anim(m, [{transform: "scale(0)"}, {transform: "scale(1.3)", offset: .6}, {transform: "scale(1)"}], {duration: 450, easing: "ease-out"}); return m; }
  return {wrap, objs, H, add, place, drop, move, salt, splash, bubbles, mark, floatY, bottomY};
}

// the toy boat of level 4: coins go on board one by one, it sits deeper, too heavy and water floods in
function toyBoat(tk){
  const B = O.toyBoat, n = tk.add(B, 50, "fl-boatel");
  const coins = n.querySelector(".fl-coins"), flood = n.querySelector(".fl-flood");
  let count = 0;
  tk.place(n, B, true, .22);
  return {n,
    async load(frac){ // one more coin; frac = load / what the boat carries
      const k = count++, x = 32 + (k % 7) * 22.5, y = 33 - Math.floor(k / 7) * 9;
      coins.insertAdjacentHTML("beforeend", `<ellipse cx="${x}" cy="${y}" rx="10" ry="5" fill="#FFC43D" stroke="${INK}" stroke-width="2.5"/>`);
      anim(coins.lastElementChild, [{transform: "translateY(-70px)", opacity: 0}, {transform: "translateY(2px)", opacity: 1, offset: .8}, {transform: "none"}], {duration: 320, easing: "ease-in"});
      snd.clink(k);
      await pause(300);
      if (frac <= 1) return tk.move(n, tk.floatY(n, B, .22 + .62 * frac), 380, "ease-out");
      flood.setAttribute("opacity", ".85"); snd.glug();
      anim(n.firstElementChild, [{transform: "rotate(0)"}, {transform: "rotate(-12deg)"}], {duration: 1500, fill: "forwards"});
      tk.bubbles(50, tk.H() * .6, 6);
      return tk.move(n, tk.bottomY(n, B), 1700, "ease-in");
    }};
}

// the seesaw of the lessons: the thing against its water twin (same size, made of water)
function seesaw(o){
  const w = el("div", "fl-see" + (calm() ? " fl-calm" : ""));
  w.innerHTML = `<div class="fl-ground"></div><div class="fl-pivot"></div><div class="fl-beam"><span class="fl-pan fl-pl">${face(o)}</span>
    <span class="fl-pan fl-pr"><span class="fl-twin">${face(o)}</span><i>💧</i></span></div>`;
  const beam = w.querySelector(".fl-beam");
  // the lighter side goes up: a positive angle lifts the thing (left) and lowers the water (right)
  return {wrap: w, tilt(a){ beam.style.transform = `rotate(${a}deg)`; if (!calm()) { snd.creak(); later(550, () => snd.thud()); } return pause(1300); }};
}

/* ---------- the rounds of each level ---------- */
function plan(lvl){
  if (lvl <= 1) {
    const F = shuffle(L1F);
    return [...F, F[rnd(F.length)]].map(f => ({kind: "pick", ks: shuffle([f, ...pick(L1S, 2)])}));
  }
  if (lvl === 2) {
    const six = shuffle([...pick(L2F, 3), ...pick(L2S, 3)]);
    return [...six.map(k => ({kind: "guess", k})), {kind: "sort", ks: six}];
  }
  if (lvl === 3) {
    const r = pick(["ice", "apple", "egg", "ship", "log"], 4);
    r.splice(rnd(5), 0, "clayBall", "clayBoat"); // the same clay, first a ball, then a boat
    return r.map(k => ({kind: "why", k}));
  }
  const coins = () => { const w = 3 + rnd(7), n = 4 + rnd(6); return {kind: "coins", w, n, C: n * w + 1 + rnd(w - 1)}; };
  const cubes = () => { const f = pick(["cork", "wood", "wax", "ice"], 1 + rnd(3)); return {kind: "cubes", ms: shuffle([...f, ...pick(["glass", "stone", "iron", "gold"], 4 - f.length)])}; };
  const q = pick(Object.keys(QUIZ), 2);
  return [{kind: "egg"}, coins(), {kind: "quiz", k: q[0]}, cubes(), coins(), {kind: "quiz", k: q[1]}];
}

registerGame({id: ID, em: "⛵", name: "Flotte ou coule ? (cours)", title: TX.title, sub: TX.sub, multi: true, cat: "sciences"}, function () {
  const lang = LG(), lvl = levelOf(ID), rounds = plan(lvl), total = rounds.length, res = [];
  const t = k => TX[k][lang], L = x => x[lang];
  const nm = o => o[lang];
  // Luxembourgish is heard through the lod.lu recording of a lexicon word: "d'Int", not the capitalised "D'Int"
  const speak = s => say(lang === "lb" ? s.replace(/(^|[.!?:]\s+)(D'|Den |De )/g, (m, a, b) => a + b.toLowerCase()) : s, lang);
  // in Luxembourgish the voice only plays one lexicon word: leave the written explanation on screen long enough to be read
  const readMs = s => Math.min(9000, 1200 + String(s).length * 45);
  const tell = s => lang === "lb" ? Promise.all([speak(s), pause(readMs(s))]) : speak(s);
  const praise = () => { const p = (PHRASES[lang] || PHRASES.en).praise; return p[rnd(p.length)]; };
  const result = o => o.f ? TX.floats[lang](nm(o)) : TX.sinks[lang](nm(o));
  startSession(ID, null, total); const gen = GEN;
  const body = $("gameBody");
  let i = 0;

  const frame = (html, ask) => {
    body.innerHTML = "";
    let cur = ask;
    const p = el("p", "prompt", html);
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => cur, t("again"), lang));
    body.append(p, row);
    return {set(h, a){ p.innerHTML = h; cur = a; }};
  };
  const line = () => { const l = el("p", "fl-line", ""); body.append(l); return l; };
  const cheer = tk => { if (calm()) return; const r = tk.wrap.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height * .3, 16); snd.chime(); };
  const end = async (key, first, tries) => {
    logRound(key, first, tries + 1, {lvl});
    res.push(first ? 1 : 0); renderDots(res, total, -1);
    i++; await pause(900);
    next();
  };
  // the first touch decides (a guess): resolves with the index touched
  const choose = (btns, right) => new Promise(ok => {
    let done = false;
    btns.forEach((b, k) => {
      if (k === right) markOk(b);
      b.onclick = () => { if (done || !alive(gen)) return; done = true; G.taps++; btns.forEach(x => { delete x.dataset.ok; x.disabled = true; }); ok(k); };
    });
  });
  // written answers: resolves with the number of wrong tries once the right one is touched; a wrong one is explained in writing (ln) and aloud
  const askText = (items, host, ln) => new Promise(ok => {
    let tries = 0, busy = false;
    const box = el("div", "fl-whys");
    shuffle(items).forEach(it => {
      const b = el("button", "fl-why chunky", it.txt);
      if (it.ok) markOk(b);
      b.onclick = async () => {
        if (busy || b.disabled || !alive(gen)) return; G.taps++;
        if (it.ok) { busy = true; delete b.dataset.ok; b.classList.add("ok"); [...box.children].forEach(x => x.disabled = true); sfx.ok(); ok(tries); return; }
        tries++; busy = true; b.disabled = true; b.classList.add("ko"); sfx.ko();
        const why = it.wrong || t("retry"); if (ln) ln.textContent = why;
        await speak(why); busy = false;
      };
      box.append(b);
    });
    // reading is the point; the voice reads the answers only on request (a hint)
    const hear = el("button", "chip", "🔊 " + t("hear"));
    hear.onclick = () => { G.hints++; speak(items.map(x => x.txt.replace(/^[^\p{L}\d]+/u, "")).join(" ")); };
    const r = el("div", "row"); r.style.justifyContent = "center"; r.append(hear);
    host.append(box, r);
  });

  /* ---------- the lesson ---------- */
  async function lesson(){
    body.innerHTML = "";
    const head = el("p", "prompt", `📖 ${t("learn")}`), stage = el("div", "fl-stage"), capt = el("p", "fl-cap", "");
    stage.style.cssText = "display:flex; flex-direction:column; align-self:stretch";
    const foot = el("div", "row fl-foot"), dots = el("div", "fl-steps"), skip = el("button", "fl-skip chunky", "⏭️");
    skip.setAttribute("aria-label", t("skip")); markOk(skip);
    foot.append(dots, skip); body.append(head, stage, capt, foot);
    let skipped = false, onSkip = null;
    const skipP = new Promise(r => onSkip = r), l0 = loops.length;
    skip.onclick = () => {
      if (skipped || !alive(gen)) return; skipped = true; G.taps++; delete skip.dataset.ok; snd.whoosh();
      try { speechSynthesis.cancel(); } catch(e) {}
      // the lesson's pending splashes, bubbles and glugs must not sound over the first round
      loops.splice(l0).forEach(id => clearTimeout(id));
      onSkip();
    };
    let tk = null;
    const useTank = () => { if (!tk) { stage.innerHTML = ""; tk = tank(); stage.append(tk.wrap); } return tk; };
    const dropIt = async (k, x) => { const a = useTank(), n = a.add(O[k], x, "fl-hang"); await pause(500); await a.drop(n, O[k]); return n; };
    const see = async (k, lighter) => { tk = null; stage.innerHTML = ""; const s = seesaw(O[k]); stage.append(s.wrap); await pause(600); return s.tilt(lighter ? 13 : -13); };
    const S = {
      1: [{cap: `🦆 ⬆️ ${result(O.duck)}`, say: result(O.duck), run: () => dropIt("duck", 32)},
          {cap: `🪨 ⬇️ ${result(O.stone)}`, say: result(O.stone), run: () => dropIt("stone", 68)},
          {cap: `⬆️ 🦆 · ⬇️ 🪨<small>${L(LT.rule1)}</small>`, say: L(LT.rule1), run: async () => { const a = useTank(); a.mark("⬆️", 16, a.H() * .2); a.mark("⬇️", 84, a.H() * .6); await pause(800); }},
          {cap: `👉 ${L(LT.turn1)}`, say: L(LT.turn1)}],
      2: [{cap: `🦆 ${L(LT.duck2)}`, run: () => dropIt("duck", 30)},
          {cap: `🔑 ${L(LT.key2)}`, run: () => dropIt("key", 70)},
          {cap: `🍎 ⚖️ 💧 ${L(LT.twin2)}`, run: () => see("apple", true)},
          {cap: `🔑 ⚖️ 💧 ${L(LT.keyTwin2)}`, run: () => see("key", false)},
          {cap: `👉 ${L(LT.turn2)}`}],
      3: [{cap: `🍎 ⚖️ 💧 ${L(LT.apple3)}`, run: () => see("apple", true)},
          {cap: `🔑 ⚖️ 💧 ${L(LT.key3)}`, run: () => see("key", false)},
          {cap: `⚽ 💨 ${L(LT.air3)}`, run: () => dropIt("ball", 50)},
          {cap: `🤔 ${L(LT.surprise3)}`}],
      4: [{cap: `⚖️ 💧 ${L(LT.rule4)}`, run: () => see("apple", true)},
          {cap: `🧂 ${L(LT.salt4)}`, run: async () => { const a = useTank(), n = a.add(O.swimmer, 50); a.place(n, O.swimmer, true, .8); await pause(500); await a.salt(); await a.move(n, a.floatY(n, O.swimmer, .55), 1200, "ease-out"); }},
          {cap: `🪙 ${L(LT.boat4)}`, run: async () => { tk = null; const a = useTank(), b = toyBoat(a); for (let k = 1; k <= 5; k++) { await b.load(k / 4.5); if (skipped) return; } }},
          {cap: `🔬 ${L(LT.go4)}`}]
    }[Math.min(4, Math.max(1, lvl))];
    snd.jingle();
    for (let k = 0; k < S.length && !skipped; k++) {
      if (!alive(gen)) return false;
      dots.innerHTML = S.map((_, j) => `<i class="${j <= k ? "on" : ""}"></i>`).join("");
      const st = S[k];
      capt.innerHTML = st.cap;
      const written = st.cap.replace(/<[^>]+>/g, " "), words = st.say || written.replace(/^[^\p{L}]+/u, "");
      await Promise.race([Promise.all([st.run ? st.run() : pause(400), speak(words), pause(lang === "lb" ? Math.max(2400, readMs(written)) : 2400)]), skipP]);
      if (!skipped) await Promise.race([pause(500), skipP]);
    }
    delete skip.dataset.ok;
    return alive(gen);
  }

  /* ---------- level 1: which one floats? ---------- */
  async function pickRound(r){
    const items = r.ks.map(k => O[k]), q = t("q1"), ask = `${q} ${t("q1s")}`;
    frame(`👆 ${ICON(1)} ${q}<small>${t("q1s")}</small>`, ask); // the picture says it too: the ball stays on top of the water
    const tk = tank(); body.append(tk.wrap);
    const shelf = el("div", "fl-shelf"); body.append(shelf);
    const ln = line();
    const xs = [20, 50, 80];
    let tries = 0, busy = false, done = false;
    const btns = items.map((o, k) => {
      const b = el("button", "fl-item chunky", face(o)); b.setAttribute("aria-label", nm(o));
      if (o.f) markOk(b);
      b.onclick = async () => {
        if (busy || done || b.disabled || !alive(gen)) return; busy = true; G.taps++;
        b.disabled = true; delete b.dataset.ok; b.classList.add("fl-used");
        await tk.drop(tk.add(o, xs[k]), o); if (!alive(gen)) return;
        const s = result(o); ln.textContent = s;
        if (o.f) {
          done = true; b.classList.add("ok"); sfx.ok(); cheer(tk);
          const first = tries === 0; if (first) addStar();
          await speak(`${praise()} ${s}`); if (!alive(gen)) return;
          // the other two go in as well: they sink
          const rest = btns.map((x, j) => [x, j]).filter(([x]) => !x.disabled);
          rest.forEach(([x, j]) => { x.disabled = true; x.classList.add("fl-used"); tk.drop(tk.add(items[j], xs[j]), items[j]); });
          if (rest.length) { await pause(2500); if (!alive(gen)) return; }
          return end(r.ks.find(x => O[x].f), first, tries); // one key per floating thing, whatever the two others were
        }
        tries++; sfx.ko();
        await speak(`${s} ${t("retry")}`); busy = false;
      };
      shelf.append(b);
      return b;
    });
    await speak(ask);
  }

  /* ---------- a guess before the drop (levels 2 and 3) ---------- */
  async function guess(o, pre){
    const q = TX.predict[lang](nm(o), o.g);
    const f = frame(pre ? `${pre}<small>${q}</small>` : q, pre ? `${pre} ${q}` : q);
    const tk = tank(); body.append(tk.wrap);
    const n = tk.add(o, 50, "fl-hang");
    const two = el("div", "fl-two");
    const bF = el("button", "fl-ans chunky", `${ICON(1)}<span>${t("floatsBtn")}</span>`), bS = el("button", "fl-ans chunky", `${ICON(0)}<span>${t("sinksBtn")}</span>`);
    two.append(bF, bS); body.append(two);
    const ln = line();
    const picked = choose([bF, bS], o.f ? 0 : 1);
    speak(pre ? `${pre} ${q}` : q);
    const saidF = (await picked) === 0;
    if (!alive(gen)) return null;
    const ok = saidF === !!o.f;
    (saidF ? bF : bS).classList.add(ok ? "ok" : "ko");
    await tk.drop(n, o); if (!alive(gen)) return null;
    (o.f ? bF : bS).classList.add("ok");
    const s = result(o); ln.textContent = s;
    if (ok) { sfx.ok(); cheer(tk); } else snd.oops();
    await speak(ok ? `${praise()} ${s}` : `${t("look")} ${s}`);
    return {ok, tk, n, two, ln, f};
  }
  async function guessRound(r){
    const g = await guess(O[r.k]); if (!g || !alive(gen)) return;
    if (g.ok) addStar();
    return end(r.k, g.ok, g.ok ? 0 : 1);
  }

  /* ---------- level 2: sort the six into two tubs ---------- */
  async function sortRound(r){
    const ask = `${t("sortQ")} ${t("sortS")}`;
    frame(`🧺 ${t("sortQ")}<small>${t("sortS")}</small>`, ask);
    const cur = el("div", "fl-cur"), queue = el("div", "fl-queue"), tubs = el("div", "fl-tubs");
    const tub = f => { const b = el("button", "fl-tub chunky", `<span class="fl-tubw"></span><b>${ICON(f)}${t(f ? "floatsBtn" : "sinksBtn")}</b>`); return b; };
    const tF = tub(1), tS = tub(0); tubs.append(tF, tS);
    body.append(cur, queue, tubs);
    const ln = line();
    const put = {1: 0, 0: 0};
    let mistakes = 0;
    speak(ask);
    for (let j = 0; j < r.ks.length; j++) {
      if (!alive(gen)) return;
      const o = O[r.ks[j]];
      cur.innerHTML = face(o); cur.setAttribute("aria-label", nm(o));
      queue.innerHTML = r.ks.slice(j + 1).map(k => `<span>${face(O[k])}</span>`).join("");
      anim(cur, [{transform: "scale(.4)", opacity: 0}, {transform: "scale(1.1)", offset: .7}, {transform: "none"}], {duration: 380, easing: "ease-out"});
      await new Promise(ok => {
        let busy = false;
        const right = o.f ? tF : tS, wrong = o.f ? tS : tF;
        markOk(right); delete wrong.dataset.ok;
        const tap = async b => {
          if (busy || !alive(gen)) return; busy = true; G.taps++;
          if (b === right) {
            delete right.dataset.ok; right.onclick = wrong.onclick = null;
            const f = o.f ? 1 : 0, k = put[f]++;
            const s = el("span", "", face(o)); s.style.left = (16 + (k % 3) * 34) + "%"; s.style.top = f ? "24%" : "76%";
            right.querySelector(".fl-tubw").append(s);
            anim(s, [{transform: "translateY(-40px) scale(.5)", opacity: 0}, {transform: "none", opacity: 1}], {duration: 380, easing: "ease-out"});
            f ? snd.plop() : snd.thud(); fx.bounce(right);
            ln.textContent = result(o);
            await speak(result(o)); await pause(250);
            return ok();
          }
          mistakes++; sfx.ko(); ln.textContent = result(o);
          await speak(result(o)); busy = false;
        };
        right.onclick = () => tap(right); wrong.onclick = () => tap(wrong);
        // drag the thing onto a tub, or just touch the tub
        cur.onpointerdown = e => {
          if (busy) return;
          const x0 = e.clientX, y0 = e.clientY; let moved = false;
          try { cur.setPointerCapture(e.pointerId); } catch(err) {}
          cur.onpointermove = ev => { const dx = ev.clientX - x0, dy = ev.clientY - y0; if (Math.abs(dx) + Math.abs(dy) > 8) moved = true; cur.style.transform = `translate(${dx}px,${dy}px) scale(.8)`; };
          cur.onpointercancel = () => { cur.onpointermove = cur.onpointerup = cur.onpointercancel = null; cur.style.transform = ""; };
          cur.onpointerup = ev => {
            cur.onpointermove = cur.onpointerup = cur.onpointercancel = null; cur.style.transform = "";
            if (!moved) return;
            cur.style.visibility = "hidden"; const under = document.elementFromPoint(ev.clientX, ev.clientY); cur.style.visibility = "";
            const hit = under && under.closest(".fl-tub");
            if (hit === tF || hit === tS) tap(hit);
          };
        };
      });
    }
    if (!alive(gen)) return;
    cur.innerHTML = "🎉"; queue.innerHTML = "";
    const first = mistakes === 0; if (first) { addStar(); sfx.ok(); }
    await speak(praise()); if (!alive(gen)) return;
    return end("sort", first, mistakes);
  }

  /* ---------- level 3: surprises, then why ---------- */
  async function whyRound(r){
    const w = WHY[r.k];
    let tries = 0, tk, fr, ln, n;
    if (r.k === "log") {
      const ask = `${L(w.pre)} ${L(w.ask)}`;
      fr = frame(`⚖️ ${L(w.pre)}<small>${L(w.ask)}</small>`, ask);
      tk = tank(); body.append(tk.wrap);
      const nW = tk.add(O.wood, 30, "fl-hang fl-big"), nK = tk.add(O.key, 72, "fl-hang");
      const shelf = el("div", "fl-shelf"); shelf.style.gridTemplateColumns = "1fr 1fr";
      const bW = el("button", "fl-item fl-big-i chunky", face(O.wood)), bK = el("button", "fl-item chunky", face(O.key));
      bW.setAttribute("aria-label", nm(O.wood)); bK.setAttribute("aria-label", nm(O.key));
      shelf.append(bW, bK); body.append(shelf); ln = line();
      const picked = choose([bW, bK], 0); speak(ask);
      const ok = (await picked) === 0; if (!alive(gen)) return;
      (ok ? bW : bK).classList.add(ok ? "ok" : "fl-used");
      tk.drop(nK, O.key); await tk.drop(nW, O.wood); if (!alive(gen)) return;
      bW.classList.add("ok");
      const s = `${result(O.wood)} ${result(O.key)}`; ln.textContent = s;
      if (ok) { sfx.ok(); cheer(tk); } else { tries++; snd.oops(); }
      await speak(ok ? `${praise()} ${s}` : `${t("look")} ${s}`); if (!alive(gen)) return;
      shelf.remove();
    } else {
      const o = O[r.k], g = await guess(o, w.pre && L(w.pre)); if (!g || !alive(gen)) return;
      if (!g.ok) tries++;
      tk = g.tk; fr = g.f; ln = g.ln; n = g.n; g.two.remove();
    }
    // why? three written answers
    fr.set(`🤔 ${L(w.q)}`, L(w.q));
    const host = el("div", "fl-host"); host.style.cssText = "display:flex; flex-direction:column; gap:10px";
    ln.before(host);
    const got = askText([{txt: L(w.ok), ok: true}, ...w.no.map(x => ({txt: L(x)}))], host, ln);
    speak(L(w.q));
    tries += await got; if (!alive(gen)) return;
    ln.textContent = L(w.more);
    // the ice cube: most of it is under the water
    if (r.k === "ice" && n) tk.mark("⬅️", 64, tk.floatY(n, O.ice) + 4);
    const first = tries === 0; if (first) addStar(); // a right guess and the right reason, both at the first touch
    await tell(`${praise()} ${L(w.more)}`); if (!alive(gen)) return;
    return end(r.k, first, tries);
  }

  /* ---------- level 4: the egg in salt water ---------- */
  async function eggRound(){
    const o = O.egg, fr = frame(`🥚 ${t("look")}`, t("look"));
    const tk = tank(); body.append(tk.wrap);
    const n = tk.add(o, 50, "fl-hang");
    const host = el("div", "fl-host"); host.style.cssText = "display:flex; flex-direction:column; gap:10px"; body.append(host);
    const ln = line();
    speak(t("look"));
    await pause(500); await tk.drop(n, o, false); if (!alive(gen)) return;
    fr.set(`🥚 ${result(o)}`, result(o));
    await speak(result(o)); if (!alive(gen)) return;
    fr.set(`🥚 ${L(EGG.q)}`, L(EGG.q));
    const got = askText([{txt: L(EGG.salt), ok: true}, {txt: L(EGG.water), wrong: L(EGG.noWater)}, {txt: L(EGG.stir), wrong: L(EGG.noStir)}], host, ln);
    speak(L(EGG.q));
    const tries = await got; if (!alive(gen)) return;
    ln.textContent = "";
    await tk.salt(); if (!alive(gen)) return;
    await tk.move(n, tk.floatY(n, o, .92), 1800, "ease-out"); n.classList.add("fl-bob"); if (!alive(gen)) return;
    cheer(tk); ln.textContent = L(EGG.more);
    const first = tries === 0; if (first) addStar();
    await tell(`${praise()} ${L(EGG.more)}`); if (!alive(gen)) return;
    return end("egg-salt", first, tries);
  }

  /* ---------- level 4: how many coins can the boat carry? ---------- */
  async function coinsRound(r){
    const {C, w, n} = r, q = COINS.q[lang](C, w), a = L(COINS.ask);
    frame(`🪙 ${a}<small>${q}</small>`, `${q} ${a}`);
    const tk = tank(); body.append(tk.wrap);
    let bt = toyBoat(tk);
    const sum = el("p", "fl-sum", ""); body.append(sum);
    const grid = el("div", "fl-nums"); body.append(grid);
    const ln = line();
    let tries = 0, busy = false, over = false;
    const answer = async (k, b) => {
      if (busy || over || b.disabled || !alive(gen)) return; busy = true; G.taps++; ln.textContent = "";
      const upto = Math.min(k, n + 1);
      for (let j = 1; j <= upto; j++) {
        sum.textContent = `${j} × ${w} = ${j * w} g`;
        await bt.load(j * w / C); if (!alive(gen)) return;
      }
      if (k === n) {
        over = true; delete b.dataset.ok; b.classList.add("ok"); sfx.ok();
        const first = tries === 0; if (first) addStar();
        const s1 = COINS.ok[lang](n, w, C); ln.textContent = s1;
        await tell(`${praise()} ${s1}`); if (!alive(gen)) return;
        sum.textContent = `${n + 1} × ${w} = ${(n + 1) * w} g`;
        await bt.load((n + 1) * w / C); if (!alive(gen)) return;
        const s2 = COINS.oneMore[lang](n + 1, w, C); ln.textContent = s2;
        await tell(s2); if (!alive(gen)) return;
        return end(`${C}/${w}`, first, tries);
      }
      tries++; b.disabled = true; b.classList.add("ko"); sfx.ko();
      const s = k > n ? COINS.tooMany[lang](n + 1, w, C) : COINS.tooFew[lang](k, w);
      ln.textContent = s; await tell(s); await pause(900); if (!alive(gen)) return;
      bt.n.remove(); bt = toyBoat(tk); sum.textContent = "";
      busy = false;
    };
    [n - 1, n, n + 1, n + 2].forEach(k => {
      const b = el("button", "num chunky", String(k)); if (k === n) markOk(b);
      b.onclick = () => answer(k, b); grid.append(b);
    });
    await speak(`${q} ${a}`);
  }

  /* ---------- level 4: which cubes float? ---------- */
  async function cubesRound(r){
    const ask = `${L(CUBES.q)} ${L(CUBES.small)}`;
    frame(`🧊 ${L(CUBES.q)}<small>${L(CUBES.small)}</small>`, ask);
    const ref = el("div", "fl-ref", `${cubeSVG("#66CCEE", "", true)}<span>💧 ${L(CUBES.water)} = 100 g</span>`); body.append(ref);
    const grid = el("div", "fl-cubes"); body.append(grid);
    const check = el("button", "bigbtn chunky fl-check", "✔️"); check.setAttribute("aria-label", "✔️"); body.append(check);
    const tk = tank(); body.append(tk.wrap);
    const ln = line();
    const ms = r.ms.map(k => ({k, ...MAT[k], fl: MAT[k].g < 100}));
    const sel = new Set();
    let over = false;
    // recette: the cubes still to touch (a floating one not chosen, or a sinking one chosen by mistake), then ✔️
    const remark = () => {
      ms.forEach(m => { if (m.fl !== sel.has(m.k)) markOk(m.b); else delete m.b.dataset.ok; });
      if (ms.every(m => m.fl === sel.has(m.k))) markOk(check); else delete check.dataset.ok;
    };
    ms.forEach(m => {
      m.b = el("button", "fl-cube chunky", `${cubeSVG(m.c, "")}<b>${m[lang]}</b><span>${m.g} g</span>`);
      m.b.onclick = () => { if (over || !alive(gen)) return; G.taps++; sfx.tap(); if (sel.has(m.k)) sel.delete(m.k); else sel.add(m.k); m.b.classList.toggle("fl-sel", sel.has(m.k)); remark(); };
      grid.append(m.b);
    });
    remark();
    speak(ask);
    await new Promise(ok => { check.onclick = () => { if (over || !alive(gen)) return; over = true; G.taps++; delete check.dataset.ok; ok(); }; });
    if (!alive(gen)) return;
    ms.forEach(m => { m.b.disabled = true; delete m.b.dataset.ok; }); check.disabled = true;
    const right = ms.every(m => m.fl === sel.has(m.k));
    // the experiment: all four go into the water
    const xs = [14, 38, 62, 86];
    const drops = ms.map((m, j) => { const o = {svg: cubeSVG(m.c, m.g).replace(/^<svg[^>]*>|<\/svg>$/g, ""), f: m.fl, sub: m.sub || .5, box: [.1, .9]};
      const n = tk.add(o, xs[j]); return pause(j * 220).then(() => alive(gen) && tk.drop(n, o)); });
    await Promise.all(drops); if (!alive(gen)) return;
    ms.forEach(m => m.b.classList.add(m.fl === sel.has(m.k) ? "ok" : "ko"));
    ln.textContent = L(CUBES.rule);
    if (right) { sfx.ok(); cheer(tk); addStar(); } else snd.oops();
    await tell(`${right ? praise() : t("look")} ${L(CUBES.rule)}`); if (!alive(gen)) return;
    return end("cubes:" + r.ms.join("+"), right, right ? 0 : 1);
  }

  /* ---------- level 4: written questions, then the experiment shows it ---------- */
  async function quizRound(r){
    const Q = QUIZ[r.k];
    frame(`${Q.em} ${L(Q.q)}`, L(Q.q));
    const tk = tank(); body.append(tk.wrap);
    const host = el("div", "fl-host"); host.style.cssText = "display:flex; flex-direction:column; gap:10px"; body.append(host);
    const ln = line();
    // what the aquarium shows before and after the answer
    let after = () => pause(300);
    if (r.k === "oil") {
      after = async () => {
        if (!calm()) for (let k = 0; k < 6; k++) later(k * 120, () => {
          const d = el("span", "fl-oilb"); d.style.left = (40 + k * 4) + "%"; d.style.top = (tk.H() * .1) + "px"; tk.objs.append(d);
          const a = anim(d, [{transform: "translateY(0)"}, {transform: `translateY(${tk.H() * .38}px)`, offset: .45}, {transform: `translateY(${tk.H() * .19}px)`, opacity: .3}], {duration: 1400, easing: "ease-in-out"});
          if (a) a.onfinish = () => d.remove(); else d.remove();
        });
        const oil = el("div", "fl-oil"); tk.wrap.insertBefore(oil, tk.wrap.querySelector(".fl-front"));
        await pause(1300); oil.style.opacity = "1"; anim(oil, [{opacity: 0, transform: "scaleY(.2)"}, {opacity: 1, transform: "none"}], {duration: 500});
        await pause(500);
      };
    } else if (r.k === "sub") {
      const n = tk.add(O.sub, 50); tk.place(n, O.sub, true, .72);
      after = async () => {
        n.classList.remove("fl-bob"); tk.bubbles(50, tk.H() * .34, 5);
        await tk.move(n, tk.H() * .62, 1600, "ease-in-out"); await pause(500);
        tk.bubbles(56, tk.H() * .6, 5);
        await tk.move(n, tk.floatY(n, O.sub, .72), 1600, "ease-in-out"); n.classList.add("fl-bob");
      };
    } else if (r.k === "load") {
      const n = tk.add(O.ship, 50); tk.place(n, O.ship, true, .3);
      after = async () => { // three boxes land on deck, the ship sits a little deeper each time
        n.classList.remove("fl-bob");
        for (let k = 0; k < 3; k++) {
          const b = el("span", "", "📦"); b.style.cssText = `position:absolute; font-size:.3em; left:${22 + k * 18}%; top:14%`; n.append(b);
          anim(b, [{transform: "translateY(-60px)", opacity: 0}, {transform: "none", opacity: 1}], {duration: 350, easing: "ease-in"});
          await pause(350);
          await tk.move(n, tk.floatY(n, O.ship, .42 + k * .12), 500, "ease-out");
        }
      };
    } else if (r.k === "berg") {
      const berg = el("div", "fl-berg", `<svg viewBox="0 0 160 100" preserveAspectRatio="none" aria-hidden="true">
        <g class="fl-under" opacity="0"><path d="M58 30 L44 44 L40 62 L52 80 L80 86 L106 78 L118 60 L112 42 L100 30 Z" fill="#E4F7FF" stroke="${INK}" stroke-width="1.6"/></g>
        <path d="M66 30 L74 21 L80 24 L88 18 L96 30 Z" fill="#fff" stroke="${INK}" stroke-width="1.6"/></svg>`);
      tk.objs.append(berg);
      after = async () => { const u = berg.querySelector(".fl-under"); u.setAttribute("opacity", "1"); anim(u, [{opacity: 0}, {opacity: 1}], {duration: 900}); await pause(900); };
    } else if (r.k === "sea") {
      const n = tk.add(O.swimmer, 50); tk.place(n, O.swimmer, true, .8);
      after = async () => { await tk.salt(); await tk.move(n, tk.floatY(n, O.swimmer, .55), 1200, "ease-out"); };
    }
    const got = askText([{txt: L(Q.ok), ok: true}, ...Q.no.map(x => ({txt: L(x)}))], host, ln);
    speak(L(Q.q));
    const tries = await got; if (!alive(gen)) return;
    ln.textContent = "";
    await after(); if (!alive(gen)) return;
    cheer(tk); ln.textContent = L(Q.more);
    const first = tries === 0; if (first) addStar();
    await tell(`${praise()} ${L(Q.more)}`); if (!alive(gen)) return;
    return end(r.k, first, tries);
  }

  const next = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const r = rounds[i];
    ({pick: pickRound, guess: guessRound, sort: sortRound, why: whyRound, egg: eggRound, coins: coinsRound, cubes: cubesRound, quiz: quizRound})[r.kind](r);
  };
  lesson().then(go => { if (go) next(); });
});
})();
