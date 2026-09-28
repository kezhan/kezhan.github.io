/* L'Île aux Mots : contenus du jeu « Épelle ». Consignes et répliques de la chenille dans les quatre langues apprises
   (jamais de français pour l'enfant), mots en plus du lexique, pinyin, lettres et caractères qui se ressemblent.
   Luxembourgeois vérifié sur lod.lu : schreif! (SCHREIWEN1), hëllef! (HELLEFEN1), lauschter! (LAUSCHTEREN1), d'Raup (RAUP1),
   « wéi schreift een dat Wuert? » (exemple de SCHREIWEN1), hoppla, nee, lecker, wonnerbar, geschriwwen. */
const EPELLE = {
  txt: {
    en: {title: "Spell it!", sub: "Help the caterpillar spell the word", go: "Spell the word!", dict: "Listen and spell!", again: "Again", hihi: "Hee hee!"},
    de: {title: "Buchstabier mal!", sub: "Hilf der Raupe beim Schreiben", go: "Schreib das Wort!", dict: "Hör zu und schreib!", again: "Nochmal", hihi: "Hihi!", art: "Der, die oder das?"},
    lb: {title: "Schreif d'Wuert!", sub: "Hëllef der Raup!", go: "Wéi schreift een dat Wuert?", dict: "Lauschter a schreif!", again: "Nach eng Kéier", hihi: "Hihi!"},
    zh: {title: "拼一拼", sub: "帮毛毛虫拼出词语", go: "拼出这个词！", dict: "听一听，拼出来！", again: "再听一次", hihi: "嘻嘻！"}
  },
  // what the big one hears at the start of a word: f = the word as in the lexicon (with its article), b = without it
  // (Luxembourgish is heard through the lod.lu recording of the word alone)
  ask: {
    en: [f => `Spell ${f}!`, f => `Can you spell ${f}?`, f => `Let's spell ${f}!`, f => `How do you spell ${f}?`, f => `Help the caterpillar spell ${f}!`],
    de: [f => `Schreib das Wort: ${f}!`, (f, b) => `Wie schreibt man ${b}?`, f => `Buchstabier mal: ${f}!`, f => `Hilf der Raupe: ${f}!`, (f, b) => `Kannst du ${b} schreiben?`],
    zh: [f => `请拼出：${f}！`, f => `${f}，怎么拼？`, f => `帮毛毛虫拼出${f}！`, f => `我们来拼${f}！`, f => `${f}，拼一拼！`]
  },
  oops: {
    en: ["Yuck!", "Oops!", "Not that one!", "Bleh!", "Nope!"],
    de: ["Igitt!", "Hoppla!", "Nicht der!", "Bäh!", "Oh nein!"],
    lb: ["Hoppla!", "Oh nee!", "Nee!"],
    zh: ["哎呀！", "不对哦！", "呸呸！", "再试试！"]
  },
  yum: {
    en: ["Yum!", "Nom nom!", "Crunch!", "Tasty!"],
    de: ["Mjam!", "Lecker!", "Mampf!"],
    lb: ["Lecker!", "Mmmh!"],
    zh: ["好吃！", "啊呜！", "嗯嗯！"]
  },
  cheer: {
    en: ["Brilliant!", "Fantastic spelling!", "You're a super speller!", "Perfect!", "Hooray!"],
    de: ["Super geschrieben!", "Klasse!", "Spitze!", "Wunderbar!", "Toll gemacht!"],
    lb: ["Wonnerbar!", "Super!", "Bravo!", "Richteg!", "Flott geschriwwen!"],
    zh: ["拼对了！", "真厉害！", "太棒了！", "你是拼字小能手！", "好极了！"]
  },
  // words beyond the lexicon, for English, German and Chinese (no Luxembourgish recording for them)
  extra: [
    {e: "🦊", en: "fox", de: "der Fuchs", zh: "狐狸"}, {e: "🐝", en: "bee", de: "die Biene", zh: "蜜蜂"},
    {e: "🐜", en: "ant", de: "die Ameise", zh: "蚂蚁"}, {e: "🦇", en: "bat", de: "die Fledermaus", zh: "蝙蝠"},
    {e: "🐛", en: "caterpillar", de: "die Raupe", zh: "毛毛虫"}, {e: "🐌", en: "snail", de: "die Schnecke", zh: "蜗牛"},
    {e: "🦀", en: "crab", de: "die Krabbe", zh: "螃蟹"}, {e: "🦈", en: "shark", de: "der Hai", zh: "鲨鱼"},
    {e: "🐺", en: "wolf", de: "der Wolf", zh: "狼"}, {e: "🦔", en: "hedgehog", de: "der Igel", zh: "刺猬"},
    {e: "🦜", en: "parrot", de: "der Papagei", zh: "鹦鹉"}, {e: "🐞", en: "ladybird", de: "der Marienkäfer", zh: "瓢虫"},
    {e: "🦄", en: "unicorn", de: "das Einhorn", zh: "独角兽"}, {e: "🦖", en: "dinosaur", de: "der Dinosaurier", zh: "恐龙"},
    {e: "🐉", en: "dragon", de: "der Drache", zh: "龙"}, {e: "🦩", en: "flamingo", de: "der Flamingo", zh: "火烈鸟"},
    {e: "🐿️", en: "squirrel", de: "das Eichhörnchen", zh: "松鼠"}, {e: "🦥", en: "sloth", de: "das Faultier", zh: "树懒"},
    {e: "🐹", en: "hamster", de: "der Hamster", zh: "仓鼠"}, {e: "🐐", en: "goat", de: "die Ziege", zh: "山羊"},
    {e: "🦦", en: "otter", de: "der Otter", zh: "水獭"}, {e: "🐓", en: "rooster", de: "der Hahn", zh: "公鸡"},
    {e: "🦢", en: "swan", de: "der Schwan", zh: "天鹅"}, {e: "🐪", en: "camel", de: "das Kamel", zh: "骆驼"},
    {e: "🦏", en: "rhino", de: "das Nashorn", zh: "犀牛"}, {e: "🦛", en: "hippo", de: "das Nilpferd", zh: "河马"},
    {e: "🦚", en: "peacock", de: "der Pfau", zh: "孔雀"}, {e: "🐼", en: "panda", de: "der Panda", zh: "熊猫"},
    {e: "🐨", en: "koala", de: "der Koala", zh: "考拉"}, {e: "🦅", en: "eagle", de: "der Adler", zh: "老鹰"},
    {e: "☕", en: "cup", de: "die Tasse", zh: "杯子"}, {e: "🛏️", en: "bed", de: "das Bett", zh: "床"},
    {e: "📦", en: "box", de: "die Kiste", zh: "盒子"}, {e: "🗺️", en: "map", de: "die Karte", zh: "地图"},
    {e: "🕸️", en: "web", de: "das Spinnennetz", zh: "蜘蛛网"}, {e: "🖊️", en: "pen", de: "der Stift", zh: "笔"},
    {e: "🪁", en: "kite", de: "der Drachen", zh: "风筝"}, {e: "🎁", en: "gift", de: "das Geschenk", zh: "礼物"},
    {e: "🥁", en: "drum", de: "die Trommel", zh: "鼓"}, {e: "🎸", en: "guitar", de: "die Gitarre", zh: "吉他"},
    {e: "🪑", en: "chair", de: "der Stuhl", zh: "椅子"}, {e: "🛁", en: "bath", de: "die Badewanne", zh: "浴缸"},
    {e: "🧸", en: "teddy", de: "der Teddy", zh: "玩具熊"}, {e: "🎈", en: "balloon", de: "der Luftballon", zh: "气球"},
    {e: "🍭", en: "lollipop", de: "der Lutscher", zh: "棒棒糖"}, {e: "🍪", en: "cookie", de: "der Keks", zh: "饼干"},
    {e: "🍯", en: "honey", de: "der Honig", zh: "蜂蜜"}, {e: "🌽", en: "corn", de: "der Mais", zh: "玉米"},
    {e: "🥜", en: "peanut", de: "die Erdnuss", zh: "花生"}, {e: "🍔", en: "burger", de: "der Burger", zh: "汉堡"},
    {e: "🌋", en: "volcano", de: "der Vulkan", zh: "火山"}, {e: "🏰", en: "castle", de: "die Burg", zh: "城堡"},
    {e: "⚓", en: "anchor", de: "der Anker", zh: "锚"}, {e: "🌊", en: "wave", de: "die Welle", zh: "海浪"},
    {e: "❄️", en: "snow", de: "der Schnee", zh: "雪"}, {e: "🔥", en: "fire", de: "das Feuer", zh: "火"},
    {e: "🪐", en: "planet", de: "der Planet", zh: "行星"}, {e: "🤖", en: "robot", de: "der Roboter", zh: "机器人"},
    {e: "👑", en: "crown", de: "die Krone", zh: "王冠"}, {e: "🧲", en: "magnet", de: "der Magnet", zh: "磁铁"},
    {e: "🔔", en: "bell", de: "die Glocke", zh: "铃铛"}, {e: "🥣", en: "bowl", de: "die Schüssel", zh: "碗"},
    {e: "🧃", en: "juice", de: "der Saft", zh: "果汁"}, {e: "🍫", en: "chocolate", de: "die Schokolade", zh: "巧克力"},
    {e: "🌻", en: "sunflower", de: "die Sonnenblume", zh: "向日葵"}, {e: "🍁", en: "leaf", de: "das Blatt", zh: "叶子"},
    {e: "🐾", en: "paw", de: "die Pfote", zh: "爪子"}, {e: "🥅", en: "goal", de: "das Tor", zh: "球门"},
    {e: "🧩", en: "puzzle", de: "das Puzzle", zh: "拼图"}, {e: "🛶", en: "canoe", de: "das Kanu", zh: "独木舟"},
    {e: "🚜", en: "tractor", de: "der Traktor", zh: "拖拉机"}, {e: "🪥", en: "toothbrush", de: "die Zahnbürste", zh: "牙刷"},
    {e: "🧽", en: "sponge", de: "der Schwamm", zh: "海绵"}, {e: "🍿", en: "popcorn", de: "das Popcorn", zh: "爆米花"},
    {e: "🥄", en: "spoon", de: "der Löffel", zh: "勺子"}, {e: "🪣", en: "bucket", de: "der Eimer", zh: "水桶"},
    {e: "🕯️", en: "candle", de: "die Kerze", zh: "蜡烛"}, {e: "🎺", en: "trumpet", de: "die Trompete", zh: "小号"},
    {e: "🎻", en: "violin", de: "die Geige", zh: "小提琴"}, {e: "🏝️", en: "island", de: "die Insel", zh: "小岛"},
    {e: "🌵", en: "cactus", de: "der Kaktus", zh: "仙人掌"}, {e: "🍀", en: "clover", de: "das Kleeblatt", zh: "四叶草"},
    {e: "🥥", en: "coconut", de: "die Kokosnuss", zh: "椰子"}, {e: "🥝", en: "kiwi", de: "die Kiwi", zh: "猕猴桃"},
    {e: "🍩", en: "doughnut", de: "der Donut", zh: "甜甜圈"}, {e: "🛞", en: "wheel", de: "das Rad", zh: "轮子"},
    {e: "🪓", en: "axe", de: "die Axt", zh: "斧头"}, {e: "👵", en: "granny", de: "die Oma", zh: "奶奶"},
    {e: "👴", en: "grandpa", de: "der Opa", zh: "爷爷"}, {e: "👶", en: "baby", de: "das Baby", zh: "宝宝"},
    {e: "🚪", en: "door", de: "die Tür", zh: "门"}, {e: "🦌", en: "deer", de: "das Reh", zh: "鹿"},
    {e: "🍵", en: "tea", de: "der Tee", zh: "茶"}
  ],
  // pinyin, one syllable per character, shown in small under the caterpillar
  py: Object.fromEntries(`奶牛 nǎi niú|鸭子 yā zi|狮子 shī zi|青蛙 qīng wā|兔子 tù zi|猴子 hóu zi|大象 dà xiàng|老鼠 lǎo shǔ|老虎 lǎo hǔ|斑马 bān mǎ
蝴蝶 hú dié|企鹅 qǐ é|乌龟 wū guī|海豚 hǎi tún|鲸鱼 jīng yú|章鱼 zhāng yú|袋鼠 dài shǔ|鳄鱼 è yú|长颈鹿 cháng jǐng lù|猫头鹰 māo tóu yīng
苹果 píng guǒ|香蕉 xiāng jiāo|草莓 cǎo méi|葡萄 pú tao|橙子 chéng zi|樱桃 yīng táo|西瓜 xī guā|面包 miàn bāo|奶酪 nǎi lào|牛奶 niú nǎi
鸡蛋 jī dàn|蛋糕 dàn gāo|披萨 pī sà|土豆 tǔ dòu|柠檬 níng méng|桃子 táo zi|面条 miàn tiáo|米饭 mǐ fàn|黄瓜 huáng guā|蘑菇 mó gu
菠萝 bō luó|胡萝卜 hú luó bo|西红柿 xī hóng shì|冰淇淋 bīng qí lín|三明治 sān míng zhì|西兰花 xī lán huā
红色 hóng sè|蓝色 lán sè|绿色 lǜ sè|黄色 huáng sè|粉色 fěn sè|紫色 zǐ sè|黑色 hēi sè|白色 bái sè|棕色 zōng sè|灰色 huī sè|金色 jīn sè|银色 yín sè
眼睛 yǎn jing|鼻子 bí zi|嘴巴 zuǐ ba|耳朵 ěr duo|牙齿 yá chǐ|胳膊 gē bo|头发 tóu fa|舌头 shé tou|手指 shǒu zhǐ|骨头 gǔ tou|心脏 xīn zàng|大脑 dà nǎo
汽车 qì chē|飞机 fēi jī|火车 huǒ chē|房子 fáng zi|太阳 tài yáng|月亮 yuè liang|星星 xīng xing|雨伞 yǔ sǎn|彩虹 cǎi hóng|钥匙 yào shi
眼镜 yǎn jìng|铅笔 qiān bǐ|剪刀 jiǎn dāo|火箭 huǒ jiàn|雪人 xuě rén|书包 shū bāo|自行车 zì xíng chē|公交车 gōng jiāo chē|直升机 zhí shēng jī
帽子 mào zi|裙子 qún zi|鞋子 xié zi|袜子 wà zi|外套 wài tào|围巾 wéi jīn|手套 shǒu tào|靴子 xuē zi|裤子 kù zi|领带 lǐng dài|鸭舌帽 yā shé mào
医生 yī shēng|老师 lǎo shī|农民 nóng mín|厨师 chú shī|警察 jǐng chá|歌手 gē shǒu|画家 huà jiā|消防员 xiāo fáng yuán|飞行员 fēi xíng yuán
宇航员 yǔ háng yuán|科学家 kē xué jiā|修理工 xiū lǐ gōng|跑步 pǎo bù|睡觉 shuì jiào|游泳 yóu yǒng|唱歌 chàng gē|跳舞 tiào wǔ|写字 xiě zì
狐狸 hú li|蜜蜂 mì fēng|蚂蚁 mǎ yǐ|蝙蝠 biān fú|毛毛虫 máo mao chóng|蜗牛 wō niú|螃蟹 páng xiè|鲨鱼 shā yú|刺猬 cì wei|鹦鹉 yīng wǔ
瓢虫 piáo chóng|独角兽 dú jiǎo shòu|恐龙 kǒng lóng|火烈鸟 huǒ liè niǎo|松鼠 sōng shǔ|树懒 shù lǎn|仓鼠 cāng shǔ|山羊 shān yáng|水獭 shuǐ tǎ
公鸡 gōng jī|天鹅 tiān é|骆驼 luò tuo|犀牛 xī niú|河马 hé mǎ|孔雀 kǒng què|熊猫 xióng māo|考拉 kǎo lā|老鹰 lǎo yīng|杯子 bēi zi|盒子 hé zi
地图 dì tú|蜘蛛网 zhī zhū wǎng|风筝 fēng zheng|礼物 lǐ wù|吉他 jí tā|椅子 yǐ zi|浴缸 yù gāng|玩具熊 wán jù xióng|气球 qì qiú|棒棒糖 bàng bàng táng
饼干 bǐng gān|蜂蜜 fēng mì|玉米 yù mǐ|花生 huā shēng|汉堡 hàn bǎo|火山 huǒ shān|城堡 chéng bǎo|海浪 hǎi làng|行星 xíng xīng|机器人 jī qì rén
王冠 wáng guān|磁铁 cí tiě|铃铛 líng dang|果汁 guǒ zhī|巧克力 qiǎo kè lì|向日葵 xiàng rì kuí|叶子 yè zi|爪子 zhuǎ zi|球门 qiú mén|拼图 pīn tú
独木舟 dú mù zhōu|拖拉机 tuō lā jī|牙刷 yá shuā|海绵 hǎi mián|爆米花 bào mǐ huā|勺子 sháo zi|水桶 shuǐ tǒng|蜡烛 là zhú|小号 xiǎo hào
小提琴 xiǎo tí qín|小岛 xiǎo dǎo|仙人掌 xiān rén zhǎng|四叶草 sì yè cǎo|椰子 yē zi|猕猴桃 mí hóu táo|甜甜圈 tián tián quān
轮子 lún zi|斧头 fǔ tou|奶奶 nǎi nai|爷爷 yé ye|宝宝 bǎo bao`
    .split(/[|\n]/).map(s => s.trim().split(/ (.+)/).slice(0, 2))),
  // letters that look alike: the traps of levels 3 and 4 (kept only if the language has them)
  look: {a: "oeä", b: "dph", c: "eo", d: "bqp", e: "caë", f: "tl", g: "qy", h: "nb", i: "lj", j: "ig", k: "xh", l: "it", m: "nw",
    n: "mhu", o: "acö", p: "qbd", q: "pg", r: "n", s: "zß", t: "fl", u: "nvü", v: "wu", w: "vm", x: "k", y: "vg", z: "s",
    ä: "aö", ö: "oü", ü: "uö", ß: "sb", é: "eë", ë: "eé"},
  // Chinese characters that look alike: first the character of the word, then its traps
  zhLook: Object.fromEntries(`奶乃扔妈 牛午生半 鸭鸡甲鸟 子了字孑 狮师帅 青请清晴 蛙娃洼挂 兔免 猴候喉 大太犬天 象像 老考者孝 虎虑 斑班
马鸟乌与 蝴湖胡 蝶碟蜂 企止金 鹅我鸭 乌鸟马 龟电 海每悔梅 豚逐家 鲸惊京 鱼鲁角 章童竟 袋代装 苹平萍 果课棵呆 香秀季 蕉焦 草早章
莓梅每 葡蒲 萄陶淘 橙登澄 樱婴缨 桃挑逃跳 西四酉 瓜爪抓 面而画 包抱泡句 酪路格 蛋旦 糕羔 披皮波坡 土士上工 豆头短逗 柠宁拧 檬蒙
条杀余 米来木 饭板反 黄横 蘑磨摩 菇姑苦 菠波坡 萝罗 红江虹工 色巴 蓝监篮 绿录缘 粉份纷分 紫柴此 黑墨里 白百自日 棕宗综 灰火
金全企 银很根恨 眼很跟眠 睛晴请情 鼻臭 嘴角 巴色吧把 耳目且 朵朱杂 牙芽呀 齿岁 胳格略 膊博搏 头买实 发友反 舌话活古 手毛
指旨脂 骨滑 心必小 脏庄 脑恼 汽气汁 车东年连 飞乙 机几 火大灭 房方放防 太大犬 阳阴旧 月目用 亮高 星生是 雨两丽 伞全平
彩采影 虹红江 钥钓钢 匙是题 镜境竟 铅船沿 笔毛竿 剪前箭煎 刀力刃 箭剪前 雪雷霜 人入八 书节 帽冒 裙群君 鞋挂桂 袜抹末
外处 套奎 围国困固 巾币布 靴靶 裤库 领岭令零 带常帮 医区 生牛星 师帅 农衣 民氏 厨橱 警敬 察擦祭 歌哥 画田面 家豪 跑泡抱炮
步少 睡垂锤 觉学见 游旅 泳永冰 唱昌倡 跳桃挑逃 舞午无 写与 字子学宇 长张 颈劲经径 鹿麓 猫描锚苗 鹰应 胡湖蝴 卜下上卡 柿市
冰水永 淇棋其 淋林 三王二 明朋阴 治冶台 兰半 花化华 自白目 行衔 公松翁 交父校 直真值 升开丹 消肖销 防方房 员贝圆 宇字学
航抗坑 科料 学字觉 修候 理里埋 工土干 狐孤瓜 狸理里 蜜密 蜂峰锋 蚂妈码 蚁议 蝙编偏 蝠福幅 毛手 虫中 蜗锅 螃旁 蟹解 鲨沙
猬胃 鹦婴 鹉武 瓢飘 独虫 角用 恐巩 龙尤 烈列 松公 树对村 懒赖 仓仑 山出 杯坏怀 盒合 地他池 图国 蜘知 蛛珠株 网冈 风凤
筝争 礼扎 物勿 吉古 他地她 椅倚奇 浴谷俗 缸扛 玩元完 具县 气汽 球求救 棒奉 糖唐塘 饼并 干千于 玉王主 汉双 堡保 城成诚
浪狼良 冠寇 磁滋 铁失 铃铅令 铛挡 巧功 克兑 力刀 向同问 日目 叶什 爪瓜 拼拆 舟丹 拖施 拉粒 刷刚 绵棉锦 爆暴 獭赖 骆路络
驼陀 河何 孔扎 雀省 熊能 考老 勺匀 桶捅 蜡猎 烛虫 号亏 提题 琴琶 岛鸟 仙山 掌赏 四西 椰耶 猕弥 甜舌 圈卷 水小永 门们闪问 轮论抡 斧父爷 爷斧爸 宝宇玉`
    .split(/\s+/).map(g => [g[0], g.slice(1)])),
  noAudio: ["GIRAFF1"] // lexicon words whose lod.lu recording is not in audio/lb yet
};
