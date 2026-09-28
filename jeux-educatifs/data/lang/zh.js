/*
 * L'Île aux mots — pack de langue : CHINOIS MANDARIN (词语岛), caractères simplifiés + pinyin.
 *
 * Même structure et mêmes clés que data/lang/fr.js (pack de référence).
 * Public : enfants qui apprennent le chinois comme langue étrangère.
 *
 * MOTS : { mot (caractères), emoji, genre (null), syl (un caractère par élément),
 *          pinyin (tons des dictionnaires, une syllabe par caractère : 'píng guǒ' ; ton neutre
 *          sans accent : 'tù zi'), epeler (pinyin sans tons collé, ü conservé : 'pingguo'),
 *          niveau (1 = 1–2 caractères très courants, 2 = 2 caractères, 3 = 3–4 caractères),
 *          theme, cl (classificateur 量词 le plus standard), art ('一' + cl), def ('这' + cl) }
 * cl = null : le classificateur est ambigu parmi les choix du jeu (狗 : 只 ou 条 ; 船 : 条, 只
 * ou 艘…) ; le mot est alors écarté du jeu des classificateurs, art/def gardent la forme usuelle.
 * Un cl hors des choix du jeu (匹, 头, 座, 棵…) est exact mais écarté du jeu de la même façon.
 *
 * PINYIN : pinyin de tout mot chinois affiché hors MOTS (rimes, contraires, mots simples, mots
 * des phrases, groupes « 一只 », « 这只 »…). « 一 » y porte son ton modifié, comme dans les
 * manuels pour étrangers : 一个 yí gè, 一只 yì zhī.
 *
 * RIMES : familles de même finale (韵母) exacte, ton ignoré. Une seule famille par « rime
 * traditionnelle » (十三辙) : ao et iao, an et ian… riment dans les chansons chinoises, on n'en
 * garde donc qu'une, pour qu'un mot d'une autre famille ne rime jamais avec la cible.
 */
(function () {
  // Pinyin sans tons, collé, en minuscules (ü conservé : lǜ → lü).
  const sansTon = (p) => p.normalize('NFD').replace(/[̀-̇̉-ͯ]/g, '')
    .normalize('NFC').replace(/\s+/g, '').toLowerCase();

  // M(mot, emoji, pinyin, niveau, theme, cl, clUsuel) : clUsuel sert à art/def quand cl est null.
  const M = (mot, emoji, pinyin, niveau, theme, cl, clUsuel) => ({
    mot, emoji, genre: null, syl: Array.from(mot), pinyin, epeler: sansTon(pinyin),
    niveau, theme, cl, art: '一' + (cl || clUsuel), def: '这' + (cl || clUsuel),
  });

  const MOTS = [
    // --- 动物 : animaux ---
    M('猫', '🐱', 'māo', 1, 'animaux', '只'),
    M('狗', '🐶', 'gǒu', 1, 'animaux', null, '只'), // 一只狗 ou 一条狗
    M('鸟', '🐦', 'niǎo', 1, 'animaux', '只'),
    M('鱼', '🐟', 'yú', 1, 'animaux', '条'),
    M('马', '🐴', 'mǎ', 1, 'animaux', '匹'),
    M('牛', '🐮', 'niú', 1, 'animaux', '头'),
    M('羊', '🐑', 'yáng', 1, 'animaux', '只'),
    M('鸡', '🐔', 'jī', 1, 'animaux', '只'),
    M('猪', '🐷', 'zhū', 1, 'animaux', '头'),
    M('蛇', '🐍', 'shé', 1, 'animaux', '条'),
    M('龙', '🐉', 'lóng', 1, 'animaux', '条'),
    M('兔子', '🐰', 'tù zi', 1, 'animaux', '只'),
    M('熊猫', '🐼', 'xióng māo', 1, 'animaux', '只'),
    M('鸭子', '🦆', 'yā zi', 2, 'animaux', '只'),
    M('老虎', '🐯', 'lǎo hǔ', 2, 'animaux', '只'),
    M('狮子', '🦁', 'shī zi', 2, 'animaux', '只'),
    M('猴子', '🐒', 'hóu zi', 2, 'animaux', '只'),
    M('青蛙', '🐸', 'qīng wā', 2, 'animaux', '只'),
    M('蝴蝶', '🦋', 'hú dié', 2, 'animaux', '只'),
    M('蜜蜂', '🐝', 'mì fēng', 2, 'animaux', '只'),
    M('老鼠', '🐭', 'lǎo shǔ', 2, 'animaux', '只'),
    M('大象', '🐘', 'dà xiàng', 2, 'animaux', '头'),
    M('企鹅', '🐧', 'qǐ é', 2, 'animaux', '只'),
    M('乌龟', '🐢', 'wū guī', 2, 'animaux', '只'),
    M('恐龙', '🦕', 'kǒng lóng', 2, 'animaux', '只'),
    M('长颈鹿', '🦒', 'cháng jǐng lù', 3, 'animaux', '只'),
    M('猫头鹰', '🦉', 'māo tóu yīng', 3, 'animaux', '只'),
    M('毛毛虫', '🐛', 'máo mao chóng', 3, 'animaux', null, '条'), // 一条 ou 一只
    M('独角兽', '🦄', 'dú jiǎo shòu', 3, 'animaux', '只'),
    M('火烈鸟', '🦩', 'huǒ liè niǎo', 3, 'animaux', '只'),
    M('大猩猩', '🦍', 'dà xīng xing', 3, 'animaux', '只'),
    M('霸王龙', '🦖', 'bà wáng lóng', 3, 'animaux', '只'),

    // --- 食物 : nourriture ---
    M('苹果', '🍎', 'píng guǒ', 1, 'nourriture', '个'),
    M('梨', '🍐', 'lí', 1, 'nourriture', '个'),
    M('西瓜', '🍉', 'xī guā', 1, 'nourriture', '个'),
    M('香蕉', '🍌', 'xiāng jiāo', 1, 'nourriture', '根'),
    M('米饭', '🍚', 'mǐ fàn', 1, 'nourriture', '碗'),
    M('鸡蛋', '🥚', 'jī dàn', 2, 'nourriture', null, '个'), // 一个 (一只 : usage régional)
    M('牛奶', '🥛', 'niú nǎi', 2, 'nourriture', '杯'),
    M('蛋糕', '🎂', 'dàn gāo', 2, 'nourriture', '个'),
    M('面条', '🍜', 'miàn tiáo', 2, 'nourriture', '碗'),
    M('草莓', '🍓', 'cǎo méi', 2, 'nourriture', '颗'),
    M('葡萄', '🍇', 'pú tao', 2, 'nourriture', '串'),
    M('橘子', '🍊', 'jú zi', 2, 'nourriture', '个'), // 🍊 = mandarine (橘子), pas orange (橙子)
    M('西红柿', '🍅', 'xī hóng shì', 3, 'nourriture', '个'),
    M('胡萝卜', '🥕', 'hú luó bo', 3, 'nourriture', '根'),
    M('冰淇淋', '🍦', 'bīng qí lín', 3, 'nourriture', null, '个'), // 一个 ou 一支
    M('巧克力', '🍫', 'qiǎo kè lì', 3, 'nourriture', '块'),
    M('棒棒糖', '🍭', 'bàng bàng táng', 3, 'nourriture', '根'),
    M('甜甜圈', '🍩', 'tián tián quān', 3, 'nourriture', '个'),
    M('猕猴桃', '🥝', 'mí hóu táo', 3, 'nourriture', '个'),
    M('汉堡包', '🍔', 'hàn bǎo bāo', 3, 'nourriture', '个'),
    M('爆米花', '🍿', 'bào mǐ huā', 3, 'nourriture', '桶'),
    M('三明治', '🥪', 'sān míng zhì', 3, 'nourriture', '个'),

    // --- 自然 : nature ---
    M('太阳', '☀️', 'tài yáng', 1, 'nature', '个'),
    M('月亮', '🌙', 'yuè liang', 1, 'nature', '个'),
    M('星星', '⭐', 'xīng xing', 1, 'nature', '颗'),
    M('花', '🌸', 'huā', 1, 'nature', '朵'),
    M('树', '🌳', 'shù', 1, 'nature', '棵'),
    M('云', '☁️', 'yún', 1, 'nature', '朵'),
    M('山', '⛰️', 'shān', 1, 'nature', '座'),
    M('雨', '🌧️', 'yǔ', 1, 'nature', '场'),
    M('彩虹', '🌈', 'cǎi hóng', 2, 'nature', '道'),
    M('雪花', '❄️', 'xuě huā', 2, 'nature', '片'),
    M('叶子', '🍃', 'yè zi', 2, 'nature', '片'),
    M('火山', '🌋', 'huǒ shān', 2, 'nature', '座'),
    M('向日葵', '🌻', 'xiàng rì kuí', 3, 'nature', '朵'),
    M('仙人掌', '🌵', 'xiān rén zhǎng', 3, 'nature', '棵'),
    M('椰子树', '🌴', 'yē zi shù', 3, 'nature', '棵'),
    M('郁金香', '🌷', 'yù jīn xiāng', 3, 'nature', '朵'),

    // --- 东西 : objets ---
    M('书', '📖', 'shū', 1, 'objets', '本'),
    M('车', '🚗', 'chē', 1, 'objets', '辆'),
    M('船', '⛵', 'chuán', 1, 'objets', null, '条'), // 一条, 一只 ou 一艘
    M('门', '🚪', 'mén', 1, 'objets', '扇'),
    M('笔', '🖊️', 'bǐ', 1, 'objets', '支'),
    M('床', '🛏️', 'chuáng', 1, 'objets', '张'),
    M('纸', '📄', 'zhǐ', 1, 'objets', '张'),
    M('刀', '🔪', 'dāo', 1, 'objets', '把'),
    M('足球', '⚽', 'zú qiú', 1, 'objets', null, '个'), // 一个 (parfois 一只)
    M('飞机', '✈️', 'fēi jī', 2, 'objets', '架'),
    M('火车', '🚂', 'huǒ chē', 2, 'objets', '列'),
    M('雨伞', '☂️', 'yǔ sǎn', 2, 'objets', '把'),
    M('椅子', '🪑', 'yǐ zi', 2, 'objets', null, '把'), // 一把 (manuels) ou 一张 (usage courant)
    M('钥匙', '🔑', 'yào shi', 2, 'objets', '把'),
    M('裤子', '👖', 'kù zi', 2, 'objets', '条'),
    M('裙子', '👗', 'qún zi', 2, 'objets', '条'),
    M('袜子', '🧦', 'wà zi', 2, 'objets', '双'),
    M('帽子', '🎩', 'mào zi', 2, 'objets', '顶'),
    M('电脑', '💻', 'diàn nǎo', 2, 'objets', '台'),
    M('自行车', '🚲', 'zì xíng chē', 3, 'objets', '辆'),
    M('公交车', '🚌', 'gōng jiāo chē', 3, 'objets', '辆'),
    M('出租车', '🚕', 'chū zū chē', 3, 'objets', '辆'),
    M('救护车', '🚑', 'jiù hù chē', 3, 'objets', '辆'),
    M('消防车', '🚒', 'xiāo fáng chē', 3, 'objets', '辆'),
    M('直升机', '🚁', 'zhí shēng jī', 3, 'objets', '架'),
    M('机器人', '🤖', 'jī qì rén', 3, 'objets', '个'),
    M('望远镜', '🔭', 'wàng yuǎn jìng', 3, 'objets', '架'),
    M('照相机', '📷', 'zhào xiàng jī', 3, 'objets', '台'),
    M('指南针', '🧭', 'zhǐ nán zhēn', 3, 'objets', '个'),
    M('小提琴', '🎻', 'xiǎo tí qín', 3, 'objets', '把'),
    M('降落伞', '🪂', 'jiàng luò sǎn', 3, 'objets', '顶'),

    // --- 身体 : corps ---
    M('手', '✋', 'shǒu', 1, 'corps', '只'),
    M('脚', '🦶', 'jiǎo', 1, 'corps', '只'),
    M('嘴', '👄', 'zuǐ', 1, 'corps', '张'),
    M('眼睛', '👁️', 'yǎn jing', 1, 'corps', '只'),
    M('鼻子', '👃', 'bí zi', 1, 'corps', '个'),
    M('耳朵', '👂', 'ěr duo', 2, 'corps', '只'),
    M('牙齿', '🦷', 'yá chǐ', 2, 'corps', '颗'),
  ];

  // Familles de rimes : même finale (韵母), ton ignoré ; « son » = la finale en pinyin.
  // Une famille par rime traditionnelle (十三辙), pour qu'aucun distracteur ne rime avec la cible.
  const RIMES = [
    { son: 'a', mots: ['马', '八', '爬', '茶', '拿', '擦'] },
    { son: 'e', mots: ['车', '鹅', '河', '喝', '歌', '饿'] },
    { son: 'ie', mots: ['鞋', '姐', '写', '谢', '街', '铁'] },
    { son: 'i', mots: ['鸡', '笔', '七', '米', '皮', '西'] },
    { son: 'u', mots: ['书', '树', '猪', '兔', '路', '读'] },
    { son: 'ai', mots: ['海', '菜', '奶', '爱', '带', '猜'] },
    { son: 'ui', mots: ['嘴', '水', '腿', '睡', '鬼', '贵'] },
    { son: 'ao', mots: ['猫', '刀', '包', '草', '跑', '桃'] },
    { son: 'ou', mots: ['狗', '手', '口', '头', '走', '肉'] },
    { son: 'an', mots: ['山', '伞', '饭', '蓝', '看', '三'] },
    { son: 'en', mots: ['门', '人', '本', '盆', '根', '分'] },
    { son: 'ang', mots: ['糖', '房', '汤', '唱', '狼', '帮'] },
    { son: 'ong', mots: ['龙', '红', '虫', '东', '钟', '洞'] },
  ];

  // Paires de contraires (反义词) : [a, b, niveau, nature ('adj' | 'verbe' | 'nom' | 'adv')].
  // Les mots de position (上/下, 左/右, 前/后, 里面/外面) sont des noms en chinois (方位词).
  const CONTRAIRES = [
    ['大', '小', 1, 'adj'], ['多', '少', 1, 'adj'], ['高', '矮', 1, 'adj'], ['长', '短', 1, 'adj'],
    ['快', '慢', 1, 'adj'], ['冷', '热', 1, 'adj'], ['黑', '白', 1, 'adj'], ['好', '坏', 1, 'adj'],
    ['上', '下', 1, 'nom'], ['左', '右', 1, 'nom'], ['开', '关', 1, 'verbe'], ['哭', '笑', 1, 'verbe'],
    ['来', '去', 1, 'verbe'],
    ['胖', '瘦', 2, 'adj'], ['早', '晚', 2, 'adj'], ['新', '旧', 2, 'adj'], ['远', '近', 2, 'adj'],
    ['轻', '重', 2, 'adj'], ['对', '错', 2, 'adj'], ['甜', '苦', 2, 'adj'], ['软', '硬', 2, 'adj'],
    ['干', '湿', 2, 'adj'], ['前', '后', 2, 'nom'], ['买', '卖', 2, 'verbe'], ['进', '出', 2, 'verbe'],
    ['问', '答', 2, 'verbe'],
    ['干净', '脏', 3, 'adj'], ['高兴', '难过', 3, 'adj'], ['安静', '吵闹', 3, 'adj'], ['容易', '困难', 3, 'adj'],
    ['勇敢', '胆小', 3, 'adj'], ['聪明', '笨', 3, 'adj'], ['危险', '安全', 3, 'adj'], ['开始', '结束', 3, 'verbe'],
    ['记得', '忘记', 3, 'verbe'], ['喜欢', '讨厌', 3, 'verbe'], ['朋友', '敌人', 3, 'nom'], ['入口', '出口', 3, 'nom'],
    ['里面', '外面', 3, 'nom'], ['白天', '黑夜', 3, 'nom'],
  ].map(([a, b, niveau, nature]) => ({ a, b, niveau, nature }));

  // Phrases à remettre dans l'ordre : mots séparés par des espaces ici, ponctuation en dernier.
  // Le chinois s'écrit sans espace : texte = mots.join('').
  const F = (niveau, decoupage) => {
    const mots = decoupage.split(' ');
    return { texte: mots.join(''), niveau, mots };
  };
  const PHRASES = [
    F(1, '我 喜欢 吃 苹果 。'), F(1, '爸爸 在 看 书 。'), F(1, '小鸟 会 飞 。'),
    F(1, '小猫 在 睡觉 。'), F(1, '我 有 一只 狗 。'), F(1, '妈妈 喝 牛奶 。'),
    F(1, '天气 很 热 。'), F(1, '我们 去 公园 。'), F(1, '天上 有 很多 星星 。'),
    F(1, '弟弟 在 画画 。'),
    F(2, '小猫 在 草地上 玩 。'), F(2, '妹妹 画了 一朵 花 。'), F(2, '桌子上 有 一本 书 。'),
    F(2, '猴子 喜欢 吃 香蕉 。'), F(2, '我们 一起 唱歌 吧 。'), F(2, '这只 小狗 很 可爱 。'),
    F(2, '大象的 鼻子 很 长 。'), F(2, '天上 出现了 一道 彩虹 。'), F(2, '奶奶 讲的 故事 很 好听 。'),
    F(2, '他们 在 海边 玩 沙子 。'), F(2, '你 想 吃 冰淇淋 吗 ？'),
    F(3, '海盗 把 金币 藏在 山洞里 。'), F(3, '船长 用 望远镜 寻找 小岛 。'),
    F(3, '这只 鹦鹉 会 说 三种 语言 。'), F(3, '奶奶 做的 蛋糕 特别 好吃 。'),
    F(3, '大风 把 船长的 帽子 吹走了 。'), F(3, '小朋友们 在 沙滩上 堆 沙堡 。'),
    F(3, '海浪 把 小船 推到了 岸边 。'), F(3, '我们 发现了 一个 神秘的 小岛 。'),
    F(3, '长颈鹿 伸长 脖子 吃 树叶 。'), F(3, '海盗船长 找到了 一张 藏宝图 。'),
  ];

  // Mots supplémentaires sans image (pour les dictées, mots cachés, alphabet…).
  const MOTS_SIMPLES = {
    1: ['爸爸', '妈妈', '哥哥', '姐姐', '弟弟', '妹妹', '爷爷', '奶奶', '你好', '谢谢', '再见', '我', '你', '他', '家'],
    2: ['学校', '老师', '同学', '名字', '早上', '晚上', '今天', '明天', '公园', '生日', '游戏', '音乐', '颜色', '医生', '中国'],
    3: ['船长', '海盗', '水手', '冒险', '宝藏', '大海', '灯塔', '地图', '暴风雨', '图书馆', '探险家', '地平线', '对不起', '没关系', '生日快乐'],
  };

  // Pas de pluriel grammatical en chinois : le jeu « pluriel » n'existe pas (jeux.pluriel = null).
  const PLURIELS = [];
  const REGLES_PLURIEL = {};

  // Pinyin des mots affichés hors MOTS (vérifié par tests/packs.js).
  const PINYIN = {
    // Rimes
    '八': 'bā', '爬': 'pá', '茶': 'chá', '拿': 'ná', '擦': 'cā',
    '鹅': 'é', '河': 'hé', '喝': 'hē', '歌': 'gē', '饿': 'è',
    '鞋': 'xié', '姐': 'jiě', '写': 'xiě', '谢': 'xiè', '街': 'jiē', '铁': 'tiě',
    '七': 'qī', '米': 'mǐ', '皮': 'pí', '西': 'xī',
    '兔': 'tù', '路': 'lù', '读': 'dú',
    '海': 'hǎi', '菜': 'cài', '奶': 'nǎi', '爱': 'ài', '带': 'dài', '猜': 'cāi',
    '水': 'shuǐ', '腿': 'tuǐ', '睡': 'shuì', '鬼': 'guǐ', '贵': 'guì',
    '包': 'bāo', '草': 'cǎo', '跑': 'pǎo', '桃': 'táo',
    '口': 'kǒu', '头': 'tóu', '走': 'zǒu', '肉': 'ròu',
    '伞': 'sǎn', '饭': 'fàn', '蓝': 'lán', '看': 'kàn', '三': 'sān',
    '人': 'rén', '本': 'běn', '盆': 'pén', '根': 'gēn', '分': 'fēn',
    '糖': 'táng', '房': 'fáng', '汤': 'tāng', '唱': 'chàng', '狼': 'láng', '帮': 'bāng',
    '红': 'hóng', '虫': 'chóng', '东': 'dōng', '钟': 'zhōng', '洞': 'dòng',
    // Contraires
    '大': 'dà', '小': 'xiǎo', '多': 'duō', '少': 'shǎo', '高': 'gāo', '矮': 'ǎi', '长': 'cháng', '短': 'duǎn',
    '快': 'kuài', '慢': 'màn', '冷': 'lěng', '热': 'rè', '黑': 'hēi', '白': 'bái', '好': 'hǎo', '坏': 'huài',
    '上': 'shàng', '下': 'xià', '左': 'zuǒ', '右': 'yòu', '开': 'kāi', '关': 'guān', '哭': 'kū', '笑': 'xiào',
    '来': 'lái', '去': 'qù',
    '胖': 'pàng', '瘦': 'shòu', '早': 'zǎo', '晚': 'wǎn', '新': 'xīn', '旧': 'jiù', '远': 'yuǎn', '近': 'jìn',
    '轻': 'qīng', '重': 'zhòng', '对': 'duì', '错': 'cuò', '甜': 'tián', '苦': 'kǔ', '软': 'ruǎn', '硬': 'yìng',
    '干': 'gān', '湿': 'shī', '前': 'qián', '后': 'hòu', '买': 'mǎi', '卖': 'mài', '进': 'jìn', '出': 'chū',
    '问': 'wèn', '答': 'dá',
    '干净': 'gān jìng', '脏': 'zāng', '高兴': 'gāo xìng', '难过': 'nán guò', '安静': 'ān jìng', '吵闹': 'chǎo nào',
    '容易': 'róng yì', '困难': 'kùn nan', '勇敢': 'yǒng gǎn', '胆小': 'dǎn xiǎo', '聪明': 'cōng ming', '笨': 'bèn',
    '危险': 'wēi xiǎn', '安全': 'ān quán', '开始': 'kāi shǐ', '结束': 'jié shù', '记得': 'jì de', '忘记': 'wàng jì',
    '喜欢': 'xǐ huan', '讨厌': 'tǎo yàn', '朋友': 'péng you', '敌人': 'dí rén', '入口': 'rù kǒu', '出口': 'chū kǒu',
    '里面': 'lǐ miàn', '外面': 'wài miàn', '白天': 'bái tiān', '黑夜': 'hēi yè',
    // Mots simples
    '爸爸': 'bà ba', '妈妈': 'mā ma', '哥哥': 'gē ge', '姐姐': 'jiě jie', '弟弟': 'dì di', '妹妹': 'mèi mei',
    '爷爷': 'yé ye', '奶奶': 'nǎi nai', '你好': 'nǐ hǎo', '谢谢': 'xiè xie', '再见': 'zài jiàn',
    '我': 'wǒ', '你': 'nǐ', '他': 'tā', '家': 'jiā',
    '学校': 'xué xiào', '老师': 'lǎo shī', '同学': 'tóng xué', '名字': 'míng zi', '早上': 'zǎo shang',
    '晚上': 'wǎn shang', '今天': 'jīn tiān', '明天': 'míng tiān', '公园': 'gōng yuán', '生日': 'shēng rì',
    '游戏': 'yóu xì', '音乐': 'yīn yuè', '颜色': 'yán sè', '医生': 'yī shēng', '中国': 'zhōng guó',
    '船长': 'chuán zhǎng', '海盗': 'hǎi dào', '水手': 'shuǐ shǒu', '冒险': 'mào xiǎn', '宝藏': 'bǎo zàng',
    '大海': 'dà hǎi', '灯塔': 'dēng tǎ', '地图': 'dì tú', '暴风雨': 'bào fēng yǔ', '图书馆': 'tú shū guǎn',
    '探险家': 'tàn xiǎn jiā', '地平线': 'dì píng xiàn', '对不起': 'duì bu qǐ', '没关系': 'méi guān xi',
    '生日快乐': 'shēng rì kuài lè',
    // Mots des phrases
    '吃': 'chī', '在': 'zài', '小鸟': 'xiǎo niǎo', '会': 'huì', '飞': 'fēi', '小猫': 'xiǎo māo',
    '睡觉': 'shuì jiào', '有': 'yǒu', '天气': 'tiān qì', '很': 'hěn', '我们': 'wǒ men', '天上': 'tiān shàng',
    '很多': 'hěn duō', '画画': 'huà huà', '草地上': 'cǎo dì shang', '玩': 'wán', '画了': 'huà le',
    '桌子上': 'zhuō zi shang', '一起': 'yì qǐ', '唱歌': 'chàng gē', '吧': 'ba', '小狗': 'xiǎo gǒu',
    '可爱': 'kě ài', '大象的': 'dà xiàng de', '出现了': 'chū xiàn le', '讲的': 'jiǎng de', '故事': 'gù shi',
    '好听': 'hǎo tīng', '他们': 'tā men', '海边': 'hǎi biān', '沙子': 'shā zi', '想': 'xiǎng', '吗': 'ma',
    '把': 'bǎ', '金币': 'jīn bì', '藏在': 'cáng zài', '山洞里': 'shān dòng li', '用': 'yòng',
    '寻找': 'xún zhǎo', '小岛': 'xiǎo dǎo', '鹦鹉': 'yīng wǔ', '说': 'shuō', '三种': 'sān zhǒng',
    '语言': 'yǔ yán', '做的': 'zuò de', '特别': 'tè bié', '好吃': 'hǎo chī', '大风': 'dà fēng',
    '船长的': 'chuán zhǎng de', '吹走了': 'chuī zǒu le', '小朋友们': 'xiǎo péng you men', '沙滩上': 'shā tān shang',
    '堆': 'duī', '沙堡': 'shā bǎo', '海浪': 'hǎi làng', '小船': 'xiǎo chuán', '推到了': 'tuī dào le',
    '岸边': 'àn biān', '发现了': 'fā xiàn le', '神秘的': 'shén mì de', '伸长': 'shēn cháng', '脖子': 'bó zi', '树叶': 'shù yè',
    '海盗船长': 'hǎi dào chuán zhǎng', '找到了': 'zhǎo dào le', '藏宝图': 'cáng bǎo tú',
  };

  // Classificateurs et groupes « 一 + cl » / « 这 + cl » des mots illustrés.
  // « 一 » : yí devant un 4e ton, yì devant les autres tons.
  const PINYIN_CL = {
    '只': 'zhī', '个': 'gè', '条': 'tiáo', '匹': 'pǐ', '头': 'tóu', '本': 'běn', '辆': 'liàng', '朵': 'duǒ',
    '张': 'zhāng', '把': 'bǎ', '碗': 'wǎn', '杯': 'bēi', '根': 'gēn', '颗': 'kē', '棵': 'kē', '座': 'zuò',
    '场': 'cháng', '扇': 'shàn', '支': 'zhī', '串': 'chuàn', '道': 'dào', '片': 'piàn', '架': 'jià',
    '列': 'liè', '双': 'shuāng', '顶': 'dǐng', '台': 'tái', '块': 'kuài', '桶': 'tǒng',
  };
  const quatriemeTon = (p) => /̀/.test(p.normalize('NFD'));
  Object.keys(PINYIN_CL).forEach((c) => {
    const p = PINYIN_CL[c];
    if (!PINYIN[c]) PINYIN[c] = p;
    PINYIN['一' + c] = (quatriemeTon(p) ? 'yí ' : 'yì ') + p;
    PINYIN['这' + c] = 'zhè ' + p;
  });

  window.ILE_LANGS = window.ILE_LANGS || {};
  window.ILE_LANGS.zh = {
    code: 'zh',
    nom: '中文',
    drapeau: '🇨🇳',
    tts: 'zh-CN',
    ttsRate: 0.8, // un peu plus lent : les enfants découvrent la langue
    dir: 'ltr',
    htmlLang: 'zh-Hans',
    // Pas d'espace entre les mots ; écriture en caractères (aide : pinyin).
    sepMots: '',
    ecriture: 'hanzi',

    niveaux: [
      { n: 1, nom: '小水手', classe: '入门', emoji: '🐣' },
      { n: 2, nom: '水手', classe: '进阶', emoji: '⚓' },
      { n: 3, nom: '船长', classe: '高级', emoji: '🏴‍☠️' },
    ],

    // Textes de l'interface commune (accueil, en-tête, fenêtre de résultat).
    ui: {
      titreSite: '词语岛',
      accroche: '探索小岛，和词语一起玩，把你的宝箱装满吧！',
      descriptionSite: '给 5 到 10 岁孩子的免费学习游戏：一边玩，一边学中文。',
      choisisLangue: '语言',
      choisisGrade: '选择你的海盗等级',
      lesJeux: '游戏',
      etoilesTotal: (n, t) => n + ' / ' + t + ' 颗星',
      etoilesNiveau: (n) => '这个级别：' + n + ' / 3 颗星',
      surprise: '🎲 随机游戏',
      footer: '给 5 到 10 岁孩子的免费学习游戏 · 进度只保存在这台设备上。',
      effacer: '🧹 清除我的进度',
      confirmerEffacer: '要清除这台设备上中文游戏的所有星星吗？',
      retourIle: '回到小岛',
      niveau: '级别',
      etoilesJeu: '这个游戏里得到的星星',
      sonActiver: '打开声音',
      sonCouper: '关闭声音',
      question: (i, n) => '第 ' + i + ' 题，共 ' + n + ' 题',
      ecouter: '听一听',
      bravo: ['真棒！', '太好了！', '好极了！', '非常好！', '你真厉害！', '做得好！', '完全正确！'],
      encore: ['差一点！', '再试一次！', '不太对哦……', '加油！'],
      resultTitres: ['继续练习吧！', '开了个好头！', '很好！', '太厉害了！'],
      score: (s, t) => s + ' / ' + t + ' 题答对了', // commence par « s / t », comme les autres langues
      etoilesSur3: (n) => n + ' 颗星（满分 3 颗）',
      record: '🏆 新纪录！',
      rejouer: '🔁 再玩一次',
      jeuSuivant: '下一个游戏',
      ile: '小岛',
      parametres: '设置',
      languesVisibles: '可选的语言',
      languesVisiblesAide: '取消勾选某种语言，孩子们就看不到它了（例如隐藏法语，只用外语来玩）。',
      fermer: '关闭',
      jeuIndisponible: '这个游戏没有中文版。请选择别的游戏：',
    },

    // Nom, lieu de l'île, compétence et description de chaque jeu.
    // Jeux de lettres (mélange, pendu, dictée) : on écrit le pinyin sans tons.
    // « lettre » cache un caractère du mot (丢失的汉字), pas une lettre de pinyin.
    jeux: {
      images: { titre: '看图找词', lieu: '沙滩', competence: '识字', desc: '找出和图片对应的词语。' },
      genre: { titre: '选量词', lieu: '码头', competence: '语法', desc: '给每个词选出正确的量词：一只、一个、一本……' },
      lettre: { titre: '丢失的汉字', lieu: '山洞', competence: '识字', desc: '找出词语里缺少的汉字。' },
      melange: { titre: '打乱的字母', lieu: '宝箱', competence: '拼音', desc: '把字母排好，拼出词语的拼音。' },
      syllabes: { titre: '汉字桥', lieu: '藤桥', competence: '识字', desc: '把汉字连起来，组成词语。' },
      memory: { titre: '词语记忆卡', lieu: '村庄', competence: '记忆', desc: '翻开卡片，把图片和词语配成一对。' },
      pendu: { titre: '椰子掉下来', lieu: '椰子树', competence: '拼音', desc: '在椰子掉下来之前，一个一个地猜出拼音字母。' },
      dictee: { titre: '鹦鹉听写', lieu: '丛林', competence: '听写', desc: '听鹦鹉说一个词，把它的拼音写出来。' },
      'mots-caches': { titre: '藏起来的词', lieu: '沙丘', competence: '识字', desc: '在汉字方格里找出藏起来的词语。' },
      rimes: { titre: '找押韵的字', lieu: '瀑布', competence: '语音', desc: '找出押韵的字：它们的韵母一样。' },
      contraires: { titre: '反义词', lieu: '灯塔', competence: '词汇', desc: '把每个词和它的反义词配成一对。' },
      pluriel: null, // pas de pluriel grammatical en chinois
      phrase: { titre: '句子排排队', lieu: '漂流瓶', competence: '语法', desc: '把词语按正确的顺序排成句子。' },
      alphabet: { titre: '音序排列', lieu: '船长的图书馆', competence: '拼音', desc: '按照拼音字母的顺序给词语排队。' },
    },

    // Écriture : les jeux de lettres utilisent le pinyin (26 lettres + ü).
    alphabet: 'abcdefghijklmnopqrstuvwxyz'.split(''),
    voyelles: ['a', 'o', 'e', 'i', 'u', 'ü'],
    familles: {},
    touchesSpeciales: ['ü'],
    lettresClavier: ['ü'],
    ligatures: {},

    // Jeu des classificateurs (量词) : bonne réponse = mot.cl ; après la réponse on affiche
    // mot.art (一只猫). Seuls les mots dont le classificateur fait partie des choix sont posés,
    // et chacun n'a qu'un seul classificateur correct parmi ces choix.
    articles: {
      1: { champ: 'cl', choix: ['个', '只'], affiche: 'art', filtrer: true },
      2: { champ: 'cl', choix: ['个', '只', '本', '条'], affiche: 'art', filtrer: true },
      3: { champ: 'cl', choix: ['个', '只', '本', '条', '辆', '朵', '张', '把'], affiche: 'art', filtrer: true },
    },

    MOTS, MOTS_SIMPLES, RIMES, CONTRAIRES, PHRASES, PLURIELS, REGLES_PLURIEL, PINYIN,
  };
})();
