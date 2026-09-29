/* L'Île aux Mots : jeu « Les métiers et leurs outils », en anglais, allemand, luxembourgeois ou chinois (jamais en français).
   The 12 jobs of THEMES.jobs, linked to their tools and to their workplace (emoji), then short written riddles.
   1: find the job heard among 3 pictures, each holding its tool (the little one) · 2: a tool and its job, both ways, 4 choices
   3: the workplace, both ways ("Who works in a hospital?" / "Where does the cook work?"), places written · 4: a written riddle, read silently
      ("I help sick people. Who am I?"), answer with the written word; the voice reads it only on request (a hint)
   Each right answer plays the sound of the job (siren, school bell, heartbeat, rocket...).
   Luxembourgish checked on lod.lu: Spidol, Bauerenhaff, Kommissariat ("um Kommissariat"), Fluchhafen, Weltall, Labo, Garage (m., "am Garage"),
   Atelier, Restaurant, benotzen, schaffen, wien → "wie" before a consonant (n rule), erwëschen ("d'Police huet d'Déif erwëscht"),
   stoppen ("stoppt d'Autoen, déi ze séier fueren"), Trakter, Rakéit, schwiewen, steieren, Mikro, Bün, Stëmm, Pinsel ("ech brauch e ... Pinsel"),
   Faarwen, Experimenter, Mikroskop ("ënner dem Mikroskop"), reparéieren, Ueleg, Zopp, Kuchen, Iessen, Camion, Schlauch, Leeder.
   Still to be read by a Luxembourgish speaker: "an der Pompjeeskasär" (lod.lu has no word for a fire station, Kasär = barracks),
   "Ech maache gutt Iessen an enger grousser Kichen", "Ech bréngen d'Leit an aner Länner", "Ech hunn eng schéi Stëmm",
   "Meng Hänn si schwaarz vum Ueleg". German: "auf der Feuerwache", "auf der Polizeiwache" (idiomatic, to confirm). */
(() => {
const ID = "metiers";
const LG = () => ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const anim = (n, frames, o) => { if (!n || calm()) return null; try { return n.animate(frames, o); } catch(e) { return null; } };

// tools of each job (the first one is held by the job at level 1), by the English word of the lexicon
const TOOLS = {
  "doctor":["🩺","💉","💊"], "teacher":["📚","🧮","📐"], "farmer":["🚜","🌾"], "cook":["🍳","🥘","🧂"],
  "police officer":["🚓","🚔"], "firefighter":["🚒","🧯"], "pilot":["✈️","🛩️"], "singer":["🎤","🎶"],
  "astronaut":["🚀","🌕"], "artist":["🎨","🖌️"], "scientist":["🔬","🧪","🧬"], "mechanic":["🔧","🔩","🛠️"]
};
// the workplace, as it is said after "Who works ...?" (zh: the place alone, 在 is added)
const PLACES = {
  "doctor":        {e:"🏥",   en:"in a hospital",      de:"im Krankenhaus",       lb:"am Spidol",            zh:"医院"},
  "teacher":       {e:"🏫",   en:"at a school",        de:"in der Schule",        lb:"an der Schoul",        zh:"学校"},
  "farmer":        {e:"🏡🐄", en:"on a farm",          de:"auf dem Bauernhof",    lb:"um Bauerenhaff",       zh:"农场"},
  "cook":          {e:"🍽️",   en:"in a restaurant",    de:"im Restaurant",        lb:"am Restaurant",        zh:"餐厅"},
  "police officer":{e:"🏢🚓", en:"at a police station", de:"auf der Polizeiwache", lb:"um Kommissariat",      zh:"警察局"},
  "firefighter":   {e:"🏢🚒", en:"at a fire station",  de:"auf der Feuerwache",   lb:"an der Pompjeeskasär", zh:"消防站"},
  "pilot":         {e:"🛫",   en:"at an airport",      de:"am Flughafen",         lb:"um Fluchhafen",        zh:"机场"},
  "astronaut":     {e:"🪐",   en:"in space",           de:"im Weltall",           lb:"am Weltall",           zh:"太空"},
  "artist":        {e:"🖼️",   en:"in a studio",        de:"im Atelier",           lb:"am Atelier",           zh:"画室"},
  "scientist":     {e:"🥼🧪", en:"in a laboratory",    de:"im Labor",             lb:"am Labo",              zh:"实验室"},
  "mechanic":      {e:"🚗🔧", en:"in a garage",        de:"in der Autowerkstatt", lb:"am Garage",            zh:"修车厂"}
};
// two riddles per job ("first|second"), never with the answer inside (no "I sing" for the singer)
const RIDDLES = {
  "doctor":{en:"I help sick people.|I listen to your heart.", de:"Ich helfe kranken Menschen.|Ich höre dein Herz ab.",
    lb:"Ech hëllefe kranke Leit.|Ech lauschteren, wéi däin Häerz klappt.", zh:"我帮助生病的人。|我听你的心跳。"},
  "teacher":{en:"I help children learn to read and write.|I write on the board.", de:"Ich helfe Kindern, lesen und schreiben zu lernen.|Ich schreibe an die Tafel.",
    lb:"Ech hëllefen de Kanner beim Léieren.|Ech schreiwen op d'Tafel.", zh:"我帮小朋友学习读书写字。|我在黑板上写字。"},
  "farmer":{en:"I look after cows, pigs and chickens.|I drive a tractor.", de:"Ich kümmere mich um Kühe, Schweine und Hühner.|Ich fahre Traktor.",
    lb:"Ech këmmere mech ëm d'Kéi, d'Schwäin an d'Hénger.|Ech fuere mam Trakter.", zh:"我照顾奶牛、猪和鸡。|我开拖拉机。"},
  "cook":{en:"I make tasty food in a big kitchen.|I make soup, noodles and cakes.", de:"Ich mache leckeres Essen in einer großen Küche.|Ich mache Suppe, Nudeln und Kuchen.",
    lb:"Ech maache gutt Iessen an enger grousser Kichen.|Ech maachen Zopp, Nuddelen a Kuchen.", zh:"我每天给大家做好吃的饭菜。|我会做汤、面条和蛋糕。"},
  "police officer":{en:"I catch thieves.|I stop cars that drive too fast.", de:"Ich fange Diebe.|Ich halte Autos an, die zu schnell fahren.",
    lb:"Ech erwëschen d'Déif.|Ech stoppen d'Autoen, déi ze séier fueren.", zh:"我抓小偷。|我拦下开得太快的汽车。"},
  "firefighter":{en:"I spray water on the flames with a hose.|My truck is red and has a long ladder.", de:"Ich spritze mit einem Schlauch Wasser auf die Flammen.|Mein Auto ist rot und hat eine lange Leiter.",
    lb:"Ech sprëtze Waasser mat engem Schlauch.|Mäi Camion ass rout an huet eng laang Leeder.", zh:"我用水管喷水灭火。|我的车是红色的，上面有长长的梯子。"},
  "pilot":{en:"I fly a plane.|I take people to other countries.", de:"Ich fliege ein Flugzeug.|Ich bringe Menschen in andere Länder.",
    lb:"Ech steieren e Fliger.|Ech bréngen d'Leit an aner Länner.", zh:"我开飞机。|我带大家飞到别的国家。"},
  "singer":{en:"I stand on a stage with a microphone.|I have a beautiful voice.", de:"Ich stehe mit einem Mikrofon auf der Bühne.|Ich habe eine schöne Stimme.",
    lb:"Ech sti mat engem Mikro op der Bün.|Ech hunn eng schéi Stëmm.", zh:"我拿着麦克风站在舞台上。|我的声音很好听。"},
  "astronaut":{en:"I fly in a rocket.|I float in space.", de:"Ich fliege mit einer Rakete.|Ich schwebe im Weltall.",
    lb:"Ech fléie mat enger Rakéit.|Ech schwiewen am Weltall.", zh:"我坐火箭飞行。|我在太空里飘来飘去。"},
  "artist":{en:"I paint beautiful pictures.|I need lots of colours and a brush.", de:"Ich male schöne Bilder.|Ich brauche viele Farben und einen Pinsel.",
    lb:"Ech mole schéi Biller.|Ech brauch vill Faarwen an e Pinsel.", zh:"我画漂亮的图画。|我需要很多颜料和一把刷子。"},
  "scientist":{en:"I do experiments in a laboratory.|I look at tiny things under a microscope.", de:"Ich mache Experimente im Labor.|Ich schaue mir winzige Dinge unter dem Mikroskop an.",
    lb:"Ech maachen Experimenter am Labo.|Ech kucke ganz kleng Saachen ënner dem Mikroskop.", zh:"我在实验室里做实验。|我用显微镜看很小很小的东西。"},
  "mechanic":{en:"I repair cars.|My hands are black with oil.", de:"Ich repariere Autos.|Meine Hände sind schwarz vom Öl.",
    lb:"Ech reparéieren Autoen.|Meng Hänn si schwaarz vum Ueleg.", zh:"我会把坏掉的汽车弄好。|我的手上沾满了黑黑的机油。"}
};
const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
// Luxembourgish starts the sentence with the place, so the job keeps its small article and its lod.lu recording plays
const UI = {
  en:{who:"Who uses this?", what:j => `What does the ${j} use?`, whoAt:p => `Who works ${p}?`, where:j => `Where does the ${j} work?`,
      works:(j, p) => `The ${j} works ${p}.`, at:p => p, read:"Read and guess!", whoAmI:"Who am I?", yes:j => `Yes! I'm the ${j}!`, again:"Again"},
  de:{who:"Wer benutzt das?", what:j => `Was benutzt ${j}?`, whoAt:p => `Wer arbeitet ${p}?`, where:j => `Wo arbeitet ${j}?`,
      works:(j, p) => `${cap(j)} arbeitet ${p}.`, at:p => p, read:"Lies und rate!", whoAmI:"Wer bin ich?", yes:j => `Ja! Ich bin ${j}!`, again:"Nochmal"},
  lb:{who:"Wie benotzt dat?", what:j => `Wat benotzt ${j}?`, whoAt:p => `Wie schafft ${p}?`, where:j => `Wou schafft ${j}?`,
      works:(j, p) => `${cap(p)} schafft ${j}.`, at:p => p, read:"Lies a rod!", whoAmI:"Wie sinn ech?", yes:j => `Jo! Ech sinn ${j}!`, again:"Nach eng Kéier"},
  zh:{who:"谁用这个？", what:j => `${j}用什么？`, whoAt:p => `谁在${p}工作？`, where:j => `${j}在哪里工作？`,
      works:(j, p) => `${j}在${p}工作。`, at:p => `在${p}`, read:"读一读，猜一猜！", whoAmI:"我是谁？", yes:j => `对啦！我是${j}！`, again:"再听一次"}
};

/* the sound of each job, made with the Web Audio API (silent in the recette) */
const snd = (() => {
  let c = null, noise = null;
  const ctx = () => { if (TEST) return null; try { c = c || new (window.AudioContext || window.webkitAudioContext)(); if (c.state === "suspended") c.resume(); return c; } catch(e) { return null; } };
  function note(type, f0, f1, dur, vol, at){
    const a = ctx(); if (!a) return;
    const t = a.currentTime + (at || 0), o = a.createOscillator(), g = a.createGain();
    o.type = type; o.frequency.setValueAtTime(f0, t); o.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .02); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(a.destination); o.start(t); o.stop(t + dur + .05);
  }
  function hiss(dur, vol, at, f0, f1){
    const a = ctx(); if (!a) return;
    if (!noise) { noise = a.createBuffer(1, a.sampleRate, a.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    const t = a.currentTime + (at || 0), s = a.createBufferSource(), f = a.createBiquadFilter(), g = a.createGain();
    s.buffer = noise; f.type = "bandpass"; f.frequency.setValueAtTime(f0, t); f.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(a.destination); s.start(t); s.stop(t + dur);
  }
  return {
    "doctor": () => [0, .7].forEach(t => { note("sine", 90, 60, .13, .35, t); note("sine", 80, 55, .15, .3, t + .2); }),          // heartbeat
    "teacher": () => [0, .25, .5].forEach(t => { note("triangle", 1568, 1560, .22, .12, t); note("sine", 3136, 3130, .18, .03, t); }), // school bell
    "farmer": () => { note("sawtooth", 190, 120, .9, .09); note("sine", 95, 62, .9, .15); },                                        // moo
    "cook": () => { hiss(.6, .25, 0, 4000, 6500); note("sine", 1760, 1750, .45, .12, .6); },                                        // sizzle, ding
    "police officer": () => [0, .3, .6, .9].forEach((t, k) => note("square", k % 2 ? 660 : 880, k % 2 ? 655 : 875, .28, .04, t)),   // wee-woo
    "firefighter": () => [0, .36, .72, 1.08].forEach((t, k) => note("triangle", k % 2 ? 440 : 587, k % 2 ? 438 : 585, .34, .13, t)), // nee-naw
    "pilot": () => { hiss(1.1, .35, 0, 300, 3500); note("sawtooth", 110, 240, 1.1, .04); },                                         // take-off
    "singer": () => [523, 659, 784, 1047].forEach((f, k) => note("triangle", f, f * 1.01, .22, .12, k * .2)),                       // la la la
    "astronaut": () => { [0, .3, .6].forEach(t => note("square", 880, 875, .08, .05, t)); note("sawtooth", 80, 700, 1, .07, .9); hiss(1, .3, .9, 200, 2500); },
    "artist": () => { hiss(.15, .3, 0, 2000, 5000); hiss(.15, .3, .22, 2000, 5000); [1319, 1568, 2093].forEach((f, k) => note("sine", f, f, .14, .08, .5 + k * .1)); },
    "scientist": () => { [0, .12, .24, .36, .48].forEach((t, k) => note("sine", 300 + k * 120, 900 + k * 150, .08, .12, t)); note("square", 1200, 200, .1, .06, .7); },
    "mechanic": () => [0, .2, .4].forEach(t => { note("square", 320, 180, .07, .07, t); hiss(.05, .3, t, 3000, 1500); })            // clank
  };
})();

addStyle(`
.metiers-scene{align-self:center; background:#fff; padding:6px 22px; display:flex; gap:10px; align-items:center; justify-content:center; min-width:140px; max-width:100%}
.metiers-gros{font-size:clamp(64px,18vw,100px); line-height:1.1; display:inline-block}
.metiers-perso{position:relative; display:inline-block; line-height:1; animation:bob 2.4s ease-in-out infinite; animation-delay:var(--d,0s)}
.metiers-outil{position:absolute; right:-.3em; bottom:-.15em; font-size:.42em; width:1.55em; height:1.55em; display:grid; place-items:center; background:#fff; border:2px solid var(--ink); border-radius:50%}
.choices.metiers-trois{grid-template-columns:repeat(3,minmax(0,1fr)); gap:10px}
.choice.metiers-lieu{aspect-ratio:auto; min-height:120px; font-size:40px; padding:10px 6px; gap:6px}
.metiers-lieu-t{font-size:17px; font-family:var(--display); font-weight:600; line-height:1.15; text-align:center; overflow-wrap:anywhere}
.choice.metiers-txt{aspect-ratio:auto; min-height:88px; font-size:22px; font-family:var(--display); font-weight:600; padding:10px 8px; text-align:center; line-height:1.15}
.metiers-devinette{display:flex; gap:12px; align-items:center; background:#fff; padding:12px 16px; max-width:100%; align-self:center}
.metiers-masque{font-size:56px; line-height:1; flex:none; display:inline-block; min-width:1.1em; text-align:center}
.metiers-texte{font-family:var(--display); font-size:clamp(19px,4.8vw,26px); font-weight:600; line-height:1.25; overflow-wrap:anywhere}
.metiers-texte b{display:block; margin-top:6px; color:var(--coral)}
@media (prefers-reduced-motion: reduce){ .metiers-perso{animation:none} }
`);

// a tool flies from one element to another (the tool into the hands of its job)
function fly(txt, from, to){
  if (calm() || !from || !to) return;
  const a = from.getBoundingClientRect(), b = to.getBoundingClientRect();
  const x0 = a.left + a.width / 2, y0 = a.top + a.height / 2, dx = b.left + b.width / 2 - x0, dy = b.top + b.height / 2 - y0;
  const d = document.createElement("div"); d.textContent = txt;
  d.style.cssText = `position:fixed; left:${x0}px; top:${y0}px; font-size:64px; pointer-events:none; z-index:60; will-change:transform; transform:translate(-50%,-50%)`;
  document.body.append(d);
  d.animate([
    {transform: "translate(-50%,-50%) scale(1)"},
    {transform: `translate(calc(-50% + ${dx / 2}px), calc(-50% + ${dy / 2 - 90}px)) scale(1.3) rotate(-25deg)`, offset: .5},
    {transform: `translate(calc(-50% + ${dx}px), calc(-50% + ${dy}px)) scale(.5) rotate(10deg)`, opacity: .4}
  ], {duration: 750, easing: "ease-in-out"}).onfinish = () => { d.remove(); fx.bounce(to); fx.sparkle(b.left + b.width / 2, b.top + b.height / 2, 10); };
}

registerGame({id:ID, em:"🧑‍🚒", name:"Les métiers", desc:"Métiers, outils et lieux de travail", multi:true, cat:"monde",
  title:{en:"Jobs", de:"Berufe", lb:"Beruffer", zh:"职业"},
  sub:{en:"Who uses this?", de:"Wer benutzt das?", lb:"Wie benotzt dat?", zh:"谁用这个？"}}, function () {
  const lang = LG(), lvl = levelOf(ID), U = UI[lang], P = PHRASES[lang] || PHRASES.en;
  const ALL = THEMES.jobs.words, easy = ALL.filter(j => (j.lvl || 1) <= 2), withPlace = ALL.filter(j => PLACES[j.en]);
  const nm = j => j[lang] || j.en;
  const praise = () => P.praise[rnd(P.praise.length)];
  const one = a => a[rnd(a.length)];
  const person = (j, tool) => `<span class="metiers-perso">${j.e}${tool ? `<span class="metiers-outil">${TOOLS[j.en][0]}</span>` : ""}</span>`;
  const jobChoices = (j, others, tool) => [j, ...others].map(x => ({html: person(x, tool), ok: x === j, label: nm(x), name: P.thats(nm(x))}));
  const big = e => `<span class="metiers-gros">${e}</span>`;
  const total = lvl === 1 ? 6 : 8;

  const rounds = (() => {
    if (lvl === 1) return pick(easy, total).map(j => {
      const ask = P.find(nm(j));
      return {kind: "find", key: j.en, job: j, ask, prompt: "🔎 " + ask, grid3: true, after: `${praise()} ${P.thats(nm(j))}`,
        choices: jobChoices(j, pick(easy.filter(x => x !== j), 2), true)};
    });
    if (lvl === 2) return pick(ALL, total).map((j, k) => {
      const tool = one(TOOLS[j.en]), others = pick(ALL.filter(x => x !== j), 3);
      if (k % 2 === 0) return {kind: "tool", key: `${tool} ${j.en}`, job: j, tool, ask: U.who, prompt: "🧰 " + U.who, scene: big(tool),
        after: `${praise()} ${P.thats(nm(j))}`, choices: jobChoices(j, others)};
      const ask = U.what(nm(j));
      return {kind: "job", key: `${j.en} ${tool}`, job: j, tool, ask, prompt: "🧰 " + ask, scene: big(j.e), after: praise(),
        choices: [{html: tool, ok: true}, ...others.map(x => ({html: one(TOOLS[x.en])}))]};
    });
    if (lvl === 3) return pick(withPlace, total).map((j, k) => {
      const pl = PLACES[j.en], others = pick(withPlace.filter(x => x !== j), 3), after = U.works(nm(j), pl[lang]);
      if (k % 2 === 0) {
        const ask = U.whoAt(pl[lang]);
        return {kind: "whoAt", key: `${j.en} @${pl.en}`, job: j, place: pl, ask, prompt: "📍 " + ask, scene: big(pl.e), after, choices: jobChoices(j, others)};
      }
      const ask = U.where(nm(j));
      return {kind: "where", key: `${j.en} @${pl.en}`, job: j, place: pl, ask, prompt: "📍 " + ask, scene: big(j.e), after,
        choices: [j, ...others].map(x => { const p = PLACES[x.en]; return {html: `<span>${p.e}</span><span class="metiers-lieu-t">${U.at(p[lang])}</span>`, ok: x === j, cls: "metiers-lieu", name: U.at(p[lang])}; })};
    });
    return pick(ALL, total).map(j => {
      const rid = one(RIDDLES[j.en][lang].split("|"));
      return {kind: "riddle", key: `riddle ${j.en}`, job: j, ask: U.read, prompt: "🔮 " + U.read, riddle: rid,
        listen: lang === "lb" ? null : `${rid} ${U.whoAmI}`, after: U.yes(nm(j)),   // no Luxembourgish voice reads a whole sentence
        choices: [j, ...pick(ALL.filter(x => x !== j), 3)].map(x => ({html: nm(x), ok: x === j, cls: "metiers-txt", name: nm(x)}))};
    });
  })();

  const res = []; let i = 0;
  startSession(ID, null, total); const gen = GEN;
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));

  // the fun part of a right answer, different at each level
  function celebrate(r, b, scene){
    const j = r.job;
    if (r.kind === "find") { anim(b.querySelector(".metiers-outil"), [{transform: "scale(1)"}, {transform: "scale(1.9) rotate(-15deg)"}, {transform: "scale(1)"}], {duration: 750, easing: "ease-out"}); fx.bounce(b); }
    else if (r.kind === "tool") { const t = scene.querySelector(".metiers-gros"); fly(r.tool, t, b); if (!calm()) t.style.visibility = "hidden"; }
    else if (r.kind === "job") { fly(r.tool, b, scene); }
    else if (r.kind === "whoAt" || r.kind === "where") {
      // the job walks to its workplace
      scene.innerHTML = big(j.e) + big(r.place.e);
      anim(scene.firstChild, [{transform: "translateX(-90px) rotate(-8deg)", opacity: 0}, {transform: "translateX(-40px) translateY(-20px) rotate(6deg)", opacity: 1, offset: .5}, {transform: "none"}], {duration: 850, easing: "ease-out"});
      if (!calm()) { const s = scene.getBoundingClientRect(); fx.sparkle(s.left + s.width / 2, s.top + s.height / 2, 12); }
    } else if (r.kind === "riddle") {
      // the mask flips over: it was the ... !
      const m = scene.querySelector(".metiers-masque");
      const swap = () => { m.textContent = j.e; anim(m, [{transform: "rotateY(90deg) scale(1.4)"}, {transform: "rotateY(0) scale(1)"}], {duration: 350, easing: "ease-out"}); if (!calm()) { const s = m.getBoundingClientRect(); fx.sparkle(s.left + s.width / 2, s.top + s.height / 2, 12); } };
      const a = anim(m, [{transform: "rotateY(0)"}, {transform: "rotateY(90deg)"}], {duration: 220, easing: "ease-in"});
      if (a) a.onfinish = swap; else swap();
    }
  }

  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const r = rounds[i]; let tries = 0, locked = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("p", "prompt", r.prompt));
    let scene = null;
    if (r.scene) scene = el("div", "metiers-scene chunky", r.scene);
    if (r.riddle) scene = el("div", "metiers-devinette chunky", `<span class="metiers-masque">❓</span><span class="metiers-texte">${r.riddle}<b>${U.whoAmI}</b></span>`);
    if (scene) body.append(scene);
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => r.ask, U.again, lang));
    // reading rounds: the riddle is read aloud only on request, counted as a hint
    if (r.listen) { const h = el("button", "chip", "🔊 📖"); h.onclick = () => { G.hints++; say(r.listen, lang); }; row.append(h); }
    body.append(row);
    const grid = el("div", "choices" + (r.grid3 ? " metiers-trois" : ""));
    shuffle(r.choices).forEach((c, k) => {
      const b = el("button", "choice chunky" + (c.cls ? " " + c.cls : ""), `${c.html}<span class="w"></span>`);
      b.style.setProperty("--d", (k * .35) + "s");
      if (c.ok) markOk(b);
      b.onclick = async () => {
        if (locked) return; G.taps++;
        if (c.ok) {
          locked = true; delete b.dataset.ok; b.classList.add("ok"); sfx.ok();
          const first = tries === 0; if (first) addStar();
          logRound(r.key, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
          celebrate(r, b, scene);
          await wait(250); if (!alive(gen)) return;
          (snd[r.job.en] || (() => {}))();
          await wait(1200); if (!alive(gen)) return;
          await say(r.after, lang); if (!alive(gen)) return;
          i++; loops.push(setTimeout(round, TEST ? 30 : 700));
        } else {
          tries++; sfx.ko(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
          if (c.label) b.querySelector(".w").textContent = c.label;
          say(c.name || r.ask, lang);
        }
      };
      grid.append(b);
    });
    body.append(grid);
    say(r.ask, lang);
  };
  round();
});
})();
