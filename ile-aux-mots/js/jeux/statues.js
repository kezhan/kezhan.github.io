/* L'Île aux Mots : jeu « Les statues musicales » (catégorie Bouger et jouer).
   A happy tune plays (Web Audio API) and the child dances; the music stops at a random moment, the voice says how to freeze,
   and a grown-up (or the child) judges with two buttons, as in Simon says.
   1: dance, then "Freeze!" · 2: freeze like an animal · 3: freeze in a body pose (on one foot, hands on your head)
   4: two moves to remember, told BEFORE the music ("When the music stops, freeze like a cat and close your eyes!"); the voice then only says "Freeze!"
   English, German, Luxembourgish or Chinese, never French. #test: the music lasts 0.3 s and "done" is marked.
   Luxembourgish checked on lod.lu: "réier dech net" (sech réieren: to move, to stir), "lee" (imperative of leeën), d'Statu (f).
   Still to be read by a Luxembourgish speaker: "Stopp! Maach wéi d'Kaz!" (freeze like a cat), "Maach de Mond wäit op",
   "Maach e witzegt Gesiicht", "Musikalesch Statuen" (title), "Lass geet's!", "Mierk der déi zwou Saachen!".
   Luxembourgish is heard only through the lod.lu recording of the animal (d'Kaz, den Hond...): the rest is written. */
(() => {
const ID = "statues";
const TXT = {
  en:{dance:"Dance!", stopHint:"When the music stops: freeze!", freeze:"Freeze!", when:l => `When the music stops, ${low(l)}`,
    remember:"Remember both moves!", go:"🎵 Go!", stopNow:"Stop the music", again:"Again", done:"✅ Done!", notYet:"🔁 Not yet",
    judge:"Grown-up: ✅ if they stay still, like a statue.", judge2:"Grown-up: ✅ only if both moves are right.",
    tryAgain:"Nice try!", referee:"Play with a grown-up: they are the referee!"},
  de:{dance:"Tanz!", stopHint:"Wenn die Musik aufhört: Stopp!", freeze:"Stopp!", when:l => `Wenn die Musik aufhört, ${low(l)}`,
    remember:"Merk dir beides!", go:"🎵 Los!", stopNow:"Musik stoppen", again:"Nochmal", done:"✅ Geschafft!", notYet:"🔁 Noch nicht",
    judge:"Eltern: ✅, wenn sich das Kind nicht bewegt, wie eine Statue.", judge2:"Eltern: ✅ nur, wenn beides stimmt.",
    tryAgain:"Guter Versuch!", referee:"Spiel mit einem Erwachsenen: Er ist der Schiedsrichter!"},
  lb:{dance:"Danz!", stopHint:"Wann d'Musek ophält: Stopp!", freeze:"Stopp!", when:l => `Wann d'Musek ophält, ${low(l)}`,
    remember:"Mierk der déi zwou Saachen!", go:"🎵 Lass geet's!", stopNow:"Musek stoppen", again:"Nach eng Kéier", done:"✅ Gepackt!", notYet:"🔁 Nach net",
    judge:"Elteren: ✅, wann d'Kand sech net réiert, wéi eng Statu.", judge2:"Elteren: ✅ just, wann déi zwou Saache stëmmen.",
    tryAgain:"Gutt probéiert!", referee:"Spill mat engem Erwuessenen: hien ass den Arbitter!"},
  zh:{dance:"跳舞吧！", stopHint:"音乐一停，就不许动！", freeze:"定住！", when:l => `音乐一停，就${l}`,
    remember:"两个动作都要记住！", go:"🎵 开始！", stopNow:"停止音乐", again:"再听一次", done:"✅ 做到了！", notYet:"🔁 还没有",
    judge:"家长：孩子像雕像一样一动不动，就点✅。", judge2:"家长：两个动作都做到了，才点✅。",
    tryAgain:"差一点点！", referee:"和大人一起玩：大人当裁判！"}
};
// German and Luxembourgish imperatives lose their capital inside a sentence ("... und mach die Augen zu")
function low(s){ return s.charAt(0).toLowerCase() + s.slice(1); }
// Chinese measure words for the animals used here (两 is never needed: always one animal)
const CL = {cat:"只", dog:"只", cow:"头", pig:"头", duck:"只", horse:"匹", lion:"头", frog:"只", bird:"只", rabbit:"只", monkey:"只", elephant:"头",
  bear:"头", mouse:"只", chicken:"只", penguin:"只", giraffe:"只", kangaroo:"只", owl:"只", tiger:"只", crocodile:"条", butterfly:"只"};
const enA = w => (/^[aeiou]/.test(w.en) ? "an " : "a ") + w.en;                                    // an elephant
const deEin = w => { const [art, ...n] = w.de.split(" "); return (art === "die" ? "eine " : "ein ") + n.join(" "); }; // "wie ein Löwe": nominative after wie
const zhOne = w => "一" + CL[w.en] + w.zh;                                                            // 一头大象
const lbAnd = next => /^[aeiouäéëdtzhn]/i.test(next) ? "an" : "a";                                  // n-rule: "a maach", "an ..."
// body poses (imperatives, so two of them join into one sentence at level 4)
const POSES = [
  {e:"🦩", en:"Stand on one foot", de:"Steh auf einem Bein", lb:"Stéi op engem Been", zh:"单脚站着"},
  {e:"🙆", en:"Put your hands on your head", de:"Leg die Hände auf den Kopf", lb:"Lee d'Hänn op de Kapp", zh:"把手放在头上"},
  {e:"🙌", en:"Put your hands up high", de:"Heb die Hände hoch", lb:"Streck d'Hänn an d'Luucht", zh:"举起双手"},
  {e:"😌", en:"Close your eyes", de:"Mach die Augen zu", lb:"Maach d'Aen zou", zh:"闭上眼睛"},
  {e:"😛", en:"Stick out your tongue", de:"Streck die Zunge raus", lb:"Streck d'Zong eraus", zh:"吐出舌头"},
  {e:"🤏", en:"Make yourself very small", de:"Mach dich ganz klein", lb:"Maach dech ganz kleng", zh:"缩成小小的一团"},
  {e:"👃", en:"Touch your nose", de:"Fass dir an die Nase", lb:"Beréier deng Nues", zh:"摸着鼻子"},
  {e:"🧘", en:"Sit on the floor", de:"Setz dich auf den Boden", lb:"Setz dech op de Buedem", zh:"坐在地上"},
  {e:"🤪", en:"Make a funny face", de:"Mach ein lustiges Gesicht", lb:"Maach e witzegt Gesiicht", zh:"做个鬼脸"},
  {e:"😮", en:"Open your mouth wide", de:"Mach den Mund weit auf", lb:"Maach de Mond wäit op", zh:"张大嘴巴"}
];
const LINES = {
  still: {en:"Freeze! Don't move!", de:"Stopp! Nicht bewegen!", lb:"Stopp! Réier dech net!", zh:"木头人！不许动！"},
  // Luxembourgish keeps the lexicon form (d'Kaz, den Hond) so that its lod.lu recording is heard
  animal: {en:w => `Freeze like ${enA(w)}!`, de:w => `Erstarre wie ${deEin(w)}!`, lb:w => `Stopp! Maach wéi ${w.lb}!`, zh:w => `定住！变成${zhOne(w)}！`},
  pose: {en:p => `Freeze! ${p.en}!`, de:p => `Stopp! ${p.de}!`, lb:p => `Stopp! ${p.lb}!`, zh:p => `定住！${p.zh}！`},
  both: {en:(w, p) => `Freeze like ${enA(w)} and ${low(p.en)}!`, de:(w, p) => `Erstarre wie ${deEin(w)} und ${low(p.de)}!`,
    lb:(w, p) => `Maach wéi ${w.lb} ${lbAnd(p.lb)} ${low(p.lb)}!`, zh:(w, p) => `变成${zhOne(w)}，${p.zh}！`}
};
const DANCERS = ["💃","🕺","🤸","🙋","🦸","🧚"];

/* the music: three little tunes (melody, bass, drums), scheduled a quarter of a second ahead so that it can stop dead */
const music = (() => {
  let c = null, master = null, timer = null, noise = null;
  // [bpm, melody (MIDI, 0 = rest) in eighths, bass per half bar]
  const TUNES = [
    [132, [72,76,79,76,72,76,79,0,81,79,76,72,74,76,72,0, 72,76,79,84,83,79,77,74,72,74,76,77,79,0,72,0], [48,48,53,55,48,53,55,48]],
    [120, [67,69,71,74,71,69,67,0,64,67,69,67,64,62,64,0, 67,69,71,74,76,74,71,69,67,64,62,64,67,0,67,0], [43,43,48,50,43,48,50,43]],
    [140, [65,0,69,72,69,0,65,69,70,69,67,65,67,0,72,0, 65,67,69,70,72,74,72,70,69,67,65,67,65,0,0,0], [41,41,46,48,41,46,48,41]]
  ];
  const hz = m => 440 * Math.pow(2, (m - 69) / 12);
  const ctx = () => {
    try { c = c || new (window.AudioContext || window.webkitAudioContext)(); if (c.state === "suspended") { const p = c.resume(); if (p && p.catch) p.catch(() => {}); } return c; }
    catch(e) { return null; }
  };
  const noiseBuf = () => {
    if (!noise) { noise = c.createBuffer(1, c.sampleRate / 2, c.sampleRate); const d = noise.getChannelData(0); for (let k = 0; k < d.length; k++) d[k] = Math.random() * 2 - 1; }
    return noise;
  };
  function beep(f, t, dur, vol, type, out){
    const o = c.createOscillator(), g = c.createGain();
    o.type = type || "triangle"; o.frequency.value = f;
    g.gain.setValueAtTime(.0001, t); g.gain.exponentialRampToValueAtTime(vol, t + .012); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    o.connect(g); g.connect(out || c.destination); o.start(t); o.stop(t + dur + .03);
  }
  function kick(t, out){
    const o = c.createOscillator(), g = c.createGain();
    o.type = "sine"; o.frequency.setValueAtTime(150, t); o.frequency.exponentialRampToValueAtTime(45, t + .12);
    g.gain.setValueAtTime(.45, t); g.gain.exponentialRampToValueAtTime(.0001, t + .15);
    o.connect(g); g.connect(out); o.start(t); o.stop(t + .17);
  }
  function hiss(t, dur, vol, out, type, f0, f1){
    const s = c.createBufferSource(), f = c.createBiquadFilter(), g = c.createGain();
    s.buffer = noiseBuf(); f.type = type; f.frequency.setValueAtTime(f0, t); if (f1) f.frequency.exponentialRampToValueAtTime(f1, t + dur);
    g.gain.setValueAtTime(vol, t); g.gain.exponentialRampToValueAtTime(.0001, t + dur);
    s.connect(f); f.connect(g); g.connect(out); s.start(t); s.stop(t + dur + .01);
  }
  function start(){
    const a = ctx(); if (!a) return;
    stop();
    const [bpm, mel, bass] = TUNES[rnd(TUNES.length)], sd = 30 / bpm, out = a.createGain();
    out.gain.value = .32; out.connect(a.destination); master = out;
    let step = 0, t = a.currentTime + .06;
    const tick = () => {
      if (master !== out) return;
      while (t < a.currentTime + .3) {
        const k = step % mel.length, m = mel[k], b = bass[Math.floor(k / 4) % bass.length];
        if (m) { beep(hz(m), t, sd * .9, .13, "triangle", out); beep(hz(m), t, sd * .8, .05, "square", out); }
        if (k % 4 === 0) beep(hz(b), t, sd * 1.8, .22, "triangle", out);
        if (k % 4 === 2) beep(hz(b + 12), t, sd * 1.2, .13, "triangle", out);
        if (k % 2 === 0) kick(t, out); else hiss(t, .05, .1, out, "highpass", 6500);
        t += sd; step++;
      }
    };
    tick(); timer = setInterval(tick, 70); loops.push(timer);
  }
  function stop(){
    clearInterval(timer); timer = null;
    if (master && c) {
      const m = master, now = c.currentTime;
      try { m.gain.cancelScheduledValues(now); m.gain.setValueAtTime(m.gain.value, now); m.gain.linearRampToValueAtTime(.0001, now + .04); } catch(e) {}
      setTimeout(() => { try { m.disconnect(); } catch(e) {} }, 500); // only frees the node: nothing of the game runs here
    }
    master = null;
  }
  // the needle scratches, then the ice tinkles
  function freeze(){
    const a = ctx(); if (!a) return; const t = a.currentTime;
    hiss(t, .24, .35, a.destination, "bandpass", 2600, 300);
    [2093, 2637, 3136, 4186].forEach((f, k) => beep(f, t + .28 + k * .07, .2, .06, "sine"));
  }
  // the pose has been held long enough
  function chime(){ const a = ctx(); if (!a) return; const t = a.currentTime; [1047, 1568].forEach((f, k) => beep(f, t + k * .12, .45, .08, "sine")); }
  return {start, stop, freeze, chime};
})();

addStyle(`
.st-stage{position:relative; height:150px; flex:none; border:3px solid var(--ink); border-radius:22px; overflow:hidden; background:linear-gradient(#2B1B5A,#5A2F8C); transition:background .3s}
.st-lights{position:absolute; inset:-45%; animation:st-spin 7s linear infinite;
  background:radial-gradient(circle at 28% 35%,rgba(255,111,89,.8) 0 7%,transparent 13%), radial-gradient(circle at 70% 30%,rgba(255,196,61,.8) 0 6%,transparent 12%),
  radial-gradient(circle at 48% 72%,rgba(67,170,139,.8) 0 7%,transparent 13%), radial-gradient(circle at 80% 72%,rgba(120,170,255,.8) 0 6%,transparent 12%)}
.st-floor{position:absolute; left:0; right:0; bottom:0; height:22px; border-top:3px solid var(--ink); background:repeating-linear-gradient(90deg,#FFC43D 0 26px,#FF6F59 26px 52px,#43AA8B 52px 78px)}
.st-dancers{position:absolute; left:0; right:0; bottom:14px; display:flex; justify-content:space-evenly; align-items:flex-end}
.st-d{display:inline-block; font-size:clamp(46px,13vw,64px); line-height:1; transform-origin:50% 100%}
.st-d1{animation:st-hop .45s ease-in-out infinite alternate}
.st-d2{animation:st-sway .9s ease-in-out infinite}
.st-d3{animation:st-twist .45s ease-in-out infinite alternate}
.st-notes i{position:absolute; bottom:24px; font-style:normal; font-size:26px; opacity:0; animation:st-note 2.4s linear infinite}
.st-notes i:nth-child(1){left:8%} .st-notes i:nth-child(2){left:36%; animation-delay:.6s} .st-notes i:nth-child(3){left:62%; animation-delay:1.2s} .st-notes i:nth-child(4){left:86%; animation-delay:1.8s}
.st-flakes{position:absolute; inset:0; display:flex; justify-content:space-around; align-items:flex-start; padding-top:8px; font-size:24px; opacity:0; transition:opacity .3s; pointer-events:none}
.st-frozen{background:linear-gradient(#9FD8F5,#E3F6FF)}
.st-frozen *{animation-play-state:paused !important}
.st-frozen .st-lights,.st-frozen .st-notes{opacity:0}
.st-frozen .st-flakes{opacity:1}
.st-frozen .st-d{filter:saturate(.35) brightness(1.15) drop-shadow(0 0 5px #fff)}
.st-party .st-d{animation:st-jump .3s ease-out 4 alternate !important}
.st-pose{font-size:clamp(70px,20vw,110px); text-align:center; line-height:1.1}
.st-go{align-self:center; font-family:var(--display); font-size:28px; font-weight:700; padding:14px 30px; background:var(--sun)}
.st-stop{align-self:center; font-size:26px; padding:6px 18px}
.st-hold{height:14px; flex:none; border:3px solid var(--ink); border-radius:999px; background:#fff; overflow:hidden}
.st-hold i{display:block; height:100%; background:linear-gradient(90deg,#8FD3F4,#C9F2FF); transform-origin:0 50%; animation:st-melt 5s linear forwards}
.statues-judge button{min-width:0}
@keyframes st-hop{to{transform:translateY(-18px) rotate(8deg)}}
@keyframes st-sway{0%,100%{transform:rotate(-12deg)} 50%{transform:rotate(12deg) translateY(-8px)}}
@keyframes st-twist{from{transform:scaleX(1) rotate(-6deg)} to{transform:scaleX(-1) rotate(6deg)}}
@keyframes st-note{0%{transform:translateY(0) rotate(-10deg); opacity:0} 15%{opacity:1} 100%{transform:translateY(-115px) rotate(15deg); opacity:0}}
@keyframes st-spin{to{transform:rotate(360deg)}}
@keyframes st-melt{to{transform:scaleX(0)}}
@keyframes st-jump{to{transform:translateY(-26px) scale(1.15)}}
@media (prefers-reduced-motion:reduce){.st-stage *,.st-hold i{animation:none !important}}
`);

registerGame({id:ID, em:"💃", name:"Les statues musicales", desc:"Danse, puis fige-toi quand la musique s'arrête", multi:true, cat:"bouger",
  title:{en:"Musical statues", de:"Stopptanz", lb:"Musikalesch Statuen", zh:"音乐木头人"},
  sub:{en:"Dance, then freeze!", de:"Tanz, dann Stopp!", lb:"Danz, an dann: Stopp!", zh:"跳舞，然后定住！"}}, function () {
  const lvl = levelOf(ID), total = 6, res = [];
  const lang = ["en","de","lb","zh"].includes(langOf()) ? langOf() : "en", X = TXT[lang];
  const speak = t => say(t, lang);
  const wait = ms => new Promise(r => loops.push(setTimeout(r, ms)));
  const all = f => ({en:f("en"), de:f("de"), lb:f("lb"), zh:f("zh")}); // the help button reads the line in the child's other language
  // the rounds: level 2 easy animals, level 4 every animal, and a different pose each time
  const animals = pick(THEMES.animals.words.filter(w => CL[w.en] && (lvl === 4 || (w.lvl || 1) === 1)), total);
  const poses = pick(POSES, total);
  const orders = [...Array(total)].map((_, k) => {
    const w = animals[k], p = poses[k];
    if (lvl === 1) return {e:"🧍", key:"freeze", txt:L => LINES.still[L]};
    if (lvl === 2) return {e:w.e, key:w.en, txt:L => LINES.animal[L](w)};
    if (lvl === 3) return {e:p.e, key:p.en, txt:L => LINES.pose[L](p)};
    return {e:w.e + p.e, key:`${w.en} + ${p.en}`, txt:L => LINES.both[L](w, p)};
  });
  const danceMs = () => TEST ? 300 : lvl === 1 ? 4000 + rnd(4000) : 5000 + rnd(7000);
  startSession(ID, null, total); const gen = GEN;
  if (!S.present) toast(X.referee);
  const body = $("gameBody");
  let i = 0;

  const stage = () => {
    const st = el("div", "st-stage");
    const ds = pick(DANCERS, 3).map((d, k) => `<span class="st-d st-d${k + 1}">${d}</span>`).join("");
    st.innerHTML = `<div class="st-lights"></div><div class="st-notes"><i>🎵</i><i>🎶</i><i>🎵</i><i>🎶</i></div>
      <div class="st-flakes"><span>❄️</span><span>❄️</span><span>❄️</span><span>❄️</span></div><div class="st-floor"></div><div class="st-dancers">${ds}</div>`;
    return st;
  };

  const round = async () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const o = orders[i], line = o.txt(lang);
    renderDots(res, total, i);
    body.innerHTML = "";
    // level 4: both moves are told before the music, to be kept in mind while dancing
    if (lvl === 4) {
      const ask = X.when(line);
      const p = el("p", "prompt"); p.textContent = "🧠 " + ask; p.append(el("small", "", X.remember));
      const row = el("div", "row"); row.style.justifyContent = "center";
      row.append(speakBtn(() => ask, X.again, lang), bridgeBtn(all(L => TXT[L].when(o.txt(L)))));
      const go = markOk(el("button", "st-go chunky", X.go));
      body.append(el("div", "st-pose", "🧠"), p, row, go);
      const started = new Promise(r => { go.onclick = () => { G.taps++; sfx.pop(); r(); }; });
      speak(ask);
      await started;
      if (!alive(gen)) return;
      body.innerHTML = "";
    }
    // the dance: the tune plays until a random moment, or until the grown-up taps ✋
    const st = stage();
    const dp = el("p", "prompt"); dp.textContent = "💃 " + X.dance; dp.append(el("small", "", lvl === 4 ? "🧠 " + X.remember : X.stopHint));
    const stop = el("button", "chip st-stop", "✋"); stop.setAttribute("aria-label", X.stopNow);
    body.append(st, dp, stop);
    await speak(X.dance);
    if (!alive(gen)) return;
    music.start();
    await new Promise(r => { loops.push(setTimeout(r, danceMs())); stop.onclick = () => { G.taps++; r(); }; });
    music.stop();
    if (!alive(gen)) return;
    freeze(o, line, st);
  };

  const freeze = (o, line, st) => {
    st.classList.add("st-frozen"); music.freeze();
    body.innerHTML = ""; body.append(st);
    const r = st.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 14);
    const pose = el("div", "st-pose", o.e);
    const p = el("p", "prompt");
    const hidden = lvl === 4; // the voice only says "Freeze!": the child remembers, then the grown-up sees what was asked
    const reveal = () => { p.textContent = "🧊 " + line; p.append(el("small", "", lvl === 4 ? X.judge2 : X.judge)); pose.textContent = o.e; };
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => line, X.again, lang), bridgeBtn(all(L => o.txt(L))));
    const hold = el("div", "st-hold", "<i></i>");
    const judge = el("div", "judge statues-judge");
    const ok = markOk(el("button", "chunky", X.done)); ok.style.background = "#C9F2DF";
    const ko = el("button", "chunky", X.notYet); ko.style.background = "#FFD6CF";
    if (hidden) {
      p.textContent = "🧊 " + X.freeze; p.append(el("small", "", "🧠 ❓ + ❓")); pose.textContent = "❓";
      if (TEST) reveal(); else loops.push(setTimeout(() => { if (alive(gen) && !judge.dataset.done) reveal(); }, 3000));
    } else reveal();
    // after five seconds of statue, a soft chime
    if (!TEST) loops.push(setTimeout(() => { if (alive(gen) && !judge.dataset.done) { music.chime(); fx.bounce(ok); } }, 5000));
    const next = async good => {
      if (!alive(gen) || judge.dataset.done) return; judge.dataset.done = "1"; delete ok.dataset.ok;
      G.taps++;
      if (hidden) reveal();
      if (good) {
        sfx.ok(); addStar(); confetti(12);
        st.classList.remove("st-frozen"); st.classList.add("st-party"); fx.bounce(pose);
      } else sfx.ko();
      logRound(o.key, good, 1, {lvl, lang}); res.push(good ? 1 : 0); i++;
      await speak(good ? praiseT() : X.tryAgain);
      if (!alive(gen)) return;
      await wait(TEST ? 0 : 700);
      round();
    };
    ok.onclick = () => next(true);
    ko.onclick = () => next(false);
    judge.append(ok, ko);
    body.append(pose, p, row, hold, judge);
    speak(hidden ? X.freeze : line);
  };
  round();
});
Object.defineProperty(ACTS.find(a => a.id === ID), "badge", {configurable:true, enumerable:true,
  get: () => ({en:"with a grown-up", de:"mit Erwachsenen", lb:"mat Erwuessenen", zh:"和大人一起"})[langOf()] || "with a grown-up"});
})();
