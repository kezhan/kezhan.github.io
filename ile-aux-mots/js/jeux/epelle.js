/* L'Île aux Mots : jeu « Épelle » (ticket 2026-09-27_jeu-epelle). On entend le mot, on voit l'image, on touche les lettres
   dans l'ordre : chacune devient un anneau de la chenille, qui rote, éternue ou fait pouet quand le mot est fini.
   1: 3 letters, only the right ones, ghost letters · 2: 4 to 5 letters, 2 extra letters · 3: 6 to 10 letters, 3 look-alike traps
   4: dictation without the picture, German then asks der/die/das. Chinese: the characters of the word, look-alike traps at 3 and 4.
   English, German, Luxembourgish or Chinese, never French. Texts and extra words: js/contenus/epelle.js. */
(() => {
const C = EPELLE;
const LV = {
  1: {len: [3, 3], zh: [2, 2], extra: 0, ghost: 2, pic: true, names: true, rounds: 6},
  2: {len: [4, 5], zh: [2, 2], extra: 2, ghost: 1, pic: true, names: true, rounds: 7},
  3: {len: [6, 10], zh: [3, 3], extra: 3, look: true, pic: true, names: true, rounds: 6},
  4: {len: [5, 9], zh: [2, 3], extra: 3, look: true, rounds: 6, art: true}
};
const ALPHA = {en: "abcdefghijklmnopqrstuvwxyz", de: "abcdefghijklmnopqrstuvwxyzäöüß", lb: "abcdefghijklmnopqrstuvwxyzäëé"};
const OKW = {en: /^[a-z]+$/, de: /^[A-Za-zÄÖÜäöüß]+$/, lb: /^[A-Za-zÄËÉÖÜäëéöü]+$/, zh: /^[一-鿿]+$/};
const SEGC = ["#8BD450", "#5CC8A8", "#B5E36B", "#4FB99A"], TILEC = ["#FFD0C6", "#D4ECFF", "#BDEBDD", "#FFE3A3", "#F4E1FF", "#E6F5C2"];
const lang = () => ["en", "de", "lb", "zh"].includes(langOf()) ? langOf() : "en";
const calm = () => typeof fx === "undefined" || fx.calm();
const one = a => a[rnd(a.length)];
const bare = (w, L) => L === "de" ? w.de.replace(/^(der|die|das) /, "") : L === "lb" ? w.lb.replace(/^(d'|(den|de|dat|déi) )/, "") : w[L];
const artOf = (w, L) => L === "de" || L === "lb" ? w[L].slice(0, w[L].length - bare(w, L).length) : "";
const letterName = ch => ch === "ß" ? "ß" : ch.toUpperCase();
const anim = (e, kf, ms, o) => { if (e && !calm()) return e.animate(kf, Object.assign({duration: ms, easing: "ease-in-out"}, o)); };

let pool = null, zhPool = null;
const words = () => pool || (pool = (() => {
  const seen = new Set(), out = [];
  Object.values(THEMES).flatMap(t => t.words).concat(C.extra).forEach(w => { if (!seen.has(w.en)) { seen.add(w.en); out.push(w); } });
  return out;
})());
const zhChars = () => zhPool || (zhPool = [...new Set(words().filter(w => OKW.zh.test(w.zh) && w.zh.length > 1).flatMap(w => [...w.zh]))]);
function choose(L, lv, n, avoid){
  const [lo, hi] = L === "zh" ? lv.zh : lv.len;
  const fit = words().filter(w => {
    if (!w[L] || (L === "lb" && (!w.lod || C.noAudio.includes(w.lod)))) return false; // Luxembourgish is heard only through lod.lu
    const s = bare(w, L), k = [...s].length;
    return OKW[L].test(s) && k >= lo && k <= hi && !avoid.has(s);
  });
  // up to two words missed before come back first
  const missed = w => { const s = S.prof[S.kid].words[w.en]; return s && s[1] > s[0]; };
  const back = shuffle(fit.filter(missed)).slice(0, 2), out = [], used = new Set();
  back.concat(shuffle(fit.filter(w => !back.includes(w)))).forEach(w => { const s = bare(w, L); if (out.length < n && !used.has(s)) { used.add(s); out.push(w); } });
  return out;
}
function traps(L, letters, lv){
  const out = [], have = new Set(letters.map(c => c.toLowerCase()));
  const add = c => { if (c && out.length < (lv.extra || 0) && !have.has(c.toLowerCase()) && !out.includes(c)) out.push(c); };
  if (lv.look) shuffle(letters).forEach(ch => {
    const alts = L === "zh" ? [...(C.zhLook[ch] || "")]
      : [...(C.look[ch.toLowerCase()] || "")].filter(c => ALPHA[L].includes(c) && (ch === ch.toLowerCase() || c !== "ß")) // "ß".toUpperCase() is "SS"
        .map(c => ch === ch.toLowerCase() ? c : c.toUpperCase());
    if (alts.length) add(one(alts));
  });
  const spare = L === "zh" ? zhChars() : [...ALPHA[L]];
  for (let k = 0; k < 80 && out.length < (lv.extra || 0); k++) add(one(spare));
  return out;
}

/* ---------- sounds: Web Audio, silent in the recette ---------- */
function snd(kind){
  if (TEST) return;
  try {
    ac = ac || new (window.AudioContext || window.webkitAudioContext)();
    const t = ac.currentTime;
    const osc = (type, f0, f1, dur, vol = .18, at = 0) => {
      const o = ac.createOscillator(), g = ac.createGain();
      o.type = type; o.frequency.setValueAtTime(f0, t + at); o.frequency.exponentialRampToValueAtTime(f1, t + at + dur);
      g.gain.setValueAtTime(.0001, t + at); g.gain.exponentialRampToValueAtTime(vol, t + at + .02); g.gain.exponentialRampToValueAtTime(.0001, t + at + dur);
      o.connect(g); g.connect(ac.destination); o.start(t + at); o.stop(t + at + dur + .05); return o;
    };
    const wob = (o, rate, depth, dur) => { const l = ac.createOscillator(), g = ac.createGain(); l.frequency.value = rate; g.gain.value = depth; l.connect(g); g.connect(o.frequency); l.start(t); l.stop(t + dur + .05); };
    const noise = (dur, at) => {
      const b = ac.createBuffer(1, Math.floor(ac.sampleRate * dur), ac.sampleRate), d = b.getChannelData(0);
      for (let k = 0; k < d.length; k++) d[k] = (Math.random() * 2 - 1) * (1 - k / d.length);
      const s = ac.createBufferSource(), f = ac.createBiquadFilter(), g = ac.createGain();
      s.buffer = b; f.type = "highpass"; f.frequency.value = 1500; g.gain.value = .45;
      s.connect(f); f.connect(g); g.connect(ac.destination); s.start(t + at);
    };
    ({
      munch: () => { osc("triangle", 260, 150, .07); osc("triangle", 230, 140, .07, .18, .09); },
      pop: () => osc("sine", 500, 1400, .09, .2),
      boing: () => wob(osc("sine", 160, 420, .38, .22), 18, 60, .38),
      raspberry: () => wob(osc("sawtooth", 150, 90, .38, .1), 28, 40, .38),
      burp: () => wob(osc("sawtooth", 115, 68, .6, .15), 13, 25, .6),
      toot: () => wob(osc("square", 95, 58, .5, .1), 32, 30, .5),
      sneeze: () => { [0, 1, 2].forEach(k => osc("sine", 380 + k * 90, 430 + k * 90, .16, .12, k * .2)); noise(.35, .72); },
      giggle: () => { for (let k = 0; k < 6; k++) osc("triangle", k % 2 ? 900 : 1150, k % 2 ? 860 : 1100, .07, .12, k * .08); },
      slideUp: () => osc("sine", 320, 1150, .42, .12),
      slideDown: () => osc("sine", 1100, 260, .5, .12),
      yawn: () => osc("sine", 420, 190, .9, .1)
    })[kind]();
  } catch (e) {}
}

/* ---------- the caterpillar's head (original drawing): blinking eyes, pupils that follow the finger, four mouths ---------- */
const INK = `stroke="#1B2D45" stroke-width="2.5"`;
const HEAD = `<svg viewBox="-6 -24 72 88" aria-hidden="true">
<g class="ep-ant l"><path d="M22 9 Q17 -6 9 -13" fill="none" stroke="#1B2D45" stroke-width="3" stroke-linecap="round"/><circle cx="9" cy="-14" r="5.5" fill="#FF6F59" ${INK}/></g>
<g class="ep-ant r"><path d="M38 9 Q43 -6 51 -13" fill="none" stroke="#1B2D45" stroke-width="3" stroke-linecap="round"/><circle cx="51" cy="-14" r="5.5" fill="#FF6F59" ${INK}/></g>
<circle cx="30" cy="31" r="28" fill="#FFB938" stroke="#1B2D45" stroke-width="3"/>
<ellipse class="ep-cheek" cx="12" cy="40" rx="6.5" ry="4.5" fill="#FF7E96" opacity=".8"/><ellipse class="ep-cheek" cx="48" cy="40" rx="6.5" ry="4.5" fill="#FF7E96" opacity=".8"/>
<ellipse cx="21" cy="25" rx="7.5" ry="9" fill="#fff" ${INK}/><ellipse cx="39" cy="25" rx="7.5" ry="9" fill="#fff" ${INK}/>
<g class="ep-pupil"><circle cx="22" cy="27" r="4" fill="#1B2D45"/><circle cx="23.4" cy="25.4" r="1.3" fill="#fff"/></g>
<g class="ep-pupil"><circle cx="40" cy="27" r="4" fill="#1B2D45"/><circle cx="41.4" cy="25.4" r="1.3" fill="#fff"/></g>
<g class="ep-lid"><ellipse cx="21" cy="25" rx="8.8" ry="10.3" fill="#FFB938" ${INK}/><ellipse cx="39" cy="25" rx="8.8" ry="10.3" fill="#FFB938" ${INK}/></g>
<path class="ep-m smile" d="M19 42 Q30 53 41 42" fill="none" stroke="#1B2D45" stroke-width="3" stroke-linecap="round"/>
<g class="ep-m open"><ellipse cx="30" cy="45" rx="9" ry="8" fill="#8E2436" ${INK}/><ellipse cx="30" cy="49.5" rx="5" ry="3" fill="#FF7E96"/></g>
<g class="ep-m yuck"><path d="M20 45 Q25 41 30 45 T40 45" fill="none" stroke="#1B2D45" stroke-width="3" stroke-linecap="round"/><path d="M26 45.5 q4 11 8 0" fill="#FF7E96" stroke="#1B2D45" stroke-width="2"/></g>
<g class="ep-m oh"><ellipse cx="30" cy="46" rx="4.5" ry="6" fill="#8E2436" ${INK}/></g>
</svg>`;

addStyle(`
.ep-prompt{font-size:clamp(20px,5vw,30px); margin:0}
.ep-stage{position:relative; overflow:hidden; display:flex; flex-direction:column; align-items:center; gap:2px; padding:2px 2px 4px}
.ep-top{display:flex; width:100%; justify-content:space-between; align-items:flex-start; min-height:104px; position:relative}
.ep-think{position:relative; min-width:116px; height:94px; padding:0 16px; display:grid; place-items:center; font-size:60px; line-height:1;
  background:#fff; border:3px solid var(--ink); border-radius:48px; box-shadow:3px 4px 0 var(--ink)}
.ep-think::after{content:""; position:absolute; right:-16px; bottom:-10px; width:16px; height:16px; border-radius:50%; background:#fff; border:3px solid var(--ink)}
.ep-think .swatch{width:62px}
.ep-think.mys{font-family:var(--display); font-weight:700; color:var(--sea-deep); animation:ep-wob 1.8s ease-in-out infinite}
.ep-say{max-width:calc(100% - 142px); margin-top:4px; background:#fff; border:3px solid var(--ink); border-radius:16px 16px 4px 16px; padding:6px 12px;
  font-family:var(--display); font-weight:600; font-size:20px; line-height:1.15; opacity:0; transform:scale(.5) translateY(10px); transform-origin:bottom right;
  transition:opacity .15s, transform .25s cubic-bezier(.3,1.7,.5,1); pointer-events:none}
.ep-say.on{opacity:1; transform:none}
.ep-cat{display:flex; align-items:flex-end; justify-content:center; padding:4px 0 10px}
.ep-seg{position:relative; flex:none; width:var(--s); height:var(--s); margin-right:-6px; border-radius:50%; border:3px dashed rgba(27,45,69,.45);
  background:rgba(255,255,255,.75); display:flex; flex-direction:column; align-items:center; justify-content:center; gap:1px;
  font-family:var(--display); font-weight:700; font-size:calc(var(--s) * .56); color:var(--ink); animation:ep-breathe 2.4s ease-in-out infinite; animation-delay:calc(var(--k) * -.2s)}
.ep-seg.zh{font-family:var(--body); font-size:calc(var(--s) * .46)}
.ep-seg b{font-weight:700; line-height:1} .ep-seg .gh{color:rgba(27,45,69,.28)}
.ep-seg i{font-style:normal; font-family:var(--body); font-weight:800; font-size:11px; line-height:1; color:var(--ink-soft)}
.ep-seg.next{border-color:var(--ink); background:#FFF3C4; animation:ep-next .9s ease-in-out infinite}
.ep-seg.full{border:3px solid var(--ink); background:radial-gradient(circle at 30% 28%, rgba(255,255,255,.6) 0 13%, transparent 14%), var(--c)}
.ep-seg.full::after{content:""; position:absolute; bottom:-9px; left:24%; width:16%; height:9px; border-radius:5px; background:var(--ink); box-shadow:calc(var(--s) * .34) 0 0 var(--ink); z-index:-1}
.ep-cat.go .ep-seg{animation-duration:.3s}
.ep-head{flex:none; width:var(--h); height:calc(var(--h) * 1.22); padding:0; margin-left:-4px; position:relative; z-index:30; animation:ep-nod 2.6s ease-in-out infinite}
.ep-head svg{width:100%; height:100%; overflow:visible; display:block}
.ep-head .ep-m{opacity:0}
.ep-head[data-mood="smile"] .smile,.ep-head[data-mood="open"] .open,.ep-head[data-mood="yuck"] .yuck,.ep-head[data-mood="oh"] .oh{opacity:1}
.ep-pupil{transition:transform .15s}
.ep-lid{transform-box:fill-box; transform-origin:50% 0; transform:scaleY(0); animation:ep-blink 4.3s infinite}
.ep-head[data-mood="yuck"] .ep-lid{animation:none; transform:scaleY(.55)}
.ep-ant{transform-box:fill-box; transform-origin:100% 100%; animation:ep-antl 1.7s ease-in-out infinite}
.ep-ant.r{transform-origin:0 100%; animation-name:ep-antr}
.ep-cheek{transform-box:fill-box; transform-origin:center}
.ep-word{min-height:40px; text-align:center; font-family:var(--display); font-weight:700; font-size:34px; line-height:1.1}
.ep-word small{display:block; font-family:var(--body); font-size:15px; color:var(--ink-soft)}
.ep-word .art{color:var(--coral)} .ep-word .der{color:#1E88E5} .ep-word .die{color:#E53935} .ep-word .das{color:#2F9E6E}
.ep-tiles{display:flex; flex-wrap:wrap; justify-content:center; gap:10px; min-height:74px}
.ep-tile{width:64px; height:64px; padding:0; font-family:var(--display); font-weight:700; font-size:34px; line-height:1; background:var(--c);
  border:3px solid var(--ink); border-radius:16px; box-shadow:3px 4px 0 var(--ink); transform:rotate(var(--r)); transition:transform .08s, opacity .2s, background .2s}
.ep-tile.zh{font-family:var(--body); font-size:32px}
.ep-tile:active{transform:rotate(var(--r)) translateY(3px) scale(.86); box-shadow:0 0 0 var(--ink)}
.ep-tile.no{background:#FF9E8F}
.ep-tile.used{opacity:0; pointer-events:none}
.ep-tile.hint{animation:ep-hint .75s ease-in-out infinite}
.ep-fly{position:fixed; margin:0; z-index:70; pointer-events:none}
.ep-puff{position:fixed; z-index:65; font-size:42px; pointer-events:none}
.ep-arts{display:flex; gap:12px; justify-content:center; width:100%}
.ep-arts button{min-width:88px; height:66px; font-family:var(--display); font-size:28px; font-weight:700}
.ep-arts .der{background:#D4ECFF} .ep-arts .die{background:#FFD6CF} .ep-arts .das{background:#C9F2DF}
.ep-arts .no{animation:shake .4s}
@keyframes ep-breathe{50%{transform:translateY(-3px) scale(1.04,.96)}}
@keyframes ep-next{50%{transform:scale(1.13)}}
@keyframes ep-nod{50%{transform:translateY(-3px) rotate(-4deg)}}
@keyframes ep-blink{0%,91%,100%{transform:scaleY(0)} 95%{transform:scaleY(1)}}
@keyframes ep-antl{50%{transform:rotate(-13deg)}}
@keyframes ep-antr{50%{transform:rotate(13deg)}}
@keyframes ep-wob{50%{transform:rotate(-5deg) scale(1.05)}}
@keyframes ep-hint{50%{transform:rotate(var(--r)) scale(1.2); opacity:.45}}
`);

registerGame({id: "epelle", em: "🐛", name: "Épelle", desc: "Écris le mot lettre par lettre", multi: true,
  title: {en: C.txt.en.title, de: C.txt.de.title, lb: C.txt.lb.title, zh: C.txt.zh.title},
  sub: {en: C.txt.en.sub, de: C.txt.de.sub, lb: C.txt.lb.sub, zh: C.txt.zh.sub}}, function () {
  const lvl = levelOf("epelle"), lv = LV[lvl], small = S.kid === "p4";
  let L = lang(), targets = choose(L, lv, lv.rounds, new Set());
  const total = targets.length, res = [], body = $("gameBody");
  startSession("epelle", null, total); const gen = GEN;
  let i = 0, busy = false, lastTap = Date.now(), R = null;
  const wait = ms => new Promise(r => loops.push(setTimeout(r, TEST ? 0 : ms)));

  const round = () => {
    if (!alive(gen)) return;
    // a flag touched in the game bar: the words still to come are chosen again in the new language
    if (lang() !== L) { L = lang(); const past = targets.slice(0, i); targets = past.concat(choose(L, lv, total - i, new Set(past.map(w => bare(w, L))))); }
    if (i >= total || !targets[i]) return finish();
    busy = false;
    const w = targets[i], word = bare(w, L), letters = [...word.normalize("NFC")], n = letters.length, zh = L === "zh", tx = C.txt[L];
    const py = zh && C.py[w.zh] ? C.py[w.zh].split(" ") : null, full = w[L], art = artOf(w, L);
    const ghost = small && lvl <= 2 ? 2 : lv.ghost || 0, speaks = L === "en" || L === "de";
    const spoken = L === "lb" ? w.lb : small || lvl === 1 ? full : one(C.ask[L])(full, word);
    const ask = () => say(spoken, L);
    let idx = 0, errs = 0, tries = 0;
    renderDots(res, total, i);
    body.innerHTML = "";
    body.append(el("p", "prompt ep-prompt", `🐛 ${lv.pic ? tx.go : tx.dict}`)); // written for both children (Kezhan: a chance to read)
    const row = el("div", "row"); row.style.justifyContent = "center";
    const again = el("button", "speak chunky", `🔊 <span>${tx.again}</span>`);
    again.onclick = () => { if (!busy && lang() !== L) return round(); G.replays++; ask(); };
    const tip = el("button", "chip", "💡"); tip.style.cssText = "font-size:26px; min-width:64px; min-height:56px";
    tip.onclick = () => { if (busy) return; G.hints++; hint(true); };
    row.append(again, tip);

    const stage = el("div", "ep-stage"), top = el("div", "ep-top");
    const think = el("button", "ep-think" + (lv.pic ? "" : " mys"), lv.pic ? wordFace(w) : "?");
    const bub = el("div", "ep-say");
    top.append(think, bub);
    const cat = el("div", "ep-cat");
    const avail = Math.min(body.clientWidth || 330, 560) - 8, hs = Math.max(52, Math.min(76, Math.round(avail * .2)));
    cat.style.setProperty("--s", Math.max(28, Math.min(zh ? 66 : 58, Math.floor((avail - hs + 6 * (n - 1)) / n))) + "px");
    cat.style.setProperty("--h", hs + "px");
    const segs = letters.map((ch, k) => {
      const d = el("div", "ep-seg" + (zh ? " zh" : ""), (ghost === 2 || (ghost === 1 && !k) ? `<b class="gh">${ch}</b>` : "") + (py && lvl < 4 ? `<i>${py[k]}</i>` : ""));
      d.style.setProperty("--k", k); d.style.setProperty("--c", SEGC[k % SEGC.length]); d.style.zIndex = k + 1;
      d.onclick = () => { if (!d.classList.contains("full")) return; snd("boing"); anim(d, [{transform: "none"}, {transform: "translateY(-18px) scale(1.15)"}, {transform: "none"}], 380); if (speaks) say(letterName(letters[k]), L); };
      return d;
    });
    const head = el("button", "ep-head", HEAD); head.dataset.mood = "smile"; head.setAttribute("aria-label", "🐛");
    cat.append(...segs, head);
    stage.append(top, cat);
    const reveal = el("div", "ep-word"), box = el("div", "ep-tiles");
    body.append(row, stage, reveal, box);

    /* ---------- the caterpillar's faces and tricks ---------- */
    let tMood, tBub, tLook;
    const mood = (m, ms) => { head.dataset.mood = m; clearTimeout(tMood); if (ms) loops.push(tMood = setTimeout(() => { head.dataset.mood = "smile"; }, ms)); };
    const bubble = (txt, ms = 1200) => { bub.textContent = txt; bub.classList.add("on"); clearTimeout(tBub); loops.push(tBub = setTimeout(() => bub.classList.remove("on"), ms)); };
    const cheeks = () => head.querySelectorAll(".ep-cheek").forEach(c => anim(c, [{transform: "scale(1)"}, {transform: "scale(1.8)"}, {transform: "scale(1)"}], 700));
    const look = (target, ms = 700) => {
      if (calm() || !target) return;
      const a = head.getBoundingClientRect(), b = target.getBoundingClientRect();
      const dx = b.left + b.width / 2 - a.left - a.width / 2, dy = b.top + b.height / 2 - a.top - a.height / 2, m = Math.hypot(dx, dy) || 1;
      head.querySelectorAll(".ep-pupil").forEach(p => { p.style.transform = `translate(${(dx / m * 2.8).toFixed(1)}px,${(dy / m * 2.8).toFixed(1)}px)`; });
      clearTimeout(tLook); loops.push(tLook = setTimeout(() => head.querySelectorAll(".ep-pupil").forEach(p => { p.style.transform = ""; }), ms));
    };
    const puff = (txt, from, dx, dy) => {
      if (calm()) return;
      const r = from.getBoundingClientRect(), d = el("div", "ep-puff", txt);
      d.style.left = r.left + r.width / 2 + "px"; d.style.top = r.top + r.height / 2 + "px";
      document.body.append(d);
      d.animate([{transform: "translate(-50%,-50%) scale(.4)", opacity: 1}, {transform: `translate(calc(-50% + ${dx}px),calc(-50% + ${dy}px)) scale(1.7)`, opacity: 0}],
        {duration: 1100, easing: "ease-out"}).onfinish = () => d.remove();
    };
    const tricks = [
      () => { snd("burp"); mood("open", 700); puff("🫧", head, 34, -60); cheeks(); anim(head, [{transform: "none"}, {transform: "scale(1.25,.82)"}, {transform: "scale(.94,1.08)"}, {transform: "none"}], 650); },
      () => { mood("oh"); snd("sneeze"); anim(head, [{transform: "none"}, {transform: "rotate(-16deg) translateY(-5px)", offset: .62}, {transform: "rotate(12deg) scale(1.16)", offset: .72}, {transform: "none"}], 1100);
        loops.push(setTimeout(() => { mood("smile"); puff("💦", head, 60, -6); }, 780)); },
      () => { snd("toot"); mood("oh", 600); puff("💨", segs[0], -70, -14); anim(segs[0], [{transform: "none"}, {transform: "translateX(-5px) scale(1.3,.78)"}, {transform: "none"}], 450);
        loops.push(setTimeout(() => { cheeks(); bubble(tx.hihi, 900); snd("giggle"); }, 600)); },
      () => { snd("giggle"); bubble(tx.hihi, 1000); cheeks(); anim(cat, [{transform: "none"}, {transform: "rotate(-3deg)"}, {transform: "rotate(3deg)"}, {transform: "rotate(-2deg)"}, {transform: "none"}], 600); },
      () => { snd("boing"); anim(cat, [{transform: "none"}, {transform: "translateY(-42px) scale(.95,1.07)", offset: .45}, {transform: "translateY(0) scale(1.1,.88)", offset: .8}, {transform: "none"}], 720); }
    ];

    /* ---------- letter tiles ---------- */
    const pieces = shuffle(letters.concat(traps(L, letters, lv)));
    if (n > 1 && pieces.join("") === word) pieces.push(pieces.shift()); // never handed out already in order
    const tiles = pieces.map((ch, k) => {
      const r = rnd(13) - 6, b = el("button", "ep-tile" + (zh ? " zh" : ""), ch), t = {ch, b, used: false};
      b.style.setProperty("--r", r + "deg"); b.style.setProperty("--c", TILEC[k % TILEC.length]);
      b.onclick = () => tap(t);
      box.append(b);
      anim(b, [{transform: `rotate(${r}deg) scale(.2)`, opacity: 0}, {transform: `rotate(${r}deg) scale(1)`, opacity: 1}], 380, {delay: 50 * k, easing: "cubic-bezier(.3,1.6,.5,1)", fill: "backwards"});
      return t;
    });
    const nextTile = () => tiles.find(t => !t.used && t.ch === letters[idx]);
    const marks = () => {
      tiles.forEach(t => delete t.b.dataset.ok);
      segs.forEach((d, k) => d.classList.toggle("next", k === idx && !busy));
      if (!busy && idx < n) markOk((nextTile() || {}).b);
    };
    const hint = manual => {
      const t = nextTile(); if (!t) return;
      t.b.classList.add("hint"); look(t.b, 1600);
      if (manual && !segs[idx].querySelector(".gh")) segs[idx].insertAdjacentHTML("afterbegin", `<b class="gh">${letters[idx]}</b>`);
      if (speaks) say(letterName(letters[idx]), L);
    };
    const fill = (seg, k) => {
      seg.classList.add("full"); seg.classList.remove("next");
      seg.innerHTML = `<b>${letters[k]}</b>` + (py && lvl < 4 ? `<i>${py[k]}</i>` : "");
      snd("pop");
      if (calm()) return;
      anim(seg, [{transform: "scale(.3)"}, {transform: "scale(1.3,.75)", offset: .45}, {transform: "scale(.9,1.12)", offset: .75}, {transform: "none"}], 380, {easing: "ease-out"});
      const r = seg.getBoundingClientRect(); fx.sparkle(r.left + r.width / 2, r.top + r.height / 2, 5);
    };
    // the letter flies from its tile into the next ring of the caterpillar
    const fly = (t, seg, k) => {
      t.b.classList.add("used"); t.b.classList.remove("hint", "no");
      if (calm()) return fill(seg, k);
      const a = t.b.getBoundingClientRect(), b = seg.getBoundingClientRect(), d = t.b.cloneNode(true);
      d.className = "ep-tile ep-fly" + (zh ? " zh" : ""); delete d.dataset.ok;
      Object.assign(d.style, {left: a.left + "px", top: a.top + "px", width: a.width + "px", height: a.height + "px"});
      document.body.append(d);
      const dx = b.left + b.width / 2 - a.left - a.width / 2, dy = b.top + b.height / 2 - a.top - a.height / 2, sc = b.width / a.width;
      d.animate([{transform: "none"}, {transform: `translate(${dx * .5}px,${dy * .5 - 70}px) rotate(200deg) scale(1.15)`, offset: .5}, {transform: `translate(${dx}px,${dy}px) rotate(360deg) scale(${sc})`}],
        {duration: 330, easing: "ease-in"}).onfinish = () => { d.remove(); if (alive(gen)) fill(seg, k); };
    };
    const tap = t => {
      if (busy || t.used || !alive(gen)) return;
      G.taps++; lastTap = Date.now(); look(t.b);
      if (t.ch === letters[idx]) {
        const k = idx++; errs = 0; t.used = true;
        fly(t, segs[k], k);
        mood("open", 170); snd("munch"); anim(head, [{transform: "none"}, {transform: "scale(1.12,.9)"}, {transform: "none"}], 220);
        if (Math.random() < .3) bubble(one(C.yum[L]), 700);
        if (lv.names && speaks) say(letterName(t.ch), L);
        if (idx === n) complete(); else marks();
      } else {
        tries++; errs++;
        if (typeof fx !== "undefined") fx.wrong();
        snd("raspberry"); mood("yuck", 700); bubble(one(C.oops[L]), 900);
        t.b.classList.add("no"); loops.push(setTimeout(() => t.b.classList.remove("no"), 600));
        if (errs >= 2) hint(false); // after two misses the right letter blinks
      }
    };

    /* ---------- German at level 4: der, die or das? ---------- */
    const articleStep = () => new Promise(done => {
      const right = art.trim(), q = C.txt.de.art, row3 = el("div", "ep-arts");
      box.innerHTML = ""; bubble(q, 2600); say(q, "de");
      ["der", "die", "das"].forEach(a => {
        const b = el("button", "chunky " + a, a);
        if (a === right) markOk(b);
        b.onclick = () => {
          if (!alive(gen) || row3.dataset.done) return; G.taps++;
          if (a === right) { row3.dataset.done = "1"; delete b.dataset.ok; snd("pop"); [...row3.children].forEach(x => { if (x !== b) x.style.opacity = ".2"; }); anim(b, [{transform: "none"}, {transform: "scale(1.25)"}, {transform: "none"}], 350); done(); }
          else { tries++; if (typeof fx !== "undefined") fx.wrong(); snd("raspberry"); mood("yuck", 700); b.classList.remove("no"); void b.offsetWidth; b.classList.add("no"); say(q, "de"); }
        };
        row3.append(b);
      });
      box.append(row3);
    });
    const complete = async () => {
      busy = true; marks();
      await wait(380); if (!alive(gen)) return;
      if (lv.art && L === "de" && art) { await articleStep(); if (!alive(gen)) return; }
      const first = tries === 0;
      if (first) addStar();
      logRound(w.en, first, tries + 1, {lvl, lang: L}); res.push(first ? 1 : 0); renderDots(res, total, -1);
      // the word with its article: the gender is learnt with it
      const a = art.trim();
      reveal.innerHTML = (a ? `<span class="art ${a.replace("'", "")}">${a}</span>${art.endsWith(" ") ? " " : ""}` : "") + `<span>${word}</span>` + (zh && C.py[w.zh] ? `<small>${C.py[w.zh]}</small>` : "");
      anim(reveal, [{transform: "scale(.3)", opacity: 0}, {transform: "scale(1.15)", opacity: 1, offset: .6}, {transform: "none"}], 450, {easing: "ease-out"});
      if (!lv.pic) { think.classList.remove("mys"); think.innerHTML = wordFace(w); }
      if (!calm()) {
        segs.forEach((d, k) => anim(d, [{transform: "none"}, {transform: "translateY(-16px) scale(1.1)"}, {transform: "none"}], 420, {delay: 60 * k}));
        anim(think, [{transform: "scale(1)"}, {transform: "scale(1.2) rotate(-6deg)"}, {transform: "none"}], 500);
        loops.push(setTimeout(() => { if (alive(gen)) one(tricks)(); }, 60 * n + 250));
      }
      const cheer = one(C.cheer[L]);
      bubble(cheer, 1800);
      await say(L === "lb" ? w.lb : zh ? cheer + full : `${cheer} ${full}!`, L); if (!alive(gen)) return;
      await wait(900); if (!alive(gen)) return;
      if (!calm()) {
        cat.classList.add("go"); snd("slideDown");
        anim(think, [{opacity: 1}, {opacity: 0}], 400, {fill: "forwards"});
        await new Promise(r => { cat.animate([{transform: "none"}, {transform: `translateX(${avail + 40}px)`}], {duration: 850, easing: "cubic-bezier(.5,0,.9,.6)", fill: "forwards"}).onfinish = r; });
        if (!alive(gen)) return;
      }
      i++; round();
    };

    R = {idle: () => {
      // nobody has touched anything for a while: the little one is shown the letter with the eyes, the big one sees a yawn
      if (small || lvl <= 2) { const t = nextTile(); look(t && t.b, 1500); anim(head, [{transform: "none"}, {transform: "rotate(8deg)"}, {transform: "none"}], 600); if (small) ask(); }
      else { mood("oh", 900); snd("yawn"); anim(head, [{transform: "none"}, {transform: "rotate(-10deg) scale(1.08)"}, {transform: "none"}], 900); }
    }};
    head.onclick = () => { G.taps++; G.replays++; lastTap = Date.now(); snd("giggle"); cheeks(); mood("open", 400); anim(head, [{transform: "none"}, {transform: "rotate(-12deg)"}, {transform: "rotate(10deg)"}, {transform: "none"}], 500); ask(); };
    think.onclick = () => { G.replays++; lastTap = Date.now(); anim(think, [{transform: "none"}, {transform: "scale(1.15) rotate(5deg)"}, {transform: "none"}], 380); ask(); };
    marks();
    if (!calm()) { snd("slideUp"); anim(cat, [{transform: `translateX(-${avail}px)`}, {transform: "translateX(12px)", offset: .8}, {transform: "none"}], 650, {easing: "ease-out"}); }
    ask();
  };
  if (!TEST) loops.push(setInterval(() => { if (alive(gen) && !busy && R && Date.now() - lastTap > 8000) { lastTap = Date.now(); R.idle(); } }, 2000));
  round();
});
})();
