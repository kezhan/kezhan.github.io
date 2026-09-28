/* L'Île aux Mots : état, enregistrement des retours, voix, sons, écrans, sessions et aides communes. */
/* ---------- state ---------- */
const S = {
  kid:"p7", present:false,
  names:{p7:KID_DEFAULT.p7.name, p4:KID_DEFAULT.p4.name},
  prof:{p7:{stars:0, words:{}, scores:{}}, p4:{stars:0, words:{}, scores:{}}},
  pending:[]
};
const $ = id => document.getElementById(id);
const rnd = n => Math.floor(Math.random()*n);
const shuffle = a => { a = a.slice(); for (let i=a.length-1;i>0;i--){ const j=rnd(i+1); [a[i],a[j]]=[a[j],a[i]]; } return a; };
const pick = (a, n) => shuffle(a).slice(0, n);
const uid = () => Date.now().toString(36) + Math.random().toString(36).slice(2,7);
const kidCfg = k => KID_DEFAULT[k || S.kid];
const device = (() => { const u = navigator.userAgent; return /iPad|Macintosh.*Mobile|Macintosh.*Touch/.test(u) || (/Macintosh/.test(u) && navigator.maxTouchPoints > 1) ? "iPad" : /iPhone/.test(u) ? "iPhone" : /Android/.test(u) ? (/Mobile/.test(u) ? "Android phone" : "Android tablet") : /Windows/.test(u) ? "Windows" : /Mac/.test(u) ? "Mac" : "other"; })();

function toast(msg){ const t=$("toast"); t.textContent=msg; t.hidden=false; clearTimeout(toast.h); toast.h=setTimeout(()=>t.hidden=true, 2600); }

/* ---------- local cache (per device, fallback only) ---------- */
function loadLocal(){
  try { const d = JSON.parse(localStorage.getItem("iam") || "null"); if (d) { Object.assign(S.names, d.names||{}); if (d.prof) S.prof = d.prof; S.pending = d.pending || []; S.outbox = d.outbox || []; S.present = !!d.present; S.kid = d.kid || S.kid; } } catch(e) {}
  if (S.names.p4 === "Le petit") S.names.p4 = "La petite"; // she is a girl; fixes names saved by 1.x
  normProf();
}
// profiles saved by version 1.0 have no scores yet
function normProf(){ ["p7","p4"].forEach(k => { S.prof[k] = Object.assign({stars:0, words:{}, scores:{}, levels:{}}, S.prof[k]); }); }
function saveLocal(){
  try { localStorage.setItem("iam", JSON.stringify({names:S.names, prof:S.prof, pending:S.pending.slice(-200), outbox:(S.outbox || []).slice(-200), present:S.present, kid:S.kid})); } catch(e) {}
}

/* ---------- where feedback goes ----------
   on claude.ai (artifact): its database · on the home PC: serveur.py writes retours/*.jsonl · otherwise: this browser, with an export button */
const Store = {
  mode:"local", db:null, queues:{},
  label(){ return tx({artifact:"storeArtifact", server:"storeServer", local:"storeLocal"}[this.mode]); },
  put(kind, id, data){
    const key = kind + "/" + id;
    const run = async () => {
      if (this.mode === "artifact") return this.db.doc(key).set(data);
      const r = await fetch("api/doc", {method:"POST", headers:{"Content-Type":"application/json"}, body:JSON.stringify({kind, id, data})});
      if (!r.ok) throw {code:"unavailable"};
    };
    // one write at a time per document
    this.queues[key] = (this.queues[key] || Promise.resolve()).then(run, run);
    return this.queues[key];
  },
  async get(kind, id){
    if (this.mode === "artifact") { const d = await this.db.doc(kind + "/" + id).get(); return d.exists ? d.data() : null; }
    const r = await fetch("api/doc?kind=" + kind + "&id=" + id); return r.ok ? r.json() : null;
  },
  async list(kind, n){
    if (this.mode === "artifact") return (await this.db.collection(kind).orderBy("t","desc").limit(n).get()).docs.map(d => d.data());
    if (this.mode === "server") { const r = await fetch("api/list?kind=" + kind + "&limit=" + n); return r.ok ? r.json() : []; }
    return [];
  }
};
/* Google Sheet collector (COLLECTE in donnees.js): write-only, no names, lets Claude see what they play from any device.
   Plain-text JSON keeps it a simple request; a no-cors post never reveals success, so only network errors wait in the outbox. */
function sendForm(kind, id, data){
  if (!COLLECTE.url || !navigator.onLine) return Promise.reject();
  const body = JSON.stringify({app:"ile-aux-mots", kind, id, ...data});
  return fetch(COLLECTE.url, {method:"POST", mode:"no-cors", headers:{"Content-Type":"text/plain;charset=utf-8"}, body});
}
function collect(kind, id, data){
  if (!COLLECTE.url || (kind !== "sessions" && kind !== "notes")) return;
  S.outbox = S.outbox || [];
  sendForm(kind, id, data).catch(() => { S.outbox.push({kind, id, data}); saveLocal(); });
}
async function flushOutbox(){
  if (!COLLECTE.url || !(S.outbox || []).length) return;
  const items = S.outbox.splice(0); saveLocal();
  for (const it of items) {
    try { await sendForm(it.kind, it.id, it.data); } catch(e) { S.outbox.push(it); }
  }
  saveLocal();
}
window.addEventListener("online", () => flushOutbox());
function record(kind, id, data){
  collect(kind, id, data);
  if (Store.mode === "local") { S.pending.push({kind, id, data}); saveLocal(); return Promise.resolve(); }
  return Store.put(kind, id, data).catch(e => {
    if (e && e.code === "quota_exceeded") toast(tx("storageFull"));
    S.pending.push({kind, id, data}); saveLocal();
  });
}
async function flushPending(){
  const items = S.pending.splice(0); saveLocal();
  for (const it of items) await record(it.kind, it.id, it.data);
}
function saveProfile(k){ saveLocal(); if (Store.mode !== "local") Store.put("profiles", k, {stars:S.prof[k].stars, words:S.prof[k].words, scores:S.prof[k].scores, levels:S.prof[k].levels, lang:S.prof[k].lang || "en"}).catch(()=>{}); }
function saveConfig(){ saveLocal(); if (Store.mode !== "local") Store.put("config", "main", {names:S.names, present:S.present}).catch(()=>{}); }

async function connect(){
  if (window.claude && window.claude.use) {
    try { const db = await window.claude.use("db"); if (db) { Store.mode = "artifact"; Store.db = db; } } catch(e) {}
  } else if (location.protocol.startsWith("http")) {
    try { const r = await fetch("api/ping"); if (r.ok) Store.mode = "server"; } catch(e) {}
  }
  if (Store.mode === "local") { $("status").textContent = Store.label(); return; }
  try {
    const [cfg, a, b] = await Promise.all([Store.get("config","main"), Store.get("profiles","p7"), Store.get("profiles","p4")]);
    if (cfg) { Object.assign(S.names, cfg.names||{}); S.present = !!cfg.present; } else saveConfig();
    [["p7",a],["p4",b]].forEach(([k,d]) => {
      if (d) S.prof[k] = {stars:d.stars||0, words:JSON.parse(JSON.stringify(d.words||{})), scores:JSON.parse(JSON.stringify(d.scores||{})), levels:JSON.parse(JSON.stringify(d.levels||{})), lang:d.lang || "en"};
      else saveProfile(k);
    });
    await flushPending();
    $("status").textContent = "";
    saveLocal(); renderHome();
  } catch(e) { $("status").textContent = tx("unreachable"); }
}

/* ---------- voice ---------- */
let voices = [];
function loadVoices(){ try { voices = speechSynthesis.getVoices() || []; } catch(e) { voices = []; } }
if ("speechSynthesis" in window) { loadVoices(); speechSynthesis.onvoiceschanged = loadVoices; }
const LANG = {en:"en-GB", fr:"fr-FR", zh:"zh-CN", de:"de-DE", lb:"lb-LU"};
function voiceFor(lang){
  if (!voices.length) loadVoices();
  const want = LANG[lang].toLowerCase(), base = want.slice(0,2);
  const norm = v => v.lang.toLowerCase().replace("_","-");
  const good = v => /google|natural|enhanced|premium|samantha|daniel|serena|amelie|thomas|ting|mei/i.test(v.name);
  const exact = voices.filter(v => norm(v) === want), loose = voices.filter(v => norm(v).startsWith(base));
  const pool = exact.length ? exact : loose;
  return pool.find(good) || pool[0] || null;
}
// English number words from 0 to 100: 14 "fourteen", 40 "forty", 56 "fifty-six"
const NUM_ONES = ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"];
const NUM_TENS = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"];
function numberWords(n){
  if (n < 20) return NUM_ONES[n];
  if (n === 100) return "one hundred";
  const t = Math.floor(n / 10), u = n % 10;
  return NUM_TENS[t] + (u ? "-" + NUM_ONES[u] : "");
}
// automated recette (address ending in #test): instant voice, no confetti, right answers marked
const TEST = /(^#|&)test\b/.test(location.hash);
function markOk(elm){ if (TEST && elm) elm.dataset.ok = "1"; return elm; }
// the voice follows the language being learnt unless a game asks for another one (help in 中文, English words)
function say(text, lang, rate){
  lang = lang || langOf();
  return new Promise(res => {
    if (TEST) { S.lastSaid = text; S.lastLang = lang; return res(); }
    if (lang === "lb") return playLb(text).then(res);
    if (!("speechSynthesis" in window)) return res();
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(text);
      u.lang = LANG[lang]; const v = voiceFor(lang); if (v) u.voice = v;
      u.rate = rate || voiceRate();   // the natural pitch: a raised one sounds robotic
      let done = false; const fin = () => { if (!done) { done = true; res(); } };
      // safety net for a lost onend, long enough for a slow sentence: a fixed 5 s cut long riddles in the middle
      u.onend = fin; u.onerror = fin; setTimeout(fin, 3000 + text.length * 110 / u.rate);
      speechSynthesis.speak(u);
    } catch(e) { res(); }
  });
}


/* ---------- sounds ---------- */
let ac;
function tone(freqs, dur=.12, type="triangle"){
  try {
    ac = ac || new (window.AudioContext || window.webkitAudioContext)();
    const t = ac.currentTime;
    freqs.forEach((f,i) => {
      const o = ac.createOscillator(), g = ac.createGain();
      o.type = type; o.frequency.value = f;
      g.gain.setValueAtTime(.0001, t+i*dur); g.gain.exponentialRampToValueAtTime(.22, t+i*dur+.02); g.gain.exponentialRampToValueAtTime(.0001, t+(i+1)*dur);
      o.connect(g); g.connect(ac.destination); o.start(t+i*dur); o.stop(t+(i+1)*dur+.05);
    });
  } catch(e) {}
}
const sfx = {ok:()=>tone([660,880,1320]), ko:()=>{ tone([300,220],.16,"sine"); if (typeof fx !== "undefined") fx.wrong(); }, pop:()=>tone([900,1500],.05,"square"), win:()=>tone([523,659,784,1047,1319],.12), tap:()=>tone([520],.06)};
function confetti(n=18){
  if (TEST || matchMedia("(prefers-reduced-motion: reduce)").matches) return;
  const set = ["⭐","🎉","✨","🌟","🎈"];
  for (let i=0;i<n;i++){ const s=document.createElement("div"); s.className="confetti"; s.textContent=set[rnd(set.length)]; s.style.left=rnd(100)+"vw"; s.style.animationDelay=(Math.random()*.5)+"s"; document.body.appendChild(s); setTimeout(()=>s.remove(), 2400); }
}
/* ---------- screens ---------- */
const SCREENS = ["home","themes","game","end","parent"];
function show(id){ SCREENS.forEach(s => $(s).hidden = s !== id); window.scrollTo(0,0); if (typeof fx !== "undefined") fx.enter($(id)); }
function goHome(){ GEN++; if (G && !G.done) endSession(false); try { speechSynthesis.cancel(); } catch(e) {} stopLoops(); renderHome(); show("home"); }
document.addEventListener("click", e => { if (e.target.closest("[data-home]")) goHome(); });
$("quitBtn").onclick = goHome;

function el(tag, cls, html){ const n = document.createElement(tag); if (cls) n.className = cls; if (html != null) n.innerHTML = html; return n; }

function renderHome(){
  applyUI();
  const kids = $("kids"); kids.innerHTML = "";
  ["p7","p4"].forEach(k => {
    const c = kidCfg(k), st = S.prof[k].stars, got = Math.floor(st/5);
    const b = el("button","kid chunky");
    b.setAttribute("aria-pressed", String(S.kid === k));
    b.innerHTML = `<span class="ava">${c.ava}</span><span style="display:flex;flex-direction:column;gap:2px;min-width:0">
      <span class="nm"></span><span class="meta">⭐ ${st} · ${tx("nextSticker", 5 - st%5)}</span>
      <span class="stickers">${STICKERS.slice(0, Math.min(got, STICKERS.length)).join("")}</span></span>`;
    b.querySelector(".nm").textContent = kidName(k);
    b.onclick = () => { S.kid = k; saveLocal(); sfx.tap(); renderHome(); sayT(tx("hello", kidName(k))); };
    kids.appendChild(b);
  });
  const map = $("map"); map.innerHTML = "";
  // an island may be kept for one child (meta.ages, e.g. ["p7"] for reading the clock)
  const island = a => {
    const b = el("button","spot chunky");
    b.style.setProperty("--i", map.children.length);
    const sc = S.prof[S.kid].scores[a.id];
    const niv = a.levels === false ? "" : `${tx("lvl", levelOf(a.id))} · `;
    const score = sc && sc.plays ? `<span class="score">${niv}🏆 ${sc.best} · ${fmtTime(sc.secs)}</span>` : `<span class="score">${niv}${tx("isNew")}</span>`;
    // games written for English only say so when another language is chosen
    const badge = a.badge || (!a.multi && langOf() !== "en" ? tx("inEnglish") : "");
    b.innerHTML = `${badge ? `<span class="badge">${badge}</span>` : ""}<span class="em">${a.em}</span><h3>${actTitle(a)}</h3><p>${actSub(a)}</p>${score}`;
    b.onclick = () => { sfx.tap(); openAct(a.id); };
    return b;
  };
  const list = ACTS.filter(a => !a.ages || a.ages.includes(S.kid));
  // islands grouped by category with tabs (js/categories.js); a plain list if that file is missing
  if (typeof renderCategories === "function") renderCategories(map, list, island);
  else list.forEach(a => map.appendChild(island(a)));
  $("homeHint").textContent = tx("chooseIsland", kidName(S.kid));
}

function openAct(id){
  const a = ACTS.find(x => x.id === id);
  if (!a.themes) return launch(id, null);
  openAct.cur = id;
  $("themeTitle").textContent = a.em + " " + actTitle(a);
  const g = $("themeGrid"); g.innerHTML = "";
  Object.entries(THEMES).forEach(([key,t]) => {
    const b = el("button","spot chunky");
    const icon = key === "colors" ? "🎨" : t.icon;
    b.innerHTML = `<span class="em">${icon}</span><h3>${themeLabel(key)}</h3>`;
    b.onclick = () => { sfx.tap(); launch(id, key); };
    g.appendChild(b);
  });
  show("themes");
}

/* ---------- sessions: every game leaves a trace for the feedback loop ---------- */
let G = null;
function startSession(act, theme, total){
  G = {id:uid(), act, theme, kid:S.kid, lvl:levelOf(act), present:S.present, t0:Date.now(), rounds:[], stars:0, replays:0, hints:0, taps:0, done:false, rating:null, total, saved:false};
  $("gStars").textContent = "0";
}

/* ---------- levels: 1 to 4 per child and per game, rising on their own ---------- */
const LEVEL_MAX = 4, LEVEL_START = {p7:3, p4:1}; // the big one knows his times tables: start high
// a game may start a child higher (registerGame meta.start, e.g. {p7:3} for sums in English)
function levelOf(act, kid){
  kid = kid || S.kid;
  const own = (S.prof[kid].levels || {})[act];
  if (own) return own;
  const meta = ACTS.find(a => a.id === act);
  return (meta && meta.start && meta.start[kid]) || LEVEL_START[kid];
}
function setLevel(act, lvl, kid){
  kid = kid || S.kid;
  S.prof[kid].levels = S.prof[kid].levels || {};
  S.prof[kid].levels[act] = Math.max(1, Math.min(LEVEL_MAX, lvl));
  saveProfile(kid);
}
// at least 5 rounds: 80 % right first time goes up, under 50 % goes down
function adjustLevel(g){
  const n = g.rounds.length; if (n < 5) return;
  const rate = g.rounds.filter(r => r.ok).length / n;
  if (rate >= 0.8 && g.lvl < LEVEL_MAX) { g.levelUp = g.lvl + 1; S.prof[g.kid].levels = S.prof[g.kid].levels || {}; S.prof[g.kid].levels[g.act] = g.levelUp; }
  else if (rate < 0.5 && g.lvl > 1) { g.levelDown = g.lvl - 1; S.prof[g.kid].levels = S.prof[g.kid].levels || {}; S.prof[g.kid].levels[g.act] = g.levelDown; }
}
function sessionDoc(g){
  return {
    kid:g.kid, act:g.act, theme:g.theme, lang:langOf(g.kid), lvl:g.lvl, lvlAfter:g.levelUp || g.levelDown || g.lvl, withParent:g.present,
    at:new Date(g.t0).toISOString(), t:g.t0, secs:Math.round(((g.tEnd||Date.now()) - g.t0)/1000),
    completed:g.done, planned:g.total, played:g.rounds.length,
    firstTry:g.rounds.filter(r => r.ok).length,
    rounds:g.rounds, missed:[...new Set(g.rounds.filter(r => !r.ok).map(r => r.w))],
    stars:g.stars, replays:g.replays, hints:g.hints, taps:g.taps, rating:g.rating,
    voice:(voiceFor("en") || {}).name || "none", device, ver:VERSION
  };
}
function logRound(word, firstTry, tries, extra){
  G.rounds.push({w:word, ok:firstTry, tries, ...(extra || {})});
  const w = S.prof[G.kid].words; const s = w[word] || [0,0];
  if (firstTry) s[0]++; else s[1]++;
  w[word] = s;
}
function addStar(n=1){
  const p = S.prof[G.kid], before = p.stars;
  G.stars += n; p.stars += n; $("gStars").textContent = G.stars;
  if (typeof fx !== "undefined") fx.star();
  if (Math.floor(p.stars/5) > Math.floor(before/5)) {
    const s = STICKERS[(Math.floor(p.stars/5) - 1) % STICKERS.length];
    setTimeout(() => { toast(tx("newSticker", s)); confetti(24); if (typeof fx !== "undefined") fx.sticker(s); }, 500);
  }
}
function endSession(completed){
  if (!G || G.saved) return;
  G.done = completed; G.tEnd = Date.now(); G.saved = true;
  // an open-then-leave with nothing played is noise, not feedback
  if (!completed && !G.rounds.length && G.taps < 2 && (G.tEnd - G.t0) < 8000) return;
  const sc = S.prof[G.kid].scores[G.act] || {best:0, plays:0, done:0, secs:0};
  sc.plays++; sc.secs += Math.round((G.tEnd - G.t0)/1000);
  if (completed) {
    sc.done++;
    G.record = sc.done > 1 && G.stars > sc.best;
    sc.best = Math.max(sc.best, G.stars);
    adjustLevel(G);
  }
  sc.last = new Date(G.t0).toISOString();
  S.prof[G.kid].scores[G.act] = sc;
  record("sessions", G.id, sessionDoc(G));
  saveProfile(G.kid);
}
const fmtTime = s => s < 60 ? `${s} s` : s < 3600 ? `${Math.round(s/60)} min` : `${Math.floor(s/3600)} h ${Math.round((s%3600)/60)} min`;
let lastLaunch = null;
function finish(){
  const g = G; endSession(true); stopLoops();
  sfx.win(); confetti();
  const ok = g.rounds.filter(r => r.ok).length;
  $("endEmoji").textContent = g.record ? "🏆" : g.stars >= g.total * 0.8 ? "🥇" : g.stars ? "🌟" : "🎉";
  $("endTitle").textContent = g.record ? tx("newRecord") : praiseT();
  if (g.record) { confetti(30); toast(tx("recordToast", g.stars)); }
  if (g.levelUp) { $("endEmoji").textContent = "🚀"; $("endTitle").textContent = tx("levelTitle", g.levelUp); confetti(40); toast(tx("levelUp", g.levelUp)); if (typeof fx !== "undefined") setTimeout(() => fx.rocket(), 400); }
  else if (g.levelDown) toast(tx("levelDown", g.levelDown));
  $("endSub").textContent = g.rounds.length ? tx("endSub", ok, g.rounds.length, g.stars) : `⭐ ${g.stars}`;
  document.querySelectorAll(".face").forEach(f => f.setAttribute("aria-pressed","false"));
  show("end");
  if (typeof fx !== "undefined") fx.trophy($("endEmoji"));
  sayT($("endTitle").textContent).then(() => sayT(tx("didYouLike")));
}
document.querySelectorAll(".face").forEach(f => f.onclick = () => {
  if (!G) return;
  document.querySelectorAll(".face").forEach(x => x.setAttribute("aria-pressed", String(x === f)));
  G.rating = +f.dataset.r; sfx.tap();
  record("sessions", G.id, sessionDoc(G));
  sayT(tx(G.rating === 3 ? "thanksLove" : "thanks"));
});
$("againBtn").onclick = () => { if (lastLaunch) launch(...lastLaunch); };

/* helpers shared by games */
function speakBtn(getText, label, lang){
  const b = el("button","speak chunky", `🔊 <span>${label || tx("listen")}</span>`);
  b.onclick = () => { G.replays++; say(getText(), typeof lang === "function" ? lang() : lang); }; // lang may be langOf, read at the tap
  return b;
}
function renderDots(done, total, cur){
  const d = $("dots"); d.innerHTML = "";
  for (let i=0;i<total;i++){ const x = el("i"); if (i < done.length) x.className = done[i] ? "ok" : "ko"; else if (i === cur) x.className = "now"; d.appendChild(x); }
}
let loops = [];
function stopLoops(){ loops.forEach(clearInterval); loops.forEach(clearTimeout); loops = []; }
function wordFace(w){ return w.e.startsWith("#") ? `<span class="swatch" style="background:${w.e}"></span>` : w.e; }

let GEN = 0; // bumps on every screen change so a pending voice line cannot resume an old game
const alive = g => g === GEN && !$("game").hidden;
function launch(id, theme){
  GEN++; lastLaunch = [id, theme]; stopLoops();
  $("gameBody").innerHTML = ""; $("dots").innerHTML = "";
  show("game");
  GAMES[id](theme);
}

const GAMES = {}; // each js/jeux/<jeu>.js adds its game
// a new game touches only its own file: registerGame({id, em, name, desc, themes?, badge?, mic?}, fn)
function registerGame(meta, fn){
  if (!ACTS.some(a => a.id === meta.id)) ACTS.push(meta);
  GAMES[meta.id] = fn;
}
function addStyle(css){ const s = document.createElement("style"); s.textContent = css; document.head.append(s); }
