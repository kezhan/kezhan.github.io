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
  normProf();
}
// profiles saved by version 1.0 have no scores yet
function normProf(){ ["p7","p4"].forEach(k => { S.prof[k] = Object.assign({stars:0, words:{}, scores:{}}, S.prof[k]); }); }
function saveLocal(){
  try { localStorage.setItem("iam", JSON.stringify({names:S.names, prof:S.prof, pending:S.pending.slice(-200), outbox:(S.outbox || []).slice(-200), present:S.present, kid:S.kid})); } catch(e) {}
}

/* ---------- where feedback goes ----------
   on claude.ai (artifact): its database · on the home PC: serveur.py writes retours/*.jsonl · otherwise: this browser, with an export button */
const Store = {
  mode:"local", db:null, queues:{},
  label(){ return {artifact:"Retours enregistrés en ligne sur claude.ai.", server:"Retours enregistrés sur le PC, dans le dossier retours.", local:"Retours gardés dans ce navigateur. Lancez serveur.py sur le PC pour que Claude les lise."}[this.mode]; },
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
    if (e && e.code === "quota_exceeded") toast("Mémoire pleine : prévenez Claude");
    S.pending.push({kind, id, data}); saveLocal();
  });
}
async function flushPending(){
  const items = S.pending.splice(0); saveLocal();
  for (const it of items) await record(it.kind, it.id, it.data);
}
function saveProfile(k){ saveLocal(); if (Store.mode !== "local") Store.put("profiles", k, {stars:S.prof[k].stars, words:S.prof[k].words, scores:S.prof[k].scores}).catch(()=>{}); }
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
      if (d) S.prof[k] = {stars:d.stars||0, words:JSON.parse(JSON.stringify(d.words||{})), scores:JSON.parse(JSON.stringify(d.scores||{}))};
      else saveProfile(k);
    });
    await flushPending();
    $("status").textContent = "";
    saveLocal(); renderHome();
  } catch(e) { $("status").textContent = "Carnet de bord injoignable : les retours attendent dans ce navigateur."; }
}

/* ---------- voice ---------- */
let voices = [];
function loadVoices(){ try { voices = speechSynthesis.getVoices() || []; } catch(e) { voices = []; } }
if ("speechSynthesis" in window) { loadVoices(); speechSynthesis.onvoiceschanged = loadVoices; }
const LANG = {en:"en-GB", fr:"fr-FR", zh:"zh-CN"};
function voiceFor(lang){
  if (!voices.length) loadVoices();
  const want = LANG[lang].toLowerCase(), base = want.slice(0,2);
  const norm = v => v.lang.toLowerCase().replace("_","-");
  const good = v => /google|natural|enhanced|premium|samantha|daniel|serena|amelie|thomas|ting|mei/i.test(v.name);
  const exact = voices.filter(v => norm(v) === want), loose = voices.filter(v => norm(v).startsWith(base));
  const pool = exact.length ? exact : loose;
  return pool.find(good) || pool[0] || null;
}
function say(text, lang="en", rate){
  return new Promise(res => {
    if (!("speechSynthesis" in window)) return res();
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(text);
      u.lang = LANG[lang]; const v = voiceFor(lang); if (v) u.voice = v;
      u.rate = rate || (lang === "en" ? (S.kid === "p4" ? 0.75 : 0.85) : 0.95); u.pitch = 1.1;
      let done = false; const fin = () => { if (!done) { done = true; res(); } };
      u.onend = fin; u.onerror = fin; setTimeout(fin, 5000);
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
const sfx = {ok:()=>tone([660,880,1320]), ko:()=>tone([300,220],.16,"sine"), pop:()=>tone([900,1500],.05,"square"), win:()=>tone([523,659,784,1047,1319],.12), tap:()=>tone([520],.06)};
function confetti(n=18){
  if (matchMedia("(prefers-reduced-motion: reduce)").matches) return;
  const set = ["⭐","🎉","✨","🌟","🎈"];
  for (let i=0;i<n;i++){ const s=document.createElement("div"); s.className="confetti"; s.textContent=set[rnd(set.length)]; s.style.left=rnd(100)+"vw"; s.style.animationDelay=(Math.random()*.5)+"s"; document.body.appendChild(s); setTimeout(()=>s.remove(), 2400); }
}
/* ---------- screens ---------- */
const SCREENS = ["home","themes","game","end","parent"];
function show(id){ SCREENS.forEach(s => $(s).hidden = s !== id); window.scrollTo(0,0); }
function goHome(){ GEN++; if (G && !G.done) endSession(false); try { speechSynthesis.cancel(); } catch(e) {} stopLoops(); renderHome(); show("home"); }
document.addEventListener("click", e => { if (e.target.closest("[data-home]")) goHome(); });
$("quitBtn").onclick = goHome;

function el(tag, cls, html){ const n = document.createElement(tag); if (cls) n.className = cls; if (html != null) n.innerHTML = html; return n; }

function renderHome(){
  const kids = $("kids"); kids.innerHTML = "";
  ["p7","p4"].forEach(k => {
    const c = kidCfg(k), st = S.prof[k].stars, got = Math.floor(st/5);
    const b = el("button","kid chunky");
    b.setAttribute("aria-pressed", String(S.kid === k));
    b.innerHTML = `<span class="ava">${c.ava}</span><span style="display:flex;flex-direction:column;gap:2px;min-width:0">
      <span class="nm"></span><span class="meta">${c.age} ans · ⭐ ${st} · prochain autocollant dans ${5 - st%5}</span>
      <span class="stickers">${STICKERS.slice(0, Math.min(got, STICKERS.length)).join("")}</span></span>`;
    b.querySelector(".nm").textContent = S.names[k];
    b.onclick = () => { S.kid = k; saveLocal(); sfx.tap(); renderHome(); say("Hello " + S.names[k] + "!"); };
    kids.appendChild(b);
  });
  const map = $("map"); map.innerHTML = "";
  ACTS.filter(a => !a.mic || MIC_OK).forEach(a => {
    const b = el("button","spot chunky");
    const sc = S.prof[S.kid].scores[a.id];
    const score = sc && sc.plays ? `<span class="score">🏆 ${sc.best} · ${sc.plays} partie${sc.plays > 1 ? "s" : ""} · ${fmtTime(sc.secs)}</span>` : `<span class="score">Nouveau !</span>`;
    b.innerHTML = `${a.badge ? `<span class="badge">${a.badge}</span>` : ""}<span class="em">${a.em}</span><h3>${a.name}</h3><p>${a.desc}</p>${score}`;
    b.onclick = () => { sfx.tap(); openAct(a.id); };
    map.appendChild(b);
  });
  $("homeHint").textContent = `${S.names[S.kid]}, choisis une île !`;
}

function openAct(id){
  const a = ACTS.find(x => x.id === id);
  if (!a.themes) return launch(id, null);
  $("themeTitle").textContent = a.em + " " + a.name;
  const g = $("themeGrid"); g.innerHTML = "";
  Object.entries(THEMES).forEach(([key,t]) => {
    const b = el("button","spot chunky");
    const icon = key === "colors" ? "🎨" : t.icon;
    b.innerHTML = `<span class="em">${icon}</span><h3>${t.label}</h3>`;
    b.onclick = () => { sfx.tap(); launch(id, key); };
    g.appendChild(b);
  });
  show("themes");
}

/* ---------- sessions: every game leaves a trace for the feedback loop ---------- */
let G = null;
function startSession(act, theme, total){
  G = {id:uid(), act, theme, kid:S.kid, present:S.present, t0:Date.now(), rounds:[], stars:0, replays:0, hints:0, taps:0, done:false, rating:null, total, saved:false};
  $("gStars").textContent = "0";
}
function sessionDoc(g){
  return {
    kid:g.kid, age:kidCfg(g.kid).age, act:g.act, theme:g.theme, withParent:g.present,
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
  if (Math.floor(p.stars/5) > Math.floor(before/5)) {
    const s = STICKERS[(Math.floor(p.stars/5) - 1) % STICKERS.length];
    setTimeout(() => { toast("Nouvel autocollant ! " + s); confetti(24); }, 500);
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
  $("endTitle").textContent = g.record ? "New record!" : PRAISE[rnd(PRAISE.length)];
  if (g.record) { confetti(30); toast("Nouveau record : ⭐ " + g.stars); }
  $("endSub").textContent = g.rounds.length ? `${ok} sur ${g.rounds.length} du premier coup · ⭐ ${g.stars}` : `⭐ ${g.stars}`;
  document.querySelectorAll(".face").forEach(f => f.setAttribute("aria-pressed","false"));
  show("end");
  say($("endTitle").textContent).then(() => say("Tu as aimé ?", "fr"));
}
document.querySelectorAll(".face").forEach(f => f.onclick = () => {
  if (!G) return;
  document.querySelectorAll(".face").forEach(x => x.setAttribute("aria-pressed", String(x === f)));
  G.rating = +f.dataset.r; sfx.tap();
  record("sessions", G.id, sessionDoc(G));
  say(G.rating === 3 ? "Yay! Thank you!" : "Thank you!");
});
$("againBtn").onclick = () => { if (lastLaunch) launch(...lastLaunch); };

/* helpers shared by games */
function speakBtn(getText, label="Écoute"){
  const b = el("button","speak chunky", `🔊 <span>${label}</span>`);
  b.onclick = () => { G.replays++; say(getText()); };
  return b;
}
function bridgeBtn(w){
  // the language each child already knows: 中文 for the 7-year-old, French for the 4-year-old
  const lang = kidCfg(G.kid).bridge, txt = lang === "zh" ? w.zh : w.fr;
  const b = el("button","chip", lang === "zh" ? "中文 ?" : "En français ?");
  b.onclick = e => { e.stopPropagation(); G.hints++; b.textContent = txt; say(txt, lang); };
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
