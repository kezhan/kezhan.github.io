/* Académia : la voix lit les questions et les explications (la petite ne lit pas encore). Voix du navigateur pour
   le français, l'anglais et l'allemand. Le luxembourgeois n'est JAMAIS dit par le navigateur (sa voix serait
   mauvaise) : chaque phrase enregistrée d'un seul tenant (exemple du dictionnaire), sinon chaque mot, est jouée
   depuis son enregistrement de lod.lu, copié dans le jeu (assets/audio/lb, donnees/lexique_lb.json, données CC0)
   pour marcher sans réseau. Un passage sans enregistrement se tait (le contrôle des questions l'interdit, la recette
   le relève dans window.__lbMuet) ; un enregistrement qui ne joue pas est redemandé une fois. Quand la tablette passe
   à une autre application, la voix et les sons s'arrêtent. */
const LOCALES = {fr: "fr-FR", en: "en-GB", de: "de-DE", lb: "lb-LU", zh: "zh-CN"};
let voixListe = [];
function chargerVoix(){ try { voixListe = speechSynthesis.getVoices() || []; } catch (e) { voixListe = []; } }
if ("speechSynthesis" in window) { chargerVoix(); speechSynthesis.onvoiceschanged = chargerVoix; }
function voixPour(lang){
  if (!voixListe.length) chargerVoix();
  const voulu = LOCALES[lang].toLowerCase(), base = voulu.slice(0, 2);
  const norm = v => v.lang.toLowerCase().replace("_", "-");
  const belle = v => /natural|online|google|enhanced|premium|neural/i.test(v.name);
  const exactes = voixListe.filter(v => norm(v) === voulu), proches = voixListe.filter(v => norm(v).startsWith(base));
  const l = exactes.length ? exactes : proches;
  return l.find(belle) || l[0] || null;
}
// emojis are shown, never read aloud ("🍎🍎" would be read as "pomme rouge pomme rouge")
const sansEmoji = t => String(t || "").replace(/[\p{Extended_Pictographic}\u{FE0F}\u{200D}]/gu, "").replace(/\s+/g, " ").trim();
let voixCoupee = false, generation = 0, audioLb = null;
// the music listens (js/sons.js): it steps back while the voice speaks
const signalerVoix = parle => document.dispatchEvent(new CustomEvent("voix", {detail: parle ? "debut" : "fin"}));
// options.lent: a foreign word said slowly
function dire(texte, lang = "fr", options){
  const t = sansEmoji(texte), moi = ++generation, lent = !!(options && options.lent);
  if (TEST) {   // nothing is heard in the checks; a Luxembourgish passage without its recordings is noted
    window.__dit = t;
    if (lang === "lb") chargerLexiqueLb().then(lex => { if (!enregistrementsLb(motsLb(t), lex)) (window.__lbMuet = window.__lbMuet || []).push(t); });
    return Promise.resolve();
  }
  if (audioLb) { audioLb.pause(); audioLb = null; }
  if (!t || voixCoupee) return Promise.resolve();
  signalerVoix(true);
  const fini = () => { if (moi === generation) signalerVoix(false); };
  const p = lang === "lb" ? direLb(t, moi) : parler(t, lang, lent);   // Luxembourgish: the recordings, nothing else
  return p.then(fini, fini);
}
// in the Android app (android/), the web view has no speech synthesis: the tablet's own voice speaks, and tells when
// it has finished (window.__voixAndroid); never for Luxembourgish (the app refuses it too)
const attenteAndroid = {};
let numeroAndroid = 0;
window.__voixAndroid = id => { const f = attenteAndroid[id]; delete attenteAndroid[id]; if (f) f(); };
function parlerAndroid(t, lang, lent){
  return new Promise(fin => {
    const id = "v" + (++numeroAndroid), p = P();
    attenteAndroid[id] = fin;
    setTimeout(() => window.__voixAndroid(id), 3000 + t.length * 110);   // never waits forever
    try { AcademiaAndroid.parler(id, t, LOCALES[lang] || "fr-FR", lent ? .7 : p && p.age <= 5 ? .85 : .95); } catch (e) { window.__voixAndroid(id); }
  });
}
function parler(t, lang, lent = false){
  if (window.AcademiaAndroid && lang !== "lb") return parlerAndroid(t, lang, lent);
  return new Promise(fin => {
    if (!("speechSynthesis" in window)) return fin();
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(t), v = voixPour(lang);
      u.lang = v ? v.lang : (LOCALES[lang] || "fr-FR"); if (v) u.voice = v;
      const p = P(); u.rate = lent ? .7 : p && p.age <= 5 ? .82 : .92;   // a little slower for the little ones, slow for a foreign word
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      u.onend = ok; u.onerror = ok; setTimeout(ok, 2500 + t.length * 90);
      setTimeout(() => { try { speechSynthesis.resume(); speechSynthesis.speak(u); } catch (e) { ok(); } }, 60);
    } catch (e) { fin(); }
  });
}

// Luxembourgish: the recordings of the words, one after the other ("d'Kaz": the article, then the word)
let lexiqueLb = null;
async function chargerLexiqueLb(){
  for (let essai = 0; !lexiqueLb && essai < 3; essai++) {   // a network hiccup: asked again (nothing kept on failure)
    try { const r = await fetch("donnees/lexique_lb.json"); if (r.ok) lexiqueLb = (await r.json()).mots || {}; } catch (e) {}
    if (!lexiqueLb) await new Promise(ok => setTimeout(ok, 400 * (essai + 1)));
  }
  return lexiqueLb || {};
}
function motsLb(t){
  const mots = [];
  t.replace(/[?!.,;:«»]/g, " ").split(/\s+/).filter(Boolean).forEach(m => {
    m = m.toLowerCase().replace("’", "'");
    if (m.startsWith("d'") && m.length > 2) { mots.push("d'"); m = m.slice(2); }
    mots.push(m);
  });
  return mots;
}
// a text that the recordings say entirely (the lexicon already loaded: chargerLexiqueLb at the start of the game)
const couvertLb = t => !!lexiqueLb && !!enregistrementsLb(motsLb(sansEmoji(t)), lexiqueLb);
// the recordings that say a text: the longest sentence recorded whole first (« ech hunn dech gär », an example of
// lod.lu), else word by word; null when a word has none
function enregistrementsLb(mots, lex){
  const urls = [];
  for (let i = 0; i < mots.length;) {
    let n = Math.min(mots.length - i, 8);
    while (n > 1 && !(lex[mots.slice(i, i + n).join(" ")] || {}).audio) n--;
    const e = lex[mots.slice(i, i + n).join(" ")];
    if (!e || !e.audio) return null;
    urls.push(e.audio); i += n;
  }
  return urls;
}
// false when a word has no recording, or a recording does not play even asked twice: then nothing more is said
async function direLb(t, moi){
  const lex = await chargerLexiqueLb(), urls = enregistrementsLb(motsLb(t), lex);
  if (!urls || !urls.length) { (window.__lbMuet = window.__lbMuet || []).push(t); console.warn("luxembourgeois sans enregistrement :", t); return false; }
  const jouer = u => new Promise(fin => {
    const a = audioLb = new Audio(u);
    let fait = false; const fini = v => { if (!fait) { fait = true; fin(v); } };
    a.onended = () => fini(true); a.onpause = () => fini(true); a.onerror = () => fini(false); setTimeout(() => fini(true), 4000);
    a.play().catch(() => fini(false));
  });
  for (const u of urls) {
    if (moi !== generation) return true;
    if (!(await jouer(u)) && moi === generation && !(await jouer(u + "?essai=1")) && moi === generation) return false;
  }
  return true;
}
function taire(){
  generation++;
  if (audioLb) { audioLb.pause(); audioLb = null; }
  try { speechSynthesis.cancel(); } catch (e) {}
  try { if (window.AcademiaAndroid) AcademiaAndroid.taire(); } catch (e) {}
  signalerVoix(false);
}
// another app in front: the voice stops and the little sounds sleep (ton() wakes them up at the next sound)
document.addEventListener("visibilitychange", () => {
  if (!document.hidden) return;
  taire();
  try { if (audioCtx && audioCtx.state === "running") audioCtx.suspend(); } catch (e) {}
});
