/* Académia : la voix lit les questions et les explications (la petite ne lit pas encore). Voix du navigateur ;
   le luxembourgeois n'en a pas : chaque mot est joué depuis son enregistrement de lod.lu (donnees/lexique_lb.json,
   données CC0), et la voix allemande prend le relais pour un mot qui n'en aurait pas. */
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
function dire(texte, lang = "fr"){
  const t = sansEmoji(texte), moi = ++generation;
  if (TEST) { window.__dit = t; return Promise.resolve(); }
  if (audioLb) { audioLb.pause(); audioLb = null; }
  if (!t || voixCoupee) return Promise.resolve();
  if (lang === "lb" && !voixPour("lb")) return direLb(t, moi).then(ok => ok || moi !== generation ? undefined : parler(t, "de"));
  return parler(t, lang);
}
function parler(t, lang){
  return new Promise(fin => {
    if (!("speechSynthesis" in window)) return fin();
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(t), v = voixPour(lang);
      u.lang = v ? v.lang : (LOCALES[lang] || "fr-FR"); if (v) u.voice = v;
      const p = P(); u.rate = p && p.age <= 5 ? .82 : .92;   // a little slower for the little ones
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      u.onend = ok; u.onerror = ok; setTimeout(ok, 2500 + t.length * 90);
      setTimeout(() => { try { speechSynthesis.resume(); speechSynthesis.speak(u); } catch (e) { ok(); } }, 60);
    } catch (e) { fin(); }
  });
}

// Luxembourgish: the recordings of the words, one after the other ("d'Kaz": the article, then the word)
let lexiqueLb = null;
async function chargerLexiqueLb(){
  if (!lexiqueLb) { try { lexiqueLb = (await (await fetch("donnees/lexique_lb.json")).json()).mots || {}; } catch (e) { lexiqueLb = {}; } }
  return lexiqueLb;
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
async function direLb(t, moi){
  const lex = await chargerLexiqueLb(), urls = motsLb(t).map(m => lex[m] && lex[m].audio);
  if (!urls.length || urls.some(u => !u)) return false;
  for (const u of urls) {
    if (moi !== generation) return true;
    await new Promise(fin => {
      const a = audioLb = new Audio(u);
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      a.onended = ok; a.onerror = ok; a.onpause = ok; setTimeout(ok, 4000);
      a.play().catch(ok);
    });
  }
  return true;
}
function taire(){
  generation++;
  if (audioLb) { audioLb.pause(); audioLb = null; }
  try { speechSynthesis.cancel(); } catch (e) {}
}
