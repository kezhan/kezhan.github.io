/* Académia : la voix lit les questions et les explications (la petite ne lit pas encore).
   Voix du navigateur ; le luxembourgeois n'en a pas : le texte reste écrit (voix enregistrées : ticket voix). */
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
let voixCoupee = false;
function dire(texte, lang = "fr"){
  const t = sansEmoji(texte);
  return new Promise(fin => {
    if (TEST) { window.__dit = t; return fin(); }
    if (!t || voixCoupee || !("speechSynthesis" in window) || lang === "lb" && !voixPour("lb")) return fin();
    try {
      speechSynthesis.cancel();
      const u = new SpeechSynthesisUtterance(t), v = voixPour(lang);
      u.lang = LOCALES[lang] || "fr-FR"; if (v) u.voice = v;
      const p = P(); u.rate = p && p.age <= 5 ? .82 : .92;   // a little slower for the little ones
      let fait = false; const ok = () => { if (!fait) { fait = true; fin(); } };
      u.onend = ok; u.onerror = ok; setTimeout(ok, 2500 + t.length * 90);
      setTimeout(() => { try { speechSynthesis.resume(); speechSynthesis.speak(u); } catch (e) { ok(); } }, 60);
    } catch (e) { fin(); }
  });
}
function taire(){ try { speechSynthesis.cancel(); } catch (e) {} }
