/* L'Île aux Mots : langue apprise par chaque enfant (anglais, allemand, luxembourgeois, chinois ; pas de français), consignes, aide. */
const LANGS = {en:{flag:"🇬🇧", label:"English"}, zh:{flag:"🇨🇳", label:"中文"}, de:{flag:"🇩🇪", label:"Deutsch"}, lb:{flag:"🇱🇺", label:"Lëtzebuergesch"}};
// German accusative: der becomes den, and weak masculine nouns take -n or -en (den Löwen, den Elefanten)
const DE_WEAK = {"Löwe":"Löwen", "Affe":"Affen", "Elefant":"Elefanten", "Bär":"Bären", "Pilot":"Piloten", "Astronaut":"Astronauten", "Polizist":"Polizisten", "Bauer":"Bauern"};
const deAkk = w => w.replace(/^der (.+)$/, (m, n) => "den " + (DE_WEAK[n] || n));
const PHRASES = {
  en:{find:w => `Find the ${w}!`, findColor:w => `Find ${w}!`, thats:w => `That's the ${w}.`, thatsColor:w => `That's ${w}.`, pop:w => `Pop the ${w} balloon!`, praise:PRAISE},
  fr:{find:w => `Trouve : ${w} !`, findColor:w => `Trouve : ${w} !`, thats:w => `Ça, c'est : ${w}.`, thatsColor:w => `Ça, c'est : ${w}.`, pop:w => `Éclate le ballon ${w} !`, praise:["Bravo !","Super !","Génial !","Oui !","Bien joué !"]},
  zh:{find:w => `找到${w}！`, findColor:w => `找到${w}！`, thats:w => `这是${w}。`, thatsColor:w => `这是${w}。`, pop:w => `把${w}的气球戳破！`, praise:["太棒了！","真棒！","对了！","好厉害！"]},
  // German "zeigen" takes the accusative: der Hund → den Hund (the lexicon gives the nominative)
  de:{find:w => `Zeig mir ${deAkk(w)}!`, findColor:w => `Zeig mir ${w}!`, thats:w => `Das ist ${w}.`, thatsColor:w => `Das ist ${w}.`, pop:w => `Lass den Ballon platzen: ${w}!`, praise:["Super!","Toll!","Richtig!","Prima!","Sehr gut!"]},
  // Luxembourgish is heard through the lod.lu recording of the word inside the sentence
  lb:{find:w => `Weis mer ${w}!`, findColor:w => `Weis mer ${w}!`, thats:w => `Dat ass ${w}.`, thatsColor:w => `Dat ass ${w}.`, pop:w => `Looss de Ballon platzen: ${w}!`, praise:["Super!","Bravo!","Richteg!","Flott!","Ganz gutt!"]}
};
// no French for the children (Kezhan): a profile left on "fr" learns English
function langOf(kid){ const l = S.prof[kid || S.kid].lang; return LANGS[l] ? l : "en"; }
function T(w){ return w[langOf()] || w.en; }            // the word in the language being learnt
function sayT(text){ return say(text, langOf()); }
function phrase(){ return PHRASES[langOf()] || PHRASES.en; }
// words met at a game level: level 1 only easy words, 2 up to word level 2, 3 and 4 all (never fewer than 4)
function wordsOf(theme, lvl){
  const all = THEMES[theme].words, ws = all.filter(w => (w.lvl || 1) <= Math.min(3, lvl || 1));
  return ws.length >= 4 ? ws : all;
}
function setLang(kid, lang){ S.prof[kid].lang = lang; saveProfile(kid); showLangTag(); }
// the badge next to the title names the language the selected child is learning
function showLangTag(){ const t = $("langTag"); if (t) t.textContent = LANGS[langOf()].label + "!"; }
function praiseT(){ const p = phrase().praise; return p[rnd(p.length)]; }
// help language: the one the child knows, never the one being learnt (null = no help needed)
function bridgeLang(kid){ kid = kid || S.kid; const b = kidCfg(kid).bridge; if (!LANGS[b]) return null; return b !== langOf(kid) ? b : "en"; }

function bridgeBtn(w){
  // help in a language the child knows: 中文 for the big one (English if he learns Chinese);
  // the little one has no other language here, so 🐢 says the word again slowly
  const lang = bridgeLang(G.kid);
  if (!lang) {
    if (G.kid !== "p4") return el("span");
    const s = el("button","chip","🐢"); s.setAttribute("aria-label", "slow");
    s.onclick = e => { e.stopPropagation(); G.hints++; say(T(w), langOf(), 0.55); };
    return s;
  }
  const txt = w[lang];
  const b = el("button","chip", lang === "zh" ? "中文 ?" : "English?");
  b.onclick = e => { e.stopPropagation(); G.hints++; b.textContent = txt; say(txt, lang); };
  return b;
}

/* Luxembourgish: no device has a voice, so play the lod.lu recording (CC0) of the word found in the text */
let lbIndex = null, lbPlayer = null;
function lbAudio(text){
  if (!lbIndex) {
    lbIndex = Object.values(THEMES).flatMap(t => t.words).filter(w => w.lb && w.lod).map(w => [w.lb, w.lod.toLowerCase()]);
    lbIndex.sort((a, b) => b[0].length - a[0].length); // longest first: "d'Kanéngchen" before shorter words
  }
  // a whole word only: "gro" (grey) must not be heard inside "grouss"
  const letter = c => !!c && /\p{L}/u.test(c);
  const whole = word => { for (let i = text.indexOf(word); i >= 0; i = text.indexOf(word, i + 1)) if (!letter(text[i - 1]) && !letter(text[i + word.length])) return true; return false; };
  const hit = lbIndex.find(([word]) => whole(word));
  return hit ? `audio/lb/${hit[1]}.m4a` : null;
}
function playLb(text){
  return new Promise(res => {
    try { speechSynthesis.cancel(); } catch(e) {}
    if (lbPlayer) { lbPlayer.pause(); lbPlayer = null; }
    const src = lbAudio(text); if (!src) return res();
    const a = new Audio(src); lbPlayer = a;
    let done = false; const fin = () => { if (!done) { done = true; res(); } };
    a.onended = fin; a.onerror = fin; setTimeout(fin, 5000);
    a.play().catch(fin);
  });
}
