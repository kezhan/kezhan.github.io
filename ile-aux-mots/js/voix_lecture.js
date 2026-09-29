/* L'Île aux Mots : l'anglais, l'allemand et le chinois joués depuis des enregistrements Azure faits à l'avance
   (audio/<langue>/<id>.webm, outils/generer_voix.py) ; js/voix.js dit lesquels existent, sans requête.
   Kezhan : « la voix n'est pas du tout fluide !!! ». Une phrase sans fichier garde la voix du navigateur.
   Les calculs de voixId et voixPieces sont faits à l'identique par outils/voix_commun.py. */
const voixNorm = t => String(t).replace(/\s+/g, " ").trim();
// the sentences of a text, without those that have neither letter nor digit (an emoji alone)
const voixPieces = t => (voixNorm(t).match(/[^.!?。！？]+(?:[.!?。！？]+|$)/g) || []).map(s => s.trim()).filter(s => /[\p{L}\p{N}]/u.test(s));
function voixId(t){
  const s = voixNorm(t); let h1 = 0xdeadbeef, h2 = 0x41c6ce57;
  for (let i = 0; i < s.length; i++) { const ch = s.charCodeAt(i); h1 = Math.imul(h1 ^ ch, 2654435761); h2 = Math.imul(h2 ^ ch, 1597334677); }
  h1 = Math.imul(h1 ^ (h1 >>> 16), 2246822507); h1 ^= Math.imul(h2 ^ (h2 >>> 13), 3266489909);
  h2 = Math.imul(h2 ^ (h2 >>> 16), 2246822507); h2 ^= Math.imul(h1 ^ (h1 >>> 13), 3266489909);
  return ((4294967296 * (2097151 & h2) + (h1 >>> 0)) % 1099511627776).toString(36).padStart(8, "0");
}
const VOIX_SETS = {}, VOIX_BAD = new Set(); // VOIX_BAD: files that could not be played here (missing online, format refused)
function voixHas(lang, id){
  if (typeof VOIX === "undefined" || !VOIX[lang] || VOIX_BAD.has(lang + id)) return false;
  if (!VOIX_SETS[lang]) { const s = VOIX[lang], set = new Set(); for (let i = 0; i < s.length; i += 8) set.add(s.slice(i, i + 8)); VOIX_SETS[lang] = set; }
  return VOIX_SETS[lang].has(id);
}
// the files of a line: its own, or one per sentence ("cat!" may use the file of "cat"); null if one is missing
function voixFiles(text, lang){
  const own = voixId(text); if (voixHas(lang, own)) return [own];
  const ids = voixPieces(text).map(p => [voixId(p), voixId(p.replace(/[.!?。！？ ]+$/, ""))].find(id => voixHas(lang, id)));
  return ids.length && ids.every(Boolean) ? ids : null;
}

// one player for every line, unlocked by the first tap (as js/son.js does for the other sounds): lines said later,
// after an animation or a wait, still play on a tablet
const VOIX_SILENCE = "data:audio/mpeg;base64,//NIxAAAAANIAAAAAExBTUVVVVVMQU1FMy4xMDBVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV"; // 26 ms of silence
let voixEl = null, voixTok = 0, voixEnd = null;
const voixPlayer = () => voixEl = voixEl || new Audio();
function voixUnlock(){
  ["pointerdown", "touchstart", "keydown"].forEach(ev => removeEventListener(ev, voixUnlock, true));
  const a = voixPlayer(); if (a.src) return;
  a.muted = true; a.src = VOIX_SILENCE;
  a.play().then(() => { if (a.src === VOIX_SILENCE) a.pause(); }, () => {});
}
["pointerdown", "touchstart", "keydown"].forEach(ev => addEventListener(ev, voixUnlock, {capture: true, passive: true}));

const voixCancelSpeech = "speechSynthesis" in window ? speechSynthesis.cancel.bind(speechSynthesis) : () => {};
// one line at a time: the one playing stops, and its say() ends as a cancelled spoken line would
function voixStop(){
  voixTok++;
  if (voixEl && voixEl.src !== VOIX_SILENCE) voixEl.pause();
  if (voixEnd) { const e = voixEnd; voixEnd = null; e(); }
}
// every speechSynthesis.cancel() of the site (leaving a game, the microphone, lod.lu words) stops the recording too
if ("speechSynthesis" in window) try { speechSynthesis.cancel = () => { voixStop(); voixCancelSpeech(); }; } catch(e) {}

/* Called first by say(). Returns false when the line has no recording: say() goes on with the browser voice.
   Otherwise plays it and calls done at the end. If nothing could be played, say() speaks the line with the
   browser voice; a file that fails to load (missing online, format refused) is not tried again. */
let voixSkip = false; // the next say() goes straight to the browser voice
function playVoix(text, lang, rate, done){
  if (TEST) { (S.said = S.said || []).push({lang, text}); return false; } // read by outils/relever_phrases.py
  if (voixSkip) { voixSkip = false; return false; }
  const ids = voixFiles(text, lang); if (!ids) return false;
  voixStop(); voixCancelSpeech();
  if (typeof lbPlayer !== "undefined" && lbPlayer) { lbPlayer.pause(); lbPlayer = null; }
  const tok = voixTok, urls = ids.map(id => `audio/${lang}/${id}.webm`), speed = fileRate(rate || voiceRate());
  urls.slice(1).forEach(u => fetch(u).catch(() => {})); // the next sentences are fetched while the first one plays
  const a = voixPlayer();
  let started = false, timer = null;
  const end = () => { clearTimeout(timer); if (voixEnd === end) voixEnd = null; done(); };
  const giveUp = () => { clearTimeout(timer); voixTok++; if (voixEnd === end) voixEnd = null; voixSkip = true; say(text, lang, rate).then(done); };
  voixEnd = end;
  const next = k => {
    if (tok !== voixTok) return;
    if (k >= urls.length) return end();
    a.muted = false; a.src = urls[k]; a.defaultPlaybackRate = speed; a.playbackRate = speed; a.preservesPitch = true;
    const fail = () => { if (tok === voixTok) { if (started) next(k + 1); else giveUp(); } };
    // safety net for a lost "ended": the length of the file once known, 10 s before
    clearTimeout(timer); timer = setTimeout(fail, 10000);
    const broken = () => { VOIX_BAD.add(lang + ids[k]); fail(); };
    a.onloadedmetadata = () => { if (tok !== voixTok || !isFinite(a.duration)) return; clearTimeout(timer); timer = setTimeout(() => tok === voixTok && next(k + 1), a.duration / speed * 1000 + 1500); };
    a.onended = () => next(k + 1);
    a.onerror = broken;
    a.play().then(() => { started = true; }, fail);
  };
  next(0);
  return true;
}
