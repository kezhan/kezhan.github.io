/* L'Île aux Mots : déblocage du son au premier toucher, et bouton de test du son dans le coin des parents.
   Android Chrome only plays sound after a tap: the first tap wakes the sound-effect engine and the voice,
   so that later sounds (after a delay, an animation, a voice line) are not silent. */
(() => {
  let done = false;
  const unlock = () => {
    if (done) return; done = true;
    try { // the sound effects: create or resume the audio context during the tap, with a silent blip
      ac = ac || new (window.AudioContext || window.webkitAudioContext)();
      if (ac.state === "suspended") ac.resume();
      const o = ac.createOscillator(), g = ac.createGain(); g.gain.value = 0.0001;
      o.connect(g); g.connect(ac.destination); o.start(); o.stop(ac.currentTime + 0.05);
    } catch(e) {}
    try { // the voice: an empty utterance during the tap unlocks later speech on Android
      if ("speechSynthesis" in window) {
        speechSynthesis.resume();
        const u = new SpeechSynthesisUtterance(" "); u.volume = 0; speechSynthesis.speak(u);
      }
    } catch(e) {}
    try { const a = new Audio(); a.muted = true; a.play().catch(() => {}); } catch(e) {} // the lod.lu recordings
  };
  ["pointerdown", "touchstart", "click", "keydown"].forEach(ev => addEventListener(ev, unlock, {capture: true, passive: true}));
  // Android sometimes leaves the voice paused after the tablet sleeps or the app comes back
  document.addEventListener("visibilitychange", () => { if (!document.hidden) try { speechSynthesis.resume(); } catch(e) {} });
})();

// parents' corner: 🔊 plays a sound, speaks in the language being learnt and lists the voices of this device
function soundTest(out){
  const lines = [];
  try { tone([523, 659, 784], .15); lines.push("🎵 " + (ac ? ac.state : "no audio")); } catch(e) { lines.push("🎵 error"); }
  const has = "speechSynthesis" in window;
  if (has) loadVoices();
  ["en", "de", "zh"].forEach(l => { const v = has ? voiceFor(l) : null; lines.push(`${LANGS[l].flag} ${v ? v.name : "—"}`); });
  lines.push(`🇱🇺 lod.lu`);
  lines.push(`🗣️ ${has ? voices.length : 0}`);
  out.textContent = lines.join(" · ");
  say(T(THEMES.animals.words[0]));
}
function addSoundTest(){
  const box = $("parent"); if (!box || $("soundTest")) return;
  const row = el("div", "row"); row.id = "soundTest";
  const b = el("button", "chip", "🔊 ✓"); b.setAttribute("aria-label", "sound test");
  const out = el("span", "muted", "");
  b.onclick = () => soundTest(out);
  row.append(b, out);
  const first = box.querySelector("section");
  if (first) first.parentNode.insertBefore(row, first); else box.append(row);
}
addEventListener("load", addSoundTest);
