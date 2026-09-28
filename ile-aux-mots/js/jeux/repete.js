/* L'Île aux Mots : jeu « repete ».
   Everything, the words to the parent included, is in the language being learnt (Kezhan: the whole site in the chosen language).
   Speech recognition in that language (en-GB, de-DE, zh-CN); Luxembourgish has none, so the parent judges. */
const REPETE_SR = {en:"en-GB", de:"de-DE", zh:"zh-CN"};
const REPETE_TXT = {
  en:{mic:"Tap the microphone and say the word", aloud:"Say the word out loud! Grown-up: tap ✅ if it was right",
    hear:"Listen", go:"🎤 Your turn!", listening:"👂 I'm listening…", ok:"✅ That was right", skip:"⏭️ Next word",
    parent:"Grown-up: if the microphone gets it wrong, tap ✅ yourself.", heard:x => `I heard “${x}”. Once more!`, nothing:"I didn't hear anything. Speak louder!",
    denied:"Microphone blocked: allow it in the browser", offline:"Speech recognition needs the internet"},
  de:{mic:"Drück auf das Mikrofon und sag das Wort", aloud:"Sag das Wort laut! Eltern: Tippt auf ✅, wenn es richtig war",
    hear:"Hören", go:"🎤 Du bist dran!", listening:"👂 Ich höre zu…", ok:"✅ Das war richtig", skip:"⏭️ Nächstes Wort",
    parent:"Eltern: Wenn das Mikrofon falsch versteht, tippt selbst auf ✅.", heard:x => `Ich habe „${x}“ gehört. Noch einmal!`, nothing:"Ich habe nichts gehört. Sprich lauter!",
    denied:"Mikrofon blockiert: Bitte im Browser erlauben", offline:"Die Spracherkennung braucht Internet"},
  lb:{mic:"Dréck op de Mikro a so d'Wuert", aloud:"So d'Wuert haart! Elteren: tippt op ✅, wann et richteg war",
    hear:"Lauschteren", go:"🎤 Du bass drun!", listening:"👂 Ech lauschteren…", ok:"✅ Dat war richteg", skip:"⏭️ D'nächst Wuert",
    parent:"Elteren: wann de Mikro falsch versteet, tippt selwer op ✅.", heard:x => `Ech hunn „${x}“ héieren. Nach eng Kéier!`, nothing:"Ech hunn näischt héieren. Schwätz méi haart!",
    denied:"De Mikro ass blockéiert: erlaabt en am Browser", offline:"De Mikro brauch Internet"},
  zh:{mic:"按一下话筒，说出这个词", aloud:"大声说出这个词！家长：说对了就点 ✅",
    hear:"听一听", go:"🎤 轮到你了！", listening:"👂 我在听……", ok:"✅ 说对了", skip:"⏭️ 下一个词",
    parent:"家长：如果话筒没听懂，请自己点 ✅。", heard:x => `我听到的是“${x}”。再说一次！`, nothing:"我什么也没听到。大声一点！",
    denied:"话筒被拒绝了：请在浏览器里允许使用", offline:"语音识别需要联网"}
};
GAMES.repete = function (theme) {
  const small = S.kid === "p4", total = small ? 5 : 8, res = [];
  const lvl = levelOf("repete");
  const tx = k => (REPETE_TXT[langOf()] || REPETE_TXT.en)[k];
  const micOn = () => MIC_OK && !!REPETE_SR[langOf()]; // no recognition in Luxembourgish: the parent judges
  let words = pick(wordsOf(theme, lvl).filter(w => lvl < 3 || !/s$/.test(w.en) || w.en === "bus"), total);
  // English levels 3-4: word groups ("an orange cat"), then short sentences ("I can see a red bus.")
  if (langOf() === "en" && lvl >= 3 && theme !== "colors" && theme !== "actions") {
    const cols = wordsOf("colors", 2);
    const a = x => (/^[aeiou]/.test(x) ? "an " : "a ") + x;
    words = words.map(w => {
      const c = pick(cols, 1)[0].en, group = a(`${c} ${w.en}`);
      const text = lvl === 3 ? group : pick([`I can see ${group}.`, `The ${w.en} is ${c}.`, `Where is the ${c} ${w.en}?`], 1)[0];
      return {...w, en: text, base: w};
    });
  }
  const norm = x => x.toLowerCase().replace(/[^a-z ]/g, "").split(" ").map(t => t.replace(/s$/, "")).join(" ");
  const clean = x => x.toLowerCase().replace(/[\s.,!?;:。！？，、]/g, "");
  // German: the child may say "Katze" for "die Katze"; the noun alone is enough
  const bare = x => x.replace(/^(der|die|das) /i, "");
  startSession("repete", theme, total);
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const w = words[i], withMic = micOn(); let tries = 0, heard = [], rec = null, moved = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("div","order", wordFace(w)));
    // the word is always written, for both children (Kezhan: a chance to read), and said by the voice
    const p = el("p","prompt", ""); p.textContent = "🗣️ " + T(w);
    // without speech recognition (e.g. the Android tablet, or Luxembourgish), the child says it out loud and the parent judges
    const info = el("small","", withMic ? tx("mic") : tx("aloud"));
    p.append(info); body.append(p);
    const row = el("div","row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => T(w), tx("hear"), langOf), bridgeBtn(w.base || w)); body.append(row);
    const mic = el("button","bigbtn chunky", tx("go")); mic.style.alignSelf = "center"; mic.style.fontSize = "28px";
    const judge = el("div","judge");
    const okB = el("button","chunky", tx("ok")); okB.style.background = "#C9F2DF";
    const skipB = el("button","chunky", tx("skip")); skipB.style.background = "#FFE3A3";
    judge.append(okB, skipB);
    if (withMic) body.append(mic, el("p","muted", tx("parent")));
    body.append(judge);
    const next = async (ok, how) => {
      if (moved || !alive(gen)) return; moved = true;
      if (rec) try { rec.abort(); } catch(e) {}
      if (ok && tries === 0) addStar();
      logRound(w.en, ok && tries === 0, tries + 1, {heard, how, mic: withMic});
      res.push(ok && tries === 0 ? 1 : 0); renderDots(res, total, -1);
      await sayT(ok ? praiseT() + " " + T(w) + "!" : T(w));
      i++; round();
    };
    okB.onclick = () => { sfx.ok(); next(true, "parent"); };
    skipB.onclick = () => next(false, "skip");
    mic.onclick = () => {
      try { speechSynthesis.cancel(); } catch(e) {}
      const L = langOf(); if (!REPETE_SR[L]) return;
      rec = new SR(); rec.lang = REPETE_SR[L]; rec.maxAlternatives = 5; rec.interimResults = false;
      mic.textContent = tx("listening"); mic.disabled = true; G.taps++;
      rec.onresult = ev => {
        const alts = [...ev.results[0]].map(a => a.transcript);
        heard.push(alts[0] || "");
        // English: whole words, plural tolerated; other languages: the word anywhere in what was heard
        const hit = L === "en" ? alts.some(a => (" " + norm(a) + " ").includes(" " + norm(w.en) + " "))
          : alts.some(a => clean(a).includes(clean(bare(T(w)))));
        if (hit) { sfx.ok(); next(true, "mic"); return; }
        tries++; sfx.ko(); info.textContent = tx("heard")(alts[0] || "…");
        // young voices are hard for recognition: never let the child stay stuck
        if (tries >= (small ? 2 : 3)) next(false, "mic");
        else sayT(T(w));
      };
      rec.onerror = ev => {
        if (ev.error === "not-allowed" || ev.error === "service-not-allowed") toast(tx("denied"));
        else if (ev.error === "no-speech") info.textContent = tx("nothing");
        else if (ev.error === "network") toast(tx("offline"));
      };
      rec.onend = () => { mic.textContent = tx("go"); mic.disabled = false; };
      try { rec.start(); } catch(e) { mic.disabled = false; }
    };
    sayT(T(w));
  };
  round();
};
