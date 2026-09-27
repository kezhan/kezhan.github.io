/* L'Île aux Mots : jeu « repete ». */
GAMES.repete = function (theme) {
  const small = S.kid === "p4", total = small ? 5 : 8, res = [];
  const words = pick(THEMES[theme].words, total);
  const norm = x => x.toLowerCase().replace(/[^a-z ]/g, "").split(" ").map(t => t.replace(/s$/, "")).join(" ");
  startSession("repete", theme, total);
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const w = words[i]; let tries = 0, heard = [], rec = null, moved = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("div","order", wordFace(w)));
    const p = el("p","prompt", ""); p.textContent = kidCfg().showWord ? w.en : "Écoute, puis répète !";
    const info = el("small","", "Appuie sur le micro et dis le mot en anglais"); p.append(info); body.append(p);
    const row = el("div","row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => w.en, "Écoute"), bridgeBtn(w)); body.append(row);
    const mic = el("button","bigbtn chunky", "🎤 À toi !"); mic.style.alignSelf = "center"; mic.style.fontSize = "28px";
    const judge = el("div","judge");
    const okB = el("button","chunky", "✅ C'était bien"); okB.style.background = "#C9F2DF";
    const skipB = el("button","chunky", "⏭️ Mot suivant"); skipB.style.background = "#FFE3A3";
    judge.append(okB, skipB);
    body.append(mic, el("p","muted","Parent : si le micro comprend mal, validez vous-même."), judge);
    const next = async (ok, how) => {
      if (moved || !alive(gen)) return; moved = true;
      if (rec) try { rec.abort(); } catch(e) {}
      if (ok && tries === 0) addStar();
      logRound(w.en, ok && tries === 0, tries + 1, {heard, how});
      res.push(ok && tries === 0 ? 1 : 0); renderDots(res, total, -1);
      await say(ok ? PRAISE[rnd(PRAISE.length)] + " " + w.en + "!" : "Good try! " + w.en + "!");
      i++; round();
    };
    okB.onclick = () => { sfx.ok(); next(true, "parent"); };
    skipB.onclick = () => next(false, "skip");
    mic.onclick = () => {
      try { speechSynthesis.cancel(); } catch(e) {}
      rec = new SR(); rec.lang = "en-US"; rec.maxAlternatives = 5; rec.interimResults = false;
      mic.textContent = "👂 J'écoute…"; mic.disabled = true; G.taps++;
      rec.onresult = ev => {
        const alts = [...ev.results[0]].map(a => a.transcript);
        heard.push(alts[0] || "");
        if (alts.some(a => (" " + norm(a) + " ").includes(" " + norm(w.en) + " "))) { sfx.ok(); next(true, "mic"); return; }
        tries++; sfx.ko(); info.textContent = `J'ai entendu « ${alts[0] || "…"} ». Encore une fois !`;
        // young voices are hard for recognition: never let the child stay stuck
        if (tries >= (small ? 2 : 3)) next(false, "mic");
        else say("Try again! " + w.en);
      };
      rec.onerror = ev => {
        if (ev.error === "not-allowed" || ev.error === "service-not-allowed") toast("Micro refusé : autorisez-le dans le navigateur");
        else if (ev.error === "no-speech") info.textContent = "Je n'ai rien entendu. Parle plus fort !";
        else if (ev.error === "network") toast("La reconnaissance vocale a besoin d'internet");
      };
      rec.onend = () => { mic.textContent = "🎤 À toi !"; mic.disabled = false; };
      try { rec.start(); } catch(e) { mic.disabled = false; }
    };
    say(w.en);
  };
  round();
};
