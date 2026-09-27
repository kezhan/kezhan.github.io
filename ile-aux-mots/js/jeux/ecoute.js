/* L'Île aux Mots : jeu « ecoute ».
   1: 3 pictures · 2: 4 pictures · 3: 6 pictures, other themes and sound-alike traps · 4: read the word, no voice */
const findPrompt = (w, theme) => theme === "colors" ? phrase().findColor(T(w)) : phrase().find(T(w));
// words that sound alike, to make level 3 listen carefully
const SOUNDALIKE = [["bear","pear"],["mouse","house"],["car","star"],["cake","snake"],["sheep","ship"],["tree","three"],["hat","cat"],["boat","goat"],["cow","owl"]];

GAMES.ecoute = function (theme) {
  const lvl = levelOf("ecoute"), n = [3, 4, 6, 6][lvl - 1], total = lvl === 1 ? 6 : 8;
  const words = THEMES[theme].words, targets = pick(words, Math.min(total, words.length)), res = [];
  const others = lvl >= 3 && theme !== "colors" ? Object.entries(THEMES).filter(([k]) => k !== theme && k !== "colors").flatMap(([, t]) => t.words) : [];
  const all = words.concat(others);
  const reading = lvl === 4;
  startSession("ecoute", theme, targets.length);
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= targets.length) return finish();
    const t = targets[i];
    const twin = lvl >= 3 && langOf() === "en" ? SOUNDALIKE.flatMap(p => p.includes(t.en) ? p.filter(x => x !== t.en) : []).map(x => all.find(w => w.en === x)).filter(Boolean) : [];
    const rest = pick(lvl >= 3 ? all.filter(w => w !== t && !twin.includes(w)) : words.filter(w => w !== t), n - 1 - twin.length);
    const opts = shuffle([t, ...twin, ...rest]);
    let tries = 0, locked = false;
    renderDots(res, targets.length, i);
    const body = $("gameBody"); body.innerHTML = "";
    if (reading) body.append(el("p", "prompt", `<span style="font-size:1.6em">${T(t)}</span><small>Lis le mot et touche la bonne image</small>`));
    else body.append(el("p", "prompt", "Écoute bien…<small>et touche la bonne image</small>"));
    const row = el("div", "row"); row.style.justifyContent = "center";
    if (reading) { const b = el("button", "chip", "🔊 Écoute"); b.onclick = () => { G.hints++; sayT(T(t)); }; row.append(b); }
    else row.append(speakBtn(() => findPrompt(t, theme), "Encore", langOf));
    if (lvl <= 3) row.append(bridgeBtn(t));
    body.append(row);
    const grid = el("div", "choices");
    opts.forEach(w => {
      const c = el("button", "choice chunky", `${wordFace(w)}<span class="w"></span>`);
      if (w === t) markOk(c);
      c.onclick = async () => {
        if (locked) return; G.taps++;
        if (w === t) {
          locked = true; c.classList.add("ok"); sfx.ok();
          if (S.kid === "p7" || tries) c.querySelector(".w").textContent = T(w);
          const first = tries === 0; if (first) addStar();
          logRound(t.en, first, tries + 1, {lvl}); res.push(first ? 1 : 0);
          renderDots(res, targets.length, -1);
          await sayT(praiseT() + " " + T(t) + "!");
          i++; loops.push(setTimeout(round, 350));
        } else {
          tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko");
          c.querySelector(".w").textContent = T(w);
          sayT((theme === "colors" ? phrase().thatsColor(T(w)) : phrase().thats(T(w))) + " " + findPrompt(t, theme));
          // the 4-year-old gets a nudge after two misses so the game never stalls
          if (tries >= 2 && S.kid === "p4") [...grid.children][opts.indexOf(t)].classList.add("bob");
        }
      };
      grid.appendChild(c);
    });
    body.append(grid);
    if (!reading) sayT(findPrompt(t, theme));
  };
  round();
};
