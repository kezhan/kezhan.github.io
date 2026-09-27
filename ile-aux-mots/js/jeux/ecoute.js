/* L'Île aux Mots : jeu « ecoute ». */
const findPrompt = (w, theme) => theme === "colors" ? `Find ${w.en}!` : `Find the ${w.en}!`;

GAMES.ecoute = function (theme) {
  const k = kidCfg(), n = k.choices, total = S.kid === "p4" ? 6 : 8;
  const words = THEMES[theme].words, targets = pick(words, total), res = [];
  startSession("ecoute", theme, total);
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const t = targets[i], opts = shuffle([t, ...pick(words.filter(w => w !== t), n-1)]);
    let tries = 0, locked = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("p","prompt","Écoute bien…<small>et touche la bonne image</small>"));
    const row = el("div","row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => findPrompt(t, theme), "Encore"), bridgeBtn(t)); body.append(row);
    const grid = el("div","choices");
    opts.forEach(w => {
      const c = el("button","choice chunky", `${wordFace(w)}<span class="w"></span>`);
      c.onclick = async () => {
        if (locked) return; G.taps++;
        if (w === t) {
          locked = true; c.classList.add("ok"); sfx.ok();
          if (k.showWord || tries) c.querySelector(".w").textContent = w.en;
          const first = tries === 0; if (first) addStar();
          logRound(t.en, first, tries + 1); res.push(first ? 1 : 0);
          renderDots(res, total, -1);
          await say(PRAISE[rnd(PRAISE.length)] + " " + (theme === "colors" ? t.en : "The " + t.en + "!"));
          i++; loops.push(setTimeout(round, 350));
        } else {
          tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko");
          c.querySelector(".w").textContent = w.en;
          say(`That's ${theme === "colors" ? "" : "the "}${w.en}. ${findPrompt(t, theme)}`);
          // the 4-year-old gets a nudge after two misses so the game never stalls
          if (tries >= 2 && S.kid === "p4") [...grid.children][opts.indexOf(t)].classList.add("bob");
        }
      };
      grid.appendChild(c);
    });
    body.append(grid);
    say(findPrompt(t, theme));
  };
  round();
};
