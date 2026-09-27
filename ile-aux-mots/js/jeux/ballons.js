/* L'Île aux Mots : jeu « ballons ». */
GAMES.ballons = function () {
  const small = S.kid === "p4", total = small ? 6 : 8;
  const pool = THEMES.colors.words.slice(0, small ? 6 : 10);
  const targets = [...Array(total)].map((_, j) => pool[(j + rnd(pool.length)) % pool.length]);
  const res = []; let i = 0, tries = 0, locked = false;
  startSession("ballons", "colors", total); const gen = GEN;
  const body = $("gameBody");
  const p = el("p","prompt",""); const row = el("div","row"); row.style.justifyContent = "center";
  const sky = el("div","sky");
  body.append(p, row, sky);
  const ask = () => { const t = targets[i]; p.innerHTML = `Éclate le bon ballon !<small>Écoute la couleur</small>`; row.innerHTML = ""; row.append(speakBtn(() => phrase().pop(T(t)), "Encore", langOf()), bridgeBtn(t)); renderDots(res, total, i); sayT(phrase().pop(T(t))); tries = 0; locked = false; if (TEST) document.body.dataset.target = t.en; };
  const spawn = () => {
    if (i >= total) return;
    const t = targets[i], w = Math.random() < 0.4 ? t : pool[rnd(pool.length)];
    const b = el("button","balloon"); b.style.background = w.e; b.setAttribute("aria-label", w.en + " balloon");
    if (TEST) b.dataset.color = w.en;
    const W = sky.clientWidth, H = sky.clientHeight;
    b.style.left = Math.max(4, rnd(Math.max(1, W - 86))) + "px";
    const dur = (small ? 9000 : 7000) * (matchMedia("(prefers-reduced-motion: reduce)").matches ? 1.6 : 1);
    const anim = b.animate([{transform:"translateY(0)"},{transform:`translateY(-${H + 240}px)`}], {duration:dur, easing:"linear"});
    anim.onfinish = () => b.remove();
    b.onpointerdown = async ev => {
      ev.preventDefault(); if (locked) return; G.taps++;
      const now = targets[i];
      if (w.en === now.en) {
        locked = true; anim.pause(); b.classList.add("pop"); sfx.pop(); setTimeout(() => b.remove(), 260);
        const first = tries === 0; if (first) addStar();
        logRound(now.en, first, tries + 1); res.push(first ? 1 : 0); renderDots(res, total, -1);
        await sayT(praiseT() + " " + T(now) + "!");
        if (!alive(gen)) return;
        i++; if (i >= total) return finish(); ask();
      } else {
        tries++; sfx.ko(); b.classList.remove("wrong"); void b.offsetWidth; b.classList.add("wrong");
        sayT(phrase().thatsColor(T(w)));
      }
    };
    sky.appendChild(b);
  };
  ask(); spawn();
  loops.push(setInterval(spawn, small ? 1300 : 950));
};
