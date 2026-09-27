/* L'Île aux Mots : jeu « ballons ».
   1: 6 colours, slow · 2: all colours, faster · 3: colour and shape ("the green star") · 4: two orders to remember, faster still.
   Other languages than English keep colours only at levels 3-4 (no adjective agreement to get wrong), just faster. */
addStyle(`.balloon .forme{position:absolute; inset:0; display:grid; place-items:center; font-size:34px; pointer-events:none; filter:drop-shadow(0 1px 0 #fff)}`);
GAMES.ballons = function () {
  const lvl = levelOf("ballons"), en = langOf() === "en";
  const shapes = lvl >= 3 && en;
  const total = [6, 8, 8, 6][lvl - 1];
  const colors = wordsOf("colors", lvl).filter(w => w.en !== "white" || lvl >= 2).slice(0, lvl === 1 ? 6 : 13);
  const SHAPES = [["⭐","star"],["❤️","heart"],["🌙","moon"],["⚽","ball"]];
  const target = () => ({c: pick(colors, 1)[0], s: shapes ? pick(SHAPES, 1)[0] : null});
  // level 4: each round is a pair of targets, popped in order
  const rounds = [...Array(total)].map(() => lvl === 4 && en ? [target(), target()] : [target()]);
  const key = t => t.c.en + (t.s ? " " + t.s[1] : "");
  const said = t => t.s ? `the ${t.c.en} ${t.s[1]}` : T(t.c);
  const order = r => r.length === 2 ? `Pop ${said(r[0])}, then ${said(r[1])}!` : (shapes ? `Pop ${said(r[0])}!` : phrase().pop(T(r[0].c)));
  const speed = [9000, 7000, 6000, 5000][lvl - 1] * (S.kid === "p4" ? 1.25 : 1);
  const every = [1300, 950, 800, 700][lvl - 1];
  const res = []; let i = 0, step = 0, tries = 0, locked = false;
  startSession("ballons", "colors", total); const gen = GEN;
  const body = $("gameBody");
  const p = el("p", "prompt", ""); const row = el("div", "row"); row.style.justifyContent = "center";
  const sky = el("div", "sky");
  body.append(p, row, sky);
  const now = () => rounds[i][step];
  const ask = () => {
    const r = rounds[i];
    p.innerHTML = r.length === 2 ? `Deux ballons, dans l'ordre !<small>Écoute bien les deux</small>` : `Éclate le bon ballon !<small>Écoute bien</small>`;
    row.innerHTML = ""; row.append(speakBtn(() => order(r), "Encore", langOf), bridgeBtn(r[0].c));
    renderDots(res, total, i); sayT(order(r)); tries = 0; locked = false; step = 0;
    if (TEST) document.body.dataset.target = key(now());
  };
  const spawn = () => {
    if (i >= total) return;
    // the wanted balloon comes often enough; the others are random colours and shapes
    const t = Math.random() < 0.4 ? now() : target();
    const b = el("button", "balloon", t.s ? `<span class="forme">${t.s[0]}</span>` : "");
    b.style.background = t.c.e; b.setAttribute("aria-label", key(t) + " balloon");
    if (TEST) b.dataset.color = key(t);
    const W = sky.clientWidth, H = sky.clientHeight;
    b.style.left = Math.max(4, rnd(Math.max(1, W - 86))) + "px";
    const dur = speed * (matchMedia("(prefers-reduced-motion: reduce)").matches ? 1.6 : 1);
    const anim = b.animate([{transform: "translateY(0)"}, {transform: `translateY(-${H + 240}px)`}], {duration: dur, easing: "linear"});
    anim.onfinish = () => b.remove();
    b.onpointerdown = async ev => {
      ev.preventDefault(); if (locked) return; G.taps++;
      const want = now();
      if (key(t) === key(want)) {
        anim.pause(); b.classList.add("pop"); sfx.pop(); setTimeout(() => b.remove(), 260);
        if (step === 0 && rounds[i].length === 2) { step = 1; if (TEST) document.body.dataset.target = key(now()); sayT(praiseT()); return; }
        locked = true;
        const first = tries === 0; if (first) addStar();
        logRound(rounds[i].map(key).join(" > "), first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
        await sayT(praiseT() + " " + (want.s ? `${want.c.en} ${want.s[1]}` : T(want.c)) + "!");
        if (!alive(gen)) return;
        i++; if (i >= total) return finish(); ask();
      } else {
        tries++; sfx.ko(); b.classList.remove("wrong"); void b.offsetWidth; b.classList.add("wrong");
        sayT(t.s ? `That's the ${t.c.en} ${t.s[1]}!` : phrase().thatsColor(T(t.c)));
      }
    };
    sky.appendChild(b);
  };
  ask(); spawn();
  loops.push(setInterval(spawn, every));
};
