/* L'Île aux Mots : jeu « memory ».
   1: 3 pairs of identical pictures · 2: 6 pairs picture and written word · 3: 8 pairs, mixed themes
   4: 8 pairs word and its translation in the help language, no picture, no voice */
// cards turn over in 3D (the hidden side is the blue back)
addStyle(`.grid-cards .card{transition:transform .38s cubic-bezier(.3,1.4,.5,1), background .2s; transform-style:preserve-3d}
.grid-cards .card.back{transform:rotateY(180deg)}
.grid-cards .card.done{animation:paire .5s ease-out}
@keyframes paire{40%{transform:scale(1.15) rotate(-4deg)} 70%{transform:scale(.95)}}
@media (prefers-reduced-motion:reduce){.grid-cards .card{transition:none}}`);
GAMES.memory = function (theme) {
  const lvl = levelOf("memory"), want = [3, 6, 8, 8][lvl - 1];
  const own = wordsOf(theme, lvl);
  const others = Object.entries(THEMES).filter(([k]) => k !== theme && k !== "colors").flatMap(([k]) => wordsOf(k, lvl));
  // levels 3-4 mix 4 words of the theme with words from the other themes (colours stay alone)
  const words = lvl >= 3 && theme !== "colors" ? [...pick(own, 4), ...pick(others, want - 4)] : pick(own, Math.min(want, own.length));
  const pairs = words.length;
  const help = bridgeLang();
  const twoWords = lvl === 4 && help;
  startSession("memory", theme, pairs);
  const face = (c) => c.kind === "pic" ? wordFace(c.w)
    : `<span class="w" style="font-size:22px">${c.kind === "help" ? c.w[help] : T(c.w)}</span>`;
  const cards = shuffle(words.flatMap(w => [
    {w, kind: twoWords ? "help" : "pic"},
    {w, kind: lvl === 1 ? "pic" : "word"}
  ]));
  const body = $("gameBody");
  body.append(el("p", "prompt", lvl === 1 ? "Trouve les deux pareils !" : twoWords ? "Relie le mot et sa traduction !" : "Trouve l'image et son mot !"));
  const grid = el("div", "grid-cards"); body.append(grid);
  let open = [], found = 0, busy = false, misses = 0;
  const res = [];
  renderDots(res, pairs, -1);
  cards.forEach(c => {
    const b = el("button", "card chunky back", face(c));
    if (TEST) b.dataset.pair = c.w.en; // the recette finds pairs without guessing
    b.onclick = () => {
      if (busy || !b.classList.contains("back")) return;
      G.taps++; b.classList.remove("back");
      if (!twoWords) sayT(T(c.w)); // level 4 is about reading
      open.push({b, c});
      if (open.length < 2) return;
      const [x, y] = open; open = [];
      if (x.c.w === y.c.w) {
        x.b.classList.add("done"); y.b.classList.add("done"); sfx.ok(); addStar(); found++;
        // a pair found with at most one miss since the last one counts as right first time
        logRound(x.c.w.en, misses <= 1, misses + 1, {lvl}); misses = 0;
        res.push(1); renderDots(res, pairs, -1);
        if (twoWords) sayT(T(x.c.w));
        if (found === pairs) loops.push(setTimeout(finish, 900));
      } else { misses++; busy = true; loops.push(setTimeout(() => { x.b.classList.add("back"); y.b.classList.add("back"); busy = false; }, 1100)); }
    };
    grid.append(b);
  });
};
