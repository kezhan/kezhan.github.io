/* L'Île aux Mots : jeu « memory ». */
GAMES.memory = function (theme) {
  const small = S.kid === "p4", pairs = small ? 3 : 6;
  const words = pick(THEMES[theme].words, pairs);
  startSession("memory", theme, pairs);
  // 4 ans: two identical pictures; 7 ans: a picture and its written English word
  const cards = shuffle(words.flatMap(w => [{w, kind:"pic"}, {w, kind: small ? "pic" : "word"}]));
  const body = $("gameBody");
  body.append(el("p","prompt", small ? "Trouve les deux pareils !" : "Trouve l'image et son mot !"));
  const grid = el("div","grid-cards"); body.append(grid);
  let open = [], found = 0, busy = false;
  const res = [];
  renderDots(res, pairs, -1);
  cards.forEach(c => {
    const b = el("button","card chunky back", c.kind === "pic" ? `${wordFace(c.w)}` : `<span class="w" style="font-size:22px">${c.w.en}</span>`);
    b.onclick = () => {
      if (busy || !b.classList.contains("back")) return;
      G.taps++; b.classList.remove("back"); say(c.w.en); open.push({b, c});
      if (open.length < 2) return;
      const [x, y] = open; open = [];
      if (x.c.w === y.c.w) {
        x.b.classList.add("done"); y.b.classList.add("done"); sfx.ok(); addStar(); found++;
        res.push(1); renderDots(res, pairs, -1);
        if (found === pairs) loops.push(setTimeout(finish, 900));
      } else { busy = true; loops.push(setTimeout(() => { x.b.classList.add("back"); y.b.classList.add("back"); busy = false; }, 1100)); }
    };
    grid.append(b);
  });
};
