/* L'Île aux Mots : jeu « compte ». */
GAMES.compte = function () {
  const small = S.kid === "p4", total = small ? 5 : 8, max = small ? 5 : 10;
  const res = []; let i = 0;
  const qs = [...Array(total)].map(() => ({n:1 + rnd(max), it:COUNT_ITEMS[rnd(COUNT_ITEMS.length)]}));
  startSession("compte", null, total); const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const q = qs[i], word = {en:NUMS[q.n-1], fr:NUMS_FR[q.n-1], zh:NUMS_ZH[q.n-1]};
    const plural = q.n === 1 ? q.it[1].replace(/ies$/, "y").replace(/s$/, "") : q.it[1];
    let tries = 0, locked = false, counted = 0;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("p","prompt",`How many ${q.it[1]}?<small>Touche-les pour compter en anglais</small>`));
    const pile = el("div","pile");
    for (let j=0;j<q.n;j++){ const s = el("button","", q.it[0]); s.style.fontSize = "inherit"; s.onclick = () => { if (s.dataset.c) return; s.dataset.c = 1; s.style.opacity = ".45"; G.taps++; say(NUMS[counted++]); }; pile.append(s); }
    body.append(pile);
    const row = el("div","row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => `How many ${q.it[1]}?`, "Encore")); body.append(row);
    const opts = shuffle([q.n, ...pick([...Array(max)].map((_,j) => j+1).filter(x => x !== q.n), small ? 2 : 3)]);
    const nums = el("div","nums");
    opts.forEach(v => {
      const b = el("button","num chunky", String(v));
      b.onclick = async () => {
        if (locked) return;
        if (v === q.n) {
          locked = true; b.classList.add("ok"); sfx.ok();
          const first = tries === 0; if (first) addStar();
          logRound(word.en, first, tries + 1); res.push(first ? 1 : 0); renderDots(res, total, -1);
          await say(`Yes! ${word.en} ${plural}!`); i++; loops.push(setTimeout(round, 300));
        } else { tries++; sfx.ko(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko"); say(`${NUMS[v-1]}? Let's count!`); }
      };
      nums.append(b);
    });
    body.append(nums);
    say(`How many ${q.it[1]}?`);
  };
  round();
};
