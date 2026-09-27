/* L'Île aux Mots : moteur commun des jeux « écoute et touche ».
   rounds: [{say, show?, visual?, word, praise?, lang?, choices:[{html, ok?, label?, sayWrong?, small?}]}]
   lang: the voice of that round (default English); a multilingual game passes langOf() */
addStyle(`.choice.txt{aspect-ratio:auto; min-height:96px; font-size:24px; font-family:var(--display); font-weight:600; padding:12px}`);
function runQuiz(act, theme, rounds){
  const total = rounds.length, res = []; let i = 0;
  startSession(act, theme, total); const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const r = rounds[i], L = r.lang || "en"; let tries = 0, locked = false;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("p", "prompt", r.show || "👂 👉" /* no French for the children: pictograms when the game gives no text */));
    if (r.visual) body.append(el("div", "order", r.visual));
    const row = el("div", "row"); row.style.justifyContent = "center";
    if (r.say) row.append(speakBtn(() => r.say, "", L));
    // reading rounds: silent at first, the voice only on request (counted as a hint)
    if (r.listen) { const b = el("button", "chip", "🔊"); b.onclick = () => { G.hints++; say(r.listen, L); }; row.append(b); }
    body.append(row);
    const grid = el("div", "choices");
    shuffle(r.choices).forEach(c => {
      const b = el("button", "choice chunky" + (c.small ? " txt" : ""), `${c.html}<span class="w"></span>`);
      if (c.ok) markOk(b);
      b.onclick = async () => {
        if (locked) return; G.taps++;
        if (c.ok) {
          locked = true; b.classList.add("ok"); sfx.ok();
          const first = tries === 0; if (first) addStar();
          logRound(r.word, first, tries + 1); res.push(first ? 1 : 0); renderDots(res, total, -1);
          await say(r.praise || (r.lang ? praiseT() : PRAISE[rnd(PRAISE.length)]), L);
          i++; loops.push(setTimeout(round, 350));
        } else {
          tries++; sfx.ko(); b.classList.remove("ko"); void b.offsetWidth; b.classList.add("ko");
          if (c.label) b.querySelector(".w").textContent = c.label;
          say(c.sayWrong || r.say || "Try again!", L);
        }
      };
      grid.append(b);
    });
    body.append(grid);
    if (r.say) say(r.say, L);
  };
  round();
}
