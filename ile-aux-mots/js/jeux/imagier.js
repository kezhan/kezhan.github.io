/* L'Île aux Mots : jeu « imagier ». */
GAMES.imagier = function (theme) {
  const words = wordsOf(theme, levelOf("imagier")), seen = new Set();
  startSession("imagier", theme, words.length);
  const body = $("gameBody");
  body.append(el("p","prompt",`Touche une image !<small>Elle te dit son nom : ${LANGS[langOf()].label}</small>`));
  const grid = el("div","grid-cards");
  words.forEach(w => {
    const c = el("button","card chunky", `${wordFace(w)}<span class="w"></span><span class="fr"></span>`);
    c.onclick = () => {
      G.taps++; seen.add(w.en); sayT(T(w));
      c.querySelector(".w").textContent = T(w);
      c.querySelector(".fr").textContent = bridgeLang() ? w[bridgeLang()] : "";
      c.classList.add("seen","flipped"); setTimeout(() => c.classList.remove("flipped"), 700);
      renderDots([...Array(seen.size)].map(() => 1), words.length, -1);
      if (seen.size === words.length && !G.allSeen) { G.allSeen = true; addStar(3); confetti(10); sayT(praiseT()); }
    };
    grid.appendChild(c);
  });
  body.append(grid);
  const done = el("button","bigbtn chunky","Fini ! ⭐"); done.style.alignSelf = "center";
  done.onclick = () => { if (!G.allSeen) addStar(Math.min(3, Math.ceil(seen.size/4))); finish(); };
  body.append(done);
  renderDots([], words.length, -1);
};
