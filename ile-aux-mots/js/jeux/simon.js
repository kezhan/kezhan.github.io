/* L'Île aux Mots : jeu « simon ». */
GAMES.simon = function () {
  const small = S.kid === "p4", total = 8, res = [];
  const orders = pick(SIMON, total).map(o => ({...o, trick: !small && Math.random() < 0.3}));
  startSession("simon", null, total);
  if (!S.present) toast("Ce jeu marche mieux avec un parent pour arbitrer");
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const o = orders[i], line = (o.trick ? "" : "Simon says: ") + o.en;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("div","order bob", o.e));
    const p = el("p","prompt", ""); p.textContent = kidCfg().showWord ? line : "Écoute Simon !";
    const hint = el("small","", o.trick ? "Parent : piège ! Sans « Simon says », il ne faut pas bouger." : "Parent : fais-le avec lui la première fois.");
    p.append(hint); body.append(p);
    const row = el("div","row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => line, "Encore"), bridgeBtn(o)); body.append(row);
    const judge = el("div","judge");
    const ok = el("button","chunky", "✅ Réussi"); ok.style.background = "#C9F2DF";
    const ko = el("button","chunky", "🔁 Pas encore"); ko.style.background = "#FFD6CF";
    ok.onclick = async () => { sfx.ok(); addStar(); logRound(o.en, true, 1); res.push(1); i++; await say(o.trick ? "Good! Simon didn't say!" : PRAISE[rnd(PRAISE.length)]); round(); };
    ko.onclick = async () => { sfx.ko(); logRound(o.en, false, 1); res.push(0); i++; await say(o.trick ? "Oops! Simon didn't say!" : "Nice try!"); round(); };
    judge.append(ok, ko); body.append(judge);
    say(line);
  };
  round();
};
