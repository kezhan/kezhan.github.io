/* L'Île aux Mots : jeu « simon ».
   1: always "Simon says" · 2: traps without "Simon says" · 3: two actions in a row · 4: two actions, counted moves ("Jump three times"), more traps */
GAMES.simon = function () {
  const lvl = levelOf("simon"), total = 8, res = [];
  const trapRate = [0, 0.3, 0.3, 0.4][lvl - 1];
  const bare = s => s.replace(/!$/, "");
  const COUNTABLE = ["Jump!", "Clap your hands!", "Turn around!", "Stomp your feet!", "Wave hello!"];
  const one = () => {
    const o = pick(SIMON, 1)[0];
    if (lvl === 4 && COUNTABLE.includes(o.en)) {
      const n = 2 + rnd(3);
      return {e: o.e.repeat(Math.min(n, 3)), en: `${bare(o.en)} ${numberWords(n)} times!`, fr: `${o.fr} ${n} fois`, zh: `${o.zh}${NUMS_ZH[n - 1]}次`};
    }
    return o;
  };
  const orders = [...Array(total)].map(() => {
    let o = one();
    if (lvl >= 3) { // two actions joined: "Touch your nose and clap your hands!"
      let b = one(); while (b.en === o.en) b = one();
      o = {e: o.e + b.e, en: `${bare(o.en)} and ${b.en.charAt(0).toLowerCase() + b.en.slice(1)}`, fr: `${o.fr}, puis ${b.fr.toLowerCase()}`, zh: `${o.zh}，然后${b.zh}`};
    }
    return {...o, trick: Math.random() < trapRate};
  });
  startSession("simon", null, total);
  if (!S.present) toast("Ce jeu marche mieux avec un parent pour arbitrer");
  let i = 0; const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    const o = orders[i], line = (o.trick ? "" : "Simon says: ") + o.en;
    renderDots(res, total, i);
    const body = $("gameBody"); body.innerHTML = "";
    body.append(el("div", "order bob", o.e));
    const p = el("p", "prompt", ""); p.textContent = kidCfg().showWord ? line : "Écoute Simon !";
    const hint = el("small", "", o.trick ? "Parent : piège ! Sans « Simon says », il ne faut pas bouger." : lvl >= 3 ? "Parent : les deux gestes, dans l'ordre." : "Parent : fais-le avec lui la première fois.");
    p.append(hint); body.append(p);
    const row = el("div", "row"); row.style.justifyContent = "center";
    row.append(speakBtn(() => line, "Encore"), bridgeBtn(o)); body.append(row);
    const judge = el("div", "judge");
    const ok = el("button", "chunky", "✅ Réussi"); ok.style.background = "#C9F2DF";
    const ko = el("button", "chunky", "🔁 Pas encore"); ko.style.background = "#FFD6CF";
    ok.onclick = async () => { sfx.ok(); addStar(); logRound(o.en, true, 1, {lvl, trick: o.trick}); res.push(1); i++; await say(o.trick ? "Good! Simon didn't say!" : PRAISE[rnd(PRAISE.length)]); round(); };
    ko.onclick = async () => { sfx.ko(); logRound(o.en, false, 1, {lvl, trick: o.trick}); res.push(0); i++; await say(o.trick ? "Oops! Simon didn't say!" : "Nice try!"); round(); };
    judge.append(ok, ko); body.append(judge);
    say(line);
  };
  round();
};
