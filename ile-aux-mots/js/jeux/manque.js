/* L'Île aux Mots : jeu « Qu'est-ce qui manque ? » (atelier de conception, 6,7/10).
   The magician names objects, a hat falls, one object is gone: which one?
   1: 3 objects of one theme, 3 pictures · 2: 5 mixed objects, 4 pictures · 3: 6 objects, 4 written words · 4: 8 objects, 6 written words */
addStyle(`
.table-magie{display:flex; flex-wrap:wrap; justify-content:center; gap:10px; padding:16px; min-height:120px; background:#3E2A5C; border:3px solid var(--ink); border-radius:18px; position:relative; overflow:hidden}
.table-magie .obj{font-size:clamp(44px,10vw,64px); background:none; border:0; line-height:1.1; transition:transform .15s}
.table-magie .obj.vide{opacity:.9; filter:grayscale(1)}
.table-magie .drap{position:absolute; inset:0; display:grid; place-items:center; font-size:72px; background:repeating-linear-gradient(90deg,#8E24AA 0 22px,#6A1B9A 22px 44px); transform:translateY(-105%); transition:transform .5s cubic-bezier(.5,0,.3,1.3)}
.table-magie .drap.tombe{transform:none}
.choice.mot{aspect-ratio:auto; min-height:84px; font-size:24px; font-family:var(--display); font-weight:600}
`);
registerGame({id:"manque", em:"🎩", name:"Qu'est-ce qui manque ?", desc:"Retiens les objets du magicien", themes:true, multi:true}, function (theme) {
  const lvl = levelOf("manque"), n = [3, 5, 6, 8][lvl - 1], nChoix = [3, 4, 4, 6][lvl - 1], words4 = lvl >= 3;
  const MISSING = {en:"What's missing?", fr:"Qu'est-ce qui manque ?", zh:"少了什么？", de:"Was fehlt?", lb:"Wat feelt?"};
  const CLOSE = {en:"Close your eyes!", fr:"Ferme les yeux !", zh:"闭上眼睛！", de:"Augen zu!", lb:"Maach d'Aen zou!"};
  const pool = lvl === 1 || theme === "colors" ? wordsOf(theme, lvl)
    : Object.keys(THEMES).filter(k => k !== "colors").flatMap(k => wordsOf(k, lvl));
  const total = 5, res = []; let i = 0;
  startSession("manque", theme, total); const gen = GEN;
  const round = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const objs = pick(pool, n), gone = objs[rnd(n)];
    const body = $("gameBody"); body.innerHTML = "";
    const p = el("p", "prompt", "Regarde bien les objets du magicien 🎩<small>Touche-les pour entendre leur nom</small>");
    const table = el("div", "table-magie");
    const drap = el("div", "drap", "🎩");
    const ready = el("button", "bigbtn chunky", "J'ai retenu ! 👀"); ready.style.alignSelf = "center";
    body.append(p, table, ready);
    const cells = objs.map(w => {
      const b = el("button", "obj", wordFace(w));
      b.onclick = () => { G.taps++; sayT(T(w)); if (typeof fx !== "undefined") fx.bounce(b); };
      table.append(b); return b;
    });
    table.append(drap);
    // each object hops when it is named
    (async () => {
      for (let k = 0; k < objs.length && alive(gen) && !ready.disabled; k++) {
        if (typeof fx !== "undefined") fx.bounce(cells[k]);
        await sayT(T(objs[k]));
      }
    })();
    const reveal = async () => {
      if (ready.disabled || !alive(gen)) return; ready.disabled = true; ready.remove();
      p.innerHTML = "Abracadabra !<small>Un objet a disparu…</small>";
      sayT(CLOSE[langOf()] || CLOSE.en);
      drap.classList.add("tombe");
      await new Promise(r => loops.push(setTimeout(r, TEST ? 50 : 1300)));
      if (!alive(gen)) return;
      const slot = cells[objs.indexOf(gone)];
      slot.innerHTML = "❓"; slot.classList.add("vide"); slot.onclick = null;
      drap.classList.remove("tombe");
      p.innerHTML = `${MISSING[langOf()] || MISSING.en}<small>Qu'est-ce qui a disparu ?</small>`;
      sayT(MISSING[langOf()] || MISSING.en);
      // the wrong answers are never on the table: the child has to remember
      const choix = shuffle([gone, ...pick(pool.filter(w => !objs.includes(w)), nChoix - 1)]);
      const grid = el("div", "choices"); let tries = 0, locked = false;
      choix.forEach(w => {
        const c = el("button", "choice chunky" + (words4 ? " mot" : ""), words4 ? T(w) : wordFace(w));
        if (w === gone) markOk(c);
        c.onclick = async () => {
          if (locked) return; G.taps++;
          if (w === gone) {
            locked = true; c.classList.add("ok"); sfx.ok();
            slot.innerHTML = wordFace(gone); slot.classList.remove("vide");
            if (typeof fx !== "undefined") fx.bounce(slot);
            const first = tries === 0; if (first) addStar();
            logRound(gone.en, first, tries + 1, {lvl, n}); res.push(first ? 1 : 0); renderDots(res, total, -1);
            await sayT(praiseT() + " " + T(gone) + "!");
            i++; loops.push(setTimeout(round, 400));
          } else {
            tries++; sfx.ko(); c.classList.remove("ko"); void c.offsetWidth; c.classList.add("ko");
            sayT(T(w));
          }
        };
        grid.append(c);
      });
      body.append(grid);
    };
    ready.onclick = reveal;
    loops.push(setTimeout(reveal, TEST ? 100 : 20000)); // after 20 s the hat falls on its own
  };
  round();
});
