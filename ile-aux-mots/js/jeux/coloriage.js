/* L'Île aux Mots : jeu « Coloriage en suivant la consigne » (idée de Claude).
   A house drawn in SVG; "Colour the roof red!": touch the colour, then the part.
   1: 3 colours, big parts · 2: all basic colours and parts · 3: two orders in one sentence · 4: the order is written, voice on request.
   In English, German, Luxembourgish or Chinese, never French. */
addStyle(`
.dessin{width:min(100%,420px); align-self:center; background:#E9F8FF; border:3px solid var(--ink); border-radius:18px}
.dessin .zone{cursor:pointer; stroke:#1B2D45; stroke-width:3; transition:fill .3s}
.dessin .zone:hover{filter:brightness(.97)}
.palette{display:flex; gap:10px; justify-content:center; flex-wrap:wrap}
.palette .pot{width:56px; height:56px; border-radius:50%; border:3px solid var(--ink); box-shadow:2px 3px 0 var(--ink)}
.palette .pot[aria-pressed="true"]{transform:translateY(-6px) scale(1.12); box-shadow:0 8px 0 var(--ink)}
`);
registerGame({id:"coloriage", em:"🖍️", name:"Colouring", desc:"Colour what you hear", multi:true}, function () {
  const lvl = levelOf("coloriage"), lang = langOf() === "fr" ? "en" : langOf();
  // parts of the picture: German [article, noun] (accusative is built below), Luxembourgish with its article
  const PARTS = {
    wall:   {big:true, en:"wall", de:["die","Wand"], lb:"d'Mauer", zh:"墙"},
    roof:   {big:true, en:"roof", de:["das","Dach"], lb:"den Daach", zh:"屋顶"},
    door:   {en:"door", de:["die","Tür"], lb:"d'Dier", zh:"门"},
    window: {en:"window", de:["das","Fenster"], lb:"d'Fënster", zh:"窗户"},
    sun:    {big:true, en:"sun", de:["die","Sonne"], lb:"d'Sonn", zh:"太阳"},
    tree:   {big:true, en:"tree", de:["der","Baum"], lb:"de Bam", zh:"树"}
  };
  const acc = ([art, noun]) => `${art === "der" ? "den" : art} ${noun}`; // German accusative: der → den
  const one = (part, col) => ({
    en: `colour the ${PARTS[part].en} ${col.en}`,
    de: `mal ${acc(PARTS[part].de)} ${col.de} an`,
    lb: `mol ${PARTS[part].lb} ${col.lb}`,
    zh: `把${PARTS[part].zh}涂成${col.zh}`
  })[lang];
  const sentence = orders => {
    const parts = orders.map(o => one(o.part, o.col));
    const joined = lang === "zh" ? parts.join("，再") : parts.join(lang === "en" ? " and " : lang === "de" ? " und " : " an ");
    return lang === "zh" ? `${joined}！` : joined.charAt(0).toUpperCase() + joined.slice(1) + "!";
  };
  const allCols = THEMES.colors.words.filter(w => ["red","blue","green","yellow","orange","pink","purple","brown"].includes(w.en));
  const cols = lvl === 1 ? allCols.slice(0, 3) : allCols.slice(0, 6);
  const partIds = Object.keys(PARTS).filter(p => lvl > 1 || PARTS[p].big);
  const svg = `<svg class="dessin" viewBox="0 0 300 220" role="img" aria-label="house">
    <circle class="zone" data-part="sun" cx="255" cy="40" r="24" fill="#fff"/>
    <rect class="zone" data-part="tree" x="20" y="90" width="50" height="60" rx="25" fill="#fff"/>
    <rect x="40" y="148" width="10" height="42" fill="#795548" stroke="#1B2D45" stroke-width="3"/>
    <rect class="zone" data-part="wall" x="95" y="100" width="130" height="90" fill="#fff"/>
    <polygon class="zone" data-part="roof" points="85,102 160,45 235,102" fill="#fff"/>
    <rect class="zone" data-part="door" x="145" y="140" width="32" height="50" fill="#fff"/>
    <rect class="zone" data-part="window" x="105" y="115" width="30" height="26" fill="#fff"/>
    <line x1="0" y1="190" x2="300" y2="190" stroke="#1B2D45" stroke-width="3"/></svg>`;
  const total = lvl === 3 ? 4 : 6, res = [];
  startSession("coloriage", null, total); const gen = GEN;
  let i = 0;
  const body = $("gameBody"); body.innerHTML = "";
  const p = el("p", "prompt", ""); const row = el("div", "row"); row.style.justifyContent = "center";
  const pic = el("div", "", svg); pic.style.cssText = "display:flex; justify-content:center";
  const pal = el("div", "palette");
  body.append(p, row, pic, pal);
  let chosen = null, todo = [], tries = 0;
  const pots = cols.map(c => {
    const b = el("button", "pot", ""); b.style.background = c.e; b.setAttribute("aria-label", c[lang] || c.en);
    b.onclick = () => { chosen = c; G.taps++; pots.forEach(x => x.setAttribute("aria-pressed", String(x === b))); say(T(c), langOf()); marks(); };
    pal.append(b); return b;
  });
  const zones = [...pic.querySelectorAll(".zone")];
  // test markers: the colour still needed, then its part
  const marks = () => {
    if (!TEST) return;
    const next = todo[0];
    pots.forEach(b => delete b.dataset.ok); zones.forEach(z => z.removeAttribute("data-ok"));
    if (!next) return;
    if (chosen !== next.col) pots[cols.indexOf(next.col)].dataset.ok = "1";
    else zones.find(z => z.dataset.part === next.part).setAttribute("data-ok", "1");
  };
  const ask = () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const parts = pick(partIds, lvl === 3 ? 2 : 1);
    todo = parts.map(part => ({part, col: pick(cols, 1)[0]}));
    const text = sentence(todo);
    p.innerHTML = lvl === 4 ? `<b>${text}</b>` : "🖍️ 👂";
    row.innerHTML = "";
    if (lvl === 4) { const b = el("button", "chip", "🔊"); b.onclick = () => { G.hints++; say(text, lang); }; row.append(b); }
    else row.append(speakBtn(() => text, "", () => lang));
    tries = 0; marks();
    if (lvl !== 4) say(text, lang);
  };
  zones.forEach(z => z.addEventListener("click", async () => {
    if (!todo.length || !alive(gen)) return;
    G.taps++;
    const want = todo.find(o => o.part === z.dataset.part);
    if (want && chosen === want.col) {
      z.setAttribute("fill", chosen.e); sfx.ok(); if (typeof fx !== "undefined") fx.sparkle();
      todo = todo.filter(o => o !== want); marks();
      if (todo.length) return;
      const first = tries === 0; if (first) addStar();
      logRound(`${z.dataset.part} ${want.col.en}`, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
      await say(praiseT(), langOf());
      i++; loops.push(setTimeout(ask, 350));
    } else {
      tries++; sfx.ko();
      z.animate([{transform: "translateX(0)"}, {transform: "translateX(-4px)"}, {transform: "translateX(4px)"}, {transform: "none"}], {duration: 300});
      say(sentence(todo), lang);
    }
  }));
  ask();
});
