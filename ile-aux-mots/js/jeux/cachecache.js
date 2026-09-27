/* L'Île aux Mots : jeu « Cache-cache dans la maison » (atelier de conception, 6,3/10), mode « Où est-il ? ».
   An animal cries from its hiding place, a sentence says where it is, the child lifts the right one.
   1: 3-4 places, the right one trembles during the cry · 2: 6 places, no help · 3: two animals hidden, which one is asked
   4: the sentence is written, no voice (listen button counted as a hint). Languages: en, de, lb, zh; never French. */
addStyle(`
.piece{display:grid; grid-template-columns:repeat(auto-fit,minmax(110px,1fr)); gap:14px; padding:14px; background:#FFE9C7; border:3px solid var(--ink); border-radius:18px}
.cachette{position:relative; aspect-ratio:1; font-size:clamp(52px,12vw,76px); background:#fff; display:grid; place-items:center; overflow:visible}
.cachette .qui{position:absolute; inset:0; display:grid; place-items:center; font-size:.8em; opacity:0; transform:translateY(20px) scale(.5); transition:all .35s cubic-bezier(.3,1.6,.5,1)}
.cachette.ouverte .qui{opacity:1; transform:translateY(-58%) scale(1)}
.cachette.frisson{animation:frisson .12s linear 8}
@keyframes frisson{25%{transform:rotate(-6deg)} 75%{transform:rotate(6deg)}}
`);
registerGame({id:"cachecache", em:"🙈", name:"Hide & Seek", desc:"Where is the cat?", multi:true}, function () {
  const lvl = levelOf("cachecache"), lang = langOf() === "fr" ? "en" : langOf(); // never French for the children
  // hiding places; German and Luxembourgish carry their dative article after the preposition
  const PLACES = [
    // p: the prepositions that make sense for that place (never "in the sofa")
    // German: [article, noun]; Luxembourgish: [gender, noun], since d' is both feminine and neuter
    {e:"🛏️", p:["under","on","in","next to"], en:"bed", de:["das","Bett"], lb:["n","Bett"], zh:"床"},
    {e:"📦", p:["in","on","behind","next to"], en:"box", de:["die","Kiste"], lb:["f","Këscht"], zh:"箱子"},
    {e:"🪑", p:["under","on","behind","next to"], en:"chair", de:["der","Stuhl"], lb:["m","Stull"], zh:"椅子"},
    {e:"🧺", p:["in","behind","next to"], en:"basket", de:["der","Korb"], lb:["m","Kuerf"], zh:"篮子"},
    {e:"🛋️", p:["under","on","behind","next to"], en:"sofa", de:["das","Sofa"], lb:["m","Canapé"], zh:"沙发"},
    {e:"🛁", p:["in","behind","next to"], en:"bath", de:["die","Badewanne"], lb:["f","Buedbidden"], zh:"浴缸"},
    {e:"🪴", p:["behind","next to"], en:"plant", de:["die","Pflanze"], lb:["f","Planz"], zh:"花盆"}
  ];
  const PREPS = lvl === 1 ? ["in", "under"] : ["in", "under", "on", "behind", "next to"];
  const P = {
    en: {in:"in", under:"under", on:"on", behind:"behind", "next to":"next to"},
    de: {in:"in", under:"unter", on:"auf", behind:"hinter", "next to":"neben"},
    lb: {in:"an", under:"ënner", on:"op", behind:"hannert", "next to":"nieft"},
    zh: {in:"里面", under:"下面", on:"上面", behind:"后面", "next to":"旁边"}
  };
  const DAT = {de: {der:"dem", das:"dem", die:"der"}, lb: {m:"dem", n:"dem", f:"der"}};
  const place = (pl, prep) => {
    if (lang === "en") return `${P.en[prep]} the ${pl.en}`;
    if (lang === "zh") return `在${pl.zh}${P.zh[prep]}`;
    const [art, noun] = pl[lang], d = DAT[lang][art];
    if (prep === "in" && d === "dem") return `${lang === "de" ? "im" : "am"} ${noun}`; // in dem → im, an dem → am
    if (lang === "lb" && prep === "on" && d === "dem") return `um ${noun}`; // op dem → um
    return `${P[lang][prep]} ${d} ${noun}`;
  };
  const cap = s => s.charAt(0).toUpperCase() + s.slice(1);
  const CRY = {cat:["Meow!","Miau!","Miau!","喵喵！"], dog:["Woof!","Wau wau!","Wau wau!","汪汪！"], cow:["Moo!","Muh!","Muh!","哞！"], pig:["Oink!","Grunz!","Grunz!","哼哼！"],
    duck:["Quack!","Quak!","Quak!","嘎嘎！"], sheep:["Baa!","Mäh!","Mä!","咩！"], mouse:["Squeak!","Piep!","Piip!","吱吱！"], frog:["Ribbit!","Quak!","Quak!","呱呱！"]};
  const LI = {en:0, de:1, lb:2, zh:3};
  const animals = THEMES.animals.words.filter(w => CRY[w.en]);
  const name = a => lang === "en" ? `the ${a.en}` : a[lang];
  const sentence = (a, pl, prep) => lang === "zh" ? `${a.zh}${place(pl, prep)}。` : `${cap(name(a))} ${lang === "en" ? "is" : lang === "de" ? "ist" : "ass"} ${place(pl, prep)}.`;
  const where = a => ({en: `Where is ${name(a)}?`, de: `Wo ist ${a.de}?`, lb: `Wou ass ${a.lb}?`, zh: `${a.zh}在哪里？`})[lang];
  const SURPRISES = [["🧦", "Oh, a sock!"], ["👻", "Boo!"], ["🍌", "A banana!"], ["🎈", "Pop!"], ["🧸", "Hello!"]];
  const nPlaces = [4, 6, 6, 6][lvl - 1], total = 6, res = [];
  startSession("cachecache", null, total); const gen = GEN;
  let i = 0;
  const round = async () => {
    if (!alive(gen)) return;
    if (i >= total) return finish();
    renderDots(res, total, i);
    const spots = pick(PLACES, lvl === 1 ? 3 + rnd(2) : nPlaces);
    const hidden = pick(animals, lvl === 3 ? 2 : 1).map((a, k) => ({a, pl: spots[k === 0 ? rnd(spots.length) : 0], prep: null}));
    // level 1 prefers in and under when the place allows it
    hidden.forEach(h => { const ok = h.pl.p.filter(x => PREPS.includes(x)); h.prep = pick(ok.length ? ok : h.pl.p, 1)[0]; });
    if (hidden[1] && hidden[1].pl === hidden[0].pl) hidden[1].pl = spots.find(s => s !== hidden[0].pl);
    const asked = hidden[hidden.length - 1];
    const text = hidden.map(h => sentence(h.a, h.pl, h.prep)).join(" ") + " " + where(asked.a);
    const body = $("gameBody"); body.innerHTML = "";
    const p = el("p", "prompt", lvl === 4 ? text : "🙈 👀"); body.append(p);
    const row = el("div", "row"); row.style.justifyContent = "center";
    if (lvl === 4) { const b = el("button", "chip", "🔊"); b.onclick = () => { G.hints++; say(text, lang); }; row.append(b); }
    else row.append(speakBtn(() => text, "🔊", () => lang));
    body.append(row);
    const room = el("div", "piece"); body.append(room);
    let tries = 0, locked = false;
    const cells = spots.map(pl => {
      const inside = hidden.find(h => h.pl === pl);
      const surprise = pick(SURPRISES, 1)[0];
      const c = el("button", "cachette chunky", `${pl.e}<span class="qui">${inside ? inside.a.e : surprise[0]}</span>`);
      c.setAttribute("aria-label", lang === "en" ? pl.en : lang === "zh" ? pl.zh : pl[lang][1]);
      if (pl === asked.pl) markOk(c);
      c.onclick = async () => {
        if (locked || c.classList.contains("ouverte")) return; G.taps++;
        c.classList.add("ouverte");
        if (pl === asked.pl) {
          locked = true; sfx.ok();
          const first = tries === 0; if (first) addStar();
          logRound(`${asked.a.en} ${asked.prep} ${pl.en}`, first, tries + 1, {lvl}); res.push(first ? 1 : 0); renderDots(res, total, -1);
          await say(`${lang === "zh" ? "找到了！" : "Peekaboo!"} ${CRY[asked.a.en][LI[lang]]}`, lang === "lb" ? "de" : lang);
          i++; loops.push(setTimeout(round, 700));
        } else {
          tries++; sfx.ko();
          say(inside ? CRY[inside.a.en][LI[lang]] : surprise[1], inside && lang !== "lb" ? lang : "en");
          loops.push(setTimeout(() => c.classList.remove("ouverte"), 1200));
          if (tries >= 2 && lvl === 1) cells[spots.indexOf(asked.pl)].classList.add("bob");
        }
      };
      room.append(c); return c;
    });
    if (lvl === 4) return;
    // the cry comes first (and the hiding place trembles at level 1), then the sentence
    const cryAt = cells[spots.indexOf(asked.pl)];
    if (lvl === 1) cryAt.classList.add("frisson");
    await say(CRY[asked.a.en][LI[lang]], lang === "lb" ? "de" : lang);
    if (!alive(gen)) return;
    cryAt.classList.remove("frisson");
    say(text, lang);
  };
  round();
});
