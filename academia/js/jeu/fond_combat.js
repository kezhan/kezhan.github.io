/* Académia : le décor des combats, en pixel art. Le ciel, l'horizon de la région (forêt, mer, montagnes,
   jardin en fleurs, nuit étoilée), le sol fait des cases du pack et les deux estrades des créatures.
   Tout est posé en pixels du décor (unités entières) ; la caméra les agrandit d'un facteur entier : rendu net. */
const THEMES = {
  dojo:         {ciel: [0x7EC8F8, 0xA4DAFB, 0xD3EEFF], sol: "sable", horizon: "foret", piece: "torii", estrade: "sable"},
  albion:       {ciel: [0x6DB6F4, 0x9AD0FA, 0xD5EFFF], sol: "sable", horizon: "mer", estrade: "sable"},
  germania:     {ciel: [0x86B4EE, 0xB0D0F6, 0xDDEBFF], sol: "herbe", horizon: "montagnes", estrade: "herbe"},
  duche:        {ciel: [0x7EC8F8, 0xA4DAFB, 0xD3EEFF], sol: "herbe", horizon: "foret", piece: "maisonRouge", estrade: "herbe"},
  jardin:       {ciel: [0x8FD0FF, 0xBFE4FF, 0xFFE0F0], sol: "herbe", horizon: "jardin", estrade: "herbe", fleurs: .16},
  observatoire: {ciel: [0x161241, 0x272068, 0x463A98], sol: "herbe", horizon: "foret", piece: "statue", estrade: "herbe", nuit: true}
};
const CIEL_BOSS = [0x2A1650, 0x6B2E7A, 0xE07A5A];   // dusk: the boss's lair
const ESTRADES = {
  herbe: {bord: 0x24502A, cote: 0x4E9A3A, dessus: 0x86D266, clair: 0xA2E27E},
  sable: {bord: 0x7A4A22, cote: 0xC98A4B, dessus: 0xF5CF96, clair: 0xFFE2B4}
};
const SEMIS = [T(3, 15), T(4, 15), T(1, 8)];   // sunflower, clover, little flowers
// the sheet draws its trees in overlapping groups of four: whole groups only, so that no cut tree shows against the sky
const MASSIFS = {
  buissons: [[3, 9, 2, 1, 48, 0], [0, 10, 6, 2, 0, 16]],   // [column, row, width, height, dx, dy] from the group's corner
  sapins: [[9, 9, 2, 1, 48, 0], [6, 10, 6, 2, 0, 16]],
  roses: [[7, 15, 2, 1, 48, 0], [4, 16, 6, 2, 0, 16]]
};
const mult = (a, b) => [16, 8, 0].reduce((s, d) => s + (Math.round(((a >> d) & 255) * ((b >> d) & 255) / 255) << d), 0);

// a filled ellipse, one row at a time on whole units: crisp pixels once zoomed
function disque(g, cx, cy, rx, ry, couleur, alpha = 1){
  g.fillStyle(couleur, alpha);
  for (let dy = -ry; dy <= ry; dy++) {
    const h = Math.round(rx * Math.sqrt(Math.max(0, 1 - (dy / (ry + .5)) ** 2)));
    g.fillRect(cx - h, cy + dy, 2 * h + 1, 1);
  }
}

function dessinerFond(S, L, regionId, boss){
  const th = THEMES[regionId] || THEMES.dojo, {W, H, yh} = L;
  const teinte = th.nuit ? 0x7480B8 : boss ? 0xD2BCE8 : null, ton = col => teinte ? mult(col, teinte) : col;
  const hasard = (x, y) => Math.abs(Math.sin(x * 12.9898 + y * 78.233) * 43758.5453) % 1;
  const tuile = (x, y, f, prof) => { const i = S.add.image(Math.round(x), Math.round(y), "tuiles", f).setOrigin(0).setDepth(prof); if (teinte) i.setTint(teinte); return i; };
  const bloc = (c, r, w, h, x, y, prof) => { for (let j = 0; j < h; j++) for (let i = 0; i < w; i++) tuile(x + 16 * i, y + 16 * j, T(c + i, r + j), prof); };
  const piece = (nom, x, y, prof = 2) => { const [c, r, w, h] = PIECES[nom]; bloc(c, r, w, h, x, y, prof); };
  const massif = (nom, x, y) => MASSIFS[nom].forEach(([c, r, w, h, dx, dy]) => bloc(c, r, w, h, x + dx, y + dy, 2));
  const g = S.add.graphics().setDepth(0), loin = S.add.graphics().setDepth(.5);

  // sky: three bands, dithered seams; clouds by day, stars at night or at dusk
  const c = boss && !th.nuit ? CIEL_BOSS : th.ciel, b = Math.ceil(yh / 3);
  c.forEach((col, i) => g.fillStyle(col).fillRect(0, i * b, W, b + 1));
  for (let i = 1; i < 3; i++) for (let x = 0; x < W; x++) {
    g.fillStyle(c[i]).fillRect(x, i * b - 1 - (x % 2), 1, 1);
    g.fillStyle(c[i - 1]).fillRect(x, i * b + (x % 2), 1, 1);
  }
  if (th.nuit || boss) for (let i = 0; i < W * yh / 70; i++) {
    const x = Math.floor(hasard(i, 1) * W), y = Math.floor(hasard(i, 2) * yh * .8), col = hasard(i, 3) < .3 ? 0xFFF2A8 : 0xFFFFFF;
    g.fillStyle(col).fillRect(x, y, 1, 1);
    if (hasard(i, 4) < .1) g.fillRect(x - 1, y, 3, 1).fillRect(x, y - 1, 1, 3);
  }
  if (th.nuit) { const x = Math.round(W * .82), y = Math.round(yh * .3); disque(g, x, y, 7, 7, 0xFFF4C2); disque(g, x - 2, y - 1, 2, 2, 0xEADFA8); disque(g, x + 3, y + 2, 1, 1, 0xEADFA8); }
  else if (!boss) [[.06, .2, 34], [.42, .08, 46], [.78, .34, 30]].forEach(([x, y, l]) => nuage(g, Math.round(W * x), Math.round(yh * y) + 5, l));

  // horizon
  if (th.horizon === "mer") {
    const ym = yh - 16;
    disque(loin, Math.round(W * .2), ym, 18, 5, 0x3E8E4A); disque(loin, Math.round(W * .2) + 9, ym - 2, 7, 3, 0x56A85E);
    [[0x2C7FD0, 0, 5], [0x3E9BE6, 5, 6], [0x5BB6F2, 11, 6]].forEach(([col, y, h]) => loin.fillStyle(ton(col)).fillRect(0, ym + y, W, h));
    for (let i = 0; i < W / 9; i++) loin.fillStyle(0xFFFFFF, .85).fillRect(Math.floor(hasard(i, 7) * W), ym + 2 + Math.floor(hasard(i, 8) * 13), 2 + (i % 2), 1);
    piece("barque", Math.round(W * .42), ym + 4, 1.5);
  } else {
    if (th.horizon === "montagnes") {
      const m = {clair: ton(0x8C93C4), ombre: ton(0x6E74A6), neige: ton(0xF4F7FF), neigeOmbre: ton(0xD3DAF0)};
      montagne(loin, Math.round(W * .3), yh + 1, Math.round(W * .24), Math.round(yh * .82), m);
      montagne(loin, Math.round(W * .66), yh + 1, Math.round(W * .2), Math.round(yh * .64), m);
      montagne(loin, Math.round(W * .94), yh + 1, Math.round(W * .15), Math.round(yh * .5), m);
    }
    const colline = th.nuit ? 0x2E4A6E : boss ? 0x4F6E5A : 0x5FAE6A;
    for (let x = -8, i = 0; x < W + 12; x += 12, i++) disque(loin, x, yh, 10 + Math.floor(hasard(i, 5) * 5), 5 + Math.floor(hasard(i, 6) * 6), colline);
    loin.fillStyle(colline).fillRect(0, yh - 1, W, 3);
    const groupes = th.horizon === "jardin" ? ["roses", "buissons"] : th.horizon === "montagnes" ? ["sapins"] : ["buissons", "sapins"];
    const pas = th.horizon === "montagnes" ? 150 : 86;
    for (let i = 0, x = -24; x < W; i++, x += pas + Math.floor(hasard(i, 9) * 10)) massif(groupes[i % groupes.length], x, yh - 42);
  }
  if (th.piece === "torii") piece("torii", Math.round(W * .1), yh - 26, 2.5);
  if (th.piece === "maisonRouge") piece("maisonRouge", Math.round(W * .06), yh - 40, 2.5);
  if (th.piece === "statue") piece("statue", Math.round(W * .12), yh - 22, 2.5);

  // ground, with a few flowers (many in the garden)
  if (th.sol === "sable") {   // plain sand, a few grains
    const s = S.add.graphics().setDepth(1); s.fillStyle(ton(0xF2B36E)).fillRect(0, yh, W, H - yh);
    for (let i = 0; i < W * (H - yh) / 45; i++) s.fillStyle(ton(hasard(i, 11) < .6 ? 0xDE9752 : 0xFFD08F)).fillRect(Math.floor(hasard(i, 12) * W), yh + Math.floor(hasard(i, 13) * (H - yh)), 1, 1);
  } else for (let y = yh; y < H; y += 16) for (let x = 0; x < W; x += 16) tuile(x, y, SOL.herbe[0], 1);
  if (th.horizon === "mer") { const f = S.add.graphics().setDepth(1.5); for (let x = 0; x < W; x += 7) f.fillStyle(0xFFFFFF, .8).fillRect(x + Math.floor(hasard(x, 3) * 3), yh, 3, 1); }
  const dens = th.fleurs || .05;
  for (let y = yh + 8; y < H - 8; y += 12) for (let x = 4; x < W - 8; x += 12)
    if (hasard(x, y) < dens) tuile(x + Math.floor(hasard(y, x) * 6), y, SEMIS[Math.floor(hasard(x * 3, y) * SEMIS.length)], 2);

  // the two stands
  const e = S.add.graphics().setDepth(3);
  const ce = Object.fromEntries(Object.entries(ESTRADES[th.estrade]).map(([n, col]) => [n, ton(col)]));
  [L.lui, L.moi].forEach(p => estrade(e, p, ce, hasard));
}

function nuage(g, x, y, l){
  const r = Math.max(3, Math.round(l / 6));
  const forme = (dy, col) => {
    disque(g, x + Math.round(l * .28), y + dy, r, r, col);
    disque(g, x + Math.round(l * .52), y - Math.round(r * .55) + dy, Math.round(r * 1.35), Math.round(r * 1.35), col);
    disque(g, x + Math.round(l * .76), y + dy, Math.round(r * 1.1), Math.round(r * 1.1), col);
    g.fillStyle(col).fillRect(x + Math.round(l * .12), y + dy, Math.round(l * .78), r + 1);
  };
  forme(1, 0xC9DDF2); forme(0, 0xFFFFFF);
}

function montagne(g, cx, base, hw, ht, m){
  for (let r = 0; r < ht; r++) {
    const y = base - ht + r, w = Math.max(1, Math.round(hw * (r + 1) / ht)), neige = r < ht * .3;
    g.fillStyle(neige ? m.neige : m.clair).fillRect(cx - w, y, w, 1);
    g.fillStyle(neige ? m.neigeOmbre : m.ombre).fillRect(cx, y, w + 1, 1);
    if (!neige && r < ht * .3 + 2) for (let x = cx - w + (r % 2); x <= cx + w; x += 2) g.fillStyle(x < cx ? m.neige : m.neigeOmbre).fillRect(x, y, 1, 1);
  }
}

// a round stand seen from the side: an outline, a darker edge, a lighter top
function estrade(g, p, c, hasard){
  const {x, y, rx, ry} = p, ep = Math.max(3, Math.round(ry * .5));
  disque(g, x, y + ep, rx + 1, ry + 1, c.bord); disque(g, x, y, rx + 1, ry + 1, c.bord);
  g.fillStyle(c.bord).fillRect(x - rx - 1, y, 2 * rx + 3, ep);
  disque(g, x, y + ep, rx, ry, c.cote); g.fillStyle(c.cote).fillRect(x - rx, y, 2 * rx + 1, ep);
  disque(g, x, y, rx, ry, c.dessus);
  disque(g, x - Math.round(rx * .08), y - Math.round(ry * .12), Math.round(rx * .8), Math.round(ry * .64), c.clair);
  for (let i = 0; i < rx / 3; i++) {   // little strokes on the top
    const a = hasard(i, x) * Math.PI * 2, d = Math.sqrt(hasard(x, i)) * .85;
    g.fillStyle(c.cote).fillRect(Math.round(x + Math.cos(a) * rx * d), Math.round(y + Math.sin(a) * ry * d), 1, 2);
  }
}
