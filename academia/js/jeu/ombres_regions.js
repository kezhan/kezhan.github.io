/* Académia : les Ombres des régions et les Ombres brillantes.
   1. Chaque maison teinte ses Ombres et leur donne une parure : le bandeau du Dojo des Nombres, la marinière sur le
      sable du Port d'Albion, la neige sur la tête à la Montagne de Germania, la ceinture aux couleurs du pays dans la
      Vallée du Grand-Duché, des fleurs au Jardin des Éclats, des étoiles à l'Observatoire. Le dessin d'origine
      (assets/hd ou assets/leger, creatures) est recoloré une fois dans une toile qui devient une texture de Phaser :
      rien à télécharger, tout marche hors ligne. La face tout de suite (60 ms pour un grand dessin de combat en haute
      définition sur le PC, 20 ms en léger), les deux autres poses plus tard, l'Ombre entrée dans l'arène, pour que
      rien ne s'arrête à l'écran ; au village, seule la petite face sert et seule elle est peinte.
      Seul le violet du corps change ; les yeux, la bouche, les joues, le cœur, la goutte et le contour sombre restent
      ceux du dessin, et le contour repasse par-dessus la parure.
   2. Une Ombre brillante (une sur vingt environ) : sa teinte plus claire, un reflet et des éclats peints, une lueur
      dorée et des étincelles qui scintillent autour d'elle, au village comme au combat.
   Contrat : ombreTeinte(cle, region, {brillante, petit, ensuite}) rend la clé (sans « hd_ ») de la texture teinte, ou `cle`
   quand le dessin d'origine n'est pas encore là ; scintillerOmbre(scene, sprite) ; tirerBrillante(). */
const CHANCE_BRILLANTE = 1 / 20;
// window.__brillantes, set by the recette before the page loads (tests/recette_ombres.py), forces the draw: 1 or 0
const tirerBrillante = () => Math.random() < (typeof window.__brillantes === "number" ? window.__brillantes : CHANCE_BRILLANTE);
const OR_BRILLANT = 0xFFD45C;

// each house: the body's colour (hue, saturation, lightness; null keeps the Ombre's own violet) and its parure
const TEINTES = {
  dojo:         {corps: null,             parure: "bandeau"},
  albion:       {corps: [38, .7, .72],    parure: "mariniere"},
  germania:     {corps: [212, .42, .62],  parure: "neige"},
  duche:        {corps: [354, .66, .71],  parure: "ceinture"},
  jardin:       {corps: [104, .44, .6],   parure: "fleurs"},
  observatoire: {corps: [232, .52, .52],  parure: "etoiles"}
};
const TRAIT = "rgba(42,30,69,.85)";   // the characters' outline (outils/personnages/scripts/exporter.py)
const DORE = [45, .92, .64];          // the body of a shining Ombre

const OMBRES_REGIONS = (() => {
  const borne = v => v < 0 ? 0 : v > 1 ? 1 : v;
  // a colour from its hue, saturation and lightness, written in RGB (0 to 255): no array made per pixel (a fight's
  // three poses are some 400 000 of them, and a tablet must not wait)
  const RGB = new Float32Array(3);
  function versRgb(h, s, l){
    const c = (1 - Math.abs(2 * l - 1)) * s, x = c * (1 - Math.abs((h / 60) % 2 - 1)), m = l - c / 2;
    const r = h < 60 || h >= 300 ? c : h < 120 ? x : h < 240 ? 0 : x;
    const g = h < 60 ? x : h < 180 ? c : h < 240 ? x : 0;
    const b = h < 120 ? 0 : h < 180 ? x : h < 300 ? c : x;
    RGB[0] = (r + m) * 255; RGB[1] = (g + m) * 255; RGB[2] = (b + m) * 255;
  }
  // every pixel's hue (degrees), saturation and lightness, worked out once for the whole drawing (a pixel never seen
  // stays at 0: grey)
  function teintes(d, n){
    const H = new Float32Array(n), S = new Float32Array(n), L = new Float32Array(n);
    for (let k = 0, i = 0; k < n; k++, i += 4) {
      if (!d[i + 3]) continue;
      const r = d[i] / 255, g = d[i + 1] / 255, b = d[i + 2] / 255;
      const mx = r > g ? (r > b ? r : b) : (g > b ? g : b), mn = r < g ? (r < b ? r : b) : (g < b ? g : b), c = mx - mn, l = (mx + mn) / 2;
      L[k] = l;
      if (!c) continue;
      let h = mx === r ? ((g - b) / c) % 6 : mx === g ? (b - r) / c + 2 : (r - g) / c + 4;
      h *= 60; if (h < 0) h += 360;
      H[k] = h; S[k] = c / (1 - Math.abs(2 * l - 1));
    }
    return {H, S, L};
  }
  // how much a pixel belongs to the violet body (not the outline, the eyes, the pink cheeks or the blue drop)
  function duCorps(h, s, l){
    if (s < .12) return 0;
    const wh = h < 226 || h > 292 ? 0 : h < 236 ? (h - 226) / 10 : h > 284 ? (292 - h) / 8 : 1;
    return wh * (l < .24 ? 0 : l > .4 ? 1 : (l - .24) / .16);
  }

  // where things are on a drawing: its box, the top of its head (antennae and ears left out), its face (the brown of
  // the eyes and the mouth). T: the pixels' colours (teintes)
  function mesurer(d, w, h, T = teintes(d, w * h)){
    const rang = new Array(h).fill(0), v = {haut: h, bas: -1, gauche: w, droite: -1};
    const g = {haut: -1, bas: -1, gauche: w, droite: -1, visage: v};
    for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) {
      const k = y * w + x, a = d[k * 4 + 3];
      if (a < 128) continue;
      rang[y]++;
      if (g.haut < 0) g.haut = y;
      g.bas = y; if (x < g.gauche) g.gauche = x; if (x > g.droite) g.droite = x;
      const hh = T.H[k];
      if (a > 200 && T.L[k] < .3 && T.S[k] > .1 && hh > 12 && hh < 50) { if (y < v.haut) v.haut = y; if (y > v.bas) v.bas = y; if (x < v.gauche) v.gauche = x; if (x > v.droite) v.droite = x; }
    }
    const large = Math.max(...rang);
    g.tete = Math.max(0, rang.findIndex(n => n >= large * .3));
    g.H = Math.max(1, g.bas - g.tete);
    if (v.bas < 0) Object.assign(v, {haut: g.tete + g.H * .25, bas: g.tete + g.H * .55, gauche: g.gauche + (g.droite - g.gauche) * .25, droite: g.droite - (g.droite - g.gauche) * .25});
    g.cx = (v.gauche + v.droite) / 2;
    g.trait = Math.max(1, g.H * .016);
    g.alpha = (x, y) => { x = Math.round(x); y = Math.round(y); return x < 0 || y < 0 || x >= w || y >= h ? 0 : d[(y * w + x) * 4 + 3]; };
    g.sommet = x => { for (let y = 0; y < h; y++) if (g.alpha(x, y) > 128) return y; return g.tete; };   // the top of a column
    return g;
  }

  // ---- the parures, painted on the body only (source-atop): flat colours, a light and a shade, the dark outline
  const ligne = (c, x0, x1, y, ep) => { c.beginPath(); c.moveTo(x0, y); c.lineTo(x1, y); c.lineWidth = ep; c.strokeStyle = TRAIT; c.stroke(); };
  function etoile5(c, x, y, r, rot = -Math.PI / 2){
    c.beginPath();
    for (let i = 0; i < 10; i++) { const a = rot + i * Math.PI / 5, rr = i % 2 ? r * .46 : r; c.lineTo(x + Math.cos(a) * rr, y + Math.sin(a) * rr); }
    c.closePath();
  }
  // spots on the body away from the face and the feet, spread out (the same on every redraw)
  function places(g, n, rayon){
    const l = [], pas = g.H * .17;
    let graine = 7;
    const hasard = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647; };
    const v = g.visage, marge = g.H * .05;
    for (let y = g.tete + pas * .5; y < g.bas - g.H * .1; y += pas)
      for (let x = g.gauche + pas * .5; x < g.droite; x += pas) {
        const px = x + (hasard() - .5) * pas * .6, py = y + (hasard() - .5) * pas * .6;
        if (px > v.gauche - marge - rayon && px < v.droite + marge + rayon && py > v.haut - marge - rayon && py < v.bas + marge + rayon) continue;
        if (g.alpha(px, py) > 200 && g.alpha(px, py - rayon) > 0) l.push([px, py]);
      }
    return l.sort((a, b) => ((a[0] * 7 + a[1] * 13) % 11) - ((b[0] * 7 + b[1] * 13) % 11)).slice(0, n);
  }
  // one more spot on the top of the head, to one side (a flower in the hair, a star on the forehead)
  function surLaTete(g, cote, r){
    const x = g.cx + cote * (g.droite - g.gauche) * .2, y = g.sommet(x) + r * 1.5;
    return g.alpha(x, y) > 200 && y < g.visage.haut - r * .5 ? [x, y] : null;
  }
  const PARURES = {
    // the Dojo's white headband across the forehead, a blue « + » in its middle
    bandeau(c, g){
      const haut = g.sommet(g.cx) + g.H * .03, bas = g.visage.haut - g.H * .012;
      const ep = Math.min(g.H * .085, Math.max(g.H * .05, (bas - haut) * .62)), y = Math.max(haut, (haut + bas) / 2 - ep / 2);
      c.fillStyle = "#FFFFFF"; c.fillRect(g.gauche - 4, y, g.droite - g.gauche + 8, ep);
      c.fillStyle = "#DCD6F0"; c.fillRect(g.gauche - 4, y + ep * .68, g.droite - g.gauche + 8, ep * .32);
      ligne(c, g.gauche - 4, g.droite + 4, y, g.trait); ligne(c, g.gauche - 4, g.droite + 4, y + ep, g.trait);
      const k = ep * .32, x = g.cx, ym = y + ep / 2;
      c.lineCap = "round";
      [[TRAIT, k * .62 + g.trait * 1.4], ["#36B5F0", k * .62]].forEach(([coul, lw]) => {
        c.beginPath(); c.moveTo(x - k, ym); c.lineTo(x + k, ym); c.moveTo(x, ym - k); c.lineTo(x, ym + k);
        c.strokeStyle = coul; c.lineWidth = lw; c.stroke();
      });
    },
    // the sailor's top: navy stripes on the lower body
    mariniere(c, g){
      const y0 = g.visage.bas + g.H * .07, pas = g.H * .085;
      for (let y = y0; y < g.bas + pas; y += pas) {
        c.fillStyle = "#2C4E9E"; c.fillRect(g.gauche - 4, y, g.droite - g.gauche + 8, pas * .48);
        c.fillStyle = "rgba(255,255,255,.28)"; c.fillRect(g.gauche - 4, y, g.droite - g.gauche + 8, pas * .12);
      }
    },
    // snow on the top of the head, its edge in soft drips
    neige(c, g){
      const v = g.visage, bord = g.tete + Math.min(g.H * .2, Math.max(g.H * .09, (v.haut - g.tete) * .55));
      const chapeau = dy => {
        const x0 = g.gauche - 6, x1 = g.droite + 6, n = 6, l = (x1 - x0) / n;
        c.beginPath(); c.moveTo(x0, -10); c.lineTo(x1, -10); c.lineTo(x1, bord + dy);
        for (let i = n - 1; i >= 0; i--) c.quadraticCurveTo(x0 + (i + .5) * l, bord + dy + g.H * (i % 2 ? .07 : .045), x0 + i * l, bord + dy);
        c.closePath();
      };
      chapeau(0); c.fillStyle = "#CFE4F7"; c.fill();
      chapeau(-g.H * .022); c.fillStyle = "#FFFFFF"; c.fill();
      chapeau(0); c.lineWidth = g.trait; c.strokeStyle = TRAIT; c.stroke();
    },
    // a belt in the country's colours: red, white, light blue
    ceinture(c, g){
      const ep = g.H * .045, y0 = g.visage.bas + g.H * .09;
      ["#EF3340", "#FFFFFF", "#7CC7F2"].forEach((coul, i) => { c.fillStyle = coul; c.fillRect(g.gauche - 4, y0 + i * ep, g.droite - g.gauche + 8, ep + .5); });
      ligne(c, g.gauche - 4, g.droite + 4, y0, g.trait); ligne(c, g.gauche - 4, g.droite + 4, y0 + 3 * ep, g.trait);
    },
    // little flowers of the garden, white, pink and yellow
    fleurs(c, g){
      const r0 = g.H * .045, tete = surLaTete(g, 1, r0 * 1.3);
      [...(tete ? [[...tete, 1.3]] : []), ...places(g, tete ? 6 : 7, r0 * 2)].forEach(([x, y, k1 = 1], k) => {
        const coul = ["#FF8FB8", "#FFFFFF", "#FFE07A"][k % 3], rot = k * .7, r = r0 * k1;
        for (const [f, dr] of [[TRAIT, g.trait], [coul, 0]])
          for (let i = 0; i < 5; i++) {
            const a = rot + i * Math.PI * .4;
            c.beginPath(); c.arc(x + Math.cos(a) * r * 1.05, y + Math.sin(a) * r * 1.05, r * .72 + dr, 0, 7); c.fillStyle = f; c.fill();
          }
        c.beginPath(); c.arc(x, y, r * .55, 0, 7); c.fillStyle = "#F7A531"; c.fill();
        c.beginPath(); c.arc(x - r * .15, y - r * .18, r * .2, 0, 7); c.fillStyle = "#FFE9A6"; c.fill();
      });
    },
    // golden stars and tiny white dots: a piece of night sky
    etoiles(c, g){
      const r = g.H * .055, tete = surLaTete(g, -1, r);
      [...(tete ? [tete] : []), ...places(g, tete ? 5 : 6, r * 1.6)].forEach(([x, y], k) => {
        etoile5(c, x, y, r * (k % 2 ? .8 : 1), -Math.PI / 2 + k * .3);
        c.lineWidth = g.trait * 1.6; c.strokeStyle = TRAIT; c.lineJoin = "round"; c.stroke();
        c.fillStyle = g.brillante ? "#FFFFFF" : "#FFD45C"; c.fill();   // white stars on a golden Ombre
      });
      c.fillStyle = "#FFFFFF";
      places(g, 14, g.H * .02).slice(6).forEach(([x, y]) => { c.beginPath(); c.arc(x + g.H * .04, y - g.H * .03, g.H * .011, 0, 7); c.fill(); });
    }
  };
  // a shining Ombre: two soft sheens across its body and three painted twinkles
  function reflets(c, g){
    const w = g.droite - g.gauche;
    c.save(); c.globalAlpha = .32; c.fillStyle = "#FFFFFF";
    c.translate(g.gauche + w * .3, g.tete + g.H * .35); c.rotate(-.6);
    c.fillRect(-w, -g.H * .05, w * 2, g.H * .06); c.fillRect(-w, g.H * .06, w * 2, g.H * .025);
    c.restore();
    [[.18, .12, .05], [.8, .3, .035], [.3, .78, .03]].forEach(([fx, fy, fr]) => {
      const x = g.gauche + w * fx, y = g.tete + g.H * fy, r = g.H * fr;
      c.beginPath();
      for (let i = 0; i < 8; i++) { const a = i * Math.PI / 4, rr = i % 2 ? r * .28 : r; c.lineTo(x + Math.cos(a) * rr, y + Math.sin(a) * rr); }
      c.closePath(); c.fillStyle = "#FFF6C0"; c.fill();
    });
  }

  // one frame: recoloured in place, its parure painted, its dark lines put back on top (each pixel's colour worked out
  // once, before anything changes)
  function peindre(c, ox, oy, w, h, teinte, brillante){
    const im = c.getImageData(ox, oy, w, h), d = im.data, avant = new Uint8ClampedArray(d), n = w * h, T = teintes(avant, n);
    const g = mesurer(avant, w, h, T);
    g.brillante = brillante;
    const corps = brillante ? DORE : teinte.corps;   // a shining Ombre is golden, whatever its house
    if (corps) {
      const [Hc, Sc, Lc] = corps;
      for (let k = 0, i = 0; k < n; k++, i += 4) {
        if (!d[i + 3]) continue;
        const w8 = duCorps(T.H[k], T.S[k], T.L[k]);
        if (!w8) continue;
        versRgb(Hc, brillante ? Sc : borne(Sc * T.S[k] / .72), borne(Lc + (T.L[k] - .68)));
        d[i] += (RGB[0] - d[i]) * w8; d[i + 1] += (RGB[1] - d[i + 1]) * w8; d[i + 2] += (RGB[2] - d[i + 2]) * w8;
      }
      c.putImageData(im, ox, oy);
    }
    c.save(); c.translate(ox, oy); c.beginPath(); c.rect(0, 0, w, h); c.clip();
    c.globalCompositeOperation = "source-atop";
    PARURES[teinte.parure](c, g);
    if (brillante) reflets(c, g);
    c.restore();
    // the outline, the eyes and the mouth over the parure; the soft edge between outline and body in the outline's colour
    const fin = c.getImageData(ox, oy, w, h), f = fin.data;
    for (let k = 0, i = 0; k < n; k++, i += 4) {
      if (avant[i + 3] < 8) continue;
      const hh = T.H[k], s = T.S[k], l = T.L[k];
      if (l < .28) { f[i] = avant[i]; f[i + 1] = avant[i + 1]; f[i + 2] = avant[i + 2]; f[i + 3] = avant[i + 3]; }
      else if (l < .44 && (s < .12 || (hh > 220 && hh < 300))) {
        const t = 1 - (l - .28) / .16;
        f[i] += (42 - f[i]) * t; f[i + 1] += (30 - f[i + 1]) * t; f[i + 2] += (69 - f[i + 2]) * t;
      }
    }
    c.putImageData(fin, ox, oy);
  }

  const COMBATS = [];   // the big tinted drawings made for fights: the latest three are kept (memory)
  // `ensuite`: when the other poses are painted (ms); a fight waits until its Ombre has come in (js/combat.js)
  function ombreTeinte(cle, region, {brillante = false, petit = false, ensuite = 40} = {}){
    const T = typeof JEU !== "undefined" && JEU.textures, teinte = TEINTES[region];
    if (!T || !teinte || !cle || !T.exists("hd_" + cle)) return cle;
    const nom = `${cle}~${region}${brillante ? "~b" : ""}${petit ? "~p" : ""}`;
    if (T.exists("hd_" + nom)) return nom;
    try {
      const src = T.get("hd_" + cle), img = src.getSourceImage();
      // the village shows only the small face (js/jeu/sprites_hd.js): the other small poses are never painted
      const cadres = (petit ? ["face_petit"] : ["face", "touche", "vaincu"]).filter(n => src.has(n)).map(n => src.get(n));
      if (!cadres.length) return cle;
      const W = cadres.reduce((s, f) => s + f.cutWidth + 2, 0), H = Math.max(...cadres.map(f => f.cutHeight));
      const ct = T.createCanvas("hd_" + nom, W, H);
      if (!ct) return cle;
      const c = ct.getContext();
      let x = 0;
      const places = cadres.map(f => {
        c.drawImage(img, f.cutX, f.cutY, f.cutWidth, f.cutHeight, x, 0, f.cutWidth, f.cutHeight);
        ct.add(f.name, 0, x, 0, f.cutWidth, f.cutHeight);
        const ici = x; x += f.cutWidth + 2;
        return () => peindre(c, ici, 0, f.cutWidth, f.cutHeight, teinte, brillante);
      });
      // the face at once (what is seen first); the other poses later, one per turn of the browser, so that nothing
      // stands still on the screen (a hit or a « vaincu » only comes seconds later)
      try { places[0](); }
      catch (e) { T.remove("hd_" + nom); return cle; }   // a drawing the browser will not read back: no half-made texture
      ct.refresh(); ct.setFilter(Phaser.Textures.FilterMode.LINEAR);
      places.slice(1).forEach((p, k) => setTimeout(() => {
        if (!T.exists("hd_" + nom)) return;
        try { p(); ct.refresh(); } catch (e) { /* that pose keeps its own colour */ }
      }, ensuite + 300 * k));
      if (!petit) { COMBATS.push(nom); while (COMBATS.length > 3) { const k = "hd_" + COMBATS.shift(); if (T.exists(k)) T.remove(k); } }
      return nom;
    } catch (e) { return cle; }   // the Ombre keeps its own colour
  }
  return {ombreTeinte, peindre, mesurer};
})();
const ombreTeinte = OMBRES_REGIONS.ombreTeinte;

// golden twinkles that come and go around a shining Ombre, as long as it is there (nothing in the recette)
function scintillerOmbre(scene, s, {cadence = 380, taille = .2} = {}){
  if (TEST || !scene || !s) return null;
  const ev = scene.time.addEvent({delay: cadence, loop: true, callback: () => {
    if (!s.active) return ev.remove();
    if (!s.visible || s.alpha < .5 || calme()) return;
    const h = s.displayHeight, w = s.displayWidth, a = Math.random() * Math.PI * 2, r = .45 + Math.random() * .2;
    const x = s.x + Math.cos(a) * w * r, y = s.y - h * .5 + Math.sin(a) * h * r * .8, t = h * taille * (.7 + Math.random() * .5);
    const F = EFFETS_HD.preparer(scene, x, y, h, s.depth + .4);
    F.anime("etincelle", t, 650, (im, k, hh) => { im.setPosition(x, y - h * .06 * k).setAngle(80 * k); hh(t * Math.sin(Math.PI * k)); },
      {tint: Math.random() < .5 ? 0xFFE38A : 0xFFFFFF});
  }});
  scene.events.once("shutdown", () => ev.remove());
  return ev;
}
