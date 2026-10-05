/* Académia : l'attaque de chaque famille, une gerbe qui vole du compagnon jusqu'à l'Ombre avec ses pièces dessinées
   (outils/effets/) teintes de ses couleurs : cubes et chiffres qui pleuvent pour Matty, plumes qui tourbillonnent pour
   Plumix, cailloux qui roulent pour Bärli, étoiles et anneaux de lumière pour Léiwchen, pions qui sautent en L pour
   Cavalin, feuilles et pétales en vague pour Poussik. Une pièce pas encore dessinée cède la place à la suivante.
   Rend la durée du vol en ms (le choc sur l'Ombre suit, js/jeu/combat_spectacle.js). */
const GERBES = (() => {
  const arc = (t, h) => Math.sin(Math.PI * t) * h;
  const TRAJETS = {   // the place of piece i at t (0 to 1), from (x0, y0) to (x1, y1); T is a creature's height
    pluie: (t, i, a, b, T) => [a.x + (b.x - a.x) * t + (i % 3 - 1) * T * .08 * t, a.y + (b.y - a.y) * t - arc(t, T * (.55 + .1 * (i % 3)))],
    tourbillon: (t, i, a, b, T) => { const r = T * .16 * (1 - t), q = t * 4 * Math.PI + i * 1.3;
      return [a.x + (b.x - a.x) * t + Math.cos(q) * r, a.y + (b.y - a.y) * t + Math.sin(q) * r - arc(t, T * .15)]; },
    roule: (t, i, a, b, T) => [a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t + T * .12 - Math.abs(Math.sin(2 * Math.PI * t + i * .4)) * T * .2],
    rayon: (t, i, a, b, T) => [a.x + (b.x - a.x) * t + (i % 2 ? 1 : -1) * T * .04 * Math.sin(t * 9), a.y + (b.y - a.y) * t - arc(t, T * .06)],
    enL: (t, i, a, b, T) => { const u = Math.min(1, t * 1.6), v = Math.max(0, (t - .55) / .45), d = (i % 3 - 1) * T * .06;
      return [a.x + (b.x - a.x) * u + d, a.y - T * .25 * Math.min(1, t * 3) + (b.y - a.y + T * .25) * v]; },
    vague: (t, i, a, b, T) => [a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t - arc(t, T * .25) + Math.sin(t * 3 * Math.PI + i) * T * .07]
  };
  // pieces: [names, first existing drawing wins], size (share of T), tint
  return {
    TRAJETS,
    matty: {trajet: "pluie", n: 8, cadence: 30, pieces: [[["cube", "etoile"], [.13, .17], null], [["chiffre1", "etincelle"], [.13, .17], null],
      [["chiffre2", "etincelle"], [.13, .17], null], [["chiffre3", "etincelle"], [.13, .17], null], [["etincelle"], [.1, .14], 0xBFE6FF]]},
    plumix: {trajet: "tourbillon", n: 8, cadence: 28, pieces: [[["plume", "nuage"], [.14, .19], null], [["etincelle"], [.1, .14], 0xFFE07A], [["nuage"], [.1, .14], 0xE3D6FF]]},
    barli: {trajet: "roule", n: 6, cadence: 40, pieces: [[["caillou", "nuage"], [.12, .17], null], [["etincelle"], [.1, .13], 0x9BF0B8]]},
    leiwchen: {trajet: "rayon", n: 8, cadence: 24, pieces: [[["etoile"], [.12, .16], null], [["etincelle"], [.12, .16], 0xFFD45C], [["anneau"], [.16, .22], 0xFFE38A]]},
    cavalin: {trajet: "enL", n: 6, cadence: 40, pieces: [[["pion", "etoile"], [.14, .18], null], [["etincelle"], [.1, .14], 0xC9A227]]},
    poussik: {trajet: "vague", n: 8, cadence: 30, pieces: [[["feuille"], [.12, .16], null], [["brin"], [.12, .16], null], [["coeur"], [.08, .11], 0xFF9CC4]]}
  };
})();

SceneCombat.prototype.attaqueFamille = function(famille, fort = false, ultime = false){
  const a = this.moi, b = this.lui;
  if (TEST || calme() || !a || !b || !a.active || !b.active) return 0;
  const G = GERBES[famille] || GERBES.poussik, T = Math.max(a.displayHeight, b.displayHeight);
  const de = {x: a.x + a.displayWidth * .15, y: a.y - a.displayHeight * .6}, vers = {x: b.x, y: b.y - b.displayHeight * .5};
  const F = EFFETS_HD.preparer(this, de.x, de.y, T, 7), trajet = GERBES.TRAJETS[G.trajet];
  const n = ultime ? G.n * 2 : fort ? Math.round(G.n * 1.5) : G.n, vol = ultime ? 420 : fort ? 360 : 300;
  const entre = (m, k) => m + Math.random() * (k - m);
  for (let i = 0; i < n; i++) {
    const [noms, taille, teinte] = G.pieces[i % G.pieces.length];
    const nom = noms.find(k => this.textures.exists("fx_" + k) || this.textures.exists("fx_" + k + "_p"));
    if (!nom) continue;
    const h = T * entre(...taille) * 1.5 * (ultime ? 1.35 : fort ? 1.15 : 1), rot0 = entre(-30, 30), rot = entre(180, 420) * (i % 2 ? 1 : -1);
    F.anime(nom, h, vol, (im, t, hh) => {
      const [x, y] = trajet(t, i, de, vers, T);
      im.setPosition(x, y).setAngle(nom === "anneau" ? 0 : rot0 + rot * t).setAlpha(t < .85 ? 1 : 1 - (t - .85) / .15);
      hh(h * (t < .15 ? .4 + 4 * t : 1));
    }, {delai: i * G.cadence, tint: teinte, dz: .1 * (i % 3)});
  }
  return vol + (n - 1) * G.cadence;
};
