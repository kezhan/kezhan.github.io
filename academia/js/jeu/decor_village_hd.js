/* Académia : le village en haute définition. Le sol (herbe, chemins, place, pied des hautes herbes) est fait de tuiles
   posées d'après les rectangles de la carte (js/jeu/carte.js, elements) ; maisons, petits lieux (puits, lanternes,
   banc...), arbres, forêt du bord, rochers, mare, plage et barque, fleurs, détails du pré et hautes herbes sont des
   pièces rangées dans deux planches (table : js/jeu/decor_village_hd_pieces.js), triées par ligne comme les
   personnages : un objet prend profondeur(sa dernière ligne, 0), une touffe passe devant les pieds de qui se tient
   dans sa case. Dessins : outils/village/ (à nous, style plat des packs Kenney). Deux versions : assets/hd/village
   (8 pixels par unité du monde) et assets/leger/village (4) ; même code. Ce qui bouge est dans
   decor_village_hd_vivant.js, ce que le jeu appelle en marchant dans decor_village_hd_jeu.js.
   Contrat : IMAGES_VILLAGE, chargerVillageHD(load, version), dessinerVillageHD(scene, carte) qui rend
   {etiquettes, maisons, ecrans, touffes, eau}. */
const IMAGES_VILLAGE = Object.keys(PLANCHES_VILLAGE).map(n => ({cle: "vh_" + n, fichier: n + ".png"}));

// version 'hd' or 'leger': same drawing, same code (already loaded images are skipped)
function chargerVillageHD(load, version){
  const dossier = `assets/${version === "leger" ? "leger" : "hd"}/village/`;
  IMAGES_VILLAGE.forEach(i => { if (!load.textureManager.exists(i.cle)) load.image(i.cle, dossier + i.fichier); });
}
// the whole village in world units (a tile = CASE = 16)
function dessinerVillageHD(scene, carte){ return VILLAGE_HD.dessiner(scene, carte); }

const VILLAGE_HD = (() => {
  const PX = 8;   // HD pixels per world unit
  const hasard = (x, y, k = 0) => { const n = Math.sin(x * 127.1 + y * 311.7 + k * 74.7) * 43758.5453; return n - Math.floor(n); };
  const choisir = (l, x, y, k) => l[Math.floor(hasard(x, y, k) * l.length)];
  const grille = v => Array.from({length: HAUT}, () => Array(LARG).fill(v));
  const dans = (g, x, y) => y >= 0 && y < HAUT && x >= 0 && x < LARG && g[y][x];
  const OBJETS = ["puits", "lanterne", "poteau", "banc", "potager", "tonneaux"];   // little places that block their tiles

  // the cells each kind of ground covers, from the map's rectangles ("pris": something stands there)
  function grilles(carte){
    const g = {chemin: grille(false), place: grille(false), touffe: grille(false), pris: grille(false)};
    const remplir = (cle, x0, y0, w, h) => { for (let y = y0; y < y0 + h; y++) for (let x = x0; x < x0 + w; x++) if (y >= 0 && y < HAUT && x >= 0 && x < LARG) g[cle][y][x] = true; };
    carte.elements.forEach(e => {
      if (e.type === "chemin" || e.type === "place") remplir(e.type, e.x, e.y, e.w, e.h);
      else if (e.type === "barque") remplir("pris", e.x - 1, e.y - 1, e.w + 2, e.h + 2);   // the boat and its beach
      else if (e.type !== "touffe" && e.type !== "fleur") remplir("pris", e.x, e.y, e.w, e.h);
    });
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) g.touffe[y][x] = !!(carte.herbe && carte.herbe[y][x]);
    return g;
  }

  // ---------- the ground ----------
  // a soft wash of greens painted once on a small canvas (8 pixels per tile) and stretched over the map
  function herbeFond(scene){
    if (scene.textures.exists("vh_herbe_fond")) return;
    const k = 8, c = document.createElement("canvas"); c.width = LARG * k; c.height = HAUT * k;
    const g = c.getContext("2d");
    g.fillStyle = "#80C764"; g.fillRect(0, 0, c.width, c.height);
    const tache = (x, y, r, couleur, a) => {
      const d = g.createRadialGradient(x, y, 0, x, y, r);
      d.addColorStop(0, `rgba(${couleur},${a})`); d.addColorStop(.6, `rgba(${couleur},${a * .55})`); d.addColorStop(1, `rgba(${couleur},0)`);
      g.fillStyle = d; g.fillRect(x - r, y - r, 2 * r, 2 * r);
    };
    for (let i = 0; i < 90; i++) {
      const x = hasard(i, 1) * c.width, y = hasard(i, 2) * c.height, r = (1.5 + hasard(i, 3) * 3.5) * k;
      if (hasard(i, 4) < .55) tache(x, y, r, "150,214,118", .35); else tache(x, y, r, "98,176,82", .3);
    }
    // the forest's shade along the edges
    const bord = (x0, y0, x1, y1) => { const d = g.createLinearGradient(x0, y0, x1, y1); d.addColorStop(0, "rgba(30,96,62,.85)"); d.addColorStop(1, "rgba(30,96,62,0)"); return d; };
    const e = 3.2 * k;
    g.fillStyle = bord(0, 0, 0, e); g.fillRect(0, 0, c.width, e);
    g.fillStyle = bord(0, c.height, 0, c.height - e); g.fillRect(0, c.height - e, c.width, e);
    g.fillStyle = bord(0, 0, e, 0); g.fillRect(0, 0, e, c.height);
    g.fillStyle = bord(c.width, 0, c.width - e, 0); g.fillRect(c.width - e, 0, e, c.height);
    scene.textures.addCanvas("vh_herbe_fond", c).setFilter(Phaser.Textures.FilterMode.LINEAR);
  }

  // a sheet of tiles on the double grid: a tile sits on a corner of four cells and draws what they make together
  // (mask: top left 1, top right 2, bottom left 4, bottom right 8); 6 x 4 slots of 136 for 128 (hd) drawn
  function coucheDouble(scene, nom, masque, profondeur, variante){
    const cle = "vh_sol_" + nom, src = scene.textures.get(cle).getSourceImage();
    const pas = src.width / 6, t = Math.round(pas * 128 / 136), m = (pas - t) / 2;
    const data = [];
    for (let vy = 0; vy <= HAUT; vy++) {
      const ligne = [];
      for (let vx = 0; vx <= LARG; vx++) {
        const n = (dans(masque, vx - 1, vy - 1) ? 1 : 0) | (dans(masque, vx, vy - 1) ? 2 : 0) | (dans(masque, vx - 1, vy) ? 4 : 0) | (dans(masque, vx, vy) ? 8 : 0);
        ligne.push(n ? variante(n, vx, vy) : -1);
      }
      data.push(ligne);
    }
    const carte = scene.make.tilemap({data, tileWidth: t, tileHeight: t});
    const ts = carte.addTilesetImage(cle, cle, t, t, m, 2 * m);
    return carte.createLayer(0, ts, -CASE / 2, -CASE / 2).setScale(CASE / t).setDepth(profondeur);
  }
  // one tile per cell (the little things of the grass)
  function coucheCases(scene, cle, data, profondeur){
    const src = scene.textures.get(cle).getSourceImage();
    const pas = src.width / 6, t = Math.round(pas * 128 / 136), m = (pas - t) / 2;
    const carte = scene.make.tilemap({data, tileWidth: t, tileHeight: t});
    const ts = carte.addTilesetImage(cle, cle, t, t, m, 2 * m);
    return carte.createLayer(0, ts, 0, 0).setScale(CASE / t).setDepth(profondeur);
  }

  function sol(scene, carte, g){
    herbeFond(scene);
    scene.add.image(0, 0, "vh_herbe_fond").setOrigin(0).setDisplaySize(LARG * CASE, HAUT * CASE).setDepth(0);
    const details = Array.from({length: HAUT}, (_, y) => Array.from({length: LARG}, (_, x) => {
      if (g.chemin[y][x] || g.place[y][x] || g.touffe[y][x] || g.pris[y][x]) return -1;
      return hasard(x, y, 5) < .5 ? Math.floor(hasard(x, y, 6) * 12) : -1;
    }));
    coucheCases(scene, "vh_sol_details", details, 1);
    coucheDouble(scene, "touffe", g.touffe, 2, n => n);
    coucheDouble(scene, "chemin", g.chemin, 3, (n, x, y) => {
      const r = hasard(x, y, 7);
      if (n === 15) return r < .45 ? 16 + Math.floor(hasard(x, y, 8) * 4) : 15;
      const bord = {12: 20, 3: 21, 10: 22, 5: 23}[n];
      return bord && r < .3 ? bord : n;
    });
    coucheDouble(scene, "place", g.place, 4, (n, x, y) => n === 15 ? 15 + Math.floor(hasard(x, y, 9) * 5) : n);
    carte.elements.filter(e => e.type === "place").forEach(e => poser(scene, "mosaique", (e.x + e.w / 2) * CASE, (e.y + e.h / 2) * CASE, 5));
  }

  // ---------- pieces: a frame of a sheet, posed by its anchor, sized in world units ----------
  function cadre(scene, nom){
    const p = PIECES_VILLAGE[nom], cle = p && "vh_" + p[0];
    if (!p || !scene.textures.exists(cle)) return null;
    const t = scene.textures.get(cle);
    if (!t.has(nom)) { const k = t.source[0].width / PLANCHES_VILLAGE[p[0]]; t.add(nom, 0, p[1] * k, p[2] * k, p[3] * k, p[4] * k); }
    return cle;
  }
  function poser(scene, nom, x, y, profondeur, opts = {}){
    const cle = cadre(scene, nom); if (!cle) return null;
    const p = PIECES_VILLAGE[nom];
    const im = scene.add.image(x, y, cle, nom).setOrigin(p[5], p[6]).setDepth(profondeur);
    im.setScale((opts.echelle || 1) * p[3] / PX / im.width);
    if (opts.miroir) im.setFlipX(true);
    return im;
  }
  // a named point of a drawing (chimney, lamp, flag...), in world units from where the piece was posed
  const point = (nom, quoi, x, y) => { const q = POINTS_VILLAGE[nom] && POINTS_VILLAGE[nom][quoi]; return q && {x: x + q[0], y: y + q[1]}; };
  const pied = e => [(e.x + e.w / 2) * CASE, (e.y + e.h) * CASE];

  // houses, the Dojo's sign, the little places, rocks: the foot in the middle of the bottom of their cells. What can
  // hide a character goes into rendu.ecrans (decor_village_hd_jeu.js makes it see-through), a house and the Dojo's sign
  // into rendu.maisons (a touch on their drawing leads to the door)
  function batiments(scene, carte, rendu){
    carte.elements.forEach(e => {
      const nom = e.type === "maison" ? "maison_" + e.region : e.type === "panneau" ? "panneau_dojo" : e.type === "rochers" ? "rochers"
        : OBJETS.includes(e.type) ? e.type : null;
      if (!nom) return;
      const [x, y] = pied(e), ligne = e.y + e.h - 1, im = poser(scene, nom, x, y, profondeur(ligne, 0));
      if (!im) return;
      rendu.ecrans.push(im);
      if (e.type === "maison" || e.type === "panneau") rendu.maisons.push({region: e.region || "dojo", nom, x, y, ligne, im, bornes: im.getBounds()});
      if (e.type === "maison") {
        const q = point(nom, "etiquette", x, y);
        rendu.etiquettes[e.region] = q ? {x: q.x, y: q.y - 6} : {x, y: im.getTopCenter().y - 5};
        const d = point(nom, "drapeau", x, y);   // the flag of the tower, waved by the wind
        if (d) rendu.drapeau = poser(scene, "drapeau", d.x, d.y, profondeur(ligne, 1));
      }
      if (e.type === "lanterne") rendu.lumieres.push({...point(nom, "lumiere", x, y), profondeur: profondeur(ligne, 1)});
    });
  }

  // trees of the map, the same kind for the same tree even when it is moved: the first big one is a round tree, the
  // second a cherry tree; the others by their name in the map and their rank among those of that name
  const SORTES = {buisson1: ["arbre_fleuri", "arbre_rond"], buisson2: ["arbre_pommier", "arbre_oranger"], buisson3: ["arbre_pommier", "arbre_oranger"],
    sapin1: ["sapin", "sapin_haut"], sapin2: ["sapin_sombre", "sapin"], grandArbre: ["grand_arbre", "grand_cerisier"]};
  function arbres(scene, carte, rendu){
    const rang = {};
    carte.elements.filter(e => e.type === "arbre").forEach(e => {
      const k = rang[e.nom] = (rang[e.nom] || 0) + 1, l = SORTES[e.nom] || SORTES.buisson1;
      const [x, y] = pied(e), im = poser(scene, l[(k - 1) % l.length], x, y, profondeur(e.y + e.h - 1, 0), {miroir: hasard(e.x, e.y, 3) < .5});
      if (im) rendu.ecrans.push(im);
    });
  }

  // the forest around the map, close enough that nothing shows behind it. Along the sides, the trees stand in the
  // two edge columns and always behind whoever walks next to them (profondeur(row - 2)); snowy firs by Germania, cherry
  // trees by the Jardin des Éclats. Along the bottom, low round trees and bushes: whoever walks just above keeps in sight.
  const HAUTS = ["foret_sapin", "foret_rond", "foret_sapin_sombre", "foret_rond_sombre"];
  const NEIGE = ["foret_sapin_neige", "foret_sapin_neige_sombre"], CERISIERS = ["foret_cerisier", "foret_rond", "foret_cerisier_vif"];
  const sorteForet = (x, y, k) => {
    if (x <= 12 && y <= 9) return choisir(CERISIERS, x, y, 11 + k);   // the top left corner, by the Jardin
    if (x <= 1 && y >= 11 && y <= 23) return choisir(NEIGE, x, y, 11 + k);   // the left side, by Germania
    return choisir(HAUTS, x + k, y, 11);
  };
  function foret(scene, carte){
    carte.elements.filter(e => e.type === "bordure").forEach(e => {
      const cote = e.y + e.h >= HAUT ? "bas" : e.y <= 0 ? "haut" : "cote", miroir = k => hasard(e.x, e.y, 12 + k) < .5;
      if (cote === "haut") [[.25, 1, e.y], [.75, 2, e.y + 1]].forEach(([fx, fy, ligne], k) => {
        const x = (e.x + e.w * fx) * CASE + (hasard(e.x, e.y, 10 + k) - .5) * 6, y = (e.y + fy) * CASE - (k ? 0 : 2);
        poser(scene, sorteForet(e.x, e.y + k, k), x, y, profondeur(ligne, 0), {miroir: miroir(k)});
      });
      if (cote === "cote") [e.y, e.y + 1].forEach((ligne, k) => {
        const bord = e.x < LARG / 2 ? 10 : LARG * CASE - 10, x = bord + (hasard(e.x, ligne, 10) - .5) * 6;
        poser(scene, sorteForet(e.x, ligne, k), x, (ligne + 1) * CASE, profondeur(ligne - 2, 0), {miroir: hasard(e.x, ligne, 12) < .5});
      });
      if (cote === "bas") {   // one block in three with its flowered or berry bush in front
        const devant = Math.floor(e.x / 2) % 3 === 1, h = (k) => hasard(e.x, e.y, 20 + k), s = .9 + h(1) * .18;
        const buisson = h(2) < .5 ? "foret_buisson" : "foret_buisson_sombre", arbre = h(3) < .5 ? "foret_bas" : "foret_bas_sombre";
        poser(scene, buisson, (e.x + .5) * CASE + (h(4) - .5) * 6, devant ? HAUT * CASE - 1 : (e.y + 1) * CASE - 2 + (h(5) - .5) * 4,
          devant ? profondeur(e.y + 1, 3) : profondeur(e.y, 0), {miroir: h(6) < .5, echelle: devant ? .9 + h(7) * .1 : s});
        poser(scene, arbre, (e.x + 1.5) * CASE + (h(8) - .5) * 6, HAUT * CASE - 2 + (h(9) - .5) * 4, profondeur(e.y + 1, 0),
          {miroir: h(10) < .5, echelle: Math.min(s, 1.06)});
      }
    });
  }

  // the pond: its oval water inside the blocked tiles, reeds in a corner and by the right bank, a duckling; the beach
  // under the boat, below the pond's sandy south shore
  function mare(scene, carte, rendu){
    carte.elements.filter(e => e.type === "mare").forEach(e => {
      let x0 = LARG, y0 = HAUT, x1 = -1, y1 = -1;
      for (let y = e.y; y < e.y + e.h; y++) for (let x = e.x; x < e.x + e.w; x++)
        if (carte.bloque[y][x]) { x0 = Math.min(x0, x); y0 = Math.min(y0, y); x1 = Math.max(x1, x); y1 = Math.max(y1, y); }
      if (x1 < 0) return;
      poser(scene, "mare", (x0 + x1 + 1) / 2 * CASE, (y0 + y1 + 1) / 2 * CASE, 6);
      rendu.ecrans.push(poser(scene, "roseaux", x0 * CASE + 6, (y0 + 1) * CASE - 3, profondeur(y0, 0)));
      rendu.ecrans.push(poser(scene, "roseaux_petits", (x1 + 1) * CASE - 5, (y0 + 1.2) * CASE, profondeur(y0 + 1, 0), {miroir: true}));
      const canard = poser(scene, "canard", (x0 + 1.2) * CASE, (y0 + 1.7) * CASE, profondeur(y0 + 1, 0));
      rendu.eau = {x0: x0 * CASE, y0: y0 * CASE, x1: (x1 + 1) * CASE, y1: (y1 + 1) * CASE, canard};
    });
    carte.elements.filter(e => e.type === "barque").forEach(e => {   // the beach runs from the pond's south shore to the boat
      const [x, y] = pied(e), eau = rendu.eau;
      poser(scene, "plage", eau ? (x + (eau.x0 + eau.x1) / 2) / 2 : x, eau ? eau.y1 - 8 : e.y * CASE - CASE, 5);
      rendu.ecrans.push(poser(scene, "barque", x, y, profondeur(e.y + e.h - 1, 0)));
    });
  }

  // flowers of the meadow: low, one walks over them
  const BOUQUETS = ["fleurs_rouge", "fleurs_jaune", "fleurs_rose", "fleurs_violet", "fleurs_bleu", "fleurs_blanc"];
  function fleurs(scene, carte){
    carte.elements.filter(e => e.type === "fleur").forEach(e => {
      const nom = e.sorte === 1 ? "trefles" : e.sorte === 2 ? "paquerettes" : choisir(BOUQUETS, e.x, e.y, 13);
      poser(scene, nom, e.x * CASE + 8 + (hasard(e.x, e.y, 14) - .5) * 4, (e.y + 1) * CASE - 2, profondeur(e.y, 0), {miroir: hasard(e.x, e.y, 15) < .5});
    });
  }

  // the meadow's bigger things, flat on the grass (clover, mushrooms, a little stump, flat stones), where it is empty:
  // on free grass at least one tile away from anything, three tiles apart from each other
  const PRE = ["tapis_trefles", "pierres_plates", "champignons", "tapis_trefles", "souche", "tapis_trefles", "champignons"];
  function pre(scene, carte, g){
    const occupe = (x, y) => x < 2 || y < 2 || x >= LARG - 2 || y >= HAUT - 2 || dans(g.chemin, x, y) || dans(g.place, x, y) || dans(g.touffe, x, y)
      || dans(g.pris, x, y) || carte.bloque[y][x] || carte.portes.some(p => p.x === x && p.y === y);
    const fleur = new Set(carte.elements.filter(e => e.type === "fleur").map(e => e.x + "," + e.y));
    const poses = [];
    for (let y = 2; y < HAUT - 2; y++) for (let x = 2; x < LARG - 2; x++) {
      let libre = !fleur.has(x + "," + y);
      for (let b = y - 1; libre && b <= y + 1; b++) for (let a = x - 1; a <= x + 1; a++) if (occupe(a, b)) { libre = false; break; }
      if (!libre || hasard(x, y, 30) > .3 || poses.some(p => Math.max(Math.abs(p.x - x), Math.abs(p.y - y)) < 3)) continue;
      poses.push({x, y});
      const nom = choisir(PRE, x, y, 31);
      poser(scene, nom, x * CASE + 8 + (hasard(x, y, 32) - .5) * 6, (y + 1) * CASE - 3, 5, {miroir: hasard(x, y, 33) < .5});
    }
  }

  // the tall grass, two tufts per tile: a tall one in the middle of the tile, behind whoever stands in it, and a
  // short one at its bottom, in front of the feet (profondeur(y, 8))
  function herbes(scene, carte, rendu){
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) {
      const r = carte.herbe && carte.herbe[y][x]; if (!r) continue;
      const v = hasard(x, y, 16) < .3 ? 1 : hasard(x, y, 17) < .5 ? 0 : 2, j = (hasard(x, y, 18) - .5) * 3;
      const fond = poser(scene, `herbe_${r}_fond_${v}`, x * CASE + 8 + j, y * CASE + 9, profondeur(y, 2), {miroir: hasard(x, y, 19) < .5, echelle: .94 + hasard(x, y, 21) * .14});
      const devant = poser(scene, `herbe_${r}_${(v + 1) % 3}`, x * CASE + 8 - j, (y + 1) * CASE, profondeur(y, 8), {miroir: hasard(x, y, 20) < .5, echelle: .96 + hasard(x, y, 22) * .1});
      const l = [fond, devant].filter(Boolean);
      l.forEach((t, k) => { t.phase = x * .9 + y * .45 + k * .6; t.choc = -1e9; });
      if (l.length) rendu.touffes[x + "," + y] = l;
    }
  }

  function dessiner(scene, carte){
    const rendu = {etiquettes: {}, maisons: [], ecrans: [], touffes: {}, lumieres: [], carte};
    if (!IMAGES_VILLAGE.every(i => scene.textures.exists(i.cle))) return rendu;   // not loaded: nothing drawn, the caller keeps its own map
    IMAGES_VILLAGE.forEach(i => scene.textures.get(i.cle).setFilter(Phaser.Textures.FilterMode.LINEAR));
    const g = grilles(carte);
    sol(scene, carte, g);
    batiments(scene, carte, rendu); arbres(scene, carte, rendu); foret(scene, carte);
    mare(scene, carte, rendu); fleurs(scene, carte); pre(scene, carte, g); herbes(scene, carte, rendu);
    rendu.ecrans = rendu.ecrans.filter(Boolean);
    rendu.ecrans.forEach(im => { im.bornes = im.getBounds(); });
    rendu.etiquettesDepart = {...rendu.etiquettes};   // a sign is placed again with its real width when its text is written
    Object.keys(rendu.etiquettes).forEach(r => { rendu.etiquettes[r] = VILLAGE_HD_JEU.placer(rendu, r, rendu.etiquettesDepart[r]); });
    scene.villageHD = rendu;
    VIVANT_HD.animer(scene, carte, rendu);
    return rendu;
  }
  return {dessiner, poser, point, hasard};
})();
