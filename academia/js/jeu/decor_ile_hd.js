/* Académia : l'Île des Bâtisseurs en haute définition, dessinée par le code du village (js/jeu/decor_village_hd.js,
   qui appelle ILE_HD quand la carte est celle de l'île, js/jeu/carte_ile.js).
   1. Le sol : la mer du large, une tuile qui se répète et dérive doucement ; la terre de l'île posée dessus en tuiles
      à double grille (l'eau claire du rivage, l'écume, le sable mouillé puis sec) ; la vague qui vient mourir sur le
      sable et bat doucement ; l'herbe posée sur le sable, bordée comme les chemins du village (les sentiers et la
      plage sont le sable laissé entre les herbes) ; les petites choses du pré et le pied des hautes herbes.
   2. Les pièces : le ponton et sa barque à quai, les six terrains et leur panneau de chantier, les palmiers, les
      rochers, le tas de planches et la brouette de la cheffe, des oyats sur la plage.
   3. Ce qui vit : la vague, des reflets sur la mer, la barque qui se berce, les palmiers dans le vent, de temps en
      temps une mouette qui passe. Rien ne bouge quand l'enfant demande le calme.
   Dessins : outils/village/scripts/ile.py (table : js/jeu/decor_ile_hd_pieces.js) ; assets/hd/ile et assets/leger/ile.
   Contrat : chargerIleHD(load, version), chargerTraverseeHD(load, version), ILE_HD.pret(scene), ILE_HD.sol,
   ILE_HD.pieces, ILE_HD.animer (rendu.ileHD : {barque, ponton, terrains: {id: {terrain, panneau}}}). */
const IMAGES_ILE_SOL = ["sol_plage", "sol_ecume", "sol_herbe_ile", "mer"];   // images of their own, loaded as vh_<name>
const dossierIle = version => `assets/${version === "leger" ? "leger" : "hd"}/ile/`;

// what the island shows (its ground, its sheet of pieces); already loaded images are skipped
function chargerIleHD(load, version){
  [...IMAGES_ILE_SOL, "ile"].forEach(n => { if (!load.textureManager.exists("vh_" + n)) load.image("vh_" + n, dossierIle(version) + n + ".png"); });
}
// what the crossing shows (js/jeu/traversee.js): its backdrop, the boat seen from the side, the gulls and waves
function chargerTraverseeHD(load, version){
  ["traversee_fond", "ile_barque", "ile"].forEach(n => { if (!load.textureManager.exists("vh_" + n)) load.image("vh_" + n, dossierIle(version) + n + ".png"); });
}

const ILE_HD = (() => {
  const pret = scene => [...IMAGES_ILE_SOL, "ile"].every(n => scene.textures.exists("vh_" + n));
  const poser = (...a) => VILLAGE_HD.poser(...a);

  // ---------- the ground ----------
  function sol(scene, carte, g, O){
    [...IMAGES_ILE_SOL, "ile"].forEach(n => scene.textures.get("vh_" + n).setFilter(Phaser.Textures.FilterMode.LINEAR));
    const grille = f => Array.from({length: HAUT}, (_, y) => Array.from({length: LARG}, (_, x) => f(carte.sol[y][x], x, y)));
    const terre = grille(v => v !== "eau" && v !== "ponton"), pre = grille(v => v === "herbe");
    // the open sea: one tile of 4 x 4 cells, repeated (it drifts in ILE_HD.animer)
    const mer = scene.textures.get("vh_mer").getSourceImage();
    const fond = scene.add.tileSprite(0, 0, LARG * CASE, HAUT * CASE, "vh_mer").setOrigin(0).setDepth(0);
    fond.setTileScale(4 * CASE / mer.width);
    const h = O.hasard;
    const plage = O.coucheDouble(scene, "plage", terre, .6, (n, x, y) => {
      const r = h(x, y, 41);
      if (n === 15) return r < .16 ? 16 + Math.floor(h(x, y, 42) * 4) : 15;
      const bord = {12: 20, 3: 21, 10: 22, 5: 23}[n];
      return bord && r < .25 ? bord : n;
    });
    const ecume = O.coucheDouble(scene, "ecume", terre, .8, n => n);
    O.coucheDouble(scene, "herbe_ile", pre, 1, (n, x, y) => n === 15 && h(x, y, 43) < .3 ? 16 + Math.floor(h(x, y, 44) * 4) : n);
    const details = Array.from({length: HAUT}, (_, y) => Array.from({length: LARG}, (_, x) => {
      if (g.pris[y][x] || g.touffe[y][x]) return -1;
      return h(x, y, 5) < .45 ? Math.floor(h(x, y, 6) * 12) : -1;
    }));
    O.coucheCases(scene, "vh_sol_details", details, 1.5);
    O.coucheDouble(scene, "touffe", g.touffe, 2, n => n);
    scene.ileSol = {fond, plage, ecume};
  }

  // ---------- the pieces ----------
  const pied = e => [(e.x + e.w / 2) * CASE, (e.y + e.h) * CASE];
  function pieces(scene, carte, rendu){
    const r = rendu.ileHD = {terrains: {}};
    const ecran = im => { if (im) rendu.ecrans.push(im); return im; };
    carte.elements.forEach(e => {
      if (e.type === "ponton") r.ponton = poser(scene, "ponton", e.x * CASE, e.y * CASE, 3);   // flat: one walks on it
      else if (e.type === "barque_quai") { const [x, y] = pied(e); r.barque = ecran(poser(scene, "barque_quai", x, y, profondeur(e.y, 0))); }
      else if (e.type === "terrain") {
        const [x, y] = pied(e);
        (r.terrains[e.chantier] = r.terrains[e.chantier] || {}).terrain = ecran(poser(scene, "terrain", x, y, profondeur(e.y + e.h - 1, 0)));
      } else if (e.type === "panneau_chantier") {
        const im = ecran(poser(scene, "panneau_chantier_" + e.chantier, (e.x + .5) * CASE, (e.y + 1) * CASE, profondeur(e.y, 0)));
        (r.terrains[e.chantier] = r.terrains[e.chantier] || {}).panneau = im;
      } else if (e.type === "arbre_ile" || e.type === "lieu_ile") {
        const [x, y] = pied(e);
        const im = ecran(poser(scene, e.nom, x, y, profondeur(e.y + e.h - 1, 0), {miroir: e.type === "arbre_ile" && e.nom === "palmier" && VILLAGE_HD.hasard(e.x, e.y, 7) < .5}));
        if (im && e.type === "arbre_ile") (r.palmiers = r.palmiers || []).push(im);
      }
    });
    // beach grass here and there on the sand, out of the paths, the pier and what stands there
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) {
      if (carte.sol[y][x] !== "plage" || carte.bloque[y][x] || VILLAGE_HD.hasard(x, y, 45) > .16) continue;
      if (carte.elements.some(e => e.type !== "touffe" && x >= e.x && x < e.x + e.w && y >= e.y && y < e.y + e.h)) continue;
      if (x === carte.arrivee.x && y === carte.arrivee.y) continue;
      poser(scene, VILLAGE_HD.hasard(x, y, 46) < .5 ? "oyats" : "oyats_2", (x + .5) * CASE + (VILLAGE_HD.hasard(x, y, 47) - .5) * 6, (y + 1) * CASE - 2,
        profondeur(y, 0), {miroir: VILLAGE_HD.hasard(x, y, 48) < .5, echelle: .8});
    }
  }

  // ---------- what lives ----------
  function animer(scene, carte, rendu){
    if (calme()) return;
    const r = rendu.ileHD || {}, s = scene.ileSol || {}, entre = (a, b) => a + Math.random() * (b - a);
    if (s.ecume) scene.tweens.add({targets: s.ecume, alpha: .25, duration: 2200, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    if (s.fond) {   // the sea drifts slowly toward the east
      const deriver = (t, dt) => { s.fond.tilePositionX -= dt * .004; s.fond.tilePositionY -= dt * .0015; };
      scene.events.on("update", deriver);
      scene.events.once("shutdown", () => scene.events.off("update", deriver));
    }
    if (r.barque) scene.tweens.add({targets: r.barque, y: r.barque.y - .8, angle: 1.2, duration: 1500, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    (r.palmiers || []).forEach((p, i) => scene.tweens.add({targets: p, angle: p.flipX ? -1.6 : 1.6, duration: 2400 + i * 230, yoyo: true, repeat: -1,
      ease: "Sine.easeInOut", delay: i * 300}));
    // glints on the sea, near the hero (what the camera shows)
    const eau = [];
    for (let y = 0; y < HAUT; y++) for (let x = 0; x < LARG; x++) if (carte.sol[y][x] === "eau") eau.push([x, y]);
    scene.time.addEvent({delay: 650, loop: true, callback: () => {
      const v = scene.cameras.main.worldView, l = eau.filter(([x, y]) => v.contains(x * CASE + 8, y * CASE + 8));
      if (!l.length) return;
      const [x, y] = l[Math.floor(Math.random() * l.length)];
      const g = poser(scene, Math.random() < .5 ? "reflet" : "vague", x * CASE + entre(2, 14), y * CASE + entre(2, 14), .3, {echelle: entre(.5, .8)});
      if (!g) return;
      g.setAlpha(0);
      scene.tweens.add({targets: g, alpha: .85, x: g.x + entre(-3, 3), duration: 700, yoyo: true, hold: 300, onComplete: () => g.destroy()});
    }});
    // now and then a gull crosses what the camera shows, its wings beating
    scene.time.addEvent({delay: 9000, loop: true, callback: () => { if (Math.random() < .7) mouette(scene); }});
    scene.time.delayedCall(2500, () => mouette(scene));
  }
  function mouette(scene){
    const v = scene.cameras.main.worldView, sens = Math.random() < .5 ? 1 : -1;
    const y = v.y + v.height * (.18 + Math.random() * .3), x0 = sens > 0 ? v.x - 16 : v.right + 16, x1 = sens > 0 ? v.right + 16 : v.x - 16;
    const m = poser(scene, "mouette_0", x0, y, 9600, {echelle: .9});
    if (!m) return;
    m.setFlipX(sens < 0);
    let k = 0;
    const ailes = scene.time.addEvent({delay: 180, loop: true, callback: () => { if (m.active) m.setFrame(k++ % 2 ? "mouette_0" : "mouette_1"); }});
    VILLAGE_HD.cadre(scene, "mouette_1");   // its second frame, cut from the sheet
    scene.tweens.add({targets: m, x: x1, duration: 6500, ease: "Linear", onComplete: () => { ailes.remove(); m.destroy(); }});
    scene.tweens.add({targets: m, y: y - 6, duration: 1300, yoyo: true, repeat: 2, ease: "Sine.easeInOut"});
  }
  return {pret, sol, pieces, animer, mouette};
})();
