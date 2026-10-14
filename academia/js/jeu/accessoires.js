/* Académia : l'accessoire qu'un compagnon porte (acheté au marchand, js/marchand.js), posé sur son dessin partout où
   il se montre : au village et au combat (des images Phaser qui suivent le compagnon : position, taille, respiration,
   saut, retournement, angle, pose), dans le sac et la boutique (des calques posés sur son dessin du menu,
   js/sprite_dom.js). Les dessins sont dans un atlas par qualité (assets/<qualité>/accessoires,
   outils/personnages/scripts/accessoires.py) : chaque cadre dit son point d'attache (ancre) et sa largeur par rapport
   à la place qu'il habille. Les places de chaque compagnon, à chacun de ses stades, sont réglées ici à la main
   (PLACES_ACC, en fractions de son cadre) et vérifiées sur une planche (outils/personnages/scripts/planche_accessoires.py,
   qui lit ce tableau). Le compagnon assis (pose « fatigue ») descend d'autant. Un accessoire changé à la boutique se
   voit aussitôt, partout. Appelé par une ligne là où un compagnon est dessiné : porterAccessoire (Phaser),
   habillerDom (menus). */

// each accessory, by view: its layers [frame, place, z (-1 behind the companion, 1 in front), mirrored]
// <couches>
const COUCHES_ACC = {
 "chapeau": {"face": [["chapeau_face", "tete", 1]], "dos": [["chapeau_dos", "tete", 1]]},
 "couronne": {"face": [["couronne_face", "tete", 1]], "dos": [["couronne_dos", "tete", 1]]},
 "couronne_or": {"face": [["couronne_or_face", "tete", 1]], "dos": [["couronne_or_dos", "tete", 1]]},
 "noeud": {"face": [["noeud_face", "tete", 1]], "dos": [["noeud_dos", "tete", 1, true]]},
 "bonnet": {"face": [["bonnet_face", "tete", 1]], "dos": [["bonnet_dos", "tete", 1]]},
 "lunettes": {"face": [["lunettes_face", "yeux", 1]], "dos": [["lunettes_dos", "yeux", 1]]},
 "echarpe": {"face": [["echarpe_face", "cou", 1]], "dos": [["echarpe_dos", "cou", 1]]},
 "papillon": {"face": [["papillon_face", "cou", 1]], "dos": [["papillon_dos", "cou", 1]]},
 "cape": {"face": [["cape_derriere", "dos", -1], ["cape_attache", "cou", 1]], "dos": [["cape_dos", "dos", 1]]}
};
// </couches>
// where each companion wears them, in fractions of its frame (the same for every pose of an atlas): tete [x, y of the
// top of the head, its width], yeux [between the eyes, their spacing], cou [the neck, its width], dos [the top of the
// back, the shoulders' width, the cape's length]; assis: how far it comes down in its « fatigue » pose
// <places>
const PLACES_ACC = {
 "matty1": {"tete": [0.5, 0.2, 0.7], "yeux": [0.5, 0.455, 0.253], "cou": [0.5, 0.6, 0.74], "dos": [0.5, 0.5, 0.62, 0.36], "assis": 0.172},
 "matty2": {"tete": [0.5, 0.125, 0.68], "yeux": [0.5, 0.3615, 0.225], "cou": [0.5, 0.5, 0.72], "dos": [0.5, 0.42, 0.6, 0.4], "assis": 0.2396},
 "matty3": {"tete": [0.5, 0.18, 0.56], "yeux": [0.5, 0.3895, 0.171], "cou": [0.5, 0.52, 0.58], "dos": [0.5, 0.42, 0.5, 0.38], "assis": 0.2383},
 "plumix1": {"tete": [0.5, 0.17, 0.62], "yeux": [0.5, 0.4703, 0.22], "cou": [0.5, 0.66, 0.62], "dos": [0.5, 0.5, 0.55, 0.35], "assis": 0.1295},
 "plumix2": {"tete": [0.5, 0.18, 0.56], "yeux": [0.5, 0.4483, 0.186], "cou": [0.5, 0.62, 0.5], "dos": [0.5, 0.6, 0.42, 0.28], "assis": 0.125},
 "plumix3": {"tete": [0.5, 0.17, 0.5], "yeux": [0.5, 0.4726, 0.171], "cou": [0.5, 0.66, 0.42], "dos": [0.5, 0.62, 0.38, 0.26], "assis": 0.1328},
 "barli1": {"tete": [0.5, 0.14, 0.6], "yeux": [0.5, 0.3867, 0.248], "cou": [0.5, 0.62, 0.62], "dos": [0.5, 0.45, 0.6, 0.38], "assis": 0.1228},
 "barli2": {"tete": [0.5, 0.1, 0.6], "yeux": [0.5, 0.3253, 0.216], "cou": [0.5, 0.57, 0.5], "dos": [0.5, 0.52, 0.6, 0.4], "assis": 0.075},
 "barli3": {"tete": [0.5, 0.13, 0.54], "yeux": [0.5, 0.3385, 0.184], "cou": [0.5, 0.585, 0.46], "dos": [0.5, 0.54, 0.55, 0.4], "assis": 0.0664},
 "leiwchen1": {"tete": [0.5, 0.12, 0.5], "yeux": [0.497, 0.4638, 0.167], "cou": [0.5, 0.74, 0.4], "dos": [0.5, 0.7, 0.36, 0.2], "assis": 0.0848},
 "leiwchen2": {"tete": [0.5, 0.1, 0.56], "yeux": [0.5, 0.3925, 0.174], "cou": [0.5, 0.6, 0.42], "dos": [0.5, 0.6, 0.38, 0.28], "assis": 0.0625},
 "leiwchen3": {"tete": [0.5, 0.12, 0.34], "yeux": [0.5, 0.3742, 0.117], "cou": [0.5, 0.6, 0.24], "dos": [0.5, 0.6, 0.24, 0.3], "assis": 0.0586},
 "cavalin1": {"tete": [0.5, 0.13, 0.66], "yeux": [0.5, 0.35, 0.34], "cou": [0.5, 0.655, 0.5], "dos": [0.5, 0.64, 0.55, 0.33], "assis": 0.0714},
 "cavalin2": {"tete": [0.5, 0.1, 0.56], "yeux": [0.498, 0.368, 0.262], "cou": [0.5, 0.68, 0.44], "dos": [0.5, 0.66, 0.5, 0.3], "assis": 0.0625},
 "cavalin3": {"tete": [0.5, 0.13, 0.56], "yeux": [0.499, 0.355, 0.267], "cou": [0.5, 0.6, 0.5], "dos": [0.5, 0.58, 0.55, 0.38], "assis": 0.0586},
 "poussik1": {"tete": [0.5, 0.34, 0.6], "yeux": [0.5, 0.552, 0.219], "cou": [0.5, 0.74, 0.62], "dos": [0.5, 0.6, 0.6, 0.3], "assis": 0.1295},
 "poussik2": {"tete": [0.5, 0.16, 0.46], "yeux": [0.5, 0.388, 0.175], "cou": [0.5, 0.6, 0.4], "dos": [0.5, 0.56, 0.4, 0.3], "assis": 0.0708},
 "poussik3": {"tete": [0.5, 0.25, 0.42], "yeux": [0.5, 0.4, 0.128], "cou": [0.5, 0.61, 0.36], "dos": [0.5, 0.58, 0.36, 0.32], "assis": 0.0586}
};
// </places>
const CLE_ACC = "accessoires";   // the sheet's texture in Phaser
const srcAcc = ext => `${DOSSIER_HD}/accessoires/accessoires.${ext}`;
let ACC_JSON = null, accPromesse = null;   // the sheet's frames (anchor, width), fetched once for both worlds

// the accessory a companion wears: one the child owns, else none (a damaged save never shows a stranger's hat);
// p: the child (the one playing, unless the home screen asks about another)
function accessoirePorte(famille, p = typeof P === "function" ? P() : null){
  const a = p && p.accessoires;
  const id = a && a.porte && a.porte[famille];
  return id && COUCHES_ACC[id] && Array.isArray(a.possedes) && a.possedes.includes(id) ? id : null;
}
// the frames of the sheet (asked again twice when the network drops it)
function accessoiresJSON(){
  if (ACC_JSON) return Promise.resolve(ACC_JSON);
  if (!accPromesse) accPromesse = (async () => {
    for (let n = 0; n < 3; n++) {
      try { const r = await fetch(srcAcc("json") + (n ? `?essai=${n}` : "")); if (r.ok) return (ACC_JSON = await r.json()); } catch (e) {}
      await new Promise(f => setTimeout(f, 400 * (n + 1)));
    }
    accPromesse = null; return null;
  })();
  return accPromesse;
}
// a place for this pose: seated, it comes down (a cape gets shorter by as much)
function placeAcc(cle, lieu, pose){
  const pl = PLACES_ACC[cle], p = pl && pl[lieu] && pl[lieu].slice();
  if (!p) return null;
  if (pose === "fatigue") { p[1] += pl.assis || 0; if (p.length > 3) p[3] = Math.max(.05, p[3] - (pl.assis || 0)); }
  return p;
}
// a layer in fractions of the companion's frame (W x H pixels): [x, y, width, height] of the accessory's box
function boiteAcc(couche, cle, pose, fr, W, H){
  const [, lieu] = couche, p = placeAcc(cle, lieu, pose);
  if (!p || !fr) return null;
  const aw = p[2] * W * fr.largeur, ah = fr.hauteur && p.length > 3 ? p[3] * H * fr.hauteur : aw * fr.frame.h / fr.frame.w;
  const ax = couche[3] ? 1 - fr.ancre[0] : fr.ancre[0];
  return [p[0] - ax * aw / W, p[1] - fr.ancre[1] * ah / H, aw / W, ah / H];
}

// ---- in Phaser: images that follow the companion's sprite, frame after frame
function chargerAccessoiresPhaser(scene, fini){
  if (scene.textures.exists(CLE_ACC)) return fini();
  const ev = Phaser.Textures.Events.ADD_KEY + CLE_ACC, arrive = () => { if (scene.sys.isActive() || scene.sys.isSleeping()) fini(); };
  scene.textures.once(ev, arrive);
  scene.events.once("shutdown", () => scene.textures.off(ev, arrive));
  if (!scene.accDemande) {   // asked once per scene at a time (a companion redrawn meanwhile waits for the same sheet)
    scene.accDemande = true;
    scene.load.atlas(CLE_ACC, srcAcc("png"), srcAcc("json"));
    scene.load.once("complete", () => { scene.accDemande = false; });
    if (!scene.load.isLoading()) scene.load.start();
  }
}
// the accessory worn by companion c, on its sprite s (village: face_petit…, fight: dos, touche…); the images go
// with the sprite. The sheet is asked for only once the companion wears something (a child who never went to the
// shop never loads it)
function porterAccessoire(scene, s, c){
  if (!scene || !s || !c) return;
  const images = [];
  let pret = false, demande = false, quoi = "";   // quoi: the accessory, view and size the images show
  const vider = () => { images.forEach(i => i.destroy()); images.length = 0; quoi = ""; };
  const finir = () => { scene.events.off("postupdate", suivre); vider(); };
  const suivre = () => {
    if (!s.active || !s.scene) return finir();
    const porte = accessoirePorte(c.famille), id = pret ? porte : null, cadre = s.frame && s.frame.name;
    if (porte && !demande) {
      demande = true;
      Promise.all([accessoiresJSON(), new Promise(ok => chargerAccessoiresPhaser(scene, ok))]).then(([j]) => {
        pret = !!j && scene.textures.exists(CLE_ACC);
        if (pret) scene.textures.get(CLE_ACC).setFilter(Phaser.Textures.FilterMode.LINEAR);
      });
    }
    if (!id || !dessine(s) || !PLACES_ACC[s.hd]) { if (quoi) vider(); return; }
    const petit = /_petit$/.test(cadre), pose = cadre.replace(/_petit$/, ""), vue = /^dos/.test(pose) ? "dos" : "face";
    const couches = COUCHES_ACC[id][vue], cle2 = `${id}|${vue}|${petit}|${s.hd}`;
    if (cle2 !== quoi) {   // another accessory, another view or size: new images
      vider(); quoi = cle2;
      couches.forEach(k => images.push(scene.add.image(0, 0, CLE_ACC, k[0] + (petit ? "_p" : "")).setVisible(false)));
    }
    const W = s.width, H = s.height, cos = Math.cos(s.rotation), sin = Math.sin(s.rotation), vu = s.visible;
    couches.forEach((k, i) => {
      const im = images[i], nom = k[0] + (petit ? "_p" : ""), fr = ACC_JSON.frames[nom];
      const b = boiteAcc(k, s.hd, pose, fr, W, H);
      if (!b || !im.frame || im.frame.name !== nom) return im.setVisible(false);
      const ax = k[3] ? 1 - fr.ancre[0] : fr.ancre[0];
      // the anchor in the companion's frame, from its origin (feet), mirrored when it looks the other way
      let lx = (b[0] + ax * b[2] - .5) * W, ly = (b[1] + fr.ancre[1] * b[3] - 1) * H;
      if (s.flipX) lx = -lx;
      const dx = lx * s.scaleX, dy = ly * s.scaleY, miroir = !!k[3] !== !!s.flipX;
      // Phaser mirrors a picture inside its own box: the anchor of a mirrored one is on the other side
      im.setPosition(s.x + dx * cos - dy * sin, s.y + dx * sin + dy * cos).setOrigin(miroir ? 1 - fr.ancre[0] : fr.ancre[0], fr.ancre[1])
        .setScale(b[2] * W / im.frame.realWidth * s.scaleX, b[3] * H / im.frame.realHeight * s.scaleY)
        .setFlipX(miroir).setAngle(s.angle).setAlpha(s.alpha).setVisible(vu)
        .setDepth(s.depth + (k[2] < 0 ? -.1 : .1));
    });
  };
  scene.events.on("postupdate", suivre);
  scene.events.once("shutdown", finir);
}

// ---- in the menus: the companion's drawing (imageHD) in a box with the accessory's layers; the whole fits the
// height asked (a tall hat makes the companion a little smaller, never out of its card). acc: the accessory shown
// (a try-on at the shop), else the one it wears
function habillerDom(fig, c, acc){
  const id = acc === undefined ? accessoirePorte(c.famille) : acc;
  if (!id || !COUCHES_ACC[id]) return fig;
  const cle = cleDe(c), h = parseFloat(fig.style.getPropertyValue("--h")) || 96;
  const boite = el("div", "habille" + (fig.classList.contains("vivant") ? " vivant" : ""));
  boite.dataset.accessoire = id;
  fig.classList.remove("vivant");
  boite.style.height = h + "px"; boite.style.aspectRatio = ".8";
  boite.append(fig);
  Promise.all([accessoiresJSON(), atlasJSON(cle, "creatures")]).then(([j, a]) => {
    const f = a && (a.frames.face || null);
    if (!j || !f) return;
    const W = f.frame.w, H = f.frame.h;
    const couches = COUCHES_ACC[id].face.map(k => ({k, fr: j.frames[k[0]]})).map(x => ({...x, b: boiteAcc(x.k, cle, "face", x.fr, W, H)})).filter(x => x.b);
    // the box around the companion and its layers, in fractions of its frame
    let x0 = 0, y0 = 0, x1 = 1, y1 = 1;
    couches.forEach(({b}) => { x0 = Math.min(x0, b[0]); y0 = Math.min(y0, b[1]); x1 = Math.max(x1, b[0] + b[2]); y1 = Math.max(y1, b[1] + b[3]); });
    const lw = x1 - x0, lh = y1 - y0, pc = (v, t) => `${100 * v / t}%`;
    boite.style.aspectRatio = `${lw * W} / ${lh * H}`;
    Object.assign(fig.style, {position: "absolute", left: pc(-x0, lw), top: pc(-y0, lh), width: pc(1, lw), height: pc(1, lh)});
    const S = j.meta.size;
    couches.forEach(({k, fr, b}) => {
      const d = el("div", "acc-couche" + (k[3] ? " miroir" : "")), {x, y, w, h: hh} = fr.frame;
      const part = (o, total, cadre) => total > cadre ? 100 * o / (total - cadre) : 0;
      Object.assign(d.style, {left: pc(b[0] - x0, lw), top: pc(b[1] - y0, lh), width: pc(b[2], lw), height: pc(b[3], lh),
        backgroundImage: `url(${srcAcc("png")})`, backgroundSize: `${100 * S.w / w}% ${100 * S.h / hh}%`,
        backgroundPosition: `${part(x, S.w, w)}% ${part(y, S.h, hh)}%`});
      if (k[2] < 0) boite.insertBefore(d, fig); else boite.append(d);
    });
  });
  return boite;
}
// an accessory alone in a menu (the shop's shelf): its front view, the cape seen from the back (its star); its size
// comes from the stylesheet (--acc-h: its height, never wider than its box), its shape from its drawing (--a)
function accessoireDom(id){
  const d = el("div", "acc-seul");
  d.dataset.accessoire = id;
  const nom = id === "cape" ? "cape_dos" : id + "_face";
  accessoiresJSON().then(j => {
    const fr = j && j.frames[nom];
    if (!fr) return;
    const {x, y, w, h} = fr.frame, S = j.meta.size, part = (o, total, cadre) => total > cadre ? 100 * o / (total - cadre) : 0;
    d.style.setProperty("--a", (w / h).toFixed(4));
    Object.assign(d.style, {backgroundImage: `url(${srcAcc("png")})`,
      backgroundSize: `${100 * S.w / w}% ${100 * S.h / h}%`, backgroundPosition: `${part(x, S.w, w)}% ${part(y, S.h, h)}%`});
  });
  return d;
}
