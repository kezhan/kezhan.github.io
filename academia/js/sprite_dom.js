/* Académia : un personnage, une créature ou une pièce du village affichés dans les menus (hors du jeu) : une pose
   découpée dans sa planche haute définition (assets/hd ou assets/leger, js/jeu/qualite.js), qui respire doucement. */

// a pose of a drawing, `hauteur` CSS pixels tall (--h); the box keeps its place while the atlas loads. Sizes and
// positions are in percent of the box, so that a stylesheet can make it smaller (a narrow screen) and the drawing follows
function imageHD(cle, pose, hauteur){
  const d = el("div", "sprite sprite-hd vivant");
  d.style.setProperty("--h", Math.round(hauteur) + "px");
  const type = typeAtlas(cle);
  atlasJSON(cle, type).then(a => {
    const f = a && (a.frames[pose] || a.frames.face);
    if (!f) return;
    const {x, y, w, h} = f.frame, W = a.meta.size.w, H = a.meta.size.h;
    const part = (o, total, cadre) => total > cadre ? 100 * o / (total - cadre) : 0;   // background-position: share of the free room
    Object.assign(d.style, {aspectRatio: `${w} / ${h}`, backgroundImage: `url(${DOSSIER_HD}/${type}/${cle}.png)`,
      backgroundSize: `${100 * W / w}% ${100 * H / h}%`, backgroundPosition: `${part(x, W, w)}% ${part(y, H, h)}%`});
  });
  return d;
}
// a drawing in a menu hops once (a touch on it), then breathes again
function sauterDessin(n){
  n.classList.remove("saute"); void n.offsetWidth; n.classList.add("saute");
  n.addEventListener("animationend", () => n.classList.remove("saute"), {once: true});
}
// a companion at its stage (the drawings already grow from one stage to the next)
const spriteCompagnon = (c, taille = 96) => imageHD(cleDe(c), "face", taille);

// a piece of the village's sheets (a tuft of tall grass, a flower...: js/jeu/decor_village_hd_pieces.js), `hauteur`
// CSS pixels tall; the sheet's coordinates are in high-definition pixels, the light sheet is the same at half size
function pieceVillage(nom, hauteur){
  const p = PIECES_VILLAGE[nom], d = el("div", "sprite piece-village");
  if (!p) return d;
  const [planche, x, y, w, h] = p, k = hauteur / h;
  Object.assign(d.style, {width: `${w * k}px`, height: `${hauteur}px`, aspectRatio: "auto",
    backgroundImage: `url(assets/${QUALITE}/village/${planche}.png)`, backgroundSize: `${PLANCHES_VILLAGE[planche] * k}px auto`,
    backgroundPosition: `${-x * k}px ${-y * k}px`});
  return d;
}
