/* Académia : un personnage ou une créature affiché dans les menus (hors du jeu) : une pose découpée dans sa planche
   haute définition (atlas de assets/hd ou assets/leger, js/jeu/qualite.js), qui respire doucement. */

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
// a companion at its stage (the drawings already grow from one stage to the next)
const spriteCompagnon = (c, taille = 96) => imageHD(cleDe(c), "face", taille);
