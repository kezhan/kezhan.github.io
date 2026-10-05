/* Académia : un personnage ou une créature affiché dans les menus (hors du jeu). En haute définition, une pose
   découpée dans sa planche (atlas de assets/hd ou assets/leger), qui respire doucement ; sinon le pixel art du
   pack, net, qui marche sur place. Planches pixel : 4 colonnes (bas, haut, gauche, droite), une ligne par image. */
function spriteDOM(type, id, taille = 96, {col = 0, marche = true, echelle = 1} = {}){
  const lignes = type === "perso" ? 7 : 4, t = Math.round(taille * echelle);
  const d = el("div", "sprite" + (marche ? " marche" : ""));
  d.style.cssText = `width:${t}px;height:${t}px;background-image:url(assets/${type === "perso" ? "personnages" : "monstres"}/${id}.png);`
    + `background-size:${t * 4}px ${t * lignes}px;background-position:${-col * t}px 0;--pas:${-4 * t}px`;
  return d;
}

// a pose of a high-definition drawing, `hauteur` CSS pixels tall; `secours` gives the pixel one if it is not drawn yet
function imageHD(cle, pose, hauteur, secours){
  const d = el("div", "sprite sprite-hd vivant");
  d.style.cssText = `width:${Math.round(hauteur * .8)}px;height:${Math.round(hauteur)}px`;
  const type = typeAtlas(cle);
  atlasJSON(cle, type).then(a => {
    const f = a && (a.frames[pose] || a.frames.face);
    if (!f) { if (secours && d.parentNode) d.replaceWith(secours()); return; }
    const k = hauteur / f.frame.h;
    d.style.cssText = `width:${Math.round(f.frame.w * k)}px;height:${Math.round(hauteur)}px;background-image:url(${DOSSIER_HD}/${type}/${cle}.png);`
      + `background-size:${a.meta.size.w * k}px ${a.meta.size.h * k}px;background-position:${-f.frame.x * k}px ${-f.frame.y * k}px`;
  });
  return d;
}
const spriteCompagnon = (c, taille = 96, opts = {}) => imageHD(cleDe(c), "face", taille * (opts.echelle || 1),
  () => spriteDOM("monstre", spriteDe(c), taille, {echelle: echelleStade(stade(c)) / 1.3, ...opts}));
