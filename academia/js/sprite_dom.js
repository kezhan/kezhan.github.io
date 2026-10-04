/* Académia : un personnage ou une créature du pack affiché dans les menus (hors du jeu), en pixel art net,
   qui marche sur place. Planches : 4 colonnes (bas, haut, gauche, droite), une ligne par image de marche. */
function spriteDOM(type, id, taille = 96, {col = 0, marche = true, echelle = 1} = {}){
  const lignes = type === "perso" ? 7 : 4, t = Math.round(taille * echelle);
  const d = el("div", "sprite" + (marche ? " marche" : ""));
  d.style.cssText = `width:${t}px;height:${t}px;background-image:url(assets/${type === "perso" ? "personnages" : "monstres"}/${id}.png);`
    + `background-size:${t * 4}px ${t * lignes}px;background-position:${-col * t}px 0;--pas:${-4 * t}px`;
  return d;
}
const spriteCompagnon = (c, taille = 96, opts = {}) => spriteDOM("monstre", spriteDe(c), taille, {echelle: echelleStade(stade(c)) / 1.3, ...opts});
