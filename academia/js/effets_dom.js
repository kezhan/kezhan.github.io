/* Académia : les effets dessinés hors de la scène Phaser, pour les cartes et les grands moments en HTML : confettis,
   étoiles, cœurs et étincelles qui jaillissent ou pleuvent, lueurs teintées. Les mêmes pièces que le jeu
   (outils/effets/, assets/hd/effets ou assets/leger/effets), animées par le navigateur (Web Animations).
   Rien en recette ni pour qui préfère moins de mouvement (calme()). */
const pieceDom = nom => `${DOSSIER_HD}/effets/${nom}.png`;
const CONFETTIS = ["confetti_rose", "confetti_bleu", "confetti_vert", "confetti_jaune", "confetti_violet"];
const entreDom = (a, b) => a + Math.random() * (b - a);

// pieces burst from (x, y) (CSS pixels of the screen), fly out and fall back a little
function jaillir(x, y, {pieces = ["etoile"], n = 16, vitesse = [220, 460], angle = [200, 340], gravite = 900, taille = [16, 30],
  duree = [900, 1400], tourne = 540, z = 60, delai = 0, parent = document.body} = {}){
  if (calme()) return;
  for (let i = 0; i < n; i++) {
    const t = entreDom(...taille), a = entreDom(...angle) * Math.PI / 180, v = entreDom(...vitesse), ms = entreDom(...duree), rot = entreDom(-tourne, tourne);
    const im = el("img", "piece-fx"); im.alt = ""; im.src = pieceDom(pieces[i % pieces.length]);
    im.style.cssText = `left:${x - t / 2}px;top:${y - t / 2}px;width:${t}px;z-index:${z}`;
    const images = [];
    for (let k = 0; k <= 10; k++) {   // a parabola, sampled
      const s = k / 10, tt = s * ms / 1000;
      images.push({transform: `translate(${(Math.cos(a) * v * tt).toFixed(1)}px, ${(Math.sin(a) * v * tt + .5 * gravite * tt * tt).toFixed(1)}px) rotate(${(rot * s).toFixed(0)}deg) scale(${k ? 1 : .3})`,
        opacity: s < .72 ? 1 : 1 - (s - .72) / .28});
    }
    parent.append(im);
    const an = im.animate(images, {duration: ms, delay: delai + i * 14, fill: "both"});
    an.onfinish = an.oncancel = () => im.remove();
  }
}
// pieces falling from the top of the screen, swaying (a rain of stars, of confetti)
function pleuvoir({pieces = ["etoile"], n = 20, taille = [14, 26], duree = [1600, 2400], etale = 900, z = 60, x0 = 0, x1 = innerWidth, parent = document.body} = {}){
  if (calme()) return;
  for (let i = 0; i < n; i++) {
    const t = entreDom(...taille), x = entreDom(x0, x1), ms = entreDom(...duree), b = entreDom(-40, 40), rot = entreDom(-360, 360);
    const im = el("img", "piece-fx"); im.alt = ""; im.src = pieceDom(pieces[i % pieces.length]);
    im.style.cssText = `left:${x}px;top:${-t - 10}px;width:${t}px;z-index:${z}`;
    parent.append(im);
    const h = innerHeight + 2 * t + 20;
    const an = im.animate([
      {transform: "translate(0, 0) rotate(0deg)", opacity: 1},
      {transform: `translate(${b}px, ${h * .5}px) rotate(${rot / 2}deg)`, opacity: 1, offset: .5},
      {transform: `translate(${-b}px, ${h}px) rotate(${rot}deg)`, opacity: .85}], {duration: ms, delay: entreDom(0, etale), easing: "ease-in", fill: "both"});
    an.onfinish = an.oncancel = () => im.remove();
  }
}
// a white piece (halo, aura, ring) tinted with a colour: the drawing serves as a mask
function lueurDom(parent, piece, couleur, cote, x, y, images, options){
  const d = el("div", "lueur-fx");
  d.style.cssText = `left:${x - cote / 2}px;top:${y - cote / 2}px;width:${cote}px;height:${cote}px;background:${couleur};`
    + `-webkit-mask:url(${pieceDom(piece)}) center/contain no-repeat;mask:url(${pieceDom(piece)}) center/contain no-repeat`;
  parent.append(d);
  if (calme()) { d.remove(); return null; }
  const an = d.animate(images, {fill: "both", ...options});
  an.onfinish = an.oncancel = () => d.remove();
  return an;
}
// a victory: confetti and stars burst from the companion, a few more fall from the top
function confettisDom(x, y, n = 30){
  jaillir(x, y, {pieces: [...CONFETTIS, "etoile", ...CONFETTIS, "etincelle"], n, vitesse: [260, 620], angle: [190, 350], gravite: 1100, taille: [14, 26], duree: [1100, 1700], tourne: 720});
  pleuvoir({pieces: [...CONFETTIS, "etoile_p"], n: Math.round(n * .6), taille: [12, 22], etale: 1200});
}
// an evolution: a golden glow twice, a ring opening, and a rain of stars and sparkles over the new form
function evolutionDom(x, y, taille, parent = document.body){
  if (calme()) return;
  for (let i = 0; i < 2; i++)
    lueurDom(parent, "aura", "#FFE38A", taille * 2, x, y, [{transform: "scale(.6)", opacity: 0}, {transform: "scale(1)", opacity: .95, offset: .4}, {transform: "scale(1.35)", opacity: 0}],
      {duration: 1000, delay: i * 600, easing: "ease-out"});
  lueurDom(parent, "anneau", "#FFE38A", taille * 1.4, x, y, [{transform: "scale(.3)", opacity: .95}, {transform: "scale(1.4)", opacity: 0}], {duration: 700, delay: 150, easing: "ease-out"});
  pleuvoir({pieces: ["etoile", "etincelle", "etoile_p"], n: 26, taille: [14, 30], duree: [1300, 1900], etale: 1100, x0: x - taille * .9, x1: x + taille * .9, z: 62, parent});
  jaillir(x, y, {pieces: ["etincelle", "etoile"], n: 14, vitesse: [180, 380], angle: [0, 360], gravite: 300, taille: [16, 28], duree: [700, 1000], z: 62, parent});
}
