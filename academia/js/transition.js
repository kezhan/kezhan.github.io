/* Académia : le passage étoilé entre le village et le combat. L'écran se ferme en étoile sur l'Ombre touchée
   (fermerEtoile(x, y), appelé par le village avant commencerCombat), puis s'ouvre en étoile sur l'arène où le
   compagnon accourt (ouvrirEtoile(), à l'entrée du combat) ; l'inverse au retour. Un voile SVG plein écran percé
   d'une étoile qui rapetisse ou grandit : 600 à 800 ms en tout. Rien en recette ni pour qui préfère moins de
   mouvement : les promesses se résolvent tout de suite. */
const ETOILE_MS = {fermer: 360, ouvrir: 420};
let voileEtoile = null;

function voileEtoileSVG(){
  if (voileEtoile) return voileEtoile;
  // a five-pointed star of outer radius 1 (inner 0.5), its points rounded a little
  const pts = [];
  for (let i = 0; i < 10; i++) {
    const a = -Math.PI / 2 + i * Math.PI / 5, r = i % 2 ? .5 : 1;
    pts.push(`${(Math.cos(a) * r).toFixed(4)},${(Math.sin(a) * r).toFixed(4)}`);
  }
  const ns = "http://www.w3.org/2000/svg", v = document.createElementNS(ns, "svg");
  v.setAttribute("class", "voile-etoile"); v.setAttribute("aria-hidden", "true");
  v.innerHTML = `<defs><mask id="trouEtoile" maskUnits="userSpaceOnUse"><rect x="-10" y="-10" width="100000" height="100000" fill="#fff"/>`
    + `<polygon id="etoileTrou" points="${pts.join(" ")}" fill="#000" stroke="#000" stroke-width=".12" stroke-linejoin="round"/></mask></defs>`
    + `<rect x="-10" y="-10" width="100000" height="100000" fill="#1D1640" mask="url(#trouEtoile)"/>`;
  v.style.display = "none";
  document.body.append(v);
  return (voileEtoile = {v, trou: v.querySelector("#etoileTrou"), ferme: false});
}
// the star's size that uncovers the whole screen from (x, y): its inner radius beyond the farthest corner
const tailleEtoileMax = (x, y) => 2.2 * Math.max(...[[0, 0], [innerWidth, 0], [0, innerHeight], [innerWidth, innerHeight]].map(([a, b]) => Math.hypot(a - x, b - y)));

function animerEtoile(x, y, de, a, ms, tour){
  const V = voileEtoileSVG();
  V.v.style.display = "block";
  return new Promise(fin => {
    const t0 = performance.now();
    const pas = now => {
      const t = Math.min(1, (now - t0) / ms), e = de < a ? 1 - (1 - t) ** 3 : t * t;
      const s = Math.max(.001, de + (a - de) * e);
      V.trou.setAttribute("transform", `translate(${x.toFixed(1)} ${y.toFixed(1)}) rotate(${(tour * e).toFixed(1)}) scale(${s.toFixed(2)})`);
      if (t < 1) requestAnimationFrame(pas); else fin();
    };
    requestAnimationFrame(pas);
  });
}
// the screen closes on (x, y), in CSS pixels (the Ombre touched; the middle of the screen by default)
async function fermerEtoile(x = innerWidth / 2, y = innerHeight / 2){
  if (calme()) return;
  const V = voileEtoileSVG();
  if (V.ferme) return;
  V.ferme = true;
  clearTimeout(V.secours); V.secours = setTimeout(() => ouvrirEtoile(), 6000);   // never a screen left closed
  await animerEtoile(x, y, tailleEtoileMax(x, y), 0, ETOILE_MS.fermer, 90);
}
// and opens again (on the middle of the screen, where the arena or the hero is); nothing if it was not closed
async function ouvrirEtoile(x = innerWidth / 2, y = innerHeight / 2){
  const V = voileEtoile;
  if (!V || !V.ferme) return;
  V.ferme = false; clearTimeout(V.secours);
  if (!calme()) await animerEtoile(x, y, 0, tailleEtoileMax(x, y), ETOILE_MS.ouvrir, 90);
  if (!V.ferme) V.v.style.display = "none";
}
