/* Académia : l'évolution, en grand (GDD §9 : un diplôme). Fond nuit et rayons ; l'ancienne forme devient lumière,
   puis palpite en alternant avec la silhouette de la nouvelle, de plus en plus vite ; un éclair, la pluie d'étoiles
   de l'évolution, la nouvelle forme en grand qui saute, son nom dit et écrit en gros, sa nouvelle attaque.
   « Continuer » n'arrive qu'à la fin (4 s au plus) ; un toucher accélère sans rien sauter. */
async function sceneEvolution(c, stadeAvant){
  const avant = FAMILLES[c.famille].noms[stadeAvant - 1], apres = nomCompagnon(c), attaque = attaquesDe(c)[1];
  const M = ouvrirMoment("evolution");
  if (typeof jouerSon === "function") jouerSon("evolution");
  sfx.evolution();
  M.m.innerHTML = fondMoment() + `<p class="titre-moment"></p><div class="scene-moment"><div class="forme avant"></div>`
    + `<div class="forme apres"></div></div><p class="nom-moment"></p><p class="sous-moment"></p><div class="boutons-moment"></div>`;
  // the stage: as tall as the screen allows, the new form a little taller than the old
  const H = Math.round(Math.max(120, Math.min(innerHeight * .4, innerWidth * .6, 340)));
  M.q(".scene-moment").style.height = Math.round(H * 1.12) + "px";
  const fa = M.q(".forme.avant"), fb = M.q(".forme.apres");
  fa.append(imageHD(c.famille + stadeAvant, "face", Math.round(H * .86))); fb.append(imageHD(cleDe(c), "face", H));
  M.q(".titre-moment").textContent = affiche(`Oh ? ${avant} évolue !`);
  dire(`Oh ? ${avant} évolue !`);
  await M.attendre(700);
  fa.classList.add("lumiere");             // the old form turns to light
  await M.attendre(550);
  fb.classList.add("lumiere");
  for (const ms of [360, 300, 250, 210, 170, 140, 115, 95, 80, 65, 55, 45]) {   // the two shapes, faster and faster
    fa.classList.toggle("cachee"); fb.classList.toggle("visible");
    await M.attendre(ms);
  }
  // a flash, and the new form in colour
  M.m.classList.add("eclair");
  fa.classList.add("cachee"); fb.classList.add("visible");
  await M.attendre(100);
  fb.classList.remove("lumiere"); fb.classList.add("nee");
  const [x, y] = centre(fb);
  evolutionDom(x, y, H, M.m);
  const nom = M.q(".nom-moment"), sous = M.q(".sous-moment");
  nom.textContent = affiche(`${apres} !`); nom.classList.add("vu");
  sous.textContent = affiche(`Nouvelle attaque : ${attaque}`);
  dire(`${avant} est devenu ${apres} ! Nouvelle attaque : ${attaque} !`);
  await M.attendre(400);
  sous.classList.add("vu");
  await M.attendre(200);
  await M.boutons([{texte: "Continuer ➡", valeur: true}]);
  taire(); M.fermer();
}
