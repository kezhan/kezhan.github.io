/* Académia : quand le chef d'une maison tombe, deux grands moments avant le bilan. Le badge de la région (dessiné :
   forme, couleur et symbole de la maison, outils/effets/) tombe en grand comme une médaille, se balance et brille ;
   ses étoiles (une à trois, js/progression.js) s'allument une à une, celle qui manque se regagne en revenant.
   Puis le compagnon libéré : le chef consolé devient lumière, une bulle d'ombre éclate en étoiles, le compagnon sort
   en grand ; la voix dit qu'il est libre, et deux gros boutons : « Avec moi ! » (il suit le héros tout de suite)
   ou « Dans le sac ». Un toucher accélère, rien ne se saute. */
async function sceneBadge(region, etoiles, etoilesAvant, suite){
  const M = ouvrirMoment("badge");
  if (typeof jouerSon === "function") jouerSon("badge");
  M.m.innerHTML = fondMoment() + `<p class="titre-moment"></p><div class="scene-moment"><div class="medaille"><img alt=""></div></div>`
    + `<p class="nom-moment"></p><div class="etoiles-badge"></div><p class="sous-moment"></p><div class="boutons-moment"></div>`;
  const H = Math.round(Math.max(110, Math.min(innerHeight * .34, innerWidth * .5, 280)));
  M.q(".scene-moment").style.height = H + "px";
  const med = M.q(".medaille"), img = med.querySelector("img");
  img.src = pieceDom("badge_" + region.id); img.style.height = H + "px";
  M.q(".titre-moment").textContent = "Tu gagnes un badge !";
  const nom = M.q(".nom-moment"); nom.textContent = affiche(region.badge);
  const best = Math.max(etoiles, etoilesAvant || 0), le = /^[AEIOUÉ]/.test(region.badge) ? "l'" : "le ";
  const phrase = `Tu gagnes ${le}${region.badge} ! ` + (best === 3 ? "Trois étoiles : bravo !" : best === 2 ? "Deux étoiles !" : "Une étoile !");
  dire(phrase);
  // the medal falls from above, bounces, swings, then shines
  await M.animer(med, [{transform: "translateY(-120vh) rotate(-14deg)"}, {transform: "translateY(4%) rotate(5deg)", offset: .72},
    {transform: "translateY(-3%) rotate(-3deg)", offset: .86}, {transform: "translateY(0) rotate(0)"}], {duration: 900, easing: "cubic-bezier(.4,0,.6,1)"});
  med.classList.add("brille");
  if (!calme()) { const [x, y] = centre(med); jaillir(x, y, {pieces: ["etincelle", "etoile_p"], n: 12, angle: [0, 360], gravite: 200, vitesse: [140, 300], z: 62, parent: M.m}); }
  nom.classList.add("vu");
  // its stars light up one by one; the one still missing stays grey
  const rang = M.q(".etoiles-badge");
  for (let i = 0; i < 3; i++) {
    const e = el("img", "etoile-badge" + (i < best ? " pleine" : "")); e.alt = ""; e.src = pieceDom("etoile");
    rang.append(e);
  }
  for (let i = 0; i < best; i++) {
    await M.attendre(260);
    const e = rang.children[i]; e.classList.add("vu");
    if (!calme() && i >= (etoilesAvant || 0)) { const [x, y] = centre(e); jaillir(x, y, {pieces: ["etincelle"], n: 6, angle: [0, 360], gravite: 0, vitesse: [80, 160], taille: [12, 20], duree: [500, 700], z: 63, parent: M.m}); }
  }
  [...rang.children].slice(best).forEach(e => e.classList.add("vu"));
  if (best < 3) {
    const t = `Reviens battre ${region.chef} : ${best === 2 ? "la troisième étoile t'attend" : "deux étoiles t'attendent"} !`;
    const s = M.q(".sous-moment"); s.textContent = affiche(t); s.classList.add("vu");
  }
  if (suite) {   // the freed companion comes next: the medal stays a moment, the voice ends, then on
    await Promise.all([M.attendre(1600), Promise.race([wait(5000), new Promise(r => setTimeout(r, 1200)).then(() => attendreVoix())])]);
    M.fermer(); return;
  }
  await M.attendre(400);
  await M.boutons([{texte: "Continuer ➡", valeur: true}]);
  taire(); M.fermer();
}

async function sceneLiberation(libere, region){
  const nom = nomCompagnon(libere);
  const M = ouvrirMoment("liberation");
  if (typeof jouerSon === "function") jouerSon("compagnon");
  M.m.innerHTML = fondMoment() + `<p class="titre-moment"></p><div class="scene-moment"><div class="forme chef"></div>`
    + `<div class="bulle-ombre"><div class="ombre-dedans"></div></div><div class="forme libre"></div></div>`
    + `<p class="nom-moment"></p><p class="sous-moment"></p><div class="boutons-moment"></div>`;
  const H = Math.round(Math.max(120, Math.min(innerHeight * .4, innerWidth * .58, 320)));
  M.q(".scene-moment").style.height = Math.round(H * 1.1) + "px";
  const chef = M.q(".forme.chef"), bulle = M.q(".bulle-ombre"), libre = M.q(".forme.libre");
  chef.append(imageHD(CLES_BOSS[region.id], "vaincu", Math.round(H * .9)));
  libre.append(spriteCompagnon(libere, H));
  bulle.style.setProperty("--d", Math.round(H * 1.05) + "px");
  bulle.querySelector(".ombre-dedans").append(spriteCompagnon(libere, Math.round(H * .7)));
  const titre = `${majuscule(region.chef)} est consolé !`;
  M.q(".titre-moment").textContent = affiche(titre);
  dire(`${titre} Regarde !`);
  await M.attendre(900);
  // the consoled chief turns into light and fades away
  chef.classList.add("lumiere");
  await M.attendre(450);
  await M.animer(chef, [{transform: "translateX(-50%) scale(1)", opacity: 1}, {transform: "translateX(-50%) scale(.15)", opacity: 0}], {duration: 650, easing: "ease-in"});
  // a bubble of shadow, with someone inside, wobbles and bursts into stars
  bulle.classList.add("vu");
  await M.animer(bulle, [{transform: "translate(-50%, -50%) scale(0)"}, {transform: "translate(-50%, -50%) scale(1.08)", offset: .6}, {transform: "translate(-50%, -50%) scale(1)"}], {duration: 520, easing: "ease-out"});
  bulle.classList.add("tremble");
  await M.attendre(800);
  const [x, y] = centre(bulle);
  jaillir(x, y, {pieces: ["etoile", "etincelle", "coeur_p", "etoile_p"], n: 22, angle: [0, 360], gravite: 500, vitesse: [220, 480], taille: [16, 32], z: 62, parent: M.m});
  bulle.classList.remove("tremble"); bulle.classList.add("eclate");
  // the companion jumps out, big
  libre.classList.add("nee");
  const t = `${nom} est libre !`;
  const n = M.q(".nom-moment"); n.textContent = affiche(t); n.classList.add("vu");
  const s = M.q(".sous-moment"); s.textContent = affiche("Il veut venir avec toi !"); s.classList.add("vu");
  dire(`${t} Il veut venir avec toi !`);
  await M.attendre(700);
  const choix = await M.boutons([{texte: "⚔️ Avec moi !", valeur: "moi", cls: "avec-moi"}, {texte: "🎒 Dans le sac", valeur: "sac", cls: "gris dans-sac"}]);
  const p = P();
  if (choix === "moi") { p.actif = libere.id; sauver(); dire(`${nom}, à toi ! Il te suit partout.`); }
  else dire(`${nom} t'attend dans le sac.`);
  libre.classList.add(choix === "moi" ? "content" : "range");
  await Promise.all([M.attendre(650), Promise.race([wait(2500), new Promise(r => setTimeout(r, 300)).then(() => attendreVoix())])]);
  M.fermer();
}
// the voice has finished (or never started); at most a few seconds
async function attendreVoix(){
  for (let i = 0; i < 40 && "speechSynthesis" in window && speechSynthesis.speaking; i++) await wait(150);
}
