/* Académia : l'accueil. Le village dessiné en fond (js/jeu/vitrine.js), les compagnons de départ qui respirent sur
   l'herbe, une Ombre qui sort la tête des hautes herbes. Chaque enfant retrouve sa partie en touchant sa carte (son
   héros et son compagnon en pied, son prénom dit à voix haute) ; un nouveau Gardien donne son prénom et son âge (la
   voix répète l'âge, un gâteau montre ses bougies), choisit son héros (« C'est toi ? »), puis son premier compagnon,
   et entre dans le village. */
const STARTERS = ["poussik", "matty", "plumix", "leiwchen"];

function ouvrirAccueil(){
  fermerDialogue();
  montrer("accueil");
  preparerVitrine();
  decorAccueil();
  const l = $("profils"); l.innerHTML = "";
  Object.values(E.profils).sort((a, b) => (b.vu || "").localeCompare(a.vu || "")).forEach(p => {
    const c = p.compagnons.find(x => x.id === p.actif) || p.compagnons[0];
    const b = el("button", "profil"), scene = el("div", "profil-scene");
    scene.append(imageHD(herosHD(p), "face", 104));
    if (c) scene.append(spriteCompagnon(c, 70));
    const t = el("span", "", "<b></b><small></small>");
    t.querySelector("b").textContent = p.nom;
    t.querySelector("small").textContent = c ? affiche(`${nomCompagnon(c)} · niveau ${c.niveau}`) : "Pas encore de compagnon";
    b.append(scene, t);
    b.onclick = () => { sfx.tap(); choisirProfil(p.id); dire(`Bonjour ${p.nom} !`); p.compagnons.length ? entrerMonde() : ouvrirChoix(); };
    l.append(b);
  });
  ecrireIcones($("btnNouveau"), Object.keys(E.profils).length ? "✨ Nouveau Gardien" : "✨ Commencer l'aventure");
}
// the bottom of the home screen: the four first companions breathing on the grass, and an Ombre peeking out of the
// tall grass (a touch: it ducks; a companion touched hops and says its name)
function decorAccueil(){
  const d = $("decorAccueil");
  if (d.childElementCount) return;
  d.append(el("div", "sol-deco"));
  const petit = innerWidth < 560, h = petit ? 60 : 92, pas = petit ? 16 : 11;
  STARTERS.forEach((f, i) => {
    const s = imageHD(f + "1", "face", h); s.classList.add("starter-deco");
    s.style.left = `${3 + i * pas}%`; s.style.animationDelay = `${-i * .6}s`;
    s.onclick = () => { sauterDessin(s); dire(FAMILLES[f].noms[0] + " !"); };
    d.append(s);
  });
  const coucou = el("div", "coucou"), tete = imageHD("neantik", "face", petit ? 58 : 80);
  coucou.style.setProperty("--coucou", `${3 + 4 * pas + (petit ? 4 : 2)}%`);   // after the companions, in its grass
  tete.classList.add("tete"); tete.classList.remove("vivant");
  const fond = pieceVillage("herbe_dojo_fond_1", petit ? 40 : 52), devant = pieceVillage("herbe_dojo_0", petit ? 30 : 40);
  fond.classList.add("herbe-deco", "fond"); devant.classList.add("herbe-deco");
  coucou.append(fond, tete, devant);
  coucou.onclick = () => { coucou.classList.add("cache"); setTimeout(() => coucou.classList.remove("cache"), 2500); };
  d.append(coucou);
}

// a birthday cake with as many candles as years (3 to 11), flat like the game's drawings: few candles are big, many
// are thinner, so that a child can count them
function gateauAge(n){
  const larg = Math.min(56, 16 + n * 4.6), x0 = (60 - larg) / 2, e = n <= 6 ? 3.6 : 2.7, marge = e / 2 + 3;
  let bougies = "";
  for (let i = 0; i < n; i++) {
    const x = n === 1 ? 30 : x0 + marge + i * (larg - 2 * marge) / (n - 1);
    bougies += `<rect x="${(x - e / 2).toFixed(2)}" y="9" width="${e}" height="8" rx="1.1" fill="${["#7FC8FF", "#FF8FB8", "#8FE0A8"][i % 3]}" stroke="#1D1640" stroke-width=".9"/>`
      + `<path d="M${x.toFixed(2)} 1.6 q${(e * .85).toFixed(2)} 3.6 0 6 q${(-e * .85).toFixed(2)} -2.4 0 -6z" fill="#FFB800" stroke="#E07A1F" stroke-width=".5"/>`;
  }
  return `<svg class="gateau" viewBox="0 0 60 31" aria-hidden="true">${bougies}`
    + `<rect x="${x0.toFixed(2)}" y="17" width="${larg.toFixed(2)}" height="12.5" rx="3.4" fill="#FFE6EF" stroke="#1D1640" stroke-width="1.7"/>`
    + `<path d="M${(x0 + 1.2).toFixed(2)} 19.4 h${(larg - 2.4).toFixed(2)}" stroke="#FF8FB8" stroke-width="3.4" stroke-linecap="round"/></svg>`;
}

let ageChoisi = 0, herosChoisi = 0, herosTouche = false, tourVoix = 0;
function ouvrirCreation(){
  montrer("creation");
  const nom = $("nomGardien"); nom.value = ""; ageChoisi = 0; herosChoisi = HEROS[0]; herosTouche = false;
  const valider = () => { $("btnCreer").classList.toggle("attente", !(nom.value.trim() && ageChoisi && herosChoisi)); };
  const ages = $("ages"); ages.innerHTML = "";
  for (let a = 3; a <= 11; a++) {
    const b = el("button", "", `<span>${a}</span>${gateauAge(a)}`); b.setAttribute("aria-pressed", "false"); b.setAttribute("aria-label", `${a} ans`);
    b.onclick = () => {
      ageChoisi = a; sfx.tap(); [...ages.children].forEach(x => x.setAttribute("aria-pressed", String(x === b))); valider();
      const tour = ++tourVoix;   // « 4 ans ! », then what comes next, unless the child already touched something else
      dire(`${a} ans !`).then(() => { if (tour === tourVoix && !herosTouche && ecranCourant === "creation") dire("Choisis ton héros !"); });
    };
    ages.append(b);
  }
  const h = $("heros"); h.innerHTML = "";
  HEROS.forEach((id, i) => {
    const b = el("button", "choix-heros"), fig = imageHD(HEROS_HD[i], "face", 84); b.append(fig); b.setAttribute("aria-pressed", String(i === 0));
    if (i === 0) markOk(b);
    b.onclick = () => {
      herosChoisi = id; herosTouche = true; tourVoix++; sfx.tap();
      [...h.children].forEach(x => x.setAttribute("aria-pressed", String(x === b))); valider();
      sauterDessin(fig); dire("C'est toi ?");
    };
    h.append(b);
  });
  nom.oninput = valider; valider();
  setTimeout(() => nom.focus(), 300);
  dire("Bienvenue, Gardien ! Quel est ton prénom ?");
}
function creer(){
  const nom = $("nomGardien").value.trim();
  if (!nom) { dire("Écris ton prénom !"); return $("nomGardien").focus(); }
  if (!ageChoisi) return dire("Touche ton âge !");
  if (!herosChoisi) return dire("Choisis ton héros !");
  // a first name already known: that child's game is never overwritten, the choice is asked
  const deja = profilParNom(nom);
  if (deja) return dialogue("Le Sage", "sage", [`Ce prénom existe déjà : c'est la partie de ${deja.nom}. Tu la reprends, ou tu choisis un autre prénom ?`], [
    {texte: `▶ Reprendre la partie de ${deja.nom}`, action: () => { choisirProfil(deja.id); deja.compagnons.length ? entrerMonde() : ouvrirChoix(); }},
    {texte: "✏️ Autre prénom", action: () => { const n = $("nomGardien"); n.value = ""; n.focus(); $("btnCreer").classList.add("attente"); }}]);
  const p = creerProfil(nom, ageChoisi);
  p.heros = herosChoisi; placer(p); sauver();
  ouvrirChoix();
}

// the first companion: one card each, the drawing said aloud when touched; on a phone, two by two, without the text
function ouvrirChoix(){
  const p = P(); montrer("choix");
  preparerVitrine();
  $("choixSous").textContent = `${p.nom}, les Ombres ont envahi Académia. Choisis le compagnon qui t'aidera à les chasser !`;
  dire(`${p.nom}, choisis ton premier compagnon ! Touche un compagnon pour l'écouter.`);
  const s = $("starters"); s.innerHTML = "";
  let pris = false;
  STARTERS.forEach((f, i) => {
    const F = FAMILLES[f], r = REGIONS.find(x => x.famille === f);
    const b = el("div", "starter");
    const fig = imageHD(f + "1", "face", 150); b.append(fig);
    b.append(el("h3"), el("p")); b.querySelector("h3").textContent = F.noms[0];
    b.querySelector("p").textContent = `${F.desc} Il aime : ${r.desc.toLowerCase()}.`;
    const choisir = el("button", "gros", "Je te choisis !");
    if (i === 0) markOk(choisir);
    choisir.onclick = e => {
      e.stopPropagation();
      if (pris || P().compagnons.length) return;
      pris = true; s.querySelectorAll("button").forEach(x => { x.disabled = true; }); b.classList.add("choisi");
      sfx.victoire();
      const [x, y] = centre(fig);   // drawn sparkles and stars burst from the chosen companion (js/effets_dom.js)
      jaillir(x, y, {pieces: ["etincelle", "etoile", "etoile_p"], n: 18, angle: [0, 360], gravite: 260, vitesse: [160, 340], taille: [16, 30], duree: [700, 1100]});
      ajouterCompagnon(f, 1);
      dire(`${F.noms[0]} rejoint ton équipe !`);
      const mots = p.age <= 5 ? [`Touche une Ombre violette dans l'herbe haute : ${F.noms[0]} va la combattre !`]
        : [`Bienvenue à Académia, ${p.nom} !`, `Des Ombres violettes se cachent dans les hautes herbes. Touche-en une : ${F.noms[0]} va la combattre !`];
      setTimeout(() => entrerMonde(() => setTimeout(() => dialogue("Le Sage", "sage", mots), TEST ? 0 : 400)), TEST ? 0 : 900);
    };
    b.onclick = () => { sauterDessin(fig); dire(`${F.noms[0]}. ${F.desc} Il aime : ${r.desc.toLowerCase()}.`); };
    b.append(choisir); s.append(b);
  });
}
