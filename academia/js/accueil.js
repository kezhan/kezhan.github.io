/* Académia : l'accueil. Chaque enfant retrouve sa partie en touchant son prénom ; un nouveau Gardien donne
   son prénom et son âge, choisit son héros, puis son premier compagnon, et entre dans le village. */
const STARTERS = ["poussik", "matty", "plumix", "leiwchen"];

function ouvrirAccueil(){
  window.__ui = true; montrer("accueil");
  const l = $("profils"); l.innerHTML = "";
  Object.values(E.profils).sort((a, b) => (b.vu || "").localeCompare(a.vu || "")).forEach(p => {
    const c = p.compagnons.find(x => x.id === p.actif) || p.compagnons[0];
    const b = el("button", "profil");
    b.append(spriteDOM("perso", herosDe(p), 64));
    const t = el("span", "", "<b></b><small></small>");
    t.querySelector("b").textContent = p.nom;
    t.querySelector("small").textContent = c ? `${nomCompagnon(c)} · niveau ${c.niveau}` : "Pas encore de compagnon";
    b.append(t);
    if (c) b.append(spriteCompagnon(c, 56));
    b.onclick = () => { sfx.tap(); choisirProfil(p.id); dire(`Bonjour ${p.nom} !`); p.compagnons.length ? entrerMonde() : ouvrirChoix(); };
    l.append(b);
  });
  $("btnNouveau").textContent = Object.keys(E.profils).length ? "✨ Nouveau Gardien" : "✨ Commencer l'aventure";
}

let ageChoisi = 0, herosChoisi = 0;
function ouvrirCreation(){
  montrer("creation");
  const nom = $("nomGardien"); nom.value = ""; ageChoisi = 0; herosChoisi = 0;
  const valider = () => { $("btnCreer").disabled = !(nom.value.trim() && ageChoisi && herosChoisi); };
  const ages = $("ages"); ages.innerHTML = "";
  for (let a = 3; a <= 11; a++) {
    const b = el("button", "", String(a)); b.setAttribute("aria-pressed", "false");
    b.onclick = () => { ageChoisi = a; sfx.tap(); [...ages.children].forEach(x => x.setAttribute("aria-pressed", String(x === b))); valider(); };
    ages.append(b);
  }
  const h = $("heros"); h.innerHTML = "";
  HEROS.forEach((id, i) => {
    const b = el("button", "choix-heros"); b.append(spriteDOM("perso", id, 72)); b.setAttribute("aria-pressed", "false");
    if (i === 0) markOk(b);
    b.onclick = () => { herosChoisi = id; sfx.tap(); [...h.children].forEach(x => x.setAttribute("aria-pressed", String(x === b))); valider(); };
    h.append(b);
  });
  nom.oninput = valider; valider();
  setTimeout(() => nom.focus(), 300);
  dire("Bienvenue, Gardien ! Quel est ton prénom ?");
}
function creer(){
  const nom = $("nomGardien").value.trim(); if (!nom || !ageChoisi || !herosChoisi) return;
  const p = creerProfil(nom, ageChoisi);
  p.heros = herosChoisi; placer(p); sauver();
  if (p.compagnons.length) { dire(`Re-bonjour ${p.nom} ! Ta partie t'attendait.`); return entrerMonde(); }
  ouvrirChoix();
}

function ouvrirChoix(){
  const p = P(); montrer("choix");
  $("choixSous").textContent = `${p.nom}, les Ombres ont envahi Académia. Choisis le compagnon qui t'aidera à les chasser !`;
  dire(`${p.nom}, choisis ton premier compagnon !`);
  const s = $("starters"); s.innerHTML = "";
  STARTERS.forEach((f, i) => {
    const F = FAMILLES[f], r = REGIONS.find(x => x.famille === f);
    const b = el("div", "starter");
    const fig = spriteDOM("monstre", F.sprite, 128); b.append(fig);
    b.append(el("h3"), el("p")); b.querySelector("h3").textContent = F.noms[0];
    b.querySelector("p").textContent = `${F.desc} Il aime : ${r.desc.toLowerCase()}.`;
    const choisir = el("button", "gros", "Je te choisis !");
    if (i === 0) markOk(choisir);
    choisir.onclick = async () => {
      sfx.victoire(); etincelles(...centre(fig), 20);
      ajouterCompagnon(f, 1);
      await dire(`${F.noms[0]} rejoint ton équipe ! En route pour le village.`);
      entrerMonde();
      setTimeout(() => dialogue("Le Sage", "portrait9", [`Bienvenue à Académia, ${p.nom} !`, "Touche l'endroit où tu veux aller : ton héros y marche tout seul.",
        "Des Ombres se cachent dans les hautes herbes. Va les chasser avec " + F.noms[0] + " !"]), TEST ? 0 : 900);
    };
    fig.onclick = () => dire(`${F.noms[0]}. ${F.desc}`);
    b.append(choisir); s.append(b);
  });
}
