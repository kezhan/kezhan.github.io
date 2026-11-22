/* Académia : l'écran d'un lieu réservé aux compagnons, ouvert quand le héros arrive devant sur l'Île des Bâtisseurs
   (js/jeu/lieux_ile.js). Les règles sont dans js/lieux.js ; l'écran reprend les briques de celui d'un chantier
   (style_chantier.css) : le dessin, trois vignettes, la bulle de qui parle (ici la sentinelle de pierre), un gros bouton.
   1. Le lieu vu de près, et ses trois trouvailles en vignettes : gagnées (cochées), la prochaine (elle bat, avec le
      niveau qu'elle demande), à venir (pâles).
   2. Lieu fermé : à côté du dessin, sans rien à lire, le compagnon attendu (en silhouette s'il est à libérer), son
      niveau, une flèche, le niveau demandé, et le symbole de la maison où il grandit deux fois plus vite. La sentinelle
      dit quoi faire. Le bon compagnon est resté dans le sac : le bouton le met devant, et le lieu s'ouvre.
   3. Lieu ouvert : la sentinelle dit une chose vraie (dans un lieu de langue, d'abord un mot par son enregistrement),
      le bouton pose un exercice plus dur que d'habitude ; deux bonnes réponses, et la trouvaille du palier surgit.
   4. Ce que le lieu a fabriqué depuis la dernière visite est ramassé en entrant ; la sentinelle dit de revenir.
   Pour la petite (5 ans et moins) : tout est dit, les boutons sont des dessins. */
if (!ECRANS.includes("reserve")) ECRANS.push("reserve");   // one more screen for montrer() (js/outils.js)
const RESERVE = {id: null, occupe: false};
function imgLieu(nom, cls){
  const i = document.createElement("img");
  i.className = cls || ""; i.alt = ""; i.draggable = false; i.decoding = "async";
  imageReessayee(i, `${DOSSIER_HD}/lieux/${nom}.png`);
  return i;
}
// a line of the sentinel, written in its bubble and said by the voice (a language place's word by its recording)
function annoncerLieu(texte, langue){
  ecrireIcones($("bulleReserve"), affiche(texte));
  return langue ? direMixte(texte, {langue}) : dire(texte);
}
const nomFamille = f => FAMILLES[f].noms[0];
const duLieu = id => LIEUX_RESERVES[id].nom.replace(/^le /, "du ").replace(/^la /, "de la ");

function ouvrirLieu(id){
  const p = P(); if (!p || !LIEUX_RESERVES[id]) return;
  RESERVE.id = id;
  montrer("reserve");
  $("reserve").dataset.lieu = id;
  $("reserveTitre").textContent = majuscule(LIEUX_RESERVES[id].nom);
  imageReessayee($("reserveSentinelle"), `${DOSSIER_HD}/lieux/sentinelle.png`);
  $("panneauReserve").classList.add("cache");
  $("reserveDessin").replaceChildren(imgLieu("lieu_" + id, "lieu"));
  voirLieu(id);
  const recolte = recolterLieu(id);   // what the place has made since the last visit
  dessinerLieu();
  if (recolte) surgirTrouvaille(recolte.chose);
  return direLieu(recolte ? `${recolte.dit} : ${etincellesDites(recolte.etincelles)} pour toi ! Reviens plus tard : il y en aura encore ! ` : "", true);
}
function fermerLieu(){
  if (RESERVE.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  if (typeof majBarre === "function") majBarre();
  const m = JEU.scene.getScene("monde");
  if (m && m.chantiers && m.chantiers.reserves) m.chantiers.reserves.maj();
  if (m && m.majCompagnon) m.majCompagnon();   // the companion put in front walks with the hero
}

// the companion a closed place waits for, drawn: who, its level, the level asked, the house where it grows faster
function besoinLieu(s){
  const d = el("div", "reserve-besoin"), c = s.compagnon, r = maisonCompagnon(s.famille);
  d.dataset.raison = s.raison;
  const qui = el("span", "besoin-qui" + (c ? "" : " inconnue")), fig = c ? spriteCompagnon(c, 84) : imageHD(s.famille + "1", "face", 84);
  qui.append(fig);
  const niveaux = el("span", "besoin-niveaux"), a = el("b", "niv-a"), b = el("b", "niv-b");
  a.textContent = c ? `Niv. ${c.niveau}` : "?"; b.textContent = `Niv. ${s.niveau}`;
  niveaux.append(a, iconeDom(s.raison === "sac" ? "coche" : "suivant"), b);
  const maison = el("span", "besoin-maison");
  maison.append(iconeDom(`maison_${r.id}`), `×${XP_MAISON}`);
  d.append(qui, niveaux, ...(s.raison === "sac" ? [] : [maison]));
  d.onclick = () => { sfx.tap(); sauterDessin(fig); direLieu(); };
  return d;
}
function dessinerLieu(){
  const p = P(), id = RESERVE.id, s = etatLieu(id), petit = p.age <= 5, paliers = paliersLieu(p);
  $("reserveEtincelles").textContent = p.etincelles;
  $("reserve").dataset.etat = s.ouvert ? (s.fini ? "fini" : "ouvert") : s.raison;
  const dessin = $("reserveDessin");
  dessin.querySelectorAll(".reserve-besoin, .verrou").forEach(x => x.remove());
  if (!s.ouvert) dessin.append(imgLieu("cadenas", "verrou"), besoinLieu(s));
  // the three finds: won (a tick), the next one (it beats, with the level it asks), to come (pale)
  $("reservePaliers").replaceChildren(...[1, 2, 3].map(k => {
    const v = el("span", "etape-chantier trouvaille " + (k <= s.palier ? "faite" : k === s.palier + 1 ? "prochaine" : "avenir"));
    v.dataset.palier = k;
    v.append(imgLieu(trouvailleLieu(id, k)));
    if (k <= s.palier) v.append(iconeDom("coche", "fait"));
    else v.append(el("small", "niv-palier", String(paliers[k - 1])));
    v.onclick = () => { sfx.tap(); sauterDessin(v); dire(k <= s.palier ? `${majuscule(LIEUX_RESERVES[id].choses[k - 1])} : c'est ta trouvaille !`
      : LIEUX_RESERVES[id].equipe ? `Un trésor à trouver, quand toute l'équipe sera au niveau ${paliers[k - 1]} !`
      : `Un trésor à trouver, avec ${nomFamille(LIEUX_RESERVES[id].famille)} au niveau ${paliers[k - 1]} !`); };
    return v;
  }));
  const b = $("btnReserve"), mot = t => petit ? [] : [" " + t];
  b.replaceChildren(); b.className = "gros vert" + (petit ? " image" : "");
  if (s.ouvert) {
    b.dataset.etat = s.fini ? "entrainement" : "exercice";
    b.append(s.fini ? iconeDom("etoile") : imgLieu(trouvailleLieu(id, s.palier + 1), "vignette"), ...mot("Exercice"));
    b.setAttribute("aria-label", "Exercice");
  } else if (s.raison === "sac") {
    b.dataset.etat = "sac";
    b.append(spriteCompagnon(s.compagnon, 52), ...mot("Avec moi !"));
    b.setAttribute("aria-label", `${nomCompagnon(s.compagnon)}, avec moi !`);
  } else {
    b.dataset.etat = s.raison;
    b.append(iconeDom(`maison_${maisonCompagnon(s.famille).id}`), ...mot("J'y vais !"), ...(petit ? [iconeDom("suivant")] : []));
    b.setAttribute("aria-label", "J'y vais !");
  }
}
// what the sentinel says for where the place stands; `accueil`: the first words of this visit (the true thing to learn)
function direLieu(debut = "", accueil = false){
  const p = P(), id = RESERVE.id, L = LIEUX_RESERVES[id], s = etatLieu(id), petit = p.age <= 5, vus = reservesDe(p)[id].vus;
  if (!s.ouvert) {
    const f = s.famille, r = maisonCompagnon(f), nom = s.compagnon ? nomCompagnon(s.compagnon) : nomFamille(f);
    if (s.raison === "absent") return annoncerLieu(`${debut}${L.equipe ? "Le belvédère attend toute l'équipe" : "Ce lieu attend " + nom} ! Libère ${nom} ${DANS_REGION[r.id]} : console les Ombres, puis gagne contre ${r.chef}.`);
    if (s.raison === "sac") return annoncerLieu(`${debut}${nom} a le niveau qu'il faut ! Mais ${nom} est resté dans ton sac. Touche le bouton : ${nom} marchera avec toi.`);
    return annoncerLieu(`${debut}${L.equipe ? `Le belvédère s'ouvre quand toute l'équipe est au niveau ${s.niveau}` : `Ce lieu s'ouvre à ${nom} au niveau ${s.niveau}`}. ${nom} est au niveau ${s.compagnon.niveau}. `
      + `Va ${DANS_REGION[r.id]} : ${nom} y apprend deux fois plus vite !`);
  }
  if (s.fini) return annoncerLieu(`${debut}Tu as trouvé tous les trésors ${duLieu(id)}, bravo ! ${L.dits[(vus - 1) % L.dits.length]} ${petit ? "Touche le bouton pour jouer !" : "Un exercice, pour t'entraîner ?"}`);
  const chose = L.choses[s.palier], m = accueil ? motLieu(id, s.palier) : null;
  const suite = s.bonnes ? `Encore une bonne réponse, et ${chose} est à toi !` : `Deux bonnes réponses, et ${chose} est à toi !`;
  if (!accueil) return annoncerLieu(debut + suite + (petit ? " Touche le gros bouton !" : ""));
  return annoncerLieu(`${debut}${m ? `« ${m.mot} » : c'est « ${m.sens} », en ${m.nom} ! ` : ""}${L.dits[s.palier]} ${suite}${petit ? " Touche le gros bouton !" : ""}`, m && m.langue);
}

// the big button: an exercise in an open place; the right companion put in front; or the way to its house
async function boutonLieu(){
  if (RESERVE.occupe || !RESERVE.id) return;
  sfx.tap();
  const id = RESERVE.id, s = etatLieu(id);
  if (!s.ouvert && s.raison === "sac") {
    P().actif = s.compagnon.id; sauver(); sfx.juste();
    if (typeof noterJournal === "function") noterJournal("lieu_actif", {l: id, c: s.famille});
    dessinerLieu();
    return direLieu(`${nomCompagnon(s.compagnon)}, à toi ! `, true);
  }
  if (!s.ouvert) return fermerLieu();
  const q = questionLieu(id);
  if (!q) return annoncerLieu("Pas de question pour l'instant : reviens dans un moment !");
  RESERVE.occupe = true;
  const panneau = $("panneauReserve"), ecran = $("reserve");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: majuscule(LIEUX_RESERVES[id].nom), jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerLieu("Presque ! Rien n'est perdu : touche le bouton pour une autre question.");
    const fait = reussirLieu(id);
    dessinerLieu();
    if (fait && fait.trouvaille) surgirTrouvaille(trouvailleLieu(id, fait.trouvaille), true);
    else sfx.juste();
    // after the foreign word found, said again (js/question_vue.js), never over it
    apresRedit(() => {
      if ($("reserve").hidden || RESERVE.id !== id) return null;
      if (!fait) return annoncerLieu("Bravo ! Tu connais bien ce lieu. Reviens plus tard : il fabriquera autre chose pour toi !");
      if (!fait.trouvaille) return direLieu("Bravo ! ");
      const suivant = etatLieu(id), nom = suivant.compagnon ? nomCompagnon(suivant.compagnon) : "";
      return annoncerLieu(`Bravo ! Tu as trouvé ${fait.chose} : ${etincellesDites(fait.etincelles)} pour toi ! Regarde dans l'album de ton sac. `
        + (fait.fini ? "Tu as tous les trésors de ce lieu ! " : suivant.ouvert ? "Un autre trésor t'attend ici ! "
          : LIEUX_RESERVES[id].equipe ? `Le prochain trésor demande toute l'équipe au niveau ${suivant.niveau}. ` : `Le prochain trésor demande ${nom} au niveau ${suivant.niveau}. `)
        + "Reviens plus tard : le lieu fabriquera autre chose pour toi !");
    });
  } finally {
    RESERVE.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
// a find pops over the place's drawing, then flies away (it stays in its vignette)
function surgirTrouvaille(chose, fete){
  const d = $("reserveDessin"), i = imgLieu(chose, "trouvaille-surgie");
  (fete ? sfx.victoire : sfx.niveau)(); if (typeof jouerSon === "function") jouerSon("scintille");
  if (calme()) return;
  d.append(i);
  const r = d.getBoundingClientRect();
  jaillir(r.left + r.width / 2, r.top + r.height * .45, {pieces: ["etoile", "etincelle", "etoile_p"], n: fete ? 22 : 10});
  i.animate([{transform: "translate(-50%, -30%) scale(.3)", opacity: 0}, {transform: "translate(-50%, -50%) scale(1.1)", opacity: 1, offset: .25},
    {transform: "translate(-50%, -50%) scale(1)", opacity: 1, offset: .8}, {transform: "translate(-50%, -90%) scale(.6)", opacity: 0}], {duration: 1900, easing: "ease-out"}).onfinish = () => i.remove();
}

// ---- the bag's album: the finds, three per place, in silhouette until they are won (js/album.js)
function caseTrouvaille(t){
  const L = LIEUX_RESERVES[t.id], b = el("button", "case-trouvaille" + (t.gagnee ? " gagnee" : "")), cadre = el("span", "cadre"), nom = el("b");
  const fig = imgLieu(trouvailleLieu(t.id, t.k));
  b.dataset.trouvaille = `${t.id}_${t.k}`;
  cadre.append(fig);
  nom.textContent = t.gagnee ? affiche(majuscule(L.choses[t.k - 1].replace(/^(le |la |l')/, ""))) : "???";
  b.append(cadre, nom);
  b.onclick = () => {
    sfx.tap(); sauterDessin(fig);
    const niveau = paliersLieu()[t.k - 1];
    dire(t.gagnee ? `${majuscule(L.choses[t.k - 1])} : ta trouvaille ${duLieu(t.id)} !`
      : L.equipe ? `Un trésor caché au Belvédère des Six, sur l'Île des Bâtisseurs : il attend toute l'équipe au niveau ${niveau} !`
      : `Un trésor caché sur l'Île des Bâtisseurs : il attend ${nomFamille(L.famille)} au niveau ${niveau} !`);
  };
  return b;
}
