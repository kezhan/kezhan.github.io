/* Académia : l'écran d'un chantier de l'Île des Bâtisseurs, ouvert quand le héros arrive devant le panneau d'un terrain
   (js/jeu/chantiers_ile.js). Le bâtiment vu de près à son étape, ses quatre étapes en vignettes (faites, la prochaine,
   à venir), la cheffe de chantier qui dit l'étape suivante et son prix, et un gros bouton pour construire (la vignette
   de l'étape et son prix en étincelles : rien à lire, la voix dit tout).
   1. Le bouton touché : une question de la matière du bâtiment (js/chantiers.js, questionChantier). Juste : les
      étincelles sont payées, les marteaux frappent, le bâtiment passe à l'étape suivante sous les yeux de l'enfant.
      Fausse : l'explication, rien n'est perdu, le bouton propose une autre question.
   2. Pas assez d'étincelles : le bouton est pâle mais répond ; la cheffe dit combien il en manque et où en gagner.
   3. La quatrième étape : la fête, ce que le bâtiment apporte, et le bouton ramène sur l'île où il est dessiné fini.
   Les règles sont dans js/chantiers.js, les dessins dans assets/<qualité>/chantiers (outils/village/scripts/ile_chantiers.py). */
if (!ECRANS.includes("chantier")) ECRANS.push("chantier");   // one more screen for montrer() (js/outils.js)
const srcChantier = nom => `${DOSSIER_HD}/chantiers/${nom}.png`;
const CHANTIER = {id: null, occupe: false};

function imgChantier(nom, cls){
  const i = document.createElement("img");
  i.className = cls || ""; i.alt = ""; i.draggable = false; i.decoding = "async";
  imageReessayee(i, srcChantier(nom));
  return i;
}
// a line of the cheffe, written in her bubble and said by the voice
function annoncerChantier(texte){
  ecrireIcones($("bulleChantier"), affiche(texte));
  return dire(texte);
}

function ouvrirChantier(id){
  const p = P(); if (!p || !CHANTIERS[id]) return;
  CHANTIER.id = id;
  montrer("chantier");
  $("chantierTitre").textContent = majuscule(CHANTIERS[id].nom);
  imageReessayee($("chantierCheffe"), `${DOSSIER_HD}/portraits/cheffe.png`);
  $("panneauChantier").classList.add("cache");
  dessinerChantier();
  proposerEtape(true);
}
function fermerChantier(){
  if (CHANTIER.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  if (typeof majBarre === "function") majBarre();   // the étincelles spent show on the island's bar
  const m = JEU.scene.getScene("monde");
  if (m && m.chantiers && m.chantiers.maj) m.chantiers.maj(CHANTIER.id, true);   // the island draws what was built
}

function dessinerChantier(){
  const p = P(), id = CHANTIER.id, k = etapeChantier(id), e = etapeSuivante(id);
  $("chantierEtincelles").textContent = p.etincelles;
  $("chantier").dataset.etape = k;
  const dessin = $("chantierDessin"), [w, h] = IMAGES_CHANTIERS[k ? `${id}_${k}` : "terrain"];
  dessin.style.aspectRatio = `${w} / ${h}`;
  dessin.replaceChildren(imgChantier(k ? `${id}_${k}` : "terrain", "batiment"));
  if (id === "moulin" && k >= CHANTIER_FINI) {   // its sails turn, on its hub (POINTS_CHANTIERS, in world units from the bottom's middle)
    const [x, y] = POINTS_CHANTIERS.moulin.moyeu, a = imgChantier("moulin_ailes", "ailes");
    a.style.left = `${50 + x * 8 / w * 100}%`; a.style.top = `${100 + y * 8 / h * 100}%`; a.style.width = `${IMAGES_CHANTIERS.moulin_ailes[0] / w * 100}%`;
    dessin.append(a);
  }
  // the four steps: done (a tick), the next one (it beats), to come (pale)
  $("chantierEtapes").replaceChildren(...ETAPES_CHANTIER.map((x, i) => {
    const v = el("span", "etape-chantier " + (i < k ? "faite" : i === k ? "prochaine" : "avenir"));
    v.dataset.etape = x.id;
    v.append(imgChantier(`${id}_${i + 1}`));
    if (i < k) v.append(iconeDom("coche", "fait"));
    return v;
  }));
  const b = $("btnConstruire");
  b.replaceChildren();
  if (!e) { b.className = "gros vert"; b.dataset.etat = "fini"; b.append("Voir sur l'île ", iconeDom("suivant")); b.setAttribute("aria-label", "Voir sur l'île"); return; }
  const cher = p.etincelles < e.prix, prix = el("span", "prix");
  prix.append(iconeDom("etincelle"), " " + e.prix);
  b.className = "gros vert" + (cher ? " attente" : ""); b.dataset.etat = cher ? "cher" : "pret";
  b.append(imgChantier(`${id}_${e.k}`, "vignette"), "Construire ", prix);
  b.setAttribute("aria-label", `Construire ${e.nom} : ${e.prix} étincelles`);
}
// how many étincelles are missing for the next step (0: the child can pay)
const manqueChantier = (id, p = P()) => { const e = etapeSuivante(id, p); return e ? Math.max(0, e.prix - p.etincelles) : 0; };
const etincellesDites = n => n > 1 ? `${n} étincelles` : "1 étincelle";
// the cheffe says the next step, its price, and what to do (or how many étincelles are missing and where to win them)
function proposerEtape(accueil){
  const id = CHANTIER.id, e = etapeSuivante(id), petit = P().age <= 5, manque = manqueChantier(id);
  if (!e) return annoncerChantier(`${majuscule(CHANTIERS[id].nom)} est ${CHANTIERS[id].nom.startsWith("la ") ? "finie" : "fini"} ! ${PROMESSES_CHANTIER[id]}`);
  const quoi = `${e.nom} ${duChantier(id)}`;
  if (manque) return annoncerChantier(`Pour ${quoi}, il faut ${etincellesDites(e.prix)} : il t'en manque ${manque}. Console des Ombres dans les hautes herbes : chaque bonne réponse t'en donne !`);
  const debut = accueil && e.k === 1 ? `Ici, nous construisons ${CHANTIERS[id].nom} ! ${CHANTIERS[id].dit} ` : "";
  return annoncerChantier(petit ? `${debut}On construit ${quoi} ? Il faut ${etincellesDites(e.prix)} et une bonne réponse. Touche le gros bouton !`
    : `${debut}Prochaine étape : ${quoi}. Il faut ${etincellesDites(e.prix)} et une bonne réponse. On construit ?`);
}

// the big button: a question of the building's subject; right, the step is paid and built; wrong, nothing is lost
async function construireEtape(){
  if (CHANTIER.occupe) return;
  const id = CHANTIER.id, e = etapeSuivante(id);
  sfx.tap();
  if (!e) return fermerChantier();
  if (manqueChantier(id)) return proposerEtape();
  const q = questionChantier(id);
  if (!q) return annoncerChantier("Pas de question pour l'instant : reviens dans un moment !");
  CHANTIER.occupe = true;
  const panneau = $("panneauChantier"), ecran = $("chantier");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: `Construire ${e.nom}`, jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerChantier("Presque ! Rien n'est perdu : touche le bouton pour une autre question.");
    const fait = construire(id);
    if (!fait) return;
    await batir();
    dessinerChantier(); surgir();
    // after the foreign word found, said again (js/question_vue.js), never over it
    apresRedit(() => {
      if ($("chantier").hidden || CHANTIER.id !== id) return null;
      if (fait.fini) return feterChantier(id);
      const suite = etapeSuivante(id);
      return annoncerChantier(`Bravo ! Tu as ${fait.verbe} ${fait.nom} ${duChantier(id)} ! `
        + (manqueChantier(id) ? `Pour ${suite.nom}, il faudra ${etincellesDites(suite.prix)}.` : `On continue avec ${suite.nom} ?`));
    });
  } finally {
    CHANTIER.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
// the hammers knock, the drawing shakes, dust rises
async function batir(){
  const d = $("chantierDessin");
  if (calme()) return;
  for (let k = 0; k < 3; k++) {
    sfx.coup();
    d.animate([{transform: "translateY(0)"}, {transform: "translateY(-6px) rotate(-.6deg)"}, {transform: "translateY(0)"}], {duration: 200});
    const r = d.getBoundingClientRect();
    jaillir(r.left + r.width * (.3 + .2 * k), r.top + r.height * .8, {pieces: ["etincelle_p", "etoile_p"], n: 5, angle: [220, 320], gravite: 500, vitesse: [120, 240], taille: [10, 20], duree: [400, 700]});
    await wait(240);
  }
}
// the new step pops in
function surgir(){
  const b = $("chantierDessin").querySelector(".batiment");
  sfx.niveau(); if (typeof jouerSon === "function") jouerSon("scintille");
  if (b && !calme()) b.animate([{transform: "scale(.82)", opacity: .4}, {transform: "scale(1.06)", opacity: 1, offset: .7}, {transform: "scale(1)"}],
    {duration: 520, easing: "cubic-bezier(.2,.8,.3,1.2)"});
}
// the building is finished: confetti, the cheffe's bravo and what the building brings
function feterChantier(id){
  sfx.victoire();
  const r = $("chantierDessin").getBoundingClientRect();
  jaillir(r.left + r.width / 2, r.top + r.height * .4, {pieces: ["etoile", "etincelle", "etoile_p"], n: 26});
  return annoncerChantier(`Bravo ! ${majuscule(CHANTIERS[id].nom)} est ${CHANTIERS[id].nom.startsWith("la ") ? "finie" : "fini"} ! ${PROMESSES_CHANTIER[id]}`);
}
