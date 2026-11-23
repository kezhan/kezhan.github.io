/* Académia : le plan du village, ouvert devant le grand panneau de bois planté sous la place (js/jeu/sorties_village.js).
   Tout en dessins : le village au milieu, et autour, là où part son chemin, chaque quartier dans un gros rond (la vache
   de la ferme, le parasol du marché, la cloche de l'école, la chouette de la forêt, le coquillage de la plage). Un
   rond touché : la voix nomme le quartier, le plan se ferme, le héros s'y rend tout seul et le guide montre le chemin.
   Un quartier déjà visité porte une coche. Pour la petite, rien n'est à lire : pas de nom sous les ronds, la voix dit
   tout. Les quartiers : js/quartiers.js ; dessins : outils/village/scripts/quartiers_plan.py. */
if (!ECRANS.includes("plan")) ECRANS.push("plan");   // one more screen for montrer() (js/outils.js)
// where each quarter's round sits on the map, in % of its width and height (where its path leaves the village)
const PLAN_LIEUX = {ecole: [38, 15], foret: [11, 55], plage: [89, 52], marche: [30, 82], ferme: [52, 83]};

function annoncerPlan(texte){
  ecrireIcones($("bullePlan"), affiche(texte));
  return dire(texte);
}
function ouvrirPlan(){
  const p = P(); if (!p) return;
  montrer("plan");
  if (!histoireDe().plan) noterHistoire("plan");   // the little bubble over the board goes
  if (typeof noterJournal === "function") noterJournal("plan", {});
  const petit = p.age <= 5, vus = quartiersDe(p).visites;
  imageReessayee($("planFond"), `${dossierQuartiers()}plan_fond.png`);
  imageReessayee($("planPortrait"), `${DOSSIER_HD}/portraits/sage.png`);
  $("planLieux").replaceChildren(...Object.keys(PLAN_LIEUX).map(id => {
    const Q = QUARTIERS[id], [x, y] = PLAN_LIEUX[id], b = el("button", "plan-lieu" + (vus[id] ? " vu" : ""));
    b.type = "button"; b.dataset.quartier = id; b.style.left = x + "%"; b.style.top = y + "%";
    b.setAttribute("aria-label", majuscule(Q.nom));
    const i = document.createElement("img");
    i.alt = ""; i.draggable = false; imageReessayee(i, `${dossierQuartiers()}picto_${id}.png`);
    b.append(i);
    if (!petit) b.append(el("span", "plan-nom", majuscule(Q.nom)));
    if (vus[id]) b.append(iconeDom("coche", "plan-vu"));
    b.onclick = () => choisirQuartier(id);
    return b;
  }));
  return annoncerPlan(petit ? "Voici le plan du village ! Touche un dessin : ton héros y va tout seul."
    : "Voici le plan du village. Où veux-tu aller ? Touche un quartier : ton héros s'y rend, suis la flèche !");
}
function fermerPlan(){
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  const m = JEU.scene.getScene("monde");
  if (m && m.sorties && m.sorties.maj) m.sorties.maj();
}
// a round is touched: its name is said, the hero goes there by himself (js/jeu/sorties_village.js)
function choisirQuartier(id){
  const Q = QUARTIERS[id]; if (!Q) return;
  sfx.tap();
  fermerPlan();
  const m = JEU.scene.getScene("monde");
  if (m && m.sorties && m.sorties.aller) m.sorties.aller(id);
  dire(`${majuscule(Q.nom)} ! En route !`);
}
