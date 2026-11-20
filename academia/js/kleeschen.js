/* Académia : l'écran du Kleeschen, ouvert depuis son dialogue sur la place (js/jeu/fetes_village.js). Les cinq
   chaussons du 1er au 5 décembre, chacun avec son jour : plein, vide, celui d'aujourd'hui qui bat. Le Kleeschen parle
   dans sa bulle (son nom par l'enregistrement de lod.lu), et un gros bouton :
   1. du 1er au 5 : une question à la hauteur de l'enfant (js/adaptatif.js, une matière par jour) ; juste, le chausson
      du jour est rempli (une fois par jour) ; fausse, rien n'est perdu, une autre question ;
   2. à partir du 6 : le cadeau s'ouvre, des étincelles selon les chaussons remplis et l'autocollant du Kleeschen dans
      l'album, une seule fois par an.
   Les règles et les dates sont dans js/fetes.js. */
if (!ECRANS.includes("kleeschen")) ECRANS.push("kleeschen");
const ECRAN_KLEESCHEN = {occupe: false, ouvert: false};

function annoncerKleeschen(texte){
  ecrireIcones($("bulleKleeschen"), affiche(texte));
  return direMixte(texte, {langue: "lb"});
}
function ouvrirKleeschen(){
  const p = P(); if (!p || !momentKleeschen(p)) return;
  montrer("kleeschen");
  imageReessayee($("kleeschenPortrait"), `${DOSSIER_HD}/portraits/kleeschen.png`);
  $("panneauKleeschen").classList.add("cache");
  $("kleeschenCadeau").hidden = true; ECRAN_KLEESCHEN.ouvert = false;
  dessinerKleeschen();
  const cadeau = momentKleeschen(p) === "cadeau", n = etatKleeschen(p).chaussons.length;
  if (cadeau) annoncerKleeschen(n ? `Regarde : ${n > 1 ? `tes ${n} chaussons sont pleins` : "ton chausson est plein"} ! Et j'ai un cadeau pour toi. Touche le bouton pour l'ouvrir !`
    : "J'ai un cadeau pour toi. Touche le bouton pour l'ouvrir !");
  else if (chaussonDuJourPlein(p)) annoncerKleeschen("Ton chausson d'aujourd'hui est plein, bravo ! Reviens demain pour le suivant.");
  else annoncerKleeschen(p.age <= 5 ? "Voici tes chaussons ! Touche le gros bouton : une bonne réponse remplit celui d'aujourd'hui."
    : "Voici tes chaussons, un par jour jusqu'au 5 décembre. Une bonne réponse remplit celui d'aujourd'hui !");
}
function fermerKleeschen(){
  if (ECRAN_KLEESCHEN.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  if (typeof majBarre === "function") majBarre();
}

function dessinerKleeschen(){
  const p = P(), m = momentKleeschen(p), cadeau = m === "cadeau";
  $("kleeschenEtincelles").textContent = p.etincelles;
  $("kleeschen").dataset.moment = m || "";
  $("kleeschenChaussons").replaceChildren(...chaussonsKleeschen(p).map(c => {
    const v = el("span", "chausson"), n = el("b");
    v.dataset.etat = c.plein ? "plein" : c.aujourdhui ? "jour" : c.passe || cadeau ? "passe" : "avenir";
    v.dataset.jour = c.jour;
    n.textContent = c.jour;
    v.append(iconeDom(c.plein ? "chausson_plein" : "chausson"), n);
    return v;
  }));
  const b = $("btnKleeschen");
  b.replaceChildren(); b.className = "gros vert";
  if (ECRAN_KLEESCHEN.ouvert || !m || m === "adieu") { b.dataset.etat = "fini"; b.append("Continuer ", iconeDom("suivant")); b.setAttribute("aria-label", "Continuer"); }
  else if (cadeau) { b.dataset.etat = "cadeau"; b.append(iconeDom("cadeau"), " Ouvrir"); b.setAttribute("aria-label", "Ouvrir le cadeau"); }
  else if (chaussonDuJourPlein(p)) { b.dataset.etat = "demain"; b.classList.add("attente"); b.append(iconeDom("chausson_plein"), " Demain"); b.setAttribute("aria-label", "Reviens demain"); }
  else { b.dataset.etat = "question"; b.append(iconeDom("chausson"), " Question"); b.setAttribute("aria-label", "Une question pour le chausson du jour"); }
}

// a question of the child's level; the subject changes with the day
function questionKleeschen(){
  const ms = REGIONS.map(r => r.matiere), d = Number(moisJourFete().slice(3)) || 0;
  for (let k = 0; k < ms.length; k++) { const q = tirer(ms[(d + k) % ms.length], "rapide"); if (q) return q; }
  return null;
}
async function boutonKleeschen(){
  if (ECRAN_KLEESCHEN.occupe) return;
  sfx.tap();
  const etat = $("btnKleeschen").dataset.etat, p = P();
  if (etat === "fini") return fermerKleeschen();
  if (etat === "cadeau") return ouvrirCadeauKleeschen();
  if (etat === "demain") return annoncerKleeschen("Ton chausson d'aujourd'hui est déjà plein. Reviens demain !");
  const q = questionKleeschen();
  if (!q) return annoncerKleeschen("Pas de question pour l'instant : reviens dans un moment !");
  ECRAN_KLEESCHEN.occupe = true;
  const panneau = $("panneauKleeschen"), ecran = $("kleeschen");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: "Le chausson du jour", jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerKleeschen("Presque ! Rien n'est perdu : touche le bouton pour une autre question.");
    if (!remplirChausson(p)) return;
    dessinerKleeschen();
    sfx.niveau(); if (typeof jouerSon === "function") jouerSon("scintille");
    const c = $("kleeschenChaussons").querySelector('[data-etat="plein"][data-jour="' + Number(moisJourFete().slice(3)) + '"]');
    if (c && !calme()) { const r2 = c.getBoundingClientRect(); jaillir(r2.left + r2.width / 2, r2.top + r2.height / 2, {pieces: ["etoile_p", "etincelle_p"], n: 12}); }
    const dernier = moisJourFete() === KLEESCHEN.veille;
    apresRedit(() => $("kleeschen").hidden ? null : annoncerKleeschen(dernier ? "Bravo ! Ton chausson est plein. Cette nuit, je passe : reviens demain matin !"
      : "Bravo ! Ton chausson d'aujourd'hui est plein. Reviens demain pour le suivant !"));
  } finally {
    ECRAN_KLEESCHEN.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
// the gift of the 6th: the étincelles and the sticker, shown and said
function ouvrirCadeauKleeschen(){
  const g = donnerCadeauKleeschen();
  if (!g) return fermerKleeschen();
  ECRAN_KLEESCHEN.ouvert = true;
  const z = $("kleeschenCadeau"), im = el("img"), t = el("b");
  im.alt = ""; im.src = AUTOCOLLANTS[KLEESCHEN.autocollant].image;
  t.append("+" + g.etincelles + " ", iconeDom("etincelle"));
  z.replaceChildren(im, t); z.hidden = false;
  dessinerKleeschen();
  sfx.victoire();
  if (!calme()) { const r = z.getBoundingClientRect(); jaillir(r.left + r.width / 2, r.top + r.height / 2, {pieces: ["etoile", "etincelle", "etoile_p"], n: 24}); }
  annoncerKleeschen(`${g.etincelles} étincelles pour toi, et l'autocollant du « Kleeschen » dans ton album ! À l'année prochaine !`);
}
