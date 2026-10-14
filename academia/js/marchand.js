/* Académia : la boutique du marchand de la place (le marchand et son étal : js/jeu/marchand_village.js). L'enfant y
   échange ses étincelles :
   1. contre des jokers (Potion de clarté, Indice du sage, Sablier à partir de 6 ans), cinq au plus de chacun
      (JOKER_MAX, js/progression.js) ;
   2. contre des accessoires dessinés (js/jeu/accessoires.js), des moins chers, gagnés en deux ou trois combats, aux
      plus beaux, qui demandent de la patience. Un accessoire acheté se porte : la cabine, à gauche, montre le
      compagnon qu'on habille (on choisit lequel sous lui) ; il le porte ensuite au village, au combat et dans le sac.
   Toucher un objet le montre : la voix dit son nom et son prix, le compagnon l'essaie dans la cabine. Toucher une
   seconde fois le même objet, après une petite pause, l'achète : on ne dépense jamais par erreur. Des touchers
   rapprochés de moins de 0,7 seconde ne comptent qu'une fois : la petite qui tapote un objet ne l'achète pas, et ne
   l'enlève pas pendant la fête qui suit l'achat ; le second toucher du double toucher qui a ouvert la boutique ne
   compte pas. Un accessoire déjà à soi se met ou s'enlève d'un toucher. Tout est dit à voix haute, rien n'est à lire
   pour avancer. Les prix des graines du jardin (js/jardin.js) ne changent pas. */
if (!ECRANS.includes("boutique")) ECRANS.push("boutique");   // one more screen for montrer() (js/outils.js)
// each joker: its drawn icon, its name, what it does (said), its price; the Hourglass from 6 years (the timed combo)
const JOKERS_BOUTIQUE = {
  potion: {icone: "potion", nom: "Potion de clarté", noms: "Potions de clarté", une: "Une Potion de clarté", la: "la", prix: 15,
    fait: "elle enlève deux mauvaises réponses."},
  indice: {icone: "chouette", nom: "Indice du sage", noms: "Indices du sage", une: "Un Indice du sage", la: "le", prix: 15,
    fait: "la chouette te rappelle la règle."},
  sablier: {icone: "sablier", nom: "Sablier", noms: "Sabliers", une: "Un Sablier", la: "le", prix: 20, age: 6,
    fait: "dix secondes de plus à chaque question du combo."}
};
// each accessory: its name with its article (said and shown), its price, from the cheapest
const ACCESSOIRES_BOUTIQUE = {
  noeud: {nom: "Nœud rose", le: "le nœud rose", la: "le", prix: 30},
  lunettes: {nom: "Lunettes étoiles", le: "les lunettes étoiles", la: "les", prix: 40},
  papillon: {nom: "Nœud papillon", le: "le nœud papillon", la: "le", prix: 40},
  echarpe: {nom: "Écharpe", le: "l'écharpe", la: "la", prix: 50},
  bonnet: {nom: "Bonnet à pompon", le: "le bonnet à pompon", la: "le", prix: 60},
  couronne: {nom: "Couronne de fleurs", le: "la couronne de fleurs", la: "la", prix: 80},
  chapeau: {nom: "Chapeau de magicien", le: "le chapeau de magicien", la: "le", prix: 200},
  cape: {nom: "Cape de héros", le: "la cape de héros", la: "la", prix: 300},
  couronne_or: {nom: "Couronne d'or", le: "la couronne d'or", la: "la", prix: 450}
};
// a touch on the same object less than 0.7 s after the previous one is the same touch (a little one hammering a
// button never buys: the second touch comes after a pause); a choice is forgotten after 12 s; the shop does not
// answer in its first half second (the second tap of a double tap on the stall lands on a shelf)
const CONFIRMER_APRES_MS = 700, CHOIX_DURE_MS = 12000, OUVERTURE_MS = 500;
const BOUTIQUE = {choix: null, quand: 0, pour: null, dernier: null, quandDernier: 0, ouverte: 0};   // the object touched
// once (and when), the family being dressed, the last object touched (and the last tap on it), when the shop opened

// ---- the rules
const jokersEnVente = (p = P()) => Object.keys(JOKERS_BOUTIQUE).filter(k => !JOKERS_BOUTIQUE[k].age || p.age >= JOKERS_BOUTIQUE[k].age);
const possede = (id, p = P()) => p.accessoires.possedes.includes(id);
function acheterJoker(k){
  const p = P(), j = JOKERS_BOUTIQUE[k];
  if (!j || !jokersEnVente(p).includes(k) || (p.jokers[k] || 0) >= JOKER_MAX || p.etincelles < j.prix) return false;
  p.etincelles -= j.prix; p.jokers[k] = (p.jokers[k] || 0) + 1;
  sauver(); return true;
}
function acheterAccessoire(id){
  const p = P(), a = ACCESSOIRES_BOUTIQUE[id];
  if (!a || possede(id, p) || p.etincelles < a.prix) return false;
  p.etincelles -= a.prix; p.accessoires.possedes.push(id);
  sauver(); return true;
}
// the companion of this family wears it (null: takes it off); only an accessory the child owns
function porterSur(famille, id){
  const p = P();
  if (id && !possede(id, p)) return false;
  if (id) p.accessoires.porte[famille] = id; else delete p.accessoires.porte[famille];
  sauver(); return true;
}

// ---- the screen
const compagnonHabille = () => { const p = P(); return p.compagnons.find(c => c.famille === BOUTIQUE.pour) || actif(); };
function ouvrirBoutique(){
  const p = P(); if (!p) return Promise.resolve();
  const c = actif();
  BOUTIQUE.choix = null; BOUTIQUE.pour = c ? c.famille : null; BOUTIQUE.dernier = null; BOUTIQUE.ouverte = performance.now();
  montrer("boutique");
  imageReessayee($("portraitMarchand"), `${DOSSIER_HD}/portraits/marchand.png`);
  dessinerBoutique();
  const premiere = !p.histoire.boutique;
  if (premiere) { p.histoire.boutique = new Date().toISOString(); sauver(); }
  const n = p.etincelles;
  const compte = n ? `Tu as ${n} étincelle${n > 1 ? "s" : ""}.` : "Tu n'as pas encore d'étincelles : tu en gagnes en battant des Ombres.";
  return dire(premiere
    ? `Bonjour, ${p.nom} ! Je suis le marchand. Ici, tes étincelles s'échangent contre des jokers et de jolis accessoires pour ton compagnon. ${compte} Touche un objet pour le voir !`
    : `Bonjour, ${p.nom} ! ${compte} Touche un objet pour le voir !`);
}
function fermerBoutique(){
  if (performance.now() - BOUTIQUE.ouverte < OUVERTURE_MS) return;   // the second tap of the double tap that opened it
  taire(); window.__calme = performance.now() + 250;
  BOUTIQUE.choix = null;
  montrer("monde"); majBarre();
  const m = JEU.scene.getScene("monde");
  if (m && m.marchand) m.marchand.fini();
}
function dessinerBoutique(){
  $("boutiqueEtincelles").textContent = P().etincelles;
  dessinerCabine();
  dessinerRayons();
}
// the fitting room: the companion being dressed (wearing the object chosen, to try it on), its name, what is chosen,
// and the team to choose whom to dress
function dessinerCabine(essai){
  const p = P(), c = compagnonHabille(), box = $("cabineBoutique");
  box.replaceChildren();
  if (!c) return;
  const scene = el("div", "cabine-scene"), choisi = BOUTIQUE.choix;
  const montre = essai !== undefined ? essai : accessoirePorte(c.famille);
  const fig = habillerDom(imageHD(cleDe(c), "face", 150), c, montre);
  fig.classList.add("cabine-compagnon");
  fig.onclick = () => { sfx.tap(); sauterDessin(fig); dire(`${nomCompagnon(c)} !`); };
  scene.append(fig);
  const info = el("div", "cabine-info"), nom = el("b"), etat = el("small");
  nom.textContent = affiche(nomCompagnon(c));
  if (choisi) {
    const [type, id] = choisi.split(":"), o = type === "a" ? ACCESSOIRES_BOUTIQUE[id] : JOKERS_BOUTIQUE[id];
    nom.textContent = affiche(o.nom);
    const manque = o.prix - p.etincelles;
    etat.className = manque > 0 ? "manque" : "encore";
    etat.replaceChildren(...(manque > 0 ? [iconeDom("etincelle"), ` ${affiche(`Il en manque ${manque}`)}`] : [iconeDom("coche"), " " + affiche("Touche encore pour acheter")]));
  } else etat.textContent = affiche(montre ? `porte ${ACCESSOIRES_BOUTIQUE[montre].le}` : "Touche un objet pour l'essayer");
  info.append(nom, etat);
  box.append(scene, info);
  if (p.compagnons.length > 1) {   // whom to dress: one drawn face per companion
    const qui = el("div", "qui-liste");
    p.compagnons.forEach(x => {
      const b = el("button", "qui"); b.dataset.famille = x.famille;
      b.setAttribute("aria-pressed", String(x.famille === c.famille)); b.setAttribute("aria-label", nomCompagnon(x));
      b.append(imageHD(cleDe(x), "face", 40));
      b.onclick = () => {
        sfx.tap(); BOUTIQUE.pour = x.famille; BOUTIQUE.choix = null;
        dessinerBoutique(); dire(`Tu habilles ${nomCompagnon(x)}.`);
      };
      qui.append(b);
    });
    box.append(qui);
  }
}
// the shelves: the jokers, then the accessories; each one a big button with its drawing and its price
function dessinerRayons(){
  const p = P(), c = compagnonHabille(), porte = c ? accessoirePorte(c.famille) : null;
  const j = $("rayonJokers"), a = $("rayonAccessoires");
  j.replaceChildren(titreRayon("potion", "Jokers"));
  jokersEnVente(p).forEach(k => {
    const o = JOKERS_BOUTIQUE[k], n = p.jokers[k] || 0, plein = n >= JOKER_MAX;
    const b = article("j:" + k, o.nom, o.prix, plein ? "plein" : "");
    b.querySelector(".dessin").append(iconeDom(o.icone, "icone-joker"));
    b.append(el("span", "compte" + (plein ? " plein" : ""), `${n}/${JOKER_MAX}`));
    j.append(b);
  });
  a.replaceChildren(titreRayon("etoile", "Accessoires"));
  Object.entries(ACCESSOIRES_BOUTIQUE).forEach(([id, o]) => {
    const b = article("a:" + id, o.nom, o.prix, possede(id, p) ? "possede" + (id === porte ? " porte" : "") : "");
    b.querySelector(".dessin").append(accessoireDom(id));
    a.append(b);
  });
}
const titreRayon = (icone, texte) => { const t = el("h3", "rayon-titre"); t.append(iconeDom(icone), " " + texte); return t; };
function article(cle, nom, prix, etat){
  const p = P(), b = el("button", "article " + etat + (BOUTIQUE.choix === cle ? " choisi" : ""));
  b.dataset.article = cle; b.setAttribute("aria-label", `${nom}, ${prix} étincelles`);
  const prixDom = el("span", "prix" + (p.etincelles < prix && !/possede/.test(etat) ? " cher" : ""));
  if (/possede/.test(etat)) prixDom.append(iconeDom("coche"));
  else prixDom.append(iconeDom("etincelle"), " " + prix);
  b.append(el("span", "dessin"), prixDom);
  if (BOUTIQUE.choix === cle && p.etincelles >= prix) b.append(iconeDom("coche", "encore"));
  b.onclick = () => toucherArticle(cle);
  return b;
}

// ---- a touch on an object
function toucherArticle(cle){
  const p = P(), c = compagnonHabille(), [type, id] = cle.split(":"), t = performance.now();
  if (t - BOUTIQUE.ouverte < OUVERTURE_MS) return;   // the tap that opened the shop, landing on a shelf
  // taps on the same object less than 0.7 s apart are one touch, however many (a little one hammering the button
  // never buys: the second touch comes after a pause; nor does she take off what she has just bought)
  const rafale = cle === BOUTIQUE.dernier && t - BOUTIQUE.quandDernier < CONFIRMER_APRES_MS;
  BOUTIQUE.dernier = cle; BOUTIQUE.quandDernier = t;
  if (rafale) return;
  const confirme = BOUTIQUE.choix === cle && t - BOUTIQUE.quand < CHOIX_DURE_MS;
  sfx.tap();
  if (type === "a" && possede(id, p)) return mettreOuEnlever(c, id);
  const o = type === "a" ? ACCESSOIRES_BOUTIQUE[id] : JOKERS_BOUTIQUE[id];
  if (type === "j" && (p.jokers[id] || 0) >= JOKER_MAX) {
    BOUTIQUE.choix = null; dessinerBoutique();
    return dire(`Tu as déjà ${JOKER_MAX} ${o.noms} : c'est le plus que tu peux porter !`);
  }
  if (confirme && p.etincelles >= o.prix) return type === "a" ? achatAccessoire(c, id) : achatJoker(id);
  // a first touch: the object is shown (tried on), its price said
  BOUTIQUE.choix = cle; BOUTIQUE.quand = t;
  dessinerRayons(); dessinerCabine(type === "a" ? id : undefined);
  const manque = o.prix - p.etincelles, ici = type === "a" ? majuscule(o.le) : o.une;
  const quoi = type === "a" ? `${ici} : ${o.prix} étincelles.` : `${ici} : ${o.fait} ${o.prix} étincelles.`;
  if (manque > 0) return dire(`${quoi} Il t'en manque ${manque}. Gagne-les en battant des Ombres, puis reviens !`);
  dire(`${quoi} Touche-${o.la} encore pour ${o.la} prendre !`);   // la: its pronoun (le, la, les)
}
function mettreOuEnlever(c, id){
  if (!c) return;
  const o = ACCESSOIRES_BOUTIQUE[id], enleve = accessoirePorte(c.famille) === id;
  porterSur(c.famille, enleve ? null : id);
  BOUTIQUE.choix = null;
  dessinerBoutique();
  if (!enleve) sauterDessin($("cabineBoutique").querySelector(".cabine-compagnon"));
  dire(enleve ? `${nomCompagnon(c)} enlève ${o.le}.` : `${nomCompagnon(c)} met ${o.le}. Qu'il est beau !`);
}
// bought: the price leaves, a cheer, the companion hops; an accessory goes straight on the companion being dressed
function achatAccessoire(c, id){
  const o = ACCESSOIRES_BOUTIQUE[id];
  if (!acheterAccessoire(id)) return;
  if (c) porterSur(c.famille, id);
  fete();
  dire(`Merci ! ${majuscule(o.le)} ${o.la === "les" ? "sont" : "est"} à toi !` + (c ? ` ${nomCompagnon(c)} ${o.la} porte déjà.` : ""));
}
function achatJoker(k){
  const o = JOKERS_BOUTIQUE[k];
  if (!acheterJoker(k)) return;
  fete();
  const n = P().jokers[k];
  dire(`Merci ! Tu as maintenant ${n} ${n > 1 ? o.noms : o.nom}.`);
}
// the cheer: touches on what was just bought count for nothing for 1.5 s, and as long as she keeps hammering it
function fete(){
  BOUTIQUE.choix = null; BOUTIQUE.quandDernier = performance.now() + 800;
  dessinerBoutique();
  sfx.juste(); if (typeof jouerSon === "function") jouerSon("magie");
  const fig = $("cabineBoutique").querySelector(".cabine-compagnon");
  if (fig) {
    sauterDessin(fig);
    if (!calme() && typeof confettisDom === "function") { const [x, y] = centre(fig); confettisDom(x, y, 22); }
  }
  const n = $("boutiqueEtincelles").parentElement;
  if (n && !calme()) n.animate([{transform: "scale(1)"}, {transform: "scale(1.25)"}, {transform: "scale(1)"}], {duration: 380, easing: "ease-out"});
}
