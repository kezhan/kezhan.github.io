/* Académia : l'écran du jardin, ouvert depuis le potager du village (js/jeu/jardin_village.js). Les parcelles vues de
   près sur le fond dessiné ; toucher une parcelle :
   1. vide : les sachets de graines, chacun avec son prix en étincelles ;
   2. qui a soif : une question rapide (le moteur choisit, une matière après l'autre) ; juste, l'arrosoir passe, la
      terre devient mouillée et la plante grandit d'un stade sous les yeux de l'enfant ; fausse, l'explication, et on
      peut réessayer avec une autre question ;
   3. arrosée aujourd'hui : elle grandira demain ;
   4. mûre : la récolte vole jusqu'au panier, fruit après fruit, comptée en allemand (« eins, zwei, drei… », le nombre
      écrit sur le panier) ; la voix dit son nom en français, en anglais, en allemand et en luxembourgeois
      (l'enregistrement de lod.lu seulement).
   Le panier garde les récoltes ; en toucher une la donne au compagnon (de l'XP, et son évolution s'il la gagne) :
   « Guten Appetit! » avant qu'il mange, « Danke! » après (js/mots_du_jeu.js).
   Les règles sont dans js/jardin.js, les dessins dans assets/<qualité>/jardin (outils/village/scripts/jardin.py). */
if (!ECRANS.includes("jardin")) ECRANS.push("jardin");   // one more screen for montrer() (js/outils.js)
const srcJardin = nom => `${DOSSIER_HD}/jardin/${nom}.png`;
const JARDIN = {occupe: false, bulle: 0};

function imgJardin(nom, cls){
  const i = document.createElement("img");
  i.className = cls || ""; i.alt = ""; i.draggable = false; i.decoding = "async"; i.src = srcJardin(nom);
  i.onerror = () => { if (!i.dataset.essai) { i.dataset.essai = "1"; i.src = srcJardin(nom) + "?essai=1"; } };
  return i;
}
// a line shown in the garden's bubble and said by the voice
function annoncerJardin(texte){
  const b = $("bulleJardin");
  ecrireIcones(b, affiche(texte)); b.hidden = false;   // « ! » never alone at the start of a line
  clearTimeout(JARDIN.bulle); JARDIN.bulle = setTimeout(() => { b.hidden = true; }, 2600 + texte.length * 45);
  return dire(texte);
}

// the garden lies next to the Montagne de Germania: every plant is named in German, with its article and its recorded
// voice, the French in small to understand it (Kezhan, 13/10/2026: « indiquer les plantes en allemand »)
const allemandDe = c => c.de;   // « die Karotte »: the article stays small, as in German
// a French line, then « En allemand : die Karotte » written and said by the German voice
function annoncerJardinDe(texte, c){
  const b = $("bulleJardin");
  ecrireIcones(b, affiche(texte));
  const l = el("span", "allemand-jardin"), m = el("b");
  m.textContent = allemandDe(c); l.append(affiche("En allemand :") + " ", m);
  b.append(" ", l); b.hidden = false;
  clearTimeout(JARDIN.bulle); JARDIN.bulle = setTimeout(() => { b.hidden = true; }, 4500 + texte.length * 45);
  return suiteDeVoix([() => dire(texte), () => dire("En allemand :"), () => dire(c.de, "de")]);
}

// a German word in the garden's bubble, its meaning in small (`qui`: who says it, before it)
function motJardin(m, qui){
  const b = $("bulleJardin"), mot = el("b", "mot-etranger"), sens = el("small", "sens-mot");
  mot.lang = m.langue; mot.textContent = m.mot; sens.textContent = m.sens;
  b.replaceChildren(...(qui ? [affiche(`${qui} :`) + " "] : []), mot, " ", sens); b.hidden = false;
  clearTimeout(JARDIN.bulle); JARDIN.bulle = setTimeout(() => { b.hidden = true; }, 3500);
}

function ouvrirJardin(){
  const p = P(); if (!p) return;
  const j = jardinDe(p);
  montrer("jardin");
  $("jardin").style.backgroundImage = `url("${srcJardin("fond")}")`;
  $("grainesJardin").hidden = true; $("panneauJardin").classList.add("cache");
  dessinerJardin();
  const a = aFaireAuJardin(p);
  if (!j.vu) {
    j.vu = true; sauver();
    annoncerJardin("Voici ton jardin ! Touche une parcelle pour planter une graine. Arrose ta plante chaque jour avec une bonne réponse : elle grandit !");
  } else if (a.mures) annoncerJardin(a.mures > 1 ? "Des plantes sont mûres : touche-les pour les récolter !" : "Une plante est mûre : touche-la pour la récolter !");
  else if (a.soif) annoncerJardin("Tes plantes ont soif : arrose-les avec une bonne réponse !");
}
function fermerJardin(){
  if (JARDIN.occupe) return;
  taire(); window.__calme = performance.now() + 250;
  montrer("monde");
  const m = JEU.scene.getScene("monde");
  if (m && m.jardin) m.jardin.maj();
}

function dessinerJardin(){
  const p = P(), j = jardinDe(p);
  $("jardinEtincelles").textContent = p.etincelles;
  $("parcelles").replaceChildren(...j.parcelles.map((pa, i) => parcelleDom(pa, i)));
  dessinerPanier();
}
function parcelleDom(pa, i){
  const e = etatParcelle(pa), b = el("button", "parcelle " + e);
  b.dataset.i = i; b.dataset.etat = e;
  const c = pa.culture && CULTURES[pa.culture];
  b.setAttribute("aria-label", c ? `${allemandDe(c)} (${c.nom}), ${{soif: "à arroser", arrosee: "arrosée", mure: "mûre"}[e]}` : "Parcelle vide");
  b.append(imgJardin(pa.arrosee === jour() ? "parcelle_mouillee" : "parcelle_seche", "sol"));
  if (c) {
    b.append(imgJardin(`${pa.culture}_${pa.stade}`, "plante"));
    const n = el("span", "nom-plante"); n.textContent = allemandDe(c); b.append(n);   // its German name, under it
  }
  if (e === "soif") b.append(imgJardin("goutte", "signe"));
  if (e === "mure") b.append(iconeDom("etoile", "signe"));
  b.onclick = () => toucherParcelle(i);
  return b;
}
function toucherParcelle(i){
  if (JARDIN.occupe) return;
  const pa = jardinDe().parcelles[i];
  sfx.tap();
  ({vide: () => ouvrirGraines(i), soif: () => arroserParcelle(i), mure: () => recolterParcelle(i),
    arrosee: () => annoncerJardinDe("Elle a bu aujourd'hui. Reviens demain pour l'arroser encore !", CULTURES[pa.culture])})[etatParcelle(pa)]();
}

// ---- planting: the seed packets
function ouvrirGraines(i){
  const p = P(), g = $("grainesJardin");
  g.replaceChildren();
  const tete = el("div", "entete");
  const titre = el("h2", "titre3", "Graines"), fermer = el("button", "rond fermer");
  fermer.setAttribute("aria-label", "Fermer"); fermer.append(iconeDom("croix"));
  fermer.onclick = () => { sfx.tap(); g.hidden = true; taire(); };
  tete.append(titre, fermer);
  const liste = el("div", "sachets");
  Object.entries(CULTURES).forEach(([id, c]) => {
    const b = el("button", "sachet" + (p.etincelles < c.prix ? " cher" : ""));
    b.dataset.culture = id;
    const nom = el("b"); nom.textContent = allemandDe(c);   // the German name first, the French in small under it
    const fr = el("small", "fr"); fr.textContent = majuscule(c.nom);
    const prix = el("span", "prix"); prix.append(iconeDom("etincelle"), " " + c.prix);
    b.setAttribute("aria-label", `${allemandDe(c)}, ${c.nom}, ${c.prix} étincelles`);
    b.append(imgJardin("graines_" + id), nom, fr, prix);
    b.onclick = () => planterParcelle(i, id);
    liste.append(b);
  });
  g.append(tete, liste); g.hidden = false; $("bulleJardin").hidden = true;
  dire("Quelle graine veux-tu planter ?");
}
function planterParcelle(i, id){
  const p = P(), c = CULTURES[id];
  if (p.etincelles < c.prix) return annoncerJardin(`Il te faut ${c.prix} étincelles. Tu en gagnes en battant les Ombres !`);
  if (!planter(i, id)) return;
  sfx.juste();
  $("grainesJardin").hidden = true;
  dessinerJardin();
  pousser(i);
  annoncerJardinDe(`Des graines de ${c.nom} sont plantées ! Arrose-les avec une bonne réponse.`, c);
}
// the plant of a plot pops (planted, or one stage taller)
function pousser(i){
  const pl = $("parcelles").children[i] && $("parcelles").children[i].querySelector(".plante");
  if (pl && !calme()) pl.animate([{transform: "scale(.4)", opacity: .3}, {transform: "scale(1.12)", opacity: 1, offset: .7}, {transform: "scale(1)"}],
    {duration: 520, easing: "cubic-bezier(.2,.8,.3,1.2)"});
}

// ---- watering: a right answer
async function arroserParcelle(i){
  const m = matiereArrosage(), q = m && tirer(m, "rapide");
  if (!q) return annoncerJardin("Pas de question pour l'instant : reviens dans un moment !");
  JARDIN.occupe = true;
  const panneau = $("panneauJardin"), ecran = $("jardin");
  ecran.classList.add("question");
  try {
    const r = await poserQuestion(panneau, q, {titre: "Arroser", jokers: false});
    noter(q, r.juste);
    panneau.classList.add("cache"); ecran.classList.remove("question");
    if (!r.juste) return annoncerJardin("Presque ! Touche ta plante pour une autre question.");
    await arrosoir(i);
    const culture = jardinDe().parcelles[i].culture, st = arroser(i);
    dessinerJardin(); pousser(i);
    // after the foreign word found, said again (js/question_vue.js), never over it; the child touched something else
    // meanwhile (a new bubble): left out
    const b0 = JARDIN.bulle;
    apresRedit(() => JARDIN.bulle === b0 && !$("jardin").hidden ? annoncerJardinDe(st >= STADE_MUR ? "Elle est mûre ! Touche-la pour la récolter." : "Bravo ! Ta plante a bu, elle grandit.", CULTURES[culture]) : null);
  } finally {
    JARDIN.occupe = false; panneau.classList.add("cache"); ecran.classList.remove("question");
  }
}
// the watering can tilts over the plot, drops fall
function arrosoir(i){
  const cible = $("parcelles").children[i];
  if (!cible || calme()) return Promise.resolve();
  // its spout (89 % across, 33 % down its drawing) just above the plant; it tips 35° to the right to pour
  const r = cible.getBoundingClientRect(), a = imgJardin("arrosoir", "arrosoir-vol");
  document.body.append(a);
  const W = a.getBoundingClientRect().width || 120, H = W * .8, cx = r.left + r.width / 2, cy = r.top + r.height * .25;
  a.style.left = `${cx - W * .89}px`; a.style.top = `${cy - H * .33 - H * .45}px`;
  a.animate([{transform: "rotate(0deg)", opacity: 0}, {transform: "rotate(35deg)", opacity: 1, offset: .25}, {transform: "rotate(35deg)", opacity: 1, offset: .8},
    {transform: "rotate(0deg)", opacity: 0}], {duration: 1100, easing: "ease-in-out"});
  for (let k = 0; k < 6; k++) {
    const g = imgJardin("goutte", "goutte-vol");
    g.style.left = `${cx - r.width * .12 + r.width * .05 * k}px`; g.style.top = `${cy}px`;
    document.body.append(g);
    g.animate([{transform: "translateY(0)", opacity: 0}, {transform: "translateY(0)", opacity: 1, offset: .1}, {transform: `translateY(${r.height * .55}px)`, opacity: 0}],
      {duration: 520, delay: 300 + k * 90, easing: "ease-in", fill: "both"}).onfinish = () => g.remove();
  }
  if (typeof jouerSon === "function") jouerSon("scintille");
  return wait(1100).then(() => a.remove());
}

// ---- picking: the crop flies to the basket, counted in German fruit by fruit (« eins, zwei, drei… », the number
// written over the basket), then its name in four languages
const NOMBRES_JARDIN = ["un", "deux", "trois", "quatre", "cinq", "six"];   // a harvest gives 1 to 6 (js/jardin.js)
async function recolterParcelle(i){
  const r = recolter(i); if (!r) return;
  const c = CULTURES[r.culture], depart = $("parcelles").children[i].getBoundingClientRect(), panier = $("panierJardin").getBoundingClientRect();
  JARDIN.occupe = true;
  sfx.victoire();
  const voler = k => {   // one fruit flies from the plot into the basket
    if (calme()) return;
    const v = imgJardin("recolte_" + r.culture, "recolte-vol");
    v.style.left = `${depart.left + depart.width / 2}px`; v.style.top = `${depart.top + depart.height / 2}px`;
    document.body.append(v);
    const dx = panier.left + 60 - depart.left - depart.width / 2, dy = panier.top + panier.height / 2 - depart.top - depart.height / 2;
    v.animate([{transform: "translate(-50%,-50%) scale(.3)"}, {transform: "translate(-50%,-150%) scale(1.1)", offset: .35}, {transform: `translate(calc(-50% + ${dx}px), calc(-50% + ${dy}px)) scale(.5)`}],
      {duration: 750, easing: "cubic-bezier(.3,.7,.4,1)", fill: "both"}).onfinish = () => v.remove();
  };
  dessinerJardin();
  JARDIN.occupe = false;
  // the count: each German number recorded, or the fruits fly as before (160 ms apart) without it
  const nombres = NOMBRES_JARDIN.slice(0, r.n).map(k => typeof motDuJeu === "function" ? motDuJeu(k, "de") : null);
  const compte = nombres.every(Boolean) ? el("b", "compte-recolte") : null;
  if (compte) { compte.lang = "de"; $("panierJardin").append(compte); $("bulleJardin").hidden = true; }   // the old message goes: eyes on the fruits
  let volees = 0;
  const reste = () => { for (; volees < r.n; volees++) voler(volees); };   // the voice cut by another one: the rest fly at once
  const comptage = compte ? nombres.map((m, k) => () => { voler(k); volees = k + 1; compte.textContent = m.mot; return Promise.all([dire(m.mot, "de"), wait(450)]); })
    : [() => { for (let k = 0; k < r.n; k++) setTimeout(() => voler(k), k * 160); volees = r.n; }];
  const nombre = `${r.n} ${r.n > 1 ? c.pluriel : c.nom} !`;
  const bulle = () => {
    ecrireIcones($("bulleJardin"), affiche(`${majuscule(nombre)} En allemand : ${allemandDe(c)}. En anglais : ${c.en}.` + (c.lb ? ` En luxembourgeois : ${c.lb}.` : "")));
    $("bulleJardin").hidden = false; clearTimeout(JARDIN.bulle); JARDIN.bulle = setTimeout(() => { $("bulleJardin").hidden = true; }, 9000);
  };
  // German first: the garden is the Montagne de Germania's
  const voix = [...comptage, () => { bulle(); return dire(`Tu as cueilli ${nombre} En allemand :`); }, () => dire(c.de, "de"), () => dire("En anglais :"), () => dire(c.en, "en")];
  if (c.lb && couvertLb(c.lb)) voix.push(() => dire("En luxembourgeois :"), () => dire(c.lb, "lb"));   // its recording, nothing else
  if (!(await suiteDeVoix(voix))) reste();
}

// ---- the basket: the crops, and the companion who eats them
function dessinerPanier(){
  const j = jardinDe(), c = actif(), box = $("panierJardin");
  box.replaceChildren();
  if (c) { const f = el("div", "compagnon-jardin"); f.append(imageHD(cleDe(c), "face", 110)); box.append(f); }
  const l = el("div", "recoltes"), sortes = Object.keys(j.recoltes);
  if (!sortes.length) l.append(el("small", "", "Ton panier attend ta première récolte."));
  sortes.forEach(id => {
    const b = el("button", "recolte"); b.dataset.culture = id;
    const n = el("b"); n.textContent = j.recoltes[id];
    b.append(imgJardin("recolte_" + id), n);
    b.setAttribute("aria-label", (c ? `Donner ${CULTURES[id].une} à ${nomCompagnon(c)}` : CULTURES[id].nom) + ` (${allemandDe(CULTURES[id])})`);
    b.title = allemandDe(CULTURES[id]);
    b.onclick = () => donnerAuCompagnon(id);
    l.append(b);
  });
  box.append(l);
}
async function donnerAuCompagnon(id){
  const c = actif();
  if (JARDIN.occupe || !c) return;
  const nom = nomCompagnon(c), g = donnerRecolte(id, c);
  if (!g) return;
  sfx.niveau();
  dessinerPanier();
  const f = $("panierJardin").querySelector(".compagnon-jardin");
  // « Guten Appetit! » before it eats, « Danke! » from the companion after (their recordings; their meaning for the
  // little one, the first times): words to say again at the family table
  const appetit = typeof motDuJeu === "function" ? motDuJeu("bon_appetit", "de") : null, danke = typeof motDuJeu === "function" ? motDuJeu("merci", "de") : null;
  if (appetit) { motJardin(appetit); await direMot(appetit); }
  if (f) { f.replaceChildren(imageHD(cleDe(c), "joie", 110)); if (!calme()) f.animate([{transform: "translateY(0)"}, {transform: "translateY(-18px)"}, {transform: "translateY(0)"}], {duration: 420, iterations: 2}); }
  const fini = await annoncerJardinDe(`Miam ! ${nom} a mangé ${CULTURES[id].une} : ${g.xp} points d'expérience !`, CULTURES[id]);
  if (fini && danke && !$("jardin").hidden) { motJardin(danke, nom); await direMot(danke); }   // nothing else said meanwhile
  if (g.evolue) { JARDIN.occupe = true; await sceneEvolution(c, g.stadeAvant); JARDIN.occupe = false; montrer("jardin"); }
  dessinerJardin();
}
