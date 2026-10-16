/* Académia : le sac de l'enfant, en trois onglets dessinés : Équipe, Badges et Album (js/album.js). Rien n'y défile.
   Équipe : tous les compagnons en fiches basses (le dessin avec son niveau en pastille et le symbole de son élément,
   ses trois attaques en icônes, le nom, la barre d'XP, une étoile par notion réussie en jeu, la charge du combo, un
   bouton) ; ceux qu'il reste à libérer en silhouette. Le compagnon choisi suit le héros et combat. Au téléphone (ou
   sur un écran bas), des vignettes. Toucher un compagnon libéré ouvre sa fiche complète : son élément, ses trois
   attaques (rapide, massive, combo) écrites pour le grand, et la voix les dit pour la petite ; ses trois formes, celles
   à venir en silhouette avec le niveau qui les apporte ; d'où vient son nom (Bärli de « der Bär », l'ours ; Léiwchen
   de « de Léiw », le lion) et le mot de son élément dans les trois langues quand il est enregistré (le vent : wind,
   der Wind, de Wand). Un compagnon de langue choisi répond dans sa langue. Les pourcentages et les
   réglages sont dans le coin Parents (js/parents.js), ouvert par un appui long sur « Parents ». */
const ETOILE_SVG = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M12 2.4l2.9 5.9 6.5.9-4.7 4.6 1.1 6.5L12 17.2l-5.8 3.1 1.1-6.5-4.7-4.6 6.5-.9z" '
  + 'fill="#FFCF3D" stroke="#1D1640" stroke-width="1.7" stroke-linejoin="round"/><path d="M9.2 8.6l1.5-.2" stroke="#FFF6C9" stroke-width="1.6" stroke-linecap="round"/></svg>';
const sacCompact = () => matchMedia("(max-width: 560px), (max-height: 500px)").matches;
// the three attacks (js/compagnons.js, attaquesDe): their drawn icon (the same as in the fight) and what the voice says
const ATTAQUES_SAC = [
  {cls: "rapide", icone: "eclair", genre: "l'attaque rapide", aide: "Une question facile"},
  {cls: "massive", icone: "choc", genre: "la frappe massive", aide: "Plus dur, plus d'XP"},
  {cls: "combo", icone: "etoile", genre: "le combo", aide: "Trois bonnes réponses de suite"}];
// each family's element (FAMILLES[f].element): its drawn symbol (outils/icones, icones_sac.py) and how it is said
// (names of the bag's own: the fights may have their ELEMENTS and elementDe, js/compagnons.js)
const ELEMENTS_SAC = {"géométrie": ["element_geometrie", "la géométrie"], vent: ["element_vent", "le vent"],
  pierre: ["element_pierre", "la pierre"], "lumière": ["element_lumiere", "la lumière"],
  "stratégie": ["element_strategie", "la stratégie"], nature: ["element_nature", "la nature"]};
const elementSac = f => { const e = FAMILLES[f].element, x = ELEMENTS_SAC[e]; return {mot: e, icone: x ? x[0] : "etoile", dit: x ? x[1] : e}; };

// a notion the child succeeded in play (never one only taken for known when the game was created: js/adaptatif.js,
// placer): gagneeEnJeu() of the engine, or, with an engine that does not have it, mastered with two right answers
const reussieEnJeu = n => {
  if (typeof gagneeEnJeu === "function") return gagneeEnJeu(n);
  const e = P().maitrise[n.id]; return !!e && e.ok >= 2 && maitrisee(n);
};
const notionsReussies = matiere => notionsDe(matiere).filter(reussieEnJeu);

let ongletSacCourant = "equipe";
// the bag opens on its Team tab, or on the one asked (« badges », « album »: a sticker just won can be shown)
function ouvrirEquipe(onglet){
  const p = P(); if (!p) return;
  const m = JEU.scene.getScene("monde");
  if (ecranCourant === "monde" && (dialogueOuvert() || (m && (m.verrou || m.moment)))) return;
  if (m && m.chemin) { m.chemin = []; m.pnjVise = null; }
  montrer("equipe");
  ongletSac(["badges", "album"].includes(onglet) ? onglet : "equipe", true);
}
// a tab of the bag: its view drawn, the others hidden; the voice names it when the child touches it
function ongletSac(nom, muet){
  ongletSacCourant = nom;
  document.querySelectorAll("#ongletsSac [data-onglet]").forEach(b => b.setAttribute("aria-selected", String(b.dataset.onglet === nom)));
  const c = actif(), ico = $("icoOngletEquipe");   // the Team tab shows the companion who follows the hero
  if (ico && c && ico.dataset.cle !== cleDe(c)) { ico.replaceChildren(spriteCompagnon(c, 40)); ico.dataset.cle = cleDe(c); }
  $("vueSac").dataset.onglet = nom;
  $("badgesSac").hidden = nom !== "badges"; $("albumSac").hidden = nom !== "album";
  if (nom === "equipe") dessinerEquipe();
  else { $("listeEquipe").hidden = $("ficheEquipe").hidden = true; (nom === "badges" ? dessinerBadges : dessinerAlbum)($(nom === "badges" ? "badgesSac" : "albumSac")); }
  if (!muet) dire({equipe: "Ton équipe !", badges: texteOngletBadges(), album: "Ton album !"}[nom]);
}
document.querySelectorAll("#ongletsSac [data-onglet]").forEach(b => {
  b.onclick = () => { sfx.tap(); ongletSac(b.dataset.onglet); };
});
// turned or resized while open: the open tab is drawn again for the new screen (fiches or thumbnails, pages)
let redessinSac = 0;
addEventListener("resize", () => {
  clearTimeout(redessinSac);
  redessinSac = setTimeout(() => {
    if ($("equipe").hidden) return;
    if (ongletSacCourant === "equipe" && !$("ficheEquipe").hidden) return ouvrirFiche($("ficheEquipe").dataset.famille);
    ongletSac(ongletSacCourant, true);
  }, 150);
});

function dessinerEquipe(){
  const p = P(), l = $("listeEquipe"), compact = sacCompact();
  l.innerHTML = ""; l.hidden = false; $("ficheEquipe").hidden = true;
  l.classList.toggle("vignettes", compact);
  const rang = f => { const c = p.compagnons.find(x => x.famille === f); return c ? (c.id === p.actif ? 0 : 1) : 2; };
  Object.keys(FAMILLES).sort((a, b) => rang(a) - rang(b)).forEach(f => l.append(compact ? vignetteCompagnon(f) : ficheCompagnon(f)));
}
const compagnonDe = f => P().compagnons.find(x => x.famille === f) || null;
const A_REGION = {dojo: "au ", albion: "au ", germania: "à la ", duche: "à la ", jardin: "au ", observatoire: "à l'"};
// where a companion's name comes from (written in its full card, said with it): Bärli from « der Bär », the bear, in
// German; Léiwchen from « de Léiw », the lion, in Luxembourgish, the red lion of the flag (their recordings,
// js/mots_du_jeu.js); Melusino recalls Mélusine, for the big one. null for the others
const ORIGINES_NOMS = {barli: {cle: "ours", langue: "de"}, leiwchen: {cle: "lion", langue: "lb", plus: "le lion rouge du drapeau"}};
function origineNom(c){
  if (c.famille === "leiwchen" && stade(c) === 3) return P().age > 5 ? {texte: "Melusino rappelle Mélusine, la fée de la légende de Luxembourg."} : null;
  const o = ORIGINES_NOMS[c.famille], m = o && typeof motDuJeu === "function" ? motDuJeu(o.cle, o.langue) : null;
  return m ? {m, texte: `Son nom vient de « ${m.mot} » : ${m.sens}, en ${m.nom}${o.plus ? `, ${o.plus} !` : "."}`} : null;
}
// its words, said: the foreign one by its own voice, the French around it by the French voice
function voixOrigine(o, avant = "", apres = ""){
  if (!o.m) return [() => dire(`${avant}${o.texte}${apres}`)];
  const [debut, fin] = o.texte.split(`« ${o.m.mot} »`);
  return [() => dire(avant + debut), () => dire(o.m.mot, o.m.langue), () => dire(fin + apres)];
}
// what the voice says about a companion whose drawing is touched: name, level, where its name comes from, element,
// attacks, next evolution
function direCompagnon(f){
  const c = compagnonDe(f), r = REGIONS.find(x => x.famille === f), e = elementSac(f);
  if (!c) return dire(`Un compagnon à libérer, ${A_REGION[r.id] || "à "}${r.nom} ! Son élément : ${e.dit}.`);
  const st = stade(c), [a, b, k] = attaquesDe(c), o = origineNom(c);
  const debut = `${nomCompagnon(c)}, niveau ${c.niveau} ! `, reste = `Son élément : ${e.dit}. Ses attaques : ${a}, ${b} et ${k}.`
    + (st < 3 ? ` Il évolue au niveau ${PALIERS[st]}.` : " C'est sa forme ultime !");
  if (!o) return dire(debut + reste);
  suiteDeVoix(voixOrigine(o, debut, " " + reste));
}
// the element's word in the three languages, when they are recorded (the wind: « wind », « der Wind », « de Wand »)
const motsElement = f => typeof motsDuJeu === "function" && MOTS_DU_JEU[FAMILLES[f].element] ? ["en", "de", "lb"].map(l => motDuJeu(FAMILLES[f].element, l)).filter(Boolean) : [];
function direAttaque(c, i){
  const A = ATTAQUES_SAC[i];
  if (!c) return dire("Libère ce compagnon pour découvrir ses attaques !");
  dire(`${attaquesDe(c)[i]} : ${A.genre}. ${A.aide} !`);
}
// the three attacks: small icons to touch (a list card), or written out with their kind (the full card); never a
// <button> in a list card (the card's own button is the only one there)
function attaquesFiche(f, complete){
  const c = compagnonDe(f), noms = c ? attaquesDe(c) : ["???", "???", "???"], l = el("div", "attaques-fiche" + (complete ? " complete" : ""));
  ATTAQUES_SAC.forEach((A, i) => {
    const b = el(complete ? "button" : "span", "attaque-fiche " + A.cls);
    if (!complete) { b.setAttribute("role", "button"); b.tabIndex = 0; b.setAttribute("aria-label", c ? noms[i] : "Attaque à découvrir"); b.title = c ? noms[i] : ""; }
    b.append(iconeDom(A.icone));
    if (complete) { const t = el("span"), n = el("b"), s = el("small"); n.textContent = affiche(noms[i]); s.textContent = affiche(A.aide); t.append(n, s); b.append(t); }
    b.onclick = ev => { ev.stopPropagation(); sfx.tap(); sauterDessin(b); direAttaque(c, i); };
    l.append(b);
  });
  return l;
}
// the element's drawn symbol, on a corner of the drawing (a card or a thumbnail), or with its word (the full card)
function elementFiche(f, complete){
  const e = elementSac(f), d = el("span", complete ? "element-fiche" : "element-coin");
  d.append(iconeDom(e.icone));
  if (complete) d.append(e.mot[0].toUpperCase() + e.mot.slice(1));
  else d.title = e.mot;
  if (complete) d.onclick = () => {
    sfx.tap(); sauterDessin(d);
    const mots = motsElement(f);   // then its word in each language, by its own voice
    if (!mots.length) return dire(`Son élément : ${e.dit} !`);
    suiteDeVoix([() => dire(`Son élément : ${e.dit} !`), ...mots.flatMap(m => [() => dire(`En ${m.nom} :`), () => dire(m.mot, m.langue)])]);
  };
  return d;
}
// a companion's card: drawing and level, name, XP, stars, combo charge, attacks, element, one button; `complete`:
// the card opened alone (attacks written out, its three forms, back button)
function ficheCompagnon(f, complete = false){
  const p = P(), c = compagnonDe(f), r = REGIONS.find(x => x.famille === f);
  const d = el("div", "fiche" + (c ? "" : " inconnue") + (c && c.id === p.actif ? " active" : ""));
  d.dataset.famille = f;
  const gauche = el("div", "fiche-gauche"), dessin = el("div", "fiche-dessin"), fig = c ? habillerDom(spriteCompagnon(c, complete ? 120 : 92), c) : imageHD(f + "1", "face", 84);   // its accessory (js/jeu/accessoires.js)
  dessin.append(fig);
  if (c) dessin.append(el("span", "niv", `Niv. ${c.niveau}`));
  if (!complete) dessin.append(elementFiche(f, false));
  dessin.onclick = () => { sfx.tap(); sauterDessin(fig); direCompagnon(f); if (c && !complete) ouvrirFiche(f); };
  gauche.append(dessin);
  if (!complete) gauche.append(attaquesFiche(f, false));
  const corps = el("div", "fiche-corps", "<b></b>");
  corps.querySelector("b").textContent = c ? affiche(nomCompagnon(c)) : "???";
  const o = complete && c ? origineNom(c) : null;
  if (o) {   // where its name comes from: the foreign word in bold, said again when touched
    const l = el("button", "origine-nom"), cite = o.m ? affiche(`« ${o.m.mot} »`) : "", [debut, fin] = o.m ? affiche(o.texte).split(cite) : [affiche(o.texte), ""];
    l.append(debut);
    if (o.m) { const w = el("b"); w.lang = o.m.langue; w.textContent = cite; l.append(w, fin); }
    l.onclick = () => { sfx.tap(); suiteDeVoix(voixOrigine(o)); };
    corps.append(l);
  }
  if (complete) corps.append(elementFiche(f, true));
  const mots = complete ? motsElement(f) : [];
  if (mots.length) { const m = el("small", "mots-element"); m.textContent = mots.map(x => x.mot).join(" · "); corps.append(m); }
  if (!c) corps.append(ecrireIcones(el("small"), `À libérer : ${r.icone} ${r.nom}`));   // the house's drawn symbol (js/icones.js)
  else {
    corps.append(el("div", "barrexp petite", `<i style="width:${Math.min(100, 100 * c.xp / xpPour(c.niveau))}%"></i>`), etoilesFiche(c));
    if (complete) corps.append(attaquesFiche(f, true), formesFiche(c));
    const boutons = el("div", "boutons-fiche");
    if (complete) { const retour = ecrireIcones(el("button", "moyen gris retour"), "⬅ Équipe"); retour.onclick = () => { sfx.tap(); dessinerEquipe(); }; boutons.append(retour); }
    if (c.id !== p.actif) { const b = ecrireIcones(el("button", "moyen"), "⚔️ Avec moi !"); b.onclick = () => choisirCompagnon(c); boutons.append(b); }
    else boutons.append(ecrireIcones(el("span", "suit"), "🐾 Te suit partout"));
    corps.append(boutons);
  }
  d.append(gauche, corps);
  return d;
}
// its three forms (GDD §3): the ones reached in colour with their name, the ones to come in silhouette with the level
// that brings them (their names stay a surprise)
function formesFiche(c){
  const F = FAMILLES[c.famille], st = stade(c), l = el("div", "formes-fiche");
  [1, 2, 3].forEach(k => {
    if (k > 1) { const s = el("span", "fleche-forme"); s.append(iconeDom("suivant")); l.append(s); }
    const b = el("button", "forme-sac" + (k === st ? " actuelle" : k > st ? " a-venir" : "")), fig = imageHD(c.famille + k, "face", 52), n = el("small");
    n.textContent = k <= st ? affiche(F.noms[k - 1]) : `Niv. ${PALIERS[k - 1]}`;
    b.append(fig, n);
    b.setAttribute("aria-label", k <= st ? F.noms[k - 1] : `Forme secrète, niveau ${PALIERS[k - 1]}`);
    b.onclick = () => {
      sfx.tap(); sauterDessin(fig);
      dire(k === st ? `${F.noms[k - 1]} : sa forme d'aujourd'hui !` : k < st ? `${F.noms[k - 1]} : sa forme d'avant.`
        : `Une forme secrète ! ${nomCompagnon(c)} la prendra au niveau ${PALIERS[k - 1]}.`);
    };
    l.append(b);
  });
  return l;
}
// one star per notion succeeded in play (five drawn, then « +n »), and the combo's charge when there is one (c.charge,
// kept on the companion from one fight to the next by js/combat.js)
function etoilesFiche(c){
  const n = notionsReussies(FAMILLES[c.famille].matiere).length, l = el("div", "etoiles");
  l.title = `${n} notion${n > 1 ? "s" : ""} réussie${n > 1 ? "s" : ""} en jeu`;
  if (!n) l.innerHTML = `<span class="vide">${ETOILE_SVG}</span>`;
  else { l.innerHTML = ETOILE_SVG.repeat(Math.min(5, n)); if (n > 5) l.append(`+${n - 5}`); }
  const plein = typeof COMBO_PLEIN === "number" ? COMBO_PLEIN : 3, charge = Math.max(0, Math.min(plein, c.charge || 0));
  if (charge) {
    const k = el("span", "charge" + (charge >= plein ? " complete" : ""), "<i></i>".repeat(plein));
    [...k.children].forEach((x, i) => x.classList.toggle("remplie", i < charge));
    k.title = charge >= plein ? `${FAMILLES[c.famille].attaques[2]} est prêt !` : "Combo en charge";
    l.append(k);
  }
  return l;
}
// a phone or a low screen: a thumbnail, which opens the companion's card
function vignetteCompagnon(f){
  const p = P(), c = compagnonDe(f);
  const b = el("button", "vignette" + (c ? "" : " inconnue") + (c && c.id === p.actif ? " active" : ""), "<b></b>");
  const fig = c ? habillerDom(spriteCompagnon(c, 76), c) : imageHD(f + "1", "face", 70);
  b.prepend(fig); b.querySelector("b").textContent = c ? nomCompagnon(c) : "???";
  if (c) b.append(el("span", "niv", String(c.niveau)));
  b.append(elementFiche(f, false));
  b.onclick = () => { sfx.tap(); direCompagnon(f); if (c) ouvrirFiche(f); else sauterDessin(fig); };
  return b;
}
// a freed companion's card alone, in the place of the list (« ⬅ Équipe » brings the list back)
function ouvrirFiche(f){
  const l = $("listeEquipe"), v = $("ficheEquipe");
  if (!f || !compagnonDe(f)) return dessinerEquipe();
  l.hidden = true; v.hidden = false; v.innerHTML = ""; v.dataset.famille = f;
  v.append(ficheCompagnon(f, true));
}
function choisirCompagnon(c){
  const p = P(); p.actif = c.id; sauver(); sfx.juste();
  // a language companion answers in its language, written over its drawing (its meaning for the little one, the first times)
  const w = motCompagnon(c.famille);
  if (w) suiteDeVoix([() => dire(`${nomCompagnon(c)}, à toi !`), () => {
    bulleMot(document.querySelector("#listeEquipe .fiche.active .fiche-dessin, #listeEquipe .vignette.active"), w);
    return direMot(w);
  }]);
  else dire(`${nomCompagnon(c)}, à toi !`);
  ongletSac("equipe", true);
  const m = JEU.scene.getScene("monde"); if (m && m.majCompagnon) m.majCompagnon();
}
function fermerEquipe(){ window.__calme = performance.now() + 250; montrer("monde"); }
