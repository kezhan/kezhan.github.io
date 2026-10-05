/* Académia : la boîte de dialogue des habitants et des maisons. Un portrait (un habitant, ou le dessin d'une créature :
   le chef des Ombres derrière sa porte), le texte qui s'écrit au rythme de la voix (un toucher l'affiche en entier),
   les lignes lues l'une après l'autre, des boutons de choix à la fin, lus eux aussi quand ils ont une phrase (dit).
   Un émoji de l'interface dans une ligne ou un bouton est montré par son icône dessinée (js/icones.js).
   Pendant qu'elle est ouverte, le village attend (js/jeu/monde.js, occupe), sauf sur la dernière ligne : un toucher sur
   le village la ferme et compte comme un toucher du village (l'Ombre touchée lance la marche).
   dialogue(nom, portrait, lignes, choix, opts) ; opts : quitter (un toucher sur le village ferme aussi une dernière
   ligne qui a des boutons, sans rien choisir), instantane (le texte d'un coup : une question aux parents),
   ligne(i, voix) (appelé à chaque ligne, voix : la promesse de la voix), fin() (appelé à la fermeture). */
const DLG = {fermable: false, fermer: null};
const dialogueOuvert = () => !$("dialogue").hidden;
const dialogueFermable = () => dialogueOuvert() && DLG.fermable;
// closes the open dialogue as if the child had walked away (no choice made); sansCalme: the touch that closed it
// goes on to the village
function fermerDialogue(sansCalme){ if (DLG.fermer) DLG.fermer(null, sansCalme); }

// the face in the box: a villager's portrait, or a creature drawn from its sheet (a boss)
function portraitDialogue(cle){
  const box = $("dlgPortrait"); box.innerHTML = ""; box.hidden = !cle;
  if (!cle) return;
  if (typeAtlas(cle) === "humains") { const img = el("img"); img.alt = ""; box.append(img); imageReessayee(img, `${DOSSIER_HD}/portraits/${cle}.png`); }
  else box.append(imageHD(cle, "face", 72));
}

function dialogue(nom, portrait, lignes, choix, opts = {}){
  if (DLG.fermer) DLG.fermer(null, true);   // a dialogue opened over another one takes its place
  const d = $("dialogue"), t = $("dlgTexte"), b = $("dlgChoix");
  d.hidden = false; ecrireIcones($("dlgNom"), nom);
  portraitDialogue(portrait);
  // the text is written letter by letter in place (the rest is there, invisible: the box keeps its size); a drawn icon
  // counts as one letter (its picture is made once, then moves from the rest to the written part)
  const ecrit = el("span"), reste = el("span", "dlg-reste");
  t.replaceChildren(ecrit, reste);
  let i = 0, minuteur = null;
  const lettresParSeconde = P() && P().age <= 5 ? 12 : 14;
  const fusionner = a => a.reduce((f, x) => { if (typeof x === "string" && typeof f[f.length - 1] === "string") f[f.length - 1] += x; else f.push(x); return f; }, []);
  const ecrire = texte => {
    clearInterval(minuteur); minuteur = null;
    const l = [];
    morceauxIcones(texte).forEach(m => { if (typeof m === "string") l.push(...m); else l.push(iconeDom(m.icone)); });
    let n = calme() || opts.instantane ? l.length : 0;
    const maj = () => { ecrit.replaceChildren(...fusionner(l.slice(0, n))); reste.replaceChildren(...fusionner(l.slice(n))); };
    maj();
    if (n < l.length) minuteur = setInterval(() => { n++; maj(); if (n >= l.length) { clearInterval(minuteur); minuteur = null; } }, 1000 / lettresParSeconde);
  };
  const entier = () => { if (!minuteur) return false; clearInterval(minuteur); minuteur = null; ecrit.append(...reste.childNodes); return true; };
  const fermer = (apres, sansCalme) => {
    clearInterval(minuteur); d.hidden = true; d.onclick = null; DLG.fermer = null; DLG.fermable = false; taire();
    if (!sansCalme) window.__calme = performance.now() + 250;
    if (opts.fin) opts.fin();
    if (apres) apres();
  };
  DLG.fermer = fermer;
  const afficher = () => {
    const derniere = i === lignes.length - 1, avecChoix = derniere && choix && choix.length;
    ecrire(lignes[i]); b.innerHTML = "";
    const dits = avecChoix ? choix.map(c => c.dit).filter(Boolean) : [];
    const voix = dire([lignes[i], ...dits].join(" "));
    if (opts.ligne) opts.ligne(i, voix);
    DLG.fermable = derniere && (!avecChoix || !!opts.quitter);
    if (avecChoix) choix.forEach((c, k) => {
      const x = ecrireIcones(el("button", "moyen" + (k ? " gris" : "") + (c.classe ? " " + c.classe : "")), c.texte);
      if (c.aide) x.setAttribute("aria-label", c.aide);
      if (!k) markOk(x);
      x.onclick = e => { e.stopPropagation(); sfx.tap(); if (typeof jouerSon === "function") jouerSon("bouton"); fermer(c.action); };
      b.append(x);
    });
    else {
      const x = markOk(el("button", "rond suite" + (derniere ? " fin" : "")));
      x.setAttribute("aria-label", derniere ? "Fermer" : "Suite"); x.append(iconeDom(derniere ? "coche" : "lecture"));
      b.append(x);
    }
  };
  // a touch on the box: the whole line first, then the next line (never through its choice buttons)
  d.onclick = () => {
    if (entier()) return;
    if (choix && choix.length && i === lignes.length - 1) return;
    sfx.tap(); i++; if (i < lignes.length) afficher(); else fermer();
  };
  afficher();
}
