/* Académia : la carte de fin d'un combat, le bilan seul (le badge, le compagnon libéré et l'évolution ont leur scène).
   Toutes les lignes ont leur place dès l'ouverture et entrent l'une après l'autre en fondu : « Continuer » ne bouge
   jamais et se voit tout de suite. Victoire : ruban, rayons qui tournent, confettis et étoiles dessinés, le compagnon
   en grand qui saute de joie (sa pose « joie »), deux lanceurs de confettis à ses pieds, sa coupe après un chef.
   Fatigue : bleu nuit doux, le compagnon fatigué (pose « fatigue ») qui respire lentement sous ses « Z » dessinés.
   Une Ombre enfuie : rien de grave, elle attend dans les herbes. Une pose pas encore dessinée montre la face
   (imageHD). Les icônes des lignes sont dessinées (js/icones.js). Sur un écran bas, la carte passe en deux colonnes
   (style_scenes.css). */
function carteBilan(b, c, region, gain, xpAvant, fin){
  return new Promise(resolu => {
    const issue = b.gagne ? "victoire" : b.fuite ? "fuite" : "fatigue";
    montrer("fin");
    const ecran = $("fin"), carte = $("finCarte");
    ecran.classList.add("bilan"); ecran.dataset.issue = issue;
    carte.innerHTML = ""; carte.className = "carte fin bilan"; carte.dataset.issue = issue;
    const titre = {victoire: b.boss ? "Victoire contre le chef !" : "Victoire !", fuite: "L'Ombre s'est enfuie", fatigue: "Ton compagnon se repose"}[issue];
    // the ribbon on top; then the companion (left on a low screen)
    const scene = el("div", "bilan-scene");
    const ruban = el("div", "ruban", "<span></span>"); ruban.firstChild.textContent = affiche(titre);
    const podium = el("div", "podium", issue === "victoire" ? `<div class="rayons"></div>` : "");
    const fig = el("div", "figure");
    // an evolving companion is still shown in its old form: the evolution scene reveals the new one
    const pose = {victoire: "joie", fatigue: "fatigue"}[issue] || "face", cle = gain.evolue ? c.famille + gain.stadeAvant : cleDe(c);
    fig.append(imageHD(cle, pose, 200));
    podium.append(fig); scene.append(podium);
    if (issue === "victoire") {   // party poppers at its feet (its cup in place of the right one after a chief), twinkles
      podium.append(iconeDom("confettis", "popper gauche"), b.boss ? iconeDom("trophee", "coupe") : iconeDom("confettis", "popper droite"));
      ["etoile", "etincelle", "etoile"].forEach((n, i) => podium.append(iconeDom(n, "scintille-bilan s" + (i + 1))));
    }
    if (issue === "fatigue") {   // its tired pose has its own « Z »; a drawing without it gets three drawn ones over its head
      const z = el("div", "zzz"); z.hidden = true;
      for (let i = 0; i < 3; i++) { const x = el("i"); x.append(iconeDom("dodo")); z.append(x); }
      podium.append(z);
      atlasJSON(cle, typeAtlas(cle)).then(a => { z.hidden = !!(a && a.frames.fatigue); });
    }
    // right (or below): the lines, the XP bar, « Continuer »
    const corps = el("div", "bilan-corps"), lignes = el("div", "lignes");
    const lignesTexte = [];
    // a line, its emoji drawn; the voice reads the text without it (js/voix.js)
    const ligne = (t, cls = "") => { lignes.append(ecrireIcones(el("p", "ligne-bilan " + cls), affiche(t))); lignesTexte.push(t); };
    const s = n => n > 1 ? "s" : "";
    const nomAvant = FAMILLES[c.famille].noms[gain.stadeAvant - 1];
    ligne(`${b.bonnes} bonne${s(b.bonnes)} réponse${s(b.bonnes)} sur ${b.total}` + (b.xp ? ` · +${b.xp} XP` : ""), "fort");
    if (fin.rattrapage > 1 && b.xp) ligne(`🚀 ${gain.evolue ? nomAvant : nomCompagnon(c)} rattrape les autres : XP doublée !`);
    if (b.revanches) ligne(`🔁 ${b.revanches} revanche${s(b.revanches)} gagnée${s(b.revanches)} : XP triplée !`);
    if (b.defis) ligne(`👑 ${b.defis} défi${s(b.defis)} du chef réussi${s(b.defis)} !`);
    if (issue === "fatigue") ligne(b.xp > 0 ? "Ce n'est pas grave : il se repose et revient plus fort. Tu as gagné de l'expérience !" : "Ce n'est pas grave : ton compagnon se repose. Réessaie, tu vas y arriver !");
    if (issue === "fuite") ligne(b.boss ? `${majuscule(region.chef)} t'attend encore dans sa maison. Réessaie, tu vas y arriver !` : "Elle se cache dans les herbes. Retrouve-la : tu vas y arriver !");
    if (b.gagne && !b.boss) {   // how far the door is
      const n = Math.min(regionEtat(b.region).etape, ETAPES);
      ligne(n >= ETAPES ? `🔓 ${region.nom} : la porte est ouverte ! ${region.boss} t'attend.`
        : `${region.icone} ${region.nom} : ${n} Ombre${s(n)} sur ${ETAPES} avant la porte`);
    }
    if (fin.cadeau) ligne("🎁 Le village te remercie : une Potion de clarté et un Indice du sage !");
    if (fin.joker) ligne({potion: "🧪 Tu trouves une Potion de clarté !", indice: "🦉 Tu trouves un Indice du sage !", sablier: "⏳ Tu trouves un Sablier pour le combo !"}[fin.joker]);
    if (gain.niveaux) ligne(`⬆️ ${gain.evolue ? nomAvant : nomCompagnon(c)} passe au niveau ${c.niveau} !`, "fort niveau");
    if (gain.evolue) ligne(`✨ Oh ? ${nomAvant} évolue…`, "evolue");
    const barre = el("div", "barrexp", "<i></i>");
    const bas = el("div", "ligne suite-fin");
    const suite = markOk(ecrireIcones(el("button", "gros"), gain.evolue ? "Regarder ➡" : "Continuer ➡"));
    let parti = false;
    suite.onclick = () => {
      if (parti) return; parti = true;
      sfx.tap(); if (typeof jouerSon === "function") jouerSon("bouton");
      taire(); ecran.classList.remove("bilan"); delete ecran.dataset.issue;
      resolu();
    };
    bas.append(suite); corps.append(lignes, barre, bas);
    carte.append(ruban, scene, corps);

    // the lines come in one after the other (their room is already taken: nothing moves)
    const toutes = [...lignes.children];
    if (calme()) toutes.forEach(x => x.classList.add("vu"));
    else toutes.forEach((x, i) => setTimeout(() => { if (!parti) x.classList.add("vu"); }, 250 + 280 * i));
    dire(titre + ". " + lignesTexte.filter(t => !/^🚀/.test(t)).join(" "));
    if (issue === "victoire") {
      if (typeof jouerSon === "function") jouerSon("victoire");
      setTimeout(() => { if (!parti) { const [x, y] = centre(fig); confettisDom(x, y, b.boss ? 46 : 30); } }, 120);
    }
    // the XP bar fills from where it was; a level gained fills it up first
    const vers = Math.min(100, 100 * c.xp / xpPour(c.niveau));
    barre.firstChild.style.width = (gain.niveaux ? 0 : Math.min(100, 100 * xpAvant.xp / xpPour(xpAvant.niveau))) + "%";
    setTimeout(() => { if (!parti) barre.firstChild.style.width = vers + "%"; }, 300);
    if (gain.niveaux) setTimeout(() => { if (!parti) sfx.niveau(); }, 250 + 280 * toutes.findIndex(x => x.classList.contains("niveau")));
  });
}
