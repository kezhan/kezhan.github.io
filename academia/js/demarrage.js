/* Académia : lancement. Charge la partie et les questions, branche les boutons des menus, puis reprend là où l'enfant
   s'était arrêté. Ouvert comme un fichier, ou arrivé sans un de ses scripts, il ne démarre pas : l'écran de secours
   d'index.html le dit. */
async function demarrer(){
  if (location.protocol === "file:" || (window.__scriptsManquants || []).length) return;
  // installable app, playable offline (GitHub Pages is https): the worker starts keeping the game from the first visit
  if ("serviceWorker" in navigator && location.protocol === "https:") navigator.serviceWorker.register("sw.js").catch(() => {});
  charger();
  $("btnNouveau").onclick = () => { sfx.tap(); ouvrirCreation(); };
  $("btnNouveau").disabled = false;
  $("version").textContent = `version ${VERSION} · images ${QUALITE === "hd" ? "haute définition" : "légères"}`;
  $("btnCreer").onclick = () => { sfx.tap(); creer(); };
  $("nomGardien").onkeydown = e => { if (e.key === "Enter") creer(); };
  document.querySelector("[data-retour]").onclick = () => { sfx.tap(); ouvrirAccueil(); };
  // the bag, and the grown-ups' corner that opens from it by a long press (js/parents.js)
  $("btnEquipe").onclick = () => { sfx.tap(); taire(); ouvrirEquipe(); };
  $("btnFermerEquipe").onclick = () => { sfx.tap(); fermerEquipe(); };
  $("equipe").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerEquipe(); } };
  appuiLong($("btnParents"), APPUI_PARENTS_MS, () => { sfx.juste(); ouvrirParents(); });
  $("btnFermerJardin").onclick = () => { sfx.tap(); fermerJardin(); };   // the garden (js/jardin_vue.js)
  $("btnFermerChantier").onclick = () => { sfx.tap(); fermerChantier(); };   // a building site of the island (js/chantier_vue.js)
  $("btnConstruire").onclick = () => construireEtape();
  $("btnFermerCoupole").onclick = () => { sfx.tap(); fermerCoupole(); };   // the dome of the Observatoire (js/coupole.js)
  $("btnVue_planetes").onclick = () => { sfx.tap(); vueCoupole("planetes"); };
  $("btnVue_terre").onclick = () => { sfx.tap(); vueCoupole("terre"); };
  $("btnExerciceCoupole").onclick = () => exerciceCoupole();
  $("btnFermerChose").onclick = () => { sfx.tap(); fermerChose(); };   // a thing of a house's room (js/maison_chose.js)
  $("btnExerciceChose").onclick = () => exerciceChose();
  $("btnFermerKleeschen").onclick = () => { sfx.tap(); fermerKleeschen(); };   // the Kleeschen's slippers (js/kleeschen.js)
  $("btnKleeschen").onclick = () => boutonKleeschen();
  $("btnFermerBoutique").onclick = () => { sfx.tap(); fermerBoutique(); };   // the merchant's shop (js/marchand.js)
  $("boutique").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerBoutique(); } };
  $("btnFermerParents").onclick = () => { sfx.tap(); fermerParents(); };
  $("parents").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerParents(); } };
  document.querySelectorAll("#ongletsParents button").forEach(b => { b.onclick = () => { sfx.tap(); ongletParents(b.dataset.onglet); }; });
  $("btnChanger").onclick = () => { sfx.tap(); taire(); fermerDialogue(); JEU.scene.stop("monde"); ouvrirAccueil(); };
  // on a PC: Escape closes the bag or the grown-ups' corner, Enter or Space moves a dialogue on (never through its choice buttons)
  document.addEventListener("keydown", e => {
    if (e.key === "Escape" && !$("equipe").hidden) { sfx.tap(); return fermerEquipe(); }
    if (e.key === "Escape" && !$("parents").hidden) { sfx.tap(); return fermerParents(); }
    if (e.key === "Escape" && !$("jardin").hidden) { sfx.tap(); return fermerJardin(); }
    if (e.key === "Escape" && !$("chantier").hidden) { sfx.tap(); return fermerChantier(); }
    if (e.key === "Escape" && !$("coupole").hidden) { sfx.tap(); return fermerCoupole(); }
    if (e.key === "Escape" && !$("chose").hidden) { sfx.tap(); return fermerChose(); }
    if (e.key === "Escape" && !$("kleeschen").hidden) { sfx.tap(); return fermerKleeschen(); }
    if (e.key === "Escape" && !$("missions").hidden) { sfx.tap(); return fermerMissions(); }   // the day's missions (js/missions.js)
    if (e.key === "Escape" && !$("boutique").hidden) { sfx.tap(); return fermerBoutique(); }
    const d = $("dialogue");
    if ((e.key === "Enter" || e.key === " ") && !e.repeat && !d.hidden && d.onclick && !e.target.closest("button, input")) { e.preventDefault(); d.onclick(); }
  });
  ouvrirAccueil();
  await chargerQuestions();
  chargerLexiqueLb();   // the Luxembourgish recordings' lexicon, ready before the first correction (js/voix.js)
  if (!window.__jeuPret) await new Promise(r => document.addEventListener("jeu-pret", r, {once: true}));
  window.__pret = true;
}
// the Android app's back button (android/): closes what is open and says so, else the app steps aside
window.retourAndroid = () => {
  if (!$("grainesJardin").hidden) { $("grainesJardin").hidden = true; return true; }
  if (!$("jardin").hidden) { fermerJardin(); return true; }
  if (!$("chantier").hidden) { fermerChantier(); return true; }
  if (!$("coupole").hidden) { fermerCoupole(); return true; }
  if (!$("chose").hidden) { fermerChose(); return true; }
  if (!$("kleeschen").hidden) { fermerKleeschen(); return true; }
  if (!$("missions").hidden) { fermerMissions(); return true; }
  if (!$("boutique").hidden) { fermerBoutique(); return true; }
  if (!$("parents").hidden) { fermerParents(); return true; }
  if (!$("equipe").hidden) { fermerEquipe(); return true; }
  if (ecranCourant === "creation" || ecranCourant === "choix") { ouvrirAccueil(); return true; }
  return false;
};
demarrer().catch(e => { console.error(e); if (typeof ecranIncomplet === "function") ecranIncomplet(); });
