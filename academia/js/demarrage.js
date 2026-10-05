/* Académia : lancement. Charge la partie et les questions, branche les boutons des menus, puis reprend là où l'enfant
   s'était arrêté. */
async function demarrer(){
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
  $("btnFermerParents").onclick = () => { sfx.tap(); fermerParents(); };
  $("parents").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerParents(); } };
  document.querySelectorAll("#ongletsParents button").forEach(b => { b.onclick = () => { sfx.tap(); ongletParents(b.dataset.onglet); }; });
  $("btnChanger").onclick = () => { sfx.tap(); taire(); fermerDialogue(); JEU.scene.stop("monde"); ouvrirAccueil(); };
  // on a PC: Escape closes the bag or the grown-ups' corner, Enter or Space moves a dialogue on (never through its choice buttons)
  document.addEventListener("keydown", e => {
    if (e.key === "Escape" && !$("equipe").hidden) { sfx.tap(); return fermerEquipe(); }
    if (e.key === "Escape" && !$("parents").hidden) { sfx.tap(); return fermerParents(); }
    const d = $("dialogue");
    if ((e.key === "Enter" || e.key === " ") && !e.repeat && !d.hidden && d.onclick && !e.target.closest("button, input")) { e.preventDefault(); d.onclick(); }
  });
  ouvrirAccueil();
  await chargerQuestions();
  if (!window.__jeuPret) await new Promise(r => document.addEventListener("jeu-pret", r, {once: true}));
  // installable app, playable offline (GitHub Pages is https)
  if ("serviceWorker" in navigator && location.protocol === "https:") navigator.serviceWorker.register("sw.js").catch(() => {});
  window.__pret = true;
}
demarrer();
