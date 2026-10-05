/* Académia : lancement. Charge la partie et les questions, puis reprend là où l'enfant s'était arrêté. */
async function demarrer(){
  charger();
  $("btnNouveau").onclick = () => { sfx.tap(); ouvrirCreation(); };
  $("btnNouveau").disabled = false;
  $("version").textContent = `version ${VERSION} · images ${QUALITE === "hd" ? "haute définition" : "légères"}`;
  $("btnCreer").onclick = () => { sfx.tap(); creer(); };
  $("nomGardien").onkeydown = e => { if (e.key === "Enter") creer(); };
  document.querySelector("[data-retour]").onclick = () => { sfx.tap(); ouvrirAccueil(); };
  $("btnEquipe").onclick = () => { sfx.tap(); taire(); ouvrirEquipe(); };
  $("btnFermerEquipe").onclick = () => { sfx.tap(); fermerEquipe(); };
  $("equipe").onclick = e => { if (e.target === e.currentTarget) { sfx.tap(); fermerEquipe(); } };
  // on a PC: Escape closes the bag, Enter or Space moves a dialogue on (never through its choice buttons)
  document.addEventListener("keydown", e => {
    if (e.key === "Escape" && !$("equipe").hidden) { sfx.tap(); return fermerEquipe(); }
    const d = $("dialogue");
    if ((e.key === "Enter" || e.key === " ") && !e.repeat && !d.hidden && d.onclick && !e.target.closest("button, input")) { e.preventDefault(); d.onclick(); }
  });
  // images: high definition or light (for a tablet short of memory); the page starts again with the other version
  $("btnImages").textContent = QUALITE === "hd" ? "🖼️ Images : haute définition" : "🖼️ Images : légères";
  $("btnImages").onclick = () => { sfx.tap(); choisirQualite(QUALITE === "hd" ? "leger" : "hd"); location.reload(); };
  $("btnChanger").onclick = () => { sfx.tap(); taire(); JEU.scene.stop("monde"); ouvrirAccueil(); };
  ouvrirAccueil();
  await chargerQuestions();
  if (!window.__jeuPret) await new Promise(r => document.addEventListener("jeu-pret", r, {once: true}));
  if (!Q.notions.length) toast("Les questions n'ont pas pu être chargées : ouvrez le jeu par son adresse internet", 6000);
  // installable app, playable offline (GitHub Pages is https)
  if ("serviceWorker" in navigator && location.protocol === "https:") navigator.serviceWorker.register("sw.js").catch(() => {});
  window.__pret = true;
}
demarrer();
