/* Académia : lancement. Charge la partie et les questions, puis reprend là où l'enfant s'était arrêté. */
async function demarrer(){
  charger();
  $("btnNouveau").onclick = () => { sfx.tap(); ouvrirCreation(); };
  $("btnCreer").onclick = () => { sfx.tap(); creer(); };
  $("nomGardien").onkeydown = e => { if (e.key === "Enter") creer(); };
  document.querySelector("[data-retour]").onclick = () => { sfx.tap(); ouvrirAccueil(); };
  $("btnEquipe").onclick = () => { sfx.tap(); taire(); ouvrirEquipe(); };
  $("btnFermerEquipe").onclick = () => { sfx.tap(); fermerEquipe(); };
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
