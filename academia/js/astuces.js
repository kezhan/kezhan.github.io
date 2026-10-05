/* Académia : les astuces du Sage, montrées en vrai et une fois pour toutes. La potion de clarté : à la question qui
   suit une erreur, le Sage la montre (une bulle à son portrait, la potion qui brille) et le dit, quand la question a
   été lue : « Touche la potion : deux mauvaises réponses s'envolent ! ». L'astuce part dès que l'enfant touche la
   potion (alors elle est apprise) ou répond ; elle revient au plus trois fois. Rien en mode recette (TEST). */
const ASTUCE_POTION = "Touche la potion : deux mauvaises réponses s'envolent !";

function astucePotion(panneau){
  const p = P(); if (!p || TEST) return null;
  const vu = p.astuces = p.astuces || {};
  if (vu.potion || (vu.potionMontree || 0) >= 3) return null;
  // the potion button of the question (js/question_vue.js names it for screen readers)
  const potion = panneau.querySelector('[aria-label="Potion de clarté"]');
  if (!potion || potion.disabled) return null;
  vu.potionMontree = (vu.potionMontree || 0) + 1; sauver();
  const bulle = el("div", "astuce", `<img alt=""><p></p>`);
  bulle.querySelector("img").src = `${DOSSIER_HD}/portraits/sage.png`;
  bulle.querySelector("p").textContent = affiche(ASTUCE_POTION);
  bulle.hidden = true; document.body.append(bulle);
  let fini = false;
  const placer = () => {   // above the potion, the tail pointing at it; inside the screen
    const r = potion.getBoundingClientRect(), w = Math.min(360, innerWidth - 24);
    bulle.style.width = w + "px";
    bulle.style.left = Math.max(12, Math.min(innerWidth - w - 12, r.left + r.width / 2 - w + 46)) + "px";
    bulle.style.bottom = Math.max(12, innerHeight - r.top + 14) + "px";
    bulle.style.setProperty("--queue", (r.left + r.width / 2 - parseFloat(bulle.style.left)) + "px");
  };
  const fermer = () => {
    if (fini) return; fini = true;
    bulle.remove(); potion.classList.remove("montree");
    removeEventListener("resize", placer);
  };
  potion.addEventListener("click", () => { vu.potion = true; sauver(); fermer(); }, {once: true});
  addEventListener("resize", placer);
  // the panel slides up first; the Sage speaks once the question has been read
  setTimeout(() => { if (fini) return; placer(); bulle.hidden = false; potion.classList.add("montree"); }, 400);
  (async () => {
    const debut = Date.now();
    await wait(900);
    while (!fini && Date.now() - debut < 7000 && "speechSynthesis" in window && speechSynthesis.speaking) await wait(200);
    if (!fini) dire(ASTUCE_POTION);
  })();
  return {fermer};
}
