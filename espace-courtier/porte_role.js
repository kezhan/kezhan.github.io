/* Le choix du rôle, sur la porte : l'espace s'ouvre dans ce rôle.

   L'auteur, le 04-10 : « pré-remplis le mot de passe, et laisser choisir le rôle, mais par défaut,
   c'est admin, tout voir ». Les rôles sont ceux de l'espace (ROLES, js/roles.js), dans leur ordre,
   jamais recopiés : la page publiée les reçoit de publier.py dans le bloc #porteDonnees (clé, nom,
   résumé, famille, icône), l'aperçu local les lit dans js/roles.js.

   Le rôle choisi d'avance : celui de l'adresse (`?role=`, un lien qui ouvre un métier), sinon le
   dernier pris sur ce navigateur (« ec-porte:role »), sinon le rôle admin, qui voit tout. Un choix
   se garde, et s'écrit aussitôt dans l'adresse.

   L'espace lit son rôle dans l'adresse (`?role=`, avant le dièse, js/roles.js) : la porte l'y pose
   avant de l'ouvrir (PorteRole.entrer, appelé par porte.js), à l'entrée comme à la reprise d'une
   session gardée (« Rester connecté », un nouvel onglet). Le menu du compte permet toujours d'en
   changer ensuite : l'espace l'écrit dans l'adresse, la porte l'y suit et le garde. Un nouvel
   onglet, et la porte après une sortie, s'ouvrent ainsi sur le dernier rôle pris.

   Une liste à choix unique (le motif « select-only combobox » de l'APG) : le focus reste sur le
   champ, qui désigne l'option active ; les flèches, Début, Fin et une lettre la déplacent, Entrée
   ou Espace choisit, Échap referme, Tab choisit et passe au champ suivant. La liste s'ouvre sous le
   champ, au-dessus s'il n'y a pas la place, et reste toujours dans la fenêtre. */
(function(){
"use strict";
const CLE = "ec-porte:role", DEFAUT = "admin";
/* Qui a écrit le `?role=` de l'onglet (CLE_ROLE_QUI de js/roles.js) : si c'est une autre personne,
   l'espace ne le lit pas. La porte l'écrit pour la personne qui entre : la marque s'efface. */
const CLE_ROLE_QUI = "cr:role-qui";
const $ = (id)=> document.getElementById(id);
const garde = (f, sinon)=>{ try { return f(); } catch(e){ return sinon; } };
const esc = (s)=> String(s == null ? "" : s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
const simple = (s)=> String(s || "").normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();

function lesRoles(){
  const d = garde(()=> JSON.parse($("porteDonnees").textContent), null);
  if(d && Array.isArray(d.roles)) return d.roles.filter((r)=> r && r.cle && r.nom);
  // L'aperçu : les rôles de l'espace, chargés juste avant (js/rubriques.js, js/roles.js).
  const l = garde(()=> ROLES, null), rub = garde(()=> RUBRIQUE, null);
  return Array.isArray(l) ? l.map((r)=>({ cle:r.cle, nom:r.nom, resume:r.resume || "", dom:r.dom || "",
    svg:r.svg || (rub && rub(r.ic) ? rub(r.ic).ic : "") })) : [];
}

const LISTE = lesRoles();
const bloc = $("porteRoleBloc"), champ = $("porteRole"), liste = $("porteRoles");
window.PorteRole = null;
if(!LISTE.length || !bloc || !champ || !liste) return;

const rang = (cle)=> LISTE.findIndex((r)=> r.cle === cle);
const valide = (cle)=> rang(cle) >= 0 ? cle : null;
let choisi = valide(garde(()=> new URLSearchParams(location.search).get("role"), null))
  || valide(garde(()=> localStorage.getItem(CLE), null)) || valide(DEFAUT) || LISTE[0].cle;
let actif = rang(choisi), tape = "", tapeA = 0;

function ecrireAdresse(cle){
  const p = new URLSearchParams(location.search);
  if(p.get("role") === cle) return;
  p.set("role", cle);
  garde(()=> history.replaceState(history.state, "", location.pathname + "?" + p.toString() + location.hash));
}
/* Le rôle que l'espace prend ensuite (le menu du compte, la visite) s'écrit dans l'adresse par
   history.replaceState (js/roles.js) : la porte le suit et le garde, aussitôt. Sans cela, un nouvel
   onglet rouvrait le rôle choisi sur la porte, pas celui qu'on venait de prendre. */
function suivre(){
  const r = valide(garde(()=> new URLSearchParams(location.search).get("role"), null));
  if(r){ choisi = r; garde(()=> localStorage.setItem(CLE, r)); }
}
for(const n of ["replaceState", "pushState"]){
  const f = history[n];
  if(typeof f === "function") history[n] = function(){ const v = f.apply(this, arguments); suivre(); return v; };
}

/* ---- Le champ et sa liste ---- */
const icone = (r)=> `<svg viewBox="0 0 24 24">${r.svg || ""}</svg>`;
function dessinerChamp(){
  const r = LISTE[rang(choisi)];
  champ.dataset.dom = r.dom;
  $("porteRoleIc").innerHTML = icone(r);
  $("porteRoleNom").textContent = r.nom;
  $("porteRoleResume").textContent = r.resume;
  $("porteRoleResume").hidden = !r.resume;
}
function dessinerListe(){
  liste.innerHTML = LISTE.map((r, i)=> `<li role="option" class="porte-role-o" id="porteRole-${esc(r.cle)}" data-i="${i}"
    data-dom="${esc(r.dom)}" aria-selected="${r.cle === choisi}"><span class="porte-role-ic" aria-hidden="true">${icone(r)}</span>
    <span class="porte-role-t"><b>${esc(r.nom)}</b>${r.resume ? `<span>${esc(r.resume)}</span>` : ""}</span>
    <svg class="porte-role-coche" viewBox="0 0 24 24" aria-hidden="true"><path d="M5 12.5l4.5 4.5L19 7.5"/></svg></li>`).join("");
}
function marquer(){
  [...liste.children].forEach((o, i)=> o.classList.toggle("actif", i === actif));
  const o = liste.children[actif];
  if(!o || liste.hidden) return;
  champ.setAttribute("aria-activedescendant", o.id);
  const haut = o.offsetTop, bas = haut + o.offsetHeight;
  if(haut < liste.scrollTop) liste.scrollTop = haut - 5;
  else if(bas > liste.scrollTop + liste.clientHeight) liste.scrollTop = bas - liste.clientHeight + 5;
}
/* Sous le champ ; au-dessus s'il n'y a pas la place ; sinon par-dessus le champ, qu'elle couvre
   entier (jamais à moitié), et toujours dans la fenêtre. Couvert, le champ perd son anneau et sa
   bordure : ils dépassaient autour de la liste. */
function placer(){
  const s = liste.style, H = window.innerHeight, marge = 6, ecart = 4;
  s.top = ""; s.maxHeight = (H - 2 * marge) + "px";
  const r = champ.getBoundingClientRect(), h = liste.offsetHeight;
  const dessous = h <= H - marge - r.bottom - ecart, dessus = !dessous && h <= r.top - marge - ecart;
  const top = dessous ? r.height + ecart : dessus ? -(h + ecart)
    : Math.max(marge - r.top, Math.min(0, H - marge - h - r.top));
  s.top = Math.round(top) + "px";
  champ.classList.toggle("couvert", !dessous && !dessus);
}
function ouvrir(){
  if(!liste.hidden) return;
  actif = rang(choisi);
  dessinerListe();
  liste.hidden = false; champ.setAttribute("aria-expanded", "true");
  placer(); marquer();
}
function fermer(){
  tape = "";
  if(liste.hidden) return;
  liste.hidden = true; champ.setAttribute("aria-expanded", "false"); champ.removeAttribute("aria-activedescendant");
  champ.classList.remove("couvert");
}
function choisir(i){
  const r = LISTE[i]; if(!r) return;
  choisi = r.cle;
  garde(()=> localStorage.setItem(CLE, choisi));
  ecrireAdresse(choisi);
  dessinerChamp();
  [...liste.children].forEach((o, k)=> o.setAttribute("aria-selected", String(k === i)));
}

/* Une lettre : le rôle suivant dont le nom commence ainsi ; plusieurs lettres tapées vite
   cherchent le début du nom. La liste refermée, on repart de zéro. */
function lettre(c){
  const t = Date.now(), seule = t - tapeA > 700;
  tape = (seule ? "" : tape) + simple(c); tapeA = t;
  const meme = tape.split("").every((x)=> x === tape[0]), cherche = meme ? tape[0] : tape;
  const depart = liste.hidden ? rang(choisi) : actif, n = LISTE.length;
  for(let k = meme ? 1 : 0; k <= n; k++){
    const i = (depart + k) % n;
    if(simple(LISTE[i].nom).startsWith(cherche)){ ouvrir(); actif = i; marquer(); return; }
  }
}

champ.addEventListener("keydown", (e)=>{
  const ouvert = !liste.hidden, n = LISTE.length;
  const aller = (i)=>{ ouvrir(); actif = Math.max(0, Math.min(n - 1, i)); marquer(); };
  if(e.altKey && (e.key === "ArrowDown" || e.key === "ArrowUp")){ e.preventDefault(); if(ouvert && e.key === "ArrowUp"){ choisir(actif); fermer(); } else ouvrir(); return; }
  switch(e.key){
    case "ArrowDown": e.preventDefault(); if(ouvert) aller(actif + 1); else ouvrir(); break;
    case "ArrowUp": e.preventDefault(); if(ouvert) aller(actif - 1); else ouvrir(); break;
    case "Home": e.preventDefault(); aller(0); break;
    case "End": e.preventDefault(); aller(n - 1); break;
    case "PageDown": if(ouvert){ e.preventDefault(); aller(actif + 5); } break;
    case "PageUp": if(ouvert){ e.preventDefault(); aller(actif - 5); } break;
    case "Enter": case " ": e.preventDefault(); if(ouvert){ choisir(actif); fermer(); } else ouvrir(); break;
    case "Escape": if(ouvert){ e.preventDefault(); e.stopPropagation(); fermer(); } break;
    case "Tab": if(ouvert){ choisir(actif); fermer(); } break;
    default: if(e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey){ e.preventDefault(); lettre(e.key); }
  }
});
champ.addEventListener("click", ()=>{ if(liste.hidden) ouvrir(); else fermer(); });
champ.addEventListener("blur", ()=> fermer());
// Le focus reste sur le champ pendant qu'on clique dans la liste.
liste.addEventListener("mousedown", (e)=> e.preventDefault());
liste.addEventListener("click", (e)=>{
  const o = e.target.closest(".porte-role-o"); if(!o) return;
  choisir(Number(o.dataset.i)); fermer(); champ.focus();
});
liste.addEventListener("mousemove", (e)=>{
  const o = e.target.closest(".porte-role-o"); if(!o || Number(o.dataset.i) === actif) return;
  actif = Number(o.dataset.i); marquer();
});
document.addEventListener("mousedown", (e)=>{ if(!liste.hidden && !bloc.contains(e.target)) fermer(); });
// Une fenêtre qui change (un téléphone qu'on tourne, son clavier qui se range) : la liste se replace.
window.addEventListener("resize", ()=>{ if(!liste.hidden) placer(); });

window.PorteRole = {
  cle:()=> choisi,
  /* Au moment d'entrer, ou de reprendre une session gardée : le choix se garde, l'adresse dit le
     rôle, la marque d'une autre personne s'efface (CLE_ROLE_QUI). */
  entrer(){
    garde(()=> localStorage.setItem(CLE, choisi));
    ecrireAdresse(choisi);
    garde(()=> sessionStorage.removeItem(CLE_ROLE_QUI));
  },
};
dessinerChamp();
bloc.hidden = false;
})();
