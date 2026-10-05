/* La porte de l'espace courtier publié : un mot de passe, et un contenu chiffré.

   L'auteur, le 03-10 : « mettre un mot de passe login, même si c'est html ... peut-être que tu
   fais un hash, sans stockage des mots en clair ? ». Mieux qu'une empreinte comparée : l'espace
   publié est chiffré (AES-256-GCM) par une clé de contenu, elle-même chiffrée par la clé que
   donne chaque mot de passe (PBKDF2-SHA-256, 600 000 tours). La page ne contient aucune empreinte,
   et chaque essai coûte une fraction de seconde de calcul.

   Le mot de passe de démonstration arrive déjà saisi, et l'on choisit le rôle dans lequel l'espace
   s'ouvre (porte_role.js) ; l'auteur, le 04-10 : « pré-remplis le mot de passe, et laisser choisir
   le rôle, mais par défaut, c'est admin, tout voir ». publier.py l'écrit dans la page publiée
   seulement (le bloc #porteDonnees), jamais dans rag ; celui de l'accès complet n'est écrit nulle
   part.

   Deux mots de passe ouvrent le même contenu : celui de la démonstration et celui d'un accès
   complet. La démonstration a été limitée à cinq heures d'utilisation par navigateur (l'auteur :
   « limiter la démo à 5h ? et dire que si les gens veulent plus, faut demander un accès »), puis
   ne l'est plus (le 05-10 : « tu peux supprimer la limitation de 5h ») : `limite_s` vaut 0
   (_outils/publier.py), rien ne se compte et la porte ne parle d'aucune durée. La mécanique reste,
   pour une limite qu'on remettrait. Avec une limite, le temps ne court que pendant
   qu'on se sert de l'espace (page affichée, un geste dans les cinq dernières minutes, ou la voix
   qui parle) ; il est gardé dans le stockage du navigateur et dans un cookie, et le plus grand
   des deux fait foi.

   Écrite dans espace_courtier/porte/, publiée par _outils/publier.py. L'espace ne sait rien de
   la porte : elle pose sa coquille, ses feuilles et ses scripts, puis ajoute à la barre la
   pastille du temps qui reste.

   L'auteur, le 03-10 : « à gauche, une image, illustration, avec bouton pour lancer
   présentation, à droite login et mot de passe », puis « pas besoin de mot de passe pour lancer
   la présentation sur la page de login ». La présentation est publiée en clair à côté de la
   porte (presentation/presentation_publique.js, fabriquée par publier.py) et ne se charge
   qu'au premier clic ; si elle manque, la porte le dit en une ligne. Une fois chargée, l'espace
   ne s'ouvre plus dans la même page : ses scripts redéclareraient ceux de la présentation.

   La sortie : window.porteSortir() oublie la session et revient à la porte. La pastille s'en
   sert, le menu du compte de l'espace aussi quand elle existe.

   Ouverte du disque (porte/porte.html, sans contenu chiffré), la porte est un aperçu : elle
   s'affiche comme en ligne, un mot de passe pour la forme déjà saisi ; tout mot de passe mène à
   ../index.html?role=…#/accueil, et la présentation à ../index.html#/presentation
   (porte.html#fin montre la fin de la démonstration). */
(function(){
"use strict";
const C = window.CONTENU_CHIFFRE, APERCU = !C;
const CLE_SESSION = "ec-porte:session", CLE_TEMPS = "ec-porte:temps", COOKIE = "ec_porte_temps";
const LIMITE = ((C && C.limite_s) || 5 * 3600) * 1000;
/* Une limite à zéro : la démonstration n'a plus de temps compté, elle s'ouvre comme l'accès complet
   (l'auteur, le 05-10 : « tu peux supprimer la limitation de 5h »). */
const SANS_LIMITE = !!C && C.limite_s === 0;
const INACTIF = 5 * 60 * 1000, PAS = 15000;
const $ = (id)=> document.getElementById(id);
const garde = (f, sinon)=>{ try { return f(); } catch(e){ return sinon; } };
const b64 = (s)=> Uint8Array.from(atob(s), (c)=> c.charCodeAt(0));
const en64 = (u)=> btoa(String.fromCharCode.apply(null, Array.from(u)));
/* Le mot de passe de démonstration, écrit d'avance : celui que publier.py pose dans la page
   publiée ; dans l'aperçu, que tout mot ouvre, un mot pour la forme. */
const DONNEES = garde(()=> JSON.parse($("porteDonnees").textContent), null) || {};
const MDP_DEMO = typeof DONNEES.mdp === "string" && DONNEES.mdp ? DONNEES.mdp : APERCU ? "apercu" : "";

/* ---- Le temps d'utilisation, gardé deux fois ---- */
const chemin = ()=> location.pathname.replace(/[^/]*$/, "") || "/";
function lireCookie(){
  const m = garde(()=> document.cookie.match(new RegExp("(?:^|; )" + COOKIE + "=(\\d+)")), null);
  return m ? Number(m[1]) : 0;
}
const lireTemps = ()=> Math.max(Number(garde(()=> localStorage.getItem(CLE_TEMPS), 0)) || 0, lireCookie());
function ecrireTemps(ms){
  const v = String(Math.round(ms));
  garde(()=> localStorage.setItem(CLE_TEMPS, v));
  garde(()=>{ document.cookie = `${COOKIE}=${v}; max-age=31536000; path=${chemin()}; SameSite=Lax`; });
}
const reste = ()=> SANS_LIMITE ? Infinity : Math.max(0, LIMITE - lireTemps());
function duree(ms){
  const min = Math.ceil(ms / 60000), h = Math.floor(min / 60), m = min % 60;
  return h ? `${h} h${m ? " " + String(m).padStart(2, "0") : ""}` : `${m} min`;
}

/* ---- La session : la clé du contenu et l'identifiant saisi, jamais le mot de passe ----
   « Rester connecté » la garde aussi dans le stockage durable : un nouvel onglet la reprend, au
   nom de la même personne (l'espace lit l'identifiant dans l'onglet, « ec-porte:qui »). */
const CLE_QUI = "ec-porte:qui";
const magasins = ()=> [garde(()=> sessionStorage, null), garde(()=> localStorage, null)].filter(Boolean);
function lireSession(){
  for(const m of magasins()){ const v = garde(()=> JSON.parse(m.getItem(CLE_SESSION)), null); if(v && v.k) return v; }
  return null;
}
function poserSession(cle, niveau, longtemps, qui){
  const v = JSON.stringify({ k:en64(cle), n:niveau, q:qui || "" });
  garde(()=> sessionStorage.setItem(CLE_SESSION, v));
  if(longtemps) garde(()=> localStorage.setItem(CLE_SESSION, v));
}
function oublierSession(){
  magasins().forEach((m)=> garde(()=> m.removeItem(CLE_SESSION)));
  garde(()=> sessionStorage.removeItem(CLE_QUI));
}
/* Une session gardée reprend (un nouvel onglet) : la personne qui l'a ouverte, et le dernier rôle
   pris, que l'adresse ne dit pas encore (porte_role.js). */
function reprendre(s){
  if(s.q) garde(()=> sessionStorage.setItem(CLE_QUI, s.q));
  if(window.PorteRole && !garde(()=> new URLSearchParams(location.search).get("role"), null)) PorteRole.entrer();
}
/* La sortie retire `?role=` de l'adresse, comme le menu du compte (js/compte.js) : par l'une ou
   par l'autre, la porte revient sur le dernier rôle pris. */
function sortir(){
  oublierSession();
  garde(()=>{ const p = new URLSearchParams(location.search); p.delete("role");
    history.replaceState(history.state, "", location.pathname + (p.toString() ? "?" + p : "") + location.hash); });
  location.reload();
}
window.porteSortir = sortir;

/* ---- Le chiffre ---- */
async function cleDuMotDePasse(mdp){
  const base = await crypto.subtle.importKey("raw", new TextEncoder().encode(mdp.normalize("NFC")), "PBKDF2", false, ["deriveKey"]);
  return crypto.subtle.deriveKey({ name:"PBKDF2", salt:b64(C.sel), iterations:C.tours, hash:"SHA-256" },
    base, { name:"AES-GCM", length:256 }, false, ["decrypt"]);
}
/* La clé du contenu, si ce mot de passe en ouvre une ; le niveau d'accès avec. */
async function essayer(mdp){
  const k = await cleDuMotDePasse(mdp);
  for(const a of C.acces){
    try { return { cle:new Uint8Array(await crypto.subtle.decrypt({ name:"AES-GCM", iv:b64(a.iv) }, k, b64(a.cle))), niveau:a.niveau }; }
    catch(e){ /* pas celui-ci */ }
  }
  return null;
}
async function dechiffrer(cle){
  const k = await crypto.subtle.importKey("raw", cle, "AES-GCM", false, ["decrypt"]);
  const zip = await crypto.subtle.decrypt({ name:"AES-GCM", iv:b64(C.iv) }, k, b64(C.donnees));
  const flux = new Blob([zip]).stream().pipeThrough(new DecompressionStream("gzip"));
  return JSON.parse(await new Response(flux).text());
}

/* ---- L'espace, posé une fois la porte ouverte ---- */
function chargerScript(src){
  return new Promise((ok, ko)=>{ const s = document.createElement("script"); s.src = src; s.onload = ok; s.onerror = ko; document.head.appendChild(s); });
}
/* L'identifiant n'est pas contrôlé (le mot de passe est la seule clé) : il dit seulement qui
   l'espace accueille. Une personne de l'équipe est reconnue à son adresse ; sinon le nom se lit
   dans l'adresse (prénom.nom). */
function nomDe(adresse){
  const local = String(adresse || "").split("@")[0].replace(/[._-]+/g, " ").trim();
  return local ? local.replace(/\b\p{L}/gu, (l)=>l.toUpperCase()) : "";
}
function accueillir(){
  const qui = garde(()=> sessionStorage.getItem(CLE_QUI), "") || "";
  if(!qui) return;
  const m = (window.EQUIPE && EQUIPE.membres || []).find(x=> (x.email || "").toLowerCase() === qui.toLowerCase());
  const nom = m ? m.nom : nomDe(qui);
  const cab = document.querySelector(".cr-cab"); if(!cab || !nom) return;
  const av = cab.querySelector(".av"), b = cab.querySelector("b"), sous = b && b.parentNode.lastChild;
  if(av) av.textContent = nom.split(/\s+/).map(x=>x[0]).slice(0, 2).join("").toUpperCase();
  if(b) b.textContent = nom;
  if(m && sous && sous.nodeType === 3) sous.textContent = m.fonction.toLowerCase();
}

async function ouvrirEspace(contenu, niveau){
  const porte = $("porte"); if(porte) porte.remove();
  document.body.classList.remove("porte");
  const style = document.createElement("style"); style.textContent = contenu.css; document.head.appendChild(style);
  document.body.insertAdjacentHTML("afterbegin", contenu.coquille);
  // Les bibliothèques publiques (la table, la carte) sont publiées en clair, et chargées avant l'espace.
  for(const h of (C.vendeurs_css || [])){ const l = document.createElement("link"); l.rel = "stylesheet"; l.href = h; document.head.appendChild(l); }
  for(const v of (C.vendeurs || [])) await chargerScript(v);
  // Un script posé ainsi s'exécute à l'instant, dans l'ordre : le même ordre que index.html.
  for(const s of contenu.scripts){
    const el = document.createElement("script");
    el.textContent = s.code + "\n//# sourceURL=" + s.nom;
    document.body.appendChild(el);
  }
  poserPastille(niveau);
  accueillir();
  if(niveau === "demo") compter.demarrer();
}

/* ---- La pastille de la barre : le temps qui reste, et la sortie ---- */
function poserPastille(niveau){
  const hote = $("barreQui"); if(!hote) return;
  const b = document.createElement("button");
  b.type = "button"; b.className = "porte-pastille" + (niveau === "demo" ? "" : " complet"); b.id = "portePastille";
  b.setAttribute("aria-haspopup", "true"); b.setAttribute("aria-expanded", "false");
  hote.prepend(b);
  const menu = document.createElement("div"); menu.className = "porte-menu"; menu.id = "porteMenu"; menu.hidden = true;
  document.body.appendChild(menu);
  const peindre = ()=>{
    const r = reste(), part = Math.max(0, Math.min(1, r / LIMITE));
    b.innerHTML = niveau === "demo" ? `<i aria-hidden="true"></i><span>${duree(r)}</span>` : `<i aria-hidden="true"></i><span>Accès complet</span>`;
    b.title = niveau === "demo" ? `Accès de démonstration : il reste ${duree(r)} d'utilisation` : "Accès complet";
    b.classList.toggle("bientot", niveau === "demo" && r <= 15 * 60000);
    menu.innerHTML = niveau === "demo"
      ? `<p class="porte-menu-t">Accès de démonstration</p>
         <p>Il reste <b>${duree(r)}</b> d'utilisation sur ce navigateur.</p>
         <div class="porte-jauge" aria-hidden="true"><i style="width:${(part * 100).toFixed(1)}%"></i></div>
         <p class="porte-menu-n">Le temps ne court que pendant qu'on se sert de l'espace. Pour continuer au-delà, demandez un accès à la personne qui vous a transmis ce lien.</p>
         <button type="button" class="porte-sortir">Se déconnecter</button>`
      : `<p class="porte-menu-t">Accès complet</p><p>Sans limite de durée.</p><button type="button" class="porte-sortir">Se déconnecter</button>`;
  };
  peindre();
  setInterval(peindre, 30000);
  const ouvrir = (oui)=>{
    menu.hidden = !oui; b.setAttribute("aria-expanded", String(oui));
    if(oui){ peindre(); const r = b.getBoundingClientRect(); menu.style.top = (r.bottom + 8) + "px";
      menu.style.right = Math.max(12, window.innerWidth - r.right) + "px"; }
  };
  b.addEventListener("click", (e)=>{ e.stopPropagation(); ouvrir(menu.hidden); });
  menu.addEventListener("click", (e)=>{ e.stopPropagation(); if(e.target.closest(".porte-sortir")) sortir(); });
  document.addEventListener("click", ()=> ouvrir(false));
  document.addEventListener("keydown", (e)=>{ if(e.key === "Escape" && !menu.hidden) ouvrir(false); });
}

function avertir(texte){
  const t = document.createElement("div"); t.className = "porte-avis"; t.setAttribute("role", "status");
  t.innerHTML = `<span>${texte}</span><button type="button" aria-label="Fermer">×</button>`;
  document.body.appendChild(t);
  t.querySelector("button").addEventListener("click", ()=> t.remove());
  setTimeout(()=> t.classList.add("vu"), 20);
  setTimeout(()=> t.remove(), 12000);
}

/* ---- Le compteur : la page affichée, et quelqu'un devant ---- */
const compter = (()=>{
  let derniere = Date.now(), tic = performance.now(), averti = 0;
  const geste = ()=>{ derniere = Date.now(); };
  function pas(visible){
    const n = performance.now(), dt = Math.min(n - tic, 2 * PAS); tic = n;
    const parle = garde(()=> window.speechSynthesis && speechSynthesis.speaking, false);
    const actif = (visible || document.visibilityState === "visible") && (Date.now() - derniere < INACTIF || parle || !!document.fullscreenElement);
    if(actif) ecrireTemps(lireTemps() + dt);
    const r = reste();
    if(r <= 0){ location.reload(); return; }
    if(r <= 5 * 60000 && averti < 2){ averti = 2; avertir(`Il reste ${duree(r)} de démonstration.`); }
    else if(r <= 15 * 60000 && averti < 1){ averti = 1; avertir(`Il reste ${duree(r)} de démonstration. Pour continuer ensuite, demandez un accès.`); }
  }
  return { demarrer(){
    ["pointerdown", "keydown", "wheel", "touchstart", "scroll"].forEach((t)=> addEventListener(t, geste, { passive:true, capture:true }));
    let bouge = 0;
    addEventListener("mousemove", ()=>{ const n = Date.now(); if(n - bouge > 5000){ bouge = n; geste(); } }, { passive:true });
    document.addEventListener("visibilitychange", ()=>{ if(document.visibilityState === "hidden") pas(true); else tic = performance.now(); });
    addEventListener("pagehide", ()=> pas(true));
    setInterval(()=> pas(false), PAS);
  } };
})();

/* ---- La porte elle-même ---- */
/* Ce qu'il reste à faire, en une phrase : le mot de passe déjà saisi, il ne reste que le rôle. */
function consigne(){
  const role = !!window.PorteRole;
  if(MDP_DEMO) return role ? "Choisissez un rôle, puis connectez-vous." : "Le mot de passe de démonstration est déjà saisi.";
  return role ? "Choisissez un rôle et entrez le mot de passe qui vous a été transmis." : "Entrez le mot de passe qui vous a été transmis.";
}
/* Le mot de passe de démonstration dans son champ, sauf à la fin de la démonstration, qu'il
   n'ouvre plus ; « Déjà saisi » tant que c'est lui. */
function preremplir(oui){
  const c = $("porteMdp");
  if(oui && MDP_DEMO && !c.value) c.value = MDP_DEMO;
  if(!oui && MDP_DEMO && c.value === MDP_DEMO) c.value = "";
  majSaisi();
}
const majSaisi = ()=>{ $("porteSaisi").hidden = !MDP_DEMO || $("porteMdp").value !== MDP_DEMO || document.body.classList.contains("porte-fin"); };
function montrer(etat, erreur){
  const fin = etat === "fin";
  document.body.classList.toggle("porte-fin", fin);
  $("porteTitre").textContent = fin ? "Votre démonstration est terminée" : "Connexion";
  $("porteTexte").innerHTML = fin
    ? `Les ${duree(LIMITE)} de démonstration ont été utilisées sur ce navigateur. Pour continuer, <b>demandez un accès</b> à la personne qui vous a transmis ce lien, puis entrez-le ici.`
    : consigne();
  $("porteLabel").textContent = fin ? "Code d'accès" : "Mot de passe";
  // La durée de la démonstration ne se dit que s'il y en a une (`limite_s`, _outils/publier.py). Sans
  // elle, la note garde sa place, vide : le formulaire ne descend pas, et la liste des rôles s'ouvre
  // toujours sous son champ à la taille de l'auteur (1271 × 637).
  const sansDuree = SANS_LIMITE || APERCU;
  $("porteNote").hidden = fin;
  $("porteNote").classList.toggle("porte-note-vide", sansDuree);
  $("porteNoteT").textContent = sansDuree ? "" : `Accès de démonstration : ${duree(LIMITE)} d'utilisation.`;
  preremplir(!fin);
  dire(erreur || "");
  $("porte").hidden = false;
  // Le mot de passe déjà saisi, rien n'attend d'être tapé : le focus ne va qu'à un champ vide.
  if(!MDP_DEMO || fin) setTimeout(()=> $("porteMdp").focus(), 50);
}
function dire(t, info){ const e = $("porteErreur"); e.textContent = t; e.hidden = !t; e.classList.toggle("info", !!info); }
function occupe(oui){
  $("porteOk").disabled = oui; $("porteOk").classList.toggle("occupe", oui);
  $("porteMdp").readOnly = oui;
}

async function entrer(e){
  e.preventDefault();
  const champ = $("porteMdp"), mdp = champ.value, qui = ($("porteQui") || {}).value || "";
  garde(()=> sessionStorage.setItem(CLE_QUI, qui));
  if(!mdp){ champ.focus(); return; }
  const role = window.PorteRole ? PorteRole.cle() : "";
  if(APERCU){
    if(role) PorteRole.entrer();
    location.href = "../index.html" + (role ? "?role=" + encodeURIComponent(role) : "") + "#/accueil";
    return;
  }
  dire(""); occupe(true);
  const r = await essayer(mdp).catch(()=> null);
  if(!r){
    occupe(false); champ.select();
    const f = $("porteForm"); f.classList.remove("secoue"); void f.offsetWidth; f.classList.add("secoue");
    return dire(document.body.classList.contains("porte-fin") ? "Ce code n'ouvre pas l'espace." : "Ce mot de passe n'ouvre pas l'espace.");
  }
  if(SANS_LIMITE && r.niveau === "demo") r.niveau = "complet";
  if(r.niveau === "demo" && reste() <= 0){
    occupe(false);
    return montrer("fin", "Ce mot de passe ouvre la démonstration, dont le temps est écoulé sur ce navigateur.");
  }
  poserSession(r.cle, r.niveau, $("porteRester").checked, qui);
  // L'espace lit son rôle dans l'adresse en démarrant : le rôle choisi y est posé avant.
  if(role) PorteRole.entrer();
  // La présentation lancée d'ici a posé ses scripts dans la page, et l'espace porte les mêmes : il
  // les déclarerait une seconde fois. Il s'ouvre donc dans une page neuve, où la session tout juste
  // posée le reprend (demarrer) ; un navigateur qui ne garde rien l'ouvre ici, comme avant.
  if(chargement && lireSession()){ location.reload(); return; }
  try { await ouvrirEspace(await dechiffrer(r.cle), r.niveau); }
  catch(err){ occupe(false); dire("L'espace n'a pas pu s'ouvrir. Rechargez la page."); }
}

/* ---- La présentation, sans mot de passe ----
   Chargée au premier clic, puis lancée aussitôt : le plein écran demande que ce soit dans la
   foulée du geste. Rien ici ne lève d'erreur : ce qui manque se dit sous le bouton. */
const PRESENTATION = "presentation/presentation_publique.js", INDISPONIBLE = "La présentation n'est pas disponible pour le moment.";
let chargement = null;
function annoncer(t){ $("porteLancerInfo").textContent = t || ""; }
function chargerPresentation(){
  if(!chargement) chargement = new Promise((ok, ko)=>{
    const s = document.createElement("script"); s.src = PRESENTATION;
    s.onload = ok; s.onerror = ()=>{ s.remove(); chargement = null; ko(); };
    document.head.appendChild(s);
  });
  return chargement;
}
function montrerPresentation(){
  const f = window.lancerPresentationPublique;
  if(typeof f !== "function") return annoncer(INDISPONIBLE);
  try { Promise.resolve(f()).catch(()=> annoncer(INDISPONIBLE)); }
  catch(e){ annoncer(INDISPONIBLE); }
}
function lancer(){
  const b = $("porteLancer");
  if(b.classList.contains("occupe")) return;
  annoncer("");
  if(APERCU){ location.href = "../index.html#/presentation"; return; }
  if(typeof window.lancerPresentationPublique === "function") return montrerPresentation();
  b.classList.add("occupe"); b.setAttribute("aria-busy", "true");
  chargerPresentation().then(montrerPresentation, ()=> annoncer(INDISPONIBLE))
    .finally(()=>{ b.classList.remove("occupe"); b.removeAttribute("aria-busy"); });
}

function brancher(){
  $("porteForm").addEventListener("submit", entrer);
  $("porteOubli").addEventListener("click", ()=> dire("Le mot de passe vous a été transmis avec ce lien : redemandez-le à la personne qui vous l'a envoyé.", true));
  $("porteVoir").addEventListener("click", ()=>{
    const c = $("porteMdp"), voir = c.type === "password";
    c.type = voir ? "text" : "password";
    $("porteVoir").setAttribute("aria-label", voir ? "Masquer le mot de passe" : "Afficher le mot de passe");
    $("porteVoir").setAttribute("aria-pressed", String(voir));
    $("porteVoir").classList.toggle("on", voir); c.focus();
  });
  const maj = (e)=>{ if(e.getModifierState) $("porteMaj").hidden = !e.getModifierState("CapsLock"); };
  $("porteMdp").addEventListener("keydown", maj); $("porteMdp").addEventListener("keyup", maj);
  $("porteMdp").addEventListener("input", majSaisi);
  // « Mot de passe oublié ? » n'a pas lieu d'être quand il est déjà saisi.
  $("porteOubli").hidden = !!MDP_DEMO;
}

async function demarrer(){
  $("porteLancer").addEventListener("click", lancer);
  if(APERCU){ brancher(); return montrer(location.hash === "#fin" ? "fin" : "entree"); }
  if(!window.crypto || !crypto.subtle || !window.DecompressionStream){
    $("porte").hidden = false;
    $("porteForm").innerHTML = `<h1>Navigateur trop ancien</h1><p class="porte-texte">L'espace demande un navigateur récent : Chrome, Edge, Firefox ou Safari à jour.</p>`;
    return;
  }
  brancher();
  const s = lireSession();
  if(s){
    try {
      const contenu = await dechiffrer(b64(s.k));
      if(SANS_LIMITE && s.n === "demo") s.n = "complet";
      if(s.n === "demo" && reste() <= 0){ oublierSession(); return montrer("fin"); }
      reprendre(s);
      return await ouvrirEspace(contenu, s.n);
    } catch(e){ oublierSession(); }
  }
  montrer(reste() <= 0 ? "fin" : "entree");
}
demarrer();
})();
