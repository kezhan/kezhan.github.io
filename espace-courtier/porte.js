/* La porte de l'espace courtier publié : un mot de passe, et un contenu chiffré.

   L'auteur, le 03-10 : « mettre un mot de passe login, même si c'est html ... peut-être que tu
   fais un hash, sans stockage des mots en clair ? ». Mieux qu'une empreinte comparée : l'espace
   publié est chiffré (AES-256-GCM) par une clé de contenu, elle-même chiffrée par la clé que
   donne chaque mot de passe (PBKDF2-SHA-256, 600 000 tours). La page ne contient ni les mots de
   passe ni leurs empreintes : lire la source ne donne que du chiffré, et chaque essai coûte une
   fraction de seconde de calcul.

   Deux mots de passe ouvrent le même contenu : celui de la démonstration, limitée à cinq heures
   d'utilisation par navigateur, et celui d'un accès complet. L'auteur : « limiter la démo à 5h ?
   et dire que si les gens veulent plus, faut demander un accès ». Le temps ne court que pendant
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
   qu'au premier clic ; si elle manque, la porte le dit en une ligne.

   La sortie : window.porteSortir() oublie la session et revient à la porte. La pastille s'en
   sert, le menu du compte de l'espace aussi quand elle existe.

   Ouverte du disque (porte/porte.html, sans contenu chiffré), la porte est un aperçu : elle
   s'affiche comme en ligne ; tout mot de passe mène à ../index.html#/accueil, et la
   présentation à ../index.html#/presentation (porte.html#fin montre la fin de la
   démonstration). */
(function(){
"use strict";
const C = window.CONTENU_CHIFFRE, APERCU = !C;
const CLE_SESSION = "ec-porte:session", CLE_TEMPS = "ec-porte:temps", COOKIE = "ec_porte_temps";
const LIMITE = ((C && C.limite_s) || 5 * 3600) * 1000;
const INACTIF = 5 * 60 * 1000, PAS = 15000;
const $ = (id)=> document.getElementById(id);
const garde = (f, sinon)=>{ try { return f(); } catch(e){ return sinon; } };
const b64 = (s)=> Uint8Array.from(atob(s), (c)=> c.charCodeAt(0));
const en64 = (u)=> btoa(String.fromCharCode.apply(null, Array.from(u)));

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
const reste = ()=> Math.max(0, LIMITE - lireTemps());
function duree(ms){
  const min = Math.ceil(ms / 60000), h = Math.floor(min / 60), m = min % 60;
  return h ? `${h} h${m ? " " + String(m).padStart(2, "0") : ""}` : `${m} min`;
}

/* ---- La session : la clé du contenu, jamais le mot de passe ---- */
const magasins = ()=> [garde(()=> sessionStorage, null), garde(()=> localStorage, null)].filter(Boolean);
function lireSession(){
  for(const m of magasins()){ const v = garde(()=> JSON.parse(m.getItem(CLE_SESSION)), null); if(v && v.k) return v; }
  return null;
}
function poserSession(cle, niveau, longtemps){
  const v = JSON.stringify({ k:en64(cle), n:niveau });
  garde(()=> sessionStorage.setItem(CLE_SESSION, v));
  if(longtemps) garde(()=> localStorage.setItem(CLE_SESSION, v));
}
function oublierSession(){ magasins().forEach((m)=> garde(()=> m.removeItem(CLE_SESSION))); }
function sortir(){ oublierSession(); location.reload(); }
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
const CLE_QUI = "ec-porte:qui";
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
function montrer(etat, erreur){
  const fin = etat === "fin";
  document.body.classList.toggle("porte-fin", fin);
  $("porteTitre").textContent = fin ? "Votre démonstration est terminée" : "Connexion";
  $("porteTexte").innerHTML = fin
    ? "Les cinq heures de démonstration ont été utilisées sur ce navigateur. Pour continuer, <b>demandez un accès</b> à la personne qui vous a transmis ce lien, puis entrez-le ici."
    : "Entrez le mot de passe qui vous a été transmis.";
  $("porteLabel").textContent = fin ? "Code d'accès" : "Mot de passe";
  $("porteNote").hidden = fin;
  dire(erreur || "");
  $("porte").hidden = false;
  setTimeout(()=> $("porteMdp").focus(), 50);
}
function dire(t, info){ const e = $("porteErreur"); e.textContent = t; e.hidden = !t; e.classList.toggle("info", !!info); }
function occupe(oui){
  $("porteOk").disabled = oui; $("porteOk").classList.toggle("occupe", oui);
  $("porteMdp").readOnly = oui;
}

async function entrer(e){
  e.preventDefault();
  const champ = $("porteMdp"), mdp = champ.value;
  garde(()=> sessionStorage.setItem(CLE_QUI, ($("porteQui") || {}).value || ""));
  if(!mdp){ champ.focus(); return; }
  if(APERCU){ location.href = "../index.html#/accueil"; return; }
  dire(""); occupe(true);
  const r = await essayer(mdp).catch(()=> null);
  if(!r){
    occupe(false); champ.select();
    const f = $("porteForm"); f.classList.remove("secoue"); void f.offsetWidth; f.classList.add("secoue");
    return dire(document.body.classList.contains("porte-fin") ? "Ce code n'ouvre pas l'espace." : "Ce mot de passe n'ouvre pas l'espace.");
  }
  if(r.niveau === "demo" && reste() <= 0){
    occupe(false);
    return montrer("fin", "Ce mot de passe ouvre la démonstration, dont le temps est écoulé sur ce navigateur.");
  }
  poserSession(r.cle, r.niveau, $("porteRester").checked);
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
      if(s.n === "demo" && reste() <= 0){ oublierSession(); return montrer("fin"); }
      return await ouvrirEspace(contenu, s.n);
    } catch(e){ oublierSession(); }
  }
  montrer(reste() <= 0 ? "fin" : "entree");
}
demarrer();
})();
