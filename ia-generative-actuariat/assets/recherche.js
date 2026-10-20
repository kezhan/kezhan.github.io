/* La recherche d'un site sitekit (sitekit/recherche.py) : un bouton dans la barre, un panneau, des résultats
   par section. L'index (assets/recherche_index.js) n'est chargé qu'à la première ouverture. Raccourcis : « / »
   ou Ctrl+K ouvre, Échap ferme, les flèches choisissent, Entrée ouvre le résultat choisi. */
(function(){
  "use strict";
  const moi = document.currentScript, base = moi.src.replace(/recherche\.js(\?.*)?$/, ""), racine = base.replace(/assets\/$/, "");
  const nav = document.getElementById("site-nav");
  if(!nav) return;
  const css = document.createElement("link"); css.rel = "stylesheet"; css.href = base + "recherche.css"; document.head.appendChild(css);

  const loupe = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><circle cx="11" cy="11" r="6"/><path d="M20 20l-4.5-4.5"/></svg>';
  const bouton = document.createElement("button");
  bouton.type = "button"; bouton.className = "recherche-btn"; bouton.title = "Rechercher dans le support (/)";
  bouton.setAttribute("aria-label", "Rechercher dans le support");
  bouton.innerHTML = loupe + "<span>Rechercher</span><kbd>/</kbd>";
  nav.appendChild(bouton);   // au bout de la barre : le menu garde sa place

  const panneau = document.createElement("div");
  panneau.className = "recherche"; panneau.hidden = true;
  panneau.innerHTML = '<div class="recherche-boite" role="dialog" aria-label="Rechercher dans le support">' +
    '<div class="recherche-champ">' + loupe + '<input type="search" placeholder="Rechercher : harnais, Vasicek, CSM, injection..." autocomplete="off" spellcheck="false"><button type="button" class="recherche-fermer" aria-label="Fermer">Échap</button></div>' +
    '<div class="recherche-res" role="listbox"></div>' +
    '<p class="recherche-aide">Les flèches pour choisir, Entrée pour ouvrir. Tous les mots sont cherchés, sans tenir compte des accents.</p></div>';
  document.body.appendChild(panneau);
  const champ = panneau.querySelector("input"), res = panneau.querySelector(".recherche-res");

  // ---- l'index, chargé une fois ; chaque texte normalisé caractère par caractère (les positions se gardent)
  let index = null, chargement = null;
  const plat = (t) => Array.from(String(t), (c) => c.normalize("NFD").charAt(0).toLowerCase()).join("");
  function charger(){
    if(index) return Promise.resolve(index);
    if(chargement) return chargement;
    chargement = new Promise((ok, ko) => {
      const s = document.createElement("script"); s.src = base + "recherche_index.js";
      s.onload = () => { index = (window.RECHERCHE_INDEX || []).map(([u, p, t, x]) => ({ u, p, t, x, np: plat(p), nt: plat(t), nx: plat(x) })); ok(index); };
      s.onerror = () => ko(new Error("index absent")); document.head.appendChild(s);
    });
    return chargement;
  }

  const esc = (t) => String(t).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  function marquer(texte, mots){   // les mots trouvés, entourés de <mark>, sans tenir compte des accents
    const n = plat(texte), zones = [];
    mots.forEach((m) => { let i = n.indexOf(m); while(i >= 0){ zones.push([i, i + m.length]); i = n.indexOf(m, i + m.length); } });
    zones.sort((a, b) => a[0] - b[0]);
    let out = "", pos = 0;
    zones.forEach(([a, b]) => { if(a < pos) return; out += esc(texte.slice(pos, a)) + "<mark>" + esc(texte.slice(a, b)) + "</mark>"; pos = b; });
    return out + esc(texte.slice(pos));
  }
  function extrait(e, mots){
    const i = Math.min(...mots.map((m) => { const k = e.nx.indexOf(m); return k < 0 ? Infinity : k; }));
    if(!isFinite(i)) return e.x.slice(0, 180) + (e.x.length > 180 ? "…" : "");
    const a = Math.max(0, i - 70), b = Math.min(e.x.length, i + 150);
    return (a > 0 ? "…" : "") + e.x.slice(a, b) + (b < e.x.length ? "…" : "");
  }

  let choix = 0, trouves = [];
  function chercher(){
    const mots = plat(champ.value).split(/[^\p{L}\p{N}]+/u).filter((m) => m.length >= 2);
    if(!mots.length){ res.innerHTML = ""; trouves = []; return; }
    trouves = index.map((e) => {
      const tout = e.np + " " + e.nt + " " + e.nx;
      if(!mots.every((m) => tout.includes(m))) return null;
      let score = 0;
      mots.forEach((m) => { if(e.nt.includes(m)) score += 10; if(e.np.includes(m)) score += 6; score += Math.min(5, e.nx.split(m).length - 1); });
      return { e, score };
    }).filter(Boolean).sort((a, b) => b.score - a.score).slice(0, 40);
    choix = 0;
    res.innerHTML = trouves.length ? trouves.map(({ e }, i) =>
      `<a class="recherche-item${i === choix ? " choisi" : ""}" href="${esc(racine + e.u)}" role="option">` +
      `<span class="ou">${marquer(e.p, mots)}${e.t !== e.p ? " › " + marquer(e.t, mots) : ""}</span>` +
      `<span class="ex">${marquer(extrait(e, mots), mots)}</span></a>`).join("")
      : '<p class="recherche-vide">Aucune section ne contient tous ces mots.</p>';
  }
  function surligner(){
    res.querySelectorAll(".recherche-item").forEach((a, i) => a.classList.toggle("choisi", i === choix));
    const a = res.querySelectorAll(".recherche-item")[choix]; if(a) a.scrollIntoView({ block: "nearest" });
  }

  function ouvrir(){
    panneau.hidden = false; document.documentElement.classList.add("recherche-ouverte");
    champ.focus(); champ.select();
    res.innerHTML = index ? res.innerHTML : '<p class="recherche-vide">Chargement de l\'index…</p>';
    charger().then(() => chercher(), () => { res.innerHTML = '<p class="recherche-vide">La recherche n\'est pas disponible sur cette page.</p>'; });
  }
  function fermer(){ panneau.hidden = true; document.documentElement.classList.remove("recherche-ouverte"); }

  bouton.addEventListener("click", ouvrir);
  panneau.querySelector(".recherche-fermer").addEventListener("click", fermer);
  panneau.addEventListener("click", (ev) => { if(ev.target === panneau) fermer(); });
  res.addEventListener("click", (ev) => { if(ev.target.closest(".recherche-item")) fermer(); });
  champ.addEventListener("input", () => { if(index) chercher(); });
  champ.addEventListener("keydown", (ev) => {
    const n = trouves.length;
    if(ev.key === "ArrowDown" && n){ ev.preventDefault(); choix = (choix + 1) % n; surligner(); }
    else if(ev.key === "ArrowUp" && n){ ev.preventDefault(); choix = (choix - 1 + n) % n; surligner(); }
    else if(ev.key === "Enter" && n){ ev.preventDefault(); const a = res.querySelectorAll(".recherche-item")[choix]; if(a){ fermer(); location.href = a.href; } }
  });
  document.addEventListener("keydown", (ev) => {
    const ecrit = /^(INPUT|TEXTAREA|SELECT)$/.test((ev.target.tagName || "")) || ev.target.isContentEditable;
    if(ev.key === "Escape" && !panneau.hidden){ fermer(); return; }
    if(!ecrit && (ev.key === "/" || ((ev.ctrlKey || ev.metaKey) && ev.key.toLowerCase() === "k"))){ ev.preventDefault(); ouvrir(); }
  });
})();
