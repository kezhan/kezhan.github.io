/* L'illustration de la porte : la carte du monde en points, les sites assurés et l'alerte, les
   fils entre le contrat et ce que l'IA y a lu, et l'échelle de la scène (porte_scene.css).

   La carte : les terres de Natural Earth au 1:110 000 000 (domaine public), sans l'Antarctique,
   sur une grille en quinconce de 2,4 degrés, du 78e parallèle nord au 56e sud. Une chaîne par
   rang de la grille, un bit par point, en base64 : la carte entière tient en deux kilo-octets.
   Les sites sont des villes industrielles, sans nom à l'écran : la porte est publique, rien n'y
   vient des données de l'espace. */
(function(){
  "use strict";
  const boite = document.getElementById("porteScene");
  if(!boite) return;
  const scene = boite.querySelector(".psc-scene"), NS = "http://www.w3.org/2000/svg";
  const W = 860, H = 440;
  const TERRES = { pas:2.4, cols:150, nord:78, rangs:[
    "AAAAaf/////gAPwAAAAOAAAAAA==","AAABfu+D///gAAAAPAH/wB+AAA==","AAAB+/vgD//gAAAAYI///4QAAA==",
    "gfgBfzf8D//AAA8A4//////4BA==","h////z+fB/8AAH/2/////////A==","//////4fj/D4Af///////////A==",
    "G//////PB8BwA////////////A==","B/////A8A4AAD9//////////4A==","AeB///A/gAAAR8f///////82AA==",
    "AaAf//w/gAAAw5////////AeAA==","AgAf//+/8AAB4/////////4MAA==","AAAH////8AAC//////////wIAA==",
    "AAAD////uAAAP/////////4AAA==","AAAD////2AAAf/////////4AAA==","AAAB////gAAAP35///////MAAA==",
    "AAAB///4AAAD87yP/////4QAAA==","AAAB///wAAAB4lv//////wQAAA==","AAAB///gAAAB3pv//////ZgAAA==",
    "AAAA///gAAAA/wS/////+bwAAA==","AAAAP//AAAAD/5h/////+MAAAA==","AAAAH//AAAAB/////////AAAAA==",
    "AAAAP8CAAAAH///+////+AAAAA==","AAAAC+BgAAAP////v////AAAAA==","AAAAC8HAAAAf////8f//6AAAAA==",
    "AAAAAeY8AAAP////4H5/AAAAAA==","AAAAAfw+AAAP///vwHw+BAAAAA==","AAAAAC8AAAAP///3gHg/hAAAAA==",
    "AAAAAAcAAAAf///8AHAfDAAAAA==","AAAAAAEfwAAH////ADgLAwAAAA==","AAAAAAH/wAAH////ACgSBgAAAA==",
    "AAAAAAA/+AAD////AAgEGQAAAA==","AAAAAAA//AAAA//8AAAccAAAAA==","AAAAAAB//AAAAf/8AAAO/oAAAA==",
    "AAAAAAB//wAAA//wAAAM/GAAAA==","AAAAAAB//+AAAf/wAAAGff4gAA==","AAAAAAD///AAAf/gAAADDA/QAA==",
    "AAAAAAA///gAAP/wAAAA7Q6EAA==","AAAAAAA///AAAP/wAAAAAEAAAA==","AAAAAAAf/+AAAP/xAAAAAHIAAA==",
    "AAAAAAAf/8AAAf/3AAAAA/YABA==","AAAAAAAH/+AAAP/nAAAAA/8ABA==","AAAAAAAH/8AAAP+GAAAAH/+BAA==",
    "AAAAAAAH/8AAAH/GAAAAP//AgA==","AAAAAAAH/gAAAP+EAAAAP//AAA==","AAAAAAAH/gAAAH+AAAAAP//gAA==",
    "AAAAAAAH/AAAAH8AAAAAP//gAA==","AAAAAAAH/AAAAD4AAAAAH7/gAA==","AAAAAAAP8AAAACAAAAAAMB/AIA==",
    "AAAAAAAP8AAAAAAAAAAAAAfAGA==","AAAAAAAPgAAAAAAAAAAAAAAAGA==","AAAAAAAPAAAAAAAAAAAAAAGAMA==",
    "AAAAAAAeAAAAAAAAAAAAAAAA4A==","AAAAAAAfAAAAAAAAAAAAAAAAQA==","AAAAAAAeAAAAAAAAAQAAAAAAAA==",
    "AAAAAAAOYAAAAAAAAAAAAAAAAA==","AAAAAAAOAAAAAAAAAAAAAAAAAA==","AAAAAAAAAAAAAAAAAAAAAAAAAA=="
  ] };
  // Greenwich au milieu de la scène, 2,5 unités par degré.
  const UNITE = 2.5, X0 = 430, Y0 = 22;
  const proj = (lat, lon)=> [X0 + lon * UNITE, Y0 + (TERRES.nord - lat) * UNITE];

  /* Les points pâlissent vers les bords de la scène, par paliers : un tracé par palier, plus
     léger qu'un masque à peindre. */
  const PALIERS = [.05, .1, .16, .22, .28, .34];
  const lisse = (a, b, v)=>{ const t = Math.min(1, Math.max(0, (v - a) / (b - a))); return t * t * (3 - 2 * t); };
  function points(){
    const d = PALIERS.map(()=> []);
    TERRES.rangs.forEach((r, j)=>{
      const o = atob(r), dec = j % 2 ? .5 : 0;
      for(let i = 0; i < TERRES.cols; i++){
        if(!(o.charCodeAt(i >> 3) >> (7 - (i & 7)) & 1)) continue;
        const [x, y] = proj(TERRES.nord - j * TERRES.pas, -180 + TERRES.pas * (i + .5 + dec));
        const f = (1 - lisse(.72, 1.02, Math.abs(x - W / 2) / (W / 2))) * lisse(-4, 46, y) * (1 - lisse(300, 372, y));
        const n = Math.round(f * PALIERS.length) - 1;
        if(n >= 0) d[n].push(`M${x.toFixed(1)} ${y.toFixed(1)}h0`);
      }
    });
    const g = document.getElementById("pscPoints");
    d.forEach((l, n)=>{ if(!l.length) return; const p = document.createElementNS(NS, "path");
      p.setAttribute("d", l.join("")); p.setAttribute("stroke-opacity", PALIERS[n]); g.append(p); });
  }

  /* Les sites : [latitude, longitude]. Le premier porte l'alerte (une tempête sur un entrepôt
     portuaire, celui que nomme la carte de l'alerte). */
  const SITES = [[51.9, 4.5], [53.6, 10], [45.8, 4.8], [43.3, -2.9], [45.5, 9.2], [41.9, -87.6], [45.5, -73.6], [29.8, -95.4],
    [19.4, -99.1], [-23.6, -46.6], [-33.5, -70.7], [33.6, -7.6], [25.2, 55.3], [19.1, 72.9], [31.2, 121.5], [1.4, 103.8],
    [34.7, 135.5], [-29.9, 31]];
  const rond = (x, y, r, cls)=>{ const c = document.createElementNS(NS, "circle");
    c.setAttribute("cx", x.toFixed(1)); c.setAttribute("cy", y.toFixed(1)); c.setAttribute("r", r); c.setAttribute("class", cls); return c; };
  function sites(){
    const g = document.getElementById("pscSites");
    SITES.forEach(([lat, lon], k)=>{
      const [x, y] = proj(lat, lon);
      if(k === 0){
        const trait = document.createElementNS(NS, "path");
        trait.setAttribute("d", `M${x.toFixed(1)} ${(y - 9).toFixed(1)}V47H486`); trait.setAttribute("class", "psc-trait");
        g.append(rond(x, y, 34, "psc-orage"), trait, rond(x, y, 7, "psc-onde"), rond(x, y, 7, "psc-onde"), rond(x, y, 5.2, "psc-site-alerte"));
      } else {
        const halo = rond(x, y, 7, "psc-halo psc-respire");
        halo.style.animationDelay = `${-(k * .7) % 3.6}s`;
        g.append(halo, rond(x, y, 3.4, "psc-site"));
      }
    });
  }

  /* Les fils du contrat : de la ligne lue au champ qu'elle a rempli. Les positions se lisent
     dans la scène, avant l'échelle. */
  function dans(el){
    let x = 0, y = 0;
    for(let e = el; e && e !== scene; e = e.offsetParent){ x += e.offsetLeft; y += e.offsetTop; }
    return { x, y, l:el.offsetWidth, h:el.offsetHeight };
  }
  function fils(){
    const svg = document.querySelector(".psc-liens");
    svg.querySelectorAll("circle").forEach((c)=> c.remove());
    [1, 2].forEach((k)=>{
      const a = dans(document.getElementById("pscLu" + k)), b = dans(document.getElementById("pscChamp" + k));
      const x1 = a.x + a.l + 4, y1 = a.y + a.h / 2, x2 = b.x - 14, y2 = b.y + b.h / 2, m = (x2 - x1) * .55;
      document.getElementById("pscLien" + k).setAttribute("d", `M${x1} ${y1}C${x1 + m} ${y1},${x2 - m} ${y2},${x2} ${y2}`);
      svg.append(rond(x1, y1, 2.4, "psc-bout"), rond(x2, y2, 2.4, "psc-bout"));
    });
  }

  /* L'échelle : les cartes dans la boîte, la carte du monde débordant autour, pâlie ; sur un
     téléphone, la boîte est un bandeau que la scène couvre, en gros plan sur l'alerte
     (--psc-couvre et --psc-zoom dans la feuille). */
  function cadre(){
    let x0 = W, y0 = H, x1 = 0, y1 = 0;
    scene.querySelectorAll(".psc-carte").forEach((c)=>{ const b = dans(c); if(!b.l) return;
      x0 = Math.min(x0, b.x); y0 = Math.min(y0, b.y); x1 = Math.max(x1, b.x + b.l); y1 = Math.max(y1, b.y + b.h); });
    return { x:x0 - 12, y:y0 - 14, l:x1 - x0 + 24, h:y1 - y0 + 30 };
  }
  let vu = false;
  function ajuster(){
    const r = boite.getBoundingClientRect();
    if(!r.width || !r.height) return;
    const style = getComputedStyle(boite), couvre = style.getPropertyValue("--psc-couvre").trim() === "1";
    const zoom = Number(style.getPropertyValue("--psc-zoom")) || 1;
    // Une marge autour des cartes : elles ne touchent pas les bords du panneau.
    const c = cadre(), mx = Math.min(24, r.width * .03), mb = Math.min(26, r.height * .05);
    const k = couvre ? Math.max(r.width / W, r.height / H) * zoom : Math.min((r.width - 2 * mx) / c.l, (r.height - mb) / c.h, 1.15);
    let x = (r.width - c.l * k) / 2 - c.x * k, y = (r.height - mb - c.h * k) / 2 - c.y * k;
    if(couvre){
      const [px, py] = proj(SITES[0][0], SITES[0][1]), fx = px + 70, fy = py + 6;
      x = Math.min(0, Math.max(r.width - W * k, r.width / 2 - fx * k));
      y = Math.min(0, Math.max(r.height - H * k, r.height / 2 - fy * k));
    }
    scene.style.transform = `translate(${x.toFixed(1)}px,${y.toFixed(1)}px) scale(${k.toFixed(4)})`;
    if(vu) return;
    vu = true; fils();
    // La police des titres se charge quand la porte paraît : elle change la taille des cartes et
    // la hauteur des lignes. Les fils et l'échelle se refont quand elle est là.
    if(document.fonts && document.fonts.ready) document.fonts.ready.then(()=>{ fils(); ajuster(); });
  }

  points();
  sites();
  if(window.ResizeObserver) new ResizeObserver(ajuster).observe(boite);
  else { addEventListener("resize", ajuster); setTimeout(ajuster, 0); }
})();
