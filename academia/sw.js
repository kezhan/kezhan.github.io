/* Académia : service worker, pour jouer sans réseau et sans attendre un Wi-Fi qui rame.
   1. Une version publiée a son cache. À l'installation, il garde la coquille du jeu (page, scripts, styles, données),
      d'après precache.json écrit par publier.py ; puis, dès que le jeu montre ses images, il range en tâche de fond
      toutes celles de la même qualité et les voix luxembourgeoises. Une image qui n'a pas changé d'une version à
      l'autre (même empreinte) est recopiée du cache précédent, sans téléchargement.
   2. Ce qui porte une version ou ne change qu'avec elle (scripts ?v=, images, Phaser, icônes) vient du cache d'abord.
      La page et les données passent par le réseau, mais pas plus de trois secondes : sinon la copie gardée.
   VERSION_SW est réécrite par publier.py à chaque publication : le navigateur voit un service worker neuf et
   installe la nouvelle version. En local (VERSION_SW = "dev"), rien n'est préchargé. */
const VERSION_SW = "0.8-0cbc4d98";
const CACHE = "academia-" + VERSION_SW, EMPREINTES = "precache-empreintes.json";
const ATTENTE_RESEAU = 3000, PRECHARGE_APRES = 15000;

async function liste(){
  try { const r = await fetch("precache.json", {cache: "no-cache"}); return r.ok ? await r.json() : null; } catch (e) { return null; }
}

self.addEventListener("install", ev => ev.waitUntil((async () => {
  const l = await liste();
  if (l) {
    const cache = await caches.open(CACHE);
    await cache.addAll(l.coquille.map(u => new Request(u, {cache: "no-cache"})));
    await reprendreInchanges(cache, l.fichiers || {});
    await cache.put(EMPREINTES, new Response(JSON.stringify(l.fichiers || {}), {headers: {"Content-Type": "application/json"}}));
  }
  await self.skipWaiting();
})()));

// the pictures of the previous version that did not change: copied, not downloaded again
async function reprendreInchanges(cache, fichiers){
  for (const nom of await caches.keys()) {
    if (nom === CACHE || !nom.startsWith("academia")) continue;
    const ancien = await caches.open(nom), e = await ancien.match(EMPREINTES);
    const avant = e ? await e.json().catch(() => ({})) : {};
    for (const [chemin, h] of Object.entries(fichiers)) {
      if (avant[chemin] !== h || await cache.match(chemin)) continue;
      const r = await ancien.match(chemin);
      if (r) await cache.put(chemin, r);
    }
  }
}

self.addEventListener("activate", ev => ev.waitUntil((async () => {
  for (const nom of await caches.keys()) if (nom !== CACHE && nom.startsWith("academia")) await caches.delete(nom);
  await self.clients.claim();
})()));

// all the pictures of a quality and the Luxembourgish voices, in the background, a few at a time
const enCours = new Set();
function precharger(qualite){
  if (VERSION_SW === "dev" || enCours.has(qualite)) return Promise.resolve();
  enCours.add(qualite);
  return (async () => {
    await new Promise(ok => setTimeout(ok, PRECHARGE_APRES));   // the game first: its own pictures come before the stock
    const l = await liste(); if (!l) return;
    const cache = await caches.open(CACHE);
    const a = Object.keys(l.fichiers || {}).filter(c => c.startsWith(`assets/${qualite}/`) || c.startsWith("assets/audio/"));
    const prendre = async () => {
      for (let c = a.shift(); c; c = a.shift()) {
        if (await cache.match(c)) continue;
        try { const r = await fetch(c); if (r.ok) await cache.put(c, r); } catch (e) {}
      }
    };
    await Promise.all([prendre(), prendre(), prendre()]);
  })();
}

// the network, but not longer than `ms`: else the copy kept (and the network answer, when it comes, is kept)
async function reseauPuisCache(ev, req, ms, cle){
  const cache = await caches.open(CACHE), k = cle || req;
  const reseau = fetch(req);
  // kept as soon as it comes, even after the copy was served (the event lives until then)
  ev.waitUntil(reseau.then(r => r.ok ? cache.put(k, r.clone()) : null).catch(() => {}));
  const copie = () => cache.match(k).then(r => r || cache.match(k, {ignoreSearch: true}));
  const delai = new Promise(ok => setTimeout(ok, ms, null));
  const premier = await Promise.race([reseau.catch(() => null), delai]);
  if (premier && (premier.ok || !(await copie()))) return premier;
  const r = await copie();
  if (r) return r;
  return reseau;   // nothing kept yet: wait for the network after all
}
// the copy kept first; else the network (and the answer is kept)
async function cacheDabord(ev, req){
  const cache = await caches.open(CACHE);
  const r = await cache.match(req);
  if (r) return r;
  const n = await fetch(req);
  if (n.ok || n.type === "opaque") ev.waitUntil(cache.put(req, n.clone()));
  return n;
}

self.addEventListener("fetch", ev => {
  const req = ev.request;
  if (req.method !== "GET") return;
  const url = new URL(req.url), ici = url.origin === location.origin;
  const police = url.hostname === "fonts.googleapis.com" || url.hostname === "fonts.gstatic.com";
  if (!ici && !police) return;
  if (police) return ev.respondWith(cacheDabord(ev, req));
  const chemin = url.pathname.slice(new URL(self.registration.scope).pathname.length);
  if (req.mode === "navigate") return ev.respondWith(reseauPuisCache(ev, new Request(req.url, {cache: "no-cache", credentials: "same-origin"}), ATTENTE_RESEAU, "./"));
  const q = /^assets\/(hd|leger)\//.exec(chemin);
  if (q) ev.waitUntil(precharger(q[1]));
  // a picture asked again after a failure (?essai=): the network, else any copy kept
  if (url.searchParams.has("essai")) return ev.respondWith(reseauPuisCache(ev, req, 8000, chemin));
  if (chemin.startsWith("donnees/") || chemin === "precache.json") return ev.respondWith(reseauPuisCache(ev, req, ATTENTE_RESEAU));
  if (url.searchParams.has("v") || /^(assets|vendor|icones)\//.test(chemin) || chemin === "manifest.webmanifest")
    return ev.respondWith(cacheDabord(ev, req));
  ev.respondWith(reseauPuisCache(ev, req, ATTENTE_RESEAU));
});
