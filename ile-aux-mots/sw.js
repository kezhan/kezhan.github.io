/* L'Île aux Mots : service worker. Réseau d'abord (les mises à jour arrivent tout de suite),
   copie en cache pour jouer hors ligne. Les envois vers le formulaire Google ne passent pas par ici. */
const CACHE = "ile-aux-mots";

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", ev => ev.waitUntil(self.clients.claim()));

self.addEventListener("fetch", ev => {
  const req = ev.request;
  if (req.method !== "GET") return;
  const url = new URL(req.url);
  const ownFile = url.origin === location.origin;
  const font = url.hostname === "fonts.googleapis.com" || url.hostname === "fonts.gstatic.com";
  if (!ownFile && !font) return;
  // recordings (audio/): the player asks for a byte range, whose partial answer cannot be cached, so the whole file is fetched
  if (ownFile && url.pathname.includes("/audio/")) return ev.respondWith(wholeFile(url.href));
  ev.respondWith((async () => {
    const cache = await caches.open(CACHE);
    try {
      const res = await fetch(req);
      if (res.ok || res.type === "opaque") cache.put(req, res.clone());
      return res;
    } catch (e) {
      // offline: same file without its ?v= version, or the home page
      const hit = await cache.match(req, {ignoreSearch: true}) || (req.mode === "navigate" && await cache.match("./", {ignoreSearch: true}));
      if (hit) return hit;
      throw e;
    }
  })());
});

async function wholeFile(href){
  const cache = await caches.open(CACHE);
  try {
    const res = await fetch(href);
    if (res.status === 200) cache.put(href, res.clone());
    return res;
  } catch (e) {
    const hit = await cache.match(href, {ignoreSearch: true});
    if (hit) return hit;
    throw e;
  }
}
