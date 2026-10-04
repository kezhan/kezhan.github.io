/* Académia : service worker. Réseau d'abord (les mises à jour arrivent tout de suite), copie en cache pour jouer hors ligne. */
const CACHE = "academia";
self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", ev => ev.waitUntil(self.clients.claim()));
self.addEventListener("fetch", ev => {
  const req = ev.request;
  if (req.method !== "GET") return;
  const url = new URL(req.url);
  const font = url.hostname === "fonts.googleapis.com" || url.hostname === "fonts.gstatic.com";
  if (url.origin !== location.origin && !font) return;
  ev.respondWith((async () => {
    const cache = await caches.open(CACHE);
    try {
      const res = await fetch(req);
      if (res.ok || res.type === "opaque") cache.put(req, res.clone());
      return res;
    } catch (e) {
      const hit = await cache.match(req, {ignoreSearch: true}) || (req.mode === "navigate" && await cache.match("./", {ignoreSearch: true}));
      if (hit) return hit;
      throw e;
    }
  })());
});
