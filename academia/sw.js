/* Académia : service worker. Réseau d'abord (les mises à jour arrivent tout de suite), copie en cache pour jouer hors ligne. */
const CACHE = "academia";
self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", ev => ev.waitUntil(self.clients.claim()));
// one copy per file: when js/x.js?v=<new> is stored, the copies of older versions (js/x.js?v=<old>) go
async function ranger(cache, req, res){
  await cache.put(req, res);
  const url = new URL(req.url);
  if (url.origin !== location.origin || !url.search) return;
  for (const k of await cache.keys(req, {ignoreSearch: true})) if (k.url !== req.url) await cache.delete(k);
}
self.addEventListener("fetch", ev => {
  const req = ev.request;
  if (req.method !== "GET") return;
  const url = new URL(req.url);
  const font = url.hostname === "fonts.googleapis.com" || url.hostname === "fonts.gstatic.com";
  if (url.origin !== location.origin && !font) return;
  ev.respondWith((async () => {
    const cache = await caches.open(CACHE);
    try {
      // the page is always revalidated (GitHub Pages sends max-age=600): a new version shows at once
      const res = await fetch(req.mode === "navigate" ? new Request(req.url, {cache: "no-cache", credentials: "same-origin"}) : req);
      if (res.ok || res.type === "opaque") ev.waitUntil(ranger(cache, req, res.clone()));
      return res;
    } catch (e) {
      // offline: the exact copy first (same version as the page), else the latest one kept
      const hit = await cache.match(req) || await cache.match(req, {ignoreSearch: true})
        || (req.mode === "navigate" && await cache.match("./", {ignoreSearch: true}));
      if (hit) return hit;
      throw e;
    }
  })());
});
