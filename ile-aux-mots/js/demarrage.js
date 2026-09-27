/* L'Île aux Mots : lancement. */
/* ---------- boot ---------- */
function start(data){
  loadLocal();
  if (data && data.kid) S.kid = data.kid;
  renderHome(); show("home"); connect(); flushOutbox();
  // ask the browser not to evict stars and stickers when space runs low
  try { if (navigator.storage && navigator.storage.persist) navigator.storage.persist().then(ok => { S.persisted = ok; }, () => {}); } catch(e) {}
  // installable app and offline play, on the https site only (the home PC server stays uncached)
  if ("serviceWorker" in navigator && location.protocol === "https:") navigator.serviceWorker.register("sw.js").catch(() => {});
}
try { if (window.claude && window.claude.hot && typeof window.claude.hot.snapshot === "function") window.claude.hot.snapshot(() => ({kid:S.kid})); } catch(e) {}
if (window.claude && window.claude.hot && window.claude.hot.ready) window.claude.hot.ready(start);
else start((window.claude && window.claude.hot && window.claude.hot.data) || {});
