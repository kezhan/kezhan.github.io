/* L'Île aux Mots : lancement. */
/* ---------- boot ---------- */
function start(data){
  loadLocal();
  if (data && data.kid) S.kid = data.kid;
  renderHome(); show("home"); connect();
}
try { if (window.claude && window.claude.hot && typeof window.claude.hot.snapshot === "function") window.claude.hot.snapshot(() => ({kid:S.kid})); } catch(e) {}
if (window.claude && window.claude.hot && window.claude.hot.ready) window.claude.hot.ready(start);
else start((window.claude && window.claude.hot && window.claude.hot.data) || {});
