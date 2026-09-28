/* L'Île aux Mots : l'interface suit la langue apprise par l'enfant sélectionné (table UI de textes.js).
   Dans la page : data-t (texte), data-t-aria (aria-label), data-t-ph (placeholder), data-t-arg (paramètre). */
function tx(key, ...args){
  const e = UI[key]; if (!e) return key;
  const v = e[langOf()] !== undefined ? e[langOf()] : e.en;
  return typeof v === "function" ? v(...args) : v;
}
const LOCALES = {en:"en-GB", de:"de-DE", lb:"lb-LU", zh:"zh-CN"};
const fmtDate = iso => new Date(iso).toLocaleDateString(LOCALES[langOf()] || "en-GB");
// default names follow the language; a name typed by the parent stays as it is
function kidName(k){
  const n = S.names[k];
  return !n || n === KID_DEFAULT[k].name || n === "Le petit" ? tx(k === "p7" ? "bigKid" : "littleKid") : n;
}
const pickLang = (o, i) => { const v = o[langOf()] || o.en; return i === undefined ? v : v[i]; };
function actTitle(a){ return a.title ? pickLang(a.title) : GAME_TITLES[a.id] ? pickLang(GAME_TITLES[a.id], 0) : a.name; }
function actSub(a){ return a.sub ? pickLang(a.sub) : GAME_TITLES[a.id] ? pickLang(GAME_TITLES[a.id], 1) : a.desc; }
function themeLabel(key){ return THEME_LABELS[key] ? pickLang(THEME_LABELS[key]) : THEMES[key].label; }
function fbTagLabel(tag){ const i = FB_TAGS.indexOf(tag); return i < 0 ? tag : tx("fbTags")[i]; }

function applyUI(){
  const arg = n => n.dataset.tArg !== undefined ? [n.dataset.tArg] : [];
  document.documentElement.lang = LOCALES[langOf()] || "en";
  document.title = tx("appTitle");
  document.querySelectorAll("[data-t]").forEach(n => { n.textContent = tx(n.dataset.t, ...arg(n)); });
  document.querySelectorAll("[data-t-aria]").forEach(n => n.setAttribute("aria-label", tx(n.dataset.tAria, ...arg(n))));
  document.querySelectorAll("[data-t-ph]").forEach(n => n.setAttribute("placeholder", tx(n.dataset.tPh)));
  showLangTag();
  if (typeof renderFlags === "function") renderFlags();
  if (typeof renderSpeed === "function") renderSpeed();
}
