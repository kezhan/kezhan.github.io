/* L'Île aux Mots : les drapeaux des langues, en haut de chaque écran.
   Kezhan : « sans savoir lire, les enfants peuvent changer de langue en reconnaissant les drapeaux ».
   Dessinés en SVG : l'émoji drapeau s'affiche en lettres (GB, DE) sous Windows. */
const flagStar = (cx, cy, r, turn = 0) => {
  const p = [];
  for (let i = 0; i < 10; i++) {
    const a = turn + Math.PI / 2 + i * Math.PI / 5, d = i % 2 ? r * 0.38 : r;
    p.push((cx + d * Math.cos(a)).toFixed(2) + "," + (cy - d * Math.sin(a)).toFixed(2));
  }
  return `<polygon points="${p.join(" ")}" fill="#FFDE00"/>`;
};
const FLAG_SVG = {
  en: `<svg viewBox="0 0 60 30" preserveAspectRatio="none"><clipPath id="flagUk"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath>
    <rect width="60" height="30" fill="#012169"/><path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/>
    <path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#flagUk)" stroke="#C8102E" stroke-width="4"/>
    <path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/><path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></svg>`,
  de: `<svg viewBox="0 0 5 3" preserveAspectRatio="none"><rect width="5" height="1" fill="#000"/><rect y="1" width="5" height="1" fill="#DD0000"/><rect y="2" width="5" height="1" fill="#FFCE00"/></svg>`,
  lb: `<svg viewBox="0 0 5 3" preserveAspectRatio="none"><rect width="5" height="1" fill="#EF3340"/><rect y="1" width="5" height="1" fill="#fff"/><rect y="2" width="5" height="1" fill="#00A3E0"/></svg>`,
  zh: `<svg viewBox="0 0 30 20" preserveAspectRatio="none"><rect width="30" height="20" fill="#EE1C25"/>${flagStar(5, 5, 3)}${flagStar(10, 2, 1, 0.6)}${flagStar(12, 4, 1, 0.2)}${flagStar(12, 7, 1, -0.2)}${flagStar(10, 9, 1, -0.6)}</svg>`
};
addStyle(`.flags{display:flex; gap:8px; flex-wrap:wrap}
  .flag{width:62px; height:44px; padding:0; border:3px solid var(--ink); border-radius:10px; overflow:hidden; background:#fff;
    box-shadow:2px 3px 0 var(--ink); cursor:pointer; opacity:.8; transition:transform .15s, opacity .15s}
  .flag svg{width:100%; height:100%; display:block}
  .flag[aria-pressed="true"]{opacity:1; transform:scale(1.12); outline:4px solid var(--sun); outline-offset:1px}
  @media (max-width:420px){ .flag{width:56px; height:40px} }`);

function renderFlags(){
  const bar = $("flagBar"); if (!bar) return;
  bar.innerHTML = ""; bar.setAttribute("aria-label", tx("flagsAria"));
  Object.entries(LANGS).forEach(([code, L]) => {
    const b = el("button", "flag", FLAG_SVG[code]);
    b.setAttribute("aria-label", L.label); b.setAttribute("aria-pressed", String(langOf() === code));
    b.onclick = () => switchLang(code, b);
    bar.append(b);
  });
}
// one tap: the whole site changes language and voice, the current screen is drawn again, a game starts again in the new language
function switchLang(code, b){
  if (langOf() !== code) setLang(S.kid, code);
  sfx.tap();
  applyUI();
  const nb = [...$("flagBar").children].find(x => x.getAttribute("aria-label") === LANGS[code].label) || b;
  if (!fx.calm()) nb.animate([{transform:"rotate(0) scale(1)"}, {transform:"rotate(360deg) scale(1.35)"}, {transform:"rotate(360deg) scale(1.12)"}], {duration:600, easing:"ease-out"});
  if (!$("home").hidden) { renderHome(); say(LANGS[code].label, code); }
  else if (!$("themes").hidden && openAct.cur) openAct(openAct.cur);
  else if (!$("parent").hidden) openParent();
  // in a game: start it again in the new language, so every text and the voice change together
  else if (!$("game").hidden && lastLaunch) { if (G && !G.done) endSession(false); launch(...lastLaunch); }
  else if (!$("end").hidden) sayT($("endTitle").textContent);
}
