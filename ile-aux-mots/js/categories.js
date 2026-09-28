/* L'Île aux Mots : les îles rangées par catégories, avec des onglets en images dans la langue apprise.
   A game may declare meta.cat itself (registerGame); otherwise CAT_OF places it. The chosen tab is kept per child. */
const CATS = [
  {id:"all",     em:"🌟", t:{en:"All", de:"Alle", lb:"All", zh:"全部"}},
  {id:"mots",    em:"🗣️", t:{en:"Words", de:"Wörter", lb:"Wierder", zh:"词语"}},
  {id:"nombres", em:"🔢", t:{en:"Numbers & logic", de:"Zahlen & Logik", lb:"Zuelen & Logik", zh:"数字和逻辑"}},
  {id:"monde",   em:"🌍", t:{en:"The world", de:"Die Welt", lb:"D'Welt", zh:"世界"}},
  {id:"sciences", em:"🔬", t:{en:"Science", de:"Forschen", lb:"Fuerschen", zh:"科学"}},
  {id:"musique", em:"🎵", t:{en:"Music & dance", de:"Musik & Tanz", lb:"Musek & Danz", zh:"音乐和舞蹈"}},
  {id:"bouger",  em:"🎮", t:{en:"Move & play", de:"Bewegen & spielen", lb:"Bewegen & spillen", zh:"动一动，玩一玩"}},
  {id:"creer",   em:"🎨", t:{en:"Create", de:"Gestalten", lb:"Gestalten", zh:"创作"}}
];
const CAT_OF = {
  imagier:"mots", ecoute:"mots", memory:"mots", repete:"mots", alphabet:"mots", machine:"mots", epelle:"mots", phrases:"mots",
  devinettes:"mots", manque:"mots", vraifaux:"mots", contraires:"mots", detective:"mots",
  compte:"nombres", suites:"nombres", heure:"nombres", calendrier:"nombres", marche:"nombres", formes:"nombres", pirate:"nombres",
  pays:"monde", emotions:"monde", cris:"monde", habille:"monde", cachecache:"monde",
  ballons:"bouger", simon:"bouger", taupes:"bouger", peche:"bouger", monstre:"bouger", loup:"bouger",
  coloriage:"creer", labo:"creer"
};
addStyle(`
.cats{gap:8px; flex-wrap:wrap}
.cats .cat{font-size:15px; font-weight:800; padding:8px 12px; border:3px solid var(--ink); border-radius:999px; background:var(--paper); box-shadow:2px 3px 0 var(--ink); display:flex; align-items:center; gap:6px}
.cats .cat .ce{font-size:22px; line-height:1}
.cats .cat[aria-pressed="true"]{background:var(--sun); transform:translateY(-2px)}
.map .cat-titre{grid-column:1/-1; margin:6px 0 -4px; font-family:var(--display); font-size:22px; color:#fff; text-shadow:2px 2px 0 var(--ink); display:flex; align-items:center; gap:8px}
.map .cat-titre:first-child{margin-top:0}
.map .spot{animation-delay:calc(min(var(--i, 0), 12) * 45ms)} /* forty islands: the last ones must not wait two seconds */
`);
const catOf = a => a.cat || CAT_OF[a.id] || "mots";
const catLabel = c => c.t[langOf()] || c.t.en;
function catChosen(){ const c = S.prof[S.kid].cat; return CATS.some(x => x.id === c) ? c : "all"; }
// tabs above the islands, then the islands of the chosen category (grouped under a title in "All")
function renderCategories(map, list, island){
  const bar = $("cats");
  if (bar) {
    bar.innerHTML = "";
    CATS.filter(c => c.id === "all" || list.some(a => catOf(a) === c.id)).forEach(c => {
      const b = el("button", "cat", `<span class="ce">${c.em}</span><span>${catLabel(c)}</span>`);
      b.setAttribute("aria-pressed", String(catChosen() === c.id)); b.dataset.cat = c.id;
      b.onclick = () => { S.prof[S.kid].cat = c.id; saveLocal(); sfx.tap(); renderHome(); };
      bar.append(b);
    });
  }
  const chosen = catChosen();
  CATS.filter(c => c.id !== "all" && (chosen === "all" || chosen === c.id)).forEach(c => {
    const here = list.filter(a => catOf(a) === c.id);
    if (!here.length) return;
    if (chosen === "all") map.append(el("h2", "cat-titre", `<span>${c.em}</span><span>${catLabel(c)}</span>`));
    here.forEach(a => map.appendChild(island(a)));
  });
}
