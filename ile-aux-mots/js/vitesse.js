/* L'Île aux Mots : vitesse de la voix, choisie par un geste (🐌 🐢 🐇) en haut de chaque écran, gardée par enfant.
   Kezhan : « parfois, c'est un peu rapide […] les enfants sont petits, par défaut, ça doit être un peu plus lent ». */
const SPEEDS = [{em:"🐌", rate:0.5, key:"speedSlow"}, {em:"🐢", rate:0.65, key:"speedMid"}, {em:"🐇", rate:0.8, key:"speedFast"}];
const SPEED_DEFAULT = 1;
function speedOf(kid){ const v = S.prof[kid || S.kid].speed; return SPEEDS[v] ? v : SPEED_DEFAULT; }
function voiceRate(){ return SPEEDS[speedOf()].rate; }
// lod.lu recordings play at natural speed at 🐇, slower in step below (the pitch is kept)
function audioRate(){ return voiceRate() / SPEEDS[SPEEDS.length - 1].rate; }
// Azure recordings (js/voix_lecture.js) are made a little slow: 🐇 plays them as they are, 🐢 at 0.875, 🐌 at 0.75,
// and never under 0.7, where the stretched sound gets rough (a slower rate asked by a game stops there)
function fileRate(rate){ return Math.min(1.25, Math.max(0.7, 1 + (rate - SPEEDS[SPEEDS.length - 1].rate) * 5 / 6)); }

addStyle(`.speeds{display:flex; gap:4px}
  .speed{width:48px; height:44px; padding:0; font-size:24px; border:3px solid var(--ink); border-radius:10px; background:#fff;
    box-shadow:2px 3px 0 var(--ink); cursor:pointer; opacity:.6; transition:transform .15s, opacity .15s}
  .speed[aria-pressed="true"]{opacity:1; background:var(--sun); transform:scale(1.1)}`);

function renderSpeed(){
  const box = $("speedBar"); if (!box) return;
  box.innerHTML = ""; box.setAttribute("aria-label", tx("speedAria"));
  SPEEDS.forEach((s, i) => {
    const b = el("button", "speed", s.em);
    b.setAttribute("aria-label", tx(s.key)); b.setAttribute("aria-pressed", String(speedOf() === i));
    b.onclick = () => {
      S.prof[S.kid].speed = i; saveProfile(S.kid); sfx.tap(); renderSpeed();
      const nb = box.children[i]; if (!fx.calm()) nb.animate([{transform:"scale(1)"}, {transform:"scale(1.3) rotate(-8deg)"}, {transform:"scale(1.1)"}], {duration:400, easing:"ease-out"});
      // a word from the lexicon, so that Luxembourgish is heard too (lod.lu recording)
      say(T(THEMES.animals.words[0]));
    };
    box.append(b);
  });
}
