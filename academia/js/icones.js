/* Académia : les icônes dessinées de l'interface (outils/icones), à la place des émojis du système, dont le dessin
   change d'un appareil à l'autre et n'est jamais dans le style du jeu. Une icône est une image de la qualité du jeu
   (assets/hd/icones ou assets/leger/icones, js/jeu/qualite.js), posée dans le texte à la taille de sa police.
   1. iconeDom(nom) : l'image, sans texte alternatif (la voix ne la lit pas, le texte à côté dit tout).
   2. texteIcones(texte) : un texte de l'interface dont les émojis connus (EMOJI_ICONES) deviennent leurs icônes ;
      ecrireIcones(n, texte) l'écrit dans un élément ; iconiser(n) fait de même dans un élément déjà écrit (une bulle,
      un message). Le contenu des questions (objets à compter, images à choisir) ne passe jamais par là.
   3. Dans la page : <img data-icone="sac"> reçoit son image au lancement ; les styles lisent var(--ico-<nom>).
   Chargé après js/jeu/qualite.js. */
const ICONES = [
  // the game: money, attacks, jokers, rewards
  "etincelle", "eclair", "choc", "etoile", "bouclier", "couronne", "revanche", "fusee", "potion", "chouette", "sablier",
  "fuite", "epees", "pattes", "cadeau", "niveau", "trophee", "confettis", "dodo",
  // menus: the bag, closing, arrows, yes and no, people, the speaker, the doors' padlocks, the grown-ups' settings
  "sac", "croix", "suivant", "retour", "lecture", "demi_tour", "coche", "vrai", "faux", "pouce", "parents", "gardien",
  "haut_parleur", "cadenas", "cadenas_ouvert", "images", "enregistrer", "ouvrir", "crayon",
  // one symbol per house (so per subject) and one medal per region
  "maison_dojo", "maison_albion", "maison_germania", "maison_duche", "maison_jardin", "maison_observatoire",
  "medaille_dojo", "medaille_albion", "medaille_germania", "medaille_duche", "medaille_jardin", "medaille_observatoire"
];
// the system emojis the interface wrote, and the drawn icon shown in their place (without the emoji variation sign)
const EMOJI_ICONES = {
  "✨": "etincelle", "⚡": "eclair", "💥": "choc", "🌟": "etoile", "🛡": "bouclier", "👑": "couronne", "🔁": "revanche",
  "🚀": "fusee", "🧪": "potion", "🦉": "chouette", "⏳": "sablier", "⌛": "sablier", "🏃": "fuite", "⚔": "epees",
  "🐾": "pattes", "🎁": "cadeau", "⬆": "niveau", "🏆": "trophee", "🎉": "confettis", "💤": "dodo",
  "🎒": "sac", "✖": "croix", "❌": "faux", "➡": "suivant", "⬅": "retour", "▶": "lecture", "↩": "demi_tour",
  "✔": "coche", "✅": "vrai", "👍": "pouce", "👪": "parents", "👤": "gardien", "🔊": "haut_parleur",
  "🔒": "cadenas", "🔓": "cadenas_ouvert", "🖼": "images", "💾": "enregistrer", "📂": "ouvrir", "✏": "crayon",
  "🥋": "maison_dojo", "⛵": "maison_albion", "⛰": "maison_germania", "🏰": "maison_duche", "🌸": "maison_jardin",
  "🔭": "maison_observatoire"
};
const MOTIF_ICONES = "(" + Object.keys(EMOJI_ICONES).sort((a, b) => b.length - a.length)
  .map(k => k.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("|") + ")\\uFE0F?";
const A_ICONE = new RegExp(MOTIF_ICONES, "u");   // a test only: no position kept from one text to the next
// the picture's address at the game's quality; a region's symbol is maison_<id>, its medal medaille_<id>
const iconeSrc = nom => `${DOSSIER_HD}/icones/${nom}.png`;

// the picture of an icon; asked again once under a new address when the network drops it
function iconeDom(nom, cls){
  const i = document.createElement("img");
  i.className = "icone ico-" + nom + (cls ? " " + cls : "");
  i.alt = ""; i.draggable = false; i.decoding = "async"; i.dataset.icone = nom;
  i.onerror = () => { if (i.dataset.essai) { i.onerror = null; return; } i.dataset.essai = "1"; i.src = iconeSrc(nom) + "?essai=1"; };
  i.src = iconeSrc(nom);
  return i;
}
// a text cut into its pieces: strings, and {icone} where a known emoji stood
function morceauxIcones(t){
  const s = String(t == null ? "" : t), l = [];
  let i = 0;
  for (const m of s.matchAll(new RegExp(MOTIF_ICONES, "gu"))) {
    if (m.index > i) l.push(s.slice(i, m.index));
    l.push({icone: EMOJI_ICONES[m[1]]});
    i = m.index + m[0].length;
  }
  if (i < s.length) l.push(s.slice(i));
  return l;
}
// a text of the interface, its emojis drawn
function texteIcones(t){
  const f = document.createDocumentFragment();
  morceauxIcones(t).forEach(m => f.append(typeof m === "string" ? m : iconeDom(m.icone)));
  return f;
}
function ecrireIcones(n, t){ n.replaceChildren(texteIcones(t)); return n; }
// an element already written (a message, a bubble): its text nodes with a known emoji are drawn again
function iconiser(n){
  if (!n) return n;
  const w = document.createTreeWalker(n, NodeFilter.SHOW_TEXT), l = [];
  for (let t = w.nextNode(); t; t = w.nextNode()) if (A_ICONE.test(t.data)) l.push(t);
  l.forEach(t => t.replaceWith(texteIcones(t.data)));
  return n;
}
// the icons written in the page (index.html) and the ones the styles use
function remplirIcones(racine = document){
  racine.querySelectorAll("img[data-icone]:not([src])").forEach(i => {
    const nom = i.dataset.icone;
    i.alt = i.alt || ""; i.draggable = false;
    i.onerror = () => { i.onerror = null; i.src = iconeSrc(nom) + "?essai=1"; };
    i.src = iconeSrc(nom);
  });
}
(function iconesDeLaPage(){
  const s = document.documentElement.style;
  ICONES.forEach(nom => s.setProperty("--ico-" + nom, `url("${new URL(iconeSrc(nom), location.href).href}")`));
  remplirIcones();
  // the others in the background once the game is there: a button never shows its icon late
  const tous = () => ICONES.forEach(nom => { const i = new Image(); i.decoding = "async"; i.src = iconeSrc(nom); });
  if (window.__jeuPret) setTimeout(tous, 300);
  else document.addEventListener("jeu-pret", () => setTimeout(tous, 300), {once: true});
})();
