/* Académia : deux versions des images. Haute définition (assets/hd, toile à deux fois la densité CSS) sur une
   tablette récente, légère (assets/leger, moitié de résolution, toile à la densité CSS) sinon : même dessin, même
   code. Le choix est automatique (densité de l'écran, mémoire, carte graphique), un parent peut le forcer dans le
   sac (réglage gardé dans le navigateur). Un dessin qui n'arrive pas (réseau) est redemandé ; un personnage attend
   son dessin, invisible, au lieu d'afficher le carré noir du moteur (js/jeu/sprites_hd.js, quandAtlas). */
const CLE_QUALITE = "academia.qualite";
function qualiteVoulue(){ try { return localStorage.getItem(CLE_QUALITE) || "auto"; } catch (e) { return "auto"; } }
function choisirQualite(v){ try { localStorage.setItem(CLE_QUALITE, v); } catch (e) {} }
// the largest picture the graphics card holds (0 without WebGL: the game then draws without the card)
const TEXTURE_MAX = (() => {
  try {
    const gl = document.createElement("canvas").getContext("webgl");
    if (!gl) return 0;
    const n = gl.getParameter(gl.MAX_TEXTURE_SIZE), fin = gl.getExtension("WEBGL_lose_context");
    if (fin) fin.loseContext();   // this test context is freed at once
    return n;
  } catch (e) { return 0; }
})();
const QUALITE = (() => {
  const v = qualiteVoulue();
  if (v === "hd" || v === "leger") return v;
  const dpr = devicePixelRatio || 1, memoire = navigator.deviceMemory || 4;   // GB, Chrome only
  return dpr >= 1.5 && memoire >= 4 && TEXTURE_MAX >= 4096 ? "hd" : "leger";
})();
const RATIO = QUALITE === "hd" ? 2 : 1;   // canvas pixels per CSS pixel
const DOSSIER_HD = `assets/${QUALITE}`;

// the pictures that could not be loaded, even asked again (shown in the bag, for the grown-ups)
const ECHECS = new Set();
const ESSAIS = {};   // atlas key -> failed attempts
const ESSAIS_MAX = 3;

// atlases: one per character, frames named by pose (outils/personnages/scripts/exporter.py)
const ATLAS = {};   // key -> {frames: {...}, meta} once fetched (for the menus outside Phaser)
async function atlasJSON(cle, type){
  if (ATLAS[cle]) return ATLAS[cle];
  for (let n = 0; n < ESSAIS_MAX; n++) {   // a network hiccup: asked again, under a new address
    try {
      const r = await fetch(`${DOSSIER_HD}/${type}/${cle}.json` + (n ? `?essai=${n}` : ""));
      if (r.ok) { ECHECS.delete(cle); return (ATLAS[cle] = await r.json()); }
    } catch (e) {}
    await new Promise(f => setTimeout(f, 500 * (n + 1)));
  }
  ECHECS.add(cle);
  return null;
}
const typeAtlas = cle => /^(heros_|sage$|hugo$|paco$)/.test(cle) ? "humains" : "creatures";

// load the atlases a scene needs (from preload, or at any time with chargerPuis); already loaded ones are skipped.
// A sheet that fails is asked again half a second later under a new address (no broken copy from a cache).
// Returns the keys being loaded.
function chargerAtlas(scene, cles){
  const a = [...new Set(cles)].filter(c => c && !scene.textures.exists("hd_" + c) && (ESSAIS[c] || 0) < ESSAIS_MAX);
  if (!a.length) return [];
  a.forEach(c => {
    const t = typeAtlas(c), q = ESSAIS[c] ? `?essai=${ESSAIS[c]}` : "";
    scene.load.atlas("hd_" + c, `${DOSSIER_HD}/${t}/${c}.png${q}`, `${DOSSIER_HD}/${t}/${c}.json${q}`);
  });
  scene.load.once("complete", () => {
    const encore = [];
    a.forEach(c => {
      if (scene.textures.exists("hd_" + c)) { scene.textures.get("hd_" + c).setFilter(Phaser.Textures.FilterMode.LINEAR); ECHECS.delete(c); return; }
      ESSAIS[c] = (ESSAIS[c] || 0) + 1;
      if (ESSAIS[c] < ESSAIS_MAX) encore.push(c); else ECHECS.add(c);
    });
    if (encore.length && (scene.sys.isActive() || scene.sys.isSleeping()))
      scene.time.delayedCall(500, () => { if (chargerAtlas(scene, encore).length && !scene.load.isLoading()) scene.load.start(); });
  });
  return a;
}
const aHD = (scene, cle) => !!cle && scene.textures.exists("hd_" + cle);
// outside preload (a companion chosen in the bag, an evolution): load what is missing, then go on (at once if nothing is)
function chargerPuis(scene, cles, apres){
  if (!chargerAtlas(scene, cles).length) return apres();
  scene.load.once("complete", () => { if (scene.sys.isActive() || scene.sys.isSleeping()) apres(); });
  if (!scene.load.isLoading()) scene.load.start();
}
// a picture (not a sheet) that did not come is asked again, twice, under a new address; set once per scene
function reessayerImages(scene){
  if (scene.load.reessai) return;
  const essais = scene.load.reessai = {};
  scene.load.on("loaderror", f => {
    if (f.type !== "image" || f.multiFile || !f.url) return;
    const n = essais[f.key] = (essais[f.key] || 0) + 1;
    if (n < ESSAIS_MAX) scene.load.image(f.key, String(f.url).split("?")[0] + "?essai=" + n);
    else ECHECS.add(f.key);
  });
}
