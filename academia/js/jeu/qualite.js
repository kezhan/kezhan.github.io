/* Académia : deux versions des images. Haute définition (assets/hd, toile à deux fois la densité CSS) sur une
   tablette récente, légère (assets/leger, moitié de résolution, toile à la densité CSS) sinon : même dessin, même
   code. Le choix est automatique, un parent peut le forcer dans le sac (réglage gardé dans le navigateur). */
const CLE_QUALITE = "academia.qualite";
function qualiteVoulue(){ try { return localStorage.getItem(CLE_QUALITE) || "auto"; } catch (e) { return "auto"; } }
function choisirQualite(v){ try { localStorage.setItem(CLE_QUALITE, v); } catch (e) {} }
const QUALITE = (() => {
  const v = qualiteVoulue();
  if (v === "hd" || v === "leger") return v;
  const dpr = devicePixelRatio || 1, memoire = navigator.deviceMemory || 4;   // GB, Chrome only
  return dpr >= 1.5 && memoire >= 4 ? "hd" : "leger";
})();
const RATIO = QUALITE === "hd" ? 2 : 1;   // canvas pixels per CSS pixel
const DOSSIER_HD = `assets/${QUALITE}`;

// atlases: one per character, frames named by pose (outils/personnages/scripts/exporter.py)
const ATLAS = {};   // key -> {frames: {...}, meta} once fetched (for the menus outside Phaser)
async function atlasJSON(cle, type){
  if (ATLAS[cle] !== undefined) return ATLAS[cle];
  try { const r = await fetch(`${DOSSIER_HD}/${type}/${cle}.json`); ATLAS[cle] = r.ok ? await r.json() : null; } catch (e) { ATLAS[cle] = null; }
  return ATLAS[cle];
}
const typeAtlas = cle => /^(heros_|sage$|hugo$|paco$)/.test(cle) ? "humains" : "creatures";

// load the atlases a scene needs (call from preload; already loaded ones are skipped); missing ones fall back to the pixel art
const ATLAS_MANQUANTS = new Set();
function chargerAtlas(scene, cles){
  const a = [...new Set(cles)].filter(c => c && !scene.textures.exists("hd_" + c) && !ATLAS_MANQUANTS.has(c));
  if (!a.length) return;
  a.forEach(c => { const t = typeAtlas(c); scene.load.atlas("hd_" + c, `${DOSSIER_HD}/${t}/${c}.png`, `${DOSSIER_HD}/${t}/${c}.json`); });
  const erreur = f => { if (f.key && f.key.startsWith("hd_")) ATLAS_MANQUANTS.add(f.key.slice(3)); };
  scene.load.on("loaderror", erreur);
  scene.load.once("complete", () => {
    scene.load.off("loaderror", erreur);
    a.forEach(c => { if (scene.textures.exists("hd_" + c)) scene.textures.get("hd_" + c).setFilter(Phaser.Textures.FilterMode.LINEAR); else ATLAS_MANQUANTS.add(c); });
  });
}
const aHD = (scene, cle) => !!cle && scene.textures.exists("hd_" + cle);
// outside preload (a companion chosen in the bag, an evolution): load what is missing, then go on (at once if nothing is)
function chargerPuis(scene, cles, apres){
  const manque = [...new Set(cles)].filter(c => c && !scene.textures.exists("hd_" + c) && !ATLAS_MANQUANTS.has(c));
  if (!manque.length) return apres();
  chargerAtlas(scene, manque);
  scene.load.once("complete", () => { if (scene.sys.isActive() || scene.sys.isSleeping()) apres(); });
  scene.load.start();
}
