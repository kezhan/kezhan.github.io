/* Académia : les personnages du village en haute définition (atlas de assets/hd ou assets/leger, js/jeu/qualite.js).
   Pieds au sol ; hauteurs en unités de la carte (une case = 16). Humains : vraie marche de face, de dos et de profil.
   Créatures : dessin de face qui sautille, se balance et se retourne vers où il va ; au repos, il respire. */
const HAUTEUR_HUMAIN = 26, HAUTEUR_CREATURE = 17;

function poserSprite(s, x, y){ s.setPosition(x * CASE + 8, y * CASE + s.dy); return s; }

function spriteHumain(scene, cle, x, y){
  const s = scene.add.sprite(0, 0, "hd_" + cle, "face").setOrigin(.5, 1);
  s.setScale(HAUTEUR_HUMAIN / s.height); s.dy = 15; s.hd = cle;
  animsHumain(scene, cle);
  return poserSprite(s, x, y);
}
function animsHumain(scene, cle){
  const k = "hd_" + cle, noms = scene.textures.get(k).getFrameNames();
  const anim = (nom, cadres, fps) => {
    if (!scene.anims.exists(`${k}-${nom}`) && cadres.every(c => noms.includes(c)))
      scene.anims.create({key: `${k}-${nom}`, frames: cadres.map(c => ({key: k, frame: c})), frameRate: fps, repeat: -1});
  };
  anim("bas", [0, 1, 2, 3].map(i => "face_marche" + i), 8);
  anim("haut", [0, 1, 2, 3].map(i => "dos_marche" + i), 8);
  anim("cote", [0, 1, 2, 3, 4, 5, 6, 7].map(i => "marche" + i), 14);
}
const poseDe = dir => dir === "haut" ? "dos" : dir === "gauche" || dir === "droite" ? "profil" : "face";
function marcherHumain(s, dir){
  const cote = dir === "gauche" || dir === "droite", anim = `hd_${s.hd}-${cote ? "cote" : dir}`;
  s.setFlipX(dir === "gauche");
  if (s.scene.anims.exists(anim)) s.play(anim, true);
  else { s.stop(); s.setFrame(poseDe(dir)); balancer(s); }   // no walk cycle drawn for this side: a little sway
}
function reposHumain(s, dir){ s.stop(); s.setFlipX(dir === "gauche"); s.setFrame(poseDe(dir)); s.setAngle(0); }

function spriteCreature(scene, cle, x, y, hauteur = HAUTEUR_CREATURE){
  const s = scene.add.sprite(0, 0, "hd_" + cle, "face_petit").setOrigin(.5, 1);
  s.base = hauteur / s.height; s.setScale(s.base); s.dy = 15; s.hd = cle;
  respirer(s);
  return poserSprite(s, x, y);
}
// one step of a drawing without a walk cycle: it turns toward where it goes, hops (stretched, then squashed) and sways
function marcherCreature(s, dir, duree){
  if (s.souffle) { s.souffle.stop(); s.souffle = null; }
  const t = s.scene.textures.get("hd_" + s.hd);
  s.setFrame(dir === "haut" && t.has("dos_petit") ? "dos_petit" : "face_petit");
  if (dir === "gauche" || dir === "droite") s.setFlipX(dir === "gauche");
  s.setScale(s.base);
  s.scene.tweens.add({targets: s, scaleY: s.base * 1.08, scaleX: s.base * .94, duration: duree * .45, yoyo: true, ease: "Quad.easeOut"});
  balancer(s, duree);
}
function reposCreature(s){ s.setFrame("face_petit"); s.setAngle(0); s.setScale(s.base); respirer(s); }
function balancer(s, duree = 170){
  s.pas = (s.pas || 0) + 1;
  s.scene.tweens.add({targets: s, angle: s.pas % 2 ? 5 : -5, duration: duree * .5, yoyo: true, ease: "Sine.easeInOut"});
}
function respirer(s){
  if (TEST || s.souffle) return;
  s.souffle = s.scene.tweens.add({targets: s, scaleY: s.base * 1.03, scaleX: s.base * .985, duration: 900 + Math.random() * 300, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
}
