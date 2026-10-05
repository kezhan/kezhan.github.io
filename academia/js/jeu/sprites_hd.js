/* Académia : les personnages du village en haute définition (atlas de assets/hd ou assets/leger, js/jeu/qualite.js).
   Pieds au sol ; hauteurs en unités de la carte (une case = 16). Humains : vraie marche de face, de dos et de profil.
   Créatures : dessin de face qui sautille, se balance et se retourne vers où il va ; au repos, il respire.
   Un dessin pas encore arrivé (réseau lent) : le personnage attend, invisible, et le prend dès qu'il arrive. */
const HAUTEUR_HUMAIN = 26, HAUTEUR_CREATURE = 17;

function poserSprite(s, x, y){ s.setPosition(x * CASE + 8, y * CASE + s.dy); return s; }
const dessine = s => s.texture.key !== "__DEFAULT";   // false while the character waits for its drawing

// run fn once the drawing `cle` is there: at once, or when it arrives (it is asked for, and asked again if it fails);
// never the engine's black square in between
function quandAtlas(scene, cle, fn){
  const k = "hd_" + cle;
  if (scene.textures.exists(k)) return fn();
  const ev = Phaser.Textures.Events.ADD_KEY + k;
  const arrive = () => { if (scene.sys.isActive() || scene.sys.isSleeping()) fn(); };
  scene.textures.once(ev, arrive);
  scene.events.once("shutdown", () => scene.textures.off(ev, arrive));
  if (chargerAtlas(scene, [cle]).length && !scene.load.isLoading()) scene.load.start();
}

// vivant: a villager who breathes (the hero walks, he does not)
function spriteHumain(scene, cle, x, y, vivant = false){
  const s = scene.add.sprite(0, 0, "__DEFAULT").setOrigin(.5, 1);   // transparent until the drawing is there
  s.dy = 15; s.hd = cle; s.dir = "bas"; s.base = 1;
  quandAtlas(scene, cle, () => {
    if (!s.active) return;
    s.setTexture("hd_" + cle, "face").setScale(1);
    s.base = HAUTEUR_HUMAIN / s.height; s.setScale(s.base);
    animsHumain(scene, cle);
    reposHumain(s, s.dir);
    if (vivant) respirer(s, .012);
  });
  return poserSprite(s, x, y);
}
// a pose of a character's sheet, when the drawing is there and has it (parle, joie...)
function poseHumain(s, nom){ if (s && s.active && dessine(s) && s.texture.has(nom)) s.setFrame(nom); }
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
  s.dir = dir;
  if (!dessine(s)) return;
  const cote = dir === "gauche" || dir === "droite", anim = `hd_${s.hd}-${cote ? "cote" : dir}`;
  s.setFlipX(dir === "gauche");
  if (s.scene.anims.exists(anim)) s.play(anim, true);
  else { s.stop(); s.setFrame(poseDe(dir)); balancer(s); }   // no walk cycle drawn for this side: a little sway
}
function reposHumain(s, dir){
  s.dir = dir;
  if (!dessine(s)) return;
  s.stop(); s.setFlipX(dir === "gauche"); s.setFrame(poseDe(dir)); s.setAngle(0);
}

function spriteCreature(scene, cle, x, y, hauteur = HAUTEUR_CREATURE){
  const s = scene.add.sprite(0, 0, "__DEFAULT").setOrigin(.5, 1);
  s.base = 1; s.dy = 15; s.hd = cle;
  quandAtlas(scene, cle, () => {
    if (!s.active) return;
    if (s.souffle) { s.souffle.stop(); s.souffle = null; }
    s.setTexture("hd_" + cle, "face_petit").setScale(1);
    s.base = hauteur / s.height; s.setScale(s.base);
    respirer(s);
  });
  return poserSprite(s, x, y);
}
// one step of a drawing without a walk cycle: it turns toward where it goes, hops (stretched, then squashed) and sways
function marcherCreature(s, dir, duree){
  if (!dessine(s)) return;
  if (s.souffle) { s.souffle.stop(); s.souffle = null; }
  const t = s.scene.textures.get("hd_" + s.hd);
  s.setFrame(dir === "haut" && t.has("dos_petit") ? "dos_petit" : "face_petit");
  if (dir === "gauche" || dir === "droite") s.setFlipX(dir === "gauche");
  s.setScale(s.base);
  s.scene.tweens.add({targets: s, scaleY: s.base * 1.08, scaleX: s.base * .94, duration: duree * .45, yoyo: true, ease: "Quad.easeOut"});
  balancer(s, duree);
}
function reposCreature(s){ if (!dessine(s)) return; s.setFrame("face_petit"); s.setAngle(0); s.setScale(s.base); respirer(s); }
function balancer(s, duree = 170){
  s.pas = (s.pas || 0) + 1;
  s.scene.tweens.add({targets: s, angle: s.pas % 2 ? 5 : -5, duration: duree * .5, yoyo: true, ease: "Sine.easeInOut"});
}
// breathing: a slow stretch, forever, until arreterSouffle (before the sprite goes: no tween left running)
function respirer(s, ampleur = .03){
  if (TEST || s.souffle) return;
  s.souffle = s.scene.tweens.add({targets: s, scaleY: s.base * (1 + ampleur), scaleX: s.base * (1 - ampleur / 2), duration: 900 + Math.random() * 300,
    yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
}
function arreterSouffle(s){ if (s && s.souffle) { s.souffle.stop(); s.souffle = null; } }

// a glow around a creature (an Ombre, a companion at its last stage), gently pulsing: its own silhouette, a little
// bigger and filled with the colour, just behind it, and a soft haze further out. Pictures that follow it (pose,
// size, turn), not a graphics-card effect: some tablets draw those as black squares, and they are slow
function halo(scene, s, couleur, ampleur = 1.1, alpha = .55){
  const contour = scene.add.sprite(s.x, s.y, "__DEFAULT").setOrigin(.5, 1);
  const brume = scene.textures.exists("fx_halo") ? scene.add.image(s.x, s.y, "fx_halo").setTint(couleur) : null;
  const pouls = {v: 1};
  const battement = TEST ? null : scene.tweens.add({targets: pouls, v: .65, duration: 1100, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
  const finir = () => {   // the pulse goes with the glow (it would run forever after each fight otherwise)
    scene.events.off("update", suivre); if (battement) battement.stop();
    if (contour.active) contour.destroy(); if (brume && brume.active) brume.destroy();
  };
  const suivre = () => {
    if (!s.active || !s.scene) return finir();
    const vu = s.visible && dessine(s), h = s.displayHeight;
    if (vu && (contour.texture !== s.texture || contour.frame.name !== s.frame.name)) contour.setTexture(s.texture.key, s.frame.name).setTintFill(couleur);
    contour.setPosition(s.x, s.y + h * (ampleur - 1) / 2).setScale(s.scaleX * ampleur, s.scaleY * ampleur).setFlipX(s.flipX).setAngle(s.angle)
      .setDepth(s.depth - .2).setVisible(vu).setAlpha(alpha * s.alpha * pouls.v);
    if (brume) {
      const t = Math.max(s.displayWidth, h) * 1.7;
      brume.setPosition(s.x, s.y - h * .5).setDisplaySize(t, t).setDepth(s.depth - .3).setVisible(vu).setAlpha(alpha * .6 * s.alpha * pouls.v);
    }
  };
  suivre();
  scene.events.on("update", suivre);
  scene.events.once("shutdown", finir);
  return contour;
}
