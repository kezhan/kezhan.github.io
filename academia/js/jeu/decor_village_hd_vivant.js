/* Académia : ce qui vit dans le village en haute définition (js/jeu/decor_village_hd.js) : lanternes du Dojo, lanternes
   de la place et phare qui luisent, fumée des cheminées, drapeau dans le vent, éclats du Jardin, étoile de
   l'Observatoire, caneton et reflets sur la mare, un ou deux papillons près de chaque maison, vent dans les hautes
   herbes. Quand l'enfant demande le calme (ou pendant les tests automatiques), seul ce qui ne bouge pas reste. */

// a character steps into the tall grass of tile (x, y): its two tufts sway (Monde.apresPas, Monde.pas for the
// companion, OmbresVillage.promener for an Ombre)
function secouerHerbeHD(scene, x, y){ VIVANT_HD.secouer(scene, x, y); }

const VIVANT_HD = (() => {
  const immobile = () => (typeof calme === "function" ? calme() : typeof TEST !== "undefined" && TEST);
  const entre = (a, b) => a + Math.random() * (b - a);

  function animer(scene, carte, rendu){
    const anim = !immobile(), poser = VILLAGE_HD.poser, tw = c => anim && scene.tweens.add(c);
    const boucle = (ms, f) => anim && scene.time.addEvent({delay: ms, loop: true, callback: f});
    // a soft glow that breathes
    const lueur = (x, y, d, echelle) => {
      const g = poser(scene, "lueur", x, y, d, {echelle});
      if (g) { g.setBlendMode(Phaser.BlendModes.ADD).setAlpha(.55); tw({targets: g, alpha: .25, duration: 1300, yoyo: true, repeat: -1, ease: "Sine.easeInOut"}); }
    };
    rendu.maisons.forEach(m => {
      const p = q => VILLAGE_HD.point(m.nom, q, m.x, m.y), d = profondeur(m.ligne, 1);
      ["phare", "lanterne_g", "lanterne_d"].forEach(q => { const l = p(q); if (l) lueur(l.x, l.y, d, q === "phare" ? 2.2 : 1.1); });
      const f = p("fumee");
      if (f) boucle(1100, () => {
        const b = poser(scene, "fumee", f.x + entre(-1, 1), f.y, d, {echelle: .6}); if (!b) return;
        scene.tweens.add({targets: b, y: f.y - 16, x: f.x + entre(2, 6), scale: b.scale * 2.1, alpha: 0, duration: 2600, ease: "Sine.easeOut", onComplete: () => b.destroy()});
      });
      const e = p("eclat");   // the garden's sparkles, in tender colours
      if (e) boucle(700, () => {
        const s = poser(scene, "eclat", e.x + entre(-26, 26), e.y + entre(-4, 34), d + 2, {echelle: entre(.5, .9)}); if (!s) return;
        const k = s.scale;
        s.setTint([0xFFC93C, 0xFF6FB5, 0x5CBDFF, 0x4FD99A, 0xA77BFF][Math.floor(Math.random() * 5)]).setScale(0);
        scene.tweens.add({targets: s, scale: k, angle: 90, duration: 520, yoyo: true, ease: "Sine.easeInOut", onComplete: () => s.destroy()});
      });
      const o = p("etoile"), g = o && poser(scene, "eclat", o.x, o.y, d + 2, {echelle: .9});
      if (g) { g.setTint(0xFFF3B0).setAlpha(.9); tw({targets: g, scale: g.scale * .45, angle: 45, duration: 900, yoyo: true, repeat: -1, ease: "Sine.easeInOut"}); }
    });
    rendu.lumieres.forEach(l => { if (l.x !== undefined) lueur(l.x, l.y, l.profondeur, 1.3); });
    if (rendu.drapeau) { const k = rendu.drapeau.scaleX; tw({targets: rendu.drapeau, scaleX: k * .8, scaleY: k * 1.04, duration: 520, yoyo: true, repeat: -1, ease: "Sine.easeInOut"}); }
    mare(scene, rendu, tw, boucle);
    papillons(scene, carte, rendu, anim);
    if (anim) vent(scene, rendu);
  }

  // the duckling swims across the pond and back, glints come and go on the water (inside its oval)
  function mare(scene, rendu, tw, boucle){
    const eau = rendu.eau; if (!eau || !eau.canard) return;
    const c = eau.canard, cx = (eau.x0 + eau.x1) / 2, cy = (eau.y0 + eau.y1) / 2, rx = (eau.x1 - eau.x0) / 2, ry = (eau.y1 - eau.y0) / 2;
    c.x = cx - rx * .62;
    tw({targets: c, x: cx + rx * .62, duration: 7000, yoyo: true, repeat: -1, hold: 900, repeatDelay: 900, ease: "Sine.easeInOut",
      onYoyo: () => c.setFlipX(true), onRepeat: () => c.setFlipX(false)});
    tw({targets: c, y: c.y - .8, duration: 700, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
    boucle(900, () => {
      const a = Math.random() * Math.PI * 2, k = Math.sqrt(Math.random()) * .6;
      const r = VILLAGE_HD.poser(scene, "reflet", cx + Math.cos(a) * rx * k, cy + Math.sin(a) * ry * k, 7, {echelle: entre(.6, 1)}); if (!r) return;
      r.setAlpha(0); scene.tweens.add({targets: r, alpha: .9, duration: 600, yoyo: true, hold: 300, onComplete: () => r.destroy()});
    });
  }

  // one or two butterflies by each house, over the flowers nearest to it (or beside the house)
  function papillons(scene, carte, rendu, anim){
    const fleurs = carte.elements.filter(e => e.type === "fleur");
    rendu.maisons.filter(m => m.nom.startsWith("maison_")).forEach((m, i) => {
      const cx = m.x / CASE, cy = m.y / CASE;
      const proches = fleurs.map(e => ({e, d: Math.hypot(e.x + .5 - cx, e.y - cy)})).filter(o => o.d < 9).sort((a, b) => a.d - b.d).slice(0, 2);
      const lieux = proches.length ? proches.map(o => [o.e.x * CASE + 8, o.e.y * CASE + 4]) : [[m.x + 44, m.y - 20]];
      lieux.forEach(([x, y], k) => {
        const b = VILLAGE_HD.poser(scene, (i + k) % 2 ? "papillon_bleu" : "papillon", x, y, 9000);
        if (!b || !anim) return;
        scene.tweens.add({targets: b, scaleX: b.scaleX * .3, duration: 130, yoyo: true, repeat: -1, ease: "Sine.easeInOut"});
        const voler = () => scene.tweens.add({targets: b, x: x + entre(-26, 26), y: y + entre(-20, 8), duration: entre(1400, 2400),
          ease: "Sine.easeInOut", onComplete: voler});
        voler();
      });
    });
  }

  // the wind in the tall grass, and a tuft that a character steps into sways harder
  function vent(scene, rendu){
    const touffes = Object.values(rendu.touffes).flat();
    const maj = (t) => touffes.forEach(o => {
      const d = t - o.choc, choc = d < 900 ? 11 * Math.exp(-d / 220) * Math.sin(d / 45) : 0;
      o.setAngle(1.8 * Math.sin(t / 650 + o.phase) + choc);
    });
    scene.events.on("update", maj);
    scene.events.once("shutdown", () => scene.events.off("update", maj));
  }
  function secouer(scene, x, y){
    const r = scene.villageHD, l = r && r.touffes[x + "," + y];
    if (l && !immobile()) l.forEach((t, k) => { t.choc = scene.time.now - k * 60; });
  }
  return {animer, secouer};
})();
