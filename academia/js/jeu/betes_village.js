/* Académia : les petites bêtes du village et les cadeaux de la place, qu'on touche (Kezhan, 13/10/2026 : « revoir
   tout et donner des opportunités pour apprendre » ; lot C du ticket apprendre-partout, conception/audit_apprendre.md,
   propositions 17 et 20). Chacune se nomme dans une langue, par son enregistrement seulement (js/mots_du_jeu.js), le
   mot écrit un instant au-dessus d'elle, son sens en petit ; la petite entend ensuite le sens, les premières fois
   (direMot).
   1. Les papillons, un ou deux près de chaque maison (js/jeu/decor_village_hd_vivant.js) : ils volettent plus haut et
      se nomment dans la langue de leur maison (« butterfly » à Albion, « der Schmetterling » à Germania, « de
      Päiperlek » au Grand-Duché ; près des autres maisons, la langue du jour).
   2. Le caneton de la mare, du côté du port d'Albion : il plonge, « duck ».
   3. Les oiseaux de la place, rendus par l'Esprit des Traditions : ils s'envolent et reviennent, « de Vull ».
   4. La fontaine d'Albion, quand elle coule de nouveau : elle éclabousse, « water ».
   5. Les six étoiles de l'Observatoire autour de l'Arbre-Monde : chaque étoile touchée se compte dans la langue du jour
      (« eins, zwei, drei… »), son chiffre écrit sous le mot ; une étoile déjà comptée redit son nombre ; la sixième
      finit par « Six étoiles, bravo ! ». Dix secondes sans toucher d'étoile, et le compte recommence.
   Le doigt va d'abord au compagnon, à une Ombre, au marchand et au potager (js/jeu/monde.js), puis à un habitant
   debout là ; sur une maison, il faut viser la bête de près. Une bête touchée ne fait pas marcher le héros. Un
   habitant qui raconte quelque chose, ou une mission annoncée, n'est jamais coupé : la bête bouge sans rien dire. */
const ETIQUETTE_BETE_MS = 1900;
const NOMBRES_ETOILES = ["un", "deux", "trois", "quatre", "cinq", "six"];   // MOTS_DU_JEU keys

class BetesVillage {
  constructor(scene){
    this.s = scene; this.anim = !calme(); this.etiquette = null; this.dernier = null;
    this.compte = {vues: new Map(), langue: null, t: 0, fini: false};   // the stars counted, in this sitting
  }
  // what the finger is on: {quoi, im, m} (m: the word, null without its recording), or null
  cible(wx, wy){
    const s = this.s, r = s.villageHD || {}, pl = s.place, x = Math.floor(wx / CASE), y = Math.floor(wy / CASE);
    if ((s.pnj || []).some(q => q.x === x && (q.y === y || q.y === y + 1))) return null;   // a villager standing there: him first
    // a house: its drawing (roof, walls, sign: maisonToucheeHD), its tiles or its door always lead to its door
    if ((typeof maisonToucheeHD === "function" && maisonToucheeHD(s, wx, wy)) || (s.carte.maison[y] && s.carte.maison[y][x])
      || s.carte.portes.some(p => p.x === x && p.y === y)) return null;
    const pres = (im, marge) => !!im && im.active && im.visible
      && Phaser.Geom.Rectangle.Contains(Phaser.Geom.Rectangle.Inflate(im.getBounds(), marge, marge), wx, wy);
    const mot = (cle, langue) => typeof motDuJeu === "function" ? motDuJeu(cle, langue) : null;
    const rendu = id => !!pl && pl.rendus.has(id);
    if (rendu("observatoire")) {   // the stars: their halo is as wide as a finger
      const e = pl.objets.etoiles.find(o => o.active && o.frame && o.frame.name === "eclat" && Math.hypot(o.x - wx, o.y - wy) <= 6);
      if (e) return {quoi: "etoile", im: e};
    }
    if (rendu("duche")) {
      const o = pl.objets.oiseaux.find(o => pres(o, 3));
      if (o) return {quoi: "oiseau", im: o, m: mot("oiseau", "lb")};
    }
    const b = (r.papillons || []).find(p => pres(p.im, 4));
    if (b) return {quoi: "papillon", im: b.im, m: mot("papillon", langueDeMaison(b.region))};
    if (r.eau && pres(r.eau.canard, 3)) return {quoi: "canard", im: r.eau.canard, m: mot("canard", LANGUE_MAISON.albion)};
    if (rendu("albion")) {
      const f = pl.objets.fontaine.find(o => o.texture && o.texture.key === "pl_fontaine" && sousLeDoigt(s, o, wx, wy));
      if (f) return {quoi: "fontaine", im: f, m: mot("eau", LANGUE_MAISON.albion)};
    }
    return null;
  }
  // a touch (js/jeu/monde.js): for a little animal or a gift, what it was and whether a word was said (the game
  // journal), else null
  touche(wx, wy){
    const t = this.cible(wx, wy);
    if (!t) return null;
    effetHD(this.s, "toucher", wx, wy, 10, 9600);   // every touch gets an answer
    let dit;
    if (t.quoi === "etoile") dit = this.compter(t.im);
    else { this.bouger(t); dit = this.nommer(t.quoi, t.m, t.im); }
    return {quoi: t.quoi, dit: !!dit};
  }
  // the word above the drawing, said by its recording (its meaning for the little one, the first times); `voix`: what
  // to say instead (the stars' numbers). Nothing said over a villager telling something, nor over a mission announced
  nommer(quoi, m, im, sous = m && m.sens, voix){
    if (!m) return false;
    const libre = !(typeof habitantParle === "function" && habitantParle()) && !document.querySelector(".annonce-mission");
    this.ecrire(im, m.mot, sous);
    this.dernier = {quoi, mot: m.mot, langue: m.langue, sens: m.sens, sous, dit: libre};
    if (libre) (voix || (() => direMot(m)))();
    return libre;
  }
  // the word in bold over the drawing, its meaning in small under it; under the drawing when the top of the view is
  // too near (the top bar), never cut by an edge; it goes by itself
  ecrire(im, mot, sous){
    const s = this.s, v = s.cameras.main.worldView, b = im.getBounds();
    if (this.etiquette) this.etiquette.forEach(t => { s.tweens.killTweensOf(t); t.destroy(); });
    const style = (taille, couleur) => ({fontFamily: "Fredoka, sans-serif", fontSize: taille + "px", color: couleur, fontStyle: "bold",
      stroke: "#1D1640", strokeThickness: 2, padding: {x: 1, y: 1}});
    const l = [s.add.text(0, 0, mot, style(7, "#FFFFFF"))];
    if (sous) l.push(s.add.text(0, 0, sous, style(4.5, "#FFE9A8")));
    l.forEach(t => t.setOrigin(.5, 0).setResolution(12).setDepth(9997));
    const pas = t => t.height * .72;   // the meaning just under the word (a text's box is taller than its letters)
    const h = l.reduce((a, t) => a + pas(t), 0), w = Math.max(...l.map(t => t.width));
    let y = b.top - 9 - h;   // above where a butterfly flutters up to
    if (y < v.y + 22) y = b.bottom + 1;   // the top bar covers the top of the view
    const x = Math.max(v.x + w / 2 + 2, Math.min(v.right - w / 2 - 2, b.centerX));
    l.forEach(t => { t.setPosition(x, y); y += pas(t); });
    this.etiquette = l;
    if (this.anim) l.forEach(t => { t.setAlpha(0); s.tweens.add({targets: t, alpha: 1, y: t.y - 3, duration: 200, hold: ETIQUETTE_BETE_MS, yoyo: true, onComplete: () => t.destroy()}); });
    else s.time.delayedCall(ETIQUETTE_BETE_MS, () => l.forEach(t => t.destroy()));
  }
  // each one answers in its own way (nothing moves when the child asks for calm)
  bouger({quoi, im}){
    const s = this.s, entre = (a, c) => a + Math.random() * (c - a);
    if (typeof jouerSon === "function") jouerSon({papillon: "scintille", canard: "tic", oiseau: "joie", fontaine: "tic"}[quoi] || "tic");
    if (!this.anim || im.envol) return;
    if (quoi === "papillon") {   // it flutters up, then flies about again
      im.envol = true; if (im.vol) im.vol.stop();
      s.tweens.add({targets: im, y: im.y - 16, x: im.x + entre(-10, 10), duration: 420, hold: 500, yoyo: true, ease: "Sine.easeOut",
        onComplete: () => { im.envol = false; if (im.voler) im.voler(); }});
    } else if (quoi === "canard") {   // it dives and comes back up, ripples where it went
      im.envol = true; const k = im.scaleY;
      s.tweens.add({targets: im, scaleY: k * .25, alpha: .35, duration: 160, hold: 520, yoyo: true, ease: "Quad.easeIn", onComplete: () => { im.envol = false; im.setScale(im.scaleX, k).setAlpha(1); }});
      for (let i = 0; i < 2; i++) {
        const r = VILLAGE_HD.poser(s, "reflet", im.x + (i ? 4 : -4), im.y - 1, 7, {echelle: .9}); if (!r) continue;
        r.setAlpha(0); s.tweens.add({targets: r, alpha: .9, scale: r.scale * 1.6, duration: 400, delay: 120 + i * 140, yoyo: true, onComplete: () => r.destroy()});
      }
    } else if (quoi === "oiseau") {   // it flies off and lands back where it was (js/jeu/arbre_monde.js waits for it)
      s.tweens.getTweensOf(im).forEach(t => { if (typeof t.complete === "function") t.complete(); });
      im.envol = true; const x0 = im.x, y0 = im.y, d = im.flipX ? -1 : 1;
      s.tweens.add({targets: im, x: x0 + 12 * d, y: y0 - 24, duration: 380, hold: 700, yoyo: true, ease: "Sine.easeOut",
        onComplete: () => { im.envol = false; im.setPosition(x0, y0); }});
    } else if (quoi === "fontaine") {   // a splash of drops from the bowl
      for (let i = 0; i < 6; i++) {
        const dx = (i - 2.5) * 5, g = poserPlace(s, "goutte_eau", im.x, im.y - 32, im.depth + .1, 1.1); if (!g) continue;
        s.tweens.add({targets: g, x: im.x + dx * 1.6, duration: 700, ease: "Linear"});
        s.tweens.add({targets: g, y: im.y - 46, duration: 260, ease: "Quad.easeOut", onComplete: () =>
          s.tweens.add({targets: g, y: im.y - 12, alpha: .3, duration: 440, ease: "Quad.easeIn", onComplete: () => g.destroy()})});
      }
    }
  }
  // the Observatoire's stars counted one by one in the day's language; the sixth ends the count
  compter(im){
    const s = this.s, c = this.compte, t = performance.now();
    if (c.fini || t - c.t > 10000) { c.vues.clear(); c.langue = null; c.fini = false; }
    c.t = t; c.langue = c.langue || langueDuJour();
    let n = c.vues.get(im);
    const nouveau = !n;
    if (nouveau) { n = c.vues.size + 1; c.vues.set(im, n); }
    const m = typeof motDuJeu === "function" ? motDuJeu(NOMBRES_ETOILES[n - 1], c.langue) : null;
    if (this.anim && !im.saute) {
      im.saute = true; const k = im.scale;
      s.tweens.add({targets: im, scale: k * 1.5, duration: 140, yoyo: true, ease: "Quad.easeOut", onComplete: () => { im.saute = false; im.setScale(k); }});
    }
    if (typeof jouerSon === "function") jouerSon("scintille");
    const fin = nouveau && n === NOMBRES_ETOILES.length;
    if (fin) {
      c.fini = true;
      if (this.anim && s.place && s.place.arbrePied) effetHD(s, "evolution", s.place.arbrePied.x, s.place.arbrePied.y - 60, 30, 9600);
    }
    return this.nommer("etoile", m, im, String(n), m && (() => fin ? suiteDeVoix([() => dire(m.mot, m.langue), () => dire("Six étoiles, bravo !")]) : dire(m.mot, m.langue)));
  }
}
