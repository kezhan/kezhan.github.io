/* Académia : ce qui guide l'enfant dans le village (js/jeu/monde.js).
   1. Tant qu'aucun combat n'a eu lieu, une flèche dorée rebondit au-dessus de l'Ombre la plus proche ; hors de vue,
      elle attend au bord de l'écran, tournée vers elle.
   2. Après quelques secondes sans toucher (9 s jusqu'à 5 ans, 15 s ensuite), une main montre la porte ouverte la plus
      proche, sinon l'Ombre la plus proche ; le compagnon sautille vers elle ; la voix du Sage dit quoi faire. Tout
      s'en va au premier toucher.
   3. Une porte vient de s'ouvrir (js/jeu/portes_village.js) : sans toucher pendant 10 s, la voix le redit.
   Pièces dessinées : outils/effets/scripts/guide.py (guide_main, guide_fleche). */
const GUIDE_ATTENTE_PETIT = 9000, GUIDE_ATTENTE = 15000, GUIDE_VOIX_ESPACE = 25000, GUIDE_RAPPEL_PORTE = 10000;

function chargerGuideHD(load, version){
  ["guide_main", "guide_fleche"].forEach(n => {
    if (!load.textureManager.exists("fx_" + n)) load.image("fx_" + n, `assets/${version === "leger" ? "leger" : "hd"}/effets/${n}.png`);
  });
}
// « La porte du Dojo des Nombres », « la porte de l'Observatoire »...
const DE_REGION = {dojo: "du ", albion: "du ", germania: "de la ", duche: "de la ", jardin: "du ", observatoire: "de l'"};
const porteDe = r => "la porte " + (DE_REGION[r.id] || "de ") + r.nom;
const phrasePorte = r => { const t = porteDe(r); return t[0].toUpperCase() + t.slice(1) + " est ouverte ! Touche la maison !"; };

class GuideVillage {
  constructor(scene){
    this.s = scene; this.dernier = performance.now(); this.voixA = -1e9; this.rappel = null; this.mainLa = false;
    const piece = (k, ox, oy, h) => {
      if (!scene.textures.exists(k)) return null;
      const i = scene.add.image(0, 0, k).setOrigin(ox, oy).setDepth(10100).setVisible(false);   // above the signs
      i.setScale(h / i.height); return i;
    };
    this.fleche = piece("fx_guide_fleche", .5, .5, 13);   // heights in world units (a tile is 16)
    this.main = piece("fx_guide_main", .37, .09, 21);      // origin at the fingertip
    this.tapes = 0;
  }
  attente(){ return P() && P().age <= 5 ? GUIDE_ATTENTE_PETIT : GUIDE_ATTENTE; }
  // any touch or key: the hand goes, the waiting starts again
  toucher(){ this.dernier = performance.now(); this.cacherMain(); }
  retour(){ this.dernier = performance.now(); this.cacherMain(); }
  rappelPorte(region){ this.rappel = {region, quand: performance.now() + GUIDE_RAPPEL_PORTE, depuis: performance.now()}; }

  // what the child should touch now: the nearest open door, else the nearest Ombre ({x, y} in the world)
  cible(porteSeulement){
    const s = this.s, h = s.case;
    const portes = s.portes ? s.portes.ouvertes() : [];
    if (portes.length) {
      const p = portes.reduce((a, b) => Math.abs(b.tx - h.x) + Math.abs(b.ty - h.y) < Math.abs(a.tx - h.x) + Math.abs(a.ty - h.y) ? b : a);
      return {x: p.x, y: p.y, porte: p.region};
    }
    if (porteSeulement) return null;
    const v = this.vue(), dansLaVue = o => o.s.x > v.x0 && o.s.x < v.x1 && o.s.y - o.s.displayHeight > v.y0 && o.s.y < v.y1;
    const o = s.ombres.proche(h.x, h.y, dansLaVue) || s.ombres.proche(h.x, h.y);   // one in sight first
    return o && o.s.active ? {x: o.s.x, y: o.s.y - o.s.displayHeight / 2, haut: o.s.y - o.s.displayHeight, ombre: o} : null;
  }
  // the visible part of the world, without the top bar's band, in world units
  vue(){
    const cam = this.s.cameras.main, v = cam.worldView, k = cam.zoom / RATIO, m = 14 / k, barre = $("barre").hidden ? m : 74 / k;
    return {x0: v.x + m, x1: v.right - m, y0: v.y + barre, y1: v.bottom - m};
  }
  dansLaVue(c){ const v = this.vue(); return c.x > v.x0 && c.x < v.x1 && c.y > v.y0 && c.y < v.y1; }
  // the arrow: above the target when it is in sight (bouncing), else at the edge of the screen, turned toward it
  poserFleche(c, t){
    const f = this.fleche; if (!f) return;
    const v = this.vue(), saut = calme() ? 0 : Math.abs(Math.sin(t / 230)) * 3.5;
    if (this.dansLaVue(c)) return f.setPosition(c.x, (c.haut != null ? c.haut : c.y - 8) - 9 - saut).setAngle(0).setVisible(true);
    const x = Math.min(v.x1 - 7, Math.max(v.x0 + 7, c.x)), y = Math.min(v.y1 - 7, Math.max(v.y0 + 7, c.y));
    const a = Math.atan2(c.y - y, c.x - x);
    f.setPosition(x - Math.cos(a) * saut, y - Math.sin(a) * saut).setRotation(a - Math.PI / 2).setVisible(true);
  }
  // the hand shows the target (update keeps it on it); the companion hops toward it; the Sage's voice says what to do
  montrerMain(c){
    const s = this.s, k = s.compagnon;
    this.mainLa = true; this.cibleMain = c;
    if (k && k.active && !calme()) {
      k.setFlipX(c.x < k.x);
      s.tweens.add({targets: k, y: k.y - 5, duration: 150, yoyo: true, repeat: 2, ease: "Quad.easeOut"});
    }
    const maintenant = performance.now();
    if (maintenant - this.voixA > GUIDE_VOIX_ESPACE) {
      this.voixA = maintenant;
      dire(c.porte ? phrasePorte(regionDe(c.porte)) : "Touche une Ombre violette !");
    }
  }
  cacherMain(){
    this.mainLa = false; this.cibleMain = null;
    if (this.main) this.main.setVisible(false);
  }
  update(t){
    const s = this.s, ici = ecranCourant === "monde" && !s.verrou && !s.moment;
    const maintenant = performance.now(), p = P();
    // a door opened a while ago and nobody touched since: say it again, and show it
    if (this.rappel && maintenant > this.rappel.quand) {
      const r = this.rappel; this.rappel = null;
      if (this.dernier <= r.depuis && ici && !dialogueOuvert() && regionEtat(r.region).etape >= ETAPES) {
        this.voixA = -1e9; const c = this.cible(true); if (c) this.montrerMain(c);
      }
    }
    // the hand after a while without touching
    if (!this.mainLa && ici && !s.occupe() && !s.enMarche && maintenant - this.dernier > this.attente()) {
      const c = this.cible(); if (c) this.montrerMain(c);
    }
    let fleche = null;
    if (this.mainLa && this.cibleMain) {
      const c = this.cibleMain.ombre ? this.cible() : this.cibleMain;   // an Ombre moves: the hand follows the nearest
      if (!c || !ici) this.cacherMain();
      else if (this.main && this.dansLaVue(c)) {   // the finger taps on it
        const tape = calme() ? 0 : (Math.sin(t / 260) + 1) / 2;
        this.main.setVisible(true).setPosition(c.x + 1, c.y + 2 + tape * 4).setScale((21 / this.main.height) * (1 - .06 * (1 - tape)));
        const n = Math.floor(t / (260 * 2 * Math.PI));
        if (!calme() && n !== this.tapes) { this.tapes = n; effetHD(s, "toucher", c.x, c.y, CASE * .8, 10090); }
      } else {   // out of sight: the arrow at the edge shows the way
        if (this.main) this.main.setVisible(false);
        fleche = c;
      }
    }
    // before the first fight, the arrow over the nearest Ombre
    if (!fleche && !this.mainLa && ici && p && !(p.hist || []).length) fleche = this.cible();
    if (fleche) this.poserFleche(fleche, t); else if (this.fleche) this.fleche.setVisible(false);
  }
}
