/* Académia : la scène en 3D de la coupole de l'Observatoire (three.js, vendor/three, chargé à l'entrée seulement).
   Deux vues, sur la même toile :
   1. « planetes » : le Soleil et ses huit planètes qui tournent ; le doigt qui glisse fait tourner le regard, une
      planète touchée appelle surPlanete (js/coupole.js dit son nom et ce qu'elle a de spécial). Tailles et distances
      ne sont pas à l'échelle : tout doit se voir et se toucher.
   2. « terre » : la Terre penchée, éclairée d'un côté par le Soleil, une épingle au Luxembourg, la Lune autour ; le
      doigt qui glisse fait tourner la Terre, et surJourNuit dit quand l'épingle passe du jour à la nuit.
   Scène légère pour la tablette : peu de triangles, pas d'ombres portées, textures dessinées ici (la Terre vient de
   outils/coupole/scripts/terre.py). En mode calme (js/outils.js) rien ne tourne tout seul. */
const PLANETES = [   // name, radius, distance to the Sun, seconds for one turn, colours, rings
  {id: "mercure", nom: "Mercure", r: 1.0, d: 9, an: 14, c: ["#9C8F86", "#7D726B"]},
  {id: "venus", nom: "Vénus", r: 1.6, d: 13, an: 22, c: ["#E8C27A", "#D9A95B"]},
  {id: "terre", nom: "la Terre", r: 1.7, d: 18, an: 30, carte: true},
  {id: "mars", nom: "Mars", r: 1.3, d: 23, an: 42, c: ["#D4603A", "#B84A2A"]},
  {id: "jupiter", nom: "Jupiter", r: 4.0, d: 32, an: 70, c: ["#E3B98A", "#C98F5E", "#F0D7B4", "#B9774C"], bandes: true},
  {id: "saturne", nom: "Saturne", r: 3.3, d: 43, an: 95, c: ["#EBD49A", "#D8BA78", "#F3E3B8"], bandes: true, anneaux: true},
  {id: "uranus", nom: "Uranus", r: 2.4, d: 52, an: 120, c: ["#9FE3E6", "#86D3D8"]},
  {id: "neptune", nom: "Neptune", r: 2.3, d: 60, an: 150, c: ["#4C74E0", "#3A5CC4"]},
];
const LUXEMBOURG = [49.6, 6.1], PENCHE = 23.4 * Math.PI / 180, RAYON_TERRE = 12;

function chargerThree(){
  if (window.THREE) return Promise.resolve();
  return new Promise((ok, rate) => {
    const s = document.createElement("script");
    s.src = "vendor/three/three.min.js"; s.onload = () => ok(); s.onerror = () => rate(new Error("three.js absent"));
    document.head.append(s);
  });
}

// a planet's skin, drawn once: bands (the giants) or a plain colour with a few spots
function peauPlanete(p){
  const c = document.createElement("canvas"); c.width = 256; c.height = 128;
  const x = c.getContext("2d");
  x.fillStyle = p.c[0]; x.fillRect(0, 0, 256, 128);
  if (p.bandes) for (let k = 0; k < 9; k++) { x.fillStyle = p.c[k % p.c.length]; x.fillRect(0, k * 128 / 9, 256, 128 / 9 + 1); }
  else for (let k = 0; k < 14; k++) {
    x.fillStyle = p.c[1]; x.globalAlpha = .55;
    x.beginPath(); x.ellipse((k * 67) % 256, 14 + (k * 41) % 100, 10 + (k * 7) % 16, 5 + (k * 5) % 9, 0, 0, 7); x.fill();
  }
  const t = new THREE.CanvasTexture(c); t.colorSpace = THREE.SRGBColorSpace;
  return t;
}
// a place of the map on a sphere of three.js (the same folding as SphereGeometry)
function surSphere(lat, lon, r){
  const phi = (lon + 180) / 360 * 2 * Math.PI, theta = (90 - lat) / 180 * Math.PI;
  return new THREE.Vector3(-r * Math.cos(phi) * Math.sin(theta), r * Math.cos(theta), r * Math.sin(phi) * Math.sin(theta));
}

const C3 = {
  pret: false, vueCourante: null, surPlanete: null, surJourNuit: null, apresImage: null, jour: null,
  ouvrir(toile){
    if (!this.pret) this.construire(toile);
    this.toile = toile; this.actif = true; this.t0 = performance.now();
    this.tailler();
    if (!this.boucle) { this.boucle = () => { if (!this.actif) return; this.image(); requestAnimationFrame(this.boucle); }; }
    requestAnimationFrame(this.boucle);
  },
  fermer(){ this.actif = false; },
  construire(toile){
    const r = this.rendu = new THREE.WebGLRenderer({canvas: toile, antialias: true, powerPreference: "low-power"});
    r.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
    r.setClearColor(0x0B0A2A);
    this.camera = new THREE.PerspectiveCamera(45, 1, .5, 2000);
    this.scene = new THREE.Scene();
    // the stars, far away, shared by the two views
    const pts = [];
    const hasard = n => { const x = Math.sin(n * 12.9898) * 43758.5453; return x - Math.floor(x); };   // always the same sky
    for (let k = 0; k < 700; k++) { const v = new THREE.Vector3().setFromSphericalCoords(600, Math.acos(2 * hasard(k + 1) - 1), 2 * Math.PI * hasard(k + 1000.5)); pts.push(v.x, v.y, v.z); }
    const g = new THREE.BufferGeometry(); g.setAttribute("position", new THREE.Float32BufferAttribute(pts, 3));
    this.scene.add(new THREE.Points(g, new THREE.PointsMaterial({color: 0xFFFFFF, size: 2.2, sizeAttenuation: false})));
    const carte = new THREE.TextureLoader().load(`${DOSSIER_HD}/coupole/terre.png`); carte.colorSpace = THREE.SRGBColorSpace;
    this.systeme = this.faireSysteme(carte); this.globe = this.faireGlobe(carte);
    this.scene.add(this.systeme.groupe, this.globe.groupe);
    this.azimut = .5; this.hauteur = .8; this.tour = -1.15;   // Luxembourg faces the child, in the day
    this.gestes(toile);
    window.addEventListener("resize", () => this.tailler());
    this.pret = true;
  },
  faireSysteme(carte){
    const groupe = new THREE.Group(), corps = {};
    groupe.add(new THREE.Mesh(new THREE.SphereGeometry(5.2, 32, 20), new THREE.MeshBasicMaterial({color: 0xFFC83A})));
    const halo = new THREE.Mesh(new THREE.SphereGeometry(6.4, 24, 16), new THREE.MeshBasicMaterial({color: 0xFFE28A, transparent: true, opacity: .28}));
    groupe.add(halo, new THREE.PointLight(0xFFFFFF, 2.4, 0, 0), new THREE.AmbientLight(0xFFFFFF, .6));
    PLANETES.forEach((p, i) => {
      const m = new THREE.Mesh(new THREE.SphereGeometry(p.r, 28, 18), new THREE.MeshLambertMaterial({map: p.carte ? carte : peauPlanete(p)}));
      if (p.anneaux) {
        const a = new THREE.Mesh(new THREE.RingGeometry(p.r * 1.35, p.r * 2.1, 48), new THREE.MeshBasicMaterial({color: 0xE6CF95, side: THREE.DoubleSide, transparent: true, opacity: .8}));
        a.rotation.x = Math.PI / 2 - .35; m.add(a);
      }
      const o = new THREE.Mesh(new THREE.RingGeometry(p.d - .07, p.d + .07, 96), new THREE.MeshBasicMaterial({color: 0xFFFFFF, side: THREE.DoubleSide, transparent: true, opacity: .22}));
      o.rotation.x = Math.PI / 2;
      groupe.add(m, o);
      corps[p.id] = {p, m, depart: i * 2.2 + .6};   // spread around the Sun, so that each one shows when nothing moves
    });
    return {groupe, corps};
  },
  faireGlobe(carte){
    const groupe = new THREE.Group(), axe = new THREE.Group(), terre = new THREE.Group();
    axe.rotation.z = PENCHE; axe.add(terre); groupe.add(axe);
    terre.add(new THREE.Mesh(new THREE.SphereGeometry(RAYON_TERRE, 48, 32), new THREE.MeshLambertMaterial({map: carte})));
    const pin = new THREE.Mesh(new THREE.SphereGeometry(.55, 12, 8), new THREE.MeshBasicMaterial({color: 0xFF3B4E}));
    pin.position.copy(surSphere(LUXEMBOURG[0], LUXEMBOURG[1], RAYON_TERRE + .35)); terre.add(pin);
    const soleil = new THREE.DirectionalLight(0xFFFFFF, 2.6); soleil.position.set(1, 0, .25);
    const astre = new THREE.Mesh(new THREE.SphereGeometry(9, 24, 16), new THREE.MeshBasicMaterial({color: 0xFFC83A})); astre.position.set(120, 0, 30);
    const lune = new THREE.Mesh(new THREE.SphereGeometry(2.6, 20, 14), new THREE.MeshLambertMaterial({color: 0xD9D9E3}));
    groupe.add(soleil, astre, lune, new THREE.AmbientLight(0x8FA8FF, .4));
    return {groupe, terre, pin, lune, versSoleil: new THREE.Vector3(1, 0, .25).normalize()};
  },
  vue(nom){
    this.vueCourante = nom; this.jour = null;
    this.systeme.groupe.visible = nom === "planetes"; this.globe.groupe.visible = nom === "terre";
  },
  tailler(){
    const t = this.toile; if (!t || !this.rendu) return;
    const w = t.clientWidth || 1, h = t.clientHeight || 1;
    this.rendu.setSize(w, h, false); this.camera.aspect = w / h; this.camera.updateProjectionMatrix();
  },
  // one frame: the planets on their rounds, the camera where the finger left it
  image(){
    const s = calme() ? 0 : (performance.now() - this.t0) / 1000, cam = this.camera;
    if (this.vueCourante === "planetes") {
      Object.values(this.systeme.corps).forEach(c => {
        const a = c.depart + s * 2 * Math.PI / c.p.an;
        c.m.position.set(Math.cos(a) * c.p.d, 0, Math.sin(a) * c.p.d); c.m.rotation.y = s * .4;
      });
      const large = cam.aspect < 1.2 ? 190 : 138;   // a narrow screen steps back: Neptune stays in sight
      cam.position.set(Math.sin(this.azimut) * Math.cos(this.hauteur) * large, Math.sin(this.hauteur) * large, Math.cos(this.azimut) * Math.cos(this.hauteur) * large);
      cam.lookAt(0, -16, 0);   // the Sun a little above the middle: the Sage's bubble is at the bottom
    } else {
      const g = this.globe;
      g.terre.rotation.y = this.tour + s * .12;
      g.lune.position.set(Math.cos(s * .25 + 2) * 24, 3, Math.sin(s * .25 + 2) * 24);
      const loin = cam.aspect < 1.2 ? 1.4 : 1; cam.position.set(0, 34 * loin, 44 * loin); cam.lookAt(0, -4, 0);   // from above the north: Europe in sight
      const n = g.pin.getWorldPosition(new THREE.Vector3()).normalize().dot(g.versSoleil), jour = n > .12 ? true : n < -.12 ? false : this.jour;
      if (jour !== this.jour) { const avant = this.jour; this.jour = jour; if (this.surJourNuit) this.surJourNuit(jour, avant === null); }
    }
    this.rendu.render(this.scene, cam);
    if (this.apresImage) this.apresImage();
  },
  // where a planet (or "luxembourg") is on the screen, in CSS pixels of the page; null when it is behind
  ecranDe(id){
    const o = id === "luxembourg" ? this.globe.pin : (this.systeme.corps[id] || {}).m; if (!o) return null;
    const v = o.getWorldPosition(new THREE.Vector3()).project(this.camera), b = this.toile.getBoundingClientRect();
    if (v.z > 1) return null;
    return [b.left + (v.x + 1) / 2 * b.width, b.top + (1 - v.y) / 2 * b.height];
  },
  // the planet under a finger: the nearest one on screen, as wide as a finger at least
  planeteSous(x, y){
    let mieux = null, dmin = 1e9;
    Object.keys(this.systeme.corps).forEach(id => {
      const e = this.ecranDe(id); if (!e) return;
      const d = Math.hypot(e[0] - x, e[1] - y);
      if (d < dmin) { dmin = d; mieux = id; }
    });
    return dmin <= 44 ? mieux : null;
  },
  gestes(toile){
    let depart = null;
    toile.addEventListener("pointerdown", e => { depart = {x: e.clientX, y: e.clientY, a: this.azimut, h: this.hauteur, t: this.tour, bouge: false}; toile.setPointerCapture(e.pointerId); });
    toile.addEventListener("pointermove", e => {
      if (!depart) return;
      const dx = e.clientX - depart.x, dy = e.clientY - depart.y;
      if (Math.hypot(dx, dy) > 8) depart.bouge = true;
      if (!depart.bouge) return;
      if (this.vueCourante === "planetes") { this.azimut = depart.a - dx * .008; this.hauteur = Math.max(.18, Math.min(1.35, depart.h + dy * .006)); }
      else this.tour = depart.t + dx * .012;
    });
    const fin = e => {
      const d = depart; depart = null;
      if (!d || d.bouge || this.vueCourante !== "planetes") return;
      const id = this.planeteSous(e.clientX, e.clientY);
      if (id && this.surPlanete) this.surPlanete(this.systeme.corps[id].p);
    };
    toile.addEventListener("pointerup", fin);
    toile.addEventListener("pointercancel", () => { depart = null; });
  },
};
