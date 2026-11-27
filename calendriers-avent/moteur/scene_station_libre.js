/* The free-form station: a trial beside scene_station.js (23-11), the same doors, four different places.
 *
 * An advent calendar: 24 doors, opened one a day. Round a hub with its fir and its turning ring habitat, each
 * part of the calendar is its own kind of place, chosen from its theme (the author, 23-11: « essaie de trouver
 * des espaces assez differnts »). Nothing of a day's idea shows outside, only its number:
 *
 *   part 1  Words           the arrival port: a docking ring, six airlock pods round it like petals, dishes
 *                           that listen, capsules queuing on approach with the requests
 *   part 2  Proof           the test lab: an open scaffold of cells, labs in some, a robot arm, a test chamber
 *                           with its red and green lights, a gauge, a camera that takes the screenshot
 *   part 3  Memory          the asteroid: vault doors set in the rock, mining tunnels, a drill, ice, a conveyor
 *                           of crates to the hub, the dish that sends the log down to Earth
 *   part 4  Orchestration   the fleet: a command ship, six small ships of different shapes in formation, tied
 *                           to it by beams, pods shuttling between them
 *
 * What a reader sees:
 *   - a locked day: its hatch shut, a red lamp, its number on a dashed tag;
 *   - a day that may open: an amber lamp, a warm tag that pulses;
 *   - a click opens the hatch (the door swings out, the inside lights) and goes in: the day's practice plays as
 *     an animation in a few stops (`station_interieur.js`, one file per day `station_jNN.js`, contract in
 *     `STATION.md`); an open module keeps its door open and its portholes warm;
 *   - on the 24th, all hatches open, the station shines like a star.
 *
 * Same calls as the other scenes (see apercu_du_monde.js), and `inside(day, stop)`. Needs three.js r128 (global
 * `THREE`); every texture is drawn on a canvas.
 */
(function (global) {
  'use strict';

  var TAU = Math.PI * 2;
  /* the station, in metres: a hub, four wings of six slots, a landmark at each end */
  var HUB = 2.4, SLOT = 3.1, R0 = 3.6, MOD_L = 2.85, MOD_R = 1.05, TIP = R0 + 6 * SLOT + 1.6;
  var SPINE_Y = -1.6;
  /* the Earth, far below; the station flies over its morning side */
  var EARTH_R = 1500, EARTH_Y = -62 - EARTH_R;
  /* the wings, clockwise from the back left: words, proof, memory, orchestration */
  var ANGLES = [212, 306, 38, 142].map(function (a) { return a * Math.PI / 180; });

  var C = {white: 0xe9edf3, whiteD: 0xbfc6d2, steel: 0x9aa3b5, steelD: 0x4f5668, dark: 0x1c2030, gold: 0xd9a441,
           cell: 0x1d3a7a, green: 0x3f9a62, red: 0xd9483a, glass: 0x2d3550, suit: 0xf3f3ef};

  var SITE = {
    over: {en: 'The station', fr: 'La station'},
    main: {en: 'main', fr: 'main'},
    signs: [{en: 'REQUESTS IN', fr: 'ARRIVÉE DES DEMANDES'}, {en: 'TEST LAB', fr: "LABO D'ESSAIS"},
            {en: 'ARCHIVE & LOG', fr: 'ARCHIVES & JOURNAL'}, {en: 'MISSION CONTROL', fr: 'CONTRÔLE DE MISSION'}],
    rule: null
  };

  function mount(host, data, page) {
    var THREE = global.THREE;
    if (!THREE || !host || !data.world) return null;
    if (data.world.site) Object.keys(data.world.site).forEach(function (k) { SITE[k] = data.world.site[k]; });       /* a world may name its own signs */
    var reduced = global.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;
    var renderer;
    try {
      renderer = new THREE.WebGLRenderer({antialias: true, powerPreference: 'high-performance'});
    } catch (e) { return null; }
    renderer.setPixelRatio(Math.min(2, global.devicePixelRatio || 1));
    renderer.outputEncoding = THREE.sRGBEncoding;
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    renderer.shadowMap.autoUpdate = false;
    var canvas = renderer.domElement;
    canvas.className = 'monde-canvas';
    canvas.setAttribute('aria-hidden', 'true');
    host.appendChild(canvas);

    var scene = new THREE.Scene();
    scene.background = new THREE.Color(0x03050d);
    var camera = new THREE.PerspectiveCamera(32, 2, 0.5, 5000);
    var site = new THREE.Group();
    scene.add(site);

    /* --- light: a low sun rising on the left, the blue light of the Earth from below ------------------ */

    scene.add(new THREE.HemisphereLight(0xcfd8ff, 0x24508f, 0.62));
    var sun = new THREE.DirectionalLight(0xffd2a8, 1.25);
    sun.position.set(40, 16, -38);
    sun.target.position.set(0, 0, 0);
    sun.castShadow = true;
    sun.shadow.mapSize.set(2048, 2048);
    sun.shadow.camera.left = -32; sun.shadow.camera.right = 32;
    sun.shadow.camera.top = 32; sun.shadow.camera.bottom = -32;
    sun.shadow.camera.near = 1; sun.shadow.camera.far = 140;
    sun.shadow.bias = -0.0005; sun.shadow.normalBias = 0.03;
    scene.add(sun); scene.add(sun.target);
    var fill = new THREE.DirectionalLight(0x9fc2ff, 0.35);
    fill.position.set(10, 20, 40);
    scene.add(fill);
    var warmth = new THREE.PointLight(0xffc27a, 0, 40, 2);
    warmth.position.set(0, 6, 4);
    scene.add(warmth);

    /* --- small helpers --------------------------------------------------------------- */

    var seed = 20261201;
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
    function ease(x) { x = clamp(x, 0, 1); return x < 0.5 ? 4 * x * x * x : 1 - Math.pow(-2 * x + 2, 3) / 2; }
    function step(a, b, x) { var t = clamp((x - a) / (b - a), 0, 1); return t * t * (3 - 2 * t); }
    function css(hex) { return '#' + ('000000' + hex.toString(16)).slice(-6); }
    var UP = new THREE.Vector3(0, 1, 0);

    var mats = {};
    function mat(color, o) {
      o = o || {};
      var key = o.own ? '' : [color, o.rough, o.metal, o.emissive, o.ei].join('|');
      if (key && mats[key]) return mats[key];
      var m = new THREE.MeshStandardMaterial({color: color, flatShading: true, roughness: o.rough == null ? 0.6 : o.rough,
        metalness: o.metal || 0, emissive: o.emissive || 0x000000, emissiveIntensity: o.ei == null ? 1 : o.ei});
      if (key) mats[key] = m;
      return m;
    }
    function put(p, geo, m, x, y, z, o) {
      var me = new THREE.Mesh(geo, m);
      me.position.set(x || 0, y || 0, z || 0);
      o = o || {};
      if (o.rx) me.rotation.x = o.rx;
      if (o.ry) me.rotation.y = o.ry;
      if (o.rz) me.rotation.z = o.rz;
      me.receiveShadow = o.shadow !== false;
      me.castShadow = o.shadow !== false && o.cast !== false;
      p.add(me);
      return me;
    }
    /* centred shapes: in space nothing stands on a floor */
    function box(p, w, h, d, m, x, y, z, o) { return put(p, new THREE.BoxGeometry(w, h, d), m, x, y, z, o); }
    function ball(p, r, m, x, y, z, o) { return put(p, new THREE.SphereGeometry(r, 18, 12), m, x, y, z, o); }
    /* a cylinder lying along x, from x0 to x1 */
    function tubeX(p, r, x0, x1, m, y, z, seg) {
      return put(p, new THREE.CylinderGeometry(r, r, x1 - x0, seg || 18), m, (x0 + x1) / 2, y || 0, z || 0, {rz: Math.PI / 2});
    }
    function rod(p, a, b, r, m) {
      var va = new THREE.Vector3(a[0], a[1], a[2]), vb = new THREE.Vector3(b[0], b[1], b[2]);
      var me = new THREE.Mesh(new THREE.CylinderGeometry(r, r, va.distanceTo(vb), 6), m);
      me.position.copy(va).add(vb).multiplyScalar(0.5);
      me.quaternion.setFromUnitVectors(UP, vb.clone().sub(va).normalize());
      me.castShadow = me.receiveShadow = true;
      p.add(me);
      return me;
    }
    function grp(p, x, y, z, ry) { var g = new THREE.Group(); g.position.set(x || 0, y || 0, z || 0); g.rotation.y = ry || 0; p.add(g); return g; }

    /* merge what was built under `root` into one mesh per material */
    function bake(root) {
      root.updateMatrixWorld(true);
      var inv = new THREE.Matrix4().copy(root.matrixWorld).invert();
      var sets = {}, gone = [];
      (function walk(n) {
        n.children.forEach(function (c) {
          if (c.userData.keep) return;
          if (c.isMesh && !c.material.transparent) {
            var key = c.material.uuid + (c.castShadow ? 'c' : '') + (c.receiveShadow ? 'r' : '');
            (sets[key] || (sets[key] = {m: c.material, cast: c.castShadow, recv: c.receiveShadow, list: []})).list.push(c);
            gone.push(c);
          } else if (c.isGroup) walk(c);
        });
      })(root);
      var mtx = new THREE.Matrix4(), nm = new THREE.Matrix3(), v = new THREE.Vector3();
      Object.keys(sets).forEach(function (k) {
        var s = sets[k], pos = [], nor = [];
        s.list.forEach(function (me) {
          var g = me.geometry.index ? me.geometry.toNonIndexed() : me.geometry;
          mtx.multiplyMatrices(inv, me.matrixWorld); nm.getNormalMatrix(mtx);
          var P = g.attributes.position, N = g.attributes.normal;
          for (var i = 0; i < P.count; i++) {
            v.fromBufferAttribute(P, i).applyMatrix4(mtx); pos.push(v.x, v.y, v.z);
            v.fromBufferAttribute(N, i).applyMatrix3(nm).normalize(); nor.push(v.x, v.y, v.z);
          }
        });
        var g2 = new THREE.BufferGeometry();
        g2.setAttribute('position', new THREE.Float32BufferAttribute(pos, 3));
        g2.setAttribute('normal', new THREE.Float32BufferAttribute(nor, 3));
        var out = new THREE.Mesh(g2, s.m);
        out.castShadow = s.cast; out.receiveShadow = s.recv; out.userData.keep = true;
        root.add(out);
      });
      gone.forEach(function (me) { me.parent.remove(me); me.geometry.dispose(); });
    }

    function drawn(w, h, paint) {
      var c = document.createElement('canvas'); c.width = w; c.height = h;
      paint(c.getContext('2d'), w, h);
      var t = new THREE.CanvasTexture(c);
      t.encoding = THREE.sRGBEncoding; t.anisotropy = 4;
      return t;
    }
    var glowTex = drawn(128, 128, function (g) {
      var r = g.createRadialGradient(64, 64, 0, 64, 64, 64);
      r.addColorStop(0, 'rgba(255,255,255,1)'); r.addColorStop(0.35, 'rgba(255,255,255,.45)'); r.addColorStop(1, 'rgba(255,255,255,0)');
      g.fillStyle = r; g.fillRect(0, 0, 128, 128);
    });
    function halo(p, x, y, z, color, size, o) {
      o = o || {};
      var s = new THREE.Sprite(new THREE.SpriteMaterial({map: o.map || glowTex, color: color, transparent: true, depthWrite: false,
        depthTest: o.depth !== false, blending: THREE.AdditiveBlending, opacity: 0}));
      s.position.set(x, y, z); s.scale.set(size, o.tall || size, 1); s.visible = false;
      p.add(s);
      return s;
    }
    function glowAt(s, k) { s.material.opacity = k; s.visible = k > 0.004; }

    /* every board and card faces the reader, who looks from above the front of the station */
    var FACE = new THREE.Quaternion().setFromRotationMatrix(
      new THREE.Matrix4().lookAt(new THREE.Vector3(0.05, 0.5, 0.86).normalize(), new THREE.Vector3(), UP));
    function board(p, text, x, y, z, color, width, o) {
      o = o || {};
      var h = o.h || 0.7, px = 64;
      var tex = drawn(Math.round(px * width / h), px, function (g, w, hh) {
        g.fillStyle = css(color); g.fillRect(0, 0, w, hh);
        g.strokeStyle = 'rgba(255,255,255,.8)'; g.lineWidth = 3; g.strokeRect(4, 4, w - 8, hh - 8);
        g.fillStyle = o.ink || '#ffffff'; g.font = '800 ' + Math.round(hh * 0.5) + 'px "JetBrains Mono", Consolas, monospace';
        g.textAlign = 'center'; g.textBaseline = 'middle'; g.fillText(text, w / 2, hh / 2 + 2);
      });
      var me = new THREE.Mesh(new THREE.PlaneGeometry(width, h), new THREE.MeshStandardMaterial({map: tex, roughness: 0.7,
        emissive: 0xffffff, emissiveMap: tex, emissiveIntensity: o.lit || 0.35, side: THREE.DoubleSide}));
      me.position.set(x, y, z); me.quaternion.copy(FACE);
      me.userData.keep = true;
      p.add(me);
      return me;
    }

    /* --- the sky and the Earth: stars, a sun rising on the limb, the blue morning below --------------- */

    (function () {
      var pts = [], cols = [];
      for (var i = 0; i < 1800; i++) {
        var a = rnd() * TAU, e = Math.asin(rnd() * 1.1 - 0.1), r = 3000;
        pts.push(Math.cos(e) * Math.cos(a) * r, Math.sin(e) * r, Math.cos(e) * Math.sin(a) * r);
        var c = new THREE.Color().setHSL(0.6 - rnd() * 0.5, 0.4, 0.75 + rnd() * 0.25);
        cols.push(c.r, c.g, c.b);
      }
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      scene.add(new THREE.Points(g, new THREE.PointsMaterial({size: 2.2, sizeAttenuation: false, vertexColors: true,
        transparent: true, opacity: 0.9, depthWrite: false})));
    })();
    /* the clouds: soft blobs on a tile that repeats without a seam */
    var cloudTex = drawn(1024, 1024, function (g, w, h) {
      g.clearRect(0, 0, w, h);
      for (var i = 0; i < 520; i++) {
        var x = rnd() * w, y = rnd() * h, r = 10 + Math.pow(rnd(), 2.2) * 90, a = 0.05 + rnd() * 0.16;
        for (var dx = -1; dx <= 1; dx++) for (var dy = -1; dy <= 1; dy++) {
          var gr = g.createRadialGradient(x + dx * w, y + dy * h, 0, x + dx * w, y + dy * h, r);
          gr.addColorStop(0, 'rgba(255,255,255,' + a + ')'); gr.addColorStop(1, 'rgba(255,255,255,0)');
          g.fillStyle = gr; g.fillRect(x + dx * w - r, y + dy * h - r, r * 2, r * 2);
        }
      }
    });
    cloudTex.wrapS = cloudTex.wrapT = THREE.RepeatWrapping;
    cloudTex.repeat.set(18, 9);
    var seaTex = drawn(512, 512, function (g, w, h) {
      g.fillStyle = '#0f2f66'; g.fillRect(0, 0, w, h);
      for (var i = 0; i < 160; i++) {
        var x = rnd() * w, y = rnd() * h, r = 20 + rnd() * 70;
        for (var dx = -1; dx <= 1; dx++) for (var dy = -1; dy <= 1; dy++) {
          var gr = g.createRadialGradient(x + dx * w, y + dy * h, 0, x + dx * w, y + dy * h, r);
          gr.addColorStop(0, rnd() < 0.5 ? 'rgba(18,60,120,.35)' : 'rgba(40,110,170,.3)'); gr.addColorStop(1, 'rgba(0,0,0,0)');
          g.fillStyle = gr; g.fillRect(x + dx * w - r, y + dy * h - r, r * 2, r * 2);
        }
      }
    });
    seaTex.wrapS = seaTex.wrapT = THREE.RepeatWrapping;
    seaTex.repeat.set(30, 15);
    var earth = new THREE.Mesh(new THREE.SphereGeometry(EARTH_R, 256, 128),
      new THREE.MeshStandardMaterial({map: seaTex, roughness: 0.7, metalness: 0.05, emissive: 0x030a1c, emissiveIntensity: 1}));
    earth.position.set(0, EARTH_Y, 0); earth.rotation.x = Math.PI / 2;
    scene.add(earth);
    var clouds = new THREE.Mesh(new THREE.SphereGeometry(EARTH_R + 4, 256, 128),
      new THREE.MeshStandardMaterial({map: cloudTex, transparent: true, depthWrite: false, roughness: 1, color: 0xffffff}));
    clouds.position.copy(earth.position); clouds.rotation.x = Math.PI / 2;
    scene.add(clouds);
    [[1.006, 0.5, 0x7cc4ff], [1.016, 0.22, 0x5a9cff], [1.03, 0.08, 0x4f7dff]].forEach(function (a) {
      var m = new THREE.Mesh(new THREE.SphereGeometry(EARTH_R * a[0], 128, 64), new THREE.MeshBasicMaterial({color: a[2],
        transparent: true, opacity: a[1], side: THREE.BackSide, blending: THREE.AdditiveBlending, depthWrite: false}));
      m.position.copy(earth.position);
      scene.add(m);
    });
    /* the sun, just over the limb on the left, and the warm band of the dawn along it */
    var sunDir = new THREE.Vector3(0.24, -0.17, -0.95).normalize();
    var sunAt = sunDir.clone().multiplyScalar(1450);
    glowAt(halo(scene, sunAt.x, sunAt.y, sunAt.z, 0xfff1d6, 120), 1);
    glowAt(halo(scene, sunAt.x, sunAt.y, sunAt.z, 0xffb060, 520, {depth: false}), 0.55);
    for (var bi = -3; bi <= 3; bi++) {
      var side = new THREE.Vector3(-sunDir.z, 0, sunDir.x).normalize();
      var at = sunAt.clone().addScaledVector(side, bi * 260).add(new THREE.Vector3(0, -20 - Math.abs(bi) * 22, 0));
      glowAt(halo(scene, at.x, at.y, at.z, bi === 0 ? 0xffa050 : 0xff8060, 900, {depth: false, tall: 110}), 0.7 - Math.abs(bi) * 0.14);
    }

    /* --- the frame: the hub (main), four bare wings, a landmark and its panels at each end ------------- */

    var frameG = grp(site);
    var white = mat(C.white, {rough: 0.45, metal: 0.15}), whiteD = mat(C.whiteD, {rough: 0.5, metal: 0.2});
    var steel = mat(C.steel, {rough: 0.4, metal: 0.6}), steelD = mat(C.steelD, {rough: 0.5, metal: 0.5});
    /* the hub */
    ball(frameG, HUB, white, 0, 0, 0);
    tubeX(frameG, HUB * 0.55, -HUB - 0.5, HUB + 0.5, whiteD);
    put(frameG, new THREE.CylinderGeometry(HUB * 0.55, HUB * 0.55, 2 * HUB + 1, 18), whiteD, 0, 0, 0, {rx: Math.PI / 2});
    put(frameG, new THREE.CylinderGeometry(0.9, 1.1, 0.5, 16), steelD, 0, HUB - 0.05, 0);
    put(frameG, new THREE.TorusGeometry(HUB * 1.02, 0.12, 8, 40), steelD, 0, 0, 0, {rx: Math.PI / 2});
    board(site, page.tx(SITE.main), 0, 0.5, HUB + 0.35, C.green, 2.2, {h: 0.85, lit: 0.6});
    /* life and December (station_vie.js): the fir on the hub, its star is the star of the 24th */
    var vie = global.StationVie.make({THREE: THREE, mat: mat, put: put, box: box, ball: ball, grp: grp, halo: halo, glowAt: glowAt});
    vie.tree(site, HUB + 0.15);

    var cellTex = drawn(256, 512, function (g, w, h) {
      g.fillStyle = '#16306b'; g.fillRect(0, 0, w, h);
      g.strokeStyle = 'rgba(160,190,255,.35)'; g.lineWidth = 2;
      for (var x = 0; x <= w; x += w / 4) { g.beginPath(); g.moveTo(x, 0); g.lineTo(x, h); g.stroke(); }
      for (var y = 0; y <= h; y += h / 16) { g.beginPath(); g.moveTo(0, y); g.lineTo(w, y); g.stroke(); }
      g.strokeStyle = '#d9a441'; g.lineWidth = 8; g.strokeRect(0, 0, w, h);
    });
    var cellM = new THREE.MeshStandardMaterial({map: cellTex, roughness: 0.3, metalness: 0.55, emissive: 0x2a4fb0,
      emissiveIntensity: 0.12, side: THREE.DoubleSide});
    var panels = [], blinks = [], anim = [], LINKS = [];
    var V = function (x, y, z) { return new THREE.Vector3(x, y, z); };
    var tunnelM = mat(C.whiteD, {rough: 0.5, metal: 0.2});
    function tunnel(a, b, r) {
      LINKS.push([a.clone(), b.clone()]);
      rod(site, [a.x, a.y, a.z], [b.x, b.y, b.z], r || 0.34, tunnelM);
    }
    function along(me, dir) { me.quaternion.setFromUnitVectors(UP, dir.clone().normalize()); return me; }
    /* a thin beam of light from a to b (a tether, the log sent down) */
    function beam(p, a, b, color, r) {
      var m = new THREE.MeshBasicMaterial({color: color, transparent: true, opacity: 0.5, blending: THREE.AdditiveBlending, depthWrite: false});
      var me = new THREE.Mesh(new THREE.CylinderGeometry(r || 0.05, r || 0.05, a.distanceTo(b), 6, 1, true), m);
      me.position.copy(a).add(b).multiplyScalar(0.5);
      along(me, b.clone().sub(a));
      p.add(me);
      return me;
    }

    /* --- the four parts: each its own kind of place, built in its own frame (x right, y up, z toward the reader) --- */

    var wings = ANGLES.map(function (phi, i) {
      var d = V(Math.cos(phi), 0, Math.sin(phi));
      var face = V(0, 0, 1).addScaledVector(d, 0.32).normalize();
      var center = d.clone().multiplyScalar([15.5, 18, 17.5, 19.5][i]).add(V(0, [1.0, 1.4, -1.0, -0.6][i], 0));
      var g = grp(site, center.x, center.y, center.z, Math.atan2(face.x, face.z));
      return {i: i, phi: phi, d: d, g: g, face: face, center: center, side: face,
              tip: center.clone().addScaledVector(d, [7.5, 7.5, 8.5, 6.0][i]).add(V(0, 0, [0, 0, 0, -4.5][i]))};
    });
    function local(w, x, y, z) { w.g.updateMatrixWorld(true); return w.g.localToWorld(V(x, y, z)); }

    var weeks = data.world.villages.slice(0, 4).map(function (v, f) {
      return {def: v, i: f, hex: new THREE.Color(v.tone).getHex(), css: v.tone, days: [], wing: wings[f]};
    });
    var recs = [], byDay = {}, pickables = [];
    var HIDDEN = new THREE.MeshBasicMaterial({transparent: true, opacity: 0, depthWrite: false});
    var frameM = mat(C.steelD, {rough: 0.35, metal: 0.6});
    var goldM = mat(0xd9a441, {rough: 0.3, metal: 0.8}), goldD = mat(0xa8741e, {rough: 0.35, metal: 0.7});
    var daysOf = function (wk) { return data.days.filter(function (d) { return d.band === wk.def.id; }).slice(0, 6); };

    /* a sealed door, in the frame `p` at (x, y, z), its front toward local +z (turned by o.yaw); the shell around
     * it is built by its part. o: size, yaw, win (porthole positions), hit [w, h, d], drop (tag below the door) */
    function door(wk, idx, day, p, x, y, z, o) {
      o = o || {};
      var size = o.size || 1, yaw = o.yaw || 0;
      var hatch = grp(p, x, y, z, yaw);
      hatch.scale.setScalar(size);
      hatch.userData.keep = true;
      var winM = new THREE.MeshStandardMaterial({color: 0x1c2232, emissive: 0xffc27a, emissiveIntensity: 0, roughness: 0.3, metalness: 0.2});
      (o.win || [[-1.0, 0.32], [1.0, 0.32]]).forEach(function (wp) {
        put(hatch, new THREE.CircleGeometry(0.16, 16), winM, wp[0], wp[1], 0.04, {shadow: false}).userData.keep = true;
      });
      put(hatch, new THREE.TorusGeometry(0.6, 0.08, 8, 32), frameM, 0, 0, 0.03, {shadow: false}).userData.keep = true;
      vie.wreath(hatch);
      var innerM = new THREE.MeshBasicMaterial({color: 0xffe3b4, transparent: true, opacity: 0});
      put(hatch, new THREE.CircleGeometry(0.55, 28), innerM, 0, 0, 0.0, {shadow: false}).userData.keep = true;
      var c = document.createElement('canvas'); c.width = c.height = 256;
      var doorTex = new THREE.CanvasTexture(c); doorTex.encoding = THREE.sRGBEncoding;
      var doorM = new THREE.MeshStandardMaterial({color: 0xffffff, map: doorTex, emissive: 0xffffff, emissiveMap: doorTex,
        emissiveIntensity: 0.25, roughness: 0.5, metalness: 0.3, side: THREE.DoubleSide});
      var hinge = grp(hatch, -0.56, 0, 0.06);
      hinge.userData.keep = true;
      put(hinge, new THREE.CircleGeometry(0.55, 28), doorM, 0.56, 0, 0, {shadow: false}).userData.keep = true;
      var lampM = new THREE.MeshBasicMaterial({color: 0xe0584f});
      put(hatch, new THREE.SphereGeometry(0.09, 10, 8), lampM, 0, 0.78, 0.05, {shadow: false}).userData.keep = true;
      hatch.updateWorldMatrix(true, true);
      var hatchAt = hatch.localToWorld(V(0, 0, 0.04));
      var face = hatch.localToWorld(V(0, 0, 1)).sub(hatch.localToWorld(V(0, 0, 0))).normalize();
      var lampAt = hatch.localToWorld(V(0, 0.78, 0.05));
      var lampGlow = halo(site, lampAt.x, lampAt.y, lampAt.z, 0xffc27a, 1.1);
      var flash = halo(site, hatchAt.x, hatchAt.y, hatchAt.z, 0xffe3b4, 3);
      var hs = o.hit || [2.4, 2.4, 1.6];
      var hit = new THREE.Mesh(new THREE.BoxGeometry(hs[0], hs[1], hs[2]), HIDDEN);
      hit.position.set(0, 0, -hs[2] / 2 + 0.1); hatch.add(hit);
      hit.userData.day = day; pickables.push(hit);
      var slot = hatch.localToWorld(V(0, 0, -0.9));
      var drop = o.drop == null ? 1.05 : o.drop;
      var tc = document.createElement('canvas'); tc.width = tc.height = 128;
      var tex = new THREE.CanvasTexture(tc); tex.encoding = THREE.sRGBEncoding;
      var r = {day: day, week: wk, idx: idx, body: hatch, hinge: hinge, innerM: innerM, winM: winM, doorM: doorM, lampM: lampM,
               lampGlow: lampGlow, flash: flash, slot: slot, hatchAt: hatchAt, face: face, state: '', painted: false, t: 0, goal: 0, shake: 0, hover: 0,
               at: hatchAt.clone().addScaledVector(face, 1.4 * size).add(V(0, -drop * size, 0))};
      /* the door: steel, its number in the week's colour; nothing of the day's idea shows outside */
      r.drawDoor = function () {
        var g = c.getContext('2d'), st = r.state;
        g.clearRect(0, 0, 256, 256);
        var gr = g.createRadialGradient(110, 100, 10, 128, 128, 128);
        gr.addColorStop(0, '#f4f6fa'); gr.addColorStop(1, st === 'locked' ? '#8d95a6' : '#c9d0dc');
        g.fillStyle = gr; g.fillRect(0, 0, 256, 256);
        g.strokeStyle = '#6b7385'; g.lineWidth = 6;
        g.beginPath(); g.arc(128, 128, 96, 0, TAU); g.stroke();
        for (var b = 0; b < 8; b++) {
          var a = b * TAU / 8;
          g.beginPath(); g.arc(128 + Math.cos(a) * 112, 128 + Math.sin(a) * 112, 6, 0, TAU); g.fillStyle = '#5d6577'; g.fill();
        }
        g.fillStyle = st === 'locked' ? '#4f5668' : wk.css;
        g.font = 'italic 800 ' + (day > 9 ? 112 : 132) + 'px Fraunces, Georgia, serif';
        g.textAlign = 'center'; g.textBaseline = 'middle';
        g.fillText(String(day), 128, 138);
        doorTex.needsUpdate = true;
      };
      r.drawTag = function () {
        var g = tc.getContext('2d'), st = r.state;
        g.clearRect(0, 0, 128, 128);
        g.beginPath(); g.arc(64, 64, 52, 0, TAU);
        if (st === 'open') { g.fillStyle = wk.css; g.fill(); g.lineWidth = 7; g.strokeStyle = '#ffffff'; g.stroke(); g.fillStyle = '#ffffff'; }
        else if (st === 'ready') { g.fillStyle = '#fff6dc'; g.fill(); g.lineWidth = 8; g.strokeStyle = '#f2b441'; g.stroke(); g.fillStyle = '#2a1c14'; }
        else {
          g.fillStyle = 'rgba(20,26,52,.7)'; g.fill();
          g.setLineDash([10, 7]); g.lineWidth = 5; g.strokeStyle = 'rgba(255,255,255,.85)'; g.stroke(); g.setLineDash([]);
          g.fillStyle = 'rgba(255,255,255,.95)';
        }
        g.font = 'italic 800 ' + (day > 9 ? 58 : 68) + 'px Fraunces, Georgia, serif';
        g.textAlign = 'center'; g.textBaseline = 'middle';
        g.fillText(String(day), 64, 69);
        tex.needsUpdate = true;
        r.drawDoor();
      };
      r.ring = halo(site, r.at.x, r.at.y, r.at.z, 0xffcf6a, 1, {depth: false});
      r.ring.renderOrder = 8;
      r.tag = new THREE.Sprite(new THREE.SpriteMaterial({map: tex, transparent: true, depthWrite: false, depthTest: false}));
      r.tag.renderOrder = 9; r.tag.userData.day = day; r.tag.position.copy(r.at);
      site.add(r.tag); pickables.push(r.tag);
      recs.push(r); byDay[day] = r; wk.days.push(r);
      return r;
    }
    function band(p, wk) { return mat(wk.hex, {rough: 0.45, metal: 0.1}); }

    /* --- part 1, Words: the arrival port. A docking ring, six airlock pods round it like petals, the dishes that
     * listen, and a queue of capsules on approach, each bringing a request ---------------------------------- */
    var capsule, queue = [];
    (function () {
      var w = wings[0], wk = weeks[0], g = w.g, R = 4.3;
      put(g, new THREE.TorusGeometry(R, 0.42, 12, 64), white, 0, 0, 0);
      put(g, new THREE.TorusGeometry(R - 0.55, 0.12, 8, 64), steelD, 0, 0, 0.3);
      /* the port at the centre: a mouth that lights, its approach lights round it */
      put(g, new THREE.CylinderGeometry(1.5, 1.8, 3.2, 20), whiteD, 0, 0, -1.6, {rx: Math.PI / 2});
      put(g, new THREE.TorusGeometry(1.5, 0.16, 8, 32), steelD, 0, 0, 0.05);
      put(g, new THREE.CircleGeometry(1.35, 32), mat(0x12182a, {rough: 0.3, emissive: 0x3a6fd8, ei: 0.35}), 0, 0, 0.02, {shadow: false});
      for (var k = 0; k < 12; k++) blinks.push(halo(g, Math.cos(k * TAU / 12) * 2.05, Math.sin(k * TAU / 12) * 2.05, 0.2, 0x7ad7ff, 0.6));
      /* four spokes from the port to the ring */
      [0.6, 2.2, 3.7, 5.3].forEach(function (a) { rod(g, [Math.cos(a) * 1.7, Math.sin(a) * 1.7, -0.3], [Math.cos(a) * (R - 0.3), Math.sin(a) * (R - 0.3), -0.1], 0.13, steel); });
      /* the six airlock pods, of different lengths and tilts, petals round the ring */
      daysOf(wk).forEach(function (d, idx) {
        var a = (90 + idx * 60 + (rnd() - 0.5) * 14) * Math.PI / 180;
        var pod = grp(g, Math.cos(a) * R, Math.sin(a) * R, 0);
        pod.rotation.x = (rnd() - 0.5) * 0.35; pod.rotation.y = (rnd() - 0.5) * 0.35;
        var size = 0.9 + rnd() * 0.22, L = 1.1 + rnd() * 1.4, pr = 1.0 * size;
        put(pod, new THREE.CylinderGeometry(pr, pr * 1.08, L, 20), [white, whiteD][idx % 2], 0, 0, L / 2, {rx: Math.PI / 2});
        put(pod, new THREE.CylinderGeometry(pr * 1.12, pr * 1.12, 0.22, 20), band(pod, wk), 0, 0, L * 0.35, {rx: Math.PI / 2});
        put(pod, new THREE.TorusGeometry(pr * 1.02, 0.07, 6, 24), steelD, 0, 0, L - 0.05);
        if (idx % 3 === 1) { rod(pod, [0, pr, L * 0.5], [0, pr + 0.9, L * 0.4], 0.035, steel); blinks.push(halo(pod, 0, pr + 0.95, L * 0.4, 0xff6a5a, 0.5)); }
        if (idx % 3 === 2) put(pod, new THREE.BoxGeometry(0.5, 0.3, L * 0.6), mat(C.gold, {rough: 0.4, metal: 0.6}), pr + 0.12, 0, L * 0.45);
        door(wk, idx, d.day, pod, 0, 0, L + 0.02, {size: size, win: [], hit: [2.3, 2.3, L + 0.4]});
      });
      /* the dishes that listen: three, of different sizes, on a mast beside the ring */
      var dishes = [];
      [[-7.6, 2.6, -2.5, 2.2], [-8.4, -1.8, -1.0, 1.4], [-6.4, -4.4, -2.8, 1.0]].forEach(function (s, n) {
        rod(g, [s[0], s[1] - 0.2, s[2]], [s[0] + 2.4, s[1] * 0.4, -1.2], 0.12, steel);
        var dg = grp(g, s[0], s[1], s[2]);
        var bowl = put(dg, new THREE.SphereGeometry(s[3], 20, 8, 0, TAU, 0, Math.PI / 4.5), mat(0xf0f2f5, {rough: 0.35, metal: 0.3}), 0, 0, 0, {rx: -Math.PI / 2});
        bowl.material.side = THREE.DoubleSide;
        rod(dg, [0, 0, -s[3] * 0.1], [0, 0, s[3] * 0.85], 0.03, steel);
        blinks.push(halo(dg, 0, 0, s[3] * 0.9, 0xffd36a, 0.6));
        dg.rotation.y = -0.6 + n * 0.3;
        dishes.push(dg);
      });
      anim.push(function (clock, reduced) { dishes.forEach(function (dg, n) { dg.rotation.x = reduced ? 0 : Math.sin(clock * 0.2 + n) * 0.25; }); });
      /* the approach: capsules in line, coming in from far out to the port; the first one is wrapped as a gift */
      var from = V(-26, 7, 4), to = V(0, 0, 1.2);
      var runway = [];
      for (var q = 0; q < 9; q++) { var p = from.clone().lerp(to, 0.2 + q * 0.09); runway.push(halo(g, p.x, p.y - 1.0, p.z, 0x9dff8a, 0.7)); }
      for (var n = 0; n < 4; n++) {
        var cg = grp(g);
        put(cg, new THREE.ConeGeometry(0.7, 1.2, 16), n % 2 ? whiteD : white, 0.6, 0, 0, {rz: -Math.PI / 2});
        put(cg, new THREE.CylinderGeometry(0.7, 0.7, 0.9, 16), steelD, -0.25, 0, 0, {rz: Math.PI / 2});
        put(cg, new THREE.PlaneGeometry(0.8, 2.2), cellM, -0.25, 0, 0, {rx: -Math.PI / 2});
        blinks.push(halo(cg, 1.2, 0, 0, 0xffd36a, 0.9));
        queue.push(cg);
        if (n === 0) capsule = cg;
      }
      var dir = to.clone().sub(from).normalize(), yaw = Math.atan2(-dir.z, dir.x), pitch = Math.asin(dir.y);
      anim.push(function (clock, reduced) {
        queue.forEach(function (cg, n) {
          var u = reduced ? 0.15 + n * 0.2 : (clock * 0.02 + n / 4) % 1;
          cg.position.copy(from).lerp(to, u * 0.92);
          cg.rotation.set(0, yaw, pitch, 'YZX');
          var s = u > 0.8 ? 1 - (u - 0.8) / 0.12 : 1;
          cg.scale.setScalar(Math.max(0.01, s));
        });
        runway.forEach(function (h, k) { glowAt(h, reduced ? 0.5 : 0.25 + 0.7 * Math.max(0, Math.sin(clock * 3 - k * 0.7))); });
      });
      tunnel(w.d.clone().multiplyScalar(HUB * 0.9), local(w, 0, 0, -3.2), 0.5);
    })();

    /* --- part 2, Proof: the test lab. An open scaffold of cubic cells, labs set in some of them, a robot arm,
     * the test chamber and its two lights, a gauge, a camera that takes the screenshot ------------------------ */
    var greenL, redL;
    (function () {
      var w = wings[1], wk = weeks[1], g = w.g, S = 3.1, H = [2, 3, 2, 3], X0 = -1.5;
      var cx = function (c) { return (c + X0) * S; }, cy = function (r) { return (r - 1) * S; };
      var seen = {};
      function edge(a, b) {
        var k = a.concat(b).map(function (v) { return v.toFixed(2); }).join(',');
        var k2 = b.concat(a).map(function (v) { return v.toFixed(2); }).join(',');
        if (seen[k] || seen[k2]) return;
        seen[k] = 1;
        rod(g, a, b, 0.07, steel);
      }
      for (var c = 0; c < 4; c++) for (var r = 0; r < H[c]; r++) for (var k = 0; k < 2; k++) {
        var x0 = cx(c) - S / 2, x1 = x0 + S, y0 = cy(r) - S / 2, y1 = y0 + S, z0 = -k * S, z1 = z0 - S;
        [[x0, y0], [x1, y0], [x0, y1], [x1, y1]].forEach(function (p) { edge([p[0], p[1], z0], [p[0], p[1], z1]); });
        [[y0, z0], [y1, z0], [y0, z1], [y1, z1]].forEach(function (p) { edge([x0, p[0], p[1]], [x1, p[0], p[1]]); });
        [[x0, z0], [x1, z0], [x0, z1], [x1, z1]].forEach(function (p) { edge([p[0], y0, p[1]], [p[0], y1, p[1]]); });
        if ((c + r + k) % 3 === 0) rod(g, [x0, y0, z0], [x1, y1, z1], 0.04, steelD);
      }
      /* the labs: six cells, three kinds (a cube, an octagon, a cube with its glass dome) */
      var CELLS = [[0, 1], [1, 0], [1, 2], [2, 1], [3, 0], [3, 2]];
      var panelTex = drawn(256, 256, function (q, wd, ht) {
        q.fillStyle = '#e9edf3'; q.fillRect(0, 0, wd, ht);
        q.strokeStyle = '#b9c1ce'; q.lineWidth = 4;
        for (var i = 0; i <= 4; i++) { q.beginPath(); q.moveTo(i * wd / 4, 0); q.lineTo(i * wd / 4, ht); q.stroke(); q.beginPath(); q.moveTo(0, i * ht / 4); q.lineTo(wd, i * ht / 4); q.stroke(); }
      });
      var panelM = new THREE.MeshStandardMaterial({map: panelTex, roughness: 0.5, metalness: 0.2});
      daysOf(wk).forEach(function (d, idx) {
        var cell = CELLS[idx], x = cx(cell[0]), y = cy(cell[1]), kind = idx % 3, z = -S / 2 - 0.1, front;
        if (kind === 1) {
          put(g, new THREE.CylinderGeometry(1.32, 1.32, 2.5, 8), mat(0x5b6478, {rough: 0.45, metal: 0.4}), x, y, z, {rx: Math.PI / 2});
          put(g, new THREE.CylinderGeometry(1.36, 1.36, 0.2, 8), band(g, wk), x, y, z + 0.8, {rx: Math.PI / 2});
          front = z + 1.25;
        } else {
          box(g, 2.5, 2.5, 2.5, panelM, x, y, z);
          box(g, 2.56, 0.18, 2.56, band(g, wk), x, y - 0.9, z);
          if (kind === 2) put(g, new THREE.SphereGeometry(0.85, 16, 8, 0, TAU, 0, Math.PI / 2), mat(0x9fd4ff, {rough: 0.1, metal: 0.3, emissive: 0x2a6fb0, ei: 0.25}), x, y + 1.25, z);
          front = z + 1.25;
        }
        /* experiments: small boxes with their lights on the cell floor behind */
        [[-0.8, 0x6f5bd0], [0.1, 0xf2b630], [0.8, 0x3fe07a]].forEach(function (e, n) {
          if ((idx + n) % 2) box(g, 0.45, 0.4, 0.45, mat(e[1], {rough: 0.5}), x + e[0], y - S / 2 + 0.2, -S - 0.9 - n * 0.4);
        });
        door(wk, idx, d.day, g, x, y, front + 0.02, {win: kind === 1 ? [[-0.95, 0.0], [0.95, 0.0]] : [[-0.92, 0.78], [0.92, 0.78]], hit: [2.6, 2.6, 2.6]});
      });
      /* crates in some empty back cells */
      [[0, 0, 1], [2, 0, 1], [3, 1, 1], [1, 1, 1]].forEach(function (s, n) {
        box(g, 1.4, 1.1, 1.4, mat([0xd9a441, 0x3a6fd8, 0xe9edf3, 0xd9483a][n], {rough: 0.6}), cx(s[0]) + 0.4, cy(s[1]) - S / 2 + 0.55, -S * 1.5);
      });
      /* the robot arm on top of the second column: it turns, bends, and reaches into the cells */
      var top1 = cy(H[1] - 1) + S / 2;
      var base = grp(g, cx(1), top1, -S);
      put(base, new THREE.CylinderGeometry(0.55, 0.7, 0.5, 16), mat(0xf2b630, {rough: 0.5, metal: 0.3}), 0, 0.25, 0);
      var shoulder = grp(base, 0, 0.5, 0);
      var upper = grp(shoulder);
      box(upper, 0.35, 2.4, 0.35, mat(0xf2b630, {rough: 0.5, metal: 0.3}), 0, 1.2, 0);
      var elbow = grp(upper, 0, 2.4, 0);
      ball(elbow, 0.3, steelD, 0, 0, 0);
      var fore = grp(elbow);
      box(fore, 0.28, 2.0, 0.28, mat(0xf2b630, {rough: 0.5, metal: 0.3}), 0, 1.0, 0);
      var hand = grp(fore, 0, 2.0, 0);
      [-1, 1].forEach(function (s) { box(hand, 0.08, 0.45, 0.2, steelD, s * 0.15, 0.2, 0); });
      blinks.push(halo(hand, 0, 0.35, 0, 0x7ad7ff, 0.6));
      anim.push(function (clock, reduced) {
        var u = reduced ? 0 : clock;
        shoulder.rotation.y = Math.sin(u * 0.35) * 1.2;
        upper.rotation.z = 0.55 + Math.sin(u * 0.5) * 0.25;
        elbow.rotation.z = 1.0 + Math.sin(u * 0.5 + 1) * 0.35;
      });
      /* the test chamber on top of the last column: a glass box, a sample, red and green over it */
      var top3 = cy(H[3] - 1) + S / 2, tx = cx(3);
      box(g, 2.4, 0.15, 2.0, steelD, tx, top3 + 0.08, -S);
      var glass = box(g, 2.0, 1.4, 1.6, new THREE.MeshStandardMaterial({color: 0xbfe3ff, transparent: true, opacity: 0.3, roughness: 0.05}), tx, top3 + 0.85, -S, {shadow: false});
      glass.userData.keep = true;
      box(g, 0.6, 0.6, 0.6, mat(0x6f5bd0, {rough: 0.5}), tx, top3 + 0.45, -S);
      box(g, 0.26, 1.2, 0.26, mat(C.dark), tx + 1.3, top3 + 0.75, -S + 0.6);
      redL = ball(g, 0.18, mat(0x401010, {own: true, emissive: C.red, ei: 0.12}), tx + 1.3, top3 + 1.55, -S + 0.6);
      greenL = ball(g, 0.18, mat(0x0f3a1c, {own: true, emissive: 0x3fe07a, ei: 0.12}), tx + 1.3, top3 + 1.15, -S + 0.6);
      redL.userData.keep = greenL.userData.keep = true;
      var redH = halo(g, tx + 1.3, top3 + 1.55, -S + 0.8, 0xff5a4a, 1.4), greenH = halo(g, tx + 1.3, top3 + 1.15, -S + 0.8, 0x3fe07a, 1.4);
      anim.push(function (clock, reduced) {
        var pass = reduced ? true : Math.floor(clock / 1.6) % 3 !== 1;
        redL.material.emissiveIntensity = pass ? 0.12 : 1.6; greenL.material.emissiveIntensity = pass ? 1.6 : 0.12;
        glowAt(redH, pass ? 0 : 0.8); glowAt(greenH, pass ? 0.8 : 0);
      });
      /* the gauge on the first column: a dial and its needle, measuring */
      var top0 = cy(H[0] - 1) + S / 2;
      var gaugeTex = drawn(256, 256, function (q) {
        q.fillStyle = '#f4f6fa'; q.beginPath(); q.arc(128, 128, 120, 0, TAU); q.fill();
        q.lineWidth = 10; q.strokeStyle = '#3fe07a'; q.beginPath(); q.arc(128, 128, 96, Math.PI * 0.8, Math.PI * 1.6); q.stroke();
        q.strokeStyle = '#f2b630'; q.beginPath(); q.arc(128, 128, 96, Math.PI * 1.6, Math.PI * 1.95); q.stroke();
        q.strokeStyle = '#d9483a'; q.beginPath(); q.arc(128, 128, 96, Math.PI * 1.95, Math.PI * 2.2); q.stroke();
        q.strokeStyle = '#4f5668'; q.lineWidth = 4;
        for (var i = 0; i <= 14; i++) { var a = Math.PI * 0.8 + i * Math.PI * 1.4 / 14; q.beginPath(); q.moveTo(128 + Math.cos(a) * 80, 128 + Math.sin(a) * 80); q.lineTo(128 + Math.cos(a) * 66, 128 + Math.sin(a) * 66); q.stroke(); }
      });
      var gauge = grp(g, cx(0), top0 + 1.4, -S * 0.6);
      rod(g, [cx(0), top0, -S * 0.6], [cx(0), top0 + 0.5, -S * 0.6], 0.08, steel);
      put(gauge, new THREE.CylinderGeometry(0.95, 0.95, 0.18, 28), steelD, 0, 0, -0.1, {rx: Math.PI / 2});
      put(gauge, new THREE.CircleGeometry(0.85, 32), new THREE.MeshStandardMaterial({map: gaugeTex, roughness: 0.6, emissive: 0xffffff, emissiveMap: gaugeTex, emissiveIntensity: 0.3}), 0, 0, 0.01, {shadow: false}).userData.keep = true;
      var needle = grp(gauge, 0, 0, 0.04);
      box(needle, 0.06, 0.7, 0.02, mat(C.red), 0, 0.33, 0, {shadow: false}).userData.keep = true;
      anim.push(function (clock, reduced) { needle.rotation.z = reduced ? 0.3 : 0.5 + Math.sin(clock * 0.9) * 0.6 + Math.sin(clock * 2.3) * 0.12; });
      /* the camera on its boom, looking at a lab: now and then it takes the screenshot */
      var cam0 = grp(g, cx(2) + 0.6, cy(H[2] - 1) + S / 2 + 1.2, 1.4);
      rod(g, [cx(2) + 0.6, cy(H[2] - 1) + S / 2, -0.2], [cx(2) + 0.6, cy(H[2] - 1) + S / 2 + 1.2, 1.4], 0.06, steel);
      box(cam0, 0.6, 0.45, 0.5, mat(C.dark), 0, 0, 0);
      put(cam0, new THREE.CylinderGeometry(0.17, 0.2, 0.35, 12), steelD, 0, -0.1, 0.35, {rx: Math.PI / 2});
      cam0.rotation.x = 0.7;
      var camFlash = halo(cam0, 0, -0.1, 0.6, 0xffffff, 2.2);
      anim.push(function (clock, reduced) { var u = (clock % 5) / 5; glowAt(camFlash, reduced ? 0 : (u < 0.05 ? 1 - u / 0.05 : 0)); });
      /* the bridge from the hub: a truss, its own strings of lights */
      var a0 = w.d.clone().multiplyScalar(HUB * 0.9).add(V(0, -0.5, 0)), a1 = local(w, cx(1) + S / 2, cy(0) - S / 2, -S * 2);
      var dd = a1.clone().sub(a0), sd = V(-dd.z, 0, dd.x).normalize().multiplyScalar(0.45);
      [[1, 1], [1, -1], [-1, 1], [-1, -1]].forEach(function (q) {
        var o = sd.clone().multiplyScalar(q[0]).add(V(0, q[1] * 0.45, 0));
        rod(site, [a0.x + o.x, a0.y + o.y, a0.z + o.z], [a1.x + o.x, a1.y + o.y, a1.z + o.z], 0.06, steel);
      });
      for (var t = 0; t < 1; t += 0.08) {
        var p0 = a0.clone().lerp(a1, t), p1 = a0.clone().lerp(a1, t + 0.08);
        rod(site, [p0.x + sd.x, p0.y - 0.45, p0.z + sd.z], [p1.x - sd.x, p1.y + 0.45, p1.z - sd.z], 0.03, steel);
      }
      LINKS.push([a0, a1]);
    })();

    /* --- part 3, Memory: the asteroid. Vault doors set straight in the rock, mining tunnels, a drill on top,
     * ice, a conveyor carrying crates to the hub, the dish that sends the log down ---------------------------- */
    var dish;
    (function () {
      var w = wings[2], wk = weeks[2], g = w.g;
      var geo = new THREE.IcosahedronGeometry(5.2, 3), P = geo.attributes.position, v = V(0, 0, 0);
      for (var i = 0; i < P.count; i++) {
        v.fromBufferAttribute(P, i);
        var k = 1 + 0.12 * Math.sin(v.x * 1.1 + 1) * Math.cos(v.y * 0.9) + 0.08 * Math.sin(v.z * 1.7 + v.x) + 0.05 * Math.sin(v.y * 3.1);
        P.setXYZ(i, v.x * k * 1.5, v.y * k * 0.98, v.z * k * 0.85);
      }
      geo.computeVertexNormals();
      var rock = put(g, geo, mat(0x6a5f55, {rough: 0.95}), 0, 0.4, -3.4);
      g.updateMatrixWorld(true);
      var ray = new THREE.Raycaster();
      /* where a ray from the reader's side meets the rock (in the part's frame) */
      function onRock(x, y, from) {
        var o = from || V(x, y, 20), dir = from ? V(0, 0.4, -3.4).sub(from).normalize() : V(0, 0, -1);
        ray.set(g.localToWorld(o.clone()), g.localToWorld(o.clone().add(dir)).sub(g.localToWorld(o.clone())).normalize());
        var h = ray.intersectObject(rock, false)[0];
        return h ? g.worldToLocal(h.point.clone()) : null;
      }
      /* craters */
      for (var c = 0; c < 10; c++) {
        var a = rnd() * TAU, e = (rnd() - 0.2) * 1.3;
        var p = onRock(0, 0, V(Math.cos(a) * Math.cos(e) * 20, 0.4 + Math.sin(e) * 20, -3.4 + Math.sin(a) * Math.cos(e) * 20));
        if (p) put(g, new THREE.SphereGeometry(0.5 + rnd() * 0.8, 10, 6), mat(0x4e453d, {rough: 1}), p.x, p.y, p.z);
      }
      /* the six vault doors, at different heights in the rock face, each in a gold collar */
      var SPOTS = [[-5.0, 0.6], [-2.6, 2.9], [-2.0, -1.7], [0.4, 0.8], [2.8, 2.8], [3.4, -1.3]];
      daysOf(wk).forEach(function (d, idx) {
        var s = SPOTS[idx], hp = onRock(s[0], s[1]) || V(s[0], s[1], 1.0), z = hp.z + 0.5;
        put(g, new THREE.CylinderGeometry(0.98, 1.1, 1.6, 24, 1, true), idx % 2 ? goldM : goldD, s[0], s[1], z - 0.88, {rx: Math.PI / 2});
        put(g, new THREE.CircleGeometry(0.95, 24), mat(0x2a2218, {rough: 0.9}), s[0], s[1], z - 0.25, {shadow: false});
        put(g, new THREE.TorusGeometry(1.0, 0.14, 8, 28), goldM, s[0], s[1], z - 0.02);
        for (var b = 0; b < 6; b++) { var ba = b * TAU / 6 + 0.3; ball(g, 0.09, steelD, s[0] + Math.cos(ba) * 0.98, s[1] + Math.sin(ba) * 0.98, z + 0.06); }
        door(wk, idx, d.day, g, s[0], s[1], z, {win: [], hit: [2.4, 2.4, 2.0], yaw: (s[0] / 12)});
      });
      /* mining tunnels: dark mouths with their rims, rails going in */
      [[-6.5, -2.6], [5.6, 1.0], [-0.6, 3.9]].forEach(function (s) {
        var hp = onRock(s[0], s[1]);
        if (!hp) return;
        put(g, new THREE.CircleGeometry(0.75, 20), mat(0x0c0d12, {rough: 1}), s[0], s[1], hp.z + 0.12, {shadow: false});
        put(g, new THREE.TorusGeometry(0.78, 0.16, 6, 20), mat(0x857766, {rough: 0.9}), s[0], s[1], hp.z + 0.12);
        blinks.push(halo(g, s[0], s[1] + 0.95, hp.z + 0.3, 0xffb43a, 0.5));
      });
      /* ice: clusters of crystals catching the sun */
      var iceM = new THREE.MeshStandardMaterial({color: 0xbfeaff, roughness: 0.1, metalness: 0.1, emissive: 0x4fb6ff, emissiveIntensity: 0.3, transparent: true, opacity: 0.88, flatShading: true});
      [[-7, 3, -3], [6.5, -2.2, -1], [1.6, 4.6, -5], [-4, -4.2, -1.5], [7.2, 3.2, -4]].forEach(function (s) {
        var hp = onRock(0, 0, V(s[0] * 3, s[1] * 3 + 0.4, s[2] * 3 - 3.4));
        if (!hp) return;
        for (var n = 0; n < 4; n++) {
          var ic = put(g, new THREE.OctahedronGeometry(0.28 + rnd() * 0.2, 0), iceM, hp.x + (rnd() - 0.5) * 0.7, hp.y + 0.2, hp.z + (rnd() - 0.5) * 0.7, {rx: (rnd() - 0.5), rz: (rnd() - 0.5)});
          ic.scale.set(1, 2.4, 1); ic.userData.keep = true;
        }
      });
      /* the drill on top, turning */
      var tp = onRock(0, 0, V(-3.4, 20, -3.0)) || V(-3.4, 4.6, -3);
      [0, 1, 2].forEach(function (n) { var a = n * TAU / 3; rod(g, [tp.x + Math.cos(a) * 1.3, tp.y, tp.z + Math.sin(a) * 1.3], [tp.x, tp.y + 2.6, tp.z], 0.07, steel); });
      var drill = grp(g, tp.x, tp.y + 1.2, tp.z);
      put(drill, new THREE.ConeGeometry(0.35, 1.6, 8), mat(0xf2b630, {rough: 0.4, metal: 0.5}), 0, -0.4, 0, {rx: Math.PI});
      box(g, 0.7, 0.5, 0.7, steelD, tp.x, tp.y + 2.7, tp.z);
      anim.push(function (clock, reduced) { drill.rotation.y = reduced ? 0 : clock * 3; drill.position.y = tp.y + 1.2 + (reduced ? 0 : Math.sin(clock * 0.8) * 0.2); });
      /* the dish on the right shoulder, and the beam that sends the log down to Earth */
      var dp = onRock(0, 0, V(4.6, 20, -2.5)) || V(4.6, 4.4, -2.5);
      dish = grp(g, dp.x, dp.y + 0.2, dp.z);
      put(dish, new THREE.CylinderGeometry(0.1, 0.1, 1.4, 8), steel, 0, 0.6, 0);
      var bowl = put(dish, new THREE.SphereGeometry(1.8, 20, 8, 0, TAU, 0, Math.PI / 4.5), mat(0xf0f2f5, {rough: 0.35, metal: 0.3}), 0, 2.9, 0, {rx: Math.PI});
      bowl.material.side = THREE.DoubleSide;
      put(dish, new THREE.CylinderGeometry(0.03, 0.03, 1.3, 6), steel, 0, 1.75, 0);
      dish.rotation.z = -0.9; dish.userData.keep = true;
      blinks.push(halo(dish, 0, 1.1, 0, 0xffd36a, 0.9));
      var downAt = local(w, dp.x, dp.y + 1.2, dp.z), logBeam = beam(site, downAt, downAt.clone().add(V(4, -46, 6)), 0xffd36a, 0.09);
      anim.push(function (clock, reduced) { logBeam.material.opacity = reduced ? 0.35 : 0.15 + 0.3 * Math.max(0, Math.sin(clock * 1.4)); });
      /* the conveyor: from the lowest tunnel to the hub, crates riding it */
      var c0 = local(w, -6.5, -3.2, 0.6), c1 = w.d.clone().multiplyScalar(HUB * 1.1).add(V(0, -1.4, 0));
      var cd = c1.clone().sub(c0), cs = V(-cd.z, 0, cd.x).normalize().multiplyScalar(0.4);
      [1, -1].forEach(function (s) { var a = c0.clone().addScaledVector(cs, s), b = c1.clone().addScaledVector(cs, s); rod(site, [a.x, a.y, a.z], [b.x, b.y, b.z], 0.06, steelD); });
      for (var t = 0.05; t < 1; t += 0.1) { var sp = c0.clone().lerp(c1, t); rod(site, [sp.x, sp.y - 0.05, sp.z], [sp.x, sp.y - 1.4, sp.z], 0.05, steel); }
      LINKS.push([c0.clone().add(V(0, 0.5, 0)), c1.clone().add(V(0, 0.5, 0))]);
      var crates = [];
      for (var n = 0; n < 7; n++) crates.push(box(site, 0.55, 0.45, 0.55, n % 3 === 0 ? goldM : mat([0x3a6fd8, 0xd9483a, 0xe9edf3][n % 3], {rough: 0.6}), 0, 0, 0));
      anim.push(function (clock, reduced) {
        crates.forEach(function (cr, n) {
          var u = reduced ? n / 7 : (clock * 0.025 + n / 7) % 1;
          cr.position.copy(c0).lerp(c1, u).add(V(0, 0.3, 0));
        });
      });
      tunnel(w.d.clone().multiplyScalar(HUB * 0.9).add(V(0, 0.4, 0)), local(w, 0.4, 0.2, -6.0), 0.45);
    })();

    /* --- part 4, Orchestration: the fleet. A command ship at the centre, six small ships of different shapes
     * in formation round it, each tied to it by a beam; pods shuttle between them ----------------------------- */
    var pods = [], command;
    (function () {
      var w = wings[3], wk = weeks[3], g = w.g;
      /* the command ship: a long hull, its lit bridge, its engines, its panels */
      command = grp(g, 0, 0, -1.6);
      put(command, new THREE.CylinderGeometry(1.25, 1.25, 7, 20), white, 0, 0, 0, {rz: Math.PI / 2});
      put(command, new THREE.ConeGeometry(1.25, 1.8, 20), white, 4.4, 0, 0, {rz: -Math.PI / 2});
      put(command, new THREE.SphereGeometry(1.0, 16, 8, 0, TAU, 0, Math.PI / 2), mat(C.glass, {rough: 0.2, metal: 0.4, emissive: 0xffc27a, ei: 0.6}), 2.2, 1.0, 0);
      [-1.2, 0.6].forEach(function (x) { put(command, new THREE.CylinderGeometry(1.32, 1.32, 0.25, 20), band(command, wk), x, 0, 0, {rz: Math.PI / 2}); });
      box(command, 1.4, 1.8, 1.8, whiteD, -3.9, 0, 0);
      [[0.5, 0.5], [-0.5, 0.5], [0, -0.5]].forEach(function (q) {
        put(command, new THREE.CylinderGeometry(0.32, 0.45, 0.6, 12), steelD, -4.8, q[1], q[0], {rz: Math.PI / 2});
        blinks.push(halo(command, -5.3, q[1], q[0], 0x7ad7ff, 1.4));
      });
      rod(command, [0, 1.2, 0], [0, 3.0, 0], 0.06, steel);
      blinks.push(halo(command, 0, 3.05, 0, 0xff6a5a, 0.7));
      [-1, 1].forEach(function (s) {
        var pg = grp(command, -1.0, 0, s * 4.6);
        put(pg, new THREE.PlaneGeometry(2.2, 6.2), cellM, 0, 0, 0, {rx: -Math.PI / 2});
        pg.rotation.z = 0.25; pg.userData.keep = true;
        rod(command, [-1.0, 0, s * 1.2], [-1.0, 0, s * 1.6], 0.08, steel);
        panels.push(pg);
      });
      blinks.push(halo(command, 2.2, 2.0, 0, 0xffc27a, 2.4));
      /* the six ships: shuttle, sphere, freighter of folders, tug, saucer, twin hull */
      var SHIPS = [[-6.4, 2.8, 0.6, 0.1], [-5.8, -2.8, 1.4, -0.12], [-0.8, 4.6, 0.0, 0.05], [0.6, -4.4, 1.6, -0.08], [6.0, 2.6, 0.8, 0.14], [6.6, -2.5, 1.2, -0.1]];
      var beams = [];
      daysOf(wk).forEach(function (d, idx) {
        var s = SHIPS[idx], sh = grp(g, s[0], s[1], s[2], s[3]), front = 0.9, dy = 0, win = [], hit = [2.4, 2.2, 1.8];
        var hullM = [white, whiteD][idx % 2], tone = band(sh, wk);
        if (idx === 0) {                                     /* the shuttle */
          box(sh, 3.0, 1.5, 1.8, hullM, 0, 0, 0);
          put(sh, new THREE.ConeGeometry(0.9, 1.3, 4), hullM, 2.1, 0, 0, {rz: -Math.PI / 2, rx: Math.PI / 4});
          box(sh, 1.8, 0.1, 4.0, tone, -0.5, -0.85, 0);
          box(sh, 0.9, 0.9, 0.1, tone, -1.2, 0.95, 0, {rz: 0.4});
        } else if (idx === 1) {                              /* the sphere, its ring */
          ball(sh, 1.35, hullM, 0, 0, 0);
          put(sh, new THREE.TorusGeometry(1.95, 0.12, 8, 40), tone, 0, 0, 0, {rx: Math.PI / 2 + 0.3});
          put(sh, new THREE.CylinderGeometry(0.92, 0.92, 0.6, 20), steelD, 0, 0, 1.25, {rx: Math.PI / 2});
          front = 1.55;
        } else if (idx === 2) {                              /* the freighter: a spine of containers, coloured tabs like folders */
          box(sh, 6.0, 0.4, 0.4, steelD, -1.4, 0, -0.2);
          [0x3a6fd8, 0xf2b630, 0x3fe07a, 0xd9483a].forEach(function (col, n) {
            var x = -3.6 + n * 1.35;
            box(sh, 1.15, 1.1, 1.3, mat(col, {rough: 0.6}), x, 0, -0.2);
            box(sh, 0.45, 0.2, 0.9, mat(col, {rough: 0.6}), x - 0.25, 0.65, -0.2);
          });
          box(sh, 2.0, 1.9, 1.8, hullM, 2.0, 0, 0);
          box(sh, 0.6, 0.4, 1.4, mat(C.glass, {emissive: 0xffc27a, ei: 0.4}), 2.9, 0.6, 0);
          dy = 0; s.cab = 2.0;
        } else if (idx === 3) {                              /* the tug: compact, two arms */
          box(sh, 2.2, 1.9, 1.8, hullM, 0, 0, 0);
          [-1, 1].forEach(function (k) { rod(sh, [1.1, k * 0.6, 0], [2.6, k * 0.9, 0], 0.12, tone); box(sh, 0.2, 0.5, 0.3, steelD, 2.7, k * 0.9, 0); });
          put(sh, new THREE.CylinderGeometry(0.5, 0.6, 0.8, 12), steelD, -1.5, 0, 0, {rz: Math.PI / 2});
        } else if (idx === 4) {                              /* the saucer */
          put(sh, new THREE.CylinderGeometry(1.5, 2.1, 1.6, 24), hullM, 0, 0, -1.0);
          put(sh, new THREE.SphereGeometry(1.0, 16, 8, 0, TAU, 0, Math.PI / 2), mat(C.glass, {rough: 0.2, metal: 0.4, emissive: 0x9fd4ff, ei: 0.4}), 0, 0.8, -1.0);
          put(sh, new THREE.TorusGeometry(2.05, 0.08, 6, 32), tone, 0, -0.7, -1.0, {rx: Math.PI / 2});
          front = 1.15;
        } else {                                             /* the twin hull */
          [-1, 1].forEach(function (k) {
            put(sh, new THREE.CylinderGeometry(0.55, 0.55, 4.0, 14), hullM, 0, -0.95, k * 1.1 - 0.4, {rz: Math.PI / 2});
            put(sh, new THREE.ConeGeometry(0.55, 0.9, 14), hullM, 2.45, -0.95, k * 1.1 - 0.4, {rz: -Math.PI / 2});
          });
          box(sh, 2.4, 1.7, 1.4, tone, 0, 0.35, -0.4);
          front = 0.32; dy = 0.35;
        }
        var dx = s.cab || 0;
        var r = door(wk, idx, d.day, sh, dx, dy, front + 0.02, {win: win, hit: hit, size: 0.95});
        /* the engine glow at the back, the beam to the command ship */
        var back = idx === 2 ? -4.6 : idx === 1 ? -1.5 : idx === 4 ? -2.2 : -1.9;
        var eng = halo(sh, back, idx === 5 ? -0.95 : 0, 0, [0x7ad7ff, 0xffb060][idx % 2], 1.6);
        g.updateMatrixWorld(true);
        var bm = beam(site, sh.localToWorld(V(0, 0, -0.6)), command.localToWorld(V(s[0] * 0.25, 0, 0)), 0x7ad7ff, 0.04);
        beams.push({b: bm, e: eng, ph: idx * 0.8});
      });
      anim.push(function (clock, reduced) {
        beams.forEach(function (q) {
          q.b.material.opacity = reduced ? 0.3 : 0.12 + 0.35 * Math.max(0, Math.sin(clock * 2 + q.ph));
          glowAt(q.e, reduced ? 0.7 : 0.5 + 0.35 * Math.sin(clock * 9 + q.ph * 3));
        });
      });
      /* the coordinator's link to the hub: a beam, no tunnel */
      g.updateMatrixWorld(true);
      var hubBeam = beam(site, w.d.clone().multiplyScalar(HUB * 1.05), command.localToWorld(V(0, 0, 0)), 0xffd36a, 0.06);
      anim.push(function (clock, reduced) { hubBeam.material.opacity = reduced ? 0.3 : 0.18 + 0.25 * Math.max(0, Math.sin(clock * 1.2)); });
      /* pods: they leave the command ship for a ship, and come back */
      [0, 2, 4, 5].forEach(function (k, n) {
        var pg = grp(site);
        put(pg, new THREE.SphereGeometry(0.4, 12, 8), white, 0, 0, 0);
        put(pg, new THREE.CylinderGeometry(0.22, 0.32, 0.4, 10), steelD, 0, 0, -0.45, {rx: Math.PI / 2});
        var l = halo(pg, 0, 0.45, 0, [0x7ad7ff, 0xffd36a, 0x9dff8a, 0xff8a7a][n], 1.3);
        var from = command.localToWorld(V(0, 1.6, 0.6)), s = SHIPS[k];
        var dest = local(w, s[0], s[1] + 1.6, s[2] + 1.2);
        var mid = from.clone().add(dest).multiplyScalar(0.5).add(V(0, 2.5, 2.5));
        pods.push({g: pg, l: l, from: from, mid: mid, dest: dest, ph: n * 0.27, period: 14 + n * 3});
      });
    })();
    bake(frameG);
    vie.gift(capsule);

    /* the ring habitat round the hub, turning */
    var ring = grp(site, 0, 0.6, 0);
    (function () {
      put(ring, new THREE.TorusGeometry(5.9, 0.55, 10, 64), white, 0, 0, 0, {rx: Math.PI / 2});
      for (var k = 0; k < 4; k++) {
        var a = k * TAU / 4 + 0.4;
        rod(ring, [Math.cos(a) * 2.2, 0, Math.sin(a) * 2.2], [Math.cos(a) * 5.4, 0, Math.sin(a) * 5.4], 0.16, steel);
      }
      for (var b = 0; b < 16; b++) {
        var bb = b * TAU / 16;
        put(ring, new THREE.TorusGeometry(0.66, 0.06, 6, 16), steelD, Math.cos(bb) * 5.9, 0, Math.sin(bb) * 5.9, {ry: -bb});
        blinks.push(halo(ring, Math.cos(bb) * 6.5, 0, Math.sin(bb) * 6.5, b % 2 ? 0x7ad7ff : 0xffd36a, 0.6));
      }
      ring.rotation.x = 0.22; ring.rotation.z = -0.12;
    })();

    /* the string of lights along each tunnel and bridge */
    var bulbs;
    (function () {
      var pts = [], cols = [], tones = [0xffd36a, 0xff6a5a, 0x7ad7ff, 0x9dff8a];
      LINKS.forEach(function (l) {
        var len = l[0].distanceTo(l[1]);
        for (var x = 0.4, n = 0; x < len - 0.3; x += 0.55, n++) {
          var p = l[0].clone().lerp(l[1], x / len);
          pts.push(p.x, p.y - 0.55, p.z);
          var c = new THREE.Color(tones[n % 4]); cols.push(c.r, c.g, c.b);
        }
      });
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      bulbs = new THREE.Points(g, new THREE.PointsMaterial({size: 0.45, vertexColors: true, map: glowTex, transparent: true,
        depthWrite: false, blending: THREE.AdditiveBlending}));
      site.add(bulbs);
    })();

    /* two of the crew outside, on tethers: one on the scaffold of the lab, one by the drill */
    var walkers = [];
    [[1, -4.6, 3.2, 1.6, 0.9], [2, -1.2, 5.6, -1.0, -0.6]].forEach(function (a, n) {
      var p = local(wings[a[0]], a[1], a[2], a[3]);
      var g = grp(site, p.x, p.y, p.z, a[4]);
      var suit = mat(C.suit, {rough: 0.7});
      box(g, 0.5, 0.62, 0.34, suit, 0, 0, 0); box(g, 0.46, 0.5, 0.2, mat(0xc9ccd4), 0, 0.02, -0.26);
      ball(g, 0.24, suit, 0, 0.5, 0); ball(g, 0.15, mat(0xd9a441, {rough: 0.2, metal: 0.8}), 0, 0.52, 0.12);
      [-1, 1].forEach(function (s) { box(g, 0.16, 0.55, 0.16, suit, s * 0.34, -0.05, 0, {rz: s * 0.5}); box(g, 0.18, 0.6, 0.18, suit, s * 0.13, -0.6, 0, {rz: s * 0.15}); });
      if (n === 0) vie.hat(g);
      walkers.push({g: g, base: p.clone(), ph: n * 2});
    });

    /* the star of the 24th, over the hub */
    var starTex = drawn(256, 256, function (g, w, h) {
      g.translate(w / 2, h / 2);
      for (var k = 0; k < 4; k++) {
        g.rotate(Math.PI / 4);
        var gr = g.createLinearGradient(0, 0, 0, h / 2);
        gr.addColorStop(0, 'rgba(255,255,255,1)'); gr.addColorStop(1, 'rgba(255,255,255,0)');
        g.fillStyle = gr;
        var long = k % 2 ? 0.32 : 0.5;
        g.beginPath(); g.moveTo(-6, 0); g.lineTo(0, -h * long); g.lineTo(6, 0); g.lineTo(0, h * long); g.closePath(); g.fill();
      }
    });
    var starG = halo(site, 0, HUB + 3.35, 0, 0xffe7a8, 9, {map: starTex, depth: false});
    var starCore = halo(site, 0, HUB + 3.35, 0, 0xffd36a, 4, {depth: false});

    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(function () { recs.forEach(function (r) { r.drawTag(); }); });
    }

    /* --- a hatch opens: the door swings out on its hinge, the inside lights, the portholes warm ------------ */

    function pose(r) {
      var t = r.t, k = ease(t);
      r.hinge.rotation.y = -1.95 * k;
      r.innerM.opacity = 0.95 * step(0.05, 0.5, t);
      r.winM.emissiveIntensity = 0.9 * k;
      r.lampM.color.setHex(r.state === 'open' ? 0x2fb67e : r.state === 'ready' ? 0xf2b441 : 0xe0584f);
      glowAt(r.flash, t > 0 && t < 1 ? Math.sin(t * Math.PI) * 0.55 : 0);
    }

    /* --- states ------------------------------------------------------------------------------------ */

    var opened = 0, dirty = 3;
    function paint() {
      opened = 0;
      recs.forEach(function (r) {
        var st = page.state(r.day);
        if (st === 'open') opened++;
        if (st === r.state && r.painted) return;
        var first = !r.painted;
        r.state = st; r.painted = true;
        r.goal = st === 'open' ? 1 : 0;
        if (first || reduced || r.goal === 0) r.t = r.goal;
        pose(r);
        r.drawTag();
      });
      dirty = 3;
    }

    /* --- the camera: a point looked at, an angle around it, a distance ----------------------------- */

    var AZ = [0.5, 2.65], POL = [0.5, 1.32];
    var VIEWS = [{t: [0, 3.5, -1], az: 1.5, pol: 1.08, dist: 80, half: 36, tall: 20}];
    function sideAz(w) {
      var p = Math.atan2(w.side.z, w.side.x);
      return p * 0.75 + Math.PI / 2 * 0.25;
    }
    weeks.forEach(function (wk) {
      var mid = wk.wing.center;
      VIEWS.push({t: [mid.x, mid.y + 0.4, mid.z], az: clamp(Math.atan2(wk.wing.face.z, wk.wing.face.x), AZ[0], AZ[1]), pol: 1.02, dist: 27, half: 12.5});
    });
    var cam = {t: new THREE.Vector3(), az: 1.47, pol: 1.1, dist: 74};
    var goal = {t: new THREE.Vector3(), az: 1.47, pol: 1.1, dist: 74};
    var mode = {kind: 'over', zone: -1, day: 0};
    var narrow = false;
    function reach(v) {
      var k = Math.tan(camera.fov * Math.PI / 360);
      return Math.max(v.dist, v.half / (k * camera.aspect), v.tall ? v.tall / k : 0);
    }
    function fly(to, instant) {
      goal.t.set(to.t[0], to.t[1], to.t[2]);
      goal.az = to.az; goal.pol = to.pol; goal.dist = reach(to);
      if (instant || reduced) { cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist; }
    }
    function focus(r) { return {t: [r.slot.x, r.slot.y + 0.3, r.slot.z], az: clamp(Math.atan2(r.face.z, r.face.x), AZ[0], AZ[1]), pol: 1.08, dist: 13, half: 5.6}; }
    function again() { if (mode.kind === 'day') fly(focus(byDay[mode.day])); else fly(VIEWS[mode.zone + 1]); }
    function fit() {
      var w = host.clientWidth, h = host.clientHeight;
      if (!w || !h) return;
      renderer.setSize(w, h, false);
      camera.aspect = w / h;
      narrow = w / h < 0.9;
      camera.fov = narrow ? 42 : w / h < 1.4 ? 34 : 30;
      camera.updateProjectionMatrix();
      labels.forEach(function (l) { l.half = 0; });
      again();
      cam.dist = goal.dist;
    }
    function overview() { mode = {kind: 'over', zone: -1, day: 0}; host.setAttribute('data-mode', 'over'); fly(VIEWS[0]); chips(); }
    function toZone(i) { mode = {kind: 'zone', zone: i, day: 0}; host.setAttribute('data-mode', 'village'); fly(VIEWS[i + 1]); chips(); }
    function look(day) {
      var r = byDay[day];
      if (!r) return;
      mode = {kind: 'day', zone: r.week.i, day: day};
      host.setAttribute('data-mode', 'house');
      fly(focus(r));
      chips();
    }

    /* a click on a day: go to its module; open its hatch if it may; then go in */
    var pending = [], inside = null;
    function later(fn, ms) { pending.push(setTimeout(fn, reduced ? 0 : ms)); }
    function hatchView(r) {
      var sd = r.face;
      return {t: [r.hatchAt.x, r.hatchAt.y, r.hatchAt.z], az: Math.atan2(sd.z, sd.x), pol: Math.PI / 2 - 0.05, dist: 2.6, half: 0.9};
    }
    function enterInside(day, stop, instant) {
      var r = byDay[day];
      if (!r || !global.StationInterieur) { page.enter(day); return; }
      if (inside) inside.close();
      pending.forEach(clearTimeout); pending = [];
      mode = {kind: 'inside', zone: r.week.i, day: day};
      host.setAttribute('data-mode', 'inside');
      fly(hatchView(r), instant);
      chips();
      function go() {
        inside = global.StationInterieur.open(host, {
          tx: page.tx, lang: page.tx({en: 'en', fr: 'fr'}), day: day, stop: stop || 0,
          title: (data.world.titles || [])[day - 1] || '',
          onread: function () { page.enter(day); },
          onclose: function () { inside = null; if (mode.kind === 'inside' && mode.day === day) look(day); }
        });
      }
      if (instant) go(); else later(go, 900);
    }
    function knock(day) {
      var r = byDay[day];
      if (!r) return;
      pending.forEach(clearTimeout); pending = [];
      look(day);
      later(function () {
        var st = page.state(day);
        if (st === 'locked') { r.shake = 1; page.refuse(day); return; }
        if (st === 'ready') { page.mark(day); paint(); later(function () { enterInside(day); }, 1300); }
        else later(function () { enterInside(day); }, 250);
      }, 850);
    }

    /* --- the pointer ---------------------------------------------------------------------------------- */

    var ray = new THREE.Raycaster(), ptr = new THREE.Vector2();
    var drag = null, hoverDay = 0;
    function pick(e) {
      var b = canvas.getBoundingClientRect();
      ptr.set(((e.clientX - b.left) / b.width) * 2 - 1, -((e.clientY - b.top) / b.height) * 2 + 1);
      ray.setFromCamera(ptr, camera);
      var hits = ray.intersectObjects(pickables, false);
      return hits.length ? hits[0].object.userData : null;
    }
    canvas.addEventListener('pointerdown', function (e) {
      drag = {x: e.clientX, y: e.clientY, moved: 0, az: goal.az, pol: goal.pol, id: e.pointerId, touch: e.pointerType === 'touch', held: false};
    });
    canvas.addEventListener('pointermove', function (e) {
      if (drag) {
        var dx = e.clientX - drag.x, dy = e.clientY - drag.y;
        drag.moved = Math.max(drag.moved, Math.abs(dx) + Math.abs(dy));
        if (drag.touch && !drag.held && Math.abs(dy) > Math.abs(dx) * 1.3) return;
        if (drag.moved > 6) {
          if (!drag.held) { drag.held = true; try { canvas.setPointerCapture(drag.id); } catch (err) {} }
          goal.az = clamp(drag.az + dx * 0.005, AZ[0], AZ[1]);
          goal.pol = clamp(drag.pol - dy * 0.004, POL[0], POL[1]);
          canvas.style.cursor = 'grabbing';
        }
        return;
      }
      var hit = pick(e);
      hoverDay = hit && hit.day ? hit.day : 0;
      canvas.style.cursor = hit ? 'pointer' : 'grab';
    });
    function release(e) {
      if (!drag) return;
      var wasClick = drag.moved <= 6;
      drag = null;
      canvas.style.cursor = 'grab';
      if (!wasClick) return;
      var hit = pick(e);
      if (!hit) { if (mode.kind !== 'over') overview(); return; }
      if (hit.day) knock(hit.day);
    }
    canvas.addEventListener('pointerup', release);
    canvas.addEventListener('pointercancel', function () { drag = null; });
    canvas.addEventListener('pointerleave', function () { hoverDay = 0; });
    canvas.addEventListener('wheel', function (e) {
      e.preventDefault();
      goal.dist = clamp(goal.dist * (1 + Math.sign(e.deltaY) * 0.1), 7, 140);
    }, {passive: false});

    /* --- the names of the four wings, their signs, and the chips to go to them ------------------------- */

    var labels = [];
    weeks.forEach(function (wk, i) {
      var tip = wk.wing.tip;
      board(site, page.tx(SITE.signs[i]), tip.x, 3.6, tip.z, C.dark, 4.6, {h: 0.62, lit: 0.55, ink: '#ffd36a'});
      var el = document.createElement('button');
      el.type = 'button'; el.className = 'monde-label';
      el.style.setProperty('--tone', wk.css);
      var small = document.createElement('small'); small.textContent = page.tx(wk.def.sub);
      var strong = document.createElement('b'); strong.textContent = page.tx(wk.def.name);
      el.appendChild(small); el.appendChild(strong);
      el.addEventListener('click', function () { toZone(i); });
      host.appendChild(el);
      labels.push({el: el, small: small, at: new THREE.Vector3(tip.x, 6.2, tip.z), zone: i});
    });
    var bar = document.createElement('div');
    bar.className = 'monde-bar';
    host.appendChild(bar);
    var chipEls = [];
    function chip(text, tone, fn) {
      var b = document.createElement('button');
      b.type = 'button'; b.className = 'monde-chip'; b.textContent = text;
      if (tone) b.style.setProperty('--tone', tone);
      b.addEventListener('click', fn);
      bar.appendChild(b); chipEls.push(b);
    }
    chip(page.say('overview') || page.tx(SITE.over), '', overview);
    weeks.forEach(function (wk) { chip(page.tx(wk.def.name), wk.css, function () { toZone(wk.i); }); });
    function chips() {
      chipEls.forEach(function (b, i) {
        var on = i === 0 ? mode.kind === 'over' : mode.zone === i - 1;
        b.setAttribute('aria-pressed', on ? 'true' : 'false');
        b.blur();
      });
    }
    var hint = document.createElement('p');
    hint.className = 'monde-hint';
    hint.textContent = page.say('hint');
    bar.insertBefore(hint, bar.firstChild);

    /* --- the loop ---------------------------------------------------------------------------------------- */

    var running = false, visible = true, paused = false, raf = 0, last = 0, clock = 0;
    var tagAt = new THREE.Vector3(), podAt = new THREE.Vector3();
    function dress(dt) {
      var moving = false;
      recs.forEach(function (r) {
        if (r.t !== r.goal) {
          r.t = r.goal > r.t ? Math.min(r.goal, r.t + dt / 1.2) : r.goal;
          pose(r); moving = true;
        }
        var ready = r.state === 'ready', pulse = reduced ? 0.5 : 0.5 + 0.5 * Math.sin(clock * 2.6 + r.day);
        tagAt.copy(r.at);
        if (r.shake > 0) { r.shake = Math.max(0, r.shake - dt * 1.6); tagAt.addScaledVector(r.week.wing.d, Math.sin(r.shake * 34) * 0.14 * r.shake); moving = true; }
        r.tag.position.copy(tagAt);
        r.hover += ((hoverDay === r.day ? 1 : 0) - r.hover) * Math.min(1, dt * 10);
        var size = (r.state === 'open' ? 0.66 : ready ? 0.95 + 0.06 * pulse : 0.82) * (narrow ? 1.6 : 1)
          * (mode.kind === 'over' ? 1.9 : mode.kind === 'zone' ? 1.0 : mode.day === r.day ? 0.62 : 0.5) * (1 + r.hover * 0.22);
        r.tag.scale.set(size, size, 1);
        r.ring.position.copy(tagAt);
        r.ring.scale.set(size * 2.4, size * 2.4, 1);
        glowAt(r.ring, ready ? 0.3 + 0.22 * pulse : 0);
        glowAt(r.lampGlow, ready ? 0.25 + 0.4 * pulse : r.state === 'open' ? 0.2 : 0.12);
      });
      /* the crew pods go out and come back */
      pods.forEach(function (p) {
        var u = reduced ? 0.5 : ((clock / p.period + p.ph) % 1), k = u < 0.5 ? ease(u * 2) : ease((1 - u) * 2);
        var a = 1 - k;
        podAt.copy(p.from).multiplyScalar(a * a).addScaledVector(p.mid, 2 * a * k).addScaledVector(p.dest, k * k);
        p.g.position.copy(podAt);
        glowAt(p.l, reduced ? 0.8 : 0.5 + 0.45 * Math.max(0, Math.sin(clock * 4 + p.ph * 9)));
      });
      walkers.forEach(function (w) {
        w.g.position.y = w.base.y + Math.sin(clock * 0.5 + w.ph) * 0.25;
        w.g.rotation.z = Math.sin(clock * 0.3 + w.ph) * 0.15;
      });
      /* the capsule bringing the next request floats in, and backs off */
      anim.forEach(function (f) { f(clock, reduced); });
      if (!reduced) ring.rotation.y = clock * 0.04;
      vie.tick(clock, reduced);
      blinks.forEach(function (b, n) { glowAt(b, reduced ? 0.7 : 0.35 + 0.55 * Math.max(0, Math.sin(clock * 2.2 + n * 1.3))); });
      if (dish && !reduced) dish.rotation.y = Math.sin(clock * 0.15) * 0.6;
      panels.forEach(function (p, n) { p.rotation.z = 0.35 + (reduced ? 0 : Math.sin(clock * 0.05 + n) * 0.06); });
      /* the more modules are docked, the warmer the station; all 24, and it shines like a star */
      warmth.intensity += (opened / 24 * 1.8 - warmth.intensity) * Math.min(1, dt * 2);
      var all = opened >= 24 ? 1 : 0;
      cellM.emissiveIntensity = 0.12 + all * 0.35;
      var tw = reduced ? 1 : 1 + 0.08 * Math.sin(clock * 2.4);
      glowAt(starG, all * 0.85); starG.scale.set(9 * tw, 9 * tw, 1);
      glowAt(starCore, all * 0.6);
      bulbs.material.opacity = reduced ? 0.9 : 0.7 + 0.3 * Math.sin(clock * 1.7);
      clouds.rotation.z = reduced ? 0 : clock * 0.00012;
      if (moving) dirty = 2;
    }
    function tick(now) {
      raf = 0;
      if (!running) return;
      var dt = Math.min(0.1, (now - last) / 1000 || 0.016);
      last = now; clock += dt;
      var k = 1 - Math.exp(-dt * 3.2);
      cam.t.lerp(goal.t, k);
      cam.az += (goal.az - cam.az) * k; cam.pol += (goal.pol - cam.pol) * k; cam.dist += (goal.dist - cam.dist) * k;
      place();
      dress(dt);
      if (dirty > 0) { renderer.shadowMap.needsUpdate = true; dirty--; }
      renderer.render(scene, camera);
      nameZones();
      raf = requestAnimationFrame(tick);
    }
    function place() {
      var sway = mode.kind === 'over' && !drag && !reduced ? Math.sin(clock * 0.12) * 0.05 : 0;
      var sp = Math.sin(cam.pol), az = cam.az + sway;
      camera.position.set(cam.t.x + cam.dist * sp * Math.cos(az), cam.t.y + cam.dist * Math.cos(cam.pol), cam.t.z + cam.dist * sp * Math.sin(az));
      camera.lookAt(cam.t);
    }
    var spot = new THREE.Vector3();
    function nameZones() {
      var w = host.clientWidth, h = host.clientHeight;
      labels.forEach(function (l) {
        var zone = mode.kind === 'zone' && mode.zone === l.zone;
        spot.copy(l.at).project(camera);
        var show = spot.z < 1 && (mode.kind === 'over' || zone);
        l.el.classList.toggle('off', !show);
        if (!show) return;
        var half = (l.half || (l.half = l.el.offsetWidth / 2)) + 8;
        var x = clamp((spot.x + 1) / 2 * w, half, w - half), y = Math.max(64, (1 - spot.y) / 2 * h);
        if (zone) { x = w / 2; y = 86; }
        l.el.style.transform = 'translate(-50%, -100%) translate(' + x.toFixed(1) + 'px,' + y.toFixed(1) + 'px)';
      });
    }
    function sync() {
      var should = visible && !paused && !document.hidden;
      if (should && !running) { running = true; last = performance.now(); raf = requestAnimationFrame(tick); }
      else if (!should) { running = false; }
    }
    if (global.IntersectionObserver) {
      new IntersectionObserver(function (es) { visible = es[0].isIntersecting; sync(); }, {threshold: 0.02}).observe(host);
    }
    document.addEventListener('visibilitychange', sync);
    global.addEventListener('resize', fit);
    if (global.ResizeObserver) new ResizeObserver(fit).observe(host);

    host.setAttribute('data-mode', 'over');
    paint();
    fit();
    chips();
    canvas.style.cursor = 'grab';
    fly(VIEWS[0], true);
    sync();

    return {
      paint: paint,
      pause: function (on) { paused = on; sync(); },
      back: function () { if (inside) inside.close(); else if (mode.kind === 'day') toZone(mode.zone); },
      overview: overview, zone: toZone, knock: knock,
      /* the calls every scene offers (see apercu_du_monde.js) */
      views: function () { return ['station', 'aile1', 'aile2', 'aile3', 'aile4']; },
      view: function (name) { if (name === 'station') overview(); else toZone(Number(name.slice(-1)) - 1); },
      look: look,
      /* go into a module at once, on one of its stops (for a capture) */
      inside: function (day, stop) { enterInside(day, stop, true); return inside; },
      settle: function () {
        cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist;
        recs.forEach(function (r) { r.t = r.goal; pose(r); r.shake = 0; });
        if (inside) inside.settle();
        warmth.intensity = opened / 24 * 1.8; dirty = 3;
        place();
        dress(0.016);
      }
    };
  }

  global.SceneStationLibre = {mount: mount};
})(window);
