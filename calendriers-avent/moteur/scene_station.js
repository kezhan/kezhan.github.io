/* The space station, at dawn above the Earth: the scene of the Claude Code calendar.
 *
 * An advent calendar: 24 doors, opened one a day. Here the doors are the hatches of 24 modules, all docked on
 * the station from the first day and all sealed. Nothing of a day's idea shows outside, only its number:
 *
 *   wing 1  Words           the docking port where requests come in
 *   wing 2  Proof           the test lab
 *   wing 3  Memory          the archive and the log, sent down to Earth by the dish
 *   wing 4  Orchestration   mission control, where the crew pods (the sessions) leave from
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
  var ANGLES = [225, 315, 45, 135].map(function (a) { return a * Math.PI / 180; });

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
    var panels = [];
    var wings = ANGLES.map(function (phi, i) {
      var d = new THREE.Vector3(Math.cos(phi), 0, Math.sin(phi));
      var g = grp(frameG, 0, 0, 0, -phi);           /* local x runs out along the wing */
      /* the bare truss under the slots */
      var a = 0.38, y0 = SPINE_Y;
      [[-a, -a], [a, -a], [a, a], [-a, a]].forEach(function (c) { rod(g, [HUB - 0.3, y0 + c[0], c[1]], [TIP + 0.4, y0 + c[0], c[1]], 0.06, steel); });
      for (var x = HUB - 0.3; x < TIP; x += 1.1) {
        rod(g, [x, y0 - a, -a], [x + 1.1, y0 + a, -a], 0.03, steel); rod(g, [x, y0 - a, a], [x + 1.1, y0 + a, a], 0.03, steel);
        rod(g, [x, y0 + a, -a], [x + 1.1, y0 + a, a], 0.03, steel);
      }
      /* a saddle at each slot, under its module */
      for (var k = 0; k < 6; k++) {
        var cx = R0 + (k + 0.5) * SLOT;
        box(g, 0.9, 0.55, 1.1, steelD, cx, y0 + 0.65, 0);
      }
      /* the landmark node at the end, and its pair of panels across the wing */
      ball(g, 1.35, white, TIP + 1.2, 0, 0);
      rod(g, [TIP + 1.2, 0, -1.2], [TIP + 1.2, 0, -12.2], 0.12, steel);
      rod(g, [TIP + 1.2, 0, 1.2], [TIP + 1.2, 0, 12.2], 0.12, steel);
      [-1, 1].forEach(function (s) {
        var pg = grp(g, TIP + 1.2, 0, s * 7.2);
        put(pg, new THREE.PlaneGeometry(3.4, 9.2), cellM, 0, 0, 0, {rx: -Math.PI / 2});
        pg.rotation.z = 0.35; pg.userData.keep = true;
        panels.push(pg);
      });
      return {i: i, phi: phi, d: d, g: g, side: (function () {
        var s = new THREE.Vector3(-d.z, 0, d.x);
        return s.z < 0 ? s.negate() : s;
      })(), tip: d.clone().multiplyScalar(TIP + 1.2)};
    });
    /* the landmarks */
    var blinks = [];
    /* wing 1, words: a docking port, its approach lights, and a capsule bringing the next request */
    (function () {
      var g = wings[0].g, x = TIP + 2.5;
      put(g, new THREE.CylinderGeometry(0.75, 1.05, 1.6, 16), whiteD, x + 0.6, 0, 0, {rz: Math.PI / 2});
      put(g, new THREE.TorusGeometry(0.8, 0.12, 8, 24), steelD, x + 1.45, 0, 0, {ry: Math.PI / 2});
      [[0, 1.3], [0, -1.3], [1.3, 0], [-1.3, 0]].forEach(function (c) {
        blinks.push(halo(g, x + 1.6, c[0], c[1], 0x7ad7ff, 1.1));
      });
    })();
    var capsule = grp(site);
    (function () {
      put(capsule, new THREE.ConeGeometry(0.9, 1.5, 16), white, 0, 0, 0, {rz: Math.PI / 2});
      put(capsule, new THREE.CylinderGeometry(0.9, 0.9, 0.5, 16), steelD, -1.0, 0, 0, {rz: Math.PI / 2});
      put(capsule, new THREE.PlaneGeometry(1.0, 2.8), cellM, -1.0, 0, 1.9, {rx: -Math.PI / 2});
      put(capsule, new THREE.PlaneGeometry(1.0, 2.8), cellM, -1.0, 0, -1.9, {rx: -Math.PI / 2});
      blinks.push(halo(capsule, 0.8, 0, 0, 0xffd36a, 1.2));
    })();
    /* wing 2, proof: the test lab, an open platform of experiments and its two lights */
    var greenL, redL;
    (function () {
      var g = wings[1].g, x = TIP + 1.2;
      box(g, 3.2, 0.2, 2.6, steelD, x + 1.4, 1.5, 0);
      [[0.6, -0.6, 0x6f5bd0], [1.6, 0.5, 0xe9edf3], [2.4, -0.4, 0xf2b630], [1.0, 0.7, 0x3a6fd8]].forEach(function (b) {
        box(g, 0.6, 0.5, 0.6, mat(b[2], {rough: 0.5}), x + b[0], 1.85, b[1]);
      });
      box(g, 0.3, 1.0, 0.3, mat(C.dark), x + 2.7, 2.1, 0.9);
      redL = ball(g, 0.14, mat(0x401010, {own: true, emissive: C.red, ei: 0.12}), x + 2.7, 2.42, 1.06);
      greenL = ball(g, 0.14, mat(0x0f3a1c, {own: true, emissive: 0x3fe07a, ei: 1.4}), x + 2.7, 2.0, 1.06);
      redL.userData.keep = greenL.userData.keep = true;
      blinks.push(halo(g, x + 2.7, 2.0, 1.2, 0x3fe07a, 1.4));
    })();
    /* wing 3, memory: the archive racks and the dish that sends the log down */
    var dish;
    (function () {
      var g = wings[2].g, x = TIP + 1.2;
      for (var k = 0; k < 3; k++) tubeX(g, 0.42, x - 0.2, x + 2.6, mat(0xd79a1e, {rough: 0.5, metal: 0.2}), -1.3 + k * 0.05, -1.0 + k * 1.0, 12);
      dish = grp(g, x + 0.6, 1.4, 0);
      put(dish, new THREE.CylinderGeometry(0.1, 0.1, 1.4, 8), steel, 0, 0.6, 0);
      var bowl = put(dish, new THREE.SphereGeometry(1.8, 20, 8, 0, TAU, 0, Math.PI / 4.5), mat(0xf0f2f5, {rough: 0.35, metal: 0.3}), 0, 2.9, 0, {rx: Math.PI});
      bowl.material.side = THREE.DoubleSide;
      put(dish, new THREE.CylinderGeometry(0.03, 0.03, 1.3, 6), steel, 0, 1.75, 0);
      dish.rotation.z = -0.9; dish.userData.keep = true;
      blinks.push(halo(dish, 0, 1.1, 0, 0xffd36a, 0.9));
    })();
    /* wing 4, orchestration: mission control, its lit cupola, the crew pods docked around it */
    (function () {
      var g = wings[3].g, x = TIP + 1.2;
      put(g, new THREE.SphereGeometry(1.1, 16, 8, 0, TAU, 0, Math.PI / 2), mat(C.glass, {rough: 0.2, metal: 0.4, emissive: 0xffc27a, ei: 0.55}), x, 1.25, 0);
      [-1, 1].forEach(function (s) { put(g, new THREE.CylinderGeometry(0.5, 0.5, 1.2, 12), whiteD, x + 0.2, 0, s * 1.6, {rx: Math.PI / 2}); });
      tubeX(g, 0.7, x + 1.2, x + 3.2, white, 0, 0, 14);
      put(g, new THREE.ConeGeometry(0.7, 1.0, 14), white, x + 3.7, 0, 0, {rz: -Math.PI / 2});
      blinks.push(halo(g, x, 2.1, 0, 0xffc27a, 2.6));
    })();
    bake(frameG);
    vie.gift(capsule);
    vie.packets(site, wings, data.world.villages.slice(0, 4).map(function (v) { return new THREE.Color(v.tone).getHex(); }), SPINE_Y, HUB + 0.6, TIP);
    wings.forEach(function (w) { w.g.updateMatrixWorld(true); });

    /* the string of lights along each truss */
    var bulbs;
    (function () {
      var pts = [], cols = [], tones = [0xffd36a, 0xff6a5a, 0x7ad7ff, 0x9dff8a];
      wings.forEach(function (w) {
        for (var x = HUB + 0.6, n = 0; x < TIP; x += 0.75, n++) {
          [-1, 1].forEach(function (s) {
            var p = w.d.clone().multiplyScalar(x).addScaledVector(w.side, s * 0.5);
            pts.push(p.x, SPINE_Y - 0.55, p.z);
            var c = new THREE.Color(tones[(n + (s > 0 ? 2 : 0)) % 4]); cols.push(c.r, c.g, c.b);
          });
        }
      });
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      bulbs = new THREE.Points(g, new THREE.PointsMaterial({size: 0.45, vertexColors: true, map: glowTex, transparent: true,
        depthWrite: false, blending: THREE.AdditiveBlending}));
      site.add(bulbs);
    })();

    /* --- the weeks and their days: 24 modules, all docked, each sealed by its hatch ------------------- */

    var weeks = data.world.villages.slice(0, 4).map(function (v, f) {
      return {def: v, i: f, hex: new THREE.Color(v.tone).getHex(), css: v.tone, days: [], wing: wings[f]};
    });
    var recs = [], byDay = {}, pickables = [];
    var HIDDEN = new THREE.MeshBasicMaterial({transparent: true, opacity: 0, depthWrite: false});
    var frameM = mat(C.steelD, {rough: 0.35, metal: 0.6});
    var fittings = [];
    var kinds = global.StationModules.make({THREE: THREE, mat: mat, put: put, box: box, ball: ball, tubeX: tubeX, rod: rod, grp: grp,
      drawn: drawn, halo: halo, glowAt: glowAt, MOD_L: MOD_L, MOD_R: MOD_R});
    weeks.forEach(function (wk) {
      var w = wk.wing;
      data.days.filter(function (d) { return d.band === wk.def.id; }).slice(0, 6).forEach(function (d, idx) {
        var day = d.day, rc = R0 + (idx + 0.5) * SLOT;
        var slot = w.d.clone().multiplyScalar(rc);
        /* the module: white, two bands of its week, a ring at each end, a row of portholes */
        var body = grp(site, slot.x, 0, slot.z);
        var hull = grp(body, 0, 0, 0, -w.phi);
        var band = mat(wk.hex, {rough: 0.45, metal: 0.1});
        /* the module of its wing's kind (station_modules.js), a ring of the week's colour at each end */
        var half = kinds.shell(wk.i, idx, hull, w.d.x < 0 ? -1 : 1, fittings);
        [-1, 1].forEach(function (e) { var ex = e * Math.min(half - 0.1, MOD_L / 2 - 0.1); tubeX(hull, MOD_R * 0.96, ex - 0.07, ex + 0.07, band, 0, 0, 20); });
        box(hull, 0.5, 0.06, 1.6, mat(C.dark), 0, -MOD_R - 0.02, 0);
        bake(hull);
        /* the portholes, dark while the module is sealed, warm once it is open */
        var winM = new THREE.MeshStandardMaterial({color: 0x1c2232, emissive: 0xffc27a, emissiveIntensity: 0, roughness: 0.3, metalness: 0.2});
        var hatch = grp(body);
        hatch.position.copy(w.side).multiplyScalar(MOD_R + 0.02);
        hatch.lookAt(hatch.position.clone().add(w.side).add(body.position));
        hatch.position.copy(w.side).multiplyScalar(MOD_R + 0.02);
        hatch.userData.keep = true;
        [-1, 1].forEach(function (s) {
          var pw = put(hatch, new THREE.CircleGeometry(0.16, 16), winM, s * 1.0, 0.32, 0.04, {shadow: false});
          pw.userData.keep = true;
        });
        /* the hatch: a ring, the glow of the inside behind it, and the door on its hinge with the day's number */
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
        /* the lamp over the hatch: red while locked, amber when it may open, green once open */
        var lampM = new THREE.MeshBasicMaterial({color: 0xe0584f});
        put(hatch, new THREE.SphereGeometry(0.09, 10, 8), lampM, 0, 0.78, 0.05, {shadow: false}).userData.keep = true;
        var hatchAt = slot.clone().addScaledVector(w.side, MOD_R + 0.06);
        var lampGlow = halo(site, hatchAt.x, 0.78, hatchAt.z, 0xffc27a, 1.1);
        var flash = halo(site, hatchAt.x, 0, hatchAt.z, 0xffe3b4, 3);

        var hit = new THREE.Mesh(new THREE.BoxGeometry(SLOT - 0.2, 2.6, 2.6), HIDDEN);
        hit.position.set(slot.x, 0, slot.z); hit.rotation.y = -w.phi;
        hit.userData.day = day; site.add(hit); pickables.push(hit);

        var tc = document.createElement('canvas'); tc.width = tc.height = 128;
        var tex = new THREE.CanvasTexture(tc); tex.encoding = THREE.sRGBEncoding;
        var r = {day: day, week: wk, idx: idx, body: body, hinge: hinge, innerM: innerM, winM: winM, doorM: doorM, lampM: lampM,
                 lampGlow: lampGlow, flash: flash, slot: slot, hatchAt: hatchAt, state: '', painted: false, t: 0, goal: 0, shake: 0, hover: 0,
                 at: slot.clone().addScaledVector(w.side, 2.0).add(new THREE.Vector3(0, -0.6, 0))};
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
      });
    });

    /* --- the crew pods (the sessions): they leave mission control for the other wings, and come back --- */

    var pods = [];
    [0, 1, 2, 1].forEach(function (to, n) {
      var g = grp(site);
      put(g, new THREE.SphereGeometry(0.55, 14, 10), white, 0, 0, 0);
      put(g, new THREE.CylinderGeometry(0.3, 0.45, 0.6, 12), steelD, 0, 0, -0.6, {rx: Math.PI / 2});
      var l = halo(g, 0, 0.6, 0, [0x7ad7ff, 0xffd36a, 0x9dff8a, 0xff8a7a][n], 1.6);
      var from = wings[3].tip.clone().add(new THREE.Vector3(0, 2.2, 0));
      var dest = wings[to].d.clone().multiplyScalar(R0 + (2 + n % 3) * SLOT).add(new THREE.Vector3(0, 3.2 + n * 0.5, 0));
      var mid = from.clone().add(dest).multiplyScalar(0.5).add(new THREE.Vector3(0, 7 + n * 1.5, 0));
      pods.push({g: g, l: l, from: from, mid: mid, dest: dest, ph: n * 0.27, period: 22 + n * 3});
    });
    /* two of the crew outside, on tethers */
    var walkers = [];
    [[1, 3, 0.9], [2, 1, -0.6]].forEach(function (a, n) {
      var w = wings[a[0]], p = w.d.clone().multiplyScalar(R0 + (a[1] + 0.5) * SLOT).addScaledVector(w.side, -1.6).add(new THREE.Vector3(0, 1.9, 0));
      var g = grp(site, p.x, p.y, p.z, a[2]);
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
      var mid = wk.wing.d.clone().multiplyScalar(R0 + 3 * SLOT);
      VIEWS.push({t: [mid.x, 0.4, mid.z], az: sideAz(wk.wing), pol: 0.98, dist: 27, half: 12.5});
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
    function focus(r) { return {t: [r.slot.x, 0.3, r.slot.z], az: sideAz(r.week.wing), pol: 1.05, dist: 13, half: 5.6}; }
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
      var sd = r.week.wing.side;
      return {t: [r.hatchAt.x, 0, r.hatchAt.z], az: Math.atan2(sd.z, sd.x), pol: Math.PI / 2 - 0.05, dist: 2.6, half: 0.9};
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
      var cu = reduced ? 0.5 : 0.5 + 0.5 * Math.sin(clock * 0.25);
      capsule.position.copy(wings[0].d).multiplyScalar(TIP + 7 + cu * 4).add(new THREE.Vector3(0, 0.4, 0));
      capsule.rotation.y = -wings[0].phi + Math.PI;
      kinds.tick(fittings, clock, reduced);
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

  global.SceneStation = {mount: mount};
})(window);
