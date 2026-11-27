/* The tree village, a winter night: a trial scene for the Claude Code calendar (beside the station).
 *
 * An advent calendar: 24 doors, opened one a day. Each of the four parts lives in its own kind of place in one giant
 * tree (arbre_parties.js): Words in burrows among the roots, Proof on a workshop bough, Memory in a library dug into
 * the trunk, Orchestration in lookouts round the crow's nest at the top. Nothing of a day's idea shows outside, only
 * its number on a round door. A click opens the door and goes in: the day's practice plays as in the station
 * (`station_interieur.js`, one file per day `station_jNN.js`).
 *
 * Same calls as the other scenes (see apercu_du_monde.js), and `inside(day, stop)`. Needs three.js r128.
 */
(function (global) {
  'use strict';

  var TAU = Math.PI * 2;
  var TRUNK_H = 34.2, TOP = 39.6;
  var C = {bark: 0x6b4a2e, barkD: 0x4e3420, leaf: 0x2c6a43, leafD: 0x1f5232, snow: 0xf2f6fb, wood: 0xb07a4a,
           woodD: 0x8a5a34, plaster: 0xeadcc2, stone: 0x9aa0ab, rope: 0xc9b089, dark: 0x1c2030};
  var WALLS = [0xb07a4a, 0xeadcc2, 0xc99a6a, 0x9c6b45, 0xe4d2b0, 0xb98b60];

  function mount(host, data, page) {
    var THREE = global.THREE;
    if (!THREE || !host || !data.world) return null;
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
    scene.background = new THREE.Color(0x0d1a38);
    scene.fog = new THREE.Fog(0x0d1a38, 90, 260);
    var camera = new THREE.PerspectiveCamera(32, 2, 0.5, 2000);
    var site = new THREE.Group();
    scene.add(site);

    /* --- light: the moon, cold, from the upper left; the warm windows do the rest -------------------- */

    scene.add(new THREE.HemisphereLight(0x8fa4e0, 0x22304f, 0.5));
    var moon = new THREE.DirectionalLight(0xc9d6ff, 0.6);
    moon.position.set(-30, 50, 30);
    moon.castShadow = true;
    moon.shadow.mapSize.set(2048, 2048);
    moon.shadow.camera.left = -30; moon.shadow.camera.right = 30;
    moon.shadow.camera.top = 42; moon.shadow.camera.bottom = -12;
    moon.shadow.camera.near = 1; moon.shadow.camera.far = 140;
    moon.shadow.bias = -0.0005; moon.shadow.normalBias = 0.03;
    scene.add(moon);
    var fill = new THREE.DirectionalLight(0xffc89a, 0.3);
    fill.position.set(20, 10, 40);
    scene.add(fill);
    var warmth = new THREE.PointLight(0xffc27a, 0, 50, 2);
    warmth.position.set(0, 14, 8);
    scene.add(warmth);

    /* --- small helpers --------------------------------------------------------------- */

    var seed = 20261224;
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
    function ease(x) { x = clamp(x, 0, 1); return x < 0.5 ? 4 * x * x * x : 1 - Math.pow(-2 * x + 2, 3) / 2; }
    function step(a, b, x) { var t = clamp((x - a) / (b - a), 0, 1); return t * t * (3 - 2 * t); }
    var UP = new THREE.Vector3(0, 1, 0);

    var mats = {};
    function mat(color, o) {
      o = o || {};
      var key = o.own ? '' : [color, o.rough, o.metal, o.emissive, o.ei].join('|');
      if (key && mats[key]) return mats[key];
      var m = new THREE.MeshStandardMaterial({color: color, flatShading: true, roughness: o.rough == null ? 0.75 : o.rough,
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
    function box(p, w, h, d, m, x, y, z, o) { return put(p, new THREE.BoxGeometry(w, h, d), m, x, y, z, o); }
    function ball(p, r, m, x, y, z, o) { return put(p, new THREE.SphereGeometry(r, 14, 10), m, x, y, z, o); }
    /* a tapered limb from a to b */
    function limb(p, a, b, r0, r1, m) {
      var me = new THREE.Mesh(new THREE.CylinderGeometry(r1, r0, a.distanceTo(b), 8), m);
      me.position.copy(a).add(b).multiplyScalar(0.5);
      me.quaternion.setFromUnitVectors(UP, b.clone().sub(a).normalize());
      me.castShadow = me.receiveShadow = true;
      p.add(me);
      return me;
    }
    function rod(p, a, b, r, m) { return limb(p, a, b, r, r, m); }
    function grp(p, x, y, z, ry) { var g = new THREE.Group(); g.position.set(x || 0, y || 0, z || 0); g.rotation.y = ry || 0; p.add(g); return g; }
    function V(x, y, z) { return new THREE.Vector3(x, y, z); }

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

    /* --- the night: stars, the moon, the snowy ground and its small firs ----------------------------- */

    (function () {
      var pts = [], cols = [];
      for (var i = 0; i < 1400; i++) {
        var a = rnd() * TAU, e = 0.05 + Math.asin(rnd()) * 0.95, r = 900;
        pts.push(Math.cos(e) * Math.cos(a) * r, Math.sin(e) * r, Math.cos(e) * Math.sin(a) * r);
        var c = new THREE.Color().setHSL(0.6 - rnd() * 0.3, 0.3, 0.8 + rnd() * 0.2);
        cols.push(c.r, c.g, c.b);
      }
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      var m = new THREE.PointsMaterial({size: 2, sizeAttenuation: false, vertexColors: true, transparent: true, opacity: 0.85, depthWrite: false, fog: false});
      scene.add(new THREE.Points(g, m));
    })();
    var moonAt = V(-260, 300, -520);
    glowAt(halo(scene, moonAt.x, moonAt.y, moonAt.z, 0xf4f1e2, 60, {depth: false}), 1);
    glowAt(halo(scene, moonAt.x, moonAt.y, moonAt.z, 0x9fb6ff, 260, {depth: false}), 0.35);
    var snowM = mat(0xdde6f4, {rough: 0.95}), snowS = mat(0xc9d5ea, {rough: 0.95});
    var ground = put(scene, new THREE.CircleGeometry(400, 64), snowM, 0, 0, 0, {rx: -Math.PI / 2});
    ground.castShadow = false;
    /* the knoll under the tree, and small drifts around */
    put(site, new THREE.SphereGeometry(14, 32, 12, 0, TAU, 0, Math.PI / 2), snowS, 0, -9.5, 0).scale.set(1, 0.9, 1);
    for (var di = 0; di < 18; di++) {
      var da = rnd() * TAU, dr = 16 + rnd() * 30;
      put(site, new THREE.SphereGeometry(2 + rnd() * 4, 12, 6, 0, TAU, 0, Math.PI / 2), snowM, Math.cos(da) * dr, -0.4, Math.sin(da) * dr).scale.set(1, 0.35, 1);
    }
    var firM = mat(C.leaf, {rough: 0.8}), firD = mat(C.leafD, {rough: 0.8});
    function fir(p, x, z, s) {
      var g = grp(p, x, 0, z);
      put(g, new THREE.CylinderGeometry(0.15 * s, 0.2 * s, 0.6 * s, 6), mat(C.barkD), 0, 0.3 * s, 0);
      [[1.3, 1.6, 1.2], [1.0, 1.4, 2.1], [0.65, 1.2, 2.9]].forEach(function (t, i) {
        put(g, new THREE.ConeGeometry(t[0] * s, t[1] * s, 8), i % 2 ? firD : firM, 0, t[2] * s, 0);
        put(g, new THREE.ConeGeometry(t[0] * s * 0.6, t[1] * s * 0.35, 8), snowM, 0, t[2] * s + t[1] * s * 0.33, 0);
      });
      return g;
    }
    for (var fi = 0; fi < 26; fi++) {
      var fa = rnd() * TAU, fr = 17 + rnd() * 34;
      if (Math.sin(fa) > 0.35 && fr < 30) fr += 16;             /* keep the front open */
      fir(site, Math.cos(fa) * fr, Math.sin(fa) * fr, 1.4 + rnd() * 1.8);
    }

/* --- the tree: the trunk, its roots, the crown behind, a few bare boughs; then the four parts --------- */

    var bark = mat(C.bark, {rough: 0.9}), barkD = mat(C.barkD, {rough: 0.9});
    function trunkR(y) { return 2.7 - (y / TRUNK_H) * 1.9; }
    function groundY(x, z) { var q = 196 - x * x - z * z; return q > 0 ? Math.max(0, -9.5 + 0.9 * Math.sqrt(q)) : 0; }
    for (var ty = 0; ty < TRUNK_H; ty += 3) {
      var wob = V((rnd() - 0.5) * 0.25, 0, (rnd() - 0.5) * 0.25);
      limb(site, V(0, ty, 0).add(wob), V(0, ty + 3.05, 0), trunkR(ty), trunkR(ty + 3), ty % 6 ? bark : barkD);
    }
    /* the roots run between the burrows, and all round the back */
    [40, 66, 91, 116, 142, 175, 215, 250, 290, 325, 5].forEach(function (deg, i) {
      var ra = deg * Math.PI / 180, rl = 3.2 + (i % 3) * 0.9;
      var a = V(Math.cos(ra) * 1.8, 2.2, Math.sin(ra) * 1.8), b = V(Math.cos(ra) * (2.6 + rl), 0, Math.sin(ra) * (2.6 + rl));
      b.y = groundY(b.x, b.z) - 0.2;
      limb(site, a, b, 1.0, 0.3, bark);
    });
    var leaf = mat(C.leaf, {rough: 0.85}), leafD = mat(C.leafD, {rough: 0.85});
    function clump(at, s) {
      put(site, new THREE.IcosahedronGeometry(s, 0), rnd() < 0.5 ? leaf : leafD, at.x, at.y, at.z, {ry: rnd() * 3});
      put(site, new THREE.SphereGeometry(s * 0.78, 10, 5, 0, TAU, 0, Math.PI / 3), snowM, at.x, at.y + s * 0.28, at.z);
    }
    /* the crown sits behind the lookouts, so their doors stay in sight */
    [[-3.2, 31, -3.4, 3.0], [3.4, 30, -3.0, 3.2], [0, 33.5, -3.8, 3.3], [-5.6, 28, -1.6, 2.4], [5.8, 27.6, -1.4, 2.5],
     [-1.6, 36.4, -2.6, 2.2], [2.4, 36.0, -2.4, 2.0], [-7.4, 31.5, -2.8, 1.9], [7.6, 32.5, -2.4, 2.0]].forEach(function (c) { clump(V(c[0], c[1], c[2]), c[3]); });
    /* bare boughs at the back, for the outline of the tree */
    [[205, 11.5, 11], [330, 16, 10], [160, 26.5, 8], [20, 24, 8.5]].forEach(function (b) {
      var a = b[0] * Math.PI / 180, d = V(Math.cos(a), 0, Math.sin(a)), z = V(0, 0, -1.4);
      var p0 = V(0, b[1], 0), p1 = p0.clone().addScaledVector(d, b[2] * 0.5).add(V(0, 1.6, 0)).add(z), p2 = p0.clone().addScaledVector(d, b[2]).add(V(0, 2.4, 0)).add(z);
      limb(site, p0, p1, 0.8, 0.45, bark); limb(site, p1, p2, 0.45, 0.15, bark);
      clump(p2.clone().add(V(0, 0.4, 0)), 1.5);
    });

    /* the stair round the trunk, from the ground to the workshop bough */
    var stepM = mat(C.woodD, {rough: 0.8}), ropeM = mat(C.rope, {rough: 0.9});
    (function () {
      var n = 34, prev = null;
      for (var k = 0; k < n; k++) {
        var y = 1.4 + k * 9.6 / n, a = -k * 0.36 + 2.6, r = trunkR(y) + 0.55;
        var at = V(Math.cos(a) * r, y, Math.sin(a) * r);
        box(site, 1.1, 0.12, 0.5, stepM, at.x, at.y, at.z, {ry: -a});
        var post = V(Math.cos(a) * (r + 0.5), y + 0.8, Math.sin(a) * (r + 0.5));
        if (prev) rod(site, prev, post, 0.035, ropeM);
        prev = post;
      }
    })();
    /* the lift: a basket on a rope, from the ground up to the library, and back */
    var lift = grp(site);
    var LIFT = V(-4.6, 0, -1.2), LIFT_TOP = 13.0;
    (function () {
      rod(site, V(LIFT.x, groundY(LIFT.x, LIFT.z), LIFT.z), V(LIFT.x, LIFT_TOP + 1.2, LIFT.z), 0.03, ropeM);
      box(site, 2.4, 0.3, 0.3, stepM, LIFT.x + 1.2, LIFT_TOP + 1.2, LIFT.z);
      put(site, new THREE.CylinderGeometry(0.35, 0.35, 0.2, 12), mat(C.dark, {rough: 0.4, metal: 0.6}), LIFT.x, LIFT_TOP + 1.0, LIFT.z, {rx: Math.PI / 2});
      put(lift, new THREE.CylinderGeometry(0.6, 0.45, 0.7, 10), mat(0xa8743f, {rough: 0.9}), 0, 0, 0);
      put(lift, new THREE.TorusGeometry(0.6, 0.05, 6, 16), mat(0xd2382c, {rough: 0.5}), 0, 0.35, 0, {rx: Math.PI / 2});
      [-1, 1].forEach(function (s) { rod(lift, V(s * 0.55, 0.35, 0), V(0, 1.3, 0), 0.02, ropeM); });
    })();

    /* --- the four parts and their 24 doors, sealed until their day --------------------------------------- */

    var vie = global.StationVie && global.StationVie.make({THREE: THREE, mat: mat, put: put, box: box, ball: ball, grp: grp, halo: halo, glowAt: glowAt});
    var parts = global.ArbreParties.make({THREE: THREE, V: V, UP: UP, rnd: rnd, mat: mat, put: put, box: box, ball: ball, grp: grp, rod: rod, limb: limb,
      halo: halo, glowAt: glowAt, drawn: drawn, site: site, snowM: snowM, bark: bark, clump: clump, trunkR: trunkR, groundY: groundY, TRUNK_H: TRUNK_H});
    var BUILD = [parts.words, parts.proof, parts.memory, parts.together];
    var weeks = data.world.villages.slice(0, 4).map(function (v, f) {
      return {def: v, i: f, hex: new THREE.Color(v.tone).getHex(), css: v.tone, days: []};
    });
    var recs = [], byDay = {}, pickables = [];
    var HIDDEN = new THREE.MeshBasicMaterial({transparent: true, opacity: 0, depthWrite: false});
    var frameM = mat(0x5a3c22, {rough: 0.7});
    var Q = new THREE.Quaternion();

    weeks.forEach(function (wk) {
      var built = BUILD[wk.i](wk);
      wk.label = built.label;
      data.days.filter(function (d) { return d.band === wk.def.id; }).slice(0, 6).forEach(function (d, idx) {
        var day = d.day, spot = built.spots[idx], hs = spot.host;
        hs.updateMatrixWorld(true);
        hs.getWorldQuaternion(Q);
        var face = V(0, 0, 1).applyQuaternion(Q); face.y = 0; face.normalize();
        /* the windows, dark while sealed, warm once open */
        var winM = new THREE.MeshStandardMaterial({color: 0x1c2232, emissive: 0xffc27a, emissiveIntensity: 0, roughness: 0.4});
        (spot.wins || []).forEach(function (w) {
          var geo = w.round ? new THREE.CircleGeometry(0.2, 14) : new THREE.PlaneGeometry(w.small ? 0.3 : 0.34, w.small ? 0.3 : 0.42);
          var pw = put(hs, geo, winM, w.x, w.y, w.z, {shadow: false, ry: w.ry || 0});
          pw.userData.keep = true;
        });
        var hatch = grp(hs, spot.door.x, spot.door.y, spot.door.z);
        hatch.userData.keep = true;
        /* the door: a frame, the glow of the inside behind it, the round door on its hinge with the day's number */
        put(hatch, new THREE.TorusGeometry(0.6, 0.08, 8, 32), frameM, 0, 0, 0.03, {shadow: false}).userData.keep = true;
        if (vie) vie.wreath(hatch);
        var innerM = new THREE.MeshBasicMaterial({color: 0xffe3b4, transparent: true, opacity: 0});
        put(hatch, new THREE.CircleGeometry(0.55, 28), innerM, 0, 0, 0.0, {shadow: false}).userData.keep = true;
        var c = document.createElement('canvas'); c.width = c.height = 256;
        var doorTex = new THREE.CanvasTexture(c); doorTex.encoding = THREE.sRGBEncoding;
        var doorM = new THREE.MeshStandardMaterial({color: 0xffffff, map: doorTex, emissive: 0xffffff, emissiveMap: doorTex,
          emissiveIntensity: 0.3, roughness: 0.7, side: THREE.DoubleSide});
        var hinge = grp(hatch, -0.56, 0, 0.06);
        hinge.userData.keep = true;
        put(hinge, new THREE.CircleGeometry(0.55, 28), doorM, 0.56, 0, 0, {shadow: false}).userData.keep = true;
        var lampM = new THREE.MeshBasicMaterial({color: 0xe0584f});
        put(hatch, new THREE.SphereGeometry(0.09, 10, 8), lampM, 0, 0.78, 0.05, {shadow: false}).userData.keep = true;
        hatch.updateMatrixWorld(true);
        var hatchAt = hatch.getWorldPosition(V(0, 0, 0));
        var lampGlow = halo(site, hatchAt.x, hatchAt.y + 0.78, hatchAt.z, 0xffc27a, 1.1);
        var flash = halo(site, hatchAt.x, hatchAt.y, hatchAt.z, 0xffe3b4, 3);
        var mid = hs.localToWorld(spot.hit.c.clone());

        var hit = new THREE.Mesh(new THREE.BoxGeometry(spot.hit.s[0], spot.hit.s[1], spot.hit.s[2]), HIDDEN);
        hit.position.copy(mid); hit.quaternion.copy(Q);
        hit.userData.day = day; site.add(hit); pickables.push(hit);

        var tc = document.createElement('canvas'); tc.width = tc.height = 128;
        var tex = new THREE.CanvasTexture(tc); tex.encoding = THREE.sRGBEncoding;
        var side = V(face.z, 0, -face.x);
        var down = spot.tagDown == null ? 1.05 : spot.tagDown;
        var r = {day: day, week: wk, idx: idx, body: hs, hinge: hinge, innerM: innerM, winM: winM, doorM: doorM, lampM: lampM,
                 lampGlow: lampGlow, flash: flash, slot: mid, hatchAt: hatchAt, face: face, side: side, state: '', painted: false,
                 t: 0, goal: 0, shake: 0, hover: 0, at: hatchAt.clone().addScaledVector(face, 1.3).add(V(0, -down, 0))};
        /* the door: planks, iron studs, its number in the week's colour */
        r.drawDoor = function () {
          var g = c.getContext('2d'), st = r.state;
          g.clearRect(0, 0, 256, 256);
          g.fillStyle = st === 'locked' ? '#6e4a2c' : '#9a6a3e'; g.fillRect(0, 0, 256, 256);
          g.strokeStyle = 'rgba(40,24,12,.55)'; g.lineWidth = 5;
          for (var x = 32; x < 256; x += 48) { g.beginPath(); g.moveTo(x, 0); g.lineTo(x, 256); g.stroke(); }
          g.lineWidth = 10; g.strokeStyle = '#3a2a1c';
          g.beginPath(); g.arc(128, 128, 116, 0, TAU); g.stroke();
          g.fillStyle = '#2a2018';
          [[60, 70], [196, 70], [60, 186], [196, 186]].forEach(function (p) { g.beginPath(); g.arc(p[0], p[1], 7, 0, TAU); g.fill(); });
          g.beginPath(); g.arc(196, 132, 10, 0, TAU); g.fillStyle = '#d9a441'; g.fill();
          g.fillStyle = '#fff6e4';
          g.beginPath(); g.arc(128, 132, 70, 0, TAU); g.fill();
          g.fillStyle = st === 'locked' ? '#4f5668' : wk.css;
          g.font = 'italic 800 ' + (day > 9 ? 84 : 100) + 'px Fraunces, Georgia, serif';
          g.textAlign = 'center'; g.textBaseline = 'middle';
          g.fillText(String(day), 128, 140);
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

/* the garland that winds round the trunk, from the roots to the crown */
    var bulbs;
    (function () {
      var pts = [], cols = [], tones = [0xffd36a, 0xff6a5a, 0x7ad7ff, 0x9dff8a];
      for (var y = 3, n = 0; y < 33; y += 0.12, n++) {
        var a = y * 0.55 + 0.4, r = (y > 13.2 && y < 25.6 ? 3.55 : trunkR(y) + 0.15) + 0.1;
        if (Math.sin(a) < -0.2) continue;
        pts.push(Math.cos(a) * r, y - 0.25 * Math.abs(Math.sin(n * 0.5)), Math.sin(a) * r);
        var c = new THREE.Color(tones[n % 4]); cols.push(c.r, c.g, c.b);
      }
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      bulbs = new THREE.Points(g, new THREE.PointsMaterial({size: 0.42, vertexColors: true, map: glowTex, transparent: true,
        depthWrite: false, blending: THREE.AdditiveBlending}));
      site.add(bulbs);
    })();

    /* the snow that falls */
    var snow;
    (function () {
      var pts = [];
      for (var i = 0; i < 1600; i++) pts.push((rnd() - 0.5) * 90, rnd() * 45, (rnd() - 0.5) * 70);
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      snow = new THREE.Points(g, new THREE.PointsMaterial({size: 0.22, color: 0xffffff, map: glowTex, transparent: true, depthWrite: false, opacity: 0.85}));
      site.add(snow);
    })();

    /* the star of the 24th, on top of the tree */
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
    put(site, new THREE.OctahedronGeometry(0.7, 0), mat(0xffd76a, {rough: 0.3, metal: 0.6, emissive: 0xffb43a, ei: 0.5}), 0, TOP, 0);
    var starG = halo(site, 0, TOP, 0, 0xffe7a8, 9, {map: starTex, depth: false});
    var starCore = halo(site, 0, TOP, 0, 0xffd36a, 4, {depth: false});

    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(function () { recs.forEach(function (r) { r.drawTag(); }); });
    }

    /* --- a door opens: it swings on its hinge, the inside lights, the windows warm --------------------- */

    function pose(r) {
      var t = r.t, k = ease(t);
      r.hinge.rotation.y = -1.95 * k;
      r.innerM.opacity = 0.95 * step(0.05, 0.5, t);
      r.winM.emissiveIntensity = 0.9 * k;
      r.lampM.color.setHex(r.state === 'open' ? 0x2fb67e : r.state === 'ready' ? 0xf2b441 : 0xe0584f);
      glowAt(r.flash, t > 0 && t < 1 ? Math.sin(t * Math.PI) * 0.55 : 0);
    }

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

    /* --- the camera ------------------------------------------------------------------------------------ */

    var AZ = [0.6, 2.55], POL = [0.6, 1.42];
    function azOf(v) { return Math.atan2(v.z, v.x); }
    var VIEWS = [{t: [0, 17.4, 0], az: 1.5, pol: 1.3, dist: 70, half: 24, tall: 26.5}];
    weeks.forEach(function (wk) {
      var lo = V(1e9, 1e9, 1e9), hi = V(-1e9, -1e9, -1e9), face = V(0, 0, 0);
      wk.days.forEach(function (r) { lo.min(r.hatchAt); hi.max(r.hatchAt); face.add(r.face); });
      var mid = lo.clone().add(hi).multiplyScalar(0.5), span = hi.clone().sub(lo);
      VIEWS.push({t: [mid.x, mid.y + 0.6, mid.z], az: clamp(azOf(face), AZ[0], AZ[1]), pol: 1.28,
                  dist: 20, half: Math.max(span.x, span.z) * 0.62 + 4.5, tall: span.y * 0.6 + 3.5});
    });
    var cam = {t: new THREE.Vector3(), az: 1.5, pol: 1.2, dist: 70};
    var goal = {t: new THREE.Vector3(), az: 1.5, pol: 1.2, dist: 70};
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
    function focus(r) { return {t: [r.slot.x, r.slot.y, r.slot.z], az: azOf(r.face), pol: 1.32, dist: 13, half: 5.6}; }
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

    var pending = [], inside = null;
    function later(fn, ms) { pending.push(setTimeout(fn, reduced ? 0 : ms)); }
    function hatchView(r) { return {t: [r.hatchAt.x, r.hatchAt.y, r.hatchAt.z], az: azOf(r.face), pol: Math.PI / 2 - 0.05, dist: 2.6, half: 0.9}; }
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

    /* --- the names of the four parts, and the chips to go to them ------------------------------------ */

    var labels = [];
    weeks.forEach(function (wk, i) {
      var el = document.createElement('button');
      el.type = 'button'; el.className = 'monde-label';
      el.style.setProperty('--tone', wk.css);
      var small = document.createElement('small'); small.textContent = page.tx(wk.def.sub);
      var strong = document.createElement('b'); strong.textContent = page.tx(wk.def.name);
      el.appendChild(small); el.appendChild(strong);
      el.addEventListener('click', function () { toZone(i); });
      host.appendChild(el);
      labels.push({el: el, small: small, at: wk.label, zone: i});
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
    chip(page.say('overview') || page.tx({en: 'The tree', fr: "L'arbre"}), '', overview);
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
    var tagAt = new THREE.Vector3();
    var snowPos = snow.geometry.attributes.position;
    function dress(dt) {
      var moving = false;
      recs.forEach(function (r) {
        if (r.t !== r.goal) {
          r.t = r.goal > r.t ? Math.min(r.goal, r.t + dt / 1.2) : r.goal;
          pose(r); moving = true;
        }
        var ready = r.state === 'ready', pulse = reduced ? 0.5 : 0.5 + 0.5 * Math.sin(clock * 2.6 + r.day);
        tagAt.copy(r.at);
        if (r.shake > 0) { r.shake = Math.max(0, r.shake - dt * 1.6); tagAt.addScaledVector(r.side, Math.sin(r.shake * 34) * 0.14 * r.shake); moving = true; }
        r.tag.position.copy(tagAt);
        r.hover += ((hoverDay === r.day ? 1 : 0) - r.hover) * Math.min(1, dt * 10);
        var size = (r.state === 'open' ? 0.66 : ready ? 0.95 + 0.06 * pulse : 0.82) * (narrow ? 1.6 : 1)
          * (mode.kind === 'over' ? 1.7 : mode.kind === 'zone' ? 1.0 : mode.day === r.day ? 0.62 : 0.5) * (1 + r.hover * 0.22);
        r.tag.scale.set(size, size, 1);
        r.ring.position.copy(tagAt);
        r.ring.scale.set(size * 2.4, size * 2.4, 1);
        glowAt(r.ring, ready ? 0.3 + 0.22 * pulse : 0);
        glowAt(r.lampGlow, ready ? 0.25 + 0.4 * pulse : r.state === 'open' ? 0.2 : 0.12);
      });
      /* the lift goes up to the second bough and down again */
      var lu = reduced ? 0.5 : 0.5 - 0.5 * Math.cos(clock * 0.18);
      lift.position.set(LIFT.x, groundY(LIFT.x, LIFT.z) + 0.4 + lu * (LIFT_TOP - 1.2), LIFT.z);
      /* each part's own life (arbre_parties.js), and the snow that falls */
      parts.tick(clock, reduced);
      if (!reduced) {
        for (var i = 0; i < snowPos.count; i++) {
          var y = snowPos.getY(i) - dt * (0.9 + (i % 7) * 0.12);
          if (y < 0) y += 45;
          snowPos.setY(i, y);
          snowPos.setX(i, snowPos.getX(i) + Math.sin(clock * 0.6 + i) * dt * 0.15);
        }
        snowPos.needsUpdate = true;
      }
      if (vie) vie.tick(clock, reduced);
      /* the more doors open, the warmer the tree; all 24, and its star shines */
      warmth.intensity += (opened / 24 * 2.2 - warmth.intensity) * Math.min(1, dt * 2);
      var all = opened >= 24 ? 1 : 0;
      var tw = reduced ? 1 : 1 + 0.08 * Math.sin(clock * 2.4);
      glowAt(starG, 0.25 + all * 0.6); starG.scale.set(9 * tw, 9 * tw, 1);
      glowAt(starCore, 0.3 + all * 0.4);
      bulbs.material.opacity = reduced ? 0.9 : 0.7 + 0.3 * Math.sin(clock * 1.7);
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
      views: function () { return ['station', 'aile1', 'aile2', 'aile3', 'aile4']; },
      view: function (name) { if (name === 'station') overview(); else toZone(Number(name.slice(-1)) - 1); },
      look: look,
      inside: function (day, stop) { enterInside(day, stop, true); return inside; },
      settle: function () {
        cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist;
        recs.forEach(function (r) { r.t = r.goal; pose(r); r.shake = 0; });
        if (inside) inside.settle();
        warmth.intensity = opened / 24 * 2.2; dirty = 3;
        place();
        dress(0.016);
      }
    };
  }

  global.SceneArbre = {mount: mount};
})(window);
