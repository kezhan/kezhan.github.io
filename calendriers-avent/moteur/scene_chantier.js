/* The building site, at dawn: the scene of the Claude Code calendar.
 *
 * The thread of that calendar is "structuring is always important", so the frame of the building
 * stands from the first day, and December fills it. One storey per week, one bay per day:
 *
 *   storey 1  Words           the brief, written down before anyone builds
 *   storey 2  Proof           what is measured and inspected before it goes in
 *   storey 3  Memory          the plans and the site log that every crew reads
 *   storey 4  Orchestration   many crews at once, one site office, the crane
 *
 * What a reader sees:
 *   - a locked day is an empty bay of the frame, with its number on a dashed tag;
 *   - a day that may open has a warm tag that pulses;
 *   - an opened day gets its wall: the crane lowers the panel into the bay, and its window lights;
 *   - around the building, the four site cabins of the four weeks; the building itself is main,
 *     and only inspected work is built into it; on the 24th the topping-out tree lights on the roof.
 *
 * Same calls as the other scenes (see apercu_du_monde.js). Needs three.js r128 (global `THREE`);
 * every texture is drawn on a canvas.
 */
(function (global) {
  'use strict';

  var TAU = Math.PI * 2;
  /* the building, in metres: six bays across the front, a ground floor, four storeys */
  var BW = 2.3, BAYS = 6, GF = 3.8, SH = 3.2, DEPTH = 9;
  var X0 = -BW * BAYS / 2, X1 = BW * BAYS / 2, ZF = 2.5, ZB = ZF - DEPTH, TOP = GF + 4 * SH;
  /* the crane, behind the building on its right */
  var CRANE = {x: 9.5, z: -8.5, mast: TOP + 8.5, jib: 21};

  var C = {concrete: 0xb9b3ab, concreteD: 0x8c867f, steel: 0x8d96a8, steelD: 0x4f5668, snow: 0xe8ecf6, mud: 0x8a7a6c,
           road: 0x5b5f6b, yellow: 0xf2b630, red: 0xd9483a, white: 0xf7f5f0, wood: 0xb08250, woodD: 0x7b5337,
           panel: 0xe9dcc6, panelD: 0xcdb898, glass: 0x2d3550, orange: 0xf07b2c, blue: 0x3a6fd8, green: 0x3f9a62,
           cabin: [0x3a6fd8, 0xd9483a, 0x6f5bd0, 0xf2b630], dark: 0x23262f, leaf: 0x2f6f47};

  var SITE = {
    over: {en: 'The building site', fr: 'Le chantier'},
    main: {en: 'main', fr: 'main'},
    signs: [{en: 'THE BRIEF', fr: 'LE CAHIER'}, {en: 'INSPECTION', fr: 'CONTRÔLE'},
            {en: 'PLANS & LOG', fr: 'PLANS & JOURNAL'}, {en: 'SITE OFFICE', fr: 'BUREAU DE CHANTIER'}],
    gate: {en: 'TICKETS IN', fr: 'ENTRÉE DES TICKETS'},
    rule: {en: 'only inspected work is built in', fr: "n'entre que le travail contrôlé"}
  };

  function mount(host, data, page) {
    var THREE = global.THREE;
    if (!THREE || !host || !data.world) return null;
    if (data.world.site) Object.keys(data.world.site).forEach(function (k) { SITE[k] = data.world.site[k]; });       /* a world may name its own signs */
    var reduced = global.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;
    var renderer;
    try {
      renderer = new THREE.WebGLRenderer({antialias: true, alpha: true, powerPreference: 'high-performance'});
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
    scene.fog = new THREE.Fog(0xf3d9c8, 70, 150);
    var camera = new THREE.PerspectiveCamera(32, 2, 0.5, 260);
    var site = new THREE.Group();
    scene.add(site);

    /* --- light: a cold dawn, a low warm sun from the left, the lamps of the site ------------ */

    scene.add(new THREE.HemisphereLight(0xd6d9f5, 0x8f7c72, 0.55));
    var sun = new THREE.DirectionalLight(0xffc59a, 1.05);
    sun.position.set(-34, 22, 26);
    sun.target.position.set(0, 0, -2);
    sun.castShadow = true;
    sun.shadow.mapSize.set(2048, 2048);
    sun.shadow.camera.left = -34; sun.shadow.camera.right = 34;
    sun.shadow.camera.top = 30; sun.shadow.camera.bottom = -26;
    sun.shadow.camera.near = 1; sun.shadow.camera.far = 120;
    sun.shadow.bias = -0.0005; sun.shadow.normalBias = 0.03;
    scene.add(sun); scene.add(sun.target);
    var warmth = new THREE.PointLight(0xffb36a, 0, 30, 2);
    warmth.position.set(0, GF + 2 * SH, ZF + 4);
    scene.add(warmth);

    /* --- small helpers --------------------------------------------------------------- */

    var seed = 20261201;
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
    function ease(x) { x = clamp(x, 0, 1); return 1 - Math.pow(1 - x, 3); }
    function step(a, b, x) { var t = clamp((x - a) / (b - a), 0, 1); return t * t * (3 - 2 * t); }
    function css(hex) { return '#' + ('000000' + hex.toString(16)).slice(-6); }
    var UP = new THREE.Vector3(0, 1, 0);

    var mats = {};
    function mat(color, o) {
      o = o || {};
      var key = o.own ? '' : [color, o.rough, o.metal, o.emissive, o.ei].join('|');
      if (key && mats[key]) return mats[key];
      var m = new THREE.MeshStandardMaterial({color: color, flatShading: true, roughness: o.rough == null ? 0.85 : o.rough,
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
    /* a box by its foot: (x, y, z) is the middle of its base */
    function box(p, w, h, d, m, x, y, z, o) { return put(p, new THREE.BoxGeometry(w, h, d), m, x, (y || 0) + h / 2, z, o); }
    function cyl(p, r1, r2, h, m, x, y, z, o) { return put(p, new THREE.CylinderGeometry(r1, r2, h, 10), m, x, (y || 0) + h / 2, z, o); }
    function ball(p, r, m, x, y, z, o) { return put(p, new THREE.SphereGeometry(r, 12, 8), m, x, y, z, o); }
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

    /* merge what was built under `root` into one mesh per material: a site is thousands of small boxes */
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
    function halo(p, x, y, z, color, size) {
      var s = new THREE.Sprite(new THREE.SpriteMaterial({map: glowTex, color: color, transparent: true, depthWrite: false,
        blending: THREE.AdditiveBlending, opacity: 0}));
      s.position.set(x, y, z); s.scale.set(size, size, 1); s.visible = false;
      p.add(s);
      return s;
    }
    function glowAt(s, k) { s.material.opacity = k; s.visible = k > 0.004; }
    /* a painted board: white letters on a colour, read from the front */
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
        emissive: 0xffffff, emissiveMap: tex, emissiveIntensity: o.lit || 0.25}));
      me.position.set(x, y, z); if (o.ry) me.rotation.y = o.ry;
      me.userData.keep = true;
      p.add(me);
      return me;
    }

    /* --- the ground: snow over the plot, a site road, the hoarding and its lights ----------------- */

    var ground = grp(site);
    box(ground, 70, 0.4, 52, mat(C.snow, {rough: 0.95}), 0, -0.4, -2);
    box(ground, 46, 0.06, 34, mat(0xe4dccf, {rough: 1}), 0, 0, -3);        /* the trodden plot */
    box(ground, 6, 0.08, 15, mat(C.road), 0, 0, 10.5);                      /* the road in, from the gate */
    box(ground, 30, 0.08, 4, mat(C.road), 0, 0, 6);                          /* along the front */
    [[-17, 10], [15, 11.5], [-20, -15], [21, -6], [6, 13.5], [-8, 14]].forEach(function (s) {
      ball(ground, 1.6 + rnd() * 1.2, mat(C.snow, {rough: 1}), s[0], -0.6, s[1]);
    });
    /* the hoarding: plywood panels on three sides, a gate on the front */
    var ply = mat(0xd8c3a0, {rough: 0.9}), post = mat(C.woodD);
    function hoard(x0, z0, x1, z1) {
      var len = Math.hypot(x1 - x0, z1 - z0), n = Math.round(len / 2.4), ry = -Math.atan2(z1 - z0, x1 - x0);
      for (var i = 0; i < n; i++) {
        var k = (i + 0.5) / n, x = x0 + (x1 - x0) * k, z = z0 + (z1 - z0) * k;
        box(ground, 2.3, 2.0, 0.1, ply, x, 0, z, {ry: ry});
        box(ground, 0.12, 2.2, 0.12, post, x - Math.cos(ry) * 1.2, 0, z + Math.sin(ry) * 1.2);
      }
    }
    hoard(-23, 15, -23, -20); hoard(23, 15, 23, -20); hoard(-23, -20, 23, -20);
    hoard(-23, 15, -4.5, 15); hoard(4.5, 15, 23, 15);
    box(ground, 0.35, 4.2, 0.35, mat(C.yellow), -4.4, 0, 15); box(ground, 0.35, 4.2, 0.35, mat(C.yellow), 4.4, 0, 15);
    box(ground, 9.2, 0.3, 0.3, mat(C.yellow), 0, 4.0, 15);
    board(ground, page.tx(SITE.gate), 0, 3.45, 15.2, C.dark, 6.2, {h: 0.85, lit: 0.35, ink: '#ffd36a'});
    /* the string of lights along the top of the hoarding */
    var bulbs = [];
    (function () {
      var pts = [], cols = [], tones = [0xffd36a, 0xff6a5a, 0x7ad7ff, 0x9dff8a];
      function line(x0, z0, x1, z1) {
        var len = Math.hypot(x1 - x0, z1 - z0), n = Math.round(len / 0.9);
        for (var i = 0; i <= n; i++) {
          var k = i / n, c = new THREE.Color(tones[(pts.length / 3) % 4]);
          pts.push(x0 + (x1 - x0) * k, 2.1 + Math.sin(k * Math.PI * Math.max(1, Math.round(len / 3))) * -0.12, z0 + (z1 - z0) * k);
          cols.push(c.r, c.g, c.b);
        }
      }
      line(-23, 15, -4.5, 15); line(4.5, 15, 23, 15); line(-23, 15, -23, -20); line(23, 15, 23, -20);
      var g = new THREE.BufferGeometry();
      g.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
      g.setAttribute('color', new THREE.Float32BufferAttribute(cols, 3));
      var p = new THREE.Points(g, new THREE.PointsMaterial({size: 0.32, vertexColors: true, map: glowTex, transparent: true,
        depthWrite: false, blending: THREE.AdditiveBlending}));
      site.add(p); bulbs.push(p);
    })();
    /* floodlights on masts at the corners of the plot */
    [[-21, 13], [21, 13], [-21, -18]].forEach(function (s) {
      cyl(ground, 0.12, 0.16, 9, mat(C.steelD), s[0], 0, s[1]);
      box(ground, 1.2, 0.5, 0.4, mat(C.dark), s[0], 9, s[1]);
      glowAt(halo(site, s[0], 9.2, s[1] + 0.3, 0xfff0d0, 4.5), 0.55);
    });
    bake(ground);

    /* --- the building: its frame stands from the first day ------------------------------------- */

    var skel = grp(site);
    var conc = mat(C.concrete), concD = mat(C.concreteD);
    /* the slabs: ground, the four storeys, the roof */
    for (var k = 0; k <= 4; k++) {
      var y = GF + k * SH - 0.32;
      box(skel, X1 - X0 + 0.5, 0.32, DEPTH + 0.5, conc, 0, y, (ZF + ZB) / 2);
    }
    box(skel, X1 - X0 + 0.5, 0.25, DEPTH + 0.5, concD, 0, 0, (ZF + ZB) / 2);
    /* the columns, front and back, and one row in the middle */
    for (var i = 0; i <= BAYS; i++) {
      var x = X0 + i * BW;
      [ZF, (ZF + ZB) / 2, ZB].forEach(function (z) { box(skel, 0.42, TOP, 0.42, concD, x, 0, z); });
    }
    /* the core: stairs and lift shaft, closed, at the back */
    box(skel, 3.2, TOP + 1.6, 3.0, conc, X0 + 3.4, 0, ZB + 1.9);
    /* the ground floor is done: glass between the columns, the door, the name */
    var glassM = mat(C.glass, {rough: 0.25, metal: 0.4, emissive: 0xffb76a, ei: 0.12});
    for (i = 0; i < BAYS; i++) {
      var gx = X0 + (i + 0.5) * BW;
      box(skel, BW - 0.45, GF - 0.6, 0.12, glassM, gx, 0.25, ZF + 0.05);
      box(skel, BW - 0.3, 0.14, 0.2, mat(C.steelD), gx, GF - 0.48, ZF + 0.08);
    }
    box(skel, 2.4, 0.2, 1.6, concD, 0, 0, ZF + 0.9);
    box(skel, 2.6, 0.12, 1.8, mat(C.steelD), 0, GF - 0.62, ZF + 0.9);
    board(site, page.tx(SITE.main), 0, GF - 0.05, ZF + 0.25, C.green, 3.2, {h: 0.62, lit: 0.45});
    /* the rule, on the hoarding of the door: only inspected work goes in */
    board(site, page.tx(SITE.rule), 0, 0.85, ZF + 1.85, C.white, 4.6, {h: 0.42, lit: 0.2, ink: '#1d2433'});
    box(skel, 4.8, 0.05, 0.08, mat(C.woodD), 0, 1.06, ZF + 1.8);
    bake(skel);

    /* the scaffolding on the front of each storey: there while the storey has an empty bay */
    var scaffolds = [];
    var tube = mat(0xb8c0cf, {metal: 0.5, rough: 0.5}), plank = mat(C.wood);
    for (var s = 0; s < 4; s++) {
      var sc = grp(site), y0 = GF + s * SH, zs = ZF + 1.1;
      for (i = 0; i <= BAYS * 2; i++) {
        var sx = X0 - 0.4 + i * (X1 - X0 + 0.8) / (BAYS * 2);
        rod(sc, [sx, y0 - 0.3, zs], [sx, y0 + SH - 0.3, zs], 0.04, tube);
        rod(sc, [sx, y0 - 0.3, zs + 0.9], [sx, y0 + SH - 0.3, zs + 0.9], 0.04, tube);
      }
      rod(sc, [X0 - 0.4, y0 + SH - 0.35, zs + 0.9], [X1 + 0.4, y0 + SH - 0.35, zs + 0.9], 0.04, tube);
      rod(sc, [X0 - 0.4, y0 + 1.05, zs + 0.9], [X1 + 0.4, y0 + 1.05, zs + 0.9], 0.04, tube);
      box(sc, X1 - X0 + 0.8, 0.06, 1.0, plank, 0, y0 - 0.32, zs + 0.45);
      for (i = 0; i < BAYS; i++) {
        var bx0 = X0 + i * BW, bx1 = bx0 + BW;
        rod(sc, [bx0, y0 - 0.3, zs + 0.9], [bx1, y0 + SH - 0.35, zs + 0.9], 0.03, tube);
      }
      bake(sc);
      scaffolds.push(sc);
    }

    /* --- the weeks and their days -------------------------------------------------------------- */

    var weeks = data.world.villages.slice(0, 4).map(function (v, f) {
      return {def: v, i: f, hex: new THREE.Color(v.tone).getHex(), css: v.tone, days: []};
    });
    var recs = [], byDay = {}, pickables = [];
    var HIDDEN = new THREE.MeshBasicMaterial({transparent: true, opacity: 0, depthWrite: false});
    var panelD = mat(C.panelD);
    weeks.forEach(function (wk) {
      data.days.filter(function (d) { return d.band === wk.def.id; }).slice(0, 6).forEach(function (d, idx) {
        var day = d.day, cx = X0 + (idx + 0.5) * BW, y0 = GF + wk.i * SH;
        /* the panel: a wall with its window, built off-site and lowered into the bay */
        var body = grp(site, cx, y0, ZF + 0.02);
        var panelM = mat(new THREE.Color(wk.hex).lerp(new THREE.Color(0xfff4e6), 0.42).getHex(), {rough: 0.8});
        var ww = 1.4, wh = 1.75, sill = 0.62, pw = BW - 0.44, ph = SH - 0.34;
        box(body, pw, sill, 0.22, panelM, 0, 0, 0);
        box(body, pw, ph - sill - wh, 0.22, panelM, 0, sill + wh, 0);
        box(body, (pw - ww) / 2, wh, 0.22, panelM, -(pw + ww) / 4, sill, 0);
        box(body, (pw - ww) / 2, wh, 0.22, panelM, (pw + ww) / 4, sill, 0);
        box(body, pw + 0.04, 0.1, 0.3, mat(wk.hex, {rough: 0.6}), 0, ph - 0.1, 0.02);   /* the band of its week */
        box(body, ww + 0.1, 0.08, 0.34, panelD, 0, sill - 0.08, 0.04);
        bake(body);
        /* behind the window, once the bay is built: a lit room and the card of the day's practice */
        var title = (data.world.titles || [])[day - 1] || '';
        var winTex = drawn(256, 320, function (g, w, h) {
          var grd = g.createLinearGradient(0, 0, 0, h);
          grd.addColorStop(0, '#ffd9a0'); grd.addColorStop(1, '#f6a95e');
          g.fillStyle = grd; g.fillRect(0, 0, w, h);
          g.fillStyle = 'rgba(90,50,30,.25)'; g.fillRect(0, h * 0.78, w, h * 0.22);          /* a desk */
          g.fillStyle = '#fffaf0'; g.fillRect(22, 34, w - 44, h * 0.62);                       /* the card */
          g.fillStyle = wk.css; g.fillRect(22, 34, w - 44, 10);
          g.fillStyle = wk.css; g.font = '800 20px "JetBrains Mono", Consolas, monospace';
          g.fillText('DAY ' + day, 36, 76);
          g.fillStyle = '#1d2433'; g.font = '700 25px Fraunces, Georgia, serif';
          var words = title.split(' '), line = '', y = 112;
          words.forEach(function (wd) {
            var t = line ? line + ' ' + wd : wd;
            if (g.measureText(t).width > w - 72 && line) { g.fillText(line, 36, y); line = wd; y += 30; } else line = t;
          });
          if (line) g.fillText(line, 36, y);
        });
        var lightM = new THREE.MeshStandardMaterial({color: 0xffffff, map: winTex, emissive: 0xffffff, emissiveMap: winTex,
          emissiveIntensity: 0, roughness: 0.5});
        var pane = put(body, new THREE.PlaneGeometry(ww, wh), lightM, 0, sill + wh / 2, 0.05, {shadow: false});
        pane.userData.keep = true;
        var glow = halo(site, cx, y0 + sill + wh / 2, ZF + 0.7, 0xffc27a, 2.4);
        /* the hook and its cable, while the panel comes down */
        var cable = rod(site, [cx, y0 + ph + 0.1, ZF + 0.02], [cx, y0 + ph + 6, ZF + 0.02], 0.025, mat(C.dark));
        cable.visible = false;

        var hit = new THREE.Mesh(new THREE.BoxGeometry(BW - 0.2, SH - 0.2, 1.2), HIDDEN);
        hit.position.set(cx, y0 + SH / 2 - 0.2, ZF + 0.3);
        hit.userData.day = day; site.add(hit); pickables.push(hit);

        var c = document.createElement('canvas'); c.width = c.height = 128;
        var tex = new THREE.CanvasTexture(c); tex.encoding = THREE.sRGBEncoding;
        var r = {day: day, week: wk, idx: idx, body: body, pane: pane, lightM: lightM, glow: glow, cable: cable,
                 x: cx, y0: y0, state: '', painted: false, t: 0, goal: 0, shake: 0, hover: 0,
                 at: new THREE.Vector3(cx - BW / 2 + 0.42, y0 + 0.55, ZF + 1.9)};
        r.drawTag = function () {
          var g = c.getContext('2d'), st = r.state;
          g.clearRect(0, 0, 128, 128);
          g.beginPath(); g.arc(64, 64, 52, 0, TAU);
          if (st === 'open') { g.fillStyle = wk.css; g.fill(); g.lineWidth = 7; g.strokeStyle = '#ffffff'; g.stroke(); g.fillStyle = '#ffffff'; }
          else if (st === 'ready') { g.fillStyle = '#fff6dc'; g.fill(); g.lineWidth = 8; g.strokeStyle = '#f2b441'; g.stroke(); g.fillStyle = '#2a1c14'; }
          else {
            g.fillStyle = 'rgba(40,30,48,.62)'; g.fill();
            g.setLineDash([10, 7]); g.lineWidth = 5; g.strokeStyle = 'rgba(255,255,255,.85)'; g.stroke(); g.setLineDash([]);
            g.fillStyle = 'rgba(255,255,255,.95)';
          }
          g.font = 'italic 800 ' + (day > 9 ? 58 : 68) + 'px Fraunces, Georgia, serif';
          g.textAlign = 'center'; g.textBaseline = 'middle';
          g.fillText(String(day), 64, 69);
          tex.needsUpdate = true;
        };
        r.ring = halo(site, r.at.x, r.at.y, r.at.z, 0xffcf6a, 1);
        r.ring.renderOrder = 8;
        r.tag = new THREE.Sprite(new THREE.SpriteMaterial({map: tex, transparent: true, depthWrite: false, depthTest: false}));
        r.tag.renderOrder = 9; r.tag.userData.day = day; r.tag.position.copy(r.at);
        site.add(r.tag); pickables.push(r.tag);
        recs.push(r); byDay[day] = r; wk.days.push(r);
      });
    });

    /* --- the four site cabins, one per week, and what stands by each ------------------------------ */

    var yard = grp(site);
    function figure(p, x, z, vest, helmet, ry) {
      var g = grp(p, x, 0, z, ry || 0);
      box(g, 0.16, 0.8, 0.18, mat(0x2b3140), -0.1, 0, 0); box(g, 0.16, 0.8, 0.18, mat(0x2b3140), 0.1, 0, 0);
      box(g, 0.46, 0.68, 0.28, mat(vest, {rough: 0.6}), 0, 0.8, 0);
      box(g, 0.5, 0.06, 0.3, mat(0xdfe6ef), 0, 1.15, 0);
      ball(g, 0.15, mat(0xe8bf9a), 0, 1.67, 0);
      put(g, new THREE.SphereGeometry(0.18, 10, 6, 0, TAU, 0, Math.PI / 2), mat(helmet, {rough: 0.4}), 0, 1.72, 0);
      return g;
    }
    function cabin(x, z, ry, color, sign) {
      var g = grp(yard, x, 0, z, ry);
      box(g, 6.2, 0.35, 2.8, mat(C.dark), 0, 0, 0);
      box(g, 6.0, 2.5, 2.6, mat(color, {rough: 0.7}), 0, 0.35, 0);
      box(g, 6.3, 0.2, 2.9, mat(C.snow, {rough: 1}), 0, 2.85, 0);
      box(g, 0.9, 1.9, 0.08, mat(C.dark), -2.2, 0.35, 1.32);
      [-0.6, 1.2].forEach(function (wx) {
        box(g, 1.3, 0.9, 0.06, mat(0x3a3f57, {emissive: 0xffc27a, ei: 0.55}), wx, 1.4, 1.32);
      });
      box(g, 0.5, 0.35, 1.0, mat(C.steelD), 2.4, 0.35, 1.7); /* the steps */
      board(g, sign, 0.3, 2.55, 1.34, C.dark, 3.4, {h: 0.4, lit: 0.4, ink: '#ffd36a'});
      glowAt(halo(g, 0.3, 1.8, 2.2, 0xffc27a, 3.2), 0.35);
      return g;
    }
    /* week 1, the brief: the architect's cabin and its drawing easel; a letter goes in */
    var cab1 = cabin(-14.5, 6.5, 0.25, weeks[0].hex, page.tx(SITE.signs[0]));
    (function () {
      var g = grp(yard, -10.6, 0, 8.3, 0.2);
      rod(g, [-0.6, 0, 0.3], [0, 2.3, 0], 0.04, mat(C.woodD)); rod(g, [0.6, 0, 0.3], [0, 2.3, 0], 0.04, mat(C.woodD));
      rod(g, [0, 0, -0.5], [0, 2.3, 0], 0.04, mat(C.woodD));
      var blue = drawn(256, 192, function (c, w, h) {
        c.fillStyle = '#1f4fa8'; c.fillRect(0, 0, w, h);
        c.strokeStyle = 'rgba(255,255,255,.85)'; c.lineWidth = 3;
        c.strokeRect(30, 40, 196, 120); c.beginPath();
        for (var i = 1; i < 6; i++) { c.moveTo(30 + i * 196 / 6, 40); c.lineTo(30 + i * 196 / 6, 160); }
        for (i = 1; i < 4; i++) { c.moveTo(30, 40 + i * 30); c.lineTo(226, 40 + i * 30); }
        c.stroke();
        c.fillStyle = '#ffffff'; c.font = '700 20px monospace'; c.fillText('BRIEF', 30, 28);
      });
      var sheet = new THREE.Mesh(new THREE.PlaneGeometry(1.6, 1.2), new THREE.MeshStandardMaterial({map: blue, roughness: 0.8}));
      sheet.position.set(0, 1.6, 0.17); sheet.rotation.x = -0.12; sheet.userData.keep = true; g.add(sheet);
    })();
    figure(yard, -12.2, 9.0, C.white, C.white, 0.6);          /* the architect, in front of the brief */
    /* week 2, the proof: the inspection cabin, a surveyor's level on its tripod, the light at the door */
    var cab2 = cabin(14.5, 6.5, -0.25, weeks[1].hex, page.tx(SITE.signs[1]));
    (function () {
      var g = grp(yard, 9.5, 0, 9.0, -0.4);
      [0, 2.1, 4.2].forEach(function (a) { rod(g, [Math.cos(a) * 0.5, 0, Math.sin(a) * 0.5], [0, 1.35, 0], 0.03, mat(C.yellow)); });
      box(g, 0.34, 0.24, 0.5, mat(C.dark), 0, 1.35, 0);
      cyl(g, 0.08, 0.08, 0.5, mat(C.steel), 0, 1.47, 0.0, {rx: Math.PI / 2});
    })();
    figure(yard, 10.4, 10.0, C.red, C.red, -0.9);              /* the inspector */
    var lamp = grp(site, 2.2, 0, ZF + 1.6);
    cyl(lamp, 0.06, 0.06, 2.2, mat(C.dark), 0, 0, 0);
    box(lamp, 0.36, 0.8, 0.3, mat(C.dark), 0, 2.2, 0);
    var redL = ball(lamp, 0.11, mat(0x401010, {emissive: C.red, ei: 0.15}), 0, 2.78, 0.16);
    var greenL = ball(lamp, 0.11, mat(0x0f3a1c, {own: true, emissive: 0x3fe07a, ei: 1.2}), 0, 2.42, 0.16);
    redL.userData.keep = greenL.userData.keep = true;
    /* week 3, memory: two stacked containers for the plans, and the site log on its board */
    var cab3 = grp(yard, -16.5, 0, -10.5, 0.15);
    box(cab3, 6.0, 2.6, 2.5, mat(weeks[2].hex, {rough: 0.7}), 0, 0, 0);
    box(cab3, 6.0, 2.6, 2.5, mat(new THREE.Color(weeks[2].hex).multiplyScalar(0.72).getHex(), {rough: 0.7}), 0.3, 2.6, -0.1);
    box(cab3, 6.1, 0.2, 2.6, mat(C.snow, {rough: 1}), 0.3, 5.2, -0.1);
    for (i = 0; i < 9; i++) box(cab3, 0.06, 2.4, 0.06, mat(0x3a3770), -2.8 + i * 0.7, 0.1, 1.27);
    board(cab3, page.tx(SITE.signs[2]), 0.3, 4.4, 1.17, C.dark, 3.6, {h: 0.42, lit: 0.4, ink: '#ffd36a'});
    (function () {
      var g = grp(yard, -11.8, 0, -7.0, 0.5);
      box(g, 0.1, 2.0, 0.1, mat(C.woodD), -1, 0, 0); box(g, 0.1, 2.0, 0.1, mat(C.woodD), 1, 0, 0);
      var log = drawn(256, 160, function (c, w, h) {
        c.fillStyle = '#efe6d2'; c.fillRect(0, 0, w, h);
        c.fillStyle = '#3b3550'; c.font = '700 18px monospace'; c.fillText('SITE LOG', 14, 26);
        c.fillStyle = 'rgba(59,53,80,.55)';
        for (var i = 0; i < 6; i++) c.fillRect(14, 42 + i * 18, 120 + (i * 37) % 100, 6);
        ['#f2b630', '#7ad7ff', '#ff8a7a'].forEach(function (col, j) { c.fillStyle = col; c.fillRect(170 + j * 22, 40 + j * 30, 18, 24); });
      });
      var sheet = new THREE.Mesh(new THREE.PlaneGeometry(2.2, 1.35), new THREE.MeshStandardMaterial({map: log, roughness: 0.9}));
      sheet.position.set(0, 1.55, 0.06); sheet.userData.keep = true; g.add(sheet);
    })();
    /* week 4, orchestration: the site office at the foot of the crane, crews on the storeys */
    var cab4 = cabin(15.5, -5.5, -0.6, weeks[3].hex, page.tx(SITE.signs[3]));
    figure(yard, 12.6, -3.0, C.orange, C.white, -1.2);          /* the foreman */
    bake(yard);

    /* the crane: a lattice mast, a jib that turns, a trolley and its hook */
    var crane = grp(site, CRANE.x, 0, CRANE.z);
    var craneY = mat(C.yellow, {rough: 0.55, metal: 0.2});
    box(crane, 4, 1.0, 4, concD, 0, 0, 0);
    (function () {
      var m = grp(crane), a = 0.85;
      [[-a, -a], [a, -a], [a, a], [-a, a]].forEach(function (c) { rod(m, [c[0], 1, c[1]], [c[0], CRANE.mast, c[1]], 0.07, craneY); });
      for (var y = 1; y < CRANE.mast; y += 1.7) {
        rod(m, [-a, y, -a], [a, y + 1.7, -a], 0.035, craneY); rod(m, [a, y, a], [-a, y + 1.7, a], 0.035, craneY);
        rod(m, [-a, y, a], [-a, y + 1.7, -a], 0.035, craneY); rod(m, [a, y, -a], [a, y + 1.7, a], 0.035, craneY);
      }
      bake(m);
    })();
    var slew = grp(crane, 0, CRANE.mast, 0);
    box(slew, 2.2, 2.0, 2.2, mat(0xe8e2d4), -0.4, 0, 0);                   /* the cab */
    box(slew, 0.9, 0.6, 0.06, mat(0x3a3f57, {emissive: 0xffc27a, ei: 0.4}), 0.3, 1.0, 1.12);
    (function () {
      var j = grp(slew, 0, 2.0), L = CRANE.jib;
      rod(j, [0, 0, -0.5], [L, 0, -0.5], 0.06, craneY); rod(j, [0, 0, 0.5], [L, 0, 0.5], 0.06, craneY);
      rod(j, [0, 1.3, 0], [L, 0.2, 0], 0.06, craneY);
      for (var x = 0; x < L; x += 1.5) { rod(j, [x, 0, -0.5], [x + 1.5, 0, 0.5], 0.03, craneY); rod(j, [x, 0, 0.5], [x + 0.75, 1.3 - x / L * 1.1, 0], 0.03, craneY); }
      rod(j, [0, 0, 0], [-7, 0, 0], 0.08, craneY);
      box(j, 2.0, 1.6, 1.6, concD, -6.2, -0.8, 0);                        /* the counterweight */
      rod(j, [0, 0, 0], [0, 4.2, 0], 0.07, craneY);
      rod(j, [0, 4.2, 0], [L * 0.7, 0.2, 0], 0.025, mat(C.dark)); rod(j, [0, 4.2, 0], [-6.5, 0.2, 0], 0.025, mat(C.dark));
      bake(j);
    })();
    var trolley = grp(slew, 12, 1.85, 0);
    box(trolley, 0.8, 0.3, 1.1, mat(C.dark), 0, -0.3, 0);
    var hookLine = rod(trolley, [0, -0.3, 0], [0, -8, 0], 0.03, mat(C.dark));
    var hook = grp(trolley, 0, -8, 0);
    box(hook, 0.5, 0.5, 0.3, mat(C.red), 0, -0.5, 0);
    box(hook, 1.9, 0.12, 0.12, mat(C.dark), 0, -0.7, 0);
    glowAt(halo(slew, 0.2, 2.6, 0, 0xff4a3a, 1.2), 0.8);                   /* the red light on the cab */
    var tip = halo(slew, CRANE.jib, 2.2, 0, 0xff4a3a, 1.4);

    /* crews at work on the storeys (the sessions), and the topping-out tree on the roof */
    var crews = [];
    [[-4.6, 0, 0xf07b2c], [1.2, 0, 0xf07b2c], [-1.6, 1, 0x3a6fd8], [4.2, 1, 0x3a6fd8], [-5.4, 2, 0xf07b2c],
     [3.0, 2, 0xf2b630], [-2.8, 3, 0x3a6fd8], [5.2, 3, 0xf07b2c]].forEach(function (c, n) {
      var f = figure(site, c[0], ZF - 1.6 - (n % 3) * 1.3, c[2], 0xffffff, n * 0.9);
      f.position.y = GF + c[1] * SH;
      crews.push({g: f, s: c[1], base: f.position.x, ph: n});
    });
    var tree = grp(site, X1 - 1.6, TOP, ZF - 2.0);
    cyl(tree, 0.08, 0.1, 0.5, mat(C.woodD), 0, 0, 0);
    [[1.0, 1.2, 0.4], [0.78, 1.0, 1.05], [0.52, 0.85, 1.6]].forEach(function (l) {
      put(tree, new THREE.ConeGeometry(l[0], l[1], 9), mat(C.leaf), 0, l[2] + l[1] / 2, 0);
    });
    var star = ball(tree, 0.16, new THREE.MeshStandardMaterial({color: 0xffe08a, emissive: 0xffd36a, emissiveIntensity: 0.2}), 0, 2.55, 0);
    star.userData.keep = true;
    var treeGlow = halo(tree, 0, 1.4, 0, 0xffd36a, 4.5);

    /* snow, falling slowly */
    var flakes = null;
    if (!reduced) {
      var fp = [], fv = [];
      for (i = 0; i < 700; i++) { fp.push((rnd() - 0.5) * 70, rnd() * 34, (rnd() - 0.5) * 54 - 2); fv.push(0.5 + rnd() * 0.9); }
      var fg = new THREE.BufferGeometry();
      fg.setAttribute('position', new THREE.Float32BufferAttribute(fp, 3));
      flakes = new THREE.Points(fg, new THREE.PointsMaterial({color: 0xffffff, size: 0.18, map: glowTex, transparent: true, depthWrite: false, opacity: 0.85}));
      flakes.userData.v = fv;
      site.add(flakes);
    }

    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(function () { recs.forEach(function (r) { r.drawTag(); }); });
    }

    /* --- how a panel arrives: lowered from the hook, set in its bay, its window lights ----------------- */

    function pose(r) {
      var t = r.t, a = clamp(t / 0.75, 0, 1), e = ease(a), b = step(0.7, 1, t);
      r.body.visible = t > 0.001;
      r.body.position.y = r.y0 + (1 - e) * 7.5;
      r.body.position.z = ZF + 0.02 + (1 - e) * 1.6;
      r.body.rotation.y = Math.sin(a * 9) * 0.06 * (1 - a);
      r.cable.visible = t > 0.001 && t < 0.78;
      r.cable.position.y = r.y0 + SH + (1 - e) * 7.5 + 2.5;
      r.lightM.emissiveIntensity = b * 0.85;
      r.lightM.color.setHex(b > 0.01 ? 0xffffff : 0x2a2433);
      glowAt(r.glow, b * 0.55);
    }

    /* --- states ------------------------------------------------------------------------------------ */

    var opened = 0, dirty = 3, lastLowered = null;
    function paint() {
      opened = 0;
      recs.forEach(function (r) {
        var st = page.state(r.day);
        if (st === 'open') opened++;
        if (st === r.state && r.painted) return;
        var first = !r.painted;
        r.state = st; r.painted = true;
        r.goal = st === 'open' ? 1 : 0;
        if (first || reduced || r.goal === 0) { r.t = r.goal; pose(r); }
        else lastLowered = r;
        r.drawTag();
      });
      /* the scaffolding stands in front of a storey until its first bay is built; then the cards can be read */
      weeks.forEach(function (wk) {
        scaffolds[wk.i].visible = !wk.days.some(function (r) { return r.state === 'open'; });
      });
      dirty = 3;
    }

    /* --- the camera: a point looked at, an angle around it, a distance ----------------------------- */

    var AZ = [0.75, 2.4], POL = [0.72, 1.48];
    var VIEWS = [{t: [0, 7.5, -0.5], az: 1.18, pol: 1.13, dist: 64, half: 27, tall: 15}];
    weeks.forEach(function (wk) {
      VIEWS.push({t: [0, GF + wk.i * SH + SH / 2, ZF], az: 1.42, pol: 1.4, dist: 20, half: 9.5});
    });
    var cam = {t: new THREE.Vector3(), az: 1.18, pol: 1.13, dist: 64};
    var goal = {t: new THREE.Vector3(), az: 1.18, pol: 1.13, dist: 64};
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
    function focus(r) { return {t: [r.x, r.y0 + SH / 2, ZF], az: 1.42, pol: 1.4, dist: 11, half: 4.6}; }
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

    /* a click on a day: go to its bay; lower its panel if it may; tell the page */
    var pending = [];
    function later(fn, ms) { pending.push(setTimeout(fn, reduced ? 0 : ms)); }
    function knock(day) {
      var r = byDay[day];
      if (!r) return;
      pending.forEach(clearTimeout); pending = [];
      look(day);
      later(function () {
        var st = page.state(day);
        if (st === 'locked') { r.shake = 1; page.refuse(day); return; }
        if (st === 'ready') { page.mark(day); paint(); later(function () { page.enter(day); }, 2600); }
        else later(function () { page.enter(day); }, 500);
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
    /* the wheel and two fingers bring the camera closer, within reason */
    canvas.addEventListener('wheel', function (e) {
      e.preventDefault();
      goal.dist = clamp(goal.dist * (1 + Math.sign(e.deltaY) * 0.1), 7, 110);
    }, {passive: false});

    /* --- the names of the four weeks, and the chips to go to them -------------------------------------- */

    var labels = [];
    var anchors = [cab1.position, cab2.position, cab3.position, cab4.position];
    weeks.forEach(function (wk, i) {
      var el = document.createElement('button');
      el.type = 'button'; el.className = 'monde-label';
      el.style.setProperty('--tone', wk.css);
      var small = document.createElement('small'); small.textContent = page.tx(wk.def.sub);
      var strong = document.createElement('b'); strong.textContent = page.tx(wk.def.name);
      el.appendChild(small); el.appendChild(strong);
      el.addEventListener('click', function () { toZone(i); });
      host.appendChild(el);
      var a = anchors[i];
      labels.push({el: el, small: small, at: new THREE.Vector3(a.x - (i === 2 ? 5.5 : 0), i === 2 ? 5.8 : i === 1 ? 3.4 : 4.4, a.z), zone: i,
                   storey: new THREE.Vector3(X0 - 1.2, GF + i * SH + SH - 0.4, ZF + 1)});
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
        b.setAttribute('aria-pressed', (i === 0 ? mode.kind === 'over' : mode.zone === i - 1) ? 'true' : 'false');
      });
    }
    var hint = document.createElement('p');
    hint.className = 'monde-hint';
    hint.textContent = page.say('hint');
    bar.insertBefore(hint, bar.firstChild);

    /* --- the loop ---------------------------------------------------------------------------------------- */

    var running = false, visible = true, paused = false, raf = 0, last = 0, clock = 0, slewTo = -2.45, slewNow = -2.45;
    var tagAt = new THREE.Vector3();
    function dress(dt) {
      var moving = false;
      recs.forEach(function (r) {
        if (r.t !== r.goal) {
          r.t = r.goal > r.t ? Math.min(r.goal, r.t + dt / 2.4) : r.goal;
          pose(r); moving = true;
        }
        var ready = r.state === 'ready', pulse = reduced ? 0.5 : 0.5 + 0.5 * Math.sin(clock * 2.6 + r.day);
        tagAt.copy(r.at);
        if (r.shake > 0) { r.shake = Math.max(0, r.shake - dt * 1.6); tagAt.x += Math.sin(r.shake * 34) * 0.12 * r.shake; moving = true; }
        r.tag.position.copy(tagAt);
        r.hover += ((hoverDay === r.day ? 1 : 0) - r.hover) * Math.min(1, dt * 10);
        var size = (r.state === 'open' ? 0.62 : ready ? 0.92 + 0.06 * pulse : 0.8) * (narrow ? 1.6 : 1)
          * (mode.kind === 'over' ? 1.35 : mode.kind === 'zone' ? 0.85 : mode.day === r.day ? 0.6 : 0.5) * (1 + r.hover * 0.22);
        r.tag.scale.set(size, size, 1);
        r.ring.position.copy(tagAt);
        r.ring.scale.set(size * 2.4, size * 2.4, 1);
        glowAt(r.ring, ready ? 0.3 + 0.22 * pulse : 0);
      });
      /* the crane turns towards the panel it is lowering, then back over the site */
      if (lastLowered && lastLowered.t > 0 && lastLowered.t < 1) {
        var dx = lastLowered.x - CRANE.x, dz = ZF - CRANE.z;
        slewTo = Math.atan2(-dz, dx);
        trolley.position.x = clamp(Math.hypot(dx, dz), 4, CRANE.jib - 1);
      } else if (!reduced) {
        slewTo = -2.45 + Math.sin(clock * 0.08) * 0.35;      /* over the building, swinging a little */
        trolley.position.x = 12 + Math.sin(clock * 0.11) * 4;
      }
      slewNow += (slewTo - slewNow) * Math.min(1, dt * 0.9);
      slew.rotation.y = slewNow;
      var drop = 8 + Math.sin(clock * 0.3) * 1.5;
      hook.position.y = -drop;
      hookLine.scale.y = (drop - 0.3) / 7.7; hookLine.position.y = -(0.3 + drop) / 2;
      glowAt(tip, reduced ? 0.7 : 0.45 + 0.4 * Math.max(0, Math.sin(clock * 3)));
      /* crews walk a little along their storey */
      crews.forEach(function (c) { c.g.position.x = c.base + Math.sin(clock * 0.35 + c.ph) * 0.8; });
      /* the more bays are lit, the warmer the front; all 24, and the tree lights on the roof */
      warmth.intensity += (opened / 24 * 1.6 - warmth.intensity) * Math.min(1, dt * 2);
      var all = opened >= 24 ? 1 : 0;
      star.material.emissiveIntensity = 0.2 + all * (reduced ? 1.4 : 1.2 + 0.4 * Math.sin(clock * 2.4));
      glowAt(treeGlow, all * 0.6);
      bulbs[0].material.opacity = reduced ? 0.9 : 0.75 + 0.25 * Math.sin(clock * 1.7);
      if (moving) dirty = 2;
    }
    function frame(now) {
      raf = 0;
      if (!running) return;
      var dt = Math.min(0.1, (now - last) / 1000 || 0.016);
      last = now; clock += dt;
      var k = 1 - Math.exp(-dt * 3.2);
      cam.t.lerp(goal.t, k);
      cam.az += (goal.az - cam.az) * k; cam.pol += (goal.pol - cam.pol) * k; cam.dist += (goal.dist - cam.dist) * k;
      var sway = mode.kind === 'over' && !drag && !reduced ? Math.sin(clock * 0.12) * 0.05 : 0;
      var sp = Math.sin(cam.pol), az = cam.az + sway;
      camera.position.set(cam.t.x + cam.dist * sp * Math.cos(az), cam.t.y + cam.dist * Math.cos(cam.pol), cam.t.z + cam.dist * sp * Math.sin(az));
      camera.lookAt(cam.t);
      dress(dt);
      if (flakes) {
        var p = flakes.geometry.attributes.position, v = flakes.userData.v;
        for (var i = 0; i < v.length; i++) {
          var y = p.array[i * 3 + 1] - v[i] * dt;
          p.array[i * 3] += Math.sin(clock * 0.6 + i) * dt * 0.1;
          p.array[i * 3 + 1] = y < 0 ? 34 : y;
        }
        p.needsUpdate = true;
      }
      if (dirty > 0) { renderer.shadowMap.needsUpdate = true; dirty--; }
      renderer.render(scene, camera);
      nameZones();
      raf = requestAnimationFrame(frame);
    }
    var spot = new THREE.Vector3();
    function nameZones() {
      var w = host.clientWidth, h = host.clientHeight;
      labels.forEach(function (l) {
        var zone = mode.kind === 'zone' && mode.zone === l.zone;
        spot.copy(zone ? l.storey : l.at).project(camera);
        var show = spot.z < 1 && (mode.kind === 'over' || zone);
        l.el.classList.toggle('off', !show);
        if (!show) return;
        var full = true;
        if (l.full !== full) { l.full = full; l.small.style.display = ''; l.half = 0; }
        var half = (l.half || (l.half = l.el.offsetWidth / 2)) + 8;
        var x = clamp((spot.x + 1) / 2 * w, half, w - half), y = Math.max(64, (1 - spot.y) / 2 * h);
        if (zone) { x = w / 2; y = 86; }
        l.el.style.transform = 'translate(-50%, -100%) translate(' + x.toFixed(1) + 'px,' + y.toFixed(1) + 'px)';
      });
    }
    function sync() {
      var should = visible && !paused && !document.hidden;
      if (should && !running) { running = true; last = performance.now(); raf = requestAnimationFrame(frame); }
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
      back: function () { if (mode.kind === 'day') toZone(mode.zone); },
      overview: overview, zone: toZone, knock: knock,
      /* the calls every scene offers (see apercu_du_monde.js) */
      views: function () { return ['chantier', 'semaine1', 'semaine2', 'semaine3', 'semaine4']; },
      view: function (name) { if (name === 'chantier') overview(); else toZone(Number(name.slice(-1)) - 1); },
      look: look,
      settle: function () {
        cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist;
        recs.forEach(function (r) { r.t = r.goal; pose(r); r.shake = 0; });
        slewNow = slewTo; warmth.intensity = opened / 24 * 1.6; dirty = 3;
        dress(0.016);
      }
    };
  }

  global.SceneChantier = {mount: mount};
})(window);
