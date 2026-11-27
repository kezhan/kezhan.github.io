/* The workshop that grows: one room at dawn in winter, seen as a cutaway model
 * (two walls and the floor), that starts almost empty and fills over December.
 *
 * No door to open. Each day places one object at its place, and the four weeks
 * furnish four corners of the room:
 *   week 1  the writing corner      week 2  the measuring bench under the window
 *   week 3  the wall of shelves     week 4  the other desks, the board, the way out
 *
 * What a reader sees:
 *   - a locked day is a chalk outline on the floor or the wall, with its number;
 *   - a day that may open has a lit tag and a warm outline;
 *   - an opened day brings its object in (it slides, is lowered, is hung, is
 *     pinned or unrolls, by kind) and the object stays; a lamp that has arrived
 *     lights the room, so the room gets warmer and fuller day by day;
 *   - a click on a day flies to its place, brings the object in if it may, then
 *     tells the page.
 *
 * Every object stands on the floor or hangs on a wall by itself, so the days can
 * be opened in any order. Before a day is open nothing shows but its number.
 *
 * Same calls as the island (see apercu_du_monde.js). Needs three.js r128 (global
 * `THREE`); every texture is drawn on a canvas.
 */
(function (global) {
  'use strict';

  var TAU = Math.PI * 2;
  /* the room, in metres: x runs along the back wall, z comes towards the viewer */
  var W = 10, D = 6.2, H = 3, T = 0.22;
  var X0 = -W / 2, X1 = W / 2, Z0 = -D / 2, Z1 = D / 2;
  /* the window, in the left wall */
  var WZ0 = -1.9, WZ1 = 1.1, WY0 = 0.95, WY1 = 2.55;

  var C = {oak: 0xc99a62, oak2: 0xb08250, walnut: 0x7b5337, dark: 0x4a3426, steel: 0xb9bfc8, iron: 0x35363f,
           paper: 0xfaf5e8, card: 0xe9d8b2, kraft: 0xc6a06c, white: 0xf2eee6, cream: 0xefe4cf, brass: 0xcaa24c,
           plaster: 0xe6dccd, cork: 0xb98c5c, slate: 0x5f6c7c, sage: 0x7f9a84, rust: 0xb5573a};

  function mount(host, data, page) {
    var THREE = global.THREE;
    if (!THREE || !host || !data.world) return null;
    var reduced = global.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;
    var renderer;
    try {
      renderer = new THREE.WebGLRenderer({antialias: true, alpha: true, powerPreference: 'high-performance'});
    } catch (e) { return null; }
    renderer.setPixelRatio(Math.min(2, global.devicePixelRatio || 1));
    renderer.outputEncoding = THREE.sRGBEncoding;
    renderer.toneMapping = THREE.NoToneMapping;
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;
    /* the shadows are drawn again only when something moved */
    renderer.shadowMap.autoUpdate = false;
    var canvas = renderer.domElement;
    canvas.className = 'monde-canvas';
    canvas.setAttribute('aria-hidden', 'true');
    host.appendChild(canvas);

    var scene = new THREE.Scene();
    var camera = new THREE.PerspectiveCamera(34, 2, 0.3, 120);
    var room = new THREE.Group();
    scene.add(room);

    /* --- light: a cool dawn, a low sun through the window, and what the lamps add --- */

    var hemi = new THREE.HemisphereLight(0xdcd8fb, 0xd0a680, 0.71);
    scene.add(hemi);
    var warmAir = new THREE.AmbientLight(0xffc28a, 0.05);
    scene.add(warmAir);
    var sun = new THREE.DirectionalLight(0xffc08a, 1.5);
    sun.position.set(-17, 8.4, 5.9);
    sun.target.position.set(-1, 0, -0.6);
    sun.castShadow = true;
    sun.shadow.mapSize.set(2048, 2048);
    sun.shadow.camera.left = -8.5; sun.shadow.camera.right = 8.5;
    sun.shadow.camera.top = 7.5; sun.shadow.camera.bottom = -7.5;
    sun.shadow.camera.near = 1; sun.shadow.camera.far = 48;
    sun.shadow.bias = -0.0006; sun.shadow.normalBias = 0.02;
    scene.add(sun); scene.add(sun.target);
    /* two fill lights stand for all the lamps: one for the left of the room, one for the right */
    var fillA = new THREE.PointLight(0xffb06a, 0, 9, 2);
    fillA.position.set(-3.5, 1.9, -1.9);
    var fillB = new THREE.PointLight(0xffc48e, 0, 10, 2);
    fillB.position.set(2.2, 2.0, -0.6);
    scene.add(fillA); scene.add(fillB);

    /* --- small helpers ------------------------------------------------------- */

    var seed = 20261224;
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
    function ease(x) { x = clamp(x, 0, 1); return 1 - Math.pow(1 - x, 3); }
    function step(a, b, x) { var t = clamp((x - a) / (b - a), 0, 1); return t * t * (3 - 2 * t); }
    function mix(a, b, k) { return new THREE.Color(a).lerp(new THREE.Color(b), k).getHex(); }
    var UP = new THREE.Vector3(0, 1, 0);

    var mats = {};
    function mat(color, o) {
      o = o || {};
      var key = o.own ? '' : [color, o.rough, o.metal, o.alpha, o.side, o.emissive, o.ei].join('|');
      if (key && mats[key]) return mats[key];
      var m = new THREE.MeshStandardMaterial({color: color, flatShading: true, roughness: o.rough == null ? 0.85 : o.rough,
        metalness: o.metal || 0, emissive: o.emissive || 0x000000, emissiveIntensity: o.ei == null ? 1 : o.ei,
        transparent: o.alpha != null, opacity: o.alpha == null ? 1 : o.alpha, side: o.side ? THREE.DoubleSide : THREE.FrontSide});
      if (o.alpha != null) m.depthWrite = false;
      if (key) mats[key] = m;
      return m;
    }
    var flats = {};
    function flat(color, own) {
      if (!own && flats[color]) return flats[color];
      var m = new THREE.MeshBasicMaterial({color: color});
      if (!own) flats[color] = m;
      return m;
    }
    /* a sheet that carries a drawing, lit like the rest of the room */
    function printed(tex) { return new THREE.MeshStandardMaterial({map: tex, roughness: 0.9}); }

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
    function cyl(p, rt, rb, h, seg, m, x, y, z, o) {
      return put(p, new THREE.CylinderGeometry(rt, rb, h, seg, 1, !!(o && o.open)), m, x, y, z, o);
    }
    function ball(p, r, m, x, y, z, o) { return put(p, new THREE.SphereGeometry(r, 10, 7), m, x, y, z, o); }
    function pane(p, w, h, m, x, y, z, o) {
      o = o || {}; if (o.cast == null) o.cast = false;
      return put(p, new THREE.PlaneGeometry(w, h), m, x, y, z, o);
    }
    function disc(p, r, m, x, y, z, o) {
      o = o || {}; if (o.cast == null) o.cast = false;
      return put(p, new THREE.CircleGeometry(r, 24), m, x, y, z, o);
    }
    /* a thin bar from one point to another: legs, arms, wires */
    function rod(p, a, b, r, m, seg) {
      var va = new THREE.Vector3(a[0], a[1], a[2]), vb = new THREE.Vector3(b[0], b[1], b[2]);
      var me = new THREE.Mesh(new THREE.CylinderGeometry(r, r, va.distanceTo(vb), seg || 6), m);
      me.position.copy(va).add(vb).multiplyScalar(0.5);
      me.quaternion.setFromUnitVectors(UP, vb.clone().sub(va).normalize());
      me.castShadow = me.receiveShadow = true;
      p.add(me);
      return me;
    }
    function grp(p, x, y, z, ry) {
      var g = new THREE.Group(); g.position.set(x || 0, y || 0, z || 0); g.rotation.y = ry || 0; p.add(g); return g;
    }
    /* a part that moves by itself: kept out of its parent's merged meshes */
    var lives = [];
    function live(p, x, y, z, ry) { var g = grp(p, x, y, z, ry); g.userData.live = true; lives.push(g); return g; }

    /* Merge what was built under `root` into one mesh per material: an object is
     * made of dozens of small boxes, and a phone draws five meshes faster than fifty. */
    function bake(root) {
      root.updateMatrixWorld(true);
      var inv = new THREE.Matrix4().copy(root.matrixWorld).invert();
      var sets = {}, gone = [];
      (function walk(n) {
        n.children.forEach(function (c) {
          if (c.userData.live || c.userData.keep) return;
          if (c.isMesh) {
            var key = c.material.uuid + (c.castShadow ? 'c' : '') + (c.receiveShadow ? 'r' : '');
            (sets[key] || (sets[key] = {m: c.material, cast: c.castShadow, recv: c.receiveShadow, list: []})).list.push(c);
            gone.push(c);
          } else if (c.isGroup) walk(c);
        });
      })(root);
      var mtx = new THREE.Matrix4(), nm = new THREE.Matrix3(), v = new THREE.Vector3();
      Object.keys(sets).forEach(function (k) {
        var s = sets[k], pos = [], nor = [], uv = [];
        s.list.forEach(function (me) {
          var g = me.geometry.index ? me.geometry.toNonIndexed() : me.geometry;
          mtx.multiplyMatrices(inv, me.matrixWorld); nm.getNormalMatrix(mtx);
          var P = g.attributes.position, N = g.attributes.normal, U = g.attributes.uv;
          for (var i = 0; i < P.count; i++) {
            v.fromBufferAttribute(P, i).applyMatrix4(mtx); pos.push(v.x, v.y, v.z);
            v.fromBufferAttribute(N, i).applyMatrix3(nm).normalize(); nor.push(v.x, v.y, v.z);
            uv.push(U ? U.getX(i) : 0, U ? U.getY(i) : 0);
          }
        });
        var g2 = new THREE.BufferGeometry();
        g2.setAttribute('position', new THREE.Float32BufferAttribute(pos, 3));
        g2.setAttribute('normal', new THREE.Float32BufferAttribute(nor, 3));
        g2.setAttribute('uv', new THREE.Float32BufferAttribute(uv, 2));
        var out = new THREE.Mesh(g2, s.m);
        out.castShadow = s.cast; out.receiveShadow = s.recv;
        out.userData.keep = true;
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
      r.addColorStop(0, 'rgba(255,255,255,1)'); r.addColorStop(0.25, 'rgba(255,255,255,.55)');
      r.addColorStop(0.6, 'rgba(255,255,255,.14)'); r.addColorStop(1, 'rgba(255,255,255,0)');
      g.fillStyle = r; g.fillRect(0, 0, 128, 128);
    });
    /* shade is drawn too: only the sun casts a real shadow, and most of the room is out of the sun */
    var shadeTex = drawn(8, 64, function (g, w, h) {
      var r = g.createLinearGradient(0, 0, 0, h);
      r.addColorStop(0, 'rgba(255,255,255,1)'); r.addColorStop(0.35, 'rgba(255,255,255,.42)'); r.addColorStop(1, 'rgba(255,255,255,0)');
      g.fillStyle = r; g.fillRect(0, 0, w, h);
    });
    var blobTex = drawn(128, 128, function (g) {
      g.shadowColor = '#fff'; g.shadowBlur = 20; g.fillStyle = '#fff';
      g.fillRect(34, 34, 60, 60); g.fillRect(34, 34, 60, 60);
    });
    function shadeM(tex, opacity) {
      return new THREE.MeshBasicMaterial({map: tex, color: 0x2b1a1c, transparent: true, opacity: opacity, depthWrite: false});
    }

    /* the light of a lamp is drawn, not computed: a halo around the bulb, a pool on what it lights */
    function halo(p, x, y, z, color, size) {
      /* no depth test: a halo is a disc that faces the viewer, and a wall just behind the lamp would cut it along a line */
      var s = new THREE.Sprite(new THREE.SpriteMaterial({map: glowTex, color: color, transparent: true, opacity: 0,
        blending: THREE.AdditiveBlending, depthWrite: false, depthTest: false}));
      s.position.set(x, y, z); s.scale.set(size, size, 1); s.visible = false; s.renderOrder = 4;
      p.add(s);
      return s;
    }
    function pool(p, w, h, x, y, z, color, o) {
      var me = put(p, new THREE.PlaneGeometry(w, h), new THREE.MeshBasicMaterial({map: glowTex, color: color, transparent: true,
        opacity: 0, blending: THREE.AdditiveBlending, depthWrite: false}), x, y, z, o);
      me.castShadow = me.receiveShadow = false; me.userData.keep = true; me.visible = false; me.renderOrder = 2;
      return me;
    }
    function glowAt(obj, k) { obj.material.opacity = k; obj.visible = k > 0.004; }
    var FLOOR = {rx: -Math.PI / 2};

    /* --- the four weeks -------------------------------------------------------- */

    var zones = data.world.villages.map(function (v, i) {
      return {def: v, i: i, css: v.tone, tone: new THREE.Color(v.tone).getHex()};
    });
    function zoneOf(day) {
      for (var i = 0; i < zones.length; i++) {
        var d = zones[i].def.days;
        if (d && day >= d[0] && day <= d[1]) return zones[i];
      }
      return zones[Math.min(zones.length - 1, Math.floor((day - 1) / 6))];
    }

    /* --- the room itself: floor, two walls, the window, the door ------------------ */

    var sconce = {};
    (function shell() {
      var g = new THREE.Group(); room.add(g);
      var floorTex = drawn(1024, 1024, function (c, w, h) {
        var rows = 28, rh = h / rows;
        for (var r = 0; r < rows; r++) {
          var x = -rnd() * 300, end = x + w;
          while (x < end) {
            var len = Math.min(240 + rnd() * 330, end - x);
            if (end - x - len < 120) len = end - x;
            var fill = 'hsl(' + (26 + rnd() * 7) + ',' + (36 + rnd() * 9) + '%,' + (45 + rnd() * 11) + '%)';
            /* drawn twice, so the row wraps without a seam */
            [x, x + w].forEach(function (x0) {
              c.fillStyle = fill; c.fillRect(x0, r * rh, len, rh);
              c.fillStyle = 'rgba(46,24,10,.5)'; c.fillRect(x0 + len - 1.5, r * rh, 1.5, rh);
            });
            c.strokeStyle = 'rgba(70,38,14,.11)'; c.lineWidth = 1;
            for (var k = 0; k < 5; k++) {
              var gy = r * rh + 4 + rnd() * (rh - 8), gx = x + rnd() * len * 0.3, gl = len * (0.4 + rnd() * 0.5);
              c.beginPath(); c.moveTo(gx, gy); c.lineTo(gx + gl, gy + (rnd() - 0.5) * 3); c.stroke();
              c.beginPath(); c.moveTo(gx + w, gy); c.lineTo(gx + w + gl, gy + (rnd() - 0.5) * 3); c.stroke();
            }
            x += len;
          }
          c.fillStyle = 'rgba(46,24,10,.42)'; c.fillRect(0, r * rh, w, 1.5);
        }
      });
      floorTex.wrapS = floorTex.wrapT = THREE.RepeatWrapping; floorTex.repeat.set(W / 4, D / 4);
      var floor = pane(g, W, D, new THREE.MeshStandardMaterial({map: floorTex, roughness: 0.72}), 0, 0, 0, FLOOR);
      floor.userData.keep = true;

      /* the model stands on a plinth; outside the walls the ground is under snow */
      var snow = mat(0xfbfbff, {rough: 0.95}), out = 2.6;
      box(g, W, 0.16, D, mat(0x8d6744), 0, -0.081, 0);
      box(g, out, 0.16, D + T + out, snow, X0 - T - out / 2, -0.05, (Z0 - T - out + Z1) / 2, {cast: false});
      box(g, W + T, 0.16, out, snow, (X0 - T + X1) / 2, -0.05, Z0 - T - out / 2, {cast: false});
      box(g, W + T + out + 0.1, 0.5, D + T + out + 0.1, mat(0x4a3340, {rough: 0.9}), (X0 - T - out + X1) / 2, -0.41, (Z0 - T - out + Z1) / 2, {cast: false});

      var plaster = mat(C.plaster, {rough: 0.96}), cut = mat(0xfffdf8, {rough: 0.9}), trim = mat(0xf6f1e8, {rough: 0.7});
      /* back wall */
      box(g, W + T, H, T, plaster, (X0 - T + X1) / 2, H / 2, Z0 - T / 2);
      box(g, W + T + 0.004, 0.03, T + 0.004, cut, (X0 - T + X1) / 2, H + 0.014, Z0 - T / 2, {cast: false});
      /* left wall, around the window */
      var xc = X0 - T / 2;
      box(g, T, WY0, D, plaster, xc, WY0 / 2, 0);
      box(g, T, H - WY1, D, plaster, xc, (H + WY1) / 2, 0);
      box(g, T, WY1 - WY0, WZ0 - Z0, plaster, xc, (WY0 + WY1) / 2, (Z0 + WZ0) / 2);
      box(g, T, WY1 - WY0, Z1 - WZ1, plaster, xc, (WY0 + WY1) / 2, (WZ1 + Z1) / 2);
      box(g, T + 0.004, 0.03, D + 0.004, cut, xc, H + 0.014, 0, {cast: false});
      /* skirting boards */
      box(g, W, 0.11, 0.02, trim, 0, 0.055, Z0 + 0.01, {cast: false});
      box(g, 0.02, 0.11, D, trim, X0 + 0.01, 0.055, 0, {cast: false});

      /* where wall meets floor, and wall meets wall, it is a little darker */
      var foot = shadeM(shadeTex, 0.3), nook = shadeM(shadeTex, 0.16), side = grp(g, X0, 0, Z0, Math.PI / 2);
      [pane(g, W, 1.0, foot, 0, 0.004, Z0 + 0.5, {rx: -Math.PI / 2, shadow: false}),
       pane(side, D, 1.0, foot, -D / 2, 0.004, 0.5, {rx: -Math.PI / 2, shadow: false}),
       pane(g, H, 0.9, nook, X0 + 0.45, H / 2, Z0 + 0.004, {rz: Math.PI / 2, shadow: false}),
       pane(side, H, 0.9, nook, -0.45, H / 2, 0.004, {rz: -Math.PI / 2, shadow: false})].forEach(function (me) { me.userData.keep = true; me.renderOrder = 1; });

      /* the window: frame, two mullions, a transom, a sill, and snow on the outside ledge */
      var wz = (WZ0 + WZ1) / 2, wl = WZ1 - WZ0, wy = (WY0 + WY1) / 2, wh = WY1 - WY0, fd = T + 0.05;
      box(g, fd, wh, 0.07, trim, xc, wy, WZ0 + 0.035); box(g, fd, wh, 0.07, trim, xc, wy, WZ1 - 0.035);
      box(g, fd, 0.07, wl, trim, xc, WY1 - 0.035, wz); box(g, fd, 0.07, wl, trim, xc, WY0 + 0.035, wz);
      box(g, 0.06, wh, 0.05, trim, xc, wy, WZ0 + wl / 3); box(g, 0.06, wh, 0.05, trim, xc, wy, WZ0 + 2 * wl / 3);
      box(g, 0.06, 0.05, wl, trim, xc, WY0 + wh * 0.68, wz);
      box(g, 0.2, 0.04, wl + 0.16, trim, X0 + 0.07, WY0 - 0.02, wz);
      box(g, 0.2, 0.09, wl + 0.1, snow, X0 - T - 0.07, WY0 - 0.01, wz, {cast: false});
      var dawn = drawn(64, 256, function (c, w, h) {
        var gr = c.createLinearGradient(0, 0, 0, h);
        gr.addColorStop(0, '#cdd3f7'); gr.addColorStop(0.5, '#f3d6dc'); gr.addColorStop(1, '#ffdcae');
        c.fillStyle = gr; c.fillRect(0, 0, w, h);
      });
      var glass = pane(g, wl, wh, new THREE.MeshBasicMaterial({map: dawn, transparent: true, opacity: 0.66, depthWrite: false}),
        xc, wy, wz, {ry: Math.PI / 2, shadow: false});
      glass.userData.keep = true;
      /* the daylight the panes hold */
      var day = pool(g, wl * 1.5, wh * 1.7, xc + 0.02, wy, wz, 0xffd9b0, {ry: Math.PI / 2});
      glowAt(day, 0.22);

      /* the door, in the back wall: this is where one leaves on the 24th */
      var dx = 4.3, door = mat(0x466a60, {rough: 0.65}), door2 = mat(0x3d5e55, {rough: 0.65});
      box(g, 0.07, 2.12, 0.07, trim, dx - 0.485, 1.06, Z0 + 0.035); box(g, 0.07, 2.12, 0.07, trim, dx + 0.485, 1.06, Z0 + 0.035);
      box(g, 1.04, 0.07, 0.07, trim, dx, 2.085, Z0 + 0.035);
      box(g, 0.9, 2.05, 0.04, door, dx, 1.025, Z0 + 0.025);
      box(g, 0.62, 0.78, 0.012, door2, dx, 1.5, Z0 + 0.05); box(g, 0.62, 0.62, 0.012, door2, dx, 0.52, Z0 + 0.05);
      var brass = mat(C.brass, {metal: 0.6, rough: 0.35});
      box(g, 0.035, 0.16, 0.012, brass, dx - 0.37, 1.02, Z0 + 0.052); ball(g, 0.026, brass, dx - 0.37, 1.03, Z0 + 0.085);
      box(g, 0.8, 0.014, 0.48, mat(0x6f5a48, {rough: 1}), dx, 0.007, Z0 + 0.4, {cast: false});
      /* the lamp above the door: there from the start, lit on the last day */
      var iron = mat(C.iron, {rough: 0.5, metal: 0.4});
      box(g, 0.05, 0.03, 0.14, iron, dx, 2.5, Z0 + 0.07);
      box(g, 0.13, 0.02, 0.13, iron, dx, 2.47, Z0 + 0.13);
      sconce.m = mat(0x8f8a84, {own: true, emissive: 0xffc27a, ei: 0, rough: 0.4});
      box(g, 0.1, 0.13, 0.1, sconce.m, dx, 2.395, Z0 + 0.13, {cast: false});
      sconce.halo = halo(room, dx, 2.4, Z0 + 0.16, 0xffc27a, 1.5);
      sconce.pool = pool(room, 2.2, 2.6, dx, 2.0, Z0 + 0.012, 0xffb469);

      /* outside: firs under snow, a few drifts */
      var trunk = mat(0x5a3d2e), fir = mat(0x2f5f52), fir2 = mat(0x3a7060);
      [[-6.7, 0.1, 3.3], [-7.0, -2.7, 3.9], [-6.4, 2.4, 2.6], [-6.8, -5.1, 4.3], [-3.2, -5.2, 3.5], [-0.6, -5.4, 4.1],
       [2.2, -5.0, 3.2]].forEach(function (t, i) {
        var s = t[2] / 4.4, x = t[0], z = t[1];
        cyl(g, 0.09 * s, 0.13 * s, 0.8 * s, 6, trunk, x, 0.4 * s, z, {cast: false});
        [[1.05, 1.7, 1.45], [0.82, 1.5, 2.45], [0.58, 1.3, 3.35]].forEach(function (c, k) {
          put(g, new THREE.ConeGeometry(c[0] * s, c[1] * s, 7), k % 2 ? fir2 : fir, x, c[2] * s, z, {cast: false, ry: i + k});
          put(g, new THREE.ConeGeometry(c[0] * s * 0.62, c[1] * s * 0.5, 7), snow, x, (c[2] + c[1] * 0.27) * s, z, {cast: false, ry: i + k});
        });
      });
      [[-6.0, 1.2, 0.7], [-5.9, -1.4, 0.55], [-2.2, -4.4, 0.6], [2.9, -4.3, 0.75], [0.4, -4.6, 0.45]].forEach(function (m) {
        var me = put(g, new THREE.SphereGeometry(m[2], 9, 6), snow, m[0], 0, m[1], {cast: false}); me.scale.y = 0.35;
      });
      bake(g);
      /* The model has no ceiling and no front walls, so that one can look in; the
       * light must not know it. These three cast a shadow and are never drawn, and
       * the sun enters by the window only. */
      var unseen = new THREE.MeshBasicMaterial({colorWrite: false, depthWrite: false});
      [[W + T, 0.1, D + T, (X0 - T + X1) / 2, H + 0.08, (Z0 - T + Z1) / 2], [W + T, H, 0.1, (X0 - T + X1) / 2, H / 2, Z1 + 0.05],
       [0.1, H, D + T, X1 + 0.05, H / 2, (Z0 - T + Z1) / 2]].forEach(function (b) {
        var me = new THREE.Mesh(new THREE.BoxGeometry(b[0], b[1], b[2]), unseen);
        me.position.set(b[3], b[4], b[5]); me.castShadow = true; me.userData.keep = true;
        room.add(me);
      });
    })();

    /* the snow that falls outside, behind the walls and past the window */
    var flakes = null;
    if (!reduced) {
      (function () {
        var N = 420, fp = new Float32Array(N * 3), fv = new Float32Array(N);
        for (var i = 0; i < N; i++) {
          var left = rnd() < 0.42;
          fp[i * 3] = left ? X0 - T - 0.2 - rnd() * 2.6 : X0 - 3 + rnd() * (W + 3);
          fp[i * 3 + 1] = rnd() * 6.5;
          fp[i * 3 + 2] = left ? Z0 - 3 + rnd() * (D + 3) : Z0 - T - 0.2 - rnd() * 2.6;
          fv[i] = 0.35 + rnd() * 0.5;
        }
        var fg = new THREE.BufferGeometry();
        fg.setAttribute('position', new THREE.BufferAttribute(fp, 3));
        flakes = new THREE.Points(fg, new THREE.PointsMaterial({color: 0xffffff, size: 0.1, map: glowTex, transparent: true,
          opacity: 0.9, depthWrite: false, sizeAttenuation: true}));
        flakes.userData = {v: fv};
        room.add(flakes);
      })();
    }

    /* a string of twenty-four bulbs along the top of the walls: one lights for each opened day */
    var string = (function () {
      var y = 2.8, len1 = D - 0.5, len2 = W - 0.5, total = len1 + len2;
      function at(s) {
        var sag = 0.085 * Math.abs(Math.sin(Math.PI * s / (total / 12)));
        return s < len1 ? [X0 + 0.035, y - sag, Z1 - 0.35 - s] : [X0 + 0.15 + (s - len1), y - sag, Z0 + 0.035];
      }
      var pts = [];
      for (var s = 0; s <= total; s += 0.07) { var p = at(s); pts.push(new THREE.Vector3(p[0], p[1], p[2])); }
      room.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints(pts), new THREE.LineBasicMaterial({color: 0x39483e})));
      var bulbs = new THREE.InstancedMesh(new THREE.SphereGeometry(0.032, 8, 6), new THREE.MeshBasicMaterial({color: 0xffffff}), 24);
      var gp = new Float32Array(72), gc = new Float32Array(72), m = new THREE.Matrix4();
      for (var d = 1; d <= 24; d++) {
        var q = at((d - 0.5) * total / 24);
        m.makeTranslation(q[0], q[1] - 0.04, q[2]); bulbs.setMatrixAt(d - 1, m);
        bulbs.setColorAt(d - 1, new THREE.Color(0x7d7783));
        gp[(d - 1) * 3] = q[0]; gp[(d - 1) * 3 + 1] = q[1] - 0.04; gp[(d - 1) * 3 + 2] = q[2];
      }
      room.add(bulbs);
      var gg = new THREE.BufferGeometry();
      gg.setAttribute('position', new THREE.BufferAttribute(gp, 3));
      gg.setAttribute('color', new THREE.BufferAttribute(gc, 3));
      var glows = new THREE.Points(gg, new THREE.PointsMaterial({size: 0.42, map: glowTex, vertexColors: true, transparent: true,
        blending: THREE.AdditiveBlending, depthWrite: false, sizeAttenuation: true}));
      room.add(glows);
      var col = new THREE.Color();
      return {set: function (day, tone, on) {
        col.setHex(on ? mix(tone, 0xffffff, 0.25) : 0x7d7783);
        bulbs.setColorAt(day - 1, col); bulbs.instanceColor.needsUpdate = true;
        col.setHex(on ? tone : 0x000000);
        gc[(day - 1) * 3] = col.r; gc[(day - 1) * 3 + 1] = col.g; gc[(day - 1) * 3 + 2] = col.b;
        gg.attributes.color.needsUpdate = true;
      }, glows: glows};
    })();

    /* --- what is drawn on paper, dials and screens ---------------------------------- */

    function bars(g, x, y, w, n, gap, h, color, ragged) {
      g.fillStyle = color;
      for (var i = 0; i < n; i++) g.fillRect(x, y + i * gap, ragged ? w * (0.55 + ((i * 37) % 45) / 100) : w, h);
    }
    function sheetTex(kind, css) {
      return drawn(256, kind === 'plan' ? 188 : kind === 'note' ? 256 : 358, function (g, w, h) {
        g.fillStyle = kind === 'plan' ? '#f1e8d2' : '#f8f2e2'; g.fillRect(0, 0, w, h);
        if (kind === 'typed') {
          g.fillStyle = css; g.fillRect(26, 30, 120, 16);
          for (var i = 0; i < 7; i++) {
            g.fillStyle = css; g.fillRect(26, 78 + i * 36, 12, 12);
            g.fillStyle = '#5b5560'; g.fillRect(50, 80 + i * 36, 110 + ((i * 53) % 70), 8);
          }
        } else if (kind === 'list') {
          g.fillStyle = '#5b5560'; g.fillRect(30, 34, 110, 14);
          for (var j = 0; j < 3; j++) {
            g.strokeStyle = css; g.lineWidth = 7; g.strokeRect(34, 96 + j * 82, 36, 36);
            g.fillStyle = '#5b5560'; g.fillRect(92, 100 + j * 82, 120 - j * 22, 10); g.fillRect(92, 120 + j * 82, 80, 8);
          }
        } else if (kind === 'check') {
          g.fillStyle = css; g.fillRect(0, 0, w, 46);
          g.fillStyle = '#fff'; g.fillRect(24, 16, 120, 12);
          for (var k = 0; k < 6; k++) {
            var yy = 76 + k * 46;
            g.strokeStyle = '#5b5560'; g.lineWidth = 4; g.strokeRect(26, yy, 24, 24);
            if (k < 4) { g.strokeStyle = '#2e9a5c'; g.lineWidth = 7; g.beginPath(); g.moveTo(29, yy + 12); g.lineTo(37, yy + 21); g.lineTo(53, yy - 2); g.stroke(); }
            g.fillStyle = '#6a6470'; g.fillRect(70, yy + 8, 100 + ((k * 61) % 70), 8);
          }
        } else if (kind === 'plan') {
          g.strokeStyle = 'rgba(90,110,150,.18)'; g.lineWidth = 1;
          for (var a = 12; a < w; a += 12) { g.beginPath(); g.moveTo(a, 0); g.lineTo(a, h); g.stroke(); }
          for (var b = 12; b < h; b += 12) { g.beginPath(); g.moveTo(0, b); g.lineTo(w, b); g.stroke(); }
          var steps = [[18, 30], [98, 30], [178, 30], [178, 104], [98, 104], [18, 104]];
          g.strokeStyle = '#4a4652'; g.lineWidth = 3; g.beginPath();
          steps.forEach(function (s, n) { g[n ? 'lineTo' : 'moveTo'](s[0] + 30, s[1] + 20); }); g.stroke();
          steps.forEach(function (s, n) {
            g.fillStyle = n === 0 || n === 5 ? css : '#fffdf6'; g.fillRect(s[0], s[1], 60, 40);
            g.strokeStyle = '#4a4652'; g.lineWidth = 3; g.strokeRect(s[0], s[1], 60, 40);
            g.fillStyle = n === 0 || n === 5 ? '#fff' : '#8a8490'; g.fillRect(s[0] + 10, s[1] + 12, 40, 5); g.fillRect(s[0] + 10, s[1] + 23, 26, 5);
          });
          g.fillStyle = '#8a8490'; g.fillRect(18, 164, 90, 6);
        } else {
          g.fillStyle = css; g.fillRect(0, 0, w, h);
          bars(g, 36, 60, 170, 4, 44, 14, 'rgba(255,255,255,.85)', true);
        }
      });
    }
    function dialTex(css, clock) {
      return drawn(256, 256, function (g) {
        g.fillStyle = '#fbf6ea'; g.beginPath(); g.arc(128, 128, 128, 0, TAU); g.fill();
        g.strokeStyle = '#3a3640';
        for (var i = 0; i < (clock ? 60 : 40); i++) {
          var a = clock ? i * TAU / 60 : Math.PI * 0.75 + i * Math.PI * 1.5 / 39, big = i % 5 === 0;
          g.lineWidth = big ? 6 : 2;
          g.beginPath(); g.moveTo(128 + Math.cos(a) * 112, 128 + Math.sin(a) * 112);
          g.lineTo(128 + Math.cos(a) * (big ? 90 : 100), 128 + Math.sin(a) * (big ? 90 : 100)); g.stroke();
        }
        if (!clock) { g.strokeStyle = css; g.lineWidth = 10; g.beginPath(); g.arc(128, 128, 76, Math.PI * 1.9, Math.PI * 2.25); g.stroke(); }
      });
    }
    function screenTex(css, kind) {
      return drawn(256, 148, function (g, w, h) {
        g.fillStyle = '#17131f'; g.fillRect(0, 0, w, h);
        g.fillStyle = '#231d30'; g.fillRect(0, 0, w, 16);
        g.fillStyle = css; g.fillRect(8, 5, 34, 6);
        if (kind === 0) {
          for (var i = 0; i < 9; i++) {
            g.fillStyle = i % 4 === 0 ? css : 'rgba(240,236,255,.72)';
            g.fillRect(14 + (i % 3) * 12, 28 + i * 12, 60 + ((i * 47) % 120), 5);
          }
          g.fillStyle = '#7dffb0'; g.fillRect(14, 136, 8, 6);
        } else if (kind === 1) {
          [0, 1, 2].forEach(function (c) {
            g.fillStyle = 'rgba(240,236,255,.14)'; g.fillRect(10 + c * 82, 24, 74, 116);
            for (var r = 0; r < 4 - c; r++) { g.fillStyle = r === 0 && c === 1 ? css : 'rgba(240,236,255,.78)'; g.fillRect(16 + c * 82, 32 + r * 24, 62, 16); }
          });
        } else {
          g.fillStyle = 'rgba(240,236,255,.12)'; g.fillRect(10, 24, 112, 116); g.fillRect(134, 24, 112, 116);
          bars(g, 18, 34, 90, 8, 12, 5, 'rgba(240,236,255,.7)', true);
          bars(g, 142, 34, 90, 8, 12, 5, 'rgba(125,255,176,.75)', true);
          g.fillStyle = css; g.fillRect(142, 58, 70, 5);
        }
      });
    }

    /* --- the chalk outline of a place, and its numbered tag ---------------------------- */

    function chalk(wm, hm, round) {
      var k = 384 / Math.max(wm, hm), cw = Math.max(48, Math.round(wm * k)), ch = Math.max(48, Math.round(hm * k));
      return drawn(cw, ch, function (g) {
        var lw = Math.max(3, 0.022 * k), pad = lw + 2, r = Math.min(cw, ch) * 0.12;
        g.strokeStyle = '#fff'; g.lineWidth = lw; g.lineCap = 'round'; g.setLineDash([0.075 * k, 0.055 * k]);
        g.beginPath();
        if (round) g.ellipse(cw / 2, ch / 2, cw / 2 - pad, ch / 2 - pad, 0, 0, TAU);
        else {
          g.moveTo(pad + r, pad); g.arcTo(cw - pad, pad, cw - pad, ch - pad, r); g.arcTo(cw - pad, ch - pad, pad, ch - pad, r);
          g.arcTo(pad, ch - pad, pad, pad, r); g.arcTo(pad, pad, cw - pad, pad, r); g.closePath();
        }
        g.stroke();
        /* chalk is never a clean line */
        g.globalCompositeOperation = 'destination-out'; g.fillStyle = 'rgba(0,0,0,.55)';
        for (var i = 0; i < cw * ch / 90; i++) g.fillRect(rnd() * cw, rnd() * ch, 1.6, 1.6);
      });
    }

    var recs = [], byDay = {}, pickables = [];
    var HIDDEN = new THREE.MeshBasicMaterial({visible: false});

    /* One day: where it goes (`at`, `ry`), what marks its place (`foot`, on the
     * floor or on a wall), how it arrives (`how`, and `from` for a slide), and the
     * builder of its object. A wall object is built with the wall at z = 0 and the
     * room towards +z; one on the left wall is the same, turned a quarter. */
    function place(day, o, build) {
      var z = zoneOf(day), wall = o.on === 'wall', f = o.foot;
      var root = grp(room, o.at[0], o.at[1], o.at[2], o.ry || 0);
      var body = grp(root);
      var r = {day: day, zone: z, root: root, body: body, how: o.how, wall: wall, face: o.face || (o.ry ? 'left' : 'back'), state: '', painted: false,
               t: 0, goal: 0, shake: 0, drop: o.drop || 3.3, hover: 0};
      var made = build(body, z) || {};
      r.more = made.more; r.lit = made.lit; r.warm = made.warm; r.tick = made.tick;
      r.from = new THREE.Vector3(o.from ? o.from[0] : 0, 0, o.from ? o.from[2] : 0).applyAxisAngle(UP, -(o.ry || 0));
      bake(body);

      var fx = f.x || 0, fy = f.y || 0, fz = f.z || 0, tall = o.tall || 0.5, deep = o.deep || 0.3;
      var ol = new THREE.Mesh(new THREE.PlaneGeometry(f.w, f.h), new THREE.MeshBasicMaterial({map: chalk(f.w, f.h, f.round),
        transparent: true, opacity: 0, depthWrite: false}));
      if (wall) ol.position.set(fx, fy, 0.008); else { ol.rotation.x = -Math.PI / 2; ol.position.set(fx, 0.008, fz); }
      ol.renderOrder = 1;
      root.add(ol); r.outline = ol;

      if (!wall) {
        r.shadow = new THREE.Mesh(new THREE.PlaneGeometry(f.w * 1.3, f.h * 1.3), shadeM(f.round ? glowTex : blobTex, 0));
        r.shadow.rotation.x = -Math.PI / 2; r.shadow.position.set(fx, 0.005, fz); r.shadow.visible = false;
        r.shadowAt = [fx, fz];
        root.add(r.shadow);
      }

      var hit = new THREE.Mesh(wall ? new THREE.BoxGeometry(f.w + 0.1, f.h + 0.1, deep) : new THREE.BoxGeometry(f.w + 0.06, tall, f.h + 0.06), HIDDEN);
      if (wall) hit.position.set(fx, fy, deep / 2); else hit.position.set(fx, tall / 2, fz);
      hit.userData.day = day; root.add(hit); pickables.push(hit);

      r.closedAt = wall ? new THREE.Vector3(fx, fy, 0.1) : new THREE.Vector3(fx, 0.32, fz);
      r.openAt = o.tag ? new THREE.Vector3(o.tag[0], o.tag[1], o.tag[2])
        : wall ? new THREE.Vector3(fx, fy + f.h / 2 + 0.14, 0.12) : new THREE.Vector3(fx, tall + 0.2, fz);
      root.updateWorldMatrix(true, false);
      r.centre = root.localToWorld(wall ? new THREE.Vector3(fx, fy, 0.1) : new THREE.Vector3(fx, tall / 2, fz));
      r.radius = Math.max(f.w, f.h, wall ? 0 : tall) / 2;

      var c = document.createElement('canvas'); c.width = c.height = 128;
      var tex = new THREE.CanvasTexture(c); tex.encoding = THREE.sRGBEncoding;
      r.drawTag = function () {
        var g = c.getContext('2d'), st = r.state;
        g.clearRect(0, 0, 128, 128);
        g.beginPath(); g.arc(64, 64, 52, 0, TAU);
        if (st === 'open') { g.fillStyle = z.css; g.fill(); g.lineWidth = 7; g.strokeStyle = '#ffffff'; g.stroke(); g.fillStyle = '#ffffff'; }
        else if (st === 'ready') { g.fillStyle = '#fff6dc'; g.fill(); g.lineWidth = 8; g.strokeStyle = '#f2b441'; g.stroke(); g.fillStyle = '#2a1c14'; }
        else {
          g.fillStyle = 'rgba(40,30,48,.66)'; g.fill();
          g.setLineDash([10, 7]); g.lineWidth = 5; g.strokeStyle = 'rgba(255,255,255,.82)'; g.stroke(); g.setLineDash([]);
          g.fillStyle = 'rgba(255,255,255,.95)';
        }
        g.font = 'italic 800 ' + (day > 9 ? 58 : 68) + 'px Fraunces, Georgia, serif';
        g.textAlign = 'center'; g.textBaseline = 'middle';
        g.fillText(String(day), 64, 69);
        tex.needsUpdate = true;
      };
      r.glow = halo(root, 0, 0, 0, 0xffcf6a, 1);
      r.glow.renderOrder = 8;
      r.tag = new THREE.Sprite(new THREE.SpriteMaterial({map: tex, transparent: true, depthWrite: false, depthTest: false}));
      r.tag.renderOrder = 9; r.tag.userData.day = day;
      root.add(r.tag); pickables.push(r.tag);

      recs.push(r); byDay[day] = r;
      return r;
    }

    /* --- week 1, the writing corner ------------------------------------------------------ */

    place(1, {at: [-3.6, 0, Z0], on: 'floor', how: 'slide', from: [0, 0, 3.4], foot: {w: 1.7, h: 1.42, z: 0.73}, tall: 0.95},
      function (g, z) {
        var oak = mat(C.oak), oak2 = mat(C.oak2), tone = mat(z.tone, {rough: 0.7}), paper = mat(C.paper), top = 0.75, cz = 0.42;
        box(g, 1.5, 0.04, 0.72, oak, 0, top - 0.02, cz);
        [[-0.7, 0.1], [0.7, 0.1], [-0.7, 0.74], [0.7, 0.74]].forEach(function (p) { box(g, 0.05, top - 0.04, 0.05, oak2, p[0], (top - 0.04) / 2, p[1]); });
        box(g, 1.35, 0.1, 0.025, oak2, 0, top - 0.09, 0.1); box(g, 1.35, 0.1, 0.025, oak2, 0, top - 0.09, 0.74);
        box(g, 0.025, 0.1, 0.62, oak2, -0.7, top - 0.09, cz); box(g, 0.025, 0.1, 0.62, oak2, 0.7, top - 0.09, cz);
        box(g, 0.46, 0.085, 0.02, mat(C.walnut), 0.38, top - 0.09, 0.756);
        ball(g, 0.016, mat(C.brass, {metal: 0.6, rough: 0.35}), 0.38, top - 0.09, 0.775);
        /* an open notebook and its pen, a few loose sheets, a pot of pencils, a cup */
        var nb = grp(g, -0.1, top, 0.5, 0.12);
        box(nb, 0.36, 0.008, 0.25, tone, 0, 0.004, 0);
        box(nb, 0.165, 0.012, 0.23, paper, -0.086, 0.014, 0, {rz: 0.04}); box(nb, 0.165, 0.012, 0.23, paper, 0.086, 0.014, 0, {rz: -0.04});
        rod(nb, [0.03, 0.03, 0.07], [0.15, 0.03, -0.05], 0.006, mat(C.iron));
        box(g, 0.21, 0.025, 0.297, paper, -0.5, top + 0.0125, 0.36, {ry: -0.1});
        cyl(g, 0.042, 0.036, 0.1, 10, tone, 0.55, top + 0.05, 0.24);
        [[0.02, 0xe3b23c], [-0.015, 0x4f79a8], [0.0, 0x3d3a40]].forEach(function (p, i) {
          rod(g, [0.55 + p[0], top + 0.03, 0.24], [0.55 + p[0] * 2.4, top + 0.19, 0.24 + (i - 1) * 0.02], 0.005, mat(p[1]));
        });
        cyl(g, 0.038, 0.032, 0.085, 10, mat(C.white, {rough: 0.4}), 0.3, top + 0.0425, 0.2);
        /* the chair comes in with the desk, then tucks itself under */
        var ch = live(g, 0.1, 0, 1.0, 0.3);
        box(ch, 0.42, 0.035, 0.42, oak, 0, 0.45, 0);
        [[-0.185, -0.185], [0.185, -0.185]].forEach(function (p) { box(ch, 0.035, 0.44, 0.035, oak2, p[0], 0.22, p[1]); });
        [-0.185, 0.185].forEach(function (x) { box(ch, 0.035, 0.92, 0.035, oak2, x, 0.46, 0.19); });
        box(ch, 0.4, 0.11, 0.02, oak, 0, 0.84, 0.19); box(ch, 0.4, 0.06, 0.02, oak, 0, 0.66, 0.19);
        box(ch, 0.38, 0.03, 0.38, tone, 0, 0.482, -0.005);
        return {more: function (b) { ch.position.z = 1.0 + (1 - ease(b)) * 0.9; ch.rotation.y = 0.3 + (1 - ease(b)) * 0.7; }};
      });

    place(2, {at: [X0 + 0.38, 0, Z0 + 0.38], on: 'floor', how: 'drop', foot: {w: 0.56, h: 0.56, round: true}, tall: 1.78, face: 'room'},
      function (g, z) {
        var iron = mat(C.iron, {rough: 0.5, metal: 0.4});
        cyl(g, 0.16, 0.17, 0.025, 16, iron, 0, 0.0125, 0);
        cyl(g, 0.012, 0.012, 1.5, 6, mat(C.brass, {metal: 0.6, rough: 0.35}), 0, 0.775, 0);
        var shade = mat(mix(z.tone, 0xf6e6cc, 0.4), {own: true, side: true, rough: 0.9, emissive: 0xffa860, ei: 0});
        cyl(g, 0.13, 0.22, 0.27, 14, shade, 0, 1.6, 0, {open: true, cast: false});
        var bulb = flat(0x6a6570, true);
        ball(g, 0.045, bulb, 0, 1.55, 0, {shadow: false});
        var hl = halo(g, 0, 1.58, 0, 0xffc27a, 1.9);
        var p1 = pool(g, 3.4, 3.4, 0.25, 0.012, 0.25, 0xffa95e, FLOOR);
        var p2 = pool(g, 3.2, 3.0, 0.5, 1.45, -0.368, 0xffb469);
        var p3 = pool(g, 3.2, 3.0, -0.368, 1.45, 0.5, 0xffb469, {ry: Math.PI / 2});
        return {warm: ['A', 1.0], lit: function (k) {
          shade.emissiveIntensity = k * 0.5; bulb.color.setHex(mix(0x6a6570, 0xfff1cf, k));
          glowAt(hl, k * 0.26); glowAt(p1, k * 0.3); glowAt(p2, k * 0.22); glowAt(p3, k * 0.22);
        }};
      });

    place(3, {at: [-4.15, 1.64, Z0], on: 'wall', how: 'pin', foot: {w: 0.38, h: 0.5}},
      function (g, z) {
        box(g, 0.31, 0.43, 0.004, mat(0xd9cdb8), 0, 0, 0.004);
        pane(g, 0.3, 0.42, printed(sheetTex('typed', z.css)), 0, 0, 0.008);
        ball(g, 0.013, mat(z.tone, {rough: 0.4}), 0, 0.185, 0.014);
      });

    place(4, {at: [-2.48, 0, Z0 + 0.3], on: 'floor', how: 'slide', from: [0.3, 0, 1.3], foot: {w: 0.5, h: 0.5, round: true}, tall: 0.42, face: 'room'},
      function (g, z) {
        var wire = mat(mix(z.tone, 0x3a3038, 0.55), {side: true, rough: 0.6}), paper = mat(C.paper);
        cyl(g, 0.15, 0.11, 0.3, 12, wire, 0, 0.15, 0, {open: true});
        cyl(g, 0.11, 0.11, 0.012, 12, wire, 0, 0.006, 0);
        put(g, new THREE.TorusGeometry(0.15, 0.01, 5, 14), wire, 0, 0.3, 0, {rx: Math.PI / 2});
        [[0.03, 0.1, 0.02], [-0.04, 0.17, -0.02], [0.05, 0.21, -0.04]].forEach(function (p, i) {
          put(g, new THREE.IcosahedronGeometry(0.05, 0), paper, p[0], p[1], p[2], {ry: i});
        });
        put(g, new THREE.IcosahedronGeometry(0.045, 0), paper, 0.22, 0.045, 0.1);
        /* one more draft, thrown in once the basket is there */
        var last = live(g, -0.02, 0.27, 0.03);
        put(last, new THREE.IcosahedronGeometry(0.052, 0), paper, 0, 0, 0);
        return {more: function (b) {
          last.visible = b > 0.02;
          last.position.set(-0.02 + (1 - b) * 0.5, 0.27 + Math.sin(b * Math.PI) * 0.55 + (1 - b) * 0.5, 0.03 + (1 - b) * 0.6);
          last.rotation.x = b * 5;
        }};
      });

    place(5, {at: [-3.5, 1.86, Z0], on: 'wall', how: 'hang', foot: {w: 0.6, h: 0.48, y: -0.2}, tag: [0, 0.16, 0.12]},
      function (g, z) {
        var oak = mat(C.oak2);
        box(g, 0.54, 0.42, 0.02, oak, 0, -0.2, 0.01);
        box(g, 0.49, 0.37, 0.006, mat(C.cork, {rough: 1}), 0, -0.2, 0.022, {cast: false});
        box(g, 0.15, 0.25, 0.003, mat(C.paper), -0.12, -0.2, 0.027, {cast: false});
        pane(g, 0.15, 0.25, printed(sheetTex('list', z.css)), -0.12, -0.2, 0.03);
        box(g, 0.1, 0.1, 0.003, mat(z.tone, {rough: 0.8}), 0.1, -0.11, 0.027, {rz: 0.1, cast: false});
        box(g, 0.14, 0.09, 0.003, mat(C.card), 0.09, -0.28, 0.027, {rz: -0.06, cast: false});
        [[-0.12, -0.09], [0.1, -0.07], [0.09, -0.25]].forEach(function (p, i) {
          ball(g, 0.011, mat(i === 1 ? 0xf2efe8 : z.tone, {rough: 0.4}), p[0], p[1], 0.034);
        });
      });

    place(6, {at: [-2.55, 1.98, Z0], on: 'wall', how: 'unroll', foot: {w: 1.0, h: 0.78, y: -0.34}, tag: [0, 0.15, 0.12]},
      function (g, z) {
        var oak = mat(C.walnut), tex = sheetTex('plan', z.css), sw = 0.9, sh = 0.66;
        box(g, 0.96, 0.035, 0.022, oak, 0, 0, 0.012);
        ball(g, 0.012, mat(C.brass, {metal: 0.6, rough: 0.35}), -0.4, 0.0, 0.026); ball(g, 0.012, mat(C.brass, {metal: 0.6, rough: 0.35}), 0.4, 0.0, 0.026);
        /* the sheet hangs from the rail and unrolls downwards; the drawing is uncovered, not stretched */
        var sheet = live(g, 0, -0.012, 0.012);
        var geo = new THREE.PlaneGeometry(sw, sh); geo.translate(0, -sh / 2, 0);
        var sm = printed(tex); sm.side = THREE.DoubleSide;
        var page1 = put(sheet, geo, sm, 0, 0, 0, {cast: false}); page1.userData.keep = true;
        var roll = live(g, 0, -0.012, 0.03);
        cyl(roll, 0.022, 0.022, 0.94, 10, mat(C.paper), 0, 0, 0, {rz: Math.PI / 2});
        cyl(roll, 0.012, 0.012, 0.98, 8, oak, 0, 0, 0, {rz: Math.PI / 2});
        return {more: function (b) {
          var u = Math.max(0.03, ease(b));
          sheet.scale.y = u; tex.repeat.y = u; tex.offset.y = 1 - u;
          roll.position.y = -0.012 - sh * u;
          roll.scale.set(1, 1 - 0.45 * u, 1 - 0.45 * u);
        }};
      });

    /* --- week 2, the measuring bench under the window --------------------------------------- */

    place(7, {at: [X0, 0, -0.4], ry: Math.PI / 2, on: 'floor', how: 'slide', from: [0, 0, 3.8], foot: {w: 2.14, h: 0.84, z: 0.42}, tall: 1.05, face: 'left'},
      function (g, z) {
        var beech = mat(0xd3aa74), leg = mat(C.oak2), blue = mat(z.tone, {rough: 0.5, metal: 0.3}), steel = mat(C.steel, {metal: 0.6, rough: 0.4});
        var top = 0.88, cz = 0.4;
        box(g, 2.0, 0.07, 0.66, beech, 0, top - 0.035, cz);
        [[-0.88, 0.14], [0.88, 0.14], [-0.88, 0.66], [0.88, 0.66]].forEach(function (p) { box(g, 0.09, top - 0.07, 0.09, leg, p[0], (top - 0.07) / 2, p[1]); });
        box(g, 1.76, 0.1, 0.035, leg, 0, top - 0.13, 0.14); box(g, 1.76, 0.1, 0.035, leg, 0, top - 0.13, 0.66);
        box(g, 1.8, 0.03, 0.5, beech, 0, 0.22, cz);
        /* the vice at the end, a small piece of work held in it */
        box(g, 0.2, 0.11, 0.04, blue, -0.6, top - 0.04, 0.75); box(g, 0.2, 0.11, 0.04, blue, -0.6, top - 0.04, 0.84);
        cyl(g, 0.014, 0.014, 0.22, 8, steel, -0.6, top - 0.06, 0.86, {rx: Math.PI / 2});
        cyl(g, 0.008, 0.008, 0.18, 6, steel, -0.6, top - 0.06, 0.97, {rz: Math.PI / 2});
        box(g, 0.16, 0.14, 0.035, mat(0xe6c592), -0.6, top + 0.04, 0.795);
        /* the piece being made, and what measures it */
        box(g, 0.26, 0.11, 0.17, mat(0xe6c592), -0.2, top + 0.055, 0.4, {ry: 0.2});
        box(g, 0.22, 0.012, 0.13, mat(C.walnut), -0.2, top + 0.116, 0.4, {ry: 0.2});
        box(g, 0.34, 0.004, 0.03, steel, 0.3, top + 0.002, 0.52, {ry: -0.15});
        box(g, 0.2, 0.005, 0.02, steel, 0.62, top + 0.003, 0.35, {ry: 0.5});
        box(g, 0.02, 0.005, 0.07, steel, 0.55, top + 0.003, 0.35, {ry: 0.5}); box(g, 0.02, 0.005, 0.07, steel, 0.6, top + 0.003, 0.38, {ry: 0.5});
        box(g, 0.2, 0.012, 0.035, blue, 0.2, top + 0.006, 0.26, {ry: 0.3}); box(g, 0.035, 0.012, 0.14, steel, 0.29, top + 0.006, 0.2, {ry: 0.3});
        rod(g, [0.52, top + 0.006, 0.6], [0.66, top + 0.006, 0.56], 0.005, mat(0xe3b23c));
        box(g, 0.3, 0.2, 0.3, mat(C.kraft), 0.5, 0.335, cz); box(g, 0.24, 0.14, 0.26, mat(z.tone, {rough: 0.8}), -0.45, 0.305, cz);
      });

    place(8, {at: [X0 + 0.44, 0, 1.0], ry: Math.PI / 2, on: 'floor', how: 'drop', foot: {w: 0.5, h: 0.5, round: true}, tall: 1.45, face: 'left', tag: [0, 1.62, 0]},
      function (g, z) {
        var iron = mat(C.iron, {rough: 0.5, metal: 0.4}), blue = mat(z.tone, {rough: 0.5, metal: 0.3});
        cyl(g, 0.17, 0.19, 0.03, 16, iron, 0, 0.015, 0);
        cyl(g, 0.016, 0.016, 0.95, 8, mat(C.steel, {metal: 0.6, rough: 0.4}), 0, 0.5, 0);
        ball(g, 0.028, blue, 0, 0.98, 0);
        rod(g, [0, 0.98, 0], [0.13, 1.4, 0], 0.011, iron); rod(g, [0.03, 0.98, 0], [0.16, 1.4, 0], 0.006, iron);
        ball(g, 0.026, blue, 0.14, 1.4, 0);
        rod(g, [0.14, 1.4, 0], [0.5, 1.3, 0], 0.011, iron);
        /* the head: a ring of light around a lens, held flat over the work */
        var head = grp(g, 0.6, 1.27, 0); head.rotation.z = -0.25;
        put(head, new THREE.TorusGeometry(0.1, 0.022, 6, 18), blue, 0, 0, 0, {rx: Math.PI / 2});
        var ring = flat(0x8d9099, true);
        put(head, new THREE.TorusGeometry(0.1, 0.01, 4, 18), ring, 0, -0.02, 0, {rx: Math.PI / 2, shadow: false});
        var lens = disc(head, 0.085, mat(0xd6ecff, {own: true, alpha: 0.4, rough: 0.05, side: true}), 0, 0, 0, {rx: -Math.PI / 2, shadow: false});
        lens.userData.keep = true;
        var hl = halo(g, 0.6, 1.24, 0, 0xdcebff, 0.9), p = pool(g, 0.8, 0.7, 0.62, 0.885, -0.02, 0xcfe2ff, FLOOR);
        return {warm: ['A', 0.22], lit: function (k) { ring.color.setHex(mix(0x8d9099, 0xffffff, k)); glowAt(hl, k * 0.4); glowAt(p, k * 0.16); }};
      });

    place(9, {at: [X0, 0, 2.45], ry: Math.PI / 2, on: 'floor', how: 'slide', from: [2.6, 0, 0.4], foot: {w: 0.62, h: 0.84, z: 0.42}, tall: 1.3, face: 'left'},
      function (g, z) {
        var iron = mat(C.iron, {rough: 0.5, metal: 0.4}), steel = mat(C.steel, {metal: 0.6, rough: 0.4}), blue = mat(z.tone, {rough: 0.5, metal: 0.3});
        box(g, 0.5, 0.06, 0.6, iron, 0, 0.05, 0.45);
        box(g, 0.44, 0.02, 0.5, steel, 0, 0.09, 0.48);
        [[-0.2, 0.2], [0.2, 0.2], [-0.2, 0.7], [0.2, 0.7]].forEach(function (p) { cyl(g, 0.025, 0.03, 0.02, 8, iron, p[0], 0.01, p[1]); });
        box(g, 0.07, 1.0, 0.06, iron, 0, 0.55, 0.13);
        cyl(g, 0.17, 0.17, 0.07, 20, blue, 0, 1.08, 0.15, {rx: Math.PI / 2});
        disc(g, 0.15, printed(dialTex(z.css, false)), 0, 1.08, 0.187);
        var needle = live(g, 0, 1.08, 0.192);
        box(needle, 0.012, 0.13, 0.004, mat(0xc23a34), 0, 0.05, 0, {cast: false});
        cyl(needle, 0.016, 0.016, 0.008, 10, iron, 0, 0, 0, {rx: Math.PI / 2});
        /* a parcel is set on the plate, and the needle finds its weight */
        var parcel = live(g, 0, 0.1, 0.48, 0.15);
        box(parcel, 0.3, 0.2, 0.26, mat(C.kraft), 0, 0.1, 0);
        box(parcel, 0.31, 0.03, 0.03, mat(z.tone, {rough: 0.8}), 0, 0.19, 0); box(parcel, 0.03, 0.03, 0.27, mat(z.tone, {rough: 0.8}), 0, 0.19, 0);
        return {more: function (b) {
          var d = ease(step(0, 0.5, b));
          parcel.visible = b > 0.01; parcel.position.y = 0.1 + (1 - d) * 1.3;
          var s = step(0.45, 1, b);
          needle.rotation.z = 2.2 - s * 3.1 + Math.sin(s * 16) * 0.35 * (1 - s);
        }};
      });

    place(10, {at: [-3.25, 0, -0.15], ry: -Math.PI / 2, on: 'floor', how: 'drop', foot: {w: 0.92, h: 0.92, round: true}, tall: 1.48, face: 'left'},
      function (g, z) {
        var iron = mat(C.iron, {rough: 0.5, metal: 0.4}), dark = mat(0x26262c, {rough: 0.5});
        [0.5, 0.5 + TAU / 3, 0.5 + 2 * TAU / 3].forEach(function (a) {
          rod(g, [0, 1.02, 0], [Math.cos(a) * 0.4, 0.02, Math.sin(a) * 0.4], 0.014, iron);
          ball(g, 0.022, dark, Math.cos(a) * 0.4, 0.02, Math.sin(a) * 0.4);
        });
        cyl(g, 0.05, 0.05, 0.06, 8, dark, 0, 1.03, 0);
        cyl(g, 0.014, 0.014, 0.26, 6, mat(C.steel, {metal: 0.6, rough: 0.4}), 0, 1.16, 0);
        box(g, 0.07, 0.05, 0.07, dark, 0, 1.3, 0);
        rod(g, [0, 1.3, 0], [-0.16, 1.22, -0.1], 0.008, iron);
        /* the camera looks down at the piece of work on the bench */
        var cam = grp(g, 0, 1.38, 0); cam.rotation.x = 0.32;
        box(cam, 0.17, 0.11, 0.09, dark, 0, 0, 0);
        box(cam, 0.07, 0.035, 0.07, dark, -0.02, 0.07, 0);
        box(cam, 0.172, 0.022, 0.092, mat(z.tone, {rough: 0.5}), 0, 0.02, 0);
        cyl(cam, 0.042, 0.046, 0.1, 12, dark, 0.015, -0.005, 0.09, {rx: Math.PI / 2});
        disc(cam, 0.034, mat(0x9fc4ee, {rough: 0.1, metal: 0.5}), 0.015, -0.005, 0.141);
        ball(cam, 0.012, mat(C.steel, {metal: 0.6, rough: 0.4}), 0.06, 0.06, 0.01);
        var fl = halo(g, 0.015, 1.36, 0.15, 0xffffff, 1.4);
        return {more: function (b) { glowAt(fl, Math.max(0, Math.sin(step(0.25, 0.6, b) * Math.PI)) * 0.95); }};
      });

    place(11, {at: [X0, 1.84, 1.6], ry: Math.PI / 2, on: 'wall', how: 'hang', foot: {w: 0.44, h: 0.6, y: -0.27}, tag: [0, 0.14, 0.12]},
      function (g, z) {
        var steel = mat(C.steel, {metal: 0.6, rough: 0.4});
        cyl(g, 0.008, 0.008, 0.04, 6, steel, 0, 0, 0.02, {rx: Math.PI / 2});
        box(g, 0.34, 0.48, 0.012, mat(0x9a6c40), 0, -0.27, 0.012);
        box(g, 0.14, 0.05, 0.022, steel, 0, -0.06, 0.03); box(g, 0.05, 0.05, 0.012, steel, 0, -0.02, 0.022);
        box(g, 0.3, 0.42, 0.004, mat(C.paper), 0, -0.29, 0.021, {cast: false});
        pane(g, 0.3, 0.42, printed(sheetTex('check', z.css)), 0, -0.29, 0.025);
      });

    place(12, {at: [X0, 2.02, 2.45], ry: Math.PI / 2, on: 'wall', how: 'pin', foot: {w: 0.34, h: 0.56, y: 0.1}, deep: 0.36, tag: [0, 0.5, 0.16]},
      function (g, z) {
        var iron = mat(C.iron, {rough: 0.5, metal: 0.4});
        box(g, 0.1, 0.14, 0.02, iron, 0, -0.06, 0.01); box(g, 0.05, 0.035, 0.2, iron, 0, -0.1, 0.1);
        cyl(g, 0.014, 0.014, 0.1, 8, mat(C.steel, {metal: 0.6, rough: 0.4}), 0, -0.04, 0.18);
        cyl(g, 0.055, 0.055, 0.025, 14, iron, 0, 0.02, 0.18);
        var green = mat(0x2c5a3c, {own: true, rough: 0.35, emissive: 0x48ff8c, ei: 0}), red = mat(0x7a2a2c, {rough: 0.35});
        cyl(g, 0.05, 0.05, 0.1, 14, green, 0, 0.083, 0.18, {cast: false});
        cyl(g, 0.055, 0.055, 0.012, 14, iron, 0, 0.139, 0.18);
        cyl(g, 0.05, 0.05, 0.1, 14, red, 0, 0.195, 0.18, {cast: false});
        cyl(g, 0.055, 0.04, 0.03, 14, iron, 0, 0.26, 0.18);
        var hl = halo(g, 0, 0.083, 0.2, 0x5dff9a, 0.7), p = pool(g, 1.0, 1.0, 0, 0.08, 0.012, 0x3fe07c);
        return {lit: function (k) { green.emissiveIntensity = k * 1.25; glowAt(hl, k * 0.5); glowAt(p, k * 0.16); }};
      });

    /* --- week 3, the wall of shelves --------------------------------------------------------- */

    function bookcase(g) {
      var oak = mat(C.oak), back = mat(mix(C.oak2, 0x6a4a30, 0.35)), w = 0.85, d = 0.32, levels = [0.08, 0.47, 0.86, 1.25, 1.64];
      box(g, 0.025, 2.0, d, oak, -w / 2 + 0.0125, 1.0, d / 2); box(g, 0.025, 2.0, d, oak, w / 2 - 0.0125, 1.0, d / 2);
      box(g, w, 0.025, d, oak, 0, 1.9875, d / 2); box(g, w - 0.05, 0.012, 0.012, back, 0, 1.0, 0.006);
      box(g, w - 0.04, 1.92, 0.012, back, 0, 1.02, 0.008);
      box(g, w - 0.05, 0.08, d - 0.03, mat(C.oak2), 0, 0.04, d / 2 - 0.015);
      levels.forEach(function (y) { box(g, w - 0.05, 0.025, d - 0.02, oak, 0, y + 0.0125, d / 2 + 0.005); });
      return levels;
    }
    place(13, {at: [-1.575, 0, Z0], on: 'floor', how: 'slide', from: [0, 0, 3.3], foot: {w: 0.95, h: 0.44, z: 0.2}, tall: 2.0},
      function (g, z) {
        var levels = bookcase(g), label = mat(C.paper);
        var cols = [z.tone, 0xe9c46a, C.cream, C.slate, mix(z.tone, 0x5a3d1c, 0.45), 0xf2e2b6];
        levels.forEach(function (y, row) {
          var x = -0.385, n = 0;
          if (row === 0) {
            /* at the bottom: magazine files */
            [-0.26, -0.12, 0.02].forEach(function (mx, i) { box(g, 0.11, 0.3, 0.25, mat(i === 1 ? z.tone : C.kraft), mx, y + 0.175, 0.16); box(g, 0.06, 0.05, 0.003, label, mx, y + 0.25, 0.287, {cast: false}); });
            return;
          }
          while (x < 0.33 && !(row === 4 && n > 6)) {
            var bw = 0.055 + rnd() * 0.025, bh = 0.3 + rnd() * 0.025, col = cols[Math.floor(rnd() * cols.length)];
            if (x + bw > 0.385) break;
            box(g, bw - 0.004, bh, 0.26, mat(col, {rough: 0.75}), x + bw / 2, y + 0.025 + bh / 2, 0.16);
            box(g, bw * 0.62, 0.085, 0.003, label, x + bw / 2, y + 0.025 + bh * 0.66, 0.292, {cast: false});
            cyl(g, 0.009, 0.009, 0.004, 8, mat(C.iron), x + bw / 2, y + 0.085, 0.292, {rx: Math.PI / 2, cast: false});
            x += bw; n++;
          }
          if (row === 4) box(g, 0.06, 0.3, 0.26, mat(C.slate, {rough: 0.75}), x + 0.075, y + 0.17, 0.16, {rz: -0.22});
        });
      });

    place(14, {at: [1.15, 2.1, Z0], on: 'wall', how: 'hang', foot: {w: 0.78, h: 0.98, y: -0.45}, tag: [0, 0.15, 0.12]},
      function (g, z) {
        /* each tool as flat parts on the board: [material, x, y, w, h, turn]; the same list paints the outline behind it */
        var steel = mat(C.steel, {metal: 0.6, rough: 0.4}), wood = mat(C.oak2), tone = mat(z.tone, {rough: 0.6}), dark = mat(C.iron);
        var tools = [
          [[steel, -0.06, 0.3, 0.4, 0.085, 0], [wood, 0.2, 0.3, 0.12, 0.13, 0]],
          [[wood, -0.25, -0.02, 0.032, 0.3, 0], [steel, -0.25, 0.13, 0.14, 0.05, 0]],
          [[tone, -0.12, 0.1, 0.036, 0.1, 0], [steel, -0.12, -0.02, 0.012, 0.15, 0]],
          [[tone, -0.03, 0.11, 0.03, 0.08, 0], [steel, -0.03, 0.01, 0.01, 0.12, 0]],
          [[steel, 0.08, 0.02, 0.03, 0.22, 0], [steel, 0.08, 0.15, 0.058, 0.05, 0], [steel, 0.08, -0.11, 0.05, 0.045, 0]],
          [[steel, 0.2, 0.0, 0.035, 0.28, 0], [wood, 0.25, -0.125, 0.15, 0.036, 0]],
          [[tone, -0.24, -0.3, 0.02, 0.15, 0.13], [tone, -0.2, -0.3, 0.02, 0.15, -0.13], [steel, -0.22, -0.2, 0.04, 0.07, 0]],
          [[tone, -0.02, -0.3, 0.1, 0.1, 0], [steel, 0.045, -0.33, 0.03, 0.02, 0]],
          [[wood, 0.2, -0.26, 0.03, 0.13, 0], [dark, 0.2, -0.36, 0.065, 0.07, 0]]
        ];
        var bw = 0.7, bh = 0.9, k = 512 / bh;
        var tex = drawn(Math.round(bw * k), 512, function (c, w, h) {
          c.fillStyle = '#ead9b6'; c.fillRect(0, 0, w, h);
          c.fillStyle = 'rgba(90,62,30,.38)';
          for (var x = 0.025; x < bw; x += 0.05) for (var y = 0.025; y < bh; y += 0.05) { c.beginPath(); c.arc(x * k, y * k, 2.2, 0, TAU); c.fill(); }
          tools.forEach(function (t) {
            t.forEach(function (p) {
              c.save(); c.translate((p[1] + bw / 2) * k, (bh / 2 - p[2]) * k); c.rotate(-p[5]);
              c.fillStyle = z.css; c.globalAlpha = 0.42;
              c.fillRect(-(p[3] / 2 + 0.012) * k, -(p[4] / 2 + 0.012) * k, (p[3] + 0.024) * k, (p[4] + 0.024) * k);
              c.restore();
            });
          });
        });
        cyl(g, 0.008, 0.008, 0.04, 6, steel, 0, 0, 0.02, {rx: Math.PI / 2});
        box(g, bw + 0.05, bh + 0.05, 0.02, wood, 0, -0.45, 0.01);
        pane(g, bw, bh, printed(tex), 0, -0.45, 0.022);
        var parts = tools.map(function (t, i) {
          var tg = live(g, 0, -0.45, 0.03);
          t.forEach(function (p) { box(tg, p[3], p[4], p[0] === steel ? 0.008 : 0.02, p[0], p[1], p[2], p[0] === steel ? 0.004 : 0.01, {rz: p[5]}); });
          return tg;
        });
        return {more: function (b) {
          parts.forEach(function (tg, i) {
            var u = step(i * 0.09, i * 0.09 + 0.22, b);
            tg.visible = u > 0.01; tg.position.z = 0.03 + (1 - u) * 0.5; tg.position.y = -0.45 + (1 - u) * 0.12;
          });
        }};
      });

    place(15, {at: [0.225, 0, Z0], on: 'floor', how: 'slide', from: [0, 0, 3.0], foot: {w: 0.86, h: 0.56, z: 0.26}, tall: 1.38, tag: [0.26, 1.2, 0.42]},
      function (g, z) {
        var wal = mat(C.walnut), front = mat(mix(C.walnut, C.oak, 0.45)), brass = mat(C.brass, {metal: 0.6, rough: 0.35}), w = 0.75, d = 0.45;
        box(g, w, 0.93, d, wal, 0, 0.575, d / 2);
        box(g, w + 0.04, 0.03, d + 0.03, mat(C.oak), 0, 1.055, d / 2 + 0.005);
        [[-0.33, 0.05], [0.33, 0.05], [-0.33, 0.4], [0.33, 0.4]].forEach(function (p) { box(g, 0.05, 0.11, 0.05, wal, p[0], 0.055, p[1]); });
        /* twenty small drawers of index cards; one is pulled out */
        var open = null;
        for (var r = 0; r < 5; r++) for (var c = 0; c < 4; c++) {
          var x = -0.27 + c * 0.18, y = 0.21 + r * 0.175, pulled = r === 3 && c === 2;
          var dg = pulled ? (open = live(g, x, y, d)) : grp(g, x, y, d);
          box(dg, 0.165, 0.155, 0.016, front, 0, 0, 0.008);
          box(dg, 0.075, 0.04, 0.006, brass, 0, 0.025, 0.018); box(dg, 0.06, 0.026, 0.002, mat(C.paper), 0, 0.025, 0.022, {cast: false});
          ball(dg, 0.011, brass, 0, -0.035, 0.024);
          if (pulled) {
            box(dg, 0.15, 0.012, 0.3, wal, 0, -0.065, -0.15); box(dg, 0.012, 0.11, 0.3, wal, -0.07, -0.01, -0.15); box(dg, 0.012, 0.11, 0.3, wal, 0.07, -0.01, -0.15);
            for (var i = 0; i < 9; i++) box(dg, 0.125, 0.105 + (i % 4 === 1 ? 0.02 : 0), 0.004, mat(i % 4 === 1 ? z.tone : C.card), 0, 0.0 + (i % 4 === 1 ? 0.01 : 0), -0.03 - i * 0.027, {cast: false});
          }
        }
        /* a reading lamp on top, the kind a library has */
        var L = grp(g, -0.12, 1.07, 0.2);
        box(L, 0.16, 0.018, 0.1, brass, 0, 0.009, 0); cyl(L, 0.008, 0.008, 0.2, 6, brass, 0, 0.11, -0.03);
        var shade = mat(z.tone, {own: true, rough: 0.35, emissive: 0xffc468, ei: 0});
        cyl(L, 0.05, 0.05, 0.26, 10, shade, 0, 0.23, 0.0, {rz: Math.PI / 2, cast: false});
        var hl = halo(g, -0.12, 1.26, 0.22, 0xffcf80, 1.3), p1 = pool(g, 1.1, 0.8, -0.05, 1.073, 0.24, 0xffc070, FLOOR);
        var p2 = pool(g, 1.5, 1.3, -0.12, 1.3, 0.012, 0xffb469);
        return {warm: ['B', 0.3], lit: function (k) { shade.emissiveIntensity = k * 0.85; glowAt(hl, k * 0.4); glowAt(p1, k * 0.32); glowAt(p2, k * 0.22); },
                more: function (b) { open.position.z = d + ease(step(0.1, 0.7, b)) * 0.2; }};
      });

    place(16, {at: [-0.675, 0, Z0], on: 'floor', how: 'drop', foot: {w: 0.95, h: 0.44, z: 0.2}, tall: 2.0, drop: 3.6},
      function (g, z) {
        var levels = bookcase(g), label = mat(C.paper), cols = [C.kraft, z.tone, C.cream, mix(z.tone, 0x5a3d1c, 0.4), C.kraft];
        levels.forEach(function (y, row) {
          if (row === 4) {
            box(g, 0.36, 0.2, 0.27, mat(C.cream), -0.2, y + 0.125, 0.16);
            [0, 1, 2, 3].forEach(function (i) { box(g, 0.3, 0.022, 0.23, mat(i % 2 ? z.tone : C.card), 0.2, y + 0.037 + i * 0.023, 0.16, {ry: (i - 1.5) * 0.06}); });
            return;
          }
          [-0.2, 0.2].forEach(function (x, i) {
            var col = cols[(row * 2 + i) % cols.length];
            box(g, 0.37, 0.27, 0.27, mat(col, {rough: 0.9}), x, y + 0.16, 0.16);
            box(g, 0.385, 0.05, 0.285, mat(mix(col, 0x3a2a1a, 0.18), {rough: 0.9}), x, y + 0.275, 0.16);
            box(g, 0.16, 0.07, 0.003, label, x, y + 0.2, 0.297, {cast: false});
            box(g, 0.1, 0.012, 0.002, mat(z.tone), x - 0.02, y + 0.21, 0.3, {cast: false});
            cyl(g, 0.022, 0.022, 0.004, 10, mat(0x3a2a1a), x, y + 0.1, 0.297, {rx: Math.PI / 2, cast: false});
          });
        });
      });

    place(17, {at: [1.15, 0, Z0 + 0.03], on: 'floor', how: 'slide', from: [1.2, 0, 2.6], foot: {w: 0.66, h: 0.52, z: 0.25}, tall: 0.98, face: 'room'},
      function (g, z) {
        var steel = mat(C.steel, {metal: 0.6, rough: 0.4}), oak = mat(C.oak), body = mat(0xece4d4, {rough: 0.55}), paper = mat(C.paper);
        [[-0.24, 0.08], [0.24, 0.08], [-0.24, 0.42], [0.24, 0.42]].forEach(function (p) {
          cyl(g, 0.012, 0.012, 0.66, 6, steel, p[0], 0.39, p[1]); ball(g, 0.028, mat(C.iron), p[0], 0.03, p[1]);
        });
        [0.1, 0.4, 0.72].forEach(function (y) { box(g, 0.54, 0.022, 0.4, oak, 0, y, 0.25); });
        /* the label printer, and the strip it feeds out */
        box(g, 0.26, 0.15, 0.2, body, 0.02, 0.806, 0.22);
        box(g, 0.262, 0.03, 0.202, mat(z.tone, {rough: 0.5}), 0.02, 0.86, 0.22);
        box(g, 0.14, 0.014, 0.01, mat(C.iron), 0.02, 0.77, 0.322);
        ball(g, 0.008, flat(0x58e08a), 0.12, 0.82, 0.322, {shadow: false});
        var strip = live(g, 0.02, 0.771, 0.322);
        var sg = new THREE.BoxGeometry(0.1, 0.002, 0.2); sg.translate(0, 0, 0.1);
        put(strip, sg, paper, 0, 0, 0, {cast: false});
        [0.04, 0.1, 0.16].forEach(function (zz) { box(strip, 0.07, 0.003, 0.03, mat(z.tone, {rough: 0.8}), 0, 0.001, zz, {cast: false}); });
        [[-0.14, 0.07], [0.0, 0.06], [0.14, 0.07]].forEach(function (p, i) {
          cyl(g, p[1], p[1], 0.09, 14, paper, p[0], 0.411 + p[1], 0.25, {rx: Math.PI / 2});
          cyl(g, 0.02, 0.02, 0.092, 8, mat(C.kraft), p[0], 0.411 + p[1], 0.25, {rx: Math.PI / 2});
        });
        box(g, 0.3, 0.16, 0.24, mat(C.kraft), -0.08, 0.191, 0.25); box(g, 0.12, 0.1, 0.2, mat(z.tone, {rough: 0.8}), 0.17, 0.161, 0.25);
        return {more: function (b) { strip.scale.z = 0.05 + ease(step(0.15, 1, b)) * 0.95; strip.rotation.x = 0.5 * ease(b); }};
      });

    place(18, {at: [0.225, 1.62, Z0], on: 'wall', how: 'pin', foot: {w: 0.9, h: 0.66, y: 0.2}, deep: 0.26, tag: [0, 0.66, 0.14]},
      function (g, z) {
        var oak = mat(C.oak), iron = mat(C.iron), glassM = mat(0xdff0f6, {own: true, alpha: 0.3, rough: 0.08}), label = mat(C.paper);
        var fills = [z.tone, C.rust, C.sage, C.slate, 0xe9c46a, C.cream, 0x8a5a3a];
        var rows = [0, 0.34].map(function (y, row) {
          box(g, 0.82, 0.025, 0.17, oak, 0, y, 0.085);
          [-0.33, 0.33].forEach(function (x) { box(g, 0.02, 0.1, 0.02, iron, x, y - 0.06, 0.012); rod(g, [x, y - 0.1, 0.02], [x, y - 0.015, 0.14], 0.007, iron); });
          /* a row of labelled jars is set down on each shelf */
          var jg = live(g, 0, y + 0.0125, 0.085), n = row ? 5 : 6;
          for (var i = 0; i < n; i++) {
            var x = (i - (n - 1) / 2) * (row ? 0.15 : 0.13), hgt = row ? 0.15 : 0.12, rr = row ? 0.05 : 0.045, lvl = hgt * (0.35 + rnd() * 0.5);
            cyl(jg, rr * 0.86, rr * 0.86, lvl, 10, mat(fills[(i + row * 3) % fills.length], {rough: 0.9}), x, lvl / 2 + 0.004, 0, {cast: false});
            var gl = cyl(jg, rr, rr, hgt, 10, glassM, x, hgt / 2, 0, {cast: false}); gl.renderOrder = 3;
            cyl(jg, rr * 1.04, rr * 1.04, 0.02, 10, mat(i % 2 ? C.steel : z.tone, {metal: 0.4, rough: 0.5}), x, hgt + 0.01, 0);
            box(jg, rr * 1.1, 0.04, 0.002, label, x, hgt * 0.5, rr + 0.002, {cast: false});
          }
          return jg;
        });
        return {more: function (b) {
          rows.forEach(function (jg, i) {
            var u = ease(step(i * 0.3, i * 0.3 + 0.6, b));
            jg.visible = b > 0.01 + i * 0.3; jg.position.y = i * 0.34 + 0.0125 + (1 - u) * 0.6;
          });
        }};
      });

    /* --- week 4, the other desks, the board, and the way out ------------------------------------ */

    place(19, {at: [2.3, 1.98, Z0], on: 'wall', how: 'pin', foot: {w: 1.2, h: 0.9, y: -0.4}, tag: [-0.48, 0.14, 0.12]},
      function (g, z) {
        var oak = mat(C.oak2), bw = 1.1, bh = 0.8;
        var tex = drawn(440, 320, function (c, w, h) {
          c.fillStyle = '#f7f3ea'; c.fillRect(0, 0, w, h);
          [0, 1, 2].forEach(function (i) {
            c.fillStyle = i === 2 ? z.css : 'rgba(70,60,90,.55)'; c.fillRect(14 + i * 142, 14, 128, 20);
            c.fillStyle = 'rgba(255,255,255,.85)'; c.fillRect(22 + i * 142, 21, 50 + i * 12, 6);
            if (i) { c.fillStyle = 'rgba(70,60,90,.25)'; c.fillRect(6 + i * 142, 12, 2, h - 24); }
          });
        });
        box(g, bw + 0.06, bh + 0.06, 0.025, oak, 0, -0.4, 0.0125);
        pane(g, bw, bh, printed(tex), 0, -0.4, 0.027);
        box(g, bw * 0.5, 0.02, 0.05, oak, 0, -0.82, 0.04);
        /* the cards go up column by column: to do, in progress, done */
        var tints = [0xffe27a, mix(z.tone, 0xffffff, 0.45), 0xffffff, mix(z.tone, 0xffffff, 0.15), 0xa6dcb4];
        var colsN = [4, 2, 3], n = 0;
        var cols = colsN.map(function (count, c) {
          var cg = live(g, -0.355 + c * 0.355, -0.4, 0.03);
          for (var i = 0; i < count; i++) {
            var tint = c === 2 ? tints[4] : tints[n++ % 4];
            box(cg, 0.25, 0.1, 0.004, mat(tint, {rough: 0.85}), (rnd() - 0.5) * 0.03, 0.22 - i * 0.135, 0.002, {rz: (rnd() - 0.5) * 0.09, cast: false});
            box(cg, 0.15, 0.012, 0.002, mat(0x6a6078), -0.03, 0.235 - i * 0.135, 0.006, {cast: false});
          }
          return cg;
        });
        return {more: function (b) {
          cols.forEach(function (cg, i) {
            var u = step(i * 0.25, i * 0.25 + 0.4, b);
            cg.visible = u > 0.01; cg.position.z = 0.03 + (1 - u) * 0.6; cg.position.y = -0.4 - (1 - u) * 0.1;
          });
        }};
      });

    place(20, {at: [2.3, 2.4, Z0], on: 'wall', how: 'hang', foot: {w: 0.5, h: 0.5, round: true}, tag: [0.36, 0, 0.12]},
      function (g, z) {
        var rim = mat(mix(z.tone, 0x2a2030, 0.35), {rough: 0.5}), dark = mat(0x2c2832);
        cyl(g, 0.2, 0.2, 0.05, 28, rim, 0, 0, 0.025, {rx: Math.PI / 2});
        disc(g, 0.178, printed(dialTex(z.css, true)), 0, 0, 0.052);
        var hh = live(g, 0, 0, 0.056), mh = live(g, 0, 0, 0.06);
        box(hh, 0.018, 0.1, 0.004, dark, 0, 0.042, 0, {cast: false});
        box(mh, 0.012, 0.15, 0.004, dark, 0, 0.066, 0, {cast: false});
        cyl(mh, 0.014, 0.014, 0.008, 10, mat(z.tone), 0, 0, 0.002, {rx: Math.PI / 2});
        /* a quarter past seven, a winter morning; once hung, the clock runs */
        var start = 7.25;
        function hands(hours) { hh.rotation.z = -hours / 12 * TAU; mh.rotation.z = -(hours % 1) * TAU; }
        hands(start);
        return {more: function (b) { if (b < 1) hands(start - (1 - ease(b)) * 2.5); },
                tick: function (t) { hands(start + t / 240); }};
      });

    /* a work desk: its screens, its lamp, its chair */
    function desk(g, z, screens) {
      var topM = mat(0xf1ece3, {rough: 0.6}), iron = mat(C.iron, {rough: 0.5, metal: 0.4}), key = mat(0xddd6cb, {rough: 0.6});
      var side0 = screens === 2 ? 1 : -1;
      box(g, 1.4, 0.03, 0.7, topM, 0, 0.735, 0);
      [-0.62, 0.62].forEach(function (x) {
        box(g, 0.04, 0.7, 0.04, iron, x, 0.35, -0.28); box(g, 0.04, 0.7, 0.04, iron, x, 0.35, 0.28); box(g, 0.04, 0.04, 0.6, iron, x, 0.7, 0);
      });
      box(g, 1.24, 0.05, 0.03, iron, 0, 0.66, -0.28);
      var lit = [];
      (screens === 2 ? [[-0.3, 0.2, 0], [0.3, -0.2, 1]] : [[0.05, 0, 2]]).forEach(function (s) {
        var mo = grp(g, s[0], 0.75, -0.2, s[1]);
        box(mo, 0.2, 0.012, 0.15, iron, 0, 0.006, 0); box(mo, 0.03, 0.2, 0.03, iron, 0, 0.11, -0.02);
        box(mo, 0.58, 0.35, 0.025, mat(0x26242c, {rough: 0.5}), 0, 0.36, 0);
        var sm = new THREE.MeshBasicMaterial({map: screenTex(z.css, s[2]), color: 0x15131a});
        pane(mo, 0.54, 0.31, sm, 0, 0.36, 0.014, {shadow: false});
        lit.push(sm);
      });
      box(g, 0.36, 0.014, 0.12, key, -0.02, 0.757, 0.14); box(g, 0.055, 0.02, 0.09, key, 0.3, 0.76, 0.15);
      box(g, 0.2, 0.012, 0.15, mat(z.tone, {rough: 0.8}), -side0 * 0.5, 0.756, 0.12, {ry: -0.2});
      cyl(g, 0.036, 0.03, 0.085, 10, mat(C.white, {rough: 0.4}), -side0 * 0.47, 0.7925, -0.08);
      /* the desk lamp */
      var side = side0, L = grp(g, side * 0.61, 0.75, 0.2);
      cyl(L, 0.065, 0.07, 0.018, 12, iron, 0, 0.009, 0);
      rod(L, [0, 0.02, 0], [side * 0.03, 0.26, 0.02], 0.008, iron); rod(L, [side * 0.03, 0.26, 0.02], [-side * 0.15, 0.36, -0.04], 0.008, iron);
      var shade = mat(z.tone, {own: true, side: true, rough: 0.5, emissive: 0xffc890, ei: 0});
      var sc = cyl(L, 0.028, 0.075, 0.1, 10, shade, -side * 0.17, 0.33, -0.05, {open: true, cast: false}); sc.rotation.set(-0.25, 0, side * 0.5);
      var hl = halo(g, side * 0.43, 1.06, 0.14, 0xffd2a0, 0.8), p = pool(g, 0.9, 0.7, side * 0.36, 0.752, 0.08, 0xffbd78, FLOOR);
      /* the chair rolls in after the desk */
      var ch = live(g, 0.05, 0, 0.8, -0.35), fabric = mat(0x3c3a44, {rough: 0.95});
      for (var i = 0; i < 5; i++) {
        var a = i * TAU / 5;
        rod(ch, [0, 0.09, 0], [Math.cos(a) * 0.28, 0.04, Math.sin(a) * 0.28], 0.014, iron);
        ball(ch, 0.026, mat(0x202026), Math.cos(a) * 0.28, 0.028, Math.sin(a) * 0.28);
      }
      cyl(ch, 0.022, 0.022, 0.34, 8, mat(C.steel, {metal: 0.6, rough: 0.4}), 0, 0.26, 0);
      box(ch, 0.46, 0.07, 0.45, fabric, 0, 0.465, 0);
      box(ch, 0.04, 0.3, 0.03, iron, 0, 0.6, 0.23);
      box(ch, 0.42, 0.46, 0.055, mat(z.tone, {rough: 0.9}), 0, 0.82, 0.235);
      var white = new THREE.Color(0xffffff), off = new THREE.Color(0x15131a);
      return {warm: ['B', 0.3], lit: function (k) {
        lit.forEach(function (sm) { sm.color.copy(off).lerp(white, k); });
        shade.emissiveIntensity = k * 0.7; glowAt(hl, k * 0.5); glowAt(p, k * 0.42);
      }, more: function (b) { ch.position.z = 0.8 + (1 - ease(b)) * 0.9; ch.rotation.y = -0.35 - (1 - ease(b)) * 0.9; }};
    }

    place(21, {at: [-0.7, 0, 0.95], on: 'floor', how: 'slide', from: [0, 0, 3.4], foot: {w: 1.56, h: 1.6, z: 0.36}, tall: 1.2, face: 'room'},
      function (g, z) { return desk(g, z, 2); });

    place(22, {at: [1.35, 0, 2.15], on: 'floor', how: 'drop', foot: {w: 1.06, h: 1.2, z: 0.26}, tall: 1.42, face: 'room'},
      function (g, z) {
        /* the light one: a standing desk, a laptop, a stool */
        var topM = mat(0xf1ece3, {rough: 0.6}), iron = mat(C.iron, {rough: 0.5, metal: 0.4}), key = mat(0xddd6cb, {rough: 0.6});
        box(g, 0.9, 0.028, 0.55, topM, 0, 1.066, 0);
        [-0.36, 0.36].forEach(function (x) { box(g, 0.05, 1.04, 0.07, iron, x, 0.53, 0); box(g, 0.06, 0.025, 0.55, iron, x, 0.0125, 0); });
        box(g, 0.72, 0.04, 0.03, iron, 0, 0.9, 0);
        box(g, 0.32, 0.014, 0.22, key, 0, 1.087, 0.04);
        var lid = grp(g, 0, 1.094, -0.07); lid.rotation.x = -0.3;
        box(lid, 0.32, 0.21, 0.008, key, 0, 0.105, 0);
        var sm = new THREE.MeshBasicMaterial({map: screenTex(z.css, 0), color: 0x15131a});
        pane(lid, 0.29, 0.18, sm, 0, 0.105, 0.005, {shadow: false});
        cyl(g, 0.034, 0.03, 0.08, 10, mat(z.tone, {rough: 0.5}), 0.32, 1.12, 0.1);
        /* a small clip lamp on the edge */
        box(g, 0.04, 0.05, 0.05, iron, -0.4, 1.07, -0.2);
        rod(g, [-0.4, 1.08, -0.2], [-0.36, 1.34, -0.18], 0.007, iron);
        var shade = mat(z.tone, {own: true, side: true, rough: 0.5, emissive: 0xffc890, ei: 0});
        var sc = cyl(g, 0.022, 0.06, 0.085, 10, shade, -0.33, 1.33, -0.14, {open: true, cast: false}); sc.rotation.set(0.5, 0, -0.5);
        var hl = halo(g, -0.3, 1.3, -0.1, 0xffd2a0, 0.8), p = pool(g, 0.9, 0.6, -0.1, 1.082, 0, 0xffbd78, FLOOR);
        var st = live(g, 0.1, 0, 0.62, 0.4);
        cyl(st, 0.17, 0.17, 0.04, 14, mat(C.oak), 0, 0.72, 0);
        [0, 1, 2].forEach(function (i) { var a = i * TAU / 3; rod(st, [Math.cos(a) * 0.1, 0.7, Math.sin(a) * 0.1], [Math.cos(a) * 0.2, 0, Math.sin(a) * 0.2], 0.013, iron); });
        put(st, new THREE.TorusGeometry(0.15, 0.008, 5, 14), iron, 0, 0.28, 0, {rx: Math.PI / 2});
        var white = new THREE.Color(0xffffff), off = new THREE.Color(0x15131a);
        return {warm: ['B', 0.25], lit: function (k) { sm.color.copy(off).lerp(white, k); shade.emissiveIntensity = k * 0.7; glowAt(hl, k * 0.5); glowAt(p, k * 0.4); },
                more: function (b) { st.position.y = (1 - ease(b)) * 1.6; st.visible = b > 0.01; }};
      });

    place(23, {at: [3.2, 0, 0.8], ry: Math.PI / 2, on: 'floor', how: 'slide', from: [0, 0, 3.2], foot: {w: 1.56, h: 1.6, z: 0.36}, tall: 1.2, face: 'room'},
      function (g, z) { return desk(g, z, 1); });

    place(24, {at: [3.3, 1.82, Z0], on: 'wall', how: 'pin', foot: {w: 0.82, h: 1.12, y: -0.45}, deep: 0.32, tag: [-0.3, 0.14, 0.12]},
      function (g, z) {
        var wal = mat(C.walnut), brass = mat(C.brass, {metal: 0.6, rough: 0.35}), wool = mat(mix(z.tone, 0x2a1c3a, 0.3), {rough: 1});
        box(g, 0.7, 0.075, 0.022, wal, 0, 0, 0.011);
        [-0.24, 0, 0.24].forEach(function (x) { cyl(g, 0.012, 0.012, 0.08, 8, brass, x, 0, 0.06, {rx: Math.PI / 2}); ball(g, 0.018, brass, x, 0, 0.1); });
        /* the coat, hung last, still swinging */
        var coat = live(g, -0.24, 0, 0.075);
        var bodyM = cyl(coat, 0.17, 0.25, 0.88, 6, wool, 0, -0.52, 0); bodyM.scale.z = 0.4;
        box(coat, 0.44, 0.07, 0.11, wool, 0, -0.07, 0);
        [-1, 1].forEach(function (s) { var sl = cyl(coat, 0.05, 0.058, 0.62, 6, wool, s * 0.255, -0.4, 0); sl.rotation.z = s * 0.09; });
        box(coat, 0.2, 0.09, 0.13, mat(mix(z.tone, 0xffffff, 0.2), {rough: 1}), 0, -0.03, 0.005);
        [-0.33, -0.5, -0.67].forEach(function (y) { ball(coat, 0.014, brass, 0.03, y, 0.075); });
        /* a scarf in the four colours of the month */
        var tones = zones.map(function (q) { return mat(q.tone, {rough: 1}); });
        [[-0.045, 9], [0.045, 6]].forEach(function (s) {
          for (var i = 0; i < s[1]; i++) box(g, 0.085, 0.072, 0.02, tones[i % tones.length], s[0], -0.06 - i * 0.07, 0.085 + (s[0] > 0 ? 0.012 : 0));
        });
        box(g, 0.19, 0.05, 0.05, tones[0], 0, -0.02, 0.09);
        /* the key on its hook, and under it the handover folder in its pocket */
        var keyG = live(g, 0.24, -0.01, 0.1);
        put(keyG, new THREE.TorusGeometry(0.022, 0.005, 5, 12), brass, 0, -0.03, 0);
        box(keyG, 0.012, 0.085, 0.005, brass, 0, -0.095, 0); box(keyG, 0.03, 0.012, 0.005, brass, 0.012, -0.125, 0); box(keyG, 0.022, 0.012, 0.005, brass, 0.008, -0.105, 0);
        box(keyG, 0.036, 0.06, 0.006, mat(z.tone, {rough: 0.8}), -0.025, -0.085, 0.004, {rz: 0.3});
        box(g, 0.3, 0.2, 0.012, wal, 0.24, -0.62, 0.006); box(g, 0.3, 0.14, 0.012, wal, 0.24, -0.65, 0.06);
        box(g, 0.3, 0.012, 0.06, wal, 0.24, -0.72, 0.033);
        var folder = live(g, 0.24, -0.5, 0.034);
        box(folder, 0.25, 0.33, 0.016, mat(mix(z.tone, 0xffffff, 0.45), {rough: 0.85}), 0, 0, 0, {rz: 0.03});
        box(folder, 0.09, 0.03, 0.016, mat(z.tone, {rough: 0.85}), 0.07, 0.175, 0);
        box(folder, 0.14, 0.06, 0.003, mat(C.paper), -0.02, 0.09, 0.01, {cast: false});
        return {warm: ['B', 0.3], lit: function (k) {
          sconce.m.emissiveIntensity = k * 1.1; glowAt(sconce.halo, k * 0.3); glowAt(sconce.pool, k * 0.2);
        }, more: function (b) {
          coat.rotation.z = Math.sin(b * 13) * 0.2 * (1 - b) * (1 - b);
          keyG.rotation.z = Math.sin(b * 19) * 0.6 * (1 - b) * (1 - b);
          folder.position.y = -0.5 + (1 - ease(step(0.2, 0.8, b))) * 0.5; folder.visible = b > 0.2;
        }};
      });

    lives.forEach(bake);
    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(function () { recs.forEach(function (r) { r.drawTag(); }); });
    }

    /* --- how an object arrives -------------------------------------------------------------- */

    function pose(r) {
      var t = r.t, on = t > 0.001;
      r.body.visible = on;
      var a = clamp(t / 0.6, 0, 1), e = ease(a), b = clamp((t - 0.6) / 0.4, 0, 1), back = 1 - e;
      if (r.shadow) r.shadow.visible = on;
      if (on) {
        var p = r.body.position, q = r.body.rotation;
        p.set(0, 0, 0); q.set(0, 0, 0);
        if (r.how === 'slide') p.set(r.from.x * back, 0, r.from.z * back);
        else if (r.how === 'drop') p.y = r.drop * back;
        else if (r.how === 'hang') { p.set(0, 0.3 * back, 1.1 * back); q.z = Math.sin(b * 15) * 0.13 * (1 - b) * (1 - b); }
        else if (r.how === 'pin') { p.set(0.2 * back, -0.12 * back, 1.5 * back); q.set(-0.8 * back, 0, 0.3 * back); }
        else if (r.how === 'unroll') p.set(0, 0.3 * back, 1.1 * back);
        r.body.scale.setScalar(Math.min(1, 0.4 + a * 5));
        if (r.more) r.more(b);
        if (r.shadow) {
          /* it follows what slides, and gathers under what is lowered */
          r.shadow.position.set(r.shadowAt[0] + p.x, 0.005, r.shadowAt[1] + p.z);
          r.shadow.material.opacity = (r.how === 'drop' ? e * e : Math.min(1, a * 3)) * (r.shadow.material.map === glowTex ? 0.5 : 0.34);
        }
      }
      if (r.lit) r.lit(on ? step(0.15, 1, b) : 0);
    }

    /* --- states --------------------------------------------------------------------------------- */

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
        if (first || reduced || r.goal === 0) { r.t = r.goal; pose(r); }
        r.drawTag();
        string.set(r.day, r.zone.tone, st === 'open');
      });
      dirty = 3;
    }

    /* --- the camera: a point looked at, an angle around it, a distance ------------------------------ */

    var AZ = [0.14, 1.43], POL = [0.62, 1.42];
    var VIEWS = [
      {t: [-0.1, 0.95, -0.1], az: 0.82, pol: 1.11, dist: 13.5, half: 7.6, tall: 5.3},
      {t: [-3.5, 1.15, -2.9], az: 1.02, pol: 1.16, dist: 6.2, half: 2.3},
      {t: [-4.5, 1.15, 0.5], az: 0.46, pol: 1.14, dist: 7.6, half: 3.0},
      {t: [-0.2, 1.15, -3.2], az: 1.16, pol: 1.18, dist: 6.4, half: 2.5},
      {t: [1.7, 0.9, -0.8], az: 0.95, pol: 1.06, dist: 10.5, half: 4.6}
    ];
    var cam = {t: new THREE.Vector3(), az: 0.79, pol: 1.06, dist: 16};
    var goal = {t: new THREE.Vector3(), az: 0.79, pol: 1.06, dist: 16};
    var mode = {kind: 'over', zone: -1, day: 0};
    var narrow = false;

    /* far enough to hold `half` metres on each side, whatever the shape of the screen */
    function reach(v) {
      var k = Math.tan(camera.fov * Math.PI / 360);
      return Math.max(v.dist, v.half / (k * camera.aspect), v.tall ? v.tall / k : 0);
    }
    function fly(to, instant) {
      goal.t.set(to.t[0], to.t[1], to.t[2]);
      goal.az = to.az; goal.pol = to.pol; goal.dist = reach(to);
      if (instant || reduced) { cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist; }
    }
    function again() {
      if (mode.kind === 'day') fly(focus(byDay[mode.day]));
      else fly(VIEWS[mode.zone + 1]);
    }
    function fit() {
      var w = host.clientWidth, h = host.clientHeight;
      if (!w || !h) return;
      renderer.setSize(w, h, false);
      camera.aspect = w / h;
      narrow = w / h < 0.9;
      camera.fov = narrow ? 40 : w / h < 1.4 ? 32 : 26;
      camera.updateProjectionMatrix();
      labels.forEach(function (l) { l.half = 0; });
      again();
      cam.dist = goal.dist;
    }
    function overview() {
      mode = {kind: 'over', zone: -1, day: 0};
      host.setAttribute('data-mode', 'over');
      fly(VIEWS[0]);
      chips();
    }
    function toZone(i) {
      mode = {kind: 'zone', zone: i, day: 0};
      host.setAttribute('data-mode', 'village');
      fly(VIEWS[i + 1]);
      chips();
    }
    function focus(r) {
      var az = r.face === 'left' ? 0.5 : r.face === 'room' ? 0.86 : 1.1;
      return {t: [r.centre.x, r.centre.y, r.centre.z], az: az, pol: 1.2, dist: clamp(r.radius * 7, 2.2, 11), half: r.radius * 1.7};
    }
    function look(day) {
      var r = byDay[day];
      if (!r) return;
      mode = {kind: 'day', zone: r.zone.i, day: day};
      host.setAttribute('data-mode', 'house');
      fly(focus(r));
      chips();
    }

    /* a click on a day: go to its place; bring the object in if it may; tell the page */
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
        if (st === 'ready') { page.mark(day); paint(); later(function () { page.enter(day); }, 2300); }
        else later(function () { page.enter(day); }, 500);
      }, 850);
    }

    /* --- the pointer ------------------------------------------------------------------------------ */

    var ray = new THREE.Raycaster(), ptr = new THREE.Vector2();
    var drag = null, hoverDay = 0;
    function pick(e) {
      var r = canvas.getBoundingClientRect();
      ptr.set(((e.clientX - r.left) / r.width) * 2 - 1, -((e.clientY - r.top) / r.height) * 2 + 1);
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
        /* a vertical swipe on a phone scrolls the page; a horizontal one turns the room */
        if (drag.touch && !drag.held && Math.abs(dy) > Math.abs(dx) * 1.3) return;
        if (drag.moved > 6) {
          if (!drag.held) { drag.held = true; try { canvas.setPointerCapture(drag.id); } catch (err) {} }
          /* the room is a cutaway: one can turn it, never go behind its walls */
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

    /* --- the names of the four corners, and the chips to go to them ---------------------------------- */

    var labels = [];
    [[-2.9, 3.3, Z0 - 0.1], [X0 - 0.1, 3.3, 0.5], [-0.1, 3.3, Z0 - 0.1], [3.1, 3.3, Z0 - 0.1]].forEach(function (at, i) {
      var z = zones[i];
      if (!z) return;
      var el = document.createElement('button');
      el.type = 'button'; el.className = 'monde-label';
      el.style.setProperty('--tone', z.css);
      var small = document.createElement('small'); small.textContent = page.tx(z.def.sub);
      var strong = document.createElement('b'); strong.textContent = page.tx(z.def.name);
      el.appendChild(small); el.appendChild(strong);
      el.addEventListener('click', function () { toZone(i); });
      host.appendChild(el);
      labels.push({el: el, small: small, at: new THREE.Vector3(at[0], at[1], at[2]), zone: i});
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
    chip(page.say('overview'), '', overview);
    zones.forEach(function (z) { chip(page.tx(z.def.name), z.css, function () { toZone(z.i); }); });
    function chips() {
      chipEls.forEach(function (b, i) {
        b.setAttribute('aria-pressed', (i === 0 ? mode.kind === 'over' : mode.zone === i - 1) ? 'true' : 'false');
      });
    }
    var hint = document.createElement('p');
    hint.className = 'monde-hint';
    hint.textContent = page.say('hint');
    bar.insertBefore(hint, bar.firstChild);

    /* --- the loop ---------------------------------------------------------------------------------- */

    var running = false, visible = true, paused = false, raf = 0, last = 0, clock = 0;
    var warm = {A: 0, B: 0, air: 0};
    var tagAt = new THREE.Vector3();

    function dress(dt) {
      var sumA = 0, sumB = 0, moving = false;
      recs.forEach(function (r) {
        if (r.t !== r.goal) {
          r.t = r.goal > r.t ? Math.min(r.goal, r.t + dt / 2.1) : r.goal;
          pose(r); moving = true;
        }
        if (r.tick && r.t >= 1) r.tick(clock);
        if (r.warm) { var k = step(0.7, 1, r.t) * r.warm[1]; if (r.warm[0] === 'A') sumA += k; else sumB += k; }

        /* the mark on the floor or the wall: chalk while locked, warm when the day may open, gone once the object is there */
        var ready = r.state === 'ready', pulse = reduced ? 0.5 : 0.5 + 0.5 * Math.sin(clock * 2.6 + r.day);
        var want = r.state === 'open' ? 0 : ready ? 0.78 + 0.2 * pulse : 0.72;
        var om = r.outline.material;
        om.opacity += (want * (1 - step(0, 0.5, r.t)) - om.opacity) * Math.min(1, dt * 6);
        om.color.setHex(ready ? 0xffd27a : 0xffffff);
        r.outline.visible = om.opacity > 0.01;

        /* the tag: on the mark, then beside the object once it has arrived */
        var u = ease(r.t);
        tagAt.copy(r.closedAt).lerp(r.openAt, u);
        if (r.shake > 0) { r.shake = Math.max(0, r.shake - dt * 1.6); tagAt.x += Math.sin(r.shake * 34) * 0.07 * r.shake; moving = true; }
        r.tag.position.copy(tagAt);
        r.hover += ((hoverDay === r.day ? 1 : 0) - r.hover) * Math.min(1, dt * 10);
        var size = (r.state === 'open' ? 0.25 : ready ? 0.36 + 0.03 * pulse : 0.31) * (narrow ? 1.75 : 1)
          * (mode.kind === 'over' ? 1 : mode.kind === 'zone' ? 0.72 : mode.day === r.day ? 0.62 : 0.45) * (1 + r.hover * 0.22);
        r.tag.scale.set(size, size, 1);
        r.glow.position.copy(tagAt);
        r.glow.scale.set(size * 2.3, size * 2.3, 1);
        glowAt(r.glow, ready ? 0.3 + 0.22 * pulse : 0);
      });
      /* the more lamps have arrived, the warmer the room */
      var f = Math.min(1, dt * 3);
      warm.A += (Math.min(1.25, sumA) - warm.A) * f; warm.B += (Math.min(1.4, sumB) - warm.B) * f;
      warm.air += (opened / 24 - warm.air) * f;
      fillA.intensity = warm.A * 0.42; fillB.intensity = warm.B * 0.3;
      warmAir.intensity = 0.05 + warm.air * 0.07 + (warm.A + warm.B) * 0.015;
      string.glows.material.opacity = opened >= 24 && !reduced ? 0.8 + 0.2 * Math.sin(clock * 2.2) : 0.9;
      if (moving) dirty = 2;
    }

    function frame(now) {
      raf = 0;
      if (!running) return;
      var dt = Math.min(0.1, (now - last) / 1000 || 0.016);
      last = now; clock += dt;

      var k = 1 - Math.exp(-dt * 3.4);
      cam.t.lerp(goal.t, k);
      cam.az += (goal.az - cam.az) * k; cam.pol += (goal.pol - cam.pol) * k; cam.dist += (goal.dist - cam.dist) * k;
      /* seen whole, the room breathes a little */
      var sway = mode.kind === 'over' && !drag && !reduced ? Math.sin(clock * 0.16) * 0.035 : 0;
      var sp = Math.sin(cam.pol), az = cam.az + sway;
      camera.position.set(cam.t.x + cam.dist * sp * Math.cos(az), cam.t.y + cam.dist * Math.cos(cam.pol), cam.t.z + cam.dist * sp * Math.sin(az));
      camera.lookAt(cam.t);

      dress(dt);
      if (flakes) {
        var p = flakes.geometry.attributes.position, v = flakes.userData.v;
        for (var i = 0; i < v.length; i++) {
          var y = p.array[i * 3 + 1] - v[i] * dt;
          p.array[i * 3] += Math.sin(clock * 0.6 + i) * dt * 0.08;
          p.array[i * 3 + 1] = y < 0 ? 6.5 : y;
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
        spot.copy(l.at).project(camera);
        var show = spot.z < 1 && (mode.kind === 'over' || (mode.kind === 'zone' && mode.zone === l.zone));
        l.el.classList.toggle('off', !show);
        if (!show) return;
        /* seen whole, the room keeps the names only: four full labels would crowd its roofline */
        var full = mode.kind === 'zone';
        if (l.full !== full) { l.full = full; l.small.style.display = full ? '' : 'none'; l.half = 0; }
        var half = (l.half || (l.half = l.el.offsetWidth / 2)) + 8;
        var x = clamp((spot.x + 1) / 2 * w, half, w - half), y = Math.max(64, (1 - spot.y) / 2 * h);
        if (mode.kind === 'zone') { x = w / 2; y = 86; }
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
      /* the page closed what a day showed: step back to its corner of the room */
      back: function () { if (mode.kind === 'day') toZone(mode.zone); },
      overview: overview, zone: toZone, knock: knock,
      /* the calls every scene offers (see apercu_du_monde.js) */
      views: function () { return ['atelier', 'semaine1', 'semaine2', 'semaine3', 'semaine4']; },
      view: function (name) { if (name === 'atelier') overview(); else toZone(Number(name.slice(-1)) - 1); },
      look: look,
      /* for the captures: every camera move and every arrival at its end */
      settle: function () {
        cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist;
        recs.forEach(function (r) { r.t = r.goal; pose(r); r.shake = 0; });
        warm.A = warm.B = -1; dirty = 3;
        dress(10);
      }
    };
  }

  global.SceneAtelier = {mount: mount};
})(window);
