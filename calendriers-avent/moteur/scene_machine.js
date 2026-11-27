/* The reading machine, built one part a day.
 *
 * A workshop at night: a long stepped bench against the window wall, snow
 * outside, a lamp, tools. On the bench, a machine that reads documents is
 * assembled over December. Each day adds one part; the four weeks are the
 * four steps of the bench, each in its own tone and its own trade:
 *   week 1 reads the paper (cream enamel and brass),
 *   week 2 keeps the index (walnut and card stock),
 *   week 3 keeps control (grey consoles, dials, lamps),
 *   week 4 runs the process (workshop green, belts, a stamp, the way out).
 *
 * What a reader sees:
 *   - a locked day is a bare stand with its number on a brass plate;
 *   - a day that may open waits under a dust cloth, its pilot lamp lit;
 *   - an opened day is a part of the machine: it stands on its stand, lights
 *     up and moves, and the paper runs through it;
 *   - sheets leave the feeder and travel through every installed part, as far
 *     as the machine goes, and drop into the catch tray where it ends. On the
 *     24th, the last part prints a slip with a green check.
 *
 * Same calls as the island (`src/monde3d.js`), see `apercu_du_monde.js`.
 * Needs three.js r128 (global `THREE`). Without WebGL, `mount` returns null.
 */
(function (global) {
  'use strict';

  var TAU = Math.PI * 2;
  var PITCH = 2.2;        /* from one part to the next along a row */
  var ROW = 2.6;          /* from one row to the next */
  var STEP = 0.9;         /* a row stands this much higher than the row in front of it */
  var PY = 0.56;          /* the height of the paper above the step it runs on */
  var TURN = ROW / 2;     /* the radius of the slide that takes the paper down a step */
  var SPEED = 1.5;        /* how fast a sheet travels */
  var GAP = 5.6;          /* the distance between two sheets */
  var FLOOR = -4.5;

  /* the island's page says "island" and "door": this scene brings its own words */
  var SAY = {
    over: {en: 'The whole machine', fr: 'Toute la machine'},
    hint: {en: 'drag to look around · click a part', fr: 'glisser pour regarder · cliquer une pièce'},
    days: {en: 'days', fr: 'jours'}
  };

  function mount(host, data, page) {
    var THREE = global.THREE;
    if (!THREE || !host || !data || !data.world || !data.world.villages) return null;
    var reduced = !!(global.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches);
    var renderer;
    try {
      renderer = new THREE.WebGLRenderer({antialias: true, alpha: false, powerPreference: 'high-performance'});
    } catch (e) { return null; }
    renderer.setPixelRatio(Math.min(2, global.devicePixelRatio || 1));
    renderer.outputEncoding = THREE.sRGBEncoding;
    renderer.toneMapping = THREE.NoToneMapping;
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFShadowMap;
    renderer.shadowMap.autoUpdate = false;
    var canvas = renderer.domElement;
    canvas.className = 'monde-canvas';
    canvas.setAttribute('aria-hidden', 'true');
    host.appendChild(canvas);

    /* --- small helpers ---------------------------------------------------- */

    var seed = 20261224;
    function rnd() { seed = (seed * 16807) % 2147483647; return (seed - 1) / 2147483646; }
    /* a colour written as on the page: the renderer works in linear light */
    function col(c) { return new THREE.Color(c).convertSRGBToLinear(); }
    function hex(n) { return '#' + ('000000' + n.toString(16)).slice(-6); }
    function clamp(x) { return x < 0 ? 0 : x > 1 ? 1 : x; }
    function smooth(x) { x = clamp(x); return x * x * (3 - 2 * x); }
    function backOut(x) { x = clamp(x) - 1; return 1 + x * x * (2.6 * x + 1.6); }

    var mats = {};
    function mat(color, o) {
      o = o || {};
      var key = color + '|' + (o.glow || 0);
      if (mats[key]) return mats[key];
      /* lit per corner, not per pixel: a full window of this scene must stay cheap on a phone */
      var m = new THREE.MeshLambertMaterial({color: col(color)});
      if (o.glow) { m.emissive = col(color); m.emissiveIntensity = o.glow; }
      mats[key] = m;
      return m;
    }
    var lits = {};
    function lit(color) { return lits[color] || (lits[color] = new THREE.MeshBasicMaterial({color: col(color)})); }

    /* every box, cylinder and ball is one shared shape, scaled */
    var UBOX = new THREE.BoxGeometry(1, 1, 1);
    var USPH = new THREE.SphereGeometry(1, 10, 8);
    var UPLANE = new THREE.PlaneGeometry(1, 1);
    var UCIRC = new THREE.CircleGeometry(1, 20);
    var UCYL = {};
    function shade(o, sh) { if (sh !== false) o.castShadow = true; return o; }
    function takes(o) { o.receiveShadow = true; return o; }
    function grp(g, x, y, z) { var o = new THREE.Group(); o.position.set(x || 0, y || 0, z || 0); g.add(o); return o; }
    function bx(g, w, h, d, m, x, y, z, sh) {
      var o = new THREE.Mesh(UBOX, m);
      o.scale.set(w, h, d); o.position.set(x || 0, y || 0, z || 0);
      g.add(shade(o, sh)); return o;
    }
    function cy(g, r, h, m, x, y, z, ax, seg, sh) {
      seg = seg || 10;
      var o = new THREE.Mesh(UCYL[seg] || (UCYL[seg] = new THREE.CylinderGeometry(1, 1, 1, seg)), m);
      o.scale.set(r, h, r); o.position.set(x || 0, y || 0, z || 0);
      if (ax === 'x') o.rotation.z = Math.PI / 2; else if (ax === 'z') o.rotation.x = Math.PI / 2;
      g.add(shade(o, sh)); return o;
    }
    function cone(g, rt, rb, h, m, x, y, z, seg, sh) {
      var o = new THREE.Mesh(new THREE.CylinderGeometry(rt, rb, h, seg || 10), m);
      o.position.set(x || 0, y || 0, z || 0);
      g.add(shade(o, sh)); return o;
    }
    function sp(g, r, m, x, y, z, sh) {
      var o = new THREE.Mesh(USPH, m);
      o.scale.setScalar(r); o.position.set(x || 0, y || 0, z || 0);
      g.add(shade(o, sh)); return o;
    }
    function plane(g, w, h, m, x, y, z) {
      var o = new THREE.Mesh(UPLANE, m);
      o.scale.set(w, h, 1); o.position.set(x || 0, y || 0, z || 0);
      g.add(o); return o;
    }
    function drawn(w, h, paintIt) {
      var c = document.createElement('canvas'); c.width = w; c.height = h;
      paintIt(c.getContext('2d'), w, h);
      var t = new THREE.CanvasTexture(c);
      t.encoding = THREE.sRGBEncoding;
      return t;
    }
    var glowTex = (function () {
      var c = document.createElement('canvas'); c.width = c.height = 128;
      var g = c.getContext('2d'), r = g.createRadialGradient(64, 64, 0, 64, 64, 64);
      r.addColorStop(0, 'rgba(255,255,255,1)'); r.addColorStop(0.25, 'rgba(255,255,255,.55)');
      r.addColorStop(1, 'rgba(255,255,255,0)');
      g.fillStyle = r; g.fillRect(0, 0, 128, 128);
      return new THREE.CanvasTexture(c);
    })();
    function glow(color, size, opacity) {
      var s = new THREE.Sprite(new THREE.SpriteMaterial({map: glowTex, color: col(color), transparent: true,
        opacity: opacity == null ? 0.8 : opacity, blending: THREE.AdditiveBlending, depthWrite: false}));
      s.scale.set(size, size, 1);
      return s;
    }
    /* a small lamp: a ball that can be bright or dark, and its halo */
    function lamp(g, color, r, x, y, z, size) {
      var base = col(color), m = new THREE.MeshBasicMaterial({color: base.clone()});
      var b = new THREE.Mesh(USPH, m); b.scale.setScalar(r); b.position.set(x, y, z); g.add(b);
      var s = glow(color, size || r * 9, 0.8); s.position.set(x, y, z); g.add(s);
      var l = {
        set: function (k) { m.color.copy(base).multiplyScalar(0.16 + 0.84 * k); s.material.opacity = 0.85 * k; },
        hue: function (c) { base = c; s.material.color.copy(c); }
      };
      return l;
    }
    /* --- fewer draw calls ----------------------------------------------------
       A part is thirty boxes, and a box alone costs a draw call: too many for a
       phone once the machine is whole. Whatever never moves inside a group is
       merged into one mesh per material. What moves is not declared by hand, it
       is found: `run` plays the group's own animation and `bake` watches. */
    function stateOf(root) {
      var a = [];
      root.traverse(function (o) {
        o.updateMatrix();
        var e = o.matrix.elements;
        for (var i = 0; i < 16; i++) a.push(e[i]);
        /* twenty values a node, so that a place in the list names its node */
        a.push(o.visible, o.isMesh ? o.material.uuid : '', o.isMesh ? o.geometry.uuid : '', o.isMesh ? o.geometry.attributes.position.version : 0);
      });
      return a;
    }
    function bake(root, run, loose) {
      var nodes = [], moved = [], sets = {}, keys = [], i, j;
      root.traverse(function (o) { nodes.push(o); });
      var first = stateOf(root);
      if (run) run(function () {
        var now = stateOf(root);
        for (var q = 0; q < now.length; q++) if (now[q] !== first[q]) moved[Math.floor(q / 20)] = true;
      });
      for (i = 1; i < nodes.length; i++) {
        var o = nodes[i];
        if (!o.visible || (loose && loose.indexOf(o) > -1)) moved[i] = true;
        if (moved[nodes.indexOf(o.parent)]) moved[i] = true;      /* a parent comes before its children */
        if (moved[i] || !o.isMesh || o.material.length || o.material.visible === false || o.material.transparent) continue;
        if (o.userData.day != null || !o.geometry.attributes.normal) continue;
        var key = o.material.uuid + (o.castShadow ? 'c' : '') + (o.receiveShadow ? 'r' : '') + (o.geometry.attributes.color ? 'k' : '');
        if (!sets[key]) { sets[key] = []; keys.push(key); }
        sets[key].push(o);
      }
      keys.forEach(function (key) {
        var list = sets[key];
        if (list.length < 2) return;
        var parts = [], count = 0;
        list.forEach(function (o) {
          var m = o.matrix.clone(), up = o.parent;
          while (up !== root) { m.premultiply(up.matrix); up = up.parent; }
          var g = o.geometry.index ? o.geometry.toNonIndexed() : o.geometry.clone();
          g.applyMatrix4(m);
          parts.push({g: g, mirror: m.determinant() < 0});
          count += g.attributes.position.count;
          o.parent.remove(o);
        });
        var pos = new Float32Array(count * 3), nor = new Float32Array(count * 3), uv = new Float32Array(count * 2), at = 0;
        var tint = list[0].geometry.attributes.color ? new Float32Array(count * 3) : null;
        parts.forEach(function (q) {
          var p = q.g.attributes.position, n = q.g.attributes.normal, u = q.g.attributes.uv, c = q.g.attributes.color;
          for (var v = 0; v < p.count; v++) {
            /* a mirrored piece has its triangles turned inside out: take them in the other order */
            var from = q.mirror && v % 3 ? v + (v % 3 === 1 ? 1 : -1) : v;
            pos[at * 3] = p.getX(from); pos[at * 3 + 1] = p.getY(from); pos[at * 3 + 2] = p.getZ(from);
            nor[at * 3] = n.getX(from); nor[at * 3 + 1] = n.getY(from); nor[at * 3 + 2] = n.getZ(from);
            if (u) { uv[at * 2] = u.getX(from); uv[at * 2 + 1] = u.getY(from); }
            if (tint) { tint[at * 3] = c.getX(from); tint[at * 3 + 1] = c.getY(from); tint[at * 3 + 2] = c.getZ(from); }
            at++;
          }
          q.g.dispose();
        });
        var geo = new THREE.BufferGeometry();
        geo.setAttribute('position', new THREE.BufferAttribute(pos, 3));
        geo.setAttribute('normal', new THREE.BufferAttribute(nor, 3));
        geo.setAttribute('uv', new THREE.BufferAttribute(uv, 2));
        if (tint) geo.setAttribute('color', new THREE.BufferAttribute(tint, 3));
        var one = new THREE.Mesh(geo, list[0].material);
        one.castShadow = list[0].castShadow; one.receiveShadow = list[0].receiveShadow;
        root.add(one);
      });
    }

    /* a hand wheel: a rim, three spokes and a hub, lying flat, to turn about y */
    function handwheel(g, r, m) {
      var rim = new THREE.Mesh(new THREE.TorusGeometry(r, r * 0.13, 5, 14), m);
      rim.rotation.x = Math.PI / 2; rim.castShadow = true; g.add(rim);
      for (var k = 0; k < 3; k++) { var s = bx(g, r * 2, r * 0.1, r * 0.12, M.steel, 0, 0, 0, false); s.rotation.y = k * Math.PI / 3; }
      cy(g, r * 0.24, r * 0.34, M.steel, 0, 0, 0, 'y', 8, false);
    }
    /* a toothed wheel, flat in the xy plane */
    function gearGeo(r, teeth, depth) {
      var sh = new THREE.Shape(), n = teeth * 4;
      for (var i = 0; i < n; i++) {
        var a = i / n * TAU, rr = i % 4 < 2 ? r : r * 0.8;
        if (i) sh.lineTo(Math.cos(a) * rr, Math.sin(a) * rr); else sh.moveTo(rr, 0);
      }
      var g = new THREE.ExtrudeGeometry(sh, {depth: depth, bevelEnabled: false, curveSegments: 1});
      g.translate(0, 0, -depth / 2);
      return g;
    }

    /* --- the scene, its light ------------------------------------------------ */

    var scene = new THREE.Scene();
    var NIGHT = col(0x060a17);
    scene.background = NIGHT;
    scene.fog = new THREE.FogExp2(0x000000, 0.0085);
    scene.fog.color.copy(NIGHT);
    var camera = new THREE.PerspectiveCamera(36, 2, 0.5, 300);

    scene.add(new THREE.HemisphereLight(col(0x8fa6ee), col(0x3b2a20), 0.46));
    /* the pendant lamps, as one warm light from above the reader's left shoulder */
    var key = new THREE.DirectionalLight(col(0xffdcae), 1.2);
    key.position.set(-7, 22, 13);
    key.target.position.set(1, 0, 0);
    key.castShadow = true;
    key.shadow.mapSize.set(2048, 2048);
    key.shadow.camera.left = -15; key.shadow.camera.right = 15;
    key.shadow.camera.top = 12; key.shadow.camera.bottom = -12;
    key.shadow.camera.near = 2; key.shadow.camera.far = 60;
    key.shadow.bias = -0.0007; key.shadow.normalBias = 0.02;
    scene.add(key); scene.add(key.target);
    /* the night through the window: a cold edge on whatever stands on the bench */
    var moon = new THREE.DirectionalLight(col(0x7f9dff), 0.6);
    moon.position.set(8, 9, -14); moon.target.position.set(0, 0, 3);
    scene.add(moon); scene.add(moon.target);

    /* --- materials and drawn surfaces ---------------------------------------- */

    var M = {
      iron: mat(0x2b2f3c, {glow: 0.1}),
      steel: mat(0xb4bccb, {glow: 0.12}),
      brass: mat(0xdcae4e, {glow: 0.2}),
      rubber: mat(0x18181d),
      bake: mat(0x1f1a20, {glow: 0.05}),
      cream: mat(0xeadfc4, {glow: 0.2}),
      ivory: mat(0xf7efde, {glow: 0.26}),
      paper: mat(0xf6f2e8, {glow: 0.34}),
      glass: mat(0x15264a, {glow: 0.2}),
      walnut: mat(0x90583a, {glow: 0.17}),
      walnut2: mat(0x603823, {glow: 0.14}),
      tape: mat(0x4a2b22),
      hammer: mat(0x716f9a, {glow: 0.17}),
      hammer2: mat(0x3f3d5c, {glow: 0.14}),
      panel: mat(0xdedaf0, {glow: 0.22}),
      enamel: mat(0xaec6aa, {glow: 0.18}),
      enamel2: mat(0x4f7059, {glow: 0.16}),
      red: mat(0xd8463a, {glow: 0.3}),
      linen: mat(0xc2beb6),
      needle: lit(0x2a1c1c),
      plinth: mat(0x6f4930, {glow: 0.08})
    };

    var grain = drawn(256, 256, function (g, w) {
      g.fillStyle = '#c4c4c4'; g.fillRect(0, 0, w, w);
      for (var r = 0; r < 4; r++) {
        var v = 196 + Math.round(rnd() * 44);
        g.fillStyle = 'rgb(' + v + ',' + v + ',' + v + ')'; g.fillRect(0, r * 64, w, 62);
        g.fillStyle = 'rgba(0,0,0,.4)'; g.fillRect(0, r * 64 + 62, w, 2);
        for (var k = 0; k < 16; k++) {
          g.fillStyle = 'rgba(0,0,0,' + (0.03 + rnd() * 0.07) + ')';
          g.fillRect(rnd() * w, r * 64 + rnd() * 60, 40 + rnd() * 120, 1.5);
        }
        g.fillStyle = 'rgba(0,0,0,.3)'; g.fillRect(Math.floor(rnd() * w), r * 64, 2, 62);
      }
    });
    grain.encoding = THREE.LinearEncoding;
    function wood(color, rx, ry, turn, glowK) {
      var t = grain.clone(); t.needsUpdate = true;
      t.wrapS = t.wrapT = THREE.RepeatWrapping; t.repeat.set(rx, ry);
      if (turn) t.rotation = Math.PI / 2;
      var m = new THREE.MeshLambertMaterial({color: col(color), map: t});
      if (glowK) { m.emissive = col(color); m.emissiveIntensity = glowK; m.emissiveMap = t; }
      return m;
    }

    /* a sheet, as it looks after 0 to 4 weeks of the machine: each week leaves its mark */
    function pageTex(stage, tones) {
      return drawn(176, 128, function (g, w, h) {
        g.fillStyle = stage ? '#fbf8ef' : '#ece8dc'; g.fillRect(0, 0, w, h);
        g.save();
        g.translate(w / 2, h / 2); g.rotate(-Math.PI / 2 + (stage ? 0 : 0.05)); g.translate(-h / 2, -w / 2);
        /* now drawing a portrait page, h wide and w tall */
        g.fillStyle = '#3a3550'; g.fillRect(14, 16, 54, 8);
        g.fillStyle = stage ? '#a7a2b6' : '#b9b5bf';
        for (var y = 36; y < 96; y += 9) g.fillRect(14, y, y % 27 === 0 ? 60 : 98, 3.5);
        g.strokeStyle = stage ? '#8d88a0' : '#b9b5bf'; g.lineWidth = 1.5;
        g.strokeRect(14, 104, 98, 40);
        g.beginPath(); g.moveTo(14, 117); g.lineTo(112, 117); g.moveTo(14, 130); g.lineTo(112, 130);
        g.moveTo(47, 104); g.lineTo(47, 144); g.moveTo(80, 104); g.lineTo(80, 144); g.stroke();
        if (!stage) { g.fillStyle = 'rgba(40,40,60,.16)'; g.fillRect(0, 0, 9, w); g.fillRect(0, w - 12, h, 12); }
        if (stage >= 1) {
          g.strokeStyle = tones[0]; g.lineWidth = 3;
          g.strokeRect(10, 31, 106, 66); g.strokeRect(10, 100, 106, 48);
          g.fillStyle = tones[0]; g.fillRect(10, 12, 62, 3);
        }
        if (stage >= 2) { g.fillStyle = tones[1]; g.fillRect(h - 12, 20, 12, 30); g.fillRect(86, 15, 22, 9); }
        if (stage >= 3) {
          g.strokeStyle = tones[2]; g.lineWidth = 3; g.lineCap = 'round';
          [46, 64, 82].forEach(function (y2) { g.beginPath(); g.moveTo(2, y2); g.lineTo(5, y2 + 4); g.lineTo(9, y2 - 4); g.stroke(); });
        }
        if (stage >= 4) {
          g.strokeStyle = '#2f9a4e'; g.lineWidth = 4;
          g.beginPath(); g.arc(88, 156, 13, 0, TAU); g.stroke();
          g.beginPath(); g.moveTo(81, 156); g.lineTo(86, 162); g.lineTo(96, 150); g.stroke();
        }
        g.restore();
      });
    }
    var slipTex = drawn(128, 168, function (g, w, h) {
      g.fillStyle = '#fbf8ef'; g.fillRect(0, 0, w, h);
      g.fillStyle = '#a7a2b6'; [18, 32, 46].forEach(function (y, i) { g.fillRect(16, y, i === 2 ? 56 : 96, 6); });
      g.fillStyle = '#2fae58'; g.beginPath(); g.arc(64, 110, 40, 0, TAU); g.fill();
      g.strokeStyle = '#ffffff'; g.lineWidth = 12; g.lineCap = 'round'; g.lineJoin = 'round';
      g.beginPath(); g.moveTo(44, 110); g.lineTo(58, 126); g.lineTo(86, 94); g.stroke();
    });
    var dialM = new THREE.MeshBasicMaterial({map: drawn(128, 128, function (g) {
      g.fillStyle = '#f4ecd6'; g.beginPath(); g.arc(64, 64, 64, 0, TAU); g.fill();
      g.strokeStyle = '#3a3040'; g.lineWidth = 3;
      for (var i = 0; i <= 8; i++) {
        var a = Math.PI * (1.18 + 0.64 * i / 8);
        g.beginPath(); g.moveTo(64 + Math.cos(a) * 44, 86 + Math.sin(a) * 44);
        g.lineTo(64 + Math.cos(a) * (i % 2 ? 51 : 56), 86 + Math.sin(a) * (i % 2 ? 51 : 56)); g.stroke();
      }
      g.strokeStyle = '#d0453a'; g.lineWidth = 6;
      g.beginPath(); g.arc(64, 86, 60, Math.PI * 1.66, Math.PI * 1.82); g.stroke();
    })});
    function dial(g, r, x, y, z) {
      var d = grp(g, x, y, z);
      cy(d, r * 1.14, 0.04, M.bake, 0, 0, -0.025, 'z', 16, false);
      var f = new THREE.Mesh(UCIRC, dialM); f.scale.set(r, r, 1); d.add(f);
      var n = grp(d, 0, -r * 0.34, 0.012);
      bx(n, r * 0.08, r * 1.05, 0.006, M.needle, 0, r * 0.5, 0, false);
      return n;
    }

    /* --- where everything stands ---------------------------------------------
     * A row per week, the first week at the back and highest. The paper runs
     * left to right on one row and right to left on the next, down a curved
     * slide at each end. `order` is the 24 places in the order of the paper. */

    var weeks = data.world.villages.map(function (v, r, all) {
      var days = (data.days || []).filter(function (d) { return d.band === v.id; }).map(function (d) { return d.day; });
      if (!days.length) for (var d = v.days[0]; d <= v.days[1]; d++) days.push(d);
      return {def: v, r: r, days: days, tone: new THREE.Color(v.tone).getHex(), y: (all.length - 1 - r) * STEP,
              z: (r - (all.length - 1) / 2) * ROW, dir: r % 2 ? -1 : 1, recs: []};
    });
    var NR = weeks.length;
    var order = [], byDay = {}, segs = [], total = 0;
    weeks.forEach(function (w, r) {
      var n = w.days.length;
      w.days.forEach(function (day, i) {
        var rec = {day: day, seq: order.length, week: w, i: i, x: w.dir * (i - (n - 1) / 2) * PITCH, y: w.y, z: w.z,
                   dir: w.dir, state: '', inst: 0, instGoal: 0, shake: 0, busy: 0, anim: null, fire: null};
        if (i === 0 && r > 0) {
          var pw = weeks[r - 1];
          rec.arc = {arc: true, s0: total, len: Math.PI * TURN, xe: pw.dir * pw.days.length / 2 * PITCH, side: pw.dir,
                     z0: pw.z, y0: pw.y + PY, y1: w.y + PY, rec: rec};
          segs.push(rec.arc); total += rec.arc.len;
        }
        segs.push({s0: total, len: PITCH, x0: rec.x - w.dir * PITCH / 2, dir: w.dir, y: w.y + PY, z: w.z, rec: rec});
        rec.sIn = total; total += PITCH; rec.sOut = total;
        order.push(rec); byDay[day] = rec; w.recs.push(rec);
      });
    });
    var N = order.length;
    var HALF = Math.max.apply(null, weeks.map(function (w) { return w.days.length; })) / 2 * PITCH;   /* where a row ends */
    var BX0 = -(HALF + TURN + 1.1), BX1 = HALF + TURN + 1.1;       /* the stepped part of the bench */
    var TX1 = BX1 + 3.7;                                           /* the bench goes on: a place to work */
    var BZ0 = weeks[0].z - 1.5, BZ1 = weeks[NR - 1].z + 1.5;
    var WALLZ = BZ0 - 0.3;

    /* a point of the paper's way: where, which way it heads, how steeply it goes down */
    function pathAt(s, o) {
      var sg = segs[0];
      for (var i = 0; i < segs.length; i++) { sg = segs[i]; if (s < sg.s0 + sg.len) break; }
      if (sg.arc) {
        var u = clamp((s - sg.s0) / sg.len), a = -Math.PI / 2 + Math.PI * u;
        o.x = sg.xe + sg.side * TURN * Math.cos(a); o.z = sg.z0 + TURN + TURN * Math.sin(a);
        o.y = sg.y0 + (sg.y1 - sg.y0) * u;
        o.yaw = Math.atan2(-Math.cos(a), -sg.side * Math.sin(a));
        o.pitch = Math.atan2(sg.y1 - sg.y0, sg.len);
      } else {
        o.x = sg.x0 + sg.dir * (s - sg.s0); o.y = sg.y; o.z = sg.z;
        o.yaw = sg.dir > 0 ? 0 : Math.PI; o.pitch = 0;
      }
      o.rec = sg.rec;
      return o;
    }

    /* --- the room ------------------------------------------------------------- */

    var room = new THREE.Group();
    scene.add(room);
    var twinkles = [], flakes = null;
    var WX0 = 4.6, WX1 = 12.2, WY0 = 5.0, WY1 = 10.2;     /* the window */

    (function walls() {
      /* The wall and the floor fill the window of the page: they are painted,
       * light included, so that they cost one texture read per pixel. */
      var WX = 46, WTOP = 31.5, FZ0 = -24, FZ1 = 46;
      function pool(g, x, y, r, rgb, a) {
        var rg = g.createRadialGradient(x, y, 0, x, y, r);
        rg.addColorStop(0, 'rgba(' + rgb + ',' + a + ')'); rg.addColorStop(1, 'rgba(' + rgb + ',0)');
        g.fillStyle = rg; g.fillRect(x - r, y - r, 2 * r, 2 * r);
      }
      var wallTex = drawn(1280, 512, function (g, w, h) {
        function X(x) { return (x + WX) / (2 * WX) * w; }
        function Y(y) { return h - (y - FLOOR) / (WTOP - FLOOR) * h; }
        var k = w / (2 * WX);
        g.fillStyle = '#1c2d50'; g.fillRect(0, 0, w, h);
        for (var x = -WX; x < WX; x += 0.95) {
          g.fillStyle = 'rgba(' + (rnd() > 0.5 ? '255,255,255' : '0,0,0') + ',' + (0.02 + rnd() * 0.06) + ')';
          g.fillRect(X(x), 0, 0.95 * k, h);
          g.fillStyle = 'rgba(0,0,0,.4)'; g.fillRect(X(x + 0.95) - 1.5, 0, 1.5, h);
        }
        g.fillStyle = '#22355a'; g.fillRect(0, 0, w, Y(12.5));
        g.fillStyle = '#121d38'; g.fillRect(0, Y(12.5), w, 3.5);
        g.globalCompositeOperation = 'lighter';
        pool(g, X(-2.5), Y(6.5), 15 * k, '255,186,112', 0.3);
        pool(g, X((WX0 + WX1) / 2), Y((WY0 + WY1) / 2), 10 * k, '96,138,255', 0.26);
        pool(g, X(TX1 - 1), Y(2.5), 7 * k, '255,186,112', 0.2);
        g.globalCompositeOperation = 'source-over';
        var side = g.createLinearGradient(0, 0, w, 0);
        side.addColorStop(0, 'rgba(4,7,18,.9)'); side.addColorStop(0.3, 'rgba(4,7,18,0)');
        side.addColorStop(0.72, 'rgba(4,7,18,0)'); side.addColorStop(1, 'rgba(4,7,18,.9)');
        g.fillStyle = side; g.fillRect(0, 0, w, h);
        var up = g.createLinearGradient(0, 0, 0, h);
        up.addColorStop(0, 'rgba(4,7,18,.92)'); up.addColorStop(0.5, 'rgba(4,7,18,.25)'); up.addColorStop(0.72, 'rgba(4,7,18,0)'); up.addColorStop(1, 'rgba(4,7,18,.5)');
        g.fillStyle = up; g.fillRect(0, 0, w, h);
      });
      var wallM = new THREE.MeshBasicMaterial({map: wallTex});
      function wall(x0, x1, y0, y1) {
        var geo = new THREE.PlaneGeometry(x1 - x0, y1 - y0), uv = geo.attributes.uv;
        for (var i = 0; i < uv.count; i++) {
          uv.setXY(i, (x0 + uv.getX(i) * (x1 - x0) + WX) / (2 * WX), (y0 + uv.getY(i) * (y1 - y0) - FLOOR) / (WTOP - FLOOR));
        }
        var m = new THREE.Mesh(geo, wallM);
        m.position.set((x0 + x1) / 2, (y0 + y1) / 2, WALLZ); room.add(m);
      }
      wall(-WX, WX0, FLOOR, WTOP); wall(WX1, WX, FLOOR, WTOP); wall(WX0, WX1, FLOOR, WY0); wall(WX0, WX1, WY1, WTOP);

      var floorTex = drawn(1024, 1024, function (g, w, h) {
        function X(x) { return (x + WX) / (2 * WX) * w; }
        function Z(z) { return (z - FZ0) / (FZ1 - FZ0) * h; }
        var kx = w / (2 * WX), kz = h / (FZ1 - FZ0);
        g.fillStyle = '#3b2a20'; g.fillRect(0, 0, w, h);
        for (var z = FZ0; z < FZ1; z += 1.15) {
          for (var x = -WX - rnd() * 9; x < WX; ) {
            var len = 7 + rnd() * 9;
            g.fillStyle = 'rgba(' + (rnd() > 0.5 ? '255,220,180' : '0,0,0') + ',' + (0.03 + rnd() * 0.09) + ')';
            g.fillRect(X(x), Z(z), len * kx - 1, 1.15 * kz - 1.5);
            x += len;
          }
          g.fillStyle = 'rgba(0,0,0,.4)'; g.fillRect(0, Z(z + 1.15) - 1.5, w, 1.5);
        }
        g.globalCompositeOperation = 'lighter';
        pool(g, X(-2.5), Z(BZ1 + 5), 15 * kx, '255,176,104', 0.3);
        pool(g, X(TX1 - 1), Z(BZ1 + 3), 9 * kx, '255,176,104', 0.22);
        g.globalCompositeOperation = 'source-over';
        /* the bench keeps the lamps off the floor under it */
        for (var q = 0; q < 6; q++) {
          g.fillStyle = 'rgba(3,5,12,.2)';
          g.fillRect(X(BX0 - 1.6 + q * 0.4), Z(BZ0 - 1), (TX1 - BX0 + 3.2 - q * 0.8) * kx, (BZ1 - BZ0 + 3.4 - q * 0.45) * kz);
        }
        var rim = g.createRadialGradient(X(0), Z(BZ1 + 4), 16 * kx, X(0), Z(BZ1 + 4), 44 * kx);
        rim.addColorStop(0, 'rgba(4,7,18,0)'); rim.addColorStop(1, 'rgba(4,7,18,.94)');
        g.fillStyle = rim; g.fillRect(0, 0, w, h);
      });
      var floor = new THREE.Mesh(new THREE.PlaneGeometry(2 * WX, FZ1 - FZ0), new THREE.MeshBasicMaterial({map: floorTex}));
      floor.rotation.x = -Math.PI / 2; floor.position.set(0, FLOOR, (FZ0 + FZ1) / 2);
      room.add(floor);

      /* the window: a deep frame, six panes, the night behind */
      var frameM = mat(0x5a3a28, {glow: 0.12}), ww = WX1 - WX0, wh = WY1 - WY0, cx = (WX0 + WX1) / 2;
      bx(room, ww + 0.7, 0.34, 0.5, frameM, cx, WY1 + 0.17, WALLZ);
      bx(room, 0.34, wh, 0.5, frameM, WX0 - 0.17, WY0 + wh / 2, WALLZ);
      bx(room, 0.34, wh, 0.5, frameM, WX1 + 0.17, WY0 + wh / 2, WALLZ);
      bx(room, ww + 1.2, 0.2, 1.0, frameM, cx, WY0 - 0.1, WALLZ + 0.25);
      bx(room, 0.12, wh, 0.14, frameM, WX0 + ww / 3, WY0 + wh / 2, WALLZ - 0.1, false);
      bx(room, 0.12, wh, 0.14, frameM, WX0 + 2 * ww / 3, WY0 + wh / 2, WALLZ - 0.1, false);
      bx(room, ww, 0.12, 0.14, frameM, cx, WY0 + wh * 0.52, WALLZ - 0.1, false);
      /* snow on the sill outside, and in the corners of the panes */
      var snowM = lit(0xcfdcf8);
      bx(room, ww, 0.5, 0.5, snowM, cx, WY0 + 0.14, WALLZ - 0.5, false);
      [0, 1, 2].forEach(function (k) {
        var s = sp(room, 0.5, snowM, WX0 + ww * (k + 0.5) / 3 + (k - 1) * 0.5, WY0 + 0.36, WALLZ - 0.45, false);
        s.scale.set(1.5 - k * 0.2, 0.36, 0.4);
      });

      var outside = drawn(512, 384, function (g, w, h) {
        var sky = g.createLinearGradient(0, 0, 0, h);
        sky.addColorStop(0, '#0a1436'); sky.addColorStop(0.55, '#1c3273'); sky.addColorStop(1, '#4567b8');
        g.fillStyle = sky; g.fillRect(0, 0, w, h);
        for (var i = 0; i < 90; i++) {
          g.fillStyle = 'rgba(255,255,255,' + (0.3 + rnd() * 0.7) + ')';
          g.fillRect(rnd() * w, rnd() * h * 0.6, 1.6, 1.6);
        }
        var halo = g.createRadialGradient(392, 86, 4, 392, 86, 96);
        halo.addColorStop(0, 'rgba(255,246,214,.75)'); halo.addColorStop(1, 'rgba(255,246,214,0)');
        g.fillStyle = halo; g.fillRect(280, 0, 232, 200);
        g.fillStyle = '#fff6d8'; g.beginPath(); g.arc(392, 86, 30, 0, TAU); g.fill();
        g.fillStyle = '#93a8de'; g.beginPath(); g.moveTo(0, 262);
        for (var x = 0; x <= w; x += 16) g.lineTo(x, 262 - Math.sin(x * 0.012 + 1) * 24 - Math.sin(x * 0.031) * 8);
        g.lineTo(w, h); g.lineTo(0, h); g.fill();
        g.fillStyle = '#d3defa'; g.beginPath(); g.moveTo(0, 318);
        for (var x2 = 0; x2 <= w; x2 += 16) g.lineTo(x2, 312 - Math.sin(x2 * 0.016 + 3) * 16);
        g.lineTo(w, h); g.lineTo(0, h); g.fill();
        [[40, 300, 1.2], [96, 286, 0.9], [150, 306, 1.4], [232, 290, 1], [300, 304, 1.3], [456, 296, 1.5], [486, 310, 1]].forEach(function (t) {
          for (var k = 0; k < 3; k++) {
            var ww2 = (30 - k * 7) * t[2], yy = t[1] - k * 22 * t[2];
            g.fillStyle = '#10214a'; g.beginPath(); g.moveTo(t[0] - ww2, yy); g.lineTo(t[0] + ww2, yy); g.lineTo(t[0], yy - 38 * t[2]); g.fill();
            g.fillStyle = '#e4ecff'; g.beginPath(); g.moveTo(t[0] - ww2 * 0.5, yy - 19 * t[2]); g.lineTo(t[0] + ww2 * 0.5, yy - 19 * t[2]); g.lineTo(t[0], yy - 39 * t[2]); g.fill();
          }
        });
        /* a house far away, one window lit */
        g.fillStyle = '#172552'; g.fillRect(352, 268, 44, 28);
        g.fillStyle = '#e4ecff'; g.beginPath(); g.moveTo(344, 270); g.lineTo(374, 248); g.lineTo(404, 270); g.fill();
        g.fillStyle = '#ffc45e'; g.fillRect(366, 276, 10, 12);
      });
      var back = new THREE.Mesh(UPLANE, new THREE.MeshBasicMaterial({map: outside, fog: false}));
      back.scale.set(20, 13.5, 1); back.position.set(cx, WY0 + wh / 2 + 0.4, WALLZ - 5);
      room.add(back);

      if (!reduced) {
        var n = 260, fp = new Float32Array(n * 3), fv = new Float32Array(n);
        for (var i = 0; i < n; i++) {
          fp[i * 3] = WX0 - 3 + rnd() * (ww + 6); fp[i * 3 + 1] = WY0 - 1 + rnd() * (wh + 3); fp[i * 3 + 2] = WALLZ - 0.7 - rnd() * 3.6;
          fv[i] = 0.5 + rnd() * 0.9;
        }
        var fg = new THREE.BufferGeometry();
        fg.setAttribute('position', new THREE.BufferAttribute(fp, 3));
        flakes = new THREE.Points(fg, new THREE.PointsMaterial({color: 0xffffff, size: 0.2, map: glowTex, transparent: true,
          opacity: 0.9, depthWrite: false, sizeAttenuation: true, fog: false}));
        flakes.userData = {v: fv, y0: WY0 - 1, y1: WY1 + 2};
        room.add(flakes);
      }

      /* a string of small lights over the window, one colour per week */
      var bulbs = 15;
      for (var b = 0; b < bulbs; b++) {
        var u = b / (bulbs - 1), bxp = WX0 - 0.5 + u * (ww + 1), byp = WY1 + 0.55 - Math.sin(u * Math.PI * 3) * 0.16 - Math.sin(u * Math.PI) * 0.3;
        var tone = weeks[b % NR].tone;
        var l = lamp(room, tone, 0.09, bxp, byp, WALLZ + 0.32, 1.25);
        if (b < bulbs - 1) {
          var u2 = (b + 1) / (bulbs - 1), nx = WX0 - 0.5 + u2 * (ww + 1), ny = WY1 + 0.55 - Math.sin(u2 * Math.PI * 3) * 0.16 - Math.sin(u2 * Math.PI) * 0.3;
          var wire = bx(room, Math.sqrt((nx - bxp) * (nx - bxp) + (ny - byp) * (ny - byp)), 0.025, 0.025, M.rubber, (bxp + nx) / 2, (byp + ny) / 2 + 0.07, WALLZ + 0.3, false);
          wire.rotation.z = Math.atan2(ny - byp, nx - bxp);
        }
        twinkles.push({l: l, k: b});
      }
    })();

    (function shelves() {
      var wallG = grp(room, 4.3, 0, 0);     /* the wall of tools and boxes, set clear of the title */
      var hookG = grp(room, 1.9, 0, 0);
      /* a board of tools, a shelf of archive boxes and binders */
      var boardM = mat(0x263a63, {glow: 0.1});
      bx(wallG, 7.0, 2.5, 0.1, boardM, -5.1, 4.7, WALLZ + 0.06, false);
      var z = WALLZ + 0.2, handle = mat(0xb5683a, {glow: 0.2});
      /* a hammer, three spanners, a saw, four screwdrivers */
      bx(wallG, 0.12, 1.3, 0.1, handle, -8.0, 4.5, z, false); bx(wallG, 0.6, 0.24, 0.16, M.steel, -8.0, 5.25, z, false);
      [0, 1, 2].forEach(function (k) {
        bx(wallG, 0.1, 1.1 + k * 0.2, 0.06, M.steel, -7.1 + k * 0.45, 4.65 + k * 0.1, z, false);
        cy(wallG, 0.13, 0.07, M.steel, -7.1 + k * 0.45, 5.25 + k * 0.2, z, 'z', 6, false);
      });
      var saw = bx(wallG, 2.0, 0.42, 0.04, M.steel, -4.6, 5.2, z, false); saw.rotation.z = -0.06;
      bx(wallG, 0.5, 0.56, 0.1, handle, -5.75, 5.26, z, false);
      [0, 1, 2, 3].forEach(function (k) {
        bx(wallG, 0.07, 0.6, 0.07, M.steel, -4.9 + k * 0.42, 4.2, z, false);
        bx(wallG, 0.15, 0.46, 0.12, k % 2 ? M.red : handle, -4.9 + k * 0.42, 4.72, z, false);
      });
      var pl = bx(wallG, 0.1, 1.2, 0.06, M.steel, -2.4, 4.75, z, false); pl.rotation.z = 0.1;
      var pl2 = bx(wallG, 0.1, 1.2, 0.06, M.steel, -2.25, 4.75, z, false); pl2.rotation.z = -0.1;
      bx(wallG, 0.12, 0.6, 0.08, M.red, -2.5, 4.3, z, false); bx(wallG, 0.12, 0.6, 0.08, M.red, -2.15, 4.3, z, false);

      var shelfM = mat(0x6a452f, {glow: 0.12});
      bx(wallG, 7.6, 0.16, 1.1, shelfM, -4.3, 6.75, WALLZ + 0.55);
      [-7.7, -4.4, -1.0].forEach(function (x) { bx(wallG, 0.14, 0.7, 0.9, shelfM, x, 6.35, WALLZ + 0.45, false); });
      var kraft = mat(0xb48d5c, {glow: 0.16}), sy = 6.83;
      [[-7.2, 1.3, 1.0], [-5.8, 1.3, 1.25]].forEach(function (b, i) {
        bx(wallG, b[1], b[2], 0.9, kraft, b[0], sy + b[2] / 2, WALLZ + 0.55);
        bx(wallG, b[1] + 0.04, 0.2, 0.94, mat(0x8d6a40, {glow: 0.14}), b[0], sy + b[2] - 0.1, WALLZ + 0.55, false);
        bx(wallG, 0.6, 0.3, 0.02, M.ivory, b[0], sy + b[2] * 0.45, WALLZ + 1.01, false);
        bx(wallG, 0.3, 0.06, 0.025, lit(weeks[i % NR].tone), b[0] - 0.1, sy + b[2] * 0.45 + 0.05, WALLZ + 1.015, false);
      });
      var spines = [0x35507a, 0x7a3b3b, 0x3f6b55, 0x8a7a3a, 0x54437a, 0x35507a, 0x7a5a3b, 0x3f6b55];
      spines.forEach(function (c, i) {
        var lean = i === spines.length - 1 ? 0.22 : 0;
        var b = bx(wallG, 0.26, 1.15 + (i % 3) * 0.08, 0.8, mat(c, {glow: 0.2}), -4.4 + i * 0.3 + lean * 0.5, sy + 0.6 + (i % 3) * 0.04, WALLZ + 0.55);
        b.rotation.z = -lean;
        bx(wallG, 0.16, 0.26, 0.02, M.ivory, -4.4 + i * 0.3 + lean * 0.5, sy + 0.78, WALLZ + 0.96, false);
      });
      /* a wireless set, to keep the evening company */
      bx(wallG, 1.4, 0.95, 0.7, M.walnut, -1.1, sy + 0.475, WALLZ + 0.55);
      bx(wallG, 0.62, 0.62, 0.02, mat(0xc9b48a, {glow: 0.2}), -1.42, sy + 0.5, WALLZ + 0.91, false);
      var rd = new THREE.Mesh(UCIRC, dialM); rd.scale.set(0.22, 0.22, 1); rd.position.set(-0.72, sy + 0.56, WALLZ + 0.91); wallG.add(rd);
      cy(wallG, 0.06, 0.06, M.brass, -0.72, sy + 0.2, WALLZ + 0.92, 'z', 8, false);
      var rl = glow(0xffc45e, 0.9, 0.5); rl.position.set(-0.72, sy + 0.56, WALLZ + 0.95); wallG.add(rl);

      /* right of the window: coat hooks and an apron */
      bx(hookG, 2.4, 0.2, 0.1, shelfM, 13.4, 8.3, WALLZ + 0.06, false);
      [12.6, 13.4, 14.2].forEach(function (x) { cy(hookG, 0.05, 0.3, M.brass, x, 8.25, WALLZ + 0.2, 'z', 6, false); });
      bx(hookG, 1.1, 3.4, 0.14, mat(0x4d5d7a, {glow: 0.14}), 12.6, 6.5, WALLZ + 0.22, false);
      bx(hookG, 0.7, 0.6, 0.16, mat(0x3b4a66, {glow: 0.14}), 12.6, 5.9, WALLZ + 0.24, false);
    })();

    /* --- the bench ---------------------------------------------------------- */

    var bench = new THREE.Group();
    scene.add(bench);
    (function benchWork() {
      var topM = wood(0xb27a4a, 5, 3, false), stepA = wood(0xa87245, 5, 1, false), stepB = wood(0x9c683f, 5, 1, false);
      var dark = mat(0x5e3d28, {glow: 0.08});
      var w = TX1 - BX0, cx = (TX1 + BX0) / 2, d = BZ1 - BZ0 + 0.2, cz = (BZ0 + BZ1) / 2 + 0.1;
      [topM, stepA, stepB].forEach(function (m) { m.vertexColors = true; });
      /* A top is cut in small squares, each corner with its share of light: the
       * pool of the lamp over the machine, the desk lamp, the night at the ends. */
      function top(x0, x1, z0, z1, y, m) {
        var geo = new THREE.PlaneGeometry(x1 - x0, z1 - z0, Math.max(2, Math.round((x1 - x0) / 0.8)), Math.max(2, Math.round((z1 - z0) / 0.8)));
        geo.rotateX(-Math.PI / 2);
        var p = geo.attributes.position, c = new Float32Array(p.count * 3), mx = (x0 + x1) / 2, mz = (z0 + z1) / 2;
        for (var i = 0; i < p.count; i++) {
          var X = p.getX(i) + mx, Z = p.getZ(i) + mz;
          var k = clamp(0.12 + 0.88 * Math.exp(-Math.pow((X + 0.5) / 8.2, 2)) + 0.95 * Math.exp(-(Math.pow(X - (TX1 - 2), 2) + Math.pow(Z - 1.8, 2)) / 7));
          c[i * 3] = 0.34 + 0.86 * k; c[i * 3 + 1] = 0.4 + 0.66 * k; c[i * 3 + 2] = 0.62 + 0.26 * k;
        }
        geo.setAttribute('color', new THREE.BufferAttribute(c, 3));
        var o = new THREE.Mesh(geo, m); o.position.set(mx, y, mz); o.receiveShadow = true; bench.add(o);
      }
      var white = new Float32Array(UBOX.attributes.position.count * 3); white.fill(0.62);
      UBOX.setAttribute('color', new THREE.BufferAttribute(white, 3));
      bx(bench, w + 0.5, 0.36, d + 0.3, topM, cx, -0.19, cz);
      top(BX0 - 0.25, TX1 + 0.25, weeks[NR - 1].z - ROW / 2, BZ1 + 0.25, 0, topM);
      top(BX1, TX1 + 0.25, BZ0 - 0.05, weeks[NR - 1].z - ROW / 2, 0, topM);
      /* the steps: each row of the machine stands on its own */
      for (var k = 1; k < NR; k++) {
        var wk = weeks[NR - 1 - k], front = wk.z + ROW / 2;
        bx(bench, BX1 - BX0, STEP, front - BZ0, k % 2 ? stepA : stepB, (BX0 + BX1) / 2, (k - 0.5) * STEP - 0.01, (BZ0 + front) / 2);
        top(BX0, BX1, k === NR - 1 ? BZ0 : front - ROW, front, k * STEP, k % 2 ? stepA : stepB);
      }
      /* an edge in the tone of the week along the front of each step */
      weeks.forEach(function (wk) {
        var m = mat(wk.tone, {glow: 0.5});
        bx(bench, BX1 - BX0, 0.07, 0.1, m, (BX0 + BX1) / 2, wk.y + 0.035, wk.z + ROW / 2 - 0.05, false);
      });
      /* apron, drawers, legs, a low shelf */
      var az = BZ1 + 0.02;
      bx(bench, w, 1.0, 0.2, dark, cx, -0.86, az);
      for (var i = 0; i < 6; i++) {
        var dx = BX0 + (i + 0.5) * w / 6;
        bx(bench, w / 6 - 0.3, 0.72, 0.08, topM, dx, -0.86, az + 0.12, false);
        bx(bench, 0.7, 0.1, 0.1, M.brass, dx, -0.82, az + 0.2, false);
      }
      [[BX0 + 0.4, BZ1 - 0.3], [TX1 - 0.4, BZ1 - 0.3], [BX0 + 0.4, BZ0 + 0.5], [TX1 - 0.4, BZ0 + 0.5], [(BX0 + TX1) / 2, BZ1 - 0.3]].forEach(function (p) {
        bx(bench, 0.55, -FLOOR - 0.36, 0.55, dark, p[0], (FLOOR - 0.36) / 2, p[1]);
      });
      bx(bench, w - 0.6, 0.16, d - 1.2, dark, cx, FLOOR + 1.1, cz);
      /* crates the parts came in, under the bench */
      var crate = mat(0xa9824f, {glow: 0.1});
      [[-6.5, 2.2, 1.5], [-3.6, 1.8, 1.2], [3.2, 2.4, 1.6], [8.6, 2.0, 1.3], [11.5, 1.6, 1.8]].forEach(function (c) {
        bx(bench, c[1], c[2], 1.8, crate, c[0], FLOOR + 1.18 + c[2] / 2, BZ1 - 1.6);
        bx(bench, c[1] + 0.06, 0.12, 1.86, dark, c[0], FLOOR + 1.18 + c[2] * 0.3, BZ1 - 1.6, false);
        bx(bench, c[1] + 0.06, 0.12, 1.86, dark, c[0], FLOOR + 1.18 + c[2] * 0.8, BZ1 - 1.6, false);
      });
    })();

    /* the free end of the bench: the plan, the tools, the desk lamp */
    (function workPlace() {
      var x0 = BX1 + 0.5, cx = (x0 + TX1) / 2;
      var plan = drawn(384, 256, function (g, w, h) {
        g.fillStyle = '#1d4a8c'; g.fillRect(0, 0, w, h);
        g.strokeStyle = 'rgba(255,255,255,.14)'; g.lineWidth = 1;
        for (var x = 0; x < w; x += 16) { g.beginPath(); g.moveTo(x, 0); g.lineTo(x, h); g.stroke(); }
        for (var y = 0; y < h; y += 16) { g.beginPath(); g.moveTo(0, y); g.lineTo(w, y); g.stroke(); }
        g.strokeStyle = '#eaf2ff'; g.lineWidth = 2;
        g.strokeRect(10, 10, w - 20, h - 20);
        for (var r = 0; r < 4; r++) {
          var yy = 44 + r * 46;
          for (var c = 0; c < 6; c++) g.strokeRect(62 + c * 44, yy - 13, 30, 26);
          g.beginPath(); g.moveTo(56, yy); g.lineTo(328, yy); g.stroke();
          if (r < 3) { g.beginPath(); g.arc(r % 2 ? 56 : 328, yy + 23, 23, r % 2 ? Math.PI / 2 : -Math.PI / 2, r % 2 ? Math.PI * 1.5 : Math.PI / 2); g.stroke(); }
        }
        g.strokeRect(w - 110, h - 46, 96, 32);
        g.fillStyle = '#eaf2ff'; g.fillRect(w - 102, h - 38, 60, 4); g.fillRect(w - 102, h - 28, 40, 4);
      });
      var sheet = new THREE.Mesh(UPLANE, new THREE.MeshLambertMaterial({map: plan, emissive: 0xffffff, emissiveMap: plan, emissiveIntensity: 0.35}));
      sheet.scale.set(2.9, 1.95, 1); sheet.rotation.x = -Math.PI / 2; sheet.rotation.z = 0.12; sheet.position.set(cx - 0.1, 0.012, 1.7);
      sheet.receiveShadow = true; bench.add(sheet);
      /* what keeps the plan flat: a brass rule, a pencil, an oil can */
      var rule = bx(bench, 1.9, 0.03, 0.16, M.brass, cx - 0.2, 0.04, 2.9, false); rule.rotation.y = 0.2;
      var pen = cy(bench, 0.035, 0.9, mat(0xe0b23a, {glow: 0.2}), cx + 0.9, 0.05, 0.9, 'x', 6, false); pen.rotation.y = 0.7;
      cone(bench, 0.05, 0.3, 0.34, M.brass, cx - 1.05, 0.17, 0.55, 10);
      cy(bench, 0.3, 0.3, M.brass, cx - 1.05, 0.15, 0.55, 'y', 10);
      var spout = cy(bench, 0.025, 0.7, M.brass, cx - 0.78, 0.55, 0.55, 'y', 6, false); spout.rotation.z = -0.7;
      /* two screwdrivers and a spanner left where they were used */
      [[cx - 0.5, -0.6, 0.5, 0xd8463a], [cx - 0.1, -0.9, -0.3, 0xb5683a]].forEach(function (s) {
        var g = grp(bench, s[0], 0.07, s[1]); g.rotation.y = s[2];
        cy(g, 0.07, 0.5, mat(s[3], {glow: 0.25}), -0.3, 0, 0, 'x', 8);
        cy(g, 0.025, 0.7, M.steel, 0.3, 0, 0, 'x', 6, false);
      });
      var span = grp(bench, cx + 0.8, 0.04, -0.9); span.rotation.y = -0.5;
      bx(span, 1.0, 0.05, 0.12, M.steel, 0, 0, 0, false); cy(span, 0.14, 0.05, M.steel, 0.55, 0, 0, 'y', 6, false); cy(span, 0.11, 0.05, M.steel, -0.55, 0, 0, 'y', 6, false);
      /* a mug */
      cy(bench, 0.2, 0.42, mat(0xe6e0d2, {glow: 0.2}), cx + 1.1, 0.21, 3.5, 'y', 10);
      cy(bench, 0.16, 0.02, lit(0x3a2418), cx + 1.1, 0.425, 3.5, 'y', 10, false);
      /* an open crate with wood wool: the next part came in it */
      var crate = mat(0xb48d5c, {glow: 0.14}), g2 = grp(bench, cx + 0.2, 0, -3.2); g2.rotation.y = 0.18;
      bx(g2, 2.2, 0.12, 1.7, crate, 0, 0.06, 0);
      bx(g2, 2.2, 1.1, 0.12, crate, 0, 0.55, 0.8); bx(g2, 2.2, 1.1, 0.12, crate, 0, 0.55, -0.8);
      bx(g2, 0.12, 1.1, 1.7, crate, 1.05, 0.55, 0); bx(g2, 0.12, 1.1, 1.7, crate, -1.05, 0.55, 0);
      var wool = mat(0xe3cf9a, {glow: 0.2});
      bx(g2, 2.0, 0.5, 1.5, wool, 0, 0.6, 0, false);
      for (var i = 0; i < 9; i++) {
        var t = bx(g2, 0.9, 0.03, 0.05, wool, -0.5 + (i % 3) * 0.5, 0.87 + (i % 2) * 0.03, -0.5 + Math.floor(i / 3) * 0.5, false);
        t.rotation.y = i * 1.3; t.rotation.z = (i % 3 - 1) * 0.2;
      }
      var lid = bx(bench, 2.3, 0.1, 1.8, crate, cx + 1.3, 0.62, -4.3); lid.rotation.x = 1.15;
      /* the desk lamp, bent over the plan */
      var lg = grp(bench, cx + 1.0, 0, 2.6);
      cy(lg, 0.42, 0.1, M.iron, 0, 0.05, 0, 'y', 12);
      var a1 = grp(lg, 0, 0.1, 0); a1.rotation.z = 0.35;
      bx(a1, 0.07, 1.8, 0.07, M.steel, 0, 0.9, 0);
      var a2 = grp(a1, 0, 1.8, 0); a2.rotation.z = 0.95;
      bx(a2, 0.07, 1.5, 0.07, M.steel, 0, 0.75, 0);
      cy(a2, 0.08, 0.14, M.brass, 0, 0, 0, 'z', 8, false);
      var head = grp(a2, 0, 1.5, 0); head.rotation.z = 0.95;
      cone(head, 0.16, 0.46, 0.5, mat(0x2f5a4a, {glow: 0.25}), 0, 0.2, 0, 10);
      cone(head, 0.14, 0.42, 0.48, new THREE.MeshBasicMaterial({color: col(0xffe2a6), side: THREE.BackSide}), 0, 0.19, 0, 10, false);
      var deskGlow = glow(0xffc070, 3.4, 0.55); head.add(deskGlow);
      var warm = glow(0xffb45e, 4.2, 0.22); warm.position.set(cx - 0.1, 0.3, 1.9); bench.add(warm);
    })();

    var tones = weeks.map(function (w) { return hex(w.tone); });
    var pages = [0, 1, 2, 3, 4].map(function (s) { return pageTex(s, tones); });

    /* what the machine is for: the pile of documents that waits beside the first place */
    (function pile() {
      var w0 = weeks[0], g = grp(bench, -w0.dir * (HALF + 1.72), w0.y, w0.z + 0.1), manila = mat(0xd9b676, {glow: 0.2});
      g.rotation.y = 0.12;
      bx(g, 1.36, 0.05, 1.04, M.steel, 0, 0.03, 0);
      bx(g, 1.36, 0.16, 0.04, M.steel, 0, 0.1, 0.52, false); bx(g, 1.36, 0.16, 0.04, M.steel, 0, 0.1, -0.52, false); bx(g, 0.04, 0.16, 1.04, M.steel, -0.68, 0.1, 0, false);
      var y = 0.06;
      [0.1, 0.05, 0.12, 0.04, 0.09, 0.05, 0.07].forEach(function (h, i) {
        var b = bx(g, 1.14 - (i % 3) * 0.06, h, 0.84 - (i % 2) * 0.06, i % 3 === 1 ? manila : i % 2 ? M.ivory : M.paper, (rnd() - 0.5) * 0.14, y + h / 2, (rnd() - 0.5) * 0.12);
        b.rotation.y = (rnd() - 0.5) * 0.3; y += h;
      });
      var top = plane(g, 1.08, 0.78, new THREE.MeshBasicMaterial({map: pages[0], color: col(0xdedad0)}), 0.03, y + 0.004, -0.02);
      top.rotation.x = -Math.PI / 2; top.rotation.z = 0.2;
      var weight = new THREE.Mesh(new THREE.SphereGeometry(0.17, 10, 6, 0, TAU, 0, Math.PI / 2), M.brass);
      weight.position.set(0.25, y, 0.12); weight.castShadow = true; g.add(weight);
    })();

    /* the room never moves */
    bake(room);

    /* --- a stand, its plate, its cloth ------------------------------------------ */

    var clothGeo = (function () {
      var g = new THREE.BoxGeometry(1.46, 1.16, 1.24, 6, 5, 6);
      g.translate(0, 0.58, 0);
      var p = g.attributes.position;
      for (var i = 0; i < p.count; i++) {
        var x = p.getX(i), y = p.getY(i), z = p.getZ(i), t = y / 1.16, a = Math.atan2(z, x);
        var hem = (1 - t) * (1 - t);
        var k = (1 - 0.3 * t * t * t) * (1 + hem * (0.2 + 0.08 * Math.sin(a * 9)));
        if (t > 0.99) y += 0.09 * Math.sin(x * 3.1 + 1) * Math.cos(z * 2.7) - 0.05 - 0.1 * (x * x + z * z);
        if (t < 0.01) y += 0.03 * Math.sin(a * 9);
        p.setXYZ(i, x * k, y, z * k);
      }
      g = g.toNonIndexed();
      g.computeVertexNormals();
      return g;
    })();

    /* the number of a day, on its plate: old brass, polished brass, or enamel in the week's tone */
    function plateOf(rec) {
      var c = document.createElement('canvas'); c.width = 192; c.height = 116;
      var tex = new THREE.CanvasTexture(c); tex.encoding = THREE.sRGBEncoding;
      var m = new THREE.MeshBasicMaterial({map: tex});
      function round(g, x, y, w, h, r) {
        g.beginPath(); g.moveTo(x + r, y); g.arcTo(x + w, y, x + w, y + h, r); g.arcTo(x + w, y + h, x, y + h, r);
        g.arcTo(x, y + h, x, y, r); g.arcTo(x, y, x + w, y, r); g.closePath();
      }
      rec.drawPlate = function (st) {
        var g = c.getContext('2d'), grad = g.createLinearGradient(0, 0, 0, 116);
        /* the corners outside the rounded edge take the colour of the brass behind, not black */
        g.fillStyle = st === 'open' ? '#f4f0e6' : st === 'ready' ? '#e2b65a' : '#6a5c38'; g.fillRect(0, 0, 192, 116);
        if (st === 'open') { grad.addColorStop(0, hex(rec.week.tone)); grad.addColorStop(1, hex(rec.week.tone)); }
        else if (st === 'ready') { grad.addColorStop(0, '#ffeeb4'); grad.addColorStop(0.5, '#f2cf73'); grad.addColorStop(1, '#c9953a'); }
        else { grad.addColorStop(0, '#8d7c4e'); grad.addColorStop(1, '#5b4e30'); }
        round(g, 3, 3, 186, 110, 14); g.fillStyle = grad; g.fill();
        g.lineWidth = 5; g.strokeStyle = st === 'open' ? '#ffffff' : st === 'ready' ? '#fff7d6' : '#b09a62'; g.stroke();
        g.fillStyle = st === 'locked' ? '#3d3420' : 'rgba(40,30,10,.55)';
        [[17, 17], [175, 17], [17, 99], [175, 99]].forEach(function (q) { g.beginPath(); g.arc(q[0], q[1], 4.5, 0, TAU); g.fill(); });
        g.font = 'italic 800 86px Fraunces, Georgia, serif';
        g.textAlign = 'center'; g.textBaseline = 'middle';
        if (st === 'locked') { g.fillStyle = 'rgba(214,196,140,.5)'; g.fillText(String(rec.day), 97.5, 65.5); }
        g.fillStyle = st === 'locked' ? '#2a2416' : st === 'ready' ? '#3a2608' : '#14101f';
        g.fillText(String(rec.day), 96, 64);
        tex.needsUpdate = true;
        m.color.setScalar(st === 'locked' ? 0.5 : st === 'ready' ? 0.95 : 0.9);
      };
      return m;
    }

    var pickables = [];
    var hidden = new THREE.MeshBasicMaterial({visible: false});
    function stand(rec) {
      var g = grp(bench, rec.x, rec.y, rec.z);
      rec.stand = g;
      takes(bx(g, 2.0, 0.14, 1.8, M.plinth, 0, 0.07, 0.05));
      /* the plate leans back on the front of the stand, to be read from above */
      var pg = grp(g, 0, 0.33, 1.0); pg.rotation.x = -0.82;
      bx(pg, 1.0, 0.62, 0.06, M.brass, 0, 0, -0.04, false);
      rec.plate = plane(pg, 0.94, 0.57, plateOf(rec), 0, 0, 0);
      rec.plateG = pg;
      rec.pilot = lamp(g, 0xffc45e, 0.075, 0.82, 0.2, 1.02, 0.95);
      cy(g, 0.1, 0.06, M.iron, 0.82, 0.15, 1.02, 'y', 8, false);
      /* the cloth, and the tag that says which week it belongs to */
      var cl = grp(g, 0, 0.14, 0);
      var cm = new THREE.Mesh(clothGeo, M.linen); cm.castShadow = true; cl.add(cm);
      var tag = bx(cl, 0.24, 0.34, 0.02, mat(rec.week.tone, {glow: 0.7}), 0.42, 0.42, 0.82, false); tag.rotation.x = 0.28; tag.rotation.z = 0.12;
      rec.cloth = cl;
      rec.under = glow(0xffc070, 1.9, 0); rec.under.position.set(0, 0.3, 0.9); g.add(rec.under);
      var hit = new THREE.Mesh(UBOX, hidden);
      hit.scale.set(2.16, 2.1, 2.5); hit.position.set(rec.x, rec.y + 1.05, rec.z + 0.1);
      hit.userData.day = rec.day;
      bench.add(hit); pickables.push(hit);
    }

    /* --- the parts ----------------------------------------------------------------
     * A part is drawn in its own place: the paper comes in at x = -1.1 and leaves
     * at x = +1.1, at height PY, along z = 0; +z is the front. On a row that runs
     * right to left the whole part is mirrored. A builder returns what moves:
     * a function of the time and of `busy` (1 while a sheet is inside). */

    function bedOf(g, T, bare) {
      bx(g, 1.9, 0.14, 1.34, M.iron, 0, 0.21, 0);
      bx(g, 1.9, 0.05, 0.03, T.lit, 0, 0.215, 0.68, false);
      if (bare) return;
      bx(g, 2.2, 0.04, 0.05, M.steel, -0.15, 0.5, 0.48, false);
      bx(g, 2.2, 0.04, 0.05, M.steel, -0.15, 0.5, -0.48, false);
      [-0.8, 0.8].forEach(function (x) {
        bx(g, 0.05, 0.22, 0.05, M.steel, x, 0.39, 0.48, false); bx(g, 0.05, 0.22, 0.05, M.steel, x, 0.39, -0.48, false);
      });
      [-0.62, 0, 0.62].forEach(function (x) { cy(g, 0.04, 0.92, M.steel, x, 0.5, 0, 'z', 6, false); });
    }
    function bridge(g, x, m) {
      bx(g, 0.07, 0.5, 0.07, m, x, 0.53, 0.56, false); bx(g, 0.07, 0.5, 0.07, m, x, 0.53, -0.56, false);
      bx(g, 0.09, 0.06, 1.2, m, x, 0.78, 0, false);
    }

    var PARTS = [
      /* ---- week 1: reading the paper ---- */

      /* 1. the feeder: a sloping tray of documents, a pick roller, a crank */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.5, 0.64, 0.08, M.cream, 0.1, 0.6, 0.58); bx(g, 1.5, 0.64, 0.08, M.cream, 0.1, 0.6, -0.58);
        bx(g, 1.0, 0.1, 1.24, M.cream, 0.36, 0.96, 0);
        bx(g, 1.0, 0.045, 0.03, T.lit, 0.36, 0.95, 0.635, false);
        var pick = grp(g, -0.05, 0.75, 0);
        cy(pick, 0.15, 0.9, M.rubber, 0, 0, 0, 'z', 8); bx(pick, 0.05, 0.31, 0.96, M.brass, 0, 0, 0, false);
        var tray = grp(g, -0.76, 0.94, 0); tray.rotation.z = -0.42;
        bx(tray, 1.5, 0.05, 1.04, M.cream, 0, 0, 0);
        bx(tray, 1.5, 0.2, 0.05, M.brass, 0, 0.1, 0.52); bx(tray, 1.5, 0.2, 0.05, M.brass, 0, 0.1, -0.52);
        for (var i = 0; i < 6; i++) {
          var s = bx(tray, 1.08, 0.026, 0.8, i % 2 ? M.paper : M.ivory, -0.12 + (rnd() - 0.5) * 0.08, 0.045 + i * 0.027, (rnd() - 0.5) * 0.06, false);
          s.rotation.y = (rnd() - 0.5) * 0.12;
        }
        var top = plane(tray, 1.08, 0.8, T.page0, -0.12, 0.215, 0); top.rotation.x = -Math.PI / 2;
        var crank = grp(g, 0.55, 0.62, 0.66);
        cy(crank, 0.21, 0.05, M.brass, 0, 0, 0, 'z', 10); cy(crank, 0.04, 0.2, M.iron, 0.14, 0, 0.1, 'z', 6, false);
        var l = lamp(g, T.tone, 0.07, 0.7, 1.06, 0.4, 0.9);
        return function (t, busy) {
          pick.rotation.z = -t * 3.2; crank.rotation.z = -t * 3.2;
          top.position.x = -0.12 + ((t * SPEED / GAP) % 1 > 0.86 ? ((t * SPEED / GAP) % 1 - 0.86) * 2 : 0);
          l.set(0.6 + 0.4 * busy);
        };
      },

      /* 2. the scanner bed: a glass, a bar of light that sweeps under it, a lid left open */
      function (g, T) {
        bedOf(g, T, true);
        bx(g, 1.84, 0.2, 1.28, M.cream, 0, 0.38, 0);
        bx(g, 1.5, 0.02, 0.98, M.glass, 0, 0.49, 0, false);
        var bar = grp(g, 0, 0.51, 0);
        bx(bar, 0.09, 0.03, 0.94, T.lit, 0, 0, 0, false);
        var gl = glow(T.tone, 1.5, 0.6); gl.position.y = 0.12; bar.add(gl);
        var lid = grp(g, 0, 0.5, -0.64); lid.rotation.x = -1.02;
        bx(lid, 1.84, 0.07, 1.22, M.cream, 0, 0.035, 0.61);
        bx(lid, 1.5, 0.02, 0.98, M.rubber, 0, -0.008, 0.61, false);
        bx(lid, 0.5, 0.05, 0.08, M.brass, 0, 0.08, 1.2, false);
        bx(g, 0.56, 0.05, 0.12, M.iron, 0.56, 0.5, 0.58, false);
        [0.4, 0.56, 0.72].forEach(function (x, i) { cy(g, 0.035, 0.04, i ? M.ivory : T.lit, x, 0.53, 0.58, 'y', 6, false); });
        return function (t, busy) {
          bar.position.x = Math.sin(t * 1.7) * 0.64;
          gl.material.opacity = 0.45 + 0.4 * busy;
        };
      },

      /* 3. the rollers: two pairs that flatten a crooked sheet, a gear, a pressure screw */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.62, 1.16, 0.1, M.cream, 0, 0.86, 0.6); bx(g, 0.62, 1.16, 0.1, M.cream, 0, 0.86, -0.6);
        bx(g, 0.72, 0.12, 1.3, M.cream, 0, 1.47, 0);
        bx(g, 0.72, 0.045, 0.03, T.lit, 0, 1.47, 0.665, false);
        var rolls = [];
        [[-0.24, 0.35, 0.15], [0.24, 0.35, 0.15], [-0.24, 0.78, 0.17], [0.24, 0.78, 0.17]].forEach(function (r) {
          var w = grp(g, r[0], r[1], 0);
          cy(w, r[2], 1.06, M.rubber, 0, 0, 0, 'z', 8); bx(w, 0.05, r[2] * 1.8, 1.12, M.brass, 0, 0, 0, false);
          rolls.push(w);
        });
        var g1 = new THREE.Mesh(gearGeo(0.32, 10, 0.07), M.brass); g1.position.set(0.24, 0.78, 0.71); g1.castShadow = true; g.add(g1);
        var g2 = new THREE.Mesh(gearGeo(0.2, 6, 0.07), M.steel); g2.position.set(-0.22, 0.6, 0.71); g.add(g2);
        cy(g, 0.045, 0.5, M.steel, 0, 1.76, 0, 'y', 6, false);
        var wheel = grp(g, 0, 1.98, 0);
        handwheel(wheel, 0.27, T.mat);
        return function (t, busy) {
          var a = t * 2.6;
          rolls[0].rotation.z = a; rolls[1].rotation.z = a; rolls[2].rotation.z = -a; rolls[3].rotation.z = -a;
          g1.rotation.z = -a; g2.rotation.z = a * 1.66;
          wheel.rotation.y = Math.sin(t * 0.7) * 0.5 + busy * 0.3;
        };
      },

      /* 4. the lens: a bellows camera on its column, looking down at the sheet */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.2, 0.05, 0.3, M.cream, 0, 0.305, -0.6);
        cy(g, 0.065, 1.72, M.steel, 0, 1.14, -0.62, 'y', 8);
        var head = grp(g, 0, 1.52, -0.62);
        bx(head, 0.2, 0.2, 0.2, M.brass, 0, 0, 0, false);
        bx(head, 0.16, 0.12, 0.7, M.cream, 0, 0, 0.3);
        bx(head, 0.56, 0.36, 0.5, M.cream, 0, 0, 0.62);
        bx(head, 0.56, 0.05, 0.03, T.lit, 0, 0.08, 0.875, false);
        bx(head, 0.42, 0.1, 0.4, M.rubber, 0, -0.22, 0.62, false); bx(head, 0.34, 0.1, 0.32, M.bake, 0, -0.31, 0.62, false); bx(head, 0.27, 0.1, 0.25, M.rubber, 0, -0.4, 0.62, false);
        cy(head, 0.14, 0.14, M.brass, 0, -0.52, 0.62, 'y', 10);
        cy(head, 0.1, 0.02, lit(0xbfe6ff), 0, -0.595, 0.62, 'y', 10, false);
        var ring = new THREE.Mesh(new THREE.TorusGeometry(0.23, 0.035, 5, 14), T.lit); ring.rotation.x = Math.PI / 2; ring.position.set(0, -0.5, 0.62); head.add(ring);
        var beamM = new THREE.MeshBasicMaterial({color: col(T.tone), transparent: true, opacity: 0.16, blending: THREE.AdditiveBlending, depthWrite: false, side: THREE.DoubleSide});
        var beam = new THREE.Mesh(new THREE.CylinderGeometry(0.12, 0.5, 0.36, 12, 1, true), beamM); beam.position.set(0, -0.78, 0.62); head.add(beam);
        var gl = glow(T.tone, 1.3, 0.5); gl.position.set(0, -0.56, 0.62); head.add(gl);
        cy(g, 0.1, 0.1, M.brass, 0.1, 1.2, -0.62, 'x', 8, false);
        return function (t, busy) {
          head.position.y = 1.52 + Math.sin(t * 0.9) * 0.05;
          beamM.opacity = 0.1 + 0.22 * busy; gl.material.opacity = 0.35 + 0.5 * busy;
        };
      },

      /* 5. the sorter: four trays, one per kind of document, and the arm that lifts a sheet to the right one */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.08, 1.36, 0.08, M.steel, -0.56, 0.96, -0.74); bx(g, 0.08, 1.36, 0.08, M.steel, 0.56, 0.96, -0.74);
        var tabs = [], dim = mat(0x6a5a34);
        for (var i = 0; i < 4; i++) {
          var tr = grp(g, 0, 0.84 + i * 0.25, -0.5); tr.rotation.x = 0.32;
          bx(tr, 1.14, 0.04, 0.62, M.cream, 0, 0, 0); bx(tr, 1.14, 0.1, 0.04, M.cream, 0, 0.04, 0.31, false);
          bx(tr, 0.9, 0.03 + (i % 2) * 0.02, 0.5, M.paper, 0.04 - i * 0.02, 0.04, -0.02, false);
          tabs.push(bx(tr, 0.2, 0.04, 0.1, dim, -0.34 + i * 0.22, 0.07, 0.24, false));
        }
        var arm = grp(g, -0.86, 0.64, 0.2);
        cy(arm, 0.07, 0.3, M.brass, 0, 0, 0, 'z', 8, false);
        bx(arm, 0.95, 0.035, 0.28, M.brass, 0.47, 0, 0);
        var knob = grp(g, 0.62, 0.42, 0.68);
        cy(knob, 0.14, 0.08, M.bake, 0, 0, 0, 'z', 10); bx(knob, 0.03, 0.14, 0.02, M.ivory, 0, 0.06, 0.05, false);
        return function (t) {
          var u = t * 0.7, k = Math.floor(u) % 4, f = smooth((u % 1) * 2.5);
          var from = 0.12 + ((k + 3) % 4) * 0.13, to = 0.12 + k * 0.13;
          arm.rotation.z = from + (to - from) * f;
          knob.rotation.z = -(k + f) * 0.6;
          for (var i = 0; i < 4; i++) tabs[i].material = i === k ? T.lit : dim;
        };
      },

      /* 6. the turret: six lenses on a wheel, one reader per kind, turning over the sheet */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.14, 1.26, 0.14, M.cream, 0, 0.91, 0.62); bx(g, 0.14, 1.26, 0.14, M.cream, 0, 0.91, -0.62);
        bx(g, 0.34, 0.14, 1.42, M.cream, 0, 1.6, 0);
        bx(g, 0.03, 0.05, 1.42, T.lit, 0.175, 1.6, 0, false);
        bx(g, 0.44, 0.22, 0.44, M.cream, 0, 1.78, 0); bx(g, 0.45, 0.04, 0.45, T.lit, 0, 1.72, 0, false);
        var tur = grp(g, 0, 1.36, 0);
        cy(tur, 0.54, 0.08, M.steel, 0, 0, 0, 'y', 12); cy(tur, 0.56, 0.03, M.brass, 0, 0.03, 0, 'y', 12, false); cy(tur, 0.08, 0.3, M.steel, 0, 0.12, 0, 'y', 6, false);
        for (var i = 0; i < 6; i++) {
          var a = i * TAU / 6, len = 0.28 + (i % 3) * 0.06;
          cy(tur, 0.1, len, i % 2 ? M.brass : M.iron, Math.cos(a) * 0.37, -0.04 - len / 2, Math.sin(a) * 0.37, 'y', 8);
          cy(tur, 0.07, 0.02, lit(0xbfe6ff), Math.cos(a) * 0.37, -0.05 - len, Math.sin(a) * 0.37, 'y', 8, false);
        }
        var beamM = new THREE.MeshBasicMaterial({color: col(T.tone), transparent: true, opacity: 0.16, blending: THREE.AdditiveBlending, depthWrite: false, side: THREE.DoubleSide});
        var beam = new THREE.Mesh(new THREE.CylinderGeometry(0.07, 0.34, 0.5, 10, 1, true), beamM); beam.position.set(0.37, 0.82, 0); g.add(beam);
        var gl = glow(T.tone, 1.0, 0.5); gl.position.set(0.37, 0.62, 0); g.add(gl);
        return function (t, busy) {
          var u = t * 0.55;
          tur.rotation.y = (Math.floor(u) + smooth((u % 1) * 3)) * TAU / 6;
          beamM.opacity = 0.1 + 0.2 * busy; gl.material.opacity = 0.3 + 0.5 * busy;
        };
      },

      /* ---- week 2: the index ---- */

      /* 7. the card cabinet: six drawers over the track, one of them always being consulted */
      function (g, T) {
        bedOf(g, T);
        [[-0.8, 0.52], [0.8, 0.52], [-0.8, -0.52], [0.8, -0.52]].forEach(function (p) { bx(g, 0.12, 0.5, 0.12, M.walnut2, p[0], 0.53, p[1]); });
        bx(g, 1.8, 0.92, 1.2, M.walnut, 0, 1.24, 0);
        bx(g, 1.86, 0.05, 1.26, M.walnut2, 0, 1.725, 0);
        var dr = null;
        [-0.58, 0, 0.58].forEach(function (x, c) {
          [1.02, 1.46].forEach(function (y, r) {
            var d = grp(g, x, y, 0.6);
            bx(d, 0.52, 0.38, 0.05, M.walnut2, 0, 0, 0.025, false);
            bx(d, 0.2, 0.05, 0.05, M.brass, 0, -0.1, 0.07, false);
            bx(d, 0.28, 0.12, 0.02, M.ivory, 0, 0.07, 0.06, false);
            bx(d, 0.08, 0.12, 0.022, T.lit, -0.1, 0.07, 0.062, false);
            if (c === 1 && r === 1) {
              dr = d;
              bx(d, 0.46, 0.3, 0.7, M.walnut2, 0, -0.03, -0.35, false);
              for (var i = 0; i < 6; i++) bx(d, 0.4, 0.3, 0.02, i === 2 ? T.mat : M.ivory, 0, 0.03 + (i === 2 ? 0.04 : 0), -0.08 - i * 0.09, false);
            }
          });
        });
        /* a tray of cards left on top */
        bx(g, 0.9, 0.12, 0.5, M.walnut2, -0.3, 1.81, -0.2);
        for (var i = 0; i < 7; i++) bx(g, 0.02, 0.26, 0.42, i === 4 ? T.mat : M.ivory, -0.66 + i * 0.12, 1.9 + (i === 4 ? 0.04 : 0), -0.2, false);
        var l = lamp(g, T.tone, 0.07, 0.6, 1.8, 0.3, 0.9);
        return function (t, busy) {
          dr.position.z = 0.6 + smooth(0.5 - 0.5 * Math.cos(t * 1.3)) * 0.46;
          l.set(0.55 + 0.45 * busy);
        };
      },

      /* 8. the card drum: a wheel of index cards that flips one card at a time */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.56, 1.0, 0.09, M.walnut, 0, 0.78, 0.52); bx(g, 0.56, 1.0, 0.09, M.walnut, 0, 0.78, -0.52);
        bx(g, 0.9, 0.12, 0.09, M.walnut2, 0, 0.34, 0.52, false); bx(g, 0.9, 0.12, 0.09, M.walnut2, 0, 0.34, -0.52, false);
        var wheel = grp(g, 0, 1.2, 0);
        cy(wheel, 0.17, 0.86, M.brass, 0, 0, 0, 'z', 10);
        for (var i = 0; i < 16; i++) {
          var a = i * TAU / 16;
          var c = bx(wheel, 0.36, 0.016, 0.76, i % 4 === 0 ? T.mat : M.ivory, Math.cos(a) * 0.37, Math.sin(a) * 0.37, 0, false);
          c.rotation.z = a; c.castShadow = true;
        }
        var crank = grp(g, 0, 1.2, 0.6);
        cy(crank, 0.05, 0.1, M.brass, 0, 0, 0, 'z', 6, false); bx(crank, 0.05, 0.3, 0.04, M.brass, 0, 0.13, 0.05, false); cy(crank, 0.045, 0.16, M.bake, 0, 0.27, 0.13, 'z', 6, false);
        bx(g, 0.5, 0.06, 0.06, M.brass, 0.42, 1.36, 0.5, false);
        var l = lamp(g, T.tone, 0.07, 0, 1.36, 0.56, 0.9);
        return function (t, busy) {
          var u = t * (2.2 + busy * 2);
          wheel.rotation.z = -(Math.floor(u) + smooth((u % 1) * 2)) * TAU / 16;
          crank.rotation.z = -u * TAU / 4;
          l.set(0.5 + 0.5 * busy);
        };
      },

      /* 9. the punch: a hopper of cards and the head that punches a key into each */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.42, 1.34, 0.3, M.walnut2, 0.3, 0.95, -0.62);
        bx(g, 0.42, 0.26, 0.8, M.walnut2, 0.3, 1.5, -0.2);
        bx(g, 0.5, 0.12, 1.0, M.iron, 0.3, 0.37, 0, false);
        var head = grp(g, 0.3, 1.0, 0);
        bx(head, 0.36, 0.3, 0.62, M.steel, 0, 0.06, 0); bx(head, 0.3, 0.14, 0.54, M.iron, 0, -0.15, 0, false);
        cy(head, 0.06, 0.4, M.steel, 0, 0.36, 0, 'y', 6, false);
        var fly = grp(g, 0.3, 1.5, 0.26);
        var rim = grp(fly, 0, 0, 0); rim.rotation.x = Math.PI / 2; handwheel(rim, 0.3, M.brass);
        /* the hopper */
        bx(g, 0.64, 0.5, 0.08, M.walnut, -0.52, 0.53, 0.58); bx(g, 0.64, 0.5, 0.08, M.walnut, -0.52, 0.53, -0.58);
        bx(g, 0.64, 0.06, 1.24, M.walnut, -0.52, 0.8, 0);
        bx(g, 0.5, 0.34, 0.86, M.ivory, -0.52, 1.0, 0);
        var top = plane(g, 0.5, 0.86, T.card, -0.52, 1.172, 0); top.rotation.x = -Math.PI / 2;
        bx(g, 0.06, 0.5, 0.06, M.brass, -0.8, 1.05, 0.46, false); bx(g, 0.06, 0.5, 0.06, M.brass, -0.8, 1.05, -0.46, false);
        var l = lamp(g, T.tone, 0.08, 0.3, 1.7, -0.5, 1.1);
        return function (t, busy) {
          var c = (t * 2.3) % 1, down = Math.pow(Math.max(0, Math.sin(c * Math.PI)), 6);
          head.position.y = 1.0 - down * 0.2;
          fly.rotation.z = -t * 2.3 * TAU;
          l.set(0.25 + 0.75 * down);
        };
      },

      /* 10. the cartridge shelf: two rows of labelled cartridges, and the picker that goes to fetch one */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.1, 1.3, 0.5, M.walnut, -0.88, 0.93, -0.52); bx(g, 0.1, 1.3, 0.5, M.walnut, 0.88, 0.93, -0.52);
        [0.76, 1.17, 1.58].forEach(function (y) { bx(g, 1.86, 0.06, 0.5, M.walnut, 0, y, -0.52); });
        bx(g, 1.76, 0.82, 0.04, M.walnut2, 0, 1.17, -0.75, false);
        var cols = [0x3b4d78, 0xe6dcc2, 0x2b2f3c, 0x7a3558, 0xe6dcc2, 0x3b4d78, 0x2b2f3c];
        [0.79, 1.2].forEach(function (y, r) {
          for (var i = 0; i < 7; i++) {
            if ((r === 0 && i === 4) || (r === 1 && i === 1)) continue;
            bx(g, 0.19, 0.36, 0.34, mat(cols[(i + r * 3) % 7], {glow: 0.2}), -0.69 + i * 0.23, y + 0.18, -0.5, false);
          }
          bx(g, 1.6, 0.09, 0.02, M.ivory, 0, y + 0.13, -0.325, false);
          bx(g, 1.6, 0.03, 0.022, T.lit, 0, y + 0.2, -0.324, false);
        });
        bx(g, 1.8, 0.04, 0.04, M.steel, 0, 1.16, -0.2, false);
        var car = grp(g, 0, 1.16, -0.2);
        bx(car, 0.16, 0.5, 0.1, M.brass, 0, 0, 0); bx(car, 0.07, 0.07, 0.2, M.steel, 0, 0.16, -0.12, false);
        var l = lamp(car, T.tone, 0.06, 0, 0.3, 0, 0.9);
        var stops = [-0.69, 0.23, -0.23, 0.69, 0, 0.46];
        return function (t, busy) {
          var u = t * 0.6, k = Math.floor(u) % stops.length, f = smooth((u % 1) * 2);
          car.position.x = stops[(k + stops.length - 1) % stops.length] + (stops[k] - stops[(k + stops.length - 1) % stops.length]) * f;
          l.set(f >= 1 ? 1 : 0.3 + 0.3 * busy);
        };
      },

      /* 11. the patch board: jacks and cords, to make the links the cards alone do not show */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.8, 0.6, 0.6, M.walnut, 0, 0.58, -0.52);
        bx(g, 0.08, 1.1, 0.7, M.walnut, -0.9, 0.85, -0.56); bx(g, 0.08, 1.1, 0.7, M.walnut, 0.9, 0.85, -0.56);
        var pg = grp(g, 0, 0.88, -0.2); pg.rotation.x = -0.78;
        bx(pg, 1.74, 1.04, 0.08, M.walnut2, 0, 0.5, -0.045);
        plane(pg, 1.66, 0.96, T.jacks, 0, 0.5, 0.002);
        function jack(i, j) { return new THREE.Vector3(-0.675 + i * 0.27, 0.14 + j * 0.24, 0.01); }
        var cords = [[0, 3, 2, 0, T.mat], [1, 2, 4, 3, M.ivory], [3, 1, 5, 2, M.red], [2, 2, 5, 0, T.mat], [0, 1, 1, 0, M.steel]];
        cords.forEach(function (c) {
          var a = jack(c[0], c[1]), b = jack(c[2], c[3]), mid = a.clone().add(b).multiplyScalar(0.5);
          mid.z = 0.3; mid.y -= 0.18;
          var tube = new THREE.Mesh(new THREE.TubeGeometry(new THREE.QuadraticBezierCurve3(a, mid, b), 8, 0.025, 5), c[4]);
          pg.add(tube);
          cy(pg, 0.045, 0.1, M.brass, a.x, a.y, 0.05, 'z', 6, false); cy(pg, 0.045, 0.1, M.brass, b.x, b.y, 0.05, 'z', 6, false);
        });
        var ls = [[1, 3], [4, 1], [5, 3], [3, 0], [2, 1]].map(function (q) { var p = jack(q[0], q[1]); return lamp(pg, T.tone, 0.045, p.x, p.y, 0.03, 0.6); });
        bridge(g, 0, M.brass);
        return function (t, busy) {
          ls.forEach(function (l, i) { l.set((Math.floor(t * 3 + i * 1.7) % 3 === 0 || busy > 0.6) ? 1 : 0.1); });
        };
      },

      /* 12. the tape unit: two reels in a wooden case, where the index is kept from one day to the next */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.74, 1.3, 0.5, M.walnut, 0, 0.93, -0.54);
        bx(g, 1.6, 1.16, 0.03, M.cream, 0, 0.93, -0.275, false);
        var reels = [-0.42, 0.42].map(function (x) {
          var r = grp(g, x, 1.2, -0.23);
          cy(r, 0.34, 0.04, M.steel, 0, 0, 0, 'z', 14); cy(r, 0.25, 0.05, M.tape, 0, 0, 0, 'z', 12, false);
          cy(r, 0.09, 0.07, T.mat, 0, 0, 0, 'z', 6, false);
          [0, 1, 2].forEach(function (k) { var s = bx(r, 0.09, 0.2, 0.055, M.iron, Math.cos(k * TAU / 3) * 0.2, Math.sin(k * TAU / 3) * 0.2, 0, false); s.rotation.z = k * TAU / 3 + Math.PI / 2; });
          return r;
        });
        bx(g, 0.4, 0.14, 0.1, M.iron, 0, 0.74, -0.22, false);
        cy(g, 0.04, 0.1, M.steel, -0.3, 0.8, -0.22, 'z', 6, false); cy(g, 0.04, 0.1, M.steel, 0.3, 0.8, -0.22, 'z', 6, false);
        var t1 = bx(g, 0.44, 0.015, 0.03, M.tape, -0.5, 0.94, -0.22, false); t1.rotation.z = -1.0;
        var t2 = bx(g, 0.44, 0.015, 0.03, M.tape, 0.5, 0.94, -0.22, false); t2.rotation.z = 1.0;
        var n1 = dial(g, 0.12, -0.58, 0.53, -0.255), n2 = dial(g, 0.12, 0.58, 0.53, -0.255);
        [-0.16, 0, 0.16].forEach(function (x) { cy(g, 0.05, 0.06, M.bake, x, 0.5, -0.24, 'z', 8, false); });
        var l = lamp(g, T.tone, 0.06, 0, 1.44, -0.25, 0.8);
        bx(g, 1.8, 0.05, 0.56, M.walnut2, 0, 1.605, -0.54, false);
        return function (t, busy) {
          reels[0].rotation.z = -t * 2.2; reels[1].rotation.z = -t * 1.4;
          n1.rotation.z = Math.sin(t * 3.1) * 0.3 + 0.2 * busy; n2.rotation.z = Math.sin(t * 2.3 + 1) * 0.35 - 0.1;
          l.set(0.6 + 0.4 * Math.sin(t * 4));
        };
      },

      /* ---- week 3: control ---- */

      /* 13. the console: three dials that watch what goes through */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.8, 0.5, 0.62, M.hammer, 0, 0.53, -0.52);
        bx(g, 0.08, 1.3, 0.66, M.hammer2, -0.92, 0.93, -0.52); bx(g, 0.08, 1.3, 0.66, M.hammer2, 0.92, 0.93, -0.52);
        var pg = grp(g, 0, 0.78, -0.22); pg.rotation.x = -0.5;
        bx(pg, 1.78, 1.02, 0.08, M.panel, 0, 0.5, -0.045);
        var ns = [-0.58, 0, 0.58].map(function (x) { return dial(pg, 0.23, x, 0.64, 0.004); });
        [-0.6, -0.2, 0.2, 0.6].forEach(function (x, i) {
          cy(pg, 0.07, 0.08, M.bake, x, 0.2, 0.04, 'z', 8, false);
          bx(pg, 0.02, 0.08, 0.02, M.ivory, x + (i % 2 ? 0.03 : -0.02), 0.23, 0.085, false);
        });
        bx(pg, 1.78, 0.04, 0.02, T.lit, 0, 0.99, 0.0, false);
        var l1 = lamp(pg, T.tone, 0.05, -0.8, 0.36, 0.03, 0.7), l2 = lamp(pg, T.tone, 0.05, 0.8, 0.36, 0.03, 0.7);
        bridge(g, 0.4, M.steel);
        return function (t, busy) {
          ns.forEach(function (n, i) { n.rotation.z = Math.sin(t * (1.1 + i * 0.37) + i * 2) * 0.45 - (i === 1 ? busy * 0.3 : 0); });
          l1.set(busy); l2.set(1 - busy);
        };
      },

      /* 14. the lever: a hand lever on its quadrant, and the brake it throws on the paper */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.3, 0.36, 0.42, M.hammer, -0.15, 0.46, -0.62);
        var quad = new THREE.Mesh(new THREE.CylinderGeometry(0.62, 0.62, 0.06, 14, 1, false, -Math.PI / 2, Math.PI), M.brass);
        quad.rotation.x = -Math.PI / 2; quad.position.set(-0.15, 0.64, -0.6); quad.castShadow = true; g.add(quad);
        var lv = grp(g, -0.15, 0.64, -0.5);
        bx(lv, 0.08, 1.16, 0.05, M.steel, 0, 0.58, 0); sp(lv, 0.12, M.red, 0, 1.22, 0);
        bx(lv, 0.04, 0.4, 0.04, M.iron, 0.07, 0.86, 0, false);
        cy(lv, 0.09, 0.14, M.iron, 0, 0, 0, 'z', 8, false);
        bx(g, 0.1, 0.72, 0.1, M.hammer2, 0.62, 0.64, 0.58); bx(g, 0.1, 0.72, 0.1, M.hammer2, 0.62, 0.64, -0.58);
        bx(g, 0.18, 0.12, 1.26, M.hammer2, 0.62, 1.03, 0);
        var pad = grp(g, 0.62, 0.9, 0);
        bx(pad, 0.3, 0.08, 0.84, M.rubber, 0, 0, 0, false); cy(pad, 0.04, 0.3, M.steel, 0, 0.14, 0.2, 'y', 6, false); cy(pad, 0.04, 0.3, M.steel, 0, 0.14, -0.2, 'y', 6, false);
        cy(g, 0.15, 0.04, T.mat, 0.4, 0.66, -0.62, 'y', 10, false); cy(g, 0.11, 0.1, M.red, 0.4, 0.71, -0.62, 'y', 10, false);
        cy(g, 0.03, 0.5, M.steel, -0.72, 0.88, -0.62, 'y', 6, false);
        var l = lamp(g, T.tone, 0.08, -0.72, 1.18, -0.62, 1.1);
        return function (t) {
          var c = (t * 0.3) % 1, hold = smooth(c * 8) * (1 - smooth((c - 0.3) * 8));
          lv.rotation.z = 0.6 - hold * 1.2;
          pad.position.y = 0.92 - hold * 0.27;
          l.set(hold);
        };
      },

      /* 15. the gate: a shutter across the track, a red lamp and a green one */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.14, 1.46, 0.14, M.hammer, 0, 1.01, 0.62); bx(g, 0.14, 1.46, 0.14, M.hammer, 0, 1.01, -0.62);
        bx(g, 0.22, 0.16, 1.38, M.hammer, 0, 1.8, 0);
        var sh = grp(g, 0, 0.46, 0);
        bx(sh, 0.05, 0.64, 1.08, M.panel, 0, 0.32, 0);
        bx(sh, 0.06, 0.1, 1.08, T.mat, 0, 0.05, 0, false);
        bx(sh, 0.06, 0.05, 1.08, T.mat, 0, 0.5, 0, false);
        bx(g, 0.32, 0.26, 0.6, M.hammer2, 0, 2.01, 0);
        var red = lamp(g, 0xff4a3a, 0.09, 0, 2.2, -0.15, 1.2), green = lamp(g, 0x55f08a, 0.09, 0, 2.2, 0.15, 1.2);
        cy(g, 0.11, 0.05, M.brass, 0, 1.8, 0.75, 'x', 8, false);
        bx(g, 0.02, 1.0, 0.02, M.steel, 0, 1.3, 0.78, false);
        var wt = cy(g, 0.08, 0.22, M.brass, 0, 1.4, 0.78, 'y', 8, false);
        var open = 0;
        return function (t, busy, dt) {
          open += ((busy > 0.3 ? 1 : 0) - open) * Math.min(1, dt * 7);
          sh.position.y = 0.46 + open * 0.64;
          wt.position.y = 1.5 - open * 0.6;
          red.set(open < 0.5 ? 1 : 0.05); green.set(open < 0.5 ? 0.05 : 1);
        };
      },

      /* 16. the scope: a round screen that shows what the machine is doing, and a stack of signal lamps */
      function (g, T) {
        bedOf(g, T);
        var sc = grp(g, -0.32, 0.28, -0.82); sc.rotation.x = -0.2;
        bx(sc, 1.08, 0.9, 0.62, M.hammer, 0, 0.45, 0.31);
        cy(sc, 0.33, 0.05, M.bake, -0.16, 0.5, 0.63, 'z', 16, false);
        var screen = new THREE.Mesh(UCIRC, new THREE.MeshBasicMaterial({map: T.scope}));
        screen.scale.set(0.28, 0.28, 1); screen.position.set(-0.16, 0.5, 0.66); sc.add(screen);
        [0.72, 0.5, 0.28].forEach(function (y) { cy(sc, 0.055, 0.07, M.bake, 0.36, y, 0.64, 'z', 8, false); });
        bx(sc, 0.7, 0.05, 0.05, M.steel, 0, 0.98, 0.31, false); bx(sc, 0.05, 0.1, 0.05, M.steel, -0.33, 0.93, 0.31, false); bx(sc, 0.05, 0.1, 0.05, M.steel, 0.33, 0.93, 0.31, false);
        bx(sc, 1.08, 0.04, 0.02, T.lit, 0, 0.06, 0.625, false);
        cy(g, 0.15, 0.06, M.hammer2, 0.64, 0.31, -0.5, 'y', 10, false);
        cy(g, 0.04, 0.94, M.steel, 0.64, 0.8, -0.5, 'y', 6, false);
        var stack = [[0xff4a3a, 1.72], [0xffb43a, 1.5], [0x55f08a, 1.28]].map(function (q) {
          var m = new THREE.MeshBasicMaterial({color: col(q[0])});
          var c = new THREE.Mesh(UCYL[10] || (UCYL[10] = new THREE.CylinderGeometry(1, 1, 1, 10)), m); c.scale.set(0.12, 0.2, 0.12); c.position.set(0.64, q[1], -0.5); g.add(c);
          var gl = glow(q[0], 1.3, 0); gl.position.set(0.64, q[1], -0.5); g.add(gl);
          return {m: m, base: col(q[0]), gl: gl};
        });
        cy(g, 0.13, 0.04, M.hammer2, 0.64, 1.84, -0.5, 'y', 10, false);
        bridge(g, 0.15, M.steel);
        return function (t, busy) {
          T.scope.offset.x = t * 0.45;
          var on = busy > 0.4 ? 2 : (t % 9 < 0.5 ? 0 : 1);
          stack.forEach(function (s, i) { s.m.color.copy(s.base).multiplyScalar(i === on ? 1 : 0.16); s.gl.material.opacity = i === on ? 0.8 : 0; });
        };
      },

      /* 17. the register: a board of small lamps, the working memory of the machine */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.8, 0.42, 0.6, M.hammer, 0, 0.49, -0.52);
        bx(g, 0.08, 1.26, 0.6, M.hammer2, -0.9, 0.91, -0.52); bx(g, 0.08, 1.26, 0.6, M.hammer2, 0.9, 0.91, -0.52);
        var pg = grp(g, 0, 0.7, -0.26); pg.rotation.x = -0.46;
        bx(pg, 1.72, 1.06, 0.07, M.bake, 0, 0.53, -0.04);
        var on = lit(T.tone), off = lit(0x3a3350), bulbs = [];
        for (var j = 0; j < 3; j++) for (var i = 0; i < 6; i++) bulbs.push(sp(pg, 0.06, off, -0.65 + i * 0.26, 0.46 + j * 0.22, 0.02, false));
        for (var k = 0; k < 8; k++) {
          var sw = bx(pg, 0.04, 0.13, 0.04, M.steel, -0.7 + k * 0.2, 0.18, 0.05, false); sw.rotation.x = (k * 7) % 3 ? 0.5 : -0.5;
          cy(pg, 0.035, 0.03, M.brass, -0.7 + k * 0.2, 0.18, 0.01, 'z', 6, false);
        }
        bx(g, 1.74, 0.03, 0.02, T.lit, 0, 0.71, -0.2, false);
        var gl = glow(T.tone, 2.2, 0.3); gl.position.set(0, 1.2, -0.3); g.add(gl);
        bridge(g, -0.3, M.steel);
        var last = -1;
        return function (t, busy) {
          var n = Math.floor(t * 2.6);
          if (n === last) return;
          last = n;
          for (var i = 0; i < bulbs.length; i++) bulbs[i].material = (((n * 7 + i * 13) ^ (n >> (i % 5))) & 3) > 1 ? on : off;
          gl.material.opacity = 0.22 + 0.2 * busy;
        };
      },

      /* 18. the recorder: a pen on a moving strip of paper, the trace of everything that happened */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.5, 0.5, 0.62, M.hammer, 0, 0.53, -0.52);
        bx(g, 0.1, 0.62, 0.8, M.hammer2, -0.72, 1.02, -0.48); bx(g, 0.1, 0.62, 0.8, M.hammer2, 0.72, 1.02, -0.48);
        var pg = grp(g, 0, 0.82, -0.16); pg.rotation.x = -1.0;
        bx(pg, 1.34, 0.8, 0.06, M.hammer2, 0, 0.4, -0.04);
        var strip = new THREE.Mesh(UPLANE, new THREE.MeshBasicMaterial({map: T.chart}));
        strip.scale.set(1.04, 0.74, 1); strip.position.set(0, 0.4, 0.002); pg.add(strip);
        var pen = grp(pg, 0, 0.78, 0.03);
        bx(pen, 0.03, 0.5, 0.02, T.mat, 0, -0.25, 0, false); cy(pen, 0.05, 0.05, M.brass, 0, 0, 0, 'z', 6, false); sp(pen, 0.04, M.bake, 0, -0.5, 0, false);
        cy(g, 0.15, 1.1, M.paper, 0, 1.3, -0.8, 'x', 10); cy(g, 0.1, 1.1, M.paper, 0, 0.84, -0.14, 'x', 10, false);
        [-0.5, -0.3].forEach(function (x) { cy(g, 0.05, 0.06, M.bake, x, 0.5, -0.19, 'z', 8, false); });
        var l = lamp(g, T.tone, 0.06, 0.5, 0.52, -0.2, 0.8);
        bridge(g, 0.3, M.steel);
        return function (t, busy) {
          T.chart.offset.y = t * 0.22;
          pen.rotation.z = Math.sin(t * 2.3) * 0.26 + Math.sin(t * 5.1) * 0.1 + busy * 0.12;
          l.set(0.4 + 0.6 * busy);
        };
      },

      /* ---- week 4: the process ---- */

      /* 19. the conveyor: a belt and its motor, the process as a line that moves */
      function (g, T) {
        bedOf(g, T, true);
        bx(g, 1.9, 0.2, 0.07, M.enamel, 0, 0.45, 0.52); bx(g, 1.9, 0.2, 0.07, M.enamel, 0, 0.45, -0.52);
        bx(g, 1.9, 0.035, 0.02, T.lit, 0, 0.5, 0.56, false);
        var belt = new THREE.Mesh(UBOX, new THREE.MeshLambertMaterial({map: T.belt, emissive: 0xffffff, emissiveMap: T.belt, emissiveIntensity: 0.3}));
        belt.scale.set(1.84, 0.07, 0.96); belt.position.set(0, 0.49, 0); g.add(belt);
        cy(g, 0.11, 0.98, M.steel, -0.92, 0.44, 0, 'z', 8, false); cy(g, 0.11, 0.98, M.steel, 0.92, 0.44, 0, 'z', 8, false);
        cy(g, 0.25, 0.62, M.enamel2, -0.36, 0.62, -0.7, 'x', 10);
        [-0.56, -0.36, -0.16].forEach(function (x) { cy(g, 0.28, 0.04, M.iron, x, 0.62, -0.7, 'x', 10, false); });
        bx(g, 0.44, 0.5, 0.34, M.enamel, 0.24, 0.53, -0.7);
        var pul = grp(g, 0.6, 0.62, -0.5);
        cy(pul, 0.2, 0.06, M.steel, 0, 0, 0, 'z', 10); bx(pul, 0.34, 0.06, 0.07, M.iron, 0, 0, 0, false);
        var l = lamp(g, T.tone, 0.07, 0.24, 0.84, -0.7, 1.0);
        bx(g, 0.34, 0.22, 0.12, M.enamel2, -0.6, 0.42, 0.63, false);
        cy(g, 0.045, 0.05, lit(0x55f08a), -0.68, 0.44, 0.7, 'z', 8, false); cy(g, 0.045, 0.05, M.red, -0.52, 0.44, 0.7, 'z', 8, false);
        return function (t, busy) {
          T.belt.offset.x = -t * 0.7;
          pul.rotation.z = -t * 4;
          l.set(0.6 + 0.4 * busy);
        };
      },

      /* 20. the trays: a turning table that hands each sheet to who does the next step */
      function (g, T) {
        bedOf(g, T);
        cy(g, 0.1, 0.62, M.steel, 0, 0.59, -0.66, 'y', 8);
        cy(g, 0.26, 0.08, M.enamel2, 0, 0.32, -0.66, 'y', 10, false);
        var tb = grp(g, 0, 0.92, -0.66);
        cy(tb, 0.62, 0.05, M.enamel2, 0, 0, 0, 'y', 14); cy(tb, 0.1, 0.14, M.brass, 0, 0.07, 0, 'y', 8, false);
        var kinds = [M.enamel, M.ivory, T.mat];
        [0, 1, 2].forEach(function (k) {
          var a = grp(tb, 0, 0, 0); a.rotation.y = k * TAU / 3;
          bx(a, 0.46, 0.12, 0.4, kinds[k], 0.34, 0.085, 0);
          bx(a, 0.38, 0.02, 0.32, M.iron, 0.34, 0.15, 0, false);
          var s = bx(a, 0.3, 0.02 + k * 0.012, 0.24, M.paper, 0.34, 0.165 + k * 0.006, 0, false); s.rotation.y = 0.2 - k * 0.2;
        });
        var pd = grp(g, 0, 0.64, 0.56);
        cy(pd, 0.06, 0.2, M.brass, 0, 0.04, 0, 'y', 8, false); bx(pd, 0.05, 0.12, 0.86, M.steel, 0, 0.07, -0.43, false);
        var l = lamp(g, T.tone, 0.07, 0.7, 0.4, -0.5, 1.0);
        return function (t, busy) {
          var u = t * 0.4;
          tb.rotation.y = (Math.floor(u) + smooth((u % 1) * 3)) * TAU / 3;
          pd.rotation.y = Math.sin(t * 1.6) * 0.35;
          l.set(0.5 + 0.5 * busy);
        };
      },

      /* 21. the stamp: a press that marks each step on the sheet as it passes */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.9, 0.2, 0.46, M.enamel2, 0, 0.38, -0.62);
        bx(g, 0.56, 1.1, 0.34, M.enamel, 0, 1.0, -0.64);
        bx(g, 0.56, 0.34, 1.0, M.enamel, 0, 1.5, -0.3);
        bx(g, 0.03, 0.05, 1.0, T.lit, 0.285, 1.44, -0.3, false); bx(g, 0.03, 0.05, 1.0, T.lit, -0.285, 1.44, -0.3, false);
        bx(g, 0.7, 0.1, 1.0, M.iron, 0, 0.39, 0, false);
        cy(g, 0.13, 0.3, M.enamel2, 0, 1.24, 0.04, 'y', 8, false);
        var pl = grp(g, 0, 0, 0.04);
        cy(pl, 0.06, 0.5, M.steel, 0, 1.06, 0, 'y', 6, false);
        bx(pl, 0.5, 0.18, 0.66, M.iron, 0, 0.8, 0);
        bx(pl, 0.46, 0.04, 0.62, T.mat, 0, 0.69, 0, false);
        cy(pl, 0.11, 0.56, M.brass, 0, 0.93, 0, 'x', 8, false);
        /* the wheel on top turns the screw; the long handle brings the stamp down */
        var wh = grp(g, 0, 1.74, 0.04);
        cy(wh, 0.05, 0.16, M.steel, 0, -0.04, 0, 'y', 6, false);
        handwheel(grp(wh, 0, 0.06, 0), 0.28, T.mat);
        var lv = grp(g, 0.3, 1.5, 0);
        bx(lv, 0.05, 0.05, 0.66, M.steel, 0.04, 0, 0.33, false); sp(lv, 0.1, M.red, 0.04, 0, 0.7);
        var l = lamp(g, T.tone, 0.07, 0, 1.6, -0.72, 1.0);
        var flash = glow(T.tone, 1.6, 0); flash.position.set(0, 0.66, 0.04); g.add(flash);
        return function (t, busy) {
          var down = busy > 0.5 ? Math.pow(Math.max(0, Math.sin(t * 4.2)), 6) : Math.pow(Math.max(0, Math.sin(t * 1.3)), 24);
          pl.position.y = -down * 0.105;
          lv.rotation.x = 0.35 + down * 0.5; wh.rotation.y = down * 1.2;
          flash.material.opacity = down * 0.7; l.set(0.4 + 0.6 * down);
        };
      },

      /* 22. the pigeonholes: what was worked out once is kept here, and reread */
      function (g, T) {
        bedOf(g, T);
        bx(g, 0.1, 0.52, 0.1, M.enamel2, -0.86, 0.54, -0.56); bx(g, 0.1, 0.52, 0.1, M.enamel2, 0.86, 0.54, -0.56);
        bx(g, 1.84, 0.84, 0.04, M.enamel2, 0, 1.19, -0.77, false);
        [0.78, 1.05, 1.32, 1.59].forEach(function (y) { bx(g, 1.84, 0.045, 0.46, M.enamel, 0, y, -0.56); });
        [-0.9, -0.45, 0, 0.45, 0.9].forEach(function (x) { bx(g, 0.045, 0.84, 0.46, M.enamel, x, 1.185, -0.56); });
        var on = T.lit, off = mat(0x3c5a34), tags = [];
        for (var r = 0; r < 3; r++) for (var c = 0; c < 4; c++) {
          var x = -0.675 + c * 0.45, y = 0.81 + r * 0.27;
          if ((r * 4 + c) % 3 !== 1) { var s = bx(g, 0.26, 0.05 + ((r + c) % 3) * 0.04, 0.36, (r + c) % 4 === 0 ? M.ivory : M.paper, x, y + 0.05, -0.54, false); s.rotation.y = ((r * 5 + c * 3) % 5 - 2) * 0.08; }
          tags.push(bx(g, 0.2, 0.05, 0.02, off, x, y - 0.012, -0.325, false));
        }
        bridge(g, 0, M.steel);
        var l = lamp(g, T.tone, 0.06, 0, 0.86, 0, 0.8);
        var last = -1;
        return function (t, busy) {
          var n = Math.floor(t * (1.2 + busy * 2));
          l.set(0.3 + 0.7 * busy);
          if (n === last) return;
          last = n;
          for (var i = 0; i < tags.length; i++) tags[i].material = (i === (n * 5) % 12 || i === (n * 7 + 3) % 12) ? on : off;
        };
      },

      /* 23. the meter: wheels of figures that turn with every sheet, what a case costs to run */
      function (g, T) {
        bedOf(g, T);
        bx(g, 1.2, 0.8, 0.56, M.enamel, 0, 0.68, -0.54);
        bx(g, 1.2, 0.44, 0.42, M.enamel2, 0, 1.3, -0.61);
        bx(g, 0.96, 0.34, 0.03, M.bake, 0, 1.33, -0.395, false);
        var wheels = [-0.3, -0.1, 0.1, 0.3].map(function (x) {
          var w = grp(g, x, 1.33, -0.47);
          var m = new THREE.Mesh(new THREE.CylinderGeometry(0.15, 0.15, 0.17, 20, 1, true), T.digits);
          m.rotation.z = Math.PI / 2; m.scale.y = T.flip; w.add(m);
          return w;
        });
        bx(g, 1.0, 0.04, 0.03, T.lit, 0, 1.13, -0.39, false);
        var n = dial(g, 0.25, -0.26, 0.72, -0.255);
        cy(g, 0.05, 0.12, M.brass, 0.3, 0.86, -0.25, 'z', 8, false); cy(g, 0.05, 0.12, M.brass, 0.3, 0.6, -0.25, 'z', 8, false);
        var arm = grp(g, 0.62, 0.8, -0.54);
        cy(arm, 0.06, 0.1, M.steel, 0, 0, 0, 'x', 8, false); bx(arm, 0.04, 0.34, 0.05, M.steel, 0.05, 0.15, 0, false); sp(arm, 0.07, M.bake, 0.09, 0.32, 0, false);
        var bell = new THREE.Mesh(new THREE.SphereGeometry(0.17, 10, 6, 0, TAU, 0, Math.PI / 2), M.brass); bell.position.set(0.36, 1.52, -0.61); bell.castShadow = true; g.add(bell);
        var slip = bx(g, 0.22, 0.2, 0.012, M.paper, -0.34, 1.58, -0.61, false);
        var l = lamp(g, T.tone, 0.06, -0.52, 1.18, -0.28, 0.8);
        bx(g, 0.04, 0.04, 0.52, M.steel, -0.3, 0.68, -0.14, false); bx(g, 0.04, 0.04, 0.52, M.steel, 0.3, 0.68, -0.14, false);
        var count = grp(g, 0, 0.66, 0.08);
        cy(count, 0.08, 0.66, M.brass, 0, 0, 0, 'z', 8, false); bx(count, 0.03, 0.17, 0.7, M.iron, 0, 0, 0, false);
        var spin = 0;
        return function (t, busy, dt) {
          spin += dt * (0.6 + busy * 5);
          [0.01, 0.1, 1, 0.33].forEach(function (k, i) { wheels[i].rotation.x = -(i === 3 ? spin * 3 : Math.floor(spin * k * 3) * TAU / 10 + smooth((spin * k * 3 % 1) * 4) * TAU / 10); });
          n.rotation.z = 0.5 - busy * 0.9 + Math.sin(t * 3) * 0.05;
          arm.rotation.x = -busy * 0.7; count.rotation.z = -t * 3;
          slip.position.y = 1.56 + busy * 0.08; l.set(0.4 + 0.6 * busy);
        };
      },

      /* 24. the way out: the sheet goes in, a slip comes up with its check, and the lamp turns green */
      function (g, T, rec) {
        bedOf(g, T, true);
        bx(g, 2.2, 0.04, 0.05, M.steel, -1.0, 0.5, 0.48, false); bx(g, 2.2, 0.04, 0.05, M.steel, -1.0, 0.5, -0.48, false);
        bx(g, 1.5, 0.66, 1.24, M.enamel2, 0.1, 0.61, 0);
        bx(g, 0.04, 0.2, 0.96, M.bake, -0.655, 0.56, 0, false);
        bx(g, 1.3, 0.46, 0.84, M.enamel, 0.1, 1.17, -0.18);
        bx(g, 1.3, 0.05, 0.03, T.lit, 0.1, 0.96, 0.245, false);
        var sl = grp(g, 0.1, 0.94, 0.62); sl.rotation.x = -0.64;
        bx(sl, 1.3, 0.62, 0.05, M.enamel, 0, 0.31, -0.03);
        var on = T.lit, off = mat(0x3c5a34), keys = [];
        for (var i = 0; i < 5; i++) { cy(sl, 0.075, 0.03, M.iron, -0.44 + i * 0.22, 0.3, 0.005, 'z', 8, false); keys.push(cy(sl, 0.055, 0.05, off, -0.44 + i * 0.22, 0.3, 0.01, 'z', 8, false)); }
        bx(g, 0.62, 0.04, 0.1, M.bake, 0.1, 1.405, -0.1, false);
        var slip = new THREE.Mesh(UPLANE, new THREE.MeshBasicMaterial({map: slipTex, side: THREE.DoubleSide}));
        slip.scale.set(0.5 * T.flip, 0.66, 1); slip.rotation.x = -0.2; slip.position.set(0.1, 1.1, -0.1); g.add(slip);
        cy(g, 0.04, 0.4, M.steel, -0.38, 1.6, -0.16, 'y', 6, false);
        var amber = col(0xffb43a), green = col(0x4dff8a);
        var beacon = lamp(g, 0xffb43a, 0.15, -0.38, 1.86, -0.16, 1.7);
        var bell = new THREE.Mesh(new THREE.SphereGeometry(0.18, 10, 6, 0, TAU, 0, Math.PI / 2), M.brass); bell.position.set(0.56, 1.4, -0.42); bell.castShadow = true; g.add(bell);
        /* the tray of answers, each with its check, at the very end of the line */
        bx(g, 0.86, 0.05, 0.9, M.enamel, 1.28, 0.34, 0); bx(g, 0.05, 0.22, 0.9, M.enamel, 1.71, 0.42, 0);
        bx(g, 0.86, 0.18, 0.05, M.enamel, 1.28, 0.4, 0.45, false); bx(g, 0.86, 0.18, 0.05, M.enamel, 1.28, 0.4, -0.45, false);
        var pile = [];
        for (var k = 0; k < 3; k++) {
          var p = new THREE.Mesh(UPLANE, new THREE.MeshBasicMaterial({map: slipTex}));
          p.scale.set(0.5 * T.flip, 0.66, 1); p.rotation.x = -Math.PI / 2; p.rotation.z = (k - 1) * 0.22; p.position.set(1.26 + (k - 1) * 0.05, 0.375 + k * 0.012, 0.02); p.visible = false;
          g.add(p); pile.push(p);
        }
        var since = 99, done = 0;
        rec.fire = function () { since = 0; done++; };
        rec.rest = function (full) { since = full ? 1.2 : 99; done = full ? 3 : 0; };
        return function (t, busy, dt) {
          since += dt;
          var up = since < 3 ? backOut(since / 0.5) : 1 - smooth((since - 3) / 0.4), ok = since < 3.2;
          slip.position.y = 1.1 + clamp(up) * 0.62 + (up > 1 ? (up - 1) * 0.3 : 0);
          beacon.hue(ok ? green : amber); beacon.set(ok ? 1 : 0.35 + 0.2 * Math.sin(t * 2));
          for (var i = 0; i < 5; i++) keys[i].material = (ok ? i <= Math.min(4, Math.floor(since * 4)) : i === Math.floor(t * 2) % 5) ? on : off;
          for (var k = 0; k < 3; k++) pile[k].visible = done > k;
        };
      }
    ];

    /* the textures the parts share or scroll, one set per week tone where the tone shows */
    function kitOf(w) {
      var tone = w.tone, t = hex(tone);
      var kit = {tone: tone, lit: lit(tone), mat: mat(tone, {glow: 0.55}), flip: w.dir,
                 page0: new THREE.MeshBasicMaterial({map: pages[0], color: col(0xdedad0)})};
      kit.card = new THREE.MeshBasicMaterial({map: drawn(64, 128, function (g, cw, ch) {
        g.fillStyle = '#f3ead2'; g.fillRect(0, 0, cw, ch);
        g.fillStyle = t; g.fillRect(0, 0, 7, ch);
        g.fillStyle = '#2a2230';
        for (var c = 0; c < 5; c++) for (var r = 0; r < 14; r++) if ((c * 7 + r * 5 + c * r) % 3 === 0) g.fillRect(14 + c * 9.5, 8 + r * 8.4, 5, 3.4);
      }), color: col(0xe4e0d6)});
      kit.jacks = new THREE.MeshBasicMaterial({map: drawn(256, 148, function (g, cw, ch) {
        g.fillStyle = '#211b22'; g.fillRect(0, 0, cw, ch);
        for (var i = 0; i < 6; i++) for (var j = 0; j < 4; j++) {
          var x = 128 + (-0.675 + i * 0.27) / 1.66 * cw, y = ch - (0.14 + j * 0.24 - 0.02) / 0.96 * ch;
          g.fillStyle = '#c9a046'; g.beginPath(); g.arc(x, y, 9, 0, TAU); g.fill();
          g.fillStyle = '#0c0a0e'; g.beginPath(); g.arc(x, y, 4.5, 0, TAU); g.fill();
        }
      })});
      kit.scope = drawn(128, 128, function (g) {
        g.fillStyle = '#10151c'; g.fillRect(0, 0, 128, 128);
        g.strokeStyle = 'rgba(190,200,255,.18)'; g.lineWidth = 1;
        for (var i = 16; i < 128; i += 16) { g.beginPath(); g.moveTo(i, 0); g.lineTo(i, 128); g.moveTo(0, i); g.lineTo(128, i); g.stroke(); }
        g.strokeStyle = t; g.lineWidth = 4; g.lineJoin = 'round'; g.beginPath();
        for (var x = 0; x <= 128; x += 2) { var y = 64 - Math.sin(x / 128 * TAU * 2) * 26 - Math.sin(x / 128 * TAU * 5) * 9; if (x) g.lineTo(x, y); else g.moveTo(x, y); }
        g.stroke();
      });
      kit.scope.wrapS = THREE.RepeatWrapping;
      kit.chart = drawn(128, 128, function (g) {
        g.fillStyle = '#f6f0de'; g.fillRect(0, 0, 128, 128);
        g.strokeStyle = 'rgba(80,70,110,.22)'; g.lineWidth = 1;
        for (var i = 0; i < 128; i += 16) { g.beginPath(); g.moveTo(i, 0); g.lineTo(i, 128); g.moveTo(0, i); g.lineTo(128, i); g.stroke(); }
        g.strokeStyle = t; g.lineWidth = 3.5; g.lineJoin = 'round'; g.beginPath();
        for (var y = 0; y <= 128; y += 2) { var x = 64 + Math.sin(y / 128 * TAU * 2) * 20 + Math.sin(y / 128 * TAU * 7) * 9; if (y) g.lineTo(x, y); else g.moveTo(x, y); }
        g.stroke();
      });
      kit.chart.wrapT = THREE.RepeatWrapping;
      kit.belt = drawn(64, 64, function (g) {
        g.fillStyle = '#23252b'; g.fillRect(0, 0, 64, 64);
        g.strokeStyle = t; g.lineWidth = 5; g.globalAlpha = 0.85;
        g.beginPath(); g.moveTo(14, 8); g.lineTo(38, 32); g.lineTo(14, 56); g.stroke();
      });
      kit.belt.wrapS = kit.belt.wrapT = THREE.RepeatWrapping; kit.belt.repeat.set(6, 1);
      kit.digits = new THREE.MeshBasicMaterial({map: drawn(320, 32, function (g) {
        g.fillStyle = '#f4ecd6'; g.fillRect(0, 0, 320, 32);
        g.fillStyle = '#1c1820'; g.font = '700 24px "JetBrains Mono", monospace'; g.textAlign = 'center'; g.textBaseline = 'middle';
        for (var d = 0; d < 10; d++) { g.save(); g.translate(16 + d * 32, 16); g.rotate(Math.PI / 2); g.fillText(String(d), 0, 2); g.restore(); }
      })});
      return kit;
    }

    /* the slide that takes the paper down to the next row, built with the first part of that row */
    function chuteOf(rec, kit) {
      var g = new THREE.Group(), sg = rec.arc, n = 9, o = {};
      var wallM = mat(rec.week.tone, {glow: 0.4});
      for (var i = 0; i < n; i++) {
        pathAt(sg.s0 + sg.len * (i + 0.5) / n, o);
        var p = grp(g, o.x, o.y, o.z); p.rotation.order = 'YZX'; p.rotation.set(0, o.yaw, o.pitch);
        bx(p, sg.len / n + 0.16, 0.04, 1.0, M.steel, 0, -0.05, 0);
        bx(p, sg.len / n + 0.3, 0.2, 0.05, wallM, 0, 0.04, -0.52, false);
        bx(p, sg.len / n * 0.62, 0.2, 0.05, wallM, 0, 0.04, 0.52, false);
        if (i % 2 === 1) {
          var floorY = o.z < sg.z0 + TURN ? sg.y0 - PY : sg.y1 - PY, hgt = o.y - 0.07 - floorY;
          bx(g, 0.08, hgt, 0.08, M.iron, o.x, floorY + hgt / 2, o.z, false);
        }
      }
      return g;
    }

    var live = new THREE.Group();
    scene.add(live);
    /* the furniture and the plinths never move either: merge them before the parts come.
       A cloth and a plate stay loose, the one is pulled away, the other rattles on a refusal. */
    order.forEach(stand);
    bake(bench, null, order.reduce(function (a, rec) { a.push(rec.cloth, rec.plateG); return a; }, []));
    order.forEach(function (rec) {
      var kit = rec.week.kit || (rec.week.kit = kitOf(rec.week));
      var part = grp(bench, rec.x, rec.y, rec.z), inner = grp(part, 0, 0, 0);
      inner.scale.x = rec.dir;
      part.visible = false;
      rec.part = part;
      rec.anim = PARTS[rec.seq % PARTS.length](inner, kit, rec);
      if (rec.arc) { rec.extra = chuteOf(rec, kit); bake(rec.extra); rec.extra.visible = false; bench.add(rec.extra); }
      /* play the part for a while, busy and idle, with its one-off gestures, and merge what stayed still */
      bake(inner, function (check) {
        for (var k = 0; k < 48; k++) {
          if (k === 12 && rec.fire) rec.fire();
          if (k === 30 && rec.rest) rec.rest(true);
          if (k === 38 && rec.rest) rec.rest(false);
          rec.anim(k * 0.37, k % 12 < 7 ? 1 : 0, 0.2);
          check();
        }
      });
    });

    /* where the machine ends for now: a catch tray, and a small red disc */
    var endG = new THREE.Group();
    (function () {
      var f = bx(endG, 0.78, 0.03, 0.96, M.steel, 0.34, -0.26, 0); f.rotation.z = -0.55;
      var s1 = bx(endG, 0.78, 0.2, 0.03, M.steel, 0.34, -0.2, 0.48, false); s1.rotation.z = -0.55;
      var s2 = bx(endG, 0.78, 0.2, 0.03, M.steel, 0.34, -0.2, -0.48, false); s2.rotation.z = -0.55;
      bx(endG, 0.04, 0.34, 0.96, M.steel, 0.68, -0.36, 0, false);
      var p1 = bx(endG, 0.7, 0.02, 0.8, M.paper, 0.36, -0.235, 0.02, false); p1.rotation.z = -0.55; p1.rotation.y = 0.05;
      cy(endG, 0.025, 0.5, M.iron, 0.72, -0.1, 0.5, 'y', 6, false);
      cy(endG, 0.13, 0.03, M.red, 0.72, 0.2, 0.5, 'x', 10, false);
      bake(endG); endG.visible = false; bench.add(endG);
    })();

    /* --- the paper --------------------------------------------------------------- */

    var sheetGeo = new THREE.PlaneGeometry(1.08, 0.78);
    sheetGeo.rotateX(-Math.PI / 2);
    var sheets = [];
    for (var si = 0; si < 14; si++) {
      var sm = new THREE.MeshBasicMaterial({map: pages[0], color: col(0xe9e6dc), transparent: true, side: THREE.DoubleSide});
      var sh = new THREE.Mesh(sheetGeo, sm);
      sh.rotation.order = 'YZX'; sh.visible = false;
      live.add(sh);
      sheets.push({mesh: sh, mat: sm, s: 0, on: false, age: 0});
    }
    var S0 = 1.25;            /* a sheet appears under the cover of the feeder */
    var reach = -1, endS = 0, spawnT = 0;
    var spot = {};
    function putSheet(sh) {
      var m = sh.mesh, o;
      if (sh.s <= endS) {
        o = pathAt(sh.s, spot);
        m.position.set(o.x, o.y, o.z); m.rotation.set(0, o.yaw, o.pitch);
        sh.mat.opacity = clamp(sh.age * 3);
        var stage = Math.min(4, Math.floor((o.rec.seq + 3) / 6));
        if (sh.mat.map !== pages[stage]) sh.mat.map = pages[stage];
      } else {
        /* past the last part: tip into the catch tray and go */
        var u = (sh.s - endS) / 0.9;
        if (u >= 1 || reach >= N) { sh.on = false; m.visible = false; return; }
        o = pathAt(endS - 0.001, spot);
        var c = Math.cos(o.yaw), s = Math.sin(o.yaw), f = u * 0.42;
        m.position.set(o.x + c * f, o.y - u * 0.3, o.z - s * f);
        m.rotation.set(0, o.yaw, -0.55 * clamp(u * 1.6));
        sh.mat.opacity = 1 - clamp((u - 0.6) / 0.4);
      }
      m.visible = true;
    }
    function spawn(s) {
      for (var i = 0; i < sheets.length; i++) {
        if (!sheets[i].on) { sheets[i].on = true; sheets[i].s = s; sheets[i].age = s > S0 ? 1 : 0; putSheet(sheets[i]); return; }
      }
    }
    /* lay sheets all along the machine at once: at the start, and when nothing moves */
    function seedSheets() {
      sheets.forEach(function (sh) { sh.on = false; sh.mesh.visible = false; });
      if (reach < 1) return;
      for (var s = reduced ? S0 + 1.2 : S0 + 0.9; s < endS - 0.3; s += GAP) spawn(s);
      spawnT = 0.5;
    }
    function setReach(k) {
      var first = reach < 0;
      reach = k;
      endS = k < 1 ? 0 : k < N ? order[k - 1].sOut : order[N - 1].sIn + PITCH * 0.42;
      endG.visible = k > 0 && k < N;
      if (endG.visible) {
        var o = pathAt(order[k - 1].sOut - 0.001, spot);
        endG.position.set(o.x, o.y, o.z); endG.rotation.y = o.yaw;
      }
      var last = order[N - 1];
      if (last.rest) last.rest(k >= N && (reduced || first));
      if (first || reduced) seedSheets();
    }

    /* --- states --------------------------------------------------------------------- */

    function dress(rec) {
      var st = rec.state, tone = rec.week.toneC || (rec.week.toneC = col(rec.week.tone));
      rec.drawPlate(st);
      rec.pilot.hue(st === 'open' ? tone : AMBER);
      rec.pilot.set(st === 'locked' ? 0 : 1);
      rec.under.material.opacity = st === 'ready' ? 0.5 : 0;
    }
    var AMBER = col(0xffc45e);
    function show(rec) {
      var e = rec.inst, open = rec.state === 'open';
      var lift = open ? smooth(e / 0.6) : 0;
      rec.cloth.visible = rec.state === 'ready' || (open && e < 0.6);
      rec.cloth.position.set(lift * 0.5, 0.14 + lift * 2.0, -lift * 0.3);
      rec.cloth.rotation.z = -lift * 0.7;
      rec.cloth.scale.setScalar(1 - lift * 0.9);
      var p = open ? clamp((e - 0.18) / 0.82) : 0, b = backOut(p);
      rec.part.visible = open && e > 0.18;
      rec.part.scale.setScalar(0.62 + 0.38 * b);
      rec.part.position.y = rec.y + (1 - smooth(p * 1.4)) * 0.55;
      if (rec.extra) { rec.extra.visible = rec.part.visible; rec.extra.position.y = (1 - smooth(p * 1.4)) * 0.6; }
    }
    function paint() {
      order.forEach(function (rec) {
        var st = page.state(rec.day);
        if (st === rec.state) return;
        var first = !rec.state;
        rec.state = st;
        rec.instGoal = st === 'open' ? 1 : 0;
        if (first || reduced || st !== 'open') rec.inst = rec.instGoal;
        dress(rec); show(rec);
      });
      shadowDue = true;
      if (reach < 0 || reduced) measure();
    }
    /* how far the paper can go: the parts installed without a gap, from the first */
    function measure() {
      var k = 0;
      while (k < N && order[k].state === 'open' && order[k].inst >= 0.999) k++;
      if (k !== reach) setReach(k);
    }

    /* --- the camera: a point looked at, an angle around it, a distance -------------- */

    var cam = {t: new THREE.Vector3(), az: Math.PI / 2, pol: 0.83, dist: 30};
    var goal = {t: new THREE.Vector3(), az: Math.PI / 2, pol: 0.83, dist: 30};
    var base = {az: Math.PI / 2, pol: 0.83};
    var mode = {kind: 'over', week: -1, day: 0};
    var narrow = false, lastTouch = 0;

    /* far enough to hold `half` on each side of the point looked at, whatever the window */
    function distFor(half, lead) {
      var t = Math.tan(camera.fov * Math.PI / 360);
      return half / (t * camera.aspect) + lead;
    }
    function fit() {
      var w = host.clientWidth, h = host.clientHeight;
      if (!w || !h) return;
      renderer.setSize(w, h, false);
      camera.aspect = w / h;
      narrow = w / h < 0.9;
      camera.fov = narrow ? 48 : w / h < 1.4 ? 42 : 36;
      camera.updateProjectionMatrix();
      refly(true);
    }
    function fly(to, instant) {
      goal.t.set(to.t[0], to.t[1], to.t[2]);
      goal.az = to.az; goal.pol = to.pol; goal.dist = to.dist;
      base.az = to.az; base.pol = to.pol;
      if (instant || reduced) { cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist; }
    }
    function overShot() {
      var mid = (weeks[0].y + 1.4) / 2;
      if (narrow) return {t: [0, mid - 0.2, 0.4], az: Math.PI / 2, pol: 0.6, dist: distFor(HALF + TURN + 0.9, 4.6)};
      return {t: [(BX0 + TX1) / 2, mid + 0.9, 0.2], az: Math.PI / 2, pol: 0.74, dist: Math.max(distFor((TX1 - BX0) / 2 + 0.6, 4.2), 27)};
    }
    function weekShot(i) {
      var w = weeks[i];
      /* a narrow window looks along the row, from the end the paper comes in */
      if (narrow) return {t: [w.dir * -1.2, w.y + 0.5, w.z + 0.2], az: Math.PI / 2 + w.dir * 0.95, pol: 0.92, dist: 17};
      return {t: [0, w.y + 0.5, w.z + 0.5], az: Math.PI / 2, pol: 0.8, dist: distFor(HALF + TURN + 1.2, 2.2)};
    }
    function dayShot(day) {
      var rec = byDay[day];
      return {t: [rec.x, rec.y + 0.7, rec.z + 0.1], az: Math.PI / 2 - 0.3, pol: 1.0, dist: narrow ? 9.5 : 6.8};
    }
    function refly(instant) {
      if (mode.kind === 'over') fly(overShot(), instant);
      else if (mode.kind === 'village') fly(weekShot(mode.week), instant);
      else fly(dayShot(mode.day), instant);
    }
    function setMode(kind, week, day) {
      mode = {kind: kind, week: week, day: day};
      host.setAttribute('data-mode', kind);
      refly(false);
      chips();
    }
    function overview() { setMode('over', -1, 0); }
    function toWeek(i) { if (weeks[i]) setMode('village', i, 0); }
    function toDay(day) { if (byDay[day]) setMode('house', byDay[day].week.r, day); }

    /* a click on a day: go to its place, then install its part if it may */
    var pending = 0, entering = 0;
    function knock(day) {
      var rec = byDay[day];
      if (!rec) return;
      toDay(day);
      clearTimeout(pending); clearTimeout(entering);
      pending = setTimeout(function () {
        var st = page.state(day);
        if (st === 'locked') { rec.shake = 1; page.refuse(day); return; }
        if (st === 'ready') page.mark(day);
        paint();
        entering = setTimeout(function () { page.enter(day); }, reduced ? 0 : st === 'ready' ? 2400 : 1100);
      }, reduced ? 0 : 900);
    }

    /* --- the pointer ---------------------------------------------------------------- */

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
      lastTouch = performance.now();
    });
    canvas.addEventListener('pointermove', function (e) {
      if (drag) {
        var dx = e.clientX - drag.x, dy = e.clientY - drag.y;
        drag.moved = Math.max(drag.moved, Math.abs(dx) + Math.abs(dy));
        /* a vertical swipe on a phone scrolls the page; a horizontal one turns the view */
        if (drag.touch && !drag.held && Math.abs(dy) > Math.abs(dx) * 1.3) return;
        if (drag.moved > 6) {
          if (!drag.held) { drag.held = true; try { canvas.setPointerCapture(drag.id); } catch (err) {} }
          /* the room has one good side: the view turns a little, never round the back */
          goal.az = Math.max(base.az - 0.62, Math.min(base.az + 0.62, drag.az + dx * 0.004));
          goal.pol = Math.max(0.42, Math.min(1.22, drag.pol - dy * 0.003));
          canvas.style.cursor = 'grabbing';
        }
        lastTouch = performance.now();
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

    /* --- the chips and the names of the weeks ----------------------------------------- */

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
      return b;
    }
    chip(page.tx(SAY.over), '', overview);
    weeks.forEach(function (w) { chip(page.tx(w.def.name), w.def.tone, function () { toWeek(w.r); }); });
    function chips() {
      chipEls.forEach(function (b, i) {
        var on = i === 0 ? mode.kind === 'over' : mode.week === i - 1;
        b.setAttribute('aria-pressed', on ? 'true' : 'false');
      });
    }
    var hint = document.createElement('p');
    hint.className = 'monde-hint';
    hint.textContent = page.tx(SAY.hint);
    bar.insertBefore(hint, bar.firstChild);

    var labels = weeks.map(function (w) {
      var el = document.createElement('button');
      el.type = 'button'; el.className = 'monde-label';
      el.style.setProperty('--tone', w.def.tone);
      var small = document.createElement('small');
      small.textContent = page.tx(SAY.days) + ' ' + w.days[0] + ' · ' + w.days[w.days.length - 1];
      var strong = document.createElement('b'); strong.textContent = page.tx(w.def.name);
      /* the name sits over the rows behind: a soft dark cushion keeps it readable there */
      el.style.padding = '12px 48px 16px';
      el.style.background = 'radial-gradient(closest-side, rgba(9,11,26,.78), rgba(9,11,26,.5) 60%, rgba(9,11,26,0))';
      strong.style.textShadow = '0 2px 12px rgba(9,11,26,.95)';
      el.appendChild(small); el.appendChild(strong);
      el.addEventListener('click', function () { toWeek(w.r); });
      host.appendChild(el);
      return {el: el, week: w};
    });

    /* --- the loop ---------------------------------------------------------------------- */

    var running = false, visible = true, paused = false, raf = 0, last = 0, clock = 0, shadowDue = true, shadowAge = 0;
    function step(dt) {
      var k = 1 - Math.exp(-dt * 3.4);
      /* at rest, the wide view breathes a little from side to side */
      var sway = mode.kind === 'over' && !drag && !reduced && performance.now() - lastTouch > 2500 ? Math.sin(clock * 0.16) * 0.045 : 0;
      cam.t.lerp(goal.t, k);
      cam.az += (goal.az + sway - cam.az) * k; cam.pol += (goal.pol - cam.pol) * k; cam.dist += (goal.dist - cam.dist) * k;
      var spn = Math.sin(cam.pol);
      camera.position.set(cam.t.x + cam.dist * spn * Math.cos(cam.az), cam.t.y + cam.dist * Math.cos(cam.pol),
                          cam.t.z + cam.dist * spn * Math.sin(cam.az));
      camera.lookAt(cam.t);

      var i, rec, moving = false;
      for (i = 0; i < N; i++) { order[i].busyT = 0; }
      /* the paper */
      if (!reduced) {
        if (reach > 0) { spawnT -= dt; if (spawnT <= 0) { spawn(S0); spawnT = GAP / SPEED; } }
        for (i = 0; i < sheets.length; i++) {
          var sh = sheets[i];
          if (!sh.on) continue;
          var before = sh.s;
          sh.s += SPEED * dt; sh.age += dt;
          if (reach >= N && before <= endS && sh.s > endS && order[N - 1].fire) order[N - 1].fire();
          putSheet(sh);
          if (sh.on && sh.s <= endS) { rec = pathAt(sh.s, spot).rec; if (sh.s >= rec.sIn - 0.2) rec.busyT = 1; }
        }
      }
      var plateK = mode.kind === 'over' ? (narrow ? 1.5 : 1.12) : 1;
      for (i = 0; i < N; i++) {
        rec = order[i];
        if (rec.inst !== rec.instGoal) {
          rec.inst = Math.min(rec.instGoal, rec.inst + dt / 1.5);
          if (rec.inst > rec.instGoal) rec.inst = rec.instGoal;
          show(rec); moving = true; shadowDue = true;
        }
        if (rec.shake > 0) {
          rec.shake = Math.max(0, rec.shake - dt * 1.7);
          rec.stand.rotation.z = Math.sin(rec.shake * 34) * 0.05 * rec.shake;
          rec.plateG.position.y = 0.33 + Math.abs(Math.sin(rec.shake * 34)) * 0.06 * rec.shake;
        }
        var s = plateK * (hoverDay === rec.day ? 1.14 : 1);
        rec.plateG.scale.x += (s - rec.plateG.scale.x) * k * 2; rec.plateG.scale.y = rec.plateG.scale.z = rec.plateG.scale.x;
        if (rec.state === 'ready') {
          var pulse = 0.5 + 0.5 * Math.sin(clock * 2.6 + rec.seq);
          rec.pilot.set(0.55 + 0.45 * pulse);
          rec.under.material.opacity = (0.34 + 0.3 * pulse) * (hoverDay === rec.day ? 1.5 : 1);
        } else if (rec.state === 'open' && rec.part.visible) {
          /* a part the paper does not reach yet still turns over on its own */
          var want = i < reach ? rec.busyT : (Math.sin(clock * 1.1 + rec.seq * 1.7) > 0.55 ? 1 : 0);
          rec.busy += (want - rec.busy) * Math.min(1, dt * 9);
          rec.anim(clock, rec.busy, reduced ? 0 : dt);
        }
      }
      if (!moving) measure();

      if (flakes) {
        var p = flakes.geometry.attributes.position, v = flakes.userData.v;
        for (i = 0; i < v.length; i++) {
          var y = p.array[i * 3 + 1] - v[i] * dt;
          p.array[i * 3] += Math.sin(clock * 0.6 + i) * dt * 0.25;
          p.array[i * 3 + 1] = y < flakes.userData.y0 ? flakes.userData.y1 : y;
        }
        p.needsUpdate = true;
      }
      for (i = 0; i < twinkles.length; i++) twinkles[i].l.set(0.72 + 0.28 * Math.sin(clock * 1.7 + twinkles[i].k * 2.1));
    }
    function frame(now) {
      raf = 0;
      if (!running) return;
      var dt = Math.min(0.05, (now - last) / 1000 || 0.016);
      last = now;
      if (!reduced) clock += dt;
      step(dt);
      /* the shadows are drawn again when a part goes in, and otherwise a few times a second */
      if (shadowDue || ++shadowAge > 9) { renderer.shadowMap.needsUpdate = true; shadowDue = false; shadowAge = 0; }
      renderer.render(scene, camera);
      place();
      raf = requestAnimationFrame(frame);
    }
    function place() {
      var w = host.clientWidth;
      labels.forEach(function (l) {
        var showIt = mode.kind === 'village' && mode.week === l.week.r;
        l.el.classList.toggle('off', !showIt);
        if (showIt) l.el.style.transform = 'translate(-50%, -100%) translate(' + (w / 2).toFixed(1) + 'px,86px)';
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
    if (document.fonts && document.fonts.ready) {
      document.fonts.ready.then(function () { order.forEach(function (rec) { rec.drawPlate(rec.state); }); });
    }

    host.setAttribute('data-mode', 'over');
    paint();
    fit();
    chips();
    canvas.style.cursor = 'grab';
    sync();

    return {
      paint: paint,
      pause: function (on) { paused = on; sync(); },
      /* the page closed what the day showed: step back to the row of that day */
      back: function () { if (mode.kind === 'house') toWeek(mode.week); },
      overview: overview, week: toWeek, knock: knock,
      views: function () { return ['machine'].concat(weeks.map(function (w, i) { return 'semaine' + (i + 1); })); },
      view: function (name) { if (name === 'machine') overview(); else toWeek(Number(String(name).replace(/\D/g, '')) - 1); },
      look: toDay,
      /* for the captures: the camera where it is going, every part in place, then one clean frame */
      settle: function () {
        cam.t.copy(goal.t); cam.az = goal.az; cam.pol = goal.pol; cam.dist = goal.dist;
        order.forEach(function (rec) { rec.inst = rec.instGoal; rec.shake = 0; rec.stand.rotation.z = 0; show(rec); });
        measure();
        shadowDue = true;
        scene.updateMatrixWorld(true);
      }
    };
  }

  global.SceneMachine = {mount: mount};
})(window);
