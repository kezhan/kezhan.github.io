/* The four parts of the tree village, each its own kind of place (scene_arbre.js builds the tree and the doors).
 *
 *   Words          the roots: burrow homes round a post box, letters flying in (requests written down, word for word)
 *   Proof          the workshop bough: a forge, a mill wheel, a glasshouse lab, a clock tower, a weather hut, a crane
 *                  and a balance (run it, measure it, check it)
 *   Memory         the library in the trunk: doors on the facets of a drum of bark, bookshelves, card drawers
 *                  (write it once, keep it)
 *   Orchestration  the canopy: lookouts on their branches, rope bridges to the crow's nest and its bell, birds
 *                  carrying the messages (many sessions, one coordinator)
 *
 * `make(T)` takes the scene's helpers and returns one builder per part. A builder returns six spots, one per day:
 * {host, door, wins, hit, tagDown}: `host` a group whose local +z is the front of the house, `door` the local centre
 * of the door, `wins` its windows (local), `hit` the box that takes the click; plus `label` (world) and `tick`.
 */
(function (global) {
  'use strict';

  function make(T) {
    var THREE = T.THREE, V = T.V, TAU = Math.PI * 2, rnd = T.rnd;
    var mat = T.mat, put = T.put, box = T.box, ball = T.ball, grp = T.grp, rod = T.rod, limb = T.limb, halo = T.halo, glowAt = T.glowAt;
    var snowM = T.snowM, ropeM = mat(0xc9b089, {rough: 0.9}), wood = mat(0xb07a4a, {rough: 0.85}), woodD = mat(0x8a5a34, {rough: 0.85});
    var stone = mat(0x8d919c, {rough: 0.9}), bark = T.bark, dark = mat(0x1c2030, {rough: 0.5});
    var brass = mat(0xd9a441, {rough: 0.3, metal: 0.7}), red = mat(0xd2382c, {rough: 0.5});
    var ticks = [];

    function hostAt(x, y, z, f) { var g = T.grp(T.site, x, y, z, Math.atan2(f.x, f.z)); g.updateMatrixWorld(true); return g; }
    /* a pitched roof: a prism along x, its ridge up, snow on both slopes */
    function roof(p, w, dp, rh, y, m) {
      var sh = new THREE.Shape();
      sh.moveTo(-dp / 2 - 0.25, 0); sh.lineTo(dp / 2 + 0.25, 0); sh.lineTo(0, rh); sh.closePath();
      var geo = new THREE.ExtrudeGeometry(sh, {depth: w + 0.4, bevelEnabled: false});
      geo.translate(0, 0, -(w + 0.4) / 2); geo.rotateY(Math.PI / 2);
      put(p, geo, m, 0, y, 0);
      var sl = Math.sqrt(rh * rh + (dp / 2 + 0.25) * (dp / 2 + 0.25)), ang = Math.atan2(rh, dp / 2 + 0.25);
      [-1, 1].forEach(function (s) { box(p, w + 0.42, 0.1, sl * 0.8, snowM, 0, y + rh * 0.55 + 0.06, s * (dp / 4 + 0.06), {rx: s * ang, shadow: false}); });
    }
    function smoke(p, at) {
      var puffs = [0, 1, 2].map(function () { return halo(p, at.x, at.y, at.z, 0xcfd6e6, 0.9); }), ph = rnd();
      ticks.push(function (clock, reduced) {
        puffs.forEach(function (h, k) {
          var u = reduced ? 0.4 : (clock * 0.25 + ph + k / 3) % 1;
          h.position.set(at.x + u * 0.6, at.y + u * 2.2, at.z); h.scale.set(0.6 + u * 1.4, 0.6 + u * 1.4, 1);
          glowAt(h, (1 - u) * 0.35);
        });
      });
    }
    function lantern(p, x, y, z, color) {
      var l = halo(p, x, y, z, color || 0xffc27a, 1.1), ph = rnd() * 6;
      ball(p, 0.09, mat(0xffe0a0, {emissive: 0xffb45a, ei: 0.9}), x, y, z, {shadow: false});
      ticks.push(function (clock, reduced) { glowAt(l, reduced ? 0.7 : 0.55 + 0.25 * Math.sin(clock * 1.9 + ph)); });
    }
    /* a deck of planks, centred at (x, y, z) in p */
    function deck(p, w, d, x, y, z) { box(p, w, 0.16, d, wood, x, y, z); for (var k = -w / 2 + 0.3; k < w / 2; k += 0.5) box(p, 0.04, 0.17, d, woodD, x + k, y + 0.005, z, {shadow: false}); }
    /* a rope bridge from a to b (world), planks on a sagging line, two hand ropes */
    function bridge(a, b) {
      var n = Math.max(6, Math.round(a.distanceTo(b) / 0.55)), prevL = null, prevR = null;
      var dir = b.clone().sub(a), side = V(-dir.z, 0, dir.x).normalize().multiplyScalar(0.45);
      for (var k = 0; k <= n; k++) {
        var u = k / n, p = a.clone().lerp(b, u); p.y -= Math.sin(u * Math.PI) * 0.8;
        var pl = box(T.site, 0.95, 0.07, 0.3, woodD, p.x, p.y, p.z, {shadow: false});
        pl.lookAt(p.clone().add(dir));
        var l = p.clone().add(side).add(V(0, 0.7, 0)), r = p.clone().sub(side).add(V(0, 0.7, 0));
        if (prevL) { rod(T.site, prevL, l, 0.025, ropeM); rod(T.site, prevR, r, 0.025, ropeM); }
        prevL = l; prevR = r;
      }
    }

    /* --- Words: burrow homes in the roots, a post box, the letters flying in to the tree ---------------- */

    function words(wk) {
      var tone = mat(wk.hex, {rough: 0.55}), earth = mat(0x6e4f36, {rough: 0.95}), earthD = mat(0x5a3f2b, {rough: 0.95});
      var A = [26, 52, 79, 103, 129, 154], R = [7.6, 11.2, 7.2, 11.4, 7.5, 10.8];
      var spots = A.map(function (a, k) {
        var ar = a * Math.PI / 180, x = Math.cos(ar) * R[k], z = Math.sin(ar) * R[k];
        var f = V(Math.cos(ar), 0, Math.sin(ar)).add(V(0, 0, 0.9)).normalize();
        var host = hostAt(x, T.groundY(x, z) - 0.15, z, f);
        var s = 1.55 + rnd() * 0.35, sx = 1.2 + rnd() * 0.25;
        /* the mound, its cap of snow, the stone face that holds the door */
        put(host, new THREE.SphereGeometry(s, 18, 9, 0, TAU, 0, Math.PI / 2), k % 2 ? earth : earthD, 0, 0, -0.3).scale.set(sx, 1.05, 1.1);
        put(host, new THREE.SphereGeometry(s * 1.03, 18, 6, 0, TAU, 0, Math.PI / 3.4), snowM, 0, 0.05, -0.3, {shadow: false}).scale.set(sx, 1.05, 1.1);
        var front = Math.sqrt(Math.max(0.3, s * s - Math.pow(0.95 / 1.05, 2))) * 1.1 - 0.3;
        put(host, new THREE.CylinderGeometry(1.0, 1.05, 0.4, 16), stone, 0, 0.95, front, {rx: Math.PI / 2});
        /* round windows on the sides of the mound, where its surface is */
        var wins = [-1, 1].map(function (sg) {
          var wx = sg * 1.25 * sx / 1.25, wy = 0.6;
          var q = s * s - Math.pow(wx / sx, 2) - Math.pow(wy / 1.05, 2);
          return {x: wx, y: wy, z: Math.sqrt(Math.max(0.05, q)) * 1.1 - 0.3 + 0.02, ry: sg * 0.75, round: true};
        });
        /* a pipe that smokes, a letterbox on its post, a lantern, stepping stones */
        put(host, new THREE.CylinderGeometry(0.13, 0.13, 1.0, 8), dark, 0.5 * sx, s * 0.95, -0.6);
        smoke(host, V(0.5 * sx, s * 0.95 + 0.6, -0.6));
        rod(host, V(-1.9, 0, front + 0.5), V(-1.9, 1.0, front + 0.5), 0.05, woodD);
        box(host, 0.45, 0.35, 0.6, tone, -1.9, 1.15, front + 0.5);
        box(host, 0.05, 0.3, 0.12, red, -1.66, 1.35, front + 0.6, {rz: 0.3});
        lantern(host, 1.6, 1.3, front + 0.4);
        rod(host, V(1.6, 0, front + 0.4), V(1.6, 1.2, front + 0.4), 0.04, dark);
        for (var st = 0; st < 3; st++) put(host, new THREE.CylinderGeometry(0.28, 0.3, 0.08, 8), stone, (st % 2 ? 0.2 : -0.2), 0.04, front + 0.9 + st * 0.75);
        return {host: host, door: V(0, 0.95, front + 0.22), wins: wins, hit: {c: V(0, 1.0, 0.2), s: [2.2 * sx + 0.6, 2.4, 2.8]}, tagDown: 0.2};
      });
      /* the post box in front of the tree, and the letters it sends up to the slot in the trunk */
      var pb = V(3.6, T.groundY(3.6, 14.2), 14.2);
      var post = grp(T.site, pb.x, pb.y, pb.z, 0.3);
      put(post, new THREE.CylinderGeometry(0.5, 0.55, 1.4, 14), red, 0, 0.7, 0);
      put(post, new THREE.SphereGeometry(0.52, 14, 6, 0, TAU, 0, Math.PI / 2), red, 0, 1.4, 0);
      box(post, 0.5, 0.06, 0.1, dark, 0, 1.1, 0.5);
      put(post, new THREE.SphereGeometry(0.5, 12, 4, 0, TAU, 0, Math.PI / 3), snowM, 0, 1.62, 0, {shadow: false});
      var slot = V(0, 2.6, T.trunkR(2.6) + 0.2);
      box(T.site, 0.8, 0.12, 0.2, dark, slot.x, slot.y, slot.z - 0.1);
      var envM = mat(0xfaf7ef, {rough: 0.7}), seal = mat(0xd2382c, {rough: 0.5});
      var letters = [];
      for (var e = 0; e < 7; e++) {
        var g = grp(T.site);
        box(g, 0.5, 0.03, 0.34, envM, 0, 0, 0, {shadow: false});
        ball(g, 0.05, seal, 0, 0.03, 0, {shadow: false});
        letters.push({g: g, ph: e / 7});
      }
      var from = pb.clone().add(V(0, 1.5, 0)), mid = V(1.5, 8, 9);
      ticks.push(function (clock, reduced) {
        letters.forEach(function (l, n) {
          var u = reduced ? l.ph : (clock * 0.07 + l.ph) % 1, a = 1 - u;
          l.g.position.copy(from).multiplyScalar(a * a).addScaledVector(mid, 2 * a * u).addScaledVector(slot, u * u);
          l.g.rotation.set(Math.sin(clock * 3 + n) * 0.4, clock * 0.8 + n, Math.cos(clock * 2.6 + n) * 0.3);
          l.g.visible = u < 0.97;
        });
      });
      return {spots: spots, label: V(-8.5, 7, 11)};
    }

    /* --- Proof: the workshop bough ------------------------------------------------------------------ */

    function proof(wk) {
      var tone = mat(wk.hex, {rough: 0.55}), plaster = mat(0xeadcc2, {rough: 0.85}), brick = mat(0xa0563c, {rough: 0.9});
      var glass = new THREE.MeshStandardMaterial({color: 0xbfe2ff, transparent: true, opacity: 0.35, roughness: 0.1, metalness: 0.2});
      /* the bough: out to the right, bending toward the reader, two decks along it */
      var y0 = 10.4, phi = 10 * Math.PI / 180, d = V(Math.cos(phi), 0, Math.sin(phi));
      var pts = [];
      for (var k = 0; k <= 6; k++) {
        var u = k / 6;
        pts.push(V(0, y0, 0).addScaledVector(d, 1.2 + u * 17).add(V(0, Math.sin(u * 2) * 1.2 - u * u * 0.8, Math.sin(u * Math.PI * 0.7) * 2.4)));
      }
      var curve = new THREE.CatmullRomCurve3(pts), prev = curve.getPoint(0);
      for (var s = 1; s <= 14; s++) {
        var nx = curve.getPoint(s / 14), r0 = 1.15 * (1 - (s - 1) / 14) + 0.22, r1 = 1.15 * (1 - s / 14) + 0.22;
        limb(T.site, prev, nx, r0, r1, bark); ball(T.site, r1, bark, nx.x, nx.y, nx.z); prev = nx;
      }
      var tip = curve.getPoint(1);
      T.clump(tip.clone().add(V(0.6, 0.8, -0.6)), 1.6);
      var U = [0.15, 0.29, 0.43, 0.57, 0.71, 0.85];
      var KIND = ['forge', 'mill', 'glass', 'clock', 'weather', 'saw'];
      var spots = U.map(function (u, k) {
        var on = curve.getPoint(u), back = k % 2 === 1, rad = 1.15 * (1 - u) + 0.22;
        var base = on.clone().add(V(0, rad * 0.8 + (back ? 1.5 : 0), back ? -1.6 : 1.3));
        var f = V(0, 0, 1).applyAxisAngle(T.UP, (rnd() - 0.5) * 0.5 + 0.12);
        var host = hostAt(base.x, base.y, base.z, f);
        deck(host, 3.4, 3.0, 0, -0.08, 0.3);
        if (back) [-1, 1].forEach(function (sg) { rod(host, V(sg * 1.4, -0.1, -0.4), V(sg * 1.0, -1.5 - rad * 0.4, 1.4), 0.07, woodD); });
        var kind = KIND[k], wall = k % 2 ? plaster : wood, door = 0.9, w = 2.5, h = 2.2, dp = 2.1, front = dp / 2;
        var wins = [{x: -0.95, y: 1.55, z: front + 0.02}, {x: 0.95, y: 1.55, z: front + 0.02}];
        if (kind === 'forge') {
          box(host, w, h, dp, stone, 0, h / 2, 0);
          roof(host, w, dp, 0.9, h, tone);
          box(host, 0.7, 3.4, 0.7, brick, -w / 2 + 0.2, h + 0.6, -0.5);
          var ember = halo(host, -w / 2 + 0.2, h + 2.5, -0.5, 0xff7a2a, 1.6);
          ticks.push(function (clock, reduced) { glowAt(ember, reduced ? 0.6 : 0.45 + 0.3 * Math.abs(Math.sin(clock * 5.3) * Math.sin(clock * 2.1))); });
          smoke(host, V(-w / 2 + 0.2, h + 2.5, -0.5));
          box(host, 0.55, 0.4, 0.3, dark, 1.55, 0.25, front + 0.6); box(host, 0.75, 0.15, 0.32, dark, 1.55, 0.52, front + 0.6);
        } else if (kind === 'mill') {
          box(host, w, h + 0.3, dp, wall, 0, (h + 0.3) / 2, 0);
          roof(host, w, dp, 1.3, h + 0.3, tone);
          var wheel = grp(host, -w / 2 - 0.35, 1.4, 0);
          put(wheel, new THREE.TorusGeometry(1.35, 0.08, 6, 24), woodD, 0, 0, 0, {ry: Math.PI / 2});
          for (var sp = 0; sp < 8; sp++) {
            var a = sp * TAU / 8;
            rod(wheel, V(0, 0, 0), V(0, Math.cos(a) * 1.35, Math.sin(a) * 1.35), 0.04, woodD);
            box(wheel, 0.5, 0.06, 0.4, wood, 0, Math.cos(a) * 1.4, Math.sin(a) * 1.4, {rx: a});
          }
          ticks.push(function (clock, reduced) { if (!reduced) wheel.rotation.x = clock * 0.6; });
        } else if (kind === 'glass') {
          box(host, w, 0.7, dp, stone, 0, 0.35, 0);
          put(host, new THREE.BoxGeometry(w, h - 0.7, dp), glass, 0, 0.7 + (h - 0.7) / 2, 0, {shadow: false});
          box(host, w, h, 0.2, wood, 0, h / 2, front - 0.1);
          var gr = new THREE.Shape(); gr.moveTo(-dp / 2, 0); gr.lineTo(dp / 2, 0); gr.lineTo(0, 1.0); gr.closePath();
          var gg = new THREE.ExtrudeGeometry(gr, {depth: w, bevelEnabled: false}); gg.translate(0, 0, -w / 2); gg.rotateY(Math.PI / 2);
          put(host, gg, glass, 0, h, 0, {shadow: false});
          for (var fl = 0; fl < 7; fl++) {
            ball(host, 0.12, mat([0x7ad7ff, 0x9dff8a, 0xff8a7a, 0xffd36a][fl % 4], {emissive: [0x2a7fb0, 0x3fae3a, 0xb04a3a, 0xb08a2a][fl % 4], ei: 0.8}),
                 -0.95 + fl * 0.32, 1.1 + (fl % 2) * 0.35, -0.4, {shadow: false});
          }
          wins = [{x: -0.95, y: 1.55, z: front + 0.02}, {x: 0.95, y: 1.55, z: front + 0.02}];
        } else if (kind === 'clock') {
          w = 2.2; h = 4.4;
          box(host, w, h, dp, wall, 0, h / 2, 0);
          put(host, new THREE.ConeGeometry(1.75, 2.2, 4), tone, 0, h + 1.1, 0, {ry: Math.PI / 4});
          put(host, new THREE.ConeGeometry(1.0, 0.9, 4), snowM, 0, h + 1.75, 0, {ry: Math.PI / 4, shadow: false});
          put(host, new THREE.CircleGeometry(0.62, 24), mat(0xfff6e4, {emissive: 0xffe0a0, ei: 0.35}), 0, 3.3, front + 0.02, {shadow: false});
          put(host, new THREE.TorusGeometry(0.64, 0.06, 6, 24), brass, 0, 3.3, front + 0.03, {shadow: false});
          var hands = [grp(host, 0, 3.3, front + 0.05), grp(host, 0, 3.3, front + 0.06)];
          box(hands[0], 0.05, 0.42, 0.02, dark, 0, 0.19, 0, {shadow: false}); box(hands[1], 0.04, 0.55, 0.02, dark, 0, 0.26, 0, {shadow: false});
          ticks.push(function (clock, reduced) { if (!reduced) { hands[0].rotation.z = -clock * 0.05; hands[1].rotation.z = -clock * 0.6; } });
          wins = [{x: 0, y: 2.15, z: front + 0.02}];
        } else if (kind === 'weather') {
          var hr = 1.45;
          put(host, new THREE.CylinderGeometry(hr, hr, h, 6), wall, 0, h / 2, 0, {ry: Math.PI / 6});
          put(host, new THREE.CylinderGeometry(hr + 0.15, hr + 0.15, 0.18, 6), tone, 0, h + 0.09, 0, {ry: Math.PI / 6});
          front = hr * Math.cos(Math.PI / 6);
          rod(host, V(0, h, 0), V(0, h + 1.4, 0), 0.04, dark);
          var cups = grp(host, 0, h + 1.4, 0);
          for (var cu = 0; cu < 3; cu++) {
            var ca = cu * TAU / 3;
            rod(cups, V(0, 0, 0), V(Math.cos(ca) * 0.5, 0, Math.sin(ca) * 0.5), 0.02, dark);
            ball(cups, 0.1, red, Math.cos(ca) * 0.5, 0, Math.sin(ca) * 0.5, {shadow: false});
          }
          var vane = grp(host, 0.9, h + 0.2, -0.3);
          rod(vane, V(0, 0, 0), V(0, 0.8, 0), 0.03, dark); box(vane, 0.7, 0.25, 0.03, brass, 0.25, 0.8, 0);
          ticks.push(function (clock, reduced) { if (!reduced) { cups.rotation.y = clock * 3.2; vane.rotation.y = Math.sin(clock * 0.3) * 0.8; } });
          wins = [{x: 0, y: 1.95, z: front + 0.02}];
        } else {                                                   /* the saw shed: a saw-tooth roof with skylights */
          w = 2.9;
          box(host, w, h, dp, wall, 0, h / 2, 0);
          for (var t3 = 0; t3 < 3; t3++) {
            var sh = new THREE.Shape(); sh.moveTo(0, 0); sh.lineTo(w / 3, 0); sh.lineTo(0, 0.8); sh.closePath();
            var sg2 = new THREE.ExtrudeGeometry(sh, {depth: dp + 0.2, bevelEnabled: false}); sg2.translate(0, 0, -(dp + 0.2) / 2);
            put(host, sg2, tone, -w / 2 + t3 * w / 3, h, 0);
            put(host, new THREE.PlaneGeometry(dp, 0.7), mat(0xffd9a0, {emissive: 0xffb45a, ei: 0.5}), -w / 2 + t3 * w / 3 + 0.02, h + 0.4, 0, {ry: Math.PI / 2, shadow: false});
          }
          box(host, 1.6, 0.12, 0.6, woodD, 0, 0.9, front + 0.9); box(host, 0.9, 0.5, 0.06, mat(0xcfd6e2, {metal: 0.6, rough: 0.3}), 0.2, 1.2, front + 0.9, {rz: 0.2});
        }
        return {host: host, door: V(0, door, front + 0.03), wins: wins, hit: {c: V(0, h / 2 + 0.3, 0), s: [w + 0.4, h + 1.2, dp + 0.6]}};
      });
      /* the crane at the tip, its crate going up and down; the balance by the third shed */
      var crane = grp(T.site, tip.x + 0.3, tip.y + 0.4, tip.z + 0.6, -0.5);
      rod(crane, V(0, 0, 0), V(0, 3.4, 0), 0.12, woodD); rod(crane, V(-0.4, 3.3, 0), V(2.6, 3.3, 0), 0.09, woodD);
      rod(crane, V(0, 2.4, 0), V(1.3, 3.3, 0), 0.05, woodD);
      var crate = grp(crane, 2.4, 1, 0), rope = rod(crane, V(2.4, 3.3, 0), V(2.4, 1.4, 0), 0.025, ropeM);
      box(crate, 0.7, 0.6, 0.7, mat(wk.hex, {rough: 0.6}), 0, 0, 0); box(crate, 0.74, 0.1, 0.74, woodD, 0, 0.22, 0);
      ticks.push(function (clock, reduced) {
        var u = reduced ? 0.5 : 0.5 - 0.5 * Math.cos(clock * 0.35), yy = -3.4 + u * 4.2;
        crate.position.y = yy; rope.scale.y = Math.max(0.05, (3.3 - yy - 0.3) / 1.9); rope.position.y = (3.3 + yy + 0.3) / 2;
      });
      var bal = grp(T.site, curve.getPoint(0.42).x, curve.getPoint(0.42).y + 1.2, curve.getPoint(0.42).z + 2.6);
      rod(bal, V(0, 0, 0), V(0, 1.4, 0), 0.06, brass);
      var beam = grp(bal, 0, 1.4, 0);
      box(beam, 1.8, 0.06, 0.06, brass, 0, 0, 0, {shadow: false});
      var pans = [-1, 1].map(function (sg) {
        var pan = grp(beam, sg * 0.85, 0, 0);
        put(pan, new THREE.CylinderGeometry(0.3, 0.22, 0.08, 12), brass, 0, -0.55, 0, {shadow: false});
        rod(pan, V(0, 0, 0), V(0, -0.5, 0), 0.01, dark);
        return pan;
      });
      ticks.push(function (clock, reduced) { var a = reduced ? 0 : Math.sin(clock * 0.7) * 0.18; beam.rotation.z = a; pans.forEach(function (p) { p.rotation.z = -a; }); });
      return {spots: spots, label: tip.clone().add(V(0, 5.2, 0))};
    }

    /* --- Memory: the library in the trunk ----------------------------------------------------------- */

    function memory(wk) {
      var tone = mat(wk.hex, {rough: 0.55});
      var Y0 = 13.2, Y1 = 25.6, RD = 3.4, FR = RD * Math.cos(Math.PI / 10);
      put(T.site, new THREE.CylinderGeometry(RD, RD, Y1 - Y0, 10, 1, false, -Math.PI / 10), bark, 0, (Y0 + Y1) / 2, 0);
      [Y0, Y1].forEach(function (y, n) {
        put(T.site, new THREE.CylinderGeometry(RD + 0.35, RD + (n ? 0.2 : 0.5), 0.5, 10, 1, false, -Math.PI / 10), mat(0x5a3c22, {rough: 0.8}), 0, y, 0);
        if (n) put(T.site, new THREE.CylinderGeometry(RD + 0.2, RD + 0.4, 0.3, 10, 1, false, -Math.PI / 10), snowM, 0, y + 0.35, 0, {shadow: false});
      });
      /* the bookshelves that show through the tall windows, a texture of spines */
      var spines = T.drawn(256, 512, function (g, w, h) {
        g.fillStyle = '#3a2616'; g.fillRect(0, 0, w, h);
        var cols = ['#c0392b', '#2e86c1', '#d4ac0d', '#239b56', '#7d3c98', '#e67e22', '#ecf0f1', '#5d6d7e'];
        for (var row = 0; row < 6; row++) {
          var y = 12 + row * 82, x = 10;
          while (x < w - 14) { var bw = 10 + Math.floor(rnd() * 14), bh = 52 + Math.floor(rnd() * 22); g.fillStyle = cols[Math.floor(rnd() * cols.length)]; g.fillRect(x, y + 70 - bh, bw, bh); x += bw + 2; }
          g.fillStyle = '#6b4a2e'; g.fillRect(0, y + 70, w, 10);
        }
      });
      var shelfM = new THREE.MeshStandardMaterial({map: spines, emissive: 0xffc27a, emissiveMap: spines, emissiveIntensity: 0.45, roughness: 0.8});
      function facet(a, y) {
        var ar = a * Math.PI / 180, f = V(Math.cos(ar), 0, Math.sin(ar));
        return hostAt(f.x * FR, y, f.z * FR, f);
      }
      [[18, 17.2], [162, 18.4], [18, 22.6], [162, 23.4]].forEach(function (c) {
        var h = facet(c[0], c[1]);
        put(h, new THREE.PlaneGeometry(1.5, 2.8), shelfM, 0, 0, 0.03, {shadow: false});
        box(h, 1.7, 0.14, 0.2, mat(0x5a3c22), 0, 1.47, 0.08); box(h, 1.7, 0.14, 0.3, mat(0x5a3c22), 0, -1.47, 0.1);
        [-1, 1].forEach(function (sg) { box(h, 0.12, 3.0, 0.16, mat(0x5a3c22), sg * 0.8, 0, 0.08); });
      });
      /* the card drawers, low on the left; the clock at the top */
      var cl = facet(90, 24.85);
      put(cl, new THREE.CircleGeometry(0.6, 24), mat(0xfff6e4, {emissive: 0xffe0a0, ei: 0.35}), 0, 0, 0.05, {shadow: false});
      put(cl, new THREE.TorusGeometry(0.62, 0.06, 6, 24), brass, 0, 0, 0.06, {shadow: false});
      var hands = [grp(cl, 0, 0, 0.08), grp(cl, 0, 0, 0.09)];
      box(hands[0], 0.06, 0.36, 0.02, dark, 0, 0.16, 0, {shadow: false}); box(hands[1], 0.04, 0.5, 0.02, dark, 0, 0.23, 0, {shadow: false});
      ticks.push(function (clock, reduced) { if (!reduced) { hands[0].rotation.z = -clock * 0.04; hands[1].rotation.z = -clock * 0.5; } });
      var drawers = facet(54, 24.6);
      for (var dr = 0; dr < 4; dr++) for (var dc = 0; dc < 3; dc++) {
        box(drawers, 0.5, 0.22, 0.2, mat(0x8a5a34), -0.55 + dc * 0.55, -0.4 + dr * 0.27, 0.1);
        box(drawers, 0.14, 0.05, 0.05, brass, -0.55 + dc * 0.55, -0.4 + dr * 0.27, 0.22, {shadow: false});
      }
      var DOORS = [[54, 14.5], [126, 15.4], [90, 17.4], [54, 19.9], [126, 20.8], [90, 22.6]];
      var spots = DOORS.map(function (c, k) {
        var host = facet(c[0], c[1]);
        /* a balcony under the door, its rail, a lantern, a little roof over the door, a banner of the week */
        put(host, new THREE.CylinderGeometry(1.25, 1.25, 0.14, 16, 1, false, -Math.PI / 2, Math.PI), wood, 0, -1.0, 0.0);
        for (var b = 0; b <= 6; b++) { var ba = -Math.PI / 2 + b * Math.PI / 6; rod(host, V(Math.sin(ba) * 1.2, -0.95, Math.cos(ba) * 1.2), V(Math.sin(ba) * 1.2, -0.45, Math.cos(ba) * 1.2), 0.03, woodD); }
        var shape = new THREE.Shape(); shape.moveTo(-0.95, 0); shape.lineTo(0.95, 0); shape.lineTo(0, 0.55); shape.closePath();
        var cg = new THREE.ExtrudeGeometry(shape, {depth: 0.7, bevelEnabled: false});
        put(host, cg, tone, 0, 1.0, 0.0);
        lantern(host, 0.95, 0.75, 0.45);
        box(host, 0.4, 1.1, 0.03, tone, -0.95, -0.15, 0.08, {shadow: false});
        return {host: host, door: V(0, 0, 0.04), wins: [{x: 0, y: 1.85, z: 0.03, small: true}], hit: {c: V(0, -0.1, 0.5), s: [2.1, 2.4, 1.6]}, tagDown: 1.2};
      });
      /* ladders from balcony to balcony */
      for (var k = 0; k < 3; k++) {
        var a = DOORS[k], b = DOORS[k + 3];
        var ar = a[0] * Math.PI / 180, out = V(Math.cos(ar), 0, Math.sin(ar)), side = V(-out.z, 0, out.x);
        var p0 = out.clone().multiplyScalar(FR + 1.1).addScaledVector(side, 0.9).add(V(0, a[1] - 1.0, 0));
        var p1 = out.clone().multiplyScalar(FR + 1.1).addScaledVector(side, 0.9).add(V(0, b[1] - 1.0, 0));
        [-0.25, 0.25].forEach(function (o) { rod(T.site, p0.clone().addScaledVector(side, o), p1.clone().addScaledVector(side, o), 0.035, woodD); });
        for (var r = p0.y + 0.4; r < p1.y; r += 0.45) rod(T.site, V(p0.x, r, p0.z).addScaledVector(side, -0.25), V(p0.x, r, p0.z).addScaledVector(side, 0.25), 0.025, woodD);
      }
      return {spots: spots, label: V(-6.5, 27.5, 3)};
    }

    /* --- Orchestration: the canopy, lookouts round the crow's nest -------------------------------------- */

    function together(wk) {
      var tone = mat(wk.hex, {rough: 0.55}), straw = mat(0xd8b56a, {rough: 0.9}), twig = mat(0x6b4a2e, {rough: 1});
      var NEST = V(0, T.TRUNK_H + 0.6, 0);
      /* the crow's nest: a platform, its rail, a roof on four posts, the bell, a flag */
      var nest = grp(T.site, NEST.x, NEST.y, NEST.z);
      put(nest, new THREE.CylinderGeometry(2.6, 2.3, 0.3, 18), wood, 0, 0, 0);
      for (var p = 0; p < 18; p++) { var pa = p * TAU / 18; rod(nest, V(Math.cos(pa) * 2.5, 0.1, Math.sin(pa) * 2.5), V(Math.cos(pa) * 2.5, 0.8, Math.sin(pa) * 2.5), 0.03, woodD); }
      put(nest, new THREE.TorusGeometry(2.5, 0.04, 4, 36), woodD, 0, 0.8, 0, {rx: Math.PI / 2});
      [[1, 1], [1, -1], [-1, 1], [-1, -1]].forEach(function (c) { rod(nest, V(c[0] * 1.1, 0, c[1] * 1.1), V(c[0] * 1.1, 2.4, c[1] * 1.1), 0.07, woodD); });
      put(nest, new THREE.ConeGeometry(2.0, 1.3, 4), tone, 0, 3.05, 0, {ry: Math.PI / 4});
      put(nest, new THREE.ConeGeometry(1.3, 0.7, 4), snowM, 0, 3.4, 0, {ry: Math.PI / 4, shadow: false});
      var bell = grp(nest, 0, 2.3, 0);
      put(bell, new THREE.CylinderGeometry(0.22, 0.5, 0.7, 14), brass, 0, -0.35, 0);
      ball(bell, 0.1, dark, 0, -0.75, 0);
      ticks.push(function (clock, reduced) { bell.rotation.z = reduced ? 0 : Math.sin(clock * 2.2) * Math.max(0, Math.sin(clock * 0.4)) * 0.35; });
      var LOOK = [[24, 8.4, 28.2, 'bird'], [58, 7.2, 31.6, 'hex'], [96, 8.8, 27.4, 'nest'], [132, 8.4, 30.6, 'dome'], [160, 7.6, 33.4, 'lantern'], [116, 6.2, 36.4, 'signal']];
      var spots = LOOK.map(function (c, k) {
        var ar = c[0] * Math.PI / 180, at = V(Math.cos(ar) * c[1], c[2], Math.sin(ar) * c[1]);
        var f = V(Math.cos(ar), 0, Math.sin(ar)).add(V(0, 0, 1.1)).normalize();
        var host = hostAt(at.x, at.y, at.z, f), kind = c[3], front = 1.2, h = 2.2, w = 2.4, wins = [{x: 0, y: 1.95, z: 0}];
        /* its branch from the trunk, its deck, its bridge to the crow's nest */
        var root = V(Math.cos(ar) * 1.0, at.y - 2.6, Math.sin(ar) * 1.0);
        limb(T.site, root, at.clone().add(V(0, -0.3, 0)), 0.55, 0.25, bark);
        T.clump(at.clone().add(V(-Math.cos(ar) * 1.4, -0.6, -1.6)), 1.3);
        put(host, new THREE.CylinderGeometry(1.75, 1.75, 0.16, 14), wood, 0, -0.08, 0.2);
        bridge(T.site.worldToLocal(host.localToWorld(V(0, 0.05, -1.4))), NEST.clone().add(V(Math.cos(ar) * 2.4, 0.15, Math.sin(ar) * 2.4)));
        if (kind === 'bird') {
          put(host, new THREE.CylinderGeometry(1.2, 1.2, h, 14), mat(0xe8d6b0, {rough: 0.85}), 0, h / 2, 0);
          put(host, new THREE.ConeGeometry(1.55, 1.6, 14), tone, 0, h + 0.8, 0);
          put(host, new THREE.ConeGeometry(0.9, 0.6, 14), snowM, 0, h + 1.35, 0, {shadow: false});
          rod(host, V(0, 0.9, 1.1), V(0, 0.9, 1.7), 0.04, woodD);
        } else if (kind === 'hex') {
          put(host, new THREE.CylinderGeometry(1.4, 1.4, h, 6), wood, 0, h / 2, 0, {ry: Math.PI / 6});
          front = 1.4 * Math.cos(Math.PI / 6);
          put(host, new THREE.CylinderGeometry(1.6, 1.6, 0.16, 6), tone, 0, h + 0.08, 0, {ry: Math.PI / 6});
          for (var hp = 0; hp < 6; hp++) { var ha = hp * TAU / 6; rod(host, V(Math.cos(ha) * 1.5, h + 0.1, Math.sin(ha) * 1.5), V(Math.cos(ha) * 1.5, h + 0.7, Math.sin(ha) * 1.5), 0.03, woodD); }
          var scope = grp(host, 0.3, h + 0.6, -0.3);
          put(scope, new THREE.CylinderGeometry(0.1, 0.14, 1.0, 8), brass, 0, 0, 0, {rz: 1.1});
          ticks.push(function (clock, reduced) { if (!reduced) scope.rotation.y = Math.sin(clock * 0.25) * 1.2; });
        } else if (kind === 'nest') {
          put(host, new THREE.TorusGeometry(1.45, 0.42, 6, 18), twig, 0, 0.2, 0, {rx: Math.PI / 2});
          put(host, new THREE.CylinderGeometry(1.15, 1.15, h, 12), straw, 0, h / 2, 0);
          put(host, new THREE.ConeGeometry(1.5, 1.1, 12), tone, 0, h + 0.55, 0);
          put(host, new THREE.SphereGeometry(1.1, 10, 4, 0, TAU, 0, Math.PI / 4), snowM, 0, h + 0.3, 0, {shadow: false});
          front = 1.15;
        } else if (kind === 'dome') {
          put(host, new THREE.CylinderGeometry(1.3, 1.3, h, 14), mat(0xdfe3ea, {rough: 0.6}), 0, h / 2, 0);
          put(host, new THREE.SphereGeometry(1.36, 16, 8, 0, TAU, 0, Math.PI / 2), tone, 0, h, 0);
          box(host, 0.35, 0.9, 1.5, dark, 0, h + 0.8, 0, {rx: -0.6});
          put(host, new THREE.CylinderGeometry(0.16, 0.2, 1.6, 10), brass, 0, h + 1.1, 0.4, {rx: -0.8});
          front = 1.3; wins = [{x: 0, y: 1.95, z: 0}];
        } else if (kind === 'lantern') {
          h = 2.5;
          put(host, new THREE.CylinderGeometry(1.2, 1.2, h, 8), mat(0xf6e3c0, {emissive: 0xffb45a, ei: 0.25, rough: 0.9}), 0, h / 2, 0, {ry: Math.PI / 8});
          for (var lr = 0; lr < 3; lr++) put(host, new THREE.TorusGeometry(1.22, 0.05, 4, 8), tone, 0, 0.4 + lr * 0.85, 0, {rx: Math.PI / 2, ry: Math.PI / 8});
          put(host, new THREE.ConeGeometry(1.4, 0.9, 8), tone, 0, h + 0.45, 0, {ry: Math.PI / 8});
          front = 1.2 * Math.cos(Math.PI / 8);
          rod(host, V(0, -0.1, 0), V(0, -0.9, 0), 0.05, tone);
        } else {                                                   /* the signal post: two arms that move */
          box(host, w, h + 0.3, 2.0, wood, 0, (h + 0.3) / 2, 0);
          roof(host, w, 2.0, 1.0, h + 0.3, tone);
          rod(host, V(0.9, h + 0.6, -0.4), V(0.9, h + 2.6, -0.4), 0.06, dark);
          var arms = [grp(host, 0.9, h + 2.4, -0.3), grp(host, 0.9, h + 1.7, -0.3)];
          arms.forEach(function (a, n) { box(a, 1.2, 0.16, 0.05, n ? tone : red, 0.55, 0, 0); });
          ticks.push(function (clock, reduced) { if (!reduced) { arms[0].rotation.z = Math.round(Math.sin(clock * 0.5) * 2) * 0.6; arms[1].rotation.z = -Math.round(Math.cos(clock * 0.37) * 2) * 0.6; } });
          front = 1.0;
        }
        wins.forEach(function (wn) { wn.z = front + 0.03; });
        return {host: host, door: V(0, 0.95, front + 0.03), wins: wins, hit: {c: V(0, 1.3, 0.2), s: [w + 0.6, h + 1.6, 2.8]}};
      });
      /* birds carrying messages between the lookouts and the nest */
      var birds = [];
      for (var bi = 0; bi < 6; bi++) {
        var g = grp(T.site), wings = [];
        ball(g, 0.16, dark, 0, 0, 0, {shadow: false}).scale.set(1, 0.8, 1.6);
        box(g, 0.28, 0.02, 0.2, mat(0xfaf7ef), 0, -0.14, 0.12, {shadow: false});
        [-1, 1].forEach(function (s) { var wg = grp(g, s * 0.1, 0, 0); box(wg, 0.5, 0.02, 0.22, dark, s * 0.25, 0, 0, {shadow: false}); wings.push(wg); });
        var c = LOOK[bi], ar = c[0] * Math.PI / 180;
        birds.push({g: g, wings: wings, a: V(Math.cos(ar) * c[1], c[2] + 2.4, Math.sin(ar) * c[1]), b: NEST.clone().add(V(0, 3.0, 0)), ph: bi / 6});
      }
      ticks.push(function (clock, reduced) {
        birds.forEach(function (b, n) {
          var u = reduced ? b.ph : (clock * 0.09 + b.ph) % 1, k = u < 0.5 ? u * 2 : 2 - u * 2;
          var p = b.a.clone().lerp(b.b, k); p.y += Math.sin(k * Math.PI) * 2.2;
          var next = b.a.clone().lerp(b.b, Math.min(1, k + (u < 0.5 ? 0.02 : -0.02)));
          b.g.position.copy(p); b.g.lookAt(next.x, p.y, next.z);
          b.wings.forEach(function (wg, s) { wg.rotation.z = (s ? -1 : 1) * (reduced ? 0.3 : Math.sin(clock * 14 + n) * 0.7); });
        });
      });
      /* flags strung between the lookouts */
      var colors = [0xd2382c, 0xffd36a, 0x3f9bff, 0x35d66b, 0xffffff];
      for (var fk = 0; fk < LOOK.length - 1; fk++) {
        var A = LOOK[fk], B = LOOK[fk + 1], aa = A[0] * Math.PI / 180, bb = B[0] * Math.PI / 180;
        var pa2 = V(Math.cos(aa) * A[1], A[2] + 2.9, Math.sin(aa) * A[1]), pb2 = V(Math.cos(bb) * B[1], B[2] + 2.9, Math.sin(bb) * B[1]);
        if (pa2.distanceTo(pb2) > 9) continue;
        for (var q = 1; q < 9; q++) {
          var u2 = q / 9, fp = pa2.clone().lerp(pb2, u2); fp.y -= Math.sin(u2 * Math.PI) * 0.7;
          put(T.site, new THREE.ConeGeometry(0.16, 0.36, 3), mat(colors[q % colors.length], {rough: 0.6}), fp.x, fp.y - 0.18, fp.z, {rx: Math.PI, shadow: false});
        }
      }
      return {spots: spots, label: V(7.5, 40.5, 2)};
    }

    return {words: words, proof: proof, memory: memory, together: together,
            tick: function (clock, reduced) { ticks.forEach(function (f) { f(clock, reduced); }); }};
  }

  global.ArbreParties = {make: make};
})(window);
