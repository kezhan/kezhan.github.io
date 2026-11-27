/* The station at work, and in December.
 *
 * What moves between the modules, so the station is seen working and not only built: packets of light run along the
 * trusses (requests coming in along Words, checks out to Proof and back, the log down to Memory, the crew's orders
 * from Orchestration), in the colour of the wing they serve. And what a crew hangs up in December: a fir on the hub
 * with its bulbs (its star is the star of the 24th), a wreath around every hatch, the supply capsule wrapped as a
 * gift, a red hat on one of the two astronauts. `make(T)` takes the scene's helpers.
 */
(function (global) {
  'use strict';

  function make(T) {
    var THREE = T.THREE, TAU = Math.PI * 2;
    var fir = T.mat(0x2c7446, {rough: 0.8}), firD = T.mat(0x1f5a35, {rough: 0.8}), trunk = T.mat(0x6b4a2e, {rough: 0.9});
    var red = T.mat(0xd2382c, {rough: 0.5}), goldM = T.mat(0xffd76a, {rough: 0.3, metal: 0.7}), whiteM = T.mat(0xffffff, {rough: 0.6});
    var TONES = [0xff3b30, 0xffc233, 0x35d66b, 0x3f9bff, 0xff5fb0, 0xfff0c8];
    var bulbs = [], packets = [];

    return {
      /* the fir on top of the hub: three tiers, bulbs that breathe in turn */
      tree: function (p, y) {
        var g = T.grp(p, 0, y, 0);
        T.put(g, new THREE.CylinderGeometry(0.18, 0.22, 0.5, 8), trunk, 0, 0.25, 0);
        [[1.25, 1.3, 0.9], [1.0, 1.15, 1.7], [0.7, 1.0, 2.4]].forEach(function (t, i) {
          T.put(g, new THREE.ConeGeometry(t[0], t[1], 10), i % 2 ? firD : fir, 0, t[2], 0);
        });
        for (var i = 0; i < 26; i++) {
          var h = 0.75 + (i / 26) * 2.2, r = 1.15 * (1 - (h - 0.5) / 2.7) + 0.05, a = i * 2.4;
          bulbs.push({h: T.halo(g, Math.cos(a) * r, h, Math.sin(a) * r, TONES[i % TONES.length], 0.5), ph: i % 2});
        }
        return g;
      },
      /* a wreath around a hatch ring, its bow at the bottom (the lamp is at the top) */
      wreath: function (hatch) {
        var w = T.put(hatch, new THREE.TorusGeometry(0.7, 0.09, 6, 28), fir, 0, 0, 0.02, {shadow: false});
        w.userData.keep = true;
        for (var i = 0; i < 8; i++) {
          var a = i * TAU / 8 + 0.2;
          T.ball(hatch, 0.05, i % 2 ? red : goldM, Math.cos(a) * 0.7, Math.sin(a) * 0.7, 0.1, {shadow: false}).userData.keep = true;
        }
        var bow = T.grp(hatch, 0, -0.72, 0.1);
        bow.userData.keep = true;
        [-1, 1].forEach(function (s) { T.box(bow, 0.16, 0.1, 0.04, red, s * 0.1, 0, 0, {rz: s * 0.4, shadow: false}).userData.keep = true; });
      },
      /* the supply capsule, wrapped: a red parcel with its gold ribbon */
      gift: function (capsule) {
        var g = T.grp(capsule, -1.0, 0, 0);
        T.box(g, 0.9, 0.9, 0.9, red, -0.45, 0, 0);
        T.box(g, 0.92, 0.14, 0.92, goldM, -0.45, 0, 0);
        T.box(g, 0.14, 0.92, 0.92, goldM, -0.45, 0, 0);
        [-1, 1].forEach(function (s) { T.box(g, 0.08, 0.22, 0.3, goldM, -0.45, 0.52, s * 0.12, {rx: s * 0.6}); });
      },
      /* a red hat with its white pompom, on an astronaut's head (at y 0.5) */
      hat: function (g) {
        T.put(g, new THREE.ConeGeometry(0.22, 0.45, 10), red, 0, 0.9, -0.02, {rx: -0.35});
        T.put(g, new THREE.TorusGeometry(0.2, 0.06, 6, 14), whiteM, 0, 0.7, 0, {rx: Math.PI / 2});
        T.ball(g, 0.08, whiteM, 0, 1.12, -0.12);
      },
      /* packets of light along each wing's truss, in the wing's colour; `inward` runs tip to hub */
      packets: function (p, wings, tones, spineY, from, to) {
        wings.forEach(function (w, i) {
          for (var n = 0; n < 5; n++) {
            packets.push({h: T.halo(p, 0, 0, 0, tones[i], 0.75), w: w, ph: n / 5, inward: i === 0 || (i === 1 && n % 2), y: spineY + 0.15,
              from: from, to: to, speed: 0.06 + 0.012 * i});
          }
        });
      },
      tick: function (clock, reduced) {
        bulbs.forEach(function (b) { T.glowAt(b.h, reduced ? 0.8 : 0.45 + 0.5 * Math.max(0, Math.sin(clock * 1.6 + b.ph * Math.PI))); });
        packets.forEach(function (k) {
          var u = reduced ? k.ph : (clock * k.speed + k.ph) % 1;
          var x = k.inward ? k.to - (k.to - k.from) * u : k.from + (k.to - k.from) * u;
          k.h.position.copy(k.w.d).multiplyScalar(x).addScaledVector(k.w.side, 1.3);
          k.h.position.y = k.y;
          T.glowAt(k.h, Math.sin(u * Math.PI) * 0.95);
        });
      }
    };
  }

  global.StationVie = {make: make};
})(window);
