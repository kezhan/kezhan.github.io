/* The modules of the station, each wing built its own way, each module with its own fittings.
 *
 * The author, 23-11, showing the RAG calendar's campus: « regarde, c'est varié, c'est riche, tu vois ? des bâtiments
 * différents ». So the four wings of the station are four kinds of module, as the campus has four kinds of building:
 *
 *   wing 1  Words           habitation: quilted white blankets, handrails, a window row, antennas
 *   wing 2  Proof           laboratory: eight-sided, blue-grey panels, experiments bolted outside with their lights
 *   wing 3  Memory          archive: drums wrapped in gold foil, domed ends, canisters racked on top
 *   wing 4  Orchestration   command: a band of lit windows with the crew inside, a cupola, masts that blink
 *
 * and inside a wing no two modules carry the same fitting. The hatch side (local z = side) stays clear: the hatch,
 * its number and its lamp belong to `scene_station.js`. A module is built in its hull group: local x runs along the
 * wing, y up, z across. `make(T)` takes the scene's helpers; `shell(...)` returns what blinks.
 */
(function (global) {
  'use strict';

  function make(T) {
    var THREE = T.THREE, TAU = Math.PI * 2, L = T.MOD_L, R = T.MOD_R;

    function tex(w, h, paint, rx, ry) {
      var t = T.drawn(w, h, paint);
      t.wrapS = t.wrapT = THREE.RepeatWrapping; t.repeat.set(rx || 1, ry || 1);
      return t;
    }
    function skin(map, o) {
      o = o || {};
      return new THREE.MeshStandardMaterial({map: map, roughness: o.rough == null ? 0.6 : o.rough, metalness: o.metal || 0,
        emissive: o.emissive || 0x000000, emissiveMap: o.emap || null, emissiveIntensity: o.ei == null ? 1 : o.ei, flatShading: !!o.flat});
    }

    /* the four skins */
    var quilt = skin(tex(256, 256, function (g, w, h) {
      g.fillStyle = '#eef0ee'; g.fillRect(0, 0, w, h);
      g.strokeStyle = 'rgba(120,125,135,.35)'; g.lineWidth = 2;
      for (var i = -h; i < w + h; i += 32) {
        g.beginPath(); g.moveTo(i, 0); g.lineTo(i + h, h); g.stroke();
        g.beginPath(); g.moveTo(i, h); g.lineTo(i + h, 0); g.stroke();
      }
      g.fillStyle = 'rgba(255,255,255,.5)';
      for (var y = 16; y < h; y += 32) for (var x = 16; x < w; x += 32) { g.beginPath(); g.arc(x, y, 3, 0, TAU); g.fill(); }
    }, 3, 1), {rough: 0.85});
    var lab = skin(tex(256, 256, function (g, w, h) {
      g.fillStyle = '#7c8aa6'; g.fillRect(0, 0, w, h);
      for (var y = 0; y < h; y += 64) for (var x = 0; x < w; x += 64) {
        var k = 0.9 + ((x * 7 + y * 13) % 5) * 0.04;
        g.fillStyle = 'rgb(' + Math.round(120 * k) + ',' + Math.round(136 * k) + ',' + Math.round(166 * k) + ')';
        g.fillRect(x + 3, y + 3, 58, 58);
        g.fillStyle = 'rgba(30,36,52,.6)';
        [[8, 8], [56, 8], [8, 56], [56, 56]].forEach(function (p) { g.beginPath(); g.arc(x + p[0], y + p[1], 2.5, 0, TAU); g.fill(); });
      }
    }, 2, 1), {rough: 0.45, metal: 0.45, flat: true});
    var gold = skin(tex(256, 256, function (g, w, h) {
      var gr = g.createLinearGradient(0, 0, w, h);
      gr.addColorStop(0, '#b67a12'); gr.addColorStop(0.5, '#f1c35a'); gr.addColorStop(1, '#c48a1c');
      g.fillStyle = gr; g.fillRect(0, 0, w, h);
      for (var i = 0; i < 160; i++) {
        var x = (i * 97) % w, y = (i * 61) % h, a = (i % 7) / 7;
        g.strokeStyle = 'rgba(' + (i % 2 ? '255,236,170' : '110,64,6') + ',' + (0.25 + a * 0.3) + ')';
        g.lineWidth = 1 + (i % 3);
        g.beginPath(); g.moveTo(x, y); g.lineTo(x + 30 - (i % 60), y + 18 - (i % 36)); g.stroke();
      }
    }, 2, 1), {rough: 0.3, metal: 0.75});
    /* the command modules: around the hull (u) four rows of windows, along it (v) four windows each, someone at some */
    function windows(lit) {
      return tex(256, 256, function (g, w, h) {
        g.fillStyle = lit ? '#000000' : '#e9edf3'; g.fillRect(0, 0, w, h);
        [12, 76, 140, 204].forEach(function (u, row) {
          for (var v = 14, n = 0; v < h; v += 62, n++) {
            g.fillStyle = '#ffd89a'; g.fillRect(u, v, 40, 40);
            if (!lit && (n + row) % 3 === 0) {
              g.fillStyle = '#3a3040'; g.beginPath(); g.arc(u + 20, v + 17, 7, 0, TAU); g.fill(); g.fillRect(u + 11, v + 24, 18, 16);
            }
            if (!lit) { g.strokeStyle = '#5d6577'; g.lineWidth = 4; g.strokeRect(u, v, 40, 40); }
          }
        });
      });
    }
    var command = skin(windows(false), {rough: 0.5, emissive: 0xffffff, emap: windows(true), ei: 0.7});
    var plain = T.mat(0xe9edf3, {rough: 0.45, metal: 0.15}), steelD = T.mat(0x4f5668, {rough: 0.5, metal: 0.5});
    var steel = T.mat(0x9aa3b5, {rough: 0.4, metal: 0.6}), yellow = T.mat(0xf2b630, {rough: 0.5});
    var cellM = new THREE.MeshStandardMaterial({color: 0x1d3a7a, roughness: 0.3, metalness: 0.6, emissive: 0x2a4fb0, emissiveIntensity: 0.15, side: THREE.DoubleSide});
    var glass = T.mat(0x9fd2ff, {rough: 0.15, metal: 0.4, emissive: 0xffc27a, ei: 0.55});

    function lightAt(p, x, y, z, color, size, speed, phase) {
      var h = T.halo(p, x, y, z, color, size || 0.7);
      return {h: h, speed: speed || 2, phase: phase || 0};
    }
    /* a stalk with a blinking tip */
    function mast(p, x, z, tall, color, blinks, ph) {
      T.rod(p, [x, R * 0.9, z], [x, R + tall, z], 0.04, steel);
      T.ball(p, 0.09, steelD, x, R + tall, z);
      blinks.push(lightAt(p, x, R + tall + 0.05, z, color, 0.6, 2.6, ph));
    }

    /* each kind: its hull, then one fitting per module (idx 0 to 5) */
    var KINDS = [
      function words(h, idx, s, blinks) {
        var len = L * (idx % 2 ? 0.92 : 1);
        T.tubeX(h, R, -len / 2, len / 2, quilt, 0, 0, 22);
        [-1, 1].forEach(function (e) { T.tubeX(h, R * 0.8, e * len / 2 - 0.12, e * len / 2 + 0.12, steelD, 0, 0, 16); });
        /* yellow handrails along the top */
        [-0.35, 0.35].forEach(function (z) { T.rod(h, [-len / 2 + 0.3, R + 0.12, z], [len / 2 - 0.3, R + 0.12, z], 0.035, yellow); });
        var F = [
          function () { mast(h, 0.6, -0.2, 1.4, 0xff6a5a, blinks, idx); },
          function () { for (var i = 0; i < 3; i++) T.ball(h, 0.16, glass, -0.7 + i * 0.7, R * 0.72, -s * R * 0.72); },
          function () { T.box(h, 1.1, 0.5, 0.7, T.mat(0xd8cfb8, {rough: 0.9}), 0.2, R + 0.3, -0.1); T.box(h, 0.6, 0.4, 0.6, T.mat(0xc9b99a, {rough: 0.9}), -0.7, R + 0.25, 0.1); },
          function () { var f = T.grp(h, 0, R + 0.1, 0); T.box(f, 1.8, 0.04, 1.2, T.mat(0xf4f4f0, {rough: 0.3, metal: 0.2}), 0, 0.9, 0, {rx: 0}); T.rod(f, [0, 0, 0], [0, 0.9, 0], 0.05, steel); },
          function () { T.box(h, 0.9, 0.6, 0.8, steelD, -0.3, R + 0.35, -0.2); mast(h, 0.8, 0.1, 0.9, 0x7ad7ff, blinks, idx); },
          function () {        /* a little greenhouse */
            T.put(h, new THREE.SphereGeometry(0.55, 14, 8, 0, TAU, 0, Math.PI / 2), T.mat(0x9dff8a, {rough: 0.2, emissive: 0x3fae55, ei: 0.5}), 0, R - 0.05, -0.1);
          }
        ];
        F[idx % 6]();
        return len / 2;
      },
      function proof(h, idx, s, blinks) {
        var len = L * (idx % 3 === 1 ? 0.9 : 1);
        T.put(h, new THREE.CylinderGeometry(R * 1.04, R * 1.04, len, 8), lab, 0, 0, 0, {rz: Math.PI / 2});
        [-1, 1].forEach(function (e) { T.put(h, new THREE.CylinderGeometry(R * 0.85, R * 0.85, 0.24, 8), steelD, e * len / 2, 0, 0, {rz: Math.PI / 2}); });
        /* the pallet of experiments on top, each module its own load, each experiment its light */
        T.box(h, len * 0.8, 0.12, 1.3, steelD, 0, R + 0.1, 0);
        var loads = [[[-0.6, 0x6f5bd0], [0.3, 0xe9edf3], [0.9, 0xf2b630]], [[-0.4, 0x3a6fd8], [0.6, 0xd2553f]],
                     [[-0.8, 0x2fb67e], [-0.1, 0xe9edf3], [0.6, 0x6f5bd0]], [[0, 0xf2b630]], [[-0.5, 0xe9edf3], [0.5, 0x3a6fd8]],
                     [[-0.7, 0xd2553f], [0, 0x2fb67e], [0.7, 0xf2b630]]][idx % 6];
        loads.forEach(function (b, n) {
          var tall = 0.35 + ((idx + n) % 3) * 0.15;
          T.box(h, 0.5, tall, 0.55, T.mat(b[1], {rough: 0.5}), b[0], R + 0.16 + tall / 2, -0.2 + (n % 2) * 0.3);
          blinks.push(lightAt(h, b[0] + 0.28, R + 0.2 + tall, -0.2 + (n % 2) * 0.3, (n + idx) % 4 ? 0x3fe07a : 0xff5a4a, 0.55, 3 + n, idx + n));
        });
        if (idx % 2) {          /* a test cage of rods */
          var x0 = -len / 2 + 0.25;
          [[0, 0.5], [0, -0.5]].forEach(function (c) { T.rod(h, [x0, R + 0.16, c[1]], [x0, R + 1.1, c[1]], 0.03, steel); });
          T.rod(h, [x0, R + 1.1, 0.5], [x0, R + 1.1, -0.5], 0.03, steel);
        }
        return len / 2;
      },
      function memory(h, idx, s, blinks) {
        var len = L * 0.62;      /* a drum, its domed ends flattened so each one stands apart */
        T.tubeX(h, R, -len / 2, len / 2, gold, 0, 0, 22);
        [-1, 1].forEach(function (e) {
          T.put(h, new THREE.SphereGeometry(R, 18, 10, 0, TAU, 0, Math.PI / 2), gold, e * len / 2, 0, 0, {rz: -e * Math.PI / 2}).scale.set(1, 0.42, 1);
        });
        T.tubeX(h, R * 1.03, -0.08, 0.08, steelD, 0, 0, 22);
        /* canisters racked on top: the log, kept */
        var n = 2 + idx % 4;
        for (var i = 0; i < n; i++) {
          var x = -0.9 + i * (1.8 / Math.max(1, n - 1));
          T.put(h, new THREE.CylinderGeometry(0.2, 0.2, 0.7, 12), i % 2 ? T.mat(0xe9edf3, {rough: 0.4}) : T.mat(0xd9a441, {rough: 0.35, metal: 0.6}), x, R + 0.4, -0.15);
        }
        if (idx === 2) {        /* a small dish to send the log down */
          var d = T.grp(h, 0.2, R + 0.2, 0.3);
          T.put(d, new THREE.SphereGeometry(0.6, 14, 6, 0, TAU, 0, Math.PI / 4), T.mat(0xf0f2f5, {rough: 0.35}), 0, 0.9, 0, {rx: Math.PI});
          T.rod(d, [0, 0, 0], [0, 0.35, 0], 0.04, steel);
        }
        if (idx === 4) mast(h, 0.9, 0.3, 1.2, 0xffd36a, blinks, idx);
        blinks.push(lightAt(h, -len / 2 - R * 0.45, 0.3, 0, 0xffd36a, 0.5, 1.4, idx));
        return len / 2 + 0.08;
      },
      function orchestration(h, idx, s, blinks) {
        var len = L * (idx % 2 ? 1 : 0.94), r = R;      /* no wider than the hatch, or the hull cuts the door */
        T.tubeX(h, r, -len / 2, len / 2, command, 0, 0, 24);
        [-1, 1].forEach(function (e) { T.tubeX(h, r * 0.82, e * len / 2 - 0.12, e * len / 2 + 0.12, steelD, 0, 0, 16); });
        if (idx % 2 === 0) {    /* the cupola where someone watches the others */
          T.put(h, new THREE.SphereGeometry(0.55, 14, 8, 0, TAU, 0, Math.PI / 2), glass, 0.4, r - 0.05, -0.1);
        }
        mast(h, -0.8, -0.3, 0.8 + (idx % 3) * 0.35, 0xb78cff, blinks, idx * 0.7);
        if (idx % 3 === 1) {    /* a crew pod docked on the far side */
          T.ball(h, 0.42, plain, 0.4, 0.2, -s * (r + 0.4));
          blinks.push(lightAt(h, 0.4, 0.55, -s * (r + 0.4), 0x7ad7ff, 0.6, 2.2, idx));
        }
        return len / 2;
      }
    ];

    return {
      command: command,
      /* build the shell of one module; `s` is the hatch side in local z (1 or -1) */
      /* returns the half length of the module, where its end rings go */
      shell: function (wing, idx, hull, s, blinks) { return KINDS[wing](hull, idx, s, blinks); },
      /* the blinking lights of every module, and the crew's windows brighter at night */
      tick: function (blinks, clock, reduced) {
        blinks.forEach(function (b) {
          T.glowAt(b.h, reduced ? 0.6 : 0.2 + 0.7 * Math.max(0, Math.sin(clock * b.speed + b.phase * 1.7)));
        });
      }
    };
  }

  global.StationModules = {make: make};
})(window);
