/* The rooms of days 13 to 24: one style a day, taken from what the article is about (engine: `station_styles.js`,
 * the first twelve rooms and the vocabulary of a room: `station_salles_01_12.js`).
 */
(function (global) {
  'use strict';
  var room = global.StationStyles.room;

  /* 13. Repeat yourself? Write a skill: the instruction said again becomes a recipe one follows. A recipe card. */
  room(13, {
    key: 'recipe', ground: '#fff7ec', paper: '#fffdf8', font: '"DM Serif Display", Georgia, serif', weight: '400', round: 0.6,
    title: '#8f2418',
    shell: {frame: '#c0392b', edge: '#7a1f15', bg: '#fdeedd', chip: '#c0392b'},
    paint: function (P) {
      for (var x = 0; x < 960; x += 14) for (var y = 0; y < 14; y += 7) if ((x / 14 + y / 7) % 2 === 0) P.rect('#c0392b', x, y, 14, 7, 0.75);
      P.rect('#c0392b', 0, 0, 960, 14, 0.22);
      P.rows('#e8a79d', 120, 600, 30, 1, 0.42);
      P.line('#c0392b', 0, 18, 960, 18, 1.5, 0.6);
    }
  });

  /* 14. Advisory or enforced: a rule one may ignore, a barrier one cannot. Road signs. */
  room(14, {
    key: 'road', ground: '#e9ece9', paper: '#ffffff', font: 'Overpass, "Arial Narrow", Arial, sans-serif', weight: '800',
    round: 1.25, lw: 1.5, edge: '#3a4042',
    shell: {frame: '#0f6b3c', edge: '#ffffff', bg: '#0f6b3c', title: '#ffffff', chip: '#f2b441', chipInk: '#1d1d1d', go: '#0b4f2c'},
    paint: function (P) {
      var g = P.g, i;
      g.globalAlpha = 0.09; g.strokeStyle = '#3a4042'; g.lineWidth = 9; g.lineJoin = 'round';
      g.beginPath();                                                      /* a stop sign */
      for (i = 0; i < 8; i++) { var a = Math.PI / 8 + i * Math.PI / 4; g[i ? 'lineTo' : 'moveTo'](820 + 96 * Math.cos(a), 300 + 96 * Math.sin(a)); }
      g.closePath(); g.stroke();
      g.beginPath(); g.moveTo(120, 470); g.lineTo(215, 305); g.lineTo(310, 470); g.closePath(); g.stroke();   /* a warning */
      g.beginPath(); g.arc(480, 170, 70, 0, Math.PI * 2); g.stroke();                                         /* an order */
      g.globalAlpha = 1;
      for (i = 0; i < 960; i += 60) P.rect('#f2b441', i, 0, 36, 6, 0.9);
    }
  });

  /* 15. Spend context on the problem: ask the map where it is, then read 18 lines. An atlas. */
  room(15, {
    key: 'atlas', ground: '#f0ead5', paper: '#fdfaf0', font: '"Playfair Display", Georgia, serif', italic: true, round: 0.6,
    ink: '#2a2418', muted: '#5f553e', title: '#2f5d47',
    shell: {frame: '#6b4a2b', edge: '#2f1f10', bg: '#e8dcc0', chip: '#2f6b4f'},
    paint: function (P) {
      var g = P.g;
      [[250, 330, 1.25, 0.4], [760, 190, 0.85, -0.3], [690, 500, 0.7, 0.9]].forEach(function (hill, n) {
        for (var k = 1; k <= 7; k++) {                  /* contour lines round a summit */
          g.globalAlpha = 0.26; g.strokeStyle = '#7d9156'; g.lineWidth = k % 3 === 0 ? 1.8 : 1; g.beginPath();
          for (var a = 0; a <= 64; a++) {
            var t = a / 64 * Math.PI * 2, r = k * 34 * hill[2] * (1 + 0.16 * Math.sin(t * 3 + hill[3] + k * 0.4) + 0.08 * Math.cos(t * 5 + n));
            g[a ? 'lineTo' : 'moveTo'](hill[0] + r * Math.cos(t) * 1.25, hill[1] + r * Math.sin(t));
          }
          g.closePath(); g.stroke();
        }
      });
      g.globalAlpha = 1;
      P.ring('#6b4a2b', 905, 56, 26, 1.5, 0.35); P.ring('#6b4a2b', 905, 56, 19, 1, 0.3);            /* a compass rose */
      P.path('#6b4a2b', [[905, 22], [911, 56], [905, 90], [899, 56]], 1, 0.3, true, true);
      P.path('#6b4a2b', [[871, 56], [905, 51], [939, 56], [905, 61]], 1, 0.2, true, true);
      P.text('N', 905, 18, '700 13px Georgia, serif', '#6b4a2b', 0.55, 0, 'center');
    }
  });

  /* 16. One file for every path: one module knows where everything is. A code editor at night. */
  room(16, {
    key: 'editor', dark: true, ground: '#10142a', paper: '#19213d', paper2: '#222c50', line: '#40508a', ink: '#e9edff',
    muted: '#a8b2dc', dim: '#7c88b8', font: '"JetBrains Mono", Consolas, monospace', scale: 0.88, round: 0.5, shadow: 'none',
    title: '#7fe3f5',
    term: {bg: '#080b18', line: '#40508a'},
    shell: {frame: '#232c4f', edge: '#5ad1e6', bg: '#141a31', title: '#e9edff', chip: '#5ad1e6', chipInk: '#0c1020'},
    paint: function (P) {
      P.rect('#0b0f20', 0, 0, 30, 600);
      for (var n = 1; n <= 24; n++) P.text(String(n), 24, n * 24 + 4, '500 12px Consolas, monospace', '#5a679c', 0.8, 0, 'right');
      P.cols('#ffffff', 70, 930, 40, 1, 0.035);
      P.rect('#5ad1e6', 0, 0, 960, 3, 0.8);
    }
  });

  /* 17. Generated files are never edited by hand: the page comes off the press. A two-colour print. */
  room(17, {
    key: 'press', ground: '#fbf5ea', paper: '#fffdf8', font: '"Archivo Black", "Arial Black", sans-serif', weight: '400',
    scale: 0.9, round: 0, shadow: 'hard', cast: 'rgba(61,111,224,.4)', lw: 1.25, edge: '#1d1d1d', title: '#1d1d1d',
    shell: {frame: '#1d1d1d', edge: '#ff5c8a', bg: '#f3ecdf', chip: '#3d6fe0'},
    paint: function (P) {
      var g = P.g, x, y;
      g.fillStyle = '#ff5c8a';                            /* a halftone that fades from a corner */
      for (y = 0; y < 260; y += 13) for (x = 700; x < 960; x += 13) {
        var d = Math.hypot(960 - x, y) / 330, r = 5.2 * (1 - d);
        if (r > 0.4) { g.globalAlpha = 0.3; g.beginPath(); g.arc(x + (y / 13 % 2) * 6.5, y, r, 0, Math.PI * 2); g.fill(); }
      }
      g.fillStyle = '#3d6fe0';
      for (y = 600; y > 330; y -= 13) for (x = 0; x < 250; x += 13) {
        var d2 = Math.hypot(x, 600 - y) / 320, r2 = 5.2 * (1 - d2);
        if (r2 > 0.4) { g.globalAlpha = 0.22; g.beginPath(); g.arc(x + (y / 13 % 2) * 6.5, y, r2, 0, Math.PI * 2); g.fill(); }
      }
      g.globalAlpha = 1;
      [[14, 14], [946, 14]].forEach(function (c) {         /* registration marks */
        P.ring('#1d1d1d', c[0], c[1], 6, 1.2, 0.7); P.line('#1d1d1d', c[0] - 10, c[1], c[0] + 10, c[1], 1.2, 0.7); P.line('#1d1d1d', c[0], c[1] - 10, c[0], c[1] + 10, 1.2, 0.7);
      });
    }
  });

  /* 18. Build it to be redone fast: 18 minutes, then 3 minutes 32. A race track. */
  room(18, {
    key: 'race', ground: '#f6f6f3', paper: '#ffffff', font: 'Oswald, "Arial Narrow", sans-serif', weight: '700', italic: true,
    caps: true, scale: 1.04, round: 0.3, title: '#c21f1f',
    shell: {frame: '#c21f1f', edge: '#1d1d1d', bg: '#efefec', chip: '#1d1d1d'},
    paint: function (P) {
      for (var x = 0; x < 960; x += 8) for (var y = 0; y < 16; y += 8) if ((x / 8 + y / 8) % 2 === 0) P.rect('#1d1d1d', x, y, 8, 8);
      P.rect('#c21f1f', 0, 16, 9, 584); P.rect('#c21f1f', 951, 16, 9, 584);
      for (var i = 0; i < 16; i++) P.path('#1d1d1d', [[-80 + i * 76, 600], [20 + i * 76, 110]], 16, 0.022);
    }
  });

  /* 19. A ticket's state is its folder: open, in progress, done. Manila folders. */
  room(19, {
    key: 'folders', ground: '#ebd7a2', paper: '#fffaf0', font: '"Courier Prime", "Courier New", monospace', scale: 0.96,
    round: 0.4, ink: '#2b2416', muted: '#55482c', cast: 'rgba(70,50,10,.2)', band: 'rgba(70,50,10,.1)',
    shell: {frame: '#b9975b', edge: '#6f5626', bg: '#e5cf96', chip: '#6f5626'},
    paint: function (P) {
      [[40, 190], [300, 190], [560, 190]].forEach(function (tab, i) {      /* the tabs of the folders behind */
        P.path(['#dcc385', '#d3b873', '#c9ad63'][i], [[tab[0], 22], [tab[0] + 14, 0], [tab[0] + tab[1] - 14, 0], [tab[0] + tab[1], 22]], 0, 1, true, true);
      });
      P.rect('#c9ad63', 0, 22, 960, 2, 0.8);
      var r = P.rand(3);
      for (var i = 0; i < 260; i++) { var x = r() * 960, y = 24 + r() * 576; P.line('#b79a58', x, y, x + 10 + r() * 30, y + (r() - 0.5) * 2, 1, 0.22); }
    }
  });

  /* 20. Step back before you start: a ticket more than three days old is a suspect. Old papers from the archive. */
  room(20, {
    key: 'archive', ground: '#e8dabb', paper: '#f8efd9', font: '"IM Fell English", "Playfair Display", Georgia, serif',
    weight: '400', scale: 1.1, round: 0.25, ink: '#35261a', muted: '#63503a', title: '#5a3418', cast: 'rgba(60,35,10,.22)',
    band: 'rgba(60,35,10,.1)',
    shell: {frame: '#4a3221', edge: '#22150c', bg: '#d8c6a2', chip: '#7a4a22'},
    paint: function (P) {
      P.glow('rgba(120,80,30,.8)', 120, 520, 300, 0.16); P.glow('rgba(120,80,30,.8)', 880, 90, 260, 0.14);
      P.glow('rgba(255,250,235,.9)', 480, 300, 520, 0.35);
      P.specks(['#a8875a', '#c9b089'], 500, 9, 1.8, 0.4);
      [[760, 470, 46], [200, 150, 30]].forEach(function (s) { P.ring('#8a5a2b', s[0], s[1], s[2], 3, 0.1); P.ring('#8a5a2b', s[0] + 4, s[1] + 3, s[2] - 5, 1, 0.08); });
    }
  });

  /* 21. A coordinator that launches, checks and merges: one desk sees every session. A control tower. */
  room(21, {
    key: 'tower', dark: true, ground: '#07161d', paper: '#0e242e', paper2: '#153340', line: '#327284', ink: '#e4fbff',
    muted: '#94c8d4', dim: '#6b9ba6', font: '"Share Tech Mono", "JetBrains Mono", monospace', weight: '400', caps: true,
    scale: 0.98, round: 0.4, shadow: 'none', title: '#8ff0c8',
    term: {bg: '#030b0f', line: '#327284'},
    shell: {frame: '#12313c', edge: '#2fb67e', bg: '#0b1d25', title: '#bff3e0', chip: '#2fb67e', chipInk: '#06231a'},
    paint: function (P) {
      [110, 220, 330, 440, 550].forEach(function (r) { P.ring('#5fe0c0', 480, 310, r, 1.2, 0.1); });
      P.line('#5fe0c0', 0, 310, 960, 310, 1, 0.09); P.line('#5fe0c0', 480, 0, 480, 600, 1, 0.09);
    },
    live: function (P, t) {                              /* the sweep of the radar */
      var g = P.g, a = t * 0.9;
      g.save(); g.globalAlpha = 0.07; g.fillStyle = '#5fe0c0';
      g.beginPath(); g.moveTo(480, 310); g.arc(480, 310, 620, a - 0.5, a); g.closePath(); g.fill(); g.restore();
    }
  });

  /* 22. The lightest model that suffices: a list of levels and what each costs. A chalkboard menu. */
  room(22, {
    key: 'chalkboard', dark: true, ground: '#23342a', paper: '#2b3f33', paper2: '#354b3e', line: '#8ea595', ink: '#f6f4ea',
    muted: '#c5cfbf', dim: '#9aa896', font: '"Fredericka the Great", Caveat, cursive', weight: '400', round: 0.3,
    shadow: 'none', title: '#fff3c4',
    term: {bg: '#131d17', line: '#8ea595'},
    shell: {frame: '#8a5a2b', edge: '#4f3015', bg: '#6f4722', title: '#fff3dc', chip: '#f2b441', chipInk: '#3a230d'},
    paint: function (P) {
      P.glow('rgba(255,255,255,.9)', 300, 200, 380, 0.05); P.glow('rgba(255,255,255,.9)', 760, 420, 340, 0.04);
      var r = P.rand(21);
      for (var i = 0; i < 70; i++) { var x = r() * 960, y = r() * 600; P.line('#ffffff', x, y, x + 40 + r() * 90, y + (r() - 0.5) * 14, 6 + r() * 10, 0.012); }
      P.path('#f6f4ea', [[12, 40], [12, 12], [40, 12]], 2, 0.35); P.path('#f6f4ea', [[948, 40], [948, 12], [920, 12]], 2, 0.35);
    }
  });

  /* 23. The one who wrote it does not judge it: another hand marks the copy. An exam paper. */
  room(23, {
    key: 'exam', ground: '#fdfcf6', paper: '#ffffff', font: 'Caveat, "Segoe Print", cursive', scale: 1.32, round: 0.3,
    shadow: 'none', lw: 1.1, edge: '#9aa0c4', title: '#1f3a8a',
    shell: {frame: '#5b6fb3', edge: '#2a3566', bg: '#eceefa', chip: '#b3261e'},
    paint: function (P) {
      P.rows('#8d7fd0', 0, 600, 8, 1, 0.13); P.rows('#8d7fd0', 0, 600, 32, 1.1, 0.3);
      P.cols('#8d7fd0', 30, 960, 32, 1, 0.2);
      P.line('#d0544a', 30, 0, 30, 600, 1.6, 0.75);
      P.ring('#d0544a', 902, 52, 30, 2.5, 0.4);                                        /* where the mark goes */
      P.path('#d0544a', [[884, 54], [896, 66], [922, 38]], 3, 0.4);
    }
  });

  /* 24. Hand over before the context fills: what is left, in order, for the next watch. A ship's log. */
  room(24, {
    key: 'logbook', ground: '#f4eedb', paper: '#fffcf2', round: 0.5, ink: '#1c2638', title: '#14305c',
    shell: {frame: '#14305c', edge: '#c9a646', bg: '#ebe2c6', chip: '#c9a646', chipInk: '#14305c', go: '#14305c'},
    paint: function (P) {
      var g = P.g, i;
      P.path('#14305c', [[7, 7], [953, 7], [953, 593], [7, 593]], 2.5, 0.75, true);
      P.path('#c9a646', [[13, 13], [947, 13], [947, 587], [13, 587]], 1.2, 0.9, true);
      g.save(); g.translate(690, 300); g.globalAlpha = 0.07; g.strokeStyle = g.fillStyle = '#14305c'; g.lineWidth = 3;
      g.beginPath(); g.arc(0, 0, 190, 0, Math.PI * 2); g.stroke(); g.beginPath(); g.arc(0, 0, 150, 0, Math.PI * 2); g.stroke();
      for (i = 0; i < 8; i++) {                           /* a wind rose behind the page */
        g.rotate(Math.PI / 4); var len = i % 2 ? 190 : 120;
        g.beginPath(); g.moveTo(0, -len); g.lineTo(16, 0); g.lineTo(-16, 0); g.closePath(); g.fill();
      }
      g.restore();
      P.rows('#b8ad8c', 124, 600, 30, 1, 0.3, 24, 936);
    }
  });
})(typeof window !== 'undefined' ? window : this);
