/* The rooms of days 1 to 12: one style a day, taken from what the article is about (engine: `station_styles.js`).
 *
 * A room says: its ground and what is painted on it (`paint`, with the brush `P`), its paper, the face of its title,
 * its corners and shadows, and the frame around the canvas (`shell`). A dark room (`dark`) also gives its two
 * papers, its line and its inks. The ground stays quiet: the day's animation is drawn over all of it.
 */
(function (global) {
  'use strict';
  var room = global.StationStyles.room;

  /* 1. Write the request down: the request arrives like a letter, and is kept. An airmail letter. */
  room(1, {
    key: 'airmail', ground: '#f8f2e4', paper: '#fffdf6', font: 'Caveat, "Segoe Print", cursive', scale: 1.32, round: 0.5,
    title: '#1b3a7a',
    shell: {frame: '#d8c7a0', edge: '#8a744a', bg: '#f1e7d0', chip: '#c0392b'},
    paint: function (P) {
      var bands = ['#c0392b', '#f8f2e4', '#1f4fa8', '#f8f2e4'];
      P.tape(bands, 0, 0, 960, 11, 16); P.tape(bands, 0, 0, 11, 600, 16); P.tape(bands, 949, 0, 11, 600, 16);
      [0, 1, 2].forEach(function (i) { P.ring('#8a4b3a', 872, 60, 22 + i * 9, 2, 0.16); });          /* a postmark */
      [0, 1, 2, 3].forEach(function (i) { P.line('#8a4b3a', 790, 44 + i * 11, 842, 44 + i * 11, 2, 0.14); });
    }
  });

  /* 2. Flag the word that does not fit: a proofreader's galley, the marks in red in the margin. */
  room(2, {
    key: 'galley', ground: '#f2efe6', paper: '#fbf9f3', font: '"Special Elite", "Courier New", monospace', weight: '400',
    scale: 0.94, round: 0.15, shadow: 'none', lw: 1.2, edge: '#8d8a80',
    shell: {frame: '#3b3a37', edge: '#121211', bg: '#e4e0d5', chip: '#b3261e'},
    paint: function (P) {
      P.cols('#b9b4a6', 30, 30, 1, 1, 0.7); P.cols('#b9b4a6', 930, 930, 1, 1, 0.7);
      P.rows('#d9d5c9', 120, 520, 26, 1, 0.5, 40, 920);
      ['¶', '‸', 'sp', '✓', '?', 'stet'].forEach(function (m, i) {
        P.text(m, 15, 150 + i * 62, 'italic 700 17px Georgia, serif', '#b3261e', 0.75, -0.12, 'center');
      });
      P.path('#b3261e', [[936, 200], [944, 206], [936, 212], [944, 218], [936, 224], [944, 230]], 2, 0.6);
    }
  });

  /* 3. A short instructions file: one page, read at every start. A yellow legal pad. */
  room(3, {
    key: 'legal-pad', ground: '#fbf2b0', paper: '#fffdf0', font: '"Courier Prime", "Courier New", monospace', scale: 0.96,
    round: 0.3, ink: '#27221a', muted: '#5b523a',
    shell: {frame: '#7a2e22', edge: '#3c120c', bg: '#f1e59a', chip: '#7a2e22'},
    paint: function (P) {
      P.rows('#7fa6d8', 124, 600, 28, 1.2, 0.5);
      P.cols('#d0544a', 28, 32, 4, 1.4, 0.7);
      P.rect('#7a2e22', 0, 0, 960, 13);
      P.dots('#3c120c', 22, 2, 0.8, 11, 6.5, 960, 7);
    }
  });

  /* 4. Consolidate, never append: one line per rule, the latest state. An accounting ledger. */
  room(4, {
    key: 'ledger', ground: '#e7f1e2', paper: '#fbfdf8', font: '"Playfair Display", Georgia, serif', round: 0.3,
    ink: '#18261d', title: '#1e4d3a',
    shell: {frame: '#1e4d3a', edge: '#c9a646', bg: '#dce8d6', chip: '#1e4d3a', go: '#1e4d3a'},
    paint: function (P) {
      P.rows('#7fb08c', 110, 600, 24, 1, 0.45);
      P.cols('#c8574c', 22, 26, 4, 1.2, 0.55); P.cols('#c8574c', 934, 938, 4, 1.2, 0.55);
      P.cols('#7fb08c', 150, 900, 150, 1, 0.3, 100);
      P.rect('#1e4d3a', 0, 0, 960, 6, 0.85); P.rect('#c9a646', 0, 6, 960, 2);
    }
  });

  /* 5. Ask only your questions: what waits for the owner is pinned where it is seen. A cork board. */
  room(5, {
    key: 'cork', ground: '#d6b684', paper: '#fffef5', font: '"Patrick Hand", "Segoe Print", cursive', weight: '400', scale: 1.16,
    round: 0.3, shadow: 'lift', cast: 'rgba(60,35,10,.32)', ink: '#2a2118', muted: '#4f4030', edge: '#c9b48e',
    band: 'rgba(60,35,10,.13)',
    shell: {frame: '#8b5a2b', edge: '#4d2f12', bg: '#e8d3ae', chip: '#b3261e'},
    paint: function (P) { P.specks(['#b08d55', '#e6cda0', '#9c7a45', '#c9a56e'], 2600, 11, 2.2, 0.55); },
    deco: function (g, x, y, w) {                      /* the pin that holds a card */
      g.beginPath(); g.arc(x + w / 2 + 1, y + 3, 7, 0, Math.PI * 2); g.fillStyle = 'rgba(40,20,5,.3)'; g.fill();
      g.beginPath(); g.arc(x + w / 2, y, 7, 0, Math.PI * 2); g.fillStyle = '#c0392b'; g.fill();
      g.beginPath(); g.arc(x + w / 2 - 2, y - 2, 2.4, 0, Math.PI * 2); g.fillStyle = '#f3a59c'; g.fill();
    }
  });

  /* 6. Plan the flow: who does what, in which order, before any code. A blueprint. */
  room(6, {
    key: 'blueprint', dark: true, ground: '#1b4a8a', paper: '#21559c', paper2: '#2a63b0', line: '#9cc0f0', ink: '#f2f7ff',
    muted: '#c3d8f5', dim: '#a9c6ee', deep: 0.26, font: '"Share Tech Mono", "JetBrains Mono", monospace', weight: '400',
    caps: true, scale: 0.98, round: 0, shadow: 'none', edge: '#cfe1fb',
    term: {bg: '#0f2f5c', line: '#9cc0f0'},
    shell: {frame: '#16365f', edge: '#0a1b33', bg: '#173f73', title: '#f2f7ff', chip: '#f2f7ff', chipInk: '#16365f'},
    paint: function (P) {
      P.rows('#ffffff', 0, 600, 20, 1, 0.06); P.cols('#ffffff', 0, 960, 20, 1, 0.06);
      P.rows('#ffffff', 0, 600, 100, 1.2, 0.15); P.cols('#ffffff', 0, 960, 100, 1.2, 0.15);
      P.path('#ffffff', [[9, 9], [951, 9], [951, 591], [9, 591]], 2, 0.4, true);
      [[9, 9], [951, 9]].forEach(function (c) { P.ring('#ffffff', c[0], c[1], 5, 1.5, 0.5); });
    }
  });

  /* 7. One session, one task: each task has its line and its time. A station departure board. */
  room(7, {
    key: 'departures', dark: true, ground: '#16181d', paper: '#20242c', paper2: '#2b303b', line: '#4d5564', ink: '#f6f2e4',
    muted: '#bdb9aa', dim: '#8e8b80', font: 'Oswald, "Arial Narrow", sans-serif', weight: '600', caps: true, scale: 1.02,
    round: 0.25, shadow: 'none', title: '#f2b441',
    term: {bg: '#0b0c0f', line: '#4d5564'},
    shell: {frame: '#2b2f38', edge: '#f2b441', bg: '#1b1e25', title: '#f2b441', chip: '#f2b441', chipInk: '#16181d'},
    paint: function (P) {
      P.dots('#ffffff', 8, 1.1, 0.05);
      P.rect('#f2b441', 0, 0, 960, 7);
      P.rows('#000000', 100, 520, 60, 2, 0.35);
    }
  });

  /* 8. Share a repository: branches leave and come back to one line. A transit map. */
  room(8, {
    key: 'transit', ground: '#fbfbf7', paper: '#ffffff', font: '"Space Grotesk", "Segoe UI", sans-serif', round: 1.7,
    lw: 1.25, title: '#0d2a66',
    shell: {frame: '#1f4fa8', edge: '#0d2a66', bg: '#f1f4fa', chip: '#d62828'},
    paint: function (P) {
      var lines = [
        ['#d62828', [[0, 70], [300, 70], [420, 190], [960, 190]]],
        ['#1f4fa8', [[0, 430], [220, 430], [340, 310], [700, 310], [820, 430], [960, 430]]],
        ['#2fb67e', [[180, 0], [180, 150], [330, 300], [330, 600]]],
        ['#f2b441', [[960, 60], [800, 60], [640, 220], [640, 600]]]
      ];
      lines.forEach(function (l) { P.path(l[0], l[1], 11, 0.13); });
      [[300, 70], [420, 190], [340, 310], [700, 310], [330, 300], [640, 220], [640, 310]].forEach(function (s) {
        P.ring('#ffffff', s[0], s[1], 8, 0, 1, true); P.ring('#5d6577', s[0], s[1], 8, 3, 0.25);
      });
    }
  });

  /* 9. Run, measure, then write: the number first. Millimetre paper and a ruler. */
  room(9, {
    key: 'millimetre', ground: '#fdf9f1', paper: '#ffffff', round: 0.2, shadow: 'none', lw: 1.1, edge: '#b7a48f',
    title: '#8a3c12',
    shell: {frame: '#3d7a8c', edge: '#1e4652', bg: '#e4eff1', chip: '#d9692a'},
    paint: function (P) {
      P.rows('#e58a4c', 0, 600, 12, 1, 0.16); P.cols('#e58a4c', 0, 960, 12, 1, 0.16);
      P.rows('#e58a4c', 0, 600, 60, 1.2, 0.36); P.cols('#e58a4c', 0, 960, 60, 1.2, 0.36);
      P.rect('#f3e3c4', 0, 0, 960, 14); P.rect('#f3e3c4', 0, 0, 14, 600);
      for (var i = 0; i <= 80; i++) {
        P.line('#6b5335', i * 12, 0, i * 12, i % 5 ? 6 : 12, 1, 0.8); P.line('#6b5335', 0, i * 12, i % 5 ? 6 : 12, i * 12, 1, 0.8);
      }
    }
  });

  /* 10. Look at the screenshot: the image is the proof. A darkroom, a strip of film. */
  room(10, {
    key: 'darkroom', dark: true, ground: '#19110f', paper: '#261b18', paper2: '#342521', line: '#6a4f47', ink: '#f8ede7',
    muted: '#cfb8ad', dim: '#a08a80', italic: true, round: 0.35, shadow: 'none', title: '#ffcdc4',
    term: {bg: '#0c0706', line: '#6a4f47'},
    shell: {frame: '#3a1512', edge: '#b3261e', bg: '#1f1412', title: '#ffd9d2', chip: '#b3261e'},
    paint: function (P) {
      P.glow('rgba(214,40,40,.55)', 900, 20, 420, 0.3);
      P.rect('#0b0706', 0, 0, 27, 600); P.rect('#0b0706', 933, 0, 27, 600);
      for (var y = 14; y < 600; y += 34) {
        [6, 939].forEach(function (x) {
          P.g.fillStyle = '#4a352f'; P.g.beginPath();
          P.g.moveTo(x + 3, y); P.g.arcTo(x + 15, y, x + 15, y + 20, 3); P.g.arcTo(x + 15, y + 20, x, y + 20, 3);
          P.g.arcTo(x, y + 20, x, y, 3); P.g.arcTo(x, y, x + 15, y, 3); P.g.closePath(); P.g.fill();
        });
      }
    }
  });

  /* 11. Checks that fail before you push: a command, a total, then only the faults. An old terminal. */
  room(11, {
    key: 'crt', dark: true, ground: '#0e1f19', paper: '#14302a', paper2: '#1c4037', line: '#3f8370', ink: '#dcffe9',
    muted: '#93cdb1', dim: '#6fa58c', font: 'VT323, "JetBrains Mono", monospace', weight: '400', scale: 1.4, round: 0.3,
    shadow: 'none', title: '#7dffb0',
    term: {bg: '#06110d', line: '#3f8370', ink: '#b9f5cf', cmd: '#7dffb0'},
    shell: {frame: '#d8ceb4', edge: '#8f866c', bg: '#cfc5a9', title: '#2a2a22', chip: '#1b6b4b'},
    paint: function (P) {
      P.glow('rgba(125,255,176,.5)', 480, 280, 560, 0.1);
      P.rows('#000000', 0, 600, 4, 1.4, 0.2);
    }
  });

  /* 12. Done, or suggested and not yet done: what is built, what is only planned. A building site. */
  room(12, {
    key: 'site', ground: '#ebe8e1', paper: '#fdfcf8', font: '"Saira Stencil One", Oswald, Impact, sans-serif', weight: '400',
    caps: true, scale: 0.9, round: 0.15, shadow: 'hard', cast: 'rgba(29,29,29,.28)', lw: 1.25, edge: '#3a3a36',
    shell: {frame: '#f2b441', edge: '#1d1d1d', bg: '#e3dfd5', chip: '#1d1d1d', chipInk: '#f2b441'},
    paint: function (P) {
      P.specks(['#c9c5ba', '#ffffff', '#b5b0a3'], 900, 5, 1.6, 0.5);
      P.tape(['#f2b441', '#1d1d1d'], 0, 0, 960, 12, 18);
      P.tape(['#f2b441', '#1d1d1d'], 0, 12, 12, 588, 18); P.tape(['#f2b441', '#1d1d1d'], 948, 12, 12, 588, 18);
    }
  });
})(typeof window !== 'undefined' ? window : this);
