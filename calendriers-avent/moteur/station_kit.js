/* The drawing kit of the station's modules: what a day's animation is made of.
 *
 * Behind each hatch of the station, the day's practice plays as an animation in a few stops (contract in
 * `STATION.md`). A stop draws on a canvas of 960 by 600 units, at time `t` seconds, with the helpers below: a
 * session window, chat bubbles, a file, a terminal, a commit, a check light, an arrow that grows. Every helper draws
 * at once; what moves is decided by the stop, from `t`, with `k.at`, `k.ease` and `k.type`.
 *
 * Colours: inputs amber, work blue, results emerald, red only for what fails. No em dash. Type 19 px or more.
 *
 * A module draws with the colours and shapes below; the room it plays in (`station_styles.js`, one style a day)
 * decides the ground, translates the neutrals, and sets the corners, the shadows and the face of the title.
 */
(function (global) {
  'use strict';

  var SERIF = 'Fraunces, Georgia, serif';
  var SANS = '"Space Grotesk", "Segoe UI", system-ui, sans-serif';
  var MONO = '"JetBrains Mono", Consolas, monospace';
  var INK = '#1d2433', MUTED = '#5d6577', LINE = '#d7dce5', PAPER = '#ffffff', GROUND = '#f3f5f9';
  var ZONE = {
    amber: ['#9a5b00', '#fff3dc', '#f2b441'],
    blue: ['#1f4fa8', '#e6eefc', '#4f7fe0'],
    green: ['#0f7a52', '#e3f6ee', '#2fb67e'],
    red: ['#b3261e', '#fde8e6', '#e0584f'],
    grey: ['#4f5668', '#eef0f4', '#9aa3b5']
  };

  function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }

  function make(g, lang, S) {
    var Styles = global.StationStyles;
    S = S || (Styles ? Styles.of(0) : {round: 1, shadow: 'soft', lw: 1});
    if (Styles && S.key !== 'plain') Styles.tame(g, S);
    function raw(fn) { g.__raw = true; fn(); g.__raw = false; }
    var k = {
      g: g, lang: lang, W: 960, H: 600, SERIF: SERIF, SANS: SANS, MONO: MONO, INK: INK, MUTED: MUTED, ZONE: ZONE,
      tx: function (o) { return o == null ? '' : typeof o === 'string' ? o : (o[lang] || o.en || ''); },
      clamp: clamp,
      /* 0 before `start`, 1 after `start + len`, eased in between */
      at: function (t, start, len) { return k.ease(clamp((t - start) / (len || 0.6), 0, 1)); },
      ease: function (x) { x = clamp(x, 0, 1); return x < 0.5 ? 4 * x * x * x : 1 - Math.pow(-2 * x + 2, 3) / 2; },
      mix: function (a, b, p) { return a + (b - a) * p; },
      /* the first part of a string, as if typed: p from 0 to 1 */
      type: function (s, p) { s = k.tx(s); return s.slice(0, Math.round(s.length * clamp(p, 0, 1))); },
      pulse: function (t, speed) { return 0.5 + 0.5 * Math.sin(t * (speed || 4)); },

      style: S,
      clear: function (t) { if (Styles && S.ground) Styles.ground(S, g, t); else { g.fillStyle = GROUND; g.fillRect(0, 0, 960, 600); } },

      round: function (x, y, w, h, r) {
        r = Math.min(r * S.round, w / 2, h / 2);
        g.beginPath(); g.moveTo(x + r, y); g.arcTo(x + w, y, x + w, y + h, r); g.arcTo(x + w, y + h, x, y + h, r);
        g.arcTo(x, y + h, x, y, r); g.arcTo(x, y, x + w, y, r); g.closePath();
      },
      box: function (x, y, w, h, o) {
        o = o || {};
        k.round(x, y, w, h, o.r == null ? 12 : o.r);
        var sh = o.shadow !== false && S.shadow !== 'none';
        if (sh && S.shadow === 'hard') {          /* a print: the card's shape again, shifted, flat */
          k.round(x + 6, y + 6, w, h, o.r == null ? 12 : o.r);
          raw(function () { g.fillStyle = S.cast || 'rgba(20,30,60,.3)'; g.fill(); });
          k.round(x, y, w, h, o.r == null ? 12 : o.r); sh = false;
        }
        if (sh) {
          g.save(); g.shadowColor = S.cast || 'rgba(20,30,60,.12)'; g.shadowBlur = S.shadow === 'lift' ? 10 : 14;
          g.shadowOffsetY = S.shadow === 'lift' ? 7 : 4; if (S.shadow === 'lift') g.shadowOffsetX = 3;
        }
        g.fillStyle = o.fill || PAPER; g.fill();
        if (sh) g.restore();
        if (o.stroke !== false) {
          g.lineWidth = (o.lw || 2) * S.lw;
          if (!o.stroke && S.edge) raw(function () { g.strokeStyle = S.edge; }); else g.strokeStyle = o.stroke || LINE;
          g.stroke();
        }
        if (S.deco && o.shadow !== false && w >= 150 && h >= 70) raw(function () { S.deco(g, x, y, w, h); });
      },

      /* text; `max` wraps on words and returns the y below the last line */
      text: function (s, x, y, o) {
        o = o || {};
        s = k.tx(s);
        var size = o.size || 22;
        g.font = (o.italic ? 'italic ' : '') + (o.bold ? '700 ' : '500 ') + size + 'px ' + (o.font || SANS);
        g.fillStyle = o.ink || INK; g.textAlign = o.align || 'left'; g.textBaseline = 'alphabetic';
        var a0 = g.globalAlpha; if (o.alpha != null) g.globalAlpha = a0 * o.alpha;
        var lh = o.lh || Math.round(size * 1.3);
        if (!o.max) { g.fillText(s, x, y); g.globalAlpha = a0; return y + lh; }
        var words = s.split(' '), line = '';
        words.forEach(function (wd) {
          var tt = line ? line + ' ' + wd : wd;
          if (g.measureText(tt).width > o.max && line) { g.fillText(line, x, y); y += lh; line = wd; } else line = tt;
        });
        if (line) { g.fillText(line, x, y); y += lh; }
        g.globalAlpha = a0;
        return y;
      },
      width: function (s, size, font, bold) {
        g.font = (bold ? '700 ' : '500 ') + (size || 22) + 'px ' + (font || SANS);
        return g.measureText(k.tx(s)).width;
      },

      /* a small rounded label; zone is a key of ZONE */
      chip: function (s, x, y, zone, o) {
        o = o || {};
        var z = ZONE[zone || 'blue'], size = o.size || 19, w = k.width(s, size, o.font || MONO, true) + 24;
        if (o.align === 'center') x -= w / 2;
        if (o.align === 'right') x -= w;
        k.round(x, y - size - 8, w, size + 16, (size + 16) / 2 / Math.max(1, S.round));
        g.fillStyle = z[1]; g.fill(); g.lineWidth = 2; g.strokeStyle = z[2]; g.stroke();
        k.text(s, x + 12, y + 1, {size: size, font: o.font || MONO, bold: true, ink: z[0]});
        return w;
      },

      /* the heading of a stop, top left */
      title: function (s, sub) {
        var face = S.font || SERIF, size = Math.round(32 * (S.scale || 1));
        g.font = (S.italic ? 'italic ' : '') + (S.weight || '700') + ' ' + size + 'px ' + face;
        var wide = g.measureText(k.tx(s)).width;
        if (wide > 880) size = Math.floor(size * 880 / wide);
        g.fillStyle = INK; if (S.title) raw(function () { g.fillStyle = S.title; });
        g.textAlign = 'left'; g.textBaseline = 'alphabetic';
        g.font = (S.italic ? 'italic ' : '') + (S.weight || '700') + ' ' + size + 'px ' + face;
        g.fillText(S.caps ? k.tx(s).toUpperCase() : k.tx(s), 40, 58);
        if (sub) k.text(sub, 40, 92, {size: 20, ink: MUTED});
      },

      /* a session window: a title bar with its dot, and the inner box returned as {x, y, w, h} */
      window: function (x, y, w, h, name, o) {
        o = o || {};
        k.box(x, y, w, h, {r: 14, stroke: o.stroke || '#c9d0dc', lw: o.lw || 2});
        g.fillStyle = o.bar || '#f0eee8'; k.round(x + 1, y + 1, w - 2, 40, 13); g.fill();
        g.fillRect(x + 1, y + 26, w - 2, 15);
        k.dot(x + 22, y + 21, o.dot || 'blue', o.t);
        k.text(name, x + 40, y + 28, {size: 19, font: MONO, bold: true, ink: o.ink || INK});
        return {x: x + 14, y: y + 54, w: w - 28, h: h - 66};
      },
      /* a session's state: blue = working (pulses with t), grey = waiting, green = done, red = failed */
      dot: function (x, y, state, t) {
        var c = {blue: '#3b7be6', grey: '#a3aab8', green: '#2fb67e', red: '#e0584f', amber: '#f2b441'}[state] || state;
        if (state === 'blue' && t != null) {
          g.beginPath(); g.arc(x, y, 7 + 4 * k.pulse(t, 5), 0, Math.PI * 2); g.fillStyle = 'rgba(59,123,230,.22)'; g.fill();
        }
        g.beginPath(); g.arc(x, y, 7, 0, Math.PI * 2); g.fillStyle = c; g.fill();
      },

      /* a chat bubble; who = 'you' (right, amber) or 'agent' (left, blue); returns its height */
      bubble: function (s, x, y, w, who, o) {
        o = o || {};
        var size = o.size || 19, z = who === 'you' ? ZONE.amber : ZONE.blue;
        g.font = '500 ' + size + 'px ' + (o.font || SANS);
        var lines = [], line = '';
        k.tx(s).split(' ').forEach(function (wd) {
          var tt = line ? line + ' ' + wd : wd;
          if (g.measureText(tt).width > w - 28 && line) { lines.push(line); line = wd; } else line = tt;
        });
        if (line) lines.push(line);
        var lh = Math.round(size * 1.3), h = lines.length * lh + 22;
        var a0 = g.globalAlpha; if (o.alpha != null) g.globalAlpha = a0 * o.alpha;
        k.round(x, y, w, h, 14); g.fillStyle = z[1]; g.fill(); g.lineWidth = 2; g.strokeStyle = z[2]; g.stroke();
        lines.forEach(function (l, i) { k.text(l, x + 14, y + 16 + size * 0.85 + i * lh, {size: size, font: o.font, ink: z[0]}); });
        if (o.time) k.text(o.time, x + w - 10, y - 6, {size: 19, font: MONO, ink: MUTED, align: 'right'});
        g.globalAlpha = a0;
        return h;
      },

      /* a file: its name on a tab, its lines below; o.mark = {index: zone} colours some lines */
      file: function (x, y, w, h, name, lines, o) {
        o = o || {};
        k.box(x, y, w, h, {r: 10, stroke: o.stroke || '#c9d0dc'});
        g.fillStyle = '#f6f7fa'; k.round(x + 1, y + 1, w - 2, 36, 9); g.fill(); g.fillRect(x + 1, y + 24, w - 2, 13);
        k.text(name, x + 14, y + 26, {size: 19, font: MONO, bold: true, ink: o.ink || INK});
        var size = o.size || 19, lh = o.lh || Math.round(size * 1.4), yy = y + 46 + size;
        (lines || []).forEach(function (l, i) {
          if (yy > y + h - 8) return;
          var m = o.mark && o.mark[i];
          if (m) { g.fillStyle = ZONE[m][1]; g.fillRect(x + 6, yy - size - 2, w - 12, lh); }
          k.text(l, x + 14, yy, {size: size, font: o.font || MONO, ink: m ? ZONE[m][0] : INK});
          yy += lh;
        });
        return yy;
      },

      /* a terminal: dark, monospace; lines may start with '$ ' (a command), '✓ ' (green) or '✗ ' (red) */
      term: function (x, y, w, h, lines, o) {
        o = o || {};
        var T = S.term || {};
        k.round(x, y, w, h, 12);
        raw(function () { g.fillStyle = T.bg || '#161b26'; g.fill(); if (T.line) { g.lineWidth = 2; g.strokeStyle = T.line; g.stroke(); } });
        var size = o.size || 19, lh = Math.round(size * 1.45), yy = y + 22 + size;
        (lines || []).forEach(function (l) {
          if (yy > y + h - 8) return;
          var ink = T.ink || '#d8dee9';
          if (l.indexOf('$ ') === 0) ink = T.cmd || '#f2c66d';
          else if (l.indexOf('✓') === 0) ink = '#5fd39b';
          else if (l.indexOf('✗') === 0) ink = '#ff7b72';
          g.__raw = true; k.text(l, x + 16, yy, {size: size, font: MONO, ink: ink}); g.__raw = false;
          yy += lh;
        });
      },

      /* an arrow from a to b, drawn up to p (0..1) */
      arrow: function (x1, y1, x2, y2, p, o) {
        o = o || {};
        p = p == null ? 1 : clamp(p, 0, 1);
        if (p <= 0) return;
        var x = x1 + (x2 - x1) * p, y = y1 + (y2 - y1) * p, a = Math.atan2(y2 - y1, x2 - x1);
        g.strokeStyle = g.fillStyle = o.color || ZONE[o.zone || 'blue'][2]; g.lineWidth = o.lw || 4; g.lineCap = 'round';
        if (o.dash) g.setLineDash(o.dash);
        g.beginPath(); g.moveTo(x1, y1); g.lineTo(x, y); g.stroke(); g.setLineDash([]);
        g.beginPath(); g.moveTo(x, y); g.lineTo(x - 16 * Math.cos(a - 0.45), y - 16 * Math.sin(a - 0.45));
        g.lineTo(x - 16 * Math.cos(a + 0.45), y - 16 * Math.sin(a + 0.45)); g.closePath(); g.fill();
      },

      /* a check light: ok true (green tick), false (red cross), null (grey, waiting) */
      light: function (x, y, ok, r) {
        r = r || 18;
        g.beginPath(); g.arc(x, y, r, 0, Math.PI * 2);
        g.fillStyle = ok === true ? '#2fb67e' : ok === false ? '#e0584f' : '#c4cad6'; g.fill();
        g.strokeStyle = '#fff'; g.lineWidth = 4; g.lineCap = 'round'; g.beginPath();
        if (ok === true) { g.moveTo(x - r * 0.45, y); g.lineTo(x - r * 0.1, y + r * 0.38); g.lineTo(x + r * 0.5, y - r * 0.38); }
        else if (ok === false) { g.moveTo(x - r * 0.4, y - r * 0.4); g.lineTo(x + r * 0.4, y + r * 0.4); g.moveTo(x + r * 0.4, y - r * 0.4); g.lineTo(x - r * 0.4, y + r * 0.4); }
        g.stroke();
      },

      /* a commit: a node on a line, its short hash and message */
      commit: function (x, y, hash, msg, zone, o) {
        o = o || {};
        g.beginPath(); g.arc(x, y, 11, 0, Math.PI * 2); g.fillStyle = ZONE[zone || 'blue'][2]; g.fill();
        g.lineWidth = 3; g.strokeStyle = '#fff'; g.stroke();
        k.text(hash, x + 22, y + 7, {size: 19, font: MONO, bold: true, ink: ZONE[zone || 'blue'][0], alpha: o.alpha});
        if (msg) k.text(msg, x + 22 + k.width(hash, 19, MONO, true) + 12, y + 7, {size: 19, ink: INK, alpha: o.alpha});
      },

      /* a folder: a tab with its name, items stacked inside (strings) */
      folder: function (x, y, w, h, name, items, zone) {
        var z = ZONE[zone || 'grey'];
        g.fillStyle = z[1]; k.round(x, y, w * 0.45, 26, 8); g.fill();
        k.box(x, y + 16, w, h - 16, {r: 10, fill: z[1], stroke: z[2]});
        k.text(name, x + 14, y + 46, {size: 19, font: MONO, bold: true, ink: z[0]});
        (items || []).forEach(function (s, i) {
          k.box(x + 12, y + 60 + i * 40, w - 24, 32, {r: 7, shadow: false, stroke: z[2], lw: 1.5});
          k.text(s, x + 22, y + 83 + i * 40, {size: 19, font: MONO, ink: INK});
        });
      },

      /* strike a rectangle through (what is wrong), drawn up to p */
      strike: function (x, y, w, h, p) {
        p = clamp(p == null ? 1 : p, 0, 1);
        g.strokeStyle = '#e0584f'; g.lineWidth = 5; g.lineCap = 'round';
        g.beginPath(); g.moveTo(x, y + h); g.lineTo(x + w * p, y + h - h * p); g.stroke();
      },

      /* the caption band at the bottom of a stop: one sentence, what the reader should see */
      say: function (s) {
        raw(function () { g.fillStyle = S.band || 'rgba(29,36,51,.06)'; g.fillRect(0, 534, 960, 66); });
        var two = k.width(s, 20) > 900;
        k.text(s, 480, two ? 561 : 574, {size: 20, align: 'center', ink: INK, max: 900, lh: 26});
      }
    };
    return k;
  }

  global.StationKit = {make: make, ZONE: ZONE};
  global.StationJours = global.StationJours || {};
})(typeof window !== 'undefined' ? window : this);
