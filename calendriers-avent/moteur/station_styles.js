/* The style of a room: every module of the station has its own, fitted to its article.
 *
 * The author, 26-11: « faut faire varier le style dans chaque salle !!! », « tu dois trouver des styles adaptés pour
 * chaque article pour chaque theme ». The 24 animations are drawn with one kit and one palette; a style changes what
 * the room is made of without touching them:
 *
 *   the ground     the material the day is drawn on (an airmail letter, a blueprint, a strip of film), painted once
 *                  in an offscreen canvas, then copied at every frame
 *   the colours    the kit's neutrals are translated on the way to the canvas (`tame`): its paper, its ink, its lines
 *                  become the room's. The zones keep their meaning: inputs amber, work blue, results emerald, red for
 *                  what fails. A dark room turns the pale fills deep and the inks light.
 *   the shapes     corners (`round`), shadows (`shadow`), the weight of a line (`lw`), the face of a title (`font`)
 *   the frame      the shell around the canvas, through CSS variables (`shell`, read by `scene_station.css`)
 *
 * The rooms are in `station_salles.js`: `StationStyles.room(day, {...})`. A day without a room keeps the plain one.
 */
(function (global) {
  'use strict';

  var GROUND = '#f3f5f9';
  var ROOMS = {};

  function hex(v) {
    var m = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.exec(v);
    if (!m) return null;
    var s = m[1].length === 3 ? m[1].replace(/./g, '$&$&') : m[1], n = parseInt(s, 16);
    return [(n >> 16) / 255, ((n >> 8) & 255) / 255, (n & 255) / 255];
  }
  function out(c) {
    return '#' + c.map(function (x) { return ('0' + Math.round(Math.max(0, Math.min(1, x)) * 255).toString(16)).slice(-2); }).join('');
  }
  function hsl(c) {
    var mx = Math.max(c[0], c[1], c[2]), mn = Math.min(c[0], c[1], c[2]), l = (mx + mn) / 2, d = mx - mn, h = 0;
    if (d) {
      h = mx === c[0] ? ((c[1] - c[2]) / d) % 6 : mx === c[1] ? (c[2] - c[0]) / d + 2 : (c[0] - c[1]) / d + 4;
      h = (h * 60 + 360) % 360;
    }
    return [h, d ? d / (1 - Math.abs(2 * l - 1)) : 0, l, d];
  }
  function rgb(h, s, l) {
    var c = (1 - Math.abs(2 * l - 1)) * s, x = c * (1 - Math.abs((h / 60) % 2 - 1)), m = l - c / 2;
    var r = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x] : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
    return [r[0] + m, r[1] + m, r[2] + m];
  }

  /* one colour of the kit, as the room shows it */
  function translate(S, v) {
    var c = hex(v);
    if (!c) return v;
    if (v.toLowerCase() === GROUND) return S.ground;
    var q = hsl(c), l = q[2], grey = l > 0.88 ? q[3] <= 0.06 : q[3] <= 0.2;
    if (grey) {
      if (l > 0.985) return S.paper;
      if (S.dark) return l > 0.88 ? S.paper2 : l > 0.78 ? S.line : l > 0.5 ? S.dim : l > 0.28 ? S.muted : S.ink;
      if (l > 0.5) { var p = hex(S.paper), top = Math.max(p[0], p[1], p[2]); return out([c[0] * p[0] / top, c[1] * p[1] / top, c[2] * p[2] / top]); }
      return l > 0.28 ? (S.muted || v) : (S.ink || v);
    }
    if (!S.dark) return v;
    if (l > 0.82) return out(rgb(q[0], 0.38, S.deep || 0.2));        /* a pale fill becomes a deep one */
    if (l < 0.43) return out(rgb(q[0], Math.min(q[1], 0.8), 0.76));  /* a zone's ink becomes light */
    return v;
  }

  /* every colour set on the context goes through the room, unless `g.__raw` is on (the ground, a terminal) */
  function tame(g, S) {
    var proto = Object.getPrototypeOf(g), memo = {};
    ['fillStyle', 'strokeStyle'].forEach(function (name) {
      var d = Object.getOwnPropertyDescriptor(proto, name);
      if (!d || !d.set) return;
      Object.defineProperty(g, name, {
        configurable: true,
        get: function () { return d.get.call(g); },
        set: function (v) {
          if (typeof v === 'string' && !g.__raw) v = memo[v] || (memo[v] = translate(S, v));
          d.set.call(g, v);
        }
      });
    });
  }

  /* what a room paints its ground with; all in the canvas's 960 by 600 units */
  function brush(g) {
    var P = {
      g: g,
      rect: function (col, x, y, w, h, a) { g.globalAlpha = a == null ? 1 : a; g.fillStyle = col; g.fillRect(x, y, w, h); g.globalAlpha = 1; },
      line: function (col, x1, y1, x2, y2, lw, a) {
        g.globalAlpha = a == null ? 1 : a; g.strokeStyle = col; g.lineWidth = lw || 1;
        g.beginPath(); g.moveTo(x1, y1); g.lineTo(x2, y2); g.stroke(); g.globalAlpha = 1;
      },
      rows: function (col, y0, y1, step, lw, a, x0, x1) { for (var y = y0; y <= y1; y += step) P.line(col, x0 || 0, y, x1 == null ? 960 : x1, y, lw, a); },
      cols: function (col, x0, x1, step, lw, a, y0, y1) { for (var x = x0; x <= x1; x += step) P.line(col, x, y0 || 0, x, y1 == null ? 600 : y1, lw, a); },
      ring: function (col, x, y, r, lw, a, fill) {
        g.globalAlpha = a == null ? 1 : a; g.beginPath(); g.arc(x, y, r, 0, Math.PI * 2);
        if (fill) { g.fillStyle = col; g.fill(); } else { g.strokeStyle = col; g.lineWidth = lw || 1; g.stroke(); }
        g.globalAlpha = 1;
      },
      dots: function (col, step, r, a, x0, y0, x1, y1) {
        g.globalAlpha = a == null ? 1 : a; g.fillStyle = col; g.beginPath();
        for (var y = y0 || 0; y <= (y1 == null ? 600 : y1); y += step) for (var x = x0 || 0; x <= (x1 == null ? 960 : x1); x += step) { g.moveTo(x + r, y); g.arc(x, y, r, 0, Math.PI * 2); }
        g.fill(); g.globalAlpha = 1;
      },
      /* slanted bands of colours in a rectangle: an airmail edge, a hazard tape */
      tape: function (cols, x, y, w, h, band) {
        g.save(); g.beginPath(); g.rect(x, y, w, h); g.clip();
        var n = 0;
        for (var u = x - h; u < x + w + h; u += band, n++) {
          g.fillStyle = cols[n % cols.length]; g.beginPath();
          g.moveTo(u, y + h); g.lineTo(u + h, y); g.lineTo(u + h + band, y); g.lineTo(u + band, y + h); g.closePath(); g.fill();
        }
        g.restore();
      },
      /* the same numbers at every visit: a seed, not Math.random */
      rand: function (seed) { var s = seed || 7; return function () { s = (s * 16807) % 2147483647; return s / 2147483647; }; },
      specks: function (cols, n, seed, rmax, a) {
        var r = P.rand(seed);
        for (var i = 0; i < n; i++) P.ring(cols[i % cols.length], r() * 960, r() * 600, 0.6 + r() * (rmax || 2), 0, (a || 0.3) * (0.4 + r() * 0.6), true);
      },
      text: function (s, x, y, font, col, a, rot, align) {
        g.save(); g.translate(x, y); if (rot) g.rotate(rot);
        g.globalAlpha = a == null ? 1 : a; g.font = font; g.fillStyle = col; g.textAlign = align || 'left'; g.textBaseline = 'alphabetic';
        g.fillText(s, 0, 0); g.restore();
      },
      path: function (col, pts, lw, a, close, fill) {
        g.globalAlpha = a == null ? 1 : a; g.beginPath();
        pts.forEach(function (p, i) { if (i) g.lineTo(p[0], p[1]); else g.moveTo(p[0], p[1]); });
        if (close) g.closePath();
        if (fill) { g.fillStyle = col; g.fill(); } else { g.strokeStyle = col; g.lineWidth = lw || 1; g.lineJoin = 'round'; g.lineCap = 'round'; g.stroke(); }
        g.globalAlpha = 1;
      },
      glow: function (col, x, y, r, a) {
        var gr = g.createRadialGradient(x, y, 0, x, y, r);
        gr.addColorStop(0, col); gr.addColorStop(1, 'rgba(0,0,0,0)');
        g.globalAlpha = a == null ? 1 : a; g.fillStyle = gr; g.fillRect(x - r, y - r, r * 2, r * 2); g.globalAlpha = 1;
      }
    };
    return P;
  }

  /* the ground of the room on the canvas: painted once per size, copied after */
  function ground(S, g, t) {
    var c = g.canvas, key = c.width + 'x' + c.height;
    if (S._key !== key) {
      var off = global.document.createElement('canvas');
      off.width = c.width; off.height = c.height;
      var og = off.getContext('2d');
      og.setTransform(c.width / 960, 0, 0, c.height / 600, 0, 0);
      og.fillStyle = S.ground; og.fillRect(0, 0, 960, 600);
      if (S.paint) S.paint(brush(og));
      S._off = off; S._key = key;
    }
    g.__raw = true;
    g.save(); g.setTransform(1, 0, 0, 1, 0, 0); g.drawImage(S._off, 0, 0); g.restore();
    if (S.live) S.live(S._brush && S._brush.g === g ? S._brush : (S._brush = brush(g)), t || 0);
    g.__raw = false;
  }

  var PLAIN = {key: 'plain', ground: GROUND, paper: '#ffffff', round: 1, shadow: 'soft', lw: 1};

  /* a style with its defaults; `room` keeps it for a day of the station, a board of the factory uses it as it is */
  function style(S) {
    if (S._set) return S;
    S._set = true;
    S.round = S.round == null ? 1 : S.round;
    S.shadow = S.shadow || 'soft';
    S.lw = S.lw || 1;
    S.paper = S.paper || '#ffffff';
    if (S.dark) { S.dim = S.dim || S.muted; S.band = S.band || 'rgba(255,255,255,.07)'; }
    return S;
  }
  function room(day, S) { ROOMS[day] = style(S); }

  /* the frame of the room: its colours and its title face, as CSS variables on the interior's root */
  function dress(el, S) {
    var sh = S.shell || {};
    el.setAttribute('data-room', S.key);
    var vars = {frame: sh.frame, edge: sh.edge, bg: sh.bg, title: sh.title, chip: sh.chip, 'chip-ink': sh.chipInk, font: S.font, go: sh.go, glass: S.ground};
    Object.keys(vars).forEach(function (n) { if (vars[n]) el.style.setProperty('--st-' + n, vars[n]); });
    if (S.font && global.document.fonts && global.document.fonts.load) global.document.fonts.load((S.weight || '700') + ' 32px ' + S.font);
  }

  global.StationStyles = {
    room: room, style: style, of: function (day) { return ROOMS[day] || PLAIN; }, tame: tame, ground: ground, dress: dress,
    keys: function () { return Object.keys(ROOMS).map(function (d) { return ROOMS[d].key; }); }
  };
})(typeof window !== 'undefined' ? window : this);
