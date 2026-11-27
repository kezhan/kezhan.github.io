/* Inside a module of the station: the day's practice, played as an animation in a few stops.
 *
 * The scene opens the hatch and moves the camera in; then `StationInterieur.open` lays the module's inside over the
 * 3D view: the day and its title, the canvas where the stop plays (drawn by `StationJours[day]`, kit in
 * `station_kit.js`), the names of the stops, and the way out. A stop plays, holds its last frame, and plays again.
 */
(function (global) {
  'use strict';

  var HOLD = 3;     /* seconds a stop holds its last frame before it plays again */

  function el(tag, cls, parent, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text != null) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  function open(host, o) {
    var tx = o.tx, lang = o.lang || 'en', day = o.day, def = global.StationJours[day];
    var root = el('div', 'st-in', host);
    root.setAttribute('role', 'dialog');
    root.setAttribute('aria-label', tx({en: 'Module ', fr: 'Module '}) + day);
    var shell = el('div', 'st-in-shell', root);
    var head = el('div', 'st-in-head', shell);
    el('span', 'st-in-day', head, tx({en: 'Day ', fr: 'Jour '}) + day);
    el('h2', 'st-in-title', head, o.title || '');
    var shut = el('button', 'st-in-shut', head, '×');
    shut.setAttribute('aria-label', tx({en: 'Leave the module', fr: 'Sortir du module'}));

    var stage = el('div', 'st-in-stage', shell);
    var canvas = el('canvas', 'st-in-canvas', stage);
    var g = canvas.getContext('2d');
    var foot = el('div', 'st-in-foot', shell);
    var prev = el('button', 'st-in-btn', foot, '‹');
    prev.setAttribute('aria-label', tx({en: 'Previous step', fr: 'Étape précédente'}));
    var steps = el('div', 'st-in-steps', foot);
    var next = el('button', 'st-in-btn', foot, '›');
    next.setAttribute('aria-label', tx({en: 'Next step', fr: 'Étape suivante'}));
    var read = el('button', 'st-in-read', foot, tx({en: 'Read the article', fr: 'Lire l\'article'}));

    var stops = def ? def.stops : [];
    /* the room of the day: its ground, its colours, its frame (`station_styles.js`, `station_salles.js`) */
    var style = global.StationStyles ? global.StationStyles.of(day) : null;
    if (style) global.StationStyles.dress(root, style);
    var stop = 0, t0 = 0, frozen = null, raf = 0, alive = true, kit = global.StationKit.make(g, lang, style);
    /* when the row of stops does not fit, only the current one and the next keep their name */
    function fitSteps() { steps.classList.remove('many'); if (steps.scrollWidth > steps.clientWidth + 1) steps.classList.add('many'); }
    var pills = stops.map(function (s, i) {
      var b = el('button', 'st-in-step', steps);
      el('span', 'st-in-num', b, String(i + 1));
      el('span', 'st-in-name', b, tx(s.name));
      b.addEventListener('click', function () { go(i); });
      return b;
    });

    function size() {
      var r = stage.getBoundingClientRect(), dpr = Math.min(2, global.devicePixelRatio || 1);
      var w = Math.max(1, r.width), h = w * 600 / 960;
      if (h > r.height) { h = r.height; w = h * 960 / 600; }
      canvas.style.width = w + 'px'; canvas.style.height = h + 'px';
      canvas.width = Math.round(w * dpr); canvas.height = Math.round(h * dpr);
      fitSteps();
    }
    function draw(t) {
      g.setTransform(canvas.width / 960, 0, 0, canvas.height / 600, 0, 0);
      kit.clear(t);
      if (!def) { kit.text(tx({en: 'This module is still being fitted.', fr: 'Ce module est encore en cours d\'aménagement.'}), 480, 300, {size: 26, align: 'center'}); return; }
      var s = stops[stop];
      s.draw(kit, t);
      if (s.say) kit.say(s.say);
    }
    function go(i) {
      stop = Math.max(0, Math.min(stops.length - 1, i));
      t0 = performance.now(); frozen = null;
      pills.forEach(function (b, j) { b.classList.toggle('on', j === stop); b.classList.toggle('seen', j < stop); b.classList.toggle('next', j === stop + 1); });
      prev.disabled = stop === 0; next.disabled = stop >= stops.length - 1;
      fitSteps();
    }
    function loop(now) {
      if (!alive) return;
      var s = stops[stop], len = s ? (s.dur || 6) : 1;
      var t = frozen != null ? frozen : ((now - t0) / 1000) % (len + HOLD);
      draw(Math.min(t, len));
      raf = requestAnimationFrame(loop);
    }
    function close() {
      if (!alive) return;
      alive = false; cancelAnimationFrame(raf);
      global.removeEventListener('resize', size); global.removeEventListener('keydown', key);
      root.classList.remove('on');
      setTimeout(function () { root.remove(); }, 260);
      if (o.onclose) o.onclose();
    }
    function key(e) {
      if (e.key === 'Escape') close();
      else if (e.key === 'ArrowRight') go(stop + 1);
      else if (e.key === 'ArrowLeft') go(stop - 1);
    }
    prev.addEventListener('click', function () { go(stop - 1); });
    next.addEventListener('click', function () { go(stop + 1); });
    shut.addEventListener('click', close);
    read.addEventListener('click', function () { if (o.onread) o.onread(); });
    root.addEventListener('click', function (e) { if (e.target === root) close(); });
    global.addEventListener('resize', size); global.addEventListener('keydown', key);

    size(); go(o.stop || 0);
    requestAnimationFrame(function () { root.classList.add('on'); size(); });
    raf = requestAnimationFrame(loop);
    return {
      close: close,
      go: go,
      /* for a capture: the stop at its last frame */
      settle: function () { var s = stops[stop]; frozen = s ? (s.dur || 6) : 0; root.classList.add('on'); size(); draw(frozen); },
      at: function (t) { frozen = t; draw(t); },
      count: stops.length
    };
  }

  global.StationInterieur = {open: open};
})(typeof window !== 'undefined' ? window : this);
