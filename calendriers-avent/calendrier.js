/* Un calendrier de l'avent des enfants : la scène 3D du moteur (`moteur/`), et derrière chaque fenêtre le mot du jour.
 *
 * Le jour vient de la date : rien ne s'ouvre avant le 1er décembre 2026, la fenêtre N s'ouvre le N décembre, tout est
 * ouvert après le 24. `?jour=N` joue le N décembre (pour essayer, ou pour un parent). Les fenêtres déjà ouvertes sont
 * gardées par le navigateur, une liste par calendrier. Le mot se lit en cinq langues ; le bouton d'écoute prend la voix
 * de l'appareil (français, anglais, allemand, chinois) et l'enregistrement de lod.lu pour le luxembourgeois.
 */
(function () {
  'use strict';
  var data = JSON.parse(document.getElementById('monde-data').textContent);
  var q = new URLSearchParams(location.search);
  var now = new Date();
  var today = q.has('jour') ? Number(q.get('jour'))
    : now.getFullYear() > 2026 || (now.getFullYear() === 2026 && now.getMonth() === 11 && now.getDate() > 24) ? 25
    : now.getFullYear() === 2026 && now.getMonth() === 11 ? now.getDate() : 0;
  var KEY = 'calendrier-avent-' + data.id, opened = {};
  try { (JSON.parse(localStorage.getItem(KEY) || '[]')).forEach(function (d) { opened[d] = true; }); } catch (e) {}
  if (q.has('ouvertes')) q.get('ouvertes').split(',').filter(Boolean).forEach(function (d) { opened[Number(d)] = true; });
  function keep() { try { localStorage.setItem(KEY, JSON.stringify(Object.keys(opened).map(Number))); } catch (e) {} }

  var SAY = {overview: 'Le calendrier', hint: 'glisser pour tourner · toucher une fenêtre',
             wait: 'La fenêtre {d} s\'ouvre le {d} décembre.', open: 'Le mot du {d} décembre !'};
  var words = data.world.words || {};
  Object.keys(words).forEach(function (k) { SAY[k] = words[k].fr || words[k].en; });
  function line(k, d) { return SAY[k].split('{d}').join(d); }
  var toast = document.querySelector('.monde-toast'), timer = 0;
  function tell(text) {
    toast.textContent = text; toast.classList.add('on');
    clearTimeout(timer); timer = setTimeout(function () { toast.classList.remove('on'); }, 2600);
  }

  /* le mot du jour : l'image, le mot en grand, puis les quatre autres langues, chacune à écouter */
  var MOTS = {}; data.mots.forEach(function (m) { MOTS[m.day] = m; });
  var LANGUES = [['fr', 'Français', 'fr-FR'], ['en', 'Anglais', 'en-GB'], ['de', 'Allemand', 'de-DE'],
                 ['lb', 'Luxembourgeois', ''], ['zh', 'Chinois', 'zh-CN']];
  var carte = document.getElementById('cal-carte');
  function dire(m, l) {
    if (l[0] === 'lb') { if (m.lb_audio) { var a = new Audio(m.lb_audio); a.play().catch(function () {}); } return; }
    if (!window.speechSynthesis) return;
    speechSynthesis.cancel();
    var u = new SpeechSynthesisUtterance(m[l[0]]); u.lang = l[2]; u.rate = 0.85;
    speechSynthesis.speak(u);
  }
  function montrer(d) {
    var m = MOTS[d];
    if (!m) return;
    carte.innerHTML = '';
    var tete = document.createElement('p'); tete.className = 'cal-jour'; tete.textContent = d + ' décembre'; carte.appendChild(tete);
    var img = document.createElement('div'); img.className = 'cal-image';
    if (/^#/.test(m.e)) img.style.background = m.e; else img.textContent = m.e;
    carte.appendChild(img);
    var ol = document.createElement('ol'); ol.className = 'cal-langues';
    LANGUES.forEach(function (l, i) {
      var li = document.createElement('li'); if (!i) li.className = 'is-fr';
      var b = document.createElement('button'); b.type = 'button'; b.setAttribute('aria-label', 'Écouter en ' + l[1].toLowerCase());
      b.innerHTML = '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 9h4l5-4v14l-5-4H4z"/><path d="M16 8.5a5 5 0 0 1 0 7M18.5 6a8.5 8.5 0 0 1 0 12" fill="none"/></svg>';
      if (l[0] === 'lb' && !m.lb_audio) b.disabled = true;
      b.addEventListener('click', function () { dire(m, l); });
      var mot = document.createElement('b'); mot.textContent = m[l[0]];
      var nom = document.createElement('span'); nom.textContent = l[1];
      li.appendChild(b); li.appendChild(mot); li.appendChild(nom); ol.appendChild(li);
    });
    carte.appendChild(ol);
    var fermer = document.createElement('button'); fermer.type = 'button'; fermer.className = 'cal-fermer'; fermer.textContent = 'Fermer';
    fermer.addEventListener('click', function () { carte.close(); });
    carte.appendChild(fermer);
    if (carte.showModal) carte.showModal(); else carte.setAttribute('open', '');
    dire(m, LANGUES[0]);
  }
  carte.addEventListener('close', function () { if (window.monde && window.monde.back) window.monde.back(); });
  carte.addEventListener('click', function (e) { if (e.target === carte) carte.close(); });

  var scene = window[data.world.mount];
  var world = scene && scene.mount(document.getElementById('monde'), data, {
    tx: function (o) { return o.fr || o.en; },
    say: function (k) { return SAY[k]; },
    state: function (d) { return opened[d] ? 'open' : d <= today ? 'ready' : 'locked'; },
    mark: function (d) { opened[d] = true; keep(); },
    refuse: function (d) { tell(line('wait', d)); },
    enter: function (d) { tell(line('open', d)); montrer(d); }
  });
  window.monde = world;
  window.calendrier = {montrer: montrer, today: today};
  if (!world) document.body.classList.add('no-webgl');
})();
