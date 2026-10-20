(function(){
  var header = document.getElementById('site-nav');
  var hb = document.getElementById('hambBtn');
  if (hb) hb.addEventListener('click', function(e){ e.stopPropagation(); header.classList.toggle('open'); });
  header.querySelectorAll('.dd .menu').forEach(function(m){ m.addEventListener('click', function(e){ e.stopPropagation(); }); });
  document.addEventListener('click', function(){ header.querySelectorAll('.dd.open').forEach(function(d){ d.classList.remove('open'); }); });

  // La barre de navigation tient sur une ligne : on mesure la place que prennent
  // la marque et les entrees, et tant qu'elles ne rentrent pas, on descend d'un
  // cran (typo reduite, puis icones cachees, puis menu replie derriere le
  // bouton). Mesurer plutot que compter les entrees : un titre long pese autant
  // que trois entrees, c'est la largeur qui decide. Les crans sont dans
  // style.css (.compacte, .serree, .pliee).
  var CRANS = ['compacte', 'serree', 'pliee'];
  var largeurNaturelle = function(el){
    var st = getComputedStyle(el), gap = parseFloat(st.columnGap) || 0, somme = 0, n = 0;
    Array.prototype.forEach.call(el.children, function(e){
      if (getComputedStyle(e).display === 'none') return;
      somme += e.getBoundingClientRect().width; n += 1;
    });
    return somme + gap * Math.max(0, n - 1);
  };
  var ajusterNav = function(){
    var marque = header.querySelector('.brand'), liens = header.querySelector('.navlinks');
    if (!marque || !liens) return;
    var ouvert = header.classList.contains('open');
    header.classList.add('mesuree');
    header.classList.remove('open');
    CRANS.forEach(function(c){ header.classList.remove(c); });
    for (var i = 0; i <= CRANS.length; i++) {
      var st = getComputedStyle(header);
      var place = header.clientWidth - parseFloat(st.paddingLeft) - parseFloat(st.paddingRight);
      var besoin = marque.getBoundingClientRect().width + (parseFloat(st.columnGap) || 0) + largeurNaturelle(liens);
      if (besoin <= place || i === CRANS.length) break;
      header.classList.add(CRANS[i]);
    }
    if (ouvert && header.classList.contains('pliee')) header.classList.add('open');
  };
  ajusterNav();
  // les polices arrivent apres : mesurees en police de secours, les entrees
  // n'ont pas la meme largeur, donc on remesure une fois chargees.
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(ajusterNav);
  var attente = 0;
  window.addEventListener('resize', function(){
    cancelAnimationFrame(attente);
    attente = requestAnimationFrame(ajusterNav);
  });

  // apparition au defilement
  var els = document.querySelectorAll('.reveal');
  if ('IntersectionObserver' in window) {
    var io = new IntersectionObserver(function(entries){
      entries.forEach(function(en){ if (en.isIntersecting) { en.target.classList.add('in'); io.unobserve(en.target); } });
    }, {threshold: .12});
    els.forEach(function(el){ io.observe(el); });
  } else { els.forEach(function(el){ el.classList.add('in'); }); }

  // sommaire lateral : entree active
  var toc = document.querySelector('.toc');
  if (toc) {
    var links = Array.from(toc.querySelectorAll('a[href^="#"]'));
    var targets = links.map(function(a){ try { return document.querySelector(a.getAttribute('href')); } catch (e) { return null; } }).filter(Boolean);
    var onScroll = function(){
      var y = window.scrollY + 160, cur = targets[0];
      targets.forEach(function(t){ if (t.offsetTop <= y) cur = t; });
      links.forEach(function(a){ a.classList.toggle('active', cur && a.getAttribute('href') === '#' + cur.id); });
    };
    window.addEventListener('scroll', onScroll, {passive: true}); onScroll();
  }

  // demos integrees : l'iframe prend la hauteur de son contenu (meme origine) et la suit.
  // Une fois toutes les demos ajustees, la page a grandi : on revient sur l'ancre demandee.
  //
  // Piege : une demo qui remplit son cadre (html,body{height:100%}) a un
  // scrollHeight toujours egal a la hauteur de l'iframe. Une version precedente
  // relevait l'iframe a scrollHeight + 24 a chaque mesure, et l'observateur
  // relancait la mesure a chaque changement : l'iframe grandissait sans fin
  // (vu sur ELIZA dans nlp/site). Donc : hauteur naturelle du document tant
  // que rien ne deborde, et on ne touche a l'iframe que si l'ecart avec sa
  // hauteur actuelle est reel. Controle : python -m sitekit.check <_site> --iframes
  var frames = Array.from(document.querySelectorAll('.demo-embed iframe')), pending = frames.length;
  var realign = function(){
    if (!location.hash) return;
    var t = null; try { t = document.querySelector(location.hash); } catch (e) {}
    if (t) t.scrollIntoView({block: 'start'});
  };
  var mesurer = function(d){
    var html = d.documentElement, body = d.body;
    var cadre = html.clientHeight;
    var deborde = Math.max(body ? body.scrollHeight : 0, html.scrollHeight);
    var naturelle = Math.max(html.offsetHeight, body ? body.offsetHeight : 0);
    return deborde > cadre ? deborde : naturelle;
  };
  frames.forEach(function(f){
    var fit = function(){
      try {
        var d = f.contentDocument; if (!d || !d.documentElement) return;
        var h = mesurer(d);
        if (h <= 80 || Math.abs(h - f.clientHeight) <= 4) return;
        f.style.height = (h + 2) + 'px';
      } catch (e) {}
    };
    var onLoad = function(){
      fit();
      try { if ('ResizeObserver' in window) new ResizeObserver(fit).observe(f.contentDocument.documentElement); } catch (e) {}
      if (--pending === 0) setTimeout(realign, 60);
    };
    var done = false;
    try { done = !!(f.contentDocument && f.contentDocument.readyState === 'complete' && f.contentDocument.body && f.contentDocument.body.children.length); } catch (e) {}
    if (done) onLoad(); else f.addEventListener('load', onLoad);
    setTimeout(fit, 1500);
  });
})();
