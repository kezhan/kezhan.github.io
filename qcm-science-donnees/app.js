/* QCM : réglage, questions, résultat, bilan. Tout tourne dans la page ; le bilan se garde dans le navigateur. */
(function () {
  "use strict";
  var CATS = window.CATEGORIES, QS = window.QUESTIONS;
  var EXAMEN = { nb: 30, minutes: 40, seuil: 15 };
  var LETTRES = ["A", "B", "C", "D"];
  var CLE = "qcm-science-donnees-v1";
  var app = document.getElementById("app");

  /* ---------- Outils ---------- */
  function h(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  function melange(t) { t = t.slice(); for (var i = t.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)); var x = t[i]; t[i] = t[j]; t[j] = x; } return t; }
  function idDe(q) { var x = 0; for (var i = 0; i < q.q.length; i++) x = (x * 31 + q.q.charCodeAt(i)) | 0; return q.c + (x >>> 0).toString(36); }
  function nomCat(id) { for (var i = 0; i < CATS.length; i++) if (CATS[i].id === id) return CATS[i].nom; return id; }
  function pct(a, b) { return b ? Math.round(100 * a / b) + " %" : "–"; }
  function duree(ms) { var s = Math.max(0, Math.round(ms / 1000)); return Math.floor(s / 60) + " min " + String(s % 60).padStart(2, "0") + " s"; }
  function chronoTexte(ms) { var s = Math.max(0, Math.ceil(ms / 1000)); return String(Math.floor(s / 60)).padStart(2, "0") + ":" + String(s % 60).padStart(2, "0"); }
  QS.forEach(function (q) { q.id = idDe(q); });

  /* ---------- Mémoire du navigateur ---------- */
  function lire() { try { return JSON.parse(localStorage.getItem(CLE)) || { stats: {}, sessions: [] }; } catch (e) { return { stats: {}, sessions: [] }; } }
  function ecrire(m) { try { localStorage.setItem(CLE, JSON.stringify(m)); } catch (e) { /* navigation privée : le bilan ne se garde pas */ } }
  var memoire = lire();
  memoire.stats = memoire.stats || {}; memoire.sessions = memoire.sessions || [];
  function noter(q, juste) {
    var s = memoire.stats[q.id] || { vu: 0, bon: 0 };
    s.vu++; if (juste) s.bon++; s.dernier = juste ? 1 : 0;
    memoire.stats[q.id] = s; ecrire(memoire);
  }

  /* ---------- État ---------- */
  var vue = "qcm";
  var reglage = { mode: "entrainement", cats: CATS.map(function (c) { return c.id; }), nb: 20, erreurs: false };
  var session = null, minuteur = null, filtreRevue = "toutes";

  function questionsDe(cats, erreurs) {
    return QS.filter(function (q) {
      if (cats.indexOf(q.c) < 0) return false;
      if (!erreurs) return true;
      var s = memoire.stats[q.id]; return s && s.dernier === 0;
    });
  }
  /* L'examen tire dans chaque catégorie à tour de rôle : toutes sont représentées. */
  function tirageExamen() {
    var paquets = CATS.map(function (c) { return melange(QS.filter(function (q) { return q.c === c.id; })); });
    var out = [];
    while (out.length < EXAMEN.nb && paquets.some(function (p) { return p.length; })) {
      melange(paquets).forEach(function (p) { if (p.length && out.length < EXAMEN.nb) out.push(p.pop()); });
    }
    return melange(out);
  }
  function demarrer(liste, mode) {
    session = {
      mode: mode, i: 0, debut: Date.now(), fin: null,
      limite: mode === "examen" ? Date.now() + EXAMEN.minutes * 60000 : null,
      items: liste.map(function (q) { return { q: q, ordre: melange([0, 1, 2, 3]), choix: null }; })
    };
    filtreRevue = "toutes";
    if (session.limite) minuteur = setInterval(tic, 1000);
    rendre(); window.scrollTo(0, 0);
  }
  function tic() {
    if (!session || session.fin) return;
    var reste = session.limite - Date.now();
    var el = document.querySelector(".chrono");
    if (el) { el.textContent = chronoTexte(reste); el.classList.toggle("court", reste < 5 * 60000); }
    if (reste <= 0) terminer();
  }
  function terminer() {
    clearInterval(minuteur); minuteur = null;
    session.fin = Math.min(Date.now(), session.limite || Infinity);
    if (session.mode !== "examen") {   // un entraînement arrêté en route ne compte que ses questions répondues
      session.items = session.items.filter(function (it) { return it.choix !== null; });
      if (!session.items.length) { session = null; rendre(); return; }
    }
    var bon = 0;
    session.items.forEach(function (it) {
      var juste = it.choix === 0; if (juste) bon++;
      if (session.mode === "examen") noter(it.q, juste);   // à l'entraînement, chaque réponse est notée tout de suite
    });
    var faites = session.items.filter(function (it) { return it.choix !== null; }).length;
    if (faites) {
      memoire.sessions.unshift({ date: session.debut, mode: session.mode, bon: bon, total: session.items.length, ms: session.fin - session.debut });
      memoire.sessions = memoire.sessions.slice(0, 50); ecrire(memoire);
    }
    rendre(); window.scrollTo(0, 0);
  }

  /* ---------- Réglage ---------- */
  function vueReglage() {
    var dispo = questionsDe(reglage.cats, reglage.erreurs).length;
    var nbErreurs = questionsDe(CATS.map(function (c) { return c.id; }), true).length;
    var html = '<section class="carte"><h2>Mode</h2><div class="modes">' +
      '<button class="mode' + (reglage.mode === "entrainement" ? " actif" : "") + '" data-mode="entrainement"><strong>Entraînement</strong><span>Les catégories de votre choix, la correction et la source après chaque question.</span></button>' +
      '<button class="mode' + (reglage.mode === "examen" ? " actif" : "") + '" data-mode="examen"><strong>Examen blanc</strong><span>' + EXAMEN.nb + " questions de toutes les catégories en " + EXAMEN.minutes + " minutes, correction à la fin.</span></button></div></section>";
    if (reglage.mode === "entrainement") {
      html += '<section class="carte"><h2>Catégories</h2><ul class="cats">' + CATS.map(function (c) {
        var n = QS.filter(function (q) { return q.c === c.id; }).length;
        return '<li><label><input type="checkbox" data-cat="' + c.id + '"' + (reglage.cats.indexOf(c.id) >= 0 ? " checked" : "") + "> " + h(c.nom) + '<span class="n">' + n + "</span></label></li>";
      }).join("") + "</ul>" +
        '<div class="ligne-choix"><button class="lien-mini" data-act="tout">Tout cocher</button><button class="lien-mini" data-act="rien">Tout décocher</button></div>' +
        '<div class="ligne-choix"><label>Questions <select data-act="nb">' + [10, 20, 30, 0].map(function (v) {
          return '<option value="' + v + '"' + (reglage.nb === v ? " selected" : "") + ">" + (v || "Toutes") + "</option>";
        }).join("") + "</select></label>" +
        '<label><input type="checkbox" data-act="erreurs"' + (reglage.erreurs ? " checked" : "") + (nbErreurs ? "" : " disabled") + "> Seulement les questions ratées (" + nbErreurs + ")</label></div>" +
        '<div class="actions"><button class="bouton principal" data-act="go"' + (dispo ? "" : " disabled") + ">Commencer</button>" +
        '<span class="discret">' + (dispo ? Math.min(dispo, reglage.nb || dispo) + " questions, tirées au hasard parmi " + dispo : "Aucune question avec ce choix") + "</span></div></section>";
    } else {
      html += '<section class="carte"><h2>Examen blanc</h2><dl class="faits">' +
        "<div><dt>Questions</dt><dd>" + EXAMEN.nb + "</dd></div><div><dt>Durée</dt><dd>" + EXAMEN.minutes + " min</dd></div>" +
        "<div><dt>Seuil de réussite</dt><dd>" + EXAMEN.seuil + " / " + EXAMEN.nb + "</dd></div><div><dt>Réponses par question</dt><dd>4, une seule juste</dd></div></dl>" +
        '<p class="discret">Toutes les catégories sont représentées. On peut revenir sur une question tant que le temps n\'est pas écoulé ; une question sans réponse compte comme fausse.</p>' +
        '<div class="actions"><button class="bouton principal" data-act="go">Commencer l\'examen</button></div></section>';
    }
    return html;
  }

  /* ---------- Question ---------- */
  function vueQuestion() {
    var s = session, it = s.items[s.i], q = it.q, exam = s.mode === "examen";
    var revele = !exam && it.choix !== null;
    var html = '<div class="barre-q"><span class="pos">Question ' + (s.i + 1) + " sur " + s.items.length + '</span><div class="progression"><span style="width:' + (100 * (s.i + (revele ? 1 : 0)) / s.items.length) + '%"></span></div>' +
      (exam ? '<span class="chrono">' + chronoTexte(s.limite - Date.now()) + "</span>" : "") +
      '<button class="lien-mini" data-act="quitter">' + (exam ? "Remettre la copie" : "Arrêter") + "</button></div>";
    html += '<section class="carte"><span class="puce">' + h(nomCat(q.c)) + '</span><p class="enonce">' + h(q.q) + '</p><ul class="reponses">' +
      it.ordre.map(function (k, pos) {
        var cl = "rep";
        if (revele) { if (k === 0) cl += " bonne"; else if (k === it.choix) cl += " fausse"; }
        else if (it.choix === k) cl += " choisie";
        return '<li><button class="' + cl + '" data-rep="' + k + '"' + (revele ? " disabled" : "") + '><span class="l">' + LETTRES[pos] + "</span><span>" + h(q.r[k]) + "</span></button></li>";
      }).join("") + "</ul>";
    if (revele) html += explication(q, it.choix === 0);
    var dernier = s.i === s.items.length - 1;
    html += '<div class="actions">';
    if (exam) {
      html += '<button class="bouton" data-act="prec"' + (s.i ? "" : " disabled") + ">Précédente</button>" +
        (dernier ? '<button class="bouton principal" data-act="fin">Terminer</button>' : '<button class="bouton principal" data-act="suiv">Suivante</button>');
    } else if (revele) {
      html += '<button class="bouton principal" data-act="suiv">' + (dernier ? "Voir le résultat" : "Question suivante") + "</button>";
    }
    html += "</div>";
    if (exam) {
      html += '<div class="grille-nav">' + s.items.map(function (x, n) {
        return '<button data-aller="' + n + '" class="' + (x.choix !== null ? "repondue" : "") + (n === s.i ? " courante" : "") + '">' + (n + 1) + "</button>";
      }).join("") + "</div>";
    }
    html += '<p class="discret raccourcis">Clavier : A à D (ou 1 à 4) pour répondre, Entrée pour continuer.</p></section>';
    return html;
  }
  function explication(q, juste) {
    return '<div class="explication ' + (juste ? "ok" : "ko") + '"><p class="verdict">' + (juste ? "Bonne réponse" : "Réponse attendue : " + h(q.r[0])) + "</p><p>" + h(q.e) + "</p>" +
      '<p class="source">Source : <a href="' + h(q.s[1]) + '" target="_blank" rel="noopener">' + h(q.s[0]) + "</a></p></div>";
  }

  /* ---------- Résultat ---------- */
  function vueResultat() {
    var s = session, exam = s.mode === "examen";
    var bon = s.items.filter(function (it) { return it.choix === 0; }).length, total = s.items.length;
    var html = '<section class="carte"><div class="score"><span class="grand">' + bon + " / " + total + "</span>";
    if (exam) html += bon >= EXAMEN.seuil ? '<span class="badge ok">Seuil atteint</span>' : '<span class="badge ko">Sous le seuil de ' + EXAMEN.seuil + "</span>";
    html += '<span class="discret">' + (exam ? "Examen blanc" : "Entraînement") + ", " + duree(s.fin - s.debut) + "</span></div>";
    var parCat = CATS.map(function (c) {
      var l = s.items.filter(function (it) { return it.q.c === c.id; });
      return { nom: c.nom, n: l.length, bon: l.filter(function (it) { return it.choix === 0; }).length };
    }).filter(function (x) { return x.n; });
    html += '<div class="table-wrap" style="margin-top:18px"><table><thead><tr><th>Catégorie</th><th class="num">Bonnes</th><th class="num">Questions</th><th class="num">Réussite</th></tr></thead><tbody>' +
      parCat.map(function (x) { return "<tr><td>" + h(x.nom) + '</td><td class="num">' + x.bon + '</td><td class="num">' + x.n + '</td><td class="num">' + pct(x.bon, x.n) + "</td></tr>"; }).join("") +
      '</tbody><tfoot><tr><td>Total</td><td class="num">' + bon + '</td><td class="num">' + total + '</td><td class="num">' + pct(bon, total) + "</td></tr></tfoot></table></div>";
    var nbFaux = total - bon;
    html += '<div class="actions"><button class="bouton principal" data-act="nouveau">Nouvelle série</button>' +
      (nbFaux ? '<button class="bouton" data-act="refaire">Refaire les ' + nbFaux + " erreurs</button>" : "") + "</div></section>";
    var liste = s.items.filter(function (it) { return filtreRevue === "toutes" || it.choix !== 0; });
    html += '<section class="carte"><h2>Correction</h2><div class="filtre-revue"><button data-revue="toutes" class="' + (filtreRevue === "toutes" ? "actif" : "") + '">Toutes</button><button data-revue="erreurs" class="' + (filtreRevue === "erreurs" ? "actif" : "") + '">Erreurs</button></div>';
    html += liste.length ? '<ul class="revue">' + liste.map(function (it) {
      var q = it.q, juste = it.choix === 0, n = s.items.indexOf(it) + 1;
      return '<li><details><summary><span class="marque ' + (juste ? "ok" : "ko") + '">' + (juste ? "✓" : "✗") + "</span><span>" + n + ". " + h(q.q) + "</span></summary>" +
        '<div class="corps">' + (juste ? "" : '<p class="ta ko">Votre réponse : <strong>' + (it.choix === null ? "aucune" : h(q.r[it.choix])) + "</strong></p>") +
        '<p class="ta">Réponse attendue : <strong>' + h(q.r[0]) + "</strong></p>" + explication(q, juste) + "</div></details></li>";
    }).join("") + "</ul>" : '<p class="vide">Aucune erreur.</p>';
    return html + "</section>";
  }

  /* ---------- Bilan ---------- */
  function vueBilan() {
    var lignes = CATS.map(function (c) {
      var l = QS.filter(function (q) { return q.c === c.id; });
      var vues = l.filter(function (q) { return memoire.stats[q.id]; });
      var justes = vues.filter(function (q) { return memoire.stats[q.id].dernier === 1; }).length;
      return { nom: c.nom, n: l.length, vues: vues.length, justes: justes };
    });
    var t = lignes.reduce(function (a, x) { return { n: a.n + x.n, vues: a.vues + x.vues, justes: a.justes + x.justes }; }, { n: 0, vues: 0, justes: 0 });
    function jauge(a, b) { return '<span class="jauge"><span style="width:' + (b ? 100 * a / b : 0) + '%"></span></span>'; }
    var html = '<section class="carte"><h2>Par catégorie</h2><div class="table-wrap"><table><thead><tr><th>Catégorie</th><th class="num">Questions</th><th class="num">Vues</th><th class="num">Justes à la dernière réponse</th></tr></thead><tbody>' +
      lignes.map(function (x) { return "<tr><td>" + h(x.nom) + '</td><td class="num">' + x.n + '</td><td class="num">' + x.vues + '</td><td class="num"><span class="taux">' + pct(x.justes, x.vues) + "</span>" + jauge(x.justes, x.vues) + "</td></tr>"; }).join("") +
      '</tbody><tfoot><tr><td>Total</td><td class="num">' + t.n + '</td><td class="num">' + t.vues + '</td><td class="num"><span class="taux">' + pct(t.justes, t.vues) + "</span>" + jauge(t.justes, t.vues) + "</td></tr></tfoot></table></div></section>";
    html += '<section class="carte"><h2>Séries</h2>' + (memoire.sessions.length ? '<div class="table-wrap"><table><thead><tr><th>Date</th><th>Mode</th><th class="num">Score</th><th class="num">Réussite</th><th class="num">Durée</th></tr></thead><tbody>' +
      memoire.sessions.map(function (x) {
        var d = new Date(x.date);
        return "<tr><td>" + d.toLocaleDateString("fr-FR") + " " + d.toLocaleTimeString("fr-FR", { hour: "2-digit", minute: "2-digit" }) + "</td><td>" + (x.mode === "examen" ? "Examen blanc" : "Entraînement") +
          '</td><td class="num">' + x.bon + " / " + x.total + '</td><td class="num">' + pct(x.bon, x.total) + '</td><td class="num">' + duree(x.ms) + "</td></tr>";
      }).join("") + "</tbody></table></div>" : '<p class="vide">Aucune série terminée pour l\'instant.</p>') +
      '<div class="actions"><button class="bouton" data-act="effacer">Effacer le bilan</button><span class="discret">Le bilan reste dans ce navigateur.</span></div></section>';
    return html;
  }

  /* ---------- Rendu et événements ---------- */
  function rendre() {
    document.querySelectorAll(".onglet").forEach(function (b) { var a = b.dataset.vue === vue; b.classList.toggle("actif", a); b.setAttribute("aria-selected", a); });
    if (vue === "bilan") app.innerHTML = vueBilan();
    else if (!session) app.innerHTML = vueReglage();
    else if (session.fin) app.innerHTML = vueResultat();
    else app.innerHTML = vueQuestion();
  }
  function repondre(k) {
    var it = session.items[session.i];
    if (session.mode === "examen") { it.choix = k; rendre(); return; }
    if (it.choix !== null) return;
    it.choix = k; noter(it.q, k === 0); rendre();
  }
  function suivante() {
    if (session.i < session.items.length - 1) { session.i++; rendre(); window.scrollTo(0, 0); }
    else terminer();
  }

  document.querySelector(".onglets").addEventListener("click", function (e) {
    var b = e.target.closest(".onglet"); if (!b) return; vue = b.dataset.vue; rendre();
  });
  app.addEventListener("click", function (e) {
    var b = e.target.closest("button"); if (!b) return;
    if (b.dataset.mode) { reglage.mode = b.dataset.mode; rendre(); return; }
    if (b.dataset.rep !== undefined) { repondre(+b.dataset.rep); return; }
    if (b.dataset.aller !== undefined) { session.i = +b.dataset.aller; rendre(); return; }
    if (b.dataset.revue) { filtreRevue = b.dataset.revue; rendre(); return; }
    var act = b.dataset.act;
    if (act === "tout") { reglage.cats = CATS.map(function (c) { return c.id; }); rendre(); }
    else if (act === "rien") { reglage.cats = []; rendre(); }
    else if (act === "go") {
      if (reglage.mode === "examen") demarrer(tirageExamen(), "examen");
      else { var l = melange(questionsDe(reglage.cats, reglage.erreurs)); demarrer(reglage.nb ? l.slice(0, reglage.nb) : l, "entrainement"); }
    }
    else if (act === "suiv") suivante();
    else if (act === "prec") { session.i--; rendre(); }
    else if (act === "fin") {
      var vides = session.items.filter(function (it) { return it.choix === null; }).length;
      if (!vides || confirm(vides + " question(s) sans réponse. Remettre la copie ?")) terminer();
    }
    else if (act === "quitter") {
      if (session.mode === "examen") { if (confirm("Remettre la copie maintenant ?")) terminer(); }
      else if (session.items.some(function (it) { return it.choix !== null; })) terminer();
      else { session = null; rendre(); }
    }
    else if (act === "nouveau") { session = null; rendre(); }
    else if (act === "refaire") { demarrer(session.items.filter(function (it) { return it.choix !== 0; }).map(function (it) { return it.q; }), "entrainement"); }
    else if (act === "effacer") { if (confirm("Effacer tout le bilan de ce navigateur ?")) { memoire = { stats: {}, sessions: [] }; ecrire(memoire); rendre(); } }
  });
  app.addEventListener("change", function (e) {
    var t = e.target;
    if (t.dataset.cat) { var i = reglage.cats.indexOf(t.dataset.cat); if (t.checked && i < 0) reglage.cats.push(t.dataset.cat); if (!t.checked && i >= 0) reglage.cats.splice(i, 1); rendre(); }
    else if (t.dataset.act === "nb") { reglage.nb = +t.value; rendre(); }
    else if (t.dataset.act === "erreurs") { reglage.erreurs = t.checked; rendre(); }
  });
  document.addEventListener("keydown", function (e) {
    if (vue !== "qcm" || !session || session.fin || e.ctrlKey || e.metaKey || e.altKey) return;
    var k = e.key.toLowerCase(), pos = "abcd".indexOf(k); if (pos < 0) pos = "1234".indexOf(k);
    var it = session.items[session.i];
    if (pos >= 0) { e.preventDefault(); repondre(it.ordre[pos]); }
    else if (e.key === "Enter") {
      var finExamen = session.mode === "examen" && session.i === session.items.length - 1;   // la copie se remet par le bouton
      if (!finExamen && (session.mode === "examen" || it.choix !== null)) { e.preventDefault(); suivante(); }
    }
  });
  rendre();
})();
