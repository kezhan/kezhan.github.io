/*
 * Un ou plusieurs / One or many / Eins oder viele / Eent oder vill : « un chat → des … ». 10 questions.
 * N'existe pas en chinois (pas de pluriel grammatical : pack.jeux.pluriel = null, common.js
 * affiche alors une page de repli).
 * Données : Ile.L().PLURIELS (niveau ≤ choisi, niveau exact en priorité, règles variées)
 * et Ile.L().REGLES_PLURIEL (la règle est rappelée après chaque réponse).
 * Niveau 1 : QCM à 3 choix. Niveau 2 : QCM à 4 choix. Niveau 3 : l'enfant tape le pluriel
 * (2 essais ; en allemand et en luxembourgeois, la majuscule du nom est exigée ; renvoyer la même
 * réponse fausse ne coûte pas d'essai). Score : un point par pluriel trouvé du premier coup.
 * Mauvaises réponses : erreurs vraisemblables propres à chaque langue (T[langue].fautes :
 * -e, -en, -er, Umlaut, inchangé en allemand ; -s / -x en français…), jamais une forme correcte
 * (T[langue].variantes, T[langue].aEviter) ni un autre mot du pack.
 */
(function () {
  'use strict';
  const { el } = Ile;
  const ID = 'pluriel';
  const TOTAL = 10;
  const NB = '\u00A0'; // espace insécable
  // Emojis qui vont par deux : on en montre deux au pluriel, pas trois (des yeux, des pieds…).
  const PAIRES = ['👁️', '🦶', '👂', '✋', '🦵', '💪', '🧤', '🧦', '👟', '👞', '👢'];
  let ecouteurRedim = null; // écouteur « resize » de la partie en cours

  // ---------------------------------------------------------------------------
  // Outils pour fabriquer les erreurs vraisemblables (T[langue].fautes)
  // ---------------------------------------------------------------------------
  const VOY = 'aeiouyäöüéëèêàâîïôûœ';
  // Allemand : voyelle infléchie (Umlaut) de la syllabe accentuée : Hund → Hünd, Katze → Kätze,
  // Apfel → Äpfel, Boot → Böt, Fahrrad → Fahrräd. null si impossible (Tür, Stern, Auto…).
  function umlaut(s) {
    let tige = s;
    let fin = '';
    const t = s.match(/e[lnr]?$/);
    if (t && s.length > t[0].length + 1) { tige = s.slice(0, -t[0].length); fin = t[0]; }
    if (!fin && new RegExp('[' + VOY + ']$', 'i').test(s)) return null; // Auto, Oma, Frau : jamais
    const m = tige.match(new RegExp('([' + VOY + ']+)([^' + VOY + ']*)$', 'i'));
    if (!m) return null;
    const MAP = { a: 'ä', o: 'ö', u: 'ü', au: 'äu', aa: 'ä', oo: 'ö' };
    const v = MAP[m[1].toLowerCase()];
    if (!v) return null;
    const maj = m[1][0] !== m[1][0].toLowerCase();
    const nv = maj ? v[0].toUpperCase() + v.slice(1) : v;
    return tige.slice(0, m.index) + nv + m[2] + fin;
  }
  // Sans Umlaut ni accents, casse conservée : Bälle → Balle, Hënn → Henn, gâteaux → gateaux.
  function sansSignes(s) {
    return String(s).normalize('NFD').replace(/[̀-ͯ]/g, '').normalize('NFC');
  }
  const OUTILS = { umlaut, sansSignes };

  // ---------------------------------------------------------------------------
  // Textes propres au jeu, par langue (mêmes clés partout). Pour chaque langue :
  //   fautes(x, o) : [[forme, note], …] erreurs possibles pour le pluriel x = { s, p, g, regle }
  //                  (note 3 = très vraisemblable, 2 = vraisemblable, 1 = possible) ;
  //   variantes    : autres pluriels corrects (acceptés à la saisie, jamais proposés comme erreur) ;
  //   aEviter      : formes à ne jamais proposer (vrais mots d'une autre sorte, formes régionales…).
  // ---------------------------------------------------------------------------
  const T = {
    en: {
      cite: (t) => '“' + String(t).replace(/ /g, NB) + '”',
      consigne: (x, saisie) => 'Now there’s more than one! ' + (saisie ? 'Type the plural.' : 'Choose the right spelling.'),
      ecouter: 'Listen to the word and the question',
      plusieurs: 'several pictures',
      aTrouver: '(word to find)',
      choix: 'Choose the plural',
      ecris: (detP) => 'Type the plural: ' + detP + ' …',
      touches: 'Special letters',
      inserer: (c) => 'Type “' + c + '”',
      valider: 'Check',
      vide: 'Type the word in the box first.',
      non: (gp) => 'No, we write ' + T.en.cite(gp) + '.',
      bravo: 'Well done! Perfect spelling.',
      oui: 'Yes, that’s right!',
      variante: (gp) => 'Yes! We usually write ' + T.en.cite(gp) + '.',
      onEcrit: (gp) => 'We write ' + T.en.cite(gp) + '.',
      accents: 'Watch out for the accents!',
      majuscule: 'Watch out for capital letters!',
      marque: 'That’s still the word for one! Type the plural.',
      fin: 'Not quite… Look carefully at the end of the word!',
      regarde: 'Not quite… Look carefully at the whole word!',
      regle: 'The rule',
      suivant: 'Next',
      resultat: 'See my score',
      aRevoir: (liste) => 'To practise: ' + liste,
      parfait: 'Every plural is right!',
      fautes: (x) => {
        const { s, p } = x;
        const c = [[s, 3], [s + 's', 3], [s + 'z', 1]]; // cat, boxs, babys, childs ; catz
        const yCons = /[^aeiou]y$/.test(s);
        const yVoy = /[aeiou]y$/.test(s);
        if (!/e$/.test(s)) c.push([s + 'es', /(s|x|z|ch|sh|o)$/.test(s) || yCons ? 3 : 2]); // pianoes, babyes, cates
        if (/ss$/.test(s)) c.push([s.slice(0, -1) + 'es', 2]); // dreses
        if (yVoy) c.push([s.slice(0, /ey$/.test(s) ? -2 : -1) + 'ies', 3]); // monkies, kies, boies
        if (/fe?$/.test(s)) { c.push([s.replace(/fe?$/, 'fes'), 2]); c.push([s.replace(/fe?$/, 'vs'), 1]); } // leafes
        if (p !== s && !p.startsWith(s) && !/(ies|ves)$/.test(p)) { // irrégulier : childrens, feets, mices
          c.push([p + 's', 2]);
          if (!/e$/.test(s)) c.push([s + 'es', 2]);
        }
        return c;
      },
      variantes: { scarf: ['scarfs'], fish: ['fishes'], person: ['persons'] },
      aEviter: ['hates', 'cares', 'stares', 'manes', 'belles', 'antes', 'buss', 'busses', 'peoples', 'hoofs'],
    },
    zh: {
      // Jeu absent en chinois (pas de pluriel) : textes présents pour garder les mêmes clés.
      cite: (t) => '“' + t + '”',
      consigne: (x, saisie) => '现在有很多个！' + (saisie ? '写出复数。' : '选出正确的复数。'),
      ecouter: '听词语和题目',
      plusieurs: '很多图片',
      aTrouver: '（要找的词）',
      choix: '选出复数',
      ecris: (detP) => '写出复数：' + detP + '……',
      touches: '特殊字母',
      inserer: (c) => '写“' + c + '”',
      valider: '检查',
      vide: '先在格子里写词语。',
      non: (gp) => '不对，应该写' + T.zh.cite(gp) + '。',
      bravo: '真棒！写对了。',
      oui: '对了！',
      variante: (gp) => '对了！一般写' + T.zh.cite(gp) + '。',
      onEcrit: (gp) => '应该写' + T.zh.cite(gp) + '。',
      accents: '注意字母上的符号！',
      majuscule: '注意大小写！',
      marque: '这还是一个的说法！写出复数。',
      fin: '差一点……看看词语的结尾！',
      regarde: '差一点……把整个词仔细看一看！',
      regle: '规则',
      suivant: '下一题',
      resultat: '看看我的成绩',
      aRevoir: (liste) => '要复习：' + liste,
      parfait: '全部答对了！',
      // Titre de l'en-tête sur la page de repli (le pack n'a pas de titre : jeux.pluriel = null).
      fautes: () => [],
      variantes: {},
      aEviter: [],
    },
    de: {
      cite: (t) => '„' + String(t).replace(/ /g, NB) + '“',
      consigne: (x, saisie) => 'Jetzt sind es mehrere! ' + (saisie ? 'Schreib die Mehrzahl.' : 'Wähle die richtige Mehrzahl.'),
      ecouter: 'Wort und Aufgabe anhören',
      plusieurs: 'mehrere Bilder',
      aTrouver: '(gesuchtes Wort)',
      choix: 'Wähle die Mehrzahl',
      ecris: (detP) => 'Schreib die Mehrzahl: ' + detP + ' …',
      touches: 'Besondere Buchstaben',
      inserer: (c) => '„' + c + '“ schreiben',
      valider: 'Prüfen',
      vide: 'Schreib zuerst das Wort in das Feld.',
      non: (gp) => 'Nein, es heißt ' + T.de.cite(gp) + '.',
      bravo: 'Super! Richtig geschrieben.',
      oui: 'Ja, genau!',
      variante: (gp) => 'Richtig! Meistens schreibt man ' + T.de.cite(gp) + '.',
      onEcrit: (gp) => 'Man schreibt ' + T.de.cite(gp) + '.',
      accents: 'Achte auf ä, ö, ü und ß!',
      majuscule: 'Achtung: Nomen schreibt man groß!',
      marque: 'Das ist noch die Einzahl! Schreib die Mehrzahl.',
      fin: 'Nicht ganz … Schau dir das Ende des Wortes genau an!',
      regarde: 'Nicht ganz … Schau dir das ganze Wort genau an!',
      regle: 'Die Regel',
      suivant: 'Weiter',
      resultat: 'Mein Ergebnis',
      aRevoir: (liste) => 'Zum Üben: ' + liste,
      parfait: 'Du hast alle Wörter richtig in die Mehrzahl gesetzt!',
      // Erreurs d'apprenant : singulier inchangé, -e, -en, -er, -s, Umlaut oublié ou ajouté.
      // Jamais le datif pluriel (Hunden, Vögeln) : c'est une vraie forme, d'un autre cas.
      fautes: (x, o) => {
        const { s, p } = x;
        const c = [[s, 3]];
        const finE = /e$/.test(s);
        const finEl = /e[lr]$/.test(s);
        const finEn = /en$/.test(s);
        const finVoy = /[aiouy]$/.test(s);
        const finS = /(s|ß|x|z)$/.test(s);
        const um = o.umlaut(s);
        if (!finE && !finEl && !finEn && !finVoy) {
          c.push([s + 'e', 3], [s + 'en', 2], [s + 'er', 2]);
          if (!finS) c.push([s + 's', 2]);
          if (um) c.push([um + 'e', 3], [um + 'er', 2]);
        }
        if (finE) {
          c.push([s + 's', 2]);
          if (um) c.push([um, 1], [um + 'n', 1]);
        }
        if (finEl || finEn) {
          c.push([s + 's', 2], [s + 'e', 1]);
          if (finEl) c.push([s + 'n', 2]);
          if (um) c.push([um, 3]);
        }
        if (finVoy) {
          c.push([s + 's', 3]);
          if (!/y$/.test(s)) c.push([s + 'n', 1]);
          if (/a$/.test(s)) c.push([s.slice(0, -1) + 'en', 2]); // Zebren, Open (comme Pizzen)
          if (/y$/.test(s)) c.push([s.slice(0, -1) + 'ies', 3]); // Babies (à l'anglaise)
        }
        const su = o.sansSignes(p);
        if (su !== p && o.sansSignes(s) === s) c.push([su, 3]); // Balle, Bucher (Umlaut oublié)
        return c.filter(([f]) => f !== p + 'n');
      },
      variantes: { Papagei: ['Papageie'] },
      aEviter: ['Fischer', 'Schäfer', 'Hüter', 'Küchen', 'Buche', 'Buchen', 'Hause', 'Mannen', 'Ballen',
        'Mauser', 'Türe', 'Vogeln', 'Vögeln', 'Better', 'Oman', 'Omen', 'Mädchens', 'Eichhörnchens', 'Kinde', 'Fraun'],
    },
    lb: {
      cite: (t) => '„' + String(t).replace(/ /g, NB) + '“',
      consigne: (x, saisie) => 'Net eent, mee vill! ' + (saisie ? 'Schreif d’Wuert am Plural.' : 'Wiel de richtege Plural.'),
      ecouter: 'Lauschteren',
      plusieurs: 'vill Biller',
      aTrouver: '(d’Wuert)',
      choix: 'Wiel de Plural',
      ecris: (detP) => 'Schreif de Plural: ' + detP + ' …',
      touches: 'Speziell Buschtawen',
      inserer: (c) => '„' + c + '“ schreiwen',
      valider: 'Iwwerpréiwen',
      vide: 'Schreif d’Wuert fir d’éischt an d’Feld.',
      non: (gp) => 'Nee, et heescht ' + T.lb.cite(gp) + '.',
      bravo: 'Super! Alles richteg geschriwwen.',
      oui: 'Jo, genee!',
      variante: (gp) => 'Richteg! Meeschtens schreift een ' + T.lb.cite(gp) + '.',
      onEcrit: (gp) => 'Esou schreift een et: ' + T.lb.cite(gp) + '.',
      accents: 'Opgepasst op ä, é an ë!',
      majuscule: 'Opgepasst: D’Wuert fänkt mat engem grousse Buschtaf un!',
      marque: 'Dat ass nach de Singular! Schreif de Plural.',
      fin: 'Net ganz … Kuck gutt op d’Enn vum Wuert!',
      regarde: 'Net ganz … Kuck gutt op dat ganzt Wuert!',
      regle: 'D’Regel',
      suivant: 'Nächst Wuert',
      resultat: 'Mäi Resultat',
      aRevoir: (liste) => 'Fir ze iwwen: ' + liste,
      parfait: 'Kee Feeler! Gutt gemaach!',
      // Erreurs d'apprenant : singulier inchangé, -en, -er, -n, fin ajoutée à un pluriel déjà
      // changé (Hënnen), ä / é / ë oubliés. Pas de -s (pluriel allemand ou français, pas luxembourgeois).
      fautes: (x, o) => {
        const { s, p } = x;
        const c = [[s, 3]];
        const finVoy = /[aeiouéë]$/.test(s);
        const finEr = /[^aeiouäéëi]er$/.test(s); // Fliger (mais pas Dier, Bier)
        if (!finVoy && !finEr) c.push([s + 'en', 3], [s + 'er', 2]);
        if (finEr) c.push([s + 'en', 2], [s + 'n', 1]);
        if (finVoy) c.push([s + 'en', 3], [s + 'n', 2], [s + 'er', 1]);
        const change = p !== s && !p.startsWith(s);
        if (change && !/(en|er)$/.test(p)) c.push([p + 'en', 2]); // Hënnen, Beemen
        const su = o.sansSignes(p);
        if (su !== p && o.sansSignes(s) === s) c.push([su, 1]); // Henn, Hann
        return c;
      },
      variantes: {},
      aEviter: ['Fëschen', 'Fëscher', 'Schofer', 'Kéier', 'Een', 'Ballen', 'Appel'],
    },
    fr: {
      cite: (t) => '«' + NB + String(t).replace(/ /g, NB) + NB + '»',
      consigne: (x, saisie) => 'Il y en a plusieurs' + NB + '! ' + (saisie ? 'Écris le mot au pluriel.' : 'Choisis la bonne écriture.'),
      ecouter: 'Écouter le mot et la consigne',
      plusieurs: 'plusieurs images',
      aTrouver: '(mot à trouver)',
      choix: 'Choisis le pluriel',
      ecris: (detP) => 'Écris le pluriel' + NB + ': ' + detP + ' …',
      touches: 'Lettres spéciales',
      inserer: (c) => 'Écrire «' + NB + c + NB + '»',
      valider: 'Valider',
      vide: 'Écris d’abord le mot dans la case.',
      non: (gp) => 'Non, on écrit ' + T.fr.cite(gp) + '.',
      bravo: 'Bravo' + NB + '! C’est bien écrit.',
      oui: 'Oui, c’est ça' + NB + '!',
      variante: (gp) => 'Oui' + NB + '! On écrit plus souvent ' + T.fr.cite(gp) + '.',
      onEcrit: (gp) => 'On écrit ' + T.fr.cite(gp) + '.',
      accents: 'Attention aux accents' + NB + '!',
      majuscule: 'Attention à la majuscule' + NB + '!',
      marque: 'C’est encore le singulier' + NB + '! Écris le pluriel.',
      fin: 'Pas tout à fait… Regarde bien la fin du mot' + NB + '!',
      regarde: 'Pas tout à fait… Regarde bien tout le mot' + NB + '!',
      regle: 'La règle',
      suivant: 'Suivant',
      resultat: 'Voir mon résultat',
      aRevoir: (liste) => 'À revoir' + NB + ': ' + liste,
      parfait: 'Tous les pluriels sont justes' + NB + '!',
      // Erreurs d'élève : singulier inchangé, -s au lieu de -x (et inversement), -al → -als,
      // -aus, marque ajoutée à un mot invariable, accents oubliés.
      fautes: (x, o) => {
        const { s, p } = x;
        const c = [[s, 3]];
        if (!/[sxz]$/.test(s)) {
          c.push([s + 's', 3], [s + 'x', /(au|eu|ou)$/.test(s) ? 3 : 2], [s + 'es', 1]);
          if (/al$/.test(s)) c.push([s.slice(0, -1) + 'ux', 3], [s.slice(0, -1) + 'us', 2]); // bal → baux, cheval → chevaus
          if (/ail$/.test(s)) c.push([s.slice(0, -2) + 'ux', 3]); // rail → raux
        } else {
          const d = s.slice(-1);
          ['s', 'x', 'z'].filter((a) => a !== d).forEach((a) => c.push([s.slice(0, -1) + a, 2])); // sourix, nes
          c.push([s + 'es', 1]);
        }
        const su = o.sansSignes(p);
        if (su !== p) c.push([su, 1]); // gateaux, cles
        return c;
      },
      variantes: {},
      aEviter: ['baux', 'pris', 'loupes', 'brax'],
    },
  };

  // ---------------------------------------------------------------------------
  // Comparaison de la saisie
  // ---------------------------------------------------------------------------
  const lettres = (s) => Array.from(String(s).normalize('NFC'));
  const norme = (s) => String(s).normalize('NFC').trim().replace(/[’']/g, '’').replace(/\s+/g, ' ');

  // ---------------------------------------------------------------------------
  // Distracteurs : erreurs de T[langue].fautes, filtrées et triées (les plus vraisemblables
  // d'abord, au hasard entre deux notes égales). Jamais la bonne réponse, une variante correcte,
  // une forme à éviter ni un autre mot du pack (« die Buche » n'est pas une erreur, c'est un arbre).
  // ---------------------------------------------------------------------------
  function motsDuPack(L, locale) {
    const mots = new Set();
    const ajoute = (w) => { if (w) String(w).split(/[\s.,!?;:«»„“”"()]+/).forEach((m) => { if (m) mots.add(m.toLocaleLowerCase(locale)); }); };
    (L.MOTS || []).forEach((m) => { ajoute(m.mot); ajoute(m.pluriel); });
    Object.keys(L.MOTS_SIMPLES || {}).forEach((k) => L.MOTS_SIMPLES[k].forEach(ajoute));
    (L.RIMES || []).forEach((f) => f.mots.forEach(ajoute));
    (L.CONTRAIRES || []).forEach((c) => { ajoute(c.a); ajoute(c.b); });
    (L.PHRASES || []).forEach((ph) => ajoute(ph.texte));
    (L.PLURIELS || []).forEach((x) => { ajoute(x.s); ajoute(x.p); });
    return mots;
  }

  function fabriqueDistracteurs(L, tx) {
    const locale = L.tts;
    const bas = (w) => norme(w).toLocaleLowerCase(locale);
    const connus = motsDuPack(L, locale);
    const aEviter = new Set((tx.aEviter || []).map(bas));
    return function candidats(x) {
      const p = norme(x.p);
      const s = norme(x.s);
      const variantes = ((tx.variantes || {})[x.s] || []).map(bas);
      const vus = new Map();
      (tx.fautes(x, OUTILS) || []).forEach(([f, note]) => {
        const w = norme(f);
        const b = bas(w);
        if (!w || b === bas(p) || variantes.indexOf(b) !== -1 || aEviter.has(b)) return;
        if (/(.)\1\1/u.test(b)) return; // trois lettres identiques : jamais vraisemblable
        if (b !== bas(s) && connus.has(b)) return; // un vrai mot d'une autre sorte
        if (!vus.has(w) || vus.get(w) < note) vus.set(w, note);
      });
      const liste = Ile.shuffle(Array.from(vus.entries()).map(([forme, score]) => ({ forme, score })));
      return liste.sort((a, b) => b.score - a.score); // tri stable : le hasard départage les égalités
    };
  }

  // Tirage des 10 questions : niveau exact d'abord, en alternant les règles. Pour un QCM, les mots
  // qui ont assez d'erreurs vraisemblables passent en premier (les autres ne servent qu'en secours).
  function tirage(level, candidats, nbChoix) {
    const L = Ile.L();
    const tous = (L.PLURIELS || []).filter((x) => x.s && x.p && x.niveau <= level);
    const bon = (x) => !nbChoix || candidats(x).filter((c) => c.score >= 1).length >= nbChoix - 1;
    const pris = new Set();
    const liste = [];
    function tourner(items) {
      const groupes = {};
      Ile.shuffle(items).forEach((x) => { (groupes[x.regle] = groupes[x.regle] || []).push(x); });
      const ordre = Ile.shuffle(Object.keys(groupes));
      let encore = true;
      while (liste.length < TOTAL && encore) {
        encore = false;
        ordre.forEach((r) => {
          const g = groupes[r];
          while (g.length && pris.has(g[0].s)) g.shift();
          if (liste.length < TOTAL && g.length) {
            const x = g.shift();
            pris.add(x.s);
            liste.push(x);
            encore = true;
          }
        });
      }
    }
    tourner(tous.filter((x) => x.niveau === level && bon(x)));
    tourner(tous.filter((x) => x.niveau < level && bon(x)));
    tourner(tous.filter((x) => x.niveau === level));
    tourner(tous.filter((x) => x.niveau < level));
    return Ile.shuffle(liste);
  }

  // ---------------------------------------------------------------------------
  // Mise en page : un mot long doit tenir sur une ligne (pas de coupure au milieu).
  // ---------------------------------------------------------------------------
  function largeurTexte(texte, cs) {
    const m = document.createElement('span');
    m.setAttribute('aria-hidden', 'true');
    m.style.cssText = 'position:absolute;left:-9999px;top:0;visibility:hidden;white-space:nowrap;';
    m.style.fontFamily = cs.fontFamily;
    m.style.fontSize = cs.fontSize;
    m.style.fontWeight = cs.fontWeight;
    m.style.letterSpacing = cs.letterSpacing;
    m.textContent = texte;
    document.body.appendChild(m);
    const w = m.getBoundingClientRect().width;
    m.remove();
    return w;
  }
  // Texte réellement affiché (sans les précisions réservées aux lecteurs d'écran).
  function texteVisible(node) {
    if (node.nodeType === 3) return node.nodeValue;
    if (node.nodeType !== 1 || node.classList.contains('sr-only')) return '';
    return Array.from(node.childNodes).map(texteVisible).join('');
  }
  // Taille de police (px) pour que le plus long mot de node tienne sur une ligne ;
  // avec entier, pour que tout le texte tienne sur une ligne (sinon, repli mot par mot).
  function tailleIdeale(node, minPx, entier) {
    node.style.fontSize = '';
    const cs = window.getComputedStyle(node);
    const taille = parseFloat(cs.fontSize) || 16;
    const dispo = node.clientWidth - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight) - 4;
    const texte = texteVisible(node).trim().replace(/\s+/g, ' ');
    const morceaux = entier ? [texte] : texte.split(' ').filter(Boolean);
    if (!texte || dispo <= 0) return taille;
    const plusLong = Math.max.apply(null, morceaux.map((w) => largeurTexte(w, cs)));
    if (plusLong <= dispo) return taille;
    const t = Math.floor(taille * dispo / plusLong);
    if (entier && t < minPx) return tailleIdeale(node, minPx, false);
    return Math.max(minPx, t);
  }
  function ajuster(node, minPx, entier) {
    if (!node || !node.isConnected) return;
    node.style.fontSize = tailleIdeale(node, minPx, entier) + 'px';
  }
  // Même taille pour tout un groupe (les choix d'un QCM restent homogènes).
  function ajusterGroupe(nodes, minPx) {
    const liste = Array.from(nodes).filter((n) => n.isConnected);
    if (!liste.length) return;
    const t = Math.min.apply(null, liste.map((n) => tailleIdeale(n, minPx)));
    liste.forEach((n) => { n.style.fontSize = t + 'px'; });
  }

  // Règle du pack, sans coupure de ligne au milieu d'une terminaison (« - / er ») ni d'un
  // « a → ä » ; en français, espace insécable avant « : » (données du pack : on ne les modifie pas).
  function texteRegle(texte, code) {
    let t = String(texte);
    if (code === 'fr') t = t.replace(/ ([:;!?»])/g, NB + '$1').replace(/« /g, '«' + NB);
    const noeuds = [];
    let k = 0;
    t.replace(/-\p{L}+|\p{L}+ → \p{L}+/gu, (m, pos) => {
      if (pos > k) noeuds.push(t.slice(k, pos));
      noeuds.push(el('span', { class: 'pluriel-insecable', text: m }));
      k = pos + m.length;
      return m;
    });
    if (k < t.length) noeuds.push(t.slice(k));
    return noeuds;
  }

  // Touches spéciales sur plusieurs lignes : lignes équilibrées (4 + 3 plutôt que 6 + 1).
  function equilibrer(clavier) {
    if (!clavier || !clavier.isConnected || clavier.hidden) return;
    clavier.style.maxWidth = '';
    const btns = clavier.querySelectorAll('button');
    if (btns.length < 2) return;
    const w = btns[0].getBoundingClientRect().width;
    const gap = parseFloat(window.getComputedStyle(clavier).columnGap) || 0;
    const parLigne = Math.max(1, Math.floor((clavier.clientWidth + gap) / (w + gap)));
    if (!w || parLigne >= btns.length) return;
    const n = Math.ceil(btns.length / Math.ceil(btns.length / parLigne));
    clavier.style.maxWidth = Math.ceil(n * (w + gap) - gap + 1) + 'px';
  }

  // Lettres du pluriel reprises du singulier (plus longue sous-suite commune) : true = gardée.
  function lettresGardees(s, p) {
    const a = lettres(s);
    const b = lettres(p);
    const lcs = Array.from({ length: a.length + 1 }, () => new Array(b.length + 1).fill(0));
    for (let i = a.length - 1; i >= 0; i--) {
      for (let j = b.length - 1; j >= 0; j--) {
        lcs[i][j] = a[i] === b[j] ? lcs[i + 1][j + 1] + 1 : Math.max(lcs[i + 1][j], lcs[i][j + 1]);
      }
    }
    const garde = new Array(b.length).fill(false);
    let i = 0;
    let j = 0;
    while (i < a.length && j < b.length) {
      if (a[i] === b[j]) { garde[j] = true; i++; j++; } else if (lcs[i + 1][j] >= lcs[i][j + 1]) i++; else j++;
    }
    return garde;
  }
  // Le pluriel change-t-il aussi à l'intérieur du mot (mouse → mice, Hand → Hände, Apfel → Äpfel),
  // et pas seulement à la fin (cat → cats, Hund → Hunde) ? L'indice le dit.
  function changeDedans(s, p) {
    const garde = lettresGardees(s, p);
    return garde.some((g, k) => !g && garde.slice(k + 1).some(Boolean));
  }

  // Le pluriel, avec ce qui change mis en valeur : chat|s, chev|aux, B|ä|ll|e, H|ë|n|n, m|ic|e.
  // (plus longue sous-suite commune entre le singulier et le pluriel ; le reste est surligné)
  function motAvecChangements(s, p, lang) {
    const b = lettres(p);
    const garde = lettresGardees(s, p);
    const span = el('span', { class: 'pluriel-mot', lang });
    let k = 0;
    while (k < b.length) {
      const g = garde[k];
      let morceau = '';
      while (k < b.length && garde[k] === g) morceau += b[k++];
      span.appendChild(g ? document.createTextNode(morceau) : el('span', { class: 'pluriel-fin', text: morceau }));
    }
    return span;
  }

  Ile.mountGame({
    id: ID,
    onStart(level, root) {
      const tx = Ile.txt(T);
      const L = Ile.L();
      const lang = L.htmlLang || Ile.getLang();
      const locale = L.tts;
      const g = Ile.game(ID);
      const saisieLibre = level >= 3;
      const nbChoix = level === 1 ? 3 : 4;
      const candidats = fabriqueDistracteurs(L, tx);
      const regles = L.REGLES_PLURIEL || {};
      const touches = L.touchesSpeciales || [];
      const bas = (w) => norme(w).toLocaleLowerCase(locale);
      const plat = (w) => Ile.sansAccents(bas(w).replace(/ß/g, 'ss'));
      const elide = (det) => /[’']$/.test(det);

      const panel = el('section', { class: 'panel pluriel', 'aria-label': g ? g.titre : '' });
      const ajusterMaintenant = () => {
        if (!panel.isConnected) return;
        panel.querySelectorAll('.pluriel-texte').forEach((n) => ajuster(n, 16, true));
        // Trois choix sur une ligne tant que les mots y tiennent assez gros ; sinon deux + un
        // (Schwäinen, Bananes sur téléphone).
        const grille = panel.querySelector('.choices');
        const choix = panel.querySelectorAll('.choice');
        if (grille) {
          grille.classList.remove('choices--serre');
          ajusterGroupe(choix, 14);
          if (grille.classList.contains('choices--3') && choix.length && parseFloat(choix[0].style.fontSize) < 17) {
            grille.classList.add('choices--serre');
            ajusterGroupe(choix, 14);
          }
        }
        equilibrer(panel.querySelector('.pluriel-touches'));
      };
      // Tout de suite, puis à l'image suivante (la barre de défilement a pu changer la largeur).
      const ajusterTout = () => {
        ajusterMaintenant();
        if (window.requestAnimationFrame) window.requestAnimationFrame(ajusterMaintenant);
      };
      // Rotation de la tablette, police chargée plus tard : on réajuste tant que la partie est affichée.
      const auRedimensionnement = () => {
        if (!panel.isConnected) { window.removeEventListener('resize', auRedimensionnement); return; }
        ajusterTout();
      };
      // Un seul écouteur à la fois : celui de la partie précédente (niveau, langue, rejouer) est retiré.
      if (ecouteurRedim) window.removeEventListener('resize', ecouteurRedim);
      ecouteurRedim = auRedimensionnement;
      window.addEventListener('resize', auRedimensionnement);
      if (document.fonts && document.fonts.ready) document.fonts.ready.then(() => { if (panel.isConnected) ajusterTout(); });
      root.appendChild(panel);

      const liste = tirage(level, candidats, saisieLibre ? 0 : nbChoix);
      let i = 0;
      let score = 0; // un point par pluriel trouvé du premier coup
      const aRevoir = [];

      function poser() {
        if (!panel.isConnected) return; // partie remplacée (niveau, langue, rejouer)
        if (i >= liste.length) { terminer(); return; }
        const x = liste[i];
        const gs = Ile.groupe(x.detS, x.s);
        const gp = Ile.groupe(x.detP, x.p);
        const jeton = {};
        panel._jeton = jeton;
        const actif = () => panel.isConnected && panel._jeton === jeton;
        let fini = false;
        let essais = 0;

        panel.querySelectorAll(':scope > :not(.progress)').forEach((n) => n.remove());
        Ile.progress(panel, i, liste.length, score);
        panel.dataset.question = String(i);
        panel.dataset.etat = 'question';
        delete panel.dataset.verdict;

        // Cartes « un … » → « des … ». Taille du texte selon le plus long des deux groupes.
        const lg = Math.max(lettres(gp).length, lettres(gs).length);
        const classeTexte = 'pluriel-texte' + (lg > 11 ? ' pluriel-texte--long' : '') + (lg > 14 ? ' pluriel-texte--tres-long' : '')
          + (x.emoji ? '' : ' pluriel-texte--sans-image');
        const carteUn = el('div', { class: 'pluriel-carte pluriel-carte--un' }, [
          x.emoji ? el('div', { class: 'pluriel-images pluriel-emoji', role: 'img', 'aria-label': gs, text: x.emoji }) : null,
          el('p', { class: classeTexte, lang }, [
            el('span', { class: 'pluriel-art', text: x.detS }), elide(x.detS) ? '' : ' ',
            el('span', { class: 'pluriel-singulier', text: x.s }),
          ]),
        ]);
        const nbImages = PAIRES.indexOf(x.emoji) !== -1 ? 2 : 3;
        const trou = el('span', { class: 'pluriel-trou' }, ['?', el('span', { class: 'sr-only', text: ' ' + tx.aTrouver })]);
        const texteDes = el('p', { class: classeTexte, lang }, [
          el('span', { class: 'pluriel-art', text: x.detP }), elide(x.detP) ? '' : ' ', trou,
        ]);
        const carteDes = el('div', { class: 'pluriel-carte pluriel-carte--des' }, [
          x.emoji ? el('div', { class: 'pluriel-images pluriel-images--des pluriel-emoji', role: 'img', 'aria-label': tx.plusieurs },
            Array.from({ length: nbImages }, () => el('span', { text: x.emoji }))) : null,
          texteDes,
        ]);
        const duo = el('div', { class: 'pluriel-duo' }, [
          carteUn, el('span', { class: 'pluriel-fleche', 'aria-hidden': 'true', text: '➜' }), carteDes,
        ]);

        const texteConsigne = tx.consigne(x, saisieLibre);
        const consigne = el('div', { class: 'pluriel-entete' }, [
          el('p', { class: 'consigne', text: texteConsigne }),
          Ile.speakButton((gs + '. ' + texteConsigne).replace(/’/g, '\''), tx.ecouter),
        ]);
        const fb = el('p', { class: 'feedback', 'aria-live': 'polite' });
        const zoneRegle = el('div', { class: 'pluriel-zone-regle', 'aria-live': 'polite' });
        const suite = el('div', { class: 'actions' });

        panel.append(consigne, duo);

        // Fin de la question : pluriel affiché, règle du pack, bouton « Suivant ».
        function conclure(juste) {
          fini = true;
          panel.dataset.etat = 'reponse';
          if (juste && essais === 1) score++;
          else aRevoir.push(gs.replace(/ /g, NB) + NB + '→ ' + gp.replace(/ /g, NB));
          trou.replaceWith(motAvecChangements(x.s, x.p, lang));
          ajuster(texteDes, 16, true);
          carteDes.classList.add(juste ? 'is-good' : 'is-bad');
          Ile.flash(carteDes, juste ? 'good' : 'bad');
          Ile.sfx(juste ? 'good' : 'bad');
          Ile.progress(panel, i + 1, liste.length, score);
          Ile.say((gs + ', ' + gp).replace(/’/g, '\''));
          zoneRegle.innerHTML = '';
          if (regles[x.regle]) {
            zoneRegle.appendChild(el('p', { class: 'pluriel-regle', lang }, [
              el('span', { class: 'pluriel-emoji', 'aria-hidden': 'true', text: '💡' }),
              el('span', { class: 'sr-only', text: tx.regle + NB + ': ' }),
              el('span', { class: 'pluriel-regle__texte' }, texteRegle(regles[x.regle], Ile.getLang())),
            ]));
          }
          const dernier = i + 1 >= liste.length;
          const b = el('button', { type: 'button', class: 'btn btn--sun pluriel-suivant' }, [
            (dernier ? tx.resultat : tx.suivant) + ' ', el('span', { 'aria-hidden': 'true', class: 'pluriel-emoji', text: dernier ? '🏁' : '➜' }),
          ]);
          const cree = Date.now();
          b.addEventListener('click', () => {
            // La touche Entrée (ou le double clic) qui a répondu ne doit pas aussi passer à la suite.
            if (b.disabled || !actif() || Date.now() - cree < 350) return;
            b.disabled = true;
            Ile.sfx('click');
            i++;
            poser();
          });
          suite.appendChild(b);
          setTimeout(() => {
            if (!actif()) return;
            b.focus({ preventScroll: true });
            b.scrollIntoView({ block: 'nearest', behavior: Ile.reduceMotion ? 'auto' : 'smooth' });
          }, 0);
        }

        if (!saisieLibre) {
          // ----- QCM -----
          const formes = Ile.shuffle([x.p].concat(candidats(x).slice(0, nbChoix - 1).map((c) => c.forme)));
          const grille = el('div', { class: 'choices' + (formes.length === 3 ? ' choices--3' : ''), role: 'group', 'aria-label': tx.choix });
          formes.forEach((f) => {
            const b = el('button', { type: 'button', class: 'btn choice', lang, text: f });
            b.addEventListener('click', () => {
              if (fini || !actif()) return;
              essais = 1;
              const juste = f === x.p;
              panel.dataset.verdict = juste ? 'exact' : 'faux';
              grille.querySelectorAll('button').forEach((y) => {
                y.disabled = true;
                if (y.textContent === x.p) y.classList.add('is-correct');
              });
              if (!juste) b.classList.add('is-wrong');
              Ile.flash(b, juste ? 'good' : 'bad');
              if (juste) Ile.feedback(fb, true);
              else Ile.feedback(fb, false, tx.non(gp));
              conclure(juste);
            });
            grille.appendChild(b);
          });
          panel.append(grille, fb, zoneRegle, suite);
          ajusterTout();
        } else {
          // ----- Saisie -----
          // Allemand, luxembourgeois : le pluriel d'un nom a une majuscule, exigée ici.
          const exigeCasse = x.p !== x.p.toLocaleLowerCase(locale);
          const input = el('input', {
            type: 'text', class: 'answer', lang, autocomplete: 'off', autocapitalize: 'off', autocorrect: 'off',
            spellcheck: 'false', enterkeyhint: 'done', maxlength: '40', 'aria-label': tx.ecris(x.detP),
          });
          const clavier = el('div', { class: 'pluriel-touches', role: 'group', 'aria-label': tx.touches },
            touches.map((c) => {
              const b = el('button', { type: 'button', class: 'pluriel-touche', lang, 'aria-label': tx.inserer(c), title: tx.inserer(c), text: c });
              b.addEventListener('mousedown', (e) => e.preventDefault()); // garde le curseur dans la case
              b.addEventListener('click', () => {
                if (fini || input.disabled) return;
                const d = input.selectionStart == null ? input.value.length : input.selectionStart;
                const e = input.selectionEnd == null ? d : input.selectionEnd;
                input.setRangeText(c, d, e, 'end');
                input.focus({ preventScroll: true });
                Ile.sfx('click');
              });
              return b;
            }));
          clavier.hidden = !touches.length;
          const valider = el('button', { type: 'submit', class: 'btn btn--primary pluriel-valider' }, [
            el('span', { 'aria-hidden': 'true', text: '✓' }), ' ' + tx.valider,
          ]);
          const zoneValider = el('div', { class: 'actions' }, [valider]);
          const form = el('form', { class: 'pluriel-form', novalidate: true, autocomplete: 'off' }, [
            el('div', { class: 'pluriel-saisie' + (elide(x.detP) ? ' pluriel-saisie--elide' : '') }, [
              el('span', { class: 'pluriel-art', lang, 'aria-hidden': 'true', text: x.detP }), input,
            ]),
            fb, zoneRegle, suite, clavier, zoneValider,
          ]);
          const fermer = () => {
            input.disabled = true;
            valider.disabled = true;
            clavier.hidden = true;
            zoneValider.hidden = true;
          };
          const pareil = (a, b) => (exigeCasse ? norme(a) === norme(b) : bas(a) === bas(b));
          let dernierFaux = null; // dernière réponse fausse : la renvoyer telle quelle ne coûte pas d'essai
          form.addEventListener('submit', (e) => {
            e.preventDefault();
            if (fini || input.disabled || !actif()) return;
            let rep = norme(input.value);
            // L'enfant a pu recopier le déterminant : « des chevaux », « die Hunde », « d’Kazen ».
            const det = norme(x.detP);
            if (det) {
              const b = bas(rep);
              const d = bas(det);
              if (elide(d) && b.startsWith(d) && b.length > d.length) rep = rep.slice(det.length).trim();
              else if (b.startsWith(d + ' ')) rep = rep.slice(det.length + 1).trim();
            }
            if (!rep) {
              panel.dataset.verdict = 'vide';
              fb.className = 'feedback';
              fb.textContent = tx.vide;
              input.focus({ preventScroll: true });
              return;
            }
            // Double appui (Entrée, bouton) sur la même réponse fausse : l'indice est déjà affiché,
            // le deuxième essai n'est pas perdu.
            if (dernierFaux !== null && rep === dernierFaux) {
              Ile.flash(input, 'bad');
              input.focus({ preventScroll: true });
              return;
            }
            essais++;
            const variante = ((tx.variantes || {})[x.s] || []).some((v) => pareil(rep, v));
            if (pareil(rep, x.p) || variante) {
              panel.dataset.verdict = variante ? 'variante' : 'exact';
              fermer();
              Ile.feedback(fb, true, variante ? tx.variante(gp) : (essais === 1 ? tx.bravo : tx.oui));
              conclure(true);
              return;
            }
            dernierFaux = rep;
            // Indice : « regarde la fin » si le pluriel ne change qu'à la fin (Hund → Hunde),
            // « regarde tout le mot » sinon (mouse → mice, Hand → Hände).
            let verdict = changeDedans(x.s, x.p) ? 'regarde' : 'fin';
            if (bas(rep) === bas(x.p)) verdict = 'majuscule';
            else if (bas(rep) === bas(x.s)) verdict = 'marque'; // le singulier recopié (Apfel pour Äpfel aussi)
            else if (plat(rep) === plat(x.p)) verdict = 'accents';
            panel.dataset.verdict = verdict;
            if (essais >= 2) {
              fermer();
              Ile.feedback(fb, false, tx.onEcrit(gp));
              conclure(false);
              return;
            }
            // Premier essai manqué : un indice, puis un deuxième essai.
            let indice = tx[verdict];
            const casseFausse = exigeCasse && lettres(rep)[0] === lettres(rep)[0].toLocaleLowerCase(locale);
            if (verdict === 'accents' && casseFausse) indice += ' ' + tx.majuscule;
            // Pluriel qui ne change que par un Umlaut ou un accent (Apfel → Äpfel) : on le dit.
            if (verdict === 'marque' && plat(x.s) === plat(x.p)) indice += ' ' + tx.accents;
            Ile.flash(input, 'bad');
            Ile.sfx('bad');
            Ile.feedback(fb, false, indice);
            input.focus({ preventScroll: true });
          });
          panel.append(form);
          ajusterTout();
          input.focus({ preventScroll: true });
        }
      }

      function terminer() {
        if (!panel.isConnected) return;
        panel._jeton = null;
        panel.dataset.etat = 'fin';
        Ile.progress(panel, liste.length, liste.length, score);
        const message = aRevoir.length
          ? tx.aRevoir(aRevoir.slice(0, 4).join(' · ') + (aRevoir.length > 4 ? '…' : ''))
          : tx.parfait;
        // Score exact : un point par pluriel trouvé du premier coup (comme la barre de progression).
        Ile.showResult({ id: ID, score, total: liste.length, message });
      }

      poser();
    },
  });
})();
