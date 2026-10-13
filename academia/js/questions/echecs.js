/* Académia : générateur des premières règles des échecs, pour Cavalin (logique), sur un petit échiquier dessiné
   (5 ou 6 cases de côté, js/question_figures.js) : où une pièce peut aller (des ronds de couleur sur les cases, un
   seul atteignable), quelle pièce atteint l'étoile, le roi attaqué qui s'échappe, la pièce qui peut prendre.
   Les blancs jouent vers le haut. Les cases sont [colonne, rangée], la rangée 0 en bas, du côté de l'enfant.
   GENERATEURS.echecs(params, mecanique). Les réponses sont refaites par tests/test_questions.py, qui rejoue chaque
   pièce sur l'échiquier montré. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

var REGLES_ECHECS = self.REGLES_ECHECS = (() => {
  const BLANCS = {roi: "♔", dame: "♕", tour: "♖", fou: "♗", cavalier: "♘", pion: "♙"};
  const NOIRS = {roi: "♚", dame: "♛", tour: "♜", fou: "♝", cavalier: "♞", pion: "♟"};
  const TYPE = Object.fromEntries([...Object.entries(BLANCS), ...Object.entries(NOIRS)].map(([t, g]) => [g, t]));
  const FEMININ = {dame: true, tour: true};
  const LIGNES = [[1, 0], [-1, 0], [0, 1], [0, -1]], DIAGONALES = [[1, 1], [1, -1], [-1, 1], [-1, -1]];
  const SAUTS = [[1, 2], [2, 1], [2, -1], [1, -2], [-1, -2], [-2, -1], [-2, 1], [-1, 2]];
  const REGLES = {
    tour: "La tour va tout droit, en long et en large, aussi loin qu'elle veut, sans sauter par-dessus une pièce.",
    fou: "Le fou va en diagonale, aussi loin qu'il veut, sans sauter par-dessus une pièce.",
    dame: "La dame va tout droit et en diagonale, aussi loin qu'elle veut.",
    roi: "Le roi va d'une seule case, dans tous les sens.",
    cavalier: "Le cavalier saute en L : deux cases tout droit, puis une sur le côté. Il peut sauter par-dessus les pièces.",
    pion: "Le pion avance tout droit, d'une case, vers l'autre camp. Il ne recule jamais."};
  const INDICES = {
    tour: "Suis les lignes et les colonnes, jamais les diagonales.", fou: "Suis les diagonales, comme un pont qui penche.",
    dame: "Elle va comme la tour et comme le fou à la fois.", roi: "Une seule case, pas plus.",
    cavalier: "Compte deux cases tout droit, puis une sur le côté.", pion: "Il avance, toujours vers le haut de l'échiquier."};
  const RONDS = {"🔴": "rond rouge", "🔵": "rond bleu", "🟡": "rond jaune", "🟣": "rond violet"};
  const le = t => (FEMININ[t] ? "la " : "le ") + t;
  // the squares a piece reaches from (x, y) on an n × n board; `pieces`: [[glyph, x, y]] that block or can be taken
  function atteint(g, x, y, n, pieces = []){
    const t = TYPE[g], blanc = Object.values(BLANCS).includes(g), out = [];
    const dans = (a, b) => a >= 0 && b >= 0 && a < n && b < n;
    const qui = (a, b) => pieces.find(p => p[1] === a && p[2] === b && !(p[1] === x && p[2] === y));
    const adverse = p => Object.values(BLANCS).includes(p[0]) !== blanc;
    if (t === "cavalier" || t === "roi") {
      (t === "roi" ? [...LIGNES, ...DIAGONALES] : SAUTS).forEach(([dx, dy]) => {
        const a = x + dx, b = y + dy, p = qui(a, b);
        if (dans(a, b) && (!p || adverse(p))) out.push([a, b]);
      });
    } else if (t === "pion") {
      const s = blanc ? 1 : -1;
      if (dans(x, y + s) && !qui(x, y + s)) out.push([x, y + s]);
      [-1, 1].forEach(dx => { const p = qui(x + dx, y + s); if (p && adverse(p)) out.push([x + dx, y + s]); });
    } else {
      (t === "tour" ? LIGNES : t === "fou" ? DIAGONALES : [...LIGNES, ...DIAGONALES]).forEach(([dx, dy]) => {
        for (let a = x + dx, b = y + dy; dans(a, b); a += dx, b += dy) {
          const p = qui(a, b);
          if (p) { if (adverse(p)) out.push([a, b]); break; }
          out.push([a, b]);
        }
      });
    }
    return out;
  }
  const a = (l, c) => l.some(([x, y]) => x === c[0] && y === c[1]);
  return {BLANCS, NOIRS, TYPE, REGLES, INDICES, RONDS, le, atteint, a};
})();

(() => {
  const {BLANCS, NOIRS, TYPE, REGLES, INDICES, RONDS, le, atteint, a} = REGLES_ECHECS;
  const G = () => GEN_OUTILS, C = () => GEN_CM;
  const Le = t => { const s = le(t); return s.charAt(0).toUpperCase() + s.slice(1); };
  const toutes = n => { const l = []; for (let y = 0; y < n; y++) for (let x = 0; x < n; x++) l.push([x, y]); return l; };
  const pareil = (c, d) => c[0] === d[0] && c[1] === d[1];
  const pres = (c, d) => Math.max(Math.abs(c[0] - d[0]), Math.abs(c[1] - d[1]));
  const question = (o, m) => C().question(o, m, "logique");
  const elle = t => t === "dame" || t === "tour" ? "elle" : "il";
  const ATTAQUES = {tour: "attaque toute sa ligne et toute sa colonne", fou: "attaque toutes ses diagonales",
    dame: "attaque sa ligne, sa colonne et ses diagonales"};

  // where can it go: one disc it reaches, three it does not (the tempting ones first: the other moves)
  function aller(m){
    const {alea, choisir, melanger} = G();
    const t = choisir({rapide: ["tour", "fou", "roi", "pion"], massive: ["cavalier", "cavalier", "dame", "tour", "fou"]}[m] || ["cavalier"]);
    const n = m === "rapide" ? 5 : 6, g = BLANCS[t];
    for (let essai = 0; essai < 50; essai++) {
      const pos = [alea(0, n - 1), alea(0, t === "pion" ? n - 2 : n - 1)], ok = atteint(g, pos[0], pos[1], n);
      const non = toutes(n).filter(c => !pareil(c, pos) && !a(ok, c) && !(t === "pion" && c[0] === pos[0] && c[1] === pos[1] + 2));
      const tentant = non.filter(c => pres(c, pos) <= 2 || c[0] === pos[0] || c[1] === pos[1] || Math.abs(c[0] - pos[0]) === Math.abs(c[1] - pos[1]));
      if (!ok.length || tentant.length < 3) continue;
      const cible = choisir(ok), autres = melanger(tentant).slice(0, 3), couleurs = melanger(Object.keys(RONDS));
      const marques = melanger([cible, ...autres]).map((c, i) => [couleurs[i], c[0], c[1]]);
      const rep = marques.find(mk => pareil([mk[1], mk[2]], cible))[0];
      return question({id: `G_echecs_${t}_${pos.join("")}_${marques.map(mk => mk[1] + "" + mk[2]).join("")}`, question: `Où ${le(t)} peut-${elle(t)} aller ?`,
        dire: `Où ${le(t)} peut-${elle(t)} aller ?`, dessin: {type: "echiquier", n, pieces: [[g, pos[0], pos[1]]], marques},
        reponse: rep, distracteurs: marques.map(mk => mk[0]).filter(c => c !== rep), legendes: Object.fromEntries(marques.map(mk => [mk[0], RONDS[mk[0]]])),
        indice: INDICES[t], explication: `${REGLES[t]} ${Le(t)} peut aller sur le ${RONDS[rep]}.`}, m);
    }
    return aller("rapide");
  }

  // which white piece reaches the star (or takes the black piece): three white pieces, only one can
  function laquelle(m, prendre){
    const {alea, choisir, melanger} = G(), n = 6;
    for (let essai = 0; essai < 200; essai++) {
      const types = melanger(["tour", "fou", "cavalier", "dame", "roi"]).slice(0, 3), cases = melanger(toutes(n)).slice(0, 4);
      const cible = cases[3], noir = prendre ? choisir(["cavalier", "pion", "tour", "fou"]) : null;
      const pieces = types.map((t, i) => [BLANCS[t], cases[i][0], cases[i][1]]);
      const tout = noir ? [...pieces, [NOIRS[noir], cible[0], cible[1]]] : pieces;
      const qui = pieces.filter(p => a(atteint(p[0], p[1], p[2], n, tout), cible));
      if (qui.length !== 1) continue;
      const g = qui[0][0], t = TYPE[g];
      // with a piece to take, each one says its colour (« Ni la tour blanche ni le fou blanc ne peuvent le prendre » : the
      // black bishop is another one)
      const blanc = x => prendre ? le(x) + (elle(x) === "elle" ? " blanche" : " blanc") : le(x), nomNoir = noir && `${le(noir)} ${elle(noir) === "elle" ? "noire" : "noir"}`;
      const pourquoiPas = `Ni ${pieces.filter(p => p !== qui[0]).map(p => blanc(TYPE[p[0]])).join(" ni ")} ne peuvent ${prendre ? `${elle(noir) === "elle" ? "la" : "le"} prendre` : "y aller"}.`;
      const legendes = Object.fromEntries(pieces.map(p => [p[0], TYPE[p[0]]]));   // no article, as the names under the pictures of logic
      const texte = prendre ? `Quelle pièce blanche peut prendre ${nomNoir} ?` : "Quelle pièce peut aller sur l'étoile ?";
      return question({id: `G_echecs_${prendre ? "prendre" : "etoile"}_${tout.map(p => TYPE[p[0]][0] + p[1] + p[2]).join("")}_${cible.join("")}`,
        question: texte, dire: texte, dessin: {type: "echiquier", n, pieces: tout, ...(prendre ? {} : {etoile: cible})},
        reponse: g, distracteurs: pieces.map(p => p[0]).filter(x => x !== g), legendes,
        indice: prendre ? "On prend en allant sur la case de la pièce noire, comme on se déplace." : "Essaie chaque pièce, l'une après l'autre.",
        explication: `${REGLES[t]} ${prendre ? blanc(t).charAt(0).toUpperCase() + blanc(t).slice(1) + ` peut prendre ${nomNoir}` : `${Le(t)} atteint l'étoile`}. ${pourquoiPas}`}, m);
    }
    return aller("massive");
  }

  // the king attacked: one square around him is safe, the others are still attacked
  function esquive(m){
    const {alea, choisir, melanger} = G(), n = 6;
    for (let essai = 0; essai < 200; essai++) {
      const t = choisir(["tour", "fou", "dame"]), roi = [alea(1, n - 2), alea(1, n - 2)], noir = [alea(0, n - 1), alea(0, n - 1)];
      if (pres(roi, noir) < 2) continue;
      const attaque = atteint(NOIRS[t], noir[0], noir[1], n, [[NOIRS[t], noir[0], noir[1]]]);   // the king is not there to block
      if (!a(attaque, roi)) continue;
      const autour = atteint(BLANCS.roi, roi[0], roi[1], n), surs = autour.filter(c => !a(attaque, c)), pris = autour.filter(c => a(attaque, c));
      if (!surs.length || pris.length < 2) continue;
      const sur = choisir(surs), couleurs = melanger(Object.keys(RONDS));
      const marques = melanger([sur, ...melanger(pris).slice(0, 3)]).map((c, i) => [couleurs[i], c[0], c[1]]);
      const rep = marques.find(mk => pareil([mk[1], mk[2]], sur))[0], nomNoir = `${le(t)} ${t === "fou" ? "noir" : "noire"}`;
      return question({id: `G_echecs_esquive_${t}${noir.join("")}_${roi.join("")}_${marques.map(mk => mk[1] + "" + mk[2]).join("")}`,
        question: `Le roi blanc est attaqué par ${nomNoir}. Où peut-il aller pour être à l'abri ?`, dire: `Le roi blanc est attaqué par ${nomNoir}. Où peut-il aller pour être à l'abri ?`,
        dessin: {type: "echiquier", n, pieces: [[BLANCS.roi, roi[0], roi[1]], [NOIRS[t], noir[0], noir[1]]], marques},
        reponse: rep, distracteurs: marques.map(mk => mk[0]).filter(c => c !== rep), legendes: Object.fromEntries(marques.map(mk => [mk[0], RONDS[mk[0]]])),
        indice: `Le roi va d'une seule case : cherche celle que ${le(t)} ne peut pas atteindre.`,
        explication: `${nomNoir.charAt(0).toUpperCase() + nomNoir.slice(1)} ${ATTAQUES[t]}. Sur le ${RONDS[rep]}, ${le(t)} ne l'atteint plus : le roi est à l'abri.`}, m);
    }
    return laquelle(m, true);
  }

  // a shield: the piece and the star; can it go there?
  function bouclier(){
    const {alea, choisir} = G(), {vraiFaux} = C(), n = 5, t = choisir(["tour", "fou", "cavalier", "roi", "dame", "pion"]), g = BLANCS[t];
    const pos = [alea(0, n - 1), alea(0, t === "pion" ? n - 2 : n - 1)], ok = atteint(g, pos[0], pos[1], n), vrai = Math.random() < .5;
    const non = toutes(n).filter(c => !pareil(c, pos) && !a(ok, c) && pres(c, pos) <= 2
      && !(t === "pion" && c[0] === pos[0] && c[1] === pos[1] + 2));   // two squares ahead: a pawn's first move may do it
    const etoile = vrai || !non.length ? choisir(ok) : choisir(non), peut = a(ok, etoile);
    return vraiFaux({id: `G_echecs_vf_${t}${pos.join("")}_${etoile.join("")}`, question: `${Le(t)} peut aller sur l'étoile.`, vrai: peut,
      dessin: {type: "echiquier", n, pieces: [[g, pos[0], pos[1]]], etoile}, explication: `${REGLES[t]} ${Le(t)} ${peut ? "peut" : "ne peut pas"} aller sur l'étoile.`}, "logique");
  }

  GENERATEURS.echecs = (p = {}, m = "rapide") => {
    if (m === "bouclier") return bouclier();
    if (m === "boss") return Math.random() < .5 ? esquive(m) : laquelle(m, true);
    if (m === "massive" && Math.random() < .4) return laquelle(m, false);
    return aller(m);
  };
})();
