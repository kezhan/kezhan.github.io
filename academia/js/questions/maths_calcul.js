/* Académia : générateurs du calcul du grand (CM1) : multiplications à deux chiffres (fois 5 par fois 10 puis la
   moitié, posées en colonnes), division avec reste (le reste toujours plus petit que le diviseur), problèmes à deux
   étapes (et à trois pour le chef). Outils : GEN_OUTILS (maths.js) et GEN_CM (maths_nombres.js), lus à l'appel. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

(() => {
  const G = () => GEN_OUTILS, C = () => GEN_CM;

  /* MULTIPLICATIONS À DEUX CHIFFRES : 23 × 4, 5 × 64, 47 × 6 (retenue), 34 × 12 (posée) */
  // a × b explained by its tens and units: "47 × 6 : 40 × 6 = 240, 7 × 6 = 42, et 240 + 42 = 282."
  function expliquer(a, b){
    const {ecrit} = C(), r = a * b;
    if (b >= 10) {   // two digits: a × units, a × tens, added (the line of the tens ends with a zero)
      const u = b % 10, d = b - u;
      return u ? `${a} × ${b} : ${a} × ${u} = ${a * u}, ${a} × ${d} = ${ecrit(a * d)}, et ${a * u} + ${ecrit(a * d)} = ${ecrit(r)}.`
        : `${a} × ${b} : ${a} × ${d / 10} = ${a * d / 10}, puis fois 10 : ${ecrit(r)}.`;
    }
    if (b === 5) return `${a} × 5 : ${a} × 10 = ${a * 10}, et la moitié de ${a * 10}, c'est ${r}.`;
    const d = a - a % 10, u = a % 10;
    return u ? `${a} × ${b} : ${d} × ${b} = ${d * b}, ${u} × ${b} = ${u * b}, et ${d * b} + ${u * b} = ${ecrit(r)}.` : `${a} × ${b} : ${d / 10} × ${b} = ${d / 10 * b}, puis fois 10 : ${ecrit(r)}.`;
  }
  GENERATEURS.mult_grande = (p = {}, m = "rapide") => {
    const {alea, question} = G(), {ecrit, faussesNb, vraiFaux} = C();
    let a, b;
    if (m === "rapide") {   // no carry, or fois 5 (as the Eduscol examples of CM1: 5 × 64)
      if (Math.random() < .4) { a = 2 * alea(6, 49); b = 5; }
      else { b = alea(2, 4); a = 10 * alea(1, Math.floor(9 / b)) + alea(1, Math.floor(9 / b)); }
    } else if (m === "boss") { a = alea(12, 49); b = alea(11, 19); if (a % 10 === 0) a++; }
    else { a = alea(13, 98); b = alea(3, 9); if (a % 10 === 0) a++; if ((a % 10) * b < 10) b = 9; }
    const r = a * b, [x, y] = m === "rapide" && b === 5 && Math.random() < .5 ? [b, a] : [a, b];
    const u = a % 10, d = a - u;
    const sansRetenue = b < 10 ? d * b + (u * b) % 10 : null;                 // 47 × 6 → 242: the 4 of 42 forgotten
    const sansDecalage = b >= 10 ? a * (b % 10) + a * Math.floor(b / 10) : null;   // 34 × 12 → 68 + 34 = 102: the zero forgotten
    const faux = faussesNb(r, [sansRetenue, sansDecalage, r + 10, r - 10, a + b, r + a].filter(v => v != null && v !== r), 3, 1);
    if (m === "bouclier") {
      const v = Math.random() < .5 ? r : faux[0];
      return vraiFaux({id: `G_mult2_${x}x${y}=${v}`, question: `${x} × ${y} = ${ecrit(v)}`, dire: `${x} fois ${y} égale ${v}.`, vrai: v === r,
        explication: expliquer(a, b)});
    }
    return question({id: `G_mult2_${x}x${y}`, question: `${x} × ${y} = ?`, dire: `Combien font ${x} fois ${y} ?`, reponse: ecrit(r), distracteurs: faux.map(ecrit),
      dessin: m === "rapide" ? undefined : {type: "posee", a, b},
      indice: b === 5 ? "Fois 5, c'est fois 10, puis la moitié." : b >= 10 ? `Fais ${a} × ${b % 10}, puis ${a} × ${b - b % 10}, et ajoute.` : "Multiplie les dizaines, puis les unités, et ajoute.",
      explication: expliquer(a, b)}, m);
  };

  /* DIVISION AVEC RESTE : combien de fois 5 dans 17, le reste de 38 : 7, 38 = 7 × 5 + ? ; le chef raconte */
  GENERATEURS.division_reste = (p = {}, m = "rapide") => {
    const {alea, choisir, question, OBJETS} = G(), {faussesNb, vraiFaux, PERSOS} = C();
    const b = m === "rapide" ? alea(2, 6) : alea(3, 9), q = alea(2, 9), r = alea(1, b - 1), a = b * q + r;
    const pourquoi = `${b} × ${q} = ${b * q}, et ${b} × ${q + 1} = ${b * (q + 1)}, c'est trop. Il reste ${a} − ${b * q} = ${r}, plus petit que ${b}.`;
    if (m === "bouclier") {
      const v = Math.random() < .5 ? r : choisir([r + b, q, r + 1 < b ? r + 1 : r - 1].filter(x => x !== r && x > 0));
      return vraiFaux({id: `G_reste_${a}:${b}=${v}`, question: `Dans ${a} : ${b}, il reste ${v}.`, dire: `${a} divisé par ${b} : il reste ${v}.`, vrai: v === r,
        explication: `Dans ${a}, il y a ${q} fois ${b} : ${pourquoi}`});
    }
    const forme = choisir({rapide: ["combien"], massive: ["reste", "egalite"], boss: ["voitures", "boites", "partage"]}[m] || ["combien"]);
    if (forme === "combien") {
      return question({id: `G_fois_${b}_dans_${a}`, question: `Combien de fois ${b} dans ${a} ?`, dire: `Combien de fois ${b} dans ${a} ?`,
        reponse: q, distracteurs: faussesNb(q, [q + 1, q - 1, r, a - b], 3, 1), indice: b === q ? "Quel nombre, multiplié par lui-même, s'en approche sans le dépasser ?" : `Récite la table de ${b}, sans dépasser.`,
        explication: `Il y a ${q} fois ${b} dans ${a} : ${pourquoi}`}, m);
    }
    if (forme === "reste") {
      return question({id: `G_reste_${a}:${b}`, question: `Quel est le reste de ${a} : ${b} ?`, dire: `Quel est le reste de ${a} divisé par ${b} ?`,
        reponse: r, distracteurs: faussesNb(r, [r + b, q, r + 1 < b ? r + 1 : r - 1], 3, 0), indice: `Le reste est toujours plus petit que ${b}.`,
        explication: `Dans ${a}, il y a ${q} fois ${b} : ${pourquoi}`}, m);
    }
    if (forme === "egalite") {
      return question({id: `G_egal_${a}=${b}x${q}+?`, question: `${a} = ${b} × ${q} + ?`, dire: `${a} égale ${b} fois ${q}, plus combien ?`,
        reponse: r, distracteurs: faussesNb(r, [r + b, q, b * q], 3, 0), indice: "Calcule la multiplication, puis ce qui manque pour arriver au premier nombre.",
        explication: `${b} × ${q} = ${b * q}, et ${a} − ${b * q} = ${r}. Donc ${a} = ${b} × ${q} + ${r}.`}, m);
    }
    const qui = choisir(PERSOS), o = choisir(OBJETS);
    if (forme === "voitures") {   // the rest needs one more car: q + 1
      return question({id: `G_voitures_${a}_${b}`, question: `${a} enfants partent en promenade, ${b} par voiture. Combien de voitures faut-il pour emmener tout le monde ?`,
        dire: `${a} enfants partent en promenade, ${b} par voiture. Combien de voitures faut-il pour emmener tout le monde ?`, reponse: q + 1,
        distracteurs: faussesNb(q + 1, [q, r, q + 2], 3, 1), indice: "Personne ne reste à la maison : regarde ce qui reste.",
        explication: `${b} × ${q} = ${b * q} : ${q} voitures pleines, et il reste ${r} ${r > 1 ? "enfants" : "enfant"}. Il faut une voiture de plus : ${q + 1} voitures.`}, m);
    }
    if (forme === "boites") {   // only full boxes count: q
      return question({id: `G_boites_${a}_${b}_${o[1]}`, question: `${qui} range ${a} ${o[2]} ${o[0]} dans des boîtes de ${b}. Combien de boîtes pleines remplit-il ?`,
        dire: `${qui} range ${a} ${o[2]} dans des boîtes de ${b}. Combien de boîtes pleines remplit-il ?`, reponse: q,
        distracteurs: faussesNb(q, [q + 1, r, a - b], 3, 1), indice: "Seules les boîtes pleines comptent.",
        explication: `${b} × ${q} = ${b * q} : ${q} boîtes pleines, et il reste ${r} ${r > 1 ? o[2] : o[1]}, pas assez pour une autre boîte.`}, m);
    }
    return question({id: `G_partage_reste_${a}_${b}_${o[1]}`, question: `${qui} partage ${a} ${o[2]} ${o[0]} entre ${b} amis, autant à chacun. Combien en reste-t-il ?`,
      dire: `${qui} partage ${a} ${o[2]} entre ${b} amis, autant à chacun. Combien en reste-t-il ?`, reponse: r,
      distracteurs: faussesNb(r, [q, r + b, b], 3, 0), indice: "Donne le plus possible à chacun, pareil pour tous.",
      explication: `Chacun en a ${q}, car ${b} × ${q} = ${b * q}. Il reste ${a} − ${b * q} = ${r} ${r > 1 ? o[2] : o[1]}.`}, m);
  };

  /* PROBLÈMES À DEUX ÉTAPES ; trois pour le chef. Chaque récit a son calcul, refait par tests/test_questions.py. */
  const alea = (a, b) => G().alea(a, b);
  // each story: the attacks it serves, its numbers, its text, its answer, the traps (one step only, the wrong
  // operation), its explanation step by step
  const RECITS = {
    paquets_mange: {mec: ["massive", "bouclier"], mange: true, nombres: () => [alea(3, 6), alea(4, 9), alea(2, 9)],
      texte: (n, x, o) => `${x} achète ${n[0]} paquets de ${n[1]} ${o[2]} ${o[0]}. Il en mange ${n[2]}. Combien lui en reste-t-il ?`,
      rep: n => n[0] * n[1] - n[2], pieges: n => [n[0] * n[1], n[0] + n[1] - n[2], n[0] * n[1] + n[2]],
      expl: n => `D'abord, tous les paquets : ${n[0]} × ${n[1]} = ${n[0] * n[1]}. Ensuite, il en mange : ${n[0] * n[1]} − ${n[2]} = ${n[0] * n[1] - n[2]}.`},
    gagne_perd: {mec: ["rapide", "bouclier"], nombres: () => [alea(20, 60), alea(5, 19), alea(3, 15)],
      texte: (n, x) => `${x} a ${n[0]} billes. Il en gagne ${n[1]}, puis il en perd ${n[2]}. Combien en a-t-il maintenant ?`,
      rep: n => n[0] + n[1] - n[2], pieges: n => [n[0] + n[1], n[0] + n[1] + n[2], n[0] - n[1] + n[2]],
      expl: n => `D'abord, il en gagne : ${n[0]} + ${n[1]} = ${n[0] + n[1]}. Puis il en perd : ${n[0] + n[1]} − ${n[2]} = ${n[0] + n[1] - n[2]}.`},
    bus: {mec: ["rapide", "massive"], nombres: () => { const a = alea(12, 30); return [a, alea(3, a - 5), alea(2, 12)]; },
      texte: n => `Dans le bus, il y a ${n[0]} enfants. ${n[1]} descendent, puis ${n[2]} montent. Combien d'enfants y a-t-il maintenant ?`,
      rep: n => n[0] - n[1] + n[2], pieges: n => [n[0] - n[1], n[0] + n[1] + n[2], n[0] - n[1] - n[2]],
      expl: n => `D'abord, ${n[1]} descendent : ${n[0]} − ${n[1]} = ${n[0] - n[1]}. Puis ${n[2]} montent : ${n[0] - n[1]} + ${n[2]} = ${n[0] - n[1] + n[2]}.`},
    boites_seules: {mec: ["rapide", "massive"], nombres: () => [alea(2, 5), alea(6, 10), alea(2, 9)],
      texte: (n, x) => `${x} a ${n[0]} boîtes de ${n[1]} crayons, et ${n[2]} crayons tout seuls. Combien de crayons a-t-il en tout ?`,
      rep: n => n[0] * n[1] + n[2], pieges: n => [n[0] * n[1], n[0] + n[1] + n[2], (n[0] + 1) * n[1]],
      expl: n => `D'abord, les boîtes : ${n[0]} × ${n[1]} = ${n[0] * n[1]}. Puis les crayons seuls : ${n[0] * n[1]} + ${n[2]} = ${n[0] * n[1] + n[2]}.`},
    partage_mange: {mec: ["massive", "boss"], mange: true, nombres: () => { const k = alea(3, 6), q = alea(4, 9); return [k * q, k, alea(1, q - 2)]; },
      texte: (n, x, o) => `${x} partage ${n[0]} ${o[2]} ${o[0]} entre ${n[1]} amis. Chacun en mange ${n[2]}. Combien en reste-t-il à chacun ?`,
      rep: n => n[0] / n[1] - n[2], pieges: n => [n[0] / n[1], n[0] - n[2], n[0] / n[1] + n[2]],
      expl: n => `D'abord, chacun reçoit ${n[0] / n[1]}, car ${n[1]} × ${n[0] / n[1]} = ${n[0]}. Puis chacun en mange ${n[2]} : ${n[0] / n[1]} − ${n[2]} = ${n[0] / n[1] - n[2]}.`},
    lecture: {mec: ["massive", "boss", "bouclier"], nombres: () => { const j = alea(3, 6), pj = alea(4, 8); return [pj, j, pj * j + alea(6, 30)]; },
      texte: (n, x) => `Le livre de ${x} a ${n[2]} pages. Il en lit ${n[0]} par jour pendant ${n[1]} jours. Combien de pages lui reste-t-il à lire ?`,
      rep: n => n[2] - n[0] * n[1], pieges: n => [n[0] * n[1], n[2] - n[0] - n[1], n[2] - n[0]],
      expl: n => `D'abord, les pages lues : ${n[0]} × ${n[1]} = ${n[0] * n[1]}. Il en reste : ${n[2]} − ${n[0] * n[1]} = ${n[2] - n[0] * n[1]}.`},
    monnaie: {mec: ["boss"], nombres: () => [alea(2, 4), alea(2, 4), alea(1, 3), 20],
      texte: (n, x) => `Un cahier coûte ${n[1]} €, une gomme ${n[2]} €. ${x} achète ${n[0]} cahiers et une gomme, et paie avec un billet de ${n[3]} €. Combien lui rend-on ?`,
      rep: n => n[3] - (n[0] * n[1] + n[2]), pieges: n => [n[0] * n[1] + n[2], n[3] - n[0] * n[1], n[3] - n[1] - n[2]],
      expl: n => `Les cahiers : ${n[0]} × ${n[1]} = ${n[0] * n[1]} €. Avec la gomme : ${n[0] * n[1]} + ${n[2]} = ${n[0] * n[1] + n[2]} €. On lui rend ${n[3]} − ${n[0] * n[1] + n[2]} = ${n[3] - n[0] * n[1] - n[2]} €.`},
  };
  GENERATEURS.probleme_etapes = (p = {}, m = "rapide") => {
    const {choisir, question, OBJETS} = G(), {faussesNb, vraiFaux, PERSOS} = C();
    const ids = Object.keys(RECITS).filter(k => RECITS[k].mec.includes(m)), id = choisir(ids.length ? ids : ["gagne_perd"]), r = RECITS[id];
    const o = choisir(r.mange ? OBJETS.filter(x => ["🍎", "🍓", "🍪", "🍌"].includes(x[0])) : OBJETS);   // one eats only what can be eaten
    const n = r.nombres(), x = choisir(PERSOS), t = r.texte(n, x, o), rep = r.rep(n), faux = faussesNb(rep, r.pieges(n), 3, 0);
    if (m === "bouclier") {
      const v = Math.random() < .5 ? rep : faux[0];
      return vraiFaux({id: `G_pb2_${id}_${n.join("_")}=${v}`, question: `${t} Réponse : ${v}.`, dire: `${t} La réponse est ${v}.`, vrai: v === rep, explication: r.expl(n)});
    }
    return question({id: `G_pb2_${id}_${n.join("_")}_${o[1]}`, question: t, dire: t, reponse: rep, distracteurs: faux,
      indice: "Un calcul après l'autre : cherche d'abord ce qu'on ne sait pas encore.", explication: r.expl(n)}, m);
  };
})();
