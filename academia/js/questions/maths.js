/* Académia : générateurs de questions de mathématiques (contrat : conception/architecture.md).
   GENERATEURS.<nom>(params, mecanique) rend une question de même forme que les questions écrites.
   mecanique : "rapide" (révision simple), "massive" (haut de la notion), "boss" (question à trou ou récit),
   "bouclier" (Vrai/Faux). params.visuel : émojis à compter pour les petits.
   Ce fichier : outils communs (GEN_OUTILS), compter, comparer, additions, soustractions, compléments, doubles.
   Multiplications, divisions et problèmes : maths_mult.js ; suites logiques : logique.js. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

var GEN_OUTILS = (() => {
  const alea = (a, b) => a + Math.floor(Math.random() * (b - a + 1));   // entier entre a et b compris
  const choisir = l => l[Math.floor(Math.random() * l.length)];
  const melanger = l => { l = l.slice(); for (let i = l.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [l[i], l[j]] = [l[j], l[i]]; } return l; };
  const fois = (e, n) => Array(n).fill(e).join("");
  // big collections are shown in groups of five, easier to count: "🍎🍎🍎🍎🍎 🍎🍎🍎"
  const paquets = (e, n) => { const g = []; for (let i = 0; i < n; i += 5) g.push(fois(e, Math.min(5, n - i))); return g.join(" "); };
  const suite = (de, a) => { const l = []; for (let k = de; k <= a; k++) l.push(k); return l.join(", "); };
  // wrong answers: the typical mistakes first, then values near the answer; never the answer, never twice
  function fausses(reponse, pieges, proches = [], n = 3, min = 0, interdits = []){
    const out = [], vus = new Set([String(reponse), ...interdits.map(String)]);   // interdits: never offered (a + b under a drawn subtraction)
    const ajouter = v => { if (out.length < n && Number.isInteger(v) && v >= min && !vus.has(String(v))) { vus.add(String(v)); out.push(String(v)); } };
    pieges.forEach(ajouter); melanger(proches).forEach(ajouter);
    for (let d = 1; out.length < n && d < 50; d++) { ajouter(reponse + d); ajouter(reponse - d); }
    return out;
  }
  const inverse = n => n >= 10 && n < 100 && n % 10 && Math.floor(n / 10) !== n % 10 ? (n % 10) * 10 + Math.floor(n / 10) : null;
  const NIVEAU = {rapide: 1, massive: 2, boss: 3, bouclier: 1};
  /* the common shape; for a shield, a proposed answer (right or the first trap) is to be judged True or False.
     o: {id, question, dire, visuel, reponse, distracteurs, explication, indice, affirmer(p), affirmerDire(p)} */
  function question(o, mecanique = "rapide", matiere = "maths"){
    const q = {id: o.id, matiere, niveau: NIVEAU[mecanique] || 1, mecaniques: [mecanique], question: o.question,
      langue: "fr", visuel: o.visuel || "", reponse: String(o.reponse), distracteurs: o.distracteurs.map(String),
      explication: o.explication, dire: o.dire || "", indice: o.indice || ""};
    if (o.dessin) q.dessin = o.dessin;   // a drawing the panel makes (js/question_dessin.js): a crossed pile, a ten frame
    if (o.cible) q.cible = o.cible;      // the object to count among others, shown apart
    if (mecanique !== "bouclier" || !o.affirmer) return q;
    const vrai = Math.random() < .5, propose = vrai ? q.reponse : q.distracteurs[0];
    return {...q, id: `${o.id}=${propose}`, question: o.affirmer(propose), indice: "",
      dire: o.affirmerDire ? `${o.affirmerDire(propose)} Vrai ou faux ?` : "",
      reponse: vrai ? "vrai" : "faux", distracteurs: [vrai ? "faux" : "vrai"]};
  }
  // things to count: emoji, singular, plural, gender (for "un" or "une")
  const OBJETS = [["🍎", "pomme", "pommes", "f"], ["⭐", "étoile", "étoiles", "f"], ["🐞", "coccinelle", "coccinelles", "f"],
    ["🐟", "poisson", "poissons", "m"], ["🎈", "ballon", "ballons", "m"], ["🌸", "fleur", "fleurs", "f"], ["🐣", "poussin", "poussins", "m"],
    ["🍓", "fraise", "fraises", "f"], ["🚗", "voiture", "voitures", "f"], ["🦋", "papillon", "papillons", "m"], ["🍪", "biscuit", "biscuits", "m"],
    ["🐢", "tortue", "tortues", "f"], ["💎", "cristal", "cristaux", "m"], ["🐸", "grenouille", "grenouilles", "f"], ["🍌", "banane", "bananes", "f"],
    ["🐝", "abeille", "abeilles", "f"], ["🍄", "champignon", "champignons", "m"], ["🐌", "escargot", "escargots", "m"]];
  const de = mot => /^[aeéèêiouyh]/i.test(mot) ? `d'${mot}` : `de ${mot}`;   // "d'étoiles", "de pommes"
  const nombre = (n, o) => n === 1 ? `${o[3] === "f" ? "une" : "un"} ${o[1]}` : `${n} ${o[2]}`;
  const deux = () => { const a = choisir(OBJETS); let b = choisir(OBJETS); while (b === a) b = choisir(OBJETS); return [a, b]; };
  return {alea, choisir, melanger, fois, paquets, suite, fausses, inverse, question, OBJETS, nombre, deux, de};
})();

(() => {
  const {alea, choisir, melanger, fois, paquets, suite, fausses, inverse, question, OBJETS, nombre, deux, de} = GEN_OUTILS;

  /* COMPTER : "Combien de pommes ?", "Montre 3 pommes" (jusqu'à 5), compter un seul objet parmi d'autres (boss).
     L'objet compté n'est jamais dans la phrase : on le compterait avec le dessin. Au boss, il est montré à part. */
  GENERATEURS.denombrer = (p = {}, m = "rapide") => {
    const max = p.max || 10, o = choisir(OBJETS), [e, , pl] = o;
    const n = m === "massive" || m === "boss" ? alea(Math.ceil(max / 2), max) : alea(1, Math.ceil(max * .7));
    const compte = n <= 5 ? `Je touche chaque ${o[1]} une seule fois en comptant : ${suite(1, n)}.`
      : `Je compte par paquets de 5 : ${suite(1, Math.floor(n / 5)).split(", ").map(k => k * 5).join(", ")}${n % 5 ? `, puis ${suite(n - n % 5 + 1, n)}` : ""}.`;
    const base = {reponse: n, distracteurs: fausses(n, [n + 1, n - 1], [n + 2, n - 2], 3, 1), indice: `Touche chaque ${o[1]} une seule fois en comptant.`,
      affirmer: k => `Il y a ${nombre(+k, o)} ?`, affirmerDire: k => `Il y a ${nombre(+k, o)}.`};
    if (m === "boss") {   // count one kind among two
      const [, autre] = [o, choisir(OBJETS.filter(x => x !== o))], k = alea(2, Math.min(5, max));
      const visuel = melanger([...Array(n).fill(e), ...Array(k).fill(autre[0])]).join("");
      return question({...base, id: `G_compter_${max}_${o[1]}_${n}_parmi_${autre[1]}`, question: `Combien ${de(pl)} ?`, dire: `Combien ${de(pl)} ?`,
        visuel, cible: e, explication: `Je compte seulement les ${pl}, une par une, sans compter les ${autre[2]} : il y en a ${n}.`}, m);
    }
    if (max <= 5 && m === "rapide" && Math.random() < .5) {   // show the group that has n things
      const groupes = fausses(n, [n + 1, n - 1], [n + 2], 3, 1).map(k => fois(e, +k));
      return question({id: `G_compter_${max}_${o[1]}_${n}_montre`, question: `Montre ${nombre(n, o)}`, dire: `Montre ${nombre(n, o)}.`,
        reponse: fois(e, n), distracteurs: groupes, indice: `Compte les ${pl} de chaque groupe.`,
        explication: `${nombre(n, o)[0].toUpperCase()}${nombre(n, o).slice(1)}, c'est ${suite(1, n)} : le groupe ${fois(e, n)}.`}, m);
    }
    return question({...base, id: `G_compter_${max}_${o[1]}_${n}`, question: `Combien ${de(pl)} ?`, dire: `Combien ${de(pl)} ?`,
      visuel: paquets(e, n), explication: `${compte} Il y a ${nombre(n, o)}.`}, m);
  };

  /* COMPARER : des collections (plus, moins, autant) chez les petits, des nombres ensuite */
  function raison(x, y){   // why the bigger of x and y is bigger, said simply
    const [g, p] = x > y ? [x, y] : [y, x], dg = Math.floor(g / 10), dp = Math.floor(p / 10);
    if (String(g).length > String(p).length) return `${g} a plus de chiffres que ${p}`;
    if (g >= 10 && dg !== dp) return `Je regarde d'abord les dizaines : ${dg} dizaines, c'est plus que ${dp}`;
    if (g >= 10) return `Mêmes dizaines, alors je regarde les unités : ${g % 10}, c'est plus que ${p % 10}`;
    return `En comptant, ${g} vient après ${p}`;
  }
  GENERATEURS.comparer = (p = {}, m = "rapide") => {
    const max = p.max || 20;
    if (p.mode === "collections") {   // three groups at most, at least two apart; "le moins" and "autant" for the boss
      const [a, b] = deux(), trois = melanger(choisir([[1, 3, 5], [1, 3, 6], [1, 4, 6], [2, 4, 6]]).filter(v => v <= max));
      if (m === "bouclier") {
        const [x, y] = trois;
        return {...question({id: `G_comparer_${a[1]}${x}_${b[1]}${y}`, question: "", reponse: 0, distracteurs: [1], explication: ""}, "rapide"),
          id: `G_comparer_${a[1]}${x}_${b[1]}${y}`, mecaniques: ["bouclier"], visuel: `${fois(a[0], x)} ${fois(b[0], y)}`, indice: "",
          question: `Il y a plus ${de(a[2])} que ${de(b[2])} ?`, dire: `Il y a plus ${de(a[2])} que ${de(b[2])}. Vrai ou faux ?`,
          reponse: x > y ? "vrai" : "faux", distracteurs: [x > y ? "faux" : "vrai"],
          explication: `Je compte : ${x} ${x > 1 ? a[2] : a[1]} et ${y} ${y > 1 ? b[2] : b[1]}. ${x > y ? `${x} est plus que ${y}` : `${x} est moins que ${y}`}.`};
      }
      if (m === "boss" && Math.random() < .5) {   // as many as
        const n = trois[0];
        return question({id: `G_autant_${a[1]}_${b[1]}_${trois.join("-")}`, question: `Où y a-t-il autant ${de(a[2])} que ${de(b[2])} ?`,
          dire: `Où y a-t-il autant ${de(a[2])} que ${de(b[2])} ?`, visuel: fois(b[0], n), reponse: fois(a[0], n),
          distracteurs: trois.slice(1).map(k => fois(a[0], k)), indice: `Compte les ${b[2]}, puis cherche le même nombre ${de(a[2])}.`,
          explication: `Il y a ${n} ${n > 1 ? b[2] : b[1]}. Autant, c'est le même nombre : ${nombre(n, a)}, ${fois(a[0], n)}.`}, m);
      }
      const plus = m !== "boss", cible = plus ? Math.max(...trois) : Math.min(...trois);
      const mot = plus ? "le plus" : "le moins";
      return question({id: `G_${plus ? "plus" : "moins"}_${a[1]}_${trois.join("-")}`, question: `Où y a-t-il ${mot} ${de(a[2])} ?`,
        dire: `Où y a-t-il ${mot} ${de(a[2])} ?`, reponse: fois(a[0], cible),
        distracteurs: trois.filter(k => k !== cible).map(k => fois(a[0], k)), indice: "Compte chaque groupe.",
        explication: `Je compte chaque groupe : ${trois.slice().sort((x, y) => x - y).join(", ")}. ${plus ? `${cible} est le plus grand nombre` : `${cible} est le plus petit nombre`} : c'est là qu'il y a ${mot} ${de(a[2])}.`}, m);
    }
    // numbers: biggest, smallest, or the sign between two numbers
    const bas = max <= 20 ? 1 : 10, x = alea(bas, max);
    const proches = [inverse(x), x + 1, x - 1, x + 10, x - 10, x + alea(2, 5), x - alea(2, 5)].filter(v => v != null && v >= bas && v <= max && v !== x);
    if (m === "boss" || m === "bouclier") {
      let y = choisir(proches.length ? proches : [x + 1]);
      const signe = x > y ? ">" : "<", [g, pe] = x > y ? [x, y] : [y, x];
      return question({id: `G_signe_${x}_${y}`, question: `Quel signe va entre ${x} et ${y} ?`, dire: `Quel signe va entre ${x} et ${y} ?`,
        visuel: `${x}  …  ${y}`, reponse: signe, distracteurs: [signe === ">" ? "<" : ">", "="], indice: "La pointe du signe montre le plus petit nombre.",
        explication: `${raison(x, y)}, donc ${g} est plus grand que ${pe} : on écrit ${x} ${signe} ${y}.`,
        affirmer: s => `${x} ${s} ${y}`, affirmerDire: s => `${x} est ${s === ">" ? "plus grand que" : s === "<" ? "plus petit que" : "égal à"} ${y}.`}, m);
    }
    const nombres = [x, ...melanger(proches).filter((v, i, l) => l.indexOf(v) === i).slice(0, 3)];
    while (nombres.length < 4) { const v = alea(bas, max); if (!nombres.includes(v)) nombres.push(v); }
    const grand = m !== "massive", cible = grand ? Math.max(...nombres) : Math.min(...nombres);
    const rival = grand ? Math.max(...nombres.filter(v => v !== cible)) : Math.min(...nombres.filter(v => v !== cible));
    return question({id: `G_${grand ? "plusgrand" : "pluspetit"}_${nombres.slice().sort((a, b) => a - b).join("-")}`,
      question: `Quel est le plus ${grand ? "grand" : "petit"} nombre ?`, dire: `Quel est le plus ${grand ? "grand" : "petit"} nombre ?`,
      reponse: cible, distracteurs: nombres.filter(v => v !== cible), indice: max > 20 ? "Regarde d'abord les dizaines." : `Lequel vient en ${grand ? "dernier" : "premier"} quand on compte ?`,
      explication: `${raison(cible, rival)}. Donc ${cible} est le plus ${grand ? "grand" : "petit"}.`}, m);
  };

  /* ADDITIONS */
  const parts = n => [Math.floor(n / 100) * 100, Math.floor(n % 100 / 10) * 10, n % 10];
  function expliquerAddition(a, b){
    const s = a + b, [g, pe] = a >= b ? [a, b] : [b, a], depart = a >= b ? "je fais" : "je pars du plus grand et je fais";
    if (a === b && a <= 10) return `${a} + ${b}, c'est le double de ${a} : ${s}.`;
    if (s <= 10) return `Je pars de ${g} et j'avance de ${pe} : ${suite(g + 1, s)}. ${a} + ${b} = ${s}.`;
    if (Math.abs(a - b) === 1 && g <= 10) return `${a} + ${b}, c'est le double de ${pe}, plus 1 : ${pe} + ${pe} = ${2 * pe}, puis ${2 * pe} + 1 = ${s}.`;
    if (g < 10) return `${a} + ${b} : ${depart} ${g} + ${10 - g} = 10, puis encore ${pe - (10 - g)}, ça fait ${s}.`;
    if (g === 10 && pe < 10) return `${a} + ${b} = ${s} : une dizaine et ${pe} unités.`;
    if (pe < 10 && g % 10 + pe < 10) return `${a} + ${b} : j'ajoute les unités, ${g % 10} + ${pe} = ${g % 10 + pe}, donc ${s}.`;
    if (pe < 10) { const r = 10 - g % 10; return `${a} + ${b} : ${g} + ${r} = ${g + r}, puis encore ${pe - r}, ça fait ${s}.`; }
    if (a % 10 === 0 && b % 10 === 0 && s < 100) return `${a} + ${b} : ${a / 10} dizaines + ${b / 10} dizaines = ${s / 10} dizaines, donc ${s}.`;
    if (a % 100 === 0 && b % 100 === 0) return `${a} + ${b} : ${a / 100} centaines + ${b / 100} centaines = ${s / 100} centaines, donc ${s}.`;
    // hundreds, tens, units added apart: 38 + 25 → 30 + 20 = 50, 8 + 5 = 13, and 50 + 13 = 63
    const pa = parts(a), pb = parts(b), etapes = [], sommes = [];
    [0, 1, 2].forEach(i => { if (pa[i] && pb[i]) etapes.push(`${pa[i]} + ${pb[i]} = ${pa[i] + pb[i]}`); if (pa[i] + pb[i]) sommes.push(pa[i] + pb[i]); });
    return `${a} + ${b} : ${etapes.join(", ")}, et ${sommes.join(" + ")} = ${s}.`;
  }
  function tirerAddition(max, m){
    if (max <= 10) {
      if (m === "massive") { const s = alea(6, 10), a = alea(2, s - 2); return [a, s - a]; }
      const a = alea(1, 7); return [a, alea(1, Math.min(3, 10 - a))];
    }
    if (max <= 20) {
      if (m === "massive") { const a = alea(5, 9); return melanger([a, alea(11 - a, 9)]); }
      const v = alea(1, 3);
      if (v === 1) { const a = alea(10, 15); return [a, alea(1, 9 - a % 10)]; }
      if (v === 2) { const d = alea(3, 9); return [d, d]; }
      const d = alea(3, 8); return [d, d + 1];
    }
    if (max <= 100) {
      if (m === "massive") { const ta = alea(1, 6), ua = alea(1, 9), tb = alea(1, 8 - ta), ub = alea(10 - ua, 9); return [10 * ta + ua, 10 * tb + ub]; }
      const v = alea(1, 3), ta = alea(1, 8), ua = alea(1, 8);
      if (v === 1) return [10 * ta, 10 * alea(1, 9 - ta)];
      if (v === 2) return [10 * ta + ua, alea(1, 9 - ua)];
      return [10 * ta + ua, 10 * alea(1, 9 - ta) + alea(0, 9 - ua)];
    }
    if (m === "massive") { const a = alea(120, 680); return [a, alea(110, 990 - a)]; }
    return Math.random() < .5 ? [100 * alea(1, 5), 100 * alea(1, 4)] : [alea(1, 8) * 100 + alea(1, 8) * 10, alea(1, 9) * 10];
  }
  GENERATEURS.addition = (p = {}, m = "rapide") => {
    const max = p.max || 20, o = choisir(OBJETS);
    if (m === "boss") {   // the missing number: 8 + ? = 15
      const [a, b] = tirerAddition(max, max <= 20 ? "massive" : m), s = a + b;
      const montrer = p.visuel && s <= 10;
      return question({id: `G_add_${a}+?=${s}`, question: `${a} + ? = ${s}`, dire: `${a} plus combien égale ${s} ?`,
        visuel: montrer ? `${fois(o[0], a)} + ❓ = ${fois(o[0], s)}` : "", reponse: b,
        distracteurs: fausses(b, [b + 1, b - 1, s], [b + 2, b - 2], 3, 0),
        indice: a === b ? `C'est un double : quel nombre, ajouté à lui-même, fait ${s} ?` : `Pars de ${a} et compte jusqu'à ${s}.`,
        explication: s <= 10 || a >= 10 ? `Je pars de ${a} et je compte jusqu'à ${s} : il faut ${b}, car ${a} + ${b} = ${s}.`
          : `De ${a} à 10, il faut ${10 - a}, puis de 10 à ${s}, il faut ${s - 10} : ${10 - a} + ${s - 10} = ${b}. ${a} + ${b} = ${s}.`}, m);
    }
    const [a, b] = tirerAddition(max, m), s = a + b, retenue = a % 10 + b % 10 >= 10 && s > 20;
    const pieges = s <= 20 ? [s + 1, s - 1, s + 2] : [retenue ? s - 10 : s + 10, inverse(s), s + 1, s - 1].filter(v => v != null);
    const montrer = p.visuel && s <= 10;
    return question({id: `G_add_${a}+${b}`, question: `${a} + ${b} = ?`, dire: `Combien font ${a} plus ${b} ?`,
      visuel: montrer ? `${fois(o[0], a)} + ${fois(o[0], b)}` : "", reponse: s, distracteurs: fausses(s, pieges, [s + 2, s - 2], 3, 0),
      indice: s <= 10 ? "Pars du plus grand nombre et avance sur tes doigts." : a < 10 && b < 10 ? "Fais d'abord 10, puis ajoute le reste." : "Ajoute les dizaines, puis les unités.",
      explication: expliquerAddition(a, b), affirmer: k => `${a} + ${b} = ${k}`, affirmerDire: k => `${a} plus ${b} égale ${k}.`}, m);
  };

  /* SOUSTRACTIONS */
  function expliquerSoustraction(a, b){
    const d = a - b;
    if (a <= 10) return b <= 3 ? `Je recule de ${b} à partir de ${a} : ${suite(d, a - 1).split(", ").reverse().join(", ")}. ${a} − ${b} = ${d}.`
      : `${a} − ${b} = ${d}, car ${b} + ${d} = ${a}.`;
    if (b === 10) return `${a} − 10 = ${d} : j'enlève une dizaine.`;
    if (b < 10 && a % 10 >= b) return `${a} − ${b} : je retire ${b} aux unités, ${a % 10} − ${b} = ${a % 10 - b}, donc ${d}.`;
    if (b < 10 && a % 10 === 0) return `${a} − ${b} = ${d}, car ${d} + ${b} = ${a}.`;
    if (b < 10) { const u = a % 10; return `${a} − ${b} : je retire d'abord ${u} pour arriver à ${a - u}, puis encore ${b - u} : ${a - u} − ${b - u} = ${d}.`; }
    const db = b - b % 10, r = a - db, ub = b % 10, u = r % 10;
    if (!ub) return `${a} − ${b} : je retire ${db / 10} dizaines, ça fait ${d}.`;
    if (u >= ub) return `${a} − ${b} : je retire ${db}, ça fait ${r}, puis je retire ${ub} : ${d}.`;
    if (!u) return `${a} − ${b} : je retire ${db}, ça fait ${r}, puis je retire ${ub} : ${r} − ${ub} = ${d}.`;
    return `${a} − ${b} : je retire ${db}, ça fait ${r} ; puis je retire ${ub} : ${r} − ${u} = ${r - u}, et ${r - u} − ${ub - u} = ${d}.`;
  }
  function tirerSoustraction(max, m){
    if (max <= 10) {
      if (m === "massive") { const a = alea(6, 10); return [a, alea(3, a - 1)]; }
      const a = alea(3, 10); return [a, alea(1, 3)];
    }
    if (max <= 20) {
      if (m === "massive") { const a = alea(11, 18); return [a, alea(a % 10 + 1, 9)]; }
      const a = alea(11, 19); return Math.random() < .3 ? [a, 10] : [a, alea(1, a % 10 || 1)];
    }
    if (max <= 100) {
      const ta = alea(3, 9), tb = alea(1, ta - 1);
      if (m === "massive") { const ua = alea(0, 8); return [10 * ta + ua, Math.random() < .3 ? alea(ua + 1, 9) : 10 * tb + alea(ua + 1, 9)]; }
      const ua = alea(1, 9); return [10 * ta + ua, 10 * tb + alea(0, ua)];
    }
    if (m === "massive") { const a = alea(300, 990); return [a, alea(101, a - 100)]; }
    const a = 100 * alea(3, 9) + 10 * alea(1, 9); return Math.random() < .5 ? [a, 100 * alea(1, Math.floor(a / 100) - 1)] : [a, 10 * alea(1, a % 100 / 10)];
  }
  GENERATEURS.soustraction = (p = {}, m = "rapide") => {
    const max = p.max || 20, o = choisir(OBJETS);
    const [a, b] = tirerSoustraction(max, m === "boss" ? "massive" : m), d = a - b;
    const montrer = p.visuel && a <= 10;
    if (m === "boss") {   // the missing number: 13 − ? = 8
      return question({id: `G_sous_${a}-?=${d}`, question: `${a} − ? = ${d}`, dire: `${a} moins combien égale ${d} ?`,
        visuel: montrer ? `${fois(o[0], a)} − ❓ = ${fois(o[0], d)}` : "", reponse: b,
        distracteurs: fausses(b, [b + 1, b - 1, a + d], [b + 2, b - 2], 3, 0),
        indice: b === d ? "Ce qu'on enlève est égal à ce qui reste." : `Combien faut-il enlever à ${a} pour qu'il reste ${d} ?`,
        explication: `De ${d} à ${a}, il y a ${b} : ${d} + ${b} = ${a}, donc ${a} − ${b} = ${d}.`}, m);
    }
    const emprunt = a % 10 < b % 10 && a > 10;
    const erreur = emprunt ? (Math.floor(a / 10) - Math.floor(b / 10)) * 10 + (b % 10 - a % 10) : null;   // 63 − 28 → 45: the units turned round
    const pieges = [erreur, d + 1, d - 1, a > 20 ? inverse(d) : montrer || a + b > 20 ? null : a + b].filter(v => v != null && v !== d);
    return question({id: `G_sous_${a}-${b}`, question: `${a} − ${b} = ?`, dire: `Combien font ${a} moins ${b} ?`,
      visuel: montrer ? fois(o[0], a) : "", dessin: montrer ? {type: "barre", e: o[0], n: a, barres: b} : undefined,
      reponse: d, distracteurs: fausses(d, pieges, [d + 2, d - 2, ...(a > 10 ? [d + 10] : [])], 3, 0, montrer ? [a + b] : []),
      indice: a <= 10 ? "Recule sur tes doigts." : b > 9 ? "Retire d'abord les dizaines, puis les unités." : emprunt ? "Passe par la dizaine juste en dessous." : "Retire les unités.",
      explication: expliquerSoustraction(a, b), affirmer: k => `${a} − ${b} = ${k}`, affirmerDire: k => `${a} moins ${b} égale ${k}.`}, m);
  };

  /* COMPLÉMENTS : 7 + ? = 10 ; le boss va jusqu'à 20 ou 100 */
  GENERATEURS.complement = (p = {}, m = "rapide") => {
    let total = p.total || 10, n = alea(1, total - 1);
    if (m === "boss") { if (Math.random() < .5) { total = 20; n = alea(11, 19); } else { total = 100; n = 10 * alea(1, 9); } }
    const c = total - n, gauche = m === "massive" && Math.random() < .5;
    const visuel = m === "rapide" && total === 10 ? `${fois("🔵", Math.min(n, 5))}${fois("⚪", Math.max(0, 5 - n))} ${fois("🔵", Math.max(0, n - 5))}${fois("⚪", Math.min(5, 10 - n))}` : "";
    const explication = total === 10 ? `${n} et ${c} sont les amoureux de 10 : ${n} + ${c} = 10.`
      : total === 20 ? `${n} + ${c} = 20 : ${n % 10} + ${c} = 10, et avec la dizaine de ${n}, ça fait 20.`
      : `${n} + ${c} = 100 : ${n / 10} dizaines + ${c / 10} dizaines = 10 dizaines.`;
    return question({id: `G_comp_${n}+?=${total}`, question: gauche ? `? + ${n} = ${total}` : `${n} + ? = ${total}`,
      dire: gauche ? `Combien plus ${n} égale ${total} ?` : `${n} plus combien égale ${total} ?`, visuel, reponse: c,
      dessin: visuel ? {type: "dizaine", n} : undefined,
      distracteurs: fausses(c, [c + 1, c - 1, n], [c + 2, c - 2], 3, 0),
      indice: visuel ? "Compte les ronds blancs." : total === 100 ? "Compte les dizaines qui manquent pour aller jusqu'à 100." : "Pense aux amoureux de 10.",
      explication, affirmer: k => `${n} + ${k} = ${total}`, affirmerDire: k => `${n} plus ${k} égale ${total}.`}, m);
  };

  /* DOUBLES ET MOITIÉS ; le boss raconte une petite histoire */
  GENERATEURS.doubles = (p = {}, m = "rapide") => {
    const max = p.max || 10, o = choisir(OBJETS), grand = max > 10;
    const n = m === "massive" ? (grand ? alea(11, max) : alea(6, max)) : (grand ? 10 * alea(1, Math.floor(max / 10)) + choisir([0, 5]) : choisir([1, 2, 3, 4, 5, 10]));
    const pourquoi = k => k < 10 || k % 10 === 0 ? `${k} + ${k} = ${2 * k}` : `le double de ${k - k % 10} est ${2 * (k - k % 10)}, le double de ${k % 10} est ${2 * (k % 10)}, et ${2 * (k - k % 10)} + ${2 * (k % 10)} = ${2 * k}`;
    if (m === "boss") {
      const nom = choisir(["Matty", "Plumix", "Bärli", "Léiwchen", "Cavalin", "Poussik"]), ami = choisir(["Calculor", "Taktik", "Floradrag"]);
      return question({id: `G_double_recit_${n}_${o[1]}`, question: `${nom} a ${nombre(n, o)} ${o[0]}. ${ami} en a le double. Combien ${ami} a-t-il ${de(o[2])} ?`,
        dire: `${nom} a ${nombre(n, o)}. ${ami} en a le double. Combien ${ami} a-t-il ${de(o[2])} ?`, reponse: 2 * n,
        distracteurs: fausses(2 * n, [n + 2, n, 2 * n + 1], [2 * n - 1, 2 * n + 2], 3, 0), indice: "Le double, c'est deux fois le même nombre.",
        explication: `Le double de ${n}, c'est ${pourquoi(n)}.`}, m);
    }
    if (Math.random() < .5) {   // half of an even number
      const t = 2 * n, h = n;
      return question({id: `G_moitie_${t}`, question: `La moitié de ${t} ?`, dire: `Quelle est la moitié de ${t} ?`,
        visuel: p.visuel || (!grand && t <= 10) ? fois(o[0], t) : "", reponse: h, distracteurs: fausses(h, [h + 1, h - 1, 2 * t], [h + 2, h - 2], 3, 0),
        indice: "Partage en deux parts pareilles.", explication: `La moitié de ${t}, c'est ${h}, car ${h} + ${h} = ${t}.`,
        affirmer: k => `La moitié de ${t} est ${k}.`, affirmerDire: k => `La moitié de ${t} est ${k}.`}, m);
    }
    return question({id: `G_double_${n}`, question: `Le double de ${n} ?`, dire: `Quel est le double de ${n} ?`,
      visuel: p.visuel || (!grand && n <= 5) ? `${fois(o[0], n)} ${fois(o[0], n)}` : "", reponse: 2 * n,
      distracteurs: fausses(2 * n, [n + 2, 2 * n + 1, 2 * n - 1], [2 * n + 2, 2 * n - 2], 3, 0), indice: "Le double, c'est deux fois le même nombre.",
      explication: `Le double de ${n}, c'est ${pourquoi(n)}.`, affirmer: k => `Le double de ${n} est ${k}.`, affirmerDire: k => `Le double de ${n} est ${k}.`}, m);
  };
})();
