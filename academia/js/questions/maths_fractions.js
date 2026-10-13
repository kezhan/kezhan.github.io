/* Académia : générateur des fractions du grand (CE2 au CM2), en dessins d'abord : la pizza coupée en parts égales
   (ce qui reste), la pizza à choisir parmi d'autres (et le piège des parts inégales), la tablette de chocolat pour
   la fraction d'une quantité, deux pizzas côte à côte pour comparer ou trouver des fractions égales.
   GENERATEURS.fractions({mode: "parts" | "quantite" | "comparer" | "egales"}, mecanique). Une fraction s'écrit
   « 3/4 » dans les données ; le panneau la pose l'une sur l'autre (js/question_figures.js) et la voix la dit en
   mots (« trois quarts ») : jamais « 3/4 » dans ce qui est lu à voix haute, chaque fraction d'un bouton a son nom. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

(() => {
  const G = () => GEN_OUTILS, C = () => GEN_CM;
  const NOMS = {2: ["demi", "demis"], 3: ["tiers", "tiers"], 4: ["quart", "quarts"], 5: ["cinquième", "cinquièmes"], 6: ["sixième", "sixièmes"],
    8: ["huitième", "huitièmes"], 10: ["dixième", "dixièmes"], 12: ["douzième", "douzièmes"]};
  const pgcd = (a, b) => b ? pgcd(b, a % b) : a;
  const lire = f => f.split("/").map(Number);
  const valeur = f => { const [k, d] = lire(f); return k / d; };
  const memeValeur = (f, g) => { const [a, b] = lire(f), [c, d] = lire(g); return a * d === b * c; };
  const maj = t => t.charAt(0).toUpperCase() + t.slice(1);
  // "trois quarts", "un demi", "cinq huitièmes"
  const nom = f => { const [k, d] = lire(f); return `${C().lettres(k)} ${NOMS[d][k > 1 ? 1 : 0]}`; };
  // the part of a quantity: "la moitié", "le tiers", "les trois quarts"
  const part = f => { const [k, d] = lire(f); return k === 1 ? (d === 2 ? "la moitié" : `le ${NOMS[d][0]}`) : `les ${nom(f)}`; };
  // what is left of a pizza, said: « la moitié », never « un demi de la pizza »; « trois quarts » (and « un demi, la moitié »
  // in an explanation, where 1/2 keeps its name)
  const reste = f => f === "1/2" ? "la moitié" : nom(f), resteDit = f => f === "1/2" ? "un demi, la moitié" : nom(f);
  const legendes = l =>Object.fromEntries(l.filter(f => /^\d+\/\d+$/.test(f)).map(f => [f, nom(f)]));
  const que = t => /^(?!onze)[aeiouyéè]/i.test(t) ? `qu'${t}` : `que ${t}`;   // « plus qu'un tiers », « plus que huit huitièmes »
  // wrong fractions: never one equal in value to the right one (2/4 is also 1/2), never twice, always one with a name
  function fausses(rep, pieges, n = 3){
    const out = [];
    pieges.forEach(f => {
      const [k, d] = lire(f);
      if (out.length < n && k > 0 && NOMS[d] && !memeValeur(f, rep) && !out.some(g => memeValeur(g, f))) out.push(f);
    });
    return out;
  }
  // why one fraction is bigger: the same parts, more of them; or one part, cut in fewer pieces. `moins`: told from the
  // smaller one (« the smallest fraction » is not explained by the biggest)
  function pourquoi(f, g, moins = false){
    const [a, b] = lire(f), [c, d] = lire(g), [gr, pe] = valeur(f) > valeur(g) ? [f, g] : [g, f];
    const fin = moins ? `${maj(nom(pe))}, c'est moins ${que(nom(gr))}.` : `${maj(nom(gr))}, c'est plus ${que(nom(pe))}.`;
    if (b === d) {
      const [x, y] = [Math.max(a, c), Math.min(a, c)];
      return `Les parts sont pareilles, des ${NOMS[b][1]} : ${moins ? `${y} ${y > 1 ? "parts" : "part"}, c'est moins que ${x}` : `${x} parts, c'est plus que ${y}`}. ${fin}`;
    }
    if (a === c) return `Plus on coupe en parts nombreuses, plus chaque part est petite : ${fin.charAt(0).toLowerCase() + fin.slice(1)}`;
    return fin;
  }

  /* LES PARTS : ce qui reste de la pizza ; la pizza qui montre une fraction ; le chef raconte, ou piège les parts */
  function parts(m){
    const {alea, choisir} = G(), {vraiFaux, question} = C();
    if (m === "bouclier") {   // the pizza drawn, a part said: right or wrong
      const d = choisir([2, 3, 4, 6, 8]), k = alea(1, d - 1), f = `${k}/${d}`;
      const x = Math.random() < .5 ? f : fausses(f, [`${d - k}/${d}`, `${k}/${d + 1}`, `${k + 1}/${d}`, `1/${d === 2 ? 4 : 2}`])[0];
      return vraiFaux({id: `G_frac_reste_${f}=${x}`, question: `Il reste ${x} de la pizza.`, dire: `Il reste ${reste(x)} de la pizza.`, vrai: x === f,
        dessin: {type: "pizza", k, d}, explication: `La pizza est coupée en ${d} parts égales, et il en reste ${k} : ${resteDit(f)}.`});
    }
    if (m === "boss" && Math.random() < .5) {   // only equal parts make quarters
      const d = choisir([3, 4]);
      return question({id: `G_frac_coupe_${d}`, question: `Quelle pizza est coupée en ${NOMS[d][1]} ?`, dire: `Quelle pizza est coupée en ${NOMS[d][1]} ?`,
        dessin: {type: "boutons_pizza", boutons: "pizza"}, reponse: `${d}/${d}`, distracteurs: [`${d}/${d}*`, `${d + 1}/${d + 1}`, `${d - 1}/${d - 1}`],
        indice: "Compte les parts, et regarde si elles sont toutes pareilles.",
        explication: `Des ${NOMS[d][1]}, ce sont ${d} parts égales. Des parts de tailles différentes ne sont pas des ${NOMS[d][1]}.`}, m);
    }
    if (m === "boss") {   // told, not drawn
      const d = choisir([5, 6, 8, 10]), mange = alea(1, d - 2), k = d - mange, rep = `${k}/${d}`;
      const faux = fausses(rep, [`${mange}/${d}`, `${mange}/${k}`, `${k}/${mange}`, `${k - 1}/${d}`, `${k + 1}/${d}`]);
      return question({id: `G_frac_recit_${mange}_${d}`, question: `Une pizza est coupée en ${d} parts égales. On en mange ${mange}. Quelle part de la pizza reste-t-il ?`,
        dire: `Une pizza est coupée en ${d} parts égales. On en mange ${mange}. Quelle part de la pizza reste-t-il ?`, reponse: rep, distracteurs: faux,
        legendes: legendes([rep, ...faux]), indice: "En combien de parts est-elle coupée ? Combien en reste-t-il ?",
        explication: `Elle est coupée en ${d} parts, et il en reste ${d} − ${mange} = ${k} : ${nom(rep)}.`}, m);
    }
    const d = m === "rapide" ? choisir([2, 3, 4]) : choisir([3, 4, 6, 8]), k = m === "rapide" ? 1 : alea(2, d - 1), rep = `${k}/${d}`;
    if (m === "massive" && Math.random() < .5) {   // the pizzas are the answers; one is cut in unequal parts
      return question({id: `G_frac_montre_${rep}`, question: `Où reste-t-il ${nom(rep)} de la pizza ?`, dire: `Où reste-t-il ${nom(rep)} de la pizza ?`,
        dessin: {type: "boutons_pizza", boutons: "pizza"}, reponse: rep,
        distracteurs: [`${rep}*`, ...fausses(rep, [`${d - k}/${d}`, `${k}/${d + 2}`, `${k - 1}/${d}`, `${k + 1}/${d}`], 2)],
        indice: "Regarde en combien de parts égales chaque pizza est coupée.",
        explication: `${maj(nom(rep))} : la pizza coupée en ${d} parts égales, et il en reste ${k}. Des parts de tailles différentes ne comptent pas.`}, m);
    }
    const faux = fausses(rep, [`${d - k}/${d}`, `${k}/${d - k}`, `${k}/${d + 1}`, `${k + 1}/${d}`, `1/${d === 2 ? 3 : 2}`, `${k}/${d * 2}`]);
    return question({id: `G_frac_reste_${rep}`, question: "Quelle part de la pizza reste-t-il ?", dire: "Quelle part de la pizza reste-t-il ?",
      dessin: {type: "pizza", k, d}, reponse: rep, distracteurs: faux, legendes: legendes([rep, ...faux]),
      indice: "Compte toutes les parts, puis celles qui restent.",
      explication: `La pizza est coupée en ${d} parts égales, et il en reste ${k} : ${resteDit(rep)}.`}, m);
  }

  /* UNE FRACTION D'UNE QUANTITÉ : le quart de 12 (la tablette), les trois quarts de 20, ce qui reste après un don */
  function quantite(m){
    const {alea, choisir, melanger, OBJETS} = G(), {faussesNb, vraiFaux, question, PERSOS} = C();
    const d = m === "rapide" ? choisir([2, 3, 4, 5]) : choisir([3, 4, 5, 10]), q = alea(2, m === "rapide" ? 6 : d === 10 ? 5 : 9), n = d * q;
    const k = m === "rapide" ? 1 : choisir([...Array(d - 1).keys()].map(i => i + 1).filter(i => pgcd(i, d) === 1));   // "les trois quarts", never "les deux quarts"
    const rep = k * q, f = `${k}/${d}`, o = choisir(OBJETS);
    const une = `On partage ${n} en ${d} parts égales : ${d} × ${q} = ${n}, donc une part, c'est ${q}.`;
    const expl = k === 1 ? une : `${une} ${maj(part(f))}, ce sont ${k} parts : ${k} × ${q} = ${rep}.`;
    if (m === "bouclier") {
      const x = Math.random() < .5 ? rep : faussesNb(rep, [n - rep, q + 1, rep + q], 1, 1)[0];
      return vraiFaux({id: `G_frac_qte_${f}_${n}=${x}`, question: `${maj(part(f))} de ${n}, c'est ${x}.`, vrai: x === rep, explication: expl});
    }
    if (m === "boss") {
      const [x, y] = melanger(PERSOS), t = `${x} a ${n} ${o[2]}. Il en donne ${part(f)} à ${y}. Combien lui en reste-t-il ?`;
      return question({id: `G_frac_donne_${f}_${n}_${o[1]}`, question: t.replace(`${o[2]}.`, `${o[2]} ${o[0]}.`), dire: t, reponse: n - rep,
        distracteurs: faussesNb(n - rep, [rep, n - k, n - d], 3, 0), indice: "Cherche d'abord combien il en donne.",
        explication: `Il en donne ${part(f)}. ${expl} Il lui en reste ${n} − ${rep} = ${n - rep}.`}, m);
    }
    const tablette = m === "rapide" && n <= 30;
    return question({id: `G_frac_qte_${f}_${n}`, question: m === "rapide" ? `${maj(part(f))} de ${n} carrés de chocolat, c'est combien de carrés ?` : `${f} de ${n} = ?`,
      dire: m === "rapide" ? `${maj(part(f))} de ${n} carrés de chocolat, c'est combien de carrés ?` : `${maj(part(f))} de ${n}, c'est combien ?`,
      dessin: tablette ? {type: "tablette", lignes: d, colonnes: q} : undefined, reponse: rep,
      distracteurs: faussesNb(rep, [n - rep, d, q === rep ? q + d : q, rep + 1], 3, 1),
      indice: tablette ? "Une rangée de la tablette, c'est une part." : "Partage d'abord en parts égales.", explication: expl}, m);
  }

  /* COMPARER : deux pizzas côte à côte ; la plus grande ou la plus petite fraction ; le signe, avec une pizza entière */
  const PAIRES = [["1/2", "1/4"], ["1/3", "1/6"], ["1/2", "1/3"], ["1/4", "1/8"], ["3/8", "5/8"], ["2/6", "5/6"], ["3/4", "1/4"], ["2/3", "1/3"]];
  function comparer(m){
    const {choisir, melanger} = G(), {vraiFaux, question} = C();
    if (m === "rapide") {
      const [f, g] = melanger(choisir(PAIRES)), rep = valeur(f) > valeur(g) ? f : g;
      return question({id: `G_frac_cmp_${f}_${g}`, question: "Quelle part de pizza est la plus grande ?", dire: "Quelle part de pizza est la plus grande ?",
        dessin: {type: "pizzas", fractions: [f, g]}, reponse: rep, distracteurs: [rep === f ? g : f, "c'est pareil"], legendes: legendes([f, g]),
        indice: "Regarde les deux pizzas : où en reste-t-il le plus ?", explication: pourquoi(f, g)}, m);
    }
    if (m === "massive") {
      const unes = Math.random() < .5, d = choisir([6, 8, 10, 12]);
      const l = unes ? melanger(["1/2", "1/3", "1/4", "1/5", "1/8", "1/10"]).slice(0, 4) : melanger([1, 3, 5, 7, 9, 11].filter(k => k < d)).slice(0, 4).map(k => `${k}/${d}`);
      const grand = Math.random() < .5, tri = l.slice().sort((a, b) => valeur(a) - valeur(b)), rep = grand ? tri[tri.length - 1] : tri[0], rival = grand ? tri[tri.length - 2] : tri[1];
      return question({id: `G_frac_${grand ? "max" : "min"}_${tri.join("_")}`, question: `Quelle est la plus ${grand ? "grande" : "petite"} fraction ?`,
        dire: `Quelle est la plus ${grand ? "grande" : "petite"} fraction : ${l.map(nom).join(", ")} ?`, reponse: rep, distracteurs: l.filter(f => f !== rep), legendes: legendes(l),
        indice: unes ? "Une pizza coupée en beaucoup de parts donne de toutes petites parts." : "Les parts sont pareilles : compte combien il y en a.",
        explication: pourquoi(rep, rival, !grand)}, m);
    }
    // the sign: two fractions, or a fraction and 1 (a whole pizza)
    let f, g = "1";
    if (Math.random() < .4) { const d = choisir([2, 3, 4, 5, 8]); f = `${choisir([1, d - 1, d, d + 1, d + 3])}/${d}`; }
    else [f, g] = melanger(choisir([...PAIRES, ["2/3", "2/5"], ["3/4", "3/8"], ["5/6", "5/8"]]));
    const cmp = Math.sign(valeur(f) - (g === "1" ? 1 : valeur(g))), signe = cmp > 0 ? ">" : cmp < 0 ? "<" : "=";
    const dit = x => x === "1" ? "1" : nom(x), [k, d] = lire(f);
    const expl = g !== "1" ? pourquoi(f, g) : k === d ? `${maj(nom(f))}, ce sont toutes les parts : une pizza entière, c'est 1.`
      : k > d ? `${maj(nom(f))}, c'est plus ${que(nom(`${d}/${d}`))}, une pizza entière : plus que 1.` : `${maj(nom(f))} : il manque des parts pour faire une pizza entière, moins que 1.`;
    if (m === "bouclier") {
      const s = Math.random() < .5 ? signe : signe === ">" ? "<" : ">";
      return vraiFaux({id: `G_frac_signe_${f}${s}${g}`, question: `${f} ${s} ${g}`, vrai: s === signe, explication: expl,
        dire: `${maj(dit(f))}, c'est ${s === ">" ? `plus ${que(dit(g))}` : s === "<" ? `moins ${que(dit(g))}` : `égal à ${dit(g)}`}.`});
    }
    return question({id: `G_frac_signe_${f}_${g}`, question: `Quel signe va entre ${f} et ${g} ?`, dire: `Quel signe va entre ${dit(f)} et ${dit(g)} ?`,
      reponse: signe, distracteurs: [">", "<", "="].filter(s => s !== signe), explication: expl,
      indice: g === "1" ? "Combien faut-il de parts pour faire une pizza entière ?" : "Pense à deux pizzas pareilles."}, m);
  }

  /* FRACTIONS ÉGALES : la même part de pizza, coupée en plus de morceaux ; le chef regroupe (6/8 = 3/4) */
  const BASES = [["1/2", 4], ["1/2", 6], ["1/3", 6], ["1/4", 8], ["1/2", 8], ["2/3", 6], ["3/4", 8], ["1/5", 10], ["2/5", 10]];
  function egales(m){
    const {choisir} = G(), {faussesNb, vraiFaux, question} = C();
    const [b, d2] = choisir(BASES), [k, d] = lire(b), x = d2 / d, k2 = k * x, egale = `${k2}/${d2}`;
    const coupe = `Chaque ${NOMS[d][0]} est coupé en ${x} : ${k} × ${x} = ${k2}. ${maj(nom(b))}, c'est ${nom(egale)}.`;
    if (m === "bouclier") {
      const autre = Math.random() < .5 ? egale : `${k2 + 1}/${d2}`;
      return vraiFaux({id: `G_frac_egal_${b}=${autre}`, question: `${b} = ${autre}`, dire: `${maj(nom(b))}, c'est pareil que ${nom(autre)}.`,
        vrai: memeValeur(b, autre), explication: coupe});
    }
    if (m === "rapide") {
      return question({id: `G_frac_egal_${b}_${d2}`, question: `${b} = ?/${d2}`, dire: `${maj(nom(b))}, c'est combien de ${NOMS[d2][1]} ?`,
        dessin: {type: "pizzas", fractions: [b, `?/${d2}`]}, reponse: k2, distracteurs: faussesNb(k2, [k, d2 - k2, k2 + 1, d], 3, 1),
        indice: "Sur la deuxième pizza, chaque part de la première est coupée en plus petits morceaux.", explication: coupe}, m);
    }
    if (m === "massive") {
      const faux = fausses(egale, [`${k}/${d2}`, `${k2}/${d2 + 2}`, `${d}/${k2}`, `${k2 + 1}/${d2}`, `${k + 1}/${d + 1}`, `${k2 - 1}/${d2}`]);
      return question({id: `G_frac_egale_a_${b}_${d2}`, question: `Quelle fraction est égale à ${b} ?`, dire: `Quelle fraction est égale à ${nom(b)} ?`,
        reponse: egale, distracteurs: faux, legendes: legendes([egale, ...faux]), indice: "Coupe chaque part en morceaux pareils, et compte-les.",
        explication: `On coupe chaque part en ${x} : ${k} × ${x} = ${k2}, et ${d} × ${x} = ${d2}. ${maj(nom(b))}, c'est ${nom(egale)}.`}, m);
    }
    // the boss puts the small parts together: 6/8 = ?/4
    const g = pgcd(k2, d2), kp = k2 / g, dp = d2 / g, petit = `${kp}/${dp}`;
    return question({id: `G_frac_regroupe_${egale}_${dp}`, question: `${egale} = ?/${dp}`, dire: `${maj(nom(egale))}, c'est combien de ${NOMS[dp][1]} ?`,
      reponse: kp, distracteurs: faussesNb(kp, [k2, kp + 1, dp - kp, g], 3, 1), indice: "Regroupe les petites parts pour en faire de plus grandes.",
      explication: `On groupe les ${NOMS[d2][1]} par ${g} : ${g} × ${kp} = ${k2}, et ${g} × ${dp} = ${d2}. ${maj(nom(egale))}, c'est ${nom(petit)}.`}, m);
  }

  GENERATEURS.fractions = (p = {}, m = "rapide") => ({parts, quantite, comparer, egales}[p.mode] || parts)(m);
})();
