/* Académia : générateurs de multiplications, divisions et problèmes en une étape (contrat : conception/architecture.md).
   Les outils communs (GEN_OUTILS) sont dans maths.js ; ils sont lus à l'appel, l'ordre de chargement ne compte pas. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

(() => {
  const PERSOS = ["Matty", "Plumix", "Bärli", "Léiwchen", "Cavalin", "Poussik"];
  const ORDRE_FACILE = [10, 2, 5, 9, 4, 3, 1, 6, 8, 7];   // the table used to explain, easiest first
  // how to find a × b with the easiest of the two tables
  function astuce(a, b){
    const [t, k] = ORDRE_FACILE.indexOf(a) <= ORDRE_FACILE.indexOf(b) ? [a, b] : [b, a], r = a * b;
    const debut = `${a} × ${b}`;
    switch (t) {
      case 10: return `${debut} : j'écris un zéro après ${k}, ça fait ${r}.`;
      case 1: return `${debut} : ${k} une seule fois, c'est ${r}.`;
      case 2: return `${debut}, c'est le double de ${k} : ${k} + ${k} = ${r}.`;
      case 5: return `${debut} : ${k} × 10 = ${10 * k}, et la moitié de ${10 * k}, c'est ${r}.`;
      case 9: return `${debut} : ${k} × 10 = ${10 * k}, moins ${k}, ça fait ${r}.`;
      case 4: return `${debut}, c'est le double du double : ${k} × 2 = ${2 * k}, puis ${2 * k} × 2 = ${r}.`;
      case 3: return `${debut}, c'est ${k} + ${k} + ${k} = ${r}.`;
      case 6: return `${debut} : ${k} × 5 = ${5 * k}, plus ${k}, ça fait ${r}.`;
      case 8: return `${debut}, c'est le double de ${k} × 4 = ${4 * k} : ${r}.`;
      default: return `${debut} : ${k} × 5 = ${5 * k}, plus ${k} × 2 = ${2 * k}, ça fait ${r}.`;
    }
  }
  const facteurs = (tables, m) => {   // [table, other factor]: easy facts for a quick attack, hard ones for a heavy strike
    const durs = tables.filter(t => t >= 6), t = m === "rapide" ? GEN_OUTILS.choisir(tables) : GEN_OUTILS.choisir(durs.length ? durs : tables);
    return [t, m === "rapide" ? GEN_OUTILS.choisir([1, 2, 3, 4, 5, 10]) : GEN_OUTILS.alea(6, 9)];
  };
  const pieges = (t, k) => [t * (k + 1), t * (k - 1), (t + 1) * k, GEN_OUTILS.inverse(t * k)].filter(v => v != null && v > 0);

  /* MULTIPLICATIONS : 7 × 8 = ? ; le boss raconte des groupes */
  GENERATEURS.multiplication = (p = {}, m = "rapide") => {
    const {alea, choisir, fois, fausses, question, OBJETS, nombre, de} = GEN_OUTILS;
    const [t, k] = facteurs(p.tables || [2, 3, 4, 5, 6, 7, 8, 9], m === "boss" ? "massive" : m), r = t * k;
    const [a, b] = Math.random() < .5 ? [t, k] : [k, t], o = choisir(OBJETS);
    if (m === "boss") {
      const boite = choisir(["boîtes", "sacs", "paniers"]);
      return question({id: `G_mult_recit_${k}x${t}_${o[1]}`, question: `Il y a ${k} ${boite} de ${t} ${o[2]} ${o[0]}. Combien ${de(o[2])} en tout ?`,
        dire: `Il y a ${k} ${boite} de ${t} ${o[2]}. Combien ${de(o[2])} en tout ?`, reponse: r, distracteurs: fausses(r, [t + k, ...pieges(t, k)], [r + 1, r - 1], 3, 1),
        indice: `${k} fois ${t}, c'est une multiplication.`, explication: `${k} ${boite} de ${t}, c'est ${k} fois ${t}. ${astuce(k, t)}`}, m);
    }
    return question({id: `G_mult_${a}x${b}`, question: `${a} × ${b} = ?`, dire: `Combien font ${a} fois ${b} ?`,
      visuel: p.visuel && r <= 20 ? Array(k).fill(fois(o[0], t)).join(" ") : "", reponse: r,
      distracteurs: fausses(r, pieges(t, k), [r + 1, r - 1, t + k], 3, 0), indice: k === 1 ? "Fois 1, le nombre ne change pas." : `Récite la table de ${t} jusqu'à ${t} × ${k}.`,
      explication: astuce(a, b), affirmer: v => `${a} × ${b} = ${v}`, affirmerDire: v => `${a} fois ${b} égale ${v}.`}, m);
  };

  /* MULTIPLICATIONS À TROUS : 7 × ? = 56 ; le boss range en paquets */
  GENERATEURS.multiplication_trou = (p = {}, m = "rapide") => {
    const {choisir, fausses, question, OBJETS} = GEN_OUTILS;
    const [t, k] = facteurs(p.tables || [2, 3, 4, 5, 6, 7, 8, 9], m === "boss" ? "massive" : m), r = t * k, o = choisir(OBJETS);
    const faux = fausses(k, [k + 1, k - 1, t === k ? k + 2 : t], [k + 2, k - 2], 3, 1);
    if (m === "boss") {
      return question({id: `G_trou_recit_${r}_${t}_${o[1]}`, question: `On range ${r} ${o[2]} ${o[0]} par paquets de ${t}. Combien de paquets ?`,
        dire: `On range ${r} ${o[2]} par paquets de ${t}. Combien de paquets ?`, reponse: k, distracteurs: faux,
        indice: t === k ? `Quel nombre, multiplié par lui-même, donne ${r} ?` : `Combien de fois ${t} pour faire ${r} ?`,
        explication: `Dans la table de ${t} : ${t} × ${k} = ${r}, donc ${k} paquets.`}, m);
    }
    const gauche = Math.random() < .5;
    return question({id: `G_trou_${gauche ? `?x${t}` : `${t}x?`}=${r}`, question: gauche ? `? × ${t} = ${r}` : `${t} × ? = ${r}`,
      dire: gauche ? `Combien fois ${t} égale ${r} ?` : `${t} fois combien égale ${r} ?`, reponse: k, distracteurs: faux,
      indice: t === k ? `Quel nombre, multiplié par lui-même, donne ${r} ?` : `Cherche ${r} dans la table de ${t}.`,
      explication: `Dans la table de ${t} : ${t} × ${k} = ${r}, donc le nombre caché est ${k}.`,
      affirmer: v => `${t} × ${v} = ${r}`, affirmerDire: v => `${t} fois ${v} égale ${r}.`}, m);
  };

  /* PARTAGES : 12 potions pour 3 compagnons ; le boss fait des groupes */
  GENERATEURS.partage = (p = {}, m = "rapide") => {
    const {alea, choisir, fois, fausses, question, OBJETS, nombre} = GEN_OUTILS;
    const max = p.max || 30, o = choisir(OBJETS), perso = choisir(PERSOS);
    const g = m === "rapide" ? alea(2, 3) : alea(2, 5), q = m === "rapide" ? alea(2, 4) : alea(3, Math.max(3, Math.floor(max / g))), total = g * q;
    const faux = fausses(q, [q + 1, q - 1, total - g, g], [q + 2], 3, 1);
    if (m === "boss") {
      return question({id: `G_groupes_${total}_${q}_${o[1]}`, question: `${perso} a ${total} ${o[2]} ${o[0]}. Il en met ${q} dans chaque boîte. Combien de boîtes remplit-il ?`,
        dire: `${perso} a ${total} ${o[2]}. Il en met ${q} dans chaque boîte. Combien de boîtes remplit-il ?`, reponse: g,
        distracteurs: fausses(g, [g + 1, g - 1, total - q, q], [g + 2], 3, 1), indice: g === q ? "Fais des paquets pareils, puis compte-les." : `Fais des paquets de ${q}.`,
        explication: `Je fais des paquets de ${q} : ${Array(g).fill(q).join(" + ")} = ${total}. Il y a ${g} paquets, car ${g} × ${q} = ${total}.`}, m);
    }
    const dessin = total <= 20;   // the things to share are drawn: their emoji leaves the sentence, it would be counted too
    return question({id: `G_partage_${total}_${g}_${o[1]}`, question: `${perso} partage ${total} ${o[2]}${dessin ? "" : " " + o[0]} entre ${g} amis. Combien chacun en a-t-il ?`,
      dire: `${perso} partage ${total} ${o[2]} entre ${g} amis. Combien chacun en a-t-il ?`, visuel: dessin ? fois(o[0], total) : "", reponse: q,
      distracteurs: faux, indice: `Donne ${nombre(1, o)} à chacun, chacun son tour.`,
      explication: `Je donne ${nombre(1, o)} à chacun, chacun son tour : ${g} par tour. Au bout de ${q} tours, il n'en reste plus : chacun en a ${q}, car ${g} × ${q} = ${total}.`,
      affirmer: v => `${total} ${o[2]} pour ${g} amis : ${v} chacun.`, affirmerDire: v => `${total} ${o[2]} pour ${g} amis, ça fait ${v} chacun.`}, m);
  };

  /* DIVISIONS : 56 : 8 = ? (le signe « : » de l'école) */
  GENERATEURS.division = (p = {}, m = "rapide") => {
    const {fausses, question} = GEN_OUTILS;
    if (m === "boss") return GENERATEURS.partage({max: 50}, "boss");
    const [t, k] = facteurs(p.tables || [2, 3, 4, 5, 6, 7, 8, 9], m), r = t * k;
    return question({id: `G_div_${r}:${t}`, question: `${r} : ${t} = ?`, dire: `Combien font ${r} divisé par ${t} ?`, reponse: k,
      distracteurs: fausses(k, [k + 1, k - 1, t === k ? k + 2 : t], [k + 2, k - 2], 3, 1),
      indice: t === k ? `Quel nombre, multiplié par lui-même, donne ${r} ?` : `Cherche ${r} dans la table de ${t}.`,
      explication: `${r} : ${t}, je cherche dans la table de ${t} : ${t} × ${k} = ${r}, donc ${r} : ${t} = ${k}.`,
      affirmer: v => `${r} : ${t} = ${v}`, affirmerDire: v => `${r} divisé par ${t} égale ${v}.`}, m);
  };

  /* PROBLÈMES EN UNE ÉTAPE : ajouter, enlever, comparer ; groupes et partages */
  GENERATEURS.probleme = (p = {}, m = "rapide") => {
    const {alea, choisir, fois, fausses, question, OBJETS, nombre, de} = GEN_OUTILS;
    const ops = p.ops || ["add", "sous"], max = p.max || 20, o = choisir(OBJETS), [x, y] = GEN_OUTILS.melanger(PERSOS);
    const op = choisir(ops), haut = m === "rapide" ? Math.min(10, max) : max;
    let texte, rep, faux, expl;
    if (op === "add" || op === "sous") {
      let a = alea(3, haut - 2), b = alea(1, Math.min(9, haut - a));
      if (op === "sous") [a, b] = [a + b, b];
      if (m === "boss") {
        const sorte = choisir(["plus", "moins", "ecart"]);
        if (sorte !== "plus") { a = alea(5, haut); b = alea(1, Math.min(9, a - 2)); }
        if (sorte === "plus") { texte = `${x} a ${nombre(a, o)} ${o[0]}. ${y} en a ${b} de plus. Combien ${y} a-t-il ${de(o[2])} ?`; rep = a + b; faux = [a - b]; expl = `${b} de plus que ${a} : ${a} + ${b} = ${rep}.`; }
        else if (sorte === "moins") { texte = `${x} a ${nombre(a, o)} ${o[0]}. ${y} en a ${b} de moins. Combien ${y} a-t-il ${de(o[2])} ?`; rep = a - b; faux = [a + b]; expl = `${b} de moins que ${a} : ${a} − ${b} = ${rep}.`; }
        else { const c = a - b; texte = `${x} a ${nombre(a, o)} ${o[0]}, ${y} en a ${c}. Combien ${x} en a-t-il de plus que ${y} ?`; rep = b; faux = [a + c]; expl = `De ${c} à ${a}, il y a ${b} : ${a} − ${c} = ${b}.`; }
      } else if (op === "add") {
        texte = `${x} a ${nombre(a, o)} ${o[0]}. Il en trouve encore ${b}. Combien en a-t-il maintenant ?`; rep = a + b; faux = [a - b];
        expl = `Il en trouve encore : il en a plus, c'est une addition. ${a} + ${b} = ${rep}.`;
      } else {
        texte = `${x} a ${nombre(a, o)} ${o[0]}. Il en donne ${b} à ${y}. Combien lui en reste-t-il ?`; rep = a - b; faux = [a + b];
        expl = `Il en donne : il en a moins, c'est une soustraction. ${a} − ${b} = ${rep}.`;
      }
    } else {
      const t = m === "rapide" ? alea(2, 5) : alea(2, 9), k = m === "rapide" ? alea(2, 5) : alea(2, Math.max(2, Math.min(9, Math.floor(max / t)))), r = t * k;
      if (op === "mult") {
        texte = `${x} a ${k} boîtes de ${t} ${o[2]} ${o[0]}. Combien ${de(o[2])} en tout ?`; rep = r; faux = [t + k, t * (k + 1)];
        expl = `${k} boîtes de ${t}, c'est ${k} fois ${t} : ${k} × ${t} = ${r}.`;
      } else if (m === "boss") {
        texte = `${x} range ${r} ${o[2]} ${o[0]} par paquets de ${t}. Combien de paquets ?`; rep = k; faux = [r - t, k + 1];
        expl = `Combien de fois ${t} dans ${r} ? ${k} × ${t} = ${r}, donc ${r} : ${t} = ${k}.`;
      } else {
        texte = `${x} partage ${r} ${o[2]} ${o[0]} entre ${t} amis. Combien chacun en a-t-il ?`; rep = k; faux = [r - t, k + 1];
        expl = `Partager en ${t}, c'est diviser : ${r} : ${t} = ${k}, car ${t} × ${k} = ${r}.`;
      }
    }
    return question({id: `G_pb_${op}_${texte.replace(/[^0-9]+/g, "_")}_${o[1]}`, question: texte, dire: texte,
      reponse: rep, distracteurs: fausses(rep, faux, [rep + 1, rep - 1, rep + 2], 3, 0), indice: "Il en aura plus ou moins ? Choisis le bon calcul.",
      explication: expl, affirmer: v => `${texte} Réponse : ${v}.`, affirmerDire: v => `${texte} La réponse est ${v}.`}, m);
  };
})();
