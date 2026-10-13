/* Académia : générateurs des grands nombres jusqu'à 10 000, pour le grand (CE2, CM1) : écrire en chiffres un nombre
   dit ou écrit en lettres, chiffre et valeur d'un chiffre selon son rang, piège du zéro, nombre de centaines,
   comparer, ranger, nombre suivant et précédent. Les nombres sont écrits comme à l'école (4 782, une espace
   insécable entre les classes) ; en lettres, l'orthographe rectifiée de 1990, celle des programmes (deux-mille-six).
   Ce fichier porte aussi les outils communs des maths du grand (GEN_CM), lus à l'appel par maths_calcul.js et
   maths_fractions.js. Outils de base : GEN_OUTILS, dans maths.js. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

var GEN_CM = self.GEN_CM = (() => {
  const UNITES = ["zéro", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix", "onze", "douze",
    "treize", "quatorze", "quinze", "seize"];
  const DIZAINES = {2: "vingt", 3: "trente", 4: "quarante", 5: "cinquante", 6: "soixante"};
  function sousCent(n){
    if (n < 17) return UNITES[n];
    if (n < 20) return "dix-" + UNITES[n - 10];
    const d = Math.floor(n / 10), u = n % 10;
    if (d === 7) return n === 71 ? "soixante-et-onze" : "soixante-" + sousCent(n - 60);
    if (d === 8) return u ? "quatre-vingt-" + UNITES[u] : "quatre-vingts";
    if (d === 9) return "quatre-vingt-" + sousCent(n - 80);
    return DIZAINES[d] + (u === 1 ? "-et-un" : u ? "-" + UNITES[u] : "");
  }
  function sousMille(n){
    const c = Math.floor(n / 100), r = n % 100;
    if (!c) return sousCent(r);
    const cent = c === 1 ? "cent" : `${UNITES[c]}-cent${r ? "" : "s"}`;
    return r ? `${cent}-${sousCent(r)}` : cent;
  }
  // a whole number from 0 to 10 000 in letters, rectified spelling (every part joined by a hyphen)
  function lettres(n){
    const m = Math.floor(n / 1000), r = n % 1000;
    if (!m) return sousMille(r);
    const mille = m === 1 ? "mille" : `${sousMille(m).replace(/(cent|vingt)s$/, "$1")}-mille`;   // « trois-mille », never « troi »
    return r ? `${mille}-${sousMille(r)}` : mille;
  }
  // as the school writes it: 4 782, 10 000 (a no-break space between the classes, from four digits)
  const ecrit = n => n < 1000 ? String(n) : String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " ");
  const RANGS = ["unités", "dizaines", "centaines", "milliers"], RANG1 = ["unité", "dizaine", "centaine", "millier"];
  const chiffreDe = (n, r) => Math.floor(n / 10 ** r) % 10;
  // "4 milliers, 7 centaines, 8 dizaines et 2 unités" (a zero said too: "0 centaine")
  function decomposition(n){
    const l = String(n).length, parts = [];
    for (let r = l - 1; r >= 0; r--) { const c = chiffreDe(n, r); parts.push(`${c} ${c > 1 ? RANGS[r] : RANG1[r]}`); }
    return parts.length > 1 ? `${parts.slice(0, -1).join(", ")} et ${parts[parts.length - 1]}` : parts[0];
  }
  // why the bigger of two numbers is bigger: more digits, else the first rank where they differ; `moins`: told from the
  // smaller one (« the smallest number » is not explained by the biggest)
  function raison(a, b, moins = false){
    const [g, p] = a > b ? [a, b] : [b, a];
    if (String(g).length > String(p).length) return moins ? `${ecrit(p)} a moins de chiffres que ${ecrit(g)}` : `${ecrit(g)} a plus de chiffres que ${ecrit(p)}`;
    for (let r = String(g).length - 1; r >= 0; r--) {
      const x = chiffreDe(g, r), y = chiffreDe(p, r);
      if (x !== y) {
        const memes = String(g).length - 1 - r, avant = memes ? `mêmes ${RANGS.slice(r + 1).reverse().join(", ")} ; ` : "";
        const dit = c => `${c} ${c > 1 ? RANGS[r] : RANG1[r]}`;
        return `${avant}${moins ? `${dit(y)}, c'est moins que ${dit(x)}` : `${dit(x)}, c'est plus que ${dit(y)}`}`;
      }
    }
    return "ils sont égaux";
  }
  // a True/False shield: the statement, what the voice says, whether it is true
  function vraiFaux({id, question, dire, vrai, explication, dessin}, matiere = "maths"){
    const q = {id, matiere, niveau: 1, mecaniques: ["bouclier"], question, langue: "fr", visuel: "", reponse: vrai ? "vrai" : "faux",
      distracteurs: [vrai ? "faux" : "vrai"], explication, dire: `${dire || question} Vrai ou faux ?`, indice: ""};
    if (dessin) q.dessin = dessin;
    return q;
  }
  // wrong answers that are numbers: the given traps first, then near values; never the answer, never twice
  function faussesNb(rep, pieges, n = 3, min = 0, max = 99999){
    const out = [], vus = new Set([rep]);
    const ajouter = v => { if (out.length < n && Number.isInteger(v) && v >= min && v <= max && !vus.has(v)) { vus.add(v); out.push(v); } };
    pieges.forEach(ajouter);
    for (let d = 1; out.length < n && d < 60; d++) { ajouter(rep + d); ajouter(rep - d); }
    return out;
  }
  // the voice reads digits better without the spaces between classes, and words in letters without their hyphens
  const ditLettres = t => String(t).replace(/-/g, " ");
  const dit = t => String(t).replace(/(\d) (?=\d{3})/g, "$1");
  // the common question (GEN_OUTILS.question), with the names under the answers when there are (o.legendes)
  function question(o, m, matiere = "maths"){
    const q = GEN_OUTILS.question(o, m, matiere);
    if (o.legendes && Object.keys(o.legendes).length) q.legendes = o.legendes;
    return q;
  }
  const PERSOS = ["Matty", "Plumix", "Bärli", "Léiwchen", "Cavalin", "Poussik"];
  return {lettres, ecrit, RANGS, RANG1, chiffreDe, decomposition, raison, vraiFaux, faussesNb, dit, ditLettres, question, PERSOS};
})();

(() => {
  const G = () => GEN_OUTILS, C = () => GEN_CM;
  const majuscule = t => t.charAt(0).toUpperCase() + t.slice(1);
  // a number of four digits, all different, none zero (to ask for one of them), or with zeros (the trap)
  function quatreChiffres(zeros = 0){
    const ch = G().melanger([1, 2, 3, 4, 5, 6, 7, 8, 9]).slice(0, 4);
    if (zeros) G().melanger([1, 2, 3]).slice(0, zeros).forEach(i => { ch[i] = 0; });
    return +ch.join("");
  }
  // the writing mistakes of a number with zeros: a zero forgotten, one moved, the digits pushed to the front, one too many
  function piegesZero(n){
    const s = String(n), out = [], i = s.indexOf("0", 1);
    if (i > 0) out.push(+(s.slice(0, i) + s.slice(i + 1)));                                  // 2006 → 206
    const t = [...s]; for (let k = 1; k < t.length - 1; k++) if (t[k] === "0" && t[k + 1] !== "0") { [t[k], t[k + 1]] = [t[k + 1], t[k]]; break; }
    out.push(+t.join(""));                                                                    // 2006 → 2060, 3054 → 3504
    const reste = [...s.slice(1)]; out.push(+(s[0] + reste.filter(c => c !== "0").join("") + reste.filter(c => c === "0").join("")));   // 2006 → 2600
    out.push(+(s[0] + "000" + String(n % 1000)));                                             // 2006 → 20006 (« deux-mille », then « six »)
    return out.filter(v => v !== n && v > 0);
  }
  const permutes = n => { const s = [...String(n)]; return [+(s[1] + s[0] + s.slice(2).join("")), +(s[0] + s[2] + s[1] + s.slice(3).join("")), +(s.slice(0, -2).join("") + s[s.length - 1] + s[s.length - 2])]; };

  /* LIRE ET ÉCRIRE : écrire en chiffres un nombre dit, le chiffre d'un rang, la valeur d'un chiffre, les classes */
  GENERATEURS.grands_nombres = (p = {}, m = "rapide") => {
    const {alea, choisir, question} = G(), {lettres, ecrit, RANGS, RANG1, chiffreDe, decomposition, vraiFaux, faussesNb, dit, ditLettres, PERSOS} = C();
    const forme = choisir({rapide: ["ecrire", "chiffre"], massive: ["zero", "decompose", "valeur", "lire"], boss: ["centaines", "tresor"],
      bouclier: ["lit", "chiffre", "zero"]}[m] || ["ecrire"]);
    const zero = "Quand il n'y a rien à un rang, on y écrit un zéro.";
    if (m === "bouclier") {
      if (forme === "lit") {
        const n = quatreChiffres(alea(0, 1)), autre = choisir(permutes(n).filter(v => v >= 1000 && v !== n)), vrai = Math.random() < .5, x = vrai ? n : autre;
        // the voice never reads the number in digits: it would say the right name, and the shield would be judged by ear
        return vraiFaux({id: `G_nb_lit_${n}=${x}`, question: `${ecrit(n)} se lit « ${lettres(x)} ».`, dire: `Ce nombre se lit : ${ditLettres(lettres(x))}.`, vrai,
          explication: `${ecrit(n)}, c'est ${decomposition(n)} : on lit « ${lettres(n)} ».`});
      }
      if (forme === "chiffre") {
        const n = quatreChiffres(), r = alea(0, 3), c = chiffreDe(n, r), x = Math.random() < .5 ? c : chiffreDe(n, (r + 1) % 4);
        return vraiFaux({id: `G_nb_rang_${n}_${r}=${x}`, question: `Dans ${ecrit(n)}, le chiffre des ${RANGS[r]} est ${x}.`, vrai: x === c,
          dire: `Dans ${n}, le chiffre des ${RANGS[r]} est ${x}.`, explication: `${ecrit(n)} : ${decomposition(n)}. Le chiffre des ${RANGS[r]} est ${c}.`});
      }
      const n = quatreChiffres(2), x = Math.random() < .5 ? n : choisir(piegesZero(n));
      return vraiFaux({id: `G_nb_zero_${n}=${x}`, question: `« ${majuscule(lettres(n))} » s'écrit ${ecrit(x)}.`, dire: `${majuscule(ditLettres(lettres(n)))} s'écrit comme ce nombre.`,
        vrai: x === n, explication: `${majuscule(lettres(n))}, c'est ${decomposition(n)} : on écrit ${ecrit(n)}. ${zero}`});
    }
    if (forme === "ecrire" || forme === "zero") {
      const n = forme === "zero" ? quatreChiffres(alea(1, 2)) : Math.random() < .3 ? alea(101, 999) : quatreChiffres();
      const pieges = forme === "zero" ? piegesZero(n) : [...permutes(n), n + 1000, n - 1000];
      const txt = lettres(n);
      return question({id: `G_nb_ecrire_${n}`, question: `Écris en chiffres : ${txt}`, dire: `Écris en chiffres : ${ditLettres(txt)}.`,
        reponse: ecrit(n), distracteurs: faussesNb(n, pieges, 3, 1).map(ecrit),
        indice: forme === "zero" ? zero : "Écoute les milliers, puis les centaines, les dizaines et les unités.",
        explication: `${majuscule(txt)}, c'est ${decomposition(n)} : on écrit ${ecrit(n)}.${/0/.test(String(n)) ? " " + zero : ""}`}, m);
    }
    if (forme === "chiffre") {
      const n = quatreChiffres(), r = alea(0, 3), c = chiffreDe(n, r);
      return question({id: `G_nb_rang_${n}_${r}`, question: `Dans ${ecrit(n)}, quel est le chiffre des ${RANGS[r]} ?`,
        dire: `Dans ${n}, quel est le chiffre des ${RANGS[r]} ?`, reponse: c, distracteurs: [0, 1, 2, 3].filter(k => k !== r).map(k => chiffreDe(n, k)),
        indice: "De droite à gauche : les unités, les dizaines, les centaines, puis les milliers.",
        explication: `${ecrit(n)} : ${decomposition(n)}. Le chiffre des ${RANGS[r]} est ${c}.`}, m);
    }
    if (forme === "lire") {   // the number in digits, its name to find among the names of the zero's traps (never said aloud)
      const n = quatreChiffres(alea(1, 2)), autres = faussesNb(n, piegesZero(n).filter(v => v < 10000), 3, 101);
      return question({id: `G_nb_lire_${n}`, question: `Comment se lit ${ecrit(n)} ?`, dire: "Comment se lit ce nombre ?",
        reponse: lettres(n), distracteurs: autres.map(lettres), indice: "Lis d'abord les milliers, puis le reste ; un zéro ne se dit pas.",
        explication: `${ecrit(n)}, c'est ${decomposition(n)} : on lit « ${lettres(n)} ».`}, m);
    }
    if (forme === "valeur") {
      const n = quatreChiffres(alea(0, 1)), rangs = [0, 1, 2, 3].filter(k => chiffreDe(n, k)), r = choisir(rangs), c = chiffreDe(n, r), v = c * 10 ** r;
      return question({id: `G_nb_valeur_${n}_${r}`, question: `Dans ${ecrit(n)}, que vaut le chiffre ${c} ?`, dire: `Dans ${n}, que vaut le chiffre ${c} ?`,
        reponse: ecrit(v), distracteurs: [0, 1, 2, 3].filter(k => k !== r).map(k => ecrit(c * 10 ** k)),
        indice: "Regarde à quel rang il est : unités, dizaines, centaines ou milliers.",
        explication: `Le ${c} est au rang des ${RANGS[r]} : il vaut ${c} ${c > 1 ? RANGS[r] : RANG1[r]}, c'est ${ecrit(v)}.`}, m);
    }
    if (forme === "decompose") {
      const n = quatreChiffres(alea(1, 2)), parts = [3, 2, 1, 0].map(r => chiffreDe(n, r) * 10 ** r).filter(Boolean);
      return question({id: `G_nb_decompose_${n}`, question: `${parts.map(ecrit).join(" + ")} = ?`, dire: `Combien font ${parts.join(" plus ")} ?`,
        reponse: ecrit(n), distracteurs: faussesNb(n, piegesZero(n), 3, 1).map(ecrit), indice: zero,
        explication: `C'est ${decomposition(n)} : on écrit ${ecrit(n)}.`}, m);
    }
    if (forme === "centaines") {   // how many hundreds (or tens) in all
      const n = quatreChiffres(alea(0, 1)), r = choisir([1, 2]), t = Math.floor(n / 10 ** r);
      const nom = RANGS[r], c = chiffreDe(n, r);
      return question({id: `G_nb_entout_${n}_${r}`, question: `Combien y a-t-il de ${nom} en tout dans ${ecrit(n)} ?`, dire: `Combien y a-t-il de ${nom} en tout dans ${n} ?`,
        reponse: ecrit(t), distracteurs: faussesNb(t, [c, chiffreDe(n, 3), Math.floor(n / 10 ** (r - 1)), t + 1], 3, 0).map(ecrit),
        indice: `En tout : les ${nom} des milliers comptent aussi.`,
        explication: `${ecrit(n)}, c'est ${ecrit(t)} ${nom} et ${n % 10 ** r} ${r === 2 ? (n % 100 > 1 ? "unités" : "unité") : (n % 10 > 1 ? "unités" : "unité")} : un millier, c'est ${r === 2 ? "10 centaines" : "100 dizaines"}.`}, m);
    }
    // a treasure: chests of 1 000 sparkles, bags of 100, purses of 10, single sparkles (a rank may be empty)
    const n = quatreChiffres(alea(1, 2)), [a, b, c, d] = [3, 2, 1, 0].map(r => chiffreDe(n, r)), qui = choisir(PERSOS);
    const morceaux = [[a, "coffre", "coffres", " de 1 000 étincelles"], [b, "sac", "sacs", " de 100"], [c, "bourse", "bourses", " de 10"], [d, "étincelle toute seule", "étincelles toutes seules", ""]]
      .filter(([k]) => k).map(([k, s, pl, de], i, l) => `${k > 1 ? k : s.startsWith("b") ? "une" : s.startsWith("é") ? "une" : "un"} ${k > 1 ? pl : s}${de}`);
    const liste = morceaux.length > 1 ? `${morceaux.slice(0, -1).join(", ")} et ${morceaux[morceaux.length - 1]}` : morceaux[0];
    return question({id: `G_nb_tresor_${n}`, question: `${qui} trouve un trésor : ${liste}. Combien d'étincelles en tout ?`,
      dire: dit(`${qui} trouve un trésor : ${liste}. Combien d'étincelles en tout ?`), reponse: ecrit(n),
      distracteurs: faussesNb(n, piegesZero(n), 3, 1).map(ecrit), indice: "Un coffre pour les milliers, un sac pour les centaines, une bourse pour les dizaines.",
      explication: `${majuscule(decomposition(n))} : ${ecrit(n)} étincelles. ${zero}`}, m);
  };

  /* COMPARER ET RANGER : le plus grand, le plus petit, le nombre du milieu, juste avant, juste après, le signe */
  GENERATEURS.ranger_nombres = (p = {}, m = "rapide") => {
    const {alea, choisir, melanger, question} = G(), {ecrit, raison, vraiFaux, faussesNb} = C();
    const memes = () => {   // four numbers made of the same four digits, so that the ranks decide
      const n = quatreChiffres(alea(0, 1)), l = new Set([n]);
      while (l.size < 4) { const s = melanger([...String(n)]); if (s[0] !== "0") l.add(+s.join("")); }
      return [...l];
    };
    const forme = choisir({rapide: ["grand", "petit"], massive: ["apres", "avant", "milieu"], boss: ["signe", "former_grand", "former_petit"],
      bouclier: ["signe", "apres"]}[m] || ["grand"]);
    if (forme === "grand" || forme === "petit") {
      const l = memes(), grand = forme === "grand", s = l.slice().sort((a, b) => a - b), cible = grand ? s[3] : s[0], rival = grand ? s[2] : s[1];
      return question({id: `G_${grand ? "plusgrand" : "pluspetit"}_${s.join("-")}`, question: `Quel est le plus ${grand ? "grand" : "petit"} nombre ?`,
        dire: `Quel est le plus ${grand ? "grand" : "petit"} nombre ?`, reponse: ecrit(cible), distracteurs: l.filter(v => v !== cible).map(ecrit),
        indice: "Compare d'abord les milliers, puis les centaines, puis les dizaines.",
        explication: `${majuscule(raison(cible, rival, !grand))} : ${ecrit(cible)} est le plus ${grand ? "grand" : "petit"}.`}, m);
    }
    if (forme === "apres" || forme === "avant") {
      const apres = forme === "apres", base = alea(1, 9) * 1000 + choisir([0, alea(1, 9) * 100, alea(1, 9) * 100 + alea(1, 9) * 10]);
      const n = apres ? base - 1 : base, rep = apres ? n + 1 : n - 1;
      if (m === "bouclier") {
        const x = Math.random() < .5 ? rep : choisir([n + 10, n + 100, n + 1000].filter(v => v <= 10000));
        return vraiFaux({id: `G_apres_${n}=${x}`, question: `Juste après ${ecrit(n)}, il y a ${ecrit(x)}.`, dire: `Juste après ${n}, il y a ${x}.`, vrai: x === n + 1,
          explication: `Après ${ecrit(n)} vient ${ecrit(n + 1)} : on ajoute 1.`});
      }
      return question({id: `G_${forme}_${n}`, question: `Quel nombre vient juste ${apres ? "après" : "avant"} ${ecrit(n)} ?`,
        dire: `Quel nombre vient juste ${apres ? "après" : "avant"} ${n} ?`, reponse: ecrit(rep),
        distracteurs: faussesNb(rep, apres ? [n + 10, n + 100, n - 1, n + 1000] : [n - 10, n - 100, n + 1, n - 1000], 3, 1, 10000).map(ecrit),
        indice: apres ? "Ajoute 1 : quand les unités passent 9, on change de dizaine." : "Enlève 1 : quand il n'y a plus d'unités, on prend une dizaine.",
        explication: `${apres ? `${ecrit(n)} + 1` : `${ecrit(n)} − 1`} = ${ecrit(rep)}${/9$/.test(String(n)) && apres ? " : les 9 deviennent des 0, et le rang d'avant gagne 1" : ""}.`}, m);
    }
    if (forme === "milieu") {
      const l = memes().slice(0, 3), s = l.slice().sort((a, b) => a - b);
      return question({id: `G_milieu_${s.join("-")}`, question: `Range ${l.map(ecrit).join(", ")} du plus petit au plus grand. Lequel est au milieu ?`,
        dire: `Range ${l.join(", ")} du plus petit au plus grand. Lequel est au milieu ?`, reponse: ecrit(s[1]), distracteurs: [s[0], s[2]].map(ecrit),
        indice: "Trouve d'abord le plus petit et le plus grand.",
        explication: `Du plus petit au plus grand : ${s.map(ecrit).join(", ")}. Au milieu, il y a ${ecrit(s[1])}.`}, m);
    }
    if (forme === "signe") {
      const l = memes(), [x, y] = [l[0], l[1]], signe = x > y ? ">" : "<";
      if (m === "bouclier") {
        const s = Math.random() < .5 ? signe : signe === ">" ? "<" : ">";
        return vraiFaux({id: `G_signe_${x}${s}${y}`, question: `${ecrit(x)} ${s} ${ecrit(y)}`, dire: `${x} est ${s === ">" ? "plus grand" : "plus petit"} que ${y}.`,
          vrai: s === signe, explication: `${majuscule(raison(x, y))} : ${ecrit(Math.max(x, y))} est le plus grand. On écrit ${ecrit(x)} ${signe} ${ecrit(y)}.`});
      }
      return question({id: `G_signe_${x}_${y}`, question: `Quel signe va entre ${ecrit(x)} et ${ecrit(y)} ?`, dire: `Quel signe va entre ${x} et ${y} ?`,
        reponse: signe, distracteurs: [signe === ">" ? "<" : ">", "="], indice: "La pointe du signe montre le plus petit nombre.",
        explication: `${majuscule(raison(x, y))} : ${ecrit(Math.max(x, y))} est le plus grand. On écrit ${ecrit(x)} ${signe} ${ecrit(y)}.`}, m);
    }
    // the biggest (or smallest) number written with four digits, one of them a zero: never a zero in front
    const ch = melanger([0, ...melanger([1, 2, 3, 4, 5, 6, 7, 8, 9]).slice(0, 3)]), grand = forme === "former_grand";
    const desc = ch.slice().sort((a, b) => b - a), asc = ch.slice().sort((a, b) => a - b);
    const rep = grand ? +desc.join("") : +[asc[1], 0, ...asc.slice(2)].join("");
    const pieges = grand ? [+[desc[0], desc[1], desc[3], desc[2]].join(""), +[desc[1], desc[0], desc[2], desc[3]].join(""), +asc.slice(1).join("") * 10]
      : [+asc.slice(1).join(""), +[asc[1], asc[2], 0, asc[3]].join(""), +[asc[2], 0, asc[1], asc[3]].join("")];
    const avec = `Avec les chiffres ${ch.slice(0, 3).join(", ")} et ${ch[3]}, une seule fois chacun, quel est le plus ${grand ? "grand" : "petit"} nombre de quatre chiffres ?`;
    return question({id: `G_former_${forme}_${ch.join("")}`, question: avec, dire: avec, reponse: ecrit(rep),
      distracteurs: faussesNb(rep, pieges, 3, 100).map(ecrit), indice: grand ? "Mets le plus grand chiffre aux milliers." : "Un nombre de quatre chiffres ne commence jamais par 0.",
      explication: grand ? `Les chiffres du plus grand au plus petit : ${ecrit(rep)}.`
        : `Le plus petit chiffre qui n'est pas 0 va aux milliers, puis le 0, puis les autres dans l'ordre : ${ecrit(rep)}.`}, m);
  };
})();
