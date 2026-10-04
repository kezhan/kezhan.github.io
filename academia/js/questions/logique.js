/* Académia : générateurs de logique (contrat : conception/architecture.md), suites de motifs et suites de nombres.
   GENERATEURS.suite({type: "motifs" | "nombres"}, mecanique). Outils communs : GEN_OUTILS, dans maths.js. */
var GENERATEURS = self.GENERATEURS = self.GENERATEURS || {};

(() => {
  // the pictures of the patterns, with what the voice says (emojis are never read aloud)
  const MOTIFS = {"🔴": "le rond rouge", "🔵": "le rond bleu", "⭐": "l'étoile", "🌙": "la lune", "🔺": "le triangle",
    "🔷": "le losange", "⬛": "le carré noir", "🍎": "la pomme", "🍌": "la banane", "🐱": "le chat", "🐶": "le chien",
    "🌸": "la fleur", "🍀": "le trèfle", "🐟": "le poisson", "🎈": "le ballon"};
  const FORMES = {rapide: ["AB"], massive: ["AAB", "ABB", "ABC"], boss: ["AABB", "ABCD", "ABAC"], bouclier: ["AB", "AAB", "ABC"]};
  const sans = t => t.replace(/^(le|la|l') ?/, "");

  function motifs(m){
    const {choisir, melanger, question} = GEN_OUTILS;
    const forme = choisir(FORMES[m] || FORMES.rapide), elems = melanger(Object.keys(MOTIFS));
    const lettres = [...new Set(forme)], de = Object.fromEntries(lettres.map((l, i) => [l, elems[i]]));
    const motif = [...forme].map(l => de[l]), longueur = motif.length * 2 + Math.max(1, motif.length - 1);
    const suite = Array.from({length: longueur + 1}, (_, i) => motif[i % motif.length]);
    const trou = m === "boss" && Math.random() < .5 ? motif.length + 1 : longueur;   // the hole: at the end, or in the middle
    const reponse = suite[trou], montre = trou === longueur ? suite.slice(0, longueur) : suite.slice(0, longueur).map((e, i) => i === trou ? "❓" : e);
    if (trou === longueur) montre.push("❓");
    const autres = [...lettres.map(l => de[l]), ...elems.slice(lettres.length)].filter(e => e !== reponse).slice(0, 3);
    const dit = motif.map(e => sans(MOTIFS[e])).join(", ");
    return question({id: `G_suite_${forme}_${motif.join("")}_${trou}`, question: trou === longueur ? "Qu'est-ce qui vient après ?" : "Qu'est-ce qui manque ?",
      dire: trou === longueur ? "Qu'est-ce qui vient après ?" : "Qu'est-ce qui manque ?", visuel: montre.join(" "), reponse, distracteurs: autres,
      indice: "Trouve le morceau qui se répète.", explication: `Le motif se répète : ${dit}, puis on recommence. Ici, c'est ${MOTIFS[reponse]} ${reponse}.`,
      affirmer: e => `À la place du ❓, il faut ${e} ?`, affirmerDire: e => `À la place du point d'interrogation, il faut ${MOTIFS[e] || "cela"}.`}, m, "logique");
  }

  function nombres(m){
    const {alea, choisir, fausses, question} = GEN_OUTILS;
    const pas = choisir({rapide: [1, 2, 10, 5], massive: [-1, -2, 3, 4, -10, 5], boss: [3, 4, 6, -5, "x2"], bouclier: [1, 2, 5, 10]}[m] || [1, 2]);
    let l;
    if (pas === "x2") { const d = choisir([1, 2, 3]); l = [d, 2 * d, 4 * d, 8 * d, 16 * d]; }
    else { const debut = pas > 0 ? alea(0, 10) * (Math.abs(pas) >= 5 ? Math.abs(pas) : 1) + (pas === 10 ? alea(0, 9) : 0) : alea(5, 9) * Math.abs(pas) + alea(0, 4) * Math.abs(pas);
      l = Array.from({length: 5}, (_, i) => debut + i * pas); }
    if (l.some(v => v < 0)) l = l.map(v => v - Math.min(...l));   // never below zero
    const trou = m === "boss" && Math.random() < .5 ? 2 : 4, reponse = l[trou], ecart = pas === "x2" ? 0 : pas;
    const regle = pas === "x2" ? "On double à chaque fois" : pas > 0 ? `On ajoute ${pas} à chaque fois` : `On enlève ${-pas} à chaque fois`;
    const calc = pas === "x2" ? `${l[trou - 1]} × 2 = ${reponse}` : `${l[trou - 1]} ${pas > 0 ? "+" : "−"} ${Math.abs(pas)} = ${reponse}`;
    const pieges = pas === "x2" ? [l[trou - 1] + l[0], reponse + 1, reponse - 1] : [reponse + 1, reponse - 1, reponse + ecart, reponse - 2 * ecart];
    const montre = l.map((v, i) => i === trou ? "❓" : String(v));
    return question({id: `G_suite_nb_${l.join("_")}_${trou}`, question: trou === 4 ? "Quel nombre vient après ?" : "Quel nombre manque ?",
      dire: `${montre.map(v => v === "❓" ? "combien" : v).join(", ")} ?`, visuel: montre.join("   "), reponse,
      distracteurs: fausses(reponse, pieges, [reponse + 2, reponse - 2], 3, 0), indice: "Regarde comment on passe d'un nombre au suivant.",
      explication: `${regle} : ${calc}.`, affirmer: v => `À la place du ❓, il faut ${v} ?`, affirmerDire: v => `À la place du point d'interrogation, il faut ${v}.`}, m, "logique");
  }

  GENERATEURS.suite = (p = {}, m = "rapide") => (p.type === "nombres" ? nombres : motifs)(m);
})();
