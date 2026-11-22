/* Banque de questions, un fichier par catégorie. Chaque question : c = catégorie, q = énoncé, r = réponses
   (la bonne en premier, l'ordre est mélangé à l'affichage), e = explication, s = [titre de la source, adresse].
   Règles d'écriture (vérifiées par verifier_banque.mjs) : quatre réponses plausibles, de longueurs voisines,
   la bonne n'étant pas plus souvent la plus longue que les autres ; des mises en situation, des calculs et
   des pièges plutôt que des définitions. */
window.CATEGORIES = [
  { id: "ing", nom: "Ingénierie des données" },
  { id: "sta", nom: "Statistique et analyse quantitative" },
  { id: "sem", nom: "Architecture des données et web sémantique" },
  { id: "hpc", nom: "Calcul haute performance et optimisation" },
  { id: "api", nom: "Intégration des données et API" },
  { id: "mlo", nom: "MLOps et automatisation" },
  { id: "vis", nom: "Analyse et visualisation" },
  { id: "gou", nom: "Gouvernance des données" }
];
window.QUESTIONS = [];
