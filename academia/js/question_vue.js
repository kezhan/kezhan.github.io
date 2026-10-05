/* Académia : pose une question dans le panneau et attend la réponse de l'enfant.
   1. En haut, collés au haut du panneau même quand il défile : le titre de l'attaque, les jokers, le gros bouton 🔊
      et l'énoncé. Une question de langue dit d'abord le mot étranger, deux fois, lentement ; ses réponses s'ouvrent
      quand il a été dit (2,5 s au plus). Jusqu'à 5 ans, 🔊 lit aussi les réponses ; une question à l'oreille les lit
      toujours, chacune dans sa langue.
   2. Les réponses sont dessinées par js/question_dessin.js : une image remplit son bouton, un nombre de 1 à 10 a ses
      points de dé, des groupes en faces de dé.
   3. Jokers : Potion de clarté (elle clignote à la question qui suit une erreur) et Indice du sage (grisé quand la
      question n'a pas d'indice écrit) ; leur nombre baisse dès qu'on s'en sert.
   4. Une erreur montre la bonne réponse et l'explication, lue à voix haute (les mots étrangers avec leur voix), à côté
      des réponses sur un écran large ; une bonne réponse dit le nom écrit sous l'image (« Hiver ! »).
   5. Le minuteur du combo s'arrête quand la page est cachée et repart au retour, la question relue. */
let erreurAvant = false;   // the potion blinks at the question that follows a mistake
const HAUT_PARLEUR = `<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M3.5 9.2h4l5.2-4.4v14.4l-5.2-4.4h-4z" fill="#fff"/>
  <path d="M15.6 8.6a4.8 4.8 0 0 1 0 6.8M18.2 6a8.6 8.6 0 0 1 0 12" stroke="#fff" stroke-width="2.2" fill="none" stroke-linecap="round"/></svg>`;

// the foreign word of a language question, said first: q.mot (a word only heard), or the « … » of the sentence
function motEtranger(q){
  const lang = q.langue || "fr";
  if (q.mot) return {mot: q.mot, lang, avant: q.question, dans: false, apres: ""};
  const m = lang !== "fr" && /^(.*?)«\s*(.+?)\s*»(.*)$/.exec(q.question);
  return m ? {mot: m[2], lang, avant: m[1].trim(), dans: true, apres: m[3].replace(/^[\s?!.]*$/, "").trim()} : null;
}
// voices one after the other; stops as soon as something else is said (an answer, « J'ai compris », 🔊 again)
async function suiteDeVoix(etapes){
  for (const e of etapes) {
    const r = e(), g = typeof generation !== "undefined" ? generation : 0;
    await r;
    if (typeof generation !== "undefined" && generation !== g) return false;
  }
  return true;
}
// a French text whose « foreign words » are said with their own voice (der Bär, Moien…)
function direMixte(texte, q){
  const lang = q.oreille || q.langue || "fr", mots = Q.mots[lang], parts = [];
  if (lang === "fr" || !mots) return dire(texte);
  const re = /«\s*(.+?)\s*»/g; let m, i = 0;
  while ((m = re.exec(texte))) if (mots.has(m[1].toLowerCase())) { parts.push([texte.slice(i, m.index), "fr"], [m[1], lang]); i = re.lastIndex; }
  parts.push([texte.slice(i), "fr"]);
  return suiteDeVoix(parts.filter(([t]) => /[\p{L}\p{N}]/u.test(t)).map(([t, l]) => () => dire(t, l)));
}
const son = nom => { if (typeof jouerSon === "function") jouerSon(nom); };
// French typography on screen: no line break before « ? » or « ! », nor inside « … »
const typo = t => String(t || "").replace(/ ([?!;])/g, " $1").replace(/ :/g, " :").replace(/« /g, "« ").replace(/ »/g, " »");

function poserQuestion(panneau, q, {titre = "", limite = 0, jokers = true} = {}){
  return new Promise(fin => {
    const vf = estVraiFaux(q), lang = q.langue || "fr", t0 = Date.now(), p = P(), petit = !!p && p.age <= 5;
    const mot = motEtranger(q), langueReponses = q.oreille || "fr";
    window.__q = q; window.__qn = (window.__qn || 0) + 1;   // the question on screen and its number, read by the recette
    let fini = false, ouvert = !mot, chrono = null;
    panneau.innerHTML = ""; panneau.classList.remove("cache"); panneau.scrollTop = 0;
    const boite = el("div", "question" + (vf ? " vraifaux" : ""));
    const haut = el("div", "q-haut"), tete = el("div", "q-tete"), titreP = el("p", "titre-bouclier"), outils = el("div", "outils");
    titreP.textContent = titre; tete.append(titreP, outils);
    const ligne = el("div", "q-enonce"), ecoute = el("button", "ecouter", HAUT_PARLEUR), enonce = el("p", "enonce");
    ecoute.setAttribute("aria-label", "Écouter"); ecoute.title = "Écouter";
    enonce.textContent = typo(q.question); ligne.append(ecoute, enonce); haut.append(tete, ligne); boite.append(haut);
    let anim = null, reste = limite, depart = 0, cachee = false;
    const lancer = () => { depart = Date.now(); chrono = setTimeout(() => repondre(null), TEST ? 600000 : reste); if (anim) anim.play(); };
    if (limite) {
      const m = el("div", "minuteur", "<i></i>"); haut.append(m);
      if (!TEST) anim = m.firstChild.animate([{transform: "scaleX(1)"}, {transform: "scaleX(0)"}], {duration: limite, fill: "forwards"});
      lancer();
    }
    // the picture and the answers: one above the other, side by side on a wide and low screen
    const milieu = el("div", "q-milieu"), vis = dessinVisuel(q); if (vis) milieu.append(vis);
    const corps = el("div", "q-corps"), grille = el("div", "reponses");
    const boutons = boutonsDe(q), enGroupes = !vf && boutons.some(estGroupe);
    grille.classList.add("n" + boutons.length);
    boutons.forEach(v => {
      const b = el("button", "reponse");
      b.dataset.v = v;
      if (vf) b.textContent = v === "vrai" ? "✅ Vrai" : "❌ Faux";
      else { const c = contenuReponse(v, q, enGroupes); b.append(c.contenu); b.classList.add(c.genre); }
      if (v === q.reponse) markOk(b);
      b.onclick = () => { if (ouvert && Date.now() - t0 >= 700) repondre(b); };
      grille.append(b);
    });
    corps.append(grille); milieu.append(corps); boite.append(milieu);
    // a language question: the answers wait for the foreign word (they look the same, they just do not answer yet)
    const ouvrir = () => { if (ouvert || fini) return; ouvert = true; grille.classList.remove("attend"); [...grille.children].forEach(x => { x.disabled = false; }); };
    if (!ouvert) { grille.classList.add("attend"); [...grille.children].forEach(x => { x.disabled = true; }); setTimeout(ouvrir, TEST ? 0 : 2500); }

    if (jokers && !vf && boutons.length >= 3 && p) {
      const joker = (ico, nom, n) => { const b = el("button", "outil", `<span>${ico}</span><b></b>`); b.title = nom; b.setAttribute("aria-label", nom); b.lastChild.textContent = n; return b; };
      const potion = joker("🧪", "Potion de clarté", p.jokers.potion);
      potion.disabled = !p.jokers.potion;
      if (erreurAvant && p.jokers.potion) potion.classList.add("clignote");   // after a mistake: help is here
      potion.onclick = () => {   // removes two wrong answers (GDD §4)
        p.jokers.potion--; sauver(); potion.lastChild.textContent = p.jokers.potion; potion.disabled = true; potion.classList.remove("clignote");
        son("bouton"); sfx.juste(); dire("Potion de clarté !");
        [...grille.children].filter(b => b.dataset.v !== q.reponse).slice(0, 2).forEach(b => b.classList.add("retiree"));
      };
      const indice = joker("🦉", "Indice du sage", p.jokers.indice);
      indice.disabled = !p.jokers.indice || !q.indice;   // no written hint: the owl rests (it never gives the answer away)
      indice.onclick = () => {
        p.jokers.indice--; sauver(); indice.lastChild.textContent = p.jokers.indice; indice.disabled = true; son("bouton");
        const t = el("div", "explication indice"); t.textContent = typo("🦉 " + q.indice); boite.insertBefore(t, milieu);
        direMixte(q.indice, q);
      };
      outils.append(potion, indice);
    }
    panneau.append(boite);
    grille.scrollIntoView({block: "nearest"});   // a low screen: the answers stay in sight, the top stays stuck

    // what the voice says: the foreign word first, twice and slowly; then the sentence; the answers when asked
    const texteDit = v => (q.legendes && q.legendes[v]) || sansEmoji(v);
    let lecture = 0;
    async function ecouter(lireLesReponses){
      const moi = ++lecture;
      ecoute.classList.add("parle");
      const etapes = [];
      if (mot) {
        etapes.push(() => dire(`${mot.mot}, ${mot.mot} !`, mot.lang, {lent: true}), () => ouvrir());   // a comma: the Luxembourgish recordings play word by word
        if (mot.avant) etapes.push(() => dire(mot.avant, "fr"));
        if (mot.dans) etapes.push(() => dire(mot.mot, mot.lang, {lent: true}));
        if (mot.apres) etapes.push(() => dire(mot.apres, "fr"));
      } else etapes.push(() => dire(q.dire || q.question, lang));
      if (q.propose != null) etapes.push(() => dire("C'est ça ?", "fr"));
      if (lireLesReponses && !vf) [...grille.children].forEach(b => {
        const t = texteDit(b.dataset.v);
        if (t && !b.classList.contains("retiree")) etapes.push(async () => { b.classList.add("lue"); const r = dire(t + " ?", langueReponses); await r; b.classList.remove("lue"); });
      });
      await suiteDeVoix(etapes);
      if (moi !== lecture) return;   // 🔊 touched again: the new reading keeps the waves
      [...grille.children].forEach(b => b.classList.remove("lue"));
      ecoute.classList.remove("parle");
    }
    ecoute.onclick = () => { son("bouton"); sfx.tap(); ecouter(!!q.oreille || petit); };
    ecouter(!!q.oreille);

    // the page hidden (another app on the tablet): the combo timer waits; back on the page, the question is said again
    function vu(){
      if (fini || !boite.isConnected) return document.removeEventListener("visibilitychange", vu);
      if (document.hidden && !cachee) {
        cachee = true;
        if (limite) { clearTimeout(chrono); reste -= Date.now() - depart; if (anim) anim.pause(); }
      } else if (!document.hidden && cachee) { cachee = false; if (limite) lancer(); ecouter(!!q.oreille); }
    }
    document.addEventListener("visibilitychange", vu);

    function repondre(b){
      if (fini) return; fini = true; clearTimeout(chrono); document.removeEventListener("visibilitychange", vu);
      const juste = !!b && b.dataset.v === q.reponse, ms = Date.now() - t0;
      grille.classList.remove("attend");
      [...grille.children].forEach(x => { x.disabled = true; x.classList.remove("lue"); if (x.dataset.v === q.reponse) x.classList.add("juste"); });
      ecoute.classList.remove("parle");
      erreurAvant = !juste;
      const leg = q.legendes && q.legendes[q.reponse];
      if (juste) {
        sfx.juste(); etincelles(...centre(b), 10);
        if (leg) dire(leg + " !"); else taire();
        return setTimeout(() => fin({juste, ms}), TEST ? 0 : 650);
      }
      sfx.faux(); if (b) b.classList.add("faux");
      // the companion explains (GDD §8.1), next to the answers on a wide screen; the child taps on to continue
      boite.classList.add("corrige");
      const t = el("div", "explication");
      const bonne = vf ? (q.reponse === "vrai" ? "C'est vrai !" : "C'est faux !") : `La bonne réponse est ${leg || q.reponse}.`;
      const l1 = el("b"); l1.textContent = typo((b ? "" : "⏳ Trop tard ! ") + bonne); t.append(l1);
      if (q.explication) { const l2 = el("p"); l2.textContent = typo(q.explication); t.append(l2); }
      const suite = markOk(el("button", "moyen vert", "J'ai compris 👍"));
      suite.onclick = () => { son("bouton"); sfx.tap(); taire(); fin({juste: false, ms}); };
      const correction = el("div", "q-correction"); correction.append(t, suite); corps.append(correction);
      suite.scrollIntoView({block: "nearest"});
      const dite = vf || leg || sansEmoji(q.reponse) ? bonne : "Regarde la bonne réponse, en vert.";
      direMixte((b ? "" : "Trop tard ! ") + dite + (q.explication ? " " + q.explication : ""), q);
    }
  });
}
