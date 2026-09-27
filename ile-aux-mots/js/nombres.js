/* L'Île aux Mots : les nombres de 0 à 100 dans chaque langue apprise, pour compter et calculer.
   numberIn(56, "de") = "sechsundfünfzig", numberIn(56, "lb") = "sechsafofzeg", numberIn(56, "zh") = "五十六". */
const NUMS_IN = {
  de:{ones:["null","eins","zwei","drei","vier","fünf","sechs","sieben","acht","neun","zehn","elf","zwölf","dreizehn","vierzehn","fünfzehn","sechzehn","siebzehn","achtzehn","neunzehn"],
      tens:["","","zwanzig","dreißig","vierzig","fünfzig","sechzig","siebzig","achtzig","neunzig"], hundred:"hundert"},
  lb:{ones:["null","eent","zwee","dräi","véier","fënnef","sechs","siwen","aacht","néng","zéng","eelef","zwielef","dräizéng","véierzéng","fofzéng","siechzéng","siwwenzéng","uechtzéng","nonzéng"],
      tens:["","","zwanzeg","drësseg","véierzeg","fofzeg","sechzeg","siwwenzeg","achtzeg","nonzeg"], hundred:"honnert"},
  zh:{ones:["零","一","二","三","四","五","六","七","八","九","十"], hundred:"一百"}
};
function numberIn(n, lang){
  if (lang === "de") {
    const d = NUMS_IN.de; if (n < 20) return d.ones[n]; if (n === 100) return d.hundred;
    const t = Math.floor(n / 10), u = n % 10;
    return u ? (u === 1 ? "ein" : d.ones[u]) + "und" + d.tens[t] : d.tens[t];
  }
  if (lang === "lb") {
    const d = NUMS_IN.lb; if (n < 20) return d.ones[n]; if (n === 100) return d.hundred;
    const t = Math.floor(n / 10), u = n % 10;
    if (!u) return d.tens[t];
    // Eifel rule: "an" keeps its n before a vowel or d, t, z, h, n (eenanzwanzeg, eenavéierzeg)
    const link = /^[aeiouäéëdtzhn]/.test(d.tens[t]) ? "an" : "a";
    return (u === 1 ? "een" : d.ones[u]) + link + d.tens[t];
  }
  if (lang === "zh") {
    const d = NUMS_IN.zh; if (n <= 10) return d.ones[n]; if (n === 100) return d.hundred;
    const t = Math.floor(n / 10), u = n % 10;
    return (t > 1 ? d.ones[t] : "") + "十" + (u ? d.ones[u] : "");
  }
  return numberWords(n);
}
