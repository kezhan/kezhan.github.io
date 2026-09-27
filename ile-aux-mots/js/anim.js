/* L'Île aux Mots : animations communes. Tous les jeux en profitent sans rien faire :
   le noyau appelle fx.* aux bons moments (étoile gagnée, erreur, écran qui change).
   Seulement transform et opacity (fluide sur tablette) ; rien en mode test ni si le système demande moins d'animations. */
const fx = (() => {
  const calm = () => TEST || matchMedia("(prefers-reduced-motion: reduce)").matches;
  let last = {x: innerWidth / 2, y: innerHeight / 2};
  // where the child touched last: sparkles and flying stars start from there
  addEventListener("pointerdown", e => { last = {x: e.clientX, y: e.clientY, el: e.target.closest("button")}; }, true);

  function layer(txt, x, y, size){
    const d = document.createElement("div");
    d.textContent = txt;
    d.style.cssText = `position:fixed; left:${x}px; top:${y}px; font-size:${size}px; pointer-events:none; z-index:60; will-change:transform,opacity; transform:translate(-50%,-50%)`;
    document.body.append(d);
    return d;
  }
  function sparkle(x = last.x, y = last.y, n = 12){
    if (calm()) return;
    for (let i = 0; i < n; i++) {
      const a = (Math.PI * 2 * i) / n + Math.random() * 0.4, r = 60 + Math.random() * 70;
      const d = layer(["✨","⭐","💫","🌟"][i % 4], x, y, 18 + Math.random() * 14);
      d.animate([
        {transform: "translate(-50%,-50%) scale(.3)", opacity: 1},
        {transform: `translate(calc(-50% + ${Math.cos(a) * r}px), calc(-50% + ${Math.sin(a) * r}px)) scale(1) rotate(${Math.random() * 360}deg)`, opacity: 0}
      ], {duration: 650 + Math.random() * 250, easing: "cubic-bezier(.2,.8,.3,1)"}).onfinish = () => d.remove();
    }
  }
  function bounce(el){
    if (!el || calm()) return;
    el.animate([{transform: "scale(1)"}, {transform: "scale(1.14) rotate(-3deg)"}, {transform: "scale(.96)"}, {transform: "scale(1)"}],
      {duration: 480, easing: "ease-out"});
  }
  // a star flies from the touched spot to the star counter, which then pops
  function flyStar(){
    if (calm()) return;
    const target = document.getElementById("gStars");
    if (!target || !target.offsetParent) return;
    const r = target.getBoundingClientRect(), tx = r.left + r.width / 2, ty = r.top + r.height / 2;
    const d = layer("⭐", last.x, last.y, 34);
    d.animate([
      {transform: "translate(-50%,-50%) scale(.6)"},
      {transform: `translate(calc(-50% + ${(tx - last.x) * 0.5}px), calc(-50% + ${(ty - last.y) * 0.5 - 80}px)) scale(1.5)`, offset: .45},
      {transform: `translate(calc(-50% + ${tx - last.x}px), calc(-50% + ${ty - last.y}px)) scale(.7)`}
    ], {duration: 750, easing: "ease-in-out"}).onfinish = () => {
      d.remove();
      target.parentElement.animate([{transform: "scale(1)"}, {transform: "scale(1.5)"}, {transform: "scale(1)"}], {duration: 350, easing: "ease-out"});
    };
  }
  function star(){ sparkle(); bounce(last.el); flyStar(); }
  function wrong(){ if (calm() || !last.el) return; last.el.animate([{transform: "translateX(0)"}, {transform: "translateX(-10px) rotate(-2deg)"}, {transform: "translateX(9px) rotate(2deg)"}, {transform: "translateX(-5px)"}, {transform: "translateX(0)"}], {duration: 420}); }
  // clouds drifting across the sky, each at its own height and speed
  function clouds(){
    if (calm()) return;
    [[70, 9, 48, 0], [150, 6, 70, -25], [40, 12, 58, -42]].forEach(([y, size, secs, start]) => {
      const c = document.createElement("div");
      c.className = "nuage"; c.textContent = "☁️"; c.style.fontSize = size * 8 + "px";
      document.body.append(c);
      c.animate([{transform: `translate(-30vw, ${y}px)`}, {transform: `translate(110vw, ${y + 12}px)`}],
        {duration: secs * 1000, iterations: Infinity, delay: start * 1000});
    });
  }
  clouds();
  return {sparkle, bounce, flyStar, star, wrong, calm, get last(){ return last; }};
})();
