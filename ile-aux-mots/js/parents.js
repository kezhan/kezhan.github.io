/* L'Île aux Mots : coin des parents. */
/* ---------- parents' corner ---------- */
(() => {
  // long press keeps small hands out
  const btn = $("lockBtn"), fill = $("lockFill"); let t = null;
  const cancel = () => { clearTimeout(t); t = null; fill.style.transition = "transform .15s"; fill.style.transform = "scaleX(0)"; };
  btn.addEventListener("pointerdown", () => { fill.style.transition = "transform 1.8s linear"; fill.style.transform = "scaleX(1)"; t = setTimeout(() => { cancel(); openParent(); }, 1800); });
  ["pointerup","pointerleave","pointercancel"].forEach(ev => btn.addEventListener(ev, cancel));
  btn.addEventListener("click", e => { if (e.detail === 0) openParent(); });
  btn.addEventListener("contextmenu", e => e.preventDefault());
})();

let fbTags = new Set();
function openParent(){
  GEN++; if (G && !G.done) endSession(false); stopLoops();
  $("presentSw").setAttribute("aria-checked", String(S.present));
  $("name7").value = S.names.p7; $("name4").value = S.names.p4;
  $("fbAct").innerHTML = `<option value="general">Le site en général</option>` + ACTS.map(a => `<option value="${a.id}">${a.name}</option>`).join("");
  const tags = $("fbTags"); tags.innerHTML = ""; fbTags = new Set();
  FB_TAGS.forEach(tg => { const c = el("button","chip"); c.textContent = tg; c.setAttribute("aria-pressed","false"); c.onclick = () => { fbTags.has(tg) ? fbTags.delete(tg) : fbTags.add(tg); c.setAttribute("aria-pressed", String(fbTags.has(tg))); }; tags.append(c); });
  $("storeInfo").textContent = Store.label() + (S.pending.length ? ` ${S.pending.length} élément(s) en attente d'envoi.` : "");
  $("exportBtn").hidden = !(Store.mode === "local" && S.pending.length);
  show("parent"); loadStats();
}
$("exportBtn").onclick = () => {
  // without the server, the parent carries this file to ile-aux-mots/retours/ for Claude
  const blob = new Blob([JSON.stringify({exportedAt:new Date().toISOString(), names:S.names, prof:S.prof, items:S.pending}, null, 1)], {type:"application/json"});
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob);
  a.download = "retours-" + new Date().toISOString().slice(0,10) + ".json";
  document.body.append(a); a.click(); a.remove();
  toast("Fichier prêt : déposez-le dans ile-aux-mots/retours");
};
$("presentSw").onclick = () => { S.present = !S.present; $("presentSw").setAttribute("aria-checked", String(S.present)); saveConfig(); toast(S.present ? "Noté : vous jouez avec eux" : "Noté : ils jouent seuls"); };
$("saveNames").onclick = () => { S.names.p7 = $("name7").value.trim() || KID_DEFAULT.p7.name; S.names.p4 = $("name4").value.trim() || KID_DEFAULT.p4.name; saveConfig(); toast("Prénoms enregistrés"); };
$("fbSend").onclick = async () => {
  const text = $("fbText").value.trim();
  if (!text && !fbTags.size) return toast("Choisissez une étiquette ou écrivez un mot");
  const id = uid();
  await record("notes", id, {at:new Date().toISOString(), t:Date.now(), act:$("fbAct").value, kid:$("fbKid").value, tags:[...fbTags], text, withParent:S.present, device, ver:VERSION, status:"new"});
  $("fbText").value = ""; openParent(); toast("Merci ! Claude le lira.");
};

async function loadStats(){
  const table = $("statsTable"), hard = $("hardWords"), list = $("notesList");
  let sessions = [], notes = [];
  try { sessions = await Store.list("sessions", 600); } catch(e) {}
  try { notes = await Store.list("notes", 15); } catch(e) {}
  S.pending.forEach(p => { if (p.kind === "sessions") sessions.push(p.data); else if (p.kind === "notes") notes.unshift(p.data); });
  if (!sessions.length) { table.innerHTML = `<tr><td class="muted">Pas encore de partie jouée. Les chiffres apparaîtront ici après les premiers jeux.</td></tr>`; }
  else {
    const rows = ACTS.map(a => {
      const s = sessions.filter(x => x.act === a.id);
      if (!s.length) return `<tr><td>${a.em} ${a.name}</td><td colspan="5" class="muted">pas encore joué</td></tr>`;
      const done = s.filter(x => x.completed).length, rated = s.filter(x => x.rating);
      const rounds = s.reduce((n,x) => n + (x.played||0), 0), first = s.reduce((n,x) => n + (x.firstTry||0), 0);
      const pct = rounds ? Math.round(100*first/rounds) : null;
      const avg = rated.length ? rated.reduce((n,x) => n + x.rating, 0) / rated.length : null;
      const face = avg == null ? "–" : avg >= 2.5 ? "😍" : avg >= 1.7 ? "🙂" : "😴";
      const cls = v => v == null ? "" : v >= 75 ? "g" : v >= 50 ? "m" : "b";
      return `<tr><td>${a.em} ${a.name}</td><td>${s.length}</td><td><span class="pill ${cls(Math.round(100*done/s.length))}">${Math.round(100*done/s.length)} %</span></td>
        <td>${pct == null ? "–" : `<span class="pill ${cls(pct)}">${pct} %</span>`}</td><td>${face} ${rated.length ? `(${rated.length})` : ""}</td>
        <td>${Math.round(s.reduce((n,x) => n + (x.secs||0), 0) / s.length)} s</td></tr>`;
    }).join("");
    table.innerHTML = `<tr><th>Jeu</th><th>Parties</th><th>Finies</th><th>Du 1er coup</th><th>Avis enfants</th><th>Durée moy.</th></tr>` + rows;
  }
  hard.innerHTML = "";
  const hw = [];
  ["p7","p4"].forEach(k => Object.entries(S.prof[k].words).forEach(([w,[ok,ko]]) => { if (ko > 0 && ok + ko >= 2) hw.push({w, k, r: ko/(ok+ko), n: ok+ko}); }));
  hw.sort((a,b) => b.r - a.r || b.n - a.n);
  if (!hw.length) hard.innerHTML = `<p class="muted">Rien à signaler pour l'instant.</p>`;
  hw.slice(0, 14).forEach(x => { const c = el("button","chip"); c.textContent = `${x.w} · ${S.names[x.k]} ${Math.round(100*x.r)} % raté`; c.onclick = () => say(x.w); hard.append(c); });
  list.innerHTML = notes.length ? "" : `<p class="muted">Aucun retour écrit pour l'instant.</p>`;
  notes.forEach(n => {
    const d = el("div","note");
    const act = (ACTS.find(a => a.id === n.act) || {name:"Général"}).name;
    const head = el("b"); head.textContent = `${new Date(n.at).toLocaleDateString("fr-FR")} · ${act} · ${(n.tags||[]).join(", ")}${n.status === "done" ? " · ✅ traité" : ""}`;
    const body = el("div"); body.textContent = n.text || "";
    d.append(head, body); list.append(d);
  });
}
