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
  $("name7").value = kidName("p7"); $("name4").value = kidName("p4");
  $("fbAct").innerHTML = `<option value="general">${tx("general")}</option>` + ACTS.map(a => `<option value="${a.id}">${a.em} ${actTitle(a)}</option>`).join("");
  const tags = $("fbTags"); tags.innerHTML = ""; fbTags = new Set();
  FB_TAGS.forEach(tg => { const c = el("button","chip"); c.textContent = fbTagLabel(tg); c.setAttribute("aria-pressed","false"); c.onclick = () => { fbTags.has(tg) ? fbTags.delete(tg) : fbTags.add(tg); c.setAttribute("aria-pressed", String(fbTags.has(tg))); }; tags.append(c); });
  $("storeInfo").textContent = Store.label() + (S.pending.length ? " " + tx("pending", S.pending.length) : "");
  $("exportBtn").hidden = !(Store.mode === "local" && S.pending.length);
  $("persistInfo").textContent = tx(S.persisted === true ? "persistYes" : "persistNo");
  renderLevels(); renderTime();
  show("parent"); loadStats();
}
// where each child spends time: from the scores kept on this device, most played first
function renderTime(){
  const t = $("timeTable"); t.innerHTML = "";
  const rows = [];
  ["p7","p4"].forEach(k => Object.entries(S.prof[k].scores || {}).forEach(([id, sc]) => {
    const a = ACTS.find(x => x.id === id); if (!a || !sc.plays) return;
    rows.push({k, a, sc});
  }));
  if (!rows.length) { t.innerHTML = `<tr><td class="muted">${tx("noPlayHere")}</td></tr>`; return; }
  rows.sort((x, y) => y.sc.secs - x.sc.secs);
  const head = el("tr"); tx("timeHead").forEach(h => head.append(el("th", "", h))); t.append(head);
  rows.forEach(({k, a, sc}) => {
    const tr = el("tr");
    const kid = el("td"); kid.textContent = kidName(k);
    tr.append(kid, el("td", "", `${a.em} ${actTitle(a)}`), el("td", "", fmtTime(sc.secs)), el("td", "", String(sc.plays)),
      el("td", "", String(sc.done || 0)), el("td", "", `⭐ ${sc.best || 0}`), el("td", "", a.levels === false ? "–" : String(levelOf(a.id, k))));
    t.append(tr);
  });
}
function renderLevels(){
  const t = $("levelsTable"); t.innerHTML = "";
  const head = el("tr"); head.append(el("th", "", tx("game")));
  ["p7","p4"].forEach(k => { const th = el("th"); th.textContent = kidName(k); head.append(th); });
  t.append(head);
  ACTS.filter(a => a.levels !== false).forEach(a => {
    const tr = el("tr"); tr.append(el("td", "", `${a.em} ${actTitle(a)}`));
    ["p7","p4"].forEach(k => {
      const td = el("td"), row = el("div", "row"); row.style.gap = "6px"; row.style.flexWrap = "nowrap";
      const minus = el("button", "chip", "−"), plus = el("button", "chip", "+"), val = el("b", "", String(levelOf(a.id, k)));
      minus.setAttribute("aria-label", tx("lower", actTitle(a))); plus.setAttribute("aria-label", tx("raise", actTitle(a)));
      minus.onclick = () => { setLevel(a.id, levelOf(a.id, k) - 1, k); renderLevels(); renderHome(); };
      plus.onclick = () => { setLevel(a.id, levelOf(a.id, k) + 1, k); renderLevels(); renderHome(); };
      row.append(minus, val, plus); td.append(row); tr.append(td);
    });
    t.append(tr);
  });
}
function downloadJSON(name, obj){
  const blob = new Blob([JSON.stringify(obj, null, 1)], {type:"application/json"});
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob); a.download = name;
  document.body.append(a); a.click(); a.remove();
}
const today = () => new Date().toISOString().slice(0,10);
// Chrome on the Android tablet offers installation; keep its event for the parents' button
let installEvt = null;
window.addEventListener("beforeinstallprompt", e => { e.preventDefault(); installEvt = e; $("installBtn").hidden = false; });
window.addEventListener("appinstalled", () => { installEvt = null; $("installBtn").hidden = true; toast(tx("installed")); });
$("installBtn").onclick = async () => {
  if (!installEvt) return;
  installEvt.prompt();
  try { await installEvt.userChoice; } catch(e) {}
  installEvt = null; $("installBtn").hidden = true;
};
$("exportBtn").onclick = () => {
  // without the server, the parent carries this file to ile-aux-mots/retours/ for Claude
  downloadJSON("retours-" + today() + ".json", {exportedAt:new Date().toISOString(), names:S.names, prof:S.prof, items:S.pending});
  toast(tx("exportReady"));
};
$("saveProg").onclick = () => {
  downloadJSON("ile-aux-mots-sauvegarde-" + today() + ".json", {app:"ile-aux-mots", savedAt:new Date().toISOString(), ver:VERSION, names:S.names, prof:S.prof, present:S.present});
  toast(tx("savedDl"));
};
$("restoreProg").onchange = async ev => {
  const file = ev.target.files[0]; ev.target.value = "";
  if (!file) return;
  try {
    const d = JSON.parse(await file.text());
    if (d.app !== "ile-aux-mots" || !d.prof || !d.prof.p7 || !d.prof.p4) throw new Error("format");
    S.names = Object.assign({}, S.names, d.names || {}); S.prof = d.prof; S.present = !!d.present;
    normProf(); saveLocal(); saveConfig(); saveProfile("p7"); saveProfile("p4");
    renderHome(); openParent();
    toast(tx("restored", S.prof.p7.stars, S.prof.p4.stars));
  } catch(e) { toast(tx("notBackup")); }
};
$("presentSw").onclick = () => { S.present = !S.present; $("presentSw").setAttribute("aria-checked", String(S.present)); saveConfig(); toast(tx(S.present ? "presentOn" : "presentOff")); };
$("saveNames").onclick = () => {
  // an empty field or the default name in any language keeps the default, which follows the language
  const keep = (k, id, key) => { const v = $(id).value.trim(); S.names[k] = !v || v === tx(key) ? KID_DEFAULT[k].name : v; };
  keep("p7", "name7", "bigKid"); keep("p4", "name4", "littleKid"); saveConfig(); toast(tx("namesSaved"));
};
$("fbSend").onclick = async () => {
  const text = $("fbText").value.trim();
  if (!text && !fbTags.size) return toast(tx("pickTag"));
  const id = uid();
  await record("notes", id, {at:new Date().toISOString(), t:Date.now(), act:$("fbAct").value, kid:$("fbKid").value, tags:[...fbTags], text, withParent:S.present, device, ver:VERSION, status:"new"});
  $("fbText").value = ""; openParent(); toast(tx("thanksNote"));
};

async function loadStats(){
  const table = $("statsTable"), hard = $("hardWords"), list = $("notesList");
  let sessions = [], notes = [];
  try { sessions = await Store.list("sessions", 600); } catch(e) {}
  try { notes = await Store.list("notes", 15); } catch(e) {}
  S.pending.forEach(p => { if (p.kind === "sessions") sessions.push(p.data); else if (p.kind === "notes") notes.unshift(p.data); });
  if (!sessions.length) { table.innerHTML = `<tr><td class="muted">${tx("noSessions")}</td></tr>`; }
  else {
    const rows = ACTS.map(a => {
      const s = sessions.filter(x => x.act === a.id);
      if (!s.length) return `<tr><td>${a.em} ${actTitle(a)}</td><td colspan="5" class="muted">${tx("notPlayed")}</td></tr>`;
      const done = s.filter(x => x.completed).length, rated = s.filter(x => x.rating);
      const rounds = s.reduce((n,x) => n + (x.played||0), 0), first = s.reduce((n,x) => n + (x.firstTry||0), 0);
      const pct = rounds ? Math.round(100*first/rounds) : null;
      const avg = rated.length ? rated.reduce((n,x) => n + x.rating, 0) / rated.length : null;
      const face = avg == null ? "–" : avg >= 2.5 ? "😍" : avg >= 1.7 ? "🙂" : "😴";
      const cls = v => v == null ? "" : v >= 75 ? "g" : v >= 50 ? "m" : "b";
      return `<tr><td>${a.em} ${actTitle(a)}</td><td>${s.length}</td><td><span class="pill ${cls(Math.round(100*done/s.length))}">${Math.round(100*done/s.length)} %</span></td>
        <td>${pct == null ? "–" : `<span class="pill ${cls(pct)}">${pct} %</span>`}</td><td>${face} ${rated.length ? `(${rated.length})` : ""}</td>
        <td>${Math.round(s.reduce((n,x) => n + (x.secs||0), 0) / s.length)} s</td></tr>`;
    }).join("");
    table.innerHTML = `<tr>${tx("statsHead").map(h => `<th>${h}</th>`).join("")}</tr>` + rows;
  }
  hard.innerHTML = "";
  const hw = [];
  ["p7","p4"].forEach(k => Object.entries(S.prof[k].words).forEach(([w,[ok,ko]]) => { if (ko > 0 && ok + ko >= 2) hw.push({w, k, r: ko/(ok+ko), n: ok+ko}); }));
  hw.sort((a,b) => b.r - a.r || b.n - a.n);
  if (!hw.length) hard.innerHTML = `<p class="muted">${tx("nothing")}</p>`;
  hw.slice(0, 14).forEach(x => { const c = el("button","chip"); c.textContent = `${x.w} · ${kidName(x.k)} ${tx("missed", Math.round(100*x.r))}`; c.onclick = () => say(x.w, "en"); hard.append(c); });
  list.innerHTML = notes.length ? "" : `<p class="muted">${tx("noNotes")}</p>`;
  notes.forEach(n => {
    const d = el("div","note");
    const fa = ACTS.find(a => a.id === n.act), act = fa ? actTitle(fa) : tx("generalShort");
    const head = el("b"); head.textContent = `${fmtDate(n.at)} · ${act} · ${(n.tags||[]).map(fbTagLabel).join(", ")}${n.status === "done" ? " · " + tx("handled") : ""}`;
    const body = el("div"); body.textContent = n.text || "";
    d.append(head, body); list.append(d);
  });
}
