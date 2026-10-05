"""Second header pass (owner, 2026-09-30).
A: a file with the ASF header that is not in an OFBiz tree -> the ASF block becomes the AGPL header.
B: a file from OFBiz that Scipio changed -> keep the ASF block, add a notice for the changes after it.
Usage: python headers_pass2.py <OFBiz release folder> [--apply]"""
import os, re, sys, collections
ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
H = os.path.dirname(os.path.abspath(__file__))
REL = sys.argv[1]  # OFBiz 18.12.19 release folder
sys.path.insert(0, H)
import apply_headers as a
import verify_ofbiz as v

MOD_LINES = [
    "Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed",
    "under the GNU Affero General Public License, version 3, or a commercial",
    "license from Ilscipio GmbH (file LICENSE). The original code stays under",
    "the Apache License, version 2.0, as stated above.",
]
OPEN = {"block": rb"/\*", "xml": rb"<!--", "ftl": rb"<#--", "jsp": rb"<%--"}
CLOSE = {"block": rb"\*/", "xml": rb"-->", "ftl": rb"-->", "jsp": rb"--%>"}


def asf_block(data, style):
    m = a.ASF.search(data[:6000])
    if not m:
        return None
    if style in OPEN:
        opens = list(re.finditer(OPEN[style], data[:m.start()]))
        c = re.compile(CLOSE[style]).search(data, m.end())
        if not opens or not c:
            return None
        s, e = opens[-1].start(), c.end()
    else:
        pre = rb"[ \t]*(#|REM\b)" if style == "hash" else rb"[ \t]*REM\b"
        lines = data.split(b"\n")
        pos, idx = 0, 0
        for i, l in enumerate(lines):
            if pos + len(l) >= m.start():
                idx = i
                break
            pos += len(l) + 1
        lo = idx
        while lo > 0 and re.match(pre, lines[lo - 1]):
            lo -= 1
        hi = idx
        while hi + 1 < len(lines) and re.match(pre, lines[hi + 1]):
            hi += 1
        s = sum(len(l) + 1 for l in lines[:lo])
        e = sum(len(l) + 1 for l in lines[:hi + 1]) - 1
    t = re.match(rb"[ \t]*\r?\n?", data[e:])
    return s, e + t.end()


def body(data, style):
    b = asf_block(data, style)
    rest = data[b[1]:] if b else data
    return re.sub(rb"\s+", b"", rest)


rules = a.load_rules()
trees = [v.base_tree(ROOT)[1], v.zip_tree(REL)[1]]
cnt, manual = collections.Counter(), []
for f in a.git_files(ROOT):
    p = os.path.join(ROOT, f)
    ext = os.path.splitext(f)[1].lower()
    if not os.path.isfile(p) or ext not in rules["type"]:
        continue
    data = open(p, "rb").read()
    if a.classify(f, data, rules)[0] != "apache":
        continue
    style = rules["type"][ext]
    eol = "\r\n" if b"\r\n" in data[:4000] else "\n"
    blk = asf_block(data, style)
    if not blk:
        manual.append(("no block", f))
        continue
    s, e = blk
    orig = [t[f] for t in trees if f in t]
    if not orig and not any(f in t for t in trees):
        if data[s:e].count(b"\n") > 25:
            manual.append(("long block", f))
            continue
        new = data[:s] + a.comment(style, eol, a.AGPL_LINES) + data[e:]
        cnt["A agpl"] += 1
    else:
        mine = body(data, style)
        if any(mine == body(o, style) for o in orig):
            cnt["B unchanged"] += 1
            continue
        if b"Ilscipio GmbH. The changes are licensed" in data[:6000]:
            cnt["B done"] += 1
            continue
        new = data[:e] + a.comment(style, eol, MOD_LINES) + data[e:]
        cnt["B changed"] += 1
    if "--apply" in sys.argv:
        open(p, "wb").write(new)
for k, n in sorted(cnt.items()):
    print(k, n)
for why, f in manual:
    print("MANUAL", why, f)
