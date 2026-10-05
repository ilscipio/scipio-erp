#!/usr/bin/env python3
# Scipio Commerce: header tool for work package L-01 (docs/wp/L-01.md).
#   apply_headers.py --dry [class]                  count the classes, change nothing (list the files of one class)
#   apply_headers.py --apply --ofbiz <release dir>  write the headers
# A file with the ASF header keeps it. A file that came from OFBiz (verify_ofbiz.py) and lost the ASF header gets it
# back. Each other Scipio file gets the AGPL header. The classes and the rules are the same as in the Gradle task
# checkLicenseHeaders (rules.txt).
import fnmatch, os, re, subprocess, sys, collections

HERE = os.path.dirname(os.path.abspath(__file__))
AGPL_LINES = [
    "Scipio Commerce",
    "Copyright (C) Ilscipio GmbH",
    "",
    "This file is part of Scipio Commerce. Scipio Commerce is free software: you",
    "can redistribute it and modify it under the terms of the GNU Affero General",
    "Public License, version 3, as published by the Free Software Foundation.",
    "Scipio Commerce is distributed in the hope that it will be useful, but",
    "WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or",
    "FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License",
    "for more details. You should have received a copy of the license with this",
    "work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.",
    "A commercial license is available from Ilscipio GmbH.",
    "",
    "SPDX-License-Identifier: AGPL-3.0-only",
]
APACHE_LINES = [
    "Licensed to the Apache Software Foundation (ASF) under one",
    "or more contributor license agreements.  See the NOTICE file",
    "distributed with this work for additional information",
    "regarding copyright ownership.  The ASF licenses this file",
    "to you under the Apache License, Version 2.0 (the",
    '"License"); you may not use this file except in compliance',
    "with the License.  You may obtain a copy of the License at",
    "",
    "http://www.apache.org/licenses/LICENSE-2.0",
    "",
    "Unless required by applicable law or agreed to in writing,",
    "software distributed under the License is distributed on an",
    '"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY',
    "KIND, either express or implied.  See the License for the",
    "specific language governing permissions and limitations",
    "under the License.",
]
HEAD_LINES = 40
# A header line starts after comment characters only, so a string in code does not count.
LEAD = rb"^[\s/*#<!\-;%~]*(?:REM[ \t]+)?"
# Some old copies have damaged words in the first line, so the ASF header is found by its second sentence.
ASF = re.compile(rb"The\s+ASF\s+licenses\s+this\s+file(?:\s|rem|[/*#~%;])*to\s+you\s+under\s+the\s+Apache\s+License", re.I)
AGPL_TAG = re.compile(LEAD + rb"SPDX-License-Identifier: AGPL-3\.0-only", re.M)
AGPL_TXT = re.compile(LEAD + rb"[^\n]*GNU Affero General", re.M)
SPDX_ANY = re.compile(LEAD + rb"SPDX-License-Identifier:(?! AGPL-3\.0-only)", re.M)
OLD_TEXT = (rb"This file is subject to the terms and conditions defined in the\s+"
            rb"files 'LICENSE' and 'NOTICE', which are part of this source\s+code package\.")
OLD = re.compile(OLD_TEXT)
THIRD = re.compile(LEAD + rb"(copyright|\(c\)|\xc2\xa9|licensed under|licen[sc]e\s*:|permission is hereby granted"
                   rb"|@license|@preserve)", re.I | re.M)
THIRD2 = re.compile(LEAD + rb"[^\n]{0,60}\b(MIT|BSD|GPL|LGPL|MPL|Apache)\b[^\n]{0,20}\blicen[cs]e", re.I | re.M)


def load_rules():
    r = {"type": {}, "skip-dir": set(), "skip-prefix": [], "skip-web-prefix": [], "skip-name": []}
    for line in open(os.path.join(HERE, "rules.txt"), encoding="utf8"):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        k, v = line.split(None, 1)
        if k == "type":
            e, s = v.split()
            r["type"][e] = s
        elif k == "skip-dir":
            r[k].add(v)
        else:
            r[k].append(v)
    return r


def head(data):
    return b"\n".join(data.split(b"\n", HEAD_LINES)[:HEAD_LINES])


def classify(path, data, rules):
    """Return (class, detail). class: skip-path, skip-type, skip-content, apache, agpl, missing, wrong."""
    base = os.path.basename(path)
    ext = os.path.splitext(base)[1].lower()
    if ext not in rules["type"]:
        return "skip-type", ext
    parts = path.split("/")
    for d in parts[:-1]:
        if d in rules["skip-dir"]:
            return "skip-path", "dir " + d
    for p in rules["skip-prefix"]:
        if path.startswith(p):
            return "skip-path", "prefix " + p
    if ext in (".js", ".css", ".scss", ".less"):
        for p in rules["skip-web-prefix"]:
            if path.startswith(p):
                return "skip-path", "web prefix " + p
    for g in rules["skip-name"]:
        if fnmatch.fnmatch(base, g):
            return "skip-path", "name " + g
    h = head(data)
    a, s = bool(ASF.search(h)), bool(AGPL_TAG.search(h))
    if a and s:
        return "wrong", "Apache and AGPL header together"
    if a:
        return "apache", ""
    if s:
        if not AGPL_TXT.search(h):
            return "wrong", "AGPL tag without the notice text"
        if SPDX_ANY.search(h):
            return "wrong", "second SPDX id"
        return "agpl", ""
    if OLD.search(h):
        return "missing", "old Scipio notice"
    if THIRD.search(h) or THIRD2.search(h):
        return "skip-content", "third-party notice in the first lines"
    return "missing", "no header"


def comment(style, eol, lines):
    if style == "block":
        out = ["/*"] + [(" * " + l).rstrip() for l in lines] + [" */"]
    elif style == "xml":
        out = ["<!--"] + lines + ["-->"]
    elif style == "ftl":
        out = ["<#--"] + lines + ["-->"]
    elif style == "jsp":
        out = ["<%--"] + lines + ["--%>"]
    elif style == "hash":
        out = [("# " + l).rstrip() for l in lines]
    else:
        out = [("REM " + l).rstrip() for l in lines]
    return (eol.join(out) + eol).encode("utf8")


# The old Scipio notice sits in one comment of any of these kinds, or in three lines with a hash.
OLD_COMMENT = re.compile(rb"(?:/\*+|<!--|<#--|<%--)\s*" + OLD_TEXT + rb"\s*(?:\*+/|-->|--%>)[ \t]*(?:\r?\n)?")
OLD_HASH = re.compile(rb"(?:[ \t]*#+[ \t]*(?:This file is subject|files 'LICENSE'|code package)[^\n]*\r?\n){3}")


def apply(data, style, lines=AGPL_LINES):
    eol = "\r\n" if b"\r\n" in data[:4000] else "\n"
    bom = b"\xef\xbb\xbf" if data.startswith(b"\xef\xbb\xbf") else b""
    body = data[len(bom):]
    hdr = comment(style, eol, lines)
    if OLD.search(body[:2000]):
        # Replace the old notice where it stands (after a shebang, an XML line or a DOCTYPE line).
        mm = OLD_HASH.search(body[:2000]) if style in ("hash", "rem") else OLD_COMMENT.search(body[:2000])
        if not mm:
            return None
        return bom + body[:mm.start()] + hdr + body[mm.end():]
    pre = b""
    if style in ("hash", "rem") and body.startswith(b"#!"):
        i = body.find(b"\n") + 1
        if i == 0:
            return None
        pre, body = body[:i], body[i:]
    if style == "xml":
        mm = re.match(rb"<\?xml[^>]*\?>[ \t]*\r?\n?", body)
        if mm:
            pre, body = body[:mm.end()], body[mm.end():]
            if not pre.endswith(b"\n"):
                pre += eol.encode()
    return bom + pre + hdr + body


def git_files(root):
    out = subprocess.check_output(["git", "ls-files", "-z"], cwd=root)
    return [f.decode("utf8", "surrogateescape") for f in out.split(b"\0") if f]


if __name__ == "__main__":
    ROOT = subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()
    mode = sys.argv[1] if len(sys.argv) > 1 else "--dry"
    rules = load_rules()
    cnt = collections.Counter()
    detail = collections.Counter()
    failed = []
    samples = collections.defaultdict(list)
    restore = {}
    if "--ofbiz" in sys.argv:
        sys.path.insert(0, HERE)
        import verify_ofbiz
        restore = verify_ofbiz.load_origin(ROOT, sys.argv[sys.argv.index("--ofbiz") + 1])
    for f in git_files(ROOT):
        p = os.path.join(ROOT, f)
        if not os.path.isfile(p):
            continue
        ext = os.path.splitext(f)[1].lower()
        if ext not in rules["type"]:
            cnt["skip-type"] += 1
            continue
        data = open(p, "rb").read()
        c, d = classify(f, data, rules)
        cnt[c] += 1
        detail[(c, d)] += 1
        samples[c].append(f)
        if c == "missing" and mode == "--apply":
            is_apache = f in restore
            new = apply(data, rules["type"][ext], APACHE_LINES if is_apache else AGPL_LINES)
            if new is None:
                failed.append(f)
                continue
            if is_apache:
                cnt["restored-apache"] += 1
            open(p, "wb").write(new)
    for k, v in sorted(cnt.items()):
        print(k, v)
    if mode == "--dry":
        for k, v in sorted(detail.items()):
            print("  ", k, v)
        if len(sys.argv) > 2 and sys.argv[2] in samples:
            for f in samples[sys.argv[2]]:
                print(f)
    for f in failed:
        print("FAILED", f)
