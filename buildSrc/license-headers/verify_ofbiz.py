#!/usr/bin/env python3
# Scipio Commerce: gate 2 of L-01 (docs/wp/L-01.md). Compare the work tree with the Apache OFBiz source trees:
#   1. the fork base: git commit d07444e04b (Apache OFBiz trunk r1621460, 2014-11-11, first import)
#   2. the release apache-ofbiz-18.12.19 (unzipped folder, argument 1), the latest release; the 2018 merges came from trunk
# A file is "from OFBiz" when the same path is in one of the trees. Fail when the OFBiz copy has the ASF header
# and the file here has not.
#   verify_ofbiz.py <folder of the unzipped OFBiz release>
import os, subprocess, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import apply_headers as a

BASE = "d07444e04b"
rules = a.load_rules()


def base_tree(root):
    names = subprocess.check_output(["git", "ls-tree", "-r", "--name-only", "-z", BASE], cwd=root).split(b"\0")
    names = [n.decode("utf8", "surrogateescape") for n in names if n]
    want = [n for n in names if os.path.splitext(n)[1].lower() in rules["type"]]
    p = subprocess.Popen(["git", "cat-file", "--batch"], cwd=root, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    res = {}
    for n in want:
        p.stdin.write((BASE + ":" + n + "\n").encode("utf8", "surrogateescape"))
        p.stdin.flush()
        hdr = p.stdout.readline().split()
        size = int(hdr[2])
        res[n] = p.stdout.read(size + 1)[:size]
    p.stdin.close()
    p.wait()
    return set(names), res


def zip_tree(d):
    names, res = set(), {}
    for dp, dn, fn in os.walk(d):
        for f in fn:
            full = os.path.join(dp, f)
            rel = os.path.relpath(full, d).replace(os.sep, "/")
            names.add(rel)
            if os.path.splitext(f)[1].lower() in rules["type"]:
                res[rel] = open(full, "rb").read()
    return names, res


def load_origin(root, release_dir):
    """Return {path: tree name} for the files here whose OFBiz copy has the ASF header."""
    trees = [("base r1621460",) + base_tree(root)]
    if release_dir:
        trees.append(("release 18.12.19",) + zip_tree(release_dir))
    cur = set(a.git_files(root))
    had = {}
    for name, names, data in trees:
        for n, d in data.items():
            if n in cur and a.ASF.search(a.head(d)):
                had.setdefault(n, name)
    load_origin.origin = {n for name, names, data in trees for n in names if n in cur}
    return had


if __name__ == "__main__":
    ROOT = subprocess.check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()
    had_asf = load_origin(ROOT, sys.argv[1] if len(sys.argv) > 1 else None)
    origin = load_origin.origin
    print("files here that are also in an OFBiz tree (any type):", len(origin))
    bad = []
    kept = skipped = 0
    for n, name in sorted(had_asf.items()):
        c, _ = a.classify(n, open(os.path.join(ROOT, n), "rb").read(), rules)
        if c == "apache":
            kept += 1
        elif c.startswith("skip"):
            skipped += 1
        else:
            bad.append((n, c))
    print("OFBiz files with the ASF header in the OFBiz tree, present here:", len(had_asf))
    print("  still with the Apache header:", kept, " skipped by a rule (third-party or generated path):", skipped,
          " LOST:", len(bad))
    for n, c in bad:
        print("LOST", n, c)
    noh = []
    for n in sorted(origin):
        if n in had_asf or os.path.splitext(n)[1].lower() not in rules["type"]:
            continue
        c, _ = a.classify(n, open(os.path.join(ROOT, n), "rb").read(), rules)
        if c in ("missing", "agpl"):
            noh.append(n)
    print("OFBiz-origin files without an ASF header in the OFBiz tree, with an AGPL header or without a header:", len(noh))
    for n in noh:
        print("NOUPSTREAMHEADER", n)
    sys.exit(1 if bad else 0)
