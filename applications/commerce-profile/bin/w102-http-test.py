#!/usr/bin/env python3
# Scipio Commerce
# Copyright (C) Ilscipio GmbH
#
# This file is part of Scipio Commerce. Scipio Commerce is free software: you
# can redistribute it and modify it under the terms of the GNU Affero General
# Public License, version 3, as published by the Free Software Foundation.
# Scipio Commerce is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details. You should have received a copy of the license with this
# work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
# A commercial license is available from Ilscipio GmbH.
#
# SPDX-License-Identifier: AGPL-3.0-only
"""W1-02 HTTP test: a store token cannot run code actions; CMS code edits are denied.

Steps (see docs/wp/W1-02.md, section 3):
  1. python applications/commerce-profile/bin/w102-make-tokens.py                 (3 logins, 3 tokens: XML + JSON file)
  2. bin/profile-java.sh load-file runtime/tempfiles/w102-tokens.xml               (server stopped)
  3. bin/profile-java.sh start                                                     (-Dscipio.hosted=true, https port 8743)
  4. python applications/commerce-profile/bin/w102-http-test.py --base https://localhost:8743
Exit code 0: every check passed. Uses the Python standard library only.
"""
import argparse
import json
import ssl
import sys
import urllib.request

ap = argparse.ArgumentParser()
ap.add_argument("--base", default="https://localhost:8743")
ap.add_argument("--tokens", default="runtime/tempfiles/w102-tokens.json")
args = ap.parse_args()

TOKENS = json.load(open(args.tokens))
CTX = ssl.create_default_context()
CTX.check_hostname = False
CTX.verify_mode = ssl.CERT_NONE
RESULTS = []
_id = [0]


def rpc(webapp, token, method, params=None):
    _id[0] += 1
    body = json.dumps({"jsonrpc": "2.0", "id": _id[0], "method": method, "params": params or {}}).encode()
    req = urllib.request.Request("%s/%s/mcp" % (args.base, webapp), data=body, method="POST",
                                 headers={"Content-Type": "application/json", "Accept": "application/json, text/event-stream",
                                          "Authorization": "Bearer " + token})
    try:
        with urllib.request.urlopen(req, context=CTX, timeout=120) as r:
            status, text = r.status, r.read().decode("utf-8", "replace")
    except urllib.error.HTTPError as e:
        status, text = e.code, e.read().decode("utf-8", "replace")
    try:
        if text.startswith("event:") or "\ndata:" in text:
            text = [l[5:].strip() for l in text.splitlines() if l.startswith("data:")][-1]
        return status, json.loads(text)
    except Exception:
        return status, {"raw": text[:300]}


def call(webapp, token, tool, arguments):
    """Returns (is_error, text)."""
    status, doc = rpc(webapp, token, "tools/call", {"name": tool, "arguments": arguments})
    if "error" in doc:
        return True, "HTTP %s error %s" % (status, json.dumps(doc["error"])[:300])
    res = doc.get("result") or {}
    text = " ".join(c.get("text", "") for c in res.get("content", []) if isinstance(c, dict))
    return bool(res.get("isError")), text[:300] if text else json.dumps(doc)[:300]


def check(name, ok, detail):
    RESULTS.append(ok)
    print("%s  %s\n      %s" % ("PASS" if ok else "FAIL", name, detail.replace("\n", " ")[:260]))


owner, content, ops = TOKENS["TENANT_OWNER"], TOKENS["TENANT_CONTENT"], TOKENS["SCIPIO_OPS"]

# 0. The tokens work at all.
status, doc = rpc("cms", owner, "initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "w102", "version": "1"}})
check("owner token initializes on /cms/mcp", status == 200 and "result" in doc, "HTTP %s" % status)
err, text = call("cms", owner, "cms_page", {"action": "find"})
check("owner token can read CMS pages (content action)", not err, text)

# 1. MCP code actions of the CMS server: every one needs MCP_CODE_WRITE. The owner does not hold it.
code_actions = [
    ("cms_template", {"action": "create", "templateName": "w102", "webSiteId": "x"}),
    ("cms_template", {"action": "version_add", "pageTemplateId": "x", "templateBody": "<p/>"}),
    ("cms_template", {"action": "script_update", "pageTemplateId": "x"}),
    ("cms_template", {"action": "script_create", "templateName": "w102"}),
    ("cms_site", {"action": "asset_upsert", "templateName": "w102", "templateBody": "<p/>"}),
    ("cms_site", {"action": "import_xml", "importName": "w102"}),
]
for tool, a in code_actions:
    err, text = call("cms", owner, tool, a)
    check("owner: %s.%s is denied" % (tool, a["action"]), err and "MCP_CODE_WRITE" in text, text)
    err, text = call("cms", content, tool, a)
    check("content editor: %s.%s is denied" % (tool, a["action"]), err and "MCP_CODE_WRITE" in text, text)

# 2. The gateway: the owner may call services, but the guard refuses the code services for every caller.
for svc, params in [("cmsCreatePageTemplate", {"templateName": "w102", "webSiteId": "x"}),
                    ("cmsAddPageTemplateVersion", {"pageTemplateId": "x", "templateBody": "<p/>"}),
                    ("cmsCreateUpdateAsset", {"templateName": "w102", "templateBody": "<p/>"}),
                    ("createFile", {}),
                    ("testGroovy", {}),
                    ("createJobSandbox", {})]:
    err, text = call("cms", owner, "scipio_service", {"action": "call", "name": svc, "params": params})
    check("owner: gateway call %s is refused (hosted profile or deny list)" % svc, err and ("hosted store" in text or "blocked by policy" in text), text)
err, text = call("cms", owner, "scipio_service", {"action": "call", "name": "cmsCreatePageTemplate", "params": {"templateName": "w102", "webSiteId": "x"}})
check("owner: gateway call cmsCreatePageTemplate is refused by the hosted profile", err and "hosted store" in text, text)
err, text = call("cms", owner, "scipio_service", {"action": "call", "name": "entityImport", "params": {}})
check("owner: gateway call entityImport is refused", err and ("blocked by policy" in text or "hosted store" in text), text)

# 3. A content editor has no gateway at all.
err, text = call("cms", content, "scipio_service", {"action": "call", "name": "cmsCreatePageTemplate", "params": {"templateName": "w102"}})
check("content editor: no gateway", err and "MCP_GATEWAY" in text, text)

# 3b. The owner has no MCP_ENTITY_READ: no entity outside the server allowlist, and never the security tables.
for ent in ("UserLogin", "McpAccessToken", "SecurityGroupPermission"):
    err, text = call("cms", owner, "scipio_entity", {"action": "find", "entityName": ent})
    check("owner: scipio_entity find %s is denied" % ent, err and ("denied" in text.lower() or "blocked" in text or "required" in text), text)

# 4. An operator (SCIPIO_OPS) is not refused by the hosted profile.
err, text = call("cms", ops, "scipio_service", {"action": "call", "name": "cmsCreatePageTemplate", "params": {"templateName": "w102 ops", "webSiteId": "x"}})
check("operator: gateway call is not refused by the hosted profile", "hosted store" not in text and "Invalid token" not in text, text)
err, text = call("cms", ops, "cms_template", {"action": "create", "templateName": "w102 ops", "webSiteId": "x"})
check("operator: cms_template.create is not denied by permission", "MCP_CODE_WRITE required" not in text and "Invalid token" not in text and "hosted store" not in text, text)

print("\n%d of %d checks passed" % (sum(RESULTS), len(RESULTS)))
sys.exit(0 if all(RESULTS) else 1)
