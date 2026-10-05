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
"""W1-02 HTTP test, step 1: makes three logins and one MCP token for each, as an entity XML file and a JSON file.

The OFBiz test runner rolls back its database changes, so the data goes in with "load-file" (bin/profile-java.sh).
Token format: scp_<12 characters>_<43 characters>_<6 characters of CRC32> (framework/mcp McpTokenUtil); the table
holds the SHA-256 hash of the whole token. Logins: w102-http-owner (TENANT_OWNER), w102-http-content (TENANT_CONTENT),
w102-http-ops (SCIPIO_OPS).

  python applications/commerce-profile/bin/w102-make-tokens.py [outdir]     (default outdir: runtime/tempfiles)
"""
import hashlib
import json
import os
import secrets
import sys
import zlib

ALPHABET = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


def base62(n):
    return "".join(secrets.choice(ALPHABET) for _ in range(n))


def crc(body):
    v = zlib.crc32(body.encode()) & 0xFFFFFFFF
    out = ""
    for _ in range(6):
        out += ALPHABET[v % 62]
        v //= 62
    return out


outdir = sys.argv[1] if len(sys.argv) > 1 else "runtime/tempfiles"
os.makedirs(outdir, exist_ok=True)
users = [("w102-http-owner", "TENANT_OWNER"), ("w102-http-content", "TENANT_CONTENT"), ("w102-http-ops", "SCIPIO_OPS")]
xml = ['<?xml version="1.0" encoding="UTF-8"?>', "<entity-engine-xml>"]
tokens = {}
for login, group in users:
    token_id, secret = base62(12), base62(43)
    body = "scp_%s_%s" % (token_id, secret)
    raw = body + "_" + crc(body)
    tokens[group] = raw
    xml.append('  <UserLogin userLoginId="%s" currentPassword="{SHA}x" enabled="Y"/>' % login)
    xml.append('  <UserLoginSecurityGroup userLoginId="%s" groupId="%s" fromDate="2026-01-01 00:00:00"/>' % (login, group))
    xml.append('  <McpAccessToken tokenId="%s" userLoginId="%s" tokenName="w102 %s" tokenHash="%s" tokenPrefix="scp_%s_%s..." '
               'webapps="*" readOnly="N" disabled="N" expiresDate="2099-01-01 00:00:00" createdByUserLogin="system" createdDate="2026-01-01 00:00:00"/>'
               % (token_id, login, group, hashlib.sha256(raw.encode()).hexdigest(), token_id, secret[:4]))
xml.append("</entity-engine-xml>")
open(os.path.join(outdir, "w102-tokens.xml"), "w", encoding="utf-8").write("\n".join(xml) + "\n")
open(os.path.join(outdir, "w102-tokens.json"), "w", encoding="utf-8").write(json.dumps(tokens))
print("wrote", os.path.join(outdir, "w102-tokens.xml"), "and w102-tokens.json")
