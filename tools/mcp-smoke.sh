#!/usr/bin/env bash
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
# SCIPIO: 4.0.0: MCP smoke test. Usage: tools/mcp-smoke.sh [base-url] [token]
# Without a token it checks the 401 path only. With a token it runs initialize, tools/list and two tool calls.
set -u
BASE="${1:-https://localhost:8443}"
TOKEN="${2:-${SCIPIO_MCP_TOKEN:-}}"
CURL="curl -sk${SCIPIO_DEV_AUTH_KEY:+ -H X-Dev-Auth-Key:$SCIPIO_DEV_AUTH_KEY}"

rpc() { # url json [extra-headers...]
  local url="$1"; local json="$2"; shift 2
  $CURL -X POST "$url" -H "Content-Type: application/json" "$@" -d "$json"
}
code() { # url json [extra-headers...]
  local url="$1"; local json="$2"; shift 2
  $CURL -o /dev/null -w "%{http_code}" -X POST "$url" -H "Content-Type: application/json" "$@" -d "$json"
}

INIT='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"smoke","version":"1"}}}'

echo "== 1. no token -> expect 401"
echo " admin:    $(code "$BASE/admin/mcp" "$INIT")"
echo " ordermgr: $(code "$BASE/ordermgr/mcp" "$INIT")"
echo "== 2. GET -> expect 405"
echo " $($CURL -o /dev/null -w "%{http_code}" "$BASE/admin/mcp")"
echo "== 3. shop anonymous initialize -> expect 200"
echo " $(code "$BASE/shop/mcp" "$INIT")"

if [ -z "$TOKEN" ]; then
  echo "No token given; stop here. Create one in Webtools > Agent Access and pass it as the second argument."
  exit 0
fi
AUTH=(-H "Authorization: Bearer $TOKEN")

echo "== 4. initialize with token -> expect 200 + Mcp-Session-Id"
$CURL -D - -o /dev/null -X POST "$BASE/admin/mcp" -H "Content-Type: application/json" "${AUTH[@]}" -d "$INIT" | grep -iE "^HTTP|Mcp-Session-Id|MCP-Protocol-Version"

echo "== 5. tools/list (count)"
rpc "$BASE/admin/mcp" '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' "${AUTH[@]}" | grep -o '"name":"[a-z_]*"' | sort | tr '\n' ' '; echo

echo "== 6. scipio_whoami"
rpc "$BASE/admin/mcp" '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"scipio_whoami","arguments":{}}}' "${AUTH[@]}" | head -c 600; echo

echo "== 7. scipio_search_services create order (ordermgr)"
rpc "$BASE/ordermgr/mcp" '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"scipio_search_services","arguments":{"query":"create order","limit":5}}}' "${AUTH[@]}" | head -c 900; echo

echo "== 8. scipio_list_apps (hub)"
rpc "$BASE/admin/mcp" '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"scipio_list_apps","arguments":{}}}' "${AUTH[@]}" | grep -o '"webapp":"[a-z]*"' | tr '\n' ' '; echo

echo "== 9. idempotent replay (same key twice)"
KEY="smoke-$(date +%s)"
rpc "$BASE/admin/mcp" "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"scipio_whoami\",\"arguments\":{\"idempotencyKey\":\"$KEY\"}}}" "${AUTH[@]}" | head -c 120; echo
rpc "$BASE/admin/mcp" "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"scipio_whoami\",\"arguments\":{\"idempotencyKey\":\"$KEY\"}}}" "${AUTH[@]}" | grep -o '"idempotentReplay":true'
