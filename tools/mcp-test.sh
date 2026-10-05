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
# SCIPIO: 4.0.0: End-to-end MCP scenario tests against a running Scipio server.
#
# Usage: tools/mcp-test.sh [-b base-url] [-t admin-token] [-c customer-token] [-a agent-token] [-o orderId] scenario...
#   scenarios: smoke apps product user order invoice cms cmstools security skills core make ops setup all
# Env fallbacks: SCIPIO_MCP_TOKEN (admin, FULLADMIN user), SCIPIO_MCP_CUSTOMER_TOKEN (a shop customer, e.g. DemoCustomer),
#   SCIPIO_MCP_AGENT_TOKEN (a token of the seed user scp-agent, group SCIPIO_AGENT; needed by the security scenario)
# Exit code: number of failed checks (0 = all pass).
set -u
BASE="https://localhost:8443"
ADMIN="${SCIPIO_MCP_TOKEN:-}"
CUST="${SCIPIO_MCP_CUSTOMER_TOKEN:-}"
AGENT="${SCIPIO_MCP_AGENT_TOKEN:-}"
ORDER_ID="${MCP_TEST_ORDER_ID:-}"
while getopts "b:t:c:a:o:" o; do
  case $o in
    b) BASE=$OPTARG ;;
    t) ADMIN=$OPTARG ;;
    c) CUST=$OPTARG ;;
    a) AGENT=$OPTARG ;;
    o) ORDER_ID=$OPTARG ;;
    *) echo "bad option"; exit 1 ;;
  esac
done
shift $((OPTIND - 1))
[ $# -gt 0 ] || { echo "usage: $0 [-b base] [-t admin-token] [-c customer-token] [-a agent-token] [-o orderId] smoke|apps|product|user|order|invoice|cms|cmstools|security|skills|core|make|ops|setup|all"; exit 1; }

PASS=0; FAIL=0; TS=$(date +%s)
ok()   { PASS=$((PASS + 1)); echo "  PASS $1"; }
fail() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-}"; }

# rpc <webapp> <token|-> <session|-> <json>
rpc() {
  local w=$1 t=$2 s=$3 j=$4
  local h=(-H "Content-Type: application/json")
  [ "$t" != "-" ] && h+=(-H "Authorization: Bearer $t")
  [ "$s" != "-" ] && h+=(-H "Mcp-Session-Id: $s")
  curl -sk -X POST "$BASE/$w/mcp" "${h[@]}" -d "$j"
}
# tool <webapp> <token> <session> <toolName> <args-json>
tool() { rpc "$1" "$2" "$3" "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"$4\",\"arguments\":$5}}"; }
# svc <webapp> <token> <serviceName> <params-json>
svc() { tool "$1" "$2" - scipio_service "{\"action\":\"call\",\"name\":\"$3\",\"params\":$4}"; }
session() {
  curl -sk -D - -o /dev/null -X POST "$BASE/$1/mcp" -H "Authorization: Bearer $2" -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"mcp-test","version":"1"}}}' \
    | grep -i "^Mcp-Session-Id:" | awk '{print $2}' | tr -d '\r'
}
# get <json> <key>: first "key":"value" or "key":number from the compact structuredContent part
get() { echo "$1" | grep -o "\"$2\":\"[^\"]*\"" | head -1 | sed 's/^"[^"]*":"//; s/"$//'; }
getnum() { echo "$1" | grep -o "\"$2\":[0-9]*" | head -1 | sed 's/.*://'; }
iserr() { echo "$1" | grep -q '"isError":true'; }
errtext() { echo "$1" | grep -o '"text":"[^"]*"' | head -1 | cut -c1-220; }
expect_ok() { # <check-name> <response>
  if iserr "$2" || ! echo "$2" | grep -q '"result"'; then fail "$1" "$(errtext "$2")"; return 1; fi
  ok "$1"; return 0
}
need_admin() { [ -n "$ADMIN" ] || { echo "admin token required (-t or SCIPIO_MCP_TOKEN)"; exit 1; }; }
need_cust()  { [ -n "$CUST" ]  || { echo "customer token required (-c or SCIPIO_MCP_CUSTOMER_TOKEN)"; exit 1; }; }
json_escape() { sed 's/\\/\\\\/g; s/"/\\"/g' | tr '\n' ' '; }

scenario_smoke() {
  echo "== smoke"
  need_admin
  local out; out=$(bash "$(dirname "$0")/mcp-smoke.sh" "$BASE" "$ADMIN" 2>&1)
  echo "$out" | grep -q "idempotentReplay" && ok "smoke script (see tools/mcp-smoke.sh)" || fail "smoke script" "$(echo "$out" | tail -3 | tr '\n' ' ')"
}

scenario_product() {
  echo "== product: virtual product with two color variants, prices, category"
  need_admin
  local VIRT="MCP-SHIRT-$TS" r fid var
  r=$(svc catalog "$ADMIN" createProduct "{\"productId\":\"$VIRT\",\"productTypeId\":\"FINISHED_GOOD\",\"internalName\":\"MCP Test Shirt $TS\",\"productName\":\"MCP Test Shirt\",\"description\":\"Virtual product created through MCP\",\"isVirtual\":\"Y\",\"isVariant\":\"N\"}")
  expect_ok "createProduct virtual $VIRT" "$r" || return
  for color in Red Blue; do
    r=$(svc catalog "$ADMIN" createProductFeature "{\"productFeatureTypeId\":\"COLOR\",\"description\":\"$color (mcp $TS)\"}")
    fid=$(get "$r" productFeatureId)
    expect_ok "createProductFeature $color -> $fid" "$r" || continue
    r=$(svc catalog "$ADMIN" applyFeatureToProduct "{\"productId\":\"$VIRT\",\"productFeatureId\":\"$fid\",\"productFeatureApplTypeId\":\"SELECTABLE_FEATURE\"}")
    expect_ok "applyFeatureToProduct selectable $color" "$r"
    var="$VIRT-$(echo "$color" | tr '[:lower:]' '[:upper:]')"
    r=$(svc catalog "$ADMIN" createProduct "{\"productId\":\"$var\",\"productTypeId\":\"FINISHED_GOOD\",\"internalName\":\"MCP Test Shirt $color $TS\",\"productName\":\"MCP Test Shirt $color\",\"isVirtual\":\"N\",\"isVariant\":\"Y\"}")
    expect_ok "createProduct variant $var" "$r" || continue
    r=$(svc catalog "$ADMIN" applyFeatureToProduct "{\"productId\":\"$var\",\"productFeatureId\":\"$fid\",\"productFeatureApplTypeId\":\"STANDARD_FEATURE\"}")
    expect_ok "applyFeatureToProduct standard $color" "$r"
    r=$(svc catalog "$ADMIN" createProductAssoc "{\"productId\":\"$VIRT\",\"productIdTo\":\"$var\",\"productAssocTypeId\":\"PRODUCT_VARIANT\"}")
    expect_ok "createProductAssoc variant $color" "$r"
    r=$(svc catalog "$ADMIN" createProductPrice "{\"productId\":\"$var\",\"productPricePurposeId\":\"PURCHASE\",\"productPriceTypeId\":\"DEFAULT_PRICE\",\"currencyUomId\":\"USD\",\"productStoreGroupId\":\"_NA_\",\"price\":\"24.99\"}")
    expect_ok "createProductPrice DEFAULT_PRICE $color" "$r"
    r=$(svc catalog "$ADMIN" createProductPrice "{\"productId\":\"$var\",\"productPricePurposeId\":\"PURCHASE\",\"productPriceTypeId\":\"LIST_PRICE\",\"currencyUomId\":\"USD\",\"productStoreGroupId\":\"_NA_\",\"price\":\"29.99\"}")
    expect_ok "createProductPrice LIST_PRICE $color" "$r"
  done
  r=$(svc catalog "$ADMIN" createProductPrice "{\"productId\":\"$VIRT\",\"productPricePurposeId\":\"PURCHASE\",\"productPriceTypeId\":\"DEFAULT_PRICE\",\"currencyUomId\":\"USD\",\"productStoreGroupId\":\"_NA_\",\"price\":\"24.99\"}")
  expect_ok "createProductPrice virtual" "$r"
  r=$(svc catalog "$ADMIN" addProductToCategory "{\"productId\":\"$VIRT\",\"productCategoryId\":\"PROMOTIONS\"}")
  expect_ok "addProductToCategory PROMOTIONS" "$r"
  r=$(tool catalog "$ADMIN" - product "{\"action\":\"get\",\"productId\":\"$VIRT\"}")
  expect_ok "product:get $VIRT" "$r"
  r=$(tool catalog "$ADMIN" - scipio_entity "{\"action\":\"find\",\"entityName\":\"ProductAssoc\",\"conditions\":[{\"field\":\"productId\",\"op\":\"eq\",\"value\":\"$VIRT\"},{\"field\":\"productAssocTypeId\",\"op\":\"eq\",\"value\":\"PRODUCT_VARIANT\"}]}")
  [ "$(getnum "$r" count)" = "2" ] && ok "two variants linked" || fail "two variants linked" "$(errtext "$r")"
  echo "  product: $VIRT"
}

scenario_user() {
  echo "== user: person + user login + CUSTOMER role (admin only: *UserLogin* services need MCP_ADMIN)"
  need_admin
  local UID_="mcp-user-$TS" r pid
  r=$(svc partymgr "$ADMIN" createPersonAndUserLogin "{\"firstName\":\"Mcp\",\"lastName\":\"Tester $TS\",\"userLoginId\":\"$UID_\",\"currentPassword\":\"Scipio-2026-mcp\",\"currentPasswordVerify\":\"Scipio-2026-mcp\"}")
  pid=$(get "$r" partyId)
  expect_ok "createPersonAndUserLogin $UID_ -> party $pid" "$r" || return
  r=$(svc partymgr "$ADMIN" createPartyRole "{\"partyId\":\"$pid\",\"roleTypeId\":\"CUSTOMER\"}")
  expect_ok "createPartyRole CUSTOMER" "$r"
  r=$(tool partymgr "$ADMIN" - party "{\"action\":\"get\",\"partyId\":\"$pid\"}")
  echo "$r" | grep -q "Tester" && ok "party:get shows the new person" || fail "party:get" "$(errtext "$r")"
  r=$(tool partymgr "$ADMIN" - scipio_entity "{\"action\":\"find\",\"entityName\":\"UserLogin\",\"conditions\":[{\"field\":\"userLoginId\",\"op\":\"eq\",\"value\":\"$UID_\"}]}")
  iserr "$r" && ok "UserLogin entity stays blocked for direct reads" || fail "UserLogin entity read must be denied"
  echo "  user: $UID_ party: $pid"
}

scenario_order() {
  echo "== order: customer searches, fills the cart, checks out; admin reviews and approves"
  need_cust; need_admin
  local SID r PID
  SID=$(session shop "$CUST")
  [ -n "$SID" ] && ok "shop session created" || { fail "shop session"; return; }
  r=$(tool shop "$CUST" "$SID" shop_catalog '{"action":"search","keyword":"camera","limit":1}')
  PID=$(get "$r" productId)
  [ -n "$PID" ] && ok "shop_catalog:search -> $PID" || { fail "shop_catalog:search" "$(errtext "$r")"; return; }
  r=$(tool shop "$CUST" "$SID" shop_cart "{\"action\":\"add\",\"productId\":\"$PID\",\"quantity\":1}")
  expect_ok "shop_cart:add" "$r" || return
  r=$(tool shop "$CUST" "$SID" shop_cart '{"action":"checkout"}')
  ORDER_ID=$(get "$r" orderId)
  expect_ok "shop_cart:checkout -> order $ORDER_ID" "$r" || return
  r=$(tool ordermgr "$ADMIN" - order "{\"action\":\"get\",\"orderId\":\"$ORDER_ID\"}")
  expect_ok "order:get $ORDER_ID (admin)" "$r"
  r=$(tool ordermgr "$ADMIN" - order "{\"action\":\"set_status\",\"orderId\":\"$ORDER_ID\",\"statusId\":\"ORDER_APPROVED\"}")
  if iserr "$r" && echo "$r" | grep -qi "already\|same"; then ok "order already approved"; else expect_ok "order:set_status ORDER_APPROVED" "$r"; fi
  r=$(tool shop "$CUST" "$SID" shop_account '{"action":"orders"}')
  echo "$r" | grep -q "$ORDER_ID" && ok "shop_account:orders lists the order" || fail "shop_account:orders" "$(errtext "$r")"
  echo "  order: $ORDER_ID"
  echo "$ORDER_ID" > /tmp/mcp-test-last-order
}

scenario_invoice() {
  echo "== invoice: create the invoice for an order and read it"
  need_admin
  [ -n "$ORDER_ID" ] || ORDER_ID=$(cat /tmp/mcp-test-last-order 2>/dev/null || true)
  [ -n "$ORDER_ID" ] || { fail "invoice" "no order id (run the order scenario or pass -o)"; return; }
  local r INV
  r=$(svc accounting "$ADMIN" createInvoiceForOrderAllItems "{\"orderId\":\"$ORDER_ID\"}")
  INV=$(get "$r" invoiceId)
  expect_ok "createInvoiceForOrderAllItems $ORDER_ID -> $INV" "$r" || return
  r=$(tool accounting "$ADMIN" - scipio_entity "{\"action\":\"find\",\"entityName\":\"Invoice\",\"conditions\":[{\"field\":\"invoiceId\",\"op\":\"eq\",\"value\":\"$INV\"}]}")
  [ "$(getnum "$r" count)" = "1" ] && ok "Invoice record readable" || fail "Invoice read" "$(errtext "$r")"
  r=$(tool accounting "$ADMIN" - scipio_entity "{\"action\":\"find\",\"entityName\":\"InvoiceItem\",\"conditions\":[{\"field\":\"invoiceId\",\"op\":\"eq\",\"value\":\"$INV\"}],\"fields\":[\"invoiceItemSeqId\",\"productId\",\"quantity\",\"amount\"]}")
  [ "$(getnum "$r" count)" != "" ] && [ "$(getnum "$r" count)" -ge 1 ] && ok "InvoiceItem rows: $(getnum "$r" count)" || fail "InvoiceItem read" "$(errtext "$r")"
  echo "  invoice: $INV"
}

scenario_cms() {
  echo "== cms: page template + groovy script + page + version; render through /website"
  need_admin
  local r TPL SCR PAGE VER PPATH="/mcp-test-$TS" body script html
  body=$(cat <<'EOF' | json_escape
<html><body>
<h1>MCP test page</h1>
<p id="time">Server time: ${mcpServerTime!"n/a"}</p>
<p id="count">Products: ${mcpProductCount!0}</p>
<ul id="products"><#list mcpProducts![] as p><li>${p.productId} - ${p.internalName!""}</li></#list></ul>
</body></html>
EOF
)
  script=$(cat <<'EOF' | json_escape
import org.ofbiz.entity.util.EntityQuery;
context.mcpServerTime = new java.sql.Timestamp(System.currentTimeMillis()).toString();
context.mcpProducts = EntityQuery.use(delegator).from("Product").where("productTypeId", "FINISHED_GOOD").orderBy("-createdStamp").maxRows(3).queryList();
context.mcpProductCount = EntityQuery.use(delegator).from("Product").queryCount();
EOF
)
  r=$(svc cms "$ADMIN" cmsCreatePageTemplate "{\"templateName\":\"McpTestTemplate$TS\",\"webSiteId\":\"cmsSite\",\"description\":\"MCP test\",\"templateBody\":\"$body\"}")
  TPL=$(get "$r" pageTemplateId)
  expect_ok "cmsCreatePageTemplate -> $TPL" "$r" || return
  r=$(svc cms "$ADMIN" cmsUpdatePageTemplateScript "{\"pageTemplateId\":\"$TPL\",\"templateName\":\"McpTestScript$TS\",\"scriptLang\":\"groovy\",\"standalone\":\"N\",\"inputPosition\":10,\"templateBody\":\"$script\"}")
  SCR=$(get "$r" scriptTemplateId)
  expect_ok "cmsUpdatePageTemplateScript (groovy) -> $SCR" "$r" || return
  r=$(svc cms "$ADMIN" cmsCreatePage "{\"webSiteId\":\"cmsSite\",\"pageTemplateId\":\"$TPL\",\"pageName\":\"McpTestPage$TS\",\"primaryPath\":\"$PPATH\",\"description\":\"Created through MCP\"}")
  PAGE=$(get "$r" pageId)
  expect_ok "cmsCreatePage -> $PAGE ($PPATH)" "$r" || return
  r=$(svc cms "$ADMIN" cmsAddPageVersion "{\"pageId\":\"$PAGE\",\"content\":\"{\\\"title\\\":\\\"MCP test page\\\"}\",\"comment\":\"mcp test\"}")
  VER=$(get "$r" versionId)
  expect_ok "cmsAddPageVersion -> $VER" "$r" || return
  r=$(svc cms "$ADMIN" cmsActivatePageVersion "{\"pageId\":\"$PAGE\",\"versionId\":\"$VER\"}")
  expect_ok "cmsActivatePageVersion" "$r"
  html=$(curl -sk "$BASE/website$PPATH")
  echo "$html" | grep -q "Server time: 20" && ok "page renders groovy value (server time)" || fail "page render time" "$(echo "$html" | grep -o '<title>[^<]*' | head -1)"
  echo "$html" | grep -q "<li>" && ok "page lists products from groovy" || fail "page render products"
  echo "  page: $PAGE url: $BASE/website$PPATH"
}

scenario_apps() {
  echo "== apps: app-defined core tools listed per app and callable from the hub"
  need_admin
  local r n
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"list"}')
  echo "$r" | grep -o '"webapp":"ordermgr"[^}]*' | grep -q '"coreTools":\[[^]]*"order"' && ok "scipio_apps:list shows core tools per app (order)" || fail "scipio_apps:list coreTools" "$(errtext "$r")"
  echo "$r" | grep -q '"mcpUrl":"https://[^"]*/ordermgr/mcp"' && ok "scipio_apps:list shows the app endpoint url" || fail "scipio_apps:list mcpUrl"
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"tools","application":"accounting","featuredOnly":true}')
  n=$(getnum "$r" toolCount)
  [ -n "$n" ] && [ "$n" -ge 2 ] && ok "scipio_apps:tools accounting lists $n featured topic tools" || fail "scipio_apps:tools accounting" "$(errtext "$r")"
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"tools"}')
  n=$(echo "$r" | grep -o '"application":"[a-z]*"' | sort -u | wc -l)
  [ "$n" -ge 10 ] && ok "scipio_apps:tools lists $n apps" || fail "scipio_apps:tools all" "$n apps"
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"call","application":"order","tool":"order","arguments":{"action":"find","limit":1}}')
  expect_ok "scipio_apps:call order/order:find from the hub" "$r"
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"call","application":"party","tool":"party","arguments":{"action":"find","lastName":"Customer","limit":1}}')
  expect_ok "scipio_apps:call party/party:find from the hub" "$r"
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"call","application":"nope","tool":"x","arguments":{"action":"find"}}')
  iserr "$r" && ok "unknown app is rejected" || fail "unknown app"
  r=$(rpc ordermgr "$ADMIN" - '{"jsonrpc":"2.0","id":1,"method":"tools/list"}')
  echo "$r" | grep -q '"name":"order"' && ok "the order topic tool appears on /ordermgr/mcp" || fail "topic tool on app endpoint"
  echo "$r" | grep -q '"note_add"' && ok "the order topic lists a note_add action in _meta.scipio.actions" || fail "order topic action list" "$(errtext "$r")"
  r=$(rpc accounting "$ADMIN" - '{"jsonrpc":"2.0","id":1,"method":"tools/list"}')
  n=$(echo "$r" | grep -o '"featured":true' | wc -l)
  [ "$n" -ge 2 ] && ok "/accounting/mcp exposes $n featured topic tools" || fail "accounting endpoint featured tools" "$n"
}

scenario_cmstools() {
  echo "== cmstools: dedicated CMS tools; template + script writes need MCP_CODE_WRITE"
  need_admin
  local r TPL PAGE VER PPATH="/mcp-tools-$TS"
  r=$(tool cms "$ADMIN" - cms_template "{\"action\":\"create\",\"templateName\":\"McpToolTemplate$TS\",\"webSiteId\":\"cmsSite\",\"description\":\"MCP tool test\",\"templateBody\":\"<html><body><h1>\${cmsContent.title!''}</h1></body></html>\"}")
  TPL=$(get "$r" pageTemplateId)
  expect_ok "cms_template:create -> $TPL (admin has MCP_CODE_WRITE)" "$r" || return
  r=$(tool cms "$ADMIN" - cms_template "{\"action\":\"get\",\"pageTemplateId\":\"$TPL\"}")
  expect_ok "cms_template:get shows the active body" "$r"
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"create\",\"webSiteId\":\"cmsSite\",\"pageTemplateId\":\"$TPL\",\"pageName\":\"McpToolPage$TS\",\"primaryPath\":\"$PPATH\"}")
  PAGE=$(get "$r" pageId)
  expect_ok "cms_page:create -> $PAGE" "$r" || return
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"version_add\",\"pageId\":\"$PAGE\",\"content\":\"{\\\"title\\\":\\\"Hello from tools\\\"}\",\"comment\":\"v1\"}")
  VER=$(get "$r" versionId)
  expect_ok "cms_page:version_add -> $VER" "$r" || return
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"publish\",\"pageId\":\"$PAGE\",\"versionId\":\"$VER\"}")
  expect_ok "cms_page:publish" "$r"
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"get\",\"pageId\":\"$PAGE\"}")
  echo "$r" | grep -q "\"primaryPath\":\"$PPATH\"" && ok "cms_page:get shows the primary path" || fail "cms_page:get" "$(errtext "$r")"
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"find\",\"pathLike\":\"mcp-tools-$TS\"}")
  echo "$r" | grep -q "\"pageId\":\"$PAGE\"" && ok "cms_page:find by path" || fail "cms_page:find" "$(errtext "$r")"
  r=$(tool cms "$ADMIN" - cms_page "{\"action\":\"render\",\"pageId\":\"$PAGE\"}")
  echo "$r" | grep -q "Hello from tools" && ok "cms_page:render returns the published HTML" || fail "cms_page:render" "$(errtext "$r")"
  if [ -n "$AGENT" ]; then
    r=$(tool cms "$AGENT" - cms_template "{\"action\":\"script_update\",\"pageTemplateId\":\"$TPL\",\"templateName\":\"x\",\"scriptLang\":\"groovy\",\"templateBody\":\"context.a=1\"}")
    echo "$r" | grep -q "MCP_CODE_WRITE" && ok "scp-agent cannot write a script (MCP_CODE_WRITE)" || fail "code write gate" "$(errtext "$r")"
  fi
}

scenario_security() {
  echo "== security: fail-closed policy, transport and deny lists"
  need_admin
  local r code httpbase
  httpbase="${BASE/https:/http:}"; httpbase="${httpbase/8443/8080}"
  code=$(curl -s -o /dev/null -w "%{http_code}" -X POST "$httpbase/admin/mcp" -H "X-Forwarded-Proto: https" -H "Authorization: Bearer $ADMIN" \
      -H "Content-Type: application/json" -d '{"jsonrpc":"2.0","id":1,"method":"ping"}' 2>/dev/null || echo "000")
  [ "$code" = "403" ] && ok "spoofed X-Forwarded-Proto over plain http is rejected (403)" || fail "X-Forwarded-Proto spoof" "http status $code"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"SecurityGroupPermission","limit":1}')
  iserr "$r" && ok "Security* entities are denied even for admin" || fail "entity deny Security*"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"McpAuditLog","limit":1}')
  iserr "$r" && ok "Mcp* entities are denied (audit rows immutable through MCP)" || fail "entity deny Mcp*"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"Person","conditions":[{"field":"lastName","op":"eq","value":"nobody"}],"limit":1}')
  expect_ok "plain entity read still works for admin" "$r"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"Person","conditions":[{"field":"socialSecurityNumber","op":"like","value":"%1%"}],"limit":1}')
  echo "$r" | grep -qi "protected" && ok "conditions on protected fields are refused" || fail "protected field oracle" "$(errtext "$r")"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"Person","fields":["partyId","socialSecurityNumber"],"limit":1}')
  echo "$r" | grep -qi "protected" && ok "selecting a protected field is refused" || fail "protected field select" "$(errtext "$r")"
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"Person","orderBy":["socialSecurityNumber"],"limit":1}')
  echo "$r" | grep -qi "protected" && ok "ordering by a protected field is refused" || fail "protected field orderBy" "$(errtext "$r")"
  r=$(svc admin "$ADMIN" createMcpAccessToken '{"userLoginId":"system","tokenName":"x"}')
  iserr "$r" && ok "token services are not callable through the gateway" || fail "McpAccessToken service deny"
  r=$(tool admin "$ADMIN" - scipio_whoami '{"idempotencyKey":"sec-'$TS'"}'); r=$(tool admin "$ADMIN" - scipio_whoami '{"idempotencyKey":"sec-'$TS'"}')
  echo "$r" | grep -q '"idempotentReplay":true' && ok "idempotent replay answered (audited as REPLAY)" || fail "replay"
  r=$(tool admin "$ADMIN" - scipio_admin '{"action":"install_info"}')
  echo "$r" | grep -q 'claude mcp add' && ! echo "$r" | grep -q 'scp_' && ok "scipio_admin:install_info returns snippets without a token" || fail "install info" "$(errtext "$r")"
  if [ -z "$AGENT" ]; then echo "  (skip agent-token checks: pass -a <scp-agent token>)"; return; fi
  r=$(svc shop "$AGENT" createInvoice '{"invoiceTypeId":"SALES_INVOICE","partyIdFrom":"Company","partyId":"DemoCustomer"}')
  iserr "$r" && ok "shop endpoint: agent cannot write accounting through the gateway" || fail "shop gateway fail-closed" "$(errtext "$r")"
  r=$(svc ordermgr "$AGENT" createInvoice '{"invoiceTypeId":"SALES_INVOICE","partyIdFrom":"Company","partyId":"DemoCustomer"}')
  echo "$r" | grep -q "ACCOUNTING" && ok "ordermgr endpoint: accounting write needs ACCOUNTING_UPDATE" || fail "cross-component write" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_service '{"action":"describe","name":"getInvoicePaymentInfoList"}')
  echo "$r" | grep -q '"callable":true' && ok "ordermgr endpoint: accounting read callable with ACCOUNTING_VIEW" || fail "cross-component read" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_service '{"action":"describe","name":"createPayment"}')
  echo "$r" | grep -q '"callable":false' && echo "$r" | grep -q "ACCOUNTING_UPDATE" && ok "scipio_service:describe reports the component rule" || fail "describe callable flag" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_service '{"action":"describe","name":"createInvoice"}')
  echo "$r" | grep -q '"callable":true' && ok "a service with its own permission check is left to that check" || fail "guarded service passthrough" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_apps '{"action":"call","application":"accounting","tool":"invoice","arguments":{"action":"find","limit":1}}')
  expect_ok "agent runs an accounting read tool from the order endpoint" "$r"
  r=$(tool ordermgr "$AGENT" - scipio_apps '{"action":"call","application":"accounting","tool":"invoice","arguments":{"action":"set_status","invoiceId":"nope","statusId":"INVOICE_READY"}}')
  echo "$r" | grep -q "ACCOUNTING" && ok "agent cannot run an accounting write tool without ACCOUNTING_UPDATE" || fail "app tool write gate" "$(errtext "$r")"
  r=$(svc admin "$AGENT" createOrderNote '{"orderId":"nope","internalNote":"Y","note":"x"}')
  echo "$r" | grep -q "ORDERMGR" && ok "hub: order write needs ORDERMGR_UPDATE" || fail "hub write gate" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_entity '{"action":"find","entityName":"UserLogin","limit":1}')
  iserr "$r" && ok "agent cannot read UserLogin" || fail "UserLogin deny"
  r=$(tool ordermgr "$AGENT" - scipio_entity '{"action":"find","entityName":"Person","limit":1}')
  echo "$r" | grep -q "MCP_ENTITY_READ" && ok "agent cannot read entities outside the server allowlist" || fail "entity allowlist" "$(errtext "$r")"
  r=$(tool ordermgr "$AGENT" - scipio_entity '{"action":"find","entityName":"OrderHeader","limit":1}')
  expect_ok "agent reads an allowlisted entity on its endpoint" "$r"
}

scenario_skills() {
  echo "== skills: every skill loads, names real tools, install info available"
  need_admin
  local r n names s
  r=$(tool admin "$ADMIN" - scipio_apps '{"action":"skill_list"}')
  n=$(echo "$r" | grep -o '"uri":"scipio://skills/[^"]*"' | sort -u | wc -l)
  [ "$n" -ge 14 ] && ok "scipio_apps:skill_list lists $n skills" || fail "skill count" "$n"
  echo "$r" | grep -q '"warnings"' && fail "skills carry validation warnings" "$(echo "$r" | grep -o '"warnings":\[[^]]*\]' | head -3 | tr '\n' ' ')" || ok "no skill validation warnings"
  names=$(echo "$r" | grep -o '"uri":"scipio://skills/[^"]*"' | sort -u | sed 's/.*skills\///; s/"$//')
  for s in $names; do
    r=$(tool admin "$ADMIN" - scipio_apps "{\"action\":\"skill_get\",\"name\":\"$s\"}")
    echo "$r" | grep -q "scipio-server" && ok "scipio_apps:skill_get $s" || fail "scipio_apps:skill_get $s" "$(errtext "$r")"
  done
  r=$(rpc admin "$ADMIN" - '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"scipio://skills/scipio-agent-quickstart"}}')
  echo "$r" | grep -q '"contents"' && ok "skills readable as resources" || fail "skill resource" "$(errtext "$r")"
}

scenario_core() {
  echo "== core: document_render, mail_send_template guard, device token"
  need_admin
  local r inv
  r=$(tool admin "$ADMIN" - scipio_entity '{"action":"find","entityName":"Invoice","conditions":[{"field":"invoiceTypeId","op":"eq","value":"SALES_INVOICE"}],"limit":1}')
  inv=$(get "$r" invoiceId)
  if [ -n "$inv" ]; then
    r=$(tool accounting "$ADMIN" - scipio_document '{"action":"render","type":"invoice","id":"'$inv'"}')
    echo "$r" | grep -q '"mimeType":"application/pdf"' && ok "scipio_document:render invoice $inv" || fail "scipio_document:render invoice" "$(errtext "$r")"
    echo "$r" | grep -q '"type":"resource"' && ok "scipio_document:render returns a resource blob" || fail "scipio_document:render blob" "$(errtext "$r")"
  else
    fail "scipio_document:render" "no sales invoice in the database"
  fi
  r=$(tool admin "$ADMIN" - scipio_document '{"action":"render","type":"nope","id":"1"}')
  iserr "$r" && ok "scipio_document:render rejects an unknown type" || fail "scipio_document:render unknown type" "$(errtext "$r")"
  r=$(tool admin "$ADMIN" - scipio_document '{"action":"mail","templateId":"NOT_APPROVED","sendTo":"nobody@example.com","text":"x"}')
  iserr "$r" && ok "scipio_document:mail rejects a template outside the allow list" || fail "mail template allow list" "$(errtext "$r")"
  if [ -n "$AGENT" ]; then
    r=$(tool admin "$AGENT" - scipio_document '{"action":"mail","templateId":"MCP_CUSTOMER_NOTICE","sendTo":"nobody@example.com","text":"x"}')
    echo "$r" | grep -qi "MCP_MAIL_SEND" && ok "agent without MCP_MAIL_SEND is denied" || fail "mail permission gate" "$(errtext "$r")"
  fi
  r=$(tool admin "$ADMIN" - scipio_admin '{"action":"device_token_create","stationName":"Test station '$TS'","expiresInDays":1}')
  expect_ok "scipio_admin:device_token_create" "$r" || return
  local FLOOR; FLOOR=$(get "$r" token)
  echo "$r" | grep -q '"qrPayload":"scipio-mcp:' && ok "device token carries a qrPayload" || fail "qrPayload" "$(errtext "$r")"
  if [ -n "$FLOOR" ]; then
    r=$(tool manufacturing "$FLOOR" - shop_floor '{"action":"tasks"}')
    expect_ok "floor token reads shop_floor:tasks" "$r"
    r=$(tool manufacturing "$FLOOR" - mrp '{"action":"run","mrpName":"x","facilityId":"ScipioShopWarehouse"}')
    iserr "$r" && ok "floor token cannot run MRP" || fail "floor token MRP gate" "$(errtext "$r")"
    r=$(tool ordermgr "$FLOOR" - order '{"action":"find","limit":1}')
    if iserr "$r" || echo "$r" | grep -q '"error"'; then ok "floor token is limited to the manufacturing endpoint"; else fail "floor token endpoint limit" "$(errtext "$r")"; fi
  fi
}

scenario_make() {
  echo "== make: bom import -> work center -> routing -> schedule_check -> run -> release -> cost variance -> purchase order"
  need_admin
  local P="MCP-MAKE-$TS" C1="MCP-PART-A-$TS" C2="MCP-PART-B-$TS" r wc routing run sup po
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"import","rows":[{"parent":"'$P'","component":"'$C1'","quantity":"2","name":"MCP part A '$TS'"},{"parent":"'$P'","component":"'$C2'","quantity":"1,5","name":"MCP part B '$TS'"}]}')
  expect_ok "bom:import preview" "$r" || return
  echo "$r" | grep -q '"apply":false' && ok "bom:import preview does not write" || fail "bom:import preview flag" "$(errtext "$r")"
  r=$(tool catalog "$ADMIN" - product '{"action":"create","productId":"'$P'","productTypeId":"FINISHED_GOOD","internalName":"MCP make product '$TS'","productName":"MCP Make '$TS'"}')
  expect_ok "product:create parent $P" "$r"
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"import","apply":true,"force":true,"rows":[{"parent":"'$P'","component":"'$C1'","quantity":"2","name":"MCP part A '$TS'"},{"parent":"'$P'","component":"'$C2'","quantity":"1.5","name":"MCP part B '$TS'"}]}')
  expect_ok "bom:import apply" "$r"
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"get","productId":"'$P'","quantity":1}')
  echo "$r" | grep -q "$C1" && ok "bom:get shows the imported component" || fail "bom:get after import" "$(errtext "$r")"
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"tree","productId":"'$P'","bomType":"MANUF_COMPONENT"}')
  expect_ok "bom:tree" "$r"
  r=$(tool manufacturing "$ADMIN" - shop_floor '{"action":"work_center_create","fixedAssetName":"MCP work center '$TS'","productionCapacity":480}')
  expect_ok "shop_floor:work_center_create" "$r" || return
  wc=$(get "$r" fixedAssetId)
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"routing_create","routingName":"MCP routing '$TS'","productId":"'$P'","tasks":[{"name":"Frame","fixedAssetId":"'$wc'","setupMinutes":20,"runMinutes":10},{"name":"Final","fixedAssetId":"'$wc'","setupMinutes":5,"runMinutes":10}]}')
  expect_ok "bom:routing_create" "$r" || return
  routing=$(get "$r" routingId)
  r=$(tool manufacturing "$ADMIN" - bom '{"action":"routing_get","productId":"'$P'"}')
  echo "$r" | grep -q "Frame" && ok "bom:routing_get lists the tasks" || fail "bom:routing_get" "$(errtext "$r")"
  local due; due=$(date -d "+14 days" +%Y-%m-%d 2>/dev/null || date -v+14d +%Y-%m-%d)
  r=$(tool manufacturing "$ADMIN" - shop_floor '{"action":"schedule_check","productId":"'$P'","quantity":5,"dueDate":"'$due'","facilityId":"ScipioShopWarehouse"}')
  expect_ok "shop_floor:schedule_check" "$r"
  r=$(tool manufacturing "$ADMIN" - production_run '{"action":"create","productId":"'$P'","pRQuantity":5,"startDate":"'$(date +%Y-%m-%d)' 08:00:00","facilityId":"ScipioShopWarehouse","routingId":"'$routing'","workEffortName":"MCP run '$TS'"}')
  expect_ok "production_run:create" "$r" || return
  run=$(get "$r" productionRunId)
  r=$(tool manufacturing "$ADMIN" - production_run '{"action":"release","productionRunId":"'$run'"}')
  expect_ok "production_run:release" "$r"
  r=$(tool manufacturing "$ADMIN" - production_run '{"action":"get","productionRunId":"'$run'"}')
  echo "$r" | grep -q '"currentStatusId":"PRUN_SCHEDULED"' && ok "run is PRUN_SCHEDULED after release" || fail "run status after release" "$(errtext "$r")"
  r=$(tool manufacturing "$ADMIN" - production_run '{"action":"cost_variance","productionRunId":"'$run'"}')
  expect_ok "production_run:cost_variance" "$r"
  r=$(tool manufacturing "$ADMIN" - scipio_document '{"action":"render","type":"production_run","id":"'$run'"}')
  echo "$r" | grep -q '"mimeType":"application/pdf"' && ok "scipio_document:render production_run" || fail "scipio_document:render production_run" "$(errtext "$r")"
  r=$(tool manufacturing "$ADMIN" - mrp '{"action":"proposals","limit":3}')
  expect_ok "mrp:proposals" "$r"
  r=$(rpc manufacturing "$ADMIN" - '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"scipio://production-run/'$run'"}}')
  echo "$r" | grep -q '"contents"' && ok "production run resource" || fail "production run resource" "$(errtext "$r")"
  r=$(tool partymgr "$ADMIN" - supplier '{"action":"create","groupName":"MCP Supplier '$TS'","email":"supplier-'$TS'@example.com"}')
  expect_ok "supplier:create" "$r" || return
  sup=$(get "$r" partyId)
  r=$(tool partymgr "$ADMIN" - supplier '{"action":"product_set","partyId":"'$sup'","productId":"'$C1'","lastPrice":"2.50","currencyUomId":"USD","standardLeadTimeDays":7}')
  expect_ok "supplier:product_set" "$r"
  r=$(tool ordermgr "$ADMIN" - purchase_order '{"action":"create","supplierPartyId":"'$sup'","facilityId":"ScipioShopWarehouse","currencyUomId":"USD","items":[{"productId":"'$C1'","quantity":10}]}')
  expect_ok "purchase_order:create" "$r" || return
  po=$(get "$r" orderId)
  echo "$r" | grep -q '"statusId":"ORDER_CREATED"' && ok "purchase order is a draft (ORDER_CREATED)" || fail "purchase order draft status" "$(errtext "$r")"
  r=$(tool ordermgr "$ADMIN" - order '{"action":"approve","orderId":"'$po'"}')
  expect_ok "order:approve purchase order" "$r"
  r=$(tool ordermgr "$ADMIN" - scipio_document '{"action":"render","type":"order","id":"'$po'"}')
  echo "$r" | grep -q '"mimeType":"application/pdf"' && ok "scipio_document:render purchase order" || fail "scipio_document:render order" "$(errtext "$r")"
  echo "  make: product $P run $run purchase order $po supplier $sup"
}

scenario_ops() {
  echo "== ops: shipment plan, reorder points, open items, statement, bank statement preview, count preview"
  need_admin
  local r oid
  r=$(tool ordermgr "$ADMIN" - order '{"action":"find","orderTypeId":"SALES_ORDER","statusId":"ORDER_APPROVED","limit":1}')
  expect_ok "order:find approved sales orders" "$r"
  oid=$(get "$r" orderId)
  if [ -n "$oid" ]; then
    r=$(tool facility "$ADMIN" - shipment '{"action":"plan","orderId":"'$oid'"}')
    expect_ok "shipment:plan $oid" "$r"
  else
    echo "  (no approved sales order; shipment:plan skipped)"
  fi
  r=$(tool facility "$ADMIN" - inventory '{"action":"reorder_point","facilityId":"ScipioShopWarehouse","limit":5}')
  expect_ok "inventory:reorder_point" "$r"
  r=$(tool facility "$ADMIN" - inventory '{"action":"count_apply","facilityId":"ScipioShopWarehouse","rows":[{"productId":"GZ-1000","quantityOnHand":5}]}')
  expect_ok "inventory:count_apply preview" "$r"
  r=$(tool accounting "$ADMIN" - invoice '{"action":"open_items","limit":5}')
  expect_ok "invoice:open_items" "$r"
  r=$(tool accounting "$ADMIN" - invoice '{"action":"statement","partyId":"DemoCustomer"}')
  expect_ok "invoice:statement DemoCustomer" "$r"
  echo "$r" | grep -q '"text"' && ok "statement has a text body" || fail "statement text" "$(errtext "$r")"
  r=$(tool accounting "$ADMIN" - payment '{"action":"bank_import","format":"CSV","rows":[{"date":"2026-09-01","amount":"10.00","reference":"MCP test no match '$TS'","counterparty":"Nobody"}]}')
  expect_ok "payment:bank_import preview" "$r"
  echo "$r" | grep -q '"apply":false' && ok "bank statement preview does not write" || fail "bank statement preview flag" "$(errtext "$r")"
  r=$(tool facility "$ADMIN" - inventory '{"action":"transfer_create","inventoryItemId":"NOPE","facilityId":"ScipioShopWarehouse","facilityIdTo":"WebStoreWarehouse","statusId":"IXF_REQUESTED"}')
  iserr "$r" && ok "inventory:transfer_create rejects an unknown item" || fail "inventory:transfer_create validation" "$(errtext "$r")"
}

scenario_setup() {
  echo "== setup: checklist -> organization -> facility -> store -> catalog -> product import"
  need_admin
  local r org fac store cat
  r=$(tool setup "$ADMIN" - setup '{"action":"checklist"}')
  expect_ok "setup:checklist" "$r"
  r=$(tool setup "$ADMIN" - setup '{"action":"organization","groupName":"MCP Test Org '$TS'","currencyUomId":"USD","countryGeoId":"USA","address1":"1 Test St","city":"Testville","postalCode":"12345","stateProvinceGeoId":"CA","emailAddress":"org-'$TS'@example.com"}')
  expect_ok "setup:organization" "$r" || return
  org=$(get "$r" partyId)
  r=$(tool setup "$ADMIN" - setup '{"action":"facility","facilityName":"MCP Plant '$TS'","ownerPartyId":"'$org'","address1":"2 Plant Rd","city":"Testville","postalCode":"12345","stateProvinceGeoId":"CA","countryGeoId":"USA"}')
  expect_ok "setup:facility" "$r" || return
  fac=$(get "$r" facilityId)
  r=$(tool setup "$ADMIN" - setup '{"action":"store","storeName":"MCP Store '$TS'","ownerPartyId":"'$org'","inventoryFacilityId":"'$fac'","defaultCurrencyUomId":"USD"}')
  expect_ok "setup:store" "$r" || return
  store=$(get "$r" productStoreId)
  r=$(tool setup "$ADMIN" - setup '{"action":"catalog","catalogName":"MCP Catalog '$TS'","productStoreId":"'$store'","rootCategoryName":"All"}')
  expect_ok "setup:catalog" "$r" || return
  cat=$(get "$r" productCategoryId)
  r=$(tool catalog "$ADMIN" - product '{"action":"import","rows":[{"sku":"MCP-IMP-1-'$TS'","name":"Imported one","price":"9.90","currency":"USD","category":"'$cat'"},{"sku":"MCP-IMP-2-'$TS'","name":"Imported two","price":"19.90","currency":"USD"}]}')
  expect_ok "product:import preview" "$r"
  r=$(tool catalog "$ADMIN" - product '{"action":"import","apply":true,"rows":[{"sku":"MCP-IMP-1-'$TS'","name":"Imported one","price":"9.90","currency":"USD","category":"'$cat'"},{"sku":"MCP-IMP-2-'$TS'","name":"Imported two","price":"19.90","currency":"USD"}]}')
  expect_ok "product:import apply" "$r"
  r=$(tool catalog "$ADMIN" - product '{"action":"get","productId":"MCP-IMP-1-'$TS'"}')
  expect_ok "product:get imported product" "$r"
  r=$(tool catalog "$ADMIN" - product '{"action":"price_set","productId":"MCP-IMP-1-'$TS'","price":"10.90","currencyUomId":"USD"}')
  expect_ok "product:price_set update" "$r"
  echo "$r" | grep -q '"action":"updated"' && ok "product:price_set updated the existing price" || fail "price upsert action" "$(errtext "$r")"
  r=$(rpc setup "$ADMIN" - '{"jsonrpc":"2.0","id":1,"method":"resources/read","params":{"uri":"scipio://setup/status"}}')
  echo "$r" | grep -q '"contents"' && ok "setup status resource" || fail "setup status resource" "$(errtext "$r")"
  r=$(tool setup "$ADMIN" - setup '{"action":"checklist"}')
  expect_ok "setup:checklist after setup" "$r"
  echo "  setup: org $org facility $fac store $store category $cat"
}

for s in "$@"; do
  case $s in
    smoke) scenario_smoke ;;
    apps) scenario_apps ;;
    product) scenario_product ;;
    user) scenario_user ;;
    order) scenario_order ;;
    invoice) scenario_invoice ;;
    cms) scenario_cms ;;
    cmstools) scenario_cmstools ;;
    security) scenario_security ;;
    skills) scenario_skills ;;
    core) scenario_core ;;
    make) scenario_make ;;
    ops) scenario_ops ;;
    setup) scenario_setup ;;
    all) scenario_smoke; scenario_apps; scenario_product; scenario_user; scenario_order; scenario_invoice; scenario_cms; scenario_cmstools; scenario_security; scenario_skills; scenario_core; scenario_make; scenario_ops; scenario_setup ;;
    *) echo "unknown scenario $s"; FAIL=$((FAIL + 1)) ;;
  esac
done
echo "== result: $PASS passed, $FAIL failed"
exit $FAIL
