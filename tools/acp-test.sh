#!/usr/bin/env bash
# Scipio Commerce - Agentic Commerce Protocol (ACP) end-to-end check against a running server.
#
# Usage: ACP_TOKEN=<agent token> tools/acp-test.sh [-b https://localhost:8443] [-w shop] [-p <productId>]
#   ACP_TOKEN    MCP access token of a login with the permission ACP_CHECKOUT (for example scp-acp-agent)
#   ACP_SPT      optional Stripe shared payment token (spt_...) for a real complete; without it the complete
#                step expects the store answer for a declined or unknown token
#   ACP_WEBHOOK_PORT  optional local port of a webhook listener that tools/acp-webhook-listener.py runs
# Needs curl and python3.

set -u
BASE="https://localhost:8443"
WEBAPP="shop"
PRODUCT=""
while getopts "b:w:p:" o; do
    case "$o" in
        b) BASE="$OPTARG" ;;
        w) WEBAPP="$OPTARG" ;;
        p) PRODUCT="$OPTARG" ;;
        *) exit 2 ;;
    esac
done
: "${ACP_TOKEN:?set ACP_TOKEN}"
API="$BASE/$WEBAPP/acp"
VERSION="2026-04-17"
PY=$(command -v python3 || command -v python)
TMP=$(mktemp -d)
PASS=0
FAIL=0

ok() { PASS=$((PASS + 1)); echo "  ok   $1"; }
ko() { FAIL=$((FAIL + 1)); echo "  FAIL $1"; [ -n "${2:-}" ] && echo "       $2" | cut -c1-400; }
uuid() { "$PY" -c "import uuid;print(uuid.uuid4())"; }
jq_() { "$PY" -c "import json,sys;d=json.load(open(sys.argv[1]));print(eval(sys.argv[2]))" "$1" "$2" 2>/dev/null; }

# call METHOD PATH [BODY] [IDEMPOTENCY-KEY] -> status in $TMP/code, body in $TMP/out, headers in $TMP/hdr
call() {
    local args=(-sk -o "$TMP/out" -D "$TMP/hdr" -w "%{http_code}" -X "$1" "$API/$2"
        -H "Authorization: Bearer $ACP_TOKEN" -H "API-Version: $VERSION" -H "Content-Type: application/json")
    [ -n "${3:-}" ] && args+=(--data "$3")
    [ -n "${4:-}" ] && args+=(-H "Idempotency-Key: $4")
    curl "${args[@]}" > "$TMP/code"
}
code() { cat "$TMP/code"; }
field() { jq_ "$TMP/out" "$1"; }

echo "ACP check against $API"

# ---------------------------------------------------------------- discovery
c=$(curl -sk -o "$TMP/disc" -w "%{http_code}" "$BASE/$WEBAPP/.well-known/acp.json")
if [ "$c" = 200 ] && [ "$(jq_ "$TMP/disc" "d['protocol']['name']")" = acp ]; then ok "discovery (store webapp)"; else ko "discovery (store webapp) $c" "$(cat "$TMP/disc")"; fi
c=$(curl -sk -o "$TMP/disc2" -w "%{http_code}" "$BASE/.well-known/acp.json")
if [ "$c" = 200 ] && jq_ "$TMP/disc2" "d['api_base_url']" | grep -q "/acp$"; then ok "discovery (root)"; else ko "discovery (root) $c" "$(cat "$TMP/disc2")"; fi

# ---------------------------------------------------------------- auth and headers
c=$(curl -sk -o "$TMP/out" -w "%{http_code}" -X POST "$API/checkout_sessions" -H "API-Version: $VERSION" -d '{}')
[ "$c" = 401 ] && ok "no token -> 401" || ko "no token -> $c"
c=$(curl -sk -o "$TMP/out" -w "%{http_code}" -X POST "$API/checkout_sessions" -H "Authorization: Bearer $ACP_TOKEN" -H "Idempotency-Key: $(uuid)" -d '{}')
[ "$c" = 400 ] && ok "no API-Version -> 400" || ko "no API-Version -> $c" "$(cat "$TMP/out")"
call POST checkout_sessions '{"line_items":[{"id":"x"}]}'
[ "$(code)" = 400 ] && [ "$(field "d['code']")" = idempotency_key_required ] && ok "no Idempotency-Key -> 400" || ko "no Idempotency-Key -> $(code)" "$(cat "$TMP/out")"

# ---------------------------------------------------------------- feed (also finds a product)
c=$(curl -sk -o "$TMP/feed.jsonl" -w "%{http_code}" "$API/feed/products.jsonl" -H "Authorization: Bearer $ACP_TOKEN")
if [ "$c" = 200 ] && "$PY" -c "
import json,sys
lines=[l for l in open(sys.argv[1],encoding='utf-8') if l.strip()]
assert lines
for l in lines:
    p=json.loads(l); assert p['id'] and p['variants']
    for v in p['variants']: assert v['id'] and v['title'] and v['price']['amount']>=0
print(len(lines))" "$TMP/feed.jsonl" > "$TMP/feedn"; then ok "feed products.jsonl ($(cat "$TMP/feedn") products)"; else ko "feed products.jsonl $c" "$(head -c 300 "$TMP/feed.jsonl")"; fi
c=$(curl -sk -o "$TMP/meta" -w "%{http_code}" "$API/feed/metadata.json" -H "Authorization: Bearer $ACP_TOKEN")
[ "$c" = 200 ] && [ -n "$(jq_ "$TMP/meta" "d['id']")" ] && ok "feed metadata.json" || ko "feed metadata.json $c"
if [ -z "$PRODUCT" ]; then
    PRODUCT=$("$PY" -c "
import json,sys
c=[]
for l in open(sys.argv[1],encoding='utf-8'):
    p=json.loads(l)
    for v in p['variants']:
        if v.get('availability',{}).get('available') and v['price']['amount']>0: c.append((v['price']['amount'],v['id']))
print(min(c)[1] if c else '')" "$TMP/feed.jsonl")
fi
echo "  product: $PRODUCT"

# ---------------------------------------------------------------- create, update
ADDR='{"name":"Jane Doe","line_one":"1234 Chat Road","city":"San Francisco","state":"CA","country":"US","postal_code":"94131"}'
call POST checkout_sessions "{\"currency\":\"usd\",\"line_items\":[{\"id\":\"$PRODUCT\",\"quantity\":2}]}" "$(uuid)"
CS=$(field "d['id']")
if [ "$(code)" = 201 ] && [ "$(field "d['status']")" = not_ready_for_payment ]; then ok "create without address -> 201 not_ready_for_payment"; else ko "create $(code)" "$(cat "$TMP/out")"; fi
field "[m['param'] for m in d['messages']]" | grep -q "fulfillment_details.address" && ok "message names the missing address" || ko "missing address message" "$(field "d['messages']")"

KEY=$(uuid)
BODY="{\"buyer\":{\"first_name\":\"Jane\",\"last_name\":\"Doe\",\"email\":\"jane.doe@example.com\"},\"fulfillment_details\":{\"name\":\"Jane Doe\",\"address\":$ADDR}}"
call POST "checkout_sessions/$CS" "$BODY" "$KEY"
cp "$TMP/out" "$TMP/upd1"
if [ "$(code)" = 200 ] && [ "$(field "d['status']")" = ready_for_payment ]; then ok "update with buyer and address -> ready_for_payment"; else ko "update $(code)" "$(cat "$TMP/out")"; fi
NOPT=$(field "len(d['fulfillment_options'])")
[ "${NOPT:-0}" -ge 1 ] && ok "fulfillment options: $NOPT" || ko "no fulfillment options"
TOTAL1=$(field "[t['amount'] for t in d['totals'] if t['type']=='total'][0]")
field "sorted(set(t['type'] for t in d['totals']))" | grep -q "fulfillment" && ok "totals carry fulfillment and tax" || ko "totals" "$(field "d['totals']")"

# idempotency: replay, conflict
call POST "checkout_sessions/$CS" "$BODY" "$KEY"
grep -qi "Idempotent-Replayed: true" "$TMP/hdr" && ok "idempotent replay" || ko "idempotent replay" "$(cat "$TMP/hdr")"
call POST "checkout_sessions/$CS" '{"order_notes":"other"}' "$KEY"
[ "$(code)" = 422 ] && [ "$(field "d['code']")" = idempotency_conflict ] && ok "idempotency conflict -> 422" || ko "idempotency conflict -> $(code)" "$(cat "$TMP/out")"

# select another option: totals follow
if [ "${NOPT:-0}" -ge 2 ]; then
    OPT2=$(jq_ "$TMP/upd1" "[o['id'] for o in d['fulfillment_options'] if o['id']!=d['selected_fulfillment_options'][0]['option_id']][0]")
    call POST "checkout_sessions/$CS" "{\"selected_fulfillment_options\":[{\"type\":\"shipping\",\"option_id\":\"$OPT2\",\"item_ids\":[]}]}" "$(uuid)"
    TOTAL2=$(field "[t['amount'] for t in d['totals'] if t['type']=='total'][0]")
    SEL=$(field "d['selected_fulfillment_options'][0]['option_id']")
    [ "$SEL" = "$OPT2" ] && ok "select option $OPT2 (total $TOTAL1 -> $TOTAL2)" || ko "select option" "$(cat "$TMP/out")"
fi

# unknown item
call POST checkout_sessions '{"line_items":[{"id":"NO_SUCH_PRODUCT_X"}]}' "$(uuid)"
field "[m['code'] for m in d['messages']]" | grep -q invalid && ok "unknown item -> message invalid" || ko "unknown item" "$(cat "$TMP/out")"

# get
call GET "checkout_sessions/$CS"
[ "$(code)" = 200 ] && [ "$(field "d['id']")" = "$CS" ] && ok "get" || ko "get $(code)"
call GET "checkout_sessions/cs_doesnotexist"
[ "$(code)" = 404 ] && ok "get unknown -> 404" || ko "get unknown -> $(code)"

# ---------------------------------------------------------------- complete
call POST "checkout_sessions/$CS/complete" '{}' "$(uuid)"
[ "$(code)" = 400 ] && [ "$(field "d['param']")" = '$.payment_data' ] && ok "complete without payment_data -> 400" || ko "complete without payment_data -> $(code)" "$(cat "$TMP/out")"

call POST "checkout_sessions/$CS/complete" '{"payment_data":{"handler_id":"card_tokenized","instrument":{"type":"card","credential":{"type":"spt","token":"spt_invalid_for_test"}}}}' "$(uuid)"
if [ "$(code)" = 200 ] && [ "$(field "d['status']")" = ready_for_payment ] && field "[m.get('code') for m in d['messages']]" | grep -qE "payment_declined|invalid"; then
    ok "complete with a bad token -> session stays open with a payment message"
else
    ko "complete with a bad token -> $(code)" "$(cat "$TMP/out")"
fi

if [ -n "${ACP_SPT:-}" ]; then
    call POST "checkout_sessions/$CS/complete" "{\"buyer\":{\"email\":\"jane.doe@example.com\"},\"payment_data\":{\"handler_id\":\"card_tokenized\",\"instrument\":{\"type\":\"card\",\"credential\":{\"type\":\"spt\",\"token\":\"$ACP_SPT\"}}}}" "$(uuid)"
    ORDER=$(field "d['order']['id']")
    if [ "$(code)" = 200 ] && [ "$(field "d['status']")" = completed ] && [ -n "$ORDER" ]; then ok "complete with a Stripe SPT -> completed, order $ORDER"; else ko "complete with SPT -> $(code)" "$(cat "$TMP/out")"; fi
    call POST "checkout_sessions/$CS/cancel" '{}' "$(uuid)"
    [ "$(code)" = 405 ] && ok "cancel a completed session -> 405" || ko "cancel completed -> $(code)"
    echo "$ORDER" > "$TMP/order"
fi

# cancel
call POST checkout_sessions "{\"line_items\":[{\"id\":\"$PRODUCT\"}]}" "$(uuid)"
CS2=$(field "d['id']")
call POST "checkout_sessions/$CS2/cancel" '{"intent_trace":{"reason_code":"price_sensitivity"}}' "$(uuid)"
[ "$(code)" = 200 ] && [ "$(field "d['status']")" = canceled ] && ok "cancel -> canceled" || ko "cancel -> $(code)" "$(cat "$TMP/out")"
call POST "checkout_sessions/$CS2" '{"order_notes":"x"}' "$(uuid)"
[ "$(code)" = 409 ] && ok "update a canceled session -> 409" || ko "update canceled -> $(code)"

# ---------------------------------------------------------------- MCP binding
mcp() { curl -sk -o "$TMP/out" -w "%{http_code}" -X POST "$API/mcp" -H "Authorization: Bearer $ACP_TOKEN" -H "Content-Type: application/json" -H "Accept: application/json, text/event-stream" --data "$1" > "$TMP/code"; }
mcp '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"acp-test","version":"1"}}}'
[ "$(field "d['result']['serverInfo']['name']")" = scipio-acp ] && ok "mcp initialize" || ko "mcp initialize" "$(cat "$TMP/out")"
mcp '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
[ "$(field "len(d['result']['tools'])")" = 5 ] && ok "mcp tools/list (5 tools)" || ko "mcp tools/list" "$(cat "$TMP/out")"
mcp "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"create_checkout_session\",\"arguments\":{\"meta\":{\"api_version\":\"$VERSION\",\"idempotency_key\":\"$(uuid)\"},\"payload\":{\"currency\":\"usd\",\"line_items\":[{\"id\":\"$PRODUCT\"}],\"buyer\":{\"email\":\"mcp@example.com\"},\"fulfillment_details\":{\"address\":$ADDR}}}}}"
MCS=$(field "d['result']['id']")
[ -n "$MCS" ] && [ "$(field "d['result']['status']")" = ready_for_payment ] && ok "mcp create_checkout_session" || ko "mcp create" "$(cat "$TMP/out")"
mcp "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"get_checkout_session\",\"arguments\":{\"meta\":{\"api_version\":\"$VERSION\"},\"id\":\"cs_nope\"}}}"
[ "$(field "d['error']['code']")" = -32000 ] && [ "$(field "d['error']['data']['code']")" = not_found ] && ok "mcp error -> -32000 with ACP error" || ko "mcp error" "$(cat "$TMP/out")"
mcp "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"cancel_checkout_session\",\"arguments\":{\"meta\":{\"api_version\":\"$VERSION\"},\"id\":\"$MCS\"}}}"
[ "$(field "d['result']['status']")" = canceled ] && ok "mcp cancel_checkout_session" || ko "mcp cancel" "$(cat "$TMP/out")"

echo
echo "passed $PASS, failed $FAIL"
rm -rf "$TMP"
[ "$FAIL" = 0 ]
