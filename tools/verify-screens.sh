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
# Hardened screen/menu verification harness for the annotation-based Scipio system.
#
# Why this exists: the old test-all-screens.sh defaulted every result to PASS and
# only failed on HTTP 5xx/404 or a literal *Exception class name in the body. Scipio
# renders most widget errors as an HTTP-200 page (DEBUG mode) or an error page, with
# the stack trace going to ofbiz.log -- so broken screens passed. This harness:
#   * extracts the UNION of menu-link targets + controller @View + view-rendering
#     @Request (no @Event), so coverage matches the menus the user navigates;
#   * detects HTTP-200 error pages via has-scipio-errormsg="true", error.ftl markers,
#     freemarker/exception markers, and empty/decorator-only bodies;
#   * correlates ofbiz.log PER REQUEST (byte-offset diff, serial execution) and treats
#     any attributed [ERROR]/exception as a failure -- the most authoritative signal;
#   * treats 404 as a real failure (missing/unconverted request), and flags menu links
#     whose target resolves to no @Request/@View as BROKEN (static, no curl needed);
#   * exits non-zero on the COMBINED real-failure count (incl. log-attributed).
#
# Auth: DevAuthEvent auto-logs-in as admin unconditionally, so only -k is needed.
# Run the server with -Drender.global.exception.mode=RETHROW for crisp failures.
#
# Usage:
#   bash tools/verify-screens.sh [options] [controller...]
#   bash tools/verify-screens.sh --list            # list controllers + counts
#   bash tools/verify-screens.sh accounting         # only accounting
#   bash tools/verify-screens.sh --include-events   # also test view-requests that have @Event (UNSAFE: may mutate)
#   bash tools/verify-screens.sh --controllers accounting,partymgr   # same as passing them positionally
#   bash tools/verify-screens.sh --exclude ""                        # disable the default component exclusions
#   bash tools/verify-screens.sh --resume runtime/logs/verify-screens-TS.csv   # resume a prior v2 run
#   bash tools/verify-screens.sh --fast                               # tighter timeouts (15s / 30s retry)
#
# v2 additions: a circuit breaker aborts the sweep (exit 3) after a run of connection failures
# is confirmed by a health check, recording the untested remainder as SERVER_DOWN so --resume
# can pick up where it left off; ofbiz.log transaction-leak noise is stripped from error
# counting (it never fails a request) but is attributed via CSV column 9 to the previous
# request on the same executor thread; and pages that look like a missing required parameter
# (rather than a real bug) are classed PARAM_REQ instead of FAIL.
set -uo pipefail
export LC_ALL=C LANG=C   # required for grep -P (PCRE) in this environment

# ---- Configuration ----
BASE_URL="${SCIPIO_BASE_URL:-https://localhost:8443}"
# SCIPIO: 4.0.0: the dev auto-login needs the key in a header (security.dev.auth.bypass.key)
DEV_AUTH_HDR=(); [ -n "${SCIPIO_DEV_AUTH_KEY:-}" ] && DEV_AUTH_HDR=(-H "X-Dev-Auth-Key: ${SCIPIO_DEV_AUTH_KEY}")
TIMEOUT="${SCIPIO_TIMEOUT:-25}"
RETRY_TIMEOUT="${SCIPIO_RETRY_TIMEOUT:-40}"
WARMUP_TIMEOUT="${SCIPIO_WARMUP_TIMEOUT:-45}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LOG_FILE="${SCIPIO_LOG_FILE:-$PROJECT_ROOT/runtime/logs/ofbiz.log}"
EXPECTED_FILE="$SCRIPT_DIR/screen-test-expected.conf"
REPORT_DIR="${SCIPIO_REPORT_DIR:-$PROJECT_ROOT/runtime/logs}"
INCLUDE_EVENTS=false
SKIP_BINARY="${SCIPIO_SKIP_BINARY:-true}"   # skip screenfop/screencsv/screenxml/js exports (binary, param-required, heavy)
LOG_SETTLE="${SCIPIO_LOG_SETTLE:-0.25}"
FILTER=()
DO_LIST=false
RESUME_CSV=""
CSV_FILE=""
declare -a EXCLUDE_ARR=(assetmaint ismgr demosuite convertertest)   # default excluded controllers; --exclude overrides

# ---- Circuit breaker: N consecutive connection failures + failed health check => server is down ----
CONSEC_000=0
MAX_CONSEC_000="${SCIPIO_MAX_CONSEC_000:-3}"
SERVER_DOWN=false
HEALTH_URL="${BASE_URL}/admin/control/main"

# ---- Counters / collectors ----
PASS=0; FAIL=0; WARN=0; EXPECTED=0; PARAM_REQ=0
declare -a FAILURES=()
declare -a WARNINGS=()
declare -a BROKEN_MENU=()
declare -A RESUME_DONE=()        # "controller/uri" -> 1, loaded from --resume CSV (SERVER_DOWN rows excluded so they retest)
declare -A LAST_URI_BY_THREAD=() # exec thread name -> last request path handled (txleak attribution)
TXLEAK_RE='ControlServlet finished w/ a transaction in place|\[TransactionUtil\.rollback\]|Leaked transaction was begun'
export TXLEAK_RE   # read via ENVIRON[] in awk -- `awk -v` unescapes \[ \. \] and corrupts this regex

red(){ printf '\033[0;31m%s\033[0m' "$1"; }
grn(){ printf '\033[0;32m%s\033[0m' "$1"; }
ylw(){ printf '\033[0;33m%s\033[0m' "$1"; }
gry(){ printf '\033[0;90m%s\033[0m' "$1"; }

# ---- Controller -> mount-point ----
declare -A C2M
C2M[webtools]="admin"; C2M[accounting]="accounting"; C2M[ap]="ap"; C2M[ar]="ar"
C2M[cms]="cms"; C2M[backendsite]="backendsite"; C2M[website]="website"; C2M[ofbizsetup]="ofbizsetup"
C2M[content]="content"; C2M[humanres]="humanres"; C2M[manufacturing]="manufacturing"
C2M[marketing]="marketing"; C2M[sfa]="sfa"; C2M[ordermgr]="ordermgr"; C2M[partymgr]="partymgr"
C2M[catalog]="catalog"; C2M[facility]="facility"; C2M[shop]="shop"; C2M[workeffort]="workeffort"
C2M[setup]="setup"; C2M[crm]="crm"; C2M[iCalendar]="iCalendar"; C2M[ecommerce]="ecommerce"

# ---- Expected failures (controller/uri -> reason) ----
declare -A EXPECTED_FAIL
load_expected(){
    [ -f "$EXPECTED_FILE" ] || return 0
    while IFS= read -r line; do
        line="${line%%#*}"; line="$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
        [ -z "$line" ] && continue
        local key; key="$(echo "$line" | cut -d'|' -f1)"
        EXPECTED_FAIL["$key"]="$(echo "$line" | cut -d'|' -f2-)"
    done < "$EXPECTED_FILE"
}

# ---- --exclude helpers ----
is_excluded(){ # controller name -> 0 (excluded) / 1 (not excluded)
    local c="$1" e
    for e in "${EXCLUDE_ARR[@]:-}"; do [ -n "$e" ] && [ "$e" = "$c" ] && return 0; done
    return 1
}
build_exclude_re(){ # echoes a path-boundary alternation regex, or "" if EXCLUDE_ARR is empty
    [ "${#EXCLUDE_ARR[@]}" -eq 0 ] && { echo ""; return 0; }
    local IFS='|'; echo "(^|/)(${EXCLUDE_ARR[*]})(/|$)"
}

# ---- --resume: skip already-tested rows from a prior v2 CSV; append new rows to the same file ----
load_resume(){
    local f="$1"
    if [ ! -f "$f" ]; then
        echo "$(red ERROR): --resume CSV not found: $f" >&2; exit 1
    fi
    local expected_header="controller,mount,uri,vtype,from_menu,http,size,status,txleak,reason"
    local header; header="$(head -n1 "$f" | tr -d '\r')"
    if [ "$header" != "$expected_header" ]; then
        echo "$(red ERROR): --resume CSV has an incompatible header -- only v2 CSVs (with a txleak column) can be resumed." >&2
        echo "  found:    $header" >&2
        echo "  expected: $expected_header" >&2
        echo "  Start a fresh run instead of resuming a v1/pre-txleak CSV." >&2
        exit 1
    fi
    local c u st
    while IFS=, read -r c _ u _ _ _ _ st _ _; do
        [ "$c" = "controller" ] && continue
        [ -z "$c" ] && continue
        [ "$st" = "SERVER_DOWN" ] && continue   # untested/aborted rows: retest these
        RESUME_DONE["${c}/${u}"]=1
    done < "$f"
    CSV_FILE="$f"
}

# ---- Extraction: emit VIEW/REQ records from all annotation controller files ----
# Any java file carrying @Request( counts (ControllerDef*.java, AdminController.java,
# setup/Controller.java, ...). Runtime merges these with controller.xml (ConfigXMLReader),
# so the test universe must be the UNION of both sources.
controllerdef_files(){
    grep -rl --include='*.java' '@Request(' \
        "$PROJECT_ROOT/framework" "$PROJECT_ROOT/applications" "$PROJECT_ROOT/hot-deploy" \
        "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" 2>/dev/null \
      | grep -vE '/build/|/test/'
}
extract_controller_records(){
    controllerdef_files | while read -r f; do
        gawk '
        function emit_view(){ if(v_name!="" && v_ctrl!="") print "VIEW\t" v_ctrl "\t" v_name "\t" v_type }
        function emit_req(){ if(r_uri!="") print "REQ\t" r_ctrl "\t" r_uri "\t" r_hasview "\t" r_hasevent }
        /@(com\.ilscipio\.scipio\.ce\.webapp\.control\.def\.)?View\(/ {
            state="view"; v_name="";v_type="screen";v_ctrl=""
            if (match($0,/name = "([^"]+)"/,m)) v_name=m[1]
            if (match($0,/type = "([^"]+)"/,m)) v_type=m[1]
            if (match($0,/controller = "([^"]+)"/,m)) v_ctrl=m[1]
            next
        }
        state=="view" {
            if (v_name=="" && match($0,/name = "([^"]+)"/,m)) v_name=m[1]
            if (match($0,/type = "([^"]+)"/,m)) v_type=m[1]
            if (v_ctrl=="" && match($0,/controller = "([^"]+)"/,m)) v_ctrl=m[1]
            if ($0 ~ /^[ \t]*public /){ emit_view(); state="" }
            next
        }
        /@Request\(/ {
            state="req"; r_uri="";r_ctrl="";r_hasview=0;r_hasevent=0
            if (match($0,/uri = "([^"]+)"/,m)) r_uri=m[1]
            if (match($0,/controller = "([^"]+)"/,m)) r_ctrl=m[1]
            next
        }
        state=="req" {
            if (r_uri=="" && match($0,/uri = "([^"]+)"/,m)) r_uri=m[1]
            if (r_ctrl=="" && match($0,/controller = "([^"]+)"/,m)) r_ctrl=m[1]
            if ($0 ~ /type = "view/) r_hasview=1
            if ($0 ~ /@Event\(/) r_hasevent=1
            # SCIPIO: a delegate "public static String x(HttpServletRequest...)" IS the event (no @Event needed)
            if ($0 ~ /^[ \t]*public /){ if ($0 ~ /public static/) r_hasevent=1; emit_req(); state="" }
            next
        }
        ' "$f"
    done
}
extract_menu_targets(){
    # target name <TAB> source file (relative); excludes test-fixture menus under widget/test/
    # and menu links defined inside --exclude'd components (filtered by SOURCE path only -- the
    # target names those components expose stay in the valid-names set, see all_request_uris).
    local excl_re; excl_re="$(build_exclude_re)"
    grep -rEn '@MenuLink\(target = "[^"]+"' "$PROJECT_ROOT/applications" "$PROJECT_ROOT/framework" "$PROJECT_ROOT/hot-deploy" \
        "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" \
        --include='*.java' 2>/dev/null \
      | grep -vE '/test/' \
      | gawk -F: '{ if (match($0,/target = "([^"]+)"/,m)) { f=$1; sub(/.*scipioce-dev[\/\\]/,"",f); print m[1] "\t" f } }' \
      | awk -F'\t' -v re="$excl_re" '{ if (re=="" || $2 !~ re) print }'
}

# --- controller.xml is the live runtime request source (still full; only widget/screen XML was
#     migrated to annotations). Each webapp's real request set = its controller.xml PLUS everything
#     it <include>s (common/security/tempexpr/content/etc). We resolve includes like the runtime does.

declare -A COMP_DIR   # component name -> absolute base dir (for resolving component://NAME/...)
build_comp_dir_map(){
    local cx d
    while read -r cx; do [ -z "$cx" ] && continue; d=$(dirname "$cx"); COMP_DIR["$(basename "$d")"]="$d"; done < <(
        find "$PROJECT_ROOT/applications" "$PROJECT_ROOT/framework" "$PROJECT_ROOT/hot-deploy" \
            "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" \
            -maxdepth 2 \( -name 'scipio-component.xml' -o -name 'ofbiz-component.xml' \) 2>/dev/null)
}
resolve_location(){ # component://comp/rest -> abspath (echo nothing if unresolved)
    local loc="$1"; loc="${loc#component://}"; local comp="${loc%%/*}" rest="${loc#*/}"
    local base="${COMP_DIR[$comp]:-}"; [ -z "$base" ] && return 1; echo "$base/$rest"
}
# Strip XML comments (incl. multiline, e.g. commented-out <request-map> blocks) before any
# line-oriented grep/awk parsing of controller.xml -- otherwise dead/commented request-maps
# get collected as testable URIs. -0777 slurps the whole file so ".*?" can span lines.
strip_xml_comments(){ perl -0777 -pe 's/<!--.*?-->//gs' "$1" 2>/dev/null; }
get_includes(){ # echo component:// locations of active (non-commented) <include> in file
    strip_xml_comments "$1" | grep '<include ' | sed -nE 's/.*location="([^"]+)".*/\1/p'
}
emit_reqmaps(){ # file webappDir -> webappDir<TAB>uri<TAB>hasView<TAB>hasEvent
    strip_xml_comments "$1" | gawk -v wa="$2" '
    /<request-map / { inreq=1; uri=""; hv=0; he=0; if(match($0,/uri="([^"]+)"/,m)) uri=m[1] }
    inreq {
        if(uri=="" && match($0,/uri="([^"]+)"/,m)) uri=m[1]
        if($0 ~ /<event /) he=1
        if($0 ~ /type="view/) hv=1
        if($0 ~ /<\/request-map>/) { if(uri!="") print wa "\t" uri "\t" hv "\t" he; inreq=0 }
    }' 2>/dev/null
}
expand_webapp(){ # entryfile webappDir -> emits merged request-maps (own + all includes, recursive)
    local entry="$1" wa="$2"
    declare -A vis=(); local queue=("$entry") f loc p
    while [ "${#queue[@]}" -gt 0 ]; do
        f="${queue[0]}"; queue=("${queue[@]:1}")
        [ -n "${vis[$f]:-}" ] && continue; vis["$f"]=1
        [ -f "$f" ] || continue
        emit_reqmaps "$f" "$wa"
        # SCIPIO: controller.xml is now a stub (includes/handlers/events); the included webapp's requests live in
        # annotation classes whose controller attr == the included webapp directory name -> map them to this webapp
        local inc_wa; inc_wa=$(basename "$(dirname "$(dirname "$f")")")
        [ -s "$RECORDS" ] && awk -F'\t' -v c="$inc_wa" -v wa="$wa" '$1=="REQ" && $2==c && $3!="" {print wa"\t"$3"\t"$4"\t"$5}' "$RECORDS"
        while read -r loc; do
            [ -z "$loc" ] && continue
            p=$(resolve_location "$loc") || continue
            [ -n "$p" ] && queue+=("$p")
        done < <(get_includes "$f")
    done
}
# Entry-point webapps (mountable): per-webapp controller.xml (exclude shared include + templates)
entry_webapps(){
    find "$PROJECT_ROOT/applications" "$PROJECT_ROOT/framework" "$PROJECT_ROOT/hot-deploy" \
        "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" \
        -name controller.xml -not -path "*/build/*" 2>/dev/null | while read -r f; do
        case "$f" in
            */webapp/WEB-INF/controller.xml) continue;;
            */resources/templates/*) continue;;
        esac
        local wa; wa=$(echo "$f" | sed -E 's#.*/webapp/([^/]+)/WEB-INF/controller.xml#\1#')
        is_excluded "$wa" && continue
        echo "$f"$'\t'"$wa"
    done
}
# All request uris defined anywhere (any *controller*.xml) -- validity set for menu resolution
all_request_uris(){
    find "$PROJECT_ROOT/applications" "$PROJECT_ROOT/framework" "$PROJECT_ROOT/hot-deploy" \
        "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" \
        -name '*controller*.xml' -not -path "*/build/*" -not -path "*/resources/templates/*" 2>/dev/null \
      | while read -r cf; do strip_xml_comments "$cf"; done \
      | gawk 'match($0,/<request-map[^>]*uri="([^"]+)"/,m){print m[1]}' 2>/dev/null | sort -u
}
# All view-map names defined anywhere (any *controller*.xml) -- for the "view/Name" override
# convention (a menu link may target a bare view-map name via the "view/" prefix, bypassing
# the request-map layer entirely; this is a real OFBiz runtime feature, not a broken link).
all_view_names(){
    find "$PROJECT_ROOT/applications" "$PROJECT_ROOT/framework" "$PROJECT_ROOT/hot-deploy" \
        "$PROJECT_ROOT/specialpurpose" "$PROJECT_ROOT/addons" \
        -name '*controller*.xml' -not -path "*/build/*" -not -path "*/resources/templates/*" 2>/dev/null \
      | while read -r cf; do strip_xml_comments "$cf"; done \
      | gawk 'match($0,/<view-map[^>]*name="([^"]+)"/,m){print m[1]}' 2>/dev/null | sort -u
}

# ---- Build universe (temp files) ----
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
RECORDS="$TMP/records.tsv"; VIEWS="$TMP/views.tsv"; REQS="$TMP/reqs.tsv"
MENUS="$TMP/menus.tsv"; VALIDNAMES="$TMP/valid.txt"; TESTSET="$TMP/testset.tsv"

build_universe(){
    build_comp_dir_map
    # Annotation-defined requests first: expand_webapp maps them through controller.xml includes
    extract_controller_records > "$RECORDS"
    # Per-webapp merged request set (own controller.xml + resolved includes)
    : > "$REQS"
    while IFS=$'\t' read -r entry wa; do
        [ -z "$entry" ] && continue
        expand_webapp "$entry" "$wa" >> "$REQS"
    done < <(entry_webapps)
    # Annotation-defined requests (@Request in ControllerDef/AdminController/... files).
    # Runtime merges them into each webapp's request map (ConfigXMLReader), so union them
    # here too: controller attr == webapp dir name (mapped to mount via C2M in test_one).
    awk -F'\t' '$1=="REQ" && $2!="" && $3!="" {print $2"\t"$3"\t"$4"\t"$5}' "$RECORDS" >> "$REQS"
    sort -u "$REQS" -o "$REQS"                        # webappDir uri hasview hasevent
    extract_menu_targets | sort -u > "$MENUS"         # target file

    # every uri defined anywhere -- XML request-maps + annotation @Request (menu resolution)
    { all_request_uris; awk -F'\t' '$1=="REQ" && $3!="" {print $3}' "$RECORDS"; } | sort -u > "$VALIDNAMES"
    # every view-map name defined anywhere -- XML view-map + annotation @View (for "view/Name" targets)
    { all_view_names; awk -F'\t' '$1=="VIEW" && $3!="" {print $3}' "$RECORDS"; } | sort -u > "$VIEWS"
    cut -f1 "$MENUS" | sort -u > "$TMP/menu_names.txt"

    # Test set: view-rendering requests with no <event> (safe GET), unless --include-events.
    # XML and annotation records are aggregated per webapp+uri: a request renders a view if
    # ANY source says so, and is event-bearing (unsafe) if ANY source says so.
    # Output: ctrl<TAB>uri<TAB>vtype<TAB>from_menu   (vtype inferred from uri suffix)
    awk -F'\t' -v MENUS="$TMP/menu_names.txt" -v REQSF="$REQS" -v EVENTS="$INCLUDE_EVENTS" '
    BEGIN{
        while((getline l < MENUS)>0) menu[l]=1
        while((getline l < REQSF)>0){ n=split(l,a,"\t"); if(a[1]==""||a[2]=="")continue;
            key=a[1] SUBSEP a[2]
            if(a[3]=="1") hv[key]=1
            if(a[4]=="1") he[key]=1
            if(!(key in ctrls)){ ctrls[key]=a[1]; uris[key]=a[2] }
        }
        for(key in ctrls){
            if(hv[key]!=1)continue;                      # must render a view
            if(he[key]==1 && EVENTS!="true")continue;    # skip event requests (unsafe) unless --include-events
            uri=uris[key]; vt="screen";
            if(uri ~ /\.pdf$/) vt="screenfop"; else if(uri ~ /\.csv$/) vt="screencsv";
            else if(uri ~ /\.xml$/) vt="screenxml"; else if(uri ~ /\.js$/) vt="js";
            fm=(uri in menu)?1:0; print ctrls[key]"\t"uri"\t"vt"\t"fm }
    }' < /dev/null | sort -u > "$TESTSET"

    # Broken menu links: normalize target, check it resolves to a known request uri (defined anywhere).
    # A target of the form "view/Name" is the OFBiz view-override convention -- it bypasses the
    # request-map layer and renders the named view-map directly -- so it's valid whenever "Name"
    # matches a view-map name (defined anywhere), not just a request uri.
    awk -F'\t' -v VALIDF="$VALIDNAMES" -v VIEWSF="$VIEWS" '
    BEGIN{ while((getline l < VALIDF)>0){ valid[l]=1 }; while((getline l < VIEWSF)>0){ viewvalid[l]=1 } }
    {
        target=$1; file=$2;
        if(target ~ /\$\{/) next;                 # dynamic
        if(target ~ /^(https?:|javascript:|mailto:|tel:)/) next;   # external/scheme link
        t=target; sub(/#.*/,"",t); sub(/\?.*/,"",t);
        if(t ~ /\/control\//){ sub(/.*\/control\//,"",t) }
        else if(t ~ /^\//){ next }                # non-control absolute/static link
        if(t=="") next;
        if(t ~ /^view\//){ vn=t; sub(/^view\//,"",vn); if(vn in viewvalid) next }
        if(!(t in valid)) print target"  (in "file")"
    }' "$MENUS" > "$TMP/broken.txt"
    if [ -s "$TMP/broken.txt" ]; then mapfile -t BROKEN_MENU < "$TMP/broken.txt"; fi
}

# ---- Per-request test with log correlation ----
JAR="$TMP/cookies.txt"
test_one(){
    local ctrl="$1" uri="$2" vtype="$3" from_menu="$4"
    local mount="${C2M[$ctrl]:-$ctrl}"
    local url="${BASE_URL}/${mount}/control/${uri}"
    local key="${ctrl}/${uri}"

    if [ -n "${RESUME_DONE[$key]:-}" ]; then
        echo "  $(gry SKIP) /${mount}/control/${uri} (resumed, already tested)"
        return
    fi

    # Skip heavy binary exports (PDF/CSV/XML/JS): param-required, not HTML screens, and slow enough
    # to bog a serial sweep. Recorded as SKIP, not FAIL.
    if [ "$SKIP_BINARY" = "true" ]; then
        case "$vtype" in screenfop|screencsv|screenxml|js)
            [ -n "${CSV_FILE:-}" ] && echo "${ctrl},${mount},${uri},${vtype},${from_menu},,,SKIP,0,\"binary export skipped\"" >> "$CSV_FILE"
            return;;
        esac
    fi
    local body hdr; body="$TMP/body"; hdr="$TMP/hdr"

    local off=0; [ -f "$LOG_FILE" ] && off=$(wc -c < "$LOG_FILE" | tr -d ' ')
    local http; http=$(curl -s -k -L "${DEV_AUTH_HDR[@]}" -c "$JAR" -b "$JAR" -D "$hdr" -o "$body" \
        -w '%{http_code}' --max-time "$TIMEOUT" "$url" 2>/dev/null) || http="000"
    if [ "$http" = "000" ]; then   # retry once (cold-start compilation can exceed timeout)
        http=$(curl -s -k -L "${DEV_AUTH_HDR[@]}" -c "$JAR" -b "$JAR" -D "$hdr" -o "$body" \
            -w '%{http_code}' --max-time "$RETRY_TIMEOUT" "$url" 2>/dev/null) || http="000"
    fi

    # Circuit breaker: after a run of consecutive connection failures, confirm with a health
    # check before assuming the whole server (not just this request) is down.
    if [ "$http" = "000" ]; then
        CONSEC_000=$((CONSEC_000+1))
        if [ "$CONSEC_000" -ge "$MAX_CONSEC_000" ]; then
            local health; health=$(curl -s -k -o /dev/null -w '%{http_code}' --max-time 10 "$HEALTH_URL" 2>/dev/null); [ -z "$health" ] && health="000"
            [ "$health" = "000" ] && SERVER_DOWN=true
        fi
    else
        CONSEC_000=0
    fi
    sleep "$LOG_SETTLE"

    # per-request log slice, attributed to this request via the 8443 connector thread
    local slice="$TMP/slice" rel="$TMP/rel" rel_clean="$TMP/rel_clean"; : > "$slice"; : > "$rel"; : > "$rel_clean"
    if [ -f "$LOG_FILE" ]; then tail -c "+$((off+1))" "$LOG_FILE" > "$slice" 2>/dev/null || true; fi
    # keep only lines from the web request threads (exclude JobQueue/startup/background noise)
    grep -E 'nio-8443-exec|http-nio-8443|jsse-nio-8443' "$slice" > "$rel" 2>/dev/null || true

    # exec thread handling this request: field 3 of "[LEVEL]|date |thread |logger|L| msg"
    local thread=""
    [ -s "$rel" ] && thread=$(awk -F'|' 'NR==1{print $3}' "$rel" | sed -e 's/^[ \t]*//' -e 's/[ \t]*$//')

    # Transaction-leak log lines are a known false positive for FAIL: strip them, and their raw
    # stack-continuation lines (which don't start with "[LEVEL]"), before counting real errors.
    local txleak_count; txleak_count=$(grep -cE "$TXLEAK_RE" "$rel" 2>/dev/null); txleak_count=${txleak_count:-0}
    # NOTE: read TXLEAK_RE via ENVIRON[], not `-v re=...` -- awk's -v assignment runs string-escape
    # processing on the value first, which silently unescapes \[ \. \] and corrupts this regex into
    # a bracket character class (matches almost any line, dropping real [ERROR] lines with it).
    awk '
        BEGIN { re = ENVIRON["TXLEAK_RE"] }
        /^\[[A-Za-z]+\]/ { drop = ($0 ~ re) ? 1 : 0 }
        !drop { print }
    ' "$rel" > "$rel_clean" 2>/dev/null || cp "$rel" "$rel_clean"

    local log_err; log_err=$(grep -cE '^\[ERROR\]' "$rel_clean" 2>/dev/null); log_err=${log_err:-0}
    local log_trace; log_trace=$(grep -cE 'Exception:|ScreenRenderException|WidgetRenderException|TemplateException|GenericServiceException|GenericEntityException' "$rel_clean" 2>/dev/null); log_trace=${log_trace:-0}
    local logsnip=""
    if [ "$log_err" -gt 0 ]; then
        logsnip=$(grep -E '^\[ERROR\]' "$rel_clean" 2>/dev/null | head -3 | cut -c1-400 | paste -sd'~' -)
    fi

    # txleak attribution: blame the PREVIOUS request handled by this same thread, plus the first
    # non-transaction/non-controller stack frame present in the raw (pre-strip) slice, if any.
    local txleak_field="0"
    if [ "$txleak_count" -gt 0 ]; then
        local leaked_by="${LAST_URI_BY_THREAD[$thread]:-}"
        local frame_raw frame=""
        # plain -E (no -P/\K): grep -P needs a UTF-8 locale and fails under this script's LC_ALL=C
        frame_raw=$(grep -oE 'at [A-Za-z0-9_.$<>]+\([^():]*:[0-9]+\)' "$rel" 2>/dev/null \
            | sed -E 's/^at //' \
            | grep -vE 'org\.ofbiz\.entity\.transaction|org\.ofbiz\.webapp\.control' | head -1)
        [ -n "$frame_raw" ] && frame=$(echo "$frame_raw" | sed -E 's/^([A-Za-z0-9_.$<>]+)\([^:]*:([0-9]+)\)$/\1:\2/')
        txleak_field="${txleak_count};leaked-by:${leaked_by}"
        [ -n "$frame" ] && txleak_field="${txleak_field};frame:${frame}"
    fi

    local size=0; [ -f "$body" ] && size=$(wc -c < "$body" | tr -d ' ')
    local ctype; ctype=$(grep -i '^Content-Type:' "$hdr" 2>/dev/null | tail -1 | tr -d '\r' | sed 's/^[Cc]ontent-[Tt]ype:[[:space:]]*//')

    local status="PASS" reason=""
    local is_html=true
    case "$ctype" in *text/html*|"") is_html=true;; *) is_html=false;; esac

    if [ "$http" = "000" ]; then
        status=FAIL; reason="connection/timeout"
    elif [ "$http" -ge 500 ] 2>/dev/null; then
        status=FAIL; reason="HTTP ${http}"
    elif [ "$http" = "404" ]; then
        status=FAIL; reason="HTTP 404 - request not found (missing/unconverted)"
    elif [ "$is_html" = true ]; then
        if grep -q 'has-scipio-errormsg="true"' "$body" 2>/dev/null; then
            local etext; etext=$(grep -ioE '(is required|cannot be null|may not be null|required field|not found|no [a-z]+ found|permission|not allowed|invalid)[^<]{0,60}' "$body" 2>/dev/null | head -1)
            if echo "$etext" | grep -qiE 'is required|cannot be null|not found' \
                && echo "$uri" | grep -qE '(^|/)(View|Edit|Detail|Show)[A-Z]' \
                && [ "$log_err" -eq 0 ]; then
                status=PARAM_REQ; reason="likely missing required parameter${etext:+: $etext}"
            else
                status=FAIL; reason="error message on page${etext:+: $etext}"
            fi
        elif grep -qE 'id="error"|PageTitleError|CommonFollowingErrorsOccurred|CommonErrorOccurredContactSupport' "$body" 2>/dev/null; then
            status=FAIL; reason="controller error page"
        elif grep -qiE 'FreeMarker template error|freemarker\.core\.|freemarker\.template\.|InvalidReferenceException|Error rendering|WidgetRenderException|ScreenRenderException|Could not find|No definition found|java\.[a-z.]*Exception|org\.ofbiz\.[a-z.]*Exception|NullPointerException' "$body" 2>/dev/null; then
            local esnip; esnip=$(grep -ioE '(FreeMarker template error|Error rendering[^<]{0,80}|[a-z.]+Exception[^<]{0,80}|Could not find[^<]{0,80}|No definition found[^<]{0,80})' "$body" 2>/dev/null | head -1)
            status=FAIL; reason="render error in body${esnip:+: $esnip}"
        elif [ "$size" -lt 1500 ]; then
            status=WARN; reason="suspiciously small html (${size}B)"
        else
            local cm; cm=$(grep -cE 'screenlet|<form |<table|data-table|grid-row|field-row|widget-content|<input |<h1|<h2' "$body" 2>/dev/null); cm=${cm:-0}
            [ "$cm" -eq 0 ] && { status=WARN; reason="no content markers (decorator-only? ${size}B)"; }
        fi
    else
        # binary view (pdf/csv/xml/etc): a render failure under RETHROW returns html error
        if grep -qiE 'has-scipio-errormsg="true"|id="error"|Exception|Error rendering' "$body" 2>/dev/null; then
            status=FAIL; reason="error body for binary view (${ctype})"
        elif [ "$size" -lt 64 ]; then
            status=WARN; reason="empty binary output (${size}B, ${ctype})"
        fi
    fi

    # Authoritative: any [ERROR]/trace attributed to this request = failure (OR with body verdict)
    if [ "$log_err" -gt 0 ] || [ "$log_trace" -gt 0 ]; then
        if [ "$status" != "FAIL" ]; then status=FAIL; reason="log error during request${logsnip:+: $logsnip}";
        else reason="$reason | log:${log_err}E/${log_trace}T${logsnip:+ $logsnip}"; fi
    fi

    # Reconcile against expected-failures
    if [ "$status" = "FAIL" ] && [ -n "${EXPECTED_FAIL[$key]+x}" ]; then
        status=EXPECTED; reason="$reason [expected: ${EXPECTED_FAIL[$key]}]"
    fi

    local tag=""; [ "$from_menu" = "1" ] && tag=" [menu]"
    case "$status" in
        PASS) PASS=$((PASS+1));;
        EXPECTED) EXPECTED=$((EXPECTED+1)); echo "  $(gry EXPD) /${mount}/control/${uri}${tag} ${reason}";;
        PARAM_REQ) PARAM_REQ=$((PARAM_REQ+1)); echo "  $(ylw PRM ) /${mount}/control/${uri}${tag}  ${reason}";;
        WARN) WARN=$((WARN+1)); WARNINGS+=("/${mount}/control/${uri}${tag} => ${reason}");
              echo "  $(ylw WARN) /${mount}/control/${uri}${tag}  ${reason}";;
        FAIL) FAIL=$((FAIL+1)); FAILURES+=("/${mount}/control/${uri}${tag} [${http}] => ${reason}");
              echo "  $(red FAIL) /${mount}/control/${uri}${tag} [${http}]  ${reason}";;
    esac
    # every request updates its thread's last-seen URI, leaked or not (txleak attribution)
    [ -n "$thread" ] && LAST_URI_BY_THREAD["$thread"]="/${mount}/control/${uri}"
    [ -n "${CSV_FILE:-}" ] && echo "${ctrl},${mount},${uri},${vtype},${from_menu},${http},${size},${status},${txleak_field},\"${reason//\"/\"\"}\"" >> "$CSV_FILE"
}

# ---- args ----
while [ $# -gt 0 ]; do case "$1" in
    --list) DO_LIST=true;;
    --include-events) INCLUDE_EVENTS=true;;
    --timeout) shift; TIMEOUT="$1";;
    --fast) TIMEOUT=15; RETRY_TIMEOUT=30;;
    --resume) shift; RESUME_CSV="$1";;
    --controllers) shift; IFS=',' read -ra _cf <<< "$1"; FILTER+=("${_cf[@]}");;
    --exclude) shift; IFS=',' read -ra EXCLUDE_ARR <<< "$1";;
    -h|--help) grep -E '^#( |$)' "$0" | sed 's/^# \{0,1\}//'; exit 0;;
    -*) echo "unknown option: $1"; exit 1;;
    *) FILTER+=("$1");;
esac; shift; done

load_expected
[ -n "$RESUME_CSV" ] && load_resume "$RESUME_CSV"
echo "=========================================="
echo " Scipio Hardened Screen/Menu Verification"
echo " Base: ${BASE_URL}   Log: ${LOG_FILE}"
echo "=========================================="
[ -n "$RESUME_CSV" ] && echo " Resuming: ${CSV_FILE} (${#RESUME_DONE[@]} already-tested keys will be skipped)"
echo "Extracting controller + menu definitions..."
build_universe

TOTAL_REQS=$(wc -l < "$REQS" | tr -d ' ')
TOTAL_MENU=$(cut -f1 "$MENUS" | sort -u | wc -l | tr -d ' ')
TOTAL_TEST=$(wc -l < "$TESTSET" | tr -d ' ')
echo "  xml-requests=${TOTAL_REQS}  menu-targets=${TOTAL_MENU}  test-set=${TOTAL_TEST}  broken-menu-links=${#BROKEN_MENU[@]}"

if [ "$DO_LIST" = true ]; then
    echo ""; echo "Per-controller test counts:"
    cut -f1 "$TESTSET" | sort | uniq -c | sort -rn | while read -r n c; do
        printf "  %-14s /%-12s %4d\n" "$c" "${C2M[$c]:-$c}" "$n"
    done
    echo ""; echo "Broken menu links (${#BROKEN_MENU[@]}):"
    printf '  %s\n' "${BROKEN_MENU[@]:-}(none)"
    exit 0
fi

# Connectivity
echo ""; echo "Checking connectivity..."
# a truncated body (curl exit 18) still carries a status code: judge the server by the code, not by the exit
cc=$(curl -s -k -L "${DEV_AUTH_HDR[@]}" -o /dev/null -w '%{http_code}' --max-time 10 "${BASE_URL}/admin/control/main" 2>/dev/null); [ -z "$cc" ] && cc=000
[ "$cc" = "000" ] && { echo "$(red ERROR): cannot reach ${BASE_URL} -- is the server running?"; exit 2; }
echo "Reachable (HTTP ${cc}). Warming up..."
curl -s -k -L "${DEV_AUTH_HDR[@]}" -c "$JAR" -b "$JAR" -o /dev/null --max-time 15 "${BASE_URL}/admin/control/main" >/dev/null 2>&1 || true

if [ -z "$CSV_FILE" ]; then
    TS=$(date +%Y%m%d-%H%M%S)
    mkdir -p "$REPORT_DIR" 2>/dev/null || true
    CSV_FILE="${REPORT_DIR}/verify-screens-${TS}.csv"
    echo "controller,mount,uri,vtype,from_menu,http,size,status,txleak,reason" > "$CSV_FILE"
fi

# Report broken menu links up front (static failures)
if [ "${#BROKEN_MENU[@]}" -gt 0 ]; then
    echo ""; echo "$(red 'BROKEN MENU LINKS') (target resolves to no @Request/@View): ${#BROKEN_MENU[@]}"
    printf '  %s %s\n' "$(red x)" "${BROKEN_MENU[@]}"
fi

# Run per controller
CTRLS=$(cut -f1 "$TESTSET" | sort -u)
for ctrl in $CTRLS; do
    if [ "${#FILTER[@]}" -gt 0 ]; then
        local_match=false; for fw in "${FILTER[@]}"; do [ "$fw" = "$ctrl" ] && local_match=true; done
        [ "$local_match" = false ] && continue
    fi
    mapfile -t CTRL_ROWS < <(awk -F'\t' -v c="$ctrl" '$1==c' "$TESTSET")
    n="${#CTRL_ROWS[@]}"
    echo ""; echo "--- ${ctrl} (/${C2M[$ctrl]:-$ctrl}, ${n} urls) ---"
    ctrl_start=$(date +%s)
    curl -s -k -L "${DEV_AUTH_HDR[@]}" -c "$JAR" -b "$JAR" -o /dev/null --max-time "$WARMUP_TIMEOUT" "${BASE_URL}/${C2M[$ctrl]:-$ctrl}/control/main" >/dev/null 2>&1 || true  # warm up webapp

    # Structural note: this MUST be a plain loop fed via process substitution, not a pipe into
    # `while read` -- a pipe forks a subshell, and every counter/flag test_one touches (PASS,
    # FAIL, CONSEC_000, SERVER_DOWN, LAST_URI_BY_THREAD, ...) would then die with that subshell.
    for ((i=0; i<n; i++)); do
        IFS=$'\t' read -r c u vt fm <<< "${CTRL_ROWS[$i]}"
        test_one "$c" "$u" "$vt" "$fm"
        if [ "$SERVER_DOWN" = true ]; then
            # Health check failed too: stop curling and record the untested remainder of this
            # controller as SERVER_DOWN, so --resume knows exactly which rows to retest.
            for ((j=i+1; j<n; j++)); do
                IFS=$'\t' read -r rc ru rvt rfm <<< "${CTRL_ROWS[$j]}"
                [ -n "${RESUME_DONE[${rc}/${ru}]:-}" ] && continue
                echo "${rc},${C2M[$rc]:-$rc},${ru},${rvt},${rfm},000,0,SERVER_DOWN,0,\"aborted: health-check failed\"" >> "$CSV_FILE"
            done
            break
        fi
    done

    # Per-controller summary (field-based over the CSV, so a --resume run's history counts too)
    read -r c_pass c_fail c_warn c_param c_txleak < <(awk -F',' -v c="$ctrl" '
        $1==c {
            if($8=="PASS") p++; else if($8=="FAIL") f++; else if($8=="WARN") w++; else if($8=="PARAM_REQ") pr++
            if($9!="0" && $9!="") tl++
        }
        END{ printf "%d %d %d %d %d", p+0, f+0, w+0, pr+0, tl+0 }
    ' "$CSV_FILE")
    ctrl_elapsed=$(( $(date +%s) - ctrl_start ))
    echo "  $(gry SUMMARY) ${ctrl}: pass=${c_pass} FAIL=${c_fail} warn=${c_warn} param_req=${c_param} txleak=${c_txleak} elapsed=${ctrl_elapsed}s"

    [ "$SERVER_DOWN" = true ] && break
done

# Re-tally from the CSV (field-based on the status column, field 8). This is the source of
# truth rather than in-loop counters: a --resume run's CSV also carries rows from an earlier
# process, and those must be included in the final totals.
PASS=$(awk -F',' '$8=="PASS"{c++} END{print c+0}' "$CSV_FILE")
FAIL=$(awk -F',' '$8=="FAIL"{c++} END{print c+0}' "$CSV_FILE")
WARN=$(awk -F',' '$8=="WARN"{c++} END{print c+0}' "$CSV_FILE")
EXPECTED=$(awk -F',' '$8=="EXPECTED"{c++} END{print c+0}' "$CSV_FILE")
PARAM_REQ=$(awk -F',' '$8=="PARAM_REQ"{c++} END{print c+0}' "$CSV_FILE")
SERVER_DOWN_ROWS=$(awk -F',' '$8=="SERVER_DOWN"{c++} END{print c+0}' "$CSV_FILE")

echo ""; echo "=========================================="
echo " Results: $(grn "${PASS} pass"), $(red "${FAIL} FAIL"), $(ylw "${WARN} warn"), $(gry "${EXPECTED} expected"), $(ylw "${PARAM_REQ} param_req"), $(red "${SERVER_DOWN_ROWS} server_down")"
echo " Broken menu links: ${#BROKEN_MENU[@]}"
echo " CSV: ${CSV_FILE}"
echo "=========================================="

if [ "$SERVER_DOWN" = true ]; then
    echo ""; echo "$(red 'SERVER DOWN'): ${MAX_CONSEC_000} consecutive connection failures, health check also failed."
    echo "Remaining URIs were recorded as SERVER_DOWN. Restart the server, then resume with:"
    echo "  bash tools/verify-screens.sh --resume ${CSV_FILE}"
    exit 3
fi

REAL_FAIL=$((FAIL + ${#BROKEN_MENU[@]}))
if [ "$FAIL" -gt 0 ]; then
    echo ""; echo "Failures (${FAIL}) -- see CSV for full list. First 60:"
    awk -F',' '$8=="FAIL"' "$CSV_FILE" | head -60 | while IFS=, read -r c m u vt fm http sz st txl rest; do
        echo "  $(red x) /${m}/control/${u}  [${http}]"
    done
fi
[ "$REAL_FAIL" -gt 0 ] && exit 1
exit 0
