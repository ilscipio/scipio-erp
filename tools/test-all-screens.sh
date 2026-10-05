#!/bin/bash
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
# Comprehensive screen/view test for all Scipio ERP webapps.
# Auto-extracts ALL @View definitions from ControllerDef Java files
# and tests each one via curl, with log monitoring and report generation.
#
# Usage:
#   bash tools/test-all-screens.sh [options] [webapp...]
#   bash tools/test-all-screens.sh                # test all webapps
#   bash tools/test-all-screens.sh accounting     # test only accounting
#   bash tools/test-all-screens.sh --list         # list webapps + view counts
#   bash tools/test-all-screens.sh --skip-fop     # skip PDF views
#
# Requires: curl, gawk, server running on https://localhost:8443
# Uses dev auth bypass (X-Dev-Auth-Key header)

set -euo pipefail

# ---- Configuration ----
BASE_URL="${SCIPIO_BASE_URL:-https://localhost:8443}"
AUTH_KEY="${SCIPIO_DEV_AUTH_KEY:-scipio-dev-test-key-2026}"
TIMEOUT=30
SKIP_FOP=false
SKIP_CSV=false
CHECK_LOG=true
REPORT_DIR="runtime/logs"
FILTER_WEBAPPS=()
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
EXPECTED_FILE="$SCRIPT_DIR/screen-test-expected.conf"

# ---- Counters ----
PASS=0
FAIL=0
WARN=0
SKIP=0
EXPECTED=0
ERRORS=()
WARNINGS=()

# ---- Colors ----
red()    { printf '\033[0;31m%s\033[0m' "$1"; }
green()  { printf '\033[0;32m%s\033[0m' "$1"; }
yellow() { printf '\033[0;33m%s\033[0m' "$1"; }
cyan()   { printf '\033[0;36m%s\033[0m' "$1"; }
gray()   { printf '\033[0;90m%s\033[0m' "$1"; }

# ---- Controller → Mount-point mapping ----
declare -A CONTROLLER_TO_MOUNT
CONTROLLER_TO_MOUNT[webtools]="admin"
CONTROLLER_TO_MOUNT[accounting]="accounting"
CONTROLLER_TO_MOUNT[ap]="ap"
CONTROLLER_TO_MOUNT[ar]="ar"
CONTROLLER_TO_MOUNT[cms]="cms"
CONTROLLER_TO_MOUNT[backendsite]="backendsite"
CONTROLLER_TO_MOUNT[website]="website"
CONTROLLER_TO_MOUNT[ofbizsetup]="ofbizsetup"
CONTROLLER_TO_MOUNT[content]="content"
CONTROLLER_TO_MOUNT[humanres]="humanres"
CONTROLLER_TO_MOUNT[manufacturing]="manufacturing"
CONTROLLER_TO_MOUNT[marketing]="marketing"
CONTROLLER_TO_MOUNT[sfa]="sfa"
CONTROLLER_TO_MOUNT[ordermgr]="ordermgr"
CONTROLLER_TO_MOUNT[partymgr]="partymgr"
CONTROLLER_TO_MOUNT[catalog]="catalog"
CONTROLLER_TO_MOUNT[facility]="facility"
CONTROLLER_TO_MOUNT[shop]="shop"
CONTROLLER_TO_MOUNT[workeffort]="workeffort"
CONTROLLER_TO_MOUNT[setup]="setup"

# ---- Known expected failures (loaded from conf file) ----
declare -A EXPECTED_FAILURES  # key: "controller/viewName" → value: "category|reason"

load_expected_failures() {
    if [ ! -f "$EXPECTED_FILE" ]; then
        return
    fi
    while IFS= read -r line; do
        line="${line%%#*}"          # strip comments
        line="$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
        [ -z "$line" ] && continue
        local key reason
        key="$(echo "$line" | cut -d'|' -f1)"
        reason="$(echo "$line" | cut -d'|' -f2-)"
        EXPECTED_FAILURES["$key"]="$reason"
    done < "$EXPECTED_FILE"
}

# ---- View extraction from ControllerDef Java files ----
# Outputs: controller|viewName|viewType (sorted by controller then name)
extract_views() {
    find "$PROJECT_ROOT/framework" "$PROJECT_ROOT/applications" \
        -path "*/controller/*ControllerDef*.java" \
        -not -path "*/build/*" \
        2>/dev/null | while read -r javafile; do
        awk '
/@com\.ilscipio\.scipio\.ce\.webapp\.control\.def\.View\(/ {
    in_view=1; name=""; vtype="screen"; ctrl=""
}
in_view && /name = "/ {
    match($0, /name = "([^"]+)"/, m); name=m[1]
}
in_view && /type = "/ {
    match($0, /type = "([^"]+)"/, m); vtype=m[1]
}
in_view && /controller = "/ {
    match($0, /controller = "([^"]+)"/, m); ctrl=m[1]
}
in_view && /\)$/ {
    if (name != "" && ctrl != "") print ctrl "|" name "|" vtype
    in_view=0
}
' "$javafile"
    done | sort -t'|' -k1,1 -k2,2 -u
}

# ---- Test a single URI ----
test_uri() {
    local controller="$1" view="$2" vtype="$3" mount="$4"
    local url="${BASE_URL}/${mount}/control/${view}"
    local http_code tmpfile
    local lookup_key="${controller}/${view}"

    tmpfile=$(mktemp)
    http_code=$(curl -s -k -L \
        -H "X-Dev-Auth-Key: ${AUTH_KEY}" \
        -o "$tmpfile" \
        -w "%{http_code}" \
        --max-time "$TIMEOUT" \
        "$url" 2>/dev/null) || http_code="000"

    local size=0
    if [ -f "$tmpfile" ]; then
        size=$(wc -c < "$tmpfile" | tr -d ' ')
    fi

    local status="PASS"
    local reason=""

    # Check if this is an expected failure
    local is_expected=false
    if [ -n "${EXPECTED_FAILURES[$lookup_key]+x}" ]; then
        is_expected=true
    fi

    if [ "$http_code" = "000" ]; then
        status="FAIL"
        reason="timeout/connection"
    elif [ "$http_code" -ge 500 ] 2>/dev/null; then
        status="FAIL"
        reason="HTTP ${http_code}"
        if [ -f "$tmpfile" ]; then
            local err_snippet
            err_snippet=$(grep -ioP '(exception|error|caused by)[^<]{0,120}' "$tmpfile" 2>/dev/null | head -1) || true
            [ -n "${err_snippet:-}" ] && reason="${reason}: ${err_snippet}"
        fi
    elif [ "$http_code" = "404" ]; then
        status="FAIL"
        reason="HTTP 404 - view not found"
    elif [ "$http_code" -ge 200 ] && [ "$http_code" -lt 400 ] 2>/dev/null; then
        # For binary output types, only check HTTP status
        if [ "$vtype" = "screenfop" ] || [ "$vtype" = "screencsv" ] || [ "$vtype" = "screenxml" ] || [ "$vtype" = "simplecontent" ]; then
            status="PASS"
        elif [ -f "$tmpfile" ] && grep -qiP 'java\.\w+Exception|javax\.\w+Exception|org\.ofbiz\.\w+Exception|com\.ilscipio\.\w+Exception|NullPointerException|ClassCastException|IllegalArgumentException|ScreenRenderException|TemplateException|freemarker\.\w+Exception' "$tmpfile" 2>/dev/null; then
            status="FAIL"
            reason="Exception in body"
            local err_snippet
            err_snippet=$(grep -ioP '(java|javax|org\.ofbiz|com\.ilscipio|freemarker)\.\w+Exception[^<]{0,100}' "$tmpfile" 2>/dev/null | head -1) || true
            [ -n "${err_snippet:-}" ] && reason="${reason}: ${err_snippet}"
        fi
    else
        status="WARN"
        reason="HTTP ${http_code}"
    fi

    rm -f "$tmpfile"

    # Reclassify if expected
    if [ "$is_expected" = true ] && [ "$status" = "FAIL" ]; then
        status="EXPECTED"
        reason="${reason} [expected: ${EXPECTED_FAILURES[$lookup_key]}]"
    fi

    # Output and count
    local line
    case "$status" in
        PASS)
            line="$(printf "  $(green PASS) %-50s %-6s %sB  %s" "/${mount}/control/${view}" "${http_code}" "${size}" "")"
            PASS=$((PASS+1))
            ;;
        FAIL)
            line="$(printf "  $(red FAIL) %-50s %s" "/${mount}/control/${view}" "${reason}")"
            FAIL=$((FAIL+1))
            ERRORS+=("/${mount}/control/${view} => ${reason}")
            ;;
        WARN)
            line="$(printf "  $(yellow WARN) %-50s %s" "/${mount}/control/${view}" "${reason}")"
            WARN=$((WARN+1))
            WARNINGS+=("/${mount}/control/${view} => ${reason}")
            ;;
        EXPECTED)
            line="$(printf "  $(gray EXPD) %-50s %s" "/${mount}/control/${view}" "${reason}")"
            EXPECTED=$((EXPECTED+1))
            ;;
        SKIP)
            line="$(printf "  $(yellow SKIP) %-50s %s" "/${mount}/control/${view}" "${reason}")"
            SKIP=$((SKIP+1))
            ;;
    esac
    echo "$line"

    # Write to report file
    if [ -n "${REPORT_FILE:-}" ]; then
        echo "$line" | sed 's/\x1b\[[0-9;]*m//g' >> "$REPORT_FILE"
    fi
    if [ -n "${CSV_FILE:-}" ]; then
        echo "${controller},${mount},${view},${vtype},${http_code},${status},\"${reason}\",${size}" >> "$CSV_FILE"
    fi
}

# ---- Log monitoring ----
snapshot_log() {
    local logfile="$PROJECT_ROOT/runtime/logs/ofbiz.log"
    LOG_SNAPSHOT_FILE="$logfile"
    if [ -f "$logfile" ]; then
        LOG_SNAPSHOT_SIZE=$(wc -c < "$logfile" | tr -d ' ')
    else
        LOG_SNAPSHOT_SIZE=0
    fi
}

check_log_errors() {
    if [ "$CHECK_LOG" != true ]; then
        return
    fi
    local logfile="$LOG_SNAPSHOT_FILE"
    if [ ! -f "$logfile" ]; then
        echo "  $(yellow 'Log file not found'): $logfile"
        return
    fi

    local current_size
    current_size=$(wc -c < "$logfile" | tr -d ' ')
    if [ "$current_size" -le "$LOG_SNAPSHOT_SIZE" ]; then
        echo "  $(green 'No new log entries during test run')"
        return
    fi

    local new_bytes=$((current_size - LOG_SNAPSHOT_SIZE))
    local error_count warn_count
    # Scipio log format: [LEVEL]|timestamp|thread|class|...
    error_count=$(tail -c "$new_bytes" "$logfile" 2>/dev/null | grep -c '^\[ERROR\]' 2>/dev/null) || error_count=0
    warn_count=$(tail -c "$new_bytes" "$logfile" 2>/dev/null | grep -c '^\[WARN\]' 2>/dev/null) || warn_count=0
    exception_count=$(tail -c "$new_bytes" "$logfile" 2>/dev/null | grep -cE 'Exception|Error.*at [a-z]' 2>/dev/null) || exception_count=0

    echo "  New log entries: ${new_bytes} bytes"
    echo "  [ERROR] entries: ${error_count}"
    echo "  [WARN]  entries: ${warn_count}"
    echo "  Exception traces: ${exception_count}"

    if [ "$error_count" -gt 0 ]; then
        echo ""
        echo "  Error details (first 50):"
        tail -c "$new_bytes" "$logfile" 2>/dev/null | grep '^\[ERROR\]' 2>/dev/null | head -50 | while IFS= read -r errline; do
            echo "    $(red '!') ${errline:0:200}"
        done

        if [ -n "${REPORT_FILE:-}" ]; then
            echo "" >> "$REPORT_FILE"
            echo "=== LOG ERRORS DURING TEST RUN ===" >> "$REPORT_FILE"
            tail -c "$new_bytes" "$logfile" 2>/dev/null | grep '^\[ERROR\]' 2>/dev/null >> "$REPORT_FILE" || true
        fi
    fi
}

# ---- CLI argument parsing ----
parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --list)
                do_list
                exit 0
                ;;
            --skip-fop)
                SKIP_FOP=true
                ;;
            --skip-csv)
                SKIP_CSV=true
                ;;
            --no-check-log)
                CHECK_LOG=false
                ;;
            --check-log)
                CHECK_LOG=true
                ;;
            --timeout)
                shift
                TIMEOUT="$1"
                ;;
            --report-dir)
                shift
                REPORT_DIR="$1"
                ;;
            --help|-h)
                usage
                exit 0
                ;;
            -*)
                echo "Unknown option: $1"
                usage
                exit 1
                ;;
            *)
                FILTER_WEBAPPS+=("$1")
                ;;
        esac
        shift
    done
}

usage() {
    cat <<'USAGE'
Usage: bash tools/test-all-screens.sh [options] [webapp...]

Options:
  --list              List all webapps and auto-extracted view counts
  --skip-fop          Skip screenfop (PDF) views
  --skip-csv          Skip screencsv views
  --check-log         Check ofbiz.log for errors after tests (default: on)
  --no-check-log      Disable log checking
  --timeout SECS      Per-request curl timeout (default: 30)
  --report-dir DIR    Report output directory (default: runtime/logs)
  -h, --help          Show this help

Examples:
  bash tools/test-all-screens.sh                    # test all webapps
  bash tools/test-all-screens.sh accounting cms      # test specific webapps
  bash tools/test-all-screens.sh --skip-fop          # skip PDF views
USAGE
}

do_list() {
    echo "Auto-extracting views from ControllerDef files..."
    echo ""
    echo "Available webapps:"
    local views
    views="$(extract_views)"
    local total=0
    for ctrl in $(echo "$views" | cut -d'|' -f1 | sort -u); do
        local mount="${CONTROLLER_TO_MOUNT[$ctrl]:-$ctrl}"
        local count
        count=$(echo "$views" | grep -c "^${ctrl}|" || true)
        local screen_count fop_count csv_count other_count
        screen_count=$(echo "$views" | grep "^${ctrl}|" | grep -cE '\|screen$|\|ftl$' || true)
        fop_count=$(echo "$views" | grep "^${ctrl}|" | grep -c '|screenfop$' || true)
        csv_count=$(echo "$views" | grep "^${ctrl}|" | grep -c '|screencsv$' || true)
        other_count=$((count - screen_count - fop_count - csv_count))
        printf "  %-20s /%s  %3d views (%d screen, %d fop, %d csv" "$ctrl" "$mount" "$count" "$screen_count" "$fop_count" "$csv_count"
        [ "$other_count" -gt 0 ] && printf ", %d other" "$other_count"
        printf ")\n"
        total=$((total + count))
    done
    echo ""
    echo "Total: ${total} views across $(echo "$views" | cut -d'|' -f1 | sort -u | wc -l) webapps"
}

# ---- Main ----
parse_args "$@"

echo "=========================================="
echo " Scipio ERP Comprehensive Screen Test"
echo " Base URL: ${BASE_URL}"
echo "=========================================="

# Extract views
echo ""
echo "Extracting views from ControllerDef files..."
ALL_VIEWS="$(extract_views)"
TOTAL_VIEWS=$(echo "$ALL_VIEWS" | wc -l)
echo "Found ${TOTAL_VIEWS} view definitions across $(echo "$ALL_VIEWS" | cut -d'|' -f1 | sort -u | wc -l) webapps"

# Load expected failures
load_expected_failures
expected_count=${#EXPECTED_FAILURES[@]}
if [ "$expected_count" -gt 0 ]; then
    echo "Loaded ${expected_count} expected failure entries from $(basename "$EXPECTED_FILE")"
fi

# Apply filters
if [ "$SKIP_FOP" = true ]; then
    before=$(echo "$ALL_VIEWS" | wc -l)
    ALL_VIEWS="$(echo "$ALL_VIEWS" | grep -v '|screenfop$' || true)"
    after=$(echo "$ALL_VIEWS" | wc -l)
    echo "Skipping screenfop views ($((before - after)) excluded)"
fi
if [ "$SKIP_CSV" = true ]; then
    before=$(echo "$ALL_VIEWS" | wc -l)
    ALL_VIEWS="$(echo "$ALL_VIEWS" | grep -v '|screencsv$' || true)"
    after=$(echo "$ALL_VIEWS" | wc -l)
    echo "Skipping screencsv views ($((before - after)) excluded)"
fi

# Setup report files
TIMESTAMP=$(date +%Y-%m-%d-%H%M%S)
mkdir -p "$REPORT_DIR" 2>/dev/null || true
REPORT_FILE="${REPORT_DIR}/screen-test-${TIMESTAMP}.log"
CSV_FILE="${REPORT_DIR}/screen-test-${TIMESTAMP}.csv"
echo "controller,mount,view,type,http_code,status,reason,size" > "$CSV_FILE"
echo "Scipio ERP Screen Test Report - $(date)" > "$REPORT_FILE"
echo "Base URL: ${BASE_URL}" >> "$REPORT_FILE"
echo "Total views: ${TOTAL_VIEWS}" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"

# Connectivity check
echo ""
echo "Checking server connectivity..."
check_code=$(curl -s -k -L -o /dev/null -w "%{http_code}" --max-time 10 \
    "${BASE_URL}/admin/control/main" -H "X-Dev-Auth-Key: ${AUTH_KEY}" 2>/dev/null) || check_code="000"
if [ "$check_code" = "000" ]; then
    echo "$(red 'ERROR'): Cannot reach ${BASE_URL}. Is the server running?"
    exit 1
fi
echo "Server reachable (HTTP ${check_code}). Starting tests..."

# Snapshot log
if [ "$CHECK_LOG" = true ]; then
    snapshot_log
fi

# Run tests per webapp
echo ""
CONTROLLERS="$(echo "$ALL_VIEWS" | cut -d'|' -f1 | sort -u)"
for ctrl in $CONTROLLERS; do
    # Apply webapp filter
    if [ ${#FILTER_WEBAPPS[@]} -gt 0 ]; then
        match_found=false
        for fw in "${FILTER_WEBAPPS[@]}"; do
            if [ "$fw" = "$ctrl" ]; then
                match_found=true
                break
            fi
        done
        [ "$match_found" = false ] && continue
    fi

    mount="${CONTROLLER_TO_MOUNT[$ctrl]:-$ctrl}"
    webapp_views="$(echo "$ALL_VIEWS" | grep "^${ctrl}|" || true)"
    [ -z "$webapp_views" ] && continue
    count=$(echo "$webapp_views" | wc -l)

    echo "--- ${ctrl} (/${mount}, ${count} views) ---"
    echo "" >> "$REPORT_FILE"
    echo "--- ${ctrl} (/${mount}, ${count} views) ---" >> "$REPORT_FILE"

    while IFS='|' read -r _ view vtype; do
        test_uri "$ctrl" "$view" "$vtype" "$mount"
    done <<< "$webapp_views"

    echo ""
done

# Log check
if [ "$CHECK_LOG" = true ]; then
    echo "=========================================="
    echo " Log Analysis"
    echo "=========================================="
    echo "" >> "$REPORT_FILE"
    echo "=========================================" >> "$REPORT_FILE"
    echo " Log Analysis" >> "$REPORT_FILE"
    echo "=========================================" >> "$REPORT_FILE"
    check_log_errors
    echo ""
fi

# Summary
TOTAL=$((PASS + FAIL + WARN + EXPECTED + SKIP))
echo "=========================================="
echo " Results: $(green "${PASS} passed"), $(red "${FAIL} failed"), $(yellow "${WARN} warn"), $(gray "${EXPECTED} expected"), $(yellow "${SKIP} skipped") / ${TOTAL} total"
echo "=========================================="
echo "" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"
echo " Results: ${PASS} passed, ${FAIL} failed, ${WARN} warn, ${EXPECTED} expected, ${SKIP} skipped / ${TOTAL} total" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"

if [ ${#ERRORS[@]} -gt 0 ]; then
    echo ""
    echo "Failures (${#ERRORS[@]}):"
    echo "" >> "$REPORT_FILE"
    echo "Failures:" >> "$REPORT_FILE"
    for err in "${ERRORS[@]}"; do
        echo "  $(red 'x') $err"
        echo "  x $err" >> "$REPORT_FILE"
    done
fi

if [ ${#WARNINGS[@]} -gt 0 ]; then
    echo ""
    echo "Warnings (${#WARNINGS[@]}):"
    for w in "${WARNINGS[@]}"; do
        echo "  $(yellow '!') $w"
    done
fi

echo ""
echo "Report: ${REPORT_FILE}"
echo "CSV:    ${CSV_FILE}"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
exit 0
