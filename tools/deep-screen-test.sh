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
# Deep screen content verification - checks for error screens, empty pages, and missing content.
# Run AFTER test-all-screens.sh passes HTTP checks to find rendering failures.
#
# Checks:
#   1. Application error screens (page-error, has-scipio-errormsg="true")
#   2. Empty main content (no screenlets, forms, tables, or grid content)
#   3. FreeMarker template errors in body

set -euo pipefail

BASE_URL="${SCIPIO_BASE_URL:-https://localhost:8443}"
AUTH_KEY="${SCIPIO_DEV_AUTH_KEY:-scipio-dev-test-key-2026}"
TIMEOUT=15
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
FILTER_WEBAPPS=("${@}")

red()    { printf '\033[0;31m%s\033[0m' "$1"; }
green()  { printf '\033[0;32m%s\033[0m' "$1"; }
yellow() { printf '\033[0;33m%s\033[0m' "$1"; }

PASS=0
FAIL=0
WARN=0
SKIP=0
ERRORS=()
WARNINGS=()

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

# Skip views that are non-HTML (FOP/CSV/XML/binary) or known non-renderable
SKIP_TYPES="screenfop|screencsv|screenxml|simplecontent"

extract_views() {
    find "$PROJECT_ROOT/framework" "$PROJECT_ROOT/applications" \
        -path "*/controller/*ControllerDef*.java" \
        -not -path "*/build/*" \
        2>/dev/null | while read -r javafile; do
        awk '
/@com\.ilscipio\.scipio\.ce\.webapp\.control\.def\.View\(/ {
    in_view=1; name=""; vtype="screen"; ctrl=""
}
in_view && /name = "/ { match($0, /name = "([^"]+)"/, m); name=m[1] }
in_view && /type = "/ { match($0, /type = "([^"]+)"/, m); vtype=m[1] }
in_view && /controller = "/ { match($0, /controller = "([^"]+)"/, m); ctrl=m[1] }
in_view && /\)$/ { if (name != "" && ctrl != "") print ctrl "|" name "|" vtype; in_view=0 }
' "$javafile"
    done | sort -t'|' -k1,1 -k2,2 -u
}

test_content() {
    local controller="$1" view="$2" vtype="$3" mount="$4"
    local url="${BASE_URL}/${mount}/control/${view}"

    # Skip non-HTML types
    if echo "$vtype" | grep -qE "$SKIP_TYPES"; then
        return
    fi

    local tmpfile
    tmpfile=$(mktemp)
    local http_code
    http_code=$(curl -s -k -L \
        -H "X-Dev-Auth-Key: ${AUTH_KEY}" \
        -o "$tmpfile" \
        -w "%{http_code}" \
        --max-time "$TIMEOUT" \
        "$url" 2>/dev/null) || http_code="000"

    if [ "$http_code" = "000" ] || [ "$http_code" -ge 400 ] 2>/dev/null; then
        rm -f "$tmpfile"
        SKIP=$((SKIP+1))
        return
    fi

    local size
    size=$(wc -c < "$tmpfile" | tr -d ' ')
    local status="PASS"
    local reason=""

    # Check 1: Error screen (check title for "Error" — most reliable indicator)
    if grep -q '<title>[^<]*Error[^<]*</title>' "$tmpfile" 2>/dev/null; then
        status="FAIL"
        reason="Error screen rendered"
    fi

    # Check 2: FreeMarker template error in body
    if [ "$status" = "PASS" ] && grep -qi 'FreeMarker template error\|freemarker.*ParseException\|freemarker.*InvalidReferenceException' "$tmpfile" 2>/dev/null; then
        status="FAIL"
        reason="FreeMarker error in page"
    fi

    # Check 3: Java exception in body
    if [ "$status" = "PASS" ] && grep -qiE 'java\.\w+Exception|javax\.\w+Exception|org\.ofbiz\.\w+Exception|NullPointerException|ScreenRenderException' "$tmpfile" 2>/dev/null; then
        status="FAIL"
        reason="Java exception in page"
    fi

    # Check 4: Empty content (decorator frame only, no actual content widgets)
    if [ "$status" = "PASS" ] && [ "$size" -gt 1000 ]; then
        local content_markers
        content_markers=$(grep -c 'screenlet\|<form \|<table \|data-table\|grid-row\|field-row\|widget-content\|form-body\|form-row' "$tmpfile" 2>/dev/null) || content_markers=0
        if [ "$content_markers" -eq 0 ]; then
            # Page has content but no forms/tables/screenlets — might be empty
            # Check if it's just a decorator with sidebar
            local has_sidebar
            has_sidebar=$(grep -c 'sidebar\|app-navigation\|menu-item' "$tmpfile" 2>/dev/null) || has_sidebar=0
            if [ "$has_sidebar" -gt 0 ] && [ "$size" -lt 35000 ]; then
                status="WARN"
                reason="Empty content area (${size}B, only decorator/nav)"
            fi
        fi
    fi

    rm -f "$tmpfile"

    case "$status" in
        PASS)
            PASS=$((PASS+1))
            ;;
        FAIL)
            printf "  $(red FAIL) %-55s %s\n" "/${mount}/control/${view}" "${reason}"
            FAIL=$((FAIL+1))
            ERRORS+=("/${mount}/control/${view} => ${reason}")
            ;;
        WARN)
            printf "  $(yellow WARN) %-55s %s\n" "/${mount}/control/${view}" "${reason}"
            WARN=$((WARN+1))
            WARNINGS+=("/${mount}/control/${view} => ${reason}")
            ;;
    esac
}

echo "=========================================="
echo " Scipio ERP Deep Content Verification"
echo "=========================================="
echo ""
echo "Extracting views..."
ALL_VIEWS="$(extract_views)"
TOTAL=$(echo "$ALL_VIEWS" | wc -l)
echo "Found ${TOTAL} views. Testing HTML content quality..."
echo ""

CONTROLLERS="$(echo "$ALL_VIEWS" | cut -d'|' -f1 | sort -u)"
for ctrl in $CONTROLLERS; do
    if [ ${#FILTER_WEBAPPS[@]} -gt 0 ]; then
        match_found=false
        for fw in "${FILTER_WEBAPPS[@]}"; do
            [ "$fw" = "$ctrl" ] && match_found=true && break
        done
        [ "$match_found" = false ] && continue
    fi

    mount="${CONTROLLER_TO_MOUNT[$ctrl]:-$ctrl}"
    webapp_views="$(echo "$ALL_VIEWS" | grep "^${ctrl}|" || true)"
    [ -z "$webapp_views" ] && continue

    while IFS='|' read -r _ view vtype; do
        test_content "$ctrl" "$view" "$vtype" "$mount"
    done <<< "$webapp_views"
done

TOTAL_CHECKED=$((PASS + FAIL + WARN))
echo ""
echo "=========================================="
echo " Results: $(green "${PASS} ok"), $(red "${FAIL} broken"), $(yellow "${WARN} suspect") / ${TOTAL_CHECKED} HTML views checked (${SKIP} skipped)"
echo "=========================================="

if [ ${#ERRORS[@]} -gt 0 ]; then
    echo ""
    echo "BROKEN (${#ERRORS[@]}):"
    for err in "${ERRORS[@]}"; do
        echo "  $(red 'x') $err"
    done
fi

if [ ${#WARNINGS[@]} -gt 0 ]; then
    echo ""
    echo "SUSPECT (${#WARNINGS[@]}):"
    for w in "${WARNINGS[@]}"; do
        echo "  $(yellow '!') $w"
    done
fi

[ "$FAIL" -gt 0 ] && exit 1
exit 0
