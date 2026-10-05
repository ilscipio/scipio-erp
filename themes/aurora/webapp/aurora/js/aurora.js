/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
/* ==========================================================================
   Aurora - theme behaviour
   One file. It drives the shell, the scheme switch, the widgets the backend
   macros emit, and it keeps the legacy Bootstrap and Foundation calls in the
   application templates alive, because this theme carries neither library.
   ========================================================================== */

(function (window, document) {
    "use strict";

    var ACTIVE = "is-active";
    var SIDEBAR_COOKIE = "scpSidebar";
    var SCHEME_COOKIE = "auroraScheme";

    var Aurora = window.Aurora || {};
    window.Aurora = Aurora;

    /* ---- small helpers --------------------------------------------------- */

    function all(selector, parent) {
        return Array.prototype.slice.call((parent || document).querySelectorAll(selector), 0);
    }

    function setCookie(name, value, days) {
        var expires = "";
        if (days) {
            var date = new Date();
            date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
            expires = "; expires=" + date.toUTCString();
        }
        document.cookie = name + "=" + (value || "") + expires + "; path=/; SameSite=Lax";
    }

    function getCookie(name) {
        var parts = document.cookie ? document.cookie.split(";") : [];
        for (var i = 0; i < parts.length; i++) {
            var p = parts[i].trim();
            if (p.indexOf(name + "=") === 0) {
                return decodeURIComponent(p.substring(name.length + 1));
            }
        }
        return "";
    }

    function cssVar(name, fallback) {
        var v = getComputedStyle(document.documentElement).getPropertyValue(name);
        v = v ? v.trim() : "";
        return v || fallback || "";
    }

    Aurora.setCookie = setCookie;

    /* The slider macro calls bulmaCarousel, a Bulma plugin this theme does not
       load. The slides scroll natively (CSS scroll snap); the call must not throw. */
    if (!window.bulmaCarousel) {
        window.bulmaCarousel = { attach: function () { return []; } };
    }
    Aurora.getCookie = getCookie;

    /* ---- the colour scheme ----------------------------------------------- */
    /* The server writes data-theme into the html tag, so the page never
       flashes. An empty value means "follow the operating system". */

    function systemScheme() {
        return (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) ? "dark" : "light";
    }

    Aurora.getScheme = function () {
        return document.documentElement.getAttribute("data-theme") || "";
    };

    Aurora.effectiveScheme = function () {
        return Aurora.getScheme() || systemScheme();
    };

    Aurora.setScheme = function (scheme, persist) {
        var previous = Aurora.palette();
        if (scheme === "dark" || scheme === "light") {
            document.documentElement.setAttribute("data-theme", scheme);
        } else {
            document.documentElement.removeAttribute("data-theme");
            scheme = "";
        }
        if (persist !== false) {
            setCookie(SCHEME_COOKIE, scheme, 365);
            storePreference(scheme);
        }
        Aurora.repaintCharts(previous);
        document.dispatchEvent(new CustomEvent("aurora:scheme", { detail: { scheme: Aurora.effectiveScheme() } }));
    };

    Aurora.toggleScheme = function () {
        Aurora.setScheme(Aurora.effectiveScheme() === "dark" ? "light" : "dark", true);
    };

    /* The preference follows the reader to another machine. A failure here is
       not worth a message: the cookie already holds the choice. */
    function storePreference(scheme) {
        var button = document.querySelector("[data-au-pref-url]");
        if (!button || !window.fetch) {
            return;
        }
        var body = "userPrefTypeId=AURORA_SCHEME&userPrefGroupTypeId=GLOBAL_PREFERENCES&userPrefValue=" +
            encodeURIComponent(scheme);
        try {
            window.fetch(button.getAttribute("data-au-pref-url"), {
                method: "POST",
                credentials: "same-origin",
                headers: { "Content-Type": "application/x-www-form-urlencoded" },
                body: body
            }).catch(function () { /* the cookie is enough */ });
        } catch (e) { /* the cookie is enough */ }
    }

    /* ---- the chart palette ----------------------------------------------- */
    /* The chart macro used to read its colours from a compiled SASS map. It now
       reads them from the custom properties, so the charts follow the switch. */

    function rgba(colour, alpha) {
        var c = (colour || "").trim();
        if (alpha === 1 || !c) {
            return c;
        }
        if (c.charAt(0) === "#") {
            var hex = c.substring(1);
            if (hex.length === 3) {
                hex = hex.charAt(0) + hex.charAt(0) + hex.charAt(1) + hex.charAt(1) + hex.charAt(2) + hex.charAt(2);
            }
            var n = parseInt(hex, 16);
            return "rgba(" + ((n >> 16) & 255) + "," + ((n >> 8) & 255) + "," + (n & 255) + "," + alpha + ")";
        }
        if (c.indexOf("rgb(") === 0) {
            return c.replace("rgb(", "rgba(").replace(")", "," + alpha + ")");
        }
        return c;
    }

    Aurora.palette = function () {
        var series = [];
        for (var i = 1; i <= 8; i++) {
            series.push(cssVar("--chart-" + i, "#0e7490"));
        }
        return {
            series: series,
            ink: cssVar("--au-ink-2", "#3d5064"),
            rule: cssVar("--au-rule", "rgba(0,0,0,.14)"),
            surface: cssVar("--au-surface", "#ffffff"),
            dot: cssVar("--chart-dot", "#ffffff"),
            font: cssVar("--au-font", "sans-serif")
        };
    };

    /* The key names are the ones the chart macro expects. */
    Aurora.chartTokens = function () {
        var p = Aurora.palette();
        var t = {
            primaryFillColor: rgba(p.series[0], .7),
            primaryStrokeColor: rgba(p.series[0], .9),
            primaryPointStrokeColor: rgba(p.series[0], .7),
            secondaryFillColor: rgba(p.series[1], .7),
            secondaryStrokeColor: rgba(p.series[1], .9),
            secondaryPointStrokeColor: rgba(p.series[1], .7),
            pointColor: p.dot,
            pointHighlightFill: p.surface,
            pointHighlightStroke: p.series[0],
            pointDot: true,
            scaleType: "linear",
            scaleDisplay: true,
            scaleGridLineColor: p.rule,
            scaleLabelFontFamily: p.font,
            scaleLabelFontColor: p.ink,
            scaleLabelFontSize: 11,
            scaleLabelDisplay: false,
            angleShowLineOut: true,
            scaleBeginAtZero: true,
            showTooltips: true,
            color: p.series[0],
            highlight: p.series[1],
            dataLabels: false
        };
        for (var i = 0; i < 6; i++) {
            t["pieFillColor" + (i + 1)] = rgba(p.series[i], 1);
            t["pieHighlightColor" + (i + 1)] = rgba(p.series[i], .6);
        }
        return t;
    };

    /* A chart keeps the colours it was built with. On a scheme change every
       colour that came from the old palette is replaced by its counterpart. */
    Aurora.repaintCharts = function (previous) {
        if (!window.Chart || !previous) {
            return;
        }
        var now = Aurora.palette();
        var map = {};
        for (var i = 0; i < previous.series.length; i++) {
            map[previous.series[i].toLowerCase()] = now.series[i];
            map[rgba(previous.series[i], .7).toLowerCase()] = rgba(now.series[i], .7);
            map[rgba(previous.series[i], .9).toLowerCase()] = rgba(now.series[i], .9);
            map[rgba(previous.series[i], .6).toLowerCase()] = rgba(now.series[i], .6);
        }
        map[previous.ink.toLowerCase()] = now.ink;
        map[previous.rule.toLowerCase()] = now.rule;
        map[previous.surface.toLowerCase()] = now.surface;
        map[previous.dot.toLowerCase()] = now.dot;

        function convert(value) {
            if (typeof value === "string") {
                return map[value.trim().toLowerCase()] || value;
            }
            if (Array.isArray(value)) {
                return value.map(convert);
            }
            return value;
        }

        var instances = Chart.instances || {};
        Object.keys(instances).forEach(function (key) {
            var chart = instances[key];
            if (!chart || !chart.data) {
                return;
            }
            (chart.data.datasets || []).forEach(function (ds) {
                ["backgroundColor", "borderColor", "pointBackgroundColor", "pointBorderColor",
                    "hoverBackgroundColor", "hoverBorderColor"].forEach(function (k) {
                    if (ds[k] !== undefined) {
                        ds[k] = convert(ds[k]);
                    }
                });
            });
            try {
                var scales = (chart.options && chart.options.scales) || {};
                ["xAxes", "yAxes"].forEach(function (axis) {
                    (scales[axis] || []).forEach(function (a) {
                        if (a.gridLines) { a.gridLines.color = now.rule; }
                        if (a.ticks) { a.ticks.fontColor = now.ink; }
                        if (a.scaleLabel) { a.scaleLabel.fontColor = now.ink; }
                    });
                });
                chart.update();
            } catch (e) { /* a chart that will not repaint keeps its colours */ }
        });
    };

    /* ---- the date field --------------------------------------------------- */
    /* flatpickr writes the machine format into the hidden input and shows the
       reader a readable one. */

    Aurora.attachDatePicker = function (options) {
        var display = document.getElementById(options.displayId);
        var hidden = options.valueId ? document.getElementById(options.valueId) : null;
        if (!display || !window.flatpickr) {
            return null;
        }
        if (display._auPicker) {
            return display._auPicker;
        }
        var dispFormat = options.displayFormat || "YYYY-MM-DD HH:mm:ss";
        var storeFormat = options.storeFormat || "YYYY-MM-DD HH:mm:ss.SSS";
        var hasMoment = !!window.moment;

        function parse(str) {
            if (!str) { return undefined; }
            if (!hasMoment) { return new Date(str); }
            var m = window.moment(str, dispFormat, true);
            if (!m.isValid()) { m = window.moment(str, storeFormat, true); }
            if (!m.isValid()) { m = window.moment(str); }
            return m.isValid() ? m.toDate() : undefined;
        }

        function format(date) {
            return hasMoment ? window.moment(date).format(dispFormat) : String(date);
        }

        function writeValue(dates) {
            if (!hidden) { return; }
            if (!dates || !dates.length) {
                hidden.value = "";
            } else {
                hidden.value = hasMoment ? window.moment(dates[0]).format(storeFormat) : dates[0].toISOString();
            }
            hidden.dispatchEvent(new Event("change", { bubbles: true }));
        }

        var config = {
            allowInput: true,
            enableTime: options.time !== false,
            noCalendar: options.onlyTime === true,
            enableSeconds: /s/.test(storeFormat),
            time_24hr: true,
            monthSelectorType: "static",
            disableMobile: true,
            parseDate: parse,
            formatDate: format,
            onChange: function (dates) { writeValue(dates); },
            onClose: function (dates) { writeValue(dates); }
        };

        /* The field may already hold a value, in the display format or in the
           machine format. Whichever it is, it must survive the first paint. */
        var initial = (hidden && hidden.value) || display.value;
        if (initial) {
            var parsed = parse(initial);
            if (parsed) { config.defaultDate = parsed; }
        }

        try {
            display._auPicker = window.flatpickr(display, config);
            return display._auPicker;
        } catch (e) {
            return null;
        }
    };

    /* ---- modal ------------------------------------------------------------ */

    function openModal(el) {
        if (!el) { return; }
        el.classList.add(ACTIVE);
        document.body.classList.add("au-modal-open");
    }

    function closeModal(el) {
        if (!el) { return; }
        el.classList.remove(ACTIVE);
        if (!document.querySelector(".modal." + ACTIVE)) {
            document.body.classList.remove("au-modal-open");
        }
    }

    function closeAllModals() {
        all(".modal").forEach(closeModal);
        document.body.classList.remove("au-modal-open");
    }

    Aurora.openModal = openModal;
    Aurora.closeModal = closeModal;
    Aurora.closeAllModals = closeAllModals;

    /* ---- compatibility shims ---------------------------------------------- */
    /* Some application templates still call Bootstrap or Foundation. They are
       written to fail quietly, which leaves a dead control on the page. These
       shims make the call do the right thing instead. */

    function installShims($) {
        if (!$ || !$.fn) {
            return;
        }
        if (!$.fn.foundation) {
            $.fn.foundation = function (component, action) {
                if (component === "reveal" && action === "close") {
                    this.each(function () { closeModal(this.closest ? this.closest(".modal") : null); });
                }
                return this;
            };
        }
        if (!$.fn.modal) {
            $.fn.modal = function (action) {
                return this.each(function () {
                    var el = this.classList && this.classList.contains("modal") ? this : (this.closest ? this.closest(".modal") : null);
                    if (!el) { return; }
                    if (action === "hide") { closeModal(el); }
                    else if (action === "toggle") { el.classList.contains(ACTIVE) ? closeModal(el) : openModal(el); }
                    else { openModal(el); }
                });
            };
        }
        if (!$.fn.tab) {
            $.fn.tab = function () {
                this.each(function () { if (this.click) { this.click(); } });
                return this;
            };
        }
    }

    /* ---- the shell -------------------------------------------------------- */

    /* On a wide screen the side column is part of the page and the menu
       button hides or shows it (a cookie keeps the choice, so the server
       renders it). On a narrow screen the column is a drawer over the page. */
    var WIDE = window.matchMedia ? window.matchMedia("(min-width: 1024px)") : { matches: true };
    var drawerReturnFocus = null;

    function shellEl() { return document.getElementById("scpwrap"); }
    function sideEl() { return document.getElementById("au-side"); }

    function visible(el) { return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length); }

    function syncMenuButton() {
        var shell = shellEl();
        if (!shell) { return; }
        var open = WIDE.matches ? !shell.classList.contains("is-side-hidden") : shell.classList.contains("is-side-open");
        all("[data-au-side-toggle]").forEach(function (b) { b.setAttribute("aria-expanded", open ? "true" : "false"); });
    }

    function setDrawer(open, moveFocus) {
        var shell = shellEl(), side = sideEl();
        if (!shell || !side) { return; }
        shell.classList.toggle("is-side-open", open);
        document.documentElement.classList.toggle("au-lock", open);
        all(".au-scrim").forEach(function (s) { s.hidden = !open; });
        syncMenuButton();
        if (moveFocus === false) { return; }
        if (open) {
            drawerReturnFocus = document.activeElement;
            var target = side.querySelector(".au-side-menu li.is-active > a") || side.querySelector(".au-menu-find .au-filter");
            if (target) { window.setTimeout(function () { target.focus(); }, 30); }
        } else if (drawerReturnFocus && drawerReturnFocus.focus) {
            drawerReturnFocus.focus();
            drawerReturnFocus = null;
        }
    }

    function initShell() {
        var shell = shellEl();
        if (!shell) { return; }

        all("[data-au-side-toggle]").forEach(function (b) {
            b.addEventListener("click", function () {
                if (WIDE.matches) {
                    var hidden = shell.classList.toggle("is-side-hidden");
                    setCookie(SIDEBAR_COOKIE, hidden ? "true" : "false", 365);
                    syncMenuButton();
                } else {
                    setDrawer(!shell.classList.contains("is-side-open"));
                }
            });
        });
        all("[data-au-side-close]").forEach(function (el) {
            el.addEventListener("click", function () { setDrawer(false); });
        });

        document.addEventListener("keydown", function (event) {
            if (event.key === "Escape") {
                if (closeJump() || closePopovers(null, true)) { return; }
                if (shell.classList.contains("is-side-open")) { setDrawer(false); }
                return;
            }
            /* Keep the focus inside the open drawer. */
            if (event.key === "Tab" && !WIDE.matches && shell.classList.contains("is-side-open")) {
                var items = all("a[href], button:not([disabled]), input:not([disabled])", sideEl()).filter(visible);
                if (!items.length) { return; }
                var first = items[0], last = items[items.length - 1];
                if (event.shiftKey && document.activeElement === first) { last.focus(); event.preventDefault(); }
                else if (!event.shiftKey && document.activeElement === last) { first.focus(); event.preventDefault(); }
                return;
            }
            /* "/" goes to the menu filter, as in most web applications. */
            if (event.key === "/" && !event.ctrlKey && !event.metaKey && !event.altKey) {
                var t = event.target;
                if (t && (t.isContentEditable || /^(input|textarea|select)$/i.test(t.tagName))) { return; }
                var filter = document.querySelector(".au-menu-find .au-filter");
                if (!filter) { return; }
                event.preventDefault();
                if (!WIDE.matches) { setDrawer(true, false); }
                else if (shell.classList.contains("is-side-hidden")) { shell.classList.remove("is-side-hidden"); syncMenuButton(); }
                filter.focus();
            }
        });

        var onWidthChange = function () {
            if (WIDE.matches && shell.classList.contains("is-side-open")) { setDrawer(false, false); }
            syncMenuButton();
        };
        if (WIDE.addEventListener) { WIDE.addEventListener("change", onWidthChange); }
        else if (WIDE.addListener) { WIDE.addListener(onWidthChange); }
        syncMenuButton();
    }

    /* ---- popovers: the application list, notifications, the user menu ----- */

    function setPopover(button, open) {
        var panel = document.getElementById(button.getAttribute("data-au-popover"));
        if (!panel) { return; }
        button.setAttribute("aria-expanded", open ? "true" : "false");
        panel.hidden = !open;
        if (panel.id === "au-apps") {
            var side = sideEl();
            if (side) { side.classList.toggle("is-apps-open", open); }
            if (open) {
                var filter = panel.querySelector(".au-filter");
                if (filter) { filter.focus(); }
            }
        }
    }

    /* Closes every open popover except one. Returns true when it closed one. */
    function closePopovers(except, restoreFocus) {
        var closed = false;
        all("[data-au-popover][aria-expanded='true']").forEach(function (b) {
            if (b === except) { return; }
            var panel = document.getElementById(b.getAttribute("data-au-popover"));
            var hadFocus = panel && panel.contains(document.activeElement);
            setPopover(b, false);
            if (restoreFocus && hadFocus) { b.focus(); }
            closed = true;
        });
        return closed;
    }

    function initPopovers() {
        all("[data-au-popover]").forEach(function (b) {
            b.addEventListener("click", function (event) {
                event.stopPropagation();
                var open = b.getAttribute("aria-expanded") !== "true";
                closePopovers(b);
                setPopover(b, open);
            });
        });
        document.addEventListener("click", function (event) {
            all("[data-au-popover][aria-expanded='true']").forEach(function (b) {
                var panel = document.getElementById(b.getAttribute("data-au-popover"));
                if (panel && panel.contains(event.target)) { return; }
                setPopover(b, false);
            });
        });
    }

    /* ---- the fold-out menu ------------------------------------------------ */

    var foldSeq = 0;

    function setFold(li, open) {
        li.classList.toggle("is-expanded", open);
        var b = li.querySelector(":scope > .au-fold");
        if (b) { b.setAttribute("aria-expanded", open ? "true" : "false"); }
    }

    /* A row that holds a list gets a fold control. The list of the page you
       are on is open; opening a list closes its open siblings. */
    function initFoldMenu() {
        var menu = document.getElementById("au-side-menu");
        if (!menu) { return; }
        all("li", menu).forEach(function (li) {
            var sub = li.querySelector(":scope > ul");
            var link = li.querySelector(":scope > a");
            if (!sub || !link || !sub.querySelector(":scope > li")) { return; }
            if (!sub.id) { sub.id = "au-fold-" + (++foldSeq); }
            var open = li.classList.contains("is-active") || li.classList.contains("is-active-ancestor") ||
                !!sub.querySelector("li.is-active, a.is-active");
            var b = document.createElement("button");
            b.type = "button";
            b.className = "au-fold";
            b.setAttribute("aria-controls", sub.id);
            b.setAttribute("aria-label", link.textContent.trim());
            b.innerHTML = '<i class="fa fa-angle-right" aria-hidden="true"></i>';
            link.insertAdjacentElement("afterend", b);
            li.setAttribute("data-au-open", open ? "1" : "0");
            setFold(li, open);
            b.addEventListener("click", function () {
                var willOpen = !li.classList.contains("is-expanded");
                if (willOpen) {
                    all(":scope > li.is-expanded", li.parentNode).forEach(function (s) {
                        if (s !== li) { setFold(s, false); s.setAttribute("data-au-open", "0"); }
                    });
                }
                setFold(li, willOpen);
                li.setAttribute("data-au-open", willOpen ? "1" : "0");
            });
        });
        document.documentElement.classList.add("au-js");
        var current = menu.querySelector("li.is-active > a");
        if (current && current.scrollIntoView) { current.scrollIntoView({ block: "nearest" }); }
    }

    /* ---- filters: the menu and the application list ----------------------- */

    function norm(s) {
        s = String(s || "");
        if (s.normalize) { s = s.normalize("NFD").replace(/[̀-ͯ]/g, ""); }
        return s.toLowerCase().replace(/\s+/g, " ").trim();
    }

    function applyFilter(root, query) {
        var q = norm(query);
        var items = all("li", root).filter(function (li) { return !li.classList.contains("au-menu-heading"); });
        var any = false;
        items.forEach(function (li) {
            var a = li.querySelector(":scope > a");
            li.auOwnHit = !!a && norm(a.textContent + " " + (a.getAttribute("data-au-filter-text") || "")).indexOf(q) !== -1;
        });
        items.forEach(function (li) {
            if (!q) {
                li.classList.remove("au-filtered-out");
                if (li.hasAttribute("data-au-open")) { setFold(li, li.getAttribute("data-au-open") === "1"); }
                return;
            }
            var below = all("li", li).some(function (d) { return d.auOwnHit; });
            var hit = li.auOwnHit || below;
            li.classList.toggle("au-filtered-out", !hit);
            if (li.hasAttribute("data-au-open")) { setFold(li, below); }
            if (hit) { any = true; }
        });
        all(".au-menu-heading", root).forEach(function (h) { h.classList.toggle("au-filtered-out", !!q); });
        var empty = root.querySelector(".au-menu-empty");
        if (empty) { empty.hidden = !q || any; }
    }

    function initFilters() {
        all("input[data-au-filter]").forEach(function (input) {
            var root = document.getElementById(input.getAttribute("data-au-filter"));
            if (!root) { return; }
            input.addEventListener("input", function () { applyFilter(root, input.value); });
            input.addEventListener("keydown", function (event) {
                if (event.key === "Enter") {
                    var links = all("li:not(.au-filtered-out) > a[href]", root).filter(function (a) {
                        return a.getAttribute("href") !== "#" && a.closest("li").auOwnHit;
                    });
                    if (input.value && links.length) { event.preventDefault(); window.location.href = links[0].href; }
                } else if (event.key === "Escape" && input.value) {
                    event.stopPropagation();
                    input.value = "";
                    applyFilter(root, "");
                }
            });
        });
    }

    function initScheme() {
        all("[data-au-scheme-toggle]").forEach(function (el) {
            el.addEventListener("click", function (event) {
                event.preventDefault();
                Aurora.toggleScheme();
            });
        });
        /* A reader who never chose keeps following the operating system. */
        if (window.matchMedia) {
            var mq = window.matchMedia("(prefers-color-scheme: dark)");
            var onChange = function () {
                if (!Aurora.getScheme()) {
                    var previous = Aurora.palette();
                    window.setTimeout(function () { Aurora.repaintCharts(previous); }, 0);
                }
            };
            if (mq.addEventListener) { mq.addEventListener("change", onChange); }
            else if (mq.addListener) { mq.addListener(onChange); }
        }
    }

    function initModals() {
        all(".js-modal-trigger, [data-toggle='modal']").forEach(function (trigger) {
            trigger.addEventListener("click", function (event) {
                var id = trigger.dataset.target;
                var target = id ? document.getElementById(id) : null;
                if (target) {
                    event.preventDefault();
                    openModal(target);
                }
            });
        });
        document.addEventListener("click", function (event) {
            var closer = event.target.closest(".modal-background, .modal-close, .modal-card-foot .button[data-dismiss], .modal .delete");
            if (closer) {
                var modal = closer.closest(".modal");
                if (modal) {
                    event.preventDefault();
                    closeModal(modal);
                }
            }
        });
        document.addEventListener("keydown", function (event) {
            if (event.key === "Escape" || event.keyCode === 27) {
                closeAllModals();
            }
        });
    }

    function initTabs() {
        all(".tabs").forEach(function (strip) {
            var items = all("li", strip);
            var panes = [];
            var holder = strip.nextElementSibling;
            if (holder) {
                panes = all(":scope > .tab-content", holder);
            }
            if (!items.length) { return; }
            items.forEach(function (item, index) {
                item.addEventListener("click", function (event) {
                    var link = item.querySelector("a");
                    if (link && link.getAttribute("href") && link.getAttribute("href").charAt(0) === "#") {
                        event.preventDefault();
                    }
                    items.forEach(function (i) { i.classList.remove(ACTIVE); });
                    panes.forEach(function (p) { p.classList.remove(ACTIVE); });
                    item.classList.add(ACTIVE);
                    if (panes[index]) { panes[index].classList.add(ACTIVE); }
                });
            });
            if (panes.length && !panes.some(function (p) { return p.classList.contains(ACTIVE); })) {
                items[0].classList.add(ACTIVE);
                panes[0].classList.add(ACTIVE);
            }
        });
    }

    function initDismissables() {
        document.addEventListener("click", function (event) {
            var del = event.target.closest(".notification > .delete");
            if (del && del.parentNode && del.parentNode.parentNode) {
                del.parentNode.parentNode.removeChild(del.parentNode);
            }
        });
    }

    /* A collapsible field group folds on the button in its legend. */
    function initFieldGroups() {
        all(".fieldgroup-body").forEach(function (body) {
            var parent = body.parentNode;
            var button = parent && parent.querySelector("legend > .au-disclose");
            if (!button) { return; }
            button.addEventListener("click", function () {
                var open = body.style.display === "none";
                body.style.display = open ? "" : "none";
                body.classList.toggle("is-open", open);
                button.setAttribute("aria-expanded", String(open));
            });
        });
    }

    function initDropdowns() {
        var dropdowns = all(".dropdown:not(.is-hoverable), .button-dropdown:not(.is-hoverable)");
        dropdowns.forEach(function (el) {
            el.addEventListener("click", function (event) {
                event.stopPropagation();
                var wasOpen = el.classList.contains(ACTIVE);
                dropdowns.forEach(function (d) { d.classList.remove(ACTIVE); });
                if (!wasOpen) { el.classList.add(ACTIVE); }
            });
        });
        document.addEventListener("click", function () {
            dropdowns.forEach(function (el) { el.classList.remove(ACTIVE); });
        });
    }

    /* The side column highlights the group under the pointer. */
    /* ---- the jump dialog (Ctrl K) -------------------------------------------
       Lists every application of the rail and every item of the app menu;
       type to filter, arrows to move, Enter to open, Esc to close. */
    var jumpItems = [];
    var jumpActive = -1;
    var jumpReturn = null;

    function jumpEl() { return document.getElementById("au-jump"); }

    function collectJumpItems() {
        var items = [];
        var seen = {};
        all(".au-rail-item").forEach(function (a) {
            var icon = a.querySelector(".fa");
            items.push({ label: a.getAttribute("aria-label") || a.textContent.trim(), href: a.href, where: "",
                icon: icon ? icon.className : "fa fa-folder-o" });
            seen[a.href] = true;
        });
        var titleEl = document.querySelector(".au-panel-title");
        var appName = titleEl ? titleEl.textContent.trim() : "";
        all("#au-side-menu a[href]").forEach(function (a) {
            var raw = a.getAttribute("href") || "";
            var label = a.textContent.trim();
            if (!label || seen[a.href] || seen["label:" + label] || raw === "#" || /^javascript:/i.test(raw)) { return; }
            seen[a.href] = true;
            seen["label:" + label] = true;
            items.push({ label: label, href: a.href, where: appName, icon: "fa fa-angle-right" });
        });
        return items;
    }

    function setJumpActive(index) {
        var options = all("#au-jump-list a");
        options.forEach(function (o, k) {
            o.classList.toggle("is-active", k === index);
            o.setAttribute("aria-selected", k === index ? "true" : "false");
        });
        jumpActive = index;
        var input = jumpEl().querySelector(".au-jump-input");
        if (index >= 0 && options[index]) {
            input.setAttribute("aria-activedescendant", options[index].id);
            options[index].scrollIntoView({ block: "nearest" });
        } else {
            input.removeAttribute("aria-activedescendant");
        }
    }

    function renderJump(query) {
        var dialog = jumpEl();
        var list = document.getElementById("au-jump-list");
        var q = norm(query);
        var hits = jumpItems.filter(function (it) { return !q || norm(it.label + " " + it.where).indexOf(q) !== -1; }).slice(0, 60);
        while (list.firstChild) { list.removeChild(list.firstChild); }
        hits.forEach(function (it, index) {
            var li = document.createElement("li");
            li.setAttribute("role", "presentation");
            var a = document.createElement("a");
            a.href = it.href;
            a.id = "au-jump-option-" + index;
            a.setAttribute("role", "option");
            var iconBox = document.createElement("span");
            iconBox.className = "au-jump-icon";
            var icon = document.createElement("i");
            icon.className = it.icon;
            icon.setAttribute("aria-hidden", "true");
            iconBox.appendChild(icon);
            a.appendChild(iconBox);
            var text = document.createElement("span");
            text.textContent = it.label;
            a.appendChild(text);
            if (it.where) {
                var where = document.createElement("span");
                where.className = "au-jump-where";
                where.textContent = it.where;
                a.appendChild(where);
            }
            li.appendChild(a);
            list.appendChild(li);
        });
        dialog.querySelector(".au-jump-empty").hidden = hits.length > 0;
        setJumpActive(hits.length ? 0 : -1);
    }

    function openJump() {
        var dialog = jumpEl();
        if (!dialog) { return; }
        closePopovers();
        jumpReturn = document.activeElement;
        jumpItems = collectJumpItems();
        dialog.hidden = false;
        document.documentElement.classList.add("au-lock");
        var input = dialog.querySelector(".au-jump-input");
        input.value = "";
        renderJump("");
        input.focus();
    }

    function closeJump() {
        var dialog = jumpEl();
        if (!dialog || dialog.hidden) { return false; }
        dialog.hidden = true;
        var shell = shellEl();
        if (!shell || !shell.classList.contains("is-side-open")) { document.documentElement.classList.remove("au-lock"); }
        if (jumpReturn && jumpReturn.focus) { jumpReturn.focus(); }
        return true;
    }

    function initJump() {
        var dialog = jumpEl();
        if (!dialog) {
            /* A page without the dialog (sign-in): the buttons do nothing. */
            return;
        }
        var input = dialog.querySelector(".au-jump-input");
        all("[data-au-jump-open]").forEach(function (b) { b.addEventListener("click", openJump); });
        input.addEventListener("input", function () { renderJump(input.value); });
        input.addEventListener("keydown", function (event) {
            var count = all("#au-jump-list a").length;
            if (event.key === "ArrowDown") {
                event.preventDefault();
                if (count) { setJumpActive((jumpActive + 1) % count); }
            } else if (event.key === "ArrowUp") {
                event.preventDefault();
                if (count) { setJumpActive((jumpActive - 1 + count) % count); }
            } else if (event.key === "Enter") {
                var option = all("#au-jump-list a")[jumpActive];
                if (option) { event.preventDefault(); window.location.href = option.href; }
            } else if (event.key === "Escape") {
                event.preventDefault();
                event.stopPropagation();
                closeJump();
            }
        });
        dialog.addEventListener("click", function (event) { if (event.target === dialog) { closeJump(); } });
        document.addEventListener("keydown", function (event) {
            if ((event.ctrlKey || event.metaKey) && !event.altKey && (event.key === "k" || event.key === "K")) {
                event.preventDefault();
                if (dialog.hidden) { openJump(); } else { closeJump(); }
            }
        });
    }

    /* A DataTables header row whose cells carry no label shows as an empty
       band; mark it so the theme hides it. DataTables fires init.dt and
       draw.dt on the table, and both bubble to the document. */
    function markEmptyTableHead(table) {
        if (!table || !table.closest) { return; }
        var wrapper = table.closest(".dataTables_wrapper");
        var scope = wrapper || table;
        var cells = all(".dataTables_scrollHead th, thead th", scope);
        if (!cells.length) { return; }
        var empty = cells.every(function (th) { return !th.textContent.trim() && !th.querySelector("input, select, button, a, img, i"); });
        scope.classList.toggle("au-no-head", empty);
    }

    /* A table cell keeps its value on one line and clips long text at 22rem
       (aurora-components.css). When the columns still do not fit, the last
       ones scroll out of view (EditCostCalcs hid its Remove buttons); then
       long text cells may wrap. Short values such as dates stay on one line. */
    function fitWideTable($, table) {
        if (!table || !table.closest || !table.matches("table.dataTable")) { return; }
        var box = table.closest(".dataTables_scrollBody") || table.parentElement;
        if (!table.hasAttribute("data-au-fit")) {
            if (!box || table.scrollWidth <= box.clientWidth + 1) { return; }
            table.setAttribute("data-au-fit", "true");
        }
        var changed = false;
        all("tbody td:not(.au-wrap)", table).forEach(function (td) {
            if (td.textContent.trim().length > 24 && !td.querySelector("input, select, textarea, button, .button")) {
                td.classList.add("au-wrap");
                changed = true;
            }
        });
        if (changed && $.fn.dataTable && $.fn.dataTable.isDataTable(table)) {
            $(table).DataTable().columns.adjust();
        }
    }

    function initTableHeads($) {
        if (!$ || !$.fn) { return; }
        $(document).on("init.dt draw.dt", function (event) {
            markEmptyTableHead(event.target);
            fitWideTable($, event.target);
        });
        all("table.dataTable").forEach(function (table) {
            markEmptyTableHead(table);
            fitWideTable($, table);
        });
    }

    /* The eye button of a password field shows or hides the password. */
    function initReveal() {
        all("[data-au-reveal]").forEach(function (b) {
            var input = document.getElementById(b.getAttribute("data-au-reveal"));
            if (!input) { return; }
            b.addEventListener("click", function () {
                var show = input.type === "password";
                input.type = show ? "text" : "password";
                b.setAttribute("aria-pressed", show ? "true" : "false");
                var icon = b.querySelector(".fa");
                if (icon) { icon.className = "fa " + (show ? "fa-eye-slash" : "fa-eye"); }
            });
        });
    }

    /* ---- Chart.js on demand ------------------------------------------------
       Most pages have no chart, so Chart.js (170 KB) is not in the page. The
       chart macro calls this; the first call loads the library, then runs
       every queued chart. */
    var chartQueue = null;
    Aurora.withChart = function (fn) {
        if (window.Chart) { fn(); return; }
        if (chartQueue) { chartQueue.push(fn); return; }
        chartQueue = [fn];
        var self = document.querySelector("script[src*='/aurora/js/aurora.js']");
        var script = document.createElement("script");
        script.src = self ? self.src.replace(/aurora\.js.*$/, "vendor/Chart.min.js") : "/aurora/js/vendor/Chart.min.js";
        script.onload = function () {
            var queue = chartQueue;
            chartQueue = null;
            queue.forEach(function (f) {
                try { f(); } catch (e) { if (window.console) { window.console.error("Aurora chart", e); } }
            });
        };
        document.head.appendChild(script);
    };

    /* ---- start ------------------------------------------------------------ */

    function init() {
        installShims(window.jQuery || window.$);
        initFoldMenu();
        initShell();
        initPopovers();
        initFilters();
        initJump();
        initReveal();
        initTableHeads(window.jQuery);
        initScheme();
        initModals();
        initTabs();
        initDismissables();
        initFieldGroups();
        initDropdowns();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", init);
    } else {
        init();
    }

    /* Kept for the screens that call them directly. */
    window.setCookie = window.setCookie || setCookie;
    window.getAll = window.getAll || all;
    /* Older menu markup called this to switch side menu tabs. The side column
       now shows every group, so it only brings the group into view. */
    window.openSidebarTab = function (id) {
        var el = document.getElementById(id);
        if (el && el.scrollIntoView) { el.scrollIntoView({ block: "start" }); }
    };

}(window, document));
