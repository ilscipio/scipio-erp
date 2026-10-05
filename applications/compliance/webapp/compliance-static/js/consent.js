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
/*
 * SCIPIO: 4.0.0: storefront consent (compliance component). No dependencies.
 *
 * EU regime: only necessary services run until the shopper chooses; "Reject all" and "Accept all" have equal
 * weight. US regime: services run; the shopper can opt out; a Global Privacy Control signal is an opt-out of
 * sale/sharing (marketing). Gated scripts are <script type="text/plain" data-scp-consent="category">; this file
 * runs one only when its category is allowed. The choice is stored in the scpConsent cookie and logged by
 * POST to the recordConsent request. A new consent version (service list changed) asks again.
 */
(function () {
    'use strict';
    var cfgEl = document.getElementById('scp-consent-config');
    if (!cfgEl) {
        return;
    }
    var cfg;
    try {
        cfg = JSON.parse(cfgEl.textContent);
    } catch (e) {
        return;
    }
    var gpc = cfg.gpc === true || navigator.globalPrivacyControl === true;
    var dlg = document.getElementById('scp-consent');
    var lastFocus = null;

    function readState() {
        var m = document.cookie.match(new RegExp('(?:^|; )' + cfg.cookieName + '=([^;]*)'));
        if (!m) {
            return null;
        }
        try {
            var s = JSON.parse(decodeURIComponent(m[1].replace(/\+/g, ' ')));
            return s && s.v === cfg.consentVersion ? s : null;
        } catch (e) {
            return null;
        }
    }

    function writeState(s) {
        var expires = new Date(Date.now() + 365 * 864e5).toUTCString();
        document.cookie = cfg.cookieName + '=' + encodeURIComponent(JSON.stringify(s)) + '; expires=' + expires +
            '; path=/; SameSite=Lax' + (location.protocol === 'https:' ? '; Secure' : '');
    }

    var state = readState();

    function allowed(cat) {
        if (cat === 'necessary') {
            return true;
        }
        if (cat === 'marketing' && gpc) {
            return false;
        }
        if (!state) {
            return cfg.regime === 'US';
        }
        return state[cat] === true;
    }

    function runScripts() {
        var gated = document.querySelectorAll('script[type="text/plain"][data-scp-consent]');
        for (var i = 0; i < gated.length; i++) {
            var s = gated[i];
            if (s.getAttribute('data-scp-done') || !allowed(s.getAttribute('data-scp-consent'))) {
                continue;
            }
            var n = document.createElement('script');
            if (s.getAttribute('data-src')) {
                n.src = s.getAttribute('data-src');
            }
            n.text = s.text;
            s.setAttribute('data-scp-done', '1');
            s.parentNode.insertBefore(n, s.nextSibling);
        }
    }

    function post(s, source) {
        if (!cfg.recordUrl || !window.fetch) {
            return;
        }
        var body = new URLSearchParams();
        body.set('preferences', s.preferences ? 'Y' : 'N');
        body.set('statistics', s.statistics ? 'Y' : 'N');
        body.set('marketing', s.marketing ? 'Y' : 'N');
        if (cfg.regime === 'US') {
            body.set('saleShare', s.marketing ? 'Y' : 'N');
        }
        body.set('source', source || 'banner');
        if (gpc) {
            body.set('gpcApplied', 'Y');
        }
        fetch(cfg.recordUrl, {method: 'POST', body: body, credentials: 'same-origin'}).catch(function () {});
    }

    function save(choice, source) {
        state = {
            v: cfg.consentVersion, r: cfg.regime,
            preferences: !!choice.preferences, statistics: !!choice.statistics,
            marketing: !!choice.marketing && !gpc, t: Date.now()
        };
        writeState(state);
        post(state, source);
        runScripts();
        hide();
    }

    function toggles() {
        return dlg ? dlg.querySelectorAll('input[type="checkbox"][data-scp-cat]') : [];
    }

    function show(detail) {
        if (!dlg) {
            return;
        }
        var t = toggles();
        for (var i = 0; i < t.length; i++) {
            var cat = t[i].getAttribute('data-scp-cat');
            t[i].checked = state ? state[cat] === true : cfg.regime === 'US' && !(cat === 'marketing' && gpc);
            if (cat === 'marketing' && gpc) {
                t[i].checked = false;
            }
        }
        dlg.classList.toggle('is-detail', !!detail);
        lastFocus = document.activeElement;
        dlg.hidden = false;
        var first = dlg.querySelector(detail ? 'input[data-scp-cat]:not([disabled]), button' : 'button');
        if (first) {
            first.focus();
        }
    }

    function hide() {
        if (dlg) {
            dlg.hidden = true;
            if (lastFocus && lastFocus.focus) {
                lastFocus.focus();
            }
        }
    }

    function current() {
        var c = {preferences: false, statistics: false, marketing: false};
        var t = toggles();
        for (var i = 0; i < t.length; i++) {
            c[t[i].getAttribute('data-scp-cat')] = t[i].checked;
        }
        return c;
    }

    document.addEventListener('click', function (ev) {
        var el = ev.target.closest ? ev.target.closest('[data-scp-action],[data-scp-consent-open]') : null;
        if (!el) {
            return;
        }
        if (el.hasAttribute('data-scp-consent-open')) {
            ev.preventDefault();
            show(true);
            return;
        }
        switch (el.getAttribute('data-scp-action')) {
        case 'accept-all':
            save({preferences: true, statistics: true, marketing: true}, 'banner');
            break;
        case 'reject-all':
            save({preferences: false, statistics: false, marketing: false}, 'banner');
            break;
        case 'save':
            save(current(), 'banner');
            break;
        case 'settings':
            dlg.classList.add('is-detail');
            break;
        case 'ok':
            save({preferences: true, statistics: true, marketing: !gpc}, gpc ? 'gpc' : 'banner');
            break;
        case 'close':
            hide();
            break;
        }
    });

    document.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape' && dlg && !dlg.hidden && state) {
            hide();
        }
    });

    // Notice (legal guarantee), GARAN label and print: native <dialog>, opened on the first click
    function openDialog(d) {
        if (d && typeof d.showModal === 'function' && !d.open) {
            d.showModal();
        }
    }
    document.addEventListener('click', function (ev) {
        var el = ev.target.closest ? ev.target.closest('[data-scp-notice-open],[data-scp-garan-open],[data-scp-dialog-close],[data-scp-print]') : null;
        if (!el) {
            var d = ev.target;
            if (d && d.tagName === 'DIALOG' && d.classList.contains('scp-dialog')) {
                d.close(); // click on the backdrop
            }
            return;
        }
        if (el.hasAttribute('data-scp-notice-open')) {
            ev.preventDefault();
            openDialog(document.getElementById('scp-notice'));
        } else if (el.hasAttribute('data-scp-garan-open')) {
            ev.preventDefault();
            var gd = document.getElementById('scp-garan');
            var body = gd ? gd.querySelector('[data-scp-garan-body]') : null;
            var url = el.getAttribute('data-scp-garan-open');
            if (!body) {
                return;
            }
            if (body.getAttribute('data-src') === url) {
                openDialog(gd);
                return;
            }
            fetch(url, {credentials: 'same-origin'}).then(function (r) {
                return r.ok ? r.text() : '';
            }).then(function (svg) {
                body.innerHTML = svg;
                body.setAttribute('data-src', url);
                openDialog(gd);
            }).catch(function () {});
        } else if (el.hasAttribute('data-scp-dialog-close')) {
            var dd = el.closest('dialog');
            if (dd) {
                dd.close();
            }
        } else if (el.hasAttribute('data-scp-print')) {
            window.print();
        }
    });

    if (dlg && gpc) {
        dlg.classList.add('has-gpc');
    }
    if (!state) {
        show(false);
    }
    runScripts();
})();
