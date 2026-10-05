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
 * SCIPIO: 4.0.0: Aurora Shop - home sections (includes/sections.ftl): drop countdown, shop-the-room hotspots,
 * build-your-set picker, add to bag (additem POSTs, then getCartData for the bag count), copy promo code,
 * the section switch of "make it yours".
 * No dependencies. Texts come from data- attributes of the templates.
 */
(function () {
    'use strict';

    var reduced = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    // Full-bleed sections use --as-vw (aurora-shop.js sets it on resize). A scroll bar that appears later (images load,
    // the page grows) fires no resize event: follow the body width too, or the bands overflow by the scroll bar width.
    var root = document.documentElement;
    if (window.ResizeObserver) {
        new ResizeObserver(function () { root.style.setProperty('--as-vw', root.clientWidth + 'px'); }).observe(document.body);
    }

    // ---- Hero stage: slides change every few seconds; the bar of the current tab fills meanwhile. A pointer or the
    // focus in the stage holds the time; the pause button stops it; reduced motion: no autoplay ----
    Array.prototype.forEach.call(document.querySelectorAll('[data-as-stage]'), function (stage) {
        var slides = stage.querySelectorAll('.as-stage-slide');
        var tabs = stage.querySelectorAll('[data-as-go]');
        if (slides.length < 2) {
            return;
        }
        var ms = parseInt(stage.getAttribute('data-as-interval'), 10) || 7000;
        var pauseBtn = stage.querySelector('[data-as-stage-pause]');
        var cur = 0, timer = null, started = 0, remaining = ms, paused = reduced, hold = false;
        stage.style.setProperty('--as-stage-ms', ms + 'ms');
        var run = function () {
            if (paused || hold || timer) {
                return;
            }
            stage.classList.remove('is-paused');
            started = Date.now();
            timer = setTimeout(function () { timer = null; show(cur + 1); }, remaining);
        };
        var halt = function () {
            if (timer) {
                clearTimeout(timer);
                timer = null;
                remaining = Math.max(0, remaining - (Date.now() - started));
            }
            stage.classList.add('is-paused');
        };
        var show = function (i) {
            cur = (i + slides.length) % slides.length;
            Array.prototype.forEach.call(slides, function (s, k) {
                var on = k === cur;
                s.classList.toggle('is-active', on);
                s.setAttribute('aria-hidden', on ? 'false' : 'true');
                if (on) { s.removeAttribute('inert'); } else { s.setAttribute('inert', 'inert'); }
            });
            Array.prototype.forEach.call(tabs, function (t, k) {
                t.setAttribute('aria-selected', k === cur ? 'true' : 'false');
                t.tabIndex = k === cur ? 0 : -1;
                t.classList.remove('is-running');
            });
            stage.setAttribute('data-as-tone', slides[cur].getAttribute('data-as-tone'));
            if (timer) { clearTimeout(timer); timer = null; }
            remaining = ms;
            if (!paused) {
                void tabs[cur].offsetWidth; // restarts the bar animation
                tabs[cur].classList.add('is-running');
            }
            if (paused || hold) { stage.classList.add('is-paused'); } else { run(); }
        };
        Array.prototype.forEach.call(tabs, function (t, k) {
            t.addEventListener('click', function () { show(k); });
            t.addEventListener('keydown', function (ev) {
                var d = ev.key === 'ArrowRight' ? 1 : ev.key === 'ArrowLeft' ? -1 : 0;
                if (d) { ev.preventDefault(); show(cur + d); tabs[cur].focus(); }
            });
        });
        var prev = stage.querySelector('[data-as-stage-prev]');
        var next = stage.querySelector('[data-as-stage-next]');
        if (prev) { prev.addEventListener('click', function () { show(cur - 1); }); }
        if (next) { next.addEventListener('click', function () { show(cur + 1); }); }
        if (pauseBtn) {
            pauseBtn.setAttribute('aria-pressed', paused ? 'true' : 'false');
            pauseBtn.addEventListener('click', function () {
                paused = !paused;
                pauseBtn.setAttribute('aria-pressed', paused ? 'true' : 'false');
                if (paused) {
                    halt();
                } else {
                    if (!tabs[cur].classList.contains('is-running')) { show(cur); } else { run(); }
                }
            });
        }
        // keyboard focus holds the time; the focus a mouse click leaves on a button does not
        var keyFocus = function () {
            var a = document.activeElement;
            return !!a && stage.contains(a) && a.matches(':focus-visible');
        };
        stage.addEventListener('mouseenter', function () { hold = true; halt(); });
        stage.addEventListener('mouseleave', function () { hold = keyFocus(); run(); });
        stage.addEventListener('focusin', function (ev) {
            if (ev.target.matches(':focus-visible')) { hold = true; halt(); }
        });
        stage.addEventListener('focusout', function (ev) {
            if (!stage.contains(ev.relatedTarget)) { hold = stage.matches(':hover'); run(); }
        });
        var x0 = null;
        stage.addEventListener('touchstart', function (ev) { x0 = ev.touches[0].clientX; }, { passive: true });
        stage.addEventListener('touchend', function (ev) {
            if (x0 === null) { return; }
            var dx = ev.changedTouches[0].clientX - x0;
            x0 = null;
            if (Math.abs(dx) > 50) { show(cur + (dx < 0 ? 1 : -1)); }
        });
        stage.setAttribute('data-as-tone', slides[0].getAttribute('data-as-tone'));
        show(0);
    });

    // ---- Countdown ----
    var cds = document.querySelectorAll('[data-as-countdown]');
    Array.prototype.forEach.call(cds, function (cd) {
        var end = Date.parse(cd.getAttribute('data-as-countdown'));
        if (isNaN(end)) {
            return;
        }
        var cell = function (k) { return cd.querySelector('[data-as-cd="' + k + '"]'); };
        var pad = function (n) { return (n < 10 ? '0' : '') + n; };
        var tick = function () {
            var left = Math.max(0, Math.floor((end - Date.now()) / 1000));
            cell('d').textContent = Math.floor(left / 86400);
            cell('h').textContent = pad(Math.floor(left % 86400 / 3600));
            cell('m').textContent = pad(Math.floor(left % 3600 / 60));
            cell('s').textContent = pad(left % 60);
            if (left === 0) {
                clearInterval(timer);
                var band = cd.closest('.as-drop');
                if (band) { band.classList.add('is-live'); }
            }
        };
        var timer = setInterval(tick, 1000);
        tick();
    });

    // ---- Toast (role=status) ----
    var toast = null;
    var showToast = function (text, linkText, href, bad) {
        if (!toast) {
            toast = document.createElement('div');
            toast.className = 'as-toast';
            toast.setAttribute('role', 'status');
            toast.setAttribute('aria-live', 'polite');
            document.body.appendChild(toast);
        }
        toast.innerHTML = '';
        var t = document.createElement('span');
        t.textContent = text;
        toast.appendChild(t);
        if (linkText && href) {
            var a = document.createElement('a');
            a.href = href;
            a.textContent = linkText;
            toast.appendChild(a);
        }
        toast.classList.toggle('is-bad', !!bad);
        toast.classList.add('is-on');
        clearTimeout(showToast.timer);
        showToast.timer = setTimeout(function () { toast.classList.remove('is-on'); }, 6000);
    };

    // ---- Bag count in the store header ----
    var setBagCount = function (n) {
        var bag = document.querySelector('.as-bag');
        if (!bag) {
            return;
        }
        var badge = bag.querySelector('.as-bag-count');
        if (n > 0) {
            if (!badge) {
                badge = document.createElement('span');
                badge.className = 'as-bag-count';
                bag.appendChild(badge);
            }
            badge.textContent = n;
        } else if (badge) {
            badge.parentNode.removeChild(badge);
        }
        var label = bag.getAttribute('aria-label') || '';
        bag.setAttribute('aria-label', label.replace(/\(\d+\)/, '(' + n + ')'));
    };
    var cartCount = function (url) {
        return fetch(url, {credentials: 'same-origin', headers: {'Accept': 'application/json'}})
            .then(function (r) { return r.text(); })
            .then(function (t) {
                var data = JSON.parse(t.replace(/^\s*\/\//, ''));
                return Number(data.totalQuantity || 0);
            });
    };

    // ---- Add products to the bag, one additem POST each ----
    var addToBag = function (btn, ids) {
        var addUrl = btn.getAttribute('data-as-add-url');
        var cartUrl = btn.getAttribute('data-as-cart-url');
        if (!addUrl || !cartUrl || !ids.length || !window.fetch) {
            return;
        }
        btn.disabled = true;
        btn.setAttribute('aria-busy', 'true');
        var before = 0;
        var chain = cartCount(cartUrl).then(function (n) { before = n; }, function () { before = 0; });
        ids.forEach(function (id) {
            chain = chain.then(function () {
                var body = new URLSearchParams();
                body.set('add_product_id', id);
                body.set('quantity', '1');
                return fetch(addUrl, {method: 'POST', credentials: 'same-origin', body: body});
            });
        });
        chain.then(function () { return cartCount(cartUrl); })
            .then(function (after) {
                setBagCount(after);
                var ok = after - before >= ids.length;
                showToast(btn.getAttribute(ok ? 'data-as-msg-ok' : 'data-as-msg-fail'), btn.getAttribute('data-as-msg-bag'),
                    btn.getAttribute('data-as-bag-url'), !ok);
            }, function () {
                showToast(btn.getAttribute('data-as-msg-fail'), null, null, true);
            })
            .then(function () {
                btn.removeAttribute('aria-busy');
                btn.disabled = btn.hasAttribute('data-as-set-add') ? !setFull(btn.closest('[data-as-set]')) : false;
            });
    };
    document.addEventListener('click', function (ev) {
        var btn = ev.target.closest ? ev.target.closest('[data-as-add]') : null;
        if (btn) {
            addToBag(btn, btn.getAttribute('data-as-add').split(',').filter(Boolean));
        }
    });

    // ---- Shop the room: a dot shows its product in the panel ----
    Array.prototype.forEach.call(document.querySelectorAll('[data-as-room]'), function (room) {
        var spots = room.querySelectorAll('[data-as-spot]');
        var items = room.querySelectorAll('[data-as-spot-item]');
        var select = function (n) {
            Array.prototype.forEach.call(spots, function (s) { s.setAttribute('aria-pressed', s.getAttribute('data-as-spot') === n ? 'true' : 'false'); });
            Array.prototype.forEach.call(items, function (it) {
                var on = it.getAttribute('data-as-spot-item') === n;
                it.classList.toggle('is-active', on);
                if (on && window.innerWidth < 720) {
                    it.scrollIntoView({block: 'nearest', behavior: reduced ? 'auto' : 'smooth'});
                }
            });
        };
        Array.prototype.forEach.call(spots, function (s) {
            s.addEventListener('click', function () { select(s.getAttribute('data-as-spot')); });
        });
        Array.prototype.forEach.call(items, function (it) {
            it.addEventListener('mouseenter', function () { select(it.getAttribute('data-as-spot-item')); });
        });
    });

    // ---- Build your set: pick exactly N ----
    var setFull = function (set) {
        if (!set) {
            return false;
        }
        var n = Number(set.getAttribute('data-as-set'));
        return set.querySelectorAll('[data-as-set-item]:checked').length === n;
    };
    Array.prototype.forEach.call(document.querySelectorAll('[data-as-set]'), function (set) {
        var n = Number(set.getAttribute('data-as-set'));
        var boxes = set.querySelectorAll('[data-as-set-item]');
        var count = set.querySelector('[data-as-set-count]');
        var add = set.querySelector('[data-as-set-add]');
        var update = function () {
            var picked = set.querySelectorAll('[data-as-set-item]:checked').length;
            count.textContent = picked;
            Array.prototype.forEach.call(boxes, function (b) {
                b.disabled = !b.checked && picked >= n;
                b.closest('.as-set-item').classList.toggle('is-picked', b.checked);
            });
            add.disabled = picked !== n;
        };
        Array.prototype.forEach.call(boxes, function (b) { b.addEventListener('change', update); });
        add.addEventListener('click', function () {
            var ids = Array.prototype.filter.call(boxes, function (b) { return b.checked; }).map(function (b) { return b.value; });
            addToBag(add, ids);
        });
        update();
    });

    // ---- Copy a promo code ----
    document.addEventListener('click', function (ev) {
        var btn = ev.target.closest ? ev.target.closest('[data-as-copy]') : null;
        if (!btn || !navigator.clipboard) {
            return;
        }
        var label = btn.textContent;
        navigator.clipboard.writeText(btn.getAttribute('data-as-copy')).then(function () {
            btn.textContent = btn.getAttribute('data-as-copied') || label;
            setTimeout(function () { btn.textContent = label; }, 2000);
        });
    });

    // ---- Section switch (asYours): the sections of the page (CMS slot wrappers) switch off and on in this browser ----
    Array.prototype.forEach.call(document.querySelectorAll('[data-as-switch]'), function (sec) {
        var box = sec.querySelector('.as-switch');
        var list = box && box.querySelector('.as-switch-list');
        // the section of the switch itself and slots that render nothing are left out
        var slots = Array.prototype.filter.call(document.querySelectorAll('.as-slot[data-as-slot]'), function (s) {
            return !s.contains(sec) && s.firstElementChild;
        });
        if (!list || slots.length < 2) {
            return;
        }
        var map = sec.querySelector('.as-yours-map');
        var count = box.querySelector('[data-as-switch-count]');
        var shownText = box.getAttribute('data-as-shown') || '{0} / {1}';
        var bars = [];
        var update = function () {
            var on = slots.filter(function (s) { return !s.hidden; }).length;
            count.textContent = shownText.replace('{0}', on).replace('{1}', slots.length);
        };
        map.textContent = '';
        slots.forEach(function (s, i) {
            var id = 'as-switch-' + s.getAttribute('data-as-slot');
            var li = document.createElement('li');
            li.innerHTML = '<label class="as-toggle" for="' + id + '"><input type="checkbox" role="switch" checked="checked" id="' + id + '"/>'
                + '<span class="as-toggle-ui" aria-hidden="true"></span><span class="as-toggle-name"></span></label>';
            li.querySelector('.as-toggle-name').textContent = s.getAttribute('data-as-slot-label') || s.getAttribute('data-as-slot');
            var bar = document.createElement('i');
            map.appendChild(bar);
            bars.push(bar);
            li.querySelector('input').addEventListener('change', function (ev) {
                s.hidden = !ev.target.checked;
                bar.classList.toggle('is-off', s.hidden);
                update();
            });
            list.appendChild(li);
        });
        box.querySelector('[data-as-switch-reset]').addEventListener('click', function () {
            Array.prototype.forEach.call(list.querySelectorAll('input'), function (inp, i) {
                inp.checked = true;
                slots[i].hidden = false;
                bars[i].classList.remove('is-off');
            });
            update();
        });
        box.hidden = false;
        sec.classList.add('has-switch');
        update();
    });

    // ---- Rules by region (asRules): tabs; arrow keys, Home and End move between them ----
    Array.prototype.forEach.call(document.querySelectorAll('[data-as-rules]'), function (sec) {
        var tabs = Array.prototype.slice.call(sec.querySelectorAll('[role="tab"]'));
        function select(tab, focus) {
            tabs.forEach(function (t) {
                var on = t === tab;
                t.setAttribute('aria-selected', on ? 'true' : 'false');
                t.tabIndex = on ? 0 : -1;
                var panel = document.getElementById(t.getAttribute('aria-controls'));
                if (panel) { panel.hidden = !on; }
            });
            if (focus) { tab.focus(); }
        }
        tabs.forEach(function (tab, i) {
            tab.addEventListener('click', function () { select(tab, false); });
            tab.addEventListener('keydown', function (ev) {
                var next = { ArrowRight: i + 1, ArrowLeft: i - 1, Home: 0, End: tabs.length - 1 }[ev.key];
                if (next === undefined) { return; }
                ev.preventDefault();
                select(tabs[(next + tabs.length) % tabs.length], true);
            });
        });
    });
})();
