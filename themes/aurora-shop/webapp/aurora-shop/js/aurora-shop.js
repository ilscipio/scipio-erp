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
 * SCIPIO: 4.0.0: Aurora Shop - small storefront behaviours: full-bleed width, mobile menu, hero carousel,
 * product rails, product gallery with zoom and lightbox.
 * No dependencies. Respects prefers-reduced-motion (no auto-advance).
 */
(function () {
    'use strict';

    // Viewport width without the scroll bar, for full-bleed sections (CSS --as-vw; 100vw would add a sideways scroll)
    var root = document.documentElement;
    var setVw = function () { root.style.setProperty('--as-vw', root.clientWidth + 'px'); };
    setVw();
    window.addEventListener('resize', setVw);

    var de = (root.lang || navigator.language || '').toLowerCase().indexOf('de') === 0;
    var T = de ? {close: 'Schließen', prev: 'Vorheriges Bild', next: 'Nächstes Bild', zoom: 'Bild vergrößern', of: 'von', images: 'Produktbilder'}
               : {close: 'Close', prev: 'Previous image', next: 'Next image', zoom: 'Enlarge image', of: 'of', images: 'Product images'};
    var ICON = {
        close: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M6 6l12 12M18 6 6 18"/></svg>',
        prev: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M19 12H5M11 6l-6 6 6 6"/></svg>',
        next: '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M5 12h14M13 6l6 6-6 6"/></svg>',
        zoom: '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><circle cx="11" cy="11" r="7"/><path d="m20 20-3.5-3.5M11 8v6M8 11h6"/></svg>'
    };

    // Mobile menu
    document.addEventListener('click', function (ev) {
        var btn = ev.target.closest ? ev.target.closest('[data-as-toggle]') : null;
        if (!btn) {
            return;
        }
        var target = document.getElementById(btn.getAttribute('data-as-toggle'));
        if (target) {
            var open = target.classList.toggle('is-open');
            btn.setAttribute('aria-expanded', open ? 'true' : 'false');
        }
    });

    // Hero carousel
    var reduced = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    var hero = document.querySelector('[data-as-carousel]');
    if (hero) {
        var slides = hero.querySelectorAll('.as-slide');
        var dotsBox = hero.querySelector('.as-dots');
        var current = 0;
        var timer = null;
        var paused = reduced;
        var dots = [];
        for (var i = 0; i < slides.length; i++) {
            var d = document.createElement('button');
            d.type = 'button';
            d.setAttribute('aria-label', (slides[i].getAttribute('aria-label') || ('Slide ' + (i + 1))));
            d.appendChild(document.createElement('span'));
            (function (n) { d.addEventListener('click', function () { show(n); restart(); }); })(i);
            dotsBox.appendChild(d);
            dots.push(d);
        }
        var show = function (n) {
            current = (n + slides.length) % slides.length;
            for (var j = 0; j < slides.length; j++) {
                var on = j === current;
                slides[j].hidden = !on;
                slides[j].classList.toggle('is-active', on);
                dots[j].setAttribute('aria-current', on ? 'true' : 'false');
                dots[j].classList.toggle('is-done', j < current);
            }
        };
        var restart = function () {
            if (timer) {
                clearInterval(timer);
                timer = null;
            }
            if (!paused && slides.length > 1) {
                timer = setInterval(function () { show(current + 1); }, 6000);
            }
        };
        var prev = hero.querySelector('[data-as-prev]');
        var next = hero.querySelector('[data-as-next]');
        var pause = hero.querySelector('[data-as-pause]');
        if (prev) { prev.addEventListener('click', function () { show(current - 1); restart(); }); }
        if (next) { next.addEventListener('click', function () { show(current + 1); restart(); }); }
        if (pause) {
            pause.setAttribute('aria-pressed', paused ? 'true' : 'false');
            pause.addEventListener('click', function () {
                paused = !paused;
                pause.setAttribute('aria-pressed', paused ? 'true' : 'false');
                hero.classList.toggle('is-paused', paused);
                restart();
            });
        }
        hero.classList.toggle('is-paused', paused);
        if (slides.length < 2) {
            hero.querySelector('.as-hero-controls').hidden = true;
        }
        show(0);
        restart();
    }

    // Product rails
    document.addEventListener('click', function (ev) {
        var btn = ev.target.closest ? ev.target.closest('[data-as-rail-prev],[data-as-rail-next]') : null;
        if (!btn) {
            return;
        }
        var rail = document.getElementById(btn.getAttribute('data-as-rail-prev') || btn.getAttribute('data-as-rail-next'));
        if (rail) {
            var dir = btn.hasAttribute('data-as-rail-next') ? 1 : -1;
            rail.scrollBy({left: dir * rail.clientWidth * 0.8, behavior: reduced ? 'auto' : 'smooth'});
        }
    });

    // Product gallery: thumbnails swap the main image, the main image zooms under the pointer,
    // a click opens a full-screen lightbox (zoom, arrows, swipe, keys). Without JS the thumbnails link to the images.
    var mainBox = document.querySelector('#productdetail .product-image');
    var mainImg = mainBox ? mainBox.querySelector('img') : null;
    if (mainImg) {
        var thumbList = document.querySelector('#productdetail .product-image-thumbs ul');
        var srcs = [mainImg.getAttribute('src')];
        var addSrc = function (u) { if (u && srcs.indexOf(u) < 0) { srcs.push(u); } };
        if (thumbList) {
            var li0 = document.createElement('li');
            var a0 = document.createElement('a');
            var i0 = document.createElement('img');
            a0.href = srcs[0];
            i0.src = srcs[0];
            i0.alt = '';
            i0.className = 'scipio-image';
            var box0 = document.createElement('div');
            box0.className = 'scipio-image-container';
            a0.appendChild(i0);
            box0.appendChild(a0);
            li0.appendChild(box0);
            thumbList.insertBefore(li0, thumbList.firstChild);
            var tl = thumbList.querySelectorAll('a[href]');
            for (var t = 1; t < tl.length; t++) { addSrc(tl[t].getAttribute('href')); }
        }
        var thumbs = thumbList ? thumbList.querySelectorAll('a[href]') : [];
        var cur = 0;
        var select = function (n) {
            cur = (n + srcs.length) % srcs.length;
            mainImg.src = srcs[cur];
            for (var k = 0; k < thumbs.length; k++) { thumbs[k].setAttribute('aria-current', k === cur ? 'true' : 'false'); }
        };
        for (var q = 0; q < thumbs.length; q++) {
            (function (n) {
                thumbs[n].addEventListener('click', function (ev) { ev.preventDefault(); select(n); });
            })(q);
        }
        if (thumbs.length) { thumbs[0].setAttribute('aria-current', 'true'); }

        mainBox.setAttribute('role', 'button');
        mainBox.setAttribute('tabindex', '0');
        mainBox.setAttribute('aria-label', T.zoom);
        var badge = document.createElement('span');
        badge.className = 'as-zoom-badge';
        badge.innerHTML = ICON.zoom;
        mainBox.appendChild(badge);
        var fine = window.matchMedia && window.matchMedia('(hover: hover) and (pointer: fine)').matches;
        var follow = function (img, box, ev) {
            var r = box.getBoundingClientRect();
            img.style.transformOrigin = ((ev.clientX - r.left) / r.width * 100) + '% ' + ((ev.clientY - r.top) / r.height * 100) + '%';
        };
        if (fine) {
            mainBox.addEventListener('mouseenter', function () { mainBox.classList.add('is-zooming'); });
            mainBox.addEventListener('mouseleave', function () { mainBox.classList.remove('is-zooming'); mainImg.style.transformOrigin = ''; });
            mainBox.addEventListener('mousemove', function (ev) { follow(mainImg, mainBox, ev); });
        }

        var dlg = document.createElement('dialog');
        if (dlg.showModal) {
            dlg.className = 'as-lightbox';
            dlg.setAttribute('aria-label', T.images);
            dlg.innerHTML = '<div class="as-lb-stage"><img class="as-lb-img" alt="" draggable="false"/></div>'
                + '<button type="button" class="as-lb-btn as-lb-close" aria-label="' + T.close + '">' + ICON.close + '</button>'
                + (srcs.length > 1 ? '<button type="button" class="as-lb-btn as-lb-prev" aria-label="' + T.prev + '">' + ICON.prev + '</button>'
                    + '<button type="button" class="as-lb-btn as-lb-next" aria-label="' + T.next + '">' + ICON.next + '</button>' : '')
                + '<p class="as-lb-count" aria-live="polite"></p>';
            document.body.appendChild(dlg);
            var stage = dlg.querySelector('.as-lb-stage');
            var lbImg = dlg.querySelector('.as-lb-img');
            var count = dlg.querySelector('.as-lb-count');
            var zoomed = false;
            var setZoom = function (on, ev) {
                zoomed = on;
                dlg.classList.toggle('is-zoomed', on);
                if (on && ev) { follow(lbImg, lbImg, ev); } else { lbImg.style.transformOrigin = ''; }
            };
            var showLb = function (n) {
                cur = (n + srcs.length) % srcs.length;
                setZoom(false);
                lbImg.src = srcs[cur];
                count.textContent = srcs.length > 1 ? (cur + 1) + ' ' + T.of + ' ' + srcs.length : '';
                select(cur);
            };
            var openLb = function () {
                showLb(cur);
                root.classList.add('as-lb-open');
                dlg.showModal();
            };
            dlg.addEventListener('close', function () { root.classList.remove('as-lb-open'); setZoom(false); mainBox.focus(); });
            mainBox.addEventListener('click', openLb);
            mainBox.addEventListener('keydown', function (ev) {
                if (ev.key === 'Enter' || ev.key === ' ') { ev.preventDefault(); openLb(); }
            });
            dlg.querySelector('.as-lb-close').addEventListener('click', function () { dlg.close(); });
            if (srcs.length > 1) {
                dlg.querySelector('.as-lb-prev').addEventListener('click', function () { showLb(cur - 1); });
                dlg.querySelector('.as-lb-next').addEventListener('click', function () { showLb(cur + 1); });
            }
            dlg.addEventListener('keydown', function (ev) {
                if (ev.key === 'ArrowLeft') { showLb(cur - 1); } else if (ev.key === 'ArrowRight') { showLb(cur + 1); }
            });
            // a click on the dark area closes; a click (or tap) on the image zooms in and out
            dlg.addEventListener('click', function (ev) { if (ev.target === dlg || ev.target === stage) { dlg.close(); } });
            var downX = null;
            var moved = false;
            lbImg.addEventListener('pointerdown', function (ev) { downX = ev.clientX; moved = false; });
            lbImg.addEventListener('pointermove', function (ev) {
                if (zoomed) { follow(lbImg, lbImg, ev); }
                if (downX !== null && Math.abs(ev.clientX - downX) > 8) { moved = true; }
            });
            lbImg.addEventListener('pointerup', function (ev) {
                var dx = downX === null ? 0 : ev.clientX - downX;
                downX = null;
                if (!moved) {
                    setZoom(!zoomed, ev);
                } else if (!zoomed && Math.abs(dx) > 50 && srcs.length > 1) {
                    showLb(cur + (dx < 0 ? 1 : -1));
                }
            });
        }
    }

    // ---- Widgets that the shared macros emit (tabs, modals, dropdowns, alerts, password reveal, field groups).
    // The backend theme drives them in aurora.js; the shop does not load that file (it also holds the back-office shell).
    var ACTIVE = 'is-active';
    var all = function (sel, parent) { return Array.prototype.slice.call((parent || document).querySelectorAll(sel), 0); };

    var openModal = function (el) {
        if (!el) { return; }
        el.classList.add(ACTIVE);
        el.setAttribute('aria-hidden', 'false');
        root.classList.add('as-modal-open');
        var focusable = el.querySelector('input, select, textarea, button:not(.modal-close), a[href]');
        if (focusable) { focusable.focus(); }
    };
    var closeModal = function (el) {
        if (!el) { return; }
        el.classList.remove(ACTIVE);
        el.setAttribute('aria-hidden', 'true');
        if (!document.querySelector('.modal.' + ACTIVE)) { root.classList.remove('as-modal-open'); }
    };
    document.addEventListener('click', function (ev) {
        if (!ev.target.closest) { return; }
        var trigger = ev.target.closest('[data-toggle="modal"], .js-modal-trigger');
        if (trigger) {
            var modal = document.getElementById(trigger.getAttribute('data-target') || trigger.getAttribute('data-reveal-id') || '');
            if (modal) { ev.preventDefault(); openModal(modal); return; }
        }
        var closer = ev.target.closest('.modal-background, .modal-close, .modal .delete, .modal [data-dismiss]');
        if (closer) { ev.preventDefault(); closeModal(closer.closest('.modal')); return; }
        // a closable alert: the button sits first in the [data-alert] box
        var del = ev.target.closest('[data-alert] > .delete, .notification > .delete');
        if (del) {
            var box = del.parentNode;
            var wrap = box.parentNode && box.parentNode.children.length === 1 ? box.parentNode : box;
            wrap.parentNode.removeChild(wrap);
        }
    });
    document.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape') { all('.modal.' + ACTIVE).forEach(closeModal); }
    });

    // Tabs: the strip (.tabs) is followed by the holder of the panes (.tab-content); the n-th tab shows the n-th pane.
    all('.tabs').forEach(function (strip) {
        var items = all('li', strip);
        var holder = strip.nextElementSibling;
        var panes = holder ? all(':scope > .tab-content', holder) : [];
        if (!items.length || !panes.length) { return; }
        var activate = function (index, focus) {
            items.forEach(function (it, n) {
                it.classList.toggle(ACTIVE, n === index);
                var a = it.querySelector('a');
                if (a) {
                    a.setAttribute('aria-selected', n === index ? 'true' : 'false');
                    a.setAttribute('tabindex', n === index ? '0' : '-1');
                    if (n === index && focus) { a.focus(); }
                }
            });
            panes.forEach(function (p, n) { p.classList.toggle(ACTIVE, n === index); });
        };
        items.forEach(function (item, index) {
            var a = item.querySelector('a');
            if (a && panes[index] && panes[index].id) { a.setAttribute('aria-controls', panes[index].id); }
            item.addEventListener('click', function (ev) { ev.preventDefault(); activate(index); });
            item.addEventListener('keydown', function (ev) {
                if (ev.key === 'ArrowRight' || ev.key === 'ArrowLeft') {
                    ev.preventDefault();
                    activate((index + (ev.key === 'ArrowRight' ? 1 : items.length - 1)) % items.length, true);
                }
            });
        });
        var start = items.findIndex(function (it) { return it.classList.contains(ACTIVE); });
        activate(start >= 0 && start < panes.length ? start : 0);
    });

    // Dropdowns (menu buttons that are not opened on hover)
    var dropdowns = all('.dropdown:not(.is-hoverable), .button-dropdown:not(.is-hoverable)');
    dropdowns.forEach(function (el) {
        el.addEventListener('click', function (ev) {
            ev.stopPropagation();
            var wasOpen = el.classList.contains(ACTIVE);
            dropdowns.forEach(function (d) { d.classList.remove(ACTIVE); });
            if (!wasOpen) { el.classList.add(ACTIVE); }
        });
    });
    document.addEventListener('click', function () { dropdowns.forEach(function (d) { d.classList.remove(ACTIVE); }); });

    // Show or hide a password
    all('[data-au-reveal]').forEach(function (b) {
        var input = document.getElementById(b.getAttribute('data-au-reveal'));
        if (!input) { return; }
        b.addEventListener('click', function () {
            var show = input.type === 'password';
            input.type = show ? 'text' : 'password';
            b.setAttribute('aria-pressed', show ? 'true' : 'false');
        });
    });

    // A collapsible field group folds on the button in its legend
    all('.fieldgroup-body').forEach(function (body) {
        var button = body.parentNode && body.parentNode.querySelector('legend > .au-disclose');
        if (!button) { return; }
        button.addEventListener('click', function () {
            var open = body.style.display === 'none';
            body.style.display = open ? '' : 'none';
            button.setAttribute('aria-expanded', String(open));
        });
    });

    // Old templates still call Foundation or Bootstrap; these calls must do the right thing, not fail quietly
    var $ = window.jQuery;
    if ($ && $.fn) {
        if (!$.fn.foundation) {
            $.fn.foundation = function (component, action) {
                if (component === 'reveal') {
                    this.each(function () { (action === 'close' ? closeModal : openModal)(this.closest ? this.closest('.modal') || this : this); });
                }
                return this;
            };
        }
        if (!$.fn.modal) {
            $.fn.modal = function (action) {
                return this.each(function () {
                    var el = this.classList && this.classList.contains('modal') ? this : (this.closest ? this.closest('.modal') : null);
                    if (action === 'hide') { closeModal(el); } else { openModal(el); }
                });
            };
        }
        if (!$.fn.tab) {
            $.fn.tab = function () { return this.each(function () { if (this.click) { this.click(); } }); };
        }
    }
    // Inline inputs (phone number parts) carry their meaning only in the title: show it as the placeholder
    all('.as-main input.field-inline[title]:not([placeholder])').forEach(function (i) { i.placeholder = i.title; });
    // A click anywhere on an address card selects its radio
    document.addEventListener('click', function (ev) {
        var card = ev.target.closest ? ev.target.closest('.field') : null;
        if (!card || ev.target.closest('a, button, input, select, textarea, label')) { return; }
        var radio = card.querySelector(':scope > .control .addr-select-radio input[type=radio]');
        if (radio && !radio.checked) { radio.click(); }
    });

    // The account menu (details) closes on a click outside it and on Escape
    document.addEventListener('click', function (ev) {
        all('details.as-acct[open]').forEach(function (d) { if (!d.contains(ev.target)) { d.removeAttribute('open'); } });
    });
    document.addEventListener('keydown', function (ev) {
        if (ev.key === 'Escape') { all('details.as-acct[open]').forEach(function (d) { d.removeAttribute('open'); }); }
    });

    window.AuroraShop = {openModal: openModal, closeModal: closeModal};
})();
