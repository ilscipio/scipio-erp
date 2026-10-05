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
 * Requires turbo.js
 */

/* Turbo 8 prefetches a link on hover. A GET link in the backend can run an action, so prefetch is off. */
document.addEventListener("turbo:before-prefetch", function(event) { event.preventDefault(); });

/** Defines <turbo-stream-connection/> element */
class TurboStreamConnection extends HTMLElement {
    get src() {
        return this.getAttribute("src");
    }

    set src(value) {
        if (value) {
            this.setAttribute("src", value);
        } else {
            this.removeAttribute("src");
        }
    }

    connectedCallback() {
        Turbo.connectStreamSource(this);
        this.ws = this.connectWebSocket();
    }

    disconnectedCallback() {
        Turbo.disconnectStreamSource(this);
        if (this.ws) {
            this.ws.close();
            this.ws = null;
        }
    }

    /**
     * Called in response to a websocket message. Unpacks the websocket message
     * and dispatches it as a new MessageEvent to Turbo Streams.
     *
     * @param {MessageEvent} messageEvent The original message to dispatch
     */
    dispatchMessageEvent(messageEvent) {
        const event = new MessageEvent("message", { data: messageEvent.data });
        this.dispatchEvent(event);
    }

    connectWebSocket() {
        const socketLocation = `wss://${window.location.host}${this.src}`;
        const ws = new WebSocket(socketLocation);
        ws.onmessage = msg => this.dispatchMessageEvent(msg);
        return ws;
    }
}

customElements.define("turbo-stream-connection", TurboStreamConnection);
