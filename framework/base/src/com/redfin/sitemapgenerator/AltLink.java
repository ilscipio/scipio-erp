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
package com.redfin.sitemapgenerator;

public class AltLink { // SCIPIO: 3.0.0: Added
    String url;
    String rel;
    String lang;
    String namespace;

    public AltLink(String url) {
        this.url = url;
    }

    public String url() {
        return url;
    }

    public AltLink rel(String rel) {
        this.rel = rel;
        return this;
    }

    public String rel() {
        return rel;
    }

    public AltLink lang(String lang) {
        this.lang = lang;
        return this;
    }

    public String lang() {
        return lang;
    }

    public AltLink namespace(String namespace) {
        this.namespace = namespace;
        return this;
    }

    public String namespace() {
        return namespace;
    }

    public String toLangUrlString() {
        return lang() + "=" + url();
    }
}
