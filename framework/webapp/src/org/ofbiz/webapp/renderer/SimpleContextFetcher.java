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
package org.ofbiz.webapp.renderer;

import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.collections.RenderMapStack;

/**
 * SCIPIO: Simple context fetcher that always returns the initial ones.
 * This is stock Ofbiz behavior (for ScreenRenderer).
 */
public class SimpleContextFetcher implements RenderContextFetcher {
    protected Appendable writer;
    protected MapStack<String> context;

    public SimpleContextFetcher(Appendable writer, MapStack<String> context) {
        this.writer = writer;
        if (context == null) context = RenderMapStack.createRenderContext(); // SCIPIO: Dedicated context class: MapStack.create();
        this.context = context;
    }

    @Override
    public MapStack<String> getContext() {
        return context;
    }

    @Override
    public Appendable getWriter() {
        return writer;
    }
}