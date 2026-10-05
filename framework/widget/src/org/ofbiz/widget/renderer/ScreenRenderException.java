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

package org.ofbiz.widget.renderer;

import org.ofbiz.base.util.GeneralException;

/**
 * Wraps any exceptions encountered during the rendering of
 * a screen.  It is thrown to the top of the recursive
 * rendering process so that we avoid having to log redundant
 * exceptions.
 */
@SuppressWarnings("serial")
public class ScreenRenderException extends GeneralException {

    public ScreenRenderException() {
        super();
    }

    public ScreenRenderException(Throwable nested) {
        super(nested);
    }

    public ScreenRenderException(String str) {
        super(str);
    }

    public ScreenRenderException(String str, Throwable nested) {
        super(str, nested);
    }
}
