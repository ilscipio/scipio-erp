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
package com.ilscipio.scipio.ce.webapp.renderer;

import java.io.Serializable;
import java.util.Map;

/**
 * Stores essential rendering information for a (future) ViewRenderer call.
 * <p>Needed to store webapp and store information </p>
 */
public class ViewContext implements Serializable {

    private final Map<String, Object> context;

    public ViewContext(Map<String, Object> context) {
        this.context = context;
    }

    public static ViewContext from(Map<String, Object> context) {
        return new ViewContext(context);
    }

    public Map<String, Object> getContext() {
        return context;
    }


}
