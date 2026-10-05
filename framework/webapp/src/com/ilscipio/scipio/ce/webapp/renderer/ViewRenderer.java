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

/**
 * Renders controller view definitions with support and emphasis for renderer from static context
 * such as services, and supports a service interface.
 * <p>For screen widgets, this functions like a wrapper around ScreenRenderer to help support request emulation when needed
 * while reusing controller view definitions rather than binding the code to widget technology, similar to sendMailFromScreen.</p>
 * <p>SCIPIO: 2.1.0: Added for Hotwire/Turbo support (</p>
 */
public class ViewRenderer {

    private static final ViewRenderer DEFAULT = new ViewRenderer();

    public static ViewRenderer getDefault() {
        return DEFAULT;
    }




}
