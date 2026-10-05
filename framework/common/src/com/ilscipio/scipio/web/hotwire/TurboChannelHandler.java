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
package com.ilscipio.scipio.web.hotwire;

import com.ilscipio.scipio.web.WebChannelHandler;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;

import java.util.List;

public class TurboChannelHandler extends WebChannelHandler {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final TurboChannelHandler DEFAULT = new TurboChannelHandler("default");
    protected static final List<WebChannelHandler> DEFAULT_LIST = UtilMisc.unmodifiableArrayList(new TurboChannelHandler("turbo"));

    public static WebChannelHandler getDefault() {
        return DEFAULT;
    }

    public static List<WebChannelHandler> getDefaultAsList() {
        return DEFAULT_LIST;
    }

    public TurboChannelHandler(String name) {
        super(name);
    }

    @Override
    public Object onMessage(Args args) {
        return false;
    }
}
