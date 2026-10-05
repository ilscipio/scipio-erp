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
package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.List;

import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: EscapeMethods - impl for utilities.ftl escapeVal, escapeFull, etc. functions.
 */
public class EscapeMethod implements TemplateMethodModelEx {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        if (args == null || args.size() < 1 || args.size() > 3 ) {
            throw new TemplateModelException("Invalid number of arguments (expected: 1-3)");
        }
        throw new UnsupportedOperationException("TODO - not implemented");
    }

    public static class EscapeValMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeFullMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeFullUrlMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeMsgMethod extends EscapeMethod {
        // TODO
    }

    public static class EscapeEventMsgMethod extends EscapeMethod {
        // TODO
    }
}
