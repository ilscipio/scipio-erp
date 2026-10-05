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
package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;

/**
 * SCIPIO: Class to represent an extra attribs expression expressed by a JSON-/FTL-like syntax. Immutable.
 * <p>
 * Can be used on most any widget elements, e.g.:
 * <pre>
 * <code>
 * &ltform ... attribs="{'key1': 'value1', 'key2':'value2'}"&gt
 * </code>
 * </pre>
 */
@SuppressWarnings("serial")
public class AttribsExpression implements Serializable {

    private final FlexibleStringExpander attribsExpr;

    protected AttribsExpression(FlexibleStringExpander attribsExpr) {
        super();
        this.attribsExpr = attribsExpr;
    }

    /**
     * SCIPIO: Builds an attrib expression for internal storage.
     */
    static AttribsExpression makeAttribsExpr(String attribsExprStr, AttribsExpression parentAttribsExpr) {
        FlexibleStringExpander attribsExpr;
        if (attribsExprStr.isEmpty()) {
            if (parentAttribsExpr != null) {
                attribsExpr = parentAttribsExpr.attribsExpr;
            }
            else {
                attribsExpr = FlexibleStringExpander.getInstance("");
            }
        }
        else {
            if (parentAttribsExpr != null) {
                attribsExpr = FlexibleStringExpander.getInstance(concatMapExpr(parentAttribsExpr.attribsExpr.getOriginal(), attribsExprStr));
            }
            else {
                attribsExpr = FlexibleStringExpander.getInstance(stripTrimMapExprDelims(attribsExprStr));
            }
        }
        return new AttribsExpression(attribsExpr);
    }

    static AttribsExpression makeAttribsExpr(String attribsExprStr) {
        return makeAttribsExpr(attribsExprStr, null);
    }

    static AttribsExpression makeAttribsExpr() {
        return makeAttribsExpr("", null);
    }

    /**
     * SCIPIO: Merges otherExpr into this one, producing a new.
     */
    public AttribsExpression putAll(AttribsExpression otherExpr) {
        String origStr = otherExpr.attribsExpr.getOriginal();
        if (UtilValidate.isNotEmpty(origStr)) {
            return new AttribsExpression(FlexibleStringExpander.getInstance(concatMapExpr(attribsExpr.getOriginal(), origStr)));
        }
        else {
            return new AttribsExpression(attribsExpr);
        }
    }

    /**
     * Compiles an attrib expression for usage in templates or other.
     */
    public String compile(Map<String, Object> context) {
        return "{" + attribsExpr.expandString(context) + "}";
    }

    protected static String compileAttribsExpr(FlexibleStringExpander attribsExpr, Map<String, Object> context) {
        return "{" + attribsExpr.expandString(context) + "}";
    }

    /**
     * Strips starting "{" and trailing "}" from a JSON-like map expression and trims.
     */
    protected static String stripTrimMapExprDelims(String mapExpr) {
        if (mapExpr != null && !mapExpr.isEmpty()) {
            Matcher m = Pattern.compile("^\\s*\\{\\s*(.*)\\s*\\}\\s*$").matcher(mapExpr);
            if (m.matches()) {
                mapExpr = m.group(1);
            }
            else {
                mapExpr = mapExpr.trim();
            }
            return mapExpr;
        }
        else {
            return "";
        }
    }

    /**
     * Combines two map expressions into one map expression.
     */
    protected static String concatMapExpr(String first, String second) {
        first = stripTrimMapExprDelims(first);
        second = stripTrimMapExprDelims(second);
        if (first.isEmpty()) {
            return second;
        }
        else if (second.isEmpty()) {
            return first;
        }
        else {
            return first + ", " + second;
        }
    }


}
