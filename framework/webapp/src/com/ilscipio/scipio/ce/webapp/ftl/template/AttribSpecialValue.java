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
package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Used to represent special attribs map values that fall outside of what can/should be
 * represented by a string.
 */
public abstract class AttribSpecialValue {

    private static final NoneValue noneValue = new NoneValue();
    private static final EmptyValue emptyValue = new EmptyValue();

    private static final Map<String, AttribSpecialValue> typeNameMap;
    static {
        Map<String, AttribSpecialValue> map = new HashMap<>();
        map.put("none", noneValue);
        map.put("empty", emptyValue);
        typeNameMap = map;
    }

    protected AttribSpecialValue() {
    }

    public abstract String getTypeName();
    public abstract boolean isSameType(Object other);

    public static class NoneValue extends AttribSpecialValue {
        protected NoneValue() {

        }

        @Override
        public String toString() {
            return ""; // WARN: we dont want to return null here, though it would be more appropriate
        }

        @Override
        public String getTypeName() {
            return "none";
        }

        @Override
        public boolean isSameType(Object other) {
            return (other instanceof NoneValue);
        }
    }

    public static class EmptyValue extends AttribSpecialValue {
        protected EmptyValue() {
        }

        @Override
        public String toString() {
            return "";
        }

        @Override
        public String getTypeName() {
            return "empty";
        }

        @Override
        public boolean isSameType(Object other) {
            return (other instanceof EmptyValue);
        }
    }

    @SuppressWarnings("unchecked")
    public static <T extends AttribSpecialValue> T getSpecialValue(String typeName) {
        return (T) typeNameMap.get(typeName);
    }

    public static NoneValue getNoneValue() {
        return noneValue;
    }

    public static EmptyValue getEmptyValue() {
        return emptyValue;
    }

    public static boolean isSpecialValue(Object object) {
        return (object instanceof AttribSpecialValue);
    }

    public static boolean isSpecialValue(Object object, String typeName) {
        if (typeName == null || typeName.isEmpty()) {
            return (object instanceof AttribSpecialValue);
        } else {
            AttribSpecialValue type = typeNameMap.get(typeName);
            return (type != null) && type.isSameType(object);
        }
    }

    public static boolean isNoneValue(Object object) {
        return (object instanceof NoneValue);
    }

    public static boolean isEmptyValue(Object object) {
        return (object instanceof EmptyValue);
    }

    public static Map<String, AttribSpecialValue> getTypeNameMap() {
        return Collections.unmodifiableMap(typeNameMap);
    }
}
