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
package org.ofbiz.base.util.collections;

import java.io.Serializable;
import java.util.Map;

import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.w3c.dom.Element;

/**
 * SCIPIO: Simple generic value accessor and utils, mainly used in xml models.
 */
@SuppressWarnings("serial")
public abstract class ValueAccessor implements Serializable {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final NullAccessor NULL_ACCESSOR = new NullAccessor();
    public static final ConstantAccessor BOOLEAN_TRUE_ACCESSOR = new ConstantAccessor(Boolean.TRUE);
    public static final ConstantAccessor BOOLEAN_FALSE_ACCESSOR = new ConstantAccessor(Boolean.FALSE);

    public abstract Object getValue(Map<String, Object> context);

    public abstract String getLogRepr();

    public static ValueAccessor getFieldOrExpanderAccessor(Element element, String fieldAttrName, String valueAttrName) {
        FlexibleMapAccessor<Object> fieldAccessor = null;
        FlexibleStringExpander valueExpander = null;

        String fieldVal = element.getAttribute(fieldAttrName);
        if (!fieldVal.isEmpty()) {
            fieldAccessor = FlexibleMapAccessor.getInstance(fieldVal);
        }
        String valueVal = element.getAttribute(valueAttrName);
        if (!valueVal.isEmpty()) {
            valueExpander = FlexibleStringExpander.getInstance(valueVal);
        }
        return getFieldOrExpanderAccessor(fieldAccessor, valueExpander);
    }

    /**
     * Simple switch between FlexibleMapAccessor and FlexibleStringExpander.
     */
    public static ValueAccessor getFieldOrExpanderAccessor(FlexibleMapAccessor<Object> fieldAccessor,
            FlexibleStringExpander valueExpander) {
        if (fieldAccessor != null && valueExpander != null) {
            throw new IllegalArgumentException("both field and value accessors cannot be specified at once");
        }
        if (fieldAccessor != null) {
            return new FieldAccessor(fieldAccessor);
        } else if (valueExpander != null) {
            return new ExpanderAccessor(valueExpander);
        } else {
            throw new IllegalArgumentException("no field or value accessor was specified");
        }
    }

    public static class FieldAccessor extends ValueAccessor {
        protected final FlexibleMapAccessor<Object> fieldAcsr;

        public FieldAccessor(FlexibleMapAccessor<Object> fieldAcsr) {
            this.fieldAcsr = fieldAcsr;
        }

        public Object getValue(Map<String, Object> context) {
            return fieldAcsr.get(context);
        }

        @Override
        public String getLogRepr() {
            return "context field '" + fieldAcsr.getOriginalName() + "'";
        }
    }

    public static class ExpanderAccessor extends ValueAccessor {
        protected final FlexibleStringExpander valueExdr;

        public ExpanderAccessor(FlexibleStringExpander valueExdr) {
            this.valueExdr = valueExdr;
        }

        public Object getValue(Map<String, Object> context) {
            return valueExdr.expand(context);
        }

        @Override
        public String getLogRepr() {
            return "value expression '" + valueExdr.getOriginal() + "'";
        }
    }

    public static final class ConstantAccessor extends ValueAccessor {
        protected final Object value;
        public ConstantAccessor(Object value) {
            this.value = value;
        }

        @Override
        public Object getValue(Map<String, Object> context) {
            return value;
        }

        @Override
        public String getLogRepr() {
            return "constant value '" + value + "' accessor";
        }
    }

    public static final class NullAccessor extends ValueAccessor {
        private NullAccessor() {} // singleton

        @Override
        public Object getValue(Map<String, Object> context) {
            return null;
        }

        @Override
        public String getLogRepr() {
            return "null accessor";
        }
    }
}