package org.ofbiz.base.util;

/**
 * Generic interface for any class with a {@link #getName()} method (SCIPIO).
 */
public interface NamedElement {
    String getName();

    static String getName(Object namedValue) {
        if (namedValue instanceof NamedElement) {
            return ((NamedElement) namedValue).getName();
        } else if (namedValue != null) {
            return namedValue.toString();
        } else {
            return null;
        }
    }
}
