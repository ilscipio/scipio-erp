package org.ofbiz.base.util;

import java.util.Iterator;

/**
 * Special tag interface for {@link java.util.Iterator} implementations that should not
 * call the {@link Iterator#hasNext()} method but should instead call {@link Iterator#next()} and check for null.
 * This mostly refers to EntityListIterator.
 */
public interface NextOnlyIterator {
}
