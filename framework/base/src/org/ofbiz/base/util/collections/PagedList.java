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

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * Stores the result of a subset of data items from a source (such
 * as EntityListIterator).
 */
public class PagedList<E> implements Iterable<E> {

    // SCIPIO: fields now final
    protected final int startIndex;
    protected final int endIndex;
    protected final int size;
    protected final int viewIndex;
    protected final int viewSize;
    protected final List<E> data;

    /**
     * Default constructor - populates all fields in this class
     * @param startIndex
     * @param endIndex
     * @param size
     * @param viewIndex
     * @param viewSize
     * @param data
     */
    public PagedList(int startIndex, int endIndex, int size, int viewIndex, int viewSize, List<E> data) {
        this.startIndex = startIndex;
        this.endIndex = endIndex;
        this.size = size;
        this.viewIndex = viewIndex;
        this.viewSize = viewSize;
        this.data = data;
    }

    /**
     * Copy constructor with ability to override data (SCIPIO).
     */
    public PagedList(PagedList<?> other, List<E> data) {
        this.startIndex = other.startIndex;
        this.endIndex = other.endIndex;
        this.size = other.size;
        this.viewIndex = other.viewIndex;
        this.viewSize = other.viewSize;
        this.data = data;
    }

    /**
     * @param viewIndex
     * @param viewSize
     * @return an empty PagedList object
     */
    public static <E> PagedList<E> empty(int viewIndex, int viewSize) {
        List<E> emptyList = Collections.emptyList();
        return new PagedList<>(0, 0, 0, viewIndex, viewSize, emptyList);
    }

    /**
     * @return the start index (for paginator) or known as low index
     */
    public int getStartIndex() {
        return startIndex;
    }

    /**
     * @return the end index (for paginator) or known as high index
     */
    public int getEndIndex() {
        return endIndex;
    }

    /**
     * @return the size of the full list, this can be the
     * result of <code>EntityListIterator.getResultsSizeAfterPartialList()</code>
     */
    public int getListSize() {
        return size;
    }

    /**
     * @deprecated SCIPIO: Use {@link #getListSize()} instead.
     * @return the size of the full list, this can be the
     * result of <code>EntityListIterator.getResultsSizeAfterPartialList()</code>
     */
    @Deprecated
    public int getSize() {
        return getListSize();
    }

    /**
     * @return the paged data. Eg - the result from <code>EntityListIterator.getPartialList()</code>
     */
    public List<E> getData() {
        return data;
    }

    /**
     * @return the view index supplied by client
     */
    public int getViewIndex() {
        return viewIndex;
    }

    /**
     * @return the view size supplied by client
     */
    public int getViewSize() {
        return viewSize;
    }

    /**
     * @return an interator object over the data returned in getData() method
     *         of this class
     */
    @Override
    public Iterator<E> iterator() {
        return this.data.iterator();
    }

}
