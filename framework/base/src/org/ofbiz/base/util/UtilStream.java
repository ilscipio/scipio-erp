package org.ofbiz.base.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * SCIPIO: Helpers for java lambda/collectors/stream API, to supplement the java stream API.
 */
public final class UtilStream {

    private UtilStream() {
    }

    public static <T> Collector<T, ?, List<T>> toArrayList() {
        return Collectors.toList();
    }

    public static <T> Collector<T, ?, List<T>> toArrayList(int initialCapacity) {
        return Collectors.toCollection(() -> new ArrayList<T>());
    }
}
