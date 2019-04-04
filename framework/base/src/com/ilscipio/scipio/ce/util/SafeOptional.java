package com.ilscipio.scipio.ce.util;

import java.io.Serializable;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;

/**
 * Emulation of Java 8's {@link java.util.Optional}.
 * <p>
 * 2016: ABSOLUTELY REQUIRED in many cms data object classes to record lookups that returned
 * null values - to ensure preloadContent actually works properly and
 * don't get dangerous stray database lookups during render (which violate
 * the instance's caching and validity).
 */
@SuppressWarnings("serial")
public class SafeOptional<A> implements Serializable {
    private static final SafeOptional<?> EMPTY = new SafeOptional<>(null);

    private final A value;

    private SafeOptional(A value) {
        this.value = value;
    }

    public static <T> SafeOptional<T> of(T value) {
        return new SafeOptional<>(value);
    }

    public static <T> SafeOptional<T> ofNullable(T value) {
        return value == null ? empty() : of(value);
    }

    public static <T> SafeOptional<T> empty() {
        @SuppressWarnings("unchecked")
        SafeOptional<T> empty = (SafeOptional<T>) EMPTY;
        return empty;
    }

    public boolean isPresent() {
        return value != null;
    }

    public A get() {
        if (value == null) {
            throw new NoSuchElementException("No value present");
        }
        return value;
    }

    public A orElse(A other) {
        return value != null ? value : other;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof SafeOptional)) {
            return false;
        }

        SafeOptional<?> other = (SafeOptional<?>) obj;
        return Objects.equals(value, other.value);
    }
}