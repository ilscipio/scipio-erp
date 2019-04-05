package com.ilscipio.scipio.ce.util;

import java.io.Serializable;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;

/**
 * SCIPIO: Emulation of Java 8's {@link java.util.Optional} that implements Serializable.
 * <p>
 * 2016: ABSOLUTELY REQUIRED in many cms data object classes to record lookups that returned
 * null values - to ensure preloadContent actually works properly and
 * don't get dangerous stray database lookups during render (which violate
 * the instance's caching and validity).
 */
@SuppressWarnings("serial")
public class SafeOptional<A> implements Serializable {
    private static final SafeOptional<?> EMPTY = new SafeOptional<>();

    private final A value;

    private SafeOptional() {
        this.value = null;
    }

    private SafeOptional(A value) {
        this.value = Objects.requireNonNull(value);
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
        if (obj instanceof SafeOptional) {
            SafeOptional<?> other = (SafeOptional<?>) obj;
            return Objects.equals(value, other.value);
        } else if (obj instanceof Optional) {
            Optional<?> other = (Optional<?>) obj;
            return Objects.equals(value, other.orElse(null));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(value);
    }
}