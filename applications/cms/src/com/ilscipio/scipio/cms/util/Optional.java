package com.ilscipio.scipio.cms.util;

import java.io.Serializable;
import java.util.NoSuchElementException;

/**
 * Emulation of Java 8's {@link java.util.Optional}.
 * <p>
 * 2016: ABSOLUTELY REQUIRED in many cms data object classes to record lookups that returned
 * null values - to ensure preloadContent actually works properly and
 * don't get dangerous stray database lookups during render (which violate
 * the instance's caching and validity).
 */
@SuppressWarnings("serial")
public class Optional<A> implements Serializable {
    private final A value;

    private Optional(A value) {
        this.value = value;
    }
    
    public static <T> Optional<T> ofNullable(T value) {
        return new Optional<T>(value);
    }
    
    public static <T> Optional<T> empty() {
        return new Optional<T>(null);
    }
    
    public boolean isPresent() {
        return value != null;
    }
    
    public A get() {
        if (value == null) {
            throw new NoSuchElementException("Tried to call Optional.get on empty value - check isPresent first or use orElse(null)");
        }
        return value;
    }
    
    public A orElse(A other) {
        return value != null ? value : other;
    }
}