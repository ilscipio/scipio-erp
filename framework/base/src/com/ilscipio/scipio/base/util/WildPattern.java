package com.ilscipio.scipio.base.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Simple fast wildcard pattern matcher.
 *
 * <p>FIXME: Multi-wild support (greedy pattern)</p>
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public abstract class WildPattern implements Serializable {


    public static WildPattern compile(String pattern, char wildChar) {
        return compile(pattern, compileWildIndexes(pattern, wildChar));
    }

    public static WildPattern compile(String pattern, List<Integer> wildIndexes) {
        if (wildIndexes.size() == 0) {
            return new Exact(pattern);
        } else if (wildIndexes.size() == 1) {
            int wildIndex = wildIndexes.get(0);
            if (wildIndex == 0) {
                return new EndsWith(pattern.substring(1));
            } else if (wildIndex == (pattern.length() - 1)) {
                return new StartsWith(pattern.substring(0, pattern.length() - 1));
            } else {
                return new SurroundedWith(pattern.substring(0, wildIndex), pattern.substring(wildIndex + 1));
            }
        } else {
            // TODO: incomplete
            //return new Many(pattern, wildIndexes);
            throw new UnsupportedOperationException("Does not yet support multiple wildcards");
        }
    }

    public abstract boolean matches(String input);

    public static List<Integer> compileWildIndexes(String pattern, char wildChar) {
        List<Integer> wildIndexes = new ArrayList<>(pattern.length()); // max length
        int wildIndex = -1;
        while ((wildIndex = pattern.indexOf(wildChar, wildIndex + 1)) >= 0) {
            wildIndexes.add(wildIndex);
        }
        return wildIndexes;
    }

    public static abstract class Base extends WildPattern {
    }

    public static class Exact extends Base {
        protected final String pattern;

        public Exact(String pattern) {
            this.pattern = pattern;
        }

        @Override
        public boolean matches(String input) {
            return pattern.equals(input);
        }
    }

    public static class StartsWith extends Base {
        protected final String prefix;

        public StartsWith(String prefix) {
            this.prefix = prefix;
        }

        @Override
        public boolean matches(String input) {
            return input.startsWith(prefix);
        }
    }

    public static class EndsWith extends Base {
        protected final String suffix;

        public EndsWith(String suffix) {
            this.suffix = suffix;
        }

        @Override
        public boolean matches(String input) {
            return input.endsWith(suffix);
        }
    }

    public static class SurroundedWith extends Base {
        protected final String prefix;
        protected final String suffix;

        public SurroundedWith(String prefix, String suffix) {
            this.prefix = prefix;
            this.suffix = suffix;
        }

        @Override
        public boolean matches(String input) {
            return input.startsWith(prefix) && input.endsWith(suffix);
        }
    }

    // TODO
    //public static class Many extends Base {
    //}
}
