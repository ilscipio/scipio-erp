package com.ilscipio.scipio.ce.webapp.ftl.template;

/**
 * Wrapper for strings that get passed to objectAsScript FTL macro to prevent
 * enclosing.
 * <p>
 * The wrapped is an Object instead of string so that string-likes will work
 * through toString().
 */
public class RawScript {

    protected final Object object;
    
    public RawScript(Object object) {
        this.object = object;
    }
    
    public static RawScript wrap(Object object) {
        return new RawScript(object);
    }
    
    @Override
    public String toString() {
        return (object != null) ? object.toString() : null;
    }
    
    public static boolean isRawScript(Object object) {
        return (object != null) && (object instanceof RawScript);
    }

}
