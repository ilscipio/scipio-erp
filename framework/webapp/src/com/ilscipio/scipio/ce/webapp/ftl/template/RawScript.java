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
    protected final String lang;
    
    public RawScript(Object object, String lang) {
        this.object = object;
        this.lang = (lang != null && !lang.isEmpty()) ? lang : null;
    }
    
    public RawScript(Object object) {
        this.object = object;
        this.lang = null;
    }
    
    public static RawScript wrap(Object object, String lang) {
        return new RawScript(object, lang);
    }
    
    public static RawScript wrap(Object object) {
        return new RawScript(object);
    }
    
    @Override
    public String toString() {
        return (object != null) ? object.toString() : null;
    }
    
    public String getLang() {
        return lang;
    }
    
    public static boolean isRawScript(Object object) {
        return (object != null) && (object instanceof RawScript);
    }
    
    public static String getLang(Object object) {
        if (object instanceof RawScript) {
            return ((RawScript) object).getLang();
        } else {
            return null;
        }
    }

}
