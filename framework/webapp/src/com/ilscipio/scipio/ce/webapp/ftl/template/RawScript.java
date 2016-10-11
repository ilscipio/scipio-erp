package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

/**
 * Wrapper for strings that get passed to objectAsScript FTL macro to prevent
 * enclosing.
 * <p>
 * The wrapped is an Object instead of string so that string-likes will work
 * through toString().
 */
public abstract class RawScript {

    private static Map<String, Set<String>> langParts = Collections.unmodifiableMap(new HashMap<String, Set<String>>());
    
    public static RawScript wrap(Object value, String lang) {
        return new SingleLangRawScript(value, lang);
    }
    
    public static RawScript wrap(Map<String, Object> langValueMap) {
        if (langValueMap.size() == 1) { // optimization
            Map.Entry<String, Object> entry = langValueMap.entrySet().iterator().next();
            return new SingleLangRawScript(entry.getValue(), entry.getKey());
        } else {
            return new MultiLangRawScript(langValueMap);
        }
    }
    
    @Override
    public abstract String toString(); // NOTE: avoid using this... use resolveScriptForLang...
    
    public abstract Object getValueForLang(String lang);
    
    /**
     * Returns a ScriptLang pair with a script (string) that has yet to receive escaping for the given lang,
     * including the missing lang itself (which may be different from lang).
     * <p>
     * This RawScript may already have been escaped for the given lang or only partially; this returns
     * the remainder in best-effort fashion.
     * <p>
     * Assumes lang non-empty (but may be "raw").
     */
    public abstract SingleLangRawScript resolveScriptForLang(String targetLang);


    /**
     * WARN: in most case you want resolveScriptForLang
     */
    public static boolean isRawScript(Object object) {
        return ((object != null) && (object instanceof RawScript));
    }
    
    public static boolean isRawScript(Object object, String lang) {
        if ((object != null) && (object instanceof RawScript)) {
            if (lang == null || lang.isEmpty()) {
                return true;
            } else {
                return ((RawScript) object).getValueForLang(lang) != null;
            }
        } else {
            return false;
        }
    }
    
    public static SingleLangRawScript resolveScriptForLang(Object object, String targetLang) {
        if (object == null || !(object instanceof RawScript)) {
            return new SingleLangRawScript(object, targetLang);
        } else {
            return ((RawScript) object).resolveScriptForLang(targetLang);
        }
    }
    
    public static Object getValueForLang(Object object, String targetLang) {
        if (object == null || !(object instanceof RawScript)) {
            return null;
        } else {
            return ((RawScript) object).getValueForLang(targetLang);
        }
    }
    
    public static class SingleLangRawScript extends RawScript {
        private final Object value;
        private final String lang;
        
        public SingleLangRawScript(Object value, String lang) {
            this.value = value;
            this.lang = (lang != null && !lang.isEmpty()) ? lang : null; // && !"none".equals(lang)
        }

        public Object getValue() {
            return value;
        }

        public String getLang() {
            return lang;
        }
        
        @Override
        public String toString() { // NOTE: avoid using this... use resolveScriptForLang...
            return (value != null) ? value.toString() : "";
        }

        @Override
        public Object getValueForLang(String lang) {
            if (this.lang == null) {
                return this.value;
            }
            else if (this.lang.equals(lang)) {
                return this.value;
            } else {
                return null;
            }
        }

        @Override
        public SingleLangRawScript resolveScriptForLang(String targetLang) {
            if (this.lang == null) { // special case: none, which means bypass all
                return new SingleLangRawScript(this.value, "raw");
            } else if (this.lang.equals(targetLang)) { // best-possible case: we already escaped everything
                return new SingleLangRawScript(this.value, "raw");
            } else if ("raw".equals(this.lang)) { // raw case: we escaped nothing so far
                return new SingleLangRawScript(this.value, targetLang);
            } else {
                // complex case: we may be a prefix of targetLang
                if (targetLang.startsWith(this.lang) && (targetLang.charAt(this.lang.length()) == '-')) {
                    return new SingleLangRawScript(this.value, targetLang.substring(this.lang.length() + 1));
                } else { // failure case: wrong language: we have re-escape to prevent security holes
                    return new SingleLangRawScript(this.value, targetLang);
                }
            }
        }
        
    }
    
    public static class MultiLangRawScript extends RawScript {
        
        protected final Map<String, Object> langValueMap;
        
        /**
         * WARN: does not create map copy for now (in case LinkedHashMap)
         */
        public MultiLangRawScript(Map<String, Object> langValueMap) {
            this.langValueMap = langValueMap;
        }

        public Object getDefaultObject() { // NOTE: avoid using this... use resolveScriptForLang...
            // TODO: review: this is for fallback cases
            if (langValueMap.containsKey("raw")) {
                return langValueMap.get("raw");
            } else if (langValueMap.containsKey("html")) {
                return langValueMap.get("html");
            } else {
                return langValueMap.entrySet().iterator().next().getValue();
            }
        }

        public String getDefaultLang() { // NOTE: avoid using this... use resolveScriptForLang...
            // TODO: review: this is for fallback cases
            if (langValueMap.containsKey("raw")) {
                return "raw";
            } else if (langValueMap.containsKey("html")) {
                return "html";
            } else {
                return langValueMap.entrySet().iterator().next().getKey();
            }
        }
        
        @Override
        public String toString() { // NOTE: avoid using this... use resolveScriptForLang...
            Object object = getDefaultObject();
            return (object != null) ? object.toString() : "";
        }
        
        @Override
        public Object getValueForLang(String lang) {
            return langValueMap.get(lang);
        }

        @Override
        public SingleLangRawScript resolveScriptForLang(String targetLang) {
            // best-possible case: we contain the requested language directly
            if (langValueMap.containsKey(targetLang)) { // best-possible case: we already escaped everything
                return new SingleLangRawScript(langValueMap.get(targetLang), "raw");
            } else {
                Map.Entry<String, Object> bestEntry = null;
                // complex case: one of our langs may be a prefix of targetLang. find the longest prefix.
                for(Map.Entry<String, Object> entry : langValueMap.entrySet()) {
                    String lang = entry.getKey();
                    if (targetLang.startsWith(lang) && (targetLang.charAt(lang.length()) == '-')) {
                        if (bestEntry == null || (lang.length() > bestEntry.getKey().length())) {
                            bestEntry = entry;
                        }
                    }
                }
                if (bestEntry != null) {
                    return new SingleLangRawScript(bestEntry.getValue(), 
                            targetLang.substring(bestEntry.getKey().length() + 1));
                } else {
                    if (langValueMap.containsKey("raw")) { // raw case
                        return new SingleLangRawScript(langValueMap.get("raw"), targetLang);
                    } else {
                        // failure case: wrong language(s): we have to re-escape to prevent security holes; use arbitrary value
                        return new SingleLangRawScript(langValueMap.values().iterator().next(), targetLang);
                    }
                }
            }
        }
        
    }

    private static Set<String> getLangParts(String lang) {
        // NOTE: this method is a heuristic-like optimization, but it should work out
        Set<String> parts = langParts.get(lang);
        if (parts == null) {
            synchronized(RawScript.class) {
                parts = langParts.get(lang);
                if (parts == null) {
                    Map<String, Set<String>> newLangParts = new HashMap<String, Set<String>>(langParts);
                    
                    parts = new LinkedHashSet<String>(Arrays.asList(lang.split("-")));
                    
                    newLangParts.put(lang, parts);
                    
                    // NOTE: the assignment is not synchronized, but the final inner contents of the unmodifiable map should be fine.
                    langParts = Collections.unmodifiableMap(newLangParts);
                }
            }
        }
        return parts;
    }
    
}
