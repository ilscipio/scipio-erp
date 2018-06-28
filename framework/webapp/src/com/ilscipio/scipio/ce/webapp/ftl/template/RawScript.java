package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Wrapper for strings that get passed to objectAsScript FTL macro to prevent
 * enclosing.
 * <p>
 * The wrapped is an Object instead of string so that string-likes will work
 * through toString().
 */
public abstract class RawScript {

    private static Map<String, Set<String>> langParts = Collections.emptyMap();
    
    /**
     * Maps target langs to list of langs that can be used in place of it.
     */
    private static final Map<String, Set<String>> substituteLangsMap;
    static {
        Map<String, Set<String>> map = new HashMap<>();
        
        // Any html-attribute-escaped code can also be used in markup body (but NOT vice-versa!)
        map.put("htmlmarkup", new HashSet<String>(Arrays.asList(new String[]{ "html" })));
        
        substituteLangsMap = map;
    }
    
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
    
    /**
     * Resolves value/lang or returns null if not RawScript.
     */
    public static SingleLangRawScript resolveScriptForLang(Object object, String targetLang) {
        if (object == null || !(object instanceof RawScript)) {
            return null;
        } else {
            return ((RawScript) object).resolveScriptForLang(targetLang);
        }
    }
    
    /**
     * Resolves value/lang or returns a SingleLangRawScript if not RawScript.
     */
    public static SingleLangRawScript resolveScriptForLangAlways(Object object, String targetLang) {
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
            } else if ("htmlmarkup".equals(targetLang) && "html".equals(this.lang)) { // special case: "html" can fill in for "htmlmarkup"
                return new SingleLangRawScript(this.value, "raw");
            } else {
                // complex case: we may be a prefix of targetLang
                if (targetLang.startsWith(this.lang) && (targetLang.charAt(this.lang.length()) == '-')) {
                    return new SingleLangRawScript(this.value, targetLang.substring(this.lang.length() + 1));
                } else {
                    // special case for html filling in for htmlmarkup
                    String replTargetLang = targetLang.replaceAll("htmlmarkup", "html");
                    if (replTargetLang.startsWith(this.lang) && (replTargetLang.charAt(this.lang.length()) == '-')) {
                        return new SingleLangRawScript(this.value, replTargetLang.substring(this.lang.length() + 1));
                    } else { // failure case: wrong language: we have re-escape to prevent security holes
                        return new SingleLangRawScript(this.value, targetLang);
                    }
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
            //} else if (langValueMap.containsKey("html")) {
            //    return langValueMap.get("html");
            } else {
                return langValueMap.entrySet().iterator().next().getValue();
            }
        }

        public String getDefaultLang() { // NOTE: avoid using this... use resolveScriptForLang...
            // TODO: review: this is for fallback cases
            if (langValueMap.containsKey("raw")) {
                return "raw";
            //} else if (langValueMap.containsKey("html")) {
            //    return "html";
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
            } else if ("htmlmarkup".equals(targetLang) && langValueMap.containsKey("html")) { // special case: html can fill in for htmlmarkup
                return new SingleLangRawScript(langValueMap.get("html"), "raw");
            } else {
                // complex case: one of our langs may be a prefix of targetLang. find the longest prefix.
                String bestLang = null;
                Object bestValue = null;
                String bestTargetLang = null;
                
                for(Map.Entry<String, Object> entry : langValueMap.entrySet()) {
                    String lang = entry.getKey();
                    if (bestLang == null || (lang.length() > bestLang.length())) { // IMPORTANT: always keep the first if same length.
                        if (targetLang.startsWith(lang) && (targetLang.charAt(lang.length()) == '-')) {
                            bestLang = lang;
                            bestValue = entry.getValue();
                            bestTargetLang = targetLang;
                        }
                    }
                }
                
                // special case: html can substitute for htmlmarkup
                if (targetLang.contains("htmlmarkup")) {
                    String replTargetLang = targetLang.replaceAll("htmlmarkup", "html");
                    for(Map.Entry<String, Object> entry : langValueMap.entrySet()) {
                        String lang = entry.getKey();
                        if (bestLang == null || (lang.length() > bestLang.length())) { // IMPORTANT: always keep the first if same length.
                            if (replTargetLang.startsWith(lang) && (replTargetLang.charAt(lang.length()) == '-')) {
                                bestLang = lang;
                                bestValue = entry.getValue();
                                bestTargetLang = replTargetLang;
                            }
                        }
                    }
                }

                if (bestLang != null) {
                    return new SingleLangRawScript(bestValue, bestTargetLang.substring(bestLang.length() + 1));
                } else {
                    if (langValueMap.containsKey("raw")) { // raw case, had nothing better.
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
                    Map<String, Set<String>> newLangParts = new HashMap<>(langParts);
                    
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
