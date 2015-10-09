package com.ilscipio.cato.webapp.ftl;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

import javolution.util.FastMap;

/**
 * Cato: Common generic Freemarker templating utility methods.
 * <p>
 * Java language analog of component://common/webcommon/includes/catoUtilities.ftl.
 * <p>
 * <em>NOTE:</em> This is for common, generic code only (generic utilities). Code that implements
 * template macro (e.g. catoHtmlTemplateDefault.ftl) logic belongs in separate class.
 */
public final class CommonFtlUtil {
    
    public static final String module = CommonFtlUtil.class.getName();
    
    private static final UtilCache<String, Map<String, Object>> headingElemSpecFromStyleStrCache = 
            UtilCache.createUtilCache("com.ilscipio.cato.webapp.ftl.CommonFtlUtil.headingElemSpecFromStyleStrCache");
    
    private CommonFtlUtil() {
    }
    

    /**
     * Parses a complex style string meant to describe an element notably heading into hash of constituent values.
     * <p>
     * If cacheId is specified, the result is looked up from a global cache based on a key composed
     * of styleStr, containerStyleStr, and cacheId. cacheId must uniquely identify the remaining parameters.
     *
     * @param styleStr
     * @param containerStyleStr
     * @param allowedHeadingElemTypes string of "|"-separated names or collection
     * @param allowedElemTypes string of "|"-separated names or collection, or null or boolean to let caller figure out
     * @param allowedContainerElemTypes string of "|"-separated names or collection, or null or boolean to let caller figure out
     * @param cacheId global unique cache ID that should uniquely identify combination of params other than styleStr and containerStyleStr
     * @return
     */
    public static Map<String, Object> getHeadingElemSpecFromStyleStr(String styleStr, String containerStyleStr, 
            Object allowedHeadingElemTypes, Object allowedElemTypes, Object allowedContainerElemTypes,
            String cacheId) {
        Map<String, Object> res = null;
        String cacheKey = null;
        if (cacheId != null && !cacheId.isEmpty()) {
            cacheKey = styleStr + "::" + containerStyleStr + "::" + cacheId;
        }
  
        if (cacheKey != null) {
            res = headingElemSpecFromStyleStrCache.get(cacheKey);
        }
        
        if (res == null) {
            res = getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr, allowedHeadingElemTypes, allowedElemTypes, allowedContainerElemTypes);
            
            if (cacheKey != null) {
                // note: probably no need to synchronize on cache; duplicate insertion is ok
                headingElemSpecFromStyleStrCache.put(cacheKey, res);
            }
        }
        
        return res;
    }

    public static Map<String, Object> getHeadingElemSpecFromStyleStr(String styleStr, String containerStyleStr, 
            Object allowedHeadingElemTypes, Object allowedElemTypes, Object allowedContainerElemTypes) {
        String allowedHeadingElemTypesStr = nameListArgToStr(allowedHeadingElemTypes);
        Set<String> allowedElemTypesSet = nameListArgToSet(allowedElemTypes);
        Set<String> allowedContainerElemTypesSet = nameListArgToSet(allowedContainerElemTypes);

        String headingLevel = "";
        String relHeadingLevel = "";
        boolean isHeadingElem = false;
        
        // TODO: clean up names. optimization not needed due to cache.
        
        String titleStyle = styleStr;
        String titleContainerStyle = containerStyleStr;
                
        Map<String, String> titleArgs = FastMap.newInstance();
        String titleArgsStr = titleStyle;
        
        String[] titleStyleParts = titleStyle.split(";");
        
        if (titleStyleParts.length > 3) {
            titleArgsStr = titleStyleParts[2];
        }
        else {
            titleArgsStr = titleStyleParts[titleStyleParts.length - 1];
        }
        
        if (UtilValidate.isNotEmpty(titleArgsStr) && titleArgsStr.contains("=")) { //heuristic detect params part
            titleArgs = splitStrParams(titleArgsStr, ",");
            if (titleStyleParts.length >= 3) {
                titleContainerStyle = titleStyleParts[0];
                titleStyle = titleStyleParts[1];
            }
            else if (titleStyleParts.length == 2) {
                titleStyle = titleStyleParts[0];
            }
            else {
                titleStyle = "";
            }
        }
        else if (titleStyleParts.length > 1) {
            titleContainerStyle = titleStyleParts[0];
            titleStyle = titleStyleParts[1];
        }
        
        String titleContainerElemType = "";
        String titleContainerClass = "";
        
        if (UtilValidate.isNotEmpty(titleContainerStyle)) {
            String[] titleContainerStyleParts = titleContainerStyle.split(":");
            if (titleContainerStyleParts.length <= 1) {
                titleContainerElemType = titleContainerStyle.toLowerCase();
                titleContainerClass = titleContainerStyle;
            }
            else {
                titleContainerElemType = titleContainerStyleParts[0].toLowerCase();
                titleContainerClass = titleContainerStyle.substring(titleContainerElemType.length() + 1);
            }
            
            // if omitted, leave to caller to figure out if titleContainerStyle elem or class
            if (allowedContainerElemTypesSet != null) {
                if (allowedContainerElemTypesSet.contains(titleContainerElemType)) {
                    if (titleContainerStyleParts.length <= 1) {
                        titleContainerClass = "";
                    }
                }
                else {
                    titleContainerElemType = "";
                    titleContainerClass = titleContainerStyle;
                }
            }
        }
        
        String titleElemType = "";
        String titleClass = "";
        
        if (UtilValidate.isNotEmpty(titleStyle)) {
            titleStyleParts = titleStyle.split(":");
            
            if (titleStyleParts.length <= 1) {
                titleElemType = titleStyle.toLowerCase();
                titleClass = titleStyle;
            }
            else {
                titleElemType = titleStyleParts[0].toLowerCase();
                titleClass = titleStyle.substring(titleElemType.length() + 1);
            }
            
            boolean elemTypeFound = false;
            
            if (UtilValidate.isNotEmpty(allowedHeadingElemTypesStr)) {
                Pattern pat = Pattern.compile("(" + allowedHeadingElemTypesStr + ")(\\+)?(\\d*)");
                Matcher m = pat.matcher(titleElemType);
                if (m.matches()) {
                    if (UtilValidate.isNotEmpty(m.group(2))) {
                        if (UtilValidate.isNotEmpty(m.group(3))) {
                            relHeadingLevel = m.group(3);
                        }
                    }
                    else {
                        if (UtilValidate.isNotEmpty(m.group(3))) {
                            // overrides headingLevel (so style from screen affects heading calc)
                            headingLevel = m.group(3);
                        }
                    }
                    if (titleStyleParts.length <= 1) {
                        titleClass = "";
                    }
                    titleElemType = m.group(1);
                    elemTypeFound = true;
                    isHeadingElem = true;
                }
            }
            
            
            // if not specified, let caller figure it out if titleStyle elem or class
            if (!elemTypeFound && allowedElemTypesSet != null) {
                if (allowedElemTypesSet.contains(titleElemType)) {
                    if (titleStyleParts.length <= 1) {
                        titleClass = "";
                    }
                    elemTypeFound = true;
                }
                else {
                    titleElemType = "";
                    // if invalid type found, use the full string as class, in case ":" char is important somehow
                    titleClass = titleStyle;
                }
            }
            
        }
        
        Map<String, Object> res = FastMap.newInstance();
        res.putAll(titleArgs);
        res.put("containerStyleStr", titleContainerStyle); 
        res.put("containerElemType", titleContainerElemType); 
        res.put("containerElemClass", titleContainerClass); 
        
        res.put("styleStr", titleStyle); 
        res.put("elemType", titleElemType); 
        res.put("elemClass", titleClass);
        //res.put("class", titleClass); // WARN: DO NOT USE, will conflict with getClass() method
        
        res.put("isHeadingElem", isHeadingElem); 
        if (!headingLevel.isEmpty() || !res.containsKey("level")) { // may already come from args
            res.put("level", headingLevel); 
        }
        if (!relHeadingLevel.isEmpty() || !res.containsKey("relLevel")) {
            res.put("relLevel", relHeadingLevel); 
        }
        
        res.put("argsStr", titleArgsStr); 
        
        return Collections.unmodifiableMap(res);
    }
    
    private static String nameListArgToStr(Object arg) {
        String res = null;
        if (arg instanceof String) {
            res = (String) arg;
        }
        else if (arg instanceof Collection) {
            Collection<String> argColl = UtilGenerics.checkCollection(arg);
            if (argColl.isEmpty()) {
                res = "";
            }
            else {
                res = String.join("|", argColl);
            }
        }
        return res;
    }
    
    private static Set<String> nameListArgToSet(Object arg) {
        Set<String> res = null;
        if (arg instanceof Set) {
            res = UtilGenerics.checkSet(arg);
        }
        else if (arg instanceof String) {
            res = new HashSet<String>(Arrays.asList(((String) arg).split("\\|")));
        }
        else if (arg instanceof Collection) {
            Collection<String> argColl = UtilGenerics.checkCollection(arg);
            res = new HashSet<String>(argColl);
        }
        return res;
    }    
    
    /**
     * Extracts parameters from a string in the format and returns as a hash:
     * name1=val1DELIMname2=val2DELIMname3=val3
     * where DELIM is specified delimiter (& &amp; , ; etc.)
     */
    public static Map<String, String> splitStrParams(String paramStr, String paramDelim) {
        Map<String, String> res = FastMap.newInstance();
        for(String pair : paramStr.split(paramDelim)) {
            String[] parts = pair.split("=", 2);
            if (parts.length >= 2) {
                res.put(parts[0], parts[1]);
            }
        }
        return res;
    }
    
}
