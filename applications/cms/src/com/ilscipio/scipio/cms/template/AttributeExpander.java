package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.ce.webapp.ftl.template.RawScript;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker.InvokeOptions;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateSource;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.template.CmsAttributeTemplate.Type;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;

/**
 * Expands interpolations in CMS attributes, and may also provide base code for interpreting
 * strings as other tpes.
 */
public abstract class AttributeExpander {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final List<String> javaShortTypeList = Collections.unmodifiableList(Arrays.asList(new String[] {
            "String", "BigDecimal", "Double", "Float", "Long", "Integer", "Date", "Time", "Timestamp", "Boolean", "Object"
    }));
    
    private static final List<String> javaTypeExampleList;
    static {
        ArrayList<String> all = new ArrayList<>(javaShortTypeList);
        all.add("java.lang.String");
        all.trimToSize();
        javaTypeExampleList = Collections.unmodifiableList(all);
    }
    
    /* ***************************************************************** */
    /* Enums */
    /* ***************************************************************** */
    
    private static final AttributeExpander noneExpander = new NoneAttributeExpander();
    private static final Map<ExpandLang, AttributeExpander> langExpanders;
    static {
        Map<ExpandLang, AttributeExpander> map = new EnumMap<>(ExpandLang.class);
        map.put(ExpandLang.NONE, noneExpander);
        map.put(ExpandLang.SIMPLE, new SimpleAttributeExpander());
        map.put(ExpandLang.FLEXIBLE, new FlexibleAttributeExpander());
        map.put(ExpandLang.FTL, new FtlAttributeExpander());
        langExpanders = map;
    }
    
    public enum ExpandLang {
        NONE(null, null),
        // NOTE: FTL is a special case; it could also have been implemented with CmsAttributeTemplate.Type, but this is more flexible?
        FTL("CmsExpandLang.description.FTL", "${map1.field1!}, [#assign.../], [@pageUrl id='x'.../], ..."), 
        FLEXIBLE("CmsExpandLang.description.FLEXIBLE", "${map1.field1}, ${groovy: [expr]}, ..."),
        SIMPLE("CmsExpandLang.description.SIMPLE", "{{map1.field1}}");
        
        private static final ExpandLang defaultExpandLang = ExpandLang.fromStringSafe(UtilProperties.getPropertyValue("cms.properties", 
                "render.attributes.expandLang.default"), ExpandLang.NONE);
        
        private final String descriptionProperty;
        private final String codeHintDesc;

        private ExpandLang(String descriptionProperty, String codeHintDesc) {
            this.descriptionProperty = descriptionProperty;
            this.codeHintDesc = codeHintDesc;
        }

        public String getDescriptionProperty() {
            return descriptionProperty;
        }

        public String getCodeHintDesc() {
            return codeHintDesc;
        }
        
        public String getDescription(Locale locale) {
            return descriptionProperty != null ? UtilProperties.getMessage("CMSUiLabels", descriptionProperty, locale) : null;
        }
        
        public String getDescription(Locale locale, String prefix) {
            return descriptionProperty != null ? prefix + getDescription(locale) : null;
        }

        public String getDescription() {
            return descriptionProperty != null ? UtilProperties.getMessage("CMSUiLabels", descriptionProperty, Locale.ENGLISH) : null;
        }
        
        public String getDescriptionWithExample(Locale locale) {
            return descriptionProperty != null ? getDescription(locale) + " ( " + getCodeHintDesc() + " )" : null;
        }
        
        public String getDescriptionWithExample(Locale locale, String prefix) {
            return descriptionProperty != null ? prefix + getDescriptionWithExample(locale) : null;
        }

        public static ExpandLang fromString(String str, ExpandLang defaultLang) throws IllegalArgumentException {
            if (UtilValidate.isEmpty(str)) return defaultLang;
            return Enum.valueOf(ExpandLang.class, str);
        }
        
        public static ExpandLang fromString(String str) throws IllegalArgumentException {
            return fromString(str, null);
        }
        
        public static ExpandLang fromStringSafe(String str, ExpandLang defaultLang) {
            try {
                return fromString(str, defaultLang);
            } catch(Exception e) {
                Debug.logWarning("Cms: Invalid attribute expansion language (expandLang) value encountered: '" + str 
                        + "'; using default instead: '" + defaultLang + "'", AttributeExpander.module);
            }
            return defaultLang;
        }
        
        public static ExpandLang fromStringSafe(String str) {
            return fromStringSafe(str, null);
        }
        
        public static ExpandLang getDefaultExpandLang() {
            return defaultExpandLang;
        }
        
        public static List<ExpandLang> getDisplayValues() {
            return Arrays.asList(ExpandLang.values());
        }
    }

    /* ***************************************************************** */
    /* Construction */
    /* ***************************************************************** */
    
    protected AttributeExpander() {
        //this.typeParsers = getBaseTypeParsers();
    }
    
    /**
     * Base/common type parser implementation, which subclasses may override, but may
     * also omit completely (not forced by base abstract class).
     */
    protected Map<Type, TypeParser> getBaseTypeParsers() {
        return makeTypeParserMap(
                Type.SHORT_TEXT, new BaseStringDelegatingParser(Type.SHORT_TEXT),
                Type.LONG_TEXT, new BaseStringDelegatingParser(Type.LONG_TEXT),
                Type.BOOLEAN, new BaseBooleanParser(),
                Type.INTEGER, new BaseIntegerParser(),
                Type.COMPLEX_LIST, new BaseComplexListParser(),
                Type.COMPLEX_MAP, new BaseComplexMapParser()
                //Type.DYNAMIC_LIST
                );
    }
    
    protected Map<Type, TypeParser> makeBaseDerivedTypeParserMap(Object... entries) {
        return makeTypeParserMapFromExisting(getBaseTypeParsers(), entries);
    }

    protected static Map<Type, TypeParser> makeTypeParserMapFromExisting(Map<Type, TypeParser> existing, Object... entries) {
        Map<Type, TypeParser> map = new EnumMap<Type, TypeParser>(existing);
        for(int i = 0; i < entries.length; i=i+2) {
            map.put((Type) entries[i], (TypeParser) entries[i+1]);
        }
        return map;
    }
    
    protected static Map<Type, TypeParser> makeTypeParserMap(Object... entries) {
        Map<Type, TypeParser> map = new EnumMap<Type, TypeParser>(Type.class);
        for(int i = 0; i < entries.length; i=i+2) {
            map.put((Type) entries[i], (TypeParser) entries[i+1]);
        }
        return map;
    }
    
    /* ***************************************************************** */
    /* Factory Methods */
    /* ***************************************************************** */
    
    public static AttributeExpander getExpander(ExpandLang lang) {
        return langExpanders.get(lang);
    }
    
    /**
     * Returns expander that does not do string interpolations, but
     * may still be used to interpret type coercions in generalized way.
     */
    public static AttributeExpander getNolangExpander() {
        return noneExpander;
    }

    /* ***************************************************************** */
    /* Getters/Helpers */
    /* ***************************************************************** */
    
    public abstract ExpandLang getExpandLang();
    
    protected abstract Map<Type, TypeParser> getTypeParsers();
    
    /* ***************************************************************** */
    /* Attribute Expander Main Processing Methods */
    /* ***************************************************************** */
    
    /**
     * Returns a special generic string parser, if any.
     */
    protected abstract TypeParser getStringParser();
    
    public TypeParser getParser(Type type) {
        return getTypeParsers().get(type);
    }
    
    public TypeParser getParserAlways(Type type) throws IllegalArgumentException {
        TypeParser parser = getParser(type);
        if (parser == null) {
            throw new IllegalArgumentException("CMS attribute type '" 
                    + type + "' unsupported by string expansion language '" + getExpandLang() + "'");
        }
        return parser;
    }

    public Object parse(Type type, String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
        return getParserAlways(type).parse(strValue, attrTmpl, sourceContext, pageContext);
    }
    
    /* ***************************************************************** */
    /* Type Parsers */
    /* ***************************************************************** */

    public interface TypeParser {
        /**
         * Checks if the value requires parsing and, if so, parses it.
         * If it already parsed or doesn't need parsing, returns as-is or however fits best.
         * Throws exception if invalid.
         * <p>
         * NOTE: does not do default value handling or other such extras 
         * (CmsAttributeTemplate not passed, intentionally).
         */
        public Object validateParse(Object value, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext);
        
        /**
         * (Always) Parses string representation of a {@link CmsAttributeTemplate.Type} value.
         * <p>
         * NOTE: the CmsAttributeTemplate instance should only be needed in special cases here.
         */
        public Object parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext);
        
        /**
         * Returns true if the value is already considered parsed and ready for the target
         * type (no processing required).
         */
        public boolean isParseNotRequired(Object value);
    }
    
    protected abstract class AbstractTypeParser implements TypeParser {
        @Override
        public Object validateParse(Object value, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            if (value == null) {
                return value;
            } else if (isParseNotRequired(value)) {
                return value;
            } else if (value instanceof String) {
                return parse((String) value, attrTmpl, sourceContext, pageContext);
            } else {
                throw new IllegalArgumentException("Cms Attribute expected String type for parsing or " 
                        + getReqTypesStr() + " for parsing or bypass, but found unsupported type: " + value.getClass());
            }
        }
        
        protected abstract String getReqTypesStr();
    }

    /**
     * NOTE: this is slight duplication with {@link AttributeExpander#getParserAlways}.
     */
    protected class UnsupportedTypeParser extends AbstractTypeParser {
        private final Type type;
        
        public UnsupportedTypeParser(Type type) {
            super();
            this.type = type;
        }
        
        public Type getType() { return type; }
        
        private RuntimeException getUnsupported() {
            return new UnsupportedOperationException("CMS attribute type '" 
                    + getType() + "' unsupported by string expansion language '" + getExpandLang() + "'");
        }
        @Override
        public Object parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            throw getUnsupported();
        }
        @Override
        public boolean isParseNotRequired(Object value) {
            return false;
        }
        @Override
        protected String getReqTypesStr() {
            return "";
        }
    }
    
    /**
     * NOTE: StringParser is a "virtual" type; no entry for it in CmsAttributeTemplate.Type.
     */
    protected abstract class StringParser extends AbstractTypeParser {
        @Override
        public boolean isParseNotRequired(Object value) {
            return (value instanceof RawScript); // SPECIAL: ignore RawScript
        }
        @Override
        protected String getReqTypesStr() {
            return "RawScript";
        }
    }
    
    protected static String makeStringResult(String res) {
        // 2017-03-17: FIXME?: if empty, return null, so that freemarker default operator is usable...
        // but this means there is no support for empty string value. this is a problem with the UI/json code...
        if (res == null || !res.isEmpty()) {
            return res;
        } else {
            return null;
        }
    }
    
    protected abstract class ShortTextParser extends StringParser {
    }
    
    protected abstract class LongTextParser extends StringParser {
    }
    
    protected abstract class BooleanParser extends AbstractTypeParser {
        @Override
        public boolean isParseNotRequired(Object value) {
            return (value instanceof Boolean);
        }
        @Override
        protected String getReqTypesStr() {
            return "Boolean";
        }
    }
    
    protected abstract class IntegerParser extends AbstractTypeParser {
        @Override
        public boolean isParseNotRequired(Object value) {
            return (value instanceof Integer);
        }
        @Override
        protected String getReqTypesStr() {
            return "Integer";
        }
    }
    
    protected abstract class ComplexListParser extends AbstractTypeParser {
        @Override
        public boolean isParseNotRequired(Object value) {
            return (value instanceof List);
        }
        @Override
        protected String getReqTypesStr() {
            return "List";
        }
    }
    
    protected abstract class ComplexMapParser extends AbstractTypeParser {
        @Override
        public boolean isParseNotRequired(Object value) {
            return (value instanceof Map);
        }
        @Override
        protected String getReqTypesStr() {
            return "Map";
        }
    }

    protected class BaseStringDelegatingParser extends StringParser {
        protected final Type type;

        protected BaseStringDelegatingParser(Type type) {
            super();
            this.type = type;
        }
        public Type getType() { return type; }

        @Override
        public Object parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            return getStringParser().parse(strValue, attrTmpl, sourceContext, pageContext);
        }
    }
 
    protected class BaseBooleanParser extends BooleanParser {
        @Override
        public Boolean parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            // parse as string, and then coerce result to Boolean
            Object str = getParser(Type.SHORT_TEXT).parse(strValue, attrTmpl, sourceContext, pageContext);
            
            if ("true".equals(str)) {
                return Boolean.TRUE;
            } else if ("false".equals(str)) {
                return Boolean.FALSE;
            } else if (str instanceof String && UtilValidate.isEmpty((String) str)) {
                return null;
            } else {
                throw new IllegalArgumentException("CMS attribute type BOOLEAN got invalid string representation of boolean: " + strValue);
            }
        }
    }
    
    protected class BaseIntegerParser extends IntegerParser {
        @Override
        public Integer parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            // parse as string, and then coerce result to Integer
            Object str = getParser(Type.SHORT_TEXT).parse(strValue, attrTmpl, sourceContext, pageContext);
            if (str == null) {
                return null;
            } else {
                String strVal = (String) str;
                if (strVal.isEmpty()) {
                    return null;
                } else {
                    try {
                        return Integer.parseInt(strVal);
                    } catch (NumberFormatException e) {
                        // TODO?: maybe remove the wrapper exception later
                        throw new IllegalArgumentException("CMS attribute type INTEGER got invalid string representation of integer: " + strVal, e);
                    }
                }
            }
        }
    }
    
    protected class BaseComplexListParser extends ComplexListParser {
        @Override
        public List<?> parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            // TODO: the list format will likely be common to all langs, but string components may be lang-specific
            return null;
        }
    }
    
    protected class BaseComplexMapParser extends ComplexMapParser {
        @Override
        public Map<?, ?> parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
            // TODO: the map format will likely be common to all langs, but string components may be lang-specific
            return null;
        }
    }

    /* ***************************************************************** */
    /* Attribute Expanders (groups of parsers) */
    /* ***************************************************************** */
    
    protected static class NoneAttributeExpander extends AttributeExpander {
        // DEV NOTE: each class gets a copy of these due to java inner class kludge
        protected final Map<Type, TypeParser> typeParsers;
        protected final TypeParser stringParser; // special
        
        protected NoneAttributeExpander() {
            this.typeParsers = makeBaseDerivedTypeParserMap();
            this.stringParser = new NoneStringParser();
        }
        
        protected class NoneStringParser extends StringParser {
            @Override
            public String parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
                return strValue;
            }
        }

        @Override
        protected Map<Type, TypeParser> getTypeParsers() { return typeParsers; }
        @Override
        protected TypeParser getStringParser() { return stringParser; }
        @Override
        public ExpandLang getExpandLang() { return ExpandLang.NONE; }
    }
    
    /**
     * Expands strings using <code>{{map1.field1}}</code> syntax - legacy CMS language.
     * <p>
     * For strings, this pattern matches variables in the following format:
     * <code>{{variableName}}</code>
     * it also matches a dot notation variable name
     * <code>{{aMap.keyRefenrencingAnotherMap.keyReferencingAString}}</code>
     */
    protected static class SimpleAttributeExpander extends AttributeExpander {
            
        private static final Pattern variablePattern = Pattern.compile("\\{\\{([\\w\\.]+)\\}\\}");
        
        protected final Map<Type, TypeParser> typeParsers;
        protected final TypeParser stringParser; // special
        
        protected SimpleAttributeExpander() {
            this.typeParsers = makeBaseDerivedTypeParserMap();
            this.stringParser = new SimpleStringParser();
        }
        
        protected class SimpleStringParser extends StringParser {
            @Override
            public String parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
                if (strValue == null) return null;
                StringBuffer strValueInjected = new StringBuffer();
                Matcher variableMatcher = variablePattern.matcher(strValue);
                // go through the regex matches.
                while (variableMatcher.find()) {
                    Object result = sourceContext;
                    // split up dot notation variable name
                    StringTokenizer tokenizer = new StringTokenizer(variableMatcher.group(1), ".");
                    while (tokenizer.hasMoreTokens()) {
                        // we can cast here safely because result is either the initial content map
                        // or we only continued the loop below if result is indeed a map
                        result = ((Map<?, ?>) result).get(tokenizer.nextToken());
                        // if there is no variable with the name, return an html comment
                        if (result == null) {
                            // NOTE 2017: ONLY do this in preview mode, otherwise security risk
                            if (pageContext.isPreview()) {
                                result = "<!-- " + variableMatcher.group() + " is empty --!>";
                            }
                            break;
                            // if the retrieved value is a map, we continue to
                            // evaluate the dot notation path
                        } else if (result instanceof Map) {
                            continue;
                            // if the value is a string, this is what we will
                            // return. the remaining dot notation path will be
                            // ignored
                        } else if (result instanceof String) {
                            break;
                            // if the value is anything else, there is trouble in
                            // paradise. basically do nothing
                        } else {
                            result = null;
                            break;
                        }
                    }
                    // make sure that the result we use to replace the variable is a string
                    if (result instanceof String) {
                        variableMatcher.appendReplacement(strValueInjected, (String) result);
                    } else {
                        // NOTE 2017: ONLY do this in preview mode, otherwise security risk
                        // in every other case, replace the variable with a html comment
                        if (pageContext.isPreview()) {
                            variableMatcher.appendReplacement(strValueInjected, "<!-- " + variableMatcher.group() + " is not a string --!>");
                        }
                    }
                }
                // add the rest of the string we matched against after we went
                // through all regex matches
                variableMatcher.appendTail(strValueInjected);
                return makeStringResult(strValueInjected.toString());
            }
        }

        @Override
        protected Map<Type, TypeParser> getTypeParsers() { return typeParsers; }
        @Override
        protected TypeParser getStringParser() { return stringParser; }
        @Override
        public ExpandLang getExpandLang() { return ExpandLang.SIMPLE; }
    }

    /**
     * Expands strings using <code>${map1.field1}</code> syntax - using standard ofbiz FlexibleStringExpander.
     */
    protected static class FlexibleAttributeExpander extends AttributeExpander {
        
        protected final Map<Type, TypeParser> typeParsers;
        protected final TypeParser stringParser; // special
        
        protected FlexibleAttributeExpander() {
            this.typeParsers = makeBaseDerivedTypeParserMap();
            this.stringParser = new FlexibleStringParser();
        }
        
        protected class FlexibleStringParser extends StringParser {
            @Override
            public String parse(String strValue, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
                if (strValue == null) return null;
                return makeStringResult(FlexibleStringExpander.expandString(strValue, sourceContext));
            }
        }

        @Override
        protected Map<Type, TypeParser> getTypeParsers() { return typeParsers; }
        @Override
        protected TypeParser getStringParser() { return stringParser; }
        @Override
        public ExpandLang getExpandLang() { return ExpandLang.FLEXIBLE; }
    }
    
    /**
     * COMPILES strings as special Freemarker TemplateModel wrapping an actual Template,
     * but without evaluating them (must be left to moment of rendering).
     */
    protected static class FtlAttributeExpander extends AttributeExpander {
        
        // NOTE: we currently don't get this involved in the ftl Configuration instance - no need?
        private static final boolean bracketSyntaxEnabled = UtilProperties.getPropertyAsBoolean("cms.properties",
                "render.attributes.expandLang.ftl.bracketSyntax", Boolean.TRUE);
        
        protected final Map<Type, TypeParser> typeParsers;
        protected final TypeParser stringParser; // special
        
        protected FtlAttributeExpander() {
            this.typeParsers = makeBaseDerivedTypeParserMap(
                    Type.BOOLEAN, new UnsupportedTypeParser(Type.BOOLEAN)
                    );
            this.stringParser = new FtlStringParser();
        }
        
        public boolean isBracketSyntaxEnabled() {
            return bracketSyntaxEnabled;
        }
        
        /**
         * trims and ensures bracket syntax is enabled using inline directive if appropriate.
         */
        protected String normalizeTmplStr(String tmplStr) {
            if (tmplStr == null) {
                return null;
            }
            tmplStr.trim();
            // SPECIAL: if makeResult determines null here, abort early...
            tmplStr = makeStringResult(tmplStr);
            if (tmplStr == null) {
                return null;
            }
            if (isBracketSyntaxEnabled()) {
                tmplStr = enableBracketMode(tmplStr);
            }
            return tmplStr;
        }
        
        /**
         * caller must have done a trim().
         * NOTE: we don't mess with Configuration for bracket, would just complicate needlessly.
         * as long as Configuration didn't force a mode, should be no issue.
         */
        protected String enableBracketMode(String tmplStr) {
            if (tmplStr.startsWith("<#ftl")) {
                // if for some reason and somehow non-bracket syntax was already forced, 
                // then and only then ignore it
                ;
            } else if (!tmplStr.startsWith("[#ftl")) {
                // enable bracket syntax the inline way
                //
                tmplStr = "[#ftl]" + tmplStr;
            }
            return tmplStr;
        }
        
        
        protected class FtlStringParser extends StringParser {
            @Override
            public Object parse(String tmplStr, CmsAttributeTemplate attrTmpl, Map<String, ?> sourceContext, CmsPageContext pageContext) {
                tmplStr = normalizeTmplStr(tmplStr);
                if (tmplStr == null) {
                    return null;
                }
                
                try {
                    return getStringTemplateInvokerForContent(tmplStr, null, pageContext);
                } catch (Exception e) {
                    // TODO: communicate to end user somehow?
                    Debug.logError(e, "Cms: Error interpreting FTL template for CMS attribute", module);
                    return null;
                }
            }
        }

        @Override
        protected Map<Type, TypeParser> getTypeParsers() { return typeParsers; }
        @Override
        protected TypeParser getStringParser() { return stringParser; }
        @Override
        public ExpandLang getExpandLang() { return ExpandLang.FTL; }
        
        /**
         * Gets a (String)TemplateInvoker with compiled Freemarker template for the attribute
         * content inline template.
         * <p>
         * TODO: decide cache... can make CmsAttributeTemplate-specific cache (by id or so, faster) but
         * may get memory duplication with #interpretStd calls (cached by body, because no id/name avail)
         * -> could do both at once maybe? share the Template instance between 2 cache...
         * TODO: subject to refactor/move
         * TODO?: FUTURE: this does NOT wrap the TemplateInvoker in a TemplateModel for time being...
         * we will rely on the default ObjectWrapper(s) to wrap the StringTemplateInvoker
         * in a freemarker StringModel (BeanModel) which renders throughs the ?string built-in
         * or string evaluation.
         */
        public static TemplateInvoker getStringTemplateInvokerForContent(String tmplStr, Map<String, Object> ctxVars, CmsPageContext pageContext) throws TemplateException, IOException {
            Configuration config = CmsRenderTemplate.TemplateRenderer.getDefaultCmsConfig();
            
            // TODO: REVIEW: cache selection is a problem...
            // instead of by template body we could cache by some derivative unique name
            // derived from the CmsAttributeTemplate ID (maybe?)
            // would likely be much faster
            
            UtilCache<String, Template> cache = TemplateSource.getTemplateInlineSelfCacheForConfig(config, null);
            if (cache == null) {
                Debug.logWarning("Cms: could not determine"
                        + " an inline template cache to use; not using cache", module);
            } 
            TemplateSource templateSource = TemplateSource.getForInlineSelfCache(tmplStr, cache, config);
            
            // NOTE: must get StringInvoker so BeansWrapper's StringModel can invoke toString()
            // NOTE: context parameters could be passed to the template using InvokeOptions.ctxVars...
            // but not yet needed
            TemplateInvoker invoker = TemplateInvoker.getInvoker(templateSource, new InvokeOptions(null, null, null, ctxVars, false), null);
            return invoker;
        }
    }
    
    /* ***************************************************************** */
    /* Target type conversion */
    /* ***************************************************************** */
    /* NOTE: these are in this class to abstract attribute-specific typing logic, if needed */
    
    public static Object convertToJavaType(Object value, String targetType, TimeZone timeZone, Locale locale) throws IllegalArgumentException {
        try {
            return ObjectType.simpleTypeConvert(value, targetType, null, timeZone, locale, true);
        } catch (GeneralException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    public static void checkValidJavaType(String targetType) throws ClassNotFoundException {
        ObjectType.loadClass(targetType);
    }
    
    public static List<String> getJavaShortTypeList() {
        return javaShortTypeList;
    }
    
    public static List<String> getJavaTypeExampleList() {
        return javaTypeExampleList;
    }
}
