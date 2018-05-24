package org.ofbiz.widget.renderer;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.webapp.renderer.RenderTargetExpr;
import org.ofbiz.webapp.renderer.RenderTargetUtil;
import org.ofbiz.webapp.renderer.RenderWriter;
import org.ofbiz.webapp.renderer.RenderWriter.SwitchRenderWriter;
import org.ofbiz.widget.model.ContainsExpr;
import org.ofbiz.widget.model.ModelScreenWidget;
import org.ofbiz.widget.model.ModelScreenWidget.SectionsRenderer;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.model.ftl.ModelVirtualSectionFtlWidget;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.WidgetRenderTargetState.ExecutionInfoImpl;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.WidgetRenderTargetState.ExprStateInfo;

/**
 * SCIPIO: Represents an expression describing screen widget element to target for rendering,
 * compiled from an expression string. Used to implement targeted rendering.
 * <p>
 * The query expression is usually passed as a string as the "scpRenderTargetExpr" request parameter. 
 * <p>
 * WARN: THIS FEATURE IS IN EARLY STAGES AND SUBJECT TO CHANGE.
 * More documentation can be found in widget-screen.xsd, "contains" expression attribute.
 * <p>
 * In most case (by default) it is physically impossible for the renderer to know the path from top screen element to 
 * a deep-nested element and which elements it doesn't need to render. So the client may need to specify
 * two or more anchor-like section names through these expressions, otherwise the renderer is forced to render 
 * everything to prevent anything breaking, and there is no optimization.
 * <p>
 * Expressions are composed of individual section names and container IDs, which are all treated like global identifiers. 
 * When space-separated they perform a "ancestor descendent" selection like the common CSS descendent selector.
 * <p>
 * NOTE: In order for optimization to work meaningfully, the decorator itself must implement contains-expressions
 * hints using {@code <section contains="!$my-section, ...">}, which is implemented through {@link ContainsExpr}.
 * <p>
 * WARN: There is limited support for wildcards in the query expression, and such wildcards cannot
 * be optimized, so they should be combined with exact-name sections preceding the wildcarded names.
 * Wildcards and bracketed attributes should only be specified as the last entry in the query,
 * and should not be the only entry; otherwise no optimization is possible.
 * <p>
 * Example expression strings:
 * <pre>
 * {@code
 * "$my-section": (section selector)
 *     <section name="my-section">
 * "#my-container": (id selector)
 *     <container id="my-container">
 * "#my-screenlet": (id selector)
 *     <screenlet id="my-screenlet">
 * "~my-included-section": (bottom-up decorator-section-include directive selector NOTE: ONLY matches on the current decorator layer, not the next)
 *     <decorator-section-include name="my-included-section">
 * "%container[id=my-container]": (generic widget tag/element AND Freemarker macro invocation matching support, with limited attribute support (name, id))
 *     <container id="my-container">
 *
 * "$my-section #my-container": (descendent selector, CSS-like)
 *     <section name="my-section">
 *         [...]
 *             <container id="my-container">
 * }
 * </pre>
 */
@SuppressWarnings("serial")
public class WidgetRenderTargetExpr extends WidgetRenderTargetExprBase implements RenderTargetExpr, Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    // WARN: this is public-facing so it must have size limits in cache.properties
    private static final UtilCache<String, WidgetRenderTargetExpr> cache = UtilCache.createUtilCache("widget.renderer.targeted.expr");
    private static final UtilCache<String, WidgetMultiRenderTargetExpr> multiCache = UtilCache.createUtilCache("widget.renderer.targeted.multiexpr");

    /**
     * Separator pattern to split the names/tokens in a RenderTargetExpr input string.
     * currently only support descendent selector, so spaces only.
     */
    private static final Pattern targetExprTokenSeparatorPat = Pattern.compile("\\s+");
    private static final Pattern targetExprAttrSeparatorPat = Pattern.compile("\\s*;\\s*");
    // NOTE: only id and name are supported, everything else gives error
    private static final Pattern targetExprAttrAssignPat = Pattern.compile("^\\s*(id|name)\\s*=\\s*(['\"]?)(.*?)\\2\\s*$");

    // Matching Element Type character prefixes
    public static final char MET_SECNAME = '$';
    public static final char MET_ELEMID = '#';
    public static final char MET_DECSECINCL = '~';
    public static final char MET_DECSEC = '^';
    public static final char MET_DECSEC_RECALL = '/';
    public static final char MET_ELEM = '%';
    // REMOVED: this is now integrated into the widget elem selector
    //public static final char MET_FTLELEM = '@'; // NOTE: limited implementation
    
    /**
     * Matching Element Type character prefixes. Each of these matches a widget element tag OR a type of attribute.
     * NOTE: These are shared between {@link WidgetRenderTargetExpr} and {@link ContainsExpr} so the
     * language is recognizable and consistent.
     */
    public static final Set<Character> MET_ALL = UtilMisc.unmodifiableHashSet(MET_SECNAME, MET_ELEMID, MET_DECSECINCL, MET_DECSEC, MET_ELEM);
    public static final String MET_ALL_STR = StringUtils.join(MET_ALL, ", "); // for log and exceptions
    public static final Set<Character> MET_GENERIC = UtilMisc.unmodifiableHashSet(MET_ELEM);
    public static final Set<Character> MET_SPECIFIC = UtilMisc.unmodifiableHashSet(MET_SECNAME, MET_ELEMID, MET_DECSECINCL, MET_DECSEC);
    
    public static final char WILDCARD = '*';
    public static final String WILDCARD_STRING = String.valueOf(WILDCARD);
    
    public static final char ATTR_OPEN = '[';
    public static final char ATTR_CLOSE = ']';
    public static final char ATTR_EQUALS = '=';
    public static final char ATTR_SEP = ',';
    public static final List<String> ATTR_NAMES = UtilMisc.unmodifiableArrayList("id", "name");
    
    private final String strExpr;
    private final List<Token> tokens;
    //private final boolean useChildContent; // TODO?: FUTURE: this won't work
    
    /******************************************************/
    /* Constructors */
    /******************************************************/
    
    protected WidgetRenderTargetExpr(String strExpr) {
        this.strExpr = strExpr;
        
        String[] tokenArr = targetExprTokenSeparatorPat.split(strExpr.trim());
        if (tokenArr.length < 1) {
            throw new IllegalArgumentException("Invalid render target expression: " + strExpr);
        }
        //this.useChildContent = ">".equals(tokenArr[tokenArr.length - 1]);
        
        Arrays.asList(tokenArr);
        ArrayList<Token> tokens = new ArrayList<>(tokenArr.length);
        //if (this.useChildContent) {
        //    tokens = new ArrayList<>(Arrays.asList(tokenArr).subList(0, tokenArr.length - 1));
        //} else {
        for(String tokenStr : tokenArr) {
            Token token = Token.interpret(tokenStr);
            tokens.add(token);
        }
        //}
        if (tokens.size() < 1) {
            throw new IllegalArgumentException("Invalid render target expression: " + strExpr);
        }
        tokens.trimToSize();
        this.tokens = tokens; // Collections.unmodifiableList(tokens);
    }
    
    /**
     * New expression from string, null if empty, or IllegalArgumentException if invalid. Uses global cache.
     */
    public static WidgetRenderTargetExpr fromString(String strExpr) throws IllegalArgumentException {
        if (strExpr == null || strExpr.isEmpty()) return null;
        WidgetRenderTargetExpr expr = cache.get(strExpr);
        if (expr == null) {
            expr = new WidgetRenderTargetExpr(strExpr);
            cache.put(strExpr, expr); // no sync needed
        }
        return expr;
    }
    
    /**
     * New expression from string, null if empty, or IllegalArgumentException if invalid. No cache.
     */
    public static WidgetRenderTargetExpr fromStringNew(String strExpr) throws IllegalArgumentException {
        // DEV NOTE: keep this logic in sync with RenderTargetUtil.isRenderTargetExprOn
        if (strExpr == null || strExpr.isEmpty()) return null;
        return new WidgetRenderTargetExpr(strExpr);
    }

    /**
     * Gets expression from object, either already RenderTargetExpr or string, null if empty,
     * or IllegalArgumentException if unrecognized class or invalid. Uses global cache.
     */
    public static WidgetRenderTargetExpr fromObject(Object expr) {
        if (expr == null) return null;
        if (expr instanceof WidgetRenderTargetExpr) return (WidgetRenderTargetExpr) expr;
        else if (expr instanceof String) return fromString((String) expr);
        else throw new IllegalArgumentException("Cannot create RenderTargetExpr from type: " + expr.getClass());
    }
    
    /**
     * Gets expression from object, either already RenderTargetExpr or string, null if empty,
     * or IllegalArgumentException if unrecognized class or invalid. Uses global cache.
     */
    @SuppressWarnings("unchecked")
    public static WidgetRenderTargetExprBase fromObjectOrMulti(Object expr) {
        if (expr == null) return null;
        if (expr instanceof WidgetRenderTargetExpr) return (WidgetRenderTargetExpr) expr;
        else if (expr instanceof WidgetMultiRenderTargetExpr) return (WidgetMultiRenderTargetExpr) expr;
        else if (expr instanceof String) {
            String strExpr = (String) expr;
            if (strExpr.startsWith(RenderTargetUtil.RENDERTARGETEXPR_RAW_MULTI_PREFIX)) {
                return WidgetMultiRenderTargetExpr.fromExprListString(strExpr.substring(RenderTargetUtil.RENDERTARGETEXPR_RAW_MULTI_PREFIX.length()));
            } else {
                return fromString(strExpr);
            }
        }
        else if (expr instanceof Map) return WidgetMultiRenderTargetExpr.fromMultiMap((Map<String, Object>) expr);
        else throw new IllegalArgumentException("Cannot create RenderTargetExpr from type: " + expr.getClass());
    }
    
    /******************************************************/
    /* Special Multi class */
    /******************************************************/
    
    // this serves as a flag to say the multi was "compiled"
    public static class WidgetMultiRenderTargetExpr extends WidgetRenderTargetExprBase implements MultiRenderTargetExpr<WidgetRenderTargetExpr>, Serializable {
        private final Map<String, WidgetRenderTargetExpr> map; // NOTE: immutable after create
        
        protected WidgetMultiRenderTargetExpr() {
            this.map = new HashMap<>();
        }

        protected WidgetMultiRenderTargetExpr(Map<? extends String, ? extends Object> m) {
            Map<String, WidgetRenderTargetExpr> map = new HashMap<>();
            for(Map.Entry<? extends String, ? extends Object> entry : m.entrySet()) {
                map.put(entry.getKey(), WidgetRenderTargetExpr.fromObject(entry.getValue()));
            }
            this.map = map;
        }
        
        protected WidgetMultiRenderTargetExpr(String strExpr) {
            Map<String, WidgetRenderTargetExpr> map = new HashMap<>();
            String[] pairs = StringUtils.split(strExpr, ",");
            for(String pair : pairs) {
                String[] parts = StringUtils.split(pair, ":", 2);
                if (parts.length != 2) throw new IllegalArgumentException("targeted rendering multi expression is invalid: " + strExpr);
                String name = parts[0].trim();
                String subStrExpr = parts[1].trim();
                if (name.isEmpty() || subStrExpr.isEmpty()) throw new IllegalArgumentException("targeted rendering multi expression is invalid: " + strExpr);
                map.put(name, WidgetRenderTargetExpr.fromString(subStrExpr));
            }
            this.map = map;
        }
        
        public static WidgetMultiRenderTargetExpr fromExprListString(String strExpr) throws IllegalArgumentException {
            WidgetMultiRenderTargetExpr expr = multiCache.get(strExpr);
            if (expr == null) {
                expr = new WidgetMultiRenderTargetExpr(strExpr);
                multiCache.put(strExpr, expr);
            }
            return expr;
        }
        
        public static WidgetMultiRenderTargetExpr fromMultiMap(Map<String, Object> map) throws IllegalArgumentException {
            return new WidgetMultiRenderTargetExpr(map);
        }
        
        public static WidgetMultiRenderTargetExpr fromSingle(WidgetRenderTargetExpr expr) {
            WidgetMultiRenderTargetExpr multiExpr = new WidgetMultiRenderTargetExpr();
            multiExpr.put(RenderTargetUtil.RENDERTARGETEXPR_MULTI_DEFAULT, expr);
            return multiExpr;
        }

        public int size() {
            return map.size();
        }

        public boolean isEmpty() {
            return map.isEmpty();
        }

        public boolean containsKey(Object key) {
            return map.containsKey(key);
        }

        public boolean containsValue(Object value) {
            return map.containsValue(value);
        }

        public WidgetRenderTargetExpr get(Object key) {
            return map.get(key);
        }

        public WidgetRenderTargetExpr put(String key, WidgetRenderTargetExpr value) {
            throw new UnsupportedOperationException();
        }

        public WidgetRenderTargetExpr remove(Object key) {
            throw new UnsupportedOperationException();
        }

        public void putAll(Map<? extends String, ? extends WidgetRenderTargetExpr> m) {
            throw new UnsupportedOperationException();
        }

        public void clear() {
            throw new UnsupportedOperationException();
        }

        public Set<String> keySet() {
            return map.keySet();
        }

        public Collection<WidgetRenderTargetExpr> values() {
            return map.values();
        }

        public Set<java.util.Map.Entry<String, WidgetRenderTargetExpr>> entrySet() {
            return map.entrySet(); // FIXME: prevent setValue
        }

        // TODO: REVIEW
//        public boolean equals(Object o) {
//            return map.equals(o);
//        }
//
//        public int hashCode() {
//            return map.hashCode();
//        }
    }
    
    
    /******************************************************/
    /* Token and name representations */
    /******************************************************/
    
    /**
     * Represents a generic name such as tag name, name attribute, id attribute, etc.
     * Implements the wildcard matching, so this gets parsed once and not re-parsed
     * at every single element in the screen.
     */
    public abstract static class Name {
        protected final String name;

        protected Name(String name) {
            this.name = name;
        }
        
        public static Name interpret(String name) {
            int wildIndex = name.indexOf(WILDCARD);
            if (wildIndex != name.lastIndexOf(WILDCARD))
                throw new IllegalArgumentException("Targeted rendering expression: name contains multiple wildcards (only one supported): " + name);
            
            if (wildIndex < 0) return new ExactName(name);
            else if (wildIndex == 0) {
                if (name.length() == 1)
                    return MatchAllWildName.getInstance();
                else
                    return new SuffixWildName(name, name.substring(1));
            }
            else if (wildIndex == name.length() - 1) return new PrefixWildName(name, name.substring(0, wildIndex));
            else return new PrefixSuffixWildName(name, name.substring(0, wildIndex), name.substring(wildIndex + 1));
        }
        
        public static Name interpretIfNotNull(String name) {
            if (name != null) return interpret(name);
            else return null;
        }
        
        public static Name interpretIfNotEmpty(String name) {
            if (name != null && !name.isEmpty()) return interpret(name);
            else return null;
        }
        
        public abstract boolean isWild();
        
        public abstract boolean matches(String targetName);
        
        public static boolean matches(Name expr, String targetName) {
            if (expr == null) return true; // if no expression, consider it matched
            else return expr.matches(targetName);
        }
        
        public abstract int getPrecision();
        
        @Override
        public String toString() { return name; }
        
        public static class ExactName extends Name {
            protected ExactName(String name) { super(name); }
            @Override
            public boolean isWild() { return false; }
            @Override
            public boolean matches(String targetName) { return targetName != null && targetName.equals(this.name); }
            @Override
            public int getPrecision() { return 1; }
        }
        
        public static abstract class WildName extends Name {
            public WildName(String name) { super(name); }
            @Override
            public boolean isWild() { return true; }
        }
          
        public static class MatchAllWildName extends WildName {
            private static final MatchAllWildName INSTANCE = new MatchAllWildName("*");
            protected MatchAllWildName(String name) { super(name); }
            public static final MatchAllWildName getInstance() { return INSTANCE; };
            @Override
            public boolean matches(String targetName) { return true; }
            @Override
            public int getPrecision() { return 99; }
        }
        
        public static class PrefixWildName extends WildName {
            protected final String prefix;
            protected PrefixWildName(String name, String prefix) {
                super(name);
                this.prefix = prefix;
            }
            @Override
            public boolean matches(String targetName) { return targetName != null && targetName.startsWith(prefix); }
            @Override
            public int getPrecision() { return 20; }
        }

        public static class SuffixWildName extends WildName {
            protected final String suffix;
            protected SuffixWildName(String name, String suffix) {
                super(name);
                this.suffix = suffix;
            }
            @Override
            public boolean matches(String targetName) { return targetName != null && targetName.endsWith(suffix); }
            @Override
            public int getPrecision() { return 20; }
        }
        
        public static class PrefixSuffixWildName extends WildName {
            protected final String prefix;
            protected final String suffix;
            protected PrefixSuffixWildName(String name, String prefix, String suffix) {
                super(name);
                this.prefix = prefix;
                this.suffix = suffix;
            }
            @Override
            public boolean matches(String targetName) { return targetName != null && targetName.startsWith(prefix) && targetName.endsWith(suffix); }
            @Override
            public int getPrecision() { return 20; }
        }
    }
    
    /**
     * Represents a space- or comma-separate entry in a query expression, intended to match an element.
     * <p>
     * TODO: this is missing some kind of normalization of the format and comparison logic,
     * that will be needed to fix ContainsExpr.
     */
    public abstract static class Token {
        protected final String strExpr;
//        protected final char type;
        protected final Name name; // this is the name following the type prefix, it's not necessarily "name" attribute
        
        // NO NEED for this, because we'll only ever support name and id, so use those instead for optimize
        //protected final Map<String, String> attrMap;
        protected final Name nameAttr;
        protected final Name idAttr;
        
        protected final String normStrExpr;
        
        // private, may split into subclasses later
        protected Token(String strExpr, char type, String name, Map<String, String> attrMap) {
            this.strExpr = strExpr;
//            this.type = type;
            this.name = Name.interpret(name);
            
            // OPTIMIZATION: no need to keep map for these, we only support two attributes
            this.nameAttr = Name.interpretIfNotNull(attrMap.get("name"));
            this.idAttr = Name.interpretIfNotNull(attrMap.get("id"));
            
            this.normStrExpr = makeNormStrExpr(type, this.name, idAttr, nameAttr); // FIXME: this is anti-polymorphism
        }
        
        /**
         * TODO: this is INCOMPLETE, it does not convert in-between types properly!
         * for now we're keeping the base name as the only real comparison part,
         * because ContainsExpr can't really compare attributes and it's very slow.
         */
        protected static String makeNormStrExpr(char type, Name name, Name idAttr, Name nameAttr) {
            StringBuilder sb = new StringBuilder();
            sb.append(type);
            if (type == MET_DECSEC_RECALL) {
                sb.append("/");
            } 
            sb.append(name.toString());
            makeNormAttrStr(sb, idAttr, nameAttr);
            return sb.toString();
        }
        
        protected static void makeNormAttrStr(StringBuilder sb, Name... attrs) {
            boolean opened = false;
            for(int i=0; i < attrs.length; i++) {
                Name attr = attrs[i];
                if (attr != null) {
                    if (!opened) {
                        sb.append(ATTR_OPEN);
                        opened = true;
                    }
                    sb.append(ATTR_NAMES.get(i));
                    sb.append(ATTR_EQUALS);
                    sb.append(attr.toString());
                }
            }
            if (opened) {
                sb.append(ATTR_CLOSE);
            }
        }
        
        public static Token interpret(String strExpr) throws IllegalArgumentException {
            if (WILDCARD_STRING.equals(strExpr)) {
                return AllMatcher.getInstance();
            }
            if (strExpr == null || strExpr.length() < 2) 
                throw new IllegalArgumentException("Empty or invalid render target expression token/name: " + strExpr);
            char type = strExpr.charAt(0);
            String rest = strExpr.substring(1);
            int bracketIndex = rest.indexOf(ATTR_OPEN);
            String name;
            Map<String, String> attrMap;
            if (bracketIndex > 0) {
                name = rest.substring(0, bracketIndex);
                int bracketCloseIndex = rest.lastIndexOf(ATTR_CLOSE);
                if (bracketCloseIndex <= bracketIndex)
                    throw new IllegalArgumentException("Empty or invalid render target expression token/name: bad attributes expression in brackets: " + strExpr);
                String attrStr = rest.substring(bracketIndex + 1, bracketCloseIndex);
                attrMap = parseTokenAttribStr(attrStr, strExpr);
            } else {
                name = rest;
                attrMap = Collections.emptyMap();
            }
            return getInstance(strExpr, type, name, attrMap);
        }
        
        public static Token getInstance(String strExpr, char type, String name, Map<String, String> attrMap) throws IllegalArgumentException {
            // FIXME: we will deny attributes on anything that isn't generic matcher for now
            // this will prevent more obscure errors until we fix the comparison code
            if (!(MET_GENERIC.contains(type) || MET_DECSEC == type) && !attrMap.isEmpty())
                throw new IllegalArgumentException("Render target expression has bracketed attributes"
                        + " - this is currently only supported for prefix types \"" 
                        + MET_ELEM + "\" and \"" + MET_DECSEC + "\"");
            Token token;
            switch(type) {
            case MET_SECNAME:
                token = new WidgetSectionMatcher(strExpr, type, name, attrMap);
                break;
            case MET_ELEMID:
                token = new IdMatcher(strExpr, type, name, attrMap);
                break;
            case MET_DECSECINCL:
                token = new DecoratorSectionIncludeMatcher(strExpr, type, name, attrMap);
                break;
            case MET_ELEM:
                token = new GenericWidgetElementMatcher(strExpr, type, name, attrMap);
                break;
//            case MET_FTLELEM:
//                token = new GenericFtlElementMatcher(strExpr, type, name, attrMap);
//                break;
            case MET_DECSEC:
                if (name.charAt(0) == MET_DECSEC_RECALL) {
                    token = new DecoratorScreenSectionRecallMatcher(strExpr, type, name.substring(1), attrMap);
                } else {
                    token = new DecoratorScreenSectionEarlyMatcher(strExpr, type, name, attrMap);
                }
                break;  
            case WILDCARD:
                throw new IllegalArgumentException("Invalid render target expression token/name type"
                        + " character prefix (should be one one: " + MET_ALL_STR + "), found wildcard: " + strExpr
                        + " NOTE: if you tried to define attributes on a catch-all wildcard with no type, this is currently not supported");
            default:
                throw new IllegalArgumentException("Invalid render target expression token/name type"
                        + " character prefix (should be one one: " + MET_ALL_STR + "): " + strExpr);
            }
            return token;
        }
        
        public static Map<String, String> parseTokenAttribStr(String attrStr, String strExpr) {
            if (attrStr == null || attrStr.length() <= 0)
                throw new IllegalArgumentException("Invalid render target expression token/name: invalid attributes expression: " + strExpr);
            String[] pairs = targetExprAttrSeparatorPat.split(attrStr);
            if (pairs.length <= 0)
                throw new IllegalArgumentException("Invalid render target expression token/name: invalid attributes expression: " + strExpr);
            Map<String, String> attrMap = new HashMap<>();
            for(String pair : pairs) {
                Matcher m = targetExprAttrAssignPat.matcher(pair);
                if (m.matches()) {
                    attrMap.put(m.group(1), m.group(3));
                } else {
                    throw new IllegalArgumentException("Invalid render target expression token/name:"
                            + " invalid attributes expression: " + strExpr
                            + " (failed to match assignment: [" + pair + "]) (NOTE: only id and name currently supported)");
                }
            }
            return attrMap;
        }
        
        /**
         * Returns the original full expression.
         */
        public String getStringExpr() {
            return strExpr;
        }
        
        /**
         * Returns a normalized expression of the original.
         * 
         * TODO: NOT IMPLEMENTED IN GENERAL
         */
        public String getNormStringExpr() {
            return normStrExpr;
        }
        
        /**
         * Returns a normalized expression specifically for comparison purposes,
         * for use with {@link org.ofbiz.widget.model.ContainsExpr}.
         * may CHANGE the expression.
         * 
         * TODO: NOT IMPLEMENTED IN GENERAL
         */
        public String getCmpNormStringExpr() {
            return strExpr;
        }

//        public char getType() {
//            return type;
//        }
        
        public char getType() {
            return strExpr.charAt(0);
        }

        public Name getName() {
            return name;
        }

        public boolean hasAttr() {
            return idAttr != null || nameAttr != null;
        }
        
//        public Map<String, String> getAttrMap() {
//            return attrMap;
//        }

        @Override
        public String toString() {
            return strExpr;
        }

        /**
         * Core comparison implementation for {@link WidgetRenderTargetState#handleShouldExecute}.
         * flawed design but works better for now.
         */
        protected abstract void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo);
        
        // turns out this is not needed for the time being. this was called as special case from 
//        /**
//         * Called back after a delayed (future) match comes back to us.
//         * Only some matcher types support this.
//         */
//        protected void handleShouldExecuteDelayed(ModelWidget widget, Map<String, Object> context,
//                RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
//            throw new UnsupportedOperationException("Internal error: Token/Matcher type does not support delayed widget matching: " + this.getClass().getName());
//        }
        
        /**
         * Current common implementation, this may get further split up or ruined later.
         */
        protected static boolean matchesWidget(ModelWidget widget, Map<String, Object> context,
                Name tagName, Name idAttr, Name nameAttr) {
            if (tagName != null && !tagName.matches(widget.getTagName())) {
                return false;
            }
            if (idAttr != null && !idAttr.matches(ModelWidget.getId(widget, context))) {
                return false;
            }
            if (nameAttr != null && !nameAttr.matches(ModelWidget.getName(widget, context))) {
                return false;
            }
            return true;
        }
        
        public static class AllMatcher extends Token {
            private static final AllMatcher INSTANCE = new AllMatcher();
            
            protected AllMatcher() {
                super(WILDCARD_STRING, WILDCARD, WILDCARD_STRING, null);
            }
            
            public static AllMatcher getInstance() { return INSTANCE; }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context, ExprStateInfo state,
                    ExecutionInfoImpl execInfo) {
                state.registerMatch(widget, execInfo, this);
            }
            @Override
            public String getStringExpr() { return WILDCARD_STRING; }
            @Override
            public String getNormStringExpr() { return WILDCARD_STRING; }
            @Override
            public String getCmpNormStringExpr() { return WILDCARD_STRING; }
            @Override
            public char getType() { return WILDCARD; }
            @Override
            public boolean hasAttr() { return false; };
        }
        
        public static class WidgetSectionMatcher extends Token {
            protected WidgetSectionMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if ((widget instanceof ModelScreenWidget.Section || widget instanceof ModelVirtualSectionFtlWidget) && 
                    matchesWidget(widget, context, null, idAttr, name)) {
                    state.registerMatch(widget, execInfo, this);
                }
            }
        }
        
        public static class IdMatcher extends Token {
            protected IdMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if (matchesWidget(widget, context, null, name, nameAttr)) {
                    state.registerMatch(widget, execInfo, this);
                }
            }
        }
        
        public static class DecoratorSectionIncludeMatcher extends Token {
            protected DecoratorSectionIncludeMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof ModelScreenWidget.DecoratorSectionInclude) {
                    if (name.matches(widget.getName())) {
                        state.registerMatch(widget, execInfo, this);
                    } else {
                        // SPECIAL: unlike most other widget, if the decorator-section name did not match, 
                        // we can safely exclude this element from rendering,
                        // because the selector only works on current include/decorator level.
                        execInfo.shouldExecuteCurrentExpr = false;
                    }
                }
            }
        }
        
        public static class DecoratorScreenSectionEarlyMatcher extends Token {
            private final String normCmpStrExpr;
            
            protected DecoratorScreenSectionEarlyMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
                // SPECIAL: to prevent issues for now, force the name to be "decorator-screen"
                if (!"decorator-screen".equals(name)) {
                    throw new IllegalArgumentException("Invalid render target expression token/name: invalid decorator-screen selection expression: " + strExpr);
                }
                this.normCmpStrExpr = MET_ELEM + getNormStringExpr().substring(1);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof ModelScreenWidget.DecoratorScreen && matchesWidget(widget, context, name, idAttr, nameAttr)) {
                    ModelScreenWidget.DecoratorScreen decWidget = (ModelScreenWidget.DecoratorScreen) widget;
                    state.registerMatch(decWidget, execInfo, this);
                }
            }
            
            @Override
            public String getCmpNormStringExpr() {
                return normCmpStrExpr;
            }
        }
        
        public static class DecoratorScreenSectionRecallMatcher extends Token {
            private final String normCmpStrExpr;
            
            protected DecoratorScreenSectionRecallMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
                this.normCmpStrExpr = MET_ELEM + "decorator-section[name='" + name + "']";
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof ModelScreenWidget.DecoratorSection) {
                    Object sectionsObj = context.get("scpCurrentSections");
                    if (sectionsObj instanceof SectionsRenderer) {
                        SectionsRenderer sections = (SectionsRenderer) sectionsObj;
                        Token earlyMatcher = state.matchedWidgets.get(sections.getSourceDecoratorScreen());
                        if (earlyMatcher instanceof DecoratorScreenSectionEarlyMatcher) {
                            if (getName().matches(ModelWidget.getName(widget, context))) {
                                state.registerMatch(widget, execInfo, this);
                            }
                        }
                    } else {
                        Debug.logError("Targeted rendering: \"scpCurrentSections\" missing from"
                                + " or unexpected type in context when trying to match: " + this.toString()
                                + " (full: " + state.getExpr().toString() + ")", module);
                    }
                }
            }
            
            @Override
            public String getCmpNormStringExpr() {
                return normCmpStrExpr;
            }
        }
        
        public static class GenericWidgetElementMatcher extends Token {
            protected GenericWidgetElementMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
                if (matchesWidget(widget, context, name, idAttr, nameAttr)) {
                    state.registerMatch(widget, execInfo, this);
                }
            }
        }
        
//        public static class GenericFtlElementMatcher extends Token {
//            protected GenericFtlElementMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
//                super(strExpr, type, name, attrMap);
//            }
//
//            @Override
//            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
//                    WidgetRenderTargetState.ExprStateInfo state, WidgetRenderTargetState.ExecutionInfoImpl execInfo) {
//                if (widget instanceof FtlWrapperWidget && matchesWidget(widget, context, name, idAttr, nameAttr)) {
//                    state.registerMatch(widget, execInfo, this);
//                }
//            }
//        }
    }

    /******************************************************/
    /* Getters/Info */
    /******************************************************/
    
    public String getStringExpr() {
        return strExpr;
    }
    
    @Override
    public String toString() {
        return getStringExpr();
    }
    
    public List<Token> getTokens() {
        return Collections.unmodifiableList(tokens);
    }
    
    public int getNumTokens() {
        return tokens.size();
    }
    
    public Token getToken(int index) {
        if (index < tokens.size()) {
            return tokens.get(index);
        } else {
            return null;
        }
    }
    
    public Token getLastToken() {
        return tokens.get(tokens.size() - 1);
    }
    
//    public boolean isUseChildContent() {
//        return useChildContent;
//    }

    /******************************************************/
    /* Context */
    /******************************************************/
    
    /**
     * Gets state from context (actually global context) or request attributes or returns the disabled instance.
     * <p>
     * NOTE: this is best-effort, tries to avoid too many lookups too.
     * 
     * @return the RenderTargetState instance, or the {@link WidgetRenderTargetState#DISABLED} instance if none found
     */
    public static WidgetRenderTargetState getRenderTargetState(Map<String, Object> context) {
        WidgetRenderTargetState state = (WidgetRenderTargetState) context.get(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
        if (state != null) return state;
        else {
            ServletRequest request = (ServletRequest) context.get("request");
            if (request != null) {
                state = (WidgetRenderTargetState) request.getAttribute(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
                if (state != null) return state;
            }
            return WidgetRenderTargetState.DISABLED;
        }
    }
    
    public static WidgetRenderTargetState getRenderTargetState(ServletRequest request) {
        WidgetRenderTargetState state = (WidgetRenderTargetState) request.getAttribute(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
        if (state != null) return state;
        return WidgetRenderTargetState.DISABLED;
    }
    
    /**
     * Gets state from context (actually global context) or request attributes if issues.
     * <p>
     * NOTE: this is best-effort, tries to avoid too many lookups too.
     */
    public static WidgetRenderTargetState getRenderTargetStateOrNull(Map<String, Object> context) {
        WidgetRenderTargetState state = (WidgetRenderTargetState) context.get(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
        if (state != null) return state;
        else {
            ServletRequest request = (ServletRequest) context.get("request");
            if (request != null) return (WidgetRenderTargetState) request.getAttribute(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
            return null;
        }
    }
    
    public static WidgetRenderTargetState getRenderTargetStateOrNull(ServletRequest request) {
        return (WidgetRenderTargetState) request.getAttribute(RenderTargetUtil.RENDERTARGETSTATE_ATTR);
    }
    
    /**
     * Sets the following global context vars AND request attributes:
     * scpRenderTargetState - RenderTargetState instance
     * scpRenderTargetOn - convenience boolean (NOTE: should NOT be used internally - use <code>scpRenderTargetState.isEnabled()</code>)
     */
    public static void populateRenderTargetVars(Map<String, Object> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        Object scpRenderTargetExpr = RenderTargetUtil.getRawRenderTargetExpr(request);
        populateRenderTargetVars(context, request, WidgetRenderTargetExpr.fromObjectOrMulti(scpRenderTargetExpr));
    }
    
    /**
     * Sets the following global context vars AND request attributes:
     * scpRenderTargetState - RenderTargetState instance
     * scpRenderTargetOn - convenience boolean (NOTE: should NOT be used internally - use <code>scpRenderTargetState.isEnabled()</code>)
     */
    public static void populateRenderTargetVars(Map<String, Object> context, WidgetRenderTargetExprBase expr) {
        populateRenderTargetVars(context, (HttpServletRequest) context.get("request"), expr);
    }
    
    /**
     * Sets the following global context vars AND request attributes:
     * scpRenderTargetState - RenderTargetState instance
     * scpRenderTargetOn - convenience boolean (NOTE: should NOT be used internally - use <code>scpRenderTargetState.isEnabled()</code>)
     */
    public static void populateRenderTargetVars(Map<String, Object> context, HttpServletRequest request, WidgetRenderTargetExprBase expr) {
        Map<String, Object> globalContext = UtilGenerics.checkMap(context.get("globalContext"));
        
        WidgetRenderTargetState state = WidgetRenderTargetState.fromExprOrDisabled(expr);
        String multiPrefix = null;
        boolean multi = state.isEnabled() && state.isMulti();
        if (multi && request != null) {
            // NOTE: this prefix may already be set OR we have to generate and store a new one here; 
            // the method does it all except globalContext
            multiPrefix = RenderTargetUtil.getOrGenerateSetMultiTargetDelimiterPrefix(request);
            state.setMultiPrefix(multiPrefix);
        }
        
        if (globalContext != null) {
            globalContext.put(RenderTargetUtil.RENDERTARGETSTATE_ATTR, state);
            globalContext.put(RenderTargetUtil.RENDERTARGETON_ATTR, state.isEnabled());
            if (multi) {
                globalContext.put(RenderTargetUtil.MULTITARGET_DELIM_PREFIX_ATTR, multiPrefix);
            }
        }
        
        if (request != null) {
            request.setAttribute(RenderTargetUtil.RENDERTARGETSTATE_ATTR, state);
            request.setAttribute(RenderTargetUtil.RENDERTARGETON_ATTR, state.isEnabled());
            // already done by method
//            if (multi) {
//                request.setAttribute(RenderTargetUtil.MULTITARGET_DELIM_PREFIX_ATTR, multiPrefix);
//            }
        }
    }
    
    /**
     * Returns true if target matched and currently outputting or if targeting disabled.
     * <p>
     * Currently (2017-05-09), this checks if writer is a RenderWriter; if so, returns true if
     * is not discarding output. If not a RenderWriter, try to lookup the RenderTargetState
     * in context and call {@link WidgetRenderTargetState#shouldOutput()}.
     */
    public static boolean shouldOutput(Appendable writer, Map<String, Object> context) {
        if (writer instanceof RenderWriter) {
            return !((RenderWriter) writer).isDiscarding();
        } else {
            return getRenderTargetState(context).shouldOutput();
        }
    }
    
    /**
     * Render Target State - Holds and manages the state information for targeted rendering.
     * <p>
     * NOTE: a reference is stored in both globalContext and request attributes,
     * so it should be accessible from most anywhere, as long as they aren't lost.
     */
    public static class WidgetRenderTargetState implements RenderTargetExpr.RenderTargetState, Serializable {
        public static final WidgetRenderTargetState DISABLED = new DisabledWidgetRenderTargetState();
        
        private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
        
        private final WidgetRenderTargetExprBase expr;
        
        //private ModelScreenWidget.DecoratorScreen skippedDecorator = null; // TODO? can't exploit (see below)
        private String multiPrefix = null;
        
        private Map<String, ExprStateInfo> exprStateMap;
        
        private Set<ExprStateInfo> activeTargets; // targets currently matching NOTE: due to nesting, it's possible for this to contain more than one at a time
        private Set<ExprStateInfo> unmatchedTargets; // targets never fully matched
        private Set<ExprStateInfo> finishedTargets; // targets completely done
        private boolean finished = false; // explicit finished flag, usually true once finishedTargets.size() == exprStates.size()
        
        class ExprStateInfo {
            private String name;
            private WidgetRenderTargetExpr expr;
            private boolean targetMatched = false;
            private int nextTokenIndex = 0;
            private Map<ModelWidget, Token> matchedWidgets = new HashMap<>();
            
            public ExprStateInfo(String name, WidgetRenderTargetExpr expr) {
                this.name = name;
                this.expr = expr;
            }

            public String getName() { return name; }
            public WidgetRenderTargetExpr getExpr() { return expr; }
            public Token getNextToken() { return expr.getToken(nextTokenIndex); }
            public List<Token> getNextTokenAndChildren() { return expr.tokens.subList(nextTokenIndex, expr.tokens.size()); }
            
            /**
             * NOTE: in the future could be need for parameters such as: boolean fullMatched, boolean consumeToken
             * but for now, trying to keep syntax straightforward enough so that 1 token ~= 1 full match.
             */
            void registerMatch(ModelWidget widget, ExecutionInfoImpl execInfo, Token matcher) {
                if (targetMatched) throw new IllegalStateException("Tried to modify RenderTargetState matches (register match)"
                        + " after already found target match (should be read-only)");
                this.matchedWidgets.put(widget, matcher);
                this.nextTokenIndex++;
                execInfo.registerExprTokenMatchedWidget(this);
                if (nextTokenIndex >= expr.getNumTokens()) {
                    this.targetMatched = true;
                    unmatchedTargets.remove(this);
                    activeTargets.add(this);
                }
            }
            
            void deregisterMatch(ModelWidget widget, ExecutionInfoImpl execInfo) {
                if (this.targetMatched) {
                    // if we were matched, we do not need to modify our own state.
                    activeTargets.remove(this);
                    finishedTargets.add(this);
                } else {
                    // never happens
                    //if (targetMatched) throw new IllegalStateException("Tried to modify RenderTargetState matches (deregister match)"
                    //        + " after already found target match (should be read-only)");
                    this.matchedWidgets.remove(widget);
                    this.nextTokenIndex--;
                }
            }
        }
        
        private WidgetRenderTargetState(WidgetMultiRenderTargetExpr expr) {
            this.expr = expr;
            this.exprStateMap = new HashMap<>();
            for(Map.Entry<String, WidgetRenderTargetExpr> entry : expr.entrySet()) {
                this.exprStateMap.put(entry.getKey(), 
                        new ExprStateInfo(entry.getKey(), entry.getValue()));
            }
            this.unmatchedTargets = new HashSet<>(this.exprStateMap.values());
            this.activeTargets = new HashSet<>();
            this.finishedTargets = new HashSet<>();
        }
        
        private WidgetRenderTargetState(WidgetRenderTargetExpr expr) {
            this.expr = expr;
            this.exprStateMap = new HashMap<>();
            this.exprStateMap.put(RenderTargetUtil.RENDERTARGETEXPR_MULTI_DEFAULT, 
                    new ExprStateInfo(RenderTargetUtil.RENDERTARGETEXPR_MULTI_DEFAULT, expr));
            this.unmatchedTargets = new HashSet<>(this.exprStateMap.values());
            this.activeTargets = new HashSet<>();
            this.finishedTargets = new HashSet<>();
        }
        
        private WidgetRenderTargetState() {
            this.expr = null;
        }
        
        public static WidgetRenderTargetState fromExprOrDisabled(WidgetRenderTargetExprBase expr) {
            if (expr instanceof WidgetRenderTargetExpr) {
                return new WidgetRenderTargetState((WidgetRenderTargetExpr) expr);
            } else if (expr instanceof WidgetMultiRenderTargetExpr) {
                return new WidgetRenderTargetState((WidgetMultiRenderTargetExpr) expr);
            } else {
                return DISABLED;
            }
        }

        // INFORMATION

        public boolean isEnabled() {
            return true;
        }
        
        public boolean isFinished() {
            return finished;
        }

        public WidgetRenderTargetExprBase getExpr() {
            return expr;
        }
        
        /**
         * True if should output markup. This is only ever true if we matched the target.
         * NOTE: Code in ofbiz source files should call
         * {@link WidgetRenderTargetExpr#shouldOutput(Appendable, Map)} now instead in order to
         * better centralize the logic.
         */
        public boolean shouldOutput() {
            return !finished && isTargetMatched();
        }
        
        public boolean isTargetMatched() {
            return activeTargets.size() > 0;
        }
        
        // STATE UPDATE
        
        /**
         * Marks finished, which prevents further rendering output at the least.
         * NOTE: Normally, {@link #handleFinished} should decide this.
         */
        public void markFinished() {
            this.finished = true;
        }
        
        public boolean isMulti() {
            return (expr instanceof WidgetMultiRenderTargetExpr);
        }
        
        public String getMultiPrefix() {
            return multiPrefix;
        }

        public void setMultiPrefix(String multiPrefix) {
            this.multiPrefix = multiPrefix;
        }
        
        /**
         * Sets up the Writer for targeted rendering if enabled and needed.
         */
        public Writer prepareWriter(Appendable writer, Map<String, Object> context) {
            if (!(writer instanceof RenderWriter)) {
                Writer w = (Writer) writer; // FIXME: currently assuming everything is a Writer, ofbiz utils already assume this
                return SwitchRenderWriter.getInstance(w, false, false);
            } else {
                return (Writer) writer;
            }
        }
        
        /**
         * Checks if should execute and enter the widget, and updates state.
         * <p>
         * We ALWAYS have to execute the widget actions UNLESS it's explicitly blacklisted and unless
         * it comes after the target. But note, this execution/actions is decoupled from the need
         * to output markup/html.
         */
        public ExecutionInfo handleShouldExecute(ModelWidget widget, Appendable writer, Map<String, Object> context, Object stringRenderer) throws IOException {
            // NOTE: by default, shouldExecute = true, because everything may contain our target(s).
            ExecutionInfoImpl execInfo = new ExecutionInfoImpl(widget, true, writer);
            if (finished) {
                execInfo.shouldExecute = false;
            } else {
                // SPECIAL: initialize/intercept writer, BEST-EFFORT 
                // try to convert the writer, which may be either response.getWriter() or
                // a StringWriter or something else, to a SwitchRenderWriter with outputting off.
                // we do this check at EVERY element because there are numerous instances of Writer created
                // everywhere; this is BEST-EFFORT attempt to intercept and convert them.
                // WARN: TODO: REVIEW: not guaranteed to catch all... only time will tell...
                if (!(writer instanceof RenderWriter)) {
                    Writer w = (Writer) writer; // FIXME: currently assuming everything is a Writer, ofbiz utils already assume this
                    execInfo.writerForElem = SwitchRenderWriter.getInstance(w, false, false);
                }

                if (unmatchedTargets.size() > 0) {
                    execInfo.shouldExecute = false; 
                    for(ExprStateInfo exprState : new ArrayList<>(unmatchedTargets)) { // FIXME: COPY required to fix concurrent mod
                        execInfo.shouldExecuteCurrentExpr = true;
                        
                        // not needed for now: turns out not needed for the complex selectors... yet
//                      // SPECIAL CASE: check for delayed matches - these are pre-allowed
//                      Token delayedMatchToken = this.delayedMatches.get(widget);
//                      if (delayedMatchToken != null) {
//                          delayedMatchToken.delayedMatches(widget, context, this, execInfo);
//                      } else {

                        // Compare the widget against the current token in the expression, stored in the state
                        // results are marked in execInfo
                        Token token = exprState.getNextToken();
                        token.handleShouldExecute(widget, context, exprState, execInfo);
                        
                        // Finalize
                        if (execInfo.shouldExecuteCurrentExpr) {
                            if (exprState.targetMatched) {
                                // MATCHED!
                                // Turn on the switch and hope for the best
                                SwitchRenderWriter switchWriter = (SwitchRenderWriter) execInfo.writerForElem;
                                switchWriter.useOrigWriter();
                                if (isMulti()) {
                                    switchWriter.beginSection(exprState.getName(), getMultiPrefix());
                                }
                            } else {
                                if (widget instanceof ContainsExpr.FlexibleContainsExprAttrWidget) {
                                    // Blacklist support, for execution optimization:
                                    // SPECIAL: <[section/element] contains="[expr]"> expression means we can potentially
                                    // skip executing sections/elements that we have been told do NOT contain the sections or 
                                    // elements we're after, in a blacklist-like fashion.
                                    // If ANY of the names are considered not contained (are blacklisted), 
                                    // we don't need to enter the widget.
                                    ContainsExpr containsExpr = ((ContainsExpr.FlexibleContainsExprAttrWidget) widget).getContainsExpr(context);
                                    if (!containsExpr.matchesAllNameTokens(exprState.getNextTokenAndChildren())) {
                                        execInfo.shouldExecuteCurrentExpr = false;
                                    }
                                }
                            }
                            
                            // for the widget to NOT execute, ALL of the shouldExecuteCurrentExpr must be false.
                            if (execInfo.shouldExecuteCurrentExpr) {
                                execInfo.shouldExecute = true;
                            }
                        }
                    }
                    if (RenderTargetUtil.DEBUG && !execInfo.shouldExecute) {
                        Debug.logInfo("Targeted rendering: filtered out execution of widget element"
                                + widget.getLogWidgetLocationString(), module);
                    }
                }
            }
            return execInfo;
        }
        
        public interface ExecutionInfo {
            /**
             * Returns true if the widget should be executed/rendered with actions.
             * NOTE: this is separate from whether it should output.
             */
            boolean shouldExecute();
            
            /**
             * Returns the writer that the element should use for its rendering.
             * The state may decide a different one is needed.
             */
            Appendable getWriterForElementRender();
            
            /**
             * Must be called after return from executing the target widget.
             * Should be in a finally block.
             * Behavior:
             * <ul>
             * <li>If this is the target widget, we mark finished and the state becomes read-only.
             * <li>If we're returning from a matched widget, we pop it, so something else will get a chance
             *     at the match entry.
             * </ul>
             */
            void handleFinished(Map<String, Object> context) throws IOException;
        }
        
        public class ExecutionInfoImpl implements ExecutionInfo {
            ModelWidget widget;
            boolean shouldExecute; // the whole node
            boolean shouldExecuteCurrentExpr; // the current expression
            Appendable writerForElem;

            /**
             * any token matched in any expr that matched the widget will generate an entry in this.
             */
            List<ExprStateInfo> exprTokenMatchedWidget;
            
            ExecutionInfoImpl(ModelWidget widget, boolean shouldExecute, Appendable writerForElem) {
                this.widget = widget;
                this.shouldExecute = shouldExecute;
                this.shouldExecuteCurrentExpr = shouldExecute;
                this.writerForElem = writerForElem;
            }
            
            public void registerExprTokenMatchedWidget(ExprStateInfo state) {
                if (exprTokenMatchedWidget == null) exprTokenMatchedWidget = new ArrayList<>();
                exprTokenMatchedWidget.add(state);
            }
            
            @Override
            public boolean shouldExecute() { return shouldExecute; }
            @Override
            public Appendable getWriterForElementRender() { return writerForElem; }
            @Override
            public void handleFinished(Map<String, Object> context) throws IOException  {
                if (exprTokenMatchedWidget != null) {
                    ListIterator<ExprStateInfo> li = exprTokenMatchedWidget.listIterator(exprTokenMatchedWidget.size());
                    while(li.hasPrevious()) {
                        ExprStateInfo state = li.previous();
                        SwitchRenderWriter switchWriter = (SwitchRenderWriter) writerForElem;
                        if (isMulti()) {
                            switchWriter.endSection(state.getName(), getMultiPrefix());
                        }
                        state.deregisterMatch(widget, this);
                    }
                    // if we just finished OR we are in between expressions, switch to dummy writer
                    if (finishedTargets.size() >= exprStateMap.size()) {
                        finished = true;
                        ((SwitchRenderWriter) writerForElem).useAltWriter();
                    } else if (activeTargets.size() == 0) {
                        ((SwitchRenderWriter) writerForElem).useAltWriter();
                    }
                } 
            }
        }

        public static final class DisabledWidgetRenderTargetState extends WidgetRenderTargetState {
            private DisabledWidgetRenderTargetState() { super(); }
            @Override
            public boolean isEnabled() { return false; }
            @Override
            public boolean isFinished() { return false; }
            @Override
            public boolean isTargetMatched() { return false; }
            @Override
            public WidgetRenderTargetExpr getExpr() { return null; }
            @Override
            public boolean shouldOutput() { return true; }
            @Override
            public void markFinished() { ; }
            @Override
            public Writer prepareWriter(Appendable writer, Map<String, Object> context) { return (Writer) writer; }
            @Override
            public ExecutionInfo handleShouldExecute(ModelWidget widget, Appendable writer, Map<String, Object> context, Object stringRenderer) { return new DisabledExecutionInfoImpl(writer); }
            @Override
            public String getMultiPrefix() { return null; }
            @Override
            public void setMultiPrefix(String multiPrefix) { ; }
            @Override
            public boolean isMulti() { return false; }

            public static class DisabledExecutionInfoImpl implements ExecutionInfo {
                final Appendable writer;
                DisabledExecutionInfoImpl(Appendable writer) { this.writer = writer; }
                @Override
                public boolean shouldExecute() { return true; }
                @Override
                public void handleFinished(Map<String, Object> context) {}
                @Override
                public Appendable getWriterForElementRender() { return writer; }
            }
        }
    }
    
    /******************************************************/
    /* Old comments  */
    /******************************************************/
    
    // OLD COMMENTS
    // This code was previously in the state handleShouldExecute, it still contains
    // some explanations are to why things are but this hasn't been updated in awhile.
    
    // NOTE: the following double-commented hack for PlatformSpecific
    // is being handled a different way to address point number 1.
    // comment is left for reference and because half the issues still apply:
    //} else if (widget instanceof ModelScreenWidget.PlatformSpecific) {
    //    /* ***************************************************************************************
    //     * !!! FIXME !!!
    //     * this logic is a major flaw in the targetting.
    //     * we currently cannot control the Freemarker output at all, so we're
    //     * forced to do all-or-nothing: we cannot enter any Freemarker files until we have
    //     * already found the target and outputting is enabled.
    //     * this means we cannot have things like intermediate FTL files implementing the
    //     * decorator, even if the target is a widget included by the FTL.
    //     * (if the target is a html or macro in the FTL file, it is even less likely to be possible).
    //     * 
    //     * the solution involves manipulating Environment.setOut to a dummy writer,
    //     * but it's complicated by the "screens", "sections", and other objects that hold references
    //     * to the writer, and various problems.
    //     * (for trying to target macros by ID, i.e. future improvement, this appears almost impossible - 
    //     * only possible is converting macros from the standard API to java transforms to manipulate writer, 
    //     * but this is a nightmare)
    //     * 
    //     * to clarify, there are 2 scenarios:
    //     * 1) trying to target a widget element that is included through an FTL file, 
    //     *    in other words supporting FTL boundaries, in other words support FTL-implemented decorators
    //     *    -> FIXME A.S.A.P.
    //     * 2) trying to target an element (macro) defined in an FTL file itself
    //     *    -> realistically, this could be practically impossible or too damaging to our code to implement.
    //     */
    //    if (!shouldOutput()) {
    //        shouldExecute = false;
    //    }
    /* ***************************************************************************************
       TODO? dedicated selector for include/decorator does not make much sense, because name even with location is an unreliable
       way to identify node (non-unique)
     * ***************************************************************************************
    } else if (widget instanceof ModelScreenWidget.IncludeScreen) {
        ModelScreenWidget.IncludeScreen include = (ModelScreenWidget.IncludeScreen) widget;
        if (type == '~' && ("INCLUDE".equals(name) || name.equals(include.getName(context)))) {
            registerMatch(widget);
            matchRegistered = true;
        }
    } else if (widget instanceof ModelScreenWidget.DecoratorScreen) {
        ModelScreenWidget.DecoratorScreen decorator = (ModelScreenWidget.DecoratorScreen) widget;
        if (type == '~' && ("INCLUDE".equals(name) || name.equals(decorator.getName(context)))) {
            registerMatch(widget);
            matchRegistered = true;
        }
    */
    /* ***************************************************************************************
        TODO?: REVIEW/FUTURE: it is currently impossible to skip decorator invocation in a useful way using 
        the special "decorator-section" manipulation that was attempted below.
        originally this selector/feature was meant to select a "decorator-section" without having
        to know anything about the invoked decorator's implementation.
        this would have been a bit like "copy pasting" the decorator-section contents and running them on
        their own, but additional operations are required which make this unusable:
        
        SOLN 1: (safe, but non-optimizable) 
        the essential decorator (global) actions and security checks can only be guaranteed by
        entering the decorator and letting it run... but this means we have to
        execute everything (all actions and enter all sections) in the decorator because it is 
        computationally impossible to predict the element tree where the decorator-section-include (that names our target decorator-section)
        will occur. so this feature - if implemented in a safe way - will prevent any optimization.
        
        SOLN 2: (security risk, functionality not guaranteed)
        a completely different solution involves skipping the decorator execution entirely,
        or trying to extract its actions (greater scope than this class)... but this is best-effort and likely to be full or errors, 
        and it is a MAJOR security risk to allow bypassing execution from an AJAX request... 
        essentially it can bypass the major permission checks,
        which the controller (through view rendering) already relies on heavily.
        there is no way to safely/assuredly reproduce the permission checks done by the decorators, making it a major security risk.
        also it can't be guaranteed that the necessary global actions will be properly replicated either,
        so even functionality is not assured.
     * ***************************************************************************************
    } else if (widget instanceof ModelScreenWidget.DecoratorScreen) {
        ModelScreenWidget.DecoratorScreen decorator = (ModelScreenWidget.DecoratorScreen) widget;
        if (type == '~' && ("INCLUDE".equals(name) || name.equals(decorator.getName(context)))) { // FIXME: does not support location compare
            matchDepth++;
            // SPECIAL: identify if this is decorator implementation to skip through
            String nextToken = getNextToken();
            if (nextToken != null && nextToken.charAt(0) == '^') {
                this.skippedDecorator = decorator;
            }
        } else if (type == '^') { // SPECIAL: identify if this is decorator implementation to skip through
            this.skippedDecorator = decorator;
        }
    } else if (widget instanceof ModelScreenWidget.DecoratorSection) {
        // SPECIAL
        if (type == '^' && name.equals(widget.getName())) {
            matchDepth++;
        }
    */
}
