package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.Token;
import org.w3c.dom.Element;

/**
 * SCIPIO: Widget screen section contains-expression - special expression that instructs renderer which sections 
 * contain or don't contain which other sections and elements.
 * <p>
 * See widget-screen.xsd "attlist.generic-screen-widget-elem" "contains" attribute for details.
 * <p>
 * In targeted rendering, this is used by widget renderer to determine which sections can be skipped entirely (their
 * full execution including actions, not just output).
 * <p>
 * Usually functions in blacklist fashion ("!"), so that by default all sections are said to possibly contain
 * all others.
 * <p>
 * TODO: currently this is unable to consolidate the ^ and % operators, or any other operators
 * for that matter. attributes and wildcards won't work as expected.
 * STILL NEED TO IMPLEMENT WidgetRenderTargetExpr NORMALIZATION AND HANDLING
 * FROM ContainsExpr SO THAT EXCLUDE OPTIMIZATIONS ARE FULLY HONORED.
 * Currently, only some simple exclusions based on $ and # operators work at all.
 * <p>
 * ex:
 * <pre>{@code
 * "$MySection1, !$MySection2, !$MySections-*, #MyContainerId, !#myContainerId2, *"
 * }</pre>
 */
@SuppressWarnings("serial")
public class ContainsExpr implements Serializable {
    public static final String module = ContainsExpr.class.getName();

    private static final UtilCache<String, ContainsExpr> cache = UtilCache.createUtilCache("widget.screen.containsexpr");
    
    // entries are comma-separated
    private static final Pattern containsExprTokenSplitPat = Pattern.compile("\\s*,\\s*");

    // general language
    public static final char EXCLUDE = '!';

    // pre-build expressions
    public static final ContainsExpr MATCH_ALL = new MatchAllContainsExpr();
    public static final ContainsExpr MATCH_NONE = new MatchNoneContainsExpr();
    public static final ContainsExpr DEFAULT = MATCH_ALL;
    static {
        // SPECIAL: pre-cache the default instances so they'll always be the ones looked up
        cache.put(MATCH_ALL.getStrExpr(), MATCH_ALL);
        cache.put(MATCH_NONE.getStrExpr(), MATCH_NONE);
        //cache.put(DEFAULT.getStrExpr(), DEFAULT);
    }
    
    /* 
     * FIXME:
     * this whole class needs to be updated to support the Token expressions.
     * currently the "%" type and bracket attributes won't work properly.
     * I have not figured out the way to implement the comparison logic.
     * the current comparison logic is "dumb".
     */
    
    // NOTE: these follow natural specificity
    private final String strExpr;
    private final Set<String> exactIncludes;
    private final Set<String> exactExcludes;
    private final List<String> wildIncludes; // TODO: find way to optimize
    private final List<String> wildExcludes; // TODO: find way to optimize
    private final Map<Character, Boolean> matchAllTypes;
    private final boolean matchAll;
    
    public ContainsExpr(String strExpr) throws IllegalArgumentException {
        this(strExpr, null);
    }
    
    public ContainsExpr(String strExpr, Element widgetElement) throws IllegalArgumentException {
        this.strExpr = strExpr;
        String[] tokenArr = containsExprTokenSplitPat.split(strExpr.trim());
        if (tokenArr.length <= 0) throw new IllegalArgumentException(makeErrorMsg("no names in expression", null, strExpr, widgetElement));
        
        // NOTE: these are layered by specificity from most exact to most generic
        Set<String> exactIncludes = new HashSet<>();
        Set<String> exactExcludes = new HashSet<>();
        ArrayList<String> wildIncludes = new ArrayList<>();
        ArrayList<String> wildExcludes = new ArrayList<>();
        Map<Character, Boolean> matchAllTypes = new HashMap<>();
        Boolean matchAll = null;

        for(String fullToken : tokenArr) {
            String tokenStr = fullToken;
            boolean exclude = (tokenStr.charAt(0) == EXCLUDE);
            if (exclude) tokenStr = tokenStr.substring(1);

            if (WidgetRenderTargetExpr.WILDCARD_STRING.equals(tokenStr)) { // special case
                matchAll = !exclude;
                continue;
            } else if (tokenStr.isEmpty()) {
                throw new IllegalArgumentException(makeErrorMsg("invalid or empty name", fullToken, strExpr, widgetElement));
            }

            // OLD - this did not extract the attributes
//            char type = tokenStr.charAt(0);
//            if (!WidgetRenderTargetExpr.MET_ALL.contains(type)) 
//                throw new IllegalArgumentException(makeErrorMsg("name has missing"
//                        + " or invalid type specifier (should start with one of: " + WidgetRenderTargetExpr.MET_ALL_STR + ")", fullToken, strExpr, widgetElement));
//            String name = tokenStr.substring(1);
            
            Token token;
            try {
                token = Token.interpret(tokenStr);
            } catch(Exception e) {
                throw new IllegalArgumentException(makeErrorMsg(e.getMessage(), fullToken, strExpr, widgetElement));
            }
            String name = token.getName().toString();
            char type = token.getType();
            String normTokenStr = token.getCmpNormStringExpr();
            
            // FIXME: this does not properly compare supported Tokens
            // nor does it recognized the bracketed attributes 
            // because we are ditching the Token instances
            
            if (name.equals(WidgetRenderTargetExpr.WILDCARD_STRING)) {
                // FIXME: missing attribute support - we are ditching attributes!
                if (token.hasAttr()) {
                    Debug.logWarning(makeErrorMsg("token wildcard expression has attributes; attributes not yet supported here; attributes ignored!", fullToken, strExpr, widgetElement), module);
                }
                matchAllTypes.put(type, !exclude);
            } else if (name.contains(WidgetRenderTargetExpr.WILDCARD_STRING)) {
                if (name.indexOf(WidgetRenderTargetExpr.WILDCARD) != name.lastIndexOf(WidgetRenderTargetExpr.WILDCARD)) { // TODO?: support multiple wildcard
                    throw new UnsupportedOperationException(makeErrorMsg("name has"
                            + " with multiple wildcards, which is not supported", fullToken, strExpr, widgetElement));
                }
                // FIXME: missing attribute support - we are ditching attributes!
                if (token.hasAttr()) {
                    Debug.logWarning(makeErrorMsg("token wildcard expression has attributes; attributes not yet supported here; attributes ignored!", fullToken, strExpr, widgetElement), module);
                }
                if (exclude) {
                    wildExcludes.add(type + name);
                } else {
                    wildIncludes.add(type + name);
                }
            } else {
                if (token.hasAttr()) {
                    Debug.logWarning(makeErrorMsg("token expression has attributes; not fully supported here; may not work as expected", fullToken, strExpr, widgetElement), module);
                }
                // FIXME: we lose Token instance here
                if (exclude) {
                    exactExcludes.add(normTokenStr);
                } else {
                    exactIncludes.add(normTokenStr);
                }
            }
        }
        
        this.exactIncludes = exactIncludes.isEmpty() ? Collections.<String> emptySet() : exactIncludes;
        this.exactExcludes = exactExcludes.isEmpty() ? Collections.<String> emptySet() : exactExcludes;
        wildIncludes.trimToSize();
        this.wildIncludes = wildIncludes.isEmpty() ? null : wildIncludes;
        wildExcludes.trimToSize();
        this.wildExcludes = wildExcludes.isEmpty() ? null : wildExcludes;
        this.matchAllTypes = matchAllTypes.isEmpty() ? null : matchAllTypes;
        if (matchAll == null) {
            Debug.logWarning(makeErrorMsg("missing match-all (\"*\") or match-none (\"!*\") wildcard entry;"
                    + " cannot tell if blacklist or whitelist intended", null, strExpr, widgetElement), module);
            matchAll = ContainsExpr.DEFAULT.matches("dummy"); // stay consistent with the default
        }
        this.matchAll = matchAll;
    }

    private static String makeErrorMsg(String msg, String token, String strExpr, Element widgetElement) {
        String str = "Widget element contains=\"...\" expression error: " + msg + ":";
        if (token != null) {
            str += " [name: \"" + token + "\"]";
        }
        if (strExpr != null) {
            str += " [expression: \"" + strExpr + "\"]";
        }
        if (widgetElement != null) {
            str += " [" + WidgetDocumentInfo.getElementDescriptor(widgetElement) + "]";
        }
        return str;
    }
    
    /**
     * Gets instance from CACHE.
     */
    public static ContainsExpr getInstance(String strExpr, Element widgetElement) {
        if (strExpr == null) return null;
        if (!strExpr.isEmpty()) {
            ContainsExpr expr = cache.get(strExpr);
            if (expr == null) { // no sync needed
                expr = new ContainsExpr(strExpr, widgetElement);
                cache.put(strExpr, expr);
            }
            return expr;
        }
        else return null;
    }
    
    public static ContainsExpr getInstanceOrDefault(String strExpr, Element widgetElement, ContainsExpr defaultValue) {
        ContainsExpr expr = getInstance(strExpr, widgetElement);
        return expr != null ? expr : defaultValue;
    }
    
    public static ContainsExpr getInstanceOrDefault(String strExpr, Element widgetElement) {
        return getInstanceOrDefault(strExpr, widgetElement, DEFAULT);
    }
    
    public static ContainsExpr getInstanceOrDefault(String strExpr) {
        return getInstanceOrDefault(strExpr, null, DEFAULT);
    }
    
    
    /**
     * Always creates new instance.
     */
    public static ContainsExpr makeInstance(String strExpr, Element widgetElement) {
        if (strExpr == null) return null;
        if (!strExpr.isEmpty()) return new ContainsExpr(strExpr, widgetElement);
        else return null;
    }
    
    public static ContainsExpr makeInstanceOrDefault(String strExpr, Element widgetElement, ContainsExpr defaultValue) {
        ContainsExpr expr = makeInstance(strExpr, widgetElement);
        return expr != null ? expr : defaultValue;
    }
    
    public static ContainsExpr makeInstanceOrDefault(String strExpr, Element widgetElement) {
        return makeInstanceOrDefault(strExpr, widgetElement, DEFAULT);
    }
    
    /**
     * Checks if name expression matches this contains expression.
     * Name MUST be prefixed by one of the characters defined in
     * {@link WidgetRenderTargetExpr#MET_ALL}.
     * NOTE: this specific method ONLY supports an exact name (no wildcards) 
     * and no bracketed attributes and will never support more.
     */
    public boolean matches(String nameExpr) {
        if (exactIncludes.contains(nameExpr)) return true;
        else if (exactExcludes.contains(nameExpr)) return false;
        else return matchesWild(nameExpr);
    }
    
    /**
     * Returns true if and only if ALL of the names match.
     * So one false prevents match.
     * If no names, also returns true.
     * NOTE: this specific method ONLY supports an exact name (no wildcards) 
     * and no bracketed attributes and will never support more.
     */
    public boolean matchesAllNames(List<String> nameExprList) {
        for(String nameExpr : nameExprList) {
            if (!matches(nameExpr)) return false;
        }
        return true;
    }
    
    /**
     * Returns true if and only if ALL of the tokens match.
     * So one false prevents match.
     * If no names, also returns true.
     * <p>
     * FIXME: currently this is not able to handle wildcard tokens or bracketed attributes.
     * It will only recognize exact names and may ignore entries altogether (treated as matched). 
     * The comparison logic needed to resolve this is extremely complex.
     * <p>
     * TODO: currently this is unable to consolidate the ^ and % operators, or any other operators
     * for that matter. STILL NEED TO IMPLEMENT WidgetRenderTargetExpr NORMALIZATION AND HANDLING
     * FROM ContainsExpr SO THAT EXCLUDE OPTIMIZATIONS ARE FULLY HONORED.
     * Currently, only some simple exclusions based on $ and # operators work at all.
     */
    public boolean matchesAllNameTokens(List<Token> nameTokenList) {
        // FIXME: this does not properly compare supported Tokens
        // for now we'll just get the original expression as a string and compare that directly,
        // BUT we cannot do this for the bracketed attributes
        for(Token nameExpr : nameTokenList) {
            if (nameExpr.hasAttr()) {
                // FIXME: no wildcards for bracketed attributes because current code will mess it up
                if (!matchesExact(nameExpr.getCmpNormStringExpr())) return false;
            } else {
                if (!matches(nameExpr.getCmpNormStringExpr())) return false;
            }
        }
        return true;
    }
    
    private boolean matchesExact(String nameExpr) {
        if (exactIncludes.contains(nameExpr)) return true;
        else if (exactExcludes.contains(nameExpr)) return false;
        else return matchAll;
    }
    
    private boolean matchesWild(String nameExpr) {
        if (wildIncludes != null) {
            for(String match : wildIncludes) {
                if (checkWildNameMatch(match, nameExpr)) {
                    return true;
                }
            }
        }
        if (wildExcludes != null) {
            for(String match : wildExcludes) {
                if (checkWildNameMatch(match, nameExpr)) {
                    return false;
                }
            }
        }
        if (matchAllTypes != null) {
            char nameType = nameExpr.charAt(0);
            Boolean matchExpl = matchAllTypes.get(nameType);
            if (matchExpl != null) return matchExpl;
        }
        return matchAll;
    }
    
    private boolean checkWildNameMatch(String match, String name) {
        if (match.charAt(0) == name.charAt(0)) { // same type
            String pureName = name.substring(1);
            if (match.charAt(match.length() - 1) == WidgetRenderTargetExpr.WILDCARD) {
                String part = match.substring(1, match.length() - 1);
                return pureName.startsWith(part);
            } else if (match.charAt(1) == WidgetRenderTargetExpr.WILDCARD) {
                String part = match.substring(2);
                return pureName.endsWith(part);
            } else {
                int wildIndex = match.lastIndexOf(WidgetRenderTargetExpr.WILDCARD);
                if (wildIndex < 1) {
                    throw new IllegalStateException("Section contains-expression name has missing or unexpected wildcard: " + match);
                }
                String firstPart = match.substring(2, wildIndex);
                String lastPart = match.substring(wildIndex + 1, match.length());
                return pureName.startsWith(firstPart) && pureName.endsWith(lastPart);
            }
        }
        return false;
    }
    
    public String getStrExpr() {
        return strExpr;
    }
    
    @Override
    public String toString() {
        return strExpr;
    }
    
    @Override
    public boolean equals(Object other) {
        return (other instanceof ContainsExpr) && this.strExpr.equals(((ContainsExpr) other).strExpr);
    }
    
    public static final class MatchAllContainsExpr extends ContainsExpr {
        private MatchAllContainsExpr() throws IllegalArgumentException { super("*"); }
        @Override
        public boolean matches(String nameExpr) { return true; }
        @Override
        public boolean matchesAllNames(List<String> nameExprList) { return true; }
        @Override
        public boolean matchesAllNameTokens(List<Token> nameTokenList) { return true; }
    }
    
    public static final class MatchNoneContainsExpr extends ContainsExpr {
        private MatchNoneContainsExpr() throws IllegalArgumentException { super("!*"); }
        @Override
        public boolean matches(String nameExpr) { return false; }
        @Override
        public boolean matchesAllNames(List<String> nameExprList) { return false; }
        @Override
        public boolean matchesAllNameTokens(List<Token> nameTokenList) { return false; }
    }
    
    /**
     * SCIPIO: For any ModelWidget that supports a contains-expression.
     */
    public interface FlexibleContainsExprAttrWidget {
        ContainsExpr getContainsExpr(Map<String, Object> context);
    }
    
    /**
     * SCIPIO: Special holder that allows to manage Flexible expressions automatically
     * and prevent creating unnecessary ones.
     */
    public static abstract class ContainsExprHolder implements FlexibleContainsExprAttrWidget, Serializable {
        
        public static ContainsExprHolder getInstanceOrDefault(String strExpr, Element widgetElement) {
            FlexibleStringExpander exdr = FlexibleStringExpander.getInstance(strExpr);
            if (FlexibleStringExpander.containsExpression(exdr)) {
                return new FlexibleContainsExprHolder(exdr);
            } else {
                ContainsExpr expr = ContainsExpr.getInstanceOrDefault(strExpr, widgetElement);
                if (ContainsExpr.DEFAULT.equals(expr)) {
                    return getDefaultInstance();
                } else {
                    return new SimpleContainsExprHolder(expr);
                }
            }
        }

        public static DefaultContainsExprHolder getDefaultInstance() { return DefaultContainsExprHolder.INSTANCE; }

        public static class DefaultContainsExprHolder extends ContainsExprHolder {
            private static final DefaultContainsExprHolder INSTANCE = new DefaultContainsExprHolder();
            private DefaultContainsExprHolder() {}
            @Override
            public ContainsExpr getContainsExpr(Map<String, Object> context) {
                return ContainsExpr.DEFAULT;
            }
        }
        
        public static class SimpleContainsExprHolder extends ContainsExprHolder {
            private final ContainsExpr containsExpr;

            public SimpleContainsExprHolder(ContainsExpr containsExpr) {
                super();
                this.containsExpr = containsExpr;
            }

            @Override
            public ContainsExpr getContainsExpr(Map<String, Object> context) {
                return containsExpr;
            }
        }
        
        public static class FlexibleContainsExprHolder extends ContainsExprHolder {
            private final FlexibleStringExpander containsExprExdr;

            public FlexibleContainsExprHolder(FlexibleStringExpander containsExprExdr) {
                super();
                this.containsExprExdr = containsExprExdr;
            }

            @Override
            public ContainsExpr getContainsExpr(Map<String, Object> context) {
                return ContainsExpr.getInstanceOrDefault(containsExprExdr.expandString(context));
            }
        }
    }
    
}