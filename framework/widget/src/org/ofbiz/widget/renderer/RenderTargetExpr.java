package org.ofbiz.widget.renderer;

import java.io.Serializable;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletRequest;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.webapp.renderer.RenderWriter;
import org.ofbiz.webapp.renderer.RenderWriter.SwitchRenderWriter;
import org.ofbiz.widget.model.ModelScreenWidget;
import org.ofbiz.widget.model.ModelWidget;
import org.ofbiz.widget.model.ModelWidgetVisitor;
import org.ofbiz.widget.model.WidgetDocumentInfo;
import org.w3c.dom.Element;

/**
 * SCIPIO: Represents an expression describing screen widget element to target for rendering,
 * compiled from an expression string. Used to implement targeted rendering.
 * <p>
 * The query expression is usually passed as a string as the "scpRenderTargetExpr" request parameter. 
 * <p>
 * TODO: NOT WORKING, NOT IMPLEMENTED, WORK-IN-PROGRESS - DOCUMENTATION IS PLANNING UNTIL FURTHER NOTICE
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
 * "%container[id=my-container]": (generic widget tag/element matching support, with limited attribute support (name, id))
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
public class RenderTargetExpr implements Serializable {

    public static final String module = RenderTargetExpr.class.getName();
    
    // WARN: this is public-facing so it must have size limits in cache.properties
    private static final UtilCache<String, RenderTargetExpr> cache = UtilCache.createUtilCache("renderer.targeted.expr");
    
    private static final boolean DEBUG = false || Debug.verboseOn();
    
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
    public static final char MET_WIDGETELEM = '%';
    public static final char MET_FTLELEM = '@'; // TODO: NOT IMPLEMENTED
    /**
     * Matching Element Type character prefixes. Each of these matches a widget element tag OR a type of attribute.
     * NOTE: These are shared between {@link RenderTargetExpr} and {@link ContainsExpr} so the
     * language is recognizable and consistent.
     */
    public static final Set<Character> MET_ALL = Collections.unmodifiableSet(new HashSet<Character>(Arrays.asList(new Character[] {
            MET_SECNAME, MET_ELEMID, MET_DECSECINCL, MET_WIDGETELEM, MET_FTLELEM
    })));
    public static final String MET_ALL_STR = StringUtils.join(MET_ALL, ", "); // for log and exceptions

    public static final char WILDCARD = '*';
    public static final String WILDCARD_STRING = String.valueOf(WILDCARD);
    
    public static final char ATTR_OPEN = '[';
    public static final char ATTR_CLOSE = ']';
    public static final char ATTR_EQUALS = '=';
    public static final char ATTR_SEP = ',';
    
    private final String strExpr;
    private final List<Token> tokens;
    //private final boolean useChildContent; // TODO?: FUTURE: this won't work
    
    /******************************************************/
    /* Constructors */
    /******************************************************/
    
    protected RenderTargetExpr(String strExpr) {
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
            tokens.add(Token.interpret(tokenStr));
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
    public static RenderTargetExpr fromString(String strExpr) throws IllegalArgumentException {
        if (strExpr == null || strExpr.isEmpty()) return null;
        RenderTargetExpr expr = cache.get(strExpr);
        if (expr == null) {
            expr = new RenderTargetExpr(strExpr);
            cache.put(strExpr, expr); // no sync needed
        }
        return expr;
    }
    
    /**
     * New expression from string, null if empty, or IllegalArgumentException if invalid. No cache.
     */
    public static RenderTargetExpr fromStringNew(String strExpr) throws IllegalArgumentException {
        if (strExpr == null || strExpr.isEmpty()) return null;
        return new RenderTargetExpr(strExpr);
    }

    /**
     * Gets expression from object, either already RenderTargetExpr or string, null if empty,
     * or IllegalArgumentException if unrecognized class or invalid. Uses global cache.
     */
    public static RenderTargetExpr fromObject(Object expr) {
        if (expr == null) return null;
        if (expr instanceof RenderTargetExpr) return (RenderTargetExpr) expr;
        else if (expr instanceof String) return fromString((String) expr);
        else throw new IllegalArgumentException("Cannot create RenderTargetExpr from type: " + expr.getClass());
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
        protected final char type;
        protected final Name name; // this is the name following the type prefix, it's not necessarily "name" attribute
        
        // NO NEED for this, because we'll only ever support name and id, so use those instead for optimize
        //protected final Map<String, String> attrMap;
        protected final Name nameAttr;
        protected final Name idAttr;
        
        // private, may split into subclasses later
        protected Token(String strExpr, char type, String name, Map<String, String> attrMap) {
            this.strExpr = strExpr;
            this.type = type;
            this.name = Name.interpret(name);
            
            // OPTIMIZATION: no need to keep map for these, we only support two attributes
            this.nameAttr = Name.interpretIfNotNull(attrMap.get("name"));
            this.idAttr = Name.interpretIfNotNull(attrMap.get("id"));
        }
        
        public static Token interpret(String strExpr) throws IllegalArgumentException {
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
            case MET_WIDGETELEM:
                token = new GenericWidgetElementMatcher(strExpr, type, name, attrMap);
                break;
            case MET_FTLELEM:
                token = new GenericFtlElementMatcher(strExpr, type, name, attrMap);
                break;
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
        
        public String getStringExpr() {
            return strExpr;
        }

        public char getType() {
            return type;
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
         * Core comparison implementation for {@link RenderTargetState#handleShouldExecute}.
         * flawed design but works better for now.
         */
        protected abstract void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo);
        
        
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
        
        public static class WidgetSectionMatcher extends Token {
            public WidgetSectionMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof ModelScreenWidget.Section && 
                    matchesWidget(widget, context, null, idAttr, name)) {
                    state.registerMatch(widget);
                    execInfo.matchRegistered = true;
                }
            }
        }
        
        public static class IdMatcher extends Token {
            public IdMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
                if (matchesWidget(widget, context, null, name, nameAttr)) {
                    state.registerMatch(widget);
                    execInfo.matchRegistered = true;
                }
            }
        }
        
        public static class DecoratorSectionIncludeMatcher extends Token {
            public DecoratorSectionIncludeMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof ModelScreenWidget.DecoratorSectionInclude) {
                    if (name.matches(widget.getName())) {
                        state.registerMatch(widget);
                        execInfo.matchRegistered = true;
                    } else {
                        // SPECIAL: unlike most other widget, if the decorator-section name did not match, 
                        // we can safely exclude this element from rendering,
                        // because the selector only works on current include/decorator level.
                        execInfo.shouldExecute = false;
                    }
                }
            }
        }
        
        public static class GenericWidgetElementMatcher extends Token {
            public GenericWidgetElementMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
                if (!(widget instanceof NonWidgetWrapperWidget) && matchesWidget(widget, context, name, idAttr, nameAttr)) {
                    state.registerMatch(widget);
                    execInfo.matchRegistered = true;
                }
            }
        }
        
        public static class GenericFtlElementMatcher extends Token {
            public GenericFtlElementMatcher(String strExpr, char type, String name, Map<String, String> attrMap) {
                super(strExpr, type, name, attrMap);
            }

            @Override
            protected void handleShouldExecute(ModelWidget widget, Map<String, Object> context,
                    RenderTargetState state, RenderTargetState.ExecutionInfoImpl execInfo) {
                if (widget instanceof FtlWrapperWidget && matchesWidget(widget, context, name, idAttr, nameAttr)) {
                    state.registerMatch(widget);
                    execInfo.matchRegistered = true;
                }
            }
        }
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
     * @return the RenderTargetState instance, or the {@link RenderTargetState#DISABLED} instance if none found
     */
    public static RenderTargetState getRenderTargetState(Map<String, Object> context) {
        RenderTargetState state = (RenderTargetState) context.get("scpRenderTargetState");
        if (state != null) return state;
        else {
            ServletRequest request = (ServletRequest) context.get("request");
            if (request != null) {
                state = (RenderTargetState) request.getAttribute("scpRenderTargetState");
                if (state != null) return state;
            }
            return RenderTargetState.DISABLED;
        }
    }
    
    public static RenderTargetState getRenderTargetState(ServletRequest request) {
        RenderTargetState state = (RenderTargetState) request.getAttribute("scpRenderTargetState");
        if (state != null) return state;
        return RenderTargetState.DISABLED;
    }
    
    /**
     * Gets state from context (actually global context) or request attributes if issues.
     * <p>
     * NOTE: this is best-effort, tries to avoid too many lookups too.
     */
    public static RenderTargetState getRenderTargetStateOrNull(Map<String, Object> context) {
        RenderTargetState state = (RenderTargetState) context.get("scpRenderTargetState");
        if (state != null) return state;
        else {
            ServletRequest request = (ServletRequest) context.get("request");
            if (request != null) return (RenderTargetState) request.getAttribute("scpRenderTargetState");
            return null;
        }
    }
    
    public static RenderTargetState getRenderTargetStateOrNull(ServletRequest request) {
        return (RenderTargetState) request.getAttribute("scpRenderTargetState");
    }
    
    /**
     * Sets the following global context vars AND request attributes:
     * scpRenderTargetState - RenderTargetState instance
     * scpRenderTargetOn - convenience boolean (NOTE: should NOT be used internally - use <code>scpRenderTargetState.isEnabled()</code>)
     */
    public static void setRenderTargetVars(Map<String, Object> context, RenderTargetExpr expr) {
        if (context == null) {
            if (expr != null) {
                Debug.logWarning("Targeted rendering: unable to set render target state - context is null", module);
            }
        }
        RenderTargetState state = RenderTargetState.fromExprOrDisabled(expr);
        Map<String, Object> globalContext = UtilGenerics.checkMap(context.get("globalContext"));
        if (globalContext != null) {
            globalContext.put("scpRenderTargetState", state);
            globalContext.put("scpRenderTargetOn", state.isEnabled());
        }
        ServletRequest request = (ServletRequest) context.get("request");
        if (request != null) {
            request.setAttribute("scpRenderTargetState", state);
            request.setAttribute("scpRenderTargetOn", state.isEnabled());
        }
    }
    
    /**
     * Returns true if target matched and currently outputting or if targeting disabled.
     * <p>
     * Currently (2017-05-09), this checks if writer is a RenderWriter; if so, returns true if
     * is not discarding output. If not a RenderWriter, try to lookup the RenderTargetState
     * in context and call {@link RenderTargetState#shouldOutput()}.
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
    public static class RenderTargetState implements Serializable {
        public static final RenderTargetState DISABLED = new DisabledRenderTargetState();
        
        public static final String module = RenderTargetState.class.getName();
        
        private final RenderTargetExpr expr;
        private List<ModelWidget> matchedWidgets;
        private boolean targetMatched = false;
        private boolean finished = false;
        //private ModelScreenWidget.DecoratorScreen skippedDecorator = null; // TODO? can't exploit (see below)
        
        private RenderTargetState(RenderTargetExpr expr) {
            this.expr = expr;
            this.matchedWidgets = new ArrayList<>(expr.getNumTokens());
        }
        
        private RenderTargetState() {
            this.expr = null;
        }
        
        public static RenderTargetState fromExprOrDisabled(RenderTargetExpr expr) {
            return expr != null ? new RenderTargetState(expr) : DISABLED;
        }

        // INFORMATION

        public boolean isEnabled() {
            return true;
        }
        
        public boolean isFinished() {
            return finished;
        }
        
        public boolean isTargetMatched() {
            return targetMatched;
        }

        public ModelWidget getTargetMatchedWidget() {
            return isTargetMatched() ? matchedWidgets.get(matchedWidgets.size() - 1) : null;
        }
        
        public ModelWidget getLastMatchedWidget() {
            int numMatched = matchedWidgets.size();
            return (numMatched > 0) ? matchedWidgets.get(numMatched - 1) : null;
        }
        
        public List<ModelWidget> getMatchedWidgets() {
            return Collections.unmodifiableList(matchedWidgets);
        }
        
        public int getNumMatched() {
            return matchedWidgets.size();
        }
        
        public int getMaxMatched() {
            return expr.tokens.size();
        }
        
        public boolean isNumMatchedMax() {
            return matchedWidgets.size() >= expr.tokens.size();
        }

        public RenderTargetExpr getExpr() {
            return expr;
        }

        public Token getNextToken() {
            return expr.getToken(getNumMatched());
        }
        
        public List<Token> getNextTokenAndChildren() {
            return expr.tokens.subList(getNumMatched(), expr.tokens.size());
        }
        
        /**
         * True if should output markup. This is only ever true if we matched the target.
         * NOTE: Code in ofbiz source files should call
         * {@link RenderTargetExpr#shouldOutput(Appendable, Map)} now instead in order to
         * better centralize the logic.
         */
        public boolean shouldOutput() {
            return !finished && isTargetMatched();
        }
        
        // STATE UPDATE
        
        /**
         * Marks finished, which prevents further rendering output at the least.
         * NOTE: Normally, {@link #handleFinished} should decide this.
         */
        public void markFinished() {
            this.finished = true;
        }

        private void registerMatch(ModelWidget widget) {
            if (isTargetMatched()) throw new IllegalStateException("Tried to modify RenderTargetState matches (register match)"
                    + " after already found target match (should be read-only)");
            this.matchedWidgets.add(widget);
            this.targetMatched = this.isNumMatchedMax();
        }
        
        private void deregisterLastMatch() {
            if (isTargetMatched()) throw new IllegalStateException("Tried to modify RenderTargetState matches (deregister match)"
                    + " after already found target match (should be read-only)");
            this.matchedWidgets.remove(this.matchedWidgets.size() - 1);
        }
        
        /**
         * Sets up the Writer for targeted rendering if enabled and needed.
         */
        public Writer prepareWriter(Appendable writer, Map<String, Object> context) {
            if (!(writer instanceof RenderWriter)) {
                Writer w = (Writer) writer; // FIXME: currently assuming everything is a Writer, ofbiz utils already assume this
                return SwitchRenderWriter.getInstance(w, false);
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
        public ExecutionInfo handleShouldExecute(ModelWidget widget, Appendable writer, Map<String, Object> context, Object stringRenderer) {
            // NOTE: by default, shouldExecute = true, because everything may contain our target(s).
            ExecutionInfoImpl execInfo = new ExecutionInfoImpl(widget, true, false, writer);
            if (finished) {
                execInfo.shouldExecute = false;
            } else if (isTargetMatched()) {
                ;
            } else {
                // SPECIAL: initialize/intercept writer, BEST-EFFORT 
                // try to convert the writer, which may be either response.getWriter() or
                // a StringWriter or something else, to a SwitchRenderWriter with outputting off.
                // we do this check at EVERY element because there are numerous instances of Writer created
                // everywhere; this is BEST-EFFORT attempt to intercept and convert them.
                // WARN: TODO: REVIEW: not guaranteed to catch all... only time will tell...
                if (!(writer instanceof RenderWriter)) {
                    Writer w = (Writer) writer; // FIXME: currently assuming everything is a Writer, ofbiz utils already assume this
                    execInfo.writerForElem = SwitchRenderWriter.getInstance(w, false);
                }

                // Compare the widget against the current token in the expression, stored in the state
                // results are marked in execInfo
                Token token = this.getNextToken();
                token.handleShouldExecute(widget, context, this, execInfo);
                
                // Finalize
                if (execInfo.shouldExecute) {
                    if (isTargetMatched()) {
                        // MATCHED!
                        // Turn on the switch and hope for the best
                        SwitchRenderWriter switchWriter = (SwitchRenderWriter) execInfo.writerForElem;
                        switchWriter.useOrigWriter();
                    } else {
                        if (widget instanceof ModelScreenWidget) {
                            // Blacklist support, for execution optimization:
                            // SPECIAL: <[section/element] contains="[expr]"> expression means we can potentially
                            // skip executing sections/elements that we have been told do NOT contain the sections or 
                            // elements we're after, in a blacklist-like fashion.
                            // If ANY of the names are considered not contained (are blacklisted), 
                            // we don't need to enter the widget.
                            ContainsExpr containsExpr = ((ModelScreenWidget) widget).getContainsExpr();
                            if (!containsExpr.matchesAllNameTokens(getNextTokenAndChildren())) {
                                execInfo.shouldExecute = false;
                                if (DEBUG) {
                                    Debug.logInfo("Targeted rendering: contains-expression optimization filtered out execution of widget element"
                                            + widget.getLogWidgetLocationString(), module);
                                }
                            }
                        }
                    }
                }
            }
            return execInfo;
            
            // OLD COMMENTS
            
            // NOTE: the following double-commented hack for PlatformSpecific
            // is being handled a different way to address point number 1.
            // comment is left for reference and because half the issues still apply:
//        } else if (widget instanceof ModelScreenWidget.PlatformSpecific) {
//            /* ***************************************************************************************
//             * !!! FIXME !!!
//             * this logic is a major flaw in the targetting.
//             * we currently cannot control the Freemarker output at all, so we're
//             * forced to do all-or-nothing: we cannot enter any Freemarker files until we have
//             * already found the target and outputting is enabled.
//             * this means we cannot have things like intermediate FTL files implementing the
//             * decorator, even if the target is a widget included by the FTL.
//             * (if the target is a html or macro in the FTL file, it is even less likely to be possible).
//             * 
//             * the solution involves manipulating Environment.setOut to a dummy writer,
//             * but it's complicated by the "screens", "sections", and other objects that hold references
//             * to the writer, and various problems.
//             * (for trying to target macros by ID, i.e. future improvement, this appears almost impossible - 
//             * only possible is converting macros from the standard API to java transforms to manipulate writer, 
//             * but this is a nightmare)
//             * 
//             * to clarify, there are 2 scenarios:
//             * 1) trying to target a widget element that is included through an FTL file, 
//             *    in other words supporting FTL boundaries, in other words support FTL-implemented decorators
//             *    -> FIXME A.S.A.P.
//             * 2) trying to target an element (macro) defined in an FTL file itself
//             *    -> realistically, this could be practically impossible or too damaging to our code to implement.
//             */
//            if (!shouldOutput()) {
//                shouldExecute = false;
//            }
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
            void handleFinished(Map<String, Object> context);
        }
        
        public class ExecutionInfoImpl implements ExecutionInfo {
            ModelWidget widget;
            boolean shouldExecute;
            boolean matchRegistered;
            Appendable writerForElem;
            
            ExecutionInfoImpl(ModelWidget widget, boolean shouldExecute,
                    boolean matchRegistered, Appendable writerForElem) {
                this.widget = widget;
                this.shouldExecute = shouldExecute;
                this.matchRegistered = matchRegistered;
                this.writerForElem = writerForElem;
            }
            
            @Override
            public boolean shouldExecute() { return shouldExecute; }
            @Override
            public Appendable getWriterForElementRender() { return writerForElem; }
            @Override
            public void handleFinished(Map<String, Object> context) {
                if (matchRegistered) {
                    if (isTargetMatched()) {
                        // OUTPUTTING DONE
                        // after this, assume the rest is not needed.
                        // NOTE: there are still individual renderXxxEnd calls that happen
                        // after this, and they are not filtered out by the handleShouldExecute call,
                        // but rather they get stopped by SwitchRenderWriter or RenderTargetState.shouldOuput. 
                        finished = true;
                        ((SwitchRenderWriter) writerForElem).useAltWriter();
                    } else {
                        deregisterLastMatch();
                    }
                }
            }
        }

        public static final class DisabledRenderTargetState extends RenderTargetState {
            private DisabledRenderTargetState() { super(); }
            @Override
            public boolean isEnabled() { return false; }
            @Override
            public boolean isFinished() { return false; }
            @Override
            public boolean isTargetMatched() { return false; }
            @Override
            public ModelWidget getTargetMatchedWidget() { return null; }
            @Override
            public int getNumMatched() { return 0; }
            @Override
            public RenderTargetExpr getExpr() { return null; }
            @Override
            public Token getNextToken() { return null; }
            @Override
            public boolean shouldOutput() { return true; }
            @Override
            public void markFinished() { ; }
            @Override
            public Writer prepareWriter(Appendable writer, Map<String, Object> context) { return (Writer) writer; }
            @Override
            public ExecutionInfo handleShouldExecute(ModelWidget widget, Appendable writer, Map<String, Object> context, Object stringRenderer) { return new DisabledExecutionInfoImpl(writer); }
            
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

    /**
     * Widget section contains-expression - special expression that instructs renderer which sections 
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
     * ex:
     * <pre>{@code
     * "$MySection1, !$MySection2, !$MySections-*, #MyContainerId, !#myContainerId2, *"
     * }</pre>
     */
    public static class ContainsExpr implements Serializable {
        public static final String module = ContainsExpr.class.getName();

        // entries are comma-separated
        private static final Pattern containsExprTokenSplitPat = Pattern.compile("\\s*,\\s*");
 
        // general language
        public static final char EXCLUDE = '!';

        // pre-build expressions
        public static final ContainsExpr MATCH_ALL = new MatchAllContainsExpr();
        public static final ContainsExpr MATCH_NONE = new MatchNoneContainsExpr();
        public static final ContainsExpr DEFAULT = MATCH_ALL;
        
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
            
            // FIXME: this does not properly compare supported Tokens
            // nor does it recognized the bracketed attributes 
            
            for(String fullToken : tokenArr) {
                String token = fullToken;
                boolean exclude = (token.charAt(0) == EXCLUDE);
                if (exclude) token = token.substring(1);
    
                if (WILDCARD_STRING.equals(token)) { // special case
                    matchAll = !exclude;
                    continue;
                } else if (token.isEmpty()) {
                    throw new IllegalArgumentException(makeErrorMsg("invalid or empty name", fullToken, strExpr, widgetElement));
                }
                
                char type = token.charAt(0);
                if (!MET_ALL.contains(type)) 
                    throw new IllegalArgumentException(makeErrorMsg("name has missing"
                            + " or invalid type specifier (should start with one of: " + MET_ALL_STR + ")", fullToken, strExpr, widgetElement));
                String name = token.substring(1);
                
                if (name.equals(WILDCARD_STRING)) {
                    matchAllTypes.put(type, !exclude);
                } else if (name.contains(WILDCARD_STRING)) {
                    if (name.indexOf(WILDCARD) != name.lastIndexOf(WILDCARD)) { // TODO?: support multiple wildcard
                        throw new UnsupportedOperationException(makeErrorMsg("name has"
                                + " with multiple wildcards, which is not supported", fullToken, strExpr, widgetElement));
                    }
                    if (exclude) {
                        wildExcludes.add(token);
                    } else {
                        wildIncludes.add(token);
                    }
                } else {
                    if (exclude) {
                        exactExcludes.add(token);
                    } else {
                        exactIncludes.add(token);
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
        
        public static ContainsExpr make(String strExpr, Element widgetElement) {
            if (strExpr == null) return null;
            if (!strExpr.isEmpty()) return new ContainsExpr(strExpr, widgetElement);
            else return null;
        }
        
        public static ContainsExpr makeOrDefault(String strExpr, Element widgetElement, ContainsExpr defaultValue) {
            ContainsExpr expr = make(strExpr, widgetElement);
            return expr != null ? expr : defaultValue;
        }
        
        public static ContainsExpr makeOrDefault(String strExpr, Element widgetElement) {
            return makeOrDefault(strExpr, widgetElement, DEFAULT);
        }
        
        /**
         * Checks if name expression matches this contains expression.
         * Name MUST be prefixed by one of the characters defined in
         * {@link RenderTargetExpr#MET_ALL}.
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
         * FIXME: currently this is not able to handle wildcard tokens or bracketed attributes.
         * It will only recognize exact names and may ignore entries altogether (treated as matched). 
         * The comparison logic needed to resolve this is extremely complex.
         */
        public boolean matchesAllNameTokens(List<Token> nameTokenList) {
            // FIXME: this does not properly compare supported Tokens
            // for now we'll just get the original expression as a string and compare that directly,
            // BUT we cannot do this for the bracketed attributes
            for(Token nameExpr : nameTokenList) {
                if (nameExpr.hasAttr()) {
                    // FIXME: no wildcards for bracketed attributes because current code will mess it up
                    if (!matchesExact(nameExpr.getStringExpr())) return false;
                } else {
                    if (!matches(nameExpr.getStringExpr())) return false;
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
                if (match.charAt(match.length() - 1) == WILDCARD) {
                    String part = match.substring(1, match.length() - 1);
                    return pureName.startsWith(part);
                } else if (match.charAt(1) == WILDCARD) {
                    String part = match.substring(2);
                    return pureName.endsWith(part);
                } else {
                    int wildIndex = match.lastIndexOf(WILDCARD);
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
        
    }
    
    /******************************************************/
    /* Widget Helpers */
    /******************************************************/
    
    /**
     * Identifies a ModelWidget adapter that doesn't actually represent a widget language XML element.
     */
    public interface NonWidgetWrapperWidget {
    }
    
    /**
     * Identifies a ModelWidget adapter that doesn't actually represent a widget language XML element
     * but a Freemarker language element instead.
     */
    public interface FtlWrapperWidget extends NonWidgetWrapperWidget{
    }
    
    /**
     * TODO: Special wrapper for FTL elements to pass off as widgets.
     * Currently useless, no support for FTL matching and support uncertain.
     */
    public static class ModelFtlWidget extends ModelWidget implements FtlWrapperWidget, ModelWidget.IdAttrWidget {
        private final String dirType;
        private final String location;
        private final String id;
        
        public ModelFtlWidget(String name, String dirType, String location, String id) {
            super(name);
            this.dirType = dirType;
            this.location = location;
            this.id = id;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
        }

        @Override
        public String getContainerLocation() {
            return location;
        }

        @Override
        public String getWidgetType() {
            return "ftl-" + dirType;
        }
        
        @Override
        public String getTagName() {
            return dirType;
        }

        @Override
        public String getId() {
            return id;
        }
        
    }
}
