package org.ofbiz.widget.renderer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.ServletRequest;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.widget.model.ModelScreenWidget;
import org.ofbiz.widget.model.ModelWidget;

/**
 * SCIPIO: Represents an expression describing screen widget element to target for rendering,
 * compiled from an expression string. Used to implement targeted rendering.
 * <p>
 * TODO: NOT WORKING, NOT IMPLEMENTED, WORK-IN-PROGRESS - DOCUMENTATION IS PLANNING UNTIL FURTHER NOTICE
 * <p>
 * In most case (by default) it is physically impossible for the renderer to know the path from top screen element to 
 * a deep-nested element and which elements it doesn't need to render. So the client may need to specify
 * two or more anchor-like section names through these expressions, otherwise the renderer is forced to render 
 * everything to prevent anything breaking, and there is no optimization.
 * <p>
 * Expressions are composed of individual section names and container IDs, which are all treated like global identifiers. 
 * When space-separated they perform a "parent child" selection like the common CSS child selector.
 * <p>
 * NOTE: In order for optimization to work meaningfully, the decorator itself must implement contains-expressions
 * hints using {@code <section contains="!$my-section, ...">}, which is implemented through {@link ContainsExpr}.
 * <p>
 * Example expression strings:
 * <pre>
 * {@code
 * "$my-section": (section selector)
 *     <section name="my-section">
 *     
 * "#my-container": (id selector)
 *     <container id="my-container">
 * "#my-screenlet": (id selector)
 *     <screenlet id="my-screenlet">

 * "$my-section #my-container": (child selector, CSS-like)
 *     <section name="my-section">
 *         [...]
 *             <container id="my-container">
 * }
 * </pre>
 */
@SuppressWarnings("serial")
public class RenderTargetExpr implements Serializable {

    public static final String module = RenderTargetExpr.class.getName();
    
    // WARN: this is public-facing so it must have limits. TODO: unhardcode
    private static final UtilCache<String, RenderTargetExpr> cache = UtilCache.createUtilCache("renderer.targeted.expr", 1000, 0);
    
    // only supports child selector for now (spaces only)
    private static final Pattern targetExprTokenSplitPat = Pattern.compile("\\s+");
    
    private final String strExpr;
    private final List<String> tokens;
    private final boolean useChildContent;
    
    /******************************************************/
    /* Constructors */
    /******************************************************/
    
    protected RenderTargetExpr(String strExpr) {
        this.strExpr = strExpr;
        
        String[] tokenArr = targetExprTokenSplitPat.split(strExpr.trim());
        if (tokenArr.length < 1) {
            throw new IllegalArgumentException("Invalid render target expression: " + strExpr);
        }
        this.useChildContent = ">".equals(tokenArr[tokenArr.length - 1]);
        
        Arrays.asList(tokenArr);
        ArrayList<String> tokens;
        if (this.useChildContent) {
            tokens = new ArrayList<>(Arrays.asList(tokenArr).subList(0, tokenArr.length - 1));
        } else {
            tokens = new ArrayList<>(Arrays.asList(tokenArr));
        }
        if (tokens.size() < 1) {
            throw new IllegalArgumentException("Invalid render target expression: " + strExpr);
        }
        tokens.trimToSize();
        this.tokens = tokens; // Collections.unmodifiableList(tokens);
    }
    
    public static RenderTargetExpr fromString(String strExpr) {
        RenderTargetExpr expr = cache.get(strExpr);
        if (expr == null) {
            expr = new RenderTargetExpr(strExpr);
            cache.put(strExpr, expr); // no sync needed
        }
        return expr;
    }
    
    public static RenderTargetExpr fromStringNew(String strExpr) { // force new instance
        return new RenderTargetExpr(strExpr);
    }

    public static RenderTargetExpr fromObject(Object expr) {
        if (expr == null) return null;
        if (expr instanceof RenderTargetExpr) return (RenderTargetExpr) expr;
        else if (expr instanceof String && !((String) expr).isEmpty()) return fromString((String) expr);
        else throw new IllegalArgumentException("Cannot create RenderTargetExpr from type: " + expr.getClass());
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
    
    public List<String> getTokens() {
        return Collections.unmodifiableList(tokens);
    }
    
    public int getNumTokens() {
        return tokens.size();
    }
    
    public String getToken(int index) {
        if (index < tokens.size()) {
            return tokens.get(index);
        } else {
            return null;
        }
    }
    
    public boolean isUseChildContent() {
        return useChildContent;
    }

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
     * Call from ModelScreenWidget visiting code.
     */
    public static boolean updateShouldExecute(ModelWidget widget, Map<String, Object> context) {
        RenderTargetState state = getRenderTargetState(context);
        return (state == null || state.updateShouldExecute(widget, context));
    }
    
    /**
     * Call from ScreenStringRenderer implementations.
     */
    public static boolean shouldOutput(Map<String, Object> context) {
        RenderTargetState state = getRenderTargetState(context);
        return (state == null || state.shouldOutput());
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
        private ModelWidget matchedWidget = null;
        private boolean finished = false;
        private int matchDepth = 0;
        private ModelScreenWidget.DecoratorScreen skippedDecorator = null;
        
        private RenderTargetState(RenderTargetExpr expr) {
            this.expr = expr;
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
        
        public boolean isMatched() {
            return matchedWidget != null;
        }

        public ModelWidget getMatchedWidget() {
            return matchedWidget;
        }
        
        public int getMatchDepth() {
            return matchDepth;
        }

        public RenderTargetExpr getExpr() {
            return expr;
        }

        public String getNextToken() {
            return expr.getToken(matchDepth);
        }
        
        /**
         * True if should output markup. This is only ever true if we matched the target.
         */
        public boolean shouldOutput() {
            return !finished && matchedWidget != null;
        }
        
        // STATE UPDATE
        
        /**
         * Must be called after return from executing the target widget.
         */
        public void checkMarkFinished(ModelWidget widget) {
            if (widget == matchedWidget) {
                this.finished = true;
            }
        }
        
        /**
         * Marks finished, which prevents further rendering output at the least.
         */
        public void markFinished() {
            this.finished = true;
        }

        /**
         * Checks if should go into the widget, and updates state.
         * <p>
         * We ALWAYS have to go into the widget UNLESS it's explicitly skipped and unless
         * it comes after us.
         */
        public boolean updateShouldExecute(ModelWidget widget, Map<String, Object> context) {
            if (finished) {
                return false;
            } else if (matchedWidget != null) {
                return true;
            } else {
                boolean shouldExecute = true;
                String token = getNextToken();
                char type = token.charAt(0);
                String name = token.substring(1);
                
                //////////////////////////////////////////////
                // TODO: NOT IMPLEMENTED
                //////////////////////////////////////////////
                
                
                if (widget instanceof ModelScreenWidget.Section) {
                    // TODO
                    if (type == '$' && name.equals(widget.getName())) {
                        matchDepth++;
                    } else {

                    }
                } else if (widget instanceof ModelScreenWidget.Container) {
                    // TODO
                    if (type == '#' && name.equals(((ModelScreenWidget.Container) widget).getId(context))) {
                        matchDepth++;
                    } else {

                    }
                } else if (widget instanceof ModelScreenWidget.Screenlet) {
                    // TODO
                    if (type == '#' && name.equals(((ModelScreenWidget.Screenlet) widget).getId(context))) {
                        matchDepth++;
                    } else {

                    }
                } else if (widget instanceof ModelScreenWidget.IncludeScreen) {
                    // TODO
                    ModelScreenWidget.IncludeScreen include = (ModelScreenWidget.IncludeScreen) widget;
                    if (type == '^' && ("INCLUDE".equals(name) || name.equals(include.getName(context)))) { // FIXME: does not support location compare
                        matchDepth++;
                    } else {

                    }
                } else if (widget instanceof ModelScreenWidget.DecoratorScreen) {
                    // TODO
                    ModelScreenWidget.DecoratorScreen decorator = (ModelScreenWidget.DecoratorScreen) widget;
                    if (type == '^' && ("INCLUDE".equals(name) || name.equals(decorator.getName(context)))) { // FIXME: does not support location compare
                        matchDepth++;
                        // SPECIAL: identify if this is decorator implementation to skip through
                        String nextToken = getNextToken();
                        if (nextToken != null && nextToken.charAt(0) == '!') {
                            this.skippedDecorator = decorator;
                        }
                    } else if (type == '!') { // SPECIAL: identify if this is decorator implementation to skip through
                        this.skippedDecorator = decorator;
                    } else {
                        
                    }
                } else if (widget instanceof ModelScreenWidget.DecoratorSectionInclude) {
                    // TODO
                    if (type == '@' && name.equals(widget.getName())) {
                        matchDepth++;
                    } else {

                    }
                } else if (widget instanceof ModelScreenWidget.DecoratorSection) {
                    // SPECIAL
                    // TODO
                    if (type == '!' && name.equals(widget.getName())) {
                        matchDepth++;
                    } else {
                        
                    }
                } else {
                    return true;
                }
                
                if (matchDepth >= expr.getNumTokens()) {
                    // FOUND
                    this.matchedWidget = widget;
                }
                
                return shouldExecute;
            }
        }
        
        public static final class DisabledRenderTargetState extends RenderTargetState {
            private DisabledRenderTargetState() { super(); }
            @Override
            public boolean isEnabled() { return false; }
            @Override
            public boolean isFinished() { return false; }
            @Override
            public boolean isMatched() { return false; }
            @Override
            public ModelWidget getMatchedWidget() { return null; }
            @Override
            public int getMatchDepth() { return 0; }
            @Override
            public RenderTargetExpr getExpr() { return null; }
            @Override
            public String getNextToken() { return null; }
            @Override
            public boolean shouldOutput() { return true; }
            @Override
            public void checkMarkFinished(ModelWidget widget) { ; }
            @Override
            public void markFinished() { ; }
            @Override
            public boolean updateShouldExecute(ModelWidget widget, Map<String, Object> context) { return true; }
        }
    }

    /**
     * Widget section contains-expression - special expression that instructs renderer which sections 
     * contain or don't contain which other sections and elements.
     * <p>
     * In targeted rendering, this is used by widget renderer to determine which sections can be skipped entirely (their
     * full execution including actions, not just output).
     * <p>
     * Functions in blacklist fashion, so that by default all sections are said to possibly contain
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
        public static final char WILDCARD = '*';
        public static final String WILDCARD_STRING = String.valueOf(WILDCARD);
        public static final char EXCLUDE_PREFIX = '!';
        
        // types
        public static final char SECNAME_TYPE_PREFIX = '$';
        public static final char ELEMID_TYPE_PREFIX = '#';
        public static final Set<Character> TYPE_PREFIXES = Collections.unmodifiableSet(new HashSet<Character>(Arrays.asList(new Character[] {
                SECNAME_TYPE_PREFIX, ELEMID_TYPE_PREFIX
        })));
        public static final String TYPE_PREFIXES_STR = StringUtils.join(TYPE_PREFIXES, ", ");
    
        // pre-build expressions
        public static final ContainsExpr MATCH_ALL = new MatchAllContainsExpr();
        public static final ContainsExpr MATCH_NONE = new MatchNoneContainsExpr();
        public static final ContainsExpr MATCH_ALL_SECTIONS_ONLY = new ContainsExpr("$*");
        public static final ContainsExpr MATCH_ALL_ID_ONLY = new ContainsExpr("#*");
        public static final ContainsExpr DEFAULT = MATCH_ALL;
        
        private final String strExpr;
        private final Set<String> exactIncludes;
        private final Set<String> exactExcludes;
        private final boolean matchAll;
        private final Map<Character, Boolean> matchAllTypes;
        private final List<String> wildIncludes; // TODO: find way to optimize
        private final List<String> wildExcludes; // TODO: find way to optimize
        
        public ContainsExpr(String strExpr) throws IllegalArgumentException {
            this.strExpr = strExpr;
            String[] tokenArr = containsExprTokenSplitPat.split(strExpr.trim());
            if (tokenArr.length <= 0) throw new IllegalArgumentException("Section contains-expression cannot be empty");
            
            // NOTE: these are layered by specificity from most exact to most generic
            Set<String> exactIncludes = new HashSet<>();
            Set<String> exactExcludes = new HashSet<>();
            ArrayList<String> wildIncludes = new ArrayList<>();
            ArrayList<String> wildExcludes = new ArrayList<>();
            Map<Character, Boolean> matchAllTypes = new HashMap<>();
            boolean matchAll = false;
            
            for(String fullToken : tokenArr) {
                String token = fullToken;
                boolean exclude = (token.charAt(0) == EXCLUDE_PREFIX);
                if (exclude) token = token.substring(1);
    
                if (WILDCARD_STRING.equals(token)) { // special case
                    matchAll = !exclude;
                    continue;
                } else if (token.isEmpty()) {
                    throw new IllegalArgumentException("Section contains-expression has invalid name: " + fullToken);
                }
                
                char type = token.charAt(0);
                if (!TYPE_PREFIXES.contains(type)) 
                    throw new IllegalArgumentException("Section contains-expression has a name with missing"
                            + " or invalid type specifier (should start with " + TYPE_PREFIXES_STR + "): " + fullToken);
                String name = token.substring(1);
                
                if (name.equals(WILDCARD_STRING)) {
                    // TODO: find way to optimize
                    matchAllTypes.put(type, !exclude);
                } else if (name.contains(WILDCARD_STRING)) {
                    // TODO: support multiple wild
                    if (name.indexOf(WILDCARD) != name.lastIndexOf(WILDCARD)) {
                        throw new UnsupportedOperationException("Section contains-expression has a name with multiple wildcards, not supported: " + fullToken);
                    }
                    // TODO: find way to optimize
                    if (exclude) {
                        wildExcludes.add(token);
                    } else {
                        wildIncludes.add(token);
                    }
                } else {
                    // this already optimized enoughs
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
            this.matchAll = matchAll;
        }
    
        public static ContainsExpr make(String strExpr) {
            if (strExpr == null) return null;
            if (!strExpr.isEmpty()) return new ContainsExpr(strExpr);
            else return null;
        }
        
        public static ContainsExpr makeOrDefault(String strExpr, ContainsExpr defaultValue) {
            ContainsExpr expr = make(strExpr);
            return expr != null ? expr : defaultValue;
        }
        
        public static ContainsExpr makeOrDefault(String strExpr) {
            return makeOrDefault(strExpr, DEFAULT);
        }
        
        /**
         * Checks if name expression matches this contains expression.
         * Name must be prefixed by "$" for section names or "#" for element IDs.
         */
        public boolean matches(String nameExpr) {
            if (exactIncludes.contains(nameExpr)) return true;
            else if (exactExcludes.contains(nameExpr)) return false;
            else return matchesWild(nameExpr);
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
        
        public boolean matchesSecName(String sectionName) {
            return matches("$" + sectionName);
        }
        
        public boolean matchesElemId(String elemId) {
            return matches("#" + elemId);
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
            public boolean matchesSecName(String sectionName) { return true; }
            @Override
            public boolean matchesElemId(String elemId) { return true; }
        }
        
        public static final class MatchNoneContainsExpr extends ContainsExpr {
            private MatchNoneContainsExpr() throws IllegalArgumentException { super("!*"); }
            @Override
            public boolean matches(String nameExpr) { return false; }
            @Override
            public boolean matchesSecName(String sectionName) { return false; }
            @Override
            public boolean matchesElemId(String elemId) { return false; }
        }
        
    }
}
