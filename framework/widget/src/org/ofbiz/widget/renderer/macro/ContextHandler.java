package org.ofbiz.widget.renderer.macro;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.MapStack;

/**
 * SCIPIO: Saves the initial screen context to use in populating a context upon creation
 * of Environments used to render macros, to provide context var support to Ofbiz widget macros.
 * <p>
 * 2016-01-06: Previously this class only saved and passed a few major global values from the
 * initial context (request, locale, globalContext, etc.). Now, we exploit the fact that the
 * initial context is a MapStack which is only a reference to other maps and simply pass this stack 
 * to the Environment which means it will automatically know of the last changes to it.
 * <p>
 * This is a delicate hack based on MapStack's implementation, but it works for now.
 * It has 2 major problems:
 * 1) we can't reliably add new keys to the map - but this is okay since stock code barely adds anything.
 * 2) the macros will not actually receive the REAL current context received by the java methods - that context
 *    may be something other than the global MapStack context. we simply ignore this for now
 *    because it is rare and probably not a problem.
 *    WARN: it could still be a problem in complex rendering cases.
 * <p>   
 * TODO?: The real solution to all of this is to modify all MacroXxxRenderer methods to pass context
 * to macro calls. However it's major change and may not even work with the current Freemarker calls used.
 */
class ContextHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private final String rendererLabel;
    private MacroScreenRenderer screenRenderer = null;
    private Map<String, Object> lastRenderContext = new HashMap<String, Object>();

    protected ContextHandler(String rendererLabel) {
        super();
        this.rendererLabel = rendererLabel;
    }

    private void registerScreenRenderer(Appendable writer, Map<String, Object> context) throws IOException {
        if (screenRenderer == null && context.get("screens") != null) {
            org.ofbiz.widget.renderer.ScreenRenderer screens = (org.ofbiz.widget.renderer.ScreenRenderer) context.get("screens");
            screenRenderer = (MacroScreenRenderer) screens.getScreenStringRenderer();
        }
    }
    
    /**
     * Registers the context as an initial context for the render thread.
     * Call from methods where the original context is expected to be passed at some point.
     */
    public void registerInitialContext(Appendable writer, Map<String, Object> context) throws IOException {
        registerScreenRenderer(writer, context);
        if (screenRenderer != null) {
            if (screenRenderer.initialContext == null) {
                if (context instanceof MapStack) {
                    screenRenderer.initialContext = (MapStack<String>) context;
                    //screenRenderer.initialContextCopy = new HashMap<String, Object>(context);
                }
                else {
                    throw new IllegalStateException("Scipio: Expected initial screen context to be a MapStack, "
                            + "but was of type: " + context.getClass().getName());
                }
            }
        }
        else {
            Debug.logError("macro " + rendererLabel + " renderer template environment initial context register "
                    + "could not retrieve macro screen renderer instance", module);
        }
    }
    
    /**
     * Registers/saves elements from context as needed.
     * Call from any *StringRenderer method that may be important.
     */
    public void registerContext(Appendable writer, Map<String, Object> context) throws IOException {
        registerScreenRenderer(writer, context);
    }
    
    public Map<String, Object> getInitialContext(Appendable writer, Map<String, Object> context) throws IOException {
        return screenRenderer != null ? screenRenderer.initialContext : new HashMap<String, Object>();
    }
    
    public Map<String, Object> getInitialContext(Appendable writer) throws IOException {
        return screenRenderer != null ? screenRenderer.initialContext : new HashMap<String, Object>();
    }
    
    
    /**
     * Creates a new context as source for data model for a new Environment, populated with given
     * initial values, in addition to anything we deem needed.
     * <p>
     * FIXME?: This does not handle the real, "current" context. To minimize intrusive changes
     * we currently (still) try to emulate as best as possible using an initial context.
     * otherwise have to rewrite nearly all renderer methods (may have to...).
     */
    public Map<String, Object> createRenderContext(Appendable writer, Map<String, Object> currentContext, Map<String, Object> extraValues) throws IOException {
        Map<String, Object> renderContext;
        if (currentContext != null) {
            throw new UnsupportedOperationException("Not expecting to receive current context in current implementation");
        }
        else if (screenRenderer != null) {
            MapStack<String> initContext = screenRenderer.initialContext;
            if (initContext != null) {
                renderContext = createRenderContextFromInitial(initContext, extraValues);
            }
            else {
                Debug.logError("macro " + rendererLabel + " renderer template environment initial context absent", module);
                renderContext = new HashMap<String, Object>(extraValues);
            }
        }
        else {
            Debug.logError("macro " + rendererLabel + " renderer template environment initial context populate "
                    + "could not retrieve macro screen renderer instance", module);
            renderContext = new HashMap<String, Object>(extraValues);
        }
        lastRenderContext = renderContext;
        return renderContext;
    }
    
    /**
     * Returns the most appropriate render context for macro rendering.
     * <p>
     * The returned context may have no relation to the passed <code>currentContext</code>, or
     * it may be the same. It will be a context containing the major top-level variables such as
     * request, response, locale, etc.
     */
    public Map<String, Object> getRenderContext(Appendable writer, Map<String, Object> currentContext) throws IOException {
        return lastRenderContext;
    }
    
    /**
     * NOTE: The other overload should be preferred when possible: {@link #getRenderContext(Appendable, Map)
     * @see #getRenderContext(Appendable, Map)
     */
    public Map<String, Object> getRenderContext(Appendable writer) throws IOException {
        return lastRenderContext;
    }
    
    private Map<String, Object> createRenderContextFromInitial(MapStack<String> initContext, Map<String, Object> extraValues) throws IOException {

        // FIXME: here we'll cheat and just dump this into the context. it's a terrible idea but won't matter.
        initContext.putAll(extraValues);
        
        return initContext;
    }
    
}