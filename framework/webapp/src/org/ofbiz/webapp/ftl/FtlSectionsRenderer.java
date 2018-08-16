package org.ofbiz.webapp.ftl;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.renderer.BasicSectionsRenderer;
import org.ofbiz.webapp.renderer.FtlContextFetcher;
import org.ofbiz.webapp.renderer.RenderContextFetcher;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateTransformModel;

/**
 * SCIPIO: Freemarker template rendering emulation of the widget renderer
 * {@link org.ofbiz.widget.model.ModelScreenWidget.SectionsRenderer} class.
 * <p>
 * Each section maps to a TemplateInvoker, String, TemplateDirectiveModel or
 * TemplateScalarModel instance. It supports versatile typing and so can
 * store either wrapped or unwrapped models.
 * <p>
 * NOTE: TemplateDirectiveModel is invoked with no arguments, no loop vars and no body.
 * <p>
 * NOTE: String and TemplateScalarModel are NOT specially interpreted and are simply
 * outputted. In order for rendering to occur dynamically you have pass a
 * TemplateScalarModel whose getAsString implements rendering functionality.
 */
public class FtlSectionsRenderer implements BasicSectionsRenderer, Map<String, Object> {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private final RenderContextFetcher contextFetcher;
    private final Map<String, Object> sectionMap;
    
    public FtlSectionsRenderer(Map<String, Object> sectionMap, RenderContextFetcher contextFetcher) {
        this.sectionMap = sectionMap;
        this.contextFetcher = contextFetcher;
    }
    
    public static FtlSectionsRenderer create(Map<String, Object> sectionMap) {
        return new FtlSectionsRenderer(sectionMap, new FtlContextFetcher.FallbackFtlFullFetcher(null, null));
    }
    
    public static FtlSectionsRenderer create(TemplateHashModelEx sectionMapModel) throws TemplateModelException {
        return new FtlSectionsRenderer(LangFtlUtil.makeModelObjectMap(sectionMapModel), new FtlContextFetcher.FallbackFtlFullFetcher(null, null));
    }
    
    /** 
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
     * <p>
     * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
     * */
    public String render(String sectionName, boolean asString) throws GeneralException, IOException, TemplateException {
        return render(sectionName, asString, getWriter());
    }
    
    /** 
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
     * <p>
     * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
     * */
    protected String render(String sectionName, boolean asString, Appendable writer) throws GeneralException, IOException, TemplateException {
        Object section = sectionMap.get(sectionName);
        Appendable effWriter = asString ? new java.io.StringWriter() : writer;
        // if no section by that name, write nothing
        if (effWriter != null && section != null) {
            render(effWriter, section);
        }
        return asString ? effWriter.toString() : "";
    }
    
    protected static void render(Appendable writer, Object section) throws TemplateException, IOException {
        if (section instanceof TemplateInvoker) {
            TemplateInvoker invoker = (TemplateInvoker) section;
            invoker.invoke((Writer) writer);
        } else if (section instanceof WrapperTemplateModel && ((WrapperTemplateModel) section).getWrappedObject() instanceof TemplateInvoker) {
            TemplateInvoker invoker = (TemplateInvoker) ((WrapperTemplateModel) section).getWrappedObject();
            invoker.invoke((Writer) writer);
        } else if (section instanceof TemplateDirectiveModel) {
            TemplateDirectiveModel directive = (TemplateDirectiveModel) section;
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            Writer prevOut = env.getOut();
            try {
                env.setOut((Writer) writer);
                directive.execute(env, new HashMap<>(), new TemplateModel[] {}, null);
            } finally {
                env.setOut(prevOut);
            }
        } else if (section instanceof TemplateTransformModel) {
            TemplateTransformModel directive = (TemplateTransformModel) section;
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            Writer prevOut = env.getOut();
            try {
                env.setOut((Writer) writer);
                // FIXME: we should use the return Writer somehow...
                // but for now this covers the ?interpret implementation from Freemarker 2.3.22 just enough...
                directive.getWriter((Writer) writer, new HashMap<>());
            } finally {
                env.setOut(prevOut);
            }
        } else if (section instanceof freemarker.core.Macro) {
            // WARN: Macro is deprecated and may be replaced in later versions, at which point this will fail to compile
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            final String tempMacroName = "_secRenCurMacro"; // DEV NOTE: this should be a fixed name; should not be variable
            env.setVariable(tempMacroName, (TemplateModel) section);
            Writer prevOut = env.getOut();
            try {
                env.setOut((Writer) writer);
                LangFtlUtil.execMacro(tempMacroName, env);
            } finally {
                env.setOut(prevOut);
            }
        } else if (section instanceof TemplateScalarModel) {
            TemplateScalarModel scalar = (TemplateScalarModel) section;
            String content = scalar.getAsString();
            writer.append(content);
        } else if (section instanceof String) {
            writer.append((String) section);
        } else if (section == null) {
            ;
        } else {
            throw new TemplateModelException("Unsupported ftl sections renderer section class: " + section.getClass().getName());
        }
    }
    
    /** 
     * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
     */
    public String render(String sectionName) throws GeneralException, IOException, TemplateException {
        return render(sectionName, false);
    }
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
     */
    public String renderScoped(String sectionName, Boolean asString, Boolean shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException, TemplateException {
        if (asString == null) {
            asString = Boolean.FALSE;
        }
        if (!Boolean.TRUE.equals(shareScope)) { // default is FALSE for this method (only!)
            MapStack<String> context = getContext(); // SCIPIO: simplified
            if (context != null) context.push();
            try {
                if (context != null && ctxVars != null && !ctxVars.isEmpty()) {
                    context.putAll(ctxVars);
                }
                return render(sectionName, asString, getWriter());
            } finally {
                if (context != null) context.pop();
            }
        } else {
            MapStack<String> context = getContext(); // SCIPIO: simplified
            if (context != null && ctxVars != null && !ctxVars.isEmpty()) {
                context.putAll(ctxVars);
            }
            return render(sectionName, asString);
        }
    }
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
     */
    public String renderScoped(String sectionName, Boolean asString, Boolean shareScope) throws GeneralException, IOException, TemplateException {
        return renderScoped(sectionName, asString, shareScope, null);
    }
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * generic object/ftl-friendly version.
     */
    public String renderScopedGen(String sectionName, Object asString, Object shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException, TemplateException {
        return renderScoped(sectionName, UtilMisc.booleanValue(asString), UtilMisc.booleanValue(shareScope), ctxVars);
    }
    
    /** 
     * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
     * generic object/ftl-friendly version.
     */
    public String renderScopedGen(String sectionName, Object asString, Object shareScope) throws GeneralException, IOException, TemplateException {
        return renderScoped(sectionName, UtilMisc.booleanValue(asString), UtilMisc.booleanValue(shareScope), null);
    }

    public Appendable getWriter() { // SCIPIO
        return contextFetcher.getWriter();
    }
    
    public MapStack<String> getContext() { // SCIPIO
        return contextFetcher.getContext();
    }

    @Override
    public int size() {
        return sectionMap.size();
    }

    @Override
    public boolean isEmpty() {
        return sectionMap.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return sectionMap.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return sectionMap.containsValue(value);
    }

    @Override
    public Object get(Object key) {
        return sectionMap.get(key);
    }

    @Override
    public Object put(String key, Object value) {
        return sectionMap.put(key, value);
    }

    @Override
    public Object remove(Object key) {
        return sectionMap.remove(key);
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        sectionMap.putAll(m);
    }

    @Override
    public void clear() {
        sectionMap.clear();
    }

    @Override
    public Set<String> keySet() {
        return sectionMap.keySet();
    }

    @Override
    public Collection<Object> values() {
        return sectionMap.values();
    }

    @Override
    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        return sectionMap.entrySet();
    }
}
