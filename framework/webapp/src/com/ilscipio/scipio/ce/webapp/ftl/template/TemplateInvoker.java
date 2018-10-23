package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.base.util.template.ScipioFtlWrappers.ScipioModelFactory;
import org.ofbiz.base.util.template.ScipioFtlWrappers.ScipioObjectWrapper;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateInvoker.InvokeOptions.InvokeMode;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.ObjectWrapper;
import freemarker.template.Template;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * Template invoker base class.
 * Invokes Freemarker template render in a separate render from
 * any current renderer, using options carried in the instance itself,
 * using methods such as Ofbiz's FreeMarkerWorker does, and contrary
 * to the Freemarker <code>?interpret</code> call which inlines
 * in the current renderer (and therefore poses restrictions).
 * <p>
 * This is needed to:
 * 1) carry the compilation and invocation options around
 * 2) allow nested Freemarker template renders that behave like separate renders
 *    (and not like inlined interprets like <code>?interpret</code> does)
 * 3) enable evaluating templates using <code>?string</code> operator from freemarker templates
 * 4) allow lazy compilation but that still gets cached
 * <p>
 * For invoking other templates from within an FTL template,
 * StringTemplateInvoker can be used which causes the
 * invocation through the toString() method when this
 * is wrapped in a StringModel by the ObjectWrapper.
 * <p>
 * NOTE: 2017-04-03: all of the security, error handling and java misuse issues with StringTemplateInvoker
 * have been fixed using TemplateInvokerModelFactory below. We now ALWAYS avoid wrapping TemplateInvoker
 * using BeanModel and StringTemplateInvoker is deprecated - do not use anymore.
 * <p>
 * TODO?: this currently does not support lazy template compilation... don't see much need.
 */
public class TemplateInvoker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final TemplateSource templateSource;
    protected final InvokeOptions invokeOptions;
    /**
     * Carries around the preferred FTL model to use when
     * this is wrapped by an ObjectWrapper.
     * CURRENTLY (2017-02), this is always SCALAR.
     * TODO: more usage
     * <p>
     * This is needed here because the FTL unwrapping across macro invocations
     * will cause information loss about the model or more generally form of the wrapper.
     * NOTE: ideally this should be honored by the ObjectWrapper implementation,
     * so for now we are relying on SCALAR and if DIRECTIVE is needed then a
     * manual wrapping call has to be done.
     */
    protected final WrapperModel preferredModel;

    public static class InvokeOptions {

        public enum InvokeMode {

            /**
             * Standard, standalone Ofbiz-like invocation, as if using FreeMarkerWorker.
             */
            OFBIZ_STD("ofbiz-std");

            /*
             * Inline, simulation of <code>?interpret</code> directive behavior.
             * TODO: not sure this will even possible. the point would be
             * to get ?string support.
             */
            //FTL_INLINE; // TODO

            private static final Map<String, InvokeMode> nameMap;
            static {
                Map<String, InvokeMode> map = new HashMap<>();
                for(InvokeMode mode : InvokeMode.values()) {
                    map.put(mode.getName(), mode);
                }
                nameMap = map;
            }

            private final String name;

            private InvokeMode(String name) {
                this.name = name;
            }

            public String getName() {
                return name;
            }

            public static InvokeMode fromName(String str) {
                return nameMap.get(str);
            }

            public static InvokeMode fromNameAlways(String str) {
                InvokeMode mode = fromName(str);
                if(mode == null) {
                    throw new IllegalArgumentException("Unrecognized template invoke mode: " + str);
                }
                return mode;
            }
        }

        protected final InvokeMode invokeMode;

        protected final Map<String, Object> context;

        /**
         * If true, pushes the Ofbiz MapStack context before invoke and pops after.
         */
        protected final Boolean shareScope;

        protected final Map<String, Object> ctxVars;

        protected final boolean envOut;

        public InvokeOptions(InvokeMode invokeMode, Map<String, Object> context,
                Boolean shareScope, Map<String, Object> ctxVars, boolean envOut) {
            this.invokeMode = invokeMode;
            this.context = context;
            this.shareScope = shareScope;
            this.ctxVars = ctxVars;
            this.envOut = envOut;
        }

        public InvokeOptions(InvokeMode invokeMode, Boolean shareScope, Map<String, Object> ctxVars) {
            this(invokeMode, null, shareScope, ctxVars, false);
        }

        public InvokeMode getInvokeMode() {
            return invokeMode;
        }

        /**
         * The context object to use. By default the well-known MapStack is used.
         */
        public Map<String, Object> getContext() {
            return context;
        }

        public Boolean getShareScope() {
            return shareScope;
        }

        public Boolean getShareScope(Boolean defaultVal) {
            return shareScope != null ? shareScope : defaultVal;
        }

        /**
         * NOTE: these may already be TemplateModels, or not.
         */
        public Map<String, Object> getCtxVars() {
            return ctxVars;
        }

        public boolean isEnvOut() {
            return envOut;
        }
    }

    /**
     * FTL TemplateModel type to wrap this TemplateInvoker in.
     * <p>
     * NOTE: <code>SCALAR</code> is unspecific between StringModel-wrapped StringTemplateInvoker
     * and dedicated TemplateScalarModel implementation.
     * <p>
     * Currently (2017-02) for SCALAR our code will be using StringModel-wrapped StringTemplateInvoker.
     */
    public enum WrapperModel {
        SCALAR("scalar"),
        DIRECTIVE("directive"),
        HYBRID("hybrid");

        private static final Map<String, WrapperModel> nameMap;
        static {
            Map<String, WrapperModel> map = new HashMap<>();
            for(WrapperModel mode : WrapperModel.values()) {
                map.put(mode.getName(), mode);
            }
            nameMap = map;
        }

        private final String name;

        private WrapperModel(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public static WrapperModel fromName(String str) {
            return nameMap.get(str);
        }

        public static WrapperModel fromNameAlways(String str) {
            WrapperModel mode = fromName(str);
            if(mode == null) {
                throw new IllegalArgumentException("Unrecognized template invoke wrapper model: " + str);
            }
            return mode;
        }
    }

    protected TemplateInvoker(TemplateSource templateSource, InvokeOptions invokeOptions, WrapperModel preferredModel) {
        this.templateSource = templateSource;
        this.invokeOptions = invokeOptions;
        this.preferredModel = preferredModel;
    }

    /* ***************************************************** */
    /* Factory Methods */
    /* ***************************************************** */

    /**
     * Gets a standard template invoker.
     */
    public static TemplateInvoker getInvoker(TemplateSource templateSource, InvokeOptions invokeOptions, WrapperModel preferredModel) throws TemplateException, IOException {
        return new TemplateInvoker(templateSource, invokeOptions, preferredModel);
    }

    /**
     * Gets a standard template invoker.
     */
    public static TemplateInvoker getInvoker(TemplateSource templateSource, InvokeOptions invokeOptions) throws TemplateException, IOException {
        return new TemplateInvoker(templateSource, invokeOptions, null);
    }

//    /**
//     * Gets a template invoker same as standard except its toString() method invokes rendering.
//     * @deprecated should be avoided because breaks toString contract and the original exception type is lost
//     */
//    public static TemplateInvoker getStringInvoker(TemplateSource templateSource, InvokeOptions invokeOptions, WrapperModel preferredModel) throws TemplateException, IOException {
//        return new StringTemplateInvoker(templateSource, invokeOptions, preferredModel);
//    }


    /* ***************************************************** */
    /* Main Invocation Calls */
    /* ***************************************************** */

    /**
     * Invokes rendering and outputs to Writer.
     */
    public void invoke(Writer out) throws TemplateException, IOException {
        Template template = getTemplate();
        Map<String, Object> context = getContext();
        if (invokeOptions.getInvokeMode() == null || invokeOptions.getInvokeMode() == InvokeMode.OFBIZ_STD) {
            Boolean protectScope = !invokeOptions.getShareScope(false);
            if (!(context instanceof MapStack)) protectScope = false;
            if (protectScope) {
                ((MapStack<String>) context).push();
            }
            try {
                if (invokeOptions.getCtxVars() != null) {
                    context.putAll(invokeOptions.getCtxVars());
                }
                FreeMarkerWorker.renderTemplate(template, context, out);
            } finally {
                if (protectScope) {
                    ((MapStack<String>) context).pop();
                }
            }
        } else {
            throw new UnsupportedOperationException("Unsupported template invoke mode: " + invokeOptions.invokeMode);
        }
    }

    /**
     * Invokes rendering and returns as a String.
     */
    public String invoke() throws TemplateException, IOException {
        StringWriter sw = new StringWriter();
        invoke(sw);
        return sw.toString();
    }



    /* ***************************************************** */
    /* Getters and Helpers */
    /* ***************************************************** */

    public Template getTemplate() throws TemplateException, IOException {
        return templateSource.getTemplate();
    }

    protected TemplateSource getTemplateSource() throws TemplateException, IOException {
        return templateSource;
    }

    public InvokeOptions getInvokeOptions() {
        return invokeOptions;
    }

    public WrapperModel getPreferredModel() {
        return preferredModel;
    }

    protected Map<String, Object> getContext() {
        Map<String, Object> context = invokeOptions.getContext();
        if (context == null) {
            try {
                context = ContextFtlUtil.getContext(FreeMarkerWorker.getCurrentEnvironment());
            } catch (Exception e) {
                Debug.logError(e, module);
            }
        }
        if (context == null) {
            context = MapStack.create();
        }
        return context;
    }

    /* ***************************************************** */
    /* Specific Invoker Implementations */
    /* ***************************************************** */

//    /**
//     * Variant of TemplateInvoker that invokes rendering when toString() is called.
//     * @deprecated should be avoided because breaks toString contract and the original exception type is lost
//     */
//    @Deprecated
//    public static class StringTemplateInvoker extends TemplateInvoker {
//        protected StringTemplateInvoker(TemplateSource templateSource, InvokeOptions invokeOptions,
//                WrapperModel preferredModel) {
//            super(templateSource, invokeOptions, preferredModel);
//        }
//
//        @Override
//        public String toString() {
//            try {
//                // NOTE: this env check really belongs in the TemplateModels below, but because of the bean wrapper
//                // mode, it has to be here.
//                if (this.getInvokeOptions().isEnvOut()) {
//                    Environment env = FreeMarkerWorker.getCurrentEnvironment();
//                    if (env != null) {
//                        Writer out = env.getOut();
//                        if (out != null) {
//                            this.invoke(out);
//                            return "";
//                        }
//                    }
//                }
//                return this.invoke();
//                // FIXME: we're not supposed to do this AT ALL because it breaks toString() contract,
//                // but in the circumstances we have no choice but to rethrow this, because otherwise
//                // we swallow errors that could be considered security issues
////            } catch (TemplateException e) {
////                Debug.logError(e, module);
////                return "";
////            } catch (IOException e) {
////                Debug.logError(e, module);
////                return "";
////            }
//            } catch (TemplateException e) {
//                throw new RuntimeException(e);
//            } catch (IOException e) {
//                throw new RuntimeException(e);
//            }
//        }
//    }

    /* ***************************************************** */
    /* Invoker Freemarker Wrappers */
    /* ***************************************************** */

    /**
     * Wraps the TemplateInvoker in an appropriate TemplateModel, using invoker's preferred model.
     * Can be called manually as this logic may not be present in <code>ObjectWrapper.wrap</code>.
     * <p>
     * TODO: currently (2017-02) this always wraps using StringModel, done by most of the
     * default object wrapper impls.
     */
    public static <T extends TemplateModel> T wrap(TemplateInvoker invoker, ObjectWrapper objectWrapper) throws TemplateModelException {
        return wrap(invoker, invoker.getPreferredModel(), objectWrapper);
    }

    public static <T extends TemplateModel> T wrap(TemplateInvoker invoker) throws TemplateModelException {
        return wrap(invoker, invoker.getPreferredModel(), null);
    }

    /**
     * Wraps the TemplateInvoker in an appropriate TemplateModel, using explicit model type.
     * Can be called manually as this logic may not be present in <code>ObjectWrapper.wrap</code>.
     */
    @SuppressWarnings("unchecked")
    public static <T extends TemplateModel> T wrap(TemplateInvoker invoker, WrapperModel targetModel, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetModel == null || targetModel == WrapperModel.HYBRID) {
            return (T) new HybridInvokerWrapper(invoker);
        } else if (targetModel == null || targetModel == WrapperModel.SCALAR) {
            // 2017-03-31: just avoid the bean wrapper as much as possible, even though may not entirely be able to
//            if (invoker instanceof StringTemplateInvoker && objectWrapper != null) {
//                return (T) objectWrapper.wrap(invoker);
//            } else {
            return (T) new ScalarInvokerWrapper(invoker);
//            }
        } else if (targetModel == WrapperModel.DIRECTIVE) {
            return (T) new DirectiveInvokerWrapper(invoker);
        }
        throw new UnsupportedOperationException("Unsupported template invoker FTL wrapper model: " + targetModel);
    }

    public static <T extends TemplateModel> T wrap(TemplateInvoker invoker, WrapperModel targetModel) throws TemplateModelException {
        return wrap(invoker, targetModel, null);
    }

    /**
     * Custom Freemarker wrapper TemplateModel around the TemplateInvoker.
     * <p>
     * TODO: currently this is mainly for testing; will be using StringModel-wrapped
     * StringInvokerWrapper in the interim.
     */
    public static abstract class InvokerWrapper implements TemplateModel, WrapperTemplateModel {
        protected final TemplateInvoker invoker;

        protected InvokerWrapper(TemplateInvoker invoker) {
            this.invoker = invoker;
        }

        @Override
        public Object getWrappedObject() {
            return invoker;
        }

        protected String invokeAsString() throws TemplateModelException {
            try {
                return invoker.invoke();
            } catch (TemplateException e) {
                throw new TemplateModelException(e);
            } catch (IOException e) {
                throw new TemplateModelException(e);
            }
        }
    }

    public static class DirectiveInvokerWrapper extends InvokerWrapper implements TemplateDirectiveModel {
        public DirectiveInvokerWrapper(TemplateInvoker invoker) {
            super(invoker);
        }

        @SuppressWarnings("rawtypes")
        @Override
        public void execute(Environment env, Map args, TemplateModel[] modelArgs, TemplateDirectiveBody body)
                throws TemplateException, IOException {
            invoker.invoke(env.getOut());
        }
    }

    public static class ScalarInvokerWrapper extends InvokerWrapper implements TemplateScalarModel {
        public ScalarInvokerWrapper(TemplateInvoker invoker) {
            super(invoker);
        }

        @Override
        public String getAsString() throws TemplateModelException {
            return invokeAsString();
        }
    }

    public static class HybridInvokerWrapper extends InvokerWrapper implements TemplateDirectiveModel, TemplateScalarModel {
        public HybridInvokerWrapper(TemplateInvoker invoker) {
            super(invoker);
        }

        @SuppressWarnings("rawtypes")
        @Override
        public void execute(Environment env, Map args, TemplateModel[] modelArgs, TemplateDirectiveBody body)
                throws TemplateException, IOException {
            invoker.invoke(env.getOut());
        }

        @Override
        public String getAsString() throws TemplateModelException {
            return invokeAsString();
        }
    }

    /**
     * SPECIAL model factory plugged in via freemarkerWrapperFactories.properties.
     * This ELIMINATES the need to try to work around BeanModel and toString() and all
     * the problems associated.
     */
    public static class TemplateInvokerModelFactory implements ScipioModelFactory {
        @Override
        public TemplateModel wrap(Object object, ScipioObjectWrapper objectWrapper) throws TemplateModelException {
            if (object instanceof TemplateInvoker) {
                return TemplateInvoker.wrap((TemplateInvoker) object, objectWrapper);
            }
            return null;
        }
    }

}
