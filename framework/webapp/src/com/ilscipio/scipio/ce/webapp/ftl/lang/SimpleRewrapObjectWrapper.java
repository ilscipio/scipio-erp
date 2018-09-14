package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.Map;

import org.ofbiz.webapp.ftl.ObjectRewrapper;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

public class SimpleRewrapObjectWrapper implements ObjectWrapper, ObjectRewrapper {

    protected final Environment env;
    protected final ObjectWrapper objectWrapper;
    protected final boolean preserving;
    protected final Boolean copying;

    protected SimpleRewrapObjectWrapper(Environment env, ObjectWrapper objectWrapper, boolean preserving,
            Boolean copying) {
        this.env = env;
        this.objectWrapper = objectWrapper;
        this.preserving = preserving;
        this.copying = copying;
    }

    public static SimpleRewrapObjectWrapper getInstance(Environment env, ObjectWrapper objectWrapper,
            boolean preserving, Boolean copying) {
        return new SimpleRewrapObjectWrapper(env, objectWrapper, preserving, copying);
    }

    @Override
    public TemplateModel wrap(Object arg) throws TemplateModelException {
        // TODO: this should handle other types
        if (arg instanceof Map) {
            if (preserving) {
                // this is more permissive
                if (Boolean.TRUE.equals(copying)) { // here copying just means "preferred"
                    return LangFtlUtil.makeSimpleMapCopy((Map<?, ?>) arg, objectWrapper);
                } else {
                    return LangFtlUtil.makeSimpleMapAdapter((Map<?, ?>) arg, objectWrapper, true);
                }
            } else {
                if (copying) {
                    return LangFtlUtil.makeSimpleMapCopy((Map<?, ?>) arg, objectWrapper);
                } else {
                    return LangFtlUtil.makeSimpleMapAdapter((Map<?, ?>) arg, objectWrapper, false);
                }
            }

        } else {
            return objectWrapper.wrap(arg);
        }
    }

    @Override
    public TemplateModel rewrap(TemplateModel model) throws TemplateModelException {
        // TODO: not ready for use
        return model;
    }

}