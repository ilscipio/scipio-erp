package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.Map;

import org.ofbiz.webapp.ftl.ObjectRewrapper;

import freemarker.core.Environment;
import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.beans.SimpleMapModel;
import freemarker.template.DefaultMapAdapter;
import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.utility.ObjectWrapperWithAPISupport;

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
                    return new SimpleHash((Map<?, ?>) arg, objectWrapper);
                } else {
                    if (objectWrapper instanceof BeansWrapper) {
                        return new SimpleMapModel((Map<?, ?>) arg, (BeansWrapper) objectWrapper);
                    } else if (objectWrapper instanceof ObjectWrapperWithAPISupport) {
                        return DefaultMapAdapter.adapt((Map<?, ?>) arg, (ObjectWrapperWithAPISupport) objectWrapper);
                    } else {
                        return new SimpleHash((Map<?, ?>) arg, objectWrapper);
                    }
                }
            } else {
                if (copying) {
                    return new SimpleHash((Map<?, ?>) arg, objectWrapper);
                } else {
                    if (objectWrapper instanceof BeansWrapper) {
                        return new SimpleMapModel((Map<?, ?>) arg, (BeansWrapper) objectWrapper);
                    } else if (objectWrapper instanceof ObjectWrapperWithAPISupport) {
                        return DefaultMapAdapter.adapt((Map<?, ?>) arg, (ObjectWrapperWithAPISupport) objectWrapper);
                    } else {
                        throw new TemplateModelException("SimpleMapObjectWrapper tried to make an adapter wrapped "
                                + "for a Map, but our ObjectWrapper does not support SimpleMapModel or DefaultMapAdapter");
                    }
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