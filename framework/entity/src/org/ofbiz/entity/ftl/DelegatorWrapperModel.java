package org.ofbiz.entity.ftl;

import org.ofbiz.base.util.template.ScipioFtlWrappers.ScipioModelFactory;
import org.ofbiz.base.util.template.ScipioFtlWrappers.ScipioObjectWrapper;
import org.ofbiz.entity.Delegator;

import freemarker.ext.beans.BeanModel;
import freemarker.ext.beans.BeansWrapper;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: A FreeMarker-specific wrapper around {@link org.ofbiz.entity.Delegator} that causes
 * the {@link org.ofbiz.entity.Delegator#query()} method to behave like {@link org.ofbiz.entity.Delegator#querySafe()}
 * instead of {@link org.ofbiz.entity.Delegator#queryUnsafe()}, with equivalent substitution of the
 * <code>from</code> and <code>select</code> methods.
 */
public class DelegatorWrapperModel extends BeanModel {

    public DelegatorWrapperModel(Object object, BeansWrapper wrapper) {
        super(object, wrapper);
    }

    public static class Factory implements ScipioModelFactory {
        @Override
        public TemplateModel wrap(Object object, ScipioObjectWrapper objectWrapper) throws TemplateModelException {
            if (object instanceof Delegator) {
                return new DelegatorWrapperModel(object, (BeansWrapper) objectWrapper);
            }
            return null;
        }
    }

    @Override
    public Delegator getWrappedObject() {
        return (Delegator) super.getWrappedObject();
    }

    @Override
    public TemplateModel get(String key) throws TemplateModelException {
        switch(key) {
        case "query":
            return super.get("querySafe");
        case "from":
            return super.get("fromSafe");
        case "select":
            return super.get("selectSafe");
        }
        return super.get(key);
    }
}
