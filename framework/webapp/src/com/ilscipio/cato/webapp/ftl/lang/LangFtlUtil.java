package com.ilscipio.cato.webapp.ftl.lang;

import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.utility.DeepUnwrap;

public abstract class LangFtlUtil {

    protected LangFtlUtil() {
    }

    /**
     * Unwraps template model; if cannot, returns null.
     */
    public static Object unwrapOrNull(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            Object res = DeepUnwrap.permissiveUnwrap(templateModel);
            if (res != templateModel) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }
    }

    /**
     * If TemplateModel, unwraps value, or if cannot, returns null;
     * if not TemplateModel, returns as-is.
     * <p>
     * Ensures no TemplateModels remain.
     */
    public static Object unwrapOrNull(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            Object res = DeepUnwrap.permissiveUnwrap((TemplateModel) value);
            if (res != value) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, returns as-is.
     */
    public static Object unwrapPermissive(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.permissiveUnwrap(templateModel);
        }
        else {
            return null;
        }
    }

    /**
     * Unwraps value; if cannot, returns value, even if still TemplateModel.
     */
    public static Object unwrapPermissive(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.permissiveUnwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, throws exception. If null, returns null.
     */
    public static Object unwrap(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
        }
        else {
            return null;
        }
    }

    /**
     * If template model, unwraps, or if cannot, throws exception;
     * if not template model or null, returns value.
     */
    public static Object unwrap(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, throws exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(TemplateModel templateModel) throws TemplateModelException {
        // FIXME? should all these DeepUnwrap.unwrap calls be more like FreeMarkerWorker.getWrappedObject(templateModel) instead??
        return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
    }

    /**
     * Unwraps value if template model and unwrappable; else exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel || value == null) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            throw new TemplateModelException("Cannot unwrap non-TemplateModel value (type " + value.getClass().getName() + ")");
        }
    }
    
}
