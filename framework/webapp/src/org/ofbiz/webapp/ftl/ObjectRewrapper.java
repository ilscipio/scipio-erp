package org.ofbiz.webapp.ftl;

import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: Interfaces for ObjectWrappers that support rewrapping
 * values that are already TemplateModels.
 */
public interface ObjectRewrapper {

    TemplateModel rewrap(TemplateModel model) throws TemplateModelException;
}
