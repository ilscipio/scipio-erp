package org.ofbiz.base.util.template;

import freemarker.template.TemplateModel;

/**
 * SCIPIO: An interface that instances of freemarkerTransforms.properties
 * may implement to return new TemplateModel instances.
 * Note that this is run during initialization only.
 * <p>
 * Can be used to implement singletons.
 */
public interface FtlTransformFactory {

    TemplateModel getTransform(ClassLoader loader);

}