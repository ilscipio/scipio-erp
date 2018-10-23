package org.ofbiz.webapp.ftl;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.ofbiz.base.util.template.ScipioFtlWrappers.ScipioExtendedObjectWrapper;

import freemarker.ext.beans.BeansWrapper;
import freemarker.template.Version;

/**
 * Freemarker BeansWrapper extension that implements automatic data model escaping
 * for strings and collections.
 * <p>
 * SCIPIO: This class has been MOVED from org.ofbiz.widget.model.HtmlWidget.
 * It is also generalized to accept others than "html".
 * <p>
 * TODO: Revisit this in future because Freemarker 2.4 supports an alternative to this
 * and there may be need for non-html also.
 * <p>
 * @deprecated 2017-04-03: this has been revamped to plug into the new freemarkerWrapperFactories.properties system.
 * See {@link org.ofbiz.base.util.template.ScipioFtlWrappers}.
 */
@Deprecated
public abstract class ExtendedWrapper extends BeansWrapper implements ScipioExtendedObjectWrapper { // SCIPIO: Now implements EscapingObjectWrapper for identification purposes

    protected final String lang;
    protected final SimpleEncoder encoder;

    // SCIPIO: 2017-04-03: this is now PRIVATE - use
    private ExtendedWrapper(Version version, String lang) {
        super(version);
        this.lang = lang;
        this.encoder = UtilCodec.getEncoder(lang);
    }

//    @Override
//    public TemplateModel wrap(Object object) throws TemplateModelException {
//        // This StringHtmlWrapperForFtl option seems to be the best option
//        // and handles most things without causing too many problems
//        if (object instanceof String) {
//            return new StringWrapperForFtl((String) object, this);
//        } else if (object instanceof Collection && !(object instanceof Map)) {
//            // An additional wrapper to ensure ${aCollection} is properly encoded for html
//            return new CollectionWrapperForFtl((Collection<?>) object, this);
//        }
//        return super.wrap(object);
//    }

}