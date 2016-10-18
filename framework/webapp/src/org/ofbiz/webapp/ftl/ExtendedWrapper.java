package org.ofbiz.webapp.ftl;

import java.util.Collection;
import java.util.Map;

import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;

import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.beans.CollectionModel;
import freemarker.ext.beans.StringModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
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
 */
//not sure if this is the best way to get FTL to use my fancy MapModel derivative, but should work at least...
public class ExtendedWrapper extends BeansWrapper implements EscapingObjectWrapper { // SCIPIO: Now implements EscapingObjectWrapper for identification purposes

    protected final String lang;
    protected final SimpleEncoder encoder;
    
    public ExtendedWrapper(Version version, String lang) {
        super(version);
        this.lang = lang;
        this.encoder = UtilCodec.getEncoder(lang);
    }

    @Override
    public TemplateModel wrap(Object object) throws TemplateModelException {
        // This StringHtmlWrapperForFtl option seems to be the best option
        // and handles most things without causing too many problems
        if (object instanceof String) {
            return new StringWrapperForFtl((String) object, this);
        } else if (object instanceof Collection && !(object instanceof Map)) {
            // An additional wrapper to ensure ${aCollection} is properly encoded for html
            return new CollectionWrapperForFtl((Collection<?>) object, this);
        }
        return super.wrap(object);
    }
    

    public class StringWrapperForFtl extends StringModel implements EscapingModel { // SCIPIO: special interface, and renamed
        public StringWrapperForFtl(String str, BeansWrapper wrapper) {
            super(str, wrapper);
        }
        @Override
        public String getAsString() {
            return encoder.encode(super.getAsString());
        }
    }

    public class CollectionWrapperForFtl extends CollectionModel implements EscapingModel { // SCIPIO: special interface, and renamed

        public CollectionWrapperForFtl(Collection<?> collection, BeansWrapper wrapper) {
            super(collection, wrapper);
        }

        @Override
        public String getAsString() {
            return encoder.encode(super.getAsString());
        }

    }
}