package org.ofbiz.widget.model;

import java.io.Serializable;
import java.util.Map;

import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.w3c.dom.Element;

/**
 * SCIPIO: 2017-04-21: new
 */
@SuppressWarnings("serial")
public abstract class ModelPageScript implements Serializable {

    protected ModelPageScript() {
    }

    public abstract Object getScript(Map<String, ?> context);

    public static ModelPageScript fromElement(Element element) {
        String preprocessor = element.getAttribute("preprocessor");
        String script = UtilXml.elementValue(element);

        if ("ftl".equals(preprocessor)) {
            // TODO
            throw new UnsupportedOperationException("ftl preprocessor not implemented");
            //return new FtlScript(script);
        } else if ("flexible".equals(preprocessor)) {
            return new FlexibleScript(script);
        } else {
            return new PlainScript(script);
        }
    }

    public static class PlainScript extends ModelPageScript {
        private final String script;

        public PlainScript(String script) {
            this.script = script;
        }

        @Override
        public Object getScript(Map<String, ?> context) {
            return script;
        }
    }

    public static class FlexibleScript extends ModelPageScript {
        private final FlexibleStringExpander script;

        public FlexibleScript(FlexibleStringExpander script) {
            this.script = script;
        }

        public FlexibleScript(String script) {
            this.script = FlexibleStringExpander.getInstance(script);
        }

        @Override
        public Object getScript(Map<String, ?> context) {
            return script.expandString(context);
        }
    }

    /* TODO
    public static class FtlScript extends ModelPageScript {
        private final String script;

        public FtlScript(String script) {
            this.script = script;
        }

        @Override
        public Object getScript(Map<String, ?> context) {
            return TemplateInvoker.getInvoker(TemplateSource., invokeOptions, preferredModel)
        }
    }
    */

}
