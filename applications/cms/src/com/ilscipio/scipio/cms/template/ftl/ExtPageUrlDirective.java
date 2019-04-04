package com.ilscipio.scipio.cms.template.ftl;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.template.FtlTransformFactory;
import org.ofbiz.webapp.ftl.WebappUrlDirective.PageUrlDirective;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: Delegates to either <code>PageUrlDirective</code> or <code>CmsPageUrlDirective</code> as appropriate.
 */
public class ExtPageUrlDirective implements TemplateDirectiveModel {

    public static class Factory implements FtlTransformFactory { // SCIPIO
        private static final ExtPageUrlDirective DEFAULT = new ExtPageUrlDirective();

        @Override
        public TemplateModel getTransform(ClassLoader loader) {
            return DEFAULT;
        }
    }

    public static TemplateDirectiveModel getInstance() {
        return Factory.DEFAULT;
    }

    public static TemplateDirectiveModel create() { // SCIPIO: See CMS's PageUrlDirective
        return new ExtPageUrlDirective();
    }

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        if (params.containsKey("id") || params.containsKey("name")) {
            CmsPageUrlDirective.getInstance().execute(env, params, loopVars, body);
            return;
        }
        PageUrlDirective.getInstance().execute(env, params, loopVars, body);
    }

    /**
     * SCIPIO: Delegates to either <code>PageUrlDirective</code> or <code>CmsPageUrlDirective</code> as appropriate;
     * method wrapper.
     */
    public static class Method implements TemplateMethodModelEx {
        public static class Factory implements FtlTransformFactory { // SCIPIO
            private static final Method DEFAULT = new Method();
            @Override
            public TemplateModel getTransform(ClassLoader loader) {
                return DEFAULT;
            }
        }
        public static Method getInstance() {
            return Factory.DEFAULT;
        }
        public static Method create() { // SCIPIO: See CMS's PageUrlDirective
            return new Method();
        }

        @Override
        public Object exec(@SuppressWarnings("rawtypes") List posArgs) throws TemplateModelException {
            Map<String, TemplateModel> args = org.ofbiz.webapp.ftl.WebappUrlDirective.getArgsMapOrNull(posArgs);
            if (args != null && (args.containsKey("id") || args.containsKey("name"))) {
                return CmsPageUrlDirective.Method.getInstance().exec(args, posArgs);
            }
            return PageUrlDirective.Method.getInstance().exec(args, posArgs);
        }
    }
}