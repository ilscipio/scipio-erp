package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;

public class NoopDirective implements TemplateDirectiveModel {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public void execute(Environment env, @SuppressWarnings("rawtypes") Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
            throws TemplateException, IOException {
        if (Debug.verboseOn()) {
            Debug.logVerbose("Freemarker: unimplemented directive invoked in template", module);
        }
    }

}
