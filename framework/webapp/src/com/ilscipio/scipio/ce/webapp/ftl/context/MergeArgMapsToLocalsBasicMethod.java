package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.List;

import freemarker.template.TemplateModelException;

public class MergeArgMapsToLocalsBasicMethod extends ArgMapMethod {

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execMergeArgMapsToLocals(args, false);
    }

}
