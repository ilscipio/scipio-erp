package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.List;

import freemarker.template.TemplateModelException;

public class MergeArgMapsToLocalsBasicMethod extends ArgMapMethod {

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        return execMergeArgMapsToLocals(args, false);
    }

}
