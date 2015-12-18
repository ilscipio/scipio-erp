package com.ilscipio.cato.ce.webapp.ftl.context;

import java.util.List;

import freemarker.template.TemplateModelException;

public class ProcessArgMapsBasicMethod extends ArgsMapMethod {

    /*
     * @see freemarker.template.TemplateMethodModel#exec(java.util.List)
     */
    @SuppressWarnings("unchecked")
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execProcessArgMaps(args, false);
    }

}
