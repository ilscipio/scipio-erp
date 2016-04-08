package com.ilscipio.cato.ce.webapp.ftl.lang;

import freemarker.template.AdapterTemplateModel;

/**
 * Cato: Special Freemarker TemplateModel that can be used to represent null in some functions.
 */
public final class TemplateNullModel implements AdapterTemplateModel {

    public static final TemplateNullModel nullModel = new TemplateNullModel();
    
    static TemplateNullModel getNullModel() {
        return nullModel;
    }
    
    private TemplateNullModel(){
    }
    
    @Override
    public Object getAdaptedObject(Class arg0) {
        return null;
    }
    
}
