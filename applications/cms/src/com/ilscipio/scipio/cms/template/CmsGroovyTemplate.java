package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.template.CmsTemplate.PageLinkDirective;
import com.ilscipio.scipio.cms.template.CmsTemplate.CmsTemplateLoader;

import freemarker.template.Configuration;

public class CmsGroovyTemplate extends CmsTemplate {
    private static final long serialVersionUID = 253162516731256771L;
    private Long inputPosition = 0L;
    private String templateName;
    public static final String module = CmsPageTemplate.class.getName();

    
    public Long getInputPosition() {
        return inputPosition;
    }

    public void setInputPosition(Long inputPosition) {
        this.inputPosition = inputPosition;
    }

    public String getTemplateName() {
        return templateName;
    }

    public void setTemplateName(String templateName) {
        this.templateName = templateName;
    }


    public CmsGroovyTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsGroovyTemplate(Map<String, ?> fields) {
        super(fields);
    }

    public CmsGroovyTemplate(String primaryKey) {
        super(primaryKey);
    }

    @Override
    protected CmsPageContent setDirectives(CmsPageContent content, CmsPageContext context) {
        //content.put("link_to", new PageLinkDirective(context));
        //return content;
        return null;
    }

    /**
     * Returns the template with the given name within.
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     
    public static CmsGroovyTemplate findByName(String templateName,
            String webSiteId) {
        return CmsDataObject.<CmsGroovyTemplate> findFirst(UtilMisc.toMap(
                "templateName", templateName),
                CmsGroovyTemplate.class);
    }*/
    
}
