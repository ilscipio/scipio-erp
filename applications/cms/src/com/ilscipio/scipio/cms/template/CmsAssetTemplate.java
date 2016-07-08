package com.ilscipio.scipio.cms.template;

import java.util.List;
import java.util.Map;

import javolution.util.FastList;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.widget.model.HtmlWidget.ExtendedWrapper;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public class CmsAssetTemplate extends CmsTemplate {
	private static final long serialVersionUID = -4130339900319191347L;
    private Long position = 0L;
    private Boolean inactive;
    private String importName;
	public static final String module = CmsPageTemplate.class.getName();
	
	static {
	        fmTemplateLoader = new CmsTemplateLoader<CmsAssetTemplate>();
	        fmConfig = FreeMarkerWorker.makeConfiguration(new ExtendedWrapper(FreeMarkerWorker.version));
	        fmConfig.setTemplateLoader(fmTemplateLoader);
	}

	public CmsAssetTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsAssetTemplate(Map<String, ?> fields) {
        super(fields);
    }

    public CmsAssetTemplate(String primaryKey) {
        super(primaryKey);
    }

    public Long getPosition() {
        return position;
    }

    public void setPosition(Long position) {
        this.position = position;
    }

    public String getImportName() {
        return importName;
    }

    public void setImportName(String importName) {
        this.importName = importName;
    }

    public boolean isActiveVersion() {
        Boolean isInactive = getInactive();
        // Null value implies version is active - for compatibility with old code
        return ((isInactive == null) || (isInactive == false));
    }
    
    public CmsAssetTemplate getActiveVersionOfSelf() throws CmsException {
        CmsAssetTemplate result;
        if (isActiveVersion()) {
            result = this;
        }
        else {
            // Get the active one instead
            String relatingAssetId = getId();
            String templateName = getName();
            String websiteId = getWebSiteId();
            result = CmsAssetTemplate.findActiveVersion(templateName, websiteId);
            if (result == null) {
                throw new CmsException("Could not find active or default version of asset template (templateName '" +
                        templateName + "', via related assetId '" + relatingAssetId + "'); possible invalid database status", module);
            }
        }
        return result;
    }
    
    public CmsAssetTemplate getLatestVersionOfSelf() throws CmsException {
        CmsAssetTemplate result;
        
        String relatingAssetId = getId();
        String templateName = getName();
        String websiteId = getWebSiteId();
        // No good way of determining whether current is latest without running a query
        result = CmsAssetTemplate.findLatestVersion(templateName, websiteId);
        if (result == null) {
            throw new CmsException("Could not find latest version of asset template (templateName '" +
                    templateName + "', via related assetId '" + relatingAssetId + "'); possible invalid database status", module);
        }
        
        if (this.getId().equals(result.getId())) {
            return this;
        }
        else {
            return result;
        }
    }    
    
    
    /**
     * Removes the asset template from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    public boolean remove() {
        int rowsAffected = 0;
        try {
            entity.set("inactive", true);
            rowsAffected = getDelegator().store(entity);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be removed.", e, module);
        }
        return rowsAffected > 0 ? true : false;
    }

    @Override
    protected CmsPageContent setDirectives(CmsPageContent content,
            CmsPageContext context) {
        content.put("link_to", new PageLinkDirective(context));
        return content;
    }

    @Override
    public Map<String, Object> getDescriptor() {
        Map<String, Object> descriptor = super.getDescriptor();
        descriptor.put("importName", getImportName());
        return descriptor;
    }

    /**
     * Returns the (active) template (version) with the given name within a website.
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     */
    public static CmsAssetTemplate findByName(String templateName,
            String webSiteId) {
        return findActiveVersion(templateName, webSiteId);
    }
    
    /**
     * Returns the active template version with the given name within a website.
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     */
    public static CmsAssetTemplate findActiveVersion(String templateName,
            String webSiteId) {
        List<EntityCondition> conds = FastList.newInstance();
        conds.add(EntityCondition.makeCondition("templateName", EntityOperator.EQUALS, templateName));
        conds.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId));
        conds.add(EntityCondition.makeCondition(EntityOperator.OR,
                EntityCondition.makeCondition("inactive", EntityOperator.EQUALS, "N"),
                EntityCondition.makeCondition("inactive", EntityOperator.EQUALS,  GenericEntity.NULL_FIELD)));
        
        conds.add(makeActiveAssetTemplateCondition());
        EntityCondition whereCondition = EntityCondition.makeCondition(conds, EntityOperator.AND);
        
        return CmsDataObject.<CmsAssetTemplate> findFirst(whereCondition, null, CmsAssetTemplate.class);
    }
    
    /**
     * Returns the active template version with the given name within a website.
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     */
    public static CmsAssetTemplate findLatestVersion(String templateName,
            String webSiteId) {
        List<EntityCondition> conds = FastList.newInstance();
        conds.add(EntityCondition.makeCondition("templateName", EntityOperator.EQUALS, templateName));
        conds.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId));
        EntityCondition whereCondition = EntityCondition.makeCondition(conds, EntityOperator.AND);
        List<String> orderBy = UtilMisc.toList("createdStamp DESC");
        return CmsDataObject.<CmsAssetTemplate> findFirst(whereCondition, orderBy, CmsAssetTemplate.class);
    }    
    
    /**
     * Returns the active template version related to the given asset template version
     * (i.e., same logical asset template, but different logical version).
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     */
    public static CmsAssetTemplate findRelatedActiveVersion(String relatedAssetTemplateId) {
        // Use findFirst here instead of constructor so no exceptions thrown if the asset was normally deleted or something
        // and the query was simply out of date.
        CmsAssetTemplate relatedAssetTemplate = 
            CmsDataObject.<CmsAssetTemplate> findFirst(UtilMisc.toMap("assetTemplateId", relatedAssetTemplateId), CmsAssetTemplate.class);

        if (relatedAssetTemplate != null) {
            return findActiveVersion(relatedAssetTemplate.getName(), relatedAssetTemplate.getWebSiteId());
        }
        else {
            return null;
        }
    }    
    

    /**
     * Returns all (active) asset templates of a website.
     * 
     * @param webSiteId
     * @return
     */
    public static List<CmsAssetTemplate> findByWebSiteId(String webSiteId) {
        List<EntityCondition> conds = FastList.newInstance();
        conds.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId));
        conds.add(makeActiveAssetTemplateCondition());
        EntityCondition whereCondition = EntityCondition.makeCondition(conds, EntityOperator.AND);
        return CmsDataObject.<CmsAssetTemplate> findAll(whereCondition,
                        CmsAssetTemplate.class, UtilMisc.toList("templateName ASC"));
    }
    
    private static EntityCondition makeActiveAssetTemplateCondition() {
        return EntityCondition.makeCondition(
                EntityCondition.makeCondition("inactive", EntityOperator.EQUALS, "N"),
                EntityOperator.OR,
                EntityCondition.makeCondition("inactive", EntityOperator.EQUALS, null)
        );
    }

}
