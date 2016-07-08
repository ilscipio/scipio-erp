package com.ilscipio.scipio.cms.content;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityFindOptions;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public class CmsPageVersion extends CmsDataObject {
	private static final long serialVersionUID = -714031134721469544L;
	public static final String module = CmsPageVersion.class.getName();
    private CmsPage page;

    public CmsPageVersion(GenericValue entity) {
        super(entity);
    }

    public CmsPageVersion(Map<String, ?> fields, CmsPage page) {
        super(fields);
        this.page = page;
        if (!fields.containsKey("pageId")) {
            this.entity.put("pageId", page.getId());
        }
    }

    public CmsPageVersion(String primaryKey) {
        super(primaryKey);
    }

    public void setContent(String jsonContent) {
        entity.setString("content", jsonContent);
    }

    public void setContent(Map<String, ?> content) {
        try {
			setContent(JSON.from(content).toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    }

    @SuppressWarnings("unchecked")
    public Map<String, ?> getContent() {
        Map<String, ?> content = null;
        String entityContent = (String) entity.get("content");
        if (entityContent != null) {
            try {
				content = (Map<String, ?>) JSON.from(entityContent).toObject(Map.class);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }
        return content != null ? content : FastMap
                .<String, Object> newInstance();
    }
    
    /**
     * Returns the most current page version.
     * 
     * @param pageId
     * @return
     */
    public static CmsPageVersion findLast(String pageId) {
        CmsPageVersion pv = null;
        List<CmsPageVersion> list = findAll(pageId);
        if (list.size() > 0) {
            pv = list.get(0);
        }
        return pv;
    }

    /**
     * Returns all versions of a page starting with the most recent.
     * 
     * @param pageId
     * @return
     */
    public static List<CmsPageVersion> findAll(String pageId) {
        List<CmsPageVersion> versions = new FastList<CmsPageVersion>();
        try {
            EntityFindOptions efo = new EntityFindOptions();
            efo.setFetchSize(1);
            EntityCondition ec = EntityCondition.makeCondition("pageId",
                    EntityOperator.EQUALS, pageId);
            List<GenericValue> values = (List<GenericValue>) getDelegator()
                    .findList("CmsPageVersion", ec, null,
                            UtilMisc.toList("createdStamp DESC"), efo, false);

            for (GenericValue v : values) {
                versions.add(new CmsPageVersion(v));
            }
        } catch (GenericEntityException e) {
            throw new CmsException(
                    "Could not retrieve page version. Page Id: " + pageId, e,
                    module);
        }

        return versions;
    }

    /**
     * Returns a specific page version.
     * 
     * @param pageId
     * @param versionId
     * @return
     */
    public static CmsPageVersion find(String pageId, String versionId) {
        return CmsDataObject.findFirst(
                UtilMisc.toMap("pageId", pageId, "pageVersionId", versionId),
                CmsPageVersion.class);
    }

    public String getComment() {
        return entity.getString("versionComment");
    }

    public void setComment(String comment) {
        entity.setString("versionComment", comment);
    }
    
    public String getCreatedBy() {
        return entity.getString("createdBy");
    }
    
    public String getCreatedByName(){
        try {
            return CmsUtil.getPersonDisplayName(getDelegator(), entity.getString("createdBy"));
        } catch (GenericEntityException e) {
            throw new CmsException(
                "Could not retrieve user for pageVersion " + entity.getString("pageVersionId"), e,
                module);
        }  
    }

    public void setCreatedBy(String createdBy) {
        entity.setString("createdBy", createdBy);
    }

    public CmsPage getPage() {
        return page;
    }

    public void setPage(CmsPage page) {
        this.page = page;
        this.entity.put("pageId", page.getId());
    }

}
