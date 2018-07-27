package com.ilscipio.scipio.cms.menu;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.data.CmsDataObject;

/**
 * CMS Menu Object
 */
public class CmsMenu extends CmsDataObject {
    
    private static final long serialVersionUID = -714021324721469544L;
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());    
    
    protected CmsMenu(GenericValue entity) {
        super(entity);
    }
    
    protected CmsMenu(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }

    public String getWebsiteId() {
        return getEntityWebsiteId();
    }
    
    public String getEntityWebsiteId() {
        return this.entity.getString("websiteId");
    }
    
    void setEntityWebsiteId(String websiteId) {
        this.entity.setString("websiteId", websiteId);
    }
    
    public String getMenuName() {
        return getEntityMenuName();
    }
    
    public String getEntityMenuName() {
        return this.entity.getString("menuName");
    }
    
    void EntityMenuName(String menuName) {
        this.entity.setString("menuName", menuName);
    }
    
    public String getMenuDescr() {
        return getEntityMenuDescr();
    }
    
    public String getEntityMenuDescr() {
        return this.entity.getString("description");
    }
    
    void EntityMenuDescr(String description) {
        this.entity.setString("description", description);
    }
    
    public String getMenuJson() {
        return getEntityMenuJson();
    }
    
    public String getEntityMenuJson() {
        return this.entity.getString("menuJson");
    }
    
    void EntityMenuJson(String menuJson) {
        this.entity.setString("menuJson", menuJson);
    }

    @Override
    public DataObjectWorker<?> getWorkerInst() {
        // TODO Auto-generated method stub
        return null;
    }
    

}
