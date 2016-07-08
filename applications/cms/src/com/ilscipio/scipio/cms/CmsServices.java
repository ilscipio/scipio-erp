package com.ilscipio.scipio.cms;

import java.util.Map;

import javolution.util.FastMap;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.template.CmsAssetTemplate;


public class CmsServices {
    
    public static final String module = CmsServices.class.getName();
    
    
    /**
     * Service to update and add a new groovy file to a page template
     * @param groovyAssocId
     * @param inputPosition
     * @param pageTemplateId
     * @param isActive
     *
     * @param templateName
     * @param templateLocation
     * @param createdBy
     * @param lastUpdatedBy
     * */
    public static Map<String, Object> updateAssetTemplate(DispatchContext ctx, Map<String, ? extends Object> globalContext) {
        // create copy of map, so we can add items without creating side effects
        Map<String, Object> context = UtilMisc.makeMapWritable(globalContext);
        GenericDelegator delegator = (GenericDelegator) ctx.getDelegator(); 
        
        Map<String, Object> response = ServiceUtil.returnSuccess();
        GenericValue userLogin = null;
        try {
            userLogin = (GenericValue) context.get("userLogin");
            if (userLogin == null) {
                userLogin = delegator.findByPrimaryKey( "UserLogin",
                        UtilMisc.toMap("userLoginId", "system") );
            }
            
            //check if you want update the actual thang
            if(context.get("assetTemplateId") != null){
                
                // Partos unos : Copy original template
                CmsAssetTemplate originalTemplate = new CmsAssetTemplate((String) context.get("assetTemplateId"));
                // Update the original template so it is inactive;
                //originalTemplate.setInactive(true);
                //originalTemplate.store();
                // Paritas dos tacos: Update original template and set to inactive (assetTemplateId)
                CmsAssetTemplate newTemplate = (CmsAssetTemplate) originalTemplate.copy();
                if(context.get("templateName") != null){
                    newTemplate.setName((String) context.get("templateName"));
                }                
                if(context.get("templateBody") != null){
                    newTemplate.setBody((String) context.get("templateBody"));
                }
                
                newTemplate.setCreatedBy((String) userLogin.get("userLoginId"));

                //Partunoertes trois! (haha, this amuses me) 
                //make sure this is active (in case we are restoring an old version ;) )
                newTemplate.setInactive(true);
                newTemplate.store();
                response.put("newAssetTemplateId", newTemplate.getId());
            }else{
                //ToDo: Check for templateName being unique (though we may not want to do that)
                GenericValue gv = delegator.makeValue("CmsAssetTemplate",EntityUtil.makeFields("templateName",context.get("templateName"),"templateBody",context.get("templateBody")));
                CmsAssetTemplate newTemplate = new CmsAssetTemplate(gv);
            }
            

            
        }catch(Exception e){
            response.putAll(ServiceUtil.returnError("Error while updating Groovy Template Assoc"));
            e.printStackTrace();
        }
        return response;
    }
    
    /**
     * Service to update and add a new groovy file to a page template
     * @param groovyAssocId
     * @param inputPosition
     * @param pageTemplateId
     * @param isActive
     *
     * @param templateName
     * @param templateLocation
     * @param createdBy
     * @param lastUpdatedBy
     * */
    public static Map<String, Object> updatePageTemplateGroovy(DispatchContext ctx, Map<String, ? extends Object> globalContext) {
        // create copy of map, so we can add items without creating side effects
        Map<String, Object> context = UtilMisc.makeMapWritable(globalContext);
        GenericDelegator delegator = (GenericDelegator) ctx.getDelegator(); 
        Map<String, Object> response = FastMap.newInstance();
        String[] groovyAssocAttributeList = {"groovyTemplateId","inputPosition","pageTemplateId","lastUpdatedBy"};
        String[] groovyAttributeList = {"templateName","templateLocation","templateBody"};
        
        GenericValue userLogin = null;
        try {
            userLogin = (GenericValue) context.get("userLogin");
            if (userLogin == null) {
                userLogin = delegator.findByPrimaryKey( "UserLogin",
                        UtilMisc.toMap("userLoginId", "system") );
            }
            
            // Partos unos : Adding data to CmsGroovyTemplate
            
            Map<String, Object> groovyMap = UtilMisc.<String, Object>toMap("createdBy", userLogin.get("userLoginId"));
            for (int i = 0; i < groovyAttributeList.length; i++) {
                if (context.get(groovyAttributeList[i]) != null && !context.get(groovyAttributeList[i]).equals("")) {
                    groovyMap.put(groovyAttributeList[i], context.get(groovyAttributeList[i]));
                }
            }
            
            //Don't ask. It is a stupid workaround because of the way the screens are setup. 
            // Going to shoot somebody at some point
            if(context.get("groovyTemplateBody")!=null){
                groovyMap.put("templateBody",context.get("groovyTemplateBody"));
            }
            
            if(context.get("groovyTemplateName")!=null){
                groovyMap.put("templateName",context.get("groovyTemplateName"));
            }

            GenericValue groovy = delegator.makeValue("CmsGroovyTemplate",groovyMap);
            delegator.createSetNextSeqId(groovy);
 
            String groovyTemplateId = (String) groovy.get("groovyTemplateId");
            
            // Parte deux : Adding data to CmsPageTemplateGroovyAssoc
            
            Map<String, Object> groovyAssocMap = new FastMap<String, Object>();
            // update defined attributes
            for (int i = 0; i < groovyAssocAttributeList.length; i++) {
                    if (context.get(groovyAssocAttributeList[i]) != null && !context.get(groovyAssocAttributeList[i]).equals("")) {
                        groovyAssocMap.put(groovyAssocAttributeList[i], context.get(groovyAssocAttributeList[i]));
                    }
            }
            
            groovyAssocMap.put("groovyTemplateId", groovyTemplateId);
            groovyAssocMap.put("lastUpdatedBy", userLogin.get("userLoginId"));
            
            GenericValue groovyAssoc = null;
            
            if(context.get("groovyAssocId")!= null){
                //Find existing Generic Value
                groovyAssoc = delegator.findOne("CmsPageTemplateGroovyAssoc", false,  UtilMisc.toMap( "groovyAssocId", context.get("groovyAssocId")));
                groovyAssoc.putAll(groovyAssocMap);
            }else{
                groovyAssoc = delegator.makeValue("CmsPageTemplateGroovyAssoc",groovyAssocMap);
                delegator.createSetNextSeqId(groovyAssoc);
            } 

            delegator.createOrStore(groovyAssoc);

            
        }catch(Exception e){
            response.putAll(ServiceUtil.returnError("Error while updating Groovy Template Assoc"));
            e.printStackTrace();
        }
        response.putAll(ServiceUtil.returnSuccess());
        return response;
    }
}
