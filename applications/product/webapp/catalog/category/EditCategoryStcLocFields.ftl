<#-- SCIPIO: New easy-to-use form for category simple text content localized fields  -->

<#include "component://product/webapp/catalog/catalog/catalogcommon.ftl">

    <#if stcErrorMsg?has_content>
        <@alert type="error">${stcErrorMsg}</@alert>
    </#if>

    <form action="<@ofbizUrl>updateProductCategoryContentStcLocFields</@ofbizUrl>" method="post" id="updatePccStdLocFieldsForm" name="updatePccStdLocFieldsForm">
        <input type="hidden" name="updatePccStcLocFields" value="Y"/>
        <input type="hidden" name="productCategoryId" value="${productCategoryId!}"/>
        
        <@catalogStcLocFields objectType="category" values=(categoryStcViewsByType!{}) params=parameters />
        
        <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
    </form>

    
