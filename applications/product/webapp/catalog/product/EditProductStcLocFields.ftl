<#-- SCIPIO: New easy-to-use form for product simple text content localized fields  -->

<#include "component://product/webapp/catalog/common/common.ftl">

    <#if stcErrorMsg?has_content>
        <@alert type="error">${stcErrorMsg}</@alert>
    </#if>

    <form action="<@pageUrl>updateProductContentStcLocFields</@pageUrl>" method="post" id="updatePcStcLocFieldsForm" name="updatePcStcLocFieldsForm">
        <input type="hidden" name="updatePcStcLocFields" value="Y"/>
        <input type="hidden" name="productId" value="${productId!}"/>
        
        <@cataloglib.catalogStcLocFields objectType="product" values=(productStcViewsByType!{}) params=(prodStclfParams!parameters) />
        
        <@field type="submit" name="Update" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}"/>
    </form>
    