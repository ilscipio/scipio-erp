<#include "component://setup/webapp/setup/common/common.ftl">

<#-- SCIPIO: DEPRECATED in favor of catalog tree
 -->
<#if productStoreCatalogList?has_content>

<@table type="data-list">
  <@thead>
    <@th width="30%">${uiLabelMap.ProductCatalog}</@th>
    <@th width="20%">${uiLabelMap.CommonFrom}</@th>
    <@th width="20%">${uiLabelMap.CommonThru}</@th>
    <@th width="10%">${uiLabelMap.ProductSequenceNum}</@th>
    <@th>${uiLabelMap.CommonActions}</@th>
  </@thead>
  <@tbody>
    <#list productStoreCatalogList as pscat>
      <@tr>
        <#assign cat = pscat.getRelatedOne("ProdCatalog", false)>
        <@td><@setupExtAppLink uri="/catalog/control/EditProdCatalog?prodCatalogId=${rawString(cat.prodCatalogId)}"><#t/>
                <#if cat.catalogName?has_content>${cat.catalogName} [${cat.prodCatalogId}]<#else>${cat.prodCatalogId}</#if><#t/>
             </@setupExtAppLink>
        </@td>
        <@td>${pscat.fromDate!}</@td>
        <@td>${pscat.thruDate!}</@td>
        <@td>${pscat.sequenceNum!}<#if pscat?is_first> (${uiLabelMap.CommonDefault})</#if></@td>
        <@td><#-- class="+${styles.text_right!}" -->
            <a href="javascript:jQuery('#setupCatalog-editCatalog-${escapeVal(pscat.prodCatalogId, 'js-html')}').submit();void(0);"<#rt/>
              <#lt/> class="${styles.link_nav} ${styles.action_update}">${uiLabelMap.CommonEdit}</a>
            <a href="javascript:jQuery('#setupCatalog-deleteCatalog-${escapeVal(pscat.prodCatalogId, 'js-html')}').submit();void(0);"<#rt/>
              <#lt/> class="${styles.link_nav} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
        </@td>
      </@tr>
    </#list>
  </@tbody>
</@table>

<#list productStoreCatalogList as pscat>
  <@form method="get" action=makeOfbizUrl("setupCatalog") id=("setupCatalog-editCatalog-"+rawString(pscat.prodCatalogId))>
    <@defaultWizardFormFields exclude=["prodCatalogId"]/>
    <@field type="hidden" name="setupContinue" value="N"/>
    <@field type="hidden" name="prodCatalogId" value=pscat.prodCatalogId/>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupDeleteCatalog") id=("setupCatalog-deleteCatalog-"+rawString(pscat.prodCatalogId))>
    <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId"]/>
    <@field type="hidden" name="setupContinue" value="N"/>
    <@field type="hidden" name="isDeleteCatalog" value="Y"/>
    
    <@field type="hidden" name="prodCatalogId" value=pscat.prodCatalogId/>
    <@field type="hidden" name="productStoreId" value=pscat.productStoreId/>
    <@field type="hidden" name="fromDate" value=pscat.fromDate/>
  </@form>
</#list>

</#if>
