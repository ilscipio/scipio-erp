
<#include "component://setup/webapp/setup/common/common.ftl">
<#include "component://product/webapp/catalog/catalog/tree/treecommon.ftl">

<#-- TODO? -->

<div style="display:none;">
<#macro setupDeleteProductForm id isDeleteRecord>
  <@form id=id action=makeOfbizUrl("setupDeleteProduct") method="post">
      <@defaultWizardFormFields exclude=["prodCatalogId", "productStoreId"]/>
      <@ectCommonTreeFormFields params={}/>
      <@field type="hidden" name="setupContinue" value="N"/>
      <@field type="hidden" name="isDeleteProduct" value="Y"/><#-- for our screens -->
      <@field type="hidden" name="deleteProduct" value=isDeleteRecord?string("true", "false")/><#-- for service -->

      <@field type="hidden" name="productCategoryId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="prodCatalogId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="productStoreId" value="" class="+ect-inputfield"/>
      <@field type="hidden" name="fromDate" value="" class="+ect-inputfield"/>
  </@form>
</#macro>
  <#-- TODO?
  <@setupDeleteProductForm id="ect-removeproduct-form" isDeleteRecord=true/>
  <@setupDeleteProductForm id="ect-removeproductassoc-form" isDeleteRecord=false/>
   -->
</div>
