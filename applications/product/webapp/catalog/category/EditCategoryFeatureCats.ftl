<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<form method="post" action="<@pageUrl>attachProductFeaturesToCategory</@pageUrl>" name="attachProductFeaturesToCategory">
    <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
</form>

<@menu type="button">
  <@menuitem type="link" href="javascript:document.attachProductFeaturesToCategory.submit()" text=uiLabelMap.ProductFeatureCategoryAttach class="+${styles.action_run_sys!} ${styles.action_update!}"/>
</@menu>

<#if productCategoryId?? && productCategory??>
    <@section title=uiLabelMap.PageTitleEditCategoryFeatureCategories>
      <@fields type="default-manual">
        <#if productFeatureCatGrpAppls?has_content>
            <#-- Feature Groups -->
            <@table type="data-list" autoAltRows=true>
              <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductFeatureGroup}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th align="center">${uiLabelMap.CommonThru}</@th>
                    <@th>&nbsp;</@th>
                </@tr>
                </@thead>
                <#assign line = 0>
                <#assign rowClass = "2">
                <#list productFeatureCatGrpAppls as productFeatureCatGrpAppl>
                <#assign line = line + 1>
                <#assign productFeatureGroup = (productFeatureCatGrpAppl.getRelatedOne("ProductFeatureGroup", false))?default(null)>
                <@tr valign="middle">
                    <@td><a href="<@pageUrl>EditFeatureGroupAppls?productFeatureGroupId=${(productFeatureCatGrpAppl.productFeatureGroupId)!}</@pageUrl>" class="${styles.link_nav_info_desc!}"><#if productFeatureGroup??>${(productFeatureGroup.description)!}</#if> [${(productFeatureCatGrpAppl.productFeatureGroupId)!}]</a></@td>
                    <#assign hasntStarted = false>
                    <#if (productFeatureCatGrpAppl.getTimestamp("fromDate"))?? && nowTimestamp.before(productFeatureCatGrpAppl.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
                    <@td><div<#if hasntStarted> class="${styles.text_color_alert!}"</#if>>${(productFeatureCatGrpAppl.fromDate?date?string.short)!}</div></@td>
                    <@td align="center">
                        <form method="post" action="<@pageUrl>updateProductFeatureCatGrpAppl</@pageUrl>" name="lineFormGrp${line}">
                            <#assign hasExpired = false>
                            <#if (productFeatureCatGrpAppl.getTimestamp("thruDate"))?? && nowTimestamp.after(productFeatureCatGrpAppl.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
                            <input type="hidden" name="productCategoryId" value="${(productFeatureCatGrpAppl.productCategoryId)!}" />
                            <input type="hidden" name="productFeatureGroupId" value="${(productFeatureCatGrpAppl.productFeatureGroupId)!}" />
                            <input type="hidden" name="fromDate" value="${(productFeatureCatGrpAppl.fromDate)!}" />
                            <#if hasExpired><#assign class="alert"></#if>
                            <@field type="datetime" name="thruDate" value=((productFeatureCatGrpAppl.thruDate)!) class=class!'' size="22" maxlength="25" id="fromDate1" />
                            <@field type="submit" value=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" />
                        </form>
                    </@td>
                    <@td align="center">
                        <@field type="submit" submitType="link" href="javascript:document.removeProductFeatureCatGrpApplForm_${productFeatureCatGrpAppl_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}" text=uiLabelMap.CommonDelete />
                        <form method="post" action="<@pageUrl>removeProductFeatureCatGrpAppl</@pageUrl>" name="removeProductFeatureCatGrpApplForm_${productFeatureCatGrpAppl_index}">
                            <input type="hidden" name="productFeatureGroupId" value="${(productFeatureCatGrpAppl.productFeatureGroupId)!}" />
                            <input type="hidden" name="productCategoryId" value="${(productFeatureCatGrpAppl.productCategoryId)!}" />
                            <input type="hidden" name="fromDate" value="${(productFeatureCatGrpAppl.fromDate)!}" />
                        </form>
                    </@td>
                </@tr>
                </#list>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
      </@fields>
    </@section>
    
  <#if productFeatureGroups?has_content>
    <@section title=uiLabelMap.ProductApplyFeatureGroupFromCategory>
        <form method="post" action="<@pageUrl>createProductFeatureCatGrpAppl</@pageUrl>" name="addNewGroupForm">
          <@fields type="default-nolabelarea">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <@field type="select" name="productFeatureGroupId">
                <#list productFeatureGroups as productFeatureGroup>
                    <option value="${(productFeatureGroup.productFeatureGroupId)!}">${(productFeatureGroup.description)!} [${(productFeatureGroup.productFeatureGroupId)!}]</option>
                </#list>
            </@field>
            <@field type="datetime" name="fromDate" value="" size="25" maxlength="30" id="fromDate2"/>
            <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
          </@fields>
        </form> 
    </@section>
  </#if>
  
    <@section title=uiLabelMap.ProductApplyFeatureGroupFromCategory>
      <@fields type="default-manual">
        <#if productFeatureCategoryAppls?has_content>
            <#-- Feature Categories -->
            <@table type="data-list" autoAltRows=true>
              <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductFeature}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th align="center">${uiLabelMap.CommonThru}</@th>
                    <@th>&nbsp;</@th>
                </@tr>
               </@thead> 
                <#assign line = 0>
                <#list productFeatureCategoryAppls as productFeatureCategoryAppl>
                <#assign line = line + 1>
                <#assign productFeatureCategory = (productFeatureCategoryAppl.getRelatedOne("ProductFeatureCategory", false))?default(null)>
                <@tr valign="middle">
                    <@td><a href="<@pageUrl>EditFeatureCategoryFeatures?productFeatureCategoryId=${(productFeatureCategoryAppl.productFeatureCategoryId)!}</@pageUrl>" class="${styles.link_nav_info_desc!}"><#if productFeatureCategory??>${(productFeatureCategory.description)!}</#if> [${(productFeatureCategoryAppl.productFeatureCategoryId)!}]</a></@td>
                    <#assign hasntStarted = false>
                    <#if (productFeatureCategoryAppl.getTimestamp("fromDate"))?? && nowTimestamp.before(productFeatureCategoryAppl.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
                    <#assign cellClass><#if hasntStarted>+${styles.text_color_alert!}</#if></#assign>
                    <@td class=cellClass>${(productFeatureCategoryAppl.fromDate?date?string.short)!}</@td>
                    <@td align="center">
                        <form method="post" action="<@pageUrl>updateProductFeatureCategoryAppl</@pageUrl>" name="lineForm${line}">
                            <#assign hasExpired = false>
                            <#if (productFeatureCategoryAppl.getTimestamp("thruDate"))?? && nowTimestamp.after(productFeatureCategoryAppl.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
                            <input type="hidden" name="productCategoryId" value="${(productFeatureCategoryAppl.productCategoryId)!}" />
                            <input type="hidden" name="productFeatureCategoryId" value="${(productFeatureCategoryAppl.productFeatureCategoryId)!}" />
                            <input type="hidden" name="fromDate" value="${(productFeatureCategoryAppl.fromDate)!}" />
                            <#if hasExpired><#assign class="alert"></#if>
                            <@field type="datetime" name="thruDate" value=((productFeatureCategoryAppl.thruDate)!) class=class!'' size="25" maxlength="30" id="thruDate2" />
                            <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" />
                        </form>
                    </@td>
                    <@td align="center">
                        <@field type="submit" submitType="link" href="javascript:document.removeProductFeatureCategoryApplForm_${productFeatureCategoryAppl_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}" text=uiLabelMap.CommonDelete />
                        <form method="post" action="<@pageUrl>removeProductFeatureCategoryAppl</@pageUrl>" name="removeProductFeatureCategoryApplForm_${productFeatureCategoryAppl_index}">
                            <input type="hidden" name="productFeatureCategoryId" value="${(productFeatureCategoryAppl.productFeatureCategoryId)!}" />
                            <input type="hidden" name="productCategoryId" value="${(productFeatureCategoryAppl.productCategoryId)!}" />
                            <input type="hidden" name="fromDate" value="${(productFeatureCategoryAppl.fromDate)!}" />
                        </form>
                    </@td>
                </@tr>
                </#list>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
      </@fields>
    </@section>
</#if>