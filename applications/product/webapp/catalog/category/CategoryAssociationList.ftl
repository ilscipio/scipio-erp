<@section>
    <#if productCategoryRollupList?has_content>  
         <#list productCategoryRollupList as productCategoryRollup>
            <form name="removeproductCategoryRollup_${productCategoryRollup_index}" method="post" action="<@ofbizUrl>removeProductCategoryFromCategory</@ofbizUrl>">                  
                  <input name="productCategoryId" type="hidden" value="${productCategoryRollup.productCategoryId}"/>
                  <input name="originalProductCategoryId" type="hidden" value="${originalProductCategoryId}"/>
                  <input name="parentProductCategoryId" type="hidden" value="${productCategoryRollup.parentProductCategoryId}"/>
                  <input name="fromDate" type="hidden" value="${productCategoryRollup.fromDate}"/>
            </form>
        </#list>
        <form id="UpdateCategoryAssociation" name="UpdateCategoryAssociation" method="post" action="<@ofbizUrl>updateProductCategoryToCategory</@ofbizUrl>">            
            <input name="originalProductCategoryId" type="hidden" value="${originalProductCategoryId!}"/>
            <input name="_useRowSubmit" type="hidden" value="Y"/>
          
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductCategory}</@th>                    
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.CommonSequence}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list productCategoryRollupList as productCategoryRollup>
                        <#if productCategoryAssociationMode == "child">
                            <#assign productCategory = (productCategoryRollup.getRelatedOne("CurrentProductCategory", false))!>
                        <#elseif productCategoryAssociationMode == "parent">
                            <#assign productCategory = (productCategoryRollup.getRelatedOne("ParentProductCategory", false))!>                            
                        </#if>
                        <@tr>
                            <@td>
                                  <input name="originalProductCategoryId_o_${productCategoryRollup_index}" type="hidden" value="${originalProductCategoryId!}"/>
                                  <input name="productCategoryId_o_${productCategoryRollup_index}" type="hidden" value="${productCategoryRollup.productCategoryId}"/>
                                  <input name="parentProductCategoryId_o_${productCategoryRollup_index}" type="hidden" value="${productCategoryRollup.parentProductCategoryId}"/>
                                  <input name="fromDate_o_${productCategoryRollup_index}" type="hidden" value="${productCategoryRollup.fromDate}"/>                                  
                                  <input id="_rowSubmit_o_${productCategoryRollup_index}" name="_rowSubmit_o_${productCategoryRollup_index}" type="hidden" value="N"/>
                                  <a href="<@ofbizUrl>EditCategory?productCategoryId=${productCategory.productCategoryId}</@ofbizUrl>" class="${styles.link_nav_info_name_long}">
                                      ${((productCategory.categoryName)!)} [${productCategory.productCategoryId}] 
                                  </a>
                            </@td>
                            <@td>${productCategoryRollup.fromDate?string("yyyy-MM-dd")}</@td>
                            <@td>
                                <@field type="datetime" name="thruDate_o_${productCategoryRollup_index}" value="" size="25" maxlength="30" value=((productCategoryRollup.thruDate)!)/>    
                            </@td>
                            <@td>
                                <@field type="input" name="sequenceNum_o_${productCategoryRollup_index}" value=((productCategoryRollup.sequenceNum)!) size=20 maxlength=40 />
                            </@td>
                            <@td>                    
                               <@field type="submit" submitType="link" href="javascript:document.forms['UpdateCategoryAssociation'].elements['_rowSubmit_o_${productCategoryRollup_index}'].value = 'Y';document.forms.UpdateCategoryAssociation.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>                               
                            </@td>
                            <@td>
                                <a href="javascript:document.forms.removeproductCategoryRollup_${productCategoryRollup_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
                            </@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </form>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>
</@section>