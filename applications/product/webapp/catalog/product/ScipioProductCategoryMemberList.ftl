<@section>
    <#if productCategoryMemberList?has_content>  
         <#list productCategoryMemberList as productCategoryMember>
            <form name="removeProductFromCategory_${productCategoryMember_index}" method="post" action="<@pageUrl>removeProductFromCategory</@pageUrl>">
                  <input name="productId" type="hidden" value="${productCategoryMember.productId}"/>
                  <input name="productCategoryId" type="hidden" value="${productCategoryMember.productCategoryId}"/>
                  <input name="fromDate" type="hidden" value="${productCategoryMember.fromDate}"/>
            </form>
        </#list>
        <form id="UpdateProductCategoryMember" name="UpdateProductCategoryMember" method="post" action="<@pageUrl>updateProductToCategory</@pageUrl>">            
            <input name="_useRowSubmit" type="hidden" value="Y"/>
            <input name="productId" type="hidden" value="${parameters.productId}"/>
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true>
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductId}</@th>
                        <@th>${uiLabelMap.ProductCategoryId}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.ProductSequenceNum}</@th>
                        <@th>${uiLabelMap.ProductQuantity}</@th>
                        <@th>${uiLabelMap.ProductComments}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list productCategoryMemberList as productCategoryMember>
                        <#assign productCategory = (productCategoryMember.getRelatedOne("ProductCategory", true))!>
                        <@tr>
                            <@td>
                                 ${productCategoryMember.productId}
                            </@td>
                            <@td>
                                  <a href="<@pageUrl>EditCategory?productCategoryId=${productCategoryMember.productCategoryId}</@pageUrl>" class="${styles.link_nav_info_name_long}">
                                      ${productCategoryMember.productCategoryId} 
                                  </a>
                            </@td>
                            <@td>${productCategoryMember.fromDate?string("yyyy-MM-dd")}</@td>
                            <@td><#if productCategoryMember.thruDate?has_content>${productCategoryMember.thruDate?string("yyyy-MM-dd")}</#if></@td>
                            <@td>
                                <@field type="input" name="sequenceNum_o_${productCategoryMember_index}" value=((productCategoryMember.sequenceNum)!) size=20 maxlength=40 />
                            </@td>
                            <@td>
                                <@field type="input" name="quantity_o_${productCategoryMember_index}" value=((productCategoryMember.quantity)!) size=20 maxlength=40 />
                            </@td>
                            <@td>
                                <@field type="input" name="comments_o_${productCategoryMember_index}" value=((productCategoryMember.comments)!) size=20 maxlength=40 />
                            </@td>
                            <@td>
                                <input name="productId_o_${productCategoryMember_index}" type="hidden" value="${productCategoryMember.productId}"/>
                                <input name="productCategoryId_o_${productCategoryMember_index}" type="hidden" value="${productCategoryMember.productCategoryId}"/>                                
                                <input name="fromDate_o_${productCategoryMember_index}" type="hidden" value="${productCategoryMember.fromDate}"/>  
                                <input id="_rowSubmit_o_${productCategoryMember_index}" name="_rowSubmit_o_${productCategoryMember_index}" type="hidden" value="N"/>
                                <@field type="submit" submitType="link" href="javascript:document.forms['UpdateProductCategoryMember'].elements['_rowSubmit_o_${productCategoryMember_index}'].value = 'Y';document.forms.UpdateProductCategoryMember.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>
                            </@td>
                            <@td>
                                <a href="javascript:document.removeProductFromCategory_${productCategoryMember_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
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