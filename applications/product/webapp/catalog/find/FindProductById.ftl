<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.ProductFindProductWithIdValue>
        <form name="idsearchform" method="post" action="<@ofbizUrl>FindProductById</@ofbizUrl>">
          <span>${uiLabelMap.CommonId} ${uiLabelMap.CommonValue}:</span> <input type="text" name="idValue" size="20" maxlength="50" value="${idValue!}" />&nbsp;<a href="javascript:document.idsearchform.submit()" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonFind}</a>
        </form>
        
        <p>${uiLabelMap.ProductSearchResultsWithIdValue}: ${idValue!}</p>
        <#if !goodIdentifications?has_content && !idProduct?has_content>
          <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
        <#else>
          <@table type="data-list" autoAltRows=true>
            <#if idProduct?has_content>
            <@tr valign="middle">
                <@td>
                    ${idProduct.productId}
                </@td>
                <@td>&nbsp;&nbsp;</@td>
                <@td>
                    <a href="<@ofbizUrl>ViewProduct?productId=${idProduct.productId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${(idProduct.internalName)!}</a>
                    (${uiLabelMap.ProductSearchResultsFound})
                </@td>
            </@tr>
            </#if>
            <#list goodIdentifications as goodIdentification>
                <#assign product = goodIdentification.getRelatedOne("Product", true)/>
                <#assign goodIdentificationType = goodIdentification.getRelatedOne("GoodIdentificationType", true)/>
                <@tr valign="middle">
                    <@td>
                        ${product.productId}
                    </@td>
                    <@td>&nbsp;&nbsp;</@td>
                    <@td>
                        <a href="<@ofbizUrl>ViewProduct?productId=${product.productId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${(product.internalName)!}</a>
                        (${uiLabelMap.ProductSearchResultsFound} ${goodIdentificationType.get("description",locale)?default(goodIdentification.goodIdentificationTypeId)}.)
                    </@td>
                </@tr>
            </#list>
          </@table>
        </#if>
</@section>
