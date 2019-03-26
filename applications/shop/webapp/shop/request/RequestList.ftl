<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/common/common.ftl">

<#assign shoppingCart = getShoppingCart()!>
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if shoppingCart?? && shoppingCart.items()?has_content>
            <@menuitem type="link" href=makePageUrl("createCustRequestFromCart") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.OrderCreateCustRequestFromCart />
        </#if>
        <@menuitem type="link" href=makePageUrl("NewCustRequest") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.EcommerceNewCustRequest />
    </@menu>
</#macro>
<@section title=uiLabelMap.EcommerceRequestHistory menuContent=menuContent>
    <#if requestList?has_content>
        <@table type="data-list">
            <@thead>
                <@tr>
                    <@th width="5%">${uiLabelMap.OrderRequest} ${uiLabelMap.CommonNbr}</@th>
                    <@th width="10%">${uiLabelMap.CommonType}</@th>
                    <@th width="20%">${uiLabelMap.CommonName}</@th>
                    <@th width="30%">${uiLabelMap.CommonDescription}</@th>
                    <@th width="10%">${uiLabelMap.CommonStatus}</@th>
                    <@th width="10%">${uiLabelMap.OrderRequestDate}</@th>
                    <#-- <@th width="10%">${uiLabelMap.OrderRequestCreatedDate}</@th> -->
                    <@th width="10%">${uiLabelMap.OrderRequestLastModifiedDate}</@th>
                    <@th width="5%">&nbsp;</@th>
                </@tr>
            </@thead>
            <#list requestList as custRequest>
                <#assign status = custRequest.getRelatedOne("StatusItem", true)>
                <#assign type = custRequest.getRelatedOne("CustRequestType", true)>
                <@tr>
                    <@td>${custRequest.custRequestId}</@td>
                    <@td>${type.get("description",locale)!}</@td>
                    <@td>${custRequest.custRequestName!}</@td>
                    <@td>${custRequest.description!}</@td>
                    <@td>${status.get("description",locale)}</@td>
                    <@td><@formattedDateTime date=custRequest.custRequestDate! defaultVal="0000-00-00 00:00:00"/></@td>
                    <#-- <@td><@formattedDateTime date=custRequest.createdDate! defaultVal="0000-00-00 00:00:00"/></@td> -->
                    <@td><@formattedDateTime date=custRequest.lastModifiedDate! defaultVal="0000-00-00 00:00:00"/></@td>
                    <@td align="right">
                        <a href="<@pageUrl>ViewRequest?custRequestId=${custRequest.custRequestId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                    </@td>
                </@tr>
            </#list>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoRequestFound}</@commonMsg>
    </#if>
</@section>
