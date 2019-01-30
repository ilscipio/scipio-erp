<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.EcommerceRequestHistory>
    <#if requestList?has_content>
        <@table type="data-list">
            <@tr>
                <@td width="10%"><span style="white-space: nowrap;">${uiLabelMap.OrderRequest} ${uiLabelMap.CommonNbr}</span>
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="10%"><span style="white-space: nowrap;">${uiLabelMap.CommonType}</span>
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="20%">${uiLabelMap.CommonName}
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="40%">${uiLabelMap.CommonDescription}
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="10%">${uiLabelMap.CommonStatus}
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="20%">
                    <div>${uiLabelMap.OrderRequestDate}</div>
                    <div>${uiLabelMap.OrderRequestCreatedDate}</div>
                    <div>${uiLabelMap.OrderRequestLastModifiedDate}</div>
                </@td>
                <@td width="10">&nbsp;</@td>
                <@td width="10">&nbsp;</@td>
            </@tr>
            <#list requestList as custRequest>
                <#assign status = custRequest.getRelatedOne("StatusItem", true)>
                <#assign type = custRequest.getRelatedOne("CustRequestType", true)>
                
                <@tr>
                    <@td>${custRequest.custRequestId}
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td>${type.get("description",locale)!}
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td>${custRequest.custRequestName!}
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td>${custRequest.description!}
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td>${status.get("description",locale)}
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td>
                        <div><span style="white-space: nowrap;">${custRequest.custRequestDate!}</span></div>
                        <div><span style="white-space: nowrap;">${custRequest.createdDate!}</span></div>
                        <div><span style="white-space: nowrap;">${custRequest.lastModifiedDate!}</span></div>
                    </@td>
                    <@td width="10">&nbsp;</@td>
                    <@td align="right">
                        <a href="<@pageUrl>ViewRequest?custRequestId=${custRequest.custRequestId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                    </@td>
                    <@td width="10">&nbsp;</@td>
                </@tr>
            </#list>
        </@table>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.OrderNoRequestFound}</@commonMsg>
    </#if>
</@section>
