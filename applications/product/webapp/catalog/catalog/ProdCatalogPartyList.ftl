<@section>
    <#if prodCatalogRoleList?has_content>  
         <#list prodCatalogRoleList as prodCatalogRole>
            <form name="removeProdCatalogFromParty_${prodCatalogRole_index}" method="post" action="<@ofbizUrl>removeProdCatalogFromParty</@ofbizUrl>">
                <input name="prodCatalogId" type="hidden" value="${prodCatalogRole.prodCatalogId}"/>
                <input name="partyId" type="hidden" value="${prodCatalogRole.partyId}"/>
                <input name="fromDate" type="hidden" value="${prodCatalogRole.fromDate}"/>
                <input name="roleTypeId" type="hidden" value="${prodCatalogRole.roleTypeId}"/>
            </form>
        </#list>      
        <form id="UpdateProdCatalogToParty" name="UpdateProdCatalogToParty" method="post" action="<@ofbizUrl>updateProdCatalogToParty</@ofbizUrl>">
            <input name="prodCatalogId" type="hidden" value="${parameters.prodCatalogId}"/>
            <input name="_useRowSubmit" type="hidden" value="Y"/>
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.CommonParty}</@th>
                        <@th>${uiLabelMap.PartyRole}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.CommonSequenceNum}</@th>
                        <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th>
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list prodCatalogRoleList as prodCatalogRole>
                        <#assign partyName = delegator.findOne("PartyNameView", { "partyId" : prodCatalogRole.partyId }, false) />
                        <#assign roleType = (prodCatalogRole.getRelatedOne("RoleType", true))!>
                        <@tr>
                            <@td>
                                  <input name="prodCatalogId_o_${prodCatalogRole_index}" type="hidden" value="${parameters.prodCatalogId}"/>
                                  <input name="partyId_o_${prodCatalogRole_index}" type="hidden" value="${prodCatalogRole.partyId}"/>
                                  <input name="roleTypeId_o_${prodCatalogRole_index}" type="hidden" value="${prodCatalogRole.roleTypeId}"/>
                                  <input name="fromDate_o_${prodCatalogRole_index}" type="hidden" value="${prodCatalogRole.fromDate}"/>
                                  <input id="_rowSubmit_o_${prodCatalogRole_index}" name="_rowSubmit_o_${prodCatalogRole_index}" type="hidden" value="N"/>
                                  <a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?party_id=${prodCatalogRole.partyId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_name_long}">
                                    ${partyName.groupName!} ${partyName.personalTitle!} ${partyName.firstName!} ${partyName.middleName!} ${partyName.lastName!}
                                  </a>            
                            </@td>
                            <@td>${roleType.description}</@td>
                            <@td>${prodCatalogRole.fromDate?string('yyyy-MM-dd')}</@td>
                            <@td>
                                <@field type="datetime" name="thruDate_o_${prodCatalogRole_index}" value=(prodCatalogRole.thruDate!) size=13 />
                            </@td>
                            <@td>
                                <@field type="input" name="sequenceNum_o_${prodCatalogRole_index}" value=(prodCatalogRole.sequenceNum!) size=10 maxlength=20 />
                            </@td>
                            <@td>
                                <@field type="submit" submitType="link" href="javascript:document.forms['UpdateProdCatalogToParty'].elements['_rowSubmit_o_${prodCatalogRole_index}'].value = 'Y';document.UpdateProdCatalogToParty.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/>                                                                                           
                            </@td>
                            <@td>
                                <a href="javascript:document.removeProdCatalogFromParty_${prodCatalogRole_index}.submit();" class="${styles.link_run_sys} ${styles.action_remove}">${uiLabelMap.CommonDelete}</a>
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