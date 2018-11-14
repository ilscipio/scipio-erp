<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("addressMatchMap") text=uiLabelMap.PageTitleAddressMatchMap class="+${styles.action_nav!}" />
  </@menu>
</#macro>
<@section id="address-match-map" menuContent=menuContent>

      <form name="matchform" method="post" action="<@ofbizUrl>findAddressMatch?match=true</@ofbizUrl>">
        <@field type="input" name="lastName" label=uiLabelMap.PartyLastName value=(parameters.lastName!) required=true />
        <@field type="input" name="firstName" label=uiLabelMap.PartyFirstName value=(parameters.firstName!) required=true />
        <@field type="input" name="address1" label=uiLabelMap.CommonAddress1 value=(parameters.address1!) required=true />
        <@field type="input" name="address2" label=uiLabelMap.CommonAddress2 value=(parameters.address2!) />
        <@field type="input" name="city" label=uiLabelMap.CommonCity value=(parameters.city!) required=true />
      
        <@field type="select" label=uiLabelMap.CommonStateProvince name="stateProvinceGeoId" currentValue="${(currentStateGeo.geoId)!}">
            <#if currentStateGeo?has_content>
              <option value="${currentStateGeo.geoId}">${currentStateGeo.geoName!currentStateGeo.geoId}</option>
              <option value="${currentStateGeo.geoId}">---</option>
            </#if>
            <option value="ANY">${uiLabelMap.CommonAnyStateProvince}</option>
            <@render resource="component://common/widget/CommonScreens.xml#states" ctxVars={"statesPreselect":false} />
        </@field>
        
        <@field type="input" name="postalCode" label=uiLabelMap.PartyZipCode value=(parameters.postalCode!) required=true />
      
        <@field type="submit" text=uiLabelMap.PageTitleFindMatches class="+${styles.link_run_sys!} ${styles.action_find!}" />
        
      </form>
      <@script>
          jQuery("form[name=matchform]").validate();
      </@script>
      
      
          <#if match?has_content>
            <#if matches?has_content>
              <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                <@thead>
                <@tr>
                  <@td colspan="7">${uiLabelMap.PartyAddressMatching} ${lastName} / ${firstName} @ ${addressString}</@td>
                </@tr>
                <@tr class="header-row">
                  <@th>${uiLabelMap.PartyLastName}</@th>
                  <@th>${uiLabelMap.PartyFirstName}</@th>
                  <@th>${uiLabelMap.CommonAddress1}</@th>
                  <@th>${uiLabelMap.CommonAddress2}</@th>
                  <@th>${uiLabelMap.CommonCity}</@th>
                  <@th>${uiLabelMap.PartyZipCode}</@th>
                  <@th>${uiLabelMap.PartyPartyId}</@th>
                </@tr>
                </@thead>
                <@tbody>
                <#list matches as match>
                  <#assign person = match.getRelatedOne("Party", false).getRelatedOne("Person", false)!>
                  <#assign group = match.getRelatedOne("Party", false).getRelatedOne("PartyGroup", false)!>
                  <@tr>
                    <#if person?has_content>
                      <@td>${person.lastName}</@td>
                      <@td>${person.firstName}</@td>
                    <#elseif group?has_content>
                      <@td colspan="2">${group.groupName}</@td>
                    <#else>
                      <@td colspan="2">${uiLabelMap.PartyUnknown}</@td>
                    </#if>
                    <@td>${Static["org.ofbiz.party.party.PartyWorker"].makeMatchingString(delegator, match.address1)}</@td>
                    <@td>${Static["org.ofbiz.party.party.PartyWorker"].makeMatchingString(delegator, match.address2!(uiLabelMap.CommonNA))}</@td>
                    <@td>${match.city}</@td>
                    <@td>${match.postalCode}</@td>
                    <@td class="button-col"><a href="<@ofbizUrl>viewprofile?partyId=${match.partyId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${match.partyId}</a></@td>
                  </@tr>
                </#list>
                </@tbody>
              </@table>
            <#else>
              <@commonMsg type="result-norecord">${uiLabelMap.PartyNoMatch}</@commonMsg>
            </#if>
          </#if>
</@section>