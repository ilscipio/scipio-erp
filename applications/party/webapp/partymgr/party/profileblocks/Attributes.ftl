<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("PARTYMGR", "_CREATE", request)>
      <@menuitem type="link" href=makeOfbizUrl("editPartyAttribute?partyId=${party.partyId!}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}"/>
    </#if>
    </@menu>
  </#macro>
  <@section id="partyAttributes" title=uiLabelMap.PartyAttributes menuContent=menuContent>
      <#if attributes?has_content>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.CommonName}</@th>
              <@th>${uiLabelMap.CommonValue}</@th>
              <@th>&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
          <#list attributes as attr>
            <@tr>
              <@td>
                ${attr.attrName!}
              </@td>
              <@td>
                ${attr.attrValue!}
              </@td>
              <@td class="button-col">
                <a href="<@ofbizUrl>editPartyAttribute?partyId=${partyId!}&attrName=${attr.attrName!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoPartyAttributesFound}</@commonMsg>
      </#if>
  </@section>