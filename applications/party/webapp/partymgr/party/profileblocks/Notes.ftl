<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("PARTYMGR", "_NOTE", request)>
      <@menuitem type="link" href=makePageUrl("AddPartyNote?partyId=${partyId}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}" />
    </#if>
    </@menu>
  </#macro>
  <@section id="partyNotes" title=uiLabelMap.CommonNotes menuContent=menuContent>
      <#if notes?has_content>
        <@table type="data-complex" autoAltRows=false>
        <@tbody>
          <#list notes as noteRef>
            <@tr>
              <@td style="min-width:10em;">
                <div><strong>${uiLabelMap.FormFieldTitle_noteName}: </strong>${noteRef.noteName!}</div>
                <#if noteRef.noteParty?has_content>
                  <div><strong>${uiLabelMap.CommonBy}: </strong>${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, noteRef.noteParty, true)}</div>
                </#if>
                <div><strong>${uiLabelMap.CommonAt}: </strong>${noteRef.noteDateTime.toString()}</div>
              </@td>
              <@td>
                ${noteRef.noteInfo}
              </@td>
            </@tr>
            <#if noteRef_has_next>
              <@tr type="util"><@td colspan="2"><hr/></@td></@tr>
            </#if>
          </#list>
        </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoNotesForParty}</@commonMsg>
      </#if>
  </@section>
