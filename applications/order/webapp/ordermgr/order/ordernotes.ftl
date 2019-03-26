<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if orderHeader?has_content>

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("ORDERMGR", "_NOTE", request)>
      <@menuitem type="link" href=makePageUrl("createnewnote?${raw(paramString)}") text=uiLabelMap.OrderNotesCreateNew class="+${styles.action_nav!} ${styles.action_add!}" />
    </#if>
    </@menu>
  </#macro>
  <@section title=uiLabelMap.OrderNotes menuContent=menuContent>
 
  <#if orderNotes?has_content>
    <@table type="fields">
      <#list orderNotes as note>
        <@tr>
          <@td valign="top" width="35%">
            <#if note.noteParty?has_content>
              <div>&nbsp;${uiLabelMap.CommonBy}&nbsp;${Static["org.ofbiz.party.party.PartyHelper"].getPartyName(delegator, note.noteParty, true)}</div>
            </#if>
              <div>&nbsp;${uiLabelMap.CommonAt}&nbsp;<#if note.noteDateTime?has_content><@formattedDateTime date=note.noteDateTime /></#if></div>
          </@td>
          <@td valign="top" width="50%">
            ${note.noteInfo?replace("\n", "<br/>")}
          </@td>
          <@td align="right" valign="top" width="15%">
            <#if (note.internalNote!) == "N">
                ${uiLabelMap.OrderPrintableNote}
                <form name="privateNotesForm_${note_index}" method="post" action="<@pageUrl>updateOrderNote</@pageUrl>">
                  <input type="hidden" name="orderId" value="${orderId}"/>
                  <input type="hidden" name="noteId" value="${note.noteId}"/>
                  <input type="hidden" name="internalNote" value="Y"/>
                  <a href="javascript:document.privateNotesForm_${note_index}.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.OrderNotesPrivate}</a>
                </form>
            </#if>
            <#if (note.internalNote!) == "Y">
                ${uiLabelMap.OrderNotPrintableNote}
                <form name="publicNotesForm_${note_index}" method="post" action="<@pageUrl>updateOrderNote</@pageUrl>">
                  <input type="hidden" name="orderId" value="${orderId}"/>
                  <input type="hidden" name="noteId" value="${note.noteId}"/>
                  <input type="hidden" name="internalNote" value="N"/>
                  <a href="javascript:document.publicNotesForm_${note_index}.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.OrderNotesPublic}</a>
                </form>
            </#if>
          </@td>
        </@tr>
        <#if note_has_next>
          <@tr type="util"><@td colspan="3"><hr/></@td></@tr>
        </#if>
      </#list>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.OrderNoNotes}.</@commonMsg>
  </#if>

  </@section>
</#if>
