<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl("findAddressMatch") text=uiLabelMap.PageTitleFindMatches class="+${styles.action_nav!} ${styles.action_find!}" />
  </@menu>
</#macro>
<@section id="address-match-map" title=uiLabelMap.PageTitleCreateAddressMatchMap menuContent=menuContent>
  <@section id="addressmatchmap_create">
  <form name="addaddrmap" method="post" action="<@pageUrl>createAddressMatchMap</@pageUrl>">
    <@field type="input" name="mapKey" label=uiLabelMap.PartyAddressMatchKey />
    <@field type="input" name="mapValue" label=uiLabelMap.PartyAddressMatchValue />
    <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequence value="0" size=5/>
    <@field type="submit" submitType="link" href="javascript:document.addaddrmap.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreate />
  </form>
  </@section>
  
  <@section id="addressmatchmap_import">
  <form name="importaddrmap" method="post" enctype="multipart/form-data" action="<@pageUrl>importAddressMatchMapCsv</@pageUrl>">
    <@field type="file" name="uploadedFile" label="CSV ${rawLabel('CommonDocument')}" size=14 />
    
    <#assign progressOptions = {
        "formSel" : "form[name=importaddrmap]",
        "progBarId" : "importaddrmap_progress_bar",
        "progTextBoxId" : "importaddrmap_prog_textbox",
        
        "msgContainerParentSel" : "#addressmatchmap_import_content",
        "msgContainerInsertMode" : "prepend",
        
        "expectedResultContainerSel" : "#main-content",
        "errorResultContainerSel" : "#main-${styles.alert_wrap!}",
        "errorResultAddWrapper" : false,

        "successRedirectUrl" : "${makePageUrl('addressMatchMap')}"
    }>
    <@field type="submitarea" progressOptions=progressOptions>
      <input type="submit" value="${uiLabelMap.CommonUpload} CSV" class="${styles.link_run_sys!} ${styles.action_import!}"/>    
    </@field>
  </form>
  </@section>
</@section>
<@section title=uiLabelMap.PageTitleAddressMatches>
      <#if addressMatchMaps?has_content>
        <@table type="data-list" autoAltRows=true>
          <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.PartyAddressMatchKey}</@th>
            <@th>=></@th>
            <@th>${uiLabelMap.PartyAddressMatchValue}</@th>
            <@th>${uiLabelMap.CommonSequence}</@th>
            <@th class="button-col"><a href="<@pageUrl>clearAddressMatchMap</@pageUrl>">${uiLabelMap.CommonClear} ${uiLabelMap.CommonAll}</a></@th>
          </@tr>
          </@thead>
          <@tbody>
          <#list addressMatchMaps as map>
            <@tr>
              <@td>${map.mapKey}</@td>
              <@td>=></@td>
              <@td>${map.mapValue}</@td>
              <@td>${map.sequenceNum!}</@td>
              <@td class="button-col">
                <form name="removeAddressMatchMap_${map_index}" method="post" action="<@pageUrl>removeAddressMatchMap</@pageUrl>">
                  <input type="hidden" name="mapKey" value="${map.mapKey}" />
                  <input type="hidden" name="mapValue" value="${map.mapValue}" />
                  <input type="submit" value="${uiLabelMap.CommonDelete}" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
                </form>
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord"/>
      </#if>
</@section>
