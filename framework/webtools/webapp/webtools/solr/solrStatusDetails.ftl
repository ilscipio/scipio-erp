

<#macro prettyResult testExpr trueVal falseVal>
    <#if testExpr>
      <span class="${styles.text_color_success!} solrwebstatusres-true"><b>${trueVal}</b></span>
    <#else>
      <span class="${styles.text_color_warning!} solrwebstatusres-false"><b>${falseVal}</b></span>
    </#if>
</#macro>

<@section title=uiLabelMap.SolrSolrSystemStatus>
  <#if solrStatus?has_content>
    <@table type="data-complex" autoAltRows=true>
      <@thead>
        <@tr>
          <@th width="20%">${uiLabelMap.CommonItem}</@th>
          <@th width="40%">${uiLabelMap.CommonResult}</@th>
          <@th>${uiLabelMap.CommonMessages}</@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list toSimpleMap(solrStatus) as statusName, statusInfo>
          <@tr>
            <@td><#if statusInfo.label??>${getLabel(statusInfo.label)!}<#else>${statusName}</#if></@td>
            <@td>
                <#if statusInfo.status??>
                  <#if statusInfo.status?is_boolean>
                    <@prettyResult testExpr=statusInfo.status trueVal="TRUE" falseVal="FALSE"/>
                  <#else>
                    ${statusInfo.status?string}
                  </#if>
                <#else>
                  -
                </#if>
            </@td>
            <@td>
              <#if statusInfo.msg??>${statusInfo.msg}<#if statusInfo.errMsg??><br/></#if></#if>
              <#if statusInfo.errMsg??><span class="${styles.text_color_error!}"><strong>${uiLabelMap.CommonError}</strong>: ${statusInfo.errMsg}</span></#if>
              <#if statusInfo.warnMsg??><span class="${styles.text_color_warning!}"><strong>${uiLabelMap.CommonWarning}</strong>: ${statusInfo.warnMsg}</span></#if>
            </@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>
  <#else>
    <@commonMsg type="error">${uiLabelMap.CommonUnexpectedError} (missing solrWebappStatus)</@commonMsg>
  </#if>
</@section>

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="generic">
            <@modal id="rebuildSolrIndex" label="${rawLabel('CommonUpdate')}: rebuildSolrIndex" linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_begin!}">
                <@heading>rebuildSolrIndex</@heading>
                <@render resource=rebuildIndexCoreWidgetLoc/>
            </@modal>
        </@menuitem>
        <@menuitem type="generic">
            <@modal id="markSolrDataDirty" label="${rawLabel('CommonUpdate')}: markSolrDataDirty" linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_begin!}">
                <@heading>markSolrDataDirty</@heading>
                <@render resource=markSolrDataDirtyWidgetLoc/>
            </@modal>
        </@menuitem>
    </@menu>
</#macro>
<@section title=uiLabelMap.SolrSolrDataStatus menuContent=menuContent>
  <#if solrDataStatus?has_content>
    <@table type="data-complex" autoAltRows=true>
      <@thead>
        <@tr>
          <@th width="20%">${uiLabelMap.CommonItem}</@th>
          <@th width="40%">${uiLabelMap.CommonValue}</@th>
          <@th>${uiLabelMap.CommonMessages}</@th>
        </@tr>
      </@thead>
      <@tbody>
          <@tr>
            <@td>Data Status</@td>
            <@td>
              <#assign solrDataOk = ("SOLR_DATA_OK" == rawString(solrDataStatus.dataStatusId!))>
              <@prettyResult testExpr=solrDataOk trueVal=(solrDataStatus.dataStatusId!) falseVal=(solrDataStatus.dataStatusId!)/>
            </@td>
            <@td>
              <#if !solrDataOk>
                ${getLabel('SolrDataStatusDirtyInfo', '', {})!}
              </#if>
            </@td>
          </@tr>
          <@tr>
            <@td>Data Config Version</@td>
            <@td>
              <#assign configVerOk = (rawString(solrDataStatus.dataCfgVersion!) == rawString(solrConfigVersion!))>
              <@prettyResult testExpr=configVerOk trueVal=(solrDataStatus.dataCfgVersion!) falseVal=(solrDataStatus.dataCfgVersion!) />
            </@td>
            <@td>
              <#if !configVerOk>
                ${getLabel('SolrDataConfigVersionDirtyInfo', '', {"solrConfigVersion":solrConfigVersion!})!}
              </#if>
            </@td>
          </@tr>
      </@tbody>
    </@table>
    <p class="solrstatlist-statdesc">
      <small>
        ${uiLabelMap.SolrDataStatusInfo} ${uiLabelMap.SolrRebuildIndexStartupInfo}
      </small>
    </p>
  <#else>
    <@commonMsg type="error">${uiLabelMap.SolrCouldNotGetSolrStatus}</@commonMsg>
  </#if>
</@section>

<@section title=uiLabelMap.SolrSolrConfig>
  <#if solrConfigs?has_content>
    <@table type="data-complex" autoAltRows=true>
      <@thead>
        <@tr>
          <@th width="20%">${uiLabelMap.CommonItem}</@th>
          <@th>${uiLabelMap.CommonValue}</@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list toSimpleMap(solrConfigs) as configName, configEntry>
          <@tr>
            <@td><#if configEntry.label??>${getLabel(configEntry.label)}<#else>${configEntry.title!configEntry.name!}</#if>
              <#if configEntry.propName??> <em>(${configEntry.propName})</em></#if></@td>
            <@td>${configEntry.value!}</@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>
  <#else>
    <@commonMsg type="error">${uiLabelMap.CommonUnexpectedError} (missing solrConfig)</@commonMsg>
  </#if>
</@section>

