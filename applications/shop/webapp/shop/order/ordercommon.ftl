<#--
SCIPIO: Local order template common defs
-->
<#include "../common/common.ftl">

<#macro checkoutActionsMenu text="" formName="" directLinks=true>
    <#if !formName?has_content>
      <#if directLinks>
        <#local formName = parameters.formNameValue!"">
      <#else>
        <#local formName = "checkoutInfoForm">
      </#if>
    </#if>
    <@row>
      <@cell columns=6>
        <@menu type="button">
        <#if directLinks>
          <@menuitem type="link" href=makeOfbizUrl("showcart") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
        <#else>
          <@menuitem type="link" href="javascript:submitForm(document['${escapeVal(formName, 'js')}'], 'CS', '');" class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
        </#if>
        </@menu>
      </@cell>
      <@cell columns=6 class="+${styles.text_right!}">
      <#local mainButtons><#nested></#local>
      <#if mainButtons?has_content>
        <@menu type="button">
          <#nested><#-- NOTE: must re-run #nested here -->
        </@menu>
      <#else>
        <@menu type="button">
          <#if !text?has_content>
            <#local text = uiLabelMap.CommonContinue>
          </#if>
          <#local class = "+${styles.action_run_session!} ${styles.action_continue!}">
          <#if directLinks>
            <#local href = "javascript:document['${escapeVal(formName, 'js')}'].submit();">
          <#else>
            <#local href = "javascript:submitForm(document['${escapeVal(formName, 'js')}'], 'DN', '');">
          </#if>
          <@menuitem type="link" href=href class=class text=text />
        </@menu>
      </#if>
      </@cell>
    </@row>
</#macro>


