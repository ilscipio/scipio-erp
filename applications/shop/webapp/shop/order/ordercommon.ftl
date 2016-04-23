<#--
Cato: Local order template common defs
-->
<#-- This may include more generic files as well, as long as careful about double includes
<#include "....ftl">
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#macro invertedField type="generic" labelContent="" postfixContent="" postfixSize=3 inlineArgs...>
<#--
  <#local gridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixSize":postfixSize, "widgetPostfixCombined":false})>
  <@row>
    <@cell class=addClassArg(gridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("catoLastFieldInfo").id)!"">
    <@cell class=gridStyles.widgetArea>
      ${labelContent}
    </@cell>  
    <@cell class=gridStyles.postfixArea>
      ${postfixContent}
    </@cell>  
  </@row>
-->
  <@field type=type inverted=true args=inlineArgs widgetAreaClass="+${styles.text_right!}" labelContent=labelContent postfixContent=postfixContent postfixSize=postfixSize><#nested></@field>
</#macro>

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
          <@menuitem type="link" href="javascript:submitForm(document.${formName?js_string}, 'CS', '');" class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
        </#if>
        </@menu>
      </@cell>
      <@cell columns=6 class="+${styles.text_right!}">
      <#local mainButtons><#nested></#local>
      <#if mainButtons?has_content>
        ${mainButtons}
      <#else>
        <@menu type="button">
          <#if !text?has_content>
            <#local text = uiLabelMap.CommonContinue>
          </#if>
          <#local class = "+${styles.action_run_session!} ${styles.action_continue!}">
          <#if directLinks>
            <#local href = "javascript:document.${formName?js_string}.submit();">
          <#else>
            <#local href = "javascript:submitForm(document.${formName?js_string}, 'DN', '');">
          </#if>
          <@menuitem type="link" href=href class=class text=text />
        </@menu>
      </#if>
      </@cell>
    </@row>
</#macro>


