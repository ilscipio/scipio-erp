<#--
Cato: Local order template common defs
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#-- TODO: Formalize and integrate in htmlForm - but use the shop version instead -->
<#macro invertedField type="generic" postfixSize="" widgetPostfixColumns="" postfixContent="" postfix=false inlineArgs...>
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
  <#if !postfixSize?has_content>
    <#local postfixSize = 3>
  </#if>
  <#--<#if !widgetPostfixColumns?has_content>
    <#local widgetPostfixColumns = 11>
  </#if>-->
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <@field type=type inverted=true args=inlineArgs widgetAreaClass="+${styles.text_right!}" postfix=postfix postfixContent=postfixContent widgetPostfixColumns=widgetPostfixColumns postfixSize=postfixSize><#nested></@field>
</#macro>

