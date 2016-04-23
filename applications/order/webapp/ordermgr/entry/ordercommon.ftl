<#--
Cato: Local order template common defs
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#-- TODO: Formalize and integrate in htmlForm - but use the shop version instead -->
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

