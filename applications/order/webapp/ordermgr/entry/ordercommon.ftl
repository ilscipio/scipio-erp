<#--
Cato: Local order template common defs
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#macro invertedField type="" labelContent="" postfixContent="" postfixSize=3>
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
</#macro>

