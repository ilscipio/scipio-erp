<#--
Cato: Local order template common defs
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#assign defaultFieldGridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixSize":2, "widgetPostfixCombined":false})>
<#macro invertedField type="" labelContent="" actionContent="">
  <@row>
    <@cell class=addClassArg(defaultFieldGridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("catoLastFieldInfo").id)!"">
    <@cell class=defaultFieldGridStyles.widgetArea>
      ${labelContent}
    </@cell>  
    <@cell class=defaultFieldGridStyles.postfixArea>
      ${actionContent}
    </@cell>  
  </@row>
</#macro>

