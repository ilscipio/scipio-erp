<#--
SCIPIO: Local order template common defs
-->

<#-- SCIPIO: local macro where cells of label and widget areas are inverted and tweaked 
    NOTE: the labelContent bypasses the regular @field parent-child field relation; set markup with labelContentFieldsType-->
<#macro checkoutInvField type="generic" labelContentFieldsType="default-compact" postfixColumns="" labelContent="" labelContentArgs={} widgetAreaClass="" labelColumns="" postfixContent="" postfix=false inlineArgs...>
<#--
  <#local gridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixColumns":postfixColumns, "widgetPostfixCombined":false})>
  <@row>
    <@cell class=addClassArg(gridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("scipioLastFieldInfo").id)!"">
    <@cell class=gridStyles.widgetArea>
      ${labelContent}
    </@cell>  
    <@cell class=gridStyles.postfixArea>
      ${postfixContent}
    </@cell>  
  </@row>
-->
  <#if !postfixColumns?has_content>
    <#local postfixColumns = 3>
  </#if>
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <#local widgetAreaClass = addClassArg(widgetAreaClass, styles.text_right!)>
  <#if labelContent?has_content || labelContent?is_directive>
    <#local labelContentArgs = {"labelContentFieldsType":labelContentFieldsType, "labelContent":labelContent, "labelContentArgs":labelContentArgs}>
    <#local labelContent = checkoutInvFieldLabelRender>
  </#if>
  <@field type=type inverted=true args=inlineArgs widgetAreaClass=widgetAreaClass postfix=postfix
    labelContent=labelContent labelContentArgs=labelContentArgs
    postfixContent=postfixContent labelColumns=labelColumns postfixColumns=postfixColumns><#nested></@field>
</#macro>

<#-- this is an ugly kludge needed due to only having one #nested in freemarker -->
<#macro checkoutInvFieldLabelRender args={}>
  <@fields type=args.labelContentFieldsType ignoreParentField=true>
    <@contentArgRender content=args.labelContent args=args.labelContentArgs />
  </@fields>
</#macro>

