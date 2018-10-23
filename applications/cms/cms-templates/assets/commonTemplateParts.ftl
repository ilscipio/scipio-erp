<#-- Common template parts
  These replicate the stock CMS advanced template parts using convenient macros.
  WARN: 2017-04-06: these are in review and subject to change
  
  May be imported using:
    <@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/>
  or included using:
    <@asset def="global" mode="include" name="CommonTemplateParts"/>
    
  NOTE: if you use the import version, any variables or directives that are recognized
      by the macros in this file should be (re)defined in the global namespace
      using #global.
      
  See demoTemplate.ftl for example usage.
      
  DEV NOTE: the presence of these macros adds no maintenance overhead because the demoTemplate.ftl 
      would have had to duplicate the advanced template markup anyway.
-->
<#compress>

<#macro headerHeadOpen docType=true>
<#if docType>
<!DOCTYPE html><#rt/>
</#if>
  <@render resource="component://common/webcommon/includes/htmlHeadOpen.ftl"/><#lt/>
  <#-- TODO: REVIEW: why not just call @htmlHeadOpen? --><#t/>
</#macro>

<#macro headerIncludes pageOpen=false docType=true>
  <#if pageOpen>
    <@headerHeadOpen docType=docType/><#lt/>
  </#if>
  <@render resource=headerTemplateLocation!/>
  <@render resource=appbarOpenTemplateLocation!/>
  <@render resource=appbarTemplateLocation!/>
  <@render resource=appheaderTemplate!/>
  <@render resource=appbarCloseTemplateLocation!/>
  <@render resource="component://common/webcommon/includes/commonHeadScripts.ftl"/>
</#macro>

<#macro footerIncludes pageClose=true>
  <#-- TODO: honor pageClose -->
  <@render resource="component://common/webcommon/includes/commonFootScripts.ftl"/>
  <@render resource=applicationFooterTemplate!/>
  <@render resource=footerTemplateLocation!/>
</#macro>

<#function getColumnDisplayInfo>
  <#if columnDisplayLogic??>
    <#return columnDisplayLogic>
  </#if>
  <#-- NOTE: Here the ?? operator should be used on the content variables; ?has_content may not work as expected
    and is usually not appropriate - the content objects are rendered on the fly (through ?string and output/coercion) and 
    are not guaranteed to be or behave as strings/scalars (?string may be used). 
    See demoTemplate.ftl for more info. -->
  <#local showLeft = (leftContent?? || (useDefaultLeftContent!false)) && ((showLeftContent!true) == true && (widePage!false) == false)>
  <#local showRight = (rightContent?? || (useDefaultRightContent!false)) && ((showRightContent!true) == true && (widePage!false) == false)>
  <#if leftContent??>
    <#local leftCnt = leftContent>
  <#elseif (useDefaultLeftContent!false)>
    <#local leftCnt = defaultLeftContent>
  <#else>
    <#local leftCnt = "">
  </#if>
  <#if showLeft>
    <#local leftCntEff = leftCnt>
  <#else>
    <#local leftCntEff = "">
  </#if>
  <#if rightContent??>
    <#local rightCnt = rightContent>
  <#elseif (useDefaultRightContent!false)>
    <#local rightCnt = defaultRightContent>
  <#else>
    <#local rightCnt = "">
  </#if>
  <#if showRight>
    <#local rightCntEff = rightCnt>
  <#else>
    <#local rightCntEff = "">
  </#if>
  <#global columnDisplayLogic = {"showLeft":showLeft, "showRight":showRight, 
    "customSideBar":(customSideBar!false), "widePage":(widePage!false),
    "leftContent":leftCnt, "rightContent":rightCnt,
    "leftContentEff":leftCntEff, "rightContentEff":rightCntEff}>
  <#return columnDisplayLogic>
</#function>

<#-- Column implementations -->
<#macro leftColumn class>
  <@container class=class id="left-sidebar">
    <@container class="+sidebar"><#-- NOTE: same hardcode as GlobalDecorator -->
      <#local leftCnt = getColumnDisplayInfo().leftContent!>
      <#if leftCnt?is_directive><@leftCnt/><#else>${leftCnt}</#if>
    </@container>
  </@container>
</#macro>

<#macro rightColumn class>
  <@container class=class id="right-sidebar">
    <@container class="+sidebar"><#-- NOTE: same hardcode as GlobalDecorator -->
      <#local rightCnt = getColumnDisplayInfo().rightContent!>
      <#if rightCnt?is_directive><@rightCnt/><#else>${rightCnt}</#if>
    </@container>
  </@container>
</#macro>

<#macro mainColumn class noTitle=false noMsgs=false>
  <@container class=class id="main-content">
    <#-- PRE-BODY -->
    <#-- MESSAGES -->
    <#if !noMsgs><@render resource=messagesTemplateLocation!/></#if>

    <#-- BODY -->
    <#if !noTitle && headerTitle?has_content><@heading level=1>${headerTitle}</@heading></#if>
    
    <#if mainContentBody??>
      <#if mainContentBody?is_directive><@mainContentBody/><#else>${mainContentBody}</#if>
    </#if>
  </@container>
</#macro>

<#-- Main 3-column structure definition
  Supports the following macros that can be defined in the (global) namespace and will be picked up and invoked:
    defaultLeftContent, defaultRightContent, mainContentBody -->
<#macro mainStruct3Col noTitle=false noMsgs=false usePreContent=true preContentWrapper=true>
    <#if usePreContent && preContent??>
      <#if preContentWrapper>
        <div class="${styles.grid_row!} ${styles.grid_theme_pre!}" id="pre-content-section"><#-- FIXME: the row has no cells -->
      </#if>
          <#if preContent?is_directive><@preContent/><#else>${preContent}</#if>
      <#if preContentWrapper>
        </div>
      </#if>  
    </#if>
    <@container class="+${styles.grid_theme!}" id="content-main-section">
      <#local displayInfo = getColumnDisplayInfo()>
      <#if displayInfo.showLeft>
        <#if displayInfo.showRight>
          <@leftColumn class=(styles.grid_sidebar_2_side!)/>
          <@mainColumn class=(styles.grid_sidebar_2_main!) noTitle=noTitle noMsgs=noMsgs/>
          <@rightColumn class=(styles.grid_sidebar_2_side!)/>
        <#else>
          <@leftColumn class=(styles.grid_sidebar_1_side!)/>
          <@mainColumn class=(styles.grid_sidebar_1_main!) noTitle=noTitle noMsgs=noMsgs/>
        </#if>
      <#else>
        <#if displayInfo.showRight>
          <@mainColumn class=(styles.grid_sidebar_1_main!) noTitle=noTitle noMsgs=noMsgs/>
          <@rightColumn class=(styles.grid_sidebar_1_side!)/>
        <#else>
          <div id="content-main-body"><#-- FIXME: this makes a div in between row and cell -->
            <@mainColumn class=(styles.grid_sidebar_0_main!) noTitle=noTitle noMsgs=noMsgs/>
          </div>
        </#if>
      </#if>
    </@container>
</#macro>

<#macro mainStruct1Col noTitle=false noMsgs=false mainColumnClass=true usePreContent=true>
    <#if usePreContent && preContent??><#if preContent?is_directive><@preContent/><#else>${preContent}</#if></#if>
    <@container class="+${styles.grid_theme!}" id="content-main-section">
      <div id="content-main-body"><#-- FIXME: this makes a div in between row and cell -->
        <#if mainColumnClass?is_boolean>
          <#local mainColumnClass = styles.grid_sidebar_0_main!>
        </#if>
        <@mainColumn class=mainColumnClass noTitle=noTitle noMsgs=noMsgs/>
      </div>
    </@container>
</#macro>

<#macro mainStruct3ColGlobal noTitle=false noMsgs=false usePreContent=true preContentWrapper=false>
  <#if userLogin?has_content>
    <#if (customSideBar!false) == false>
      <@mainStruct3Col noTitle=noTitle noMsgs=noMsgs usePreContent=usePreContent preContentWrapper=preContentWrapper/>
    <#else>
      <@mainStruct1Col noTitle=noTitle noMsgs=noMsgs usePreContent=usePreContent preContentWrapper=preContentWrapper/>
    </#if>
  <#else>
    <@mainStruct1Col noTitle=true noMsgs=false mainColumnClass="${styles.grid_large}12" usePreContent=usePreContent preContentWrapper=preContentWrapper/>
  </#if>
</#macro>

</#compress>