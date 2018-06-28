<#-- *****************************-->
<#-- * ADVANCED TEMPLATE * -->
<#-- *****************************-->
<#-- This is a faithful recreation of the GlobalDecorator. The Screen will adopt to the currently selected theme and process the 
screen information according to the GlobalDecorator. 
NOTE: Theme selection is also influenced by the WebSite entity settings of the website being rendered (they should be made to agree).
NOTE: Use of "@render type="ftl" or "interpretStd(Loc)" prevents variable clashes between the templates and ensures
    behavior compatible with widget renderer, which is what the themes expect, thus maintaining theme compatibility.
NOTE: The parts below are duplicated from commonTemplateParts.ftl. They can also be reused without duplication.
-->
<@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/><#t/>
<@commonTmpl.headerHeadOpen />

<#-- DEFAULT CONTENT -->

<#-- FIXME?: Due to Freemarker limitations, we must use interpretStd for left-column; #macro will not work.
    Usage of variables inside may be tricky and you may have to use setContextField and the context map to pass them;
    multiple solutions exist; but variables in FTL binding may not be visible. It is recommended to define a separate asset. -->
<#global useDefaultLeftContent = useDefaultLeftContent!true>
<#global defaultLeftContent><#noparse><#-- delay evaluation -->
  <#-- INSERT DEFAULT LEFT COLUMN CONTENT HERE (and set boolean above as needed) -->
  <@menu type="sidebar">
    <@menuitem type="link" disabled=true text="[INSERT DEFAULT LEFT SIDEBAR CONTENT HERE]"/>
  </@menu>
  <#--<@asset def="global" name="MySideBarMenu" />-->
</#noparse></#global>
<#global defaultLeftContent = interpretStd({"body":defaultLeftContent, "model":"hybrid"})><#-- hybrid allows both <@xxx/> and xxx?string syntax -->

<#global useDefaultRightContent = useDefaultRightContent!false>
<#global defaultRightContent><#noparse>
  <#-- INSERT DEFAULT RIGHT COLUMN CONTENT HERE (and set boolean above as needed) -->
  <#--<@asset def="global" name="MyPromotionInfo" />-->
</#noparse></#global>
<#global defaultRightContent = interpretStd({"body":defaultRightContent, "model":"hybrid"})>

<#macro mainContentBody>
  <#-- INSERT MAIN COLUMN CONTENT HERE -->
  <@asset name="mainContent"/>
</#macro>
<#global mainContentBody = mainContentBody>

<#-- Get display logic and content (includes the above globals) -->
<#global displayInfo = commonTmpl.getColumnDisplayInfo()>

<#-- SPECIAL: for the global template, some themes render the left-column themselves,
    so we must make it available to them, using an emulated "sections" renderer object -->
<@varSection ctxVars={"sections": makeSectionsRenderer("ftl", {
    "left-column" : displayInfo.leftContentEff!, "right-column" : displayInfo.rightContentEff!
})}>

<#-- **********-->
<#-- * HEADER *-->
<#-- **********-->
<@commonTmpl.headerIncludes />

<#-- *************-->
<#-- * MAIN BODY *-->
<#-- *************-->

<#-- IMPLEMENTATION -->

<#-- Column implementations -->
<#macro leftColumn class>
  <@container class=class id="left-sidebar">
    <@container class="+sidebar"><#-- NOTE: same hardcode as GlobalDecorator -->
      <#local leftCnt = displayInfo.leftContent!>
      <#if leftCnt?is_directive><@leftCnt/><#else>${leftCnt}</#if>
    </@container>
  </@container>
</#macro>

<#macro rightColumn class>
  <@container class=class id="right-sidebar">
    <@container class="+sidebar"><#-- NOTE: same hardcode as GlobalDecorator -->
      <#local rightCnt = displayInfo.rightContent!>
      <#if rightCnt?is_directive><@rightCnt/><#else>${rightCnt}</#if>
    </@container>
  </@container>
</#macro>

<#macro mainColumn class noTitle=false>
  <@container class=class id="main-content">
    <#-- PRE-BODY -->
    <#-- MESSAGES -->
    <@render resource=messagesTemplateLocation!/>

    <#-- BODY -->
    <#if !noTitle && headerTitle?has_content><@heading level=1>${headerTitle}</@heading></#if>
    
    <@mainContentBody/>
  </@container>
</#macro>

<#-- Main Structure -->
${preContent!}
<@container class="+${styles.grid_theme!}" id="content-main-section">
<#if userLogin?has_content>
  <#if displayInfo.customSideBar>
    <div id="content-main-body">
      <@mainColumn class=(styles.grid_sidebar_0_main!)/>
    </div>
  <#else>
    <#if displayInfo.showLeft>
      <#if displayInfo.showRight>
        <@leftColumn class=(styles.grid_sidebar_2_side!)/>
        <@mainColumn class=(styles.grid_sidebar_2_main!)/>
        <@rightColumn class=(styles.grid_sidebar_2_side!)/>
      <#else>
        <@leftColumn class=(styles.grid_sidebar_1_side!)/>
        <@mainColumn class=(styles.grid_sidebar_1_main!)/>
      </#if>
    <#else>
      <#if displayInfo.showRight>
        <@mainColumn class=(styles.grid_sidebar_1_main!)/>
        <@rightColumn class=(styles.grid_sidebar_1_side!)/>
      <#else>
        <div id="content-main-body"><#-- FIXME: this makes a div in between row and cell -->
          <@mainColumn class=(styles.grid_sidebar_0_main!)/>
        </div>
      </#if>
    </#if>
  </#if>
<#else>
  <div id="content-main-body"><#-- FIXME: this makes a div in between row and cell -->
    <@mainColumn class="${styles.grid_large}12" noTitle=true />
  </div>
</#if>
</@container>

<#-- *************-->
<#-- * Footer *-->
<#-- *************-->
<@commonTmpl.footerIncludes />

</@varSection>

