<#-- *****************************-->
<#-- * ADVANCED SHOP TEMPLATE * -->
<#-- *****************************-->
<#-- This is a faithful recreation of the ShopDecorator. The Screen will adopt to the currently selected theme and process the 
screen information according to the ShopDecorator.
NOTE: Theme selection is also influenced by the WebSite entity settings of the website being rendered (they should be made to agree).
NOTE: Use of "@render type="ftl" or "interpretStd(Loc)" prevents variable clashes between the templates and ensures
    behavior compatible with widget renderer, which is what the themes expect, thus maintaining theme compatibility.
NOTE: The parts below are duplicated from commonTemplateParts.ftl. They can also be reused without duplication.
-->
<@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/><#t/>
<@commonTmpl.headerHeadOpen />

<#-- DEFAULT CONTENT -->

<#global useDefaultLeftContent = useDefaultLeftContent!true>
<#macro defaultLeftContent>
  <#-- INSERT DEFAULT LEFT COLUMN CONTENT HERE (and set boolean above as needed) -->
  <@asset def="global" name="CategoryMenu" />
  <#--<@menu type="sidebar">
    <@menuitem type="link" disabled=true text="[INSERT DEFAULT LEFT SIDEBAR CONTENT HERE]"/>
  </@menu>-->
  <#--<@asset def="global" name="MySideBarMenu" />-->
</#macro>
<#global defaultLeftContent = defaultLeftContent>

<#global useDefaultRightContent = useDefaultRightContent!false>
<#macro defaultRightContent>
  <#-- INSERT DEFAULT RIGHT COLUMN CONTENT HERE (and set boolean above as needed) -->
  <#--<@asset def="global" name="MyPromotionInfo" />-->
</#macro>
<#global defaultRightContent = defaultRightContent>

<#macro mainContentBody>
  <#-- INSERT MAIN COLUMN CONTENT HERE -->
  <@asset name="mainContent"/>
</#macro>
<#global mainContentBody = mainContentBody>

<#-- Get display logic and content (includes the above globals) -->
<#global displayInfo = commonTmpl.getColumnDisplayInfo()>

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

<#macro mainColumn class>
  <@container class=class id="main-content">
    <#-- PRE-BODY -->
    <#-- MESSAGES -->
    <@render resource=messagesTemplateLocation!/>

    <#-- BODY -->
    <#if headerTitle?has_content><@heading level=1>${headerTitle}</@heading></#if>
    
    <@mainContentBody/>
  </@container>
</#macro>

<#-- Main Structure -->
<@container class="${styles.grid_row!} ${styles.grid_theme_pre!}" id="pre-content-section"><#-- FIXME: the row has no cells -->
  ${preContent!}
</@container>
<@container class="+${styles.grid_theme!}" id="content-main-section">
  <#if displayInfo.customSideBar>
    <div id="content-main-body"><#-- FIXME: this makes a div in between row and cell -->
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
</@container>

<#-- *************-->
<#-- * Footer *-->
<#-- *************-->
<@commonTmpl.footerIncludes />
