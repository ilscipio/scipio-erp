<#-- Demo page template, dedicated to test cases -->
<@asset def="global" mode="import" name="CommonTemplateParts" ns="commonTmpl"/><#t/>
<@commonTmpl.headerHeadOpen />

<#-- DEFAULT CONTENT -->

<#global useDefaultLeftContent = useDefaultLeftContent!true>
<#macro defaultLeftContent>
  <@menu type="sidebar">
    <@menuitem type="link" disabled=true text="[Demo default left column content]"/>
  </@menu>
</#macro>
<#global defaultLeftContent = defaultLeftContent>

<#global useDefaultRightContent = useDefaultRightContent!true>
<#macro defaultRightContent>
  <#-- INSERT DEFAULT RIGHT COLUMN CONTENT HERE (and set boolean above as needed) -->
  <#--<@asset def="global" name="MyPromotionInfo" />-->
  <span>[Demo default right column content]</span>
  <br/><br/><br/><br/>
  <p>We should promote something here!</p>
</#macro>
<#global defaultRightContent = defaultRightContent>

<#macro mainContentBody>
  <#-- INSERT MAIN COLUMN CONTENT HERE -->
  <@demoTemplateContent/>
</#macro>
<#global mainContentBody = mainContentBody>

<#-- **********-->
<#-- * HEADER *-->
<#-- **********-->
<@commonTmpl.headerIncludes />

<#-- *************-->
<#-- * MAIN BODY *-->
<#-- *************-->

<#-- IMPLEMENTATION -->

<@commonTmpl.mainStruct3Col/>


<#-- DEMO CONTENT AND TESTS -->

<#macro demoTemplateContent>
  <p>This demo template tests page content, CMS macros and functions, in the spirit of 
    <a href="<@ofbizInterWebappUrl uri="/admin/control/WebtoolsLayoutDemo" escapeAs='html'></@>">Layout Demo</a>.</p>

  <@section title="Demo Menu Asset">
    <@asset name="DemoMenu" webSite="cmsSite" def="global"/>
  </@section>

  <#-- NOTE: wrapping section is important to test the @section auto header levels across asset render boundary 
        (was non-working prior to recent renderer refactors) -->
  <@section title="Main content asset (def=\"import\") [Page Content]">
    <@asset name="mainContent" /><#-- def="import" implied -->
  </@section>
  
  <@section title="Extra page attributes">
    <p>demo_bool_1: ${(demo_bool_1!"missing")?string}</p>
    <p>demo_bool_2 (parameters.demo_bool_2 == 'true'): ${(demo_bool_2!"missing")?string}</p>
    <p>demo_integer_1 (is number type? ${(demo_integer_1!false)?is_number?string}): ${demo_integer_1!"missing"}</p>
    <p>demo_long_1 (is number type? ${(demo_long_1!false)?is_number?string}): ${demo_long_1!"missing"}</p>
  </@section>

  <@section title="Demo asset (def=\"global\", with nested assets) [Template-based Test]">
    <#-- NOTE: intentionally doesn't bother with attributes -->
    <#assign demoAssetContent>
      This content was defined in <code>demoTemplate</code> and received by <em>demoAsset</em>!
    </#assign>
    <@asset name="DemoAsset" def="global" ctxVars={"recurseDepth": 4} ovrdCtxVars={"content":demoAssetContent} /> 
  </@section>
  
  <@section title="Demo asset 2 (def=\"import\")">
    <@asset name="demoAsset2" def="import"/> 
  </@section>
  
  <@section title="Demo asset 3 (def=\"global\", with nested assets) [Template-based Test]">
    <@asset name="DemoAsset3" def="global"/> 
  </@section>
  
  <@section title="Secondary content asset (second instance of section asset)">
    <@asset name="secondaryContent" def="import"  ctxVars={"testCtxVar": "testCtxVarValue"}/>
    <p>testCtxVar (this should print "missing"; it tests context scope protection): ${context.testCtxVar!"missing"}</p>
  </@section>
  
  <@section title="Trivial content asset">
    <@asset name="trivialContent" def="import" />
  </@section>
  
  <@section title="Demo Library Asset (include)">
    <@asset name="DemoLibAsset" def="global" mode="include"/>
    <@demoLibAssetMacro1/>
    ${runDemoLibAssetFunction2()}
  </@section>
  
  <@section title="Demo Library Asset (import)">
    <@asset name="DemoLibAsset" def="global" mode="import" ns="demoLibAsset"/>
    <@demoLibAsset.demoLibAssetMacro1/>
    ${demoLibAsset.runDemoLibAssetFunction2()}
  </@section>
  
<#if cmsPageId?has_content><#-- cmsPageId is set to the current page rendering -->
  <@section title="Page links">
      <p><em>NOTE: both @pageUrl and @cmsPageUrl are valid names, but only @cmsPageUrl will
        work to link cms pages from outside of CMS rendering.</em></p>
      <@section title="Current page (by ID)">
          <ul>
            <li><@pageUrl id=cmsPageId /></li>
            <li><@pageUrl id=cmsPageId fullPath=true /></li>
            <li><@cmsPageUrl id=cmsPageId secure=true /></li>
            <li><@pageUrl id=cmsPageId escapeAs='html'/></li>
            <li><@pageUrl id=cmsPageId extLoginKey=true/></li>
            <li><@pageUrl id=cmsPageId escapeAs='html' extLoginKey=true/></li>
            <li><@pageUrl id=cmsPageId escapeAs='js'/></li>
            <li><@pageUrl id=cmsPageId escapeAs='html' extLoginKey=true params="param1=value1&param2=value2"/></li>
            <li><@pageUrl id=cmsPageId extLoginKey=true params="param1=value1&amp;param2=value2"/></li>
            <li><@pageUrl id=cmsPageId escapeAs='html' extLoginKey=true params={"param1":"value1", "param2":"value2"}/></li>
            <li><@pageUrl id=cmsPageId extLoginKey=true params={"param1":"value1", "param2":"value2"}/></li>
          </ul>
          <ul>
            <#assign pageName = cmsPage.getName()!>
            <li>${makePageUrl({"name":pageName, "escapeAs":'html'})}</li>
            <li>${makePageUrl({"id":cmsPageId, "escapeAs":'html'})}</li>
            <li>${makeCmsPageUrl({"id":cmsPageId, "escapeAs":'html', "extLoginKey":true})}</li>
            <li>${makePageUrl({"id":cmsPageId, "escapeAs":'js-html'})}</li>
            <#-- shorthand mode (page name only, very limited usage) -->
            <li>${escapeFullUrl(makePageUrl(pageName), 'html')}</li>
          </ul>
      </@section>
      
      <@section title="Demo page (explicit)">
          <ul>
            <li><@pageUrl name="DemoPage" webSiteId="cmsSite" /></li>
            <li><@pageUrl name="DemoPage" webSiteId="cmsSite" fullPath=true /></li>
            <li><@cmsPageUrl name="DemoPage" webSiteId="cmsSite" secure=true /></li>
            <li><@pageUrl name="DemoPage" webSiteId="cmsSite" escapeAs='html'/></li>
            <li><@pageUrl name="DemoPage" webSiteId="cmsSite" extLoginKey=true/></li>
            <li><@pageUrl name="DemoPage" webSiteId="cmsSite" escapeAs='html' extLoginKey=true/></li>
            <li><@pageUrl name="DemoPage" escapeAs='js'/></li>
            <li><@pageUrl name="DemoPage" escapeAs='html' extLoginKey=true params="param1=value1&param2=value2"/></li>
            <li><@pageUrl name="DemoPage" extLoginKey=true params="param1=value1&amp;param2=value2"/></li>
            <li><@pageUrl name="DemoPage" escapeAs='html' extLoginKey=true params={"param1":"value1", "param2":"value2"}/></li>
            <li><@pageUrl name="DemoPage" extLoginKey=true params={"param1":"value1", "param2":"value2"}/></li>
          </ul>
      </@section>
  </@section>
</#if>

  <#-- removed, DemoMenu instead
  <@section title="Base menu asset">
    <p><em>NOTE: This base menu when used alone is currently mostly only useful for one-time menu definitions and examples.</em></p>
    
    <#- - NOTE: the overriding order is: ovrdCtxVars overrides attribs overrides ctxVars
        compared to regular rendering, the attributes complicate the needs here because
        the attribs parsing complicates some cases - ->
    <#assign menuItems = [
        {"type":"link", "text":"Menu Tab 2", "disabled":true},
        {"type":"link", "text":"Menu Tab 1", "href":"ofbizUrl://WebtoolsLayoutDemo"},
        {"type":"link", "text":"Menu Tab 4", "contentClass":"+${styles.button_color_green}", "onClick":"javascript:alert('Clicked menu item!');"},
        {"type":"submit", "text":"Menu Tab 5 (submit)", "disabled":true, "class":"+mymenuitemclass", "contentClass":"+mymenuitemcontentclass"},
        {"type":"link", "text":"Menu Tab 6", "selected":true},
        {"type":"link", "text":"Menu Tab 7", "active":true},
        {"type":"generic", "text":"Menu Tab 8"},
        {"type":"generic", "text":"Menu Tab 9", "contentClass":"+thisclassaddsacontainer"}
    ]>
    <@asset name="BaseMenu" def="global" ctxVars={"items":[]} attribs={"type":"generic", "items":menuItems} ovrdCtxVars={"type":"button"}/> 
  </@section>
   -->
</#macro>

<@commonTmpl.footerIncludes />
