
<#macro breadcrumbs crumbs=true productContentWrapper="" catContentWrappers={} useTitleFallback="" showMain=true showLinks=true>
    <#if !useTitleFallback?has_content>
      <#local useTitleFallback = true>
    </#if>
    <@nav type="breadcrumbs">
      <#if showMain>
        <#-- Link to dashboard -->
        <li<@compiledClassAttribStr class=styles.nav_breadcrumb!/>>
          <#if showLinks><a href="<@pageUrl>main</@pageUrl>"<@compiledClassAttribStr class=styles.nav_breadcrumb_link!/>></#if>${uiLabelMap.CommonMain}<#if showLinks></a></#if>
        </li>
      </#if>
        
        <#-- Show the category branch -->
        <#if crumbs?is_boolean>
          <#if crumbs>
            <#local crumbs = Static["org.ofbiz.product.category.CategoryWorker"].getTrailNoTop(request)!/>
          <#else>
            <#local crumbs = []>
          </#if>
        </#if>
        <#local previousCategoryId = "">
        <#list crumbs as crumb>
                <#local rawCrumb = rawString(crumb)><#-- SCIPIO -->
                <#-- SCIPIO: Try content wrappers set by screen first; if not there, lookup ourselves -->
                <#local catContentWrapper = {}>
                <#if (catContentWrappers[rawCrumb])??>
                  <#local catContentWrapper = catContentWrappers[rawCrumb]>
                <#else>
                  <#local crumbProdCategory = delegator.findOne("ProductCategory", {"productCategoryId":rawCrumb}, true)!/>
                  <#if crumbProdCategory?has_content>
                    <#local catContentWrapper = Static["org.ofbiz.product.category.CategoryContentWrapper"].makeCategoryContentWrapper(crumbProdCategory, request)!>
                  </#if>
                </#if>
                <#local elemClass = styles.nav_breadcrumb!>
                <#if !crumb?has_next && !productContentWrapper?has_content>
                  <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
                </#if>
                <li<@compiledClassAttribStr class=elemClass/>><#rt/>
                   <#if showLinks><a href="<@catalogUrl currentCategoryId=rawCrumb previousCategoryId=(previousCategoryId!"")/>"<@compiledClassAttribStr class=(styles.nav_breadcrumb_link!)/>></#if><#t>
                     <#local crumbText = (catContentWrapper.get("CATEGORY_NAME"))!>
                     <#if !crumbText?has_content>
                       <#local crumbText = (catContentWrapper.get("DESCRIPTION"))!>
                       <#if !crumbText?has_content>
                         <#-- use the ID -->
                         <#local crumbText = crumb>
                       </#if>
                     </#if>
                     ${crumbText}<#t>
                   <#if showLinks></a></#if><#t>
                </li><#lt/>
            <#local previousCategoryId = rawCrumb />
        </#list>
    
        <#-- We always assume that the product Detail page is the last in trail -->
        <#if productContentWrapper?has_content>
            <#-- WARN: ?string required for ?has_content to work with result from content wrapper! -->
            <#local productText = (productContentWrapper.get("PRODUCT_NAME"))!>
            <#if !productText?has_content>
              <#local productText = (productContentWrapper.get("PRODUCT_ID"))!>
            </#if>
            <#if productText?has_content>
              <#local elemClass = styles.nav_breadcrumb!>
              <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
              <li<@compiledClassAttribStr class=elemClass/>>${productText}</li>
            </#if>
        </#if>

      <#if useTitleFallback>
        <#-- If there is neither any category or product information available, display the page title -->
        <#if !crumbs?has_content && !productContentWrapper??>
            <#local titleText><#if title?has_content>${title}<#elseif titleProperty?has_content>${uiLabelMap[titleProperty]}</#if></#local>
            <#if titleText?has_content>
              <#local elemClass = styles.nav_breadcrumb!>
              <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
              <li<@compiledClassAttribStr class=elemClass/>>${titleText}</li>
            </#if>
        </#if>
      </#if>
    </@nav>
</#macro>

<#if (crumbsLibOnly!false) != true><#-- SCIPIO: Set to true to include only the macro -->
  <@breadcrumbs catContentWrappers=(catContentWrappers!{}) productContentWrapper=(productContentWrapper!)
    useTitleFallback=(useBreadcrumbsTitleFallback!"") showMain=(crumbsShowMain!true) showLinks=(crumbsShowLinks!true)/>
</#if>
