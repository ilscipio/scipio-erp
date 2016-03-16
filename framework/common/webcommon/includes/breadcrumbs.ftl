
<#macro breadcrumbs crumbs=true catContentWrappers={}>
    <#-- Breadcrumbs -->
    <@nav type="breadcrumbs">
        <#-- Link to dashboard -->
        <li<@compiledClassAttribStr class=styles.nav_breadcrumb!/>>
          <a href="<@ofbizUrl>main</@ofbizUrl>"<@compiledClassAttribStr class=styles.nav_breadcrumb_link!/>>${uiLabelMap.CommonMain}</a>
        </li>
        
        <#-- Show the category branch -->
        <#if crumbs?is_boolean>
          <#if crumbs>
            <#local crumbs = Static["org.ofbiz.product.category.CategoryWorker"].getTrail(request)!/>
          <#else>
            <#local crumbs = []>
          </#if>
        </#if>
        <#local previousCategoryId = "">
        <#list crumbs as crumb>
            <#-- Cato: NOTE: TOP is also hardcoded in CategoryUrlTransform -->
            <#if crumb!="TOP">
                <#-- Cato: Try content wrappers set by screen first; if not there, lookup ourselves -->
                <#local catContentWrapper = {}>
                <#if (catContentWrappers[crumb])??>
                  <#local catContentWrapper = catContentWrappers[crumb]>
                <#else>
                  <#local crumbProdCategory = delegator.findOne("ProductCategory", {"productCategoryId":crumb}, true)!/>
                  <#if crumbProdCategory?has_content>
                    <#local catContentWrapper = Static["org.ofbiz.product.category.CategoryContentWrapper"].makeCategoryContentWrapper(crumbProdCategory, request)!>
                  </#if>
                </#if>
                <#local elemClass = styles.nav_breadcrumb!>
                <#if !crumb_has_next && !productContentWrapper??>
                  <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
                </#if>
                <li<@compiledClassAttribStr class=elemClass/>>
                   <a href="<@ofbizCatalogUrl currentCategoryId=crumb previousCategoryId=previousCategoryId!""/>"<@compiledClassAttribStr class=styles.nav_breadcrumb_link!/>><#rt>
                     <#local crumbText = (catContentWrapper.get("CATEGORY_NAME","html"))!"">
                     <#if !crumbText?has_content>
                       <#local crumbText = (catContentWrapper.get("DESCRIPTION","html"))!"">
                       <#if !crumbText?has_content>
                         <#-- use the ID --><#t>
                         <#local crumbText = crumb>
                       </#if>
                     </#if>
                     ${crumbText}<#t>
                   </a><#lt>
                </li>
            </#if>   
            <#local previousCategoryId = crumb />
        </#list>
    
        <#-- We always assume that the product Detail page is the last in trail -->
        <#if productContentWrapper??>
            <#local productText = (productContentWrapper.get("PRODUCT_NAME","html"))!>
            <#if !productText?has_content>
              <#local productText = (productContentWrapper.get("PRODUCT_ID","html"))!>
            </#if>
            <#if productText?has_content>
              <#local elemClass = styles.nav_breadcrumb!>
              <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
              <li<@compiledClassAttribStr class=elemClass/>>${productText}</li>
            </#if>
        </#if>
        
        <#-- If there is neither any category or product information available, display the page title -->
        <#if !crumbs?has_content && !productContentWrapper??>
            <#local titleText><#if title?has_content>${title}<#elseif titleProperty?has_content>${uiLabelMap[titleProperty]}</#if></#local>
            <#if titleText?has_content>
              <#local elemClass = styles.nav_breadcrumb!>
              <#local elemClass = addClassArg(elemClass, styles.nav_breadcrumb_active!)>
              <li<@compiledClassAttribStr class=elemClass/>>${titleText}</li>
            </#if>
        </#if>
    </@nav>
</#macro>

<@breadcrumbs catContentWrappers=catContentWrappers!{} />

