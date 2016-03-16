<#-- Breadcrumbs -->
<@nav type="breadcrumbs">
    <#-- Link to dashboard -->
    <li class="${styles.nav_breadcrumb!}">
      <a href="<@ofbizUrl>main</@ofbizUrl>" class="${styles.nav_breadcrumb_link!}">${uiLabelMap.CommonMain}</a>
    </li>
    <#-- Show the category branch -->
    <#assign crumbs = Static["org.ofbiz.product.category.CategoryWorker"].getTrail(request)!/>
    <#list crumbs as crumb>
          <li class="${styles.nav_breadcrumb!}<#if !crumb_has_next && !productContentWrapper??> ${styles.nav_breadcrumb_active!}</#if>">
             <a href="<@ofbizCatalogUrl currentCategoryId=crumb previousCategoryId=previousCategoryId!""/>" class="${styles.nav_breadcrumb_link!}">
               <#if (catContentWrappers[crumb].get("CATEGORY_NAME"))?has_content>
                 ${catContentWrappers[crumb].get("CATEGORY_NAME")}
               <#elseif (catContentWrappers[crumb].get("DESCRIPTION"))?has_content>
                 ${catContentWrappers[crumb].get("DESCRIPTION")}
               <#else>
                 ${crumb}
               </#if>
             </a>
          </li>   
        <#assign previousCategoryId = crumb />
    </#list>
    <#-- We always assume that the product Detail page is the last page-->
    <#if productContentWrapper??>
          <li class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_active!}">${productContentWrapper.get("PRODUCT_NAME")!}</li>
    </#if>
    
    <#-- If there is neither any category or product information available, display the page title -->
    <#if !crumbs?has_content && !productContentWrapper??>
        <li class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_active!}"><#if title?has_content>${title}<#elseif titleProperty?has_content>${uiLabelMap[titleProperty]}</#if></li>
    </#if>
</@nav>