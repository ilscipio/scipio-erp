<#-- Experimental breadcrumbs -->
<@nav type="breadcrumbs">
    <#-- Link to dashboard -->
    <li>
      <a href="<@ofbizUrl>main</@ofbizUrl>">${uiLabelMap.CommonMain}</a>
    </li>
    <#-- Show the category branch -->
    <#assign crumbs = Static["org.ofbiz.product.category.CategoryWorker"].getTrail(request)!/>
    <#list crumbs as crumb>
         <#if catContentWrappers?? && catContentWrappers[crumb]??>
              <li <#if !crumb_has_next && !productContentWrapper??>class="${styles.nav_breadcrumbs_active!}"</#if>>
                 <a href="<@ofbizCatalogUrl currentCategoryId=crumb previousCategoryId=previousCategoryId!""/>">
                   <#if catContentWrappers[crumb].get("CATEGORY_NAME")??>
                     ${catContentWrappers[crumb].get("CATEGORY_NAME")}
                   <#elseif catContentWrappers[crumb].get("DESCRIPTION")??>
                     ${catContentWrappers[crumb].get("DESCRIPTION")}
                   <#else>
                     ${crumb}
                   </#if>
                 </a>
              </li>   
            <#assign previousCategoryId = crumb />
         </#if>
    </#list>
    <#-- We always assume that the product Detail page is the last page-->
    <#if productContentWrapper??>
          <li class="${styles.nav_breadcrumbs_active!}">${productContentWrapper.get("PRODUCT_NAME")!}</li>
    </#if>
    
    <#-- If there is neither any category or product information available, display the page title -->
    <#if !crumbs?has_content && !productContentWrapper??>
        <li class="${styles.nav_breadcrumbs_active!}"><#if (page.titleProperty)?has_content>${uiLabelMap[page.titleProperty]}<#else>${(page.title)!}</#if></li>
    </#if>
</@nav>