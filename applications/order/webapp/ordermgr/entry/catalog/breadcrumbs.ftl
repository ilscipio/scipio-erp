<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#assign useListElems = useListElems!true>
<div<#if !useListElems> class="${styles.nav_breadcrumbs!}"</#if>>
<#if !useListElems>
  <a href="<@ofbizUrl>main</@ofbizUrl>" class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_link!}">${uiLabelMap.CommonMain}</a> &gt;
<#else>
  <ul class="${styles.nav_breadcrumbs!}">
    <li class="${styles.nav_breadcrumb!}">
      <a href="<@ofbizUrl>main</@ofbizUrl>" class="${styles.nav_breadcrumb_link!}">${uiLabelMap.CommonMain}</a>
    </li>
</#if>    
    <#-- Show the category branch -->
    <#assign crumbs = Static["org.ofbiz.product.category.CategoryWorker"].getTrail(request)/>
    <#list crumbs as crumb>
        <#if useListElems>         
          <li class="${styles.nav_breadcrumb!}">
             <a href="<@ofbizCatalogUrl currentCategoryId=crumb previousCategoryId=previousCategoryId!""/>" class="${styles.nav_breadcrumb_link!}<#if !crumb_has_next && !productContentWrapper??> ${styles.nav_breadcrumb_active!}</#if>">
               <#if (catContentWrappers[crumb].get("CATEGORY_NAME", "html"))?has_content>
                 ${catContentWrappers[crumb].get("CATEGORY_NAME", "html")}
               <#elseif (catContentWrappers[crumb].get("DESCRIPTION", "html"))?has_content>
                 ${catContentWrappers[crumb].get("DESCRIPTION", "html")}
               <#else>
                 ${crumb}
               </#if>
             </a>
          </li>
        <#else>  
           <a href="<@ofbizCatalogUrl currentCategoryId=crumb previousCategoryId=previousCategoryId!""/>" class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_link!}<#if !crumb_has_next && !productContentWrapper??> ${styles.nav_breadcrumb_active!}</#if>">
             <#if (catContentWrappers[crumb].get("CATEGORY_NAME", "html"))?has_content>
               ${catContentWrappers[crumb].get("CATEGORY_NAME", "html")}
             <#elseif (catContentWrappers[crumb].get("DESCRIPTION", "html"))?has_content>
               ${catContentWrappers[crumb].get("DESCRIPTION", "html")}
             <#else>
               ${crumb}
             </#if>
           </a>
           <#if crumb_has_next> &gt;</#if>
        </#if>  
        <#assign previousCategoryId = crumb />
    </#list>    
    <#-- Show the product, if there is one -->
    <#if productContentWrapper??>
      <#if !useListElems>
         &nbsp;&gt; <span class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_active!}">${productContentWrapper.get("PRODUCT_NAME", "html")!}</span>
      <#else>
          <li class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_active!}"><span>${productContentWrapper.get("PRODUCT_NAME", "html")!}</span></li>
      </#if>
    </#if>
<#if useListElems>
  </ul>  
</#if>  
</div>
