<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@heading>${uiLabelMap.ProductProductTaggedWith} "${currentSearch}"</@heading>

<#if !productIds?has_content>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
<#else>
  <@paginate mode="content" url=makeOfbizUrl("keywordsearch") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
    <div class="productsummary-container">
        <#list productIds as productId> <#-- note that there is no boundary range because that is being done before the list is put in the content -->
            <@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/>
        </#list>
    </div>
  </@paginate>
</#if>

