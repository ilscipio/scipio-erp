<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section>
    <@table type="fields">
        <#-- availability -->
        <#if product.introductionDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonIntroductionDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.introductionDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.releaseDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonReleaseDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.releaseDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.salesDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.salesDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>

        <#if product.supportDiscontinuationDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSupportThruDate}
              </@td>
              <@td colspan="3"><@formattedDateTime date=product.supportDiscontinuationDate defaultVal="0000-00-00 00:00:00"/></@td>
            </@tr>    
        </#if>
    </@table>


</@section>