<#if listMarketingSegment?has_content>
    <@section>
        <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
            <#-- Header Begins -->
            <@thead>
                <@tr>
                    <@th>${uiLabelMap.MarketingSegmentGroupSegmentGroupId}</@th>
                    <@th>${uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId}</@th>
                    <@th>${uiLabelMap.MarketingSegmentGroupProductStoreId}</@th>                       
                    <@th>${uiLabelMap.CommonDescription}</@th>                    
                    <@th>${uiLabelMap.CommonDelete}</@th>                   
                </@tr>
            </@thead>
            <#-- Header Ends-->
            <#list listMarketingSegment as marketingSegment>
                <#assign segmentGroupType = marketingSegment.getRelatedOne("SegmentGroupType", false) />                   
                <@tr>
                    <@td><a href="<@ofbizUrl>viewSegmentGroup?segmentGroupId=${marketingSegment.segmentGroupId}</@ofbizUrl>">${marketingSegment.segmentGroupId}</a></@td>
                    <@td>${segmentGroupType.description!}</@td>
                    <@td>${marketingSegment.productStoreId!}</@td>                                                 
                    <@td>${marketingSegment.description}</@td>
                    <@td><a href="javascript:document.deleteMarketingSegment_${marketingSegment_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>                    
                </@tr>
            </#list>
        </@table>    
        
        <#list listMarketingSegment as marketingSegment>
            <form name="deleteMarketingSegment_${marketingSegment_index}" action="<@ofbizUrl>deleteSegmentGroup</@ofbizUrl>" method="POST">
                <@field name="segmentGroupId" type="hidden" value=marketingSegment.segmentGroupId />
            </form>
        </#list>
    </@section>
<#else>
    <@commonMsg type="result-norecord">${uiLabelMap.MarketingSegmentGroupNoSegmentGroupFound}.</@commonMsg>    
</#if>