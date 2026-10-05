<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#if listMarketingSegment?has_content>
    <@section>
        <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1>
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
                    <@td><a href="<@pageUrl>viewSegmentGroup?segmentGroupId=${marketingSegment.segmentGroupId}</@pageUrl>">${marketingSegment.segmentGroupId}</a></@td>
                    <@td>${segmentGroupType.description!}</@td>
                    <@td>${marketingSegment.productStoreId!}</@td>                                                 
                    <@td>${marketingSegment.description}</@td>
                    <@td><a href="javascript:document.deleteMarketingSegment_${marketingSegment_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>                    
                </@tr>
            </#list>
        </@table>    
        
        <#list listMarketingSegment as marketingSegment>
            <form name="deleteMarketingSegment_${marketingSegment_index}" action="<@pageUrl>deleteSegmentGroup</@pageUrl>" method="POST">
                <@field name="segmentGroupId" type="hidden" value=marketingSegment.segmentGroupId />
            </form>
        </#list>
    </@section>
<#else>
    <@commonMsg type="result-norecord">${uiLabelMap.MarketingSegmentGroupNoSegmentGroupFound}.</@commonMsg>    
</#if>