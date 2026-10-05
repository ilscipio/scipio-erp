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
<@section>
    <form name="FindSegmentGroup" action="<@pageUrl>FindSegmentGroup</@pageUrl>" method="POST">
       <#assign segmentGroupTypes = delegator.findByAnd("SegmentGroupType", null, null, false) />
       <@field type="input" name="segmentGroupId" label=uiLabelMap.MarketingSegmentGroupSegmentGroupId value=(parameters.segmentGroupId!) />
       <@field type="select" name="segmentGroupTypeId" label=uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId value=(parameters.segmentGroupTypeId!)>
            <option value=""></option>
            <#list segmentGroupTypes as segmentGroupType> 
                <option value="${segmentGroupType.segmentGroupTypeId}"<#if parameters.segmentGroupTypeId?has_content && parameters.segmentGroupTypeId == segmentGroupType.segmentGroupTypeId> selected="selected"</#if>>${segmentGroupType.description}</option>
            </#list>
       </@field>
       <@field type="lookup" name="productStoreId" label=uiLabelMap.MarketingSegmentGroupProductStoreId formName="FindSegmentGroup" id="productStoreId" fieldFormName="LookupProductStore" value=(parameters.productStoreId!)/>       
       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>