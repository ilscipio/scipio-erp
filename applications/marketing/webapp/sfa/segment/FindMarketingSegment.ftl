<@section>
    <form name="FindSegmentGroup" action="<@ofbizUrl>FindSegmentGroup</@ofbizUrl>" method="POST">
       <#assign segmentGroupTypes = delegator.findByAnd("SegmentGroupType", null, null, false) />
       <@field type="input" name="segmentGroupId" label=uiLabelMap.MarketingSegmentGroupSegmentGroupId value=(parameters.segmentGroupId!) />
       <@field type="select" name="segmentGroupTypeId" label=uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId value=(parameters.segmentGroupTypeId!)>
            <option value=""></option>
            <#list segmentGroupTypes as segmentGroupType> 
                <option value="${segmentGroupType.segmentGroupTypeId}" <#if parameters.segmentGroupTypeId?has_content && parameters.segmentGroupTypeId == segmentGroupType.segmentGroupTypeId>selected="selected"</#if>>${segmentGroupType.description}</option>
            </#list>
       </@field>
       <@field type="lookup" name="productStoreId" label=uiLabelMap.MarketingSegmentGroupProductStoreId formName="FindSegmentGroup" id="productStoreId" fieldFormName="LookupProductStore" value=(parameters.productStoreId!)/>       
       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>