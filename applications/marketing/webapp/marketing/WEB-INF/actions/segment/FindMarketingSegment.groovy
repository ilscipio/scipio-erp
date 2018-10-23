import org.ofbiz.base.util.Debug

segmentGroupId = parameters.segmentGroupId;
segmentGroupTypeId = parameters.segmentGroupTypeId;
productStoreId = parameters.productStoreId;

condition=[:];
if (segmentGroupId)
    condition.put("segmentGroupId", segmentGroupId);
if (segmentGroupTypeId)
    condition.put("segmentGroupTypeId", segmentGroupTypeId);
if (productStoreId)    
    condition.put("productStoreId", productStoreId);
listMarketingSegment=from("SegmentGroup").where(condition).queryList();
context.listMarketingSegment=listMarketingSegment;