/**
This file is subject to the terms and conditions defined in
file 'LICENSE', which is part of this source code package.
**/

import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;

facility = fixedAsset.getRelatedOne("LocatedAtFacility", false);
context.locatedAtFacility = facility;

fixedAssetIdents = from("FixedAssetIdent").where("fixedAssetId", fixedAssetId).queryList();
fixedAssetIdentValue = "";
if (fixedAssetIdents) {
    fixedAssetIdents.each { ident ->
        fixedAssetIdentValue = fixedAssetIdentValue + " " + ident.idValue;
    }
}
context.fixedAssetIdentValue = fixedAssetIdentValue;

status = fixedAssetMaint.getRelatedOne("StatusItem", false);
if (status) {
    context.statusItemDesc = status.description;
}
//context.put("fixedAssetMaint",fixedAssetMaint);

intervalUom = fixedAssetMaint.getRelatedOne("IntervalUom", false);
if (intervalUom) {
    context.intervalUomDesc = intervalUom.description;
}

instanceOfProductId = fixedAsset.instanceOfProductId;
productMaintSeqId = fixedAssetMaint.productMaintSeqId;
if (productMaintSeqId) {
    productMaint = from("ProductMaint").where("productId", instanceOfProductId, "productMaintSeqId", productMaintSeqId).queryOne();
    context.productMaintName = productMaint.maintName;
}

productMaintTypeId = fixedAssetMaint.productMaintTypeId;
if (productMaintTypeId) {
    productMaintType = from("ProductMaintType").where("productMaintTypeId", productMaintTypeId).queryOne();
    if (productMaintType) {
        productMaintTypeDesc = productMaintType.description;
        context.productMaintTypeDesc = productMaintTypeDesc;
    }
}

intervalMeterTypeId = fixedAssetMaint.intervalMeterTypeId;
productMeterTypeDesc = "";
if (intervalMeterTypeId) {
    productMeterType = from("ProductMeterType").where("productMeterTypeId", intervalMeterTypeId).queryOne();
    productMeterTypeDesc  = productMeterType.description;
}
context.productMeterTypeDesc = productMeterTypeDesc;

scheduleWorkEffort = fixedAssetMaint.getRelatedOne("ScheduleWorkEffort", false);
context.scheduleWorkEffort = scheduleWorkEffort;
