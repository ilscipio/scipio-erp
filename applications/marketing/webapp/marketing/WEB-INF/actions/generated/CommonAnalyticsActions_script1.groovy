productStores = delegator.findByAnd("ProductStore", null, ["productStoreId", "storeName"], true);
                context.productStores = productStores;
                
                // Instead of listing all, we will list only those that can actually be converted...
                //context.currencies = delegator.findByAnd("Uom", ["uomTypeId": "CURRENCY_MEASURE"], ["uomId"], true);
                context.currencies = org.ofbiz.common.uom.UomWorker.getConvertibleUoms(delegator, dispatcher, 
                    Boolean.TRUE, ["uomTypeId": "CURRENCY_MEASURE"], ["uomId"], true, null, true);

                context.allowedOrderStatus = org.ofbiz.base.util.UtilMisc.toList("ORDER_APPROVED", "ORDER_SENT", "ORDER_COMPLETED")