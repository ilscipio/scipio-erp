import org.ofbiz.base.util.Debug
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.service.ServiceUtil


abandonedCart = context.abandonedCart
abandonedCartLines = context.abandonedCartLines
userLogin = context.userLogin

applyStorePromotions = (context.applyStorePromotions) ? context.applyStorePromotions : parameters.applyStorePromotions
cartPartyId = (context.cartPartyId) ? context.cartPartyId : parameters.cartPartyId

visitId = context.visitId
if (visitId && !abandonedCart) {
    abandonedCart = EntityQuery.use(delegator).from("CartAbandoned").where(["visitId" : visitId]).cache().queryOne();
    abandonedCartLines = abandonedCart.getRelated("CartAbandonedLine", true);
}

if (abandonedCart) {
    if (!visitId) {
        visitId = abandonedCart.visitId
    }
    loadCartFromAbandonedCartCtx = ["visitId" : visitId, "abandonedCart": abandonedCart, "abandonedCartLines": abandonedCartLines,
                                    "applyStorePromotions": applyStorePromotions, "cartPartyId": cartPartyId, "userLogin": userLogin]

    loadCartFromAbandonedCartResult = dispatcher.runSync("loadCartFromAbandonedCart", loadCartFromAbandonedCartCtx);
    if (ServiceUtil.isSuccess(loadCartFromAbandonedCartResult)) {
        shoppingCart = loadCartFromAbandonedCartResult.get("shoppingCart")
        context.shoppingCart = shoppingCart
        context.shoppingCartSize = shoppingCart?.size() ?: 0;
    }
} else {
    Debug.logWarning("Can't prepare cart recovery for visit: " + visitId)
}