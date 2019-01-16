package org.ofbiz.accounting.test;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * SCIPIO: GiftCertificateTests.
 * <p>
 * NOTE: Some of these are for manual invocation for time being... (TODO?: integrate into run-tests somehow)
 * WARN: All subject to change without notice.
 */
public class GiftCertificateTests extends OFBizTestCase {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public GiftCertificateTests(String name) {
        super(name);
    }

    /**
     * sendTestGCEmail.
     * <p>
     * Example usage (groovy):
     * <pre>{@code
     * org.ofbiz.accounting.test.GiftCertificateTests.sendTestStoreGCEmail(dispatcher.getDispatchContext(), userLogin, locale, "ScipioShop", "PRDS_GC_PURCHASE", "test@example.com", "123456781234", "1234");
     * org.ofbiz.accounting.test.GiftCertificateTests.sendTestStoreGCEmail(dispatcher.getDispatchContext(), userLogin, locale, "ScipioShop", "PRDS_GC_RELOAD", "test@example.com", "123456781234", "1234");
     * }</pre>
     */
    public static void sendTestStoreGCEmail(DispatchContext dctx, GenericValue userLogin, Locale locale,
            String productStoreId, String emailType, String sendTo, String cardNumber, String pinNumber) {
        Delegator delegator = dctx.getDelegator();
        GenericValue productStoreEmail = null;
        try {
            productStoreEmail = EntityQuery.use(delegator).from("ProductStoreEmailSetting").where("productStoreId", "ScipioShop", "emailType", emailType).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Unable to get product store email setting for gift card purchase", module);
        }

        Map<String, Object> answerMap = new HashMap<>();

        answerMap.put("locale", locale);
        answerMap.put("productStoreId", productStoreId);

        Map<String, Object> emailCtx = new HashMap<>();
        String bodyScreenLocation = productStoreEmail.getString("bodyScreenLocation");
        if (UtilValidate.isEmpty(bodyScreenLocation)) {
            bodyScreenLocation = ProductStoreWorker.getDefaultProductStoreEmailScreenLocation(emailType);
        }
        emailCtx.put("bodyScreenUri", bodyScreenLocation);
        emailCtx.put("bodyParameters", answerMap);
        emailCtx.put("sendTo", sendTo);
        emailCtx.put("contentType", productStoreEmail.get("contentType"));
        emailCtx.put("sendFrom", productStoreEmail.get("fromAddress"));
        emailCtx.put("sendCc", productStoreEmail.get("ccAddress"));
        emailCtx.put("subject", productStoreEmail.getString("subject"));
        emailCtx.put("userLogin", userLogin);

        // SCIPIO: Determine webSiteId for store email
        String currency = EntityUtilProperties.getPropertyValue("general", "currency.uom.id.default", "USD", delegator);

        emailCtx.put("webSiteId", "ScipioWebStore");
        answerMap.put("senderName", "Joe");
        answerMap.put("amount", new BigDecimal(33.0));
        answerMap.put("previousAmount", new BigDecimal(11.0));
        answerMap.put("giftMessage", "This is a gift");
        answerMap.put("recipientName", "Bob");
        answerMap.put("processResult", true);
        answerMap.put("giftCardNumber", cardNumber);
        answerMap.put("cardNumber", cardNumber);
        answerMap.put("pinNumber", pinNumber);

        answerMap.put("currencyUomId", currency);
        try {
            dctx.getDispatcher().runSync("sendMailFromScreen", emailCtx);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Problem sending mail", module);
        }
    }
}
