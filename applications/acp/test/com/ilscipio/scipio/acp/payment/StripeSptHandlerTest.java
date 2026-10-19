package com.ilscipio.scipio.acp.payment;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler.ChargeResult;

public class StripeSptHandlerTest {

    private static Map<String, Object> json(String s) throws Exception {
        return AcpJson.parse(s);
    }

    @Test
    void succeeded() throws Exception {
        ChargeResult r = StripeSptHandler.result(json("{\"id\":\"pi_1\",\"status\":\"succeeded\"}"));
        assertEquals(ChargeResult.Status.SUCCEEDED, r.status);
        assertEquals("pi_1", r.reference);
    }

    @Test
    void requiresAction() throws Exception {
        ChargeResult r = StripeSptHandler.result(json("{\"id\":\"pi_2\",\"status\":\"requires_action\"}"));
        assertEquals(ChargeResult.Status.ACTION_REQUIRED, r.status);
        assertEquals("pi_2", r.reference);
        assertEquals("requires_3ds", r.code);
    }

    @Test
    void requiresPaymentMethodDeclined() throws Exception {
        ChargeResult r = StripeSptHandler.result(json(
                "{\"id\":\"pi_3\",\"status\":\"requires_payment_method\",\"last_payment_error\":{\"decline_code\":\"do_not_honor\",\"message\":\"no\"}}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("do_not_honor", r.code);
        assertEquals("pi_3", r.reference);
    }

    @Test
    void requiresPaymentMethodWithoutError() throws Exception {
        ChargeResult r = StripeSptHandler.result(json("{\"id\":\"pi_4\",\"status\":\"requires_payment_method\"}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("payment_declined", r.code);
    }

    @Test
    void cardErrorDeclined() throws Exception {
        ChargeResult r = StripeSptHandler.result(json(
                "{\"error\":{\"type\":\"card_error\",\"decline_code\":\"insufficient_funds\",\"payment_intent\":{\"id\":\"pi_5\"}}}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("insufficient_funds", r.code);
        assertEquals("pi_5", r.reference);
    }

    @Test
    void invalidRequestIsDeclined() throws Exception {
        ChargeResult r = StripeSptHandler.result(json("{\"error\":{\"type\":\"invalid_request_error\",\"code\":\"resource_missing\"}}"));
        assertEquals(ChargeResult.Status.DECLINED, r.status);
        assertEquals("resource_missing", r.code);
    }

    @Test
    void apiErrorIsError() throws Exception {
        ChargeResult r = StripeSptHandler.result(json("{\"error\":{\"type\":\"api_error\"}}"));
        assertEquals(ChargeResult.Status.ERROR, r.status);
        assertEquals("payment_declined", r.code);
    }

    @Test
    void encodeBracketKeys() {
        Map<String, String> form = new LinkedHashMap<>();
        form.put("amount", "1999");
        form.put("metadata[order id]", "A&B");
        form.put("skipped", null);
        assertEquals("amount=1999&metadata%5Border+id%5D=A%26B", StripeSptHandler.encode(form));
    }
}
