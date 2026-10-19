/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.order.mcp;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.sql.Timestamp;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;

/** R4-3: order/order get returns the hold reason (newest status change reason, else newest note). */
public class OrderHoldReasonTest {

    private static GenericValue status(String seq, long at, String reason) {
        return status("ORDER_HOLD", seq, null, at, reason);
    }

    private static GenericValue status(String statusId, String seq, String pref, long at, String reason) {
        GenericValue v = mock(GenericValue.class);
        when(v.getString("statusId")).thenReturn(statusId);
        when(v.getString("orderPaymentPreferenceId")).thenReturn(pref);
        when(v.getString("orderItemSeqId")).thenReturn(seq);
        when(v.getTimestamp("statusDatetime")).thenReturn(new Timestamp(at));
        when(v.getString("changeReason")).thenReturn(reason);
        return v;
    }

    private static GenericValue note(long at, String text) {
        GenericValue v = mock(GenericValue.class);
        when(v.getTimestamp("noteDateTime")).thenReturn(new Timestamp(at));
        when(v.getString("noteInfo")).thenReturn(text);
        return v;
    }

    @Test
    void theNewestStatusChangeReasonWins() {
        List<GenericValue> st = List.of(status(null, 1000, "Old reason"), status(null, 2000, " Payment check failed "));
        assertEquals("Payment check failed", OrderMcp.holdReasonOf(st, List.of(note(3000, "A note"))));
    }

    @Test
    void anItemStatusIsNotTheOrderStatus() {
        List<GenericValue> st = List.of(status(null, 1000, "Order level"), status("00001", 5000, "Item level"));
        assertEquals("Order level", OrderMcp.holdReasonOf(st, List.of()));
    }

    @Test
    void withoutAStatusReasonTheNewestNoteIsTheReason() {
        List<GenericValue> st = List.of(status(null, 2000, null));
        List<GenericValue> notes = List.of(note(1000, "First note"), note(3000, "Address does not match"), note(2000, ""));
        assertEquals("Address does not match", OrderMcp.holdReasonOf(st, notes));
    }

    @Test
    void noReasonAnywhereIsNull() {
        assertNull(OrderMcp.holdReasonOf(List.of(status(null, 1, "")), List.of()));
        assertNull(OrderMcp.holdReasonOf(null, null));
    }

    @Test
    void aPaymentPreferenceRowDoesNotHideTheHoldReason() {
        List<GenericValue> st = List.of(status(null, 1000, "Payment check failed"),
                status("PAYMENT_NOT_AUTH", null, "pref1", 2000, "Pref reason"),
                status("ORDER_HOLD", null, "pref1", 3000, "Pref hold"));
        assertEquals("Payment check failed", OrderMcp.holdReasonOf(st, List.of()));
    }

    @Test
    void aNonHoldOrderRowIsNotTheHold() {
        List<GenericValue> st = List.of(status(null, 1000, "Hold reason"), status("ORDER_APPROVED", null, null, 2000, "Other"));
        assertEquals("Hold reason", OrderMcp.holdReasonOf(st, List.of()));
    }

    @Test
    void aNoteOlderThanTheHoldIsNotTheReason() {
        List<GenericValue> st = List.of(status(null, 2000, null));
        assertNull(OrderMcp.holdReasonOf(st, List.of(note(1000, "Old note"))));
        assertEquals("Same time", OrderMcp.holdReasonOf(st, List.of(note(1000, "Old note"), note(2000, "Same time"))));
    }

    @Test
    void aNullStatusTimeIsNotTheNewest() {
        GenericValue undated = status(null, 0, "Undated");
        when(undated.getTimestamp("statusDatetime")).thenReturn(null);
        assertEquals("Dated", OrderMcp.holdReasonOf(List.of(status(null, 1000, "Dated"), undated), List.of()));
        assertEquals("Dated", OrderMcp.holdReasonOf(List.of(undated, status(null, 1000, "Dated")), List.of()));
    }
}
