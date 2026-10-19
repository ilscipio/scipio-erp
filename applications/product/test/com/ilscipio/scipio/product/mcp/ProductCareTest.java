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
package com.ilscipio.scipio.product.mcp;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.product.service.ProductCareServices;

/** R4-7: catalog/product archive, delete, image_remove and image_order (wiring, permission, the order refusal, the photo order). */
public class ProductCareTest {

    private static McpServiceTool serviceTool(String name) {
        for (McpServiceTool t : CatalogMcp.class.getAnnotation(McpServer.class).serviceTools()) {
            if ("product".equals(t.topic()) && name.equals(t.name())) return t;
        }
        return null;
    }

    private static McpTool tool(String name) {
        for (java.lang.reflect.Method m : CatalogMcp.class.getDeclaredMethods()) {
            McpTool a = m.getAnnotation(McpTool.class);
            if (a != null && "product".equals(a.topic()) && name.equals(a.name())) return a;
        }
        return null;
    }

    @Test
    void theFourActionsAreWiredAsWrites() {
        McpServiceTool archive = serviceTool("archive");
        assertNotNull(archive);
        assertEquals("archiveProduct", archive.service());
        assertFalse(archive.readOnly());
        McpServiceTool delete = serviceTool("delete");
        assertNotNull(delete);
        assertEquals("deleteProductIfUnordered", delete.service());
        assertEquals("true", delete.destructive());
        for (String n : new String[] {"image_remove", "image_order"}) {
            McpTool t = tool(n);
            assertNotNull(t, n);
            assertFalse(t.readOnly(), n);
        }
    }

    private static DispatchContext ctx(Security sec, Delegator d) {
        DispatchContext dctx = mock(DispatchContext.class);
        when(dctx.getSecurity()).thenReturn(sec);
        when(dctx.getDelegator()).thenReturn(d);
        return dctx;
    }

    @Test
    void archiveNeedsCatalogUpdate() {
        Map<String, Object> r = ProductCareServices.archiveProduct(ctx(mock(Security.class), mock(Delegator.class)), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isError(r));
        assertTrue(ServiceUtil.getErrorMessage(r).contains("CATALOG_UPDATE"));
    }

    @Test
    void archiveSetsTheSalesEndDate() throws Exception {
        Security sec = mock(Security.class);
        when(sec.hasEntityPermission(eq("CATALOG"), eq("_UPDATE"), (GenericValue) any())).thenReturn(true);
        Delegator d = mock(Delegator.class);
        GenericValue product = mock(GenericValue.class);
        when(d.findOne(eq("Product"), anyBoolean(), any())).thenReturn(product);
        Map<String, Object> r = ProductCareServices.archiveProduct(ctx(sec, d), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isSuccess(r));
        assertNotNull(r.get("salesDiscontinuationDate"));
        verify(product).set(eq("salesDiscontinuationDate"), any());
        verify(product).store();
    }

    @Test
    void deleteNeedsCatalogDelete() {
        Map<String, Object> r = ProductCareServices.deleteProductIfUnordered(ctx(mock(Security.class), mock(Delegator.class)), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isError(r));
        assertTrue(ServiceUtil.getErrorMessage(r).contains("CATALOG_DELETE"));
    }

    private static GenericValue row(String entity, String key, String value) {
        GenericValue v = mock(GenericValue.class);
        when(v.getEntityName()).thenReturn(entity);
        when(v.get(key)).thenReturn(value);
        when(v.toString()).thenReturn(entity + ":" + value);
        return v;
    }

    private static ModelRelation fk(String relEntity, String parentField, String childField) {
        // ModelRelation and ModelKeyMap are final: Mockito 2 cannot mock them, so the test builds the real ones
        return ModelRelation.create(null, null, "one", null, relEntity, null, List.of(new ModelKeyMap(childField, parentField)), false);
    }

    private static void model(Delegator d, Map<String, List<ModelRelation>> entities) throws Exception {
        ModelReader reader = mock(ModelReader.class);
        when(d.getModelReader()).thenReturn(reader);
        when(reader.getEntityNames()).thenReturn(entities.keySet());
        for (Map.Entry<String, List<ModelRelation>> e : entities.entrySet()) {
            ModelEntity me = mock(ModelEntity.class);
            when(me.getRelationsList(true, false, false)).thenReturn(e.getValue());
            when(reader.getModelEntityNoCheck(e.getKey())).thenReturn(me);
        }
    }

    private static Delegator deleteDelegator(GenericValue product) throws Exception {
        Delegator d = mock(Delegator.class);
        when(d.findOne(eq("Product"), anyBoolean(), any())).thenReturn(product);
        when(d.findByAnd(anyString(), any(Map.class), any(), anyBoolean())).thenReturn(List.of());
        when(d.findCountByCondition(eq("OrderItem"), any(EntityCondition.class), any(), any())).thenReturn(0L);
        return d;
    }

    private static DispatchContext deleteCtx(Delegator d) {
        Security sec = mock(Security.class);
        when(sec.hasEntityPermission(eq("CATALOG"), eq("_DELETE"), (GenericValue) any())).thenReturn(true);
        return ctx(sec, d);
    }

    @Test
    void deleteRefusesAProductWithAnOrderAndRemovesNothing() throws Exception {
        Delegator d = deleteDelegator(row("Product", "productId", "P1"));
        when(d.findCountByCondition(eq("OrderItem"), any(EntityCondition.class), any(), any())).thenReturn(2L);
        Map<String, Object> r = ProductCareServices.deleteProductIfUnordered(deleteCtx(d), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isError(r));
        String message = ServiceUtil.getErrorMessage(r);
        assertTrue(message.contains("2 order lines"), message);
        assertTrue(message.contains("Archive it instead"), message);
        verify(d, never()).removeValue(any(GenericValue.class));
    }

    @Test
    void deleteRefusesWhenAnOtherOrderRowNamesTheProductAndRemovesNothing() throws Exception {
        GenericValue product = row("Product", "productId", "P1");
        Delegator d = deleteDelegator(product);
        model(d, Map.of("ProductPrice", List.of(fk("Product", "productId", "productId")),
                "ReturnItem", List.of(fk("Product", "productId", "productId"))));
        // the rows are made before the stubbing: a mock made inside thenReturn() breaks the stubbing in progress
        GenericValue price = row("ProductPrice", "productId", "P1");
        GenericValue returnItem = row("ReturnItem", "productId", "P1");
        when(d.findByAnd(eq("ProductPrice"), any(Map.class), any(), anyBoolean())).thenReturn(List.of(price));
        when(d.findByAnd(eq("ReturnItem"), any(Map.class), any(), anyBoolean())).thenReturn(List.of(returnItem));
        Map<String, Object> r = ProductCareServices.deleteProductIfUnordered(deleteCtx(d), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isError(r));
        assertTrue(ServiceUtil.getErrorMessage(r).contains("Archive it instead"));
        verify(d, never()).removeValue(any(GenericValue.class));
    }

    @Test
    void deleteRemovesEveryDependentRowFoundByTheEntityModelThenTheProduct() throws Exception {
        GenericValue product = row("Product", "productId", "P1");
        Delegator d = deleteDelegator(product);
        // InventoryItem points to Product; InventoryItemStatus points to InventoryItem; ShoppingListItem points to Product
        model(d, Map.of("InventoryItem", List.of(fk("Product", "productId", "productId")),
                "InventoryItemStatus", List.of(fk("InventoryItem", "inventoryItemId", "inventoryItemId")),
                "ShoppingListItem", List.of(fk("Product", "productId", "productId"))));
        GenericValue item = row("InventoryItem", "inventoryItemId", "I1");
        GenericValue status = row("InventoryItemStatus", "inventoryItemId", "I1");
        GenericValue shopping = row("ShoppingListItem", "productId", "P1");
        when(d.findByAnd(eq("InventoryItem"), any(Map.class), any(), anyBoolean())).thenReturn(List.of(item));
        when(d.findByAnd(eq("InventoryItemStatus"), any(Map.class), any(), anyBoolean())).thenReturn(List.of(status));
        when(d.findByAnd(eq("ShoppingListItem"), any(Map.class), any(), anyBoolean())).thenReturn(List.of(shopping));
        Map<String, Object> r = ProductCareServices.deleteProductIfUnordered(deleteCtx(d), Map.of("productId", "P1"));
        assertTrue(ServiceUtil.isSuccess(r), String.valueOf(r));
        assertEquals(List.of("P1"), r.get("deletedProductIds"));
        org.mockito.InOrder order = inOrder(d);
        order.verify(d).removeValue(status);
        order.verify(d).removeValue(item);
        order.verify(d).removeValue(product);
        verify(d).removeValue(shopping);
    }

    private static List<String> names(List<Object[]> writes) {
        List<String> out = new java.util.ArrayList<>();
        for (Object[] w : writes) out.add((String) w[0]);
        return out;
    }

    @SuppressWarnings("unchecked")
    @Test
    void aProductWithOnlyMorePhotosMovesTheFirstToMain() {
        List<Map<String, Object>> current = List.of(
                Map.of("slot", "ADDITIONAL_IMAGE_1", "url", "/a.jpg", "contentId", "C1", "dataResourceId", "D1", "fromDate", new java.sql.Timestamp(1)),
                Map.of("slot", "ADDITIONAL_IMAGE_2", "url", "/b.jpg", "contentId", "C2", "dataResourceId", "D2", "fromDate", new java.sql.Timestamp(1)));
        // image_remove of the first photo: one photo is left, it becomes the main image, both more-photo slots end
        List<Object[]> remove = CatalogMcp.planImageWrites("P1", current, List.of("/b.jpg"));
        assertEquals(List.of("updateProduct", "updateProductContent", "updateProductContent"), names(remove));
        assertEquals("/b.jpg", ((Map<String, Object>) remove.get(0)[1]).get("originalImageUrl"));
        // image_order of two photos: the second becomes MAIN, the first moves to slot 1, slot 2 ends
        List<Object[]> order = CatalogMcp.planImageWrites("P1", current, List.of("/b.jpg", "/a.jpg"));
        // slot 1 holds /a.jpg already: no data resource write, only slot 2 ends
        assertEquals(List.of("updateProduct", "updateProductContent"), names(order));
        assertEquals("/b.jpg", ((Map<String, Object>) order.get(0)[1]).get("originalImageUrl"));
        assertEquals("ADDITIONAL_IMAGE_2", ((Map<String, Object>) order.get(1)[1]).get("productContentTypeId"));
    }

    @Test
    void photoOrderMustListEachSlotOnce() throws Exception {
        List<String> current = List.of("MAIN", "ADDITIONAL_IMAGE_1", "ADDITIONAL_IMAGE_2");
        assertEquals(List.of("ADDITIONAL_IMAGE_2", "MAIN", "ADDITIONAL_IMAGE_1"),
                CatalogMcp.reorderSlots(current, List.of("ADDITIONAL_IMAGE_2", "MAIN", "ADDITIONAL_IMAGE_1")));
        assertThrows(McpToolException.class, () -> CatalogMcp.reorderSlots(current, List.of("MAIN", "ADDITIONAL_IMAGE_1")));
        assertThrows(McpToolException.class, () -> CatalogMcp.reorderSlots(current, List.of("MAIN", "MAIN", "ADDITIONAL_IMAGE_1")));
        assertThrows(McpToolException.class, () -> CatalogMcp.reorderSlots(current, List.of("MAIN", "ADDITIONAL_IMAGE_1", "ADDITIONAL_IMAGE_4")));
    }
}
