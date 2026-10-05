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
package com.ilscipio.scipio.channel.core;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Variation listings (W1-08 decision 1).
 *
 * <p>The connector contract (W1-09a) has one SKU for each {@code upsertListing} call and no variant type; it is idempotent
 * by SKU. So a variation product becomes one {@link Listing} per variant SKU. The rows of a group carry the id of the
 * virtual product ({@link Listing#variationGroupId}) and the axes ({@link Listing#variationAxesJson}, for example
 * {"Size":"16 oz","Color":"Red"}). The hub gives them to the connector as attributes of the ProductView:
 * {@link #GROUP_ATTRIBUTE} and {@link #AXIS_PREFIX} plus the axis name. A connector that needs a parent (Amazon parent and
 * child, eBay inventory item group) builds it from the group id. Stock, price, state and errors stay for each variant.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class VariationPlanner {
    public static final String GROUP_ATTRIBUTE = "variation.group";
    public static final String AXIS_PREFIX = "variation.axis.";

    private VariationPlanner() {
    }

    /** One variant of a virtual product. */
    public static final class Variant {
        public final String productId;
        public final String sku;
        /** Axis name to value, in the order of the axes. */
        public final Map<String, String> axes;

        public Variant(String productId, String sku, Map<String, String> axes) {
            this.productId = Listing.req(productId, "productId");
            this.sku = Listing.req(sku, "sku");
            this.axes = new LinkedHashMap<>(axes);
        }
    }

    /**
     * Returns the draft listings of a group for a channel. An existing listing keeps its data and gets the group data.
     *
     * @throws IllegalArgumentException for a duplicate SKU, no variant, or variants with different axes
     */
    public static List<Listing> plan(String parentProductId, List<Variant> variants, String channelId,
            ChannelStore store) {
        Listing.req(parentProductId, "parentProductId");
        if (variants == null || variants.isEmpty()) {
            throw new IllegalArgumentException("A variation product needs at least one variant");
        }
        Set<String> skus = new HashSet<>();
        Set<String> axisNames = variants.get(0).axes.keySet();
        if (axisNames.isEmpty()) {
            throw new IllegalArgumentException("A variant needs at least one axis");
        }
        for (Variant v : variants) {
            if (!skus.add(v.sku)) {
                throw new IllegalArgumentException("Two variants have the SKU " + v.sku);
            }
            if (!v.axes.keySet().equals(axisNames)) {
                throw new IllegalArgumentException("Variant " + v.sku + " has the axes " + v.axes.keySet()
                        + " but the group has " + axisNames + ". Give each variant of a group the same axes.");
            }
            for (String value : v.axes.values()) {
                if (value == null || value.trim().isEmpty()) {
                    throw new IllegalArgumentException("Variant " + v.sku + " has an empty axis value");
                }
            }
        }
        // two variants with the same axis values cannot be told apart on the channel
        Set<String> combos = new HashSet<>();
        for (Variant v : variants) {
            if (!combos.add(v.axes.values().toString())) {
                throw new IllegalArgumentException("Two variants have the axis values " + v.axes.values());
            }
        }
        List<Listing> out = new ArrayList<>();
        for (Variant v : variants) {
            Listing l = store.listing(v.productId, channelId).orElseGet(() -> new Listing(v.productId, channelId));
            l.variationGroupId = parentProductId;
            l.variationAxesJson = FlatJson.write(v.axes);
            out.add(l);
        }
        return out;
    }

    /** The attributes that the hub adds to the ProductView of a listing; empty for a listing without a group. */
    public static Map<String, String> viewAttributes(Listing l) {
        Map<String, String> out = new LinkedHashMap<>();
        if (l.variationGroupId == null || l.variationGroupId.isEmpty()) {
            return out;
        }
        out.put(GROUP_ATTRIBUTE, l.variationGroupId);
        for (Map.Entry<String, String> e : FlatJson.read(l.variationAxesJson).entrySet()) {
            out.put(AXIS_PREFIX + e.getKey(), e.getValue());
        }
        return out;
    }
}
