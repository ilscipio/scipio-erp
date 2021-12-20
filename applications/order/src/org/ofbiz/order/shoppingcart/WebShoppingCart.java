/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.order.shoppingcart;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * WebShoppingCart.java
 *
 * Extension of {@link org.ofbiz.order.shoppingcart.ShoppingCart ShoppingCart}
 * class which provides web presentation layer specific functionality
 * related specifically to user session information.
 * <p>
 * SCIPIO: IMPORTANT: 2018-11-22: Any event or code which modifies the main shopping cart stored in session ("shoppingCart") 
 * must now wrap its update code in a {@link CartUpdate#updateSection} or {@link CartSync#synchronizedSection} section.
 * The main instance currently stored in session/request attribute is now considered immutable and can only be updated using
 * an update section (which creates a modifiable copy that may be committed to replace the main session cart instance).
 *
 * @see CartUpdate
 * @see CartSync
 */
@SuppressWarnings("serial")
public class WebShoppingCart extends ShoppingCart {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Full constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public WebShoppingCart(HttpServletRequest request, Locale locale, String currencyUom) {
        // for purchase orders, bill to customer partyId must be set - otherwise, no way to know who we're purchasing for.  supplierPartyId is furnished
        // by order manager for PO entry.
        // TODO: refactor constructor and the getCartObject method which calls them to multiple constructors for different types of orders
        super((Delegator)request.getAttribute("delegator"), ProductStoreWorker.getProductStoreId(request),
                WebSiteWorker.getWebSiteId(request), (locale != null ? locale : ProductStoreWorker.getStoreLocale(request)),
                (currencyUom != null ? currencyUom : ProductStoreWorker.getStoreCurrencyUomId(request)),
                request.getParameter("billToCustomerPartyId"),
                (request.getParameter("supplierPartyId") != null ? request.getParameter("supplierPartyId") : request.getParameter("billFromVendorPartyId")));

        HttpSession session = request.getSession(true);
        this.userLogin = (GenericValue) session.getAttribute("userLogin");
        this.autoUserLogin = (GenericValue) session.getAttribute("autoUserLogin");
        this.orderPartyId = (String) session.getAttribute("orderPartyId");

        // SCIPIO: Determine if allow ship estimates (WARN: do not determine this from request params; server-side info only!)
        Boolean allowMissingShipEstimates = UtilMisc.booleanValue(request.getServletContext().getAttribute("orderAllowMissingShipEstimates"));
        if (allowMissingShipEstimates != null) {
            this.setAllowMissingShipEstimates(allowMissingShipEstimates);
        }
    }

    /**
     * Common constructor.
     * <p>
     * SCIPIO: NOTE: 2018-11-30: This constructor should ONLY be inside a {@link CartSync#synchronizedSection(HttpServletRequest)}
     * block, because it modifies session variables that must match the cart contents.
     */
    public WebShoppingCart(HttpServletRequest request) {
        this(request, UtilHttp.getLocale(request), UtilHttp.getCurrencyUom(request));
    }

    /** Creates a new cloned ShoppingCart Object. */
    public WebShoppingCart(ShoppingCart cart) {
        super(cart);
    }

    /** SCIPIO: Creates a new cloned ShoppingCart Object, exact copy. */
    public WebShoppingCart(ShoppingCart cart, boolean exactCopy) {
        super(cart, exactCopy);
    }

    // SCIPIO: 2.1.0: Added missing constructor
    public WebShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom, String billToCustomerPartyId, String billFromVendorPartyId) {
        super(delegator, productStoreId, webSiteId, locale, currencyUom, billToCustomerPartyId, billFromVendorPartyId);
    }

    // SCIPIO: 2.1.0: Added missing constructor
    public WebShoppingCart(Delegator delegator, String productStoreId, String webSiteId, Locale locale, String currencyUom) {
        super(delegator, productStoreId, webSiteId, locale, currencyUom);
    }

    // SCIPIO: 2.1.0: Added missing constructor
    public WebShoppingCart(Delegator delegator, String productStoreId, Locale locale, String currencyUom) {
        super(delegator, productStoreId, locale, currencyUom);
    }

    /** SCIPIO: Performs a copy of the instance. Added 2019-09-05. */
    @Override
    public WebShoppingCart copy(boolean exactCopy) {
        return new WebShoppingCart(this, exactCopy);
    }
}
