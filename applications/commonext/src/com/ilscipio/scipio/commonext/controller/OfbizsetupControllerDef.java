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
package com.ilscipio.scipio.commonext.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OfbizsetupControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "initialsetup",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#InitialSetup",
        controller = "ofbizsetup"
    )
    public static final String VIEW_INITIALSETUP = "initialsetup";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showMessage",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#nopartyAcctgPreference",
        controller = "ofbizsetup"
    )
    public static final String VIEW_SHOWMESSAGE = "showMessage";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditFacility",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditFacility",
        controller = "ofbizsetup"
    )
    public static final String VIEW_EDITFACILITY = "EditFacility";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProductStore",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditProductStore",
        controller = "ofbizsetup"
    )
    public static final String VIEW_EDITPRODUCTSTORE = "EditProductStore";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EntityExportAll",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/CommonScreens.xml#EntityExportAll",
        controller = "ofbizsetup"
    )
    public static final String VIEW_ENTITYEXPORTALL = "EntityExportAll";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditWebSite",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditWebSite",
        controller = "ofbizsetup"
    )
    public static final String VIEW_EDITWEBSITE = "EditWebSite";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "firstcustomer",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/ProfileScreens.xml#FirstCustomer",
        controller = "ofbizsetup"
    )
    public static final String VIEW_FIRSTCUSTOMER = "firstcustomer";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "firstproduct",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditProdCatalog",
        controller = "ofbizsetup"
    )
    public static final String VIEW_FIRSTPRODUCT = "firstproduct";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditCategory",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditCategory",
        controller = "ofbizsetup"
    )
    public static final String VIEW_EDITCATEGORY = "EditCategory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditProduct",
        type = "screen",
        page = "component://commonext/widget/ofbizsetup/SetupScreens.xml#EditProduct",
        controller = "ofbizsetup"
    )
    public static final String VIEW_EDITPRODUCT = "EditProduct";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupFacility",
        type = "screen",
        page = "component://product/widget/facility/LookupScreens.xml#LookupFacility",
        controller = "ofbizsetup"
    )
    public static final String VIEW_LOOKUPFACILITY = "LookupFacility";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPartyName",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
        controller = "ofbizsetup"
    )
    public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

    @Request(
        uri = "main",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "initialsetup", saveHomeView = "true")
    public interface Main {}

    @Request(
        uri = "initialsetup",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "initialsetup", saveHomeView = "true")
    public interface Initialsetup {}

    @Request(
        uri = "showMessage",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "showMessage")
    public interface ShowMessage {}

    @Request(
        uri = "updatePartyGroup",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "initialsetup")
    @Response(name = "error", type = "view", value = "EditPartyGroup")
    @Event(type = "service", invoke = "updatePartyGroup")
    public static String updatePartyGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "EntityExportAll",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "EntityExportAll")
    @Response(name = "error", type = "view", value = "EntityExportAll")
    public interface EntityExportAll {}

    @Request(
        uri = "entityExportAll",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "EntityExportAll")
    @Response(name = "error", type = "view", value = "EntityExportAll")
    @Event(type = "service", invoke = "entityExportAll")
    public static String entityExportAll_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "FindProductStore",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "EditProductStore")
    public interface FindProductStore {}

    @Request(
        uri = "EditProductStore",
        controller = "ofbizsetup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "EditProductStore")
    public interface EditProductStore {}


    // Auto-generated split (Part 2)
    public static class Part2 {
        @Request(
            uri = "createProductStore",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStore")
        @Response(name = "error", type = "view", value = "EditProductStore")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createProductStoreWithDefaultSetting")
        public static String createProductStore(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductStore",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductStore")
        @Response(name = "error", type = "view", value = "EditProductStore")
        @Event(type = "service", invoke = "updateProductStore")
        public static String updateProductStore(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createOrganization",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "initialsetup")
        @Response(name = "error", type = "view", value = "initialsetup")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createOrganization")
        public static String createOrganization(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "OrganizationToComplete",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "url", value = "/catalog")
        @Response(name = "error", type = "view", value = "initialsetup")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "setupDefaultGeneralLedger")
        public static String organizationToComplete(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewFacility",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        public interface ViewFacility {}

        @Request(
            uri = "EditFacility",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        public interface EditFacility {}

        @Request(
            uri = "CreateFacility",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        @Response(name = "error", type = "view", value = "EditFacility")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createFacilityAndContactMech")
        public static String createFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateFacility",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFacility")
        @Response(name = "error", type = "view", value = "EditFacility")
        @Event(type = "service", invoke = "updateFacility")
        public static String updateFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindWebSite",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        public interface FindWebSite {}

        @Request(
            uri = "EditWebSite",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        public interface EditWebSite {}

        @Request(
            uri = "createWebSite",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        @Response(name = "error", type = "view", value = "EditWebSite")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createDefaultWebSite")
        public static String createWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateWebSite",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditWebSite")
        @Response(name = "error", type = "view", value = "EditWebSite")
        @Event(type = "service", invoke = "updateWebSite")
        public static String updateWebSite(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "firstproduct",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "firstproduct")
        public interface Firstproduct {}

        @Request(
            uri = "createProdCatalog",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "firstproduct")
        @Response(name = "error", type = "view", value = "firstproduct")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createProdCatalogAndProductStoreCatalog")
        public static String createProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProdCatalog",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "firstproduct")
        @Response(name = "error", type = "view", value = "firstproduct")
        @Event(type = "service", invoke = "updateProdCatalog")
        public static String updateProdCatalog(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditCategory",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        public interface EditCategory {}

        @Request(
            uri = "createProductCategory",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        @Response(name = "error", type = "view", value = "EditCategory")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createProductCategoryAndAddToProdCatalog")
        public static String createProductCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategory",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCategory")
        @Response(name = "error", type = "view", value = "EditCategory")
        @Event(type = "service", invoke = "updateProductCategory")
        public static String updateProductCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditProduct",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        public interface EditProduct {}

        @Request(
            uri = "createUpdateProduct",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProduct")
        @Response(name = "error", type = "view", value = "EditProduct")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createUpdateProductInCategory")
        public static String createUpdateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @Request(
            uri = "firstcustomer",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "firstcustomer", saveHomeView = "true")
        public interface Firstcustomer {}

        @Request(
            uri = "createCustomer",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "firstcustomer")
        @Response(name = "error", type = "view", value = "firstcustomer")
        @Event(type = "simple", path = "component://commonext/script/org/ofbiz/setup/SetupEvents.xml", invoke = "createCustomer")
        public static String createCustomer(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupFacility",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacility")
        public interface LookupFacility {}

        @Request(
            uri = "LookupPartyName",
            controller = "ofbizsetup",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}


    }
}
