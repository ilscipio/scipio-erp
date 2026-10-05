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
package com.ilscipio.scipio.setup.controller;

import com.ilscipio.scipio.ce.webapp.control.def.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import com.ilscipio.scipio.setup.SetupEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Controller {

    @View(
        name = "initialsetup",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#InitialSetup",
        controller = "setup"
    )
    public static final String VIEW_INITIALSETUP = "initialsetup";

    @View(
        name = "showMessage",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#nopartyAcctgPreference",
        controller = "setup"
    )
    public static final String VIEW_SHOWMESSAGE = "showMessage";

    @View(
        name = "EditFacility",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditFacility",
        controller = "setup"
    )
    public static final String VIEW_EDITFACILITY = "EditFacility";

    @View(
        name = "EditProductStore",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditProductStore",
        controller = "setup"
    )
    public static final String VIEW_EDITPRODUCTSTORE = "EditProductStore";

    @View(
        name = "EntityExportAll",
        type = "screen",
        page = "component://setup/widget/CommonScreens.xml#EntityExportAll",
        controller = "setup"
    )
    public static final String VIEW_ENTITYEXPORTALL = "EntityExportAll";

    @View(
        name = "EditWebSite",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditWebSite",
        controller = "setup"
    )
    public static final String VIEW_EDITWEBSITE = "EditWebSite";

    @View(
        name = "firstcustomer",
        type = "screen",
        page = "component://setup/widget/ProfileScreens.xml#FirstCustomer",
        controller = "setup"
    )
    public static final String VIEW_FIRSTCUSTOMER = "firstcustomer";

    @View(
        name = "firstproduct",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditProdCatalog",
        controller = "setup"
    )
    public static final String VIEW_FIRSTPRODUCT = "firstproduct";

    @View(
        name = "EditCategory",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditCategory",
        controller = "setup"
    )
    public static final String VIEW_EDITCATEGORY = "EditCategory";

    @View(
        name = "EditProduct",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditProduct",
        controller = "setup"
    )
    public static final String VIEW_EDITPRODUCT = "EditProduct";

    @View(
        name = "editcontactmech",
        type = "screen",
        page = "component://setup/widget/PartyScreens.xml#editcontactmech",
        controller = "setup"
    )
    public static final String VIEW_EDITCONTACTMECH = "editcontactmech";

    @View(
        name = "SetupOrganization",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupOrganization",
        controller = "setup"
    )
    public static final String VIEW_SETUPORGANIZATION = "SetupOrganization";

    @View(
        name = "SetupUser",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupUser",
        controller = "setup"
    )
    public static final String VIEW_SETUPUSER = "SetupUser";

    @View(
        name = "SetupAccounting",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupAccounting",
        controller = "setup"
    )
    public static final String VIEW_SETUPACCOUNTING = "SetupAccounting";

    @View(
        name = "SetupFacility",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupFacility",
        controller = "setup"
    )
    public static final String VIEW_SETUPFACILITY = "SetupFacility";

    @View(
        name = "SetupCatalog",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupCatalog",
        controller = "setup"
    )
    public static final String VIEW_SETUPCATALOG = "SetupCatalog";

    @View(
        name = "SetupStore",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupStore",
        controller = "setup"
    )
    public static final String VIEW_SETUPSTORE = "SetupStore";

    @View(
        name = "SetupFinished",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupFinished",
        controller = "setup"
    )
    public static final String VIEW_SETUPFINISHED = "SetupFinished";

    @View(
        name = "SetupError",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupError",
        controller = "setup"
    )
    public static final String VIEW_SETUPERROR = "SetupError";

    @View(
        name = "SetupGlAccountsTab",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditGLAccountTree",
        controller = "setup"
    )
    public static final String VIEW_SETUPGLACCOUNTSTAB = "SetupGlAccountsTab";

    @View(
        name = "SetupFiscalPeriodsTab",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditFiscalPeriods",
        controller = "setup"
    )
    public static final String VIEW_SETUPFISCALPERIODSTAB = "SetupFiscalPeriodsTab";

    @View(
        name = "SetupAccountingTransactionsTab",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditAccountingTransactions",
        controller = "setup"
    )
    public static final String VIEW_SETUPACCOUNTINGTRANSACTIONSTAB = "SetupAccountingTransactionsTab";

    @View(
        name = "SetupJournalTab",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditJournals",
        controller = "setup"
    )
    public static final String VIEW_SETUPJOURNALTAB = "SetupJournalTab";

    @View(
        name = "SetupTaxAuthTab",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#EditTaxAuthorities",
        controller = "setup"
    )
    public static final String VIEW_SETUPTAXAUTHTAB = "SetupTaxAuthTab";

    @View(
        name = "SetupAccountingTabError",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupAccountingTabError",
        controller = "setup"
    )
    public static final String VIEW_SETUPACCOUNTINGTABERROR = "SetupAccountingTabError";

    @View(
        name = "main",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupOrganizationMain",
        controller = "setup"
    )
    public static final String VIEW_MAIN = "main";

    @View(
        name = "SetupOrganizationMain",
        type = "screen",
        page = "component://setup/widget/SetupScreens.xml#SetupOrganizationMain",
        controller = "setup"
    )
    public static final String VIEW_SETUPORGANIZATIONMAIN = "SetupOrganizationMain";

    @View(
        name = "LookupContent",
        type = "screen",
        page = "component://content/widget/content/ContentScreens.xml#LookupContent",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPCONTENT = "LookupContent";

    @View(
        name = "LookupPartyName",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

    @View(
        name = "LookupProduct",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

    @View(
        name = "LookupSupplierProduct",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupSupplierProduct",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPSUPPLIERPRODUCT = "LookupSupplierProduct";

    @View(
        name = "LookupVariantProduct",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

    @View(
        name = "LookupVirtualProduct",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupVirtualProduct",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPVIRTUALPRODUCT = "LookupVirtualProduct";

    @View(
        name = "LookupProductCategory",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

    @View(
        name = "LookupProductFeature",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

    @View(
        name = "LookupProductStore",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupProductStore",
        controller = "setup"
    )
    public static final String VIEW_LOOKUPPRODUCTSTORE = "LookupProductStore";

    @View(
        name = "DatevImportResult",
        type = "screen",
        page = "component://accounting/widget/external/ExternalAccountingScreens.xml#DatevImportResult",
        controller = "setup"
    )
    public static final String VIEW_DATEVIMPORTRESULT = "DatevImportResult";

    @Request(
        uri = "error",
        controller = "setup"
    )
    @Response(name = "success", type = "view", value = "SetupError")
    public interface Error {}

    @Request(
        uri = "main",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "setupWizardStart")
    public interface Main {}

    @Request(
        uri = "initialsetup",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "setupWizardStart")
    public interface Initialsetup {}

    @Request(
        uri = "editcontactmech",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "editcontactmech", saveCurrentView = "true")
    public interface Editcontactmech {}

    @Request(
        uri = "createContactMech",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "editcontactmech")
    @Response(name = "error", type = "view", value = "editcontactmech")
    @Event(type = "service", invoke = "createPartyContactMech")
    public static String createContactMech(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "updateContactMech",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "editcontactmech")
    @Response(name = "error", type = "view", value = "editcontactmech")
    @Event(type = "service", invoke = "updatePartyContactMech")
    public static String updateContactMech(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "deleteContactMech",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view-home", value = "editcontactmech")
    @Response(name = "error", type = "view-home", value = "editcontactmech")
    @Event(type = "service", invoke = "deletePartyContactMech")
    public static String deleteContactMech(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "createPostalAddress",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "editcontactmech")
    @Response(name = "error", type = "view", value = "editcontactmech")
    @Event(type = "service", invoke = "createPartyPostalAddress")
    public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupWizard",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    public interface SetupWizard {}

    @Request(
        uri = "setupWizardStart",
        controller = "setup"
    )
    @Response(name = "success", type = "request", value = "setupOrganization")
    public interface SetupWizardStart {}

    @Request(
        uri = "setupOrganization",
        controller = "setup"
    )
    @Response(name = "success", type = "view", value = "SetupOrganization", saveHomeView = "true")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupOrganization(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "setupCreateOrganization",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupOrganization")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "createOrganization")
    public static String setupCreateOrganization(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateOrganization",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupOrganization")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "updateOrganization")
    public static String setupUpdateOrganization(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUser",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupUser", saveHomeView = "true")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupUser(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "setupCreateUser",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupUser")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "createUser")
    public static String setupCreateUser(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateUser",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupUser")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "updateUser")
    public static String setupUpdateUser(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupAccounting",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccounting")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupAccounting(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "importDefaultGL",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccounting")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "importDefaultGL")
    public static String importDefaultGL(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupAddGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccounting")
    @Response(name = "error", type = "request", value = "setupAccounting")
    public interface SetupAddGlAccount {}

    @Request(
        uri = "setupEditGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    public interface SetupEditGlAccount {}

    @Request(
        uri = "setupCreateGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createGlAccount")
    public static String setupCreateGlAccount(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "updateGlAccount")
    public static String setupUpdateGlAccount(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupDeleteGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    public interface SetupDeleteGlAccount {}

    @Request(
        uri = "setupAssignGlAccount",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    public interface SetupAssignGlAccount {}

    @Request(
        uri = "setupImportGlAccounts",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccounting")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "importGlAccounts")
    public static String setupImportGlAccounts(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupGlAccountsTab",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupGlAccountsTab", allowViewSave = "false")
    @Response(name = "error", type = "view", value = "SetupAccountingTabError")
    public interface SetupGlAccountsTab {}

    @Request(
        uri = "setupFiscalPeriodsTab",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupFiscalPeriodsTab", allowViewSave = "false")
    @Response(name = "error", type = "view", value = "SetupAccountingTabError")
    public interface SetupFiscalPeriodsTab {}

    @Request(
        uri = "setupAccountingTransactionsTab",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccountingTransactionsTab", allowViewSave = "false")
    @Response(name = "error", type = "view", value = "SetupAccountingTabError")
    public interface SetupAccountingTransactionsTab {}

    @Request(
        uri = "setupJournalTab",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupJournalTab", allowViewSave = "false")
    @Response(name = "error", type = "view", value = "SetupAccountingTabError")
    public interface SetupJournalTab {}

    @Request(
        uri = "setupTaxAuthTab",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupTaxAuthTab", allowViewSave = "false")
    @Response(name = "error", type = "view", value = "SetupAccountingTabError")
    public interface SetupTaxAuthTab {}

    @Request(
        uri = "setupCreateTimePeriod",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createCustomTimePeriod")
    public static String setupCreateTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateTimePeriod",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "updateCustomTimePeriod")
    public static String setupUpdateTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupDeleteTimePeriod",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "deleteCustomTimePeriod")
    public static String setupDeleteTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateAcctgTransType",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createAcctgTransType")
    public static String setupCreateAcctgTransType(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateAcctgTransEntryType",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createAcctgTransEntryType")
    public static String setupCreateAcctgTransEntryType(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateGlJournal",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createGlJournal")
    public static String setupCreateGlJournal(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateTaxAuth",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "createTaxAuthority")
    public static String setupCreateTaxAuth(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateAccountingPreferences",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "createPartyAcctgPreference")
    public static String setupCreateAccountingPreferences(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateAccountingPreferences",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "service", invoke = "setupUpdatePartyAcctgPreference")
    public static String setupUpdateAccountingPreferences(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupExportDatevDataCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupAccounting")
    @Response(name = "error", type = "request", value = "setupAccounting")
    @Event(type = "simple", path = "component://accounting/script/com/ilscipio/scipio/accounting/datev/DatevEvents.xml", invoke = "exportDatevDataCategory")
    public static String setupExportDatevDataCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupFacility",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupFacility")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupFacility(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "setupCreateFacility",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupFacility")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "createFacilityAndContactMech")
    public static String setupCreateFacility(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateFacility",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupFacility")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "updateFacilityAndContactMech")
    public static String setupUpdateFacility(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCatalog",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupCatalog")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupCatalog(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "setupCreateCatalog",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "createProdCatalogAndStoreAssocVersatile")
    public static String setupCreateCatalog(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateCatalog",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "updateProdCatalogAndStoreAssocVersatile")
    public static String setupUpdateCatalog(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupDeleteCatalog",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "deleteProdCatalogAndStoreAssocVersatile")
    public static String setupDeleteCatalog(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupAddCatalog",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "addProdCatalogStoreAssocVersatile")
    public static String setupAddCatalog(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "createProductCategoryAndCatAssocVersatile")
    public static String setupCreateCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "updateProductCategoryAndCatAssocVersatile")
    public static String setupUpdateCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupDeleteCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "deleteProductCategoryAndCatAssocVersatile")
    public static String setupDeleteCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupAddCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "addProductCategoryCatAssocVersatile")
    public static String setupAddCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCopyCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "copyProductCategoryCatAssocVersatile")
    public static String setupCopyCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupMoveCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "moveProductCategoryCatAssocVersatile")
    public static String setupMoveCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCreateProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "createProductAndCatAssocVersatile")
    public static String setupCreateProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "updateProductAndCatAssocVersatile")
    public static String setupUpdateProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupDeleteProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "deleteProductAndCatAssocVersatile")
    public static String setupDeleteProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupAddProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "addProductCatAssocVersatile")
    public static String setupAddProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupCopyProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "copyProductCatAssocVersatile")
    public static String setupCopyProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupMoveProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupCatalog")
    @Event(type = "service", invoke = "moveProductCatAssocVersatile")
    public static String setupMoveProduct(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupStore",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupStore")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupStore(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "setupCreateStore",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupStore")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "createProductStoreAndWebSite")
    public static String setupCreateStore(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupUpdateStore",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "nextSetupStep")
    @Response(name = "error", type = "request", value = "setupStore")
    @Event(type = "simple", path = "component://setup/script/com/ilscipio/scipio/setup/SetupEvents.xml", invoke = "updateProductStoreAndWebSite")
    public static String setupUpdateStore(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setProductStoreDefaultWebSite",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "setupStore")
    @Response(name = "error", type = "request", value = "setupStore")
    @Event(type = "service", invoke = "setProductStoreDefaultWebSite")
    public static String setProductStoreDefaultWebSite(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupFinished",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "SetupFinished")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String setupFinished(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.setEffectiveSetupStep
        return SetupEvents.setEffectiveSetupStep(request, response);
    }

    @Request(
        uri = "nextSetupStep",
        controller = "setup",
        directRequest = "false"
    )
    @Response(name = "success", type = "view", value = "SetupError")
    @Response(name = "finished", type = "view", value = "SetupFinished")
    @Response(name = "organization", type = "view", value = "SetupOrganization", saveHomeView = "true")
    @Response(name = "user", type = "view", value = "SetupUser", saveHomeView = "true")
    @Response(name = "accounting", type = "view", value = "SetupAccounting")
    @Response(name = "facility", type = "view", value = "SetupFacility")
    @Response(name = "catalog", type = "view", value = "SetupCatalog")
    @Response(name = "store", type = "view", value = "SetupStore")
    @Response(name = "error", type = "view", value = "SetupError")
    public static String nextSetupStep(HttpServletRequest request, HttpServletResponse response) {
        // Delegates to: com.ilscipio.scipio.setup.SetupEvents.getNextSetupStep
        return SetupEvents.getNextSetupStep(request, response);
    }

    @Request(
        uri = "getProductCategoryContentLocalizedSimpleTextViews",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getProductCategoryContentLocalizedSimpleTextViews")
    public static String getProductCategoryContentLocalizedSimpleTextViews(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "getProductContentLocalizedSimpleTextViews",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getProductContentLocalizedSimpleTextViews")
    public static String getProductContentLocalizedSimpleTextViews(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "getProductCategoryExtendedData",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getProductCategoryExtendedDataVersatile")
    public static String getProductCategoryExtendedData(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "getProductExtendedData",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getProductExtendedDataVersatile")
    public static String getProductExtendedData(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "getGlAccountExtendedData",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getGlAccountAndAssocs")
    public static String getGlAccountExtendedData(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "getTimePeriodExtendedData",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "json")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "service", invoke = "getTimePeriod")
    public static String getTimePeriodExtendedData(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "setupImportDatevDataCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "DatevImportResult")
    @Response(name = "error", type = "request", value = "json")
    @Event(type = "simple", path = "component://accounting/script/com/ilscipio/scipio/accounting/datev/DatevEvents.xml", invoke = "importDatevDataCategory")
    public static String setupImportDatevDataCategory(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "LookupContent",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupContent")
    public interface LookupContent {}

    @Request(
        uri = "LookupPartyName",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupPartyName")
    public interface LookupPartyName {}

    @Request(
        uri = "LookupProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupProduct")
    public interface LookupProduct {}

    @Request(
        uri = "LookupSupplierProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupSupplierProduct")
    public interface LookupSupplierProduct {}

    @Request(
        uri = "LookupVariantProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupVariantProduct")
    public interface LookupVariantProduct {}

    @Request(
        uri = "LookupVirtualProduct",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupVirtualProduct")
    public interface LookupVirtualProduct {}

    @Request(
        uri = "LookupProductCategory",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupProductCategory")
    public interface LookupProductCategory {}

    @Request(
        uri = "LookupProductFeature",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupProductFeature")
    public interface LookupProductFeature {}

    @Request(
        uri = "LookupProductStore",
        controller = "setup",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "LookupProductStore")
    public interface LookupProductStore {}
}
