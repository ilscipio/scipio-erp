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
package com.ilscipio.scipio.accounting.widget;

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * E-invoice screens (EN 16931): the e-invoice tab of an invoice, the import of received e-invoices, and the
 * e-invoice settings of a party.
 */
public class EInvoiceScreens {

    @Screen(name = "InvoiceEInvoice", location = "component://accounting/widget/invoice/EInvoiceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "EInvoiceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "EInvoice")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "einvoice")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/EInvoice.groovy")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/EInvoice.ftl")
            })
        }
    )
    public interface InvoiceEInvoice {}

    @Screen(name = "EInvoiceImport", location = "component://accounting/widget/invoice/EInvoiceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "EInvoiceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "importEInvoice")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EInvoiceImportTitle")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetMyCompany.groovy")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/EInvoiceImport.ftl")
            })
        }
    )
    public interface EInvoiceImport {}

    @Screen(name = "EditPartyEInvoicePref", location = "component://accounting/widget/invoice/EInvoiceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "EInvoiceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "EInvoiceSettings")
    @Action(type = ActionType.SET, field = "invoiceId", fromField = "parameters.invoiceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Invoice", valueField = "invoice")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "einvoice")
    @Action(type = ActionType.SCRIPT, location = "component://accounting/webapp/accounting/WEB-INF/actions/invoice/EditPartyEInvoicePref.groovy")
    @DecoratorScreen(
        name = "CommonInvoicesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://accounting/webapp/accounting/invoice/EditPartyEInvoicePref.ftl")
            })
        }
    )
    public interface EditPartyEInvoicePref {}
}
