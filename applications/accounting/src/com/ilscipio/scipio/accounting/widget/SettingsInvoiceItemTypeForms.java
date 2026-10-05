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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SettingsInvoiceItemTypeForms {

    @Form(
        name = "ListInvoiceItemType",
        location = "component://accounting/widget/settings/InvoiceItemTypeForms.xml",
        type = FormType.LIST,
        target = "updateInvoiceItemType",
        listName = "invoiceItemTypes",
        paginateTarget = "editInvoiceItemType",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "invoiceItemTypeId", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "defaultGlAccountId", parameterName = "defaultGlAccountId", title = "${uiLabelMap.ProductGlAccount}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "glAccounts", keyName = "glAccountId", description = "${glAccountId} : ${accountName}"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListInvoiceItemType {}

}
