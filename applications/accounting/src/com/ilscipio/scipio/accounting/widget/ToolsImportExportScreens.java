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
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ToolsImportExportScreens {

    @Screen(name = "ImportExport", location = "component://accounting/widget/tools/ImportExportScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "importExport")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonImportExport")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(labels = {
                    @Label(text = "${uiLabelMap.CommonThisIsAPlaceholder}")})})
        }
    )
    public interface ImportExport {}

    @Screen(name = "ImportExportInvoice", location = "component://accounting/widget/tools/ImportExportScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "importInvoice")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingInvoice")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "partyGroup", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.organizationPartyId")})
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonImport}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ImportInvoice", location = "component://accounting/widget/tools/ImportExportForms.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonExport}", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ExportInvoice", location = "component://accounting/widget/tools/ImportExportForms.xml"
            )})
        }
    )
    public interface ImportExportInvoice {}

    @Screen(name = "ExportInvoiceCsv", location = "component://accounting/widget/tools/ImportExportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ExportInvoiceCsv", location = "component://accounting/widget/tools/ImportExportForms.xml")}))
    public interface ExportInvoiceCsv {}

    @Screen(name = "ExportTransactions", location = "component://accounting/widget/tools/ImportExportScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "exportTransactions")
    @Action(type = ActionType.SET, field = "titleProperty", value = "AccountingAcctgTrans")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "partyGroup", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "parameters.organizationPartyId")})
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ExportTransactions", location = "component://accounting/widget/tools/ImportExportForms.xml"
            )})
        }
    )
    public interface ExportTransactions {}

    @Screen(name = "ExportTransactionCsv", location = "component://accounting/widget/tools/ImportExportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "organizationPartyId", fromField = "parameters.organizationPartyId")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ExportTransactionCsv", location = "component://accounting/widget/tools/ImportExportForms.xml")}))
    public interface ExportTransactionCsv {}

}
