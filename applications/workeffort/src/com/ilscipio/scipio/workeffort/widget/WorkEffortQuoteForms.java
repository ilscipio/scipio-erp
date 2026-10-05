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
package com.ilscipio.scipio.workeffort.widget;

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
public class WorkEffortQuoteForms {

    @Form(
        name = "ListWorkEffortQuotes",
        location = "component://workeffort/widget/WorkEffortQuoteForms.xml",
        type = FormType.LIST,
        target = "ListWorkEffortQuotes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "quoteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/EditQuote", urlMode = UrlMode.INTER_APP, description = "${quoteId}", parameters = {@ParameterDef(paramName = "quoteId")})),
            @FormField(name = "quoteName", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "statusItemDescription", display = @DisplayField),
            @FormField(name = "issueDate", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortQuote", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "quoteId")}))
        }
    )
    public interface ListWorkEffortQuotes {}

    @Form(
        name = "AddWorkEffortQuote",
        location = "component://workeffort/widget/WorkEffortQuoteForms.xml",
        target = "createWorkEffortQuote",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "quoteId", lookup = @LookupField(targetFormName = "LookupQuote")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortQuote {}

    @Form(
        name = "ListWorkEffortQuoteItems",
        location = "component://workeffort/widget/WorkEffortQuoteForms.xml",
        type = FormType.LIST,
        target = "ListWorkEffortQuoteItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "quoteId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/EditQuote", urlMode = UrlMode.INTER_APP, description = "${quoteId}", parameters = {@ParameterDef(paramName = "quoteId")})),
            @FormField(name = "quoteItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/EditQuoteItem", urlMode = UrlMode.INTER_APP, description = "${quoteItemSeqId}", parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")})),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "custRequestId", display = @DisplayField),
            @FormField(name = "custRequestItemSeqId", display = @DisplayField),
            @FormField(name = "estimatedDeliveryDate", display = @DisplayField),
            @FormField(name = "comments", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteWorkEffortQuoteItem", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId"), @ParameterDef(paramName = "quoteId"), @ParameterDef(paramName = "quoteItemSeqId")}))
        }
    )
    public interface ListWorkEffortQuoteItems {}

    @Form(
        name = "AddWorkEffortQuoteItem",
        location = "component://workeffort/widget/WorkEffortQuoteForms.xml",
        target = "createWorkEffortQuoteItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", hidden = @HiddenField),
            @FormField(name = "quoteId", lookup = @LookupField(targetFormName = "LookupQuote")),
            @FormField(name = "quoteItemSeqId", lookup = @LookupField(targetFormName = "LookupQuoteItem")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddWorkEffortQuoteItem {}

}
