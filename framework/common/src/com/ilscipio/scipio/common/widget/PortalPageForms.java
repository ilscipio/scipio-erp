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
package com.ilscipio.scipio.common.widget;

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
public class PortalPageForms {

    @Form(
        name = "ListPortalPages",
        location = "component://common/widget/PortalPageForms.xml",
        type = FormType.LIST,
        listName = "portalPages",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "portalPageId", title = "${uiLabelMap.CommonEdit}", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "ManagePortalPages", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "top", title = " ", useWhen = "(ownerUserLoginId.equals(\"_NA_\"))||(itemIndex == 0)", hyperlink = @HyperlinkField),
            @FormField(name = "bot", title = " ", useWhen = "(ownerUserLoginId.equals(\"_NA_\"))||(itemIndex == listSize-1)", hyperlink = @HyperlinkField),
            @FormField(name = "up", title = " ", useWhen = "(ownerUserLoginId.equals(\"_NA_\"))||(itemIndex == 0)", hyperlink = @HyperlinkField),
            @FormField(name = "dwn", title = " ", useWhen = "(ownerUserLoginId.equals(\"_NA_\"))||(itemIndex == listSize-1)", hyperlink = @HyperlinkField),
            @FormField(name = "top", title = " ", useWhen = "(!ownerUserLoginId.equals(\"_NA_\"))&&(itemIndex > 0)", widgetStyle = "${styles.action_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updatePortalPageSeq", parameters = {@ParameterDef(paramName = "mode", value = "TOP"), @ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "bot", title = " ", useWhen = "(!ownerUserLoginId.equals(\"_NA_\"))&&(itemIndex < listSize-1)", widgetStyle = "${styles.action_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updatePortalPageSeq", parameters = {@ParameterDef(paramName = "mode", value = "BOT"), @ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "up", title = " ", useWhen = "(!ownerUserLoginId.equals(\"_NA_\"))&&(itemIndex > 0)", widgetStyle = "${styles.action_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updatePortalPageSeq", parameters = {@ParameterDef(paramName = "mode", value = "UP"), @ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "dwn", title = " ", useWhen = "(!ownerUserLoginId.equals(\"_NA_\"))&&(itemIndex < listSize-1)", widgetStyle = "${styles.action_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "updatePortalPageSeq", parameters = {@ParameterDef(paramName = "mode", value = "DWN"), @ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "portalPageName", title = "${uiLabelMap.CommonName}", useWhen = "ownerUserLoginId.equals(\"_NA_\")", display = @DisplayField),
            @FormField(name = "portalPageName", title = "${uiLabelMap.CommonName}", useWhen = "!ownerUserLoginId.equals(\"_NA_\")", idName = "portalPageName", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", useWhen = "ownerUserLoginId.equals(\"_NA_\")", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", useWhen = "!ownerUserLoginId.equals(\"_NA_\")", idName = "portalDescription", display = @DisplayField),
            @FormField(name = "originalPortalPageId", displayEntity = @DisplayEntityField(entityName = "PortalPage", keyFieldName = "portalPageId", description = "${portalPageName} [${portalPageId}]")),
            @FormField(name = "deleteAction", title = " ", useWhen = "!ownerUserLoginId.equals(\"_NA_\")", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePortalPage", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "deleteAction", title = " ", useWhen = "!ownerUserLoginId.equals(\"_NA_\")&&originalPortalPageId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePortalPage", description = "${uiLabelMap.CommonRevertPortalPage}", parameters = {@ParameterDef(paramName = "portalPageId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")}))
        }
    )
    public interface ListPortalPages {}

    @Form(
        name = "NewPortalPage",
        location = "component://common/widget/PortalPageForms.xml",
        target = "createPortalPage",
        fields = {
            @FormField(name = "parentPortalPageId", hidden = @HiddenField(value = "${parameters.parentPortalPageId}")),
            @FormField(name = "sequenceNum", hidden = @HiddenField(value = "${parameters.portalPagesSize+1}")),
            @FormField(name = "portalPageName", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "createAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface NewPortalPage {}

    @Form(
        name = "PortletCategoryAndPortlet",
        location = "component://common/widget/PortalPageForms.xml",
        type = FormType.LIST,
        listName = "portletCat",
        paginateTarget = "addPortlet",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "portletCategoryId", title = "Category", widgetStyle = "${styles.link_nav} ${styles.action_add}", hyperlink = @HyperlinkField(target = "addPortlet", description = "${portletCategoryId}", parameters = {@ParameterDef(paramName = "portletCategoryId"), @ParameterDef(paramName = "portalPortletId"), @ParameterDef(paramName = "portalPageId", fromField = "parameters.portalPageId"), @ParameterDef(paramName = "columnSeqId", fromField = "parameters.columnSeqId"), @ParameterDef(paramName = "parentPortalPageId", fromField = "parameters.parentPortalPageId")})),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        }
    )
    public interface PortletCategoryAndPortlet {}

    @Form(
        name = "PortletList",
        location = "component://common/widget/PortalPageForms.xml",
        type = FormType.LIST,
        listName = "portlets",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "portletName", widgetStyle = "${styles.link_nav_info_name} ${styles.action_view}", hyperlink = @HyperlinkField(target = "showHelp?helpTopic=HELP_${portalPortletId}", description = "${portletName}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface PortletList {}

    @Form(
        name = "FindGenericEntity",
        location = "component://common/widget/PortalPageForms.xml",
        target = "list${entity}",
        focusFieldName = "idName",
        fields = {
            @FormField(name = "idName", title = "${uiLabelMap.FormFieldTitle_${pkIdName}", text = @TextField(size = 16)),
            @FormField(name = "idName_op", hidden = @HiddenField(value = "contains")),
            @FormField(name = "idName_ic", hidden = @HiddenField(value = "Y")),
            @FormField(name = "description", text = @TextField(size = 16)),
            @FormField(name = "description_op", hidden = @HiddenField(value = "contains")),
            @FormField(name = "description_ic", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", hyperlink = @HyperlinkField(target = "javascript:ajaxUpdateArea('List${entity}Area', 'list${entity}', $(FindGenericEntity).serialize());", urlMode = UrlMode.PLAIN, description = "${uiLabelMap.CommonSearch}"))
        }
    )
    public interface FindGenericEntity {}

    @Form(
        name = "EditPortalPageColumnWidth",
        location = "component://common/widget/PortalPageForms.xml",
        target = "updatePortalPageColumnWidth",
        defaultMapName = "portalPageColumn",
        fields = {
            @FormField(name = "portalPageId", hidden = @HiddenField),
            @FormField(name = "columnSeqId", hidden = @HiddenField),
            @FormField(name = "columnWidthPixels", text = @TextField),
            @FormField(name = "columnWidthPercentage", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPortalPageColumnWidth {}

}
