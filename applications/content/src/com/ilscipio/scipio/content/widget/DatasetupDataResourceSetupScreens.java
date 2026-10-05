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
package com.ilscipio.scipio.content.widget;

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
public class DatasetupDataResourceSetupScreens {

    @Screen(name = "EditDataResourceType", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditDataResourceType")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateDataResourceType", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "DataResourceTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataResourceType", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataResourceType {}

    @Screen(name = "EditCharacterSet", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceCharacterSet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditCharacterSet")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateCharacterSet", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "DataResourceCharacterSetPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddCharacterSet", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditCharacterSet {}

    @Screen(name = "EditDataResourceTypeAttr", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceTypeAttr")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditDataResourceTypeAttr")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateDataResourceTypeAttr", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "DataResourceTypeAttrPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataResourceTypeAttr", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataResourceTypeAttr {}

    @Screen(name = "EditFileExtension", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceFileExtension")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditFileExtension")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateFileExtension", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "FileExtensionPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddFileExtension", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditFileExtension {}

    @Screen(name = "EditMetaDataPredicate", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceMetaDataPredicate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditMetaDataPredicate")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateMetaDataPredicate", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "MetaDataPredicatePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddMetaDataPredicate", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditMetaDataPredicate {}

    @Screen(name = "EditMimeType", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceMimeType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditMimeType")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateMimeType", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "DataResourceMimeTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddMimeType", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditMimeType {}

    @Screen(name = "EditMimeTypeHtmlTemplate", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceMimeTypeHtmlTemplate")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditMimeTypeHtmlTemplate")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateMimeTypeHtmlTemplate", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "MimeTypeHtmlTemplatePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "CreateMimeTypeHtmlTemplate", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditMimeTypeHtmlTemplate {}

    @Screen(name = "EditDataCategory", location = "component://content/widget/datasetup/DataResourceSetupScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditDataCategory")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/datasetup/DataCategoryPrep.groovy")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonDataResourceSetupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateDataCategory", location = "component://content/widget/datasetup/DataSetupForms.xml"
            )}, screenlets = {
                @Screenlet(name = "DataResourceCategoryPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataCategory", location = "component://content/widget/datasetup/DataSetupForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataCategory {}

}
