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
public class ContentDataResourceScreens {

    @Screen(name = "FindDataResource", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findDataResource")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindDataResource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindDataResource")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "30")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditDataResource"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindDataResource", location = "component://content/widget/content/DataResourceForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDataResource", location = "component://content/widget/content/DataResourceForms.xml"
                        )}))})})
        }
    )
    public interface FindDataResource {}

    @Screen(name = "findDataResourceSearchResults", location = "component://content/widget/content/DataResourceScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupDataResource", location = "component://content/widget/content/DataResourceForms.xml")}))
    public interface findDataResourceSearchResults {}

    @Screen(name = "navigateDataResource", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNavigateDataResources")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "navigateDataResource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleNavigateDataResources")
    @Action(type = ActionType.ENTITY_AND, entityName = "DataCategory", list = "subCategories", fieldMaps = {@FieldMap(fieldName = "parentCategoryId", value = "ROOT")})
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(widgets = @InlineWidgets(containers = {
                    @Container(id = "cmsnav", style = "left", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.PageTitleNavigateDataResources}", includeScreens = {
                    @IncludeScreen(name = "navigateMenu", location = "component://content/widget/content/DataResourceScreens.xml"
                
                    )})}),
                    @Container(id = "content-main-section", style = "leftonly", containers = {
                        @Container2(id = "cmscontent", includeScreens = {
                            @IncludeScreen(name = "listDataResources", location = "component://content/widget/content/DataResourceScreens.xml"
                        )})})}), failWidgets = @InlineWidgets(containers = {
                            @Container(id = "norender", labels = {
                                @Label(text = "${uiLabelMap.ContentCMSNotExist}", style = "common-msg-result-norecord"
                            )})}))})
        }
    )
    public interface navigateDataResource {}

    @Screen(name = "navigateMenu", location = "component://content/widget/content/DataResourceScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/content/nav.ftl")}))
    public interface navigateMenu {}

    @Screen(name = "listDataResources", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleNavigateDataResources}", includeForms = {@IncludeForm(name = "ListDataResource", location = "component://content/widget/content/DataResourceForms.xml")})}))
    public interface listDataResources {}

    @Screen(name = "LookupDataResource", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupDataResource}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "20")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupDataResource", location = "component://content/widget/content/DataResourceForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupDataResource", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface LookupDataResource {}

    @Screen(name = "EditDataResource", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResource")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editDataResource")
    @Action(type = ActionType.SET, field = "dataResourceId", fromField = "parameters.dataResourceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue")
    @Action(type = ActionType.SET, field = "dataResource", fromField = "currentValue")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "DataCategory", list = "dataCategoryList")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ContentContentsAssociatedToDataResource}", includeForms = {
                    @IncludeForm(name = "EditDataResource", location = "component://content/widget/content/DataResourceForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"currentValue"})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DataResourceViewLink"
                        )}, screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleEditDataResource}", includeForms = {
                                @IncludeForm(name = "ListContentsAssociatedToDataResource", location = "component://content/widget/content/DataResourceForms.xml"
                            )})}))})
        }
    )
    public interface EditDataResource {}

    @Screen(name = "UploadImage", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleUploadImageDataResource")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleUploadImageDataResource")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "uploadImage")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "parameters.dataResourceId")})
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ImageUpload", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface UploadImage {}

    @Screen(name = "AddDataResource", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "adddataresource")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddDataResource", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface AddDataResource {}

    @Screen(name = "AddDataResourceText", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddDataResourceText")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleAddDataResourceText")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "adddataresourcetext")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddDataResourceText", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface AddDataResourceText {}

    @Screen(name = "AddDataResourceUrl", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddDataResourceUrl")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleAddDataResourceUrl")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "adddataresourceurl")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddDataResourceUrl", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface AddDataResourceUrl {}

    @Screen(name = "AddDataResourceUpload", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddDataResourceUpload")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleAddDataResourceUpload")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "adddataresourceupload")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddDataResourceUpload", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface AddDataResourceUpload {}

    @Screen(name = "AddDataResourceFromContent", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddDataResourceFromContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleAddDataResourceFromContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "adddataresource")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddDataResourceFromContent", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface AddDataResourceFromContent {}

    @Screen(name = "EditDataResourceText", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceText")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceText")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editdataresourcetext")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditDataResourceText", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface EditDataResourceText {}

    @Screen(name = "EditDataResourceUrl", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceUrl")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceUrl")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editdataresourceurl")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditDataResourceUrl", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface EditDataResourceUrl {}

    @Screen(name = "EditDataResourceUpload", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceUpload")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceUpload")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editdataresourceupload")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditDataResourceUpload", location = "component://content/widget/content/DataResourceForms.xml"
            )})
        }
    )
    public interface EditDataResourceUpload {}

    @Screen(name = "EditElectronicText", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditElectronicText")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditElectronicText")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editElectronicText")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditElectronicText", location = "component://content/widget/content/DataResourceForms.xml"
                )})})
        }
    )
    public interface EditElectronicText {}

    @Screen(name = "EditDataResourceAttribute", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceAttribute")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceAttribute")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editDataResourceAttribute")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "parameters.dataResourceId")})
    @Action(type = ActionType.GET_RELATED, valueField = "currentValue", relationName = "DataResourceAttribute", list = "dataResourceAttribute")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDataResourceAttribute", location = "component://content/widget/content/DataResourceForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditDataResourceAttribute}", name = "DataResourceAttributePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataResourceAttribute", location = "component://content/widget/content/DataResourceForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataResourceAttribute {}

    @Screen(name = "EditDataResourceRole", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceRole")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editDataResourceRole")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "parameters.dataResourceId")})
    @Action(type = ActionType.GET_RELATED, valueField = "currentValue", relationName = "DataResourceRole", list = "dataResourceRole")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDataResourceRole", location = "component://content/widget/content/DataResourceForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditDataResourceRole}", name = "DataResourceRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataResourceRole", location = "component://content/widget/content/DataResourceForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataResourceRole {}

    @Screen(name = "EditDataResourceProductFeatures", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditDataResourceProductFeatures")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditDataResourceProductFeatures")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editDataResourceProductFeatures")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "parameters.dataResourceId")})
    @Action(type = ActionType.GET_RELATED, valueField = "currentValue", relationName = "ProductFeatureDataResource", list = "productFeatureDataResource")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListDataResourceProductFeature", location = "component://content/widget/content/DataResourceForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditDataResourceProductFeatures}", name = "DataResourceProductFeaturePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddDataResourceProductFeature", location = "component://content/widget/content/DataResourceForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditDataResourceProductFeatures {}

    @Screen(name = "EditHtmlText", location = "component://content/widget/content/DataResourceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditHtmlText")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditHtmlText")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "editHtmlText")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "currentValue")
    @DecoratorScreen(
        name = "CommonDataResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditHtmlText", location = "component://content/widget/content/DataResourceForms.xml"
                )})})
        }
    )
    public interface EditHtmlText {}

    @Screen(name = "DataResourceViewLink", location = "component://content/widget/content/DataResourceScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/msword"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/pdf"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/vnd.oasis.opendocument.text"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/jpeg"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/gif"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/tiff"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/png")})}))
    @Section(widgets = @Widgets(containers = {@Container(widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentDownload}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ViewBinaryDataResource")})}))
    public interface DataResourceViewLink {}

}
