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
package com.ilscipio.scipio.webtools.widget;

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
public class EntityScreens {

    @Screen(name = "EntitySQLProcessor", location = "component://webtools/widget/EntityScreens.xml", transactionTimeout = "7200")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntitySQLProcessor")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EntitySQLProcessor")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEntitySQLProcessor")
    @Action(type = ActionType.SET, field = "sqlCommand", fromField = "parameters.sqlCommand")
    @Action(type = ActionType.SET, field = "selGroup", fromField = "parameters.group")
    @Action(type = ActionType.SET, field = "rowLimit", fromField = "parameters.rowLimit", valueType = "Integer")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/EntitySQLProcessor.groovy")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntitySQLProcessor.ftl"
            )})
        }
    )
    public interface EntitySQLProcessor {}

    @Screen(name = "EntityExportAll", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityExportAll")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityExportAll")
    @Action(type = ActionType.SET, field = "results", fromField = "parameters.results")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityExportAll.ftl"
                )})})
        }
    )
    public interface EntityExportAll {}

    @Screen(name = "ProgramExport", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityExportAll")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "programExport")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "tmplTestEnabled", resource = "webtools", property = "dev.script.tools.enabled")
    @Action(type = ActionType.SET, field = "tmplTestEnabled", fromField = "tmplTestEnabled", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "hasTmplTestPerm", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"}), @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"}), @Condition(type = True.class, params = {"tmplTestEnabled"})}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/ProgramExport.groovy")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"OFBTOOLS", "_VIEW"
                }),
                @Condition(type = HasPermission.class, params = {"ENTITY_DATA_ADMIN"
            })}), widgets = @InlineWidgets(screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ProgramExport", location = "component://webtools/widget/MiscForms.xml"
                )}, position = 1),
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/ProgramExport.ftl"
                )}, position = 2)}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = True.class, params = {"tmplTestEnabled"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonFunctionIsDisabled}", style = "common-msg-warning"
                        )}), position = 0)}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface ProgramExport {}

    @Screen(name = "EntityImportDir", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityImportDir")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityImportDir")
    @Action(type = ActionType.SET, field = "messages", fromField = "parameters.messages")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityImportDir.ftl"
                )})})
        }
    )
    public interface EntityImportDir {}

    @Screen(name = "EntityImport", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityImport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityImport")
    @Action(type = ActionType.SET, field = "messages", fromField = "parameters.messages")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityImport.ftl"
                )})})
        }
    )
    public interface EntityImport {}

    @Screen(name = "EntityImportReaders", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityImportReaders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityImportReaders")
    @Action(type = ActionType.SET, field = "messages", fromField = "parameters.messages")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/EntityImportReaders_script1.groovy")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityImportReaders.ftl"
                )})})
        }
    )
    public interface EntityImportReaders {}

    @Screen(name = "EntityMaint", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityDataMaintenance")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entitymaint")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/EntityMaint.groovy")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntityMaint.ftl"
            )})
        }
    )
    public interface EntityMaint {}

    @Screen(name = "FindGeneric", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/FindGeneric.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WebtoolsFindValues} ${uiLabelMap.WebtoolsForEntity}: ${entityName}")
    @Action(type = ActionType.SET, field = "commonDisplaying", value = "${uiLabelMap.CommonDisplaying}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entitymaint")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"modelEntity"})}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "EntitySubTabBar", location = "component://webtools/widget/Menus.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/FindGeneric.ftl"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/ListGeneric.ftl"
                        )}))})), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "EntitySubTabBar", location = "component://webtools/widget/Menus.xml"
                        )}))})
        }
    )
    public interface FindGeneric {}

    @Screen(name = "ViewGeneric", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/ViewGeneric.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WebtoolsViewValue}: ${entityName}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entitymaint")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/ViewGeneric.ftl"
            )})
        }
    )
    public interface ViewGeneric {}

    @Screen(name = "ViewRelations", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/ViewRelations.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.WebtoolsRelations}: ${entityName}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entitymaint")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/ViewRelations.ftl"
            )})
        }
    )
    public interface ViewRelations {}

    @Screen(name = "EntityRef", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityReferenceChart")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/EntityRef.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntityRef.ftl")}))
    public interface EntityRef {}

    @Screen(name = "EntityRefMain", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityReferenceChart")
    @Action(type = ActionType.SERVICE, serviceName = "getEntityRefData", resultMapName = "result")
    @Action(type = ActionType.SET, field = "numberOfEntities", fromField = "result.numberOfEntities")
    @Action(type = ActionType.SET, field = "packagesList", fromField = "result.packagesList")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntityRefMain.ftl")}))
    public interface EntityRefMain {}

    @Screen(name = "EntityRefList", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityReference")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/EntityRefList.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntityRefList.ftl")}))
    public interface EntityRefList {}

    @Screen(name = "EntityRefReport", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityReferenceChart")
    @Action(type = ActionType.SERVICE, serviceName = "getEntityRefData", resultMapName = "result")
    @Action(type = ActionType.SET, field = "numberOfEntities", fromField = "result.numberOfEntities")
    @Action(type = ActionType.SET, field = "packagesList", fromField = "result.packagesList")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityRefReport.fo.ftl", platform = "xsl-fo")}))
    public interface EntityRefReport {}

    @Screen(name = "EntityEoModelBundle", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityEoModelBundle")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityEoModelBundle")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EntityEoModelBundle", location = "component://webtools/widget/EntityForms.xml"
                )})})
        }
    )
    public interface EntityEoModelBundle {}

    @Screen(name = "CheckDb", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsCheckUpdateDatabase")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "checkDb")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/CheckDb.groovy")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/CheckDb.ftl"
                )})})
        }
    )
    public interface CheckDb {}

    @Screen(name = "EntityPerformanceTest", location = "component://webtools/widget/EntityScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ENTITY_MAINT"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsPermissionMaint}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsPerformanceTests")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityPerformanceTest")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/EntityPerformanceTest.groovy")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListPerformanceResults", location = "component://webtools/widget/EntityForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.WebtoolsNotePerformanceResultsMayVary}", position = 0
                )})})
        }
    )
    public interface EntityPerformanceTest {}

    @Screen(name = "xmldsdump", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityExport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "xmlDsDump")
    @Action(type = ActionType.SET, field = "entityFrom", fromField = "parameters.entityFrom", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "entityThru", fromField = "parameters.entityThru", valueType = "Timestamp")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/entity/XmlDsDump.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EntityExport", list = "exportList", orderBy = {"-createdStamp"})
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/xmldsdump.ftl"
                )})})
        }
    )
    public interface xmldsdump {}

    @Screen(name = "ConnectionPoolStatus", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ConnectionPoolStatus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ConnectionPoolStatus")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ConnectionPoolStatus")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/ConnectionPoolStatus.ftl"
            )})
        }
    )
    public interface ConnectionPoolStatus {}

    @Screen(name = "EntityUtilityServices", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "SolrUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityUtilityServices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EntityUtilityServices")
    @DecoratorScreen(
        name = "CommonEntityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/EntityUtilityServices.ftl"
            )})
        }
    )
    public interface EntityUtilityServices {}

    @Screen(name = "ExcelImport", location = "component://webtools/widget/EntityScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EntityExcelImport")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EntityExcelImport")
    @DecoratorScreen(
        name = "CommonImportExportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/entity/excelimport.ftl"
            )})
        }
    )
    public interface ExcelImport {}

}
