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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingJobshopScreens {

    @Screen(name = "CommonJobshopDecorator", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "ProductionRun")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle} ${context.productionRunId}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.productionRun}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(position = 0, style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCreateProductionRun}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateProductionRun"
                ), @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCreateProductionRunFromOrder}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateProductionRunFromOrder"
                )})})
        }
    )
    public interface CommonJobshopDecorator {}

    @Screen(name = "CreateProductionRun", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingCreateProductionRun")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "jobshop")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "CreateProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )})})
        }
    )
    public interface CreateProductionRun {}

    @Screen(name = "EditProductionRun", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingEditProductionRun")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "edit")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ViewProductionRun.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/productionRunAllFixedAssets.groovy")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "mandatoryWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdTo", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "dependentWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdFrom", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingProductionRunId} ${productionRunId}", includeForms = {
                    @IncludeForm(name = "UpdateProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "ProductionRunStatusSubTabBar", location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml", position = 0
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.ManufacturingOrderItems}", includeForms = {
                    @IncludeForm(name = "ListProductionRunOrderItems", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 3),
                @Screenlet(title = "${uiLabelMap.ManufacturingListOfProductionRunRoutingTasks}", includeForms = {
                    @IncludeForm(name = "ViewListProductionRunRoutingTasks", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 4),
                @Screenlet(title = "${uiLabelMap.ManufacturingMaterials}", includeForms = {
                    @IncludeForm(name = "ListProductionRunComponents", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 5),
                @Screenlet(title = "${uiLabelMap.ManufacturingListOfProductionRunFixedAssets}", includeForms = {
                    @IncludeForm(name = "ListProductionRunTaskFixedAssets", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 6),
                @Screenlet(title = "${uiLabelMap.ManufacturingListOfProductionRunNotes}", includeForms = {
                    @IncludeForm(name = "ListProductionRunNotes", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 7)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"mandatoryWorkEfforts"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.ManufacturingPrecedingProductionRun}", includeForms = {
                            @IncludeForm(name = "mandatoryWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                        )})}), position = 1),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"dependentWorkEfforts"
                        })}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.ManufacturingSucceedingProductionRun}", includeForms = {
                                @IncludeForm(name = "dependentWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                            )})}), position = 2)})
        }
    )
    public interface EditProductionRun {}

    @Screen(name = "ProductionRunDeclaration", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunDeclaration")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "declaration")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunDeclaration.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/productionRunAllFixedAssets.groovy")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId", defaultValue = "${parameters.workEffortId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "mandatoryWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdTo", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "dependentWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdFrom", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @Action(type = ActionType.SERVICE, serviceName = "getProductionRunRejects", fieldMaps = {@FieldMap(fieldName = "productionRunId", fromField = "productionRunId")})
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(position = 0, title = "${uiLabelMap.ManufacturingProductionRunId} ${productionRunId}", includeForms = {
                    @IncludeForm(name = "ShowProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "ProductionRunStatusSubTabBar", location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml", position = 0
                )}),
                @Screenlet(position = 3, title = "${uiLabelMap.ManufacturingInventoryItemsProduced}", includeForms = {
                    @IncludeForm(name = "ListProductionRunInventoryItems", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(position = 6, title = "${uiLabelMap.ManufacturingOrderItems}", includeForms = {
                    @IncludeForm(name = "ListProductionRunOrderItems", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(position = 7, title = "${uiLabelMap.ManufacturingListOfProductionRunRoutingTasks}", includeForms = {
                    @IncludeForm(name = "ListProductionRunDeclRoutingTasks", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(position = 8, title = "${uiLabelMap.ManufacturingProductionRunDeclaration}", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.routingTaskId"
                    })}), widgets = @WidgetsForContainer(containers = {
                        @Container2(style = "${styles.grid_large}12", includeForms = {
                            @IncludeForm(name = "EditProductionRunDeclRoutingTask", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                        )}),
                        @Container2(style = "${styles.grid_large}6", includeForms = {
                            @IncludeForm(name = "CreateRoutingTaskDelivProduct", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                        )}, sections = {
                            @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                @Condition(type = Empty.class, params = {"prunInventoryProduced"
                            })}), widgets = @WidgetsForContainer2(value = {
                                @Widget(type = WidgetType.INCLUDE_FORM, name = "ProductionRunTaskInventoryProducedList", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                            )}))})}))}),
                            @Screenlet(position = 9, title = "${uiLabelMap.ManufacturingMaterialsRequiredByRunningTask}", includeForms = {
                                @IncludeForm(name = "ListIssueProductionRunDeclComponents", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                            )}),
                            @Screenlet(position = 10, title = "${uiLabelMap.ManufacturingReturnMaterials}", includeForms = {
                                @IncludeForm(name = "ListReturnProductionRunDeclComponents", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                            )}),
                            @Screenlet(position = 11, title = "${uiLabelMap.ManufacturingListOfProductionRunFixedAssets}", includeForms = {
                                @IncludeForm(name = "ListProductionRunTaskFixedAssets", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                            )}),
                            @Screenlet(position = 12, title = "${uiLabelMap.ManufacturingRejects}", sections = {
                                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                    @Condition(type = Empty.class, params = {"rejects"
                                })}), widgets = @WidgetsForContainer(value = {
                                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductionRunRejects", location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml"
                                )}))
                            })}, sections = {
                                @InlineSection(position = 1, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = Compare.class, params = {"canProduce", "equals", "Y"
                                })}), widgets = @InlineWidgets(screenlets = {
                                    @Screenlet(title = "${uiLabelMap.ManufacturingProductionRunProduce}", includeForms = {
                                        @IncludeForm(name = "ProductionRunProduce", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                    )})})),
                                    @InlineSection(position = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                        @Condition(type = Compare.class, params = {"canDeclareAndProduce", "equals", "Y"
                                    })}), widgets = @InlineWidgets(screenlets = {
                                        @Screenlet(title = "${uiLabelMap.ManufacturingProductionRunDeclareAndProduce}", includeForms = {
                                            @IncludeForm(name = "ProductionRunDeclareAndProduceTop", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                        ),
                                        @IncludeForm(name = "ProductionRunDeclareAndProduceBottom", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                    )})})),
                                    @InlineSection(position = 4, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                        @Condition(type = Empty.class, params = {"mandatoryWorkEfforts"
                                    })}), widgets = @InlineWidgets(screenlets = {
                                        @Screenlet(title = "${uiLabelMap.ManufacturingMandatoryWorkEfforts}", includeForms = {
                                            @IncludeForm(name = "mandatoryWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                        )})})),
                                        @InlineSection(position = 5, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                            @Condition(type = Empty.class, params = {"dependentWorkEfforts"
                                        })}), widgets = @InlineWidgets(screenlets = {
                                            @Screenlet(title = "${uiLabelMap.ManufacturingDependentWorkEfforts}", includeForms = {
                                                @IncludeForm(name = "dependentWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                            )})}))})
        }
    )
    public interface ProductionRunDeclaration {}

    @Screen(name = "ProductionRunPdf", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRun")
    @Action(type = ActionType.SET, field = "bodyFontSize", value = "10pt")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunDeclaration.groovy")
    @DecoratorScreen(
        name = "SimpleDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/jobshopmgt/ProductionRun.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface ProductionRunPdf {}

    @Screen(name = "ProductionRunAssocs", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunAssocs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "assocs")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "mandatoryWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdTo", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WorkEffortAssoc", list = "dependentWorkEfforts", conditions = {@ConditionExpr(fieldName = "workEffortIdFrom", fromField = "productionRunId"), @ConditionExpr(fieldName = "workEffortAssocTypeId", value = "WORK_EFF_PRECEDENCY")})
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingPrecedingProductionRun}", includeForms = {
                    @IncludeForm(name = "mandatoryWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingSucceedingProductionRun}", includeForms = {
                    @IncludeForm(name = "dependentWorkEfforts", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )})})
        }
    )
    public interface ProductionRunAssocs {}

    @Screen(name = "ProductionRunTasks", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunTasks")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "tasks")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SET, field = "routingTaskId", fromField = "parameters.routingTaskId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRunTask", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "routingTaskId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunTasks.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductionRunRoutingTasks", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
            )}, screenlets = {
                @Screenlet(name = "EditProductionRunRoutingTaskPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditProductionRunRoutingTask", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 0)})
        }
    )
    public interface ProductionRunTasks {}

    @Screen(name = "ProductionRunComponents", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunComponents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "components")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunComponents.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/jobshopmgt/ProductionRunComponentsInfo.ftl"),
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/jobshopmgt/ProductionRunReservations.ftl"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingAddInputComponent}", includeForms = {
                    @IncludeForm(name = "AddProductionRunComponent", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingAddCoProduct}", labels = {
                    @Label(text = "${uiLabelMap.ManufacturingAddCoProductHelp}")
                }, includeForms = {
                    @IncludeForm(name = "CreateRoutingTaskDelivProduct", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )})})
        }
    )
    public interface ProductionRunComponents {}

    @Screen(name = "ProductionRunActualComponents", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunActualComponents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "actualComponents")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunActualComponents.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/jobshopmgt/ProductionRunTasksInfo.ftl"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingActualMaterials}", includeForms = {
                    @IncludeForm(name = "IssueProductionRunComponent", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, position = 0)})
        }
    )
    public interface ProductionRunActualComponents {}

    @Screen(name = "ProductionRunFixedAssets", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunResources")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "fixedAssets")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunFixedAssets.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingListOfProductionRunFixedAssets}", includeForms = {
                    @IncludeForm(name = "ProductionRunTaskFixedAssets", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingTaskFixedAssets}", includeForms = {
                    @IncludeForm(name = "AddProductionRunTaskFixedAsset", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingProductionRunWorkers}", includeForms = {
                    @IncludeForm(name = "ProductionRunWorkers", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingAssignWorker}", includeForms = {
                    @IncludeForm(name = "AssignProductionRunWorker", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )})})
        }
    )
    public interface ProductionRunFixedAssets {}

    @Screen(name = "ProductionRunCosts", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunCosts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "costs")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunCosts.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/jobshopmgt/ProductionRunCosts.ftl"
            )})
        }
    )
    public interface ProductionRunCosts {}

    @Screen(name = "ProductionRunContent", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "content")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/ProductionRunContent.groovy")
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductionRunContent", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
            )}, screenlets = {
                @Screenlet(title = "Import Content From Product ${delivProductId}", includeForms = {
                    @IncludeForm(name = "FindDelivProductContent", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                ),
                @IncludeForm(name = "ListDelivProductContent", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
            )})})
        }
    )
    public interface ProductionRunContent {}

    @Screen(name = "LinkProductionRun", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductionRunLink")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "assocs")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "productionRun", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "productionRunId")})
    @DecoratorScreen(
        name = "CommonJobshopDecorator",
        location = "${parameters.commonJobshopDecorator}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "linkProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )})})
        }
    )
    public interface LinkProductionRun {}

    @Screen(name = "FindProductionRun", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingFindProductionRun")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "jobshop")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCreateProductionRun}", style = "${styles.link_nav} ${styles.action_add}", target = "CreateProductionRun"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "findProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "listFindProductionRun", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                        )}))})})
        }
    )
    public interface FindProductionRun {}

    @Screen(name = "WorkWithShipmentPlans", location = "component://manufacturing/widget/manufacturing/JobshopScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingWorkWithShipmentPlans")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ShipmentPlans")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "sortField", fromField = "parameters.sortField", defaultValue = "estimatedShipDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Shipment", valueField = "shipment")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Shipment", list = "shipmentPlans", conditions = {@ConditionExpr(fieldName = "shipmentTypeId", value = "SALES_SHIPMENT"), @ConditionExpr(fieldName = "statusId", value = "SHIPMENT_SCHEDULED")}, orderBy = {"${sortField}"})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/jobshopmgt/WorkWithShipmentPlans.groovy")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.FacilityFacility} ${uiLabelMap.FacilityShipments}", style = "${styles.link_nav}", target = "/facility/control/FindShipment"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "listShipmentPlans", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                    )}, position = 1)}, sections = {
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"shipment"})}), widgets = @InlineWidgets(value = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingPackageLabelsReport}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ShipmentLabel.pdf", targetWindow = "_BLANK", position = 2
                            )}, screenlets = {
                                @Screenlet(title = "${uiLabelMap.FormFieldTitle_shipmentId} ${shipment.shipmentId}", includeForms = {
                                    @IncludeForm(name = "listShipmentPlan", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                                )}, position = 0)}, sections = {
                                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                        @Condition(type = Compare.class, params = {"workInProgress", "equals", "true"
                                    })}), widgets = @WidgetsForContainer(value = {
                                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingTasksReport}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ShipmentWorkEffortTasks.pdf", targetWindow = "_BLANK"
                                    ),
                                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCuttingListReport}", style = "${styles.link_run_sys} ${styles.action_export}", target = "CuttingListReport.pdf", targetWindow = "_BLANK"
                                )}), failWidgets = @WidgetsForContainer(value = {
                                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingGenerateProductionRuns}", style = "${styles.link_run_sys} ${styles.action_add}", target = "createProductionRunsForShipment"
                                ),
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingShipmentPlanStockReport}", style = "${styles.link_run_sys} ${styles.action_export}", target = "ShipmentPlanStockReport.pdf", targetWindow = "_BLANK"
                            )}), position = 1)}), position = 2)})
        }
    )
    public interface WorkWithShipmentPlans {}

}
