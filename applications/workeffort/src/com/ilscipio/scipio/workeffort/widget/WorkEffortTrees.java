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

import com.ilscipio.scipio.widget.def.tree.*;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityAndAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.menu.UrlMode;
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
public class WorkEffortTrees {

    @Tree(
        name = "TreeWorkEffort",
        location = "component://workeffort/widget/WorkEffortTrees.xml",
        rootNodeName = "node-root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        defaultWrapStyle = "treeWrapper",
        entityName = "WorkEffort",
        nodes = {
            @TreeNode(name = "node-root", entityName = "WorkEffort", entityOne = @TreeEntityOne(entityName = "WorkEffort", valueField = "workEffort"), link = @TreeLink(target = "EditWorkEffort", text = "${workEffort.workEffortName} - ${workEffort.description} [${workEffort.workEffortId}]", parameters = {@TreeParameter(paramName = "workEffortId", fromField = "workEffort.workEffortId")}), subNodes = {@SubNode(nodeName = "node-list", entityCondition = @EntityCondition(entityName = "WorkEffortAssoc", conditions = {@TreeConditionExpr(fieldName = "workEffortIdFrom", fromField = "workEffortId")}))}),
            @TreeNode(name = "node-list", entryName = "workEffortAssoc", entityName = "WorkEffortAssoc", joinFieldName = "workEffortIdTo", actions = @TreeActions(entityOne = {@EntityOneAction(entityName = "WorkEffort", valueField = "workEffort"), @EntityOneAction(entityName = "WorkEffortAssocType", valueField = "workEffortAssocType")}), link = @TreeLink(target = "EditWorkEffortAssoc", text = "${workEffort.workEffortName} - ${workEffort.description} (${workEffortAssocType.description}) [${workEffort.workEffortId}]", parameters = {@TreeParameter(paramName = "workEffortIdTo", fromField = "workEffortAssoc.workEffortIdTo"), @TreeParameter(paramName = "workEffortIdFrom", fromField = "workEffortAssoc.workEffortIdFrom"), @TreeParameter(paramName = "workEffortAssocTypeId", fromField = "workEffortAssoc.workEffortAssocTypeId"), @TreeParameter(paramName = "fromDate", fromField = "workEffortAssoc.fromDate")}), subNodes = {@SubNode(nodeName = "node-list", entityCondition = @EntityCondition(entityName = "WorkEffortAssoc", conditions = {@TreeConditionExpr(fieldName = "workEffortIdFrom", fromField = "workEffortAssoc.workEffortIdTo")}))})
        }
    )
    public interface TreeWorkEffort {}

    @Tree(
        name = "ICalendarTree",
        location = "component://workeffort/widget/WorkEffortTrees.xml",
        rootNodeName = "node-root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        defaultWrapStyle = "treeWrapper",
        expandCollapseRequest = "ICalendarChildren?workEffortId=${workEffortId}",
        entityName = "WorkEffort",
        nodes = {
            @TreeNode(name = "node-root", entityName = "WorkEffort", entityOne = @TreeEntityOne(entityName = "WorkEffort", valueField = "workEffort"), subNodes = {@SubNode(nodeName = "node-list", entityCondition = @EntityCondition(entityName = "WorkEffortAssoc", conditions = {@TreeConditionExpr(fieldName = "workEffortIdFrom", fromField = "workEffortId")}))}),
            @TreeNode(name = "node-list", entryName = "workEffortAssoc", entityName = "WorkEffortAssoc", joinFieldName = "workEffortIdTo", entityOne = @TreeEntityOne(entityName = "WorkEffort", valueField = "workEffort", fieldMaps = {@FieldMap(fieldName = "workEffortId", fromField = "workEffortAssoc.workEffortIdTo")}), label = @TreeLabel(text = "${workEffort.workEffortName} - ${workEffort.description} [${workEffort.workEffortId}]"), subNodes = {@SubNode(nodeName = "node-list", entityCondition = @EntityCondition(entityName = "WorkEffortAssoc", conditions = {@TreeConditionExpr(fieldName = "workEffortIdFrom", fromField = "workEffortAssoc.workEffortIdTo")}))})
        }
    )
    public interface ICalendarTree {}

}
