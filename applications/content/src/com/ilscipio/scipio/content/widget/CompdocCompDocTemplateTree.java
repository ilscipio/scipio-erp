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
public class CompdocCompDocTemplateTree {

    @Tree(
        name = "CompDocTemplateTree",
        location = "component://content/widget/compdoc/CompDocTemplateTree.xml",
        rootNodeName = "node-root",
        defaultWrapStyle = "treeWrapper",
        nodes = {
            @TreeNode(name = "node-root", wrapStyle = "treeWrapper", entityOne = @TreeEntityOne(entityName = "Content", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")}), includeScreen = @TreeIncludeScreen(name = "rootTemplateLine", location = "component://content/widget/compdoc/CompDocScreens.xml"), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "AssocRevisionItemView", conditions = {@TreeConditionExpr(fieldName = "contentIdTo", fromField = "rootContentId"), @TreeConditionExpr(fieldName = "rootRevisionContentId", fromField = "rootContentId"), @TreeConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "rootContentRevisionSeqId", ignoreIfNull = true), @TreeConditionExpr(fieldName = "contentAssocTypeId", value = "COMPDOC_PART"), @TreeConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, conditionLists = {@TreeConditionList(combine = "or", conditions = {@TreeConditionExpr(fieldName = "thruDate"), @TreeConditionExpr(fieldName = "thruDate", operator = "greater", fromField = "nowTimestamp")})}, selectFields = {"rootRevisionContentId", "itemContentId", "maxRevisionSeqId", "contentId", "contentIdTo", "contentAssocTypeId", "fromDate", "sequenceNum"}, orderBy = {"sequenceNum"}))}),
            @TreeNode(name = "node-body", wrapStyle = "treeWrapper", entityName = "AssocRevisionItemView", joinFieldName = "itemContentId", entityOne = @TreeEntityOne(entityName = "Content", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "itemContentId")}), includeScreen = @TreeIncludeScreen(name = "childTemplateLine", location = "component://content/widget/compdoc/CompDocScreens.xml"), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "AssocRevisionItemView", conditions = {@TreeConditionExpr(fieldName = "contentIdTo", fromField = "contentId"), @TreeConditionExpr(fieldName = "rootRevisionContentId", fromField = "rootContentId"), @TreeConditionExpr(fieldName = "contentAssocTypeId", value = "COMPDOC_PART"), @TreeConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "rootContentRevisionSeqId", ignoreIfNull = true), @TreeConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, conditionLists = {@TreeConditionList(combine = "or", conditions = {@TreeConditionExpr(fieldName = "thruDate"), @TreeConditionExpr(fieldName = "thruDate", operator = "greater", fromField = "nowTimestamp")})}, selectFields = {"rootRevisionContentId", "itemContentId", "maxRevisionSeqId", "contentId", "contentIdTo", "contentAssocTypeId", "fromDate", "sequenceNum"}, orderBy = {"sequenceNum"}))})
        }
    )
    public interface CompDocTemplateTree {}

    @Tree(
        name = "CompDocInstanceTree",
        location = "component://content/widget/compdoc/CompDocTemplateTree.xml",
        rootNodeName = "node-root",
        defaultWrapStyle = "treeWrapper",
        nodes = {
            @TreeNode(name = "node-root", entityOne = @TreeEntityOne(entityName = "Content", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "instanceContent.instanceOfContentId")}), includeScreen = @TreeIncludeScreen(name = "rootInstanceLine", location = "component://content/widget/compdoc/CompDocScreens.xml"), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "AssocRevisionItemView", conditions = {@TreeConditionExpr(fieldName = "contentIdTo", fromField = "templateContentId"), @TreeConditionExpr(fieldName = "rootRevisionContentId", fromField = "templateContentId"), @TreeConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "templateContentRevisionSeqId", ignoreIfNull = true), @TreeConditionExpr(fieldName = "contentAssocTypeId", value = "COMPDOC_PART"), @TreeConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, conditionLists = {@TreeConditionList(combine = "or", conditions = {@TreeConditionExpr(fieldName = "thruDate"), @TreeConditionExpr(fieldName = "thruDate", operator = "greater", fromField = "nowTimestamp")})}, selectFields = {"rootRevisionContentId", "itemContentId", "maxRevisionSeqId", "contentId", "contentIdTo", "contentAssocTypeId", "fromDate", "sequenceNum"}, orderBy = {"sequenceNum"}))}),
            @TreeNode(name = "node-body", entityName = "AssocRevisionItemView", joinFieldName = "itemContentId", entityOne = @TreeEntityOne(entityName = "Content", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "itemContentId")}), includeScreen = @TreeIncludeScreen(name = "childInstanceLine", location = "component://content/widget/compdoc/CompDocScreens.xml"), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "AssocRevisionItemView", conditions = {@TreeConditionExpr(fieldName = "contentIdTo", fromField = "contentId"), @TreeConditionExpr(fieldName = "rootRevisionContentId", fromField = "templateContentId"), @TreeConditionExpr(fieldName = "contentAssocTypeId", value = "COMPDOC_PART"), @TreeConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "templateContentRevisionSeqId"), @TreeConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, conditionLists = {@TreeConditionList(combine = "or", conditions = {@TreeConditionExpr(fieldName = "thruDate"), @TreeConditionExpr(fieldName = "thruDate", operator = "greater", fromField = "nowTimestamp")})}, selectFields = {"rootRevisionContentId", "itemContentId", "maxRevisionSeqId", "contentId", "contentIdTo", "contentAssocTypeId", "fromDate", "sequenceNum"}, orderBy = {"sequenceNum"}))})
        }
    )
    public interface CompDocInstanceTree {}

}
