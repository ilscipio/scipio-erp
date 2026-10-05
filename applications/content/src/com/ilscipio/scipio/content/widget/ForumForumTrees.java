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
public class ForumForumTrees {

    @Tree(
        name = "MessageTree",
        location = "component://content/widget/forum/ForumTrees.xml",
        rootNodeName = "node-root",
        nodes = {
            @TreeNode(name = "node-root", entityOne = @TreeEntityOne(entityName = "Content", valueField = "rsp", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.forumId")}), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "ContentAssocViewTo", conditions = {@TreeConditionExpr(fieldName = "contentIdStart", fromField = "rsp.contentId"), @TreeConditionExpr(fieldName = "caThruDate")}, conditionLists = {@TreeConditionList(combine = "or", conditions = {@TreeConditionExpr(fieldName = "caContentAssocTypeId", value = "RESPONSE"), @TreeConditionExpr(fieldName = "caContentAssocTypeId", value = "PUBLISH_LINK")})}, orderBy = {"createdDate"}))}),
            @TreeNode(name = "node-body", entryName = "rsp", includeScreen = @TreeIncludeScreen(name = "responseTreeLine", location = "${parameters.mainDecoratorLocation}"), subNodes = {@SubNode(nodeName = "node-body", entityCondition = @EntityCondition(entityName = "ContentAssocViewTo", useCache = true, conditions = {@TreeConditionExpr(fieldName = "contentIdStart", fromField = "rsp.contentId"), @TreeConditionExpr(fieldName = "caContentAssocTypeId", value = "RESPONSE"), @TreeConditionExpr(fieldName = "caThruDate")}, orderBy = {"createdDate"}))})
        }
    )
    public interface MessageTree {}

}
