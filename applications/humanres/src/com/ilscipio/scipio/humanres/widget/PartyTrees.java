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
package com.ilscipio.scipio.humanres.widget;

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
public class PartyTrees {

    @Tree(
        name = "OrgTree",
        location = "component://humanres/widget/PartyTrees.xml",
        rootNodeName = "node-root",
        openDepth = "1",
        entityName = "PartyAndGroup",
        nodes = {
            @TreeNode(name = "node-root", renderStyle = RenderStyle.EXPAND_COLLAPSE, useDefaultRenderStyle = false, entityName = "PartyAndGroup", entityOne = @TreeEntityOne(entityName = "PartyAndGroup", valueField = "partyAndGroup"), link = @TreeLink(target = "/partymgr/control/viewprofile", text = "${partyAndGroup.groupName}", urlMode = UrlMode.INTER_APP, parameters = {@TreeParameter(paramName = "partyId")}), subNodes = {@SubNode(nodeName = "internalOrg-list", entityCondition = @EntityCondition(entityName = "PartyRelationship", filterByDate = "true", conditions = {@TreeConditionExpr(fieldName = "partyIdFrom", fromField = "partyId"), @TreeConditionExpr(fieldName = "partyRelationshipTypeId", value = "GROUP_ROLLUP")}))}),
            @TreeNode(name = "internalOrg-list", renderStyle = RenderStyle.EXPAND_COLLAPSE, useDefaultRenderStyle = false, entryName = "partyRelationship", entityName = "PartyRelationship", joinFieldName = "partyIdTo", entityOne = @TreeEntityOne(entityName = "PartyAndGroup", valueField = "partyAndGroup", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyRelationship.partyIdTo")}), link = @TreeLink(target = "/partymgr/control/viewprofile", text = "${partyAndGroup.groupName}", urlMode = UrlMode.INTER_APP, parameters = {@TreeParameter(paramName = "partyId", fromField = "partyRelationship.partyIdTo")}), subNodes = {@SubNode(nodeName = "employee-list", entityCondition = @EntityCondition(entityName = "Employment", filterByDate = "true", conditions = {@TreeConditionExpr(fieldName = "partyIdFrom", fromField = "partyRelationship.partyIdTo"), @TreeConditionExpr(fieldName = "roleTypeIdTo", value = "EMPLOYEE")}))}),
            @TreeNode(name = "employee-list", entryName = "employment", entityName = "Employment", joinFieldName = "partyIdTo", entityOne = @TreeEntityOne(entityName = "PartyAndPerson", valueField = "partyAndPerson", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "employment.partyIdTo")}), includeScreen = @TreeIncludeScreen(name = "PartyPersonTreeLine", location = "component://humanres/widget/CommonScreens.xml"))
        }
    )
    public interface OrgTree {}

}
