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
package com.ilscipio.scipio.accounting.widget;

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
public class LedgerAccountingTrees {

    @Tree(
        name = "GlAccountTree",
        location = "component://accounting/widget/ledger/AccountingTrees.xml",
        rootNodeName = "node-root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        defaultWrapStyle = "accountItem",
        expandCollapseRequest = "GlAccountNavigate",
        entityName = "GlAccount",
        nodes = {
            @TreeNode(name = "node-root", subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "GlAccount", fieldMaps = {@FieldMap(fieldName = "parentGlAccountId", fromField = "null")}, orderBy = {"accountCode"}))}),
            @TreeNode(name = "node-body", entityOne = @TreeEntityOne(entityName = "GlAccount", valueField = "glAccount"), link = @TreeLink(target = "GlAccountNavigate", text = "${glAccount.accountCode} ${glAccount.accountName}", parameters = {@TreeParameter(paramName = "glAccountId"), @TreeParameter(paramName = "trail")}), subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "GlAccount", fieldMaps = {@FieldMap(fieldName = "parentGlAccountId", fromField = "glAccountId")}, orderBy = {"accountCode"}))})
        }
    )
    public interface GlAccountTree {}

    @Tree(
        name = "ListGlAccountTree",
        location = "component://accounting/widget/ledger/AccountingTrees.xml",
        rootNodeName = "node-root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        defaultWrapStyle = "accountItem",
        entityName = "GlAccount",
        nodes = {
            @TreeNode(name = "node-root", subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "GlAccount", fieldMaps = {@FieldMap(fieldName = "parentGlAccountId", fromField = "null")}, orderBy = {"accountCode"}))}),
            @TreeNode(name = "node-body", entityOne = @TreeEntityOne(entityName = "GlAccount", valueField = "glAccount"), link = @TreeLink(target = "GlAccountNavigate", text = "${glAccount.accountCode} ${glAccount.accountName}", parameters = {@TreeParameter(paramName = "glAccountId"), @TreeParameter(paramName = "trail")}), subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "GlAccount", fieldMaps = {@FieldMap(fieldName = "parentGlAccountId", fromField = "glAccountId")}, orderBy = {"accountCode"}))})
        }
    )
    public interface ListGlAccountTree {}

    @Tree(
        name = "TreeFixedAsset",
        location = "component://accounting/widget/ledger/AccountingTrees.xml",
        rootNodeName = "node-root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        expandCollapseRequest = "FixedAssetChildren",
        entityName = "FixedAsset",
        nodes = {
            @TreeNode(name = "node-root", subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "FixedAsset", fieldMaps = {@FieldMap(fieldName = "parentFixedAssetId", fromField = "fixedAssetId")}, orderBy = {"fixedAssetName"}))}),
            @TreeNode(name = "node-body", link = @TreeLink(target = "EditFixedAsset", text = "${fixedAssetId} ${fixedAssetName} ${instanceOfProductId} ${fixedAssetTypeId} ", parameters = {@TreeParameter(paramName = "fixedAssetId", fromField = "fixedAssetId")}), subNodes = {@SubNode(nodeName = "node-body", entityAnd = @EntityAnd(entityName = "FixedAsset", fieldMaps = {@FieldMap(fieldName = "parentFixedAssetId", fromField = "fixedAssetId")}, orderBy = {"fixedAssetName"}))})
        }
    )
    public interface TreeFixedAsset {}

}
