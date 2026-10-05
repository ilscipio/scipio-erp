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
package com.ilscipio.scipio.widget.test;

import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.tree.*;

/**
 * Test class demonstrating annotation-based tree definitions.
 *
 * <p>This class contains examples of various tree patterns using annotations
 * instead of XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations testing.</p>
 */
public class TestTreesAnnotated {

    /**
     * Example 1: Simple category tree with labels.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestSimpleTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="category"&gt;
     *         &lt;label text="${category.name}"/&gt;
     *         &lt;sub-node node-name="child"&gt;
     *             &lt;entity-and entity-name="ProductCategory" list="children"&gt;
     *                 &lt;field-map field-name="parentCategoryId" from-field="category.productCategoryId"/&gt;
     *             &lt;/entity-and&gt;
     *         &lt;/sub-node&gt;
     *     &lt;/node&gt;
     *     &lt;node name="child" entry-name="childCategory"&gt;
     *         &lt;label text="${childCategory.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestSimpleTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "category",
                label = @TreeLabel(text = "${category.name}"),
                subNodes = {
                    @SubNode(nodeName = "child",
                        entityAnd = @EntityAnd(
                            entityName = "ProductCategory",
                            list = "children",
                            fieldMaps = @FieldMap(fieldName = "parentCategoryId", fromField = "category.productCategoryId")
                        )
                    )
                }
            ),
            @TreeNode(name = "child", entryName = "childCategory",
                label = @TreeLabel(text = "${childCategory.name}")
            )
        }
    )
    public interface TestSimpleTree {}

    /**
     * Example 2: Tree with links and expand/collapse.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestLinkTree" root-node-name="root"
     *       default-render-style="expand-collapse" expand-collapse-request="ExpandCategory"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;link target="ViewCategory" text="${item.categoryName}"&gt;
     *             &lt;parameter param-name="categoryId" from-field="item.productCategoryId"/&gt;
     *         &lt;/link&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestLinkTree",
        rootNodeName = "root",
        defaultRenderStyle = RenderStyle.EXPAND_COLLAPSE,
        expandCollapseRequest = "ExpandCategory",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                useDefaultRenderStyle = true,
                link = @TreeLink(
                    target = "ViewCategory",
                    text = "${item.categoryName}",
                    parameters = @TreeParameter(paramName = "categoryId", fromField = "item.productCategoryId")
                )
            )
        }
    )
    public interface TestLinkTree {}

    /**
     * Example 3: Tree with trail navigation.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestTrailTree" root-node-name="root"
     *       default-render-style="follow-trail" trail-name="categoryTrail" open-depth="2"&gt;
     *     &lt;node name="root" entry-name="category"&gt;
     *         &lt;label text="${category.name}" style="tree-node"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestTrailTree",
        rootNodeName = "root",
        defaultRenderStyle = RenderStyle.FOLLOW_TRAIL,
        trailName = "categoryTrail",
        openDepth = "2",
        nodes = {
            @TreeNode(name = "root", entryName = "category",
                useDefaultRenderStyle = true,
                label = @TreeLabel(text = "${category.name}", style = "tree-node")
            )
        }
    )
    public interface TestTrailTree {}

    /**
     * Example 4: Tree with service action.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestServiceTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;service service-name="getTreeData" result-map-list="items"/&gt;
     *         &lt;label text="${item.description}"/&gt;
     *         &lt;sub-node node-name="child"&gt;
     *             &lt;service service-name="getChildItems" result-map-list="childItems"&gt;
     *                 &lt;field-map field-name="parentId" from-field="item.itemId"/&gt;
     *             &lt;/service&gt;
     *         &lt;/sub-node&gt;
     *     &lt;/node&gt;
     *     &lt;node name="child" entry-name="childItem"&gt;
     *         &lt;label text="${childItem.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestServiceTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                service = @TreeService(
                    serviceName = "getTreeData",
                    resultMapList = "items"
                ),
                label = @TreeLabel(text = "${item.description}"),
                subNodes = {
                    @SubNode(nodeName = "child",
                        service = @TreeService(
                            serviceName = "getChildItems",
                            resultMapList = "childItems",
                            fieldMaps = @FieldMap(fieldName = "parentId", fromField = "item.itemId")
                        )
                    )
                }
            ),
            @TreeNode(name = "child", entryName = "childItem",
                label = @TreeLabel(text = "${childItem.name}")
            )
        }
    )
    public interface TestServiceTree {}

    /**
     * Example 5: Tree with conditional nodes.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestConditionalTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;condition&gt;
     *             &lt;if-has-permission permission="CATALOG" action="_VIEW"/&gt;
     *         &lt;/condition&gt;
     *         &lt;link target="ViewItem" text="${item.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestConditionalTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                condition = @TreeNodeCondition(
                    permission = "CATALOG",
                    permissionAction = "_VIEW"
                ),
                link = @TreeLink(target = "ViewItem", text = "${item.name}")
            )
        }
    )
    public interface TestConditionalTree {}

    /**
     * Example 6: Tree with include screen.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestIncludeScreenTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;include-screen name="TreeNodeContent"
     *                        location="component://myapp/widget/TreeScreens.xml"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestIncludeScreenTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                includeScreen = @TreeIncludeScreen(
                    name = "TreeNodeContent",
                    location = "component://myapp/widget/TreeScreens.xml"
                )
            )
        }
    )
    public interface TestIncludeScreenTree {}

    /**
     * Example 7: Tree with actions.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestActionsTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;actions&gt;
     *             &lt;set field="nodeTitle" value="Tree Node"/&gt;
     *         &lt;/actions&gt;
     *         &lt;label text="${nodeTitle}: ${item.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestActionsTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                actions = @TreeActions(
                    set = @SetAction(field = "nodeTitle", value = "Tree Node")
                ),
                label = @TreeLabel(text = "${nodeTitle}: ${item.name}")
            )
        }
    )
    public interface TestActionsTree {}

    /**
     * Example 8: Tree with out-field-map in sub-nodes.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestOutFieldMapTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="parent"&gt;
     *         &lt;label text="${parent.name}"/&gt;
     *         &lt;sub-node node-name="child"&gt;
     *             &lt;entity-and entity-name="ChildEntity" list="children"&gt;
     *                 &lt;field-map field-name="parentId" from-field="parent.id"/&gt;
     *             &lt;/entity-and&gt;
     *             &lt;out-field-map field-name="childId" to-field-name="selectedChildId"/&gt;
     *         &lt;/sub-node&gt;
     *     &lt;/node&gt;
     *     &lt;node name="child" entry-name="child"&gt;
     *         &lt;label text="${child.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestOutFieldMapTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "parent",
                label = @TreeLabel(text = "${parent.name}"),
                subNodes = {
                    @SubNode(nodeName = "child",
                        entityAnd = @EntityAnd(
                            entityName = "ChildEntity",
                            list = "children",
                            fieldMaps = @FieldMap(fieldName = "parentId", fromField = "parent.id")
                        ),
                        outFieldMaps = @OutFieldMap(fieldName = "childId", toFieldName = "selectedChildId")
                    )
                }
            ),
            @TreeNode(name = "child", entryName = "child",
                label = @TreeLabel(text = "${child.name}")
            )
        }
    )
    public interface TestOutFieldMapTree {}

    /**
     * Example 9: Tree with styled link and image.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestImageTree" root-node-name="root"&gt;
     *     &lt;node name="root" entry-name="item"&gt;
     *         &lt;link target="ViewItem" text="${item.name}" style="tree-link"&gt;
     *             &lt;image src="/images/folder.png" alt="Folder" style="tree-icon"/&gt;
     *         &lt;/link&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestImageTree",
        rootNodeName = "root",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                link = @TreeLink(
                    target = "ViewItem",
                    text = "${item.name}",
                    style = "tree-link",
                    image = @TreeImage(src = "/images/folder.png", alt = "Folder", style = "tree-icon")
                )
            )
        }
    )
    public interface TestImageTree {}

    /**
     * Example 10: Tree with show-peers render style.
     *
     * <p>Equivalent XML:</p>
     * <pre>
     * &lt;tree name="TestShowPeersTree" root-node-name="root"
     *       default-render-style="show-peers" default-wrap-style="tree-wrap"&gt;
     *     &lt;node name="root" entry-name="item" wrap-style="node-wrap"&gt;
     *         &lt;label text="${item.name}"/&gt;
     *     &lt;/node&gt;
     * &lt;/tree&gt;
     * </pre>
     */
    @Tree(
        name = "TestShowPeersTree",
        rootNodeName = "root",
        defaultRenderStyle = RenderStyle.SHOW_PEERS,
        defaultWrapStyle = "tree-wrap",
        nodes = {
            @TreeNode(name = "root", entryName = "item",
                wrapStyle = "node-wrap",
                useDefaultRenderStyle = true,
                label = @TreeLabel(text = "${item.name}")
            )
        }
    )
    public interface TestShowPeersTree {}
}
