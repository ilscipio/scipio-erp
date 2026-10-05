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
package com.ilscipio.scipio.entity.converter;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

/**
 * Converts entitymodel.xml to Java annotation-based entity definitions.
 *
 * <p>Generates @Entity, @ViewEntity annotated classes from entity XML definitions.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for XML-to-Annotation conversion support.</p>
 */
public class EntityXmlToAnnotationConverter {

    protected static final String INDENT = "    ";
    protected static final String NEWLINE = System.lineSeparator();

    protected final String packageName;
    protected final String className;
    protected final String componentName;
    protected final File outputDir;

    public EntityXmlToAnnotationConverter(String packageName, String className, String componentName, File outputDir) {
        this.packageName = packageName;
        this.className = className;
        this.componentName = componentName;
        this.outputDir = outputDir;
    }

    /**
     * Converts the entitymodel XML document to Java annotation source code.
     */
    public String convert(Document doc) {
        StringBuilder sb = new StringBuilder();
        sb.append(generateClassHeader());

        Element root = doc.getDocumentElement();

        // Generate Entity annotations from entity elements
        List<Element> entities = childElementList(root, "entity");
        for (Element entity : entities) {
            String entityCode = generateEntity(entity);
            if (isNotEmpty(entityCode)) {
                sb.append(NEWLINE);
                sb.append(entityCode);
            }
        }

        // Generate ViewEntity annotations from view-entity elements
        List<Element> viewEntities = childElementList(root, "view-entity");
        for (Element viewEntity : viewEntities) {
            String viewEntityCode = generateViewEntity(viewEntity);
            if (isNotEmpty(viewEntityCode)) {
                sb.append(NEWLINE);
                sb.append(viewEntityCode);
            }
        }

        // Generate ExtendEntity annotations from extend-entity elements
        List<Element> extendEntities = childElementList(root, "extend-entity");
        for (Element extendEntity : extendEntities) {
            String extendEntityCode = generateExtendEntity(extendEntity);
            if (isNotEmpty(extendEntityCode)) {
                sb.append(NEWLINE);
                sb.append(extendEntityCode);
            }
        }

        sb.append(NEWLINE);
        sb.append(generateClassFooter());
        return sb.toString();
    }

    // ========================================================================
    // Entity Generation
    // ========================================================================

    /**
     * Generates @Entity annotation for an entity element.
     */
    protected String generateEntity(Element entity) {
        String entityName = getAttr(entity, "entity-name");
        String packageNameAttr = getAttr(entity, "package-name");
        String tableName = getAttr(entity, "table-name");
        String title = getAttr(entity, "title");
        String description = childElementValue(entity, "description");
        String defaultResourceName = getAttr(entity, "default-resource-name");
        String dependentOn = getAttr(entity, "dependent-on");
        String sequenceBankSize = getAttr(entity, "sequence-bank-size");
        String enableLock = getAttr(entity, "enable-lock");
        String noAutoStamp = getAttr(entity, "no-auto-stamp");
        String neverCache = getAttr(entity, "never-cache");
        String neverCheck = getAttr(entity, "never-check");
        String autoClearCache = getAttr(entity, "auto-clear-cache");
        String redefinition = getAttr(entity, "redefinition");

        if (isEmpty(entityName)) return "";

        StringBuilder sb = new StringBuilder();
        String interfaceName = toInterfaceName(entityName);

        // Generate description comment if present
        if (isNotEmpty(title) || isNotEmpty(description)) {
            sb.append(INDENT).append("/**").append(NEWLINE);
            if (isNotEmpty(title)) {
                sb.append(INDENT).append(" * ").append(escapeJavadoc(title)).append(NEWLINE);
            }
            if (isNotEmpty(description)) {
                sb.append(INDENT).append(" * ").append(escapeJavadoc(description)).append(NEWLINE);
            }
            sb.append(INDENT).append(" */").append(NEWLINE);
        }

        // @Entity annotation
        sb.append(INDENT).append("@Entity(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(entityName));

        if (isNotEmpty(packageNameAttr)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("packageName = ").append(toStringValue(packageNameAttr));
        }
        if (isNotEmpty(tableName)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("tableName = ").append(toStringValue(tableName));
        }
        if (isNotEmpty(title)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("title = ").append(toStringValue(title));
        }
        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("description = ").append(toStringValue(description));
        }
        if (isNotEmpty(defaultResourceName)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("defaultResourceName = ").append(toStringValue(defaultResourceName));
        }
        if (isNotEmpty(dependentOn)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("dependentOn = ").append(toStringValue(dependentOn));
        }
        if (isNotEmpty(sequenceBankSize) && !"0".equals(sequenceBankSize)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("sequenceBankSize = ").append(sequenceBankSize);
        }
        if ("true".equals(enableLock)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("enableLock = true");
        }
        if ("true".equals(noAutoStamp)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("noAutoStamp = true");
        }
        if ("true".equals(neverCache)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("neverCache = true");
        }
        if ("true".equals(neverCheck)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("neverCheck = true");
        }
        if ("false".equals(autoClearCache)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("autoClearCache = false");
        }
        if ("true".equals(redefinition)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("redefinition = true");
        }

        // Fields
        List<Element> fields = childElementList(entity, "field");
        if (!fields.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("fields = {").append(NEWLINE);
            boolean first = true;
            for (Element field : fields) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateFieldNested(field, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Primary keys
        List<Element> primKeys = childElementList(entity, "prim-key");
        if (!primKeys.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("primaryKeys = {").append(NEWLINE);
            boolean first = true;
            for (Element primKey : primKeys) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generatePrimaryKeyNested(primKey, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Relations
        List<Element> relations = childElementList(entity, "relation");
        if (!relations.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("relations = {").append(NEWLINE);
            boolean first = true;
            for (Element relation : relations) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateRelationNested(relation, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Indexes
        List<Element> indexes = childElementList(entity, "index");
        if (!indexes.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("indexes = {").append(NEWLINE);
            boolean first = true;
            for (Element index : indexes) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateIndexNested(index, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(interfaceName).append("Entity {}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates @ViewEntity annotation for a view-entity element.
     */
    protected String generateViewEntity(Element viewEntity) {
        String entityName = getAttr(viewEntity, "entity-name");
        String packageNameAttr = getAttr(viewEntity, "package-name");
        String title = getAttr(viewEntity, "title");
        String description = childElementValue(viewEntity, "description");
        String defaultResourceName = getAttr(viewEntity, "default-resource-name");
        String dependentOn = getAttr(viewEntity, "dependent-on");
        String neverCache = getAttr(viewEntity, "never-cache");
        String autoClearCache = getAttr(viewEntity, "auto-clear-cache");
        String redefinition = getAttr(viewEntity, "redefinition");
        String aliasColumns = getAttr(viewEntity, "alias-columns");

        if (isEmpty(entityName)) return "";

        StringBuilder sb = new StringBuilder();
        String interfaceName = toInterfaceName(entityName);

        // Generate description comment if present
        if (isNotEmpty(title) || isNotEmpty(description)) {
            sb.append(INDENT).append("/**").append(NEWLINE);
            if (isNotEmpty(title)) {
                sb.append(INDENT).append(" * ").append(escapeJavadoc(title)).append(NEWLINE);
            }
            if (isNotEmpty(description)) {
                sb.append(INDENT).append(" * ").append(escapeJavadoc(description)).append(NEWLINE);
            }
            sb.append(INDENT).append(" */").append(NEWLINE);
        }

        // @ViewEntity annotation
        sb.append(INDENT).append("@ViewEntity(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(entityName));

        if (isNotEmpty(packageNameAttr)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("packageName = ").append(toStringValue(packageNameAttr));
        }
        if (isNotEmpty(title)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("title = ").append(toStringValue(title));
        }
        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("description = ").append(toStringValue(description));
        }
        if (isNotEmpty(defaultResourceName)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("defaultResourceName = ").append(toStringValue(defaultResourceName));
        }
        if (isNotEmpty(dependentOn)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("dependentOn = ").append(toStringValue(dependentOn));
        }
        if ("true".equals(neverCache)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("neverCache = true");
        }
        if ("false".equals(autoClearCache)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("autoClearCache = false");
        }
        if ("true".equals(redefinition)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("redefinition = true");
        }
        if (isNotEmpty(aliasColumns)) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("aliasColumns = ").append(toStringValue(aliasColumns));
        }

        // Member entities
        List<Element> members = childElementList(viewEntity, "member-entity");
        if (!members.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("members = {").append(NEWLINE);
            boolean first = true;
            for (Element member : members) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateMemberEntityNested(member, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Alias-alls
        List<Element> aliasAlls = childElementList(viewEntity, "alias-all");
        if (!aliasAlls.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("aliasAlls = {").append(NEWLINE);
            boolean first = true;
            for (Element aliasAll : aliasAlls) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateAliasAllNested(aliasAll, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Aliases
        List<Element> aliases = childElementList(viewEntity, "alias");
        if (!aliases.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("aliases = {").append(NEWLINE);
            boolean first = true;
            for (Element alias : aliases) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateAliasNested(alias, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // View links
        List<Element> viewLinks = childElementList(viewEntity, "view-link");
        if (!viewLinks.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("viewLinks = {").append(NEWLINE);
            boolean first = true;
            for (Element viewLink : viewLinks) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateViewLinkNested(viewLink, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Relations
        List<Element> relations = childElementList(viewEntity, "relation");
        if (!relations.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("relations = {").append(NEWLINE);
            boolean first = true;
            for (Element relation : relations) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateRelationNested(relation, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(interfaceName).append("View {}").append(NEWLINE);

        return sb.toString();
    }

    /**
     * Generates @ExtendEntity annotation for an extend-entity element.
     */
    protected String generateExtendEntity(Element extendEntity) {
        String entityName = getAttr(extendEntity, "entity-name");

        if (isEmpty(entityName)) return "";

        StringBuilder sb = new StringBuilder();
        String interfaceName = toInterfaceName(entityName);

        sb.append(INDENT).append("@ExtendEntity(").append(NEWLINE);
        sb.append(INDENT).append(INDENT).append("name = ").append(toStringValue(entityName));

        // Fields
        List<Element> fields = childElementList(extendEntity, "field");
        if (!fields.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("fields = {").append(NEWLINE);
            boolean first = true;
            for (Element field : fields) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateFieldNested(field, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Primary keys
        List<Element> primKeys = childElementList(extendEntity, "prim-key");
        if (!primKeys.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("primaryKeys = {").append(NEWLINE);
            boolean first = true;
            for (Element primKey : primKeys) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generatePrimaryKeyNested(primKey, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Relations
        List<Element> relations = childElementList(extendEntity, "relation");
        if (!relations.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("relations = {").append(NEWLINE);
            boolean first = true;
            for (Element relation : relations) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateRelationNested(relation, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        // Indexes
        List<Element> indexes = childElementList(extendEntity, "index");
        if (!indexes.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(INDENT).append(INDENT).append("indexes = {").append(NEWLINE);
            boolean first = true;
            for (Element index : indexes) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateIndexNested(index, 3));
                first = false;
            }
            sb.append(NEWLINE).append(INDENT).append(INDENT).append("}");
        }

        sb.append(NEWLINE);
        sb.append(INDENT).append(")").append(NEWLINE);
        sb.append(INDENT).append("public interface ").append(interfaceName).append("Extension {}").append(NEWLINE);

        return sb.toString();
    }

    // ========================================================================
    // Nested Annotation Generators
    // ========================================================================

    protected String generateFieldNested(Element field, int indentLevel) {
        String ind = indent(indentLevel);
        String name = getAttr(field, "name");
        String type = getAttr(field, "type");
        String colName = getAttr(field, "col-name");
        String description = childElementValue(field, "description");
        if (isEmpty(description)) {
            description = field.getTextContent();
            if (isNotEmpty(description)) {
                description = description.trim();
            }
        }
        String encrypt = getAttr(field, "encrypt");
        String enableAuditLog = getAttr(field, "enable-audit-log");
        String notNull = getAttr(field, "not-null");
        String fieldSet = getAttr(field, "field-set");
        String select = getAttr(field, "select");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@Field(name = ").append(toStringValue(name));
        sb.append(", type = ").append(toStringValue(type));

        if (isNotEmpty(colName)) {
            sb.append(", colName = ").append(toStringValue(colName));
        }
        if (isNotEmpty(description)) {
            sb.append(", description = ").append(toStringValue(description));
        }
        if (isNotEmpty(encrypt) && !"false".equals(encrypt)) {
            sb.append(", encrypt = ").append(toStringValue(encrypt));
        }
        if ("true".equals(enableAuditLog)) {
            sb.append(", enableAuditLog = true");
        }
        if ("true".equals(notNull)) {
            sb.append(", notNull = true");
        }
        if (isNotEmpty(fieldSet)) {
            sb.append(", fieldSet = ").append(toStringValue(fieldSet));
        }
        if ("false".equals(select)) {
            sb.append(", select = \"false\"");
        }

        sb.append(")");
        return sb.toString();
    }

    protected String generatePrimaryKeyNested(Element primKey, int indentLevel) {
        String ind = indent(indentLevel);
        String field = getAttr(primKey, "field");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@PrimaryKey(field = ").append(toStringValue(field)).append(")");
        return sb.toString();
    }

    protected String generateRelationNested(Element relation, int indentLevel) {
        String ind = indent(indentLevel);
        String type = getAttr(relation, "type");
        String relEntityName = getAttr(relation, "rel-entity-name");
        String title = getAttr(relation, "title");
        String description = getAttr(relation, "description");
        String fkName = getAttr(relation, "fk-name");

        List<Element> keyMaps = childElementList(relation, "key-map");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@Relation(").append(NEWLINE);
        sb.append(ind).append(INDENT).append("type = RelationType.").append(type.toUpperCase().replace("-", "_"));
        sb.append(",").append(NEWLINE);
        sb.append(ind).append(INDENT).append("relEntityName = ").append(toStringValue(relEntityName));

        if (isNotEmpty(title)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("title = ").append(toStringValue(title));
        }
        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("description = ").append(toStringValue(description));
        }
        if (isNotEmpty(fkName)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("fkName = ").append(toStringValue(fkName));
        }

        // Key maps
        if (!keyMaps.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("keyMaps = {").append(NEWLINE);
            boolean first = true;
            for (Element keyMap : keyMaps) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateKeyMapNested(keyMap, indentLevel + 2));
                first = false;
            }
            sb.append(NEWLINE).append(ind).append(INDENT).append("}");
        }

        sb.append(NEWLINE).append(ind).append(")");
        return sb.toString();
    }

    protected String generateKeyMapNested(Element keyMap, int indentLevel) {
        String ind = indent(indentLevel);
        String fieldName = getAttr(keyMap, "field-name");
        String relFieldName = getAttr(keyMap, "rel-field-name");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@KeyMap(fieldName = ").append(toStringValue(fieldName));
        if (isNotEmpty(relFieldName)) {
            sb.append(", relFieldName = ").append(toStringValue(relFieldName));
        }
        sb.append(")");
        return sb.toString();
    }

    protected String generateIndexNested(Element index, int indentLevel) {
        String ind = indent(indentLevel);
        String name = getAttr(index, "name");
        String unique = getAttr(index, "unique");
        String description = getAttr(index, "description");

        List<Element> indexFields = childElementList(index, "index-field");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@Index(").append(NEWLINE);
        sb.append(ind).append(INDENT).append("name = ").append(toStringValue(name));

        if ("true".equals(unique)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("unique = true");
        }
        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("description = ").append(toStringValue(description));
        }

        // Index fields
        if (!indexFields.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("fields = {").append(NEWLINE);
            boolean first = true;
            for (Element indexField : indexFields) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateIndexFieldNested(indexField, indentLevel + 2));
                first = false;
            }
            sb.append(NEWLINE).append(ind).append(INDENT).append("}");
        }

        sb.append(NEWLINE).append(ind).append(")");
        return sb.toString();
    }

    protected String generateIndexFieldNested(Element indexField, int indentLevel) {
        String ind = indent(indentLevel);
        String name = getAttr(indexField, "name");
        String function = getAttr(indexField, "function");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@IndexField(name = ").append(toStringValue(name));
        if (isNotEmpty(function)) {
            sb.append(", function = IndexFunction.").append(function.toUpperCase());
        }
        sb.append(")");
        return sb.toString();
    }

    protected String generateMemberEntityNested(Element member, int indentLevel) {
        String ind = indent(indentLevel);
        String entityAlias = getAttr(member, "entity-alias");
        String entityName = getAttr(member, "entity-name");
        String description = getAttr(member, "description");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@MemberEntity(entityAlias = ").append(toStringValue(entityAlias));
        sb.append(", entityName = ").append(toStringValue(entityName));
        if (isNotEmpty(description)) {
            sb.append(", description = ").append(toStringValue(description));
        }
        sb.append(")");
        return sb.toString();
    }

    protected String generateAliasAllNested(Element aliasAll, int indentLevel) {
        String ind = indent(indentLevel);
        String entityAlias = getAttr(aliasAll, "entity-alias");
        String prefix = getAttr(aliasAll, "prefix");
        String groupBy = getAttr(aliasAll, "group-by");
        String function = getAttr(aliasAll, "function");
        String fieldSet = getAttr(aliasAll, "field-set");
        String select = getAttr(aliasAll, "select");

        List<Element> excludes = childElementList(aliasAll, "exclude");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@AliasAll(entityAlias = ").append(toStringValue(entityAlias));

        if (isNotEmpty(prefix)) {
            sb.append(", prefix = ").append(toStringValue(prefix));
        }
        if ("true".equals(groupBy)) {
            sb.append(", groupBy = true");
        }
        if (isNotEmpty(function)) {
            sb.append(", function = AggregateFunction.").append(function.toUpperCase());
        }
        if (isNotEmpty(fieldSet)) {
            sb.append(", fieldSet = ").append(toStringValue(fieldSet));
        }
        if ("false".equals(select)) {
            sb.append(", select = \"false\"");
        }

        // Excludes
        if (!excludes.isEmpty()) {
            sb.append(", excludes = {");
            boolean first = true;
            for (Element exclude : excludes) {
                if (!first) sb.append(", ");
                sb.append(toStringValue(getAttr(exclude, "field")));
                first = false;
            }
            sb.append("}");
        }

        sb.append(")");
        return sb.toString();
    }

    protected String generateAliasNested(Element alias, int indentLevel) {
        String ind = indent(indentLevel);
        String name = getAttr(alias, "name");
        String entityAlias = getAttr(alias, "entity-alias");
        String field = getAttr(alias, "field");
        String colAlias = getAttr(alias, "col-alias");
        String primKey = getAttr(alias, "prim-key");
        String groupBy = getAttr(alias, "group-by");
        String function = getAttr(alias, "function");
        String fieldSet = getAttr(alias, "field-set");
        String select = getAttr(alias, "select");
        String description = getAttr(alias, "description");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@Alias(name = ").append(toStringValue(name));

        if (isNotEmpty(entityAlias)) {
            sb.append(", entityAlias = ").append(toStringValue(entityAlias));
        }
        if (isNotEmpty(field)) {
            sb.append(", field = ").append(toStringValue(field));
        }
        if (isNotEmpty(colAlias)) {
            sb.append(", colAlias = ").append(toStringValue(colAlias));
        }
        if (isNotEmpty(primKey)) {
            sb.append(", primKey = ").append(toStringValue(primKey));
        }
        if ("true".equals(groupBy)) {
            sb.append(", groupBy = true");
        }
        if (isNotEmpty(function)) {
            sb.append(", function = AggregateFunction.").append(function.toUpperCase());
        }
        if (isNotEmpty(fieldSet)) {
            sb.append(", fieldSet = ").append(toStringValue(fieldSet));
        }
        if ("false".equals(select)) {
            sb.append(", select = \"false\"");
        }
        if (isNotEmpty(description)) {
            sb.append(", description = ").append(toStringValue(description));
        }

        sb.append(")");
        return sb.toString();
    }

    protected String generateViewLinkNested(Element viewLink, int indentLevel) {
        String ind = indent(indentLevel);
        String entityAlias = getAttr(viewLink, "entity-alias");
        String relEntityAlias = getAttr(viewLink, "rel-entity-alias");
        String relOptional = getAttr(viewLink, "rel-optional");
        String description = getAttr(viewLink, "description");

        List<Element> keyMaps = childElementList(viewLink, "key-map");

        StringBuilder sb = new StringBuilder();
        sb.append(ind).append("@ViewLink(").append(NEWLINE);
        sb.append(ind).append(INDENT).append("entityAlias = ").append(toStringValue(entityAlias));
        sb.append(",").append(NEWLINE);
        sb.append(ind).append(INDENT).append("relEntityAlias = ").append(toStringValue(relEntityAlias));

        if ("true".equals(relOptional)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("relOptional = true");
        }
        if (isNotEmpty(description)) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("description = ").append(toStringValue(description));
        }

        // Key maps
        if (!keyMaps.isEmpty()) {
            sb.append(",").append(NEWLINE);
            sb.append(ind).append(INDENT).append("keyMaps = {").append(NEWLINE);
            boolean first = true;
            for (Element keyMap : keyMaps) {
                if (!first) sb.append(",").append(NEWLINE);
                sb.append(generateKeyMapNested(keyMap, indentLevel + 2));
                first = false;
            }
            sb.append(NEWLINE).append(ind).append(INDENT).append("}");
        }

        sb.append(NEWLINE).append(ind).append(")");
        return sb.toString();
    }

    // ========================================================================
    // File Output Methods
    // ========================================================================

    /**
     * Writes the generated source to a file.
     */
    public void writeToFile(String source) throws IOException {
        File packageDir = new File(outputDir, packageName.replace('.', File.separatorChar));
        packageDir.mkdirs();

        File javaFile = new File(packageDir, className + ".java");
        try (FileWriter writer = new FileWriter(javaFile)) {
            writer.write(source);
        }
    }

    /**
     * Gets the output Java file path.
     */
    public File getOutputFile() {
        File packageDir = new File(outputDir, packageName.replace('.', File.separatorChar));
        return new File(packageDir, className + ".java");
    }

    // ========================================================================
    // Code Generation Utilities
    // ========================================================================

    protected String generateClassHeader() {
        StringBuilder sb = new StringBuilder();

        // License header
        sb.append("/*").append(NEWLINE);
        sb.append(" * Licensed to the Apache Software Foundation (ASF) under one").append(NEWLINE);
        sb.append(" * or more contributor license agreements.  See the NOTICE file").append(NEWLINE);
        sb.append(" * distributed with this work for additional information").append(NEWLINE);
        sb.append(" * regarding copyright ownership.  The ASF licenses this file").append(NEWLINE);
        sb.append(" * to you under the Apache License, Version 2.0 (the").append(NEWLINE);
        sb.append(" * \"License\"); you may not use this file except in compliance").append(NEWLINE);
        sb.append(" * with the License.  You may obtain a copy of the License at").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * http://www.apache.org/licenses/LICENSE-2.0").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * Unless required by applicable law or agreed to in writing,").append(NEWLINE);
        sb.append(" * software distributed under the License is distributed on an").append(NEWLINE);
        sb.append(" * \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY").append(NEWLINE);
        sb.append(" * KIND, either express or implied.  See the License for the").append(NEWLINE);
        sb.append(" * specific language governing permissions and limitations").append(NEWLINE);
        sb.append(" * under the License.").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Package
        sb.append("package ").append(packageName).append(";").append(NEWLINE);
        sb.append(NEWLINE);

        // Imports
        sb.append("import com.ilscipio.scipio.entity.def.*;").append(NEWLINE);
        sb.append(NEWLINE);

        // Class javadoc
        sb.append("/**").append(NEWLINE);
        sb.append(" * Auto-generated annotation-based entity definitions.").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>").append(NEWLINE);
        sb.append(" *").append(NEWLINE);
        sb.append(" * <p>SCIPIO: 4.0.0: Auto-generated.</p>").append(NEWLINE);
        sb.append(" */").append(NEWLINE);

        // Class declaration
        sb.append("public class ").append(className).append(" {").append(NEWLINE);

        return sb.toString();
    }

    protected String generateClassFooter() {
        return "}" + NEWLINE;
    }

    protected String indent(int level) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < level; i++) {
            sb.append(INDENT);
        }
        return sb.toString();
    }

    protected String toStringValue(String value) {
        if (value == null) return "\"\"";
        return "\"" + escapeString(value) + "\"";
    }

    protected String escapeString(String s) {
        if (s == null) return null;
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    protected String escapeJavadoc(String s) {
        if (s == null) return "";
        return s.replace("*/", "* /")
                .replace("\n", " ")
                .replace("\r", "");
    }

    protected String toInterfaceName(String name) {
        if (isEmpty(name)) return "Unknown";
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : name.toCharArray()) {
            if (c == '-' || c == '_' || c == '.') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            }
        }
        String result = sb.toString();
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result = "_" + result;
        }
        return result;
    }

    protected String getAttr(Element element, String attrName) {
        return element != null ? element.getAttribute(attrName) : "";
    }

    protected String childElementValue(Element parent, String tagName) {
        Element child = firstChildElement(parent, tagName);
        if (child == null) return null;
        return child.getTextContent();
    }

    // ========================================================================
    // DOM Utilities
    // ========================================================================

    protected List<Element> childElementList(Element parent, String tagName) {
        List<Element> result = new ArrayList<>();
        if (parent == null) return result;
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                Element element = (Element) child;
                if (tagName == null || tagName.equals(element.getTagName())) {
                    result.add(element);
                }
            }
        }
        return result;
    }

    protected Element firstChildElement(Element parent, String tagName) {
        if (parent == null) return null;
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                Element element = (Element) child;
                if (tagName == null || tagName.equals(element.getTagName())) {
                    return element;
                }
            }
        }
        return null;
    }

    protected static boolean isEmpty(String s) {
        return s == null || s.isEmpty();
    }

    protected static boolean isNotEmpty(String s) {
        return s != null && !s.isEmpty();
    }
}
