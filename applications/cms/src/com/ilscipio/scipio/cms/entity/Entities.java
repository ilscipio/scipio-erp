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
package com.ilscipio.scipio.cms.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Entities {

    /**
     * CMS User Preview Access Token
     */
    @Entity(
        name = "CmsAccessToken",
        packageName = "com.ilscipio.scipio.cms.internal.security",
        title = "CMS User Preview Access Token",
        fields = {
            @Field(name = "tokenId", type = "id-ne"),
            @Field(name = "token", type = "long-varchar"),
            @Field(name = "userId", type = "id-vlong-ne"),
            @Field(name = "pageId", type = "id"),
            @Field(name = "createdDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "tokenId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "CMSACSTKN_USER_ID",
                keyMaps = {
                    @KeyMap(fieldName = "userId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface CmsAccessTokenEntity {}

    /**
     * Page
     */
    @Entity(
        name = "CmsPage",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Page",
        fields = {
            @Field(name = "pageId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id-ne", description = "Organizational and editing page default webSiteId"),
            @Field(name = "pageTemplateId", type = "id"),
            @Field(name = "pageName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "txTimeout", type = "very-long", description = "Transaction timeout; accepts any integer; -1 for default; 0 to disable transaction begin.\n            Default: -1. Supports flexible expressions that return an integer.")
        },
        primaryKeys = {
            @PrimaryKey(field = "pageId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSPAGE_TMPL",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            )
        }
    )
    public interface CmsPageEntity {}

    /**
     * Page Version
     */
    @Entity(
        name = "CmsPageVersion",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Page Version",
        fields = {
            @Field(name = "versionId", type = "id-ne"),
            @Field(name = "pageId", type = "id-ne", notNull = true),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "createdBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "versionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPAGEVER_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSPAGEVER_CNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CmsPageVersionEntity {}

    /**
     * Page Version State
     */
    @Entity(
        name = "CmsPageVersionState",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Page Version State",
        fields = {
            @Field(name = "pageId", type = "id-ne"),
            @Field(name = "versionStateId", type = "id-ne", description = "One of: CMS_VER_ACTIVE, or other CMS_VER_STATE enumeration type"),
            @Field(name = "versionId", type = "id-ne", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "pageId"),
            @PrimaryKey(field = "versionStateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPGVST_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "VersionState",
                fkName = "CMSPGVST_VERSTATE",
                keyMaps = {
                    @KeyMap(fieldName = "versionStateId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageVersion",
                fkName = "CMSPGVST_PAGEVER",
                keyMaps = {
                    @KeyMap(fieldName = "versionId", relFieldName = "versionId")
                }
            )
        }
    )
    public interface CmsPageVersionStateEntity {}

    /**
     * Page Authorization
     */
    @Entity(
        name = "CmsPageAuthorization",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Page Authorization",
        fields = {
            @Field(name = "pageAuthId", type = "id-ne"),
            @Field(name = "pageId", type = "id-ne", notNull = true),
            @Field(name = "userId", type = "id-vlong"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "groupId", type = "id", description = "Can be used as an alternate to userId to specify a security group of users to which to\n          assign the given role type. Only one of the two may be set.")
        },
        primaryKeys = {
            @PrimaryKey(field = "pageAuthId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPAGEAUTH_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "CMSPAGEAUTH_USERL",
                keyMaps = {
                    @KeyMap(fieldName = "userId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "CMSPAGEAUTH_ROLTYP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "CMSPAGEAUTH_SECGRP",
                keyMaps = {
                    @KeyMap(fieldName = "groupId", relFieldName = "groupId")
                }
            )
        }
    )
    public interface CmsPageAuthorizationEntity {}

    /**
     * Page Product Association
     */
    @Entity(
        name = "CmsPageProductAssoc",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Page Product Association",
        fields = {
            @Field(name = "pageProductAssocId", type = "id-ne"),
            @Field(name = "pageId", type = "id-ne", notNull = true),
            @Field(name = "productId", type = "id-ne", notNull = true),
            @Field(name = "importName", type = "name", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "pageProductAssocId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPGPRASS_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "CMSPGPRASS_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface CmsPageProductAssocEntity {}

    /**
     * Script To Page Association
     */
    @Entity(
        name = "CmsPageScriptAssoc",
        packageName = "com.ilscipio.scipio.cms.content",
        tableName = "CMS_PAGE_SCRIPTASSOC",
        title = "Script To Page Association",
        fields = {
            @Field(name = "scriptAssocId", type = "id-ne"),
            @Field(name = "scriptTemplateId", type = "id-ne", notNull = true),
            @Field(name = "pageId", type = "id-ne", notNull = true),
            @Field(name = "inputPosition", type = "numeric"),
            @Field(name = "invokeName", type = "name", description = "Method or function to invoke within the script template at execution time"),
            @Field(name = "lastUpdatedBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "scriptAssocId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPGSCRASS_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsScriptTemplate",
                fkName = "CMSPGSCRASS_SCTMP",
                keyMaps = {
                    @KeyMap(fieldName = "scriptTemplateId")
                }
            )
        }
    )
    public interface CmsPageScriptAssocEntity {}

    /**
     * Page Template
     */
    @Entity(
        name = "CmsPageTemplate",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Page Template",
        fields = {
            @Field(name = "pageTemplateId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id", description = "Organizational and editing page webSiteId (not used in rendering)"),
            @Field(name = "templateName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "lastUpdatedBy", type = "id"),
            @Field(name = "activeContentId", type = "id", description = "Optimization: Direct reference to the active template contentId; if null, forces regular full version lookup."),
            @Field(name = "txTimeout", type = "very-long", description = "Transaction timeout; accepts any integer; -1 for default; 0 to disable transaction begin.\n            Default: -1. Supports flexible expressions that return an integer.")
        },
        primaryKeys = {
            @PrimaryKey(field = "pageTemplateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSPTMP_ACNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "activeContentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface CmsPageTemplateEntity {}

    /**
     * Page Template Version
     */
    @Entity(
        name = "CmsPageTemplateVersion",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Page Template Version",
        fields = {
            @Field(name = "versionId", type = "id-ne"),
            @Field(name = "pageTemplateId", type = "id-ne", notNull = true),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "origVersionDate", type = "date-time", description = "This field is used if the default timestamp fields don't reflect the logical version date.")
        },
        primaryKeys = {
            @PrimaryKey(field = "versionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSPTMPV_PAGETMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSPTMPV_CNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CmsPageTemplateVersionEntity {}

    /**
     * Page Template Version State
     */
    @Entity(
        name = "CmsPageTemplateVersionState",
        packageName = "com.ilscipio.scipio.cms.template",
        tableName = "CMS_PAGE_TEMPLATE_VERSTATE",
        title = "Page Template Version State",
        fields = {
            @Field(name = "pageTemplateId", type = "id-ne"),
            @Field(name = "versionStateId", type = "id-ne", description = "One of: CMS_VER_ACTIVE, or other CMS_VER_STATE enumeration type"),
            @Field(name = "versionId", type = "id-ne", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "pageTemplateId"),
            @PrimaryKey(field = "versionStateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSPGTVST_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "VersionState",
                fkName = "CMSPGTVST_VERSTATE",
                keyMaps = {
                    @KeyMap(fieldName = "versionStateId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplateVersion",
                fkName = "CMSPGTVST_PGTMPVER",
                keyMaps = {
                    @KeyMap(fieldName = "versionId")
                }
            )
        }
    )
    public interface CmsPageTemplateVersionStateEntity {}

    /**
     * Asset To Page Template Association
     */
    @Entity(
        name = "CmsPageTemplateAssetAssoc",
        packageName = "com.ilscipio.scipio.cms.template",
        tableName = "CMS_PAGE_TEMPLATE_ASSETASSOC",
        title = "Asset To Page Template Association",
        fields = {
            @Field(name = "pageAssetTemplateAssocId", type = "id-ne"),
            @Field(name = "assetTemplateId", type = "id-ne", notNull = true),
            @Field(name = "pageTemplateId", type = "id-ne", notNull = true),
            @Field(name = "importName", type = "name", notNull = true),
            @Field(name = "displayName", type = "name", description = "Display name. If omitted, falls back on asset display name."),
            @Field(name = "inputPosition", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "pageAssetTemplateAssocId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSPGASSTASS_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplate",
                fkName = "CMSPGASSTASS_ASTMP",
                keyMaps = {
                    @KeyMap(fieldName = "assetTemplateId")
                }
            )
        }
    )
    public interface CmsPageTemplateAssetAssocEntity {}

    /**
     * Script To Page Template Association
     */
    @Entity(
        name = "CmsPageTemplateScriptAssoc",
        packageName = "com.ilscipio.scipio.cms.template",
        tableName = "CMS_PAGE_TEMPLATE_SCRIPTASSOC",
        title = "Script To Page Template Association",
        fields = {
            @Field(name = "scriptAssocId", type = "id-ne"),
            @Field(name = "scriptTemplateId", type = "id-ne", notNull = true),
            @Field(name = "pageTemplateId", type = "id-ne", notNull = true),
            @Field(name = "inputPosition", type = "numeric"),
            @Field(name = "invokeName", type = "name", description = "Method or function to invoke within the script template at execution time"),
            @Field(name = "lastUpdatedBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "scriptAssocId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSPGTSCRASS_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsScriptTemplate",
                fkName = "CMSPGTSCRASS_SCTMP",
                keyMaps = {
                    @KeyMap(fieldName = "scriptTemplateId")
                }
            )
        }
    )
    public interface CmsPageTemplateScriptAssocEntity {}

    /**
     * Asset Template
     */
    @Entity(
        name = "CmsAssetTemplate",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Asset Template",
        fields = {
            @Field(name = "assetTemplateId", type = "id-ne"),
            @Field(name = "assetType", type = "id", description = "High-level template body type: TEMPLATE (default), CONTENT"),
            @Field(name = "webSiteId", type = "id", description = "Organizational and editing page webSiteId (not used in rendering)"),
            @Field(name = "templateName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "contentTypeId", type = "id", description = "Points to a ContentType having parentTypeId SCP_TEMPLATE_PART"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "lastUpdatedBy", type = "id"),
            @Field(name = "activeContentId", type = "id", description = "Optimization: Direct reference to the active template contentId; if null, forces regular full version lookup."),
            @Field(name = "txTimeout", type = "very-long", description = "Transaction timeout; accepts any integer; -1 for default; 0 to disable transaction begin.\n            Default: -1. Supports flexible expressions that return an integer. NOTE: Will usually not have much effect on asset templates,\n            but can be used to ensure a transaction of certain length if ever rendered outside a transaction")
        },
        primaryKeys = {
            @PrimaryKey(field = "assetTemplateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSASTMP_ACNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "activeContentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface CmsAssetTemplateEntity {}

    /**
     * Asset Template Version
     */
    @Entity(
        name = "CmsAssetTemplateVersion",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Asset Template Version",
        fields = {
            @Field(name = "versionId", type = "id-ne"),
            @Field(name = "assetTemplateId", type = "id-ne", notNull = true),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "origVersionDate", type = "date-time", description = "This field is used if the default timestamp fields don't reflect the logical version date.")
        },
        primaryKeys = {
            @PrimaryKey(field = "versionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplate",
                fkName = "CMSASTMPV_ASSTMP",
                keyMaps = {
                    @KeyMap(fieldName = "assetTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSASTMPV_CNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CmsAssetTemplateVersionEntity {}

    /**
     * Asset Template Version State
     */
    @Entity(
        name = "CmsAssetTemplateVersionState",
        packageName = "com.ilscipio.scipio.cms.template",
        tableName = "CMS_ASSET_TEMPLATE_VERSTATE",
        title = "Asset Template Version State",
        fields = {
            @Field(name = "assetTemplateId", type = "id-ne"),
            @Field(name = "versionStateId", type = "id-ne", description = "One of: CMS_VER_ACTIVE, or other CMS_VER_STATE enumeration type"),
            @Field(name = "versionId", type = "id-ne", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "assetTemplateId"),
            @PrimaryKey(field = "versionStateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplate",
                fkName = "CMSASTVST_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "assetTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "VersionState",
                fkName = "CMSASTVST_VERSTATE",
                keyMaps = {
                    @KeyMap(fieldName = "versionStateId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplateVersion",
                fkName = "CMSASTVST_PGTMPVER",
                keyMaps = {
                    @KeyMap(fieldName = "versionId")
                }
            )
        }
    )
    public interface CmsAssetTemplateVersionStateEntity {}

    /**
     * Script To Asset Template Association
     */
    @Entity(
        name = "CmsAssetTemplateScriptAssoc",
        packageName = "com.ilscipio.scipio.cms.template",
        tableName = "CMS_ASSET_TEMPLATE_SCRIPTASSOC",
        title = "Script To Asset Template Association",
        fields = {
            @Field(name = "scriptAssocId", type = "id-ne"),
            @Field(name = "scriptTemplateId", type = "id-ne", notNull = true),
            @Field(name = "assetTemplateId", type = "id-ne", notNull = true),
            @Field(name = "inputPosition", type = "numeric"),
            @Field(name = "invokeName", type = "name", description = "Method or function to invoke within the script template at execution time"),
            @Field(name = "lastUpdatedBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "scriptAssocId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplate",
                fkName = "CMSASTSCRASS_ASTMP",
                keyMaps = {
                    @KeyMap(fieldName = "assetTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsScriptTemplate",
                fkName = "CMSASTSCRASS_SCTMP",
                keyMaps = {
                    @KeyMap(fieldName = "scriptTemplateId")
                }
            )
        }
    )
    public interface CmsAssetTemplateScriptAssocEntity {}

    /**
     * Attribute Template
     */
    @Entity(
        name = "CmsAttributeTemplate",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Attribute Template",
        fields = {
            @Field(name = "attributeTemplateId", type = "id-ne"),
            @Field(name = "assetTemplateId", type = "id"),
            @Field(name = "pageTemplateId", type = "id"),
            @Field(name = "attributeName", type = "name"),
            @Field(name = "displayName", type = "name"),
            @Field(name = "defaultValue", type = "long-varchar"),
            @Field(name = "inputHelp", type = "long-varchar"),
            @Field(name = "inputType", type = "id"),
            @Field(name = "permission", type = "id"),
            @Field(name = "maxLength", type = "numeric"),
            @Field(name = "regularExpression", type = "long-varchar"),
            @Field(name = "required", type = "indicator"),
            @Field(name = "expandLang", type = "id", description = "The expansion language. \n        Possible values (2017-02-20): NONE (default), FLEXIBLE (${map1.field1}), SIMPLE ({{map1.field1}}), FTL."),
            @Field(name = "expandPosition", type = "numeric", description = "The moment of expansion, relative to CmsAsset/PageTemplateScriptAssoc.inputPosition (for the same value, attributes always run first).\n        Default: 0 (runs before all scripts). NOTE: For expandLang=FTL, this has no real effect, because the ftl code will only evaluate once included by the template."),
            @Field(name = "inputPosition", type = "numeric", description = "Order of evaluation of attributes with respect to other attributes (only).\n        NOTE: This is slave to expandPosition, and has no influence on order relative to scripts."),
            @Field(name = "targetType", type = "name", description = "A Java class indicating the target type: String, Integer, java.util.String, ... uses the standard Ofbiz conversion utilities."),
            @Field(name = "inheritMode", type = "id", description = "ONE: NEVER (default), ATTR_EMPTY, FIELD_NON_NULL, FIELD_NON_EMPTY")
        },
        primaryKeys = {
            @PrimaryKey(field = "attributeTemplateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsAssetTemplate",
                fkName = "CMSATTRTMP_ASTMP",
                keyMaps = {
                    @KeyMap(fieldName = "assetTemplateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPageTemplate",
                fkName = "CMSATTRTMP_PGTMP",
                keyMaps = {
                    @KeyMap(fieldName = "pageTemplateId")
                }
            )
        }
    )
    public interface CmsAttributeTemplateEntity {}

    /**
     * Script Template
     */
    @Entity(
        name = "CmsScriptTemplate",
        packageName = "com.ilscipio.scipio.cms.template",
        title = "Script Template",
        fields = {
            @Field(name = "scriptTemplateId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id", description = "Organizational and editing page webSiteId (not used in rendering)"),
            @Field(name = "templateName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "scriptLang", type = "id", description = "Explicit script language - required if no location, frequently inferred from location.\n            Possible values (non-exhaustive): groovy, screen-actions, simple-method, auto, none. Default: auto.\n            See also: com.ilscipio.scipio.cms.template.CmsScriptTemplate.ScriptLang"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "standalone", type = "indicator", description = "If set to N, the record is deleted once it becomes orphan (not associated to any page or asset template)")
        },
        primaryKeys = {
            @PrimaryKey(field = "scriptTemplateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CMSSCRTMP_CNTNTID",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CmsScriptTemplateEntity {}

    /**
     * View to Page Mapping
     */
    @Entity(
        name = "CmsViewMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        title = "View to Page Mapping",
        fields = {
            @Field(name = "viewMappingId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id-ne", description = "Mapping webSiteId (matched and used in rendering)", notNull = true),
            @Field(name = "targetServletPath", type = "long-varchar", description = "Servlet path, from webapp root, for target view matching;\n          CANNOT BE EMPTY; if set to special value \"DEFAULT\", defaults to ControlServlet mapping path or value of cmsDefaultTargetServletPath web.xml context-param.\n          This ensures the correct servlet is being hooked into (for correctness and exotic configurations).", notNull = true),
            @Field(name = "targetViewName", type = "name", notNull = true),
            @Field(name = "pageId", type = "id-ne", description = "The page this view mapping maps to", notNull = true),
            @Field(name = "active", type = "indicator", description = "Indicates whether this mapping is active/enabled")
        },
        primaryKeys = {
            @PrimaryKey(field = "viewMappingId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSVWMAP_PAGEID",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CMS_VWMAP_VIEWKEY",
                unique = true,
                fields = {
                    @IndexField(name = "webSiteId"),
                    @IndexField(name = "targetServletPath"),
                    @IndexField(name = "targetViewName")
                }
            ),
            @Index(
                name = "CMS_VWMAP_WEBSITE",
                fields = {
                    @IndexField(name = "webSiteId")
                }
            )
        }
    )
    public interface CmsViewMappingEntity {}

    /**
     * Process to Page Mapping
     */
    @Entity(
        name = "CmsProcessMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        title = "Process to Page Mapping",
        fields = {
            @Field(name = "processMappingId", type = "id-ne"),
            @Field(name = "sourceWebSiteId", type = "id-ne", description = "Mapping source webSiteId (matched and used in rendering)", notNull = true),
            @Field(name = "sourcePath", type = "long-varchar", description = "Entry path, from webapp (servlet context) root", notNull = true),
            @Field(name = "sourceFromContextRoot", type = "indicator", description = "Ternary non-null indicator (\"Y\", \"N\", \"D\"); \n            indicates if the source path should match from webapp (servlet context) root (\"Y\");\n            if false (\"N\"), instead matches from servlet root of /control or cmsDefaultSourceServletPath web.xml context-param;\n            CANNOT BE EMPTY - special value \"D\" indicates default for website, controlled by cmsDefaultSourceFromContextRoot web.xml context-param", notNull = true),
            @Field(name = "forwardPath", type = "long-varchar", description = "Path, by default from webapp (servlet context) root,\n          to forward to when source path encountered; if forwardFromContextRoot is set, this path is appended to the\n          cmsDefaultForwardServletPath web.xml context-param value before forwarding.\n          default used:\n           - if alwaysUseForwardServletPath (process filter init param) is not set (default): targetServletPath+targetPath\n           - if alwaysUseForwardServletPath is set: defaultForwardServletPath+targetPath"),
            @Field(name = "forwardFromContextRoot", type = "indicator", description = "Indicates if the forward path should be from webapp (servlet context) root;\n          if false, instead forwards from servlet root of /control or cmsDefaultForwardServletPath web.xml context-param; defaults (if empty) to Y;\n          only takes effect if forwardPath non-empty."),
            @Field(name = "forwardExtraPathInfo", type = "indicator", description = "If Y, any extra path info found in the request path after\n          matching with the source path is appended to the forwarded path (e.g. sourcePath /hello, forward path /toYou, request path /hello/fromMe,\n          forwarded path will be /toYou/fromMe); if N, extra path info is discarded;\n          if the process mapping is intended as a wildcard process mapping, this should be set to N;\n          default is Y (in Ofbiz fashion) or the corresponding boolean set in process filter cmsDefaultForwardExtraPathInfo web.xml context-param"),
            @Field(name = "targetServletPath", type = "long-varchar", description = "Default servlet path, from webapp root, for target view matching;\n          defaults to /control or value of cmsDefaultTargetServletPath web.xml context-param"),
            @Field(name = "targetPath", type = "long-varchar", description = "Default request path, from (control) servlet root, for target view matching.\n          This is not strictly required but will generally be present."),
            @Field(name = "matchAnyTargetPath", type = "indicator", description = "Match any target path; defaults (if empty) to N"),
            @Field(name = "pageId", type = "id", description = "Default page for all target views"),
            @Field(name = "active", type = "indicator", description = "Indicates whether this mapping is active/enabled"),
            @Field(name = "requireExtraPathInfo", type = "indicator", description = "If Y, this process mapping will only take effect and forward\n          if there is extra directory/path info after the matching source path; if N, will forward regardless of whether extra path info or not;\n          can be set to Y to prevent forwarding to the base CMS pages in wildcard mappings;\n          note this flag does not factor into source path matching semantics (only cancels after matching done);\n          default is N"),
            @Field(name = "primaryForPageId", type = "id", description = "A page ID for which this mapping\n          record is considered a \"primary\" process mapping or primary path, for the given webSiteId.\n          The meaning of this is intentionally generic, but currently (2016-11-23) it means\n          this record contains and implements a simple direct path as sourcePath through which to view\n          the page, and the targetPath will be one of the special cmsPageXxx controller requests.\n          Currently (2016-11-23) each page will only have one of these (constrained by both\n          the single webSiteId currently permitted by the UI per page, and\n          then each website having only one of these per page).\n          This field is an optimization for CmsPageSpecialMapping entity and helps in code and query simplification."),
            @Field(name = "indexable", type = "indicator", description = "If Y (or not set and website default is Y),\n          the sourcePath will be indexed in sitemap generation. N prevents."),
            @Field(name = "searchIndexable", type = "indicator", description = "If Y (or not set and website default is N),\n            the sourcePath will be indexed in internal search engines (solr/algolia etc). N prevents.")
        },
        primaryKeys = {
            @PrimaryKey(field = "processMappingId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPRMAP_PAGEID",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                title = "Primary",
                fkName = "CMSPRMAP_PRPAGEID",
                keyMaps = {
                    @KeyMap(fieldName = "primaryForPageId", relFieldName = "pageId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CMS_PRMAP_SRCKEY",
                unique = true,
                fields = {
                    @IndexField(name = "sourceWebSiteId"),
                    @IndexField(name = "sourcePath"),
                    @IndexField(name = "sourceFromContextRoot")
                }
            ),
            @Index(
                name = "CMS_PRMAP_WEBSITE",
                fields = {
                    @IndexField(name = "sourceWebSiteId")
                }
            )
        }
    )
    public interface CmsProcessMappingEntity {}

    /**
     * Process View Mapping
     */
    @Entity(
        name = "CmsProcessViewMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        title = "Process View Mapping",
        fields = {
            @Field(name = "processViewMappingId", type = "id-ne"),
            @Field(name = "processMappingId", type = "id-ne", description = "Parent process mapping", notNull = true),
            @Field(name = "targetServletPath", type = "long-varchar", description = "Target servlet path; defaults (if empty) to value of parent process's field"),
            @Field(name = "targetPath", type = "long-varchar", description = "Target request path; defaults (if empty) to value of parent process's field"),
            @Field(name = "matchAnyTargetPath", type = "indicator", description = "Match any target path; defaults (if empty) to value of parent process's field"),
            @Field(name = "targetViewName", type = "name", notNull = true),
            @Field(name = "pageId", type = "id", description = "Target page; defaults (if empty) to value of parent process's field"),
            @Field(name = "active", type = "indicator", description = "Indicates whether this mapping is active/enabled")
        },
        primaryKeys = {
            @PrimaryKey(field = "processViewMappingId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsProcessMapping",
                fkName = "CMSPRVWMP_PRMPID",
                keyMaps = {
                    @KeyMap(fieldName = "processMappingId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPRVWMP_PAGEID",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CMS_PRMAP_PARENTPR",
                fields = {
                    @IndexField(name = "processMappingId")
                }
            )
        }
    )
    public interface CmsProcessViewMappingEntity {}

    /**
     * Page Special Mapping
     */
    @Entity(
        name = "CmsPageSpecialMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        title = "Page Special Mapping",
        fields = {
            @Field(name = "pageId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "mappingTypeId", type = "id-ne", description = "Enumeration of type CMS_PAGE_SPCMAP_TYPE"),
            @Field(name = "processMappingId", type = "id-ne", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "pageId"),
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "mappingTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsPage",
                fkName = "CMSPGPRMP_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "pageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CmsProcessMapping",
                fkName = "CMSPGPRMP_PRMAP",
                keyMaps = {
                    @KeyMap(fieldName = "processMappingId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "CMSPGPRMP_PRMTYP",
                keyMaps = {
                    @KeyMap(fieldName = "mappingTypeId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface CmsPageSpecialMappingEntity {}

    /**
     * Menu
     */
    @Entity(
        name = "CmsMenu",
        packageName = "com.ilscipio.scipio.cms.content",
        title = "Menu",
        fields = {
            @Field(name = "menuId", type = "id-ne"),
            @Field(name = "websiteId", type = "id-ne"),
            @Field(name = "menuName", type = "name"),
            @Field(name = "description", type = "very-long"),
            @Field(name = "menuJson", type = "very-long", description = "A Json formated menu list. Example:\n        {\n          id          : \"string\" // required \n          parent      : \"string\" // required\n          text        : \"string\" // node text\n          icon        : \"string\" // string for custom\n          li_attr     : {}  // attributes for the generated LI node\n          a_attr      : {}  // attributes for the generated A node\n          content     : {}  // HTML content\n        }\n      "),
            @Field(name = "createdBy", type = "id"),
            @Field(name = "lastUpdatedBy", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "menuId")
        }
    )
    public interface CmsMenuEntity {}

    @ViewEntity(
        name = "CmsPageUserAuth",
        packageName = "com.ilscipio.scipio.cms.content",
        members = {
            @MemberEntity(entityAlias = "PPA", entityName = "CmsPageAuthorization")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PPA")
        }
    )
    public interface CmsPageUserAuthView {}

    @ViewEntity(
        name = "CmsPageGroupAuth",
        packageName = "com.ilscipio.scipio.cms.content",
        members = {
            @MemberEntity(entityAlias = "PPA", entityName = "CmsPageAuthorization")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PPA")
        }
    )
    public interface CmsPageGroupAuthView {}

    @ViewEntity(
        name = "CmsPageGroupAuthAndSecurityGroup",
        packageName = "com.ilscipio.scipio.cms.content",
        members = {
            @MemberEntity(entityAlias = "PPGA", entityName = "CmsPageGroupAuth"),
            @MemberEntity(entityAlias = "SG", entityName = "SecurityGroup")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PPGA")
        },
        aliases = {
            @Alias(name = "groupDesc", entityAlias = "SG", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PPGA",
                relEntityAlias = "SG",
                keyMaps = {
                    @KeyMap(fieldName = "groupId", relFieldName = "groupId")
                }
            )
        }
    )
    public interface CmsPageGroupAuthAndSecurityGroupView {}

    /**
     * Page and Primary Process Mapping
     */
    @ViewEntity(
        name = "CmsPageAndPrimaryProcessMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        title = "Page and Primary Process Mapping",
        members = {
            @MemberEntity(entityAlias = "PG", entityName = "CmsPage"),
            @MemberEntity(entityAlias = "PPM", entityName = "CmsProcessMapping")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PG"),
            @AliasAll(entityAlias = "PPM", excludes = {"pageId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PG",
                relEntityAlias = "PPM",
                keyMaps = {
                    @KeyMap(fieldName = "pageId", relFieldName = "primaryForPageId")
                }
            )
        }
    )
    public interface CmsPageAndPrimaryProcessMappingView {}

    @ViewEntity(
        name = "CmsProcessAndViewMapping",
        packageName = "com.ilscipio.scipio.cms.control",
        members = {
            @MemberEntity(entityAlias = "PRM", entityName = "CmsProcessMapping"),
            @MemberEntity(entityAlias = "PRVM", entityName = "CmsProcessViewMapping")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PRM", excludes = {"targetServletPath", "targetPath", "matchAnyTargetPath", "pageId", "active", "createdStamp", "lastUpdatedStamp"}),
            @AliasAll(entityAlias = "PRVM", excludes = {"processMappingId"})
        },
        aliases = {
            @Alias(name = "createdStamp", entityAlias = "PRVM", field = "createdStamp"),
            @Alias(name = "lastUpdatedStamp", entityAlias = "PRVM", field = "lastUpdatedStamp"),
            @Alias(name = "processTargetServletPath", entityAlias = "PRM", field = "targetServletPath"),
            @Alias(name = "processTargetPath", entityAlias = "PRM", field = "targetPath"),
            @Alias(name = "processMatchAnyTargetPath", entityAlias = "PRM", field = "matchAnyTargetPath"),
            @Alias(name = "processPageId", entityAlias = "PRM", field = "pageId"),
            @Alias(name = "processPrimaryForPageId", entityAlias = "PRM", field = "primaryForPageId"),
            @Alias(name = "processActive", entityAlias = "PRM", field = "active")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PRM",
                relEntityAlias = "PRVM",
                keyMaps = {
                    @KeyMap(fieldName = "processMappingId")
                }
            )
        }
    )
    public interface CmsProcessAndViewMappingView {}

    /**
     * DataResource Media File View
     */
    @ViewEntity(
        name = "DataResourceMediaFileView",
        packageName = "com.ilscipio.scipio.cms.media",
        title = "DataResource Media File View",
        members = {
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "CNT", entityName = "Content"),
            @MemberEntity(entityAlias = "VDR", entityName = "VideoDataResource"),
            @MemberEntity(entityAlias = "IDR", entityName = "ImageDataResource"),
            @MemberEntity(entityAlias = "ADR", entityName = "AudioDataResource"),
            @MemberEntity(entityAlias = "DDR", entityName = "DocumentDataResource"),
            @MemberEntity(entityAlias = "ODR", entityName = "OtherDataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "DR", excludes = {"surveyId", "surveyResponseId", "surveyId"}),
            @AliasAll(entityAlias = "VDR", excludes = {"videoData"}),
            @AliasAll(entityAlias = "IDR", excludes = {"imageData"}),
            @AliasAll(entityAlias = "ADR", excludes = {"audioData"}),
            @AliasAll(entityAlias = "DDR", excludes = {"documentData"}),
            @AliasAll(entityAlias = "ODR", excludes = {"dataResourceContent"}),
            @AliasAll(entityAlias = "CNT", prefix = "co", excludes = {"contentId", "contentName", "contentTypeId"})
        },
        aliases = {
            @Alias(name = "contentId", entityAlias = "CNT"),
            @Alias(name = "contentName", entityAlias = "CNT"),
            @Alias(name = "contentTypeId", entityAlias = "CNT"),
            @Alias(name = "contentPath", entityAlias = "CNT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CNT",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "VDR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "IDR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "ADR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "DDR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "ODR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Content",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface DataResourceMediaFileViewView {}

}
