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
package com.ilscipio.scipio.content.entity;

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
     * Content
     */
    @Entity(
        name = "Content",
        packageName = "org.ofbiz.content.content",
        title = "Content",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "contentTypeId", type = "id"),
            @Field(name = "ownerContentId", type = "id", description = "Used for permissions checking"),
            @Field(name = "decoratorContentId", type = "id"),
            @Field(name = "instanceOfContentId", type = "id"),
            @Field(name = "dataResourceId", type = "id"),
            @Field(name = "templateDataResourceId", type = "id"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "privilegeEnumId", type = "id"),
            @Field(name = "serviceName", type = "long-varchar", description = "Deprecated : use customMethod pattern instead. Kept for backward compatibility"),
            @Field(name = "customMethodId", type = "id"),
            @Field(name = "contentName", type = "value"),
            @Field(name = "description", type = "description"),
            @Field(name = "localeString", type = "very-short"),
            @Field(name = "mimeTypeId", type = "id-vlong"),
            @Field(name = "characterSetId", type = "id-long"),
            @Field(name = "childLeafCount", type = "numeric"),
            @Field(name = "childBranchCount", type = "numeric"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "mediaProfile", type = "id", description = "Name of a media profile for the image or media. For images: ImageSizePreset.presetId or media profile name from mediaprofiles.properties (SCIPIO)"),
            @Field(name = "contentPath", type = "url", description = "Optional media path (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentType",
                fkName = "CONTENT_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "CONTENT_TO_DATA",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                title = "Template",
                fkName = "CONTENT_TO_TMPDATA",
                keyMaps = {
                    @KeyMap(fieldName = "templateDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CONTENT_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Privilege",
                fkName = "CONTENT_PRIVENM",
                keyMaps = {
                    @KeyMap(fieldName = "privilegeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                fkName = "CONTENT_CUSTMET",
                keyMaps = {
                    @KeyMap(fieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "MimeType",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CharacterSet",
                fkName = "CONTENT_CHST",
                keyMaps = {
                    @KeyMap(fieldName = "characterSetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "CONTENT_CB_ULGN",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "CONTENT_LMB_ULGN",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductFeatureDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "CONTENT_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "Decorator",
                fkName = "CONTENT_DCNTNT",
                keyMaps = {
                    @KeyMap(fieldName = "decoratorContentId", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "Owner",
                fkName = "CONTENT_PCNTNT",
                keyMaps = {
                    @KeyMap(fieldName = "ownerContentId", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "InstanceOf",
                fkName = "CONTENT_IOFCNT",
                keyMaps = {
                    @KeyMap(fieldName = "instanceOfContentId", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            )
        },
        indexes = {
            @Index(
                name = "CONTENT_PATH_SCP",
                fields = {
                    @IndexField(name = "contentPath")
                }
            )
        }
    )
    public interface ContentEntity {}

    /**
     * Content Approval
     */
    @Entity(
        name = "ContentApproval",
        packageName = "org.ofbiz.content.content",
        title = "Content Approval",
        fields = {
            @Field(name = "contentApprovalId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "contentRevisionSeqId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "approvalStatusId", type = "id-ne"),
            @Field(name = "approvalDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentApprovalId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CNTNTAPPR_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContentRevision",
                keyMaps = {
                    @KeyMap(fieldName = "contentId"),
                    @KeyMap(fieldName = "contentRevisionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CNTNTAPPR_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "CNTNTAPPR_RLTP",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Approval",
                fkName = "CNTNTAPPR_APSI",
                keyMaps = {
                    @KeyMap(fieldName = "approvalStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface ContentApprovalEntity {}

    /**
     * Content Association
     */
    @Entity(
        name = "ContentAssoc",
        packageName = "org.ofbiz.content.content",
        title = "Content Association",
        fields = {
            @Field(name = "contentId", type = "id-ne", description = "\"parent\" content"),
            @Field(name = "contentIdTo", type = "id-ne", description = "\"child\" or \"sub\" content"),
            @Field(name = "contentAssocTypeId", type = "id"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "contentAssocPredicateId", type = "id"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "mapKey", type = "name"),
            @Field(name = "upperCoordinate", type = "numeric"),
            @Field(name = "leftCoordinate", type = "numeric"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "contentIdTo"),
            @PrimaryKey(field = "contentAssocTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "From",
                fkName = "CONTENTASSC_FROM",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                title = "To",
                fkName = "CONTENTASSC_TO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentAssocType",
                fkName = "CONTENTASSC_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "contentAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "CONTENTASSC_CBUSR",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "CONTENTASSC_LMBUR",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentAssocPredicate",
                fkName = "CONTENTASSC_PRED",
                keyMaps = {
                    @KeyMap(fieldName = "contentAssocPredicateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "CONTENTASSC_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CONTENTASSC_TOQRY",
                fields = {
                    @IndexField(name = "contentIdTo"),
                    @IndexField(name = "contentAssocTypeId"),
                    @IndexField(name = "thruDate")
                }
            )
        }
    )
    public interface ContentAssocEntity {}

    /**
     * Content Association Predicate
     */
    @Entity(
        name = "ContentAssocPredicate",
        packageName = "org.ofbiz.content.content",
        title = "Content Association Predicate",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentAssocPredicateId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentAssocPredicateId")
        }
    )
    public interface ContentAssocPredicateEntity {}

    /**
     * Content Association Type
     */
    @Entity(
        name = "ContentAssocType",
        packageName = "org.ofbiz.content.content",
        title = "Content Association Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentAssocTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentAssocTypeId")
        }
    )
    public interface ContentAssocTypeEntity {}

    /**
     * Content Attribute
     */
    @Entity(
        name = "ContentAttribute",
        packageName = "org.ofbiz.content.content",
        title = "Content Attribute",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CONTENT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface ContentAttributeEntity {}

    /**
     * Content Meta-Data Predicate
     */
    @Entity(
        name = "ContentMetaData",
        packageName = "org.ofbiz.content.content",
        title = "Content Meta-Data Predicate",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "metaDataPredicateId", type = "id-ne"),
            @Field(name = "metaDataValue", type = "value"),
            @Field(name = "dataSourceId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "metaDataPredicateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CONTENTMD_CNTNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MetaDataPredicate",
                fkName = "CONTENTMD_DMDPRD",
                keyMaps = {
                    @KeyMap(fieldName = "metaDataPredicateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "CONTENTMD_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            )
        }
    )
    public interface ContentMetaDataEntity {}

    /**
     * Content Operation
     */
    @Entity(
        name = "ContentOperation",
        packageName = "org.ofbiz.content.content",
        title = "Content Operation",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentOperationId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentOperationId")
        }
    )
    public interface ContentOperationEntity {}

    /**
     * Content Purpose
     */
    @Entity(
        name = "ContentPurpose",
        packageName = "org.ofbiz.content.content",
        title = "Content Purpose",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "contentPurposeTypeId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "contentPurposeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CONTENT_PRP",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentPurposeType",
                fkName = "CONTENT_PRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contentPurposeTypeId")
                }
            )
        }
    )
    public interface ContentPurposeEntity {}

    /**
     * Content Purpose
     */
    @Entity(
        name = "ContentPurposeOperation",
        packageName = "org.ofbiz.content.content",
        title = "Content Purpose",
        fields = {
            @Field(name = "contentPurposeTypeId", type = "id-ne"),
            @Field(name = "contentOperationId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "privilegeEnumId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentPurposeTypeId"),
            @PrimaryKey(field = "contentOperationId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "privilegeEnumId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentPurposeType",
                fkName = "CONTENT_PRO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contentPurposeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentOperation",
                fkName = "CONTENT_PRO_OPER",
                keyMaps = {
                    @KeyMap(fieldName = "contentOperationId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "CONTENT_PRO_RLT",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CONTENT_PRO_STI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "CONTENT_PRO_PEI",
                keyMaps = {
                    @KeyMap(fieldName = "privilegeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ContentPurposeOperationEntity {}

    /**
     * Content Purpose Type
     */
    @Entity(
        name = "ContentPurposeType",
        packageName = "org.ofbiz.content.content",
        title = "Content Purpose Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentPurposeTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentPurposeTypeId")
        }
    )
    public interface ContentPurposeTypeEntity {}

    /**
     * Content Revision
     */
    @Entity(
        name = "ContentRevision",
        packageName = "org.ofbiz.content.content",
        title = "Content Revision",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "contentRevisionSeqId", type = "id-ne"),
            @Field(name = "committedByPartyId", type = "id-ne"),
            @Field(name = "comments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "contentRevisionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CNTNTREV_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "CommittedBy",
                fkName = "CNTNTREV_CBPTY",
                keyMaps = {
                    @KeyMap(fieldName = "committedByPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface ContentRevisionEntity {}

    /**
     * Content Revision
     */
    @Entity(
        name = "ContentRevisionItem",
        packageName = "org.ofbiz.content.content",
        title = "Content Revision",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "contentRevisionSeqId", type = "id-ne"),
            @Field(name = "itemContentId", type = "id-ne"),
            @Field(name = "oldDataResourceId", type = "id-ne"),
            @Field(name = "newDataResourceId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "contentRevisionSeqId"),
            @PrimaryKey(field = "itemContentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentRevision",
                fkName = "CNTNTREVIT_CNTREV",
                keyMaps = {
                    @KeyMap(fieldName = "contentId"),
                    @KeyMap(fieldName = "contentRevisionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                title = "Old",
                fkName = "CNTNTREVIT_OLDDR",
                keyMaps = {
                    @KeyMap(fieldName = "oldDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                title = "New",
                fkName = "CNTNTREVIT_NEWDR",
                keyMaps = {
                    @KeyMap(fieldName = "newDataResourceId", relFieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ContentRevisionItemEntity {}

    /**
     * Content Role
     */
    @Entity(
        name = "ContentRole",
        packageName = "org.ofbiz.content.content",
        title = "Content Role",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CNTNT_RL_CNTNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "CNTNT_RL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface ContentRoleEntity {}

    /**
     * Content Type
     */
    @Entity(
        name = "ContentType",
        packageName = "org.ofbiz.content.content",
        title = "Content Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "contentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentType",
                title = "Parent",
                fkName = "CNTNT_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "contentTypeId")
                }
            )
        }
    )
    public interface ContentTypeEntity {}

    /**
     * Content Type Attribute
     */
    @Entity(
        name = "ContentTypeAttr",
        packageName = "org.ofbiz.content.content",
        title = "Content Type Attribute",
        fields = {
            @Field(name = "contentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentType",
                fkName = "CONTENT_TPAT_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Content",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            )
        }
    )
    public interface ContentTypeAttrEntity {}

    /**
     * Audio Data Object
     */
    @Entity(
        name = "AudioDataResource",
        packageName = "org.ofbiz.content.data",
        title = "Audio Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "audioData", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_AUDIO",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface AudioDataResourceEntity {}

    /**
     * Character Set
     */
    @Entity(
        name = "CharacterSet",
        packageName = "org.ofbiz.content.data",
        title = "Character Set",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "characterSetId", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "characterSetId")
        }
    )
    public interface CharacterSetEntity {}

    /**
     * Data Category
     */
    @Entity(
        name = "DataCategory",
        packageName = "org.ofbiz.content.data",
        title = "Data Category",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "dataCategoryId", type = "id-ne"),
            @Field(name = "parentCategoryId", type = "id"),
            @Field(name = "categoryName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataCategory",
                title = "Parent",
                fkName = "DATA_CAT_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentCategoryId", relFieldName = "dataCategoryId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataCategory",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentCategoryId")
                }
            )
        }
    )
    public interface DataCategoryEntity {}

    /**
     * Data Object
     */
    @Entity(
        name = "DataResource",
        packageName = "org.ofbiz.content.data",
        title = "Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "dataResourceTypeId", type = "id"),
            @Field(name = "dataTemplateTypeId", type = "id"),
            @Field(name = "dataCategoryId", type = "id-ne"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "dataResourceName", type = "value"),
            @Field(name = "localeString", type = "very-short"),
            @Field(name = "mimeTypeId", type = "id-vlong"),
            @Field(name = "characterSetId", type = "id-long"),
            @Field(name = "objectInfo", type = "long-varchar", description = "For Short Text the text goes here."),
            @Field(name = "surveyId", type = "id"),
            @Field(name = "surveyResponseId", type = "id"),
            @Field(name = "relatedDetailId", type = "id", description = "Depending on the dataResourceTypeId this can point to other entities, like: Survey, SurveyResponse, etc."),
            @Field(name = "isPublic", type = "indicator", description = "If this is set to Y then anyone can download it, otherwise the download is restricted."),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "scpWidth", type = "numeric", description = "Image or video width (px) (optimization; originally added for auto-resized images, contentAssocTypeId IMGSZ_*; may be null for other purposes) (SCIPIO)"),
            @Field(name = "scpHeight", type = "numeric", description = "Image or video width (px) (optimization; originally added for auto-resized images, contentAssocTypeId IMGSZ_*; may be null for other purposes) (SCIPIO)"),
            @Field(name = "sizeId", type = "id-ne", description = "Represents an image dimension with its common fields (height and width) and other fields used for responsive images. NOTE: configuration may not necessarily reflect current image (SCIPIO)"),
            @Field(name = "srcPresetJson", type = "very-long", description = "Json map describing the preset/format data used to generate this media, regardless of whether the configuration presets were changed; for resized variant images, usually contains 'width', 'height', 'format', 'upscaleMode' (corresponds to ImageVariantConfig.VariantInfo without name) (SCIPIO)"),
            @Field(name = "srcMimeTypeId", type = "id-vlong", description = "mimeTypeId for the source image, for product image URLs, as mimeTypeId is set to text/html for these by stock (SCIPIO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "DTRSRC_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResourceType",
                fkName = "DATA_REC_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataTemplateType",
                fkName = "DATA_REC_TO_TTP",
                keyMaps = {
                    @KeyMap(fieldName = "dataTemplateTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataCategory",
                fkName = "DATA_REC_TO_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "dataCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "DATA_REC_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "MimeType",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CharacterSet",
                fkName = "DATA_REC_CHST",
                keyMaps = {
                    @KeyMap(fieldName = "characterSetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "DATA_REC_CB_ULGN",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "DATA_REC_LMB_ULGN",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "DATA_REC_SURVEY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyResponse",
                fkName = "DATA_REC_SVRSP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ImageSizeDimension",
                fkName = "DATA_REC_DIM",
                keyMaps = {
                    @KeyMap(fieldName = "sizeId")
                }
            )
        }
    )
    public interface DataResourceEntity {}

    /**
     * Data Object Attribute
     */
    @Entity(
        name = "DataResourceAttribute",
        packageName = "org.ofbiz.content.data",
        title = "Data Object Attribute",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface DataResourceAttributeEntity {}

    /**
     * Data Resource Meta-Data Predicate
     */
    @Entity(
        name = "DataResourceMetaData",
        packageName = "org.ofbiz.content.data",
        title = "Data Resource Meta-Data Predicate",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "metaDataPredicateId", type = "id-ne"),
            @Field(name = "metaDataValue", type = "value"),
            @Field(name = "dataSourceId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId"),
            @PrimaryKey(field = "metaDataPredicateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_MD_DATREC",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MetaDataPredicate",
                fkName = "DATA_MD_DMDPRD",
                keyMaps = {
                    @KeyMap(fieldName = "metaDataPredicateId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "DATA_MD_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            )
        }
    )
    public interface DataResourceMetaDataEntity {}

    /**
     * Data Object Purpose
     */
    @Entity(
        name = "DataResourcePurpose",
        packageName = "org.ofbiz.content.data",
        title = "Data Object Purpose",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "contentPurposeTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId"),
            @PrimaryKey(field = "contentPurposeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_PRP",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentPurposeType",
                fkName = "DATA_REC_PRP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "contentPurposeTypeId")
                }
            )
        }
    )
    public interface DataResourcePurposeEntity {}

    /**
     * DataResource Role
     */
    @Entity(
        name = "DataResourceRole",
        packageName = "org.ofbiz.content.data",
        title = "DataResource Role",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATARECRL_DATREC",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "DATARECRL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface DataResourceRoleEntity {}

    /**
     * Data Object Type
     */
    @Entity(
        name = "DataResourceType",
        packageName = "org.ofbiz.content.data",
        title = "Data Object Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "dataResourceTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResourceType",
                title = "Parent",
                fkName = "DATA_OBTYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "dataResourceTypeId")
                }
            )
        }
    )
    public interface DataResourceTypeEntity {}

    /**
     * Data Object Type Attribute
     */
    @Entity(
        name = "DataResourceTypeAttr",
        packageName = "org.ofbiz.content.data",
        title = "Data Object Type Attribute",
        fields = {
            @Field(name = "dataResourceTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResourceType",
                fkName = "DATA_OBTYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            )
        }
    )
    public interface DataResourceTypeAttrEntity {}

    /**
     * Data Template Type
     */
    @Entity(
        name = "DataTemplateType",
        packageName = "org.ofbiz.content.data",
        title = "Data Template Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "dataTemplateTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "extension", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataTemplateTypeId")
        }
    )
    public interface DataTemplateTypeEntity {}

    /**
     * Electronic Text
     */
    @Entity(
        name = "ElectronicText",
        packageName = "org.ofbiz.content.data",
        title = "Electronic Text",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "textData", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_TEXT",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ElectronicTextEntity {}

    /**
     * File Extension
     */
    @Entity(
        name = "FileExtension",
        packageName = "org.ofbiz.content.data",
        title = "File Extension",
        fields = {
            @Field(name = "fileExtensionId", type = "id-long-ne"),
            @Field(name = "mimeTypeId", type = "id-vlong-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "fileExtensionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MimeType",
                fkName = "FILEEXT_MMTYP",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            )
        }
    )
    public interface FileExtensionEntity {}

    /**
     * Image Data Object
     */
    @Entity(
        name = "ImageDataResource",
        packageName = "org.ofbiz.content.data",
        title = "Image Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "imageData", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_IMAGE",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ImageDataResourceEntity {}

    /**
     * Data Meta-Data Predicate
     */
    @Entity(
        name = "MetaDataPredicate",
        packageName = "org.ofbiz.content.data",
        title = "Data Meta-Data Predicate",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "metaDataPredicateId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "metaDataPredicateId")
        }
    )
    public interface MetaDataPredicateEntity {}

    /**
     * Mime Type
     */
    @Entity(
        name = "MimeType",
        packageName = "org.ofbiz.content.data",
        title = "Mime Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "mimeTypeId", type = "id-vlong-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "mimeTypeId")
        }
    )
    public interface MimeTypeEntity {}

    /**
     * Mime Text Template
     */
    @Entity(
        name = "MimeTypeHtmlTemplate",
        packageName = "org.ofbiz.content.data",
        title = "Mime Text Template",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "mimeTypeId", type = "id-vlong-ne"),
            @Field(name = "templateLocation", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "mimeTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MimeType",
                fkName = "MIMETYPE_TPL_MT",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            )
        }
    )
    public interface MimeTypeHtmlTemplateEntity {}

    /**
     * Other Data Object
     */
    @Entity(
        name = "OtherDataResource",
        packageName = "org.ofbiz.content.data",
        title = "Other Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "dataResourceContent", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_OTHER",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface OtherDataResourceEntity {}

    /**
     * Video Data Object
     */
    @Entity(
        name = "VideoDataResource",
        packageName = "org.ofbiz.content.data",
        title = "Video Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "videoData", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_VIDEO",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface VideoDataResourceEntity {}

    /**
     * Document Data Object
     */
    @Entity(
        name = "DocumentDataResource",
        packageName = "org.ofbiz.content.data",
        title = "Document Data Object",
        fields = {
            @Field(name = "dataResourceId", type = "id-ne"),
            @Field(name = "documentData", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataResourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResource",
                fkName = "DATA_REC_DOCUMENT",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface DocumentDataResourceEntity {}

    /**
     * Document
     */
    @Entity(
        name = "Document",
        packageName = "org.ofbiz.content.document",
        title = "Document",
        fields = {
            @Field(name = "documentId", type = "id-ne"),
            @Field(name = "documentTypeId", type = "id"),
            @Field(name = "dateCreated", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "documentLocation", type = "long-varchar"),
            @Field(name = "documentText", type = "long-varchar"),
            @Field(name = "imageData", type = "object")
        },
        primaryKeys = {
            @PrimaryKey(field = "documentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DocumentType",
                fkName = "DOCUMENT_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "documentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DocumentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "documentTypeId")
                }
            )
        }
    )
    public interface DocumentEntity {}

    /**
     * Document Attribute
     */
    @Entity(
        name = "DocumentAttribute",
        packageName = "org.ofbiz.content.document",
        title = "Document Attribute",
        fields = {
            @Field(name = "documentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "documentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Document",
                fkName = "DOCUMENT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "documentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DocumentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface DocumentAttributeEntity {}

    /**
     * Document Type
     */
    @Entity(
        name = "DocumentType",
        packageName = "org.ofbiz.content.document",
        title = "Document Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "documentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "documentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DocumentType",
                title = "Parent",
                fkName = "DOC_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "documentTypeId")
                }
            )
        }
    )
    public interface DocumentTypeEntity {}

    /**
     * Document Type Attribute
     */
    @Entity(
        name = "DocumentTypeAttr",
        packageName = "org.ofbiz.content.document",
        title = "Document Type Attribute",
        fields = {
            @Field(name = "documentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "documentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DocumentType",
                fkName = "DOC_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "documentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DocumentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Document",
                keyMaps = {
                    @KeyMap(fieldName = "documentTypeId")
                }
            )
        }
    )
    public interface DocumentTypeAttrEntity {}

    /**
     * Image sizes preset
     */
    @Entity(
        name = "ImageSizePreset",
        packageName = "com.ilscipio.scipio.cms.media",
        title = "Image sizes preset",
        fields = {
            @Field(name = "presetId", type = "id-ne", description = "The preset ID, now stored as Content.mediaProfile for associated images (only since 2020-09-21)"),
            @Field(name = "presetName", type = "name"),
            @Field(name = "parentProfile", type = "id", description = "Parent mediaProfile or ImageSizePreset.presetId - used for type checking and specialization (default: IMAGE_MEDIA)"),
            @Field(name = "variantConfigProfile", type = "id", description = "Name of another profile to use for base ImageVariantConfig (even if no ImageSize definitions) (TODO)"),
            @Field(name = "variantConfigLocation", type = "id", description = "Location of an ImageProperties.xml file (TODO)")
        },
        primaryKeys = {
            @PrimaryKey(field = "presetId")
        }
    )
    public interface ImageSizePresetEntity {}

    /**
     * Image size dimension preset
     */
    @Entity(
        name = "ImageSize",
        packageName = "com.ilscipio.scipio.cms.media",
        title = "Image size dimension preset",
        fields = {
            @Field(name = "presetId", type = "id-ne"),
            @Field(name = "sizeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "presetId"),
            @PrimaryKey(field = "sizeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ImageSizePreset",
                fkName = "IS_PRESET",
                keyMaps = {
                    @KeyMap(fieldName = "presetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ImageSizeDimension",
                fkName = "IS_DIMENSION",
                keyMaps = {
                    @KeyMap(fieldName = "sizeId")
                }
            )
        }
    )
    public interface ImageSizeEntity {}

    /**
     * Image dimensions
     */
    @Entity(
        name = "ImageSizeDimension",
        packageName = "com.ilscipio.scipio.cms.media",
        title = "Image dimensions",
        fields = {
            @Field(name = "sizeId", type = "id-ne"),
            @Field(name = "sizeName", type = "name"),
            @Field(name = "dimensionWidth", type = "numeric"),
            @Field(name = "dimensionHeight", type = "numeric"),
            @Field(name = "sequenceNum", type = "numeric", notNull = true),
            @Field(name = "format", type = "name", description = "File format extension, e.g: jpg, webp; see FileExtension entity; default: same as original"),
            @Field(name = "upscaleMode", type = "id", description = "(on|off|omit, default: on) What to do when both dimensions are larger than original")
        },
        primaryKeys = {
            @PrimaryKey(field = "sizeId")
        }
    )
    public interface ImageSizeDimensionEntity {}

    /**
     * Responsive Image
     */
    @Entity(
        name = "ResponsiveImage",
        packageName = "com.ilscipio.scipio.image",
        title = "Responsive Image",
        fields = {
            @Field(name = "contentId", type = "id-ne", description = "Meant to be used with parent content"),
            @Field(name = "srcsetModeEnumId", type = "id", description = "\n            (12/31/2018): As of now the spec allows device-pixel-ratio and viewport based selection. \n            See https://w3c.github.io/html/semantics-embedded-content.html#embedded-content-introduction\n        ")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Content",
                fkName = "CONTENT_VP",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "ImgSrcset",
                fkName = "ISD_SRCSET",
                keyMaps = {
                    @KeyMap(fieldName = "srcsetModeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface ResponsiveImageEntity {}

    /**
     * Responsive Image view port size
     * This is only valid for viewport based selection
     */
    @Entity(
        name = "ResponsiveImageVP",
        packageName = "com.ilscipio.scipio.image",
        title = "Responsive Image view port size",
        description = "This is only valid for viewport based selection",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "viewPortMediaQuery", type = "long-varchar"),
            @Field(name = "viewPortLength", type = "numeric"),
            @Field(name = "sequenceNum", type = "numeric", notNull = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "sequenceNum")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ResponsiveImage",
                fkName = "RESIMG_C",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ResponsiveImageVPEntity {}

    /**
     * Web Preference Type
     */
    @Entity(
        name = "WebPreferenceType",
        packageName = "org.ofbiz.content.preference",
        title = "Web Preference Type",
        fields = {
            @Field(name = "webPreferenceTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "webPreferenceTypeId")
        }
    )
    public interface WebPreferenceTypeEntity {}

    /**
     * Web User Preference
     */
    @Entity(
        name = "WebUserPreference",
        packageName = "org.ofbiz.content.preference",
        title = "Web User Preference",
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "visitId", type = "id-ne", description = "To be able to keep preferences for a non loggin in user for the current session"),
            @Field(name = "webPreferenceTypeId", type = "id-ne"),
            @Field(name = "webPreferenceValue", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "visitId"),
            @PrimaryKey(field = "webPreferenceTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebPreferenceType",
                fkName = "WEB_PREF_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "webPreferenceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "WEB_PREF_USER",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "WEB_PREF_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface WebUserPreferenceEntity {}

    /**
     * Survey
     */
    @Entity(
        name = "Survey",
        packageName = "org.ofbiz.content.survey",
        title = "Survey",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "submitCaption", type = "short-varchar"),
            @Field(name = "responseService", type = "long-varchar"),
            @Field(name = "isAnonymous", type = "indicator", description = "Allow response to the survey without login?"),
            @Field(name = "allowMultiple", type = "indicator", description = "Allow multiple responses to this survey (if Y), or just a single answer (if N)?"),
            @Field(name = "allowUpdate", type = "indicator", description = "Allow change to responses?"),
            @Field(name = "acroFormContentId", type = "id-ne", description = "Points to PDF with AcroForm"),
            @Field(name = "showOnInvoice", type = "indicator", description = "SCIPIO: Show brief survey results on invoices and/or in shopping cart (if Y; default N). Added 2019-03-14.")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId")
        }
    )
    public interface SurveyEntity {}

    /**
     * Survey Application Type
     */
    @Entity(
        name = "SurveyApplType",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Application Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "surveyApplTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyApplTypeId")
        }
    )
    public interface SurveyApplTypeEntity {}

    /**
     * Survey Multi-Response Group
     */
    @Entity(
        name = "SurveyMultiResp",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Multi-Response Group",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyMultiRespId", type = "id-ne"),
            @Field(name = "multiRespTitle", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "surveyMultiRespId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "SRVYMRSP_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            )
        }
    )
    public interface SurveyMultiRespEntity {}

    /**
     * Survey Multi-Response Group Column/Category
     */
    @Entity(
        name = "SurveyMultiRespColumn",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Multi-Response Group Column/Category",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyMultiRespId", type = "id-ne"),
            @Field(name = "surveyMultiRespColId", type = "id-ne"),
            @Field(name = "columnTitle", type = "name"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "surveyMultiRespId"),
            @PrimaryKey(field = "surveyMultiRespColId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyMultiResp",
                fkName = "SRVYMRSPCL_SMRESP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId")
                }
            )
        }
    )
    public interface SurveyMultiRespColumnEntity {}

    /**
     * Survey Page Type
     */
    @Entity(
        name = "SurveyPage",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Page Type",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyPageSeqId", type = "id-ne"),
            @Field(name = "pageName", type = "name"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "surveyPageSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "SRVYPAGE_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            )
        }
    )
    public interface SurveyPageEntity {}

    /**
     * Survey Question
     */
    @Entity(
        name = "SurveyQuestion",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question",
        fields = {
            @Field(name = "surveyQuestionId", type = "id-ne"),
            @Field(name = "surveyQuestionCategoryId", type = "id-ne"),
            @Field(name = "surveyQuestionTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "question", type = "very-long"),
            @Field(name = "hint", type = "very-long"),
            @Field(name = "enumTypeId", type = "id"),
            @Field(name = "geoId", type = "id"),
            @Field(name = "formatString", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyQuestionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestionType",
                fkName = "SRVYQST_SRVYQTP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestionCategory",
                fkName = "SRVYQST_SRVYQTCT",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "SRVYQST_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Enumeration",
                fkName = "SRVYQST_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "enumTypeId")
                }
            )
        }
    )
    public interface SurveyQuestionEntity {}

    /**
     * Survey Question Application
     */
    @Entity(
        name = "SurveyQuestionAppl",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question Application",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyQuestionId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "surveyPageSeqId", type = "id-ne"),
            @Field(name = "surveyMultiRespId", type = "id-ne"),
            @Field(name = "surveyMultiRespColId", type = "id", description = "Used to optionally associate this question to a specific column in the Multi-Response set; with this you can associate a single question to each cell in the question/column grid; this is useful for AcroForm round trips where the target PDF needs a question associated with each cell, or even the same question applied with different externalFieldRef values."),
            @Field(name = "requiredField", type = "indicator"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "externalFieldRef", type = "long-varchar", description = "External field ID/reference; for AcroForms used to track the field ID"),
            @Field(name = "withSurveyQuestionId", type = "id", description = "These two with* fields are used to specify that this question should only appear if the with option has been selected for the with question."),
            @Field(name = "withSurveyOptionSeqId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "surveyQuestionId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "SRVYQSTAPL_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestion",
                fkName = "SRVYQSTAPL_SRVYQ",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestionOption",
                title = "With",
                fkName = "SRVYQSTAPL_SVQO",
                keyMaps = {
                    @KeyMap(fieldName = "withSurveyQuestionId", relFieldName = "surveyQuestionId"),
                    @KeyMap(fieldName = "withSurveyOptionSeqId", relFieldName = "surveyOptionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyPage",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyPageSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyMultiResp",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyMultiRespColumn",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId"),
                    @KeyMap(fieldName = "surveyMultiRespColId")
                }
            )
        }
    )
    public interface SurveyQuestionApplEntity {}

    /**
     * Survey Question Category
     */
    @Entity(
        name = "SurveyQuestionCategory",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question Category",
        fields = {
            @Field(name = "surveyQuestionCategoryId", type = "id-ne"),
            @Field(name = "parentCategoryId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyQuestionCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestionCategory",
                title = "Parent",
                fkName = "SRVYQSTCT_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentCategoryId", relFieldName = "surveyQuestionCategoryId")
                }
            )
        }
    )
    public interface SurveyQuestionCategoryEntity {}

    /**
     * Survey Question Option
     */
    @Entity(
        name = "SurveyQuestionOption",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question Option",
        fields = {
            @Field(name = "surveyQuestionId", type = "id-ne"),
            @Field(name = "surveyOptionSeqId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "amountBase", type = "currency-amount"),
            @Field(name = "amountBaseUomId", type = "id"),
            @Field(name = "weightFactor", type = "floating-point"),
            @Field(name = "duration", type = "numeric"),
            @Field(name = "durationUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyQuestionId"),
            @PrimaryKey(field = "surveyOptionSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestion",
                fkName = "SRVYQSTOP_SRVYQ",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            )
        }
    )
    public interface SurveyQuestionOptionEntity {}

    /**
     * Survey Question Type
     */
    @Entity(
        name = "SurveyQuestionType",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "surveyQuestionTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyQuestionTypeId")
        }
    )
    public interface SurveyQuestionTypeEntity {}

    /**
     * Survey Response
     */
    @Entity(
        name = "SurveyResponse",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Response",
        fields = {
            @Field(name = "surveyResponseId", type = "id-ne"),
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "responseDate", type = "date-time"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "referenceId", type = "id-vlong"),
            @Field(name = "generalFeedback", type = "very-long"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "statusId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyResponseId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "SRVYRSP_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "SRVYRSP_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface SurveyResponseEntity {}

    /**
     * Survey Response Answer
     */
    @Entity(
        name = "SurveyResponseAnswer",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Response Answer",
        fields = {
            @Field(name = "surveyResponseId", type = "id-ne"),
            @Field(name = "surveyQuestionId", type = "id-ne"),
            @Field(name = "surveyMultiRespColId", type = "id-ne", description = "This is needed to support multiple responses for different MultiResp Columns; if not part of a MultiResp will be _NA_"),
            @Field(name = "surveyMultiRespId", type = "id-ne", description = "This is not part of the primary key, but should be populated so that the SurveyMultiRespColumn can be more easily looked up."),
            @Field(name = "booleanResponse", type = "indicator"),
            @Field(name = "currencyResponse", type = "currency-amount"),
            @Field(name = "floatResponse", type = "floating-point"),
            @Field(name = "numericResponse", type = "numeric"),
            @Field(name = "textResponse", type = "very-long"),
            @Field(name = "surveyOptionSeqId", type = "id"),
            @Field(name = "contentId", type = "id"),
            @Field(name = "answeredDate", type = "date-time"),
            @Field(name = "amountBase", type = "currency-amount"),
            @Field(name = "amountBaseUomId", type = "id"),
            @Field(name = "weightFactor", type = "floating-point"),
            @Field(name = "duration", type = "numeric"),
            @Field(name = "durationUomId", type = "id"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyResponseId"),
            @PrimaryKey(field = "surveyQuestionId"),
            @PrimaryKey(field = "surveyMultiRespColId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyResponse",
                fkName = "SRVYRSPA_SVRSP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestion",
                fkName = "SRVYRSPA_SVQU",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyQuestionOption",
                fkName = "SRVYRSPA_OPT",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId"),
                    @KeyMap(fieldName = "surveyOptionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "SRVYRSPA_CONT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface SurveyResponseAnswerEntity {}

    /**
     * Survey Trigger
     */
    @Entity(
        name = "SurveyTrigger",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Trigger",
        fields = {
            @Field(name = "surveyId", type = "id-ne"),
            @Field(name = "surveyApplTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "surveyId"),
            @PrimaryKey(field = "surveyApplTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Survey",
                fkName = "SRVYTRG_SRVY",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyApplType",
                fkName = "SRVYTRG_SRVYAPT",
                keyMaps = {
                    @KeyMap(fieldName = "surveyApplTypeId")
                }
            )
        }
    )
    public interface SurveyTriggerEntity {}

    /**
     * Web Site Content Associations
     */
    @Entity(
        name = "WebSiteContent",
        packageName = "org.ofbiz.content.website",
        title = "Web Site Content Associations",
        fields = {
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "webSiteContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "webSiteContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "WSCTNT_WEBSITE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "WSCTNT_CONTENT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSiteContentType",
                fkName = "WSCTNT_WSCTTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteContentTypeId")
                }
            )
        }
    )
    public interface WebSiteContentEntity {}

    /**
     * Web Site Content Type
     */
    @Entity(
        name = "WebSiteContentType",
        packageName = "org.ofbiz.content.website",
        title = "Web Site Content Type",
        defaultResourceName = "ContentEntityLabels",
        fields = {
            @Field(name = "webSiteContentTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSiteContentType",
                title = "Parent",
                fkName = "WSCT_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "webSiteContentTypeId")
                }
            )
        }
    )
    public interface WebSiteContentTypeEntity {}

    /**
     * Web Site Path Alias
     */
    @Entity(
        name = "WebSitePathAlias",
        packageName = "org.ofbiz.content.website",
        title = "Web Site Path Alias",
        fields = {
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "pathAlias", type = "id-vlong"),
            @Field(name = "aliasTo", type = "long-varchar"),
            @Field(name = "contentId", type = "id"),
            @Field(name = "mapKey", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "pathAlias")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "WSPATH_WEBSITE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "WSPATH_CONTENT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface WebSitePathAliasEntity {}

    /**
     * Web Site Publish Point
     */
    @Entity(
        name = "WebSitePublishPoint",
        packageName = "org.ofbiz.content.website",
        title = "Web Site Publish Point",
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "templateTitle", type = "long-varchar"),
            @Field(name = "styleSheetFile", type = "long-varchar"),
            @Field(name = "logo", type = "long-varchar"),
            @Field(name = "medallionLogo", type = "long-varchar"),
            @Field(name = "lineLogo", type = "long-varchar"),
            @Field(name = "leftBarId", type = "id"),
            @Field(name = "rightBarId", type = "id"),
            @Field(name = "contentDept", type = "id"),
            @Field(name = "aboutContentId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "WBSTPP_CONTENT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface WebSitePublishPointEntity {}

    /**
     * WebSite Role Association
     */
    @Entity(
        name = "WebSiteRole",
        packageName = "org.ofbiz.party.party",
        title = "WebSite Role Association",
        fields = {
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyGroup",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "WSRLE_PTYRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "WSRLE_WSITE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            )
        }
    )
    public interface WebSiteRoleEntity {}

    /**
     * Content Keyword
     */
    @Entity(
        name = "ContentKeyword",
        packageName = "org.ofbiz.content.content",
        title = "Content Keyword",
        neverCache = true,
        fields = {
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "keyword", type = "short-varchar"),
            @Field(name = "relevancyWeight", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "keyword")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CNT_KWD_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        },
        indexes = {
            @Index(
                name = "CNT_KWD_KWD",
                fields = {
                    @IndexField(name = "keyword")
                }
            )
        }
    )
    public interface ContentKeywordEntity {}

    /**
     * Content Search Result Constraint
     */
    @Entity(
        name = "ContentSearchConstraint",
        packageName = "org.ofbiz.content.content",
        title = "Content Search Result Constraint",
        neverCache = true,
        fields = {
            @Field(name = "contentSearchResultId", type = "id-ne"),
            @Field(name = "constraintSeqId", type = "id-ne"),
            @Field(name = "constraintName", type = "long-varchar"),
            @Field(name = "infoString", type = "long-varchar"),
            @Field(name = "includeSubCategories", type = "indicator"),
            @Field(name = "isAnd", type = "indicator"),
            @Field(name = "anyPrefix", type = "indicator"),
            @Field(name = "anySuffix", type = "indicator"),
            @Field(name = "removeStems", type = "indicator"),
            @Field(name = "lowValue", type = "short-varchar"),
            @Field(name = "highValue", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentSearchResultId"),
            @PrimaryKey(field = "constraintSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContentSearchResult",
                fkName = "CNT_SCHRSI_RES",
                keyMaps = {
                    @KeyMap(fieldName = "contentSearchResultId")
                }
            )
        }
    )
    public interface ContentSearchConstraintEntity {}

    /**
     * Content Search Result
     */
    @Entity(
        name = "ContentSearchResult",
        packageName = "org.ofbiz.content.content",
        title = "Content Search Result",
        neverCache = true,
        fields = {
            @Field(name = "contentSearchResultId", type = "id-ne"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "orderByName", type = "long-varchar"),
            @Field(name = "isAscending", type = "indicator"),
            @Field(name = "numResults", type = "numeric"),
            @Field(name = "secondsTotal", type = "floating-point"),
            @Field(name = "searchDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentSearchResultId")
        }
    )
    public interface ContentSearchResultEntity {}

    /**
     * Web Analytics Configuration
     */
    @Entity(
        name = "WebAnalyticsConfig",
        packageName = "org.ofbiz.content.website",
        title = "Web Analytics Configuration",
        fields = {
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "webAnalyticsTypeId", type = "id-ne"),
            @Field(name = "webAnalyticsCode", type = "very-long", description = "copy in here the analitics javascript code without the beginning- and end<script> tags")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteId"),
            @PrimaryKey(field = "webAnalyticsTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WebAnalyticsType",
                keyMaps = {
                    @KeyMap(fieldName = "webAnalyticsTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WebSite",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            )
        }
    )
    public interface WebAnalyticsConfigEntity {}

    /**
     * Web Analytics Type
     */
    @Entity(
        name = "WebAnalyticsType",
        packageName = "org.ofbiz.content.website",
        title = "Web Analytics Type",
        fields = {
            @Field(name = "webAnalyticsTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "webAnalyticsTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebAnalyticsType",
                title = "Parent",
                fkName = "WANA_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "webAnalyticsTypeId")
                }
            )
        }
    )
    public interface WebAnalyticsTypeEntity {}

    /**
     * Latest Revision Children
     */
    @ViewEntity(
        name = "AssocRevisionItemView",
        packageName = "org.ofbiz.content.compdoc",
        title = "Latest Revision Children",
        members = {
            @MemberEntity(entityAlias = "CRI", entityName = "ContentRevisionItem"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc")
        },
        aliases = {
            @Alias(name = "contentId", entityAlias = "CA", groupBy = true),
            @Alias(name = "contentIdTo", entityAlias = "CA", groupBy = true),
            @Alias(name = "contentAssocTypeId", entityAlias = "CA", groupBy = true),
            @Alias(name = "thruDate", entityAlias = "CA", groupBy = true),
            @Alias(name = "fromDate", entityAlias = "CA", groupBy = true),
            @Alias(name = "sequenceNum", entityAlias = "CA", groupBy = true),
            @Alias(name = "rootRevisionContentId", entityAlias = "CRI", field = "contentId", groupBy = true),
            @Alias(name = "itemContentId", entityAlias = "CRI", groupBy = true),
            @Alias(name = "contentRevisionSeqId", entityAlias = "CRI"),
            @Alias(name = "maxRevisionSeqId", entityAlias = "CRI", field = "contentRevisionSeqId", function = AggregateFunction.MAX)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CRI",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "itemContentId")
                }
            )
        }
    )
    public interface AssocRevisionItemViewView {}

    /**
     * Latest Revision Children
     */
    @ViewEntity(
        name = "ContentAssocRevisionItemView",
        packageName = "org.ofbiz.content.compdoc",
        title = "Latest Revision Children",
        members = {
            @MemberEntity(entityAlias = "C", entityName = "Content"),
            @MemberEntity(entityAlias = "CRI", entityName = "ContentRevisionItem"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc")
        },
        aliases = {
            @Alias(name = "instanceOfContentId", entityAlias = "C", groupBy = true),
            @Alias(name = "dataResourceId", entityAlias = "C", groupBy = true),
            @Alias(name = "contentId", entityAlias = "CA", groupBy = true),
            @Alias(name = "contentIdTo", entityAlias = "CA", groupBy = true),
            @Alias(name = "contentAssocTypeId", entityAlias = "CA", groupBy = true),
            @Alias(name = "thruDate", entityAlias = "CA", groupBy = true),
            @Alias(name = "fromDate", entityAlias = "CA", groupBy = true),
            @Alias(name = "sequenceNum", entityAlias = "CA", groupBy = true),
            @Alias(name = "rootRevisionContentId", entityAlias = "CRI", field = "contentId", groupBy = true),
            @Alias(name = "itemContentId", entityAlias = "CRI", groupBy = true),
            @Alias(name = "contentRevisionSeqId", entityAlias = "CRI"),
            @Alias(name = "maxRevisionSeqId", entityAlias = "CRI", field = "contentRevisionSeqId", function = AggregateFunction.MAX)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "C",
                relEntityAlias = "CA",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CRI",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "itemContentId")
                }
            )
        }
    )
    public interface ContentAssocRevisionItemViewView {}

    /**
     * Latest Revision Children
     */
    @ViewEntity(
        name = "MaxRevisionItemView",
        packageName = "org.ofbiz.content.compdoc",
        title = "Latest Revision Children",
        members = {
            @MemberEntity(entityAlias = "CRI", entityName = "ContentRevisionItem")
        },
        aliases = {
            @Alias(name = "rootRevisionContentId", entityAlias = "CRI", field = "contentId", groupBy = true),
            @Alias(name = "itemContentId", entityAlias = "CRI", groupBy = true),
            @Alias(name = "contentRevisionSeqId", entityAlias = "CRI"),
            @Alias(name = "maxRevisionSeqId", entityAlias = "CRI", field = "contentRevisionSeqId", function = AggregateFunction.MAX)
        }
    )
    public interface MaxRevisionItemViewView {}

    /**
     * Latest ContentApproval
     */
    @ViewEntity(
        name = "MaxContentApprovalView",
        packageName = "org.ofbiz.content.compdoc",
        title = "Latest ContentApproval",
        members = {
            @MemberEntity(entityAlias = "C", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentApproval")
        },
        aliases = {
            @Alias(name = "contentTypeId", entityAlias = "C", groupBy = true),
            @Alias(name = "contentId", entityAlias = "CA", groupBy = true),
            @Alias(name = "partyId", entityAlias = "CA", groupBy = true),
            @Alias(name = "roleTypeId", entityAlias = "CA", groupBy = true),
            @Alias(name = "sequenceNum", entityAlias = "CA"),
            @Alias(name = "contentRevisionSeqId", entityAlias = "CA"),
            @Alias(name = "maxContentRevisionSeqId", entityAlias = "CA", field = "contentRevisionSeqId", function = AggregateFunction.MAX)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "C",
                relEntityAlias = "CA",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface MaxContentApprovalViewView {}

    /**
     * Main Assoc To
     */
    @ViewEntity(
        name = "ContentAssocOptViewFrom",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc To",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "CA", prefix = "ca")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentIdTo")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocOptViewFromView {}

    /**
     * Content And Role View
     */
    @ViewEntity(
        name = "ContentAndRole",
        packageName = "org.ofbiz.content.content",
        title = "Content And Role View",
        members = {
            @MemberEntity(entityAlias = "CNT", entityName = "Content"),
            @MemberEntity(entityAlias = "CRLE", entityName = "ContentRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CNT"),
            @AliasAll(entityAlias = "CRLE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CNT",
                relEntityAlias = "CRLE",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContentType",
                keyMaps = {
                    @KeyMap(fieldName = "contentTypeId")
                }
            )
        }
    )
    public interface ContentAndRoleView {}

    /**
     * Main Assoc From and DataResource View
     */
    @ViewEntity(
        name = "ContentAssocDataResourceViewFrom",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc From and DataResource View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "CA", prefix = "ca"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentIdTo")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId", relFieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocDataResourceViewFromView {}

    /**
     * Main Assoc To and DataResource View
     */
    @ViewEntity(
        name = "ContentAssocDataResourceViewTo",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc To and DataResource View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CA", prefix = "ca"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId", relFieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocDataResourceViewToView {}

    /**
     * Main Assoc To and DataResource View Required
     */
    @ViewEntity(
        name = "ContentAssocDataResourceViewToReq",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc To and DataResource View Required",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CA", prefix = "ca"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId", relFieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "caContentIdTo", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocDataResourceViewToReqView {}

    /**
     * Main Assoc From View
     */
    @ViewEntity(
        name = "ContentAssocViewFrom",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc From View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "CA", prefix = "ca")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentIdTo"),
            @Alias(name = "fromDate", entityAlias = "CA", field = "fromDate"),
            @Alias(name = "thruDate", entityAlias = "CA", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocViewFromView {}

    /**
     * Main Assoc To View
     */
    @ViewEntity(
        name = "ContentAssocViewTo",
        packageName = "org.ofbiz.content.content",
        title = "Main Assoc To View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "CA", prefix = "ca")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentId"),
            @Alias(name = "fromDate", entityAlias = "CA", field = "fromDate"),
            @Alias(name = "thruDate", entityAlias = "CA", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocViewToView {}

    /**
     * Content and DataResource View
     */
    @ViewEntity(
        name = "ContentDataResourceView",
        packageName = "org.ofbiz.content.content",
        title = "Content and DataResource View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "VideoDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AudioDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DocumentDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ContentDataResourceViewView {}

    /**
     * Content and DataResource Required View
     */
    @ViewEntity(
        name = "ContentDataResourceRequiredView",
        packageName = "org.ofbiz.content.content",
        title = "Content and DataResource Required View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "VideoDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AudioDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DocumentDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdTo")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface ContentDataResourceRequiredViewView {}

    /**
     * Content and DataResource for SubContent View
     */
    @ViewEntity(
        name = "SubContentDataResourceView",
        packageName = "org.ofbiz.content.content",
        title = "Content and DataResource for SubContent View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "drDataResourceId", relFieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewFrom",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssocDataResourceViewTo",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdStart")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentPurpose",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentRole",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ContentAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "contentId", relFieldName = "contentIdTo")
                }
            )
        }
    )
    public interface SubContentDataResourceViewView {}

    /**
     * DataResource and Content View
     */
    @ViewEntity(
        name = "DataResourceContentView",
        packageName = "org.ofbiz.content.content",
        title = "DataResource and Content View",
        members = {
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "CO", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "DR"),
            @AliasAll(entityAlias = "CO", prefix = "co")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "CO",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "VideoDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AudioDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DocumentDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResourceType",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataCategory",
                keyMaps = {
                    @KeyMap(fieldName = "dataCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MimeType",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CharacterSet",
                keyMaps = {
                    @KeyMap(fieldName = "characterSetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceRole",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface DataResourceContentViewView {}

    /**
     * DataResource and Content Required View
     */
    @ViewEntity(
        name = "DataResourceContentRequiredView",
        packageName = "org.ofbiz.content.content",
        title = "DataResource and Content Required View",
        members = {
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "CO", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "DR"),
            @AliasAll(entityAlias = "CO", prefix = "co")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "DR",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ElectronicText",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ImageDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "VideoDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "AudioDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "DocumentDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OtherDataResource",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataResourceType",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataCategory",
                keyMaps = {
                    @KeyMap(fieldName = "dataCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "MimeType",
                keyMaps = {
                    @KeyMap(fieldName = "mimeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CharacterSet",
                keyMaps = {
                    @KeyMap(fieldName = "characterSetId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "DataResourceRole",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface DataResourceContentRequiredViewView {}

    /**
     * Content and DataResource and ElectronicText Required View
     */
    @ViewEntity(
        name = "ContentAndElectronicText",
        packageName = "org.ofbiz.content.content",
        title = "Content and DataResource and ElectronicText Required View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr"),
            @AliasAll(entityAlias = "EL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ContentAndElectronicTextView {}

    /**
     * ContentAssoc To Content and DataResource and ElectronicText Required View
     */
    @ViewEntity(
        name = "ContentAssocToElectronicText",
        packageName = "org.ofbiz.content.content",
        title = "ContentAssoc To Content and DataResource and ElectronicText Required View",
        members = {
            @MemberEntity(entityAlias = "CA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "CO", entityName = "Content"),
            @MemberEntity(entityAlias = "DR", entityName = "DataResource"),
            @MemberEntity(entityAlias = "EL", entityName = "ElectronicText")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CA", prefix = "ca"),
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "DR", prefix = "dr"),
            @AliasAll(entityAlias = "EL")
        },
        aliases = {
            @Alias(name = "contentIdStart", entityAlias = "CA", field = "contentId"),
            @Alias(name = "contentAssocTypeId", entityAlias = "CA", field = "contentAssocTypeId"),
            @Alias(name = "fromDate", entityAlias = "CA", field = "fromDate"),
            @Alias(name = "thruDate", entityAlias = "CA", field = "thruDate")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CA",
                relEntityAlias = "CO",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "DR",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            ),
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "EL",
                keyMaps = {
                    @KeyMap(fieldName = "dataResourceId")
                }
            )
        }
    )
    public interface ContentAssocToElectronicTextView {}

    /**
     * Survey Question And Application View
     */
    @ViewEntity(
        name = "SurveyQuestionAndAppl",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Question And Application View",
        members = {
            @MemberEntity(entityAlias = "SQ", entityName = "SurveyQuestion"),
            @MemberEntity(entityAlias = "SA", entityName = "SurveyQuestionAppl")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SQ"),
            @AliasAll(entityAlias = "SA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SQ",
                relEntityAlias = "SA",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyQuestionCategory",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyQuestionType",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Survey",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "SurveyQuestionOption",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Enumeration",
                keyMaps = {
                    @KeyMap(fieldName = "enumTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Geo",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyPage",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyPageSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyMultiResp",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyMultiRespColumn",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId"),
                    @KeyMap(fieldName = "surveyMultiRespColId")
                }
            )
        }
    )
    public interface SurveyQuestionAndApplView {}

    /**
     * Survey Response And Answer View
     */
    @ViewEntity(
        name = "SurveyResponseAndAnswer",
        packageName = "org.ofbiz.content.survey",
        title = "Survey Response And Answer View",
        members = {
            @MemberEntity(entityAlias = "SR", entityName = "SurveyResponse"),
            @MemberEntity(entityAlias = "SRA", entityName = "SurveyResponseAnswer")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SR"),
            @AliasAll(entityAlias = "SRA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SR",
                relEntityAlias = "SRA",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Survey",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyQuestion",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyQuestionOption",
                keyMaps = {
                    @KeyMap(fieldName = "surveyQuestionId"),
                    @KeyMap(fieldName = "surveyOptionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyResponse",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Content",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "SurveyMultiRespColumn",
                keyMaps = {
                    @KeyMap(fieldName = "surveyId"),
                    @KeyMap(fieldName = "surveyMultiRespId"),
                    @KeyMap(fieldName = "surveyMultiRespColId")
                }
            )
        }
    )
    public interface SurveyResponseAndAnswerView {}

    /**
     * Web Sites by contentId
     */
    @ViewEntity(
        name = "WebSiteAndContent",
        packageName = "org.ofbiz.content.website",
        title = "Web Sites by contentId",
        members = {
            @MemberEntity(entityAlias = "WS", entityName = "WebSite"),
            @MemberEntity(entityAlias = "WSCT", entityName = "WebSiteContent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WS"),
            @AliasAll(entityAlias = "WSCT", excludes = {"webSiteId"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WS",
                relEntityAlias = "WSCT",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            )
        }
    )
    public interface WebSiteAndContentView {}

    /**
     * ContentAssoc and ContentPurpose View
     */
    @ViewEntity(
        name = "ContentAssocAndContentPurpose",
        packageName = "org.ofbiz.content.compdoc",
        title = "ContentAssoc and ContentPurpose View",
        members = {
            @MemberEntity(entityAlias = "CNTA", entityName = "ContentAssoc"),
            @MemberEntity(entityAlias = "CNTP", entityName = "ContentPurpose")
        },
        aliases = {
            @Alias(name = "contentId", entityAlias = "CNTA"),
            @Alias(name = "contentIdTo", entityAlias = "CNTA"),
            @Alias(name = "contentAssocTypeId", entityAlias = "CNTA"),
            @Alias(name = "fromDate", entityAlias = "CNTA"),
            @Alias(name = "thruDate", entityAlias = "CNTA"),
            @Alias(name = "dataSourceId", entityAlias = "CNTA"),
            @Alias(name = "mapKey", entityAlias = "CNTA"),
            @Alias(name = "contentPurposeTypeId", entityAlias = "CNTP"),
            @Alias(name = "sequenceNum", entityAlias = "CNTP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CNTA",
                relEntityAlias = "CNTP",
                keyMaps = {
                    @KeyMap(fieldName = "contentIdTo", relFieldName = "contentId")
                }
            )
        }
    )
    public interface ContentAssocAndContentPurposeView {}

    @ExtendEntity(
        name = "WebPage",
        fields = {
            @Field(name = "contentId", type = "id-ne")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "WEB_PAGE_CONTENT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface WebPageExtension {}

    @ExtendEntity(
        name = "PortalPage",
        fields = {
            @Field(name = "helpContentId", type = "id", description = "Used to give contentId which will be shown when help on this page will be called")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "PORTPAL_HELP_CONT",
                keyMaps = {
                    @KeyMap(fieldName = "helpContentId", relFieldName = "contentId")
                }
            )
        }
    )
    public interface PortalPageExtension {}

}
