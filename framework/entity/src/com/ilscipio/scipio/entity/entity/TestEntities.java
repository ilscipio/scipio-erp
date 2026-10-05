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
package com.ilscipio.scipio.entity.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TestEntities {

    /**
     * Testing
     */
    @Entity(
        name = "Testing",
        packageName = "org.ofbiz.entity.test",
        title = "Testing",
        fields = {
            @Field(name = "testingId", type = "id-ne"),
            @Field(name = "testingTypeId", type = "id-ne"),
            @Field(name = "testingName", type = "name", enableAuditLog = true),
            @Field(name = "description", type = "description"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "testingSize", type = "numeric"),
            @Field(name = "testingDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TestingType",
                fkName = "ENTITY_ENTY_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "testingTypeId")
                }
            )
        }
    )
    public interface TestingEntity {}

    /**
     * Testing Entity Type
     */
    @Entity(
        name = "TestingType",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Entity Type",
        fields = {
            @Field(name = "testingTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingTypeId")
        }
    )
    public interface TestingTypeEntity {}

    /**
     * Testing Subtype
     */
    @Entity(
        name = "TestingSubtype",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Subtype",
        fields = {
            @Field(name = "testingTypeId", type = "id-ne"),
            @Field(name = "subtypeDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingTypeId")
        }
    )
    public interface TestingSubtypeEntity {}

    /**
     * Entity for testing EntityStatus concept
     * An entity for testing EntityStatus concept
     */
    @Entity(
        name = "TestingStatus",
        packageName = "org.ofbiz.entity.test",
        title = "Entity for testing EntityStatus concept",
        description = "An entity for testing EntityStatus concept",
        fields = {
            @Field(name = "testingStatusId", type = "id-ne"),
            @Field(name = "testingId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingStatusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "TEST_STA_STSITM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "TEST_STA_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface TestingStatusEntity {}

    /**
     * Entity for testing the blob type (Deprecated)
     * Deprecated - use TestFieldType instead
     */
    @Entity(
        name = "TestBlob",
        packageName = "org.ofbiz.entity.test",
        title = "Entity for testing the blob type (Deprecated)",
        description = "Deprecated - use TestFieldType instead",
        fields = {
            @Field(name = "testBlobId", type = "id-ne"),
            @Field(name = "testBlobField", type = "blob")
        },
        primaryKeys = {
            @PrimaryKey(field = "testBlobId")
        }
    )
    public interface TestBlobEntity {}

    /**
     * Entity for testing the field data types
     * An entity for testing the field data types
     */
    @Entity(
        name = "TestFieldType",
        packageName = "org.ofbiz.entity.test",
        title = "Entity for testing the field data types",
        description = "An entity for testing the field data types",
        fields = {
            @Field(name = "testFieldTypeId", type = "id-ne"),
            @Field(name = "blobField", type = "blob"),
            @Field(name = "byteArrayField", type = "byte-array"),
            @Field(name = "objectField", type = "object"),
            @Field(name = "dateField", type = "date"),
            @Field(name = "timeField", type = "time"),
            @Field(name = "dateTimeField", type = "date-time"),
            @Field(name = "fixedPointField", type = "fixed-point"),
            @Field(name = "floatingPointField", type = "floating-point"),
            @Field(name = "numericField", type = "numeric"),
            @Field(name = "clobField", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "testFieldTypeId")
        }
    )
    public interface TestFieldTypeEntity {}

    /**
     * Testing Item
     */
    @Entity(
        name = "TestingItem",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Item",
        fields = {
            @Field(name = "testingId", type = "id-ne"),
            @Field(name = "testingSeqId", type = "id-ne"),
            @Field(name = "testingHistory", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingId"),
            @PrimaryKey(field = "testingSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Testing",
                fkName = "TESTING_IT_TEST",
                keyMaps = {
                    @KeyMap(fieldName = "testingId")
                }
            )
        }
    )
    public interface TestingItemEntity {}

    /**
     * Testing Node
     */
    @Entity(
        name = "TestingNode",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Node",
        fields = {
            @Field(name = "testingNodeId", type = "id-ne"),
            @Field(name = "primaryParentNodeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingNodeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TestingNode",
                title = "PrimaryParent",
                fkName = "TESTNG_NDE_PARNT",
                keyMaps = {
                    @KeyMap(fieldName = "primaryParentNodeId", relFieldName = "testingNodeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "TestingNode",
                title = "PrimaryChild",
                keyMaps = {
                    @KeyMap(fieldName = "testingNodeId", relFieldName = "primaryParentNodeId")
                }
            )
        }
    )
    public interface TestingNodeEntity {}

    /**
     * Testing Node Member
     */
    @Entity(
        name = "TestingNodeMember",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Node Member",
        fields = {
            @Field(name = "testingNodeId", type = "id-ne"),
            @Field(name = "testingId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "extendFromDate", type = "date-time"),
            @Field(name = "extendThruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingNodeId"),
            @PrimaryKey(field = "testingId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Testing",
                fkName = "TESTING_NMBR_TEST",
                keyMaps = {
                    @KeyMap(fieldName = "testingId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TestingNode",
                fkName = "TEST_NMBR_NODE",
                keyMaps = {
                    @KeyMap(fieldName = "testingNodeId")
                }
            )
        }
    )
    public interface TestingNodeMemberEntity {}

    /**
     * Testing Crypto
     */
    @Entity(
        name = "TestingCrypto",
        packageName = "org.ofbiz.entity.test",
        title = "Testing Crypto",
        fields = {
            @Field(name = "testingCryptoId", type = "id-ne"),
            @Field(name = "testingCryptoTypeId", type = "id-ne"),
            @Field(name = "unencryptedValue", type = "description"),
            @Field(name = "encryptedValue", type = "description", encrypt = "true"),
            @Field(name = "saltedEncryptedValue", type = "description", encrypt = "salt")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingCryptoId")
        }
    )
    public interface TestingCryptoEntity {}

    /**
     * Testing
     */
    @Entity(
        name = "TestingRemoveAll",
        packageName = "org.ofbiz.entity.test",
        title = "Testing",
        fields = {
            @Field(name = "testingRemoveAllId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "testingRemoveAllId")
        }
    )
    public interface TestingRemoveAllEntity {}

    /**
     * Testing And TestingSubtype View
     */
    @ViewEntity(
        name = "TestingViewPks",
        packageName = "org.ofbiz.entity.test",
        title = "Testing And TestingSubtype View",
        members = {
            @MemberEntity(entityAlias = "TST", entityName = "TestingType"),
            @MemberEntity(entityAlias = "TSTSUB", entityName = "TestingSubtype")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TST"),
            @AliasAll(entityAlias = "TSTSUB")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TST",
                relEntityAlias = "TSTSUB",
                keyMaps = {
                    @KeyMap(fieldName = "testingTypeId")
                }
            )
        }
    )
    public interface TestingViewPksView {}

    /**
     * TestingNode And TestingNodeMember View
     */
    @ViewEntity(
        name = "TestingNodeAndMember",
        packageName = "org.ofbiz.entity.test",
        title = "TestingNode And TestingNodeMember View",
        members = {
            @MemberEntity(entityAlias = "TN", entityName = "TestingNode"),
            @MemberEntity(entityAlias = "TNM", entityName = "TestingNodeMember")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TN"),
            @AliasAll(entityAlias = "TNM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "TN",
                relEntityAlias = "TNM",
                keyMaps = {
                    @KeyMap(fieldName = "testingNodeId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TestingNodeMember",
                keyMaps = {
                    @KeyMap(fieldName = "testingNodeId"),
                    @KeyMap(fieldName = "testingId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TestingNode",
                keyMaps = {
                    @KeyMap(fieldName = "testingNodeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Testing",
                keyMaps = {
                    @KeyMap(fieldName = "testingId")
                }
            )
        }
    )
    public interface TestingNodeAndMemberView {}

    /**
     * TestingCrypto Raw View
     */
    @ViewEntity(
        name = "TestingCryptoRawView",
        packageName = "org.ofbiz.entity.test",
        title = "TestingCrypto Raw View",
        members = {
            @MemberEntity(entityAlias = "TC", entityName = "TestingCrypto")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "TC")
        },
        aliases = {
            @Alias(name = "rawEncryptedValue", complexAlias = @ComplexAlias(operator = "+", fields = {@ComplexAliasField(entityAlias = "TC", field = "encryptedValue")})),
            @Alias(name = "rawSaltedEncryptedValue", complexAlias = @ComplexAlias(operator = "+", fields = {@ComplexAliasField(entityAlias = "TC", field = "saltedEncryptedValue")}))
        }
    )
    public interface TestingCryptoRawViewView {}

}
