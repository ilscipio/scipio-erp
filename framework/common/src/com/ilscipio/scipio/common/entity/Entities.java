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
package com.ilscipio.scipio.common.entity;

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
     * Data Source
     */
    @Entity(
        name = "DataSource",
        packageName = "org.ofbiz.common.datasource",
        title = "Data Source",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "dataSourceId", type = "id-ne"),
            @Field(name = "dataSourceTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataSourceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSourceType",
                fkName = "DATA_SRC_TYP",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceTypeId")
                }
            )
        }
    )
    public interface DataSourceEntity {}

    /**
     * Data Source Type
     */
    @Entity(
        name = "DataSourceType",
        packageName = "org.ofbiz.common.datasource",
        title = "Data Source Type",
        fields = {
            @Field(name = "dataSourceTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "dataSourceTypeId")
        }
    )
    public interface DataSourceTypeEntity {}

    /**
     * Email Template Setting
     */
    @Entity(
        name = "EmailTemplateSetting",
        packageName = "org.ofbiz.common.email",
        title = "Email Template Setting",
        fields = {
            @Field(name = "emailTemplateSettingId", type = "id-ne"),
            @Field(name = "emailType", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "bodyScreenLocation", type = "long-varchar", description = "if empty defaults to a screen based on the emailType"),
            @Field(name = "xslfoAttachScreenLocation", type = "long-varchar", description = "if specified is used to generate XSL:FO that is transformed to a PDF via Apache FOP and attached to the email"),
            @Field(name = "fromAddress", type = "email"),
            @Field(name = "ccAddress", type = "email"),
            @Field(name = "bccAddress", type = "email"),
            @Field(name = "subject", type = "comment"),
            @Field(name = "contentType", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "emailTemplateSettingId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "EMAILSET_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "emailType", relFieldName = "enumId")
                }
            )
        }
    )
    public interface EmailTemplateSettingEntity {}

    /**
     * Enumeration
     */
    @Entity(
        name = "Enumeration",
        packageName = "org.ofbiz.common.enum",
        title = "Enumeration",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "enumId", type = "id-ne"),
            @Field(name = "enumTypeId", type = "id-ne"),
            @Field(name = "enumCode", type = "short-varchar"),
            @Field(name = "sequenceId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "enumId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EnumerationType",
                fkName = "ENUM_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "enumTypeId")
                }
            )
        }
    )
    public interface EnumerationEntity {}

    /**
     * Enumeration Type
     */
    @Entity(
        name = "EnumerationType",
        packageName = "org.ofbiz.common.enum",
        title = "Enumeration Type",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "enumTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "enumTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "EnumerationType",
                title = "Parent",
                fkName = "ENUM_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "enumTypeId")
                }
            )
        }
    )
    public interface EnumerationTypeEntity {}

    /**
     * Country Capital
     */
    @Entity(
        name = "CountryCapital",
        packageName = "org.ofbiz.common.geo",
        title = "Country Capital",
        dependentOn = "CountryCode",
        fields = {
            @Field(name = "countryCode", type = "id-ne"),
            @Field(name = "countryCapital", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "countryCode")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CountryCode",
                fkName = "CNTRY_CAP_TO_CODE",
                keyMaps = {
                    @KeyMap(fieldName = "countryCode")
                }
            )
        }
    )
    public interface CountryCapitalEntity {}

    /**
     * ISO Country Code
     */
    @Entity(
        name = "CountryCode",
        packageName = "org.ofbiz.common.geo",
        title = "ISO Country Code",
        fields = {
            @Field(name = "countryCode", type = "id-ne"),
            @Field(name = "countryAbbr", type = "short-varchar"),
            @Field(name = "countryNumber", type = "short-varchar"),
            @Field(name = "countryName", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "countryCode")
        }
    )
    public interface CountryCodeEntity {}

    /**
     * Telephone Country Code
     */
    @Entity(
        name = "CountryTeleCode",
        packageName = "org.ofbiz.common.geo",
        title = "Telephone Country Code",
        dependentOn = "CountryCode",
        fields = {
            @Field(name = "countryCode", type = "id-ne"),
            @Field(name = "teleCode", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "countryCode")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CountryCode",
                fkName = "CNTRY_TELE_TO_CODE",
                keyMaps = {
                    @KeyMap(fieldName = "countryCode")
                }
            )
        }
    )
    public interface CountryTeleCodeEntity {}

    @Entity(
        name = "CountryAddressFormat",
        packageName = "org.ofbiz.common.geo",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "geoId", type = "id-ne"),
            @Field(name = "geoAssocTypeId", type = "id"),
            @Field(name = "requireStateProvinceId", type = "id"),
            @Field(name = "requirePostalCode", type = "indicator"),
            @Field(name = "postalCodeRegex", type = "long-varchar"),
            @Field(name = "hasPostalCodeExt", type = "indicator"),
            @Field(name = "requirePostalCodeExt", type = "indicator"),
            @Field(name = "addressFormat", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                fkName = "CNY_ADR_GEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoAssocType",
                fkName = "CNY_ADR_GEO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "geoAssocTypeId")
                }
            )
        }
    )
    public interface CountryAddressFormatEntity {}

    /**
     * Geographic Boundary
     */
    @Entity(
        name = "Geo",
        packageName = "org.ofbiz.common.geo",
        title = "Geographic Boundary",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "geoId", type = "id-ne"),
            @Field(name = "geoTypeId", type = "id"),
            @Field(name = "geoName", type = "name"),
            @Field(name = "geoCode", type = "short-varchar"),
            @Field(name = "geoSecCode", type = "short-varchar"),
            @Field(name = "abbreviation", type = "short-varchar"),
            @Field(name = "wellKnownText", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoType",
                fkName = "GEO_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "geoTypeId")
                }
            )
        }
    )
    public interface GeoEntity {}

    /**
     * Geographic Boundary Association
     */
    @Entity(
        name = "GeoAssoc",
        packageName = "org.ofbiz.common.geo",
        title = "Geographic Boundary Association",
        fields = {
            @Field(name = "geoId", type = "id-ne", description = "The enclosed geo"),
            @Field(name = "geoIdTo", type = "id-ne", description = "The enclosing geo"),
            @Field(name = "geoAssocTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoId"),
            @PrimaryKey(field = "geoIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Main",
                fkName = "GEO_ASSC_TO_MAIN",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Assoc",
                fkName = "GEO_ASSC_TO_ASSC",
                keyMaps = {
                    @KeyMap(fieldName = "geoIdTo", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoAssocType",
                fkName = "GEO_ASSC_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "geoAssocTypeId")
                }
            )
        }
    )
    public interface GeoAssocEntity {}

    /**
     * Geographic Boundary Association
     */
    @Entity(
        name = "GeoAssocType",
        packageName = "org.ofbiz.common.geo",
        title = "Geographic Boundary Association",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "geoAssocTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoAssocTypeId")
        }
    )
    public interface GeoAssocTypeEntity {}

    /**
     * Geographic Location
     */
    @Entity(
        name = "GeoPoint",
        packageName = "org.ofbiz.common.geo",
        title = "Geographic Location",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "geoPointId", type = "id-ne"),
            @Field(name = "geoPointTypeEnumId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "dataSourceId", type = "id"),
            @Field(name = "latitude", type = "short-varchar", notNull = true),
            @Field(name = "longitude", type = "short-varchar", notNull = true),
            @Field(name = "elevation", type = "fixed-point"),
            @Field(name = "elevationUomId", type = "id", description = "UOM for elevation (feet, meters, etc.)"),
            @Field(name = "information", type = "comment", description = "To enter any related information")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoPointId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DataSource",
                fkName = "GEOPOINT_DTSRC",
                keyMaps = {
                    @KeyMap(fieldName = "dataSourceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "GeoPointType",
                fkName = "GEOPOINT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "geoPointTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Elevation",
                fkName = "GPT_ELEV_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "elevationUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface GeoPointEntity {}

    /**
     * Geographic Boundary Type
     */
    @Entity(
        name = "GeoType",
        packageName = "org.ofbiz.common.geo",
        title = "Geographic Boundary Type",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "geoTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "geoTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GeoType",
                title = "Parent",
                fkName = "GEO_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "geoTypeId")
                }
            )
        }
    )
    public interface GeoTypeEntity {}

    @Entity(
        name = "KeywordThesaurus",
        packageName = "org.ofbiz.common.keyword",
        fields = {
            @Field(name = "enteredKeyword", type = "long-varchar"),
            @Field(name = "alternateKeyword", type = "long-varchar"),
            @Field(name = "relationshipEnumId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "enteredKeyword"),
            @PrimaryKey(field = "alternateKeyword")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Relationship",
                fkName = "KW_THRS_RLENM",
                keyMaps = {
                    @KeyMap(fieldName = "relationshipEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface KeywordThesaurusEntity {}

    @Entity(
        name = "StandardLanguage",
        packageName = "org.ofbiz.common.language",
        fields = {
            @Field(name = "standardLanguageId", type = "id-ne"),
            @Field(name = "langCode3t", type = "very-short"),
            @Field(name = "langCode3b", type = "very-short"),
            @Field(name = "langCode2", type = "very-short"),
            @Field(name = "langName", type = "short-varchar"),
            @Field(name = "langFamily", type = "short-varchar"),
            @Field(name = "langCharset", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "standardLanguageId")
        }
    )
    public interface StandardLanguageEntity {}

    /**
     * Custom Method
     */
    @Entity(
        name = "CustomMethod",
        packageName = "org.ofbiz.common.method",
        title = "Custom Method",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "customMethodId", type = "id-ne"),
            @Field(name = "customMethodTypeId", type = "id"),
            @Field(name = "customMethodName", type = "long-varchar"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "customMethodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethodType",
                fkName = "CME_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "customMethodTypeId")
                }
            )
        }
    )
    public interface CustomMethodEntity {}

    /**
     * Custom Method Type
     */
    @Entity(
        name = "CustomMethodType",
        packageName = "org.ofbiz.common.method",
        title = "Custom Method Type",
        fields = {
            @Field(name = "customMethodTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "customMethodTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethodType",
                title = "Parent",
                fkName = "CME_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "customMethodTypeId")
                }
            )
        }
    )
    public interface CustomMethodTypeEntity {}

    /**
     * Note Data
     */
    @Entity(
        name = "NoteData",
        packageName = "org.ofbiz.common.note",
        title = "Note Data",
        fields = {
            @Field(name = "noteId", type = "id-ne"),
            @Field(name = "noteName", type = "name"),
            @Field(name = "noteInfo", type = "very-long"),
            @Field(name = "noteDateTime", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "noteId")
        }
    )
    public interface NoteDataEntity {}

    /**
     * Custom Time Period
     */
    @Entity(
        name = "CustomTimePeriod",
        packageName = "org.ofbiz.common.period",
        title = "Custom Time Period",
        fields = {
            @Field(name = "customTimePeriodId", type = "id-ne"),
            @Field(name = "parentPeriodId", type = "id"),
            @Field(name = "periodTypeId", type = "id"),
            @Field(name = "periodNum", type = "numeric"),
            @Field(name = "periodName", type = "name"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "isClosed", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "customTimePeriodId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomTimePeriod",
                title = "Parent",
                fkName = "ORG_PRD_PARPER",
                keyMaps = {
                    @KeyMap(fieldName = "parentPeriodId", relFieldName = "customTimePeriodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PeriodType",
                fkName = "ORG_PRD_PERTYP",
                keyMaps = {
                    @KeyMap(fieldName = "periodTypeId")
                }
            )
        }
    )
    public interface CustomTimePeriodEntity {}

    /**
     * Period Type
     */
    @Entity(
        name = "PeriodType",
        packageName = "org.ofbiz.common.period",
        title = "Period Type",
        fields = {
            @Field(name = "periodTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "periodLength", type = "numeric"),
            @Field(name = "uomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "periodTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "PER_TYPE_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            )
        }
    )
    public interface PeriodTypeEntity {}

    /**
     * Status
     */
    @Entity(
        name = "StatusItem",
        packageName = "org.ofbiz.common.status",
        title = "Status",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusTypeId", type = "id-ne"),
            @Field(name = "statusCode", type = "short-varchar"),
            @Field(name = "sequenceId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusType",
                fkName = "STATUS_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "statusTypeId")
                }
            )
        }
    )
    public interface StatusItemEntity {}

    /**
     * Status Type
     */
    @Entity(
        name = "StatusType",
        packageName = "org.ofbiz.common.status",
        title = "Status Type",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "statusTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusType",
                title = "Parent",
                fkName = "STATUS_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "statusTypeId")
                }
            )
        }
    )
    public interface StatusTypeEntity {}

    /**
     * Status Valid Change
     */
    @Entity(
        name = "StatusValidChange",
        packageName = "org.ofbiz.common.status",
        title = "Status Valid Change",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusIdTo", type = "id-ne"),
            @Field(name = "conditionExpression", type = "long-varchar"),
            @Field(name = "transitionName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "statusIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Main",
                fkName = "STATUS_CHG_MAIN",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "To",
                fkName = "STATUS_CHG_TO",
                keyMaps = {
                    @KeyMap(fieldName = "statusIdTo", relFieldName = "statusId")
                }
            )
        }
    )
    public interface StatusValidChangeEntity {}

    /**
     * Unit Of Measure
     */
    @Entity(
        name = "Uom",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "uomId", type = "id-ne"),
            @Field(name = "uomTypeId", type = "id"),
            @Field(name = "abbreviation", type = "short-varchar"),
            @Field(name = "numericCode", type = "numeric"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "uomId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UomType",
                fkName = "UOM_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "uomTypeId")
                }
            )
        }
    )
    public interface UomEntity {}

    /**
     * Unit Of Measure Conversion Type
     */
    @Entity(
        name = "UomConversion",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure Conversion Type",
        fields = {
            @Field(name = "uomId", type = "id-ne"),
            @Field(name = "uomIdTo", type = "id-ne"),
            @Field(name = "conversionFactor", type = "floating-point"),
            @Field(name = "customMethodId", type = "id-ne"),
            @Field(name = "decimalScale", type = "numeric"),
            @Field(name = "roundingMode", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "uomId"),
            @PrimaryKey(field = "uomIdTo")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Main",
                fkName = "UOM_CONV_MAIN",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "ConvTo",
                fkName = "UOM_CONV_TO",
                keyMaps = {
                    @KeyMap(fieldName = "uomIdTo", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                title = "uomCustomMethod",
                fkName = "UOM_CUSTOM_METHOD",
                keyMaps = {
                    @KeyMap(fieldName = "customMethodId", relFieldName = "customMethodId")
                }
            )
        }
    )
    public interface UomConversionEntity {}

    /**
     * Unit Of Measure Conversion Entity for those Units of Measure whose conversion values change over time (ie, currencies)
     */
    @Entity(
        name = "UomConversionDated",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure Conversion Entity for those Units of Measure whose conversion values change over time (ie, currencies)",
        fields = {
            @Field(name = "uomId", type = "id-ne"),
            @Field(name = "uomIdTo", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "conversionFactor", type = "floating-point"),
            @Field(name = "customMethodId", type = "id-ne"),
            @Field(name = "decimalScale", type = "numeric"),
            @Field(name = "roundingMode", type = "id"),
            @Field(name = "purposeEnumId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "uomId"),
            @PrimaryKey(field = "uomIdTo"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "DatedMain",
                fkName = "DATE_UOM_CONV_MAIN",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "DatedConvTo",
                fkName = "DATE_UOM_CONV_TO",
                keyMaps = {
                    @KeyMap(fieldName = "uomIdTo", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomMethod",
                title = "uomCustomMethod",
                fkName = "UOMD_CUSTOM_METHOD",
                keyMaps = {
                    @KeyMap(fieldName = "customMethodId", relFieldName = "customMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Purpose",
                fkName = "UOMD_PURPOSE_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "purposeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface UomConversionDatedEntity {}

    /**
     * Unit Of Measure Group
     */
    @Entity(
        name = "UomGroup",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure Group",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "uomGroupId", type = "id-ne"),
            @Field(name = "uomId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "uomGroupId"),
            @PrimaryKey(field = "uomId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "UOM_GROUP_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            )
        }
    )
    public interface UomGroupEntity {}

    /**
     * Unit Of Measure Type
     */
    @Entity(
        name = "UomType",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure Type",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "uomTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "uomTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UomType",
                title = "Parent",
                fkName = "UOM_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "uomTypeId")
                }
            )
        }
    )
    public interface UomTypeEntity {}

    /**
     * Application preferences for a given userLogin.
     * The UserPreference entity contains one entry per preference per           userLogin. User preferences are stored as key/value pairs (userPrefTypeId/userPrefValue).           All values are stored as strings. Value strings can be converted to           other data types by specifying a java data type in the userPrefDataType field.       
     */
    @Entity(
        name = "UserPreference",
        packageName = "org.ofbiz.common.user",
        title = "Application preferences for a given userLogin.",
        description = "The UserPreference entity contains one entry per preference per\n          userLogin. User preferences are stored as key/value pairs (userPrefTypeId/userPrefValue).\n          All values are stored as strings. Value strings can be converted to\n          other data types by specifying a java data type in the userPrefDataType field.\n      ",
        fields = {
            @Field(name = "userLoginId", type = "id-vlong-ne"),
            @Field(name = "userPrefTypeId", type = "id-long-ne", description = "A unique identifier for this preference"),
            @Field(name = "userPrefGroupTypeId", type = "id-long", description = "Used to assemble groups of preferences"),
            @Field(name = "userPrefValue", type = "value", description = "Contains the value of this preference"),
            @Field(name = "userPrefDataType", type = "id-long", description = "The java data type of this preference (empty = java.lang.String)")
        },
        primaryKeys = {
            @PrimaryKey(field = "userLoginId"),
            @PrimaryKey(field = "userPrefTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                fkName = "UP_USER_LOGIN",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserPrefGroupType",
                fkName = "UP_USER_GROUP_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "userPrefGroupTypeId")
                }
            )
        }
    )
    public interface UserPreferenceEntity {}

    /**
     * Defines a group of User Preferences
     * The UserPrefGroupType entity contains one entry per preference           group type.       
     */
    @Entity(
        name = "UserPrefGroupType",
        packageName = "org.ofbiz.common.user",
        title = "Defines a group of User Preferences",
        description = "The UserPrefGroupType entity contains one entry per preference\n          group type.\n      ",
        fields = {
            @Field(name = "userPrefGroupTypeId", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "userPrefGroupTypeId")
        }
    )
    public interface UserPrefGroupTypeEntity {}

    /**
     * Custom Screen
     */
    @Entity(
        name = "CustomScreen",
        packageName = "org.ofbiz.common.screen",
        title = "Custom Screen",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "customScreenId", type = "id"),
            @Field(name = "customScreenTypeId", type = "id"),
            @Field(name = "customScreenName", type = "long-varchar"),
            @Field(name = "customScreenLocation", type = "long-varchar"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "customScreenId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustomScreenType",
                fkName = "CSCR_TO_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "customScreenTypeId")
                }
            )
        }
    )
    public interface CustomScreenEntity {}

    /**
     * Custom Screen Type
     */
    @Entity(
        name = "CustomScreenType",
        packageName = "org.ofbiz.common.screen",
        title = "Custom Screen Type",
        fields = {
            @Field(name = "customScreenTypeId", type = "id"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "customScreenTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CustomScreenType",
                title = "Child",
                fkName = "CSCR_TYPE_CHILD",
                keyMaps = {
                    @KeyMap(fieldName = "customScreenTypeId", relFieldName = "parentTypeId")
                }
            )
        }
    )
    public interface CustomScreenTypeEntity {}

    /**
     * Defines a set of Visual Themes
     * Groups toghether Visual Themes that can be used for one (or a set of) application.
     */
    @Entity(
        name = "VisualThemeSet",
        packageName = "org.ofbiz.common.theme",
        title = "Defines a set of Visual Themes",
        description = "Groups toghether Visual Themes that can be used for one (or a set of) application.",
        fields = {
            @Field(name = "visualThemeSetId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "visualThemeSetId")
        }
    )
    public interface VisualThemeSetEntity {}

    /**
     * Defines a Visual Theme
     * The VisualTheme entity contains one entry per visual theme.
     */
    @Entity(
        name = "VisualTheme",
        packageName = "org.ofbiz.common.theme",
        title = "Defines a Visual Theme",
        description = "The VisualTheme entity contains one entry per visual theme.",
        defaultResourceName = "CommonEntityLabels",
        fields = {
            @Field(name = "visualThemeId", type = "id-ne"),
            @Field(name = "visualThemeSetId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "visualThemeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "VisualThemeSet",
                fkName = "VT_THEME_SET",
                keyMaps = {
                    @KeyMap(fieldName = "visualThemeSetId")
                }
            )
        }
    )
    public interface VisualThemeEntity {}

    /**
     * Contains All Visual Theme Resources
     * The VisualThemeResource entity contains visual theme           resources. Each visual theme can have any number of resources.
     */
    @Entity(
        name = "VisualThemeResource",
        packageName = "org.ofbiz.common.theme",
        title = "Contains All Visual Theme Resources",
        description = "The VisualThemeResource entity contains visual theme\n          resources. Each visual theme can have any number of resources.",
        fields = {
            @Field(name = "visualThemeId", type = "id-ne"),
            @Field(name = "resourceTypeEnumId", type = "id-ne"),
            @Field(name = "sequenceId", type = "id-ne", description = "Controls the loading order of duplicate resource types"),
            @Field(name = "resourceValue", type = "value", description = "Contains the resource value")
        },
        primaryKeys = {
            @PrimaryKey(field = "visualThemeId"),
            @PrimaryKey(field = "resourceTypeEnumId"),
            @PrimaryKey(field = "sequenceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "VisualTheme",
                fkName = "VT_RES_THEME",
                keyMaps = {
                    @KeyMap(fieldName = "visualThemeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "VT_RES_TYPE_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "resourceTypeEnumId", relFieldName = "enumId")
                }
            )
        }
    )
    public interface VisualThemeResourceEntity {}

    /**
     * Defines a Portlet to be used in Portals
     */
    @Entity(
        name = "PortalPortlet",
        packageName = "org.ofbiz.common.portal",
        title = "Defines a Portlet to be used in Portals",
        fields = {
            @Field(name = "portalPortletId", type = "id-ne"),
            @Field(name = "portletName", type = "name"),
            @Field(name = "screenName", type = "long-varchar"),
            @Field(name = "screenLocation", type = "long-varchar"),
            @Field(name = "editFormName", type = "long-varchar"),
            @Field(name = "editFormLocation", type = "long-varchar"),
            @Field(name = "description", type = "description"),
            @Field(name = "screenshot", type = "url"),
            @Field(name = "securityServiceName", type = "long-varchar", description = "The service named here is used to see if current user can see the portlet on the list of available portlets; the screen that the portlet calls should also call this service to check permission and not render; the service named here must implement the \"permissionInterface\" service just like services used for service permissions"),
            @Field(name = "securityMainAction", type = "short-varchar", description = "The main action which can be done with this portlet, possible values: CREATE UPDATE VIEW DELETE")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPortletId")
        }
    )
    public interface PortalPortletEntity {}

    /**
     * Portlet Category
     */
    @Entity(
        name = "PortletCategory",
        packageName = "org.ofbiz.common.portal",
        title = "Portlet Category",
        fields = {
            @Field(name = "portletCategoryId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "portletCategoryId")
        }
    )
    public interface PortletCategoryEntity {}

    /**
     * Defines Portlets included into Categories
     */
    @Entity(
        name = "PortletPortletCategory",
        packageName = "org.ofbiz.common.portal",
        title = "Defines Portlets included into Categories",
        fields = {
            @Field(name = "portalPortletId", type = "id-ne"),
            @Field(name = "portletCategoryId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPortletId"),
            @PrimaryKey(field = "portletCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPortlet",
                fkName = "PPTLTCAT_PTPL",
                keyMaps = {
                    @KeyMap(fieldName = "portalPortletId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortletCategory",
                fkName = "PPTLTCAT_PTLTCAT",
                keyMaps = {
                    @KeyMap(fieldName = "portletCategoryId")
                }
            )
        }
    )
    public interface PortletPortletCategoryEntity {}

    /**
     * Defines a Portal Page
     */
    @Entity(
        name = "PortalPage",
        packageName = "org.ofbiz.common.portal",
        title = "Defines a Portal Page",
        defaultResourceName = "CommonPortalEntityLabels",
        fields = {
            @Field(name = "portalPageId", type = "id-ne"),
            @Field(name = "portalPageName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "ownerUserLoginId", type = "id-vlong-ne"),
            @Field(name = "originalPortalPageId", type = "id", description = "The system portal page this page is derived from"),
            @Field(name = "parentPortalPageId", type = "id", description = "the parent this page is belonging to, normally the startpage of the portal page group"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "securityGroupId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPageId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPage",
                title = "Parent",
                fkName = "PortPage_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentPortalPageId", relFieldName = "portalPageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SecurityGroup",
                fkName = "PORTPAGE_SECGRP",
                keyMaps = {
                    @KeyMap(fieldName = "securityGroupId", relFieldName = "groupId")
                }
            )
        }
    )
    public interface PortalPageEntity {}

    /**
     * Defines a Portal Page
     */
    @Entity(
        name = "PortalPageColumn",
        packageName = "org.ofbiz.common.portal",
        title = "Defines a Portal Page",
        fields = {
            @Field(name = "portalPageId", type = "id-ne"),
            @Field(name = "columnSeqId", type = "id-ne"),
            @Field(name = "columnWidthPixels", type = "numeric"),
            @Field(name = "columnWidthPercentage", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPageId"),
            @PrimaryKey(field = "columnSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPage",
                fkName = "PRTL_PGCOL_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "portalPageId")
                }
            )
        }
    )
    public interface PortalPageColumnEntity {}

    /**
     * Defines Portlets included into Portal Pages
     */
    @Entity(
        name = "PortalPagePortlet",
        packageName = "org.ofbiz.common.portal",
        title = "Defines Portlets included into Portal Pages",
        fields = {
            @Field(name = "portalPageId", type = "id-ne"),
            @Field(name = "portalPortletId", type = "id-ne"),
            @Field(name = "portletSeqId", type = "id-ne", description = "Identify the portalPortlet instance in case more copy of the same portalPortlet are present in the same portalPage"),
            @Field(name = "columnSeqId", type = "id-ne"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPageId"),
            @PrimaryKey(field = "portalPortletId"),
            @PrimaryKey(field = "portletSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPage",
                fkName = "PRTL_PGPTLT_PAGE",
                keyMaps = {
                    @KeyMap(fieldName = "portalPageId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPortlet",
                fkName = "PRTL_PGPTLT_PTLT",
                keyMaps = {
                    @KeyMap(fieldName = "portalPortletId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PortalPageColumn",
                keyMaps = {
                    @KeyMap(fieldName = "portalPageId"),
                    @KeyMap(fieldName = "columnSeqId")
                }
            )
        }
    )
    public interface PortalPagePortletEntity {}

    /**
     * Allows to set different attribute values for each instance of the same portlet
     */
    @Entity(
        name = "PortletAttribute",
        packageName = "org.ofbiz.common.portal",
        title = "Allows to set different attribute values for each instance of the same portlet",
        fields = {
            @Field(name = "portalPageId", type = "id-ne"),
            @Field(name = "portalPortletId", type = "id-ne"),
            @Field(name = "portletSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description"),
            @Field(name = "attrType", type = "value")
        },
        primaryKeys = {
            @PrimaryKey(field = "portalPageId"),
            @PrimaryKey(field = "portalPortletId"),
            @PrimaryKey(field = "portletSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PortalPortlet",
                fkName = "PTLT_ATTR_PTLT",
                keyMaps = {
                    @KeyMap(fieldName = "portalPortletId")
                }
            )
        }
    )
    public interface PortletAttributeEntity {}

    /**
     * Configurable System Property
     * Defines and overrides properties as otherwise found in *.properties files in the filesystem (NOTE: not related to Java system properties)
     */
    @Entity(
        name = "SystemProperty",
        packageName = "org.ofbiz.common.property",
        title = "Configurable System Property",
        description = "Defines and overrides properties as otherwise found in *.properties files in the filesystem (NOTE: not related to Java system properties)",
        fields = {
            @Field(name = "systemResourceId", type = "id-long-ne"),
            @Field(name = "systemPropertyId", type = "id-long-ne"),
            @Field(name = "systemPropertyValue", type = "value"),
            @Field(name = "description", type = "description"),
            @Field(name = "useEmpty", type = "indicator", description = "Y means that empty systemPropertyValue is significant; N means empty systemPropertyValue\n                is not significant and the system should fallback on other source (e.g., *.properties file). Default: N (SCIPIO: 2018-07-27: Added)\n                NOTE: 2018-07-27: Scipio default for this differs from stock Ofbiz 16+ (which logically uses Y default);\n                    this is because in Scipio it is recognized that the SystemProperty entries and descriptions\n                    are useful and should not have to be deleted in order to trigger fallback on properties files.\n                    However, the code that updates SystemProperty must be aware of useEmpty logic."),
            @Field(name = "encrypt", type = "id", description = "Whether to consult EncryptedSystemProperty record. Values: false (default), true, true-nocache (SCIPIO: 3.0.0: Added)")
        },
        primaryKeys = {
            @PrimaryKey(field = "systemResourceId"),
            @PrimaryKey(field = "systemPropertyId")
        }
    )
    public interface SystemPropertyEntity {}

    /**
     * Configurable Encrypted System Property
     * Defines an encrypted value for a SystemProperty record (stored separately for separate cache control)
     */
    @Entity(
        name = "SystemPropertyEnc",
        packageName = "org.ofbiz.common.property",
        title = "Configurable Encrypted System Property",
        description = "Defines an encrypted value for a SystemProperty record (stored separately for separate cache control)",
        fields = {
            @Field(name = "systemResourceId", type = "id-long-ne"),
            @Field(name = "systemPropertyId", type = "id-long-ne"),
            @Field(name = "systemPropertyValue", type = "value", encrypt = "true")
        },
        primaryKeys = {
            @PrimaryKey(field = "systemResourceId"),
            @PrimaryKey(field = "systemPropertyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SystemProperty",
                fkName = "SYSPROP_ORIG",
                keyMaps = {
                    @KeyMap(fieldName = "systemResourceId"),
                    @KeyMap(fieldName = "systemPropertyId")
                }
            )
        }
    )
    public interface SystemPropertyEncEntity {}

    /**
     * Configurable Label Property
     * Defines and overrides localized resource and label properties as otherwise found in *UiLabels.xml files in the filesystem (SCIPIO)
     */
    @Entity(
        name = "LocalizedProperty",
        packageName = "org.ofbiz.common.property",
        title = "Configurable Label Property",
        description = "Defines and overrides localized resource and label properties as otherwise found in *UiLabels.xml files in the filesystem (SCIPIO)",
        fields = {
            @Field(name = "resourceId", type = "id-long-ne"),
            @Field(name = "propertyId", type = "id-long-ne"),
            @Field(name = "lang", type = "id-ne"),
            @Field(name = "value", type = "value"),
            @Field(name = "description", type = "description"),
            @Field(name = "useEmpty", type = "indicator", description = "SCIPIO: Y means that empty systemPropertyValue is significant; N means empty systemPropertyValue\n                is not significant and the system should fallback on other source (e.g., *.properties file). Default: N (added 2018-07-27)\n                NOTE: 2018-07-27: Scipio default for this differs from stock Ofbiz 16+ (which logically uses Y default);\n                this is because in Scipio it is recognized that the SystemProperty entries and descriptions\n                are useful and should not have to be deleted in order to trigger fallback on properties files.\n                However, the code that updates SystemProperty must be aware of useEmpty logic.")
        },
        primaryKeys = {
            @PrimaryKey(field = "resourceId"),
            @PrimaryKey(field = "propertyId"),
            @PrimaryKey(field = "lang")
        }
    )
    public interface LocalizedPropertyEntity {}

    /**
     * User System Message
     * Used to send system notifications to the UI
     */
    @Entity(
        name = "SystemMessages",
        packageName = "com.ilscipio.scipio.common",
        title = "User System Message",
        description = "Used to send system notifications to the UI",
        fields = {
            @Field(name = "messageId", type = "id-ne"),
            @Field(name = "typeId", type = "id"),
            @Field(name = "fromPartyId", type = "id"),
            @Field(name = "toPartyId", type = "id"),
            @Field(name = "title", type = "value"),
            @Field(name = "description", type = "description"),
            @Field(name = "url", type = "value", description = "URL linked to current notification"),
            @Field(name = "isRead", type = "indicator", description = "Marks the message as read by the user")
        },
        primaryKeys = {
            @PrimaryKey(field = "messageId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "SYSMSGS_FROM_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "fromPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "SYSMSGS_TO_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "toPartyId", relFieldName = "partyId")
                }
            )
        }
    )
    public interface SystemMessagesEntity {}

    @ViewEntity(
        name = "EnumTypeChildAndEnum",
        packageName = "org.ofbiz.common.enum",
        members = {
            @MemberEntity(entityAlias = "PARENT", entityName = "EnumerationType"),
            @MemberEntity(entityAlias = "CHILD", entityName = "EnumerationType"),
            @MemberEntity(entityAlias = "ENUM", entityName = "Enumeration")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PARENT", prefix = "parent"),
            @AliasAll(entityAlias = "CHILD", prefix = "child", excludes = {"parentTypeId"}),
            @AliasAll(entityAlias = "ENUM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PARENT",
                relEntityAlias = "CHILD",
                keyMaps = {
                    @KeyMap(fieldName = "enumTypeId", relFieldName = "parentTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "CHILD",
                relEntityAlias = "ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "enumTypeId")
                }
            )
        }
    )
    public interface EnumTypeChildAndEnumView {}

    /**
     * Telephone country code and country name
     */
    @ViewEntity(
        name = "CountryTeleCodeAndName",
        packageName = "org.ofbiz.common.geo",
        title = "Telephone country code and country name",
        members = {
            @MemberEntity(entityAlias = "CC", entityName = "CountryCode"),
            @MemberEntity(entityAlias = "CT", entityName = "CountryTeleCode")
        },
        aliases = {
            @Alias(name = "teleCode", entityAlias = "CT"),
            @Alias(name = "countryCode", entityAlias = "CC", primKey = "countryCodeId"),
            @Alias(name = "countryName", entityAlias = "CC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CC",
                relEntityAlias = "CT",
                keyMaps = {
                    @KeyMap(fieldName = "countryCode")
                }
            )
        }
    )
    public interface CountryTeleCodeAndNameView {}

    @ViewEntity(
        name = "GeoAssocAndGeoFrom",
        packageName = "org.ofbiz.common.geo",
        members = {
            @MemberEntity(entityAlias = "GA", entityName = "GeoAssoc"),
            @MemberEntity(entityAlias = "GFR", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GFR")
        },
        aliases = {
            @Alias(name = "geoIdTo", entityAlias = "GA"),
            @Alias(name = "geoAssocTypeId", entityAlias = "GA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GA",
                relEntityAlias = "GFR",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface GeoAssocAndGeoFromView {}

    @ViewEntity(
        name = "GeoAssocAndGeoTo",
        packageName = "org.ofbiz.common.geo",
        members = {
            @MemberEntity(entityAlias = "GA", entityName = "GeoAssoc"),
            @MemberEntity(entityAlias = "GTO", entityName = "Geo")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GTO")
        },
        aliases = {
            @Alias(name = "geoIdFrom", entityAlias = "GA", field = "geoId"),
            @Alias(name = "geoAssocTypeId", entityAlias = "GA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GA",
                relEntityAlias = "GTO",
                keyMaps = {
                    @KeyMap(fieldName = "geoIdTo", relFieldName = "geoId")
                }
            )
        }
    )
    public interface GeoAssocAndGeoToView {}

    @ViewEntity(
        name = "GeoAssocAndGeoToWithState",
        packageName = "org.ofbiz.common.geo",
        members = {
            @MemberEntity(entityAlias = "GA", entityName = "GeoAssoc"),
            @MemberEntity(entityAlias = "GTO", entityName = "Geo"),
            @MemberEntity(entityAlias = "GWS", entityName = "CountryAddressFormat")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "GTO")
        },
        aliases = {
            @Alias(name = "geoIdFrom", entityAlias = "GA", field = "geoId"),
            @Alias(name = "geoAssocTypeId", entityAlias = "GA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "GA",
                relEntityAlias = "GTO",
                keyMaps = {
                    @KeyMap(fieldName = "geoIdTo", relFieldName = "geoId")
                }
            ),
            @ViewLink(
                entityAlias = "GA",
                relEntityAlias = "GWS",
                keyMaps = {
                    @KeyMap(fieldName = "geoId")
                }
            )
        }
    )
    public interface GeoAssocAndGeoToWithStateView {}

    /**
     * Status Valid Change To Detail View
     */
    @ViewEntity(
        name = "StatusValidChangeToDetail",
        packageName = "org.ofbiz.common.status",
        title = "Status Valid Change To Detail View",
        members = {
            @MemberEntity(entityAlias = "SVC", entityName = "StatusValidChange"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SVC"),
            @AliasAll(entityAlias = "SI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SVC",
                relEntityAlias = "SI",
                keyMaps = {
                    @KeyMap(fieldName = "statusIdTo", relFieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusValidChange",
                keyMaps = {
                    @KeyMap(fieldName = "statusId"),
                    @KeyMap(fieldName = "statusIdTo")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface StatusValidChangeToDetailView {}

    /**
     * Unit Of Measure and Group/Type View
     */
    @ViewEntity(
        name = "UomAndGroup",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure and Group/Type View",
        members = {
            @MemberEntity(entityAlias = "UOMGP", entityName = "UomGroup"),
            @MemberEntity(entityAlias = "UOM", entityName = "Uom"),
            @MemberEntity(entityAlias = "UOMTP", entityName = "UomType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "UOMGP"),
            @AliasAll(entityAlias = "UOM"),
            @AliasAll(entityAlias = "UOMTP", prefix = "type")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "UOMGP",
                relEntityAlias = "UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @ViewLink(
                entityAlias = "UOM",
                relEntityAlias = "UOMTP",
                keyMaps = {
                    @KeyMap(fieldName = "uomTypeId")
                }
            )
        }
    )
    public interface UomAndGroupView {}

    /**
     * Unit Of Measure and Type View
     */
    @ViewEntity(
        name = "UomAndType",
        packageName = "org.ofbiz.common.uom",
        title = "Unit Of Measure and Type View",
        members = {
            @MemberEntity(entityAlias = "UOM", entityName = "Uom"),
            @MemberEntity(entityAlias = "UOMTP", entityName = "UomType")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "UOM"),
            @AliasAll(entityAlias = "UOMTP", prefix = "type")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "UOM",
                relEntityAlias = "UOMTP",
                keyMaps = {
                    @KeyMap(fieldName = "uomTypeId")
                }
            )
        }
    )
    public interface UomAndTypeView {}

    /**
     * PortalPage accessible via security group to a userLogin
     */
    @ViewEntity(
        name = "PortalPageAndUserLogin",
        packageName = "org.ofbiz.common.portal",
        title = "PortalPage accessible via security group to a userLogin",
        members = {
            @MemberEntity(entityAlias = "PP", entityName = "PortalPage"),
            @MemberEntity(entityAlias = "UG", entityName = "UserLoginSecurityGroup")
        },
        aliases = {
            @Alias(name = "portalPageId", entityAlias = "PP"),
            @Alias(name = "securityGroupId", entityAlias = "PP"),
            @Alias(name = "parentPortalPageId", entityAlias = "PP"),
            @Alias(name = "userLoginId", entityAlias = "UG"),
            @Alias(name = "fromDate", entityAlias = "UG"),
            @Alias(name = "thruDate", entityAlias = "UG")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PP",
                relEntityAlias = "UG",
                keyMaps = {
                    @KeyMap(fieldName = "securityGroupId", relFieldName = "groupId")
                }
            )
        }
    )
    public interface PortalPageAndUserLoginView {}

    /**
     * View entity to have all Portlet information with portalPageId 
     */
    @ViewEntity(
        name = "PortalPagePortletView",
        packageName = "org.ofbiz.common.portal",
        title = "View entity to have all Portlet information with portalPageId ",
        members = {
            @MemberEntity(entityAlias = "PPGPTLT", entityName = "PortalPagePortlet"),
            @MemberEntity(entityAlias = "PTLT", entityName = "PortalPortlet")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PPGPTLT"),
            @AliasAll(entityAlias = "PTLT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PPGPTLT",
                relEntityAlias = "PTLT",
                keyMaps = {
                    @KeyMap(fieldName = "portalPortletId")
                }
            )
        }
    )
    public interface PortalPagePortletViewView {}

    /**
     * View entity to have all Portal and Portlet information
     */
    @ViewEntity(
        name = "PortalPageAndPortlet",
        packageName = "org.ofbiz.common.portal",
        title = "View entity to have all Portal and Portlet information",
        members = {
            @MemberEntity(entityAlias = "PP", entityName = "PortalPage"),
            @MemberEntity(entityAlias = "PPP", entityName = "PortalPagePortlet")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PP"),
            @AliasAll(entityAlias = "PPP", excludes = {"sequenceNum"})
        },
        aliases = {
            @Alias(name = "portletSequenceNum", entityAlias = "PPP", field = "sequenceNum")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PP",
                relEntityAlias = "PPP",
                keyMaps = {
                    @KeyMap(fieldName = "portalPageId")
                }
            )
        }
    )
    public interface PortalPageAndPortletView {}

    @ExtendEntity(
        name = "Visit",
        fields = {
            @Field(name = "clientIpStateProvGeoId", type = "id"),
            @Field(name = "clientIpCountryGeoId", type = "id")
        }
    )
    public interface VisitExtension {}

}
