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
package com.ilscipio.scipio.webapp.entity;

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
     * Browser Type
     */
    @Entity(
        name = "BrowserType",
        packageName = "org.ofbiz.webapp.visit",
        title = "Browser Type",
        fields = {
            @Field(name = "browserTypeId", type = "id-ne"),
            @Field(name = "browserName", type = "name"),
            @Field(name = "browserVersion", type = "very-short")
        },
        primaryKeys = {
            @PrimaryKey(field = "browserTypeId")
        }
    )
    public interface BrowserTypeEntity {}

    /**
     * Platform Type
     */
    @Entity(
        name = "PlatformType",
        packageName = "org.ofbiz.webapp.visit",
        title = "Platform Type",
        fields = {
            @Field(name = "platformTypeId", type = "id-ne"),
            @Field(name = "platformName", type = "name"),
            @Field(name = "platformVersion", type = "very-short")
        },
        primaryKeys = {
            @PrimaryKey(field = "platformTypeId")
        }
    )
    public interface PlatformTypeEntity {}

    /**
     * Protocol Type
     */
    @Entity(
        name = "ProtocolType",
        packageName = "org.ofbiz.webapp.visit",
        title = "Protocol Type",
        fields = {
            @Field(name = "protocolTypeId", type = "id-ne"),
            @Field(name = "protocolName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "protocolTypeId")
        }
    )
    public interface ProtocolTypeEntity {}

    /**
     * Server Hit
     */
    @Entity(
        name = "ServerHit",
        packageName = "org.ofbiz.webapp.visit",
        title = "Server Hit",
        neverCache = true,
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "contentId", type = "id-vlong-ne"),
            @Field(name = "hitStartDateTime", type = "date-time"),
            @Field(name = "hitTypeId", type = "id-ne"),
            @Field(name = "numOfBytes", type = "numeric"),
            @Field(name = "runningTimeMillis", type = "numeric"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "requestUrl", type = "url"),
            @Field(name = "referrerUrl", type = "url"),
            @Field(name = "serverIpAddress", type = "id"),
            @Field(name = "serverHostName", type = "long-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "hitStartDateTime"),
            @PrimaryKey(field = "hitTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ServerHitType",
                fkName = "SERVER_HIT_SHTYP",
                keyMaps = {
                    @KeyMap(fieldName = "hitTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Visit",
                fkName = "SERVER_HIT_VISIT",
                keyMaps = {
                    @KeyMap(fieldName = "visitId")
                }
            )
        },
        indexes = {
            @Index(
                name = "SERVER_HIT_STRTIDX",
                fields = {
                    @IndexField(name = "hitTypeId"),
                    @IndexField(name = "hitStartDateTime")
                }
            )
        }
    )
    public interface ServerHitEntity {}

    /**
     * Server Hit Bin
     */
    @Entity(
        name = "ServerHitBin",
        packageName = "org.ofbiz.webapp.visit",
        title = "Server Hit Bin",
        neverCache = true,
        fields = {
            @Field(name = "serverHitBinId", type = "id-ne"),
            @Field(name = "contentId", type = "id-vlong-ne"),
            @Field(name = "hitTypeId", type = "id-ne"),
            @Field(name = "serverIpAddress", type = "id"),
            @Field(name = "serverHostName", type = "long-varchar"),
            @Field(name = "binStartDateTime", type = "date-time"),
            @Field(name = "binEndDateTime", type = "date-time"),
            @Field(name = "numberHits", type = "numeric"),
            @Field(name = "totalTimeMillis", type = "numeric"),
            @Field(name = "minTimeMillis", type = "numeric"),
            @Field(name = "maxTimeMillis", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "serverHitBinId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ServerHitType",
                fkName = "SERVER_HBIN_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "hitTypeId")
                }
            )
        },
        indexes = {
            @Index(
                name = "HITBIN_DATE_HITS",
                fields = {
                    @IndexField(name = "numberHits"),
                    @IndexField(name = "binStartDateTime"),
                    @IndexField(name = "binEndDateTime")
                }
            )
        }
    )
    public interface ServerHitBinEntity {}

    /**
     * Server Hit Bin
     */
    @Entity(
        name = "ServerHitType",
        packageName = "org.ofbiz.webapp.visit",
        title = "Server Hit Bin",
        defaultResourceName = "WebappEntityLabels",
        fields = {
            @Field(name = "hitTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "hitTypeId")
        }
    )
    public interface ServerHitTypeEntity {}

    /**
     * User Agent
     */
    @Entity(
        name = "UserAgent",
        packageName = "org.ofbiz.webapp.visit",
        title = "User Agent",
        fields = {
            @Field(name = "userAgentId", type = "id-ne"),
            @Field(name = "browserTypeId", type = "id"),
            @Field(name = "platformTypeId", type = "id"),
            @Field(name = "protocolTypeId", type = "id"),
            @Field(name = "userAgentTypeId", type = "id"),
            @Field(name = "userAgentMethodTypeId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "userAgentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BrowserType",
                fkName = "UAGENT_BROWSER",
                keyMaps = {
                    @KeyMap(fieldName = "browserTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PlatformType",
                fkName = "UAGENT_PLATFORM",
                keyMaps = {
                    @KeyMap(fieldName = "platformTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProtocolType",
                fkName = "UAGENT_PROTOCOL",
                keyMaps = {
                    @KeyMap(fieldName = "protocolTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserAgentType",
                fkName = "UAGENT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "userAgentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserAgentMethodType",
                fkName = "UAGENT_METHOD",
                keyMaps = {
                    @KeyMap(fieldName = "userAgentMethodTypeId")
                }
            )
        }
    )
    public interface UserAgentEntity {}

    /**
     * User Agent Method Type
     */
    @Entity(
        name = "UserAgentMethodType",
        packageName = "org.ofbiz.webapp.visit",
        title = "User Agent Method Type",
        fields = {
            @Field(name = "userAgentMethodTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "userAgentMethodTypeId")
        }
    )
    public interface UserAgentMethodTypeEntity {}

    /**
     * User Agent Type
     */
    @Entity(
        name = "UserAgentType",
        packageName = "org.ofbiz.webapp.visit",
        title = "User Agent Type",
        fields = {
            @Field(name = "userAgentTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "userAgentTypeId")
        }
    )
    public interface UserAgentTypeEntity {}

    /**
     * Visit
     */
    @Entity(
        name = "Visit",
        packageName = "org.ofbiz.webapp.visit",
        title = "Visit",
        sequenceBankSize = 100,
        neverCache = true,
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "visitorId", type = "id"),
            @Field(name = "userLoginId", type = "id-vlong"),
            @Field(name = "userCreated", type = "indicator"),
            @Field(name = "sessionId", type = "id-vlong"),
            @Field(name = "serverIpAddress", type = "id"),
            @Field(name = "serverHostName", type = "long-varchar"),
            @Field(name = "webappName", type = "short-varchar"),
            @Field(name = "initialLocale", type = "short-varchar"),
            @Field(name = "initialRequest", type = "url"),
            @Field(name = "initialReferrer", type = "url"),
            @Field(name = "initialUserAgent", type = "long-varchar"),
            @Field(name = "userAgentId", type = "id"),
            @Field(name = "clientIpAddress", type = "short-varchar"),
            @Field(name = "clientHostName", type = "long-varchar"),
            @Field(name = "clientUser", type = "short-varchar"),
            @Field(name = "clientIpIspName", type = "short-varchar"),
            @Field(name = "clientIpPostalCode", type = "short-varchar"),
            @Field(name = "cookie", type = "short-varchar"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Visitor",
                fkName = "VISIT_VISITOR",
                keyMaps = {
                    @KeyMap(fieldName = "visitorId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserAgent",
                fkName = "VISIT_USER_AGNT",
                keyMaps = {
                    @KeyMap(fieldName = "userAgentId")
                }
            )
        },
        indexes = {
            @Index(
                name = "VISIT_THRU_IDX",
                unique = true,
                fields = {
                    @IndexField(name = "thruDate"),
                    @IndexField(name = "visitId")
                }
            ),
            @Index(
                name = "VISIT_FROM_DATE",
                unique = true,
                fields = {
                    @IndexField(name = "fromDate"),
                    @IndexField(name = "visitId")
                }
            )
        }
    )
    public interface VisitEntity {}

    /**
     * Visitor
     */
    @Entity(
        name = "Visitor",
        packageName = "org.ofbiz.webapp.visit",
        title = "Visitor",
        sequenceBankSize = 100,
        fields = {
            @Field(name = "visitorId", type = "id-ne"),
            @Field(name = "userLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitorId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "VISITOR_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "userLoginId")
                }
            )
        }
    )
    public interface VisitorEntity {}

    /**
     * Web Page
     */
    @Entity(
        name = "WebPage",
        packageName = "org.ofbiz.webapp.website",
        title = "Web Page",
        fields = {
            @Field(name = "webPageId", type = "id-ne"),
            @Field(name = "pageName", type = "name"),
            @Field(name = "webSiteId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "webPageId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "WEB_PAGE_SITE",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            )
        }
    )
    public interface WebPageEntity {}

    /**
     * Web Site
     */
    @Entity(
        name = "WebSite",
        packageName = "org.ofbiz.webapp.website",
        title = "Web Site",
        fields = {
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "siteName", type = "name"),
            @Field(name = "httpHost", type = "long-varchar"),
            @Field(name = "httpPort", type = "very-short"),
            @Field(name = "httpsHost", type = "long-varchar"),
            @Field(name = "httpsPort", type = "very-short"),
            @Field(name = "enableHttps", type = "indicator"),
            @Field(name = "standardContentPrefix", type = "url"),
            @Field(name = "secureContentPrefix", type = "url"),
            @Field(name = "cookieDomain", type = "long-varchar"),
            @Field(name = "visualThemeSetId", type = "id"),
            @Field(name = "visualThemeSelectorScript", type = "long-varchar", description = "SCIPIO: Location of a script tasked with selecting a visual theme at every new render"),
            @Field(name = "visualThemeId", type = "id", description = "SCIPIO: Specific visual theme ID override; may be used to override the product store \n            visualThemeId when set (and WebSite is available)"),
            @Field(name = "webappPathPrefix", type = "url", description = "SCIPIO: URL path prefix appended between the domain/port and the webapp context path in generated links,\n            for webapp and navigation links (@pageUrl); must start with slash but end with no slash. (added 2018-07-27)"),
            @Field(name = "redirects", type = "very-long", description = "A field for redirect data"),
            @Field(name = "robots", type = "very-long", description = "A field for robots.txt data"),
            @Field(name = "prewarmcache", type = "very-long", description = "A field for prewarm cache urls, line separated")
        },
        primaryKeys = {
            @PrimaryKey(field = "webSiteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "VisualThemeSet",
                fkName = "WEB_SITE_THEME_SET",
                keyMaps = {
                    @KeyMap(fieldName = "visualThemeSetId")
                }
            )
        }
    )
    public interface WebSiteEntity {}

}
