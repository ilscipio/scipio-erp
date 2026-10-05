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
package com.ilscipio.scipio.webtools.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.webtools.UtilCacheEvents;
import org.ofbiz.webapp.event.CoreEvents;
import org.ofbiz.webtools.print.FoPrintServerEvents;
import org.ofbiz.webtools.artifactinfo.RunTestEvents;
import org.ofbiz.service.engine.HttpEngine;
import org.ofbiz.webtools.GenericWebEvent;
import com.ilscipio.scipio.webtools.TargetedRenderingTestEvents;
import org.ofbiz.webapp.event.TestEvent;
import com.ilscipio.scipio.webtools.event.ServiceValidationEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://webtools/widget/CommonScreens.xml#main",
        controller = "webtools"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ping",
        type = "ftl",
        page = "component://webtools/webapp/webtools/ping.ftl",
        controller = "webtools"
    )
    public static final String VIEW_PING = "ping";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showDateTime",
        type = "ftl",
        page = "component://webtools/webapp/webtools/showDateTime.ftl",
        controller = "webtools"
    )
    public static final String VIEW_SHOWDATETIME = "showDateTime";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "entityref",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#EntityRef",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYREF = "entityref";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "entityref_list",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#EntityRefList",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYREF_LIST = "entityref_list";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "entityref_main",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#EntityRefMain",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYREF_MAIN = "entityref_main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "entityrefReport",
        type = "screenfop",
        page = "component://webtools/widget/EntityScreens.xml#EntityRefReport",
        contentType = "application/pdf",
        encoding = "none",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYREFREPORT = "entityrefReport";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "entitymaint",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#EntityMaint",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYMAINT = "entitymaint";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindGeneric",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#FindGeneric",
        controller = "webtools"
    )
    public static final String VIEW_FINDGENERIC = "FindGeneric";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewGeneric",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#ViewGeneric",
        controller = "webtools"
    )
    public static final String VIEW_VIEWGENERIC = "ViewGeneric";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ViewRelations",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#ViewRelations",
        controller = "webtools"
    )
    public static final String VIEW_VIEWRELATIONS = "ViewRelations";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "tablesMySql",
        page = "/entity/tablesMySql.jsp",
        controller = "webtools"
    )
    public static final String VIEW_TABLESMYSQL = "tablesMySql";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "droptablesMySql",
        page = "/entity/droptablesMySql.jsp",
        controller = "webtools"
    )
    public static final String VIEW_DROPTABLESMYSQL = "droptablesMySql";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "droptablesplain",
        page = "/entity/droptablesplain.jsp",
        controller = "webtools"
    )
    public static final String VIEW_DROPTABLESPLAIN = "droptablesplain";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "dataMySql",
        page = "/entity/dataMySql.jsp",
        controller = "webtools"
    )
    public static final String VIEW_DATAMYSQL = "dataMySql";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ModelWriter",
        page = "/entity/ModelWriter.jsp",
        controller = "webtools"
    )
    public static final String VIEW_MODELWRITER = "ModelWriter";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ModelGroupWriter",
        page = "/entity/ModelGroupWriter.jsp",
        controller = "webtools"
    )
    public static final String VIEW_MODELGROUPWRITER = "ModelGroupWriter";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ModelInduceFromDb",
        page = "/entity/ModelInduceFromDb.jsp",
        controller = "webtools"
    )
    public static final String VIEW_MODELINDUCEFROMDB = "ModelInduceFromDb";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EntityEoModelBundle",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#EntityEoModelBundle",
        controller = "webtools"
    )
    public static final String VIEW_ENTITYEOMODELBUNDLE = "EntityEoModelBundle";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "checkdb",
        type = "screen",
        page = "component://webtools/widget/EntityScreens.xml#CheckDb",
        controller = "webtools"
    )
    public static final String VIEW_CHECKDB = "checkdb";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "xmldsdump",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#xmldsdump",
            controller = "webtools"
        )
        public static final String VIEW_XMLDSDUMP = "xmldsdump";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "xmldsrawdump",
            page = "/entity/xmldsrawdump.jsp",
            controller = "webtools"
        )
        public static final String VIEW_XMLDSRAWDUMP = "xmldsrawdump";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindUtilCache",
            type = "screen",
            page = "component://webtools/widget/CacheScreens.xml#FindUtilCache",
            controller = "webtools"
        )
        public static final String VIEW_FINDUTILCACHE = "FindUtilCache";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindUtilCacheElements",
            type = "screen",
            page = "component://webtools/widget/CacheScreens.xml#FindUtilCacheElements",
            controller = "webtools"
        )
        public static final String VIEW_FINDUTILCACHEELEMENTS = "FindUtilCacheElements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditUtilCache",
            type = "screen",
            page = "component://webtools/widget/CacheScreens.xml#EditUtilCache",
            controller = "webtools"
        )
        public static final String VIEW_EDITUTILCACHE = "EditUtilCache";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPrewarmCacheUrls",
            type = "screen",
            page = "component://webtools/widget/CacheScreens.xml#EditPrewarmCacheUrls",
            controller = "webtools"
        )
        public static final String VIEW_EDITPREWARMCACHEURLS = "EditPrewarmCacheUrls";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "viewdatafile",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#viewdatafile",
            controller = "webtools"
        )
        public static final String VIEW_VIEWDATAFILE = "viewdatafile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LogView",
            type = "screen",
            page = "component://webtools/widget/LogScreens.xml#LogView",
            controller = "webtools"
        )
        public static final String VIEW_LOGVIEW = "LogView";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "StatsSinceStart",
            type = "screen",
            page = "component://webtools/widget/StatsScreens.xml#StatsSinceStart",
            controller = "webtools"
        )
        public static final String VIEW_STATSSINCESTART = "StatsSinceStart";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "StatBinsHistory",
            type = "screen",
            page = "component://webtools/widget/StatsScreens.xml#StatBinsHistory",
            controller = "webtools"
        )
        public static final String VIEW_STATBINSHISTORY = "StatBinsHistory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewMetrics",
            type = "screen",
            page = "component://webtools/widget/StatsScreens.xml#ViewMetrics",
            controller = "webtools"
        )
        public static final String VIEW_VIEWMETRICS = "ViewMetrics";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityPerformanceTest",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityPerformanceTest",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYPERFORMANCETEST = "EntityPerformanceTest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ServiceLog",
            type = "screen",
            page = "component://webtools/widget/LogScreens.xml#ServiceLog",
            controller = "webtools"
        )
        public static final String VIEW_SERVICELOG = "ServiceLog";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ServiceList",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#ServiceList",
            controller = "webtools"
        )
        public static final String VIEW_SERVICELIST = "ServiceList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindJob",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#FindJob",
            controller = "webtools"
        )
        public static final String VIEW_FINDJOB = "FindJob";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "JobDetails",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#JobDetails",
            controller = "webtools"
        )
        public static final String VIEW_JOBDETAILS = "JobDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "serviceResult",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#ServiceResult",
            controller = "webtools"
        )
        public static final String VIEW_SERVICERESULT = "serviceResult";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "threadList",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#ThreadList",
            controller = "webtools"
        )
        public static final String VIEW_THREADLIST = "threadList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "scheduleJob",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#ScheduleJob",
            controller = "webtools"
        )
        public static final String VIEW_SCHEDULEJOB = "scheduleJob";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "runService",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#RunService",
            controller = "webtools"
        )
        public static final String VIEW_RUNSERVICE = "runService";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "setServiceParameters",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#setServiceParameters",
            controller = "webtools"
        )
        public static final String VIEW_SETSERVICEPARAMETERS = "setServiceParameters";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "setSyncServiceParameters",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#setSyncServiceParameters",
            controller = "webtools"
        )
        public static final String VIEW_SETSYNCSERVICEPARAMETERS = "setSyncServiceParameters";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "serviceEcaDetail",
            type = "screen",
            page = "component://webtools/widget/AvailableServicesScreens.xml#ServiceEcaDetail",
            controller = "webtools"
        )
        public static final String VIEW_SERVICEECADETAIL = "serviceEcaDetail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddJobManagerLock",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#AddJobManagerLock",
            controller = "webtools"
        )
        public static final String VIEW_ADDJOBMANAGERLOCK = "AddJobManagerLock";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindJobManagerLock",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#FindJobManagerLock",
            controller = "webtools"
        )
        public static final String VIEW_FINDJOBMANAGERLOCK = "FindJobManagerLock";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "JobStats",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#JobStats",
            controller = "webtools"
        )
        public static final String VIEW_JOBSTATS = "JobStats";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "currentJobStats",
            type = "screen",
            page = "component://webtools/widget/ServiceScreens.xml#currentJobStats",
            controller = "webtools"
        )
        public static final String VIEW_CURRENTJOBSTATS = "currentJobStats";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "printStart",
            type = "screen",
            page = "component://webtools/widget/CommonScreens.xml#printStart",
            controller = "webtools"
        )
        public static final String VIEW_PRINTSTART = "printStart";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "printDone",
            type = "screen",
            page = "component://webtools/widget/CommonScreens.xml#printDone",
            controller = "webtools"
        )
        public static final String VIEW_PRINTDONE = "printDone";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntitySyncStatus",
            type = "screen",
            page = "component://webtools/widget/EntitySyncScreens.xml#EntitySyncStatus",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYSYNCSTATUS = "EntitySyncStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntitySQLProcessor",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntitySQLProcessor",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYSQLPROCESSOR = "EntitySQLProcessor";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ConnectionPoolStatus",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#ConnectionPoolStatus",
            controller = "webtools"
        )
        public static final String VIEW_CONNECTIONPOOLSTATUS = "ConnectionPoolStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityExportAll",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityExportAll",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYEXPORTALL = "EntityExportAll";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProgramExport",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#ProgramExport",
            controller = "webtools"
        )
        public static final String VIEW_PROGRAMEXPORT = "ProgramExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityImportDir",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityImportDir",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYIMPORTDIR = "EntityImportDir";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityImport",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityImport",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYIMPORT = "EntityImport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityImportReaders",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityImportReaders",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYIMPORTREADERS = "EntityImportReaders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "viewbrowsercerts",
            type = "screen",
            page = "component://webtools/widget/CommonScreens.xml#browsercerts",
            controller = "webtools"
        )
        public static final String VIEW_VIEWBROWSERCERTS = "viewbrowsercerts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewComponents",
            type = "screen",
            page = "component://webtools/widget/ArtifactInfoScreens.xml#ViewComponents",
            controller = "webtools"
        )
        public static final String VIEW_VIEWCOMPONENTS = "ViewComponents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewComponent",
            type = "screen",
            page = "component://webtools/widget/ArtifactInfoScreens.xml#ViewComponent",
            controller = "webtools"
        )
        public static final String VIEW_VIEWCOMPONENT = "ViewComponent";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TestSuiteInfo",
            type = "screen",
            page = "component://webtools/widget/ArtifactInfoScreens.xml#TestSuiteInfo",
            controller = "webtools"
        )
        public static final String VIEW_TESTSUITEINFO = "TestSuiteInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ArtifactInfo",
            type = "screen",
            page = "component://webtools/widget/ArtifactInfoScreens.xml#ArtifactInfo",
            controller = "webtools"
        )
        public static final String VIEW_ARTIFACTINFO = "ArtifactInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SearchLabels",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#SearchLabels",
            controller = "webtools"
        )
        public static final String VIEW_SEARCHLABELS = "SearchLabels";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "UpdateLabel",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#UpdateLabel",
            controller = "webtools"
        )
        public static final String VIEW_UPDATELABEL = "UpdateLabel";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewReferences",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#ViewReferences",
            controller = "webtools"
        )
        public static final String VIEW_VIEWREFERENCES = "ViewReferences";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewFile",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#ViewFile",
            controller = "webtools"
        )
        public static final String VIEW_VIEWFILE = "ViewFile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityLabels",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#EntityLabels",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYLABELS = "EntityLabels";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SystemProperties",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#SystemProperties",
            controller = "webtools"
        )
        public static final String VIEW_SYSTEMPROPERTIES = "SystemProperties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupLabelNames",
            type = "screen",
            page = "component://webtools/widget/LabelManagerScreens.xml#LookupLabelNames",
            controller = "webtools"
        )
        public static final String VIEW_LOOKUPLABELNAMES = "LookupLabelNames";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editTemporalExpression",
            type = "screen",
            page = "component://webtools/widget/TempExprScreens.xml#EditTemporalExpression",
            controller = "webtools"
        )
        public static final String VIEW_EDITTEMPORALEXPRESSION = "editTemporalExpression";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findTemporalExpression",
            type = "screen",
            page = "component://webtools/widget/TempExprScreens.xml#FindTemporalExpression",
            controller = "webtools"
        )
        public static final String VIEW_FINDTEMPORALEXPRESSION = "findTemporalExpression";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGeo",
            type = "screen",
            page = "component://webtools/widget/GeoManagementScreens.xml#FindGeo",
            controller = "webtools"
        )
        public static final String VIEW_FINDGEO = "FindGeo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditGeo",
            type = "screen",
            page = "component://webtools/widget/GeoManagementScreens.xml#EditGeo",
            controller = "webtools"
        )
        public static final String VIEW_EDITGEO = "EditGeo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LinkGeos",
            type = "screen",
            page = "component://webtools/widget/GeoManagementScreens.xml#LinkGeos",
            controller = "webtools"
        )
        public static final String VIEW_LINKGEOS = "LinkGeos";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupGeo",
            type = "screen",
            page = "component://webtools/widget/GeoManagementScreens.xml#LookupGeo",
            controller = "webtools"
        )
        public static final String VIEW_LOOKUPGEO = "LookupGeo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WebtoolsLayoutDemo",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#WebtoolsLayoutDemo",
            controller = "webtools"
        )
        public static final String VIEW_WEBTOOLSLAYOUTDEMO = "WebtoolsLayoutDemo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TargetedRenderingTest",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#TargetedRenderingTest",
            controller = "webtools"
        )
        public static final String VIEW_TARGETEDRENDERINGTEST = "TargetedRenderingTest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TemplateTest",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#TemplateTest",
            controller = "webtools"
        )
        public static final String VIEW_TEMPLATETEST = "TemplateTest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EntityUtilityServices",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#EntityUtilityServices",
            controller = "webtools"
        )
        public static final String VIEW_ENTITYUTILITYSERVICES = "EntityUtilityServices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListDemoDataGeneratorServices",
            type = "screen",
            page = "component://webtools/widget/DemoDataGeneratorScreens.xml#ListDemoDataGeneratorServices",
            controller = "webtools"
        )
        public static final String VIEW_LISTDEMODATAGENERATORSERVICES = "ListDemoDataGeneratorServices";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RunDemoDataGeneratorService",
            type = "screen",
            page = "component://webtools/widget/DemoDataGeneratorScreens.xml#RunDemoDataGeneratorService",
            controller = "webtools"
        )
        public static final String VIEW_RUNDEMODATAGENERATORSERVICE = "RunDemoDataGeneratorService";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "DemoDataGeneratorResult",
            type = "screen",
            page = "component://webtools/widget/DemoDataGeneratorScreens.xml#DemoDataGeneratorResult",
            controller = "webtools"
        )
        public static final String VIEW_DEMODATAGENERATORRESULT = "DemoDataGeneratorResult";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListDemoDataGeneratorProviders",
            type = "screen",
            page = "component://webtools/widget/DemoDataGeneratorScreens.xml#ListDemoDataGeneratorProviders",
            controller = "webtools"
        )
        public static final String VIEW_LISTDEMODATAGENERATORPROVIDERS = "ListDemoDataGeneratorProviders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "DeveloperDocIndex",
            type = "screen",
            page = "component://webtools/widget/DocScreens.xml#DeveloperDocIndex",
            controller = "webtools"
        )
        public static final String VIEW_DEVELOPERDOCINDEX = "DeveloperDocIndex";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TemplateApiDocPage",
            type = "screen",
            page = "component://webtools/widget/DocScreens.xml#TemplateApiDocPage",
            controller = "webtools"
        )
        public static final String VIEW_TEMPLATEAPIDOCPAGE = "TemplateApiDocPage";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SolrStatus",
            type = "screen",
            page = "component://webtools/widget/SolrScreens.xml#SolrStatus",
            controller = "webtools"
        )
        public static final String VIEW_SOLRSTATUS = "SolrStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SolrServices",
            type = "screen",
            page = "component://webtools/widget/SolrScreens.xml#SolrServices",
            controller = "webtools"
        )
        public static final String VIEW_SOLRSERVICES = "SolrServices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SolrDoc",
            type = "screen",
            page = "component://webtools/widget/SolrScreens.xml#SolrDoc",
            controller = "webtools"
        )
        public static final String VIEW_SOLRDOC = "SolrDoc";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "hotwireStaticComments",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#hotwireStaticComments",
            controller = "webtools"
        )
        public static final String VIEW_HOTWIRESTATICCOMMENTS = "hotwireStaticComments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "hotwireEmptyView",
            type = "screen",
            page = "component://webtools/widget/MiscScreens.xml#hotwireEmptyView",
            controller = "webtools"
        )
        public static final String VIEW_HOTWIREEMPTYVIEW = "hotwireEmptyView";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ExcelImport",
            type = "screen",
            page = "component://webtools/widget/EntityScreens.xml#ExcelImport",
            controller = "webtools"
        )
        public static final String VIEW_EXCELIMPORT = "ExcelImport";

        @Request(
            uri = "httpService",
            controller = "webtools"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "none")
        public static String httpService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.service.engine.HttpEngine.httpEngine
            return HttpEngine.httpEngine(request, response);
        }

        @Request(
            uri = "SOAPService",
            controller = "webtools"
        )
        @Response(name = "error", type = "none")
        @Response(name = "success", type = "none")
        @Event(type = "soap")
        public static String sOAPService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ping",
            controller = "webtools"
        )
        @Response(name = "error", type = "view", value = "ping")
        @Response(name = "success", type = "view", value = "ping")
        @Event(type = "service", invoke = "ping")
        public static String ping(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "showDateTime",
            controller = "webtools"
        )
        @Response(name = "success", type = "view", value = "showDateTime")
        public interface ShowDateTime {}

        @Request(
            uri = "secureCertDateTime",
            controller = "webtools",
            secure = "true",
            cert = "true"
        )
        @Response(name = "success", type = "view", value = "showDateTime")
        public interface SecureCertDateTime {}

        @Request(
            uri = "secureAuthDateTime",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showDateTime")
        public interface SecureAuthDateTime {}

        @Request(
            uri = "TestService",
            controller = "webtools"
        )
        @Response(name = "error", type = "view", value = "error")
        @Response(name = "success", type = "view", value = "error")
        @Event(type = "service", invoke = "testScv")
        public static String testService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "streamTest",
            controller = "webtools"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "none")
        @Event(type = "service-stream", invoke = "serviceStreamTest")
        public static String streamTest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "yahoo",
            controller = "webtools"
        )
        @Response(name = "success", type = "url", value = "http://www.yahoo.com")
        public interface Yahoo {}

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "view",
            controller = "webtools",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface View {}

        @Request(
            uri = "chain",
            controller = "webtools"
        )
        @Response(name = "success", type = "request", value = "/view")
        @Response(name = "error", type = "view", value = "error")
        public static String chain(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.TestEvent.test
            return TestEvent.test(request, response);
        }

        @Request(
            uri = "main",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "entitymaint",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "entitymaint")
        public interface Entitymaint {}

        @Request(
            uri = "FindGeneric",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGeneric")
        public interface FindGeneric {}

        @Request(
            uri = "ViewGeneric",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGeneric")
        public interface ViewGeneric {}

        @Request(
            uri = "UpdateGeneric",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGeneric")
        @Response(name = "error", type = "view", value = "ViewGeneric")
        public static String updateGeneric(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.GenericWebEvent.updateGeneric
            return GenericWebEvent.updateGeneric(request, response);
        }

        @Request(
            uri = "ViewRelations",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRelations")
        public interface ViewRelations {}

        @Request(
            uri = "entityref",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "entityref")
        public interface Entityref {}

        @Request(
            uri = "entityrefReport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "entityrefReport")
        public interface EntityrefReport {}

        @Request(
            uri = "ModelWriter",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ModelWriter")
        public interface ModelWriter {}

        @Request(
            uri = "ModelGroupWriter",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ModelGroupWriter")
        public interface ModelGroupWriter {}

        @Request(
            uri = "EntityEoModelBundle",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityEoModelBundle")
        public interface EntityEoModelBundle {}

        @Request(
            uri = "exportEntityEoModelBundle",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityEoModelBundle")
        @Response(name = "error", type = "view", value = "EntityEoModelBundle")
        @Event(type = "service", invoke = "exportEntityEoModelBundle")
        public static String exportEntityEoModelBundle(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindUtilCache",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCache")
        public interface FindUtilCache {}

        @Request(
            uri = "FindUtilCacheClear",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCache")
        @Response(name = "error", type = "view", value = "FindUtilCache")
        public static String findUtilCacheClear(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.UtilCacheEvents.clearEvent
            return UtilCacheEvents.clearEvent(request, response);
        }

        @Request(
            uri = "FindUtilCacheClearAll",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCache")
        @Response(name = "error", type = "view", value = "FindUtilCache")
        public static String findUtilCacheClearAll(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.UtilCacheEvents.clearAllEvent
            return UtilCacheEvents.clearAllEvent(request, response);
        }

        @Request(
            uri = "ForceGarbageCollection",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCache")
        @Response(name = "error", type = "view", value = "FindUtilCache")
        @Event(type = "service", invoke = "forceGarbageCollection")
        public static String forceGarbageCollection(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditUtilCache",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUtilCache")
        public interface EditUtilCache {}

        @Request(
            uri = "EditUtilCacheUpdate",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUtilCache")
        @Response(name = "error", type = "view", value = "EditUtilCache")
        public static String editUtilCacheUpdate(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.UtilCacheEvents.updateEvent
            return UtilCacheEvents.updateEvent(request, response);
        }

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "EditPrewarmCacheUrls",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPrewarmCacheUrls")
        public interface EditPrewarmCacheUrls {}

        @Request(
            uri = "UpdatePrewarmCacheUrls",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPrewarmCacheUrls")
        @Response(name = "error", type = "view", value = "EditPrewarmCacheUrls")
        @Event(type = "service", invoke = "updateWebSite")
        public static String updatePrewarmCacheUrls(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PrewarmCache",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPrewarmCacheUrls")
        @Response(name = "error", type = "view", value = "EditPrewarmCacheUrls")
        @Event(type = "service", invoke = "prewarmContentCacheFromDb")
        public static String prewarmCache(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditUtilCacheClear",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditUtilCache")
        @Response(name = "error", type = "request-redirect", value = "EditUtilCache")
        public static String editUtilCacheClear(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.UtilCacheEvents.clearEvent
            return UtilCacheEvents.clearEvent(request, response);
        }

        @Request(
            uri = "FindUtilCacheElements",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCacheElements")
        public interface FindUtilCacheElements {}

        @Request(
            uri = "FindUtilCacheElementsRemoveElement",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUtilCacheElements")
        @Response(name = "error", type = "view", value = "FindUtilCacheElements")
        public static String findUtilCacheElementsRemoveElement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.UtilCacheEvents.removeElementEvent
            return UtilCacheEvents.removeElementEvent(request, response);
        }

        @Request(
            uri = "viewdatafile",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewdatafile")
        public interface Viewdatafile {}

        @Request(
            uri = "StatsSinceStart",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "StatsSinceStart")
        public interface StatsSinceStart {}

        @Request(
            uri = "StatBinsHistory",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "StatBinsHistory")
        public interface StatBinsHistory {}

        @Request(
            uri = "ViewMetrics",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewMetrics")
        public interface ViewMetrics {}

        @Request(
            uri = "ResetMetric",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewMetrics")
        @Response(name = "error", type = "view", value = "ViewMetrics")
        @Event(type = "service", invoke = "resetMetric")
        public static String resetMetric(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AdjustDebugLevels",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LogView")
        @Event(type = "service", invoke = "adjustDebugLevels")
        public static String adjustDebugLevels(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LogView",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LogView")
        public interface LogView {}

        @Request(
            uri = "ServiceLog",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ServiceLog")
        public interface ServiceLog {}

        @Request(
            uri = "ServiceList",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ServiceList")
        public interface ServiceList {}

        @Request(
            uri = "threadList",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "threadList")
        public interface ThreadList {}

        @Request(
            uri = "FindJob",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJob")
        public interface FindJob {}

        @Request(
            uri = "JobDetails",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "JobDetails")
        public interface JobDetails {}

        @Request(
            uri = "cancelJob",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJob")
        @Response(name = "error", type = "view", value = "FindJob")
        @Event(type = "service", invoke = "cancelScheduledJob")
        public static String cancelJob(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "resetJob",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJob")
        @Response(name = "error", type = "view", value = "FindJob")
        @Event(type = "service", invoke = "resetScheduledJob")
        public static String resetJob(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "scheduleJob",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "scheduleJob")
        public interface ScheduleJob {}

        @Request(
            uri = "runService",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "runService")
        public interface RunService {}

        @Request(
            uri = "setServiceParameters",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "setServiceParameters")
        public interface SetServiceParameters {}

        @Request(
            uri = "setSyncServiceParameters",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "setSyncServiceParameters")
        public interface SetSyncServiceParameters {}

        @Request(
            uri = "scheduleService",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJob")
        @Response(name = "sync_success", type = "view", value = "serviceResult")
        @Response(name = "error", type = "view", value = "scheduleJob")
        public static String scheduleService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.scheduleService
            return CoreEvents.scheduleService(request, response);
        }

        @Request(
            uri = "scheduleServiceSync",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "serviceResult")
        @Response(name = "sync_success", type = "view", value = "serviceResult")
        @Response(name = "error", type = "view", value = "runService")
        public static String scheduleServiceSync(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.scheduleService
            return CoreEvents.scheduleService(request, response);
        }

        @Request(
            uri = "serviceResult",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "serviceResult")
        public interface ServiceResult {}

        @Request(
            uri = "saveServiceResultsToSession",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "runService")
        @Response(name = "error", type = "view", value = "error")
        public static String saveServiceResultsToSession(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.saveServiceResultsToSession
            return CoreEvents.saveServiceResultsToSession(request, response);
        }

        @Request(
            uri = "AddJobManagerLock",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddJobManagerLock")
        public interface AddJobManagerLock {}

        @Request(
            uri = "FindJobManagerLock",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobManagerLock")
        public interface FindJobManagerLock {}

        @Request(
            uri = "createJobManagerLock",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createJobManagerLock")
        public static String createJobManagerLock(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateJobManagerLock",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobManagerLock")
        @Response(name = "error", type = "view", value = "FindJobManagerLock")
        @Event(type = "service", invoke = "updateJobManagerLock")
        public static String updateJobManagerLock(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "JobStats",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "JobStats")
        public interface JobStats {}

        @Request(
            uri = "currentJobStatsJson",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "groovy")
        public static String currentJobStatsJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "globalJobStatsJson",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "groovy")
        public static String globalJobStatsJson(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "currentJobStats",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "currentJobStats")
        public interface CurrentJobStats {}

        @Request(
            uri = "clearGlobalJobStats",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "JobStats")
        @Event(type = "groovy")
        public static String clearGlobalJobStats(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "serviceEcaDetail",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "serviceEcaDetail")
        public interface ServiceEcaDetail {}

        @Request(
            uri = "exportServiceEoModelBundle",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ServiceList")
        @Response(name = "error", type = "view", value = "ServiceList")
        @Event(type = "service", invoke = "exportServiceEoModelBundle")
        public static String exportServiceEoModelBundle(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EntityPerformanceTest",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityPerformanceTest")
        public interface EntityPerformanceTest {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "ViewComponents",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewComponents")
        public interface ViewComponents {}

        @Request(
            uri = "ViewComponent",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewComponent")
        public interface ViewComponent {}

        @Request(
            uri = "ajaxDashboardCurrentRequest",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getServerRequestsThisHour")
        public static String ajaxDashboardCurrentRequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "TestSuiteInfo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TestSuiteInfo")
        public interface TestSuiteInfo {}

        @Request(
            uri = "RunTest",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "TestSuiteInfo")
        public static String runTest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.artifactinfo.RunTestEvents.runTest
            return RunTestEvents.runTest(request, response);
        }

        @Request(
            uri = "ValidateServices",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ServiceList")
        @Response(name = "error", type = "view", value = "ServiceList")
        public static String validateServices(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: com.ilscipio.scipio.webtools.event.ServiceValidationEvents.validateAllServices
            return ServiceValidationEvents.validateAllServices(request, response);
        }

        @Request(
            uri = "EntitySQLProcessor",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySQLProcessor")
        public interface EntitySQLProcessor {}

        @Request(
            uri = "ConnectionPoolStatus",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ConnectionPoolStatus")
        public interface ConnectionPoolStatus {}

        @Request(
            uri = "ProgramExport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProgramExport")
        @Response(name = "error", type = "view", value = "ProgramExport")
        public interface ProgramExport {}

        @Request(
            uri = "EntityExportAll",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityExportAll")
        @Response(name = "error", type = "view", value = "EntityExportAll")
        public interface EntityExportAll {}

        @Request(
            uri = "entityExportAll",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityExportAll")
        @Response(name = "error", type = "view", value = "EntityExportAll")
        @Event(type = "service", invoke = "entityExportAll")
        public static String entityExportAll_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EntityImportDir",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImportDir")
        public interface EntityImportDir {}

        @Request(
            uri = "entityImportDir",
            controller = "webtools",
            method = "post",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImportDir")
        @Response(name = "error", type = "view", value = "EntityImportDir")
        @Event(type = "service", invoke = "entityImportDir")
        public static String entityImportDir_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EntityImport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImport")
        public interface EntityImport {}

        @Request(
            uri = "entityImport",
            controller = "webtools",
            method = "post",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImport")
        @Response(name = "error", type = "view", value = "EntityImport")
        @Event(type = "service", invoke = "entityImport")
        public static String entityImport_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EntityImportReaders",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImportReaders")
        public interface EntityImportReaders {}

        @Request(
            uri = "entityImportReaders",
            controller = "webtools",
            method = "post",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityImportReaders")
        @Response(name = "error", type = "view", value = "EntityImportReaders")
        @Event(type = "service", invoke = "entityImportReaders")
        public static String entityImportReaders_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "xmldsdump",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "xmldsdump")
        public interface Xmldsdump {}

        @Request(
            uri = "xmldsrawdump",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "xmldsrawdump")
        public interface Xmldsrawdump {}

        @Request(
            uri = "deleteEntityExport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "xmldsdump")
        @Response(name = "error", type = "view", value = "xmldsdump")
        @Event(type = "service", invoke = "deleteEntityExport")
        public static String deleteEntityExport(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "EntitySyncStatus",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySyncStatus")
        public interface EntitySyncStatus {}

        @Request(
            uri = "resetEntitySyncStatusToNotStarted",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySyncStatus")
        @Response(name = "error", type = "view", value = "EntitySyncStatus")
        @Event(type = "service", invoke = "resetEntitySyncStatusToNotStarted")
        public static String resetEntitySyncStatusToNotStarted(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "runOfflineEntitySync",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySyncStatus")
        @Response(name = "error", type = "view", value = "EntitySyncStatus")
        @Event(type = "service", path = "ASYNC", invoke = "runOfflineEntitySync")
        public static String runOfflineEntitySync(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOfflineEntitySync",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySyncStatus")
        @Response(name = "error", type = "view", value = "EntitySyncStatus")
        @Event(type = "service", invoke = "updateOfflineEntitySync")
        public static String updateOfflineEntitySync(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "loadOfflineEntitySyncData",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntitySyncStatus")
        @Response(name = "error", type = "view", value = "EntitySyncStatus")
        @Event(type = "service", invoke = "loadOfflineEntitySyncData")
        public static String loadOfflineEntitySyncData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ArtifactInfo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ArtifactInfo")
        public interface ArtifactInfo {}

        @Request(
            uri = "FileLabels",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SearchLabels")
        public interface FileLabels {}

        @Request(
            uri = "SearchLabels",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SearchLabels")
        public interface SearchLabels {}

        @Request(
            uri = "SaveLabelsToXmlFile",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SearchLabels")
        @Event(type = "service", invoke = "saveLabelsToXmlFile")
        public static String saveLabelsToXmlFile(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateLabel",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateLabel")
        public interface UpdateLabel {}

        @Request(
            uri = "EntityLabels",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityLabels")
        public interface EntityLabels {}

        @Request(
            uri = "SystemProperties",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SystemProperties")
        public interface SystemProperties {}

        @Request(
            uri = "updateLocalizedPropertyMulti",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearLocalizedPropertyCaches")
        @Response(name = "error", type = "view", value = "EntityLabels")
        @Event(type = "service-multi", invoke = "updateLocalizedPropertyOptional")
        public static String updateLocalizedPropertyMulti(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "clearLocalizedPropertyCaches",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EntityLabels")
        @Response(name = "error", type = "view", value = "EntityLabels")
        @Event(type = "service", invoke = "clearLocalizedPropertyCaches")
        public static String clearLocalizedPropertyCaches(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getLocalizedPropertyValues",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getLocalizedPropertyValues")
        public static String getLocalizedPropertyValues(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "SystemProperties",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SystemProperties")
        public interface SystemProperties1 {}

        @Request(
            uri = "ViewReferences",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewReferences")
        @Response(name = "error", type = "view", value = "ViewReferences")
        public interface ViewReferences {}

        @Request(
            uri = "ViewFile",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFile")
        @Response(name = "error", type = "view", value = "ViewFile")
        public interface ViewFile {}

        @Request(
            uri = "myCertificates",
            controller = "webtools",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "viewbrowsercerts")
        public interface MyCertificates {}


    }

    // Auto-generated split (Part 11)
    public static class Part11 {


        @Request(
            uri = "getXslFo",
            controller = "webtools"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "none")
        public static String getXslFo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webtools.print.FoPrintServerEvents.getXslFo
            return FoPrintServerEvents.getXslFo(request, response);
        }






        @Request(
            uri = "editTemporalExpression",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editTemporalExpression")
        public interface EditTemporalExpression {}

        @Request(
            uri = "findTemporalExpression",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findTemporalExpression")
        public interface FindTemporalExpression {}

        @Request(
            uri = "FindGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGeo", saveHomeView = "true")
        public interface FindGeo {}

        @Request(
            uri = "EditGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGeo")
        public interface EditGeo {}

        @Request(
            uri = "LinkGeos",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LinkGeos")
        public interface LinkGeos {}

        @Request(
            uri = "LookupGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupGeo")
        public interface LookupGeo {}

        @Request(
            uri = "createGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGeo")
        @Response(name = "error", type = "view", value = "EditGeo")
        @Event(type = "service", invoke = "createGeo")
        public static String createGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGeo")
        @Response(name = "error", type = "view", value = "EditGeo")
        @Event(type = "service", invoke = "updateGeo")
        public static String updateGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteGeo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "FindGeo")
        @Response(name = "error", type = "view", value = "FindGeo")
        @Event(type = "service", invoke = "deleteGeo")
        public static String deleteGeo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "linkGeos",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LinkGeos")
        @Response(name = "error", type = "view", value = "LinkGeos")
        @Event(type = "service", invoke = "linkGeos")
        public static String linkGeos_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "security",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindUserLogin")
        public interface Security {}

        @Request(
            uri = "WebtoolsLayoutDemo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "LayoutDemo")
        public interface WebtoolsLayoutDemo {}

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "LayoutDemo",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebtoolsLayoutDemo")
        public interface LayoutDemo {}

        @Request(
            uri = "TargetedRenderingTest",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TargetedRenderingTest")
        @Response(name = "error", type = "view", value = "TargetedRenderingTest")
        public static String targetedRenderingTest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: com.ilscipio.scipio.webtools.TargetedRenderingTestEvents.testEvent
            return TargetedRenderingTestEvents.testEvent(request, response);
        }

        @Request(
            uri = "TemplateTest",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TemplateTest")
        public interface TemplateTest {}

        @Request(
            uri = "sendExamplePushNotifications",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "sendExamplePushNotifications")
        public static String sendExamplePushNotifications(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "runExampleCtrlInlineGroovy",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "WebtoolsLayoutDemo")
        @Response(name = "error", type = "request", value = "WebtoolsLayoutDemo")
        @Event(type = "groovy")
        public static String runExampleCtrlInlineGroovy(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "testAdminService",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WebtoolsLayoutDemo")
        @Event(type = "service-multi", invoke = "testAdminService")
        public static String testAdminService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "hotwireStaticCommentsAdd",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "hotwireStaticComments")
        @Event(type = "groovy")
        public static String hotwireStaticCommentsAdd(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "hotwireStaticCommentsRemove",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "hotwireStaticComments")
        @Event(type = "groovy")
        public static String hotwireStaticCommentsRemove(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "hotwireStreamCommentsAdd",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Event(type = "groovy")
        public static String hotwireStreamCommentsAdd(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EntityUtilityServices",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EntityUtilityServices")
        public interface EntityUtilityServices {}

        @Request(
            uri = "ListDemoDataGeneratorServices",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListDemoDataGeneratorServices")
        public interface ListDemoDataGeneratorServices {}

        @Request(
            uri = "RunDemoDataGeneratorService",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RunDemoDataGeneratorService")
        public interface RunDemoDataGeneratorService {}

        @Request(
            uri = "DemoDataGeneratorResult",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "DemoDataGeneratorResult")
        @Response(name = "sync_success", type = "view", value = "DemoDataGeneratorResult")
        @Response(name = "error", type = "view", value = "RunDemoDataGeneratorService")
        public static String demoDataGeneratorResult(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.scheduleService
            return CoreEvents.scheduleService(request, response);
        }

        @Request(
            uri = "ListDemoDataGeneratorProviders",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListDemoDataGeneratorProviders")
        public interface ListDemoDataGeneratorProviders {}

        @Request(
            uri = "DeveloperDocIndex",
            controller = "webtools",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "DeveloperDocIndex")
        public interface DeveloperDocIndex {}

        @Request(
            uri = "ViewTemplateApiDocPage",
            controller = "webtools",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "TemplateApiDocPage")
        public interface ViewTemplateApiDocPage {}

        @Request(
            uri = "SolrStatus",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrStatus")
        public interface SolrStatus {}

        @Request(
            uri = "SolrServices",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrServices")
        public interface SolrServices {}

        @Request(
            uri = "SolrDoc",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrDoc")
        public interface SolrDoc {}

        @Request(
            uri = "runSolrService",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrServices")
        @Response(name = "sync_success", type = "view", value = "SolrServices")
        @Response(name = "error", type = "view", value = "SolrServices")
        public static String runSolrService(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.scheduleService
            return CoreEvents.scheduleService(request, response);
        }

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "runSolrServiceForStatus",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrStatus")
        @Response(name = "sync_success", type = "view", value = "SolrStatus")
        @Response(name = "error", type = "view", value = "SolrStatus")
        public static String runSolrServiceForStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.webapp.event.CoreEvents.scheduleService
            return CoreEvents.scheduleService(request, response);
        }

        @Request(
            uri = "setSolrSystemProperty",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrStatus")
        @Response(name = "error", type = "view", value = "SolrStatus")
        @Event(type = "service", invoke = "setSolrSystemProperty")
        public static String setSolrSystemProperty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSolrSystemProperty",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SolrStatus")
        @Response(name = "error", type = "view", value = "SolrStatus")
        @Event(type = "service", invoke = "removeSolrSystemProperty")
        public static String removeSolrSystemProperty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "excelimport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ExcelImport")
        public interface Excelimport {}

        @Request(
            uri = "excelI18nImport",
            controller = "webtools",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "excelimport")
        @Event(type = "service", invoke = "excelI18nImport")
        public static String excelI18nImport(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }


    }
}
