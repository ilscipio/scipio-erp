/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import org.ofbiz.webapp.control.LoginWorker;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;


ofbizServerName = application.getAttribute("_serverId")!= null ? application.getAttribute("_serverId") : "default-server";
primaryMenu = LoginWorker.getAppBarWebInfos(security, userLogin, ofbizServerName, "main");
secondaryMenu = LoginWorker.getAppBarWebInfos(security, userLogin, ofbizServerName, "secondary");
allApps = LoginWorker.getAppBarWebInfos(security, userLogin, ofbizServerName, null);


// Fill the context
context.primaryMenu = primaryMenu;
context.secondaryMenu = secondaryMenu;
context.allApps = allApps;
context.contextPath = request.getContextPath();