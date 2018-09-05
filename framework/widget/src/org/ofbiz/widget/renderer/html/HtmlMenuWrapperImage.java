/*******************************************************************************
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
 *******************************************************************************/
package org.ofbiz.widget.renderer.html;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.widget.model.ModelMenuItem;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.xml.sax.SAXException;

/**
 * Widget Library - HTML Menu Wrapper class - makes it easy to do the setup and render of a menu
 * <p>
 * @deprecated SCIPIO: 2016-08-30: Considered deprecated and
 * will not be maintained. Use macro renderer instead.
 */
@Deprecated
public class HtmlMenuWrapperImage extends HtmlMenuWrapper {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected HtmlMenuWrapperImage() {}

    public HtmlMenuWrapperImage(String resourceName, String menuName, HttpServletRequest request, HttpServletResponse response)
            throws IOException, SAXException, ParserConfigurationException {
        super(resourceName, menuName, request, response);
    }

    @Override
    public MenuStringRenderer getMenuRenderer() {
        return new HtmlMenuRendererImage(request, response);
    }

    @Override
    public void init(String resourceName, String menuName, HttpServletRequest request, HttpServletResponse response)
            throws IOException, SAXException, ParserConfigurationException {

        super.init(resourceName, menuName, request, response);
        Map<String, Object> dummyMap = new HashMap<>();
        Delegator delegator = (Delegator)request.getAttribute("delegator");
        try {
            for (ModelMenuItem menuItem : modelMenu.getMenuItemList()) {
               String contentId = menuItem.getAssociatedContentId(dummyMap);
               GenericValue webSitePublishPoint = EntityQuery.use(delegator).from("WebSitePublishPoint").where("contentId", contentId).cache().queryOne();
               String menuItemName = menuItem.getName();
               putInContext(menuItemName, "WebSitePublishPoint", webSitePublishPoint);
            }
        } catch (GenericEntityException e) {
            return;
        }
    }
}
