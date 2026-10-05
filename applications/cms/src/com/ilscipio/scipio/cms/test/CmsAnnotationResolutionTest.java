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
package com.ilscipio.scipio.cms.test;

import org.ofbiz.service.testtools.OFBizTestCase;
import org.ofbiz.widget.model.MenuFactory;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ScreenFactory;

/**
 * Tests that CMS annotation-based screen and menu definitions resolve correctly
 * via the location alias mechanism.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class CmsAnnotationResolutionTest extends OFBizTestCase {

    private static final String CMS_SCREENS_LOCATION = "component://cms/widget/CMSScreens.xml";
    private static final String CMS_COMMON_SCREENS_LOCATION = "component://cms/widget/CommonScreens.xml";
    private static final String CMS_MENUS_LOCATION = "component://cms/widget/CMSMenus.xml";

    public CmsAnnotationResolutionTest(String name) {
        super(name);
    }

    public void testCmsScreensLocationAlias() {
        assertTrue("CMSScreens.xml location alias should be registered",
                ScreenFactory.hasLocationAlias(CMS_SCREENS_LOCATION));
    }

    public void testCmsCommonScreensLocationAlias() {
        assertTrue("CommonScreens.xml location alias should be registered",
                ScreenFactory.hasLocationAlias(CMS_COMMON_SCREENS_LOCATION));
    }

    public void testMainScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "main");
        assertNotNull("Screen 'main' should resolve from CMSScreens.xml alias", screen);
        assertEquals("main", screen.getName());
    }

    public void testPagesScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "pages");
        assertNotNull("Screen 'pages' should resolve", screen);
    }

    public void testTemplatesScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "templates");
        assertNotNull("Screen 'templates' should resolve", screen);
    }

    public void testEditPageScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "editPage");
        assertNotNull("Screen 'editPage' should resolve", screen);
    }

    public void testMediaScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "media");
        assertNotNull("Screen 'media' should resolve", screen);
    }

    public void testMenusScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "menus");
        assertNotNull("Screen 'menus' should resolve", screen);
    }

    public void testCommonCMSAppDecoratorResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "CommonCMSAppDecorator");
        assertNotNull("CommonCMSAppDecorator should resolve", screen);
    }

    public void testMainDecoratorResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "main-decorator");
        assertNotNull("main-decorator should resolve", screen);
    }

    public void test404ScreenResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "404");
        assertNotNull("404 screen should resolve", screen);
    }

    public void testCmsContentTreeResolvable() {
        ModelScreen screen = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "CmsContentTree");
        assertNotNull("CmsContentTree should resolve", screen);
    }

    public void testCmsMenusLocationAlias() {
        assertTrue("CMSMenus.xml location alias should be registered",
                MenuFactory.hasLocationAlias(CMS_MENUS_LOCATION));
    }

    public void testMainAppBarMenuResolvable() {
        assertNotNull("MainAppBar menu should resolve",
                MenuFactory.getMenuFromLocationAlias(CMS_MENUS_LOCATION, "MainAppBar"));
    }

    public void testDecoratorChain() throws Exception {
        ModelScreen main = ScreenFactory.getScreenFromLocationAlias(CMS_SCREENS_LOCATION, "main");
        assertNotNull("main screen should resolve for decorator chain test", main);

        ModelScreen decorator = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "CommonCMSAppDecorator");
        assertNotNull("CommonCMSAppDecorator should resolve", decorator);

        ModelScreen mainDecorator = ScreenFactory.getScreenFromLocationAlias(CMS_COMMON_SCREENS_LOCATION, "main-decorator");
        assertNotNull("main-decorator should resolve", mainDecorator);

        ModelScreen appDecorator = ScreenFactory.getScreenFromLocationAlias(
                "component://commonext/widget/CommonScreens.xml", "ApplicationDecorator");
        assertNotNull("ApplicationDecorator from commonext should resolve", appDecorator);
    }
}
