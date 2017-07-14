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
package org.ofbiz.content.image;

import org.ofbiz.base.util.UtilProperties;

/**
 * SCIPIO: Content/generic image utilities.
 * Added 2017-07-04.
 */
public abstract class ContentImageWorker {

    public static final String module = ContentImageWorker.class.getName();
    
    /**
     * Special image size type name designating the original (unscaled/unmodified) image.
     * <p>
     * This should be the same value as {@link org.ofbiz.webapp.content.ContentRequestWorker#ORIGINAL_SIZETYPE}
     * which is used in the <code>@ofbizContentUrl</code> macro.
     */
    public static final String ORIGINAL_SIZETYPE = "original";
    
    public static final String CONTENT_IMAGEPROP_FILEPATH = "/applications/content/config/ImageProperties.xml";
    
    /**
     * Keeps from overloading log with giant filenames.
     */
    public static final int LOG_INFO_MAXPATH = UtilProperties.getPropertyAsInteger("content", "image.log.info.maxpath", 80);
    
    protected ContentImageWorker() {
    }

    public static String getImagePropertiesFullPath(String imgPropertyPath) {
        return System.getProperty("ofbiz.home") + imgPropertyPath;
    }
    
    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for generic image size definitions.
     * Added 2017-07-04.
     */
    public static String getContentImagePropertiesFullPath() {
        return getImagePropertiesFullPath(CONTENT_IMAGEPROP_FILEPATH);
    }
    
    public static String getContentImagePropertiesPath() {
        return CONTENT_IMAGEPROP_FILEPATH;
    }
    
    public static String formatLogInfoPath(String filename) {
        if (filename == null || filename.isEmpty()) return "[none]";
        else return "'" 
                + (filename.length() > LOG_INFO_MAXPATH ? "..." + filename.substring(filename.length() - LOG_INFO_MAXPATH) : filename)
                + "'";
    }

}
