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
package org.ofbiz.webapp.event;

import java.io.Serializable;
import java.util.Collections;
import java.util.Map;

import org.apache.commons.fileupload.ProgressListener;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * FileUploadProgressListener - Commons FileUpload progress listener
 * <p>SCIPIO: 2.1.0: Added synchronization.</p>
 */
@SuppressWarnings("serial")
public class FileUploadProgressListener implements ProgressListener, Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected long contentLength = -1;
    protected long bytesRead = -1;
    protected int items = -1;
    protected boolean hasStarted = false;
    protected Map<String, Object> eventMessages = Collections.emptyMap();

    public synchronized void update(long bytesRead, long contentLength, int items) {
        this.contentLength = contentLength;
        this.bytesRead = bytesRead;
        this.items = items;
        if (!hasStarted) {
            hasStarted = true;
        }
    }

    public synchronized long getContentLength() {
        return contentLength;
    }

    public synchronized long getBytesRead() {
        return bytesRead;
    }

    public synchronized int getItems() {
        return items;
    }

    public synchronized boolean hasStarted() {
        return hasStarted;
    }

    public synchronized Map<String, Object> getEventMessages() {
        return eventMessages;
    }

    public synchronized void setEventMessages(Map<String, Object> eventMessages) {
        this.eventMessages = eventMessages;
    }

    /**
     * Manual callback needed to be called after events which use the file listener to attach resulting event messages
     * to the listener.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static String updateEventMessages(HttpServletRequest request, HttpServletResponse response) {
        FileUploadProgressListener fupl = UtilHttp.getSessionAttr(request, "uploadProgressListener");
        if (fupl != null) {
            fupl.setEventMessages(EventUtil.getEventErrorAttributesAsMap(request));
        } else {
            Debug.logWarning("No uploadProgressListener in session attributes; can't update event messages on listener", module);
        }
        return "success";
    }
}
