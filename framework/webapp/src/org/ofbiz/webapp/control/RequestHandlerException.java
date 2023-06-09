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
package org.ofbiz.webapp.control;

import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.PropertyMessage;

import java.util.Collection;

/**
 * RequestHandlerException
 *
 * <p>SCIPIO: 3.0.0: Enhanced with localized message support.</p>
 */
@SuppressWarnings("serial")
public class RequestHandlerException extends GeneralException {

    public RequestHandlerException() {
    }

    public RequestHandlerException(String msg) {
        super(msg);
    }

    public RequestHandlerException(String msg, Throwable nested) {
        super(msg, nested);
    }

    public RequestHandlerException(Throwable nested) {
        super(nested);
    }

    public RequestHandlerException(String msg, Collection<?> messageList) {
        super(msg, messageList);
    }

    public RequestHandlerException(String msg, Collection<?> messageList, Throwable nested) {
        super(msg, messageList, nested);
    }

    public RequestHandlerException(Collection<?> messageList, Throwable nested) {
        super(messageList, nested);
    }

    public RequestHandlerException(Collection<?> messageList) {
        super(messageList);
    }

    public RequestHandlerException(PropertyMessage propMsg) {
        super(propMsg);
    }

    public RequestHandlerException(PropertyMessage propMsg, Throwable nested) {
        super(propMsg, nested);
    }

}

