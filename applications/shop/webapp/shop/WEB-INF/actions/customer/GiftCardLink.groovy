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

import org.ofbiz.product.store.*;

processResult = parameters.processResult;
if (processResult != null) {
   if (!processResult) {
       request.setAttribute("_ERROR_MESSAGE_", "<li>There was a problem linking your cards. Please check the numbers and try again.</li>");
       request.removeAttribute("_EVENT_MESSAGE_");
   } else {
       request.setAttribute("_EVENT_MESSAGE_", "<li>Thank-you. Your gift card account is now linked.</li>");
   }
}

context.userLogin = userLogin;
context.paymentProperties = ProductStoreWorker.getProductStorePaymentProperties(request, "GIFT_CARD", null, true);
