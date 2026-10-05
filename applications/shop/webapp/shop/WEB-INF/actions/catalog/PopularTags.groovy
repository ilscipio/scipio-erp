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

import java.util.List;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.transaction.*
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.string.*;
import org.ofbiz.content.content.*;
import org.ofbiz.entity.util.EntityUtilProperties;

int minFontSize = Integer.parseInt(EntityUtilProperties.getPropertyValue("shop", "tagcloud.min.fontsize", delegator) ?: "8");
int maxFontSize = Integer.parseInt(EntityUtilProperties.getPropertyValue("shop", "tagcloud.max.fontsize", delegator) ?: "24");
int limitTagCloud = Integer.parseInt(EntityUtilProperties.getPropertyValue("shop", "tagcloud.limit", delegator) ?: "20");

tagCloudList = [] as LinkedList;
tagList = [] as LinkedList;

productKeywords = select("keyword", "keywordTypeId", "statusId")
                    .from("ProductKeyword")
                    .where(keywordTypeId : "KWT_TAG", statusId : "KW_APPROVED")
                    .orderBy("keyword")
                    .distinct(true)
                    .queryList();

if (UtilValidate.isNotEmpty(productKeywords)) {
    productKeywords.each { productKeyword ->
        productTags = from("ProductKeyword").where("keyword", productKeyword.keyword, "keywordTypeId", "KWT_TAG", "statusId", "KW_APPROVED").queryList();
        searchResult = [:];
        searchResult.tag = productKeyword.keyword;
        searchResult.countTag = productTags.size();
        tagList.add(searchResult);
    }
}

if (tagList) {
    int tag = 0;
    tagList.sort{ a,b -> b.countTag <=> a.countTag };
    if (tagList.size() < limitTagCloud) {
        limitTagCloud = tagList.size();
    }
    int maxResult = tagList[0].countTag;
    int minResult = tagList[limitTagCloud - 1].countTag;
    tagList.each { tagCloud ->
        if (tag < limitTagCloud) {
            tagCloudOfbizInfo = [:];
            double weight = 0;
            if ((maxResult - minResult) > 0) {
                weight = (tagCloud.countTag - minResult) / (maxResult - minResult);
            }
            double fontSize = minFontSize + ((maxFontSize - minFontSize) * weight);
            tagCloudOfbizInfo.tag = tagCloud.tag;
            tagCloudOfbizInfo.fontSize = fontSize;
            tagCloudList.add(tagCloudOfbizInfo);
            tag++;
        }
    }
}

tagCloudList = tagCloudList.sort{it.tag}

context.tagCloudList = tagCloudList;
