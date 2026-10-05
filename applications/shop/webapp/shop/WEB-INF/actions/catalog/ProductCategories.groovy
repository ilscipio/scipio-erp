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

/*
 * This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */
import org.ofbiz.entity.util.EntityUtil;

import org.ofbiz.base.util.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import org.ofbiz.entity.*;

List fillTree(rootCat ,CatLvl, parentCategoryId) {
    if(rootCat) {
        rootCat.sort{ it.productCategoryId }
        def listTree = [];
        for(root in rootCat) {
            preCatChilds = from("ProductCategoryRollup").where("parentProductCategoryId", root.productCategoryId).queryList();
            catChilds = EntityUtil.getRelated("CurrentProductCategory",null,preCatChilds,false);
            def childList = [];
            
            // CatLvl uses for identify the Category level for display different css class
            if(catChilds) {
                if(CatLvl==2)
                    childList = fillTree(catChilds,CatLvl+1, parentCategoryId.replaceAll("/", "")+'/'+root.productCategoryId);
                    // replaceAll and '/' uses for fix bug in the breadcrum for href of category
                else if(CatLvl==1)
                    childList = fillTree(catChilds,CatLvl+1, parentCategoryId.replaceAll("/", "")+root.productCategoryId);
                else
                    childList = fillTree(catChilds,CatLvl+1, parentCategoryId+'/'+root.productCategoryId);
            }
            
            productsInCat  = from("ProductCategoryAndMember").where("productCategoryId", root.productCategoryId).queryList();
            
            // Display the category if this category containing products or contain the category that's containing products
            if(productsInCat || childList) {
                def rootMap = [:];
                category = from("ProductCategory").where("productCategoryId", root.productCategoryId).queryOne();
                categoryContentWrapper = new CategoryContentWrapper(category, request);
                // SCIPIO: don't want page title overridden/forced by groovy
                // SCIPIO: Do NOT HTML-escape this here
                //context.title = categoryContentWrapper.get("CATEGORY_NAME");
                context.categoryTitle = categoryContentWrapper.get("CATEGORY_NAME");
                categoryDescription = categoryContentWrapper.get("DESCRIPTION");
                
                if(categoryContentWrapper.get("CATEGORY_NAME"))
                    rootMap["categoryName"] = categoryContentWrapper.get("CATEGORY_NAME");
                else
                    rootMap["categoryName"] = root.categoryName;
                
                if(categoryContentWrapper.get("DESCRIPTION"))
                    rootMap["categoryDescription"] = categoryContentWrapper.get("DESCRIPTION");
                else
                    rootMap["categoryDescription"] = root.description;
                
                rootMap["productCategoryId"] = root.productCategoryId;
                rootMap["parentCategoryId"] = parentCategoryId;
                rootMap["child"] = childList;

                listTree.add(rootMap);
            }
        }
        return listTree;
    }
}

CategoryWorker.getRelatedCategories(request, "topLevelList", CatalogWorker.getCatalogTopCategoryId(request, CatalogWorker.getCurrentCatalogId(request)), true);
curCategoryId = parameters.category_id ?: parameters.CATEGORY_ID ?: "";
request.setAttribute("curCategoryId", curCategoryId);
CategoryWorker.setTrail(request, curCategoryId);

categoryList = request.getAttribute("topLevelList");
if (categoryList) {
    catContentWrappers = [:];
    CategoryWorker.getCategoryContentWrappers(catContentWrappers, categoryList, request);
    context.catContentWrappers = catContentWrappers;
    completedTree = fillTree(categoryList,1,"");
    context.completedTree = completedTree;
}
