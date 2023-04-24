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

package com.redfin.sitemapgenerator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** One configurable Google Image Search URL.  To configure, use {@link Options}
 *
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=183668">Creating Image Sitemaps</a>
 */
public class GoogleImageSitemapUrl extends WebSitemapUrl {

    private final List<Image> images;

    public GoogleImageSitemapUrl(String url) throws MalformedURLException {
        this(new Options(url));
    }

    public GoogleImageSitemapUrl(URL url) {
        this(new Options(url));
    }

    public GoogleImageSitemapUrl(Options options) {
        super(options);
        this.images = options.images;
    }

    public void addImage(Image image) {
        this.images.add(image);
        if(this.images.size() > 1000) {
            throw new RuntimeException("A URL cannot have more than 1000 image tags");
        }
    }

    /** Options to configure Google Extension URLs */
    public static class Options extends AbstractSitemapUrlOptions<GoogleImageSitemapUrl, GoogleImageSitemapUrl.Options> {
        private List<Image> images;


        public Options(URL url) {
            super(url, GoogleImageSitemapUrl.class);
            images = new ArrayList<Image>();
        }

        public Options(String url) throws MalformedURLException {
            super(url, GoogleImageSitemapUrl.class);
            images = new ArrayList<Image>();
        }

        public Options images(List<Image> images) {
            if(images != null && images.size() > 1000) {
                throw new RuntimeException("A URL cannot have more than 1000 image tags");
            }
            this.images = images;
            return this;
        }

        public Options images(Image...images) {
            if(images.length > 1000) {
                throw new RuntimeException("A URL cannot have more than 1000 image tags");
            }
            return images(Arrays.asList(images));

        }
    }

    /**Retrieves list of images*/
    public List<Image> getImages() {
        return this.images;
    }
}
