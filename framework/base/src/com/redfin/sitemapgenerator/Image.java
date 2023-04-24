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

/**
 * Represent a single image and image properties for use in extended sitemaps
 * @see <a href="https://support.google.com/webmasters/answer/178636">Image sitemaps</a>
 */
public class Image {
    private final URL url;
    private final String title;
    private final String caption;
    private final String geoLocation;
    private final URL license;

    public Image(String url) throws MalformedURLException {
        this(new URL(url));
    }

    public Image(URL url) {
        this.url = url;
        this.title = null;
        this.caption = null;
        this.geoLocation = null;
        this.license = null;
    }

    public Image(URL url, String title, String caption, String geoLocation, String license) throws MalformedURLException {
        this(url, title, caption, geoLocation, new URL(license));
    }

    public Image(URL url, String title, String caption, String geoLocation, URL license) {
        this.url = url;
        this.title = title;
        this.caption = caption;
        this.geoLocation = geoLocation;
        this.license = license;
    }


    /** Retrieves URL of Image*/
    public URL getUrl() { return url; }

    /** Retrieves title of image*/
    public String getTitle() { return title; }

    /** Retrieves captionof image*/
    public String getCaption() { return caption; }

    /** Retrieves geolocation string of image*/
    public String getGeoLocation() { return geoLocation; }

    /** Retrieves license string of image*/
    public URL getLicense() { return license; }

    public static class ImageBuilder {
        private URL url;
        private String title;
        private String caption;
        private String geoLocation;
        private URL license;

        public ImageBuilder(String url) throws MalformedURLException {
            this(new URL(url));
        }

        public ImageBuilder(URL url) {
            this.url = url;
        }

        public ImageBuilder title(String title) {
            this.title = title;
            return this;
        }

        public ImageBuilder caption(String caption) {
            this.caption = caption;
            return this;
        }

        public ImageBuilder geoLocation(String geoLocation) {
            this.geoLocation = geoLocation;
            return this;
        }

        public ImageBuilder license(String license) throws MalformedURLException {
            return license(new URL(license));
        }

        public ImageBuilder license(URL license) {
            this.license = license;
            return this;
        }

        public Image build() {
            return new Image(url, title, caption, geoLocation, license);
        }
    }
}
