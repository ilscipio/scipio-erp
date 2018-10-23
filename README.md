```
**************************************************************************

   _____    _____   _   _____    _    ____      _____   _____    _____
  / ____|  / ____| | | |  __ \  | |  / __ \    |  ___| |  __ \  |  __ \
 | (___   | |      | | | |__) | | | | |  | |   | |___  | |__) | | |__) |
  \___ \  | |      | | |  ___/  | | | |  | |   |  ___| |  _  /  |  ___/
  ____) | | |____  | | | |      | | | |__| |   | |___  | | \ \  | |
 |_____/   \_____| |_| |_|      |_|  \____/    |_____| |_|  \_\ |_|

                    https://www.scipioerp.com

**************************************************************************
```
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-blue.svg?style=flat)](http://makeapullrequest.com) [![Maintainability](https://api.codeclimate.com/v1/badges/0193ee026d92287b5db0/maintainability)](https://codeclimate.com/github/ilscipio/scipio-erp/maintainability) [![Discourse topics](https://img.shields.io/discourse/https/forum.scipioerp.com/topics.svg)](https://forum.scipioerp.com) [![Read the Docs](https://img.shields.io/readthedocs/pip.svg)](https://www.scipioerp.com/community/developer/installation-configuration/)

# SCIPIO ERP - Community Edition
![Scipio ERP is a toolkit for the development of modern business applications.](https://www.scipioerp.com/files/2018/06/slider_desk_900.jpg)

* [Website](https://www.scipioerp.com)
* [Demo](https://www.scipioerp.com/demo/)
* [Developer Docs](https://www.scipioerp.com/community/developer/installation-configuration/)
* [User Docs](https://www.scipioerp.com/community/end-user/applications/)
* [Community](https://forum.scipioerp.com/)

## What is Scipio ERP
Scipio ERP is an Open Source Business Application Toolkit based on Java 8 and a built-in
Tomcat application server. We provide standard applications, functions (services)
and a well thought-out datamodel, so that you can create modern web applications.

Our templating toolkit simplifies the creation of modern UIs and is compatible with most
modern HTML frameworks.

[Technologies](https://www.scipioerp.com/products/technologies/)

### TL;DR
* Simplifies the creation of business or ECommerce applications
* Highly modular, extendable and customizable
* Bundles a long list of working applications
* Supports most modern HTML frameworks
* Supports Caching & Clustering
* Can be rolled out internationally

## What's included
* Business Applications & functions for
  * Accounting
  * Asset Maintenance
  * Catalog Management
  * Content Management
  * Customer Relationship Management
  * Ecommerce
  * Human Resource Management
  * Order Management
  * User Management
  * Warehouse Management
  * Work Effort (Time tracking)
* A templating toolkit (freemarker macros) to simplify UI creation
* A multi-language, multi-national, multi-store Ecommerce application
* A flexible datamodel
* Support of various third-party APIs (payment, shipping, apache camel, etc.)

## Installation
### System Requirements
* Operating System: Windows, Linux, OS X
* Core 2 Duo or Athlon X2 at 2.4 GHz or higher
* 4+GB RAM, 2+GB free hard disk space

### Software Requirements
* Java 1.8 (or greater) SDK
* Apache ANT

### Recommended Development Tools
* GIT Client
* Eclipse Java IDE
  * m2eclipse
  * Groovy-eclipse
  * Freemarker IDE editor (select from JBooss tools plugin)

### Prerequisites
In order to install SCIPIO ERP, the following prerequisites must be installed:
* Java 1.8 (or greater) SDK
  * Download and Install
  * Set JAVA_HOME Path
  * Validate Settings
* Install Apache Ant
  * Download and Install

### Download
The standard way to get SCIPIO ERP to is to checkout the scipioce-1.14 branch, which
provides you with the latest version of the 1.14.x line with latest important updates:

1. Open your command line and run:
  * git clone https://github.com/ilscipio/scipio-erp.git
  * cd scipio-erp
  * git checkout scipioce-1.14

You may also use the master branch (simply omit step c.), the main development branch,
for access to latest features and changes, but it is subject to compatibility-breaking
changes without notice (unless you have a supported client project, in which case we
will assist). For some projects, master may be the recommended branch to use.

### Installation Process
In order to install, the following steps must be taken:

1. Open your command line, go to the extracted folder and run:
  * Linux ./install.sh
  * OS X: ./install.sh
  * Windows: install.bat

2. From the same command line run:
  * Linux ./start.sh
  * OS X: bash ./start.sh
  * Windows: start.bat

3. To access the application visit the SCIPIO ERP Dashboard:
  https://localhost:8443/admin

4. To access the SCIPIO ERP applications from the Dashboard use:
  Username: admin
  Password: scipio

                     **Congratulations, you have installed SCIPIO ERP!**

### Updates
1. Retrieve latest code updates from git:
a. git checkout scipioce-1.14
b. git pull

2. Reload visual themes:
a. Restart SCIPIO server
b. Visit entity utility services page:
  https://localhost:8443/admin/control/EntityUtilityServices
c. Click "Visual Theme Resources - Reload All, Now"

### Optional Configuration
  https://www.scipioerp.com/community/developer/installation-configuration/configuration/

### Addons
Community and enterprise SCIPIO ERP addons can be added to your working
copy and updated using the 'git-addons' Bash script in the project root.
It requires a Bash 4-compatible terminal (Linux, Mac, Windows Git Bash, Cygwin).

Instructions can be found at:

  https://www.scipioerp.com/community/end-user/addons/

or for brief help and command list, type:

  ./git-addons help

### Docker
We also provide Docker images if you would like to try out Scipio with minimal effort. To create a fully functional SCIPIO ERP instance with some demo data already loaded, you can create a container with the following command:

  docker create -p 8080:8080 -p 8443:8443 ilscipio/scipio-erp:demo


## Support
For detailed information and changes about the SCIPIO ERP suite, visit the official website at:

  [https://www.scipioerp.com](https://www.scipioerp.com "Scipio ERP Website")

You can get in touch with the community over at:

  [https://forum.scipioerp.com](https://forum.scipioerp.com "Community Forum")

## OFBiz
Scipio ERP is a fork of the Apache OFBiz project.

For more details about OFBiz please visit the OFBiz Documentation page:

  http://ofbiz.apache.org/documentation.html

## License
The source code that makes up The SCIPIO ERP Community Edition
(hereinafter referred to as "SCIPIO ERP") and the majority of the
libraries distributed with it are licensed under the Apache License v2.0.

Other licenses used by libraries distributed with SCIPIO ERP are listed
in the LICENSE file. This file includes a list of all libraries distributed with SCIPIO
ERP and the full text of the license used for each.

For additional details, see the NOTICE file.

## Disclaimer
This software is provided as is and free of charge. There is no warranty
or support implied under the terms of the license included.

SCIPIO ERP and the SCIPIO logo are trademarks of Ilscipio GmbH.
© Copyright SCIPIO components 2016 Ilscipio GmbH.
Apache OFBiz, Apache, the Apache feather logo are trademarks
of The Apache Software Foundation.

### BIS Crypto TSU exception notice

   This distribution includes cryptographic software.  The country in
   which you currently reside may have restrictions on the import,
   possession, use, and/or re-export to another country, of
   encryption software.  BEFORE using any encryption software, please
   check your country's laws, regulations and policies concerning the
   import, possession, or use, and re-export of encryption software, to
   see if this is permitted.  See <http://www.wassenaar.org/> for more
   information.

   The U.S. Government Department of Commerce, Bureau of Industry and
   Security (BIS), has classified this software as Export Commodity
   Control Number (ECCN) 5D002.C.1, which includes information security
   software using or performing cryptographic functions with asymmetric
   algorithms.  The form and manner of this Apache Software Foundation
   distribution makes it eligible for export under the License Exception
   ENC Technology Software Unrestricted (TSU) exception (see the BIS
   Export Administration Regulations, Section 740.13) for both object
   code and source code.

   The following provides more details on the included cryptographic
   software:

    * Various classes in Scipio, including DesCrypt, HashCrypt, and
     BlowFishCrypt use libraries from the Sun Java JDK API including
     java.security.* and javax.crypto.* (the JCE, Java Cryptography
     Extensions API)
    * Other classes such as HttpClient and various related ones use
     the JSSE (Java Secure Sockets Extension) API
