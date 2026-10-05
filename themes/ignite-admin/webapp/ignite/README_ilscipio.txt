NOTE (Scipio 4.0): the npm/Gulp build files (package.json, package-lock.json, gulpfile.js) are removed.
The toolchain had known security problems and does not install on a current Node.js. The compiled CSS and
JavaScript in this folder are final. To change the SCSS, take the build files from branch 3.x.

-----------------
Setup
-----------------
To setup system to compile sass files, the following packages/commands are needed (ubuntu/debian/mint):

sudo apt-get install ruby ruby-dev ruby-full nodejs nodejs-dev npm
npm install
npm install -g bower
bower install

If npm install throws an error about a missing node-sass url, run 
'npm i gulp-sass@latest --save-dev' 
and retry the install process.

-----------------
Compilation
-----------------
To start watching for less changes to auto compile to css, go to:

@component-name/webapp/@component-name/

and run:
gulp

-----------------
Bower update
-----------------
Run command from @component-name/webapp/@component-name/:

bower update


-----------------
Style changes
-----------------
Currently we include our own styles in @component-name/webapp/@component-name/styles