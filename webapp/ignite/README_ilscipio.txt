-----------------
LESS setup
-----------------
This is based on: http://getbootstrap.com/getting-started/ & https://github.com/FezVrasta/bootstrap-material-design 
to give bootstrap a more modern appeal. 
To setup system to compile sass files, the following packages/commands are needed (ubuntu/debian/mint):

sudo apt-get install ruby ruby-dev ruby-full nodejs nodejs-dev npm
npm install
npm install -g bower
bower install

If npm install throws an error about a missing node-sass url, run 
'npm i gulp-sass@latest --save-dev' 
and retry the install process.


-----------------
LESS compile
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

-----------------
Overlays
-----------------
Additional slider overlays can be downloaded from: http://html5backgroundvideos.com