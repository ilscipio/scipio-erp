-----------------
SASS setup
-----------------
To setup system to compile sass files, the following packages/commands are needed (ubuntu/debian/mint):

sudo apt-get install ruby ruby-dev ruby-full nodejs nodejs-dev npm
gem install bundler
gem install compass
gem install foundation
npm install -g bower grunt-cli
bundle install

-----------------
SASS compile
-----------------
To start watching for sass changes to auto compile to css, run command:

bundle exec compass watch

-----------------
Bower update
-----------------
Run command (TODO, probably incomplete):

bower update


