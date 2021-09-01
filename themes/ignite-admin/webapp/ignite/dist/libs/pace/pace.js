/**
 * Pace
 *
 * A progress bar for the command-line.
 *
 * Example usage:
 *
 *     var total = 50000,
 *         count = 0,
 *         pace = require('pace')(total);
 *
 *     while (count++ < total) {
 *       pace.op();
 *
 *       // Cause some work to be done.
 *       for (var i = 0; i < 1000000; i++) {
 *         count = count;
 *       }
 *     }
 */

// Module dependencies.
var charm = require('charm');

/**
 * Pace 'class'.
 */
function Pace(options) {
  options = options || {};

  // Total number of items to process.
  if (!options.total) {
    throw new Error('You MUST specify the total number of operations that will be processed.');
  }
  this.total = options.total;

  // Current item number.
  this.current = 0;

  // Maximum percent of total time the progressbar is allowed to take during processing.
  // Defaults to 0.5%
  this.max_burden = options.maxBurden || 0.5;

  // Whether to show current burden %.
  this.show_burden = options.showBurden || false;

  // Internal time tracking properties.
  this.started = false;
  this.size = 50;
  this.inner_time = 0;
  this.outer_time = 0;
  this.elapsed = 0;
  this.time_start = 0;
  this.time_end = 0;
  this.time_left = 0;
  this.time_burden = 0;
  this.skip_steps = 0;
  this.skipped = 0;
  this.aborted = false;

  // Setup charm.
  this.charm = charm();
  this.charm.pipe(process.stdout);

  // Prepare the output.
  this.charm.write("\n\n\n");
}

/**
 * Export a factory function for new pace instances.
 */
module.exports = function(options) {
  if (typeof options === 'number') {
    options = {
      total: options
    };
  }
  return new Pace(options);
};

/**
 * An operation has been emitted.
 */
Pace.prototype.op = function op(count) {
  if (count) {
    this.current = count;
  }
  else {
    this.current++;
  }

  if (this.burdenReached()) {
    return;
  }

  // Record the start time of the whole task.
  if (!this.started) {
    this.started = new Date().getTime();
  }

  // Record start time.
  this.time_start = new Date().getTime();

  this.updateTimes();
  this.clear();
  this.outputProgress();
  this.outputStats();
  this.outputTimes();

  // The task is complete.
  if (this.current >= this.total) {
    this.finished();
  }

  // Record end time.
  this.time_end = new Date().getTime();
  this.inner_time = this.time_end - this.time_start;
};

/**
 * Update times.
 */
Pace.prototype.updateTimes = function updateTimes() {
  this.elapsed = this.time_start - this.started;
  if (this.time_end > 0) {
    this.outer_time = this.time_start - this.time_end;
  }
  if (this.inner_time > 0 && this.outer_time > 0) {
    // Set Current Burden
    this.time_burden = (this.inner_time / (this.inner_time + this.outer_time)) * 100;

    // Estimate time left.
    this.time_left = (this.elapsed / this.current) * (this.total - this.current);

    if (this.time_left < 0) this.time_left = 0;
  }
  // If our "burden" is too high, increase the skip steps.
  if (this.time_burden > this.max_burden && (this.skip_steps < (this.total / this.size))) {
    this.skip_steps = Math.floor(++this.skip_steps * 1.3);
  }
};

/**
 * Move the cursor back to the beginning and clear old output.
 */
Pace.prototype.clear = function clear() {
  this.charm.erase('line').up(1).erase('line').up(1).erase('line').write("\r");
};

/**
 * Output the progress bar.
 */
Pace.prototype.outputProgress = function outputProgress() {
  this.charm.write('Processing: ');
  this.charm.foreground('green').background('green');
  for (var i = 0; i < ((this.current / this.total) * this.size) - 1 ; i++) {
     this.charm.write(' ');
  }
  this.charm.foreground('white').background('white');
  while (i < this.size - 1) {
    this.charm.write(' ');
    i++;
  }
  this.charm.display('reset').down(1).left(100);
};

/**
 * Output numerical progress stats.
 */
Pace.prototype.outputStats = function outputStats() {
  this.perc = (this.current/this.total)*100;
  this.perc = padLeft(this.perc.toFixed(2), 2);
  this.charm.write('            ').display('bright').write(this.perc + '%').display('reset');
  this.total_len = formatNumber(this.total).length;
  this.charm.write('   ').display('bright').write(padLeft(formatNumber(this.current), this.total_len)).display('reset');
  this.charm.write('/' + formatNumber(this.total));

  // Output burden.
  if (this.show_burden) {
    this.charm.write('    ').display('bright').write('Burden: ').display('reset');
    this.charm.write(this.time_burden.toFixed(2) + '% / ' + this.skip_steps);
  }

  this.charm.display('reset').down(1).left(100);
};

/**
 * Output times.
 */
Pace.prototype.outputTimes = function outputTimes() {
  // Output times.
  var hours = Math.floor(this.elapsed / (1000 * 60 * 60));
  var min = Math.floor(((this.elapsed / 1000) % (60 * 60)) / 60);
  var sec = Math.floor((this.elapsed / 1000) % 60);

  this.charm.write('            ').display('bright').write('Elapsed: ').display('reset');
  this.charm.write(hours + 'h ' + min + 'm ' + sec + 's');

  if (this.time_left){
    hours = Math.floor(this.time_left / (1000 * 60 * 60));
    min = Math.floor(((this.time_left / 1000) % (60 * 60)) / 60);
    sec = Math.ceil((this.time_left / 1000) % 60);

    this.charm.write('   ').display('bright').write('Remaining: ').display('reset');
    this.charm.write(hours + 'h ' + min + 'm ' + sec + 's');
  }
};

/**
 * The progress has finished.
 */
Pace.prototype.finished = function finished() {
  this.charm.write("\n\n");
  this.charm.write('Finished!');
  this.charm.write("\n\n");
};

/**
 * Check if the burden threshold has been reached.
 */
Pace.prototype.burdenReached = function burdenReached() {
  // Skip this cycle if the burden has determined we should.
  if ((this.skip_steps > 0) && (this.current < this.total)) {
    if (this.skipped < this.skip_steps) {
      this.skipped++;
      return true;
    }
    else {
      this.skipped = 0;
    }
  }
  return false;
};


/**
 * Utility functions.
 */

// Left-pad a string.
function padLeft(str, length, pad) {
  pad = pad || ' ';
  while (str.length < length)
    str = pad + str;
  return str;
}

// Ported from php.js. Same has php's number_format().
function formatNumber(number, decimals, dec_point, thousands_sep) {
  number = (number + '').replace(/[^0-9+\-Ee.]/g, '');
  var n = !isFinite(+number) ? 0 : +number,
    prec = !isFinite(+decimals) ? 0 : Math.abs(decimals),
    sep = (typeof thousands_sep === 'undefined') ? ',' : thousands_sep,
    dec = (typeof dec_point === 'undefined') ? '.' : dec_point,
    s = '',
    toFixedFix = function (n, prec) {
      var k = Math.pow(10, prec);
      return '' + Math.round(n * k) / k;
    };
  // Fix for IE parseFloat(0.55).toFixed(0) = 0;
  s = (prec ? toFixedFix(n, prec) : '' + Math.round(n)).split('.');
  if (s[0].length > 3) {
    s[0] = s[0].replace(/\B(?=(?:\d{3})+(?!\d))/g, sep);
  }
  if ((s[1] || '').length < prec) {
    s[1] = s[1] || '';
    s[1] += new Array(prec - s[1].length + 1).join('0');
  }
  return s.join(dec);
}
