const readline = require('readline');
const step = process.argv[2];
const elm = require(`./${step}.js`);

var runner = elm.Mal.worker();
var rl = readline.createInterface({input: process.stdin, output: process.stdout});

rl.output.write('user> ')
runner.ports.output.subscribe(s => rl.output.write(s + '\nuser> '));
runner.ports.end.subscribe(n => process.exit(n));
rl.on('line', function(cmd) { runner.ports.input.send(cmd); });
rl.on('', function() { runner.ports.close.send([]); });
