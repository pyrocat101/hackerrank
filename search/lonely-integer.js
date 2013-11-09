function processData(input) {
    var xs = input.split('\n')[1].split(' ').map(function (x) { return parseInt(x); });
    console.log(xs.reduce(function (acc, x) { return acc ^ x; }));
}

process.stdin.resume();
process.stdin.setEncoding("ascii");
_input = "";
process.stdin.on("data", function (input) {
    _input += input;
});

process.stdin.on("end", function () {
   processData(_input);
});
