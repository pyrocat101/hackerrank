var IPV4 = /^(25[0-5]|2[0-4][0-9]|1?[0-9][0-9]{1,2})(\.(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})){3}$/,
    IPV6 = /^([0-9a-f]){1,4}(:([0-9a-f]){1,4}){7}$/i;

function processData(input) {
    var lines = input.split('\n');
    lines.shift();
    lines.map(function (line) {
        if (IPV4.test(line)) {
            console.log("IPv4");
        } else if (IPV6.test(line)) {
            console.log("IPv6");
        } else {
            console.log("Neither");
        }
    })
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
