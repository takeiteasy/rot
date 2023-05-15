// const webSocket = new WebSocket('ws://localhost:8765/');

// webSocket.addEventListener("open", function() {
//     console.log("Connected!");
//     webSocket.send("test");
// });

// webSocket.addEventListener("message", function(event) {
//     console.log(event);
// });

// webSocket.addEventListener("error", function(event) {
//     console.log("WebSocket error: ", event);
// });

// webSocket.addEventListener("close", function() {
//     console.log("Goodbye!");
// });

function byte2Hex(n) {
    var nybHexString = "0123456789ABCDEF";
    return String(nybHexString.substr((n >> 4) & 0x0F, 1)) + nybHexString.substr(n & 0x0F, 1);
}

function RGB2Color(r, g, b) {
    return '#' + byte2Hex(r) + byte2Hex(g) + byte2Hex(b);
}

function easeOut(t, b, c, d) {
    var ts = (t /= d) * t;
    var tc = ts * t;
    return b + c * (tc + -3 * ts + 3 * t);
}

var canvas = document.getElementById("canvas");
let offset = -1.65806;
let arc = Math.PI / (37 / 2);
let wheel = [0, 32, 15, 19, 4, 21, 2, 25, 17, 34, 6, 27, 13, 36, 11, 30, 8, 23, 10, 5, 24, 16, 33, 1, 20, 14, 31, 9, 22, 18, 29, 7, 28, 12, 35, 3, 26];
var spinAngle = offset;
var spinTime = 0;
const spinTimeMax = 1000;
const slowSpinTimeMax = 1000;
var spinTimeout = spinTimeMax;
var spinTimer = null;
const spinSpeedMax = 10;
var spinSpeed = 0;
var slowSpin = false;

function drawWheel() {
    var outsideRadius = 200;
    var textRadius = 180;
    var insideRadius = 125;

    var ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, 500, 500);

    ctx.strokeStyle = "black";
    ctx.lineWidth = 2;

    ctx.font = 'bold 20px Helvetica, Arial';

    for (var i = 0; i < 37; i++) {
        var angle = spinAngle + i * arc;
        ctx.fillStyle = i == 0 ? RGB2Color(0, 255, 0) : i % 2 ? RGB2Color(0, 0, 0) : RGB2Color(221, 56, 51);

        ctx.beginPath();
        ctx.arc(250, 250, outsideRadius, angle, angle + arc, false);
        ctx.arc(250, 250, insideRadius, angle + arc, angle, true);
        ctx.stroke();
        ctx.fill();

        ctx.save();
        ctx.fillStyle = "white";
        ctx.translate(250 + Math.cos(angle + arc / 2) * textRadius,
            250 + Math.sin(angle + arc / 2) * textRadius);
        ctx.rotate(angle + arc / 2 + Math.PI / 2);
        var text = wheel[i].toString();
        ctx.fillText(text, -ctx.measureText(text).width / 2, 0);
        ctx.restore();
    }
}

function beginSpin() {
    spinAngle = offset;
    spinTime = 0;
    spinTimeout = spinTimeMax + (Math.random() * 1000);
    spinSpeed = spinSpeedMax;
    slowSpin = false;
    spin();
}

function spin() {
    spinTime += 10;
    if (spinTime >= spinTimeout) {
        endSpin();
        return;
    }
    if (slowSpin)
        spinSpeed = spinSpeedMax - easeOut(spinTime, 0, spinSpeedMax, spinTimeout);
    spinAngle += (spinSpeed * Math.PI / 180);
    drawWheel();
    spinTimer = setTimeout('spin()', 10);
}

function endSpin() {
    console.log("endSpin");
    clearTimeout(spinTimer);
    spinTime = 0;
    if (!slowSpin) {
        slowSpin = true;
        spinTimeout = slowSpinTimeMax + Math.random() * 1000; 
        spin();
    } else {
        finish();
    }
}

function finish() {
    console.log("finished");
}

drawWheel();
beginSpin();