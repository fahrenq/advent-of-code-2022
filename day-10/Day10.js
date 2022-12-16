const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const WIDTH = 40;

let cycle = 0;
let register = 1;
let cyclesToCapture = [20, 60, 100, 140, 180, 220];
let part1 = 0;

function checkPart1Solution() {
    if (cyclesToCapture.includes(cycle)) {
        part1 += register * cycle;
    }
}

function isSpriteVisible() {
    const pixelBeingDrawn = cycle % WIDTH;
    return Math.abs(pixelBeingDrawn - register) <= 1;
}

function draw() {
    if (cycle % WIDTH === 0) {
        process.stdout.write("\n");
    }
    if (isSpriteVisible()) {
        process.stdout.write("#");
    } else {
        process.stdout.write(".");
    }
}

function bumpCycle() {
    draw();
    cycle++;
    checkPart1Solution();
}

console.log('Part 2 solution:');
let line;
for (line of input) {
    bumpCycle();
    if (line.startsWith('addx')) {
        bumpCycle();
        register += Number(line.split(' ')[1]);
    }
}
process.stdout.write("\n\n");
console.log('Part 1 solution:', part1);

/*
* I kind of wanted to play with mutable variables anyway, so to make it
* exciting, I abandoned immutability for just one day! :)
* */
