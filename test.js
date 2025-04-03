import * as process from "node:process";

const fs = await import("node:fs");

export class Foo {
	#a;
	b = 1;
	static c = 1;

	static {
		this.c++;
	}
};

const f1 = () => { };
function* f2() {
}

async function f3() {
	await Promise.resolve();
}
async function* f4() {
}
function f5(...rest) {
}

const s = `${1 + 1}`;
const n = 2 ** 3;
const b = 10000n;
const l = [...[1, 2, 3]];
const r1 = /a/s;
const r2 = /a/d;
const r3 = /a/v;

const x = l?.[0]?.a ?? s;

let a = null;
a ??= s;
a &&= b;
a ||= n;

for (let i of [1, 2]) {
}

for await (const x of f4()) {
}

try {
} catch {
}
